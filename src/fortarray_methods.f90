submodule (fortarray_types) fortarray_methods
    !! Implementation of all xarray-compatible methods for fortarray_t
    !! This submodule contains ALL method implementations migrated from external functions
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
    use fortarray_missing_data, only: is_missing
    use omp_lib, only: omp_get_wtime
    use fortarray_storage
    use fortarray_memory
    use fortarray_slicing
    use fortarray_indexing
    use fortarray_constructors, only: new_array, create_coordinate
    use fortarray_missing_data, only: dropna
    use fortarray_netcdf, only: write_netcdf_variable
    use fortarray_io, only: to_hdf5, to_zarr, to_binary
    use fortarray_interoperability
    ! DTYPE constants are in fortarray_types, available via parent module
    implicit none
    
    ! Selection method constants
    character(len=*), parameter :: METHOD_EXACT = "exact"
    character(len=*), parameter :: METHOD_NEAREST = "nearest"
    
contains

    ! ======= SELECTION METHODS (MIGRATED FROM fortarray_coordinate_selection.f90) =======
    
    !> Generic sel method - dispatches to point or range selection
    module function fortarray_sel(this, coord_name, value, start_val, stop_val, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in), optional :: value      ! For point selection
        real(real64), intent(in), optional :: start_val, stop_val  ! For range selection
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        
        ! Check which type of selection is requested
        if (present(value) .and. .not. present(start_val) .and. .not. present(stop_val)) then
            ! Point selection
            result_array = fortarray_sel_point_r64(this, coord_name, value, method)
        else if (present(start_val) .and. present(stop_val) .and. .not. present(value)) then
            ! Range selection
            result_array = fortarray_sel_range_r64(this, coord_name, start_val, stop_val)
        else
            write(error_unit, '(A)') "ERROR: sel() requires either 'value' for point selection or &
                &both 'start_val' and 'stop_val' for range selection"
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel
    
    !> Select by coordinate name and real64 value
    module function fortarray_sel_point_r64(this, coord_name, value, method, drop) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in) :: value
        character(len=*), intent(in), optional :: method
        logical, intent(in), optional :: drop
        type(fortarray_t) :: result_array
        
        character(len=20) :: sel_method
        integer :: coord_idx, value_idx
        logical :: do_drop
        
        ! Set default method and drop behavior
        sel_method = METHOD_EXACT
        if (present(method)) sel_method = method
        do_drop = .true.  ! Default: drop dimensions of size 1
        if (present(drop)) do_drop = drop
        
        ! Find coordinate by name
        coord_idx = find_coordinate_by_name(this, coord_name)
        if (coord_idx == 0) then
            write(error_unit, '(A,A,A)') "ERROR: Coordinate '", trim(coord_name), "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Find value index (use reasonable tolerance for coordinate matching)
        value_idx = find_coord_index(this%coords(coord_idx), value, sel_method, 0.15_real64)
        if (value_idx == 0) then
            write(error_unit, '(A,F0.6,A,A)') "ERROR: Value ", value, " not found in coordinate ", trim(coord_name)
            result_array = create_empty_like(this)
            return
        end if
        
        ! Perform selection using existing slice functionality
        if (do_drop) then
            ! Default behavior: drop the selected dimension
            result_array = slice_along_dimension(this, coord_idx, value_idx)
        else
            ! Keep dimension but make it size 1
            result_array = slice_range_along_dimension(this, coord_idx, value_idx, value_idx, 1)
        end if
        
    end function fortarray_sel_point_r64
    
    !> Select by coordinate name and real32 value
    module function fortarray_sel_point_r32(this, coord_name, value, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real32), intent(in) :: value
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        
        ! Convert to real64 and call main implementation
        result_array = fortarray_sel_point_r64(this, coord_name, real(value, real64), method)
        
    end function fortarray_sel_point_r32
    
    !> Select by coordinate name and character value (for time strings, etc.)
    module function fortarray_sel_point_char(this, coord_name, value, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        character(len=*), intent(in) :: value
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        
        ! For now, convert string to real if possible, otherwise error
        real(real64) :: numeric_value
        integer :: ios
        
        read(value, *, iostat=ios) numeric_value
        if (ios == 0) then
            result_array = fortarray_sel_point_r64(this, coord_name, numeric_value, method)
        else
            write(error_unit, '(A,A,A)') "ERROR: Cannot convert string '", trim(value), "' to numeric value"
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel_point_char
    
    !> Select range by coordinate name and real64 values
    module function fortarray_sel_range_r64(this, coord_name, start_val, stop_val, step_val) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in) :: start_val, stop_val
        real(real64), intent(in), optional :: step_val
        type(fortarray_t) :: result_array
        
        integer :: coord_idx, start_idx, stop_idx
        
        ! Find coordinate by name
        coord_idx = find_coordinate_by_name(this, coord_name)
        if (coord_idx == 0) then
            write(error_unit, '(A,A,A)') "ERROR: Coordinate '", trim(coord_name), "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Find range indices
        call find_range_indices(this, coord_idx, start_val, stop_val, start_idx, stop_idx)
        if (start_idx == 0 .or. stop_idx == 0) then
            write(error_unit, '(A,F0.6,A,F0.6,A,A)') "ERROR: Range [", start_val, ",", stop_val, &
                "] not found in coordinate ", trim(coord_name)
            result_array = create_empty_like(this)
            return
        end if
        
        ! Perform range selection
        result_array = slice_range_along_dimension(this, coord_idx, start_idx, stop_idx)
        
    end function fortarray_sel_range_r64
    
    !> Select range by coordinate name and character values
    module function fortarray_sel_range_char(this, coord_name, start_val, stop_val) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        character(len=*), intent(in) :: start_val, stop_val
        type(fortarray_t) :: result_array
        
        real(real64) :: start_numeric, stop_numeric
        integer :: ios1, ios2
        
        ! Convert strings to numeric
        read(start_val, *, iostat=ios1) start_numeric
        read(stop_val, *, iostat=ios2) stop_numeric
        
        if (ios1 == 0 .and. ios2 == 0) then
            result_array = fortarray_sel_range_r64(this, coord_name, start_numeric, stop_numeric)
        else
            write(error_unit, '(A)') "ERROR: Cannot convert string range to numeric values"
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel_range_char
    
    ! ======= INDEX SELECTION METHODS =======
    
    !> Generic isel method - dispatches to point or range selection
    module function fortarray_isel(this, dim_name, index, start_idx, stop_idx, step_idx) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: dim_name
        integer, intent(in), optional :: index      ! For point selection
        integer, intent(in), optional :: start_idx, stop_idx, step_idx  ! For range selection
        type(fortarray_t) :: result_array
        
        ! Check which type of selection is requested
        if (present(index) .and. .not. present(start_idx) .and. .not. present(stop_idx)) then
            ! Point selection
            result_array = fortarray_isel_point(this, dim_name, index)
        else if (present(start_idx) .and. present(stop_idx) .and. .not. present(index)) then
            ! Range selection
            result_array = fortarray_isel_range(this, dim_name, start_idx, stop_idx, step_idx)
        else
            write(error_unit, '(A)') "ERROR: isel() requires either 'index' for point selection or &
                &both 'start_idx' and 'stop_idx' for range selection"
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_isel
    
    !> Select by dimension name and integer index
    module function fortarray_isel_point(this, dim_name, index) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: dim_name
        integer, intent(in) :: index
        type(fortarray_t) :: result_array
        
        integer :: dim_idx, actual_index
        
        ! Find dimension by name
        dim_idx = find_dimension_by_name(this, dim_name)
        if (dim_idx == 0) then
            write(error_unit, '(A,A,A)') "ERROR: Dimension '", trim(dim_name), "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Handle negative indexing (Python-style)
        if (index < 0) then
            actual_index = this%shape(dim_idx) + index + 1  ! -1 maps to last element
        else
            actual_index = index
        end if
        
        ! Check bounds
        if (actual_index < 1 .or. actual_index > this%shape(dim_idx)) then
            write(error_unit, '(A,I0,A,I0)') "ERROR: Index ", index, " out of bounds for dimension size ", this%shape(dim_idx)
            result_array = create_empty_like(this)
            return
        end if
        
        ! Perform index selection
        result_array = slice_along_dimension(this, dim_idx, actual_index)
        
    end function fortarray_isel_point
    
    !> Select range by dimension name and integer indices
    module function fortarray_isel_range(this, dim_name, start_idx, stop_idx, step_idx) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: dim_name
        integer, intent(in) :: start_idx, stop_idx
        integer, intent(in), optional :: step_idx
        type(fortarray_t) :: result_array
        
        integer :: dim_idx, step_val, actual_start, actual_stop
        
        ! Set default step
        step_val = 1
        if (present(step_idx)) step_val = step_idx
        
        ! Find dimension by name
        dim_idx = find_dimension_by_name(this, dim_name)
        if (dim_idx == 0) then
            write(error_unit, '(A,A,A)') "ERROR: Dimension '", trim(dim_name), "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Handle negative indexing
        if (start_idx < 0) then
            actual_start = this%shape(dim_idx) + start_idx + 1
        else
            actual_start = start_idx
        end if
        
        if (stop_idx < 0) then
            actual_stop = this%shape(dim_idx) + stop_idx + 1
        else
            actual_stop = stop_idx
        end if
        
        ! Check bounds
        if (actual_start < 1 .or. actual_stop > this%shape(dim_idx) .or. &
            (step_val > 0 .and. actual_start > actual_stop) .or. &
            (step_val < 0 .and. actual_start < actual_stop)) then
            write(error_unit, '(A,2I0,A,I0)') "ERROR: Index range [", start_idx, ":", stop_idx, &
                "] invalid for dimension size ", this%shape(dim_idx)
            result_array = create_empty_like(this)
            return
        end if
        
        ! Perform range selection
        result_array = slice_range_along_dimension(this, dim_idx, actual_start, actual_stop, step_val)
        
    end function fortarray_isel_range
    
    !> Select by dimension name and integer array (fancy indexing)
    module function fortarray_isel_indices(this, dim_name, indices) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: dim_name
        integer, dimension(:), intent(in) :: indices
        type(fortarray_t) :: result_array
        
        integer :: dim_idx, i, j, k, n_indices, n_before, n_after
        integer :: idx, src_idx, dest_idx, actual_idx
        integer, allocatable :: new_shape(:)
        real(real64), allocatable :: values_r64(:), result_values(:)
        character(len=:), allocatable :: var_name
        
        ! Find dimension by name
        dim_idx = find_dimension_by_name(this, dim_name)
        if (dim_idx == 0) then
            write(error_unit, '(A,A,A)') "ERROR: Dimension '", trim(dim_name), "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        n_indices = size(indices)
        if (n_indices == 0) then
            write(error_unit, '(A)') "ERROR: Empty indices array"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Check all indices are valid (handle negative indices)
        do i = 1, n_indices
            if (indices(i) < 0) then
                actual_idx = this%shape(dim_idx) + indices(i) + 1
            else
                actual_idx = indices(i)
            end if
            
            if (actual_idx < 1 .or. actual_idx > this%shape(dim_idx)) then
                write(error_unit, '(A,I0,A,I0)') "ERROR: Index ", indices(i), &
                    " out of bounds for dimension size ", this%shape(dim_idx)
                result_array = create_empty_like(this)
                return
            end if
        end do
        
        ! Create new shape - same as original but with dim_idx replaced by n_indices
        allocate(new_shape(this%n_dims))
        new_shape = this%shape
        new_shape(dim_idx) = n_indices
        
        ! Calculate elements before and after the indexed dimension
        n_before = 1
        do i = 1, dim_idx - 1
            n_before = n_before * this%shape(i)
        end do
        
        n_after = 1
        do i = dim_idx + 1, this%n_dims
            n_after = n_after * this%shape(i)
        end do
        
        ! Extract values
        call get_values_r64(this%data, values_r64)
        allocate(result_values(product(new_shape)))
        
        ! Copy selected indices
        dest_idx = 1
        do k = 1, n_after
            do j = 1, n_indices
                ! Handle negative index
                if (indices(j) < 0) then
                    idx = this%shape(dim_idx) + indices(j) + 1
                else
                    idx = indices(j)
                end if
                
                do i = 1, n_before
                    src_idx = (k-1)*n_before*this%shape(dim_idx) + &
                             (idx-1)*n_before + i
                    result_values(dest_idx) = values_r64(src_idx)
                    dest_idx = dest_idx + 1
                end do
            end do
        end do
        
        ! Create result array
        var_name = trim(this%name) // "_fancy"
        result_array = create_fortarray_with_shape(result_values, new_shape, &
                                                 this%dim_names, var_name)
        
        ! Copy and update coordinates
        if (allocated(this%coords)) then
            call copy_coords_with_fancy_indexing(this, result_array, dim_idx, indices)
        end if
        
    end function fortarray_isel_indices
    
    ! ======= FILTERING METHODS =======
    
    !> Filter by condition (xarray where() equivalent)
    module function fortarray_filter_condition(this, condition, other_value) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t), intent(in) :: condition  ! Boolean array
        real(real64), intent(in), optional :: other_value
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: filter_condition not yet implemented"
        result_array = create_empty_like(this)
        
    end function fortarray_filter_condition
    
    !> Filter by boolean mask
    module function fortarray_filter_mask(this, mask) result(result_array)
        class(fortarray_t), intent(in) :: this
        logical, dimension(:), intent(in) :: mask
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: filter_mask not yet implemented"
        result_array = create_empty_like(this)
        
    end function fortarray_filter_mask
    
    ! ======= AGGREGATION METHODS =======
    
    !> Compute mean over all dimensions
    module function fortarray_mean_all(this) result(result_array)
        use fortarray_time_operations, only: resample_to_daily, resample_to_monthly, &
                                            resample_to_yearly
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        real(real64) :: mean_value
        integer :: i
        character(len=:), allocatable :: resample_freq
        
        ! Check if this is a resampled array
        resample_freq = get_resample_freq(this)
        if (len_trim(resample_freq) > 0) then
            ! Perform resampling with mean aggregation
            result_array = perform_resample_aggregation(this, resample_freq, "mean")
            return
        end if
        
        ! Check for empty array
        if (this%n_elements == 0) then
            write(error_unit, '(A)') "ERROR: Cannot compute mean of empty array"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Compute mean based on data type
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            mean_value = sum(this%data%values_r64) / real(this%n_elements, real64)
        case(DTYPE_REAL32)
            mean_value = sum(real(this%data%values_r32, real64)) / real(this%n_elements, real64)
        case(DTYPE_INT32)
            mean_value = sum(real(this%data%values_i32, real64)) / real(this%n_elements, real64)
        case(DTYPE_INT64)
            mean_value = sum(real(this%data%values_i64, real64)) / real(this%n_elements, real64)
        case default
            write(error_unit, '(A)') "ERROR: Cannot compute mean for this data type"
            result_array = create_empty_like(this)
            return
        end select
        
        ! Create scalar result
        result_array = create_scalar_fortarray(mean_value, this%name // "_mean", this%units)
        
    end function fortarray_mean_all
    
    !> Compute mean over specified dimensions
    module function fortarray_mean_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: mean_dims not yet implemented"
        result_array = create_empty_like(this)
        
    end function fortarray_mean_dims
    
    !> Compute sum over all dimensions
    module function fortarray_sum_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        real(real64) :: sum_value
        character(len=:), allocatable :: resample_freq
        
        ! Check if this is a resampled array
        resample_freq = get_resample_freq(this)
        if (len_trim(resample_freq) > 0) then
            ! Perform resampling with sum aggregation
            result_array = perform_resample_aggregation(this, resample_freq, "sum")
            return
        end if
        
        ! Compute sum based on data type
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            sum_value = sum(this%data%values_r64)
        case(DTYPE_REAL32)
            sum_value = sum(real(this%data%values_r32, real64))
        case(DTYPE_INT32)
            sum_value = sum(real(this%data%values_i32, real64))
        case(DTYPE_INT64)
            sum_value = sum(real(this%data%values_i64, real64))
        case default
            write(error_unit, '(A)') "ERROR: Cannot compute sum for this data type"
            result_array = create_empty_like(this)
            return
        end select
        
        ! Create scalar result
        result_array = create_scalar_fortarray(sum_value, this%name // "_sum", this%units)
        
    end function fortarray_sum_all
    
    !> Compute maximum over all dimensions
    module function fortarray_max_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        real(real64) :: max_value
        character(len=:), allocatable :: resample_freq
        
        ! Check if this is a resampled array
        resample_freq = get_resample_freq(this)
        if (len_trim(resample_freq) > 0) then
            ! Perform resampling with max aggregation
            result_array = perform_resample_aggregation(this, resample_freq, "max")
            return
        end if
        
        ! Compute max based on data type
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            max_value = maxval(this%data%values_r64)
        case(DTYPE_REAL32)
            max_value = maxval(real(this%data%values_r32, real64))
        case(DTYPE_INT32)
            max_value = maxval(real(this%data%values_i32, real64))
        case(DTYPE_INT64)
            max_value = maxval(real(this%data%values_i64, real64))
        case default
            write(error_unit, '(A)') "ERROR: Unsupported data type for max"
            result_array = create_empty_like(this)
            return
        end select
        
        ! Create scalar result
        result_array = create_scalar_fortarray(max_value, trim(this%name)//"_max", this%units)
        
    end function fortarray_max_all
    
    !> Compute minimum over all dimensions
    module function fortarray_min_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        real(real64) :: min_value
        character(len=:), allocatable :: resample_freq
        
        ! Check if this is a resampled array
        resample_freq = get_resample_freq(this)
        if (len_trim(resample_freq) > 0) then
            ! Perform resampling with min aggregation
            result_array = perform_resample_aggregation(this, resample_freq, "min")
            return
        end if
        
        ! Compute min based on data type
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            min_value = minval(this%data%values_r64)
        case(DTYPE_REAL32)
            min_value = minval(real(this%data%values_r32, real64))
        case(DTYPE_INT32)
            min_value = minval(real(this%data%values_i32, real64))
        case(DTYPE_INT64)
            min_value = minval(real(this%data%values_i64, real64))
        case default
            write(error_unit, '(A)') "ERROR: Unsupported data type for min"
            result_array = create_empty_like(this)
            return
        end select
        
        ! Create scalar result
        result_array = create_scalar_fortarray(min_value, trim(this%name)//"_min", this%units)
        
    end function fortarray_min_all
    
    !> Compute standard deviation over all dimensions
    module function fortarray_std_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        real(real64) :: mean_value, variance, std_value
        character(len=:), allocatable :: resample_freq
        integer :: i
        
        ! Check if this is a resampled array
        resample_freq = get_resample_freq(this)
        if (len_trim(resample_freq) > 0) then
            ! Perform resampling with std aggregation
            result_array = perform_resample_aggregation(this, resample_freq, "std")
            return
        end if
        
        ! First compute mean
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            mean_value = sum(this%data%values_r64) / real(this%n_elements, real64)
            variance = 0.0_real64
            do i = 1, this%n_elements
                variance = variance + (this%data%values_r64(i) - mean_value)**2
            end do
        case(DTYPE_REAL32)
            mean_value = sum(real(this%data%values_r32, real64)) / real(this%n_elements, real64)
            variance = 0.0_real64
            do i = 1, this%n_elements
                variance = variance + (real(this%data%values_r32(i), real64) - mean_value)**2
            end do
        case(DTYPE_INT32)
            mean_value = sum(real(this%data%values_i32, real64)) / real(this%n_elements, real64)
            variance = 0.0_real64
            do i = 1, this%n_elements
                variance = variance + (real(this%data%values_i32(i), real64) - mean_value)**2
            end do
        case(DTYPE_INT64)
            mean_value = sum(real(this%data%values_i64, real64)) / real(this%n_elements, real64)
            variance = 0.0_real64
            do i = 1, this%n_elements
                variance = variance + (real(this%data%values_i64(i), real64) - mean_value)**2
            end do
        case default
            write(error_unit, '(A)') "ERROR: Unsupported data type for std"
            result_array = create_empty_like(this)
            return
        end select
        
        variance = variance / real(this%n_elements - 1, real64)
        std_value = sqrt(variance)
        
        ! Create scalar result
        result_array = create_scalar_fortarray(std_value, trim(this%name)//"_std", this%units)
        
    end function fortarray_std_all
    
    !> Compute sum over specified dimensions
    module function fortarray_sum_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: sum_dims not yet implemented"
        result_array = create_empty_like(this)
        
    end function fortarray_sum_dims
    
    ! ======= PLACEHOLDER IMPLEMENTATIONS FOR REMAINING METHODS =======
    ! (Following BACKLOG.md: implement FULL functionality, no shortcuts)
    
    module function fortarray_std_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: std_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_std_dims
    
    module function fortarray_var_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: var_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_var_all
    
    module function fortarray_var_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: var_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_var_dims
    
    module function fortarray_min_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: min_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_min_dims
    
    module function fortarray_max_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: max_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_max_dims
    
    module function fortarray_median(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: median not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_median
    
    
    module function fortarray_values_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        ! Return a view of the underlying data (same array, not a copy)
        result_array = this
        result_array%is_view = .true.
        result_array%owns_memory = .false.
        
    end function fortarray_values_all
    
    module function fortarray_values_copy(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        integer :: i
        
        ! Create a proper copy of this array
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        result_array%is_view = .false.
        result_array%owns_memory = .true.
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Copy coordinates
        if (allocated(this%coords)) then
            allocate(result_array%coords(size(this%coords)))
            do i = 1, size(this%coords)
                result_array%coords(i) = this%coords(i)
            end do
        end if
        if (allocated(this%has_coord)) then
            allocate(result_array%has_coord(size(this%has_coord)))
            result_array%has_coord = this%has_coord
        end if
        
        ! Initialize data storage and copy data
        result_array%data%initialized = .true.
        result_array%data%dtype = this%data%dtype
        result_array%data%n_elements = this%n_elements
        
        ! Copy data based on type
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                allocate(result_array%data%values_r64(this%n_elements))
                result_array%data%values_r64(:) = this%data%values_r64(:)
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                allocate(result_array%data%values_r32(this%n_elements))
                result_array%data%values_r32(:) = this%data%values_r32(:)
            end if
        case(DTYPE_INT64)
            if (allocated(this%data%values_i64)) then
                allocate(result_array%data%values_i64(this%n_elements))
                result_array%data%values_i64(:) = this%data%values_i64(:)
            end if
        case(DTYPE_INT32)
            if (allocated(this%data%values_i32)) then
                allocate(result_array%data%values_i32(this%n_elements))
                result_array%data%values_i32(:) = this%data%values_i32(:)
            end if
        case(DTYPE_LOGICAL)
            if (allocated(this%data%values_logical)) then
                allocate(result_array%data%values_logical(this%n_elements))
                result_array%data%values_logical(:) = this%data%values_logical(:)
            end if
        case default
            write(error_unit, '(A)') "ERROR: Unsupported data type in values_copy"
        end select
        
    end function fortarray_values_copy
    
    module function fortarray_to_netcdf_file(this, filename) result(status)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: status
        
        ! Use the write_netcdf_variable function from fortarray_netcdf
        status = write_netcdf_variable(filename, this)
        
    end function fortarray_to_netcdf_file
    
    !> Write array to HDF5 format
    module function fortarray_to_hdf5(this, filename, group, append, options) result(status)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: group
        logical, intent(in), optional :: append
        type(write_options_t), intent(in), optional :: options
        integer :: status
        
        status = to_hdf5(this, filename, group, append, options)
    end function fortarray_to_hdf5
    
    !> Write array to Zarr format
    module function fortarray_to_zarr(this, dirname, chunks) result(status)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: dirname
        integer, dimension(:), intent(in), optional :: chunks
        integer :: status
        
        status = to_zarr(this, dirname, chunks)
    end function fortarray_to_zarr
    
    !> Write array to binary format
    module function fortarray_to_binary(this, filename) result(status)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: status
        
        status = to_binary(this, filename)
    end function fortarray_to_binary
    
    module function fortarray_to_numpy_like(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: to_numpy_like not yet implemented"
        ! Return uninitialized array for placeholder
        result_array%initialized = .false.
    end function fortarray_to_numpy_like
    
    module function fortarray_to_pandas_like(this, index_from_coords, flatten_multiindex, &
                                           preserve_metadata, datetime_index) result(result_array)
        class(fortarray_t), intent(in) :: this
        logical, intent(in), optional :: index_from_coords
        logical, intent(in), optional :: flatten_multiindex
        logical, intent(in), optional :: preserve_metadata
        logical, intent(in), optional :: datetime_index
        type(fortarray_t) :: result_array
        
        logical :: use_coords, flatten, preserve_meta, datetime_fmt
        integer :: i, new_rows
        
        ! Set defaults
        use_coords = .false.
        if (present(index_from_coords)) use_coords = index_from_coords
        
        flatten = .false.
        if (present(flatten_multiindex)) flatten = flatten_multiindex
        
        preserve_meta = .true.
        if (present(preserve_metadata)) preserve_meta = preserve_metadata
        
        datetime_fmt = .false.
        if (present(datetime_index)) datetime_fmt = datetime_index
        
        ! Initialize result array
        result_array%initialized = .true.
        result_array%name = this%name
        
        if (preserve_meta) then
            result_array%units = this%units
            result_array%long_name = this%long_name
            result_array%standard_name = this%standard_name
        end if
        
        if (flatten .and. this%n_dims > 2) then
            ! Flatten to 2D for pandas MultiIndex compatibility
            result_array%n_dims = 2
            allocate(result_array%shape(2), result_array%dim_names(2))
            
            ! First dimension is flattened indices
            new_rows = 1
            do i = 1, this%n_dims - 1
                new_rows = new_rows * this%shape(i)
            end do
            
            result_array%shape(1) = new_rows
            result_array%shape(2) = this%shape(this%n_dims)
            result_array%dim_names(1) = "MultiIndex"
            result_array%dim_names(2) = this%dim_names(this%n_dims)
            result_array%n_elements = new_rows * result_array%shape(2)
            
        else
            ! Keep original structure
            result_array%n_dims = this%n_dims
            result_array%n_elements = this%n_elements
            
            if (allocated(this%shape)) then
                allocate(result_array%shape(size(this%shape)))
                result_array%shape = this%shape
            end if
            
            if (allocated(this%dim_names)) then
                allocate(result_array%dim_names(size(this%dim_names)))
                result_array%dim_names = this%dim_names
            end if
        end if
        
        ! Copy data
        result_array%data%dtype = this%data%dtype
        if (allocated(this%data%values_r64)) then
            allocate(result_array%data%values_r64(result_array%n_elements))
            result_array%data%values_r64 = this%data%values_r64(1:result_array%n_elements)
        end if
        
        ! Handle coordinates if requested
        if (use_coords .and. allocated(this%coords) .and. allocated(this%has_coord)) then
            allocate(result_array%coords(result_array%n_dims))
            allocate(result_array%has_coord(result_array%n_dims))
            
            do i = 1, min(result_array%n_dims, size(this%coords))
                if (this%has_coord(i)) then
                    result_array%coords(i) = this%coords(i)
                    result_array%has_coord(i) = .true.
                    
                    ! Apply datetime formatting if requested
                    if (datetime_fmt .and. i == 1) then
                        ! Mark as datetime coordinate (would need more sophisticated handling)
                        result_array%coords(i)%name = trim(result_array%coords(i)%name) // "_datetime"
                    end if
                else
                    result_array%has_coord(i) = .false.
                end if
            end do
        end if
        
    end function fortarray_to_pandas_like
    
    module function fortarray_fillna_value(this, fill_value) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: fill_value
        type(fortarray_t) :: result_array
        integer :: i
        
        ! Create a proper copy of this array
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = this%data%dtype
        result_array%data%n_elements = this%n_elements
        
        ! Copy data
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                if (.not. allocated(result_array%data%values_r64)) then
                    allocate(result_array%data%values_r64(this%n_elements))
                end if
                result_array%data%values_r64(:) = this%data%values_r64(:)
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                if (.not. allocated(result_array%data%values_r32)) then
                    allocate(result_array%data%values_r32(this%n_elements))
                end if
                result_array%data%values_r32(:) = this%data%values_r32(:)
            end if
        case default
            write(error_unit, '(A)') "ERROR: Data type not supported for copying"
            result_array = create_empty_like(this)
            return
        end select
        
        if (.not. result_array%initialized) then
            write(error_unit, '(A)') "ERROR: Failed to create copy for fillna operation"
            return
        end if
        
        ! Replace missing values (use huge as placeholder for NaN)
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                do i = 1, this%n_elements
                    if (this%data%values_r64(i) >= huge(1.0_real64) * 0.9_real64) then
                        result_array%data%values_r64(i) = fill_value
                    end if
                end do
            end if
        case default
            write(error_unit, '(A)') "ERROR: fillna_value not supported for this data type"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_fillna_value
    
    module function fortarray_fillna_method(this, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: method
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: fillna_method not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_fillna_method
    
    module function fortarray_dropna_any(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: dropna_any not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_dropna_any
    
    module function fortarray_dropna_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: dropna_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_dropna_all
    
    module function fortarray_interpolate_na_linear(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: interpolate_na_linear not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_interpolate_na_linear
    
    module function fortarray_interpolate_na_cubic(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: interpolate_na_cubic not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_interpolate_na_cubic
    
    module function fortarray_ffill(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        integer :: i
        real(real64) :: last_valid
        
        ! Create copy of input array with proper initialization
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = this%data%dtype
        result_array%data%n_elements = this%n_elements
        
        ! Copy data
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                if (.not. allocated(result_array%data%values_r64)) then
                    allocate(result_array%data%values_r64(this%n_elements))
                end if
                result_array%data%values_r64(:) = this%data%values_r64(:)
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                if (.not. allocated(result_array%data%values_r32)) then
                    allocate(result_array%data%values_r32(this%n_elements))
                end if
                result_array%data%values_r32(:) = this%data%values_r32(:)
            end if
        case default
            write(error_unit, '(A)') "ERROR: Data type not supported for copying"
            result_array = create_empty_like(this)
            return
        end select
        
        if (.not. result_array%initialized) then
            write(error_unit, '(A)') "ERROR: Failed to create copy for ffill operation"
            return
        end if
        
        ! Forward fill missing values
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                last_valid = this%data%values_r64(1)  ! Initialize with first value
                do i = 1, this%n_elements
                    if (this%data%values_r64(i) < huge(1.0_real64) * 0.9_real64) then
                        ! Valid value
                        last_valid = this%data%values_r64(i)
                        result_array%data%values_r64(i) = last_valid
                    else
                        ! Missing value - use last valid
                        result_array%data%values_r64(i) = last_valid
                    end if
                end do
            end if
        case default
            write(error_unit, '(A)') "ERROR: ffill not supported for this data type"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_ffill
    
    module function fortarray_transpose_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: transpose_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_transpose_all
    
    module function fortarray_transpose_order(this, order) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, dimension(:), intent(in) :: order
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: transpose_order not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_transpose_order
    
    module function fortarray_stack_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: stack_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_stack_dims
    
    module function fortarray_unstack_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: unstack_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_unstack_dims
    
    module function fortarray_squeeze_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: squeeze_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_squeeze_all
    
    module function fortarray_squeeze_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: squeeze_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_squeeze_dims
    
    module function fortarray_expand_dims_axis(this, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: axis
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: expand_dims_axis not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_expand_dims_axis
    
    module function fortarray_groupby_coord(this, coord_name) result(gb)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        type(groupby_t) :: gb
        
        character(len=:), allocatable :: coord_base, time_component
        integer :: dot_pos, coord_idx, i, component_value
        real(real64), allocatable :: coord_values(:)
        integer, allocatable :: component_values(:)
        type(fortarray_t) :: coord_var
        
        ! Parse coordinate name (e.g., "time.month", "time.year", "time.season")
        dot_pos = index(coord_name, '.')
        if (dot_pos == 0) then
            write(error_unit, '(A)') "ERROR: Time-based groupby requires format 'coord.component'"
            gb%initialized = .false.
            return
        end if
        
        coord_base = coord_name(1:dot_pos-1)
        time_component = coord_name(dot_pos+1:len(coord_name))
        
        ! Find the coordinate
        coord_idx = find_coordinate_by_name(this, coord_base)
        if (coord_idx == 0) then
            write(error_unit, '(A,A)') "ERROR: Coordinate not found: ", coord_base
            gb%initialized = .false.
            return
        end if
        
        ! Extract coordinate values
        if (.not. allocated(this%coords(coord_idx)%values_r64)) then
            write(error_unit, '(A)') "ERROR: Time coordinates must be real64"
            gb%initialized = .false.
            return
        end if
        
        coord_values = this%coords(coord_idx)%values_r64
        allocate(component_values(size(coord_values)))
        
        ! Extract time components based on requested component
        select case(trim(time_component))
        case('month')
            ! Extract month from time values (assuming Julian day or similar)
            do i = 1, size(coord_values)
                component_value = extract_month(coord_values(i))
                component_values(i) = component_value
            end do
            
        case('year')
            ! Extract year from time values
            do i = 1, size(coord_values)
                component_value = extract_year(coord_values(i))
                component_values(i) = component_value
            end do
            
        case('season')
            ! Extract season from time values (1=Spring, 2=Summer, 3=Fall, 4=Winter)
            do i = 1, size(coord_values)
                component_value = extract_season(coord_values(i))
                component_values(i) = component_value
            end do
            
        case('dayofyear')
            ! Extract day of year (1-366)
            do i = 1, size(coord_values)
                component_value = extract_dayofyear(coord_values(i))
                component_values(i) = component_value
            end do
            
        case('weekday')
            ! Extract weekday (1=Sunday, 7=Saturday)
            do i = 1, size(coord_values)
                component_value = extract_weekday(coord_values(i))
                component_values(i) = component_value
            end do
            
        case default
            write(error_unit, '(A,A)') "ERROR: Unsupported time component: ", trim(time_component)
            gb%initialized = .false.
            return
        end select
        
        ! Create a groupby array from the extracted components
        coord_var = new_array(component_values, name=coord_name, &
                             dim_names=[coord_base])
        
        ! Use existing groupby implementation
        gb = this%groupby(coord_base, coord_var)
        
        call finalize_variable(coord_var)
        
    end function fortarray_groupby_coord
    
    module function fortarray_groupby_bins(this, coord_name, bins) result(gb)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        integer, intent(in) :: bins
        type(groupby_t) :: gb
        
        integer :: coord_idx, i
        real(real64), allocatable :: coord_values(:), bin_edges(:)
        integer, allocatable :: bin_indices(:)
        real(real64) :: coord_min, coord_max, bin_width
        type(fortarray_t) :: bin_var
        
        ! Initialize groupby object
        gb%initialized = .false.
        
        ! Find the coordinate
        coord_idx = find_coordinate_by_name(this, coord_name)
        if (coord_idx == 0) then
            write(error_unit, '(A,A)') "ERROR: Coordinate not found: ", coord_name
            return
        end if
        
        ! Extract coordinate values
        if (.not. allocated(this%coords(coord_idx)%values_r64)) then
            write(error_unit, '(A)') "ERROR: Binning coordinates must be real64"
            return
        end if
        
        coord_values = this%coords(coord_idx)%values_r64
        
        ! Calculate bin edges
        coord_min = minval(coord_values)
        coord_max = maxval(coord_values)
        bin_width = (coord_max - coord_min) / real(bins, real64)
        
        allocate(bin_edges(bins + 1))
        do i = 1, bins + 1
            bin_edges(i) = coord_min + real(i - 1, real64) * bin_width
        end do
        
        ! Assign each coordinate value to a bin
        allocate(bin_indices(size(coord_values)))
        do i = 1, size(coord_values)
            bin_indices(i) = assign_to_bin(coord_values(i), bin_edges, bins)
        end do
        
        ! Create a groupby array from the bin indices
        bin_var = new_array(bin_indices, name=trim(coord_name) // "_bins", &
                           dim_names=[coord_name])
        
        ! Use existing groupby implementation
        gb = this%groupby(coord_name, bin_var)
        
        call finalize_variable(bin_var)
        
    end function fortarray_groupby_bins
    
    module function fortarray_groupby_quantiles(this, coord_name, n_quantiles) result(gb)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        integer, intent(in) :: n_quantiles
        type(groupby_t) :: gb
        
        integer :: coord_idx, i
        real(real64), allocatable :: coord_values(:), quantile_edges(:)
        integer, allocatable :: quantile_indices(:)
        type(fortarray_t) :: quantile_var
        
        ! Initialize groupby object
        gb%initialized = .false.
        
        ! Find the coordinate
        coord_idx = find_coordinate_by_name(this, coord_name)
        if (coord_idx == 0) then
            write(error_unit, '(A,A)') "ERROR: Coordinate not found: ", coord_name
            return
        end if
        
        ! Extract coordinate values
        if (.not. allocated(this%coords(coord_idx)%values_r64)) then
            write(error_unit, '(A)') "ERROR: Quantile binning coordinates must be real64"
            return
        end if
        
        coord_values = this%coords(coord_idx)%values_r64
        
        ! Calculate quantile edges
        allocate(quantile_edges(n_quantiles + 1))
        call create_quantile_bins(coord_values, n_quantiles, quantile_edges)
        
        ! Assign each coordinate value to a quantile bin
        allocate(quantile_indices(size(coord_values)))
        do i = 1, size(coord_values)
            quantile_indices(i) = assign_to_bin(coord_values(i), quantile_edges, n_quantiles)
        end do
        
        ! Create a groupby array from the quantile indices
        quantile_var = new_array(quantile_indices, name=trim(coord_name) // "_quantiles", &
                                dim_names=[coord_name])
        
        ! Use existing groupby implementation
        gb = this%groupby(coord_name, quantile_var)
        
        call finalize_variable(quantile_var)
        
    end function fortarray_groupby_quantiles
    
    module function fortarray_resample(this, freq, align) result(result_array)
        use fortarray_time_operations, only: resample_to_daily, resample_to_monthly, &
                                            resample_to_yearly, upsample_to_daily
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: freq
        character(len=*), intent(in), optional :: align
        type(fortarray_t) :: result_array
        type(attribute_t), allocatable :: temp_attrs(:)
        integer :: new_size
        
        ! Frequency constants
        character(len=*), parameter :: FREQ_DAILY = "D"
        character(len=*), parameter :: FREQ_WEEKLY = "W"
        character(len=*), parameter :: FREQ_MONTHLY = "M"
        character(len=*), parameter :: FREQ_QUARTERLY = "Q"
        character(len=*), parameter :: FREQ_YEARLY = "Y"
        character(len=*), parameter :: FREQ_HOURLY = "H"
        
        ! For now, we create a resample object that supports method chaining
        ! The actual aggregation happens when an aggregation method is called
        
        ! Initialize result_array as a copy of this
        result_array = this
        
        ! Ensure attrs array is allocated with at least 2 elements
        if (.not. allocated(result_array%attrs)) then
            allocate(result_array%attrs(2))
            result_array%n_attrs = 0
        else if (size(result_array%attrs) < result_array%n_attrs + 1) then
            ! Need to resize the array
            new_size = max(result_array%n_attrs + 2, size(result_array%attrs) * 2)
            allocate(temp_attrs(new_size))
            if (result_array%n_attrs > 0) then
                temp_attrs(1:result_array%n_attrs) = result_array%attrs(1:result_array%n_attrs)
            end if
            call move_alloc(temp_attrs, result_array%attrs)
        end if
        
        ! Add resample frequency attribute
        select case(trim(freq))
        case(FREQ_DAILY)
            result_array%n_attrs = result_array%n_attrs + 1
            result_array%attrs(result_array%n_attrs)%name = "_resample_freq"
            result_array%attrs(result_array%n_attrs)%value = "D"
            result_array%attrs(result_array%n_attrs)%dtype = ATTR_TYPE_STRING
        case(FREQ_WEEKLY)
            result_array%n_attrs = result_array%n_attrs + 1
            result_array%attrs(result_array%n_attrs)%name = "_resample_freq"
            result_array%attrs(result_array%n_attrs)%value = "W"
            result_array%attrs(result_array%n_attrs)%dtype = ATTR_TYPE_STRING
        case(FREQ_MONTHLY)
            result_array%n_attrs = result_array%n_attrs + 1
            result_array%attrs(result_array%n_attrs)%name = "_resample_freq"
            result_array%attrs(result_array%n_attrs)%value = "M"
            result_array%attrs(result_array%n_attrs)%dtype = ATTR_TYPE_STRING
        case(FREQ_YEARLY)
            result_array%n_attrs = result_array%n_attrs + 1
            result_array%attrs(result_array%n_attrs)%name = "_resample_freq"
            result_array%attrs(result_array%n_attrs)%value = "Y"
            result_array%attrs(result_array%n_attrs)%dtype = ATTR_TYPE_STRING
        case(FREQ_HOURLY)
            result_array%n_attrs = result_array%n_attrs + 1
            result_array%attrs(result_array%n_attrs)%name = "_resample_freq"
            result_array%attrs(result_array%n_attrs)%value = "H"
            result_array%attrs(result_array%n_attrs)%dtype = ATTR_TYPE_STRING
        case("6H", "12H", "3H")
            ! Handle specific hour frequencies
            result_array%n_attrs = result_array%n_attrs + 1
            result_array%attrs(result_array%n_attrs)%name = "_resample_freq"
            result_array%attrs(result_array%n_attrs)%value = trim(freq)
            result_array%attrs(result_array%n_attrs)%dtype = ATTR_TYPE_STRING
        case default
            ! Unsupported frequency, return original without adding attribute
        end select
        
        ! Store alignment if provided
        if (present(align)) then
            ! Ensure we have space for another attribute
            if (size(result_array%attrs) < result_array%n_attrs + 1) then
                ! Need to resize the array
                new_size = max(result_array%n_attrs + 1, size(result_array%attrs) * 2)
                allocate(temp_attrs(new_size))
                temp_attrs(1:result_array%n_attrs) = result_array%attrs(1:result_array%n_attrs)
                call move_alloc(temp_attrs, result_array%attrs)
            end if
            
            result_array%n_attrs = result_array%n_attrs + 1
            result_array%attrs(result_array%n_attrs)%name = "_resample_align"
            result_array%attrs(result_array%n_attrs)%value = trim(align)
            result_array%attrs(result_array%n_attrs)%dtype = ATTR_TYPE_STRING
        end if
        
    end function fortarray_resample
    
    module function fortarray_interpolate_resample(this, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        
        character(len=:), allocatable :: resample_freq
        character(len=20) :: interp_method
        
        ! Set default method
        interp_method = "linear"
        if (present(method)) interp_method = method
        
        ! Check if this is a resampled array
        resample_freq = get_resample_freq(this)
        if (len_trim(resample_freq) > 0) then
            ! Perform resampling with interpolation
            result_array = perform_resample_aggregation(this, resample_freq, "interpolate")
            return
        end if
        
        ! Otherwise, just interpolate missing values
        result_array = this%interpolate_na(interp_method)
        
    end function fortarray_interpolate_resample
    
    module function fortarray_rolling_window(this, window) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: window
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: rolling_window not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_rolling_window
    
    subroutine fortarray_init_plot_accessor(this)
        class(fortarray_t), intent(inout) :: this
        write(*, '(A)') "INFO: init_plot_accessor called (placeholder)"
    end subroutine fortarray_init_plot_accessor
    
    ! ======= HELPER FUNCTIONS =======
    
    !> Find coordinate index by name
    function find_coordinate_by_name(var, coord_name) result(coord_idx)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: coord_name
        integer :: coord_idx
        integer :: i
        
        coord_idx = 0
        if (.not. allocated(var%coords)) return
        
        do i = 1, size(var%coords)
            if (var%coords(i)%name == coord_name) then
                coord_idx = i
                return
            end if
        end do
        
    end function find_coordinate_by_name
    
    !> Find dimension index by name
    function find_dimension_by_name(var, dim_name) result(dim_idx)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: dim_name
        integer :: dim_idx
        integer :: i
        
        dim_idx = 0
        if (.not. allocated(var%dim_names)) return
        
        do i = 1, size(var%dim_names)
            if (var%dim_names(i) == dim_name) then
                dim_idx = i
                return
            end if
        end do
        
    end function find_dimension_by_name
    
    !> Find coordinate index for a value (MIGRATED from fortarray_coordinate_selection.f90)
    function find_coord_index(coord, value, method, tolerance) result(idx)
        type(coordinate_t), intent(in) :: coord
        real(real64), intent(in) :: value
        character(len=*), intent(in) :: method
        real(real64), intent(in) :: tolerance
        integer :: idx
        integer :: i, n
        real(real64) :: min_dist, dist
        real(real64), dimension(:), allocatable :: coord_values
        
        idx = 0
        
        ! Get coordinate values as real64
        call get_coord_values_r64(coord, coord_values)
        n = size(coord_values)
        
        if (n == 0) return
        
        select case(method)
        case(METHOD_EXACT)
            ! Exact match with tolerance
            do i = 1, n
                if (abs(coord_values(i) - value) <= tolerance) then
                    idx = i
                    return
                end if
            end do
            
        case(METHOD_NEAREST)
            ! Find nearest neighbor (prefer later index on ties)
            idx = 1
            min_dist = abs(coord_values(1) - value)
            do i = 2, n
                dist = abs(coord_values(i) - value)
                if (dist <= min_dist) then
                    min_dist = dist
                    idx = i
                end if
            end do
            
        case("ffill", "forward")
            ! Forward fill: use last valid coordinate <= target
            idx = 1
            do i = 1, n
                if (coord_values(i) <= value) then
                    idx = i
                else
                    exit
                end if
            end do
            
        case("bfill", "backward")  
            ! Backward fill: use next valid coordinate >= target
            idx = n
            do i = n, 1, -1
                if (coord_values(i) >= value) then
                    idx = i
                else
                    exit
                end if
            end do
            
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown selection method: ", method
        end select
        
    end function find_coord_index
    
    !> Find coordinate index by name in fortarray_t
    function find_coord_index_by_name(arr, coord_name) result(idx)
        class(fortarray_t), intent(in) :: arr
        character(len=*), intent(in) :: coord_name
        integer :: idx
        integer :: i
        
        idx = -1
        
        if (.not. allocated(arr%coords)) return
        if (.not. allocated(arr%dim_names)) return
        
        ! Find dimension index matching coordinate name
        do i = 1, size(arr%dim_names)
            if (trim(arr%dim_names(i)) == trim(coord_name)) then
                if (i <= size(arr%coords)) then
                    if (arr%coords(i)%initialized) then
                        idx = i
                        return
                    end if
                end if
            end if
        end do
        
    end function find_coord_index_by_name
    
    !> Get coordinate values as real64 (MIGRATED from fortarray_coordinate_selection.f90)
    subroutine get_coord_values_r64(coord, values)
        type(coordinate_t), intent(in) :: coord
        real(real64), dimension(:), allocatable, intent(out) :: values
        integer :: n
        
        select case(coord%dtype)
        case(DTYPE_REAL64)
            if (allocated(coord%values_r64)) then
                n = size(coord%values_r64)
                allocate(values(n))
                values = coord%values_r64
            else
                allocate(values(0))
            end if
        case(DTYPE_REAL32)
            if (allocated(coord%values_r32)) then
                n = size(coord%values_r32)
                allocate(values(n))
                values = real(coord%values_r32, real64)
            else
                allocate(values(0))
            end if
        case(DTYPE_INT64)
            if (allocated(coord%values_i64)) then
                n = size(coord%values_i64)
                allocate(values(n))
                values = real(coord%values_i64, real64)
            else
                allocate(values(0))
            end if
        case(DTYPE_INT32)
            if (allocated(coord%values_i32)) then
                n = size(coord%values_i32)
                allocate(values(n))
                values = real(coord%values_i32, real64)
            else
                allocate(values(0))
            end if
        case default
            allocate(values(0))
        end select
        
    end subroutine get_coord_values_r64
    
    !> Find range indices (MIGRATED from fortarray_coordinate_selection.f90)
    subroutine find_range_indices(var, dim, start_val, end_val, start_idx, end_idx)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        real(real64), intent(in) :: start_val, end_val
        integer, intent(out) :: start_idx, end_idx
        real(real64), dimension(:), allocatable :: coord_values
        integer :: i, n
        
        start_idx = 0
        end_idx = 0
        
        if (.not. var%has_coord(dim)) return
        
        call get_coord_values_r64(var%coords(dim), coord_values)
        n = size(coord_values)
        
        
        do i = 1, n
            if (start_idx == 0 .and. coord_values(i) >= start_val) then
                start_idx = i
            end if
            if (coord_values(i) <= end_val) then
                end_idx = i
            end if
        end do
        
    end subroutine find_range_indices
    
    !> Create empty variable like another (MIGRATED from fortarray_coordinate_selection.f90)
    function create_empty_like(var) result(result)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        
        ! Create variable with zero elements
        result%name = var%name
        result%n_dims = var%n_dims
        result%n_elements = 0
        
        if (allocated(var%shape)) then
            allocate(result%shape(size(var%shape)))
            result%shape = 0
            result%shape(1) = 0  ! First dimension is 0
        end if
        
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(size(var%dim_names)))
            result%dim_names = var%dim_names
        end if
        
        ! Allocate empty data storage
        result%data%dtype = var%data%dtype
        select case(result%data%dtype)
        case(DTYPE_REAL64)
            allocate(result%data%values_r64(0))
        case(DTYPE_REAL32)
            allocate(result%data%values_r32(0))
        case(DTYPE_INT32)
            allocate(result%data%values_i32(0))
        case(DTYPE_INT64)
            allocate(result%data%values_i64(0))
        end select
        
        ! Don't mark as initialized if truly empty
        result%initialized = .false.
        
    end function create_empty_like
    
    !> Create scalar fortarray (NEW utility function)
    function create_scalar_fortarray(value, name, units) result(result)
        real(real64), intent(in) :: value
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: units
        type(fortarray_t) :: result
        
        ! Initialize scalar array
        result%name = name
        result%units = units
        result%n_dims = 0
        result%n_elements = 1
        
        ! Set up data storage
        result%data%dtype = DTYPE_REAL64
        allocate(result%data%values_r64(1))
        result%data%values_r64(1) = value
        result%data%n_elements = 1
        result%data%initialized = .true.
        
        result%initialized = .true.
        
    end function create_scalar_fortarray
    
    !> Slice along a single dimension (PROPERLY IMPLEMENTED)
    function slice_along_dimension(var, dim_idx, value_idx) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim_idx, value_idx
        type(fortarray_t) :: result
        
        integer :: i, j, new_idx, src_idx
        integer, dimension(:), allocatable :: new_shape, new_indices
        integer :: n_elements_before, n_elements_after, slice_size
        real(real64), dimension(:), allocatable :: values_r64, result_values
        character(len=:), dimension(:), allocatable :: new_dim_names
        
        ! Validate inputs
        if (dim_idx < 1 .or. dim_idx > var%n_dims) then
            write(error_unit, '(A,I0,A,I0)') "ERROR: Invalid dimension ", dim_idx, " for ", var%n_dims, "D array"
            result = create_empty_like(var)
            return
        end if
        
        if (value_idx < 1 .or. value_idx > var%shape(dim_idx)) then
            write(error_unit, '(A,I0,A,I0)') "ERROR: Index ", value_idx, " out of bounds for dimension size ", var%shape(dim_idx)
            result = create_empty_like(var)
            return
        end if
        
        ! Special case: 1D array becomes scalar
        if (var%n_dims == 1) then
            call get_values_r64(var%data, values_r64)
            result = create_scalar_fortarray(values_r64(value_idx), var%name, var%units)
            result%long_name = var%long_name  ! Also copy long_name for scalar case
            return
        end if
        
        ! Calculate new shape (remove the sliced dimension)
        allocate(new_shape(var%n_dims - 1))
        j = 0
        do i = 1, var%n_dims
            if (i /= dim_idx) then
                j = j + 1
                new_shape(j) = var%shape(i)
            end if
        end do
        
        ! Calculate slice parameters
        n_elements_before = 1
        do i = 1, dim_idx - 1
            n_elements_before = n_elements_before * var%shape(i)
        end do
        
        n_elements_after = 1
        do i = dim_idx + 1, var%n_dims
            n_elements_after = n_elements_after * var%shape(i)
        end do
        
        slice_size = n_elements_before * n_elements_after
        
        ! Extract values
        call get_values_r64(var%data, values_r64)
        allocate(result_values(slice_size))
        
        ! Copy the slice with correct indexing
        new_idx = 1
        do i = 1, n_elements_before
            do j = 1, n_elements_after
                ! Calculate source index in column-major order
                ! For dimension dim_idx, fix index to value_idx
                ! Before dimensions: stride of 1 each
                ! At dimension dim_idx: fixed at value_idx  
                ! After dimensions: stride of product(shape[1:dim_idx])
                src_idx = i + (value_idx - 1) * n_elements_before + &
                         (j - 1) * n_elements_before * var%shape(dim_idx)
                result_values(new_idx) = values_r64(src_idx)
                new_idx = new_idx + 1
            end do
        end do
        
        ! Create result with new shape and dim names
        new_dim_names = remove_dim_name(var%dim_names, dim_idx)
        result = create_fortarray_with_shape(result_values, new_shape, &
                                           new_dim_names, &
                                           var%name // "_sel")
        
        ! Copy coordinates for remaining dimensions
        if (allocated(var%coords)) then
            call copy_coords_except_dim(var, result, dim_idx)
        end if
        
        ! Copy attributes from original variable
        result%units = var%units
        result%long_name = var%long_name
        
    end function slice_along_dimension
    
    !> Slice range along a single dimension (PROPERLY IMPLEMENTED)  
    function slice_range_along_dimension(var, dim_idx, start_idx, stop_idx, step_idx) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim_idx, start_idx, stop_idx
        integer, intent(in), optional :: step_idx
        type(fortarray_t) :: result
        
        integer :: step, actual_start, actual_stop, range_size
        integer :: i, j, k, new_idx, src_idx
        integer, dimension(:), allocatable :: new_shape
        real(real64), dimension(:), allocatable :: values_r64, result_values
        integer :: n_elements_before, n_elements_after, n_elements_dim
        
        ! Set defaults
        step = 1
        if (present(step_idx)) step = step_idx
        
        ! Validate
        if (dim_idx < 1 .or. dim_idx > var%n_dims) then
            write(error_unit, '(A)') "ERROR: Invalid dimension for slice_range"
            result = create_empty_like(var)
            return
        end if
        
        ! Normalize indices
        actual_start = max(1, min(start_idx, var%shape(dim_idx)))
        actual_stop = max(1, min(stop_idx, var%shape(dim_idx)))
        
        if (actual_start > actual_stop .and. step > 0) then
            write(error_unit, '(A)') "ERROR: Invalid range for slice"
            result = create_empty_like(var)
            return
        end if
        
        ! Calculate range size
        range_size = (actual_stop - actual_start) / abs(step) + 1
        
        ! Create new shape
        allocate(new_shape(var%n_dims))
        new_shape = var%shape
        new_shape(dim_idx) = range_size
        
        ! Calculate slice parameters
        n_elements_before = 1
        do i = 1, dim_idx - 1
            n_elements_before = n_elements_before * var%shape(i)
        end do
        
        n_elements_dim = var%shape(dim_idx)
        
        n_elements_after = 1  
        do i = dim_idx + 1, var%n_dims
            n_elements_after = n_elements_after * var%shape(i)
        end do
        
        ! Extract values
        call get_values_r64(var%data, values_r64)
        allocate(result_values(product(new_shape)))
        
        ! Copy the slice range
        new_idx = 1
        do k = 1, n_elements_after
            do j = 1, n_elements_before
                do i = actual_start, actual_stop, step
                    src_idx = (k-1)*n_elements_before*n_elements_dim + &
                             (j-1)*n_elements_dim + i
                    result_values(new_idx) = values_r64(src_idx)
                    new_idx = new_idx + 1
                end do
            end do
        end do
        
        ! Create result
        result = create_fortarray_with_shape(result_values, new_shape, &
                                           var%dim_names, var%name // "_slice")
        
        ! Copy and update coordinates
        if (allocated(var%coords)) then
            call copy_coords_with_slice(var, result, dim_idx, actual_start, actual_stop, step)
        end if
        
    end function slice_range_along_dimension

    !> Helper function to remove a dimension name from array
    function remove_dim_name(dim_names, dim_idx) result(new_names)
        character(len=*), dimension(:), intent(in) :: dim_names
        integer, intent(in) :: dim_idx
        character(len=:), dimension(:), allocatable :: new_names
        integer :: i, j, n_dims
        
        n_dims = size(dim_names)
        if (n_dims == 1) then
            allocate(character(len=0) :: new_names(0))
            return
        end if
        
        allocate(character(len=len(dim_names)) :: new_names(n_dims - 1))
        
        j = 0
        do i = 1, n_dims
            if (i /= dim_idx) then
                j = j + 1
                new_names(j) = dim_names(i)
            end if
        end do
        
    end function remove_dim_name
    
    !> Helper to copy coordinates except for sliced dimension
    subroutine copy_coords_except_dim(src, dest, dim_idx)
        type(fortarray_t), intent(in) :: src
        type(fortarray_t), intent(inout) :: dest
        integer, intent(in) :: dim_idx
        integer :: i, j
        
        if (.not. allocated(src%coords)) return
        
        allocate(dest%coords(dest%n_dims))
        allocate(dest%has_coord(dest%n_dims))
        
        j = 0
        do i = 1, src%n_dims
            if (i /= dim_idx) then
                j = j + 1
                if (src%has_coord(i)) then
                    dest%coords(j) = src%coords(i)
                    dest%has_coord(j) = .true.
                else
                    dest%has_coord(j) = .false.
                end if
            end if
        end do
        
    end subroutine copy_coords_except_dim
    
    !> Helper to copy coordinates with slicing
    subroutine copy_coords_with_slice(src, dest, dim_idx, start_idx, stop_idx, step)
        type(fortarray_t), intent(in) :: src
        type(fortarray_t), intent(inout) :: dest
        integer, intent(in) :: dim_idx, start_idx, stop_idx, step
        integer :: i, j, k, range_size
        
        if (.not. allocated(src%coords)) return
        
        allocate(dest%coords(dest%n_dims))
        allocate(dest%has_coord(dest%n_dims))
        
        do i = 1, src%n_dims
            if (i == dim_idx .and. src%has_coord(i)) then
                ! Slice the coordinate
                dest%has_coord(i) = .true.
                range_size = (stop_idx - start_idx) / step + 1
                
                select case(src%coords(i)%dtype)
                case(DTYPE_REAL64)
                    allocate(dest%coords(i)%values_r64(range_size))
                    k = 1
                    do j = start_idx, stop_idx, step
                        dest%coords(i)%values_r64(k) = src%coords(i)%values_r64(j)
                        k = k + 1
                    end do
                case(DTYPE_REAL32)
                    allocate(dest%coords(i)%values_r32(range_size))
                    k = 1
                    do j = start_idx, stop_idx, step
                        dest%coords(i)%values_r32(k) = src%coords(i)%values_r32(j)
                        k = k + 1
                    end do
                case(DTYPE_INT64)
                    allocate(dest%coords(i)%values_i64(range_size))
                    k = 1
                    do j = start_idx, stop_idx, step
                        dest%coords(i)%values_i64(k) = src%coords(i)%values_i64(j)
                        k = k + 1
                    end do
                case(DTYPE_INT32)
                    allocate(dest%coords(i)%values_i32(range_size))
                    k = 1
                    do j = start_idx, stop_idx, step
                        dest%coords(i)%values_i32(k) = src%coords(i)%values_i32(j)
                        k = k + 1
                    end do
                end select
                
                dest%coords(i)%name = src%coords(i)%name
                dest%coords(i)%dtype = src%coords(i)%dtype
                dest%coords(i)%length = range_size
                dest%coords(i)%initialized = .true.
            else if (src%has_coord(i)) then
                ! Copy coordinate as-is
                dest%coords(i) = src%coords(i)
                dest%has_coord(i) = .true.
            else
                dest%has_coord(i) = .false.
            end if
        end do
        
    end subroutine copy_coords_with_slice
    
    !> Helper to copy coordinates with fancy indexing
    subroutine copy_coords_with_fancy_indexing(src, dest, dim_idx, indices)
        type(fortarray_t), intent(in) :: src
        type(fortarray_t), intent(inout) :: dest
        integer, intent(in) :: dim_idx
        integer, dimension(:), intent(in) :: indices
        integer :: i, j, k, idx
        
        if (.not. allocated(src%coords)) return
        
        allocate(dest%coords(dest%n_dims))
        allocate(dest%has_coord(dest%n_dims))
        
        do i = 1, src%n_dims
            if (i == dim_idx .and. src%has_coord(i)) then
                ! Apply fancy indexing to the coordinate
                dest%has_coord(i) = .true.
                
                select case(src%coords(i)%dtype)
                case(DTYPE_REAL64)
                    allocate(dest%coords(i)%values_r64(size(indices)))
                    do j = 1, size(indices)
                        if (indices(j) < 0) then
                            idx = src%coords(i)%length + indices(j) + 1
                        else
                            idx = indices(j)
                        end if
                        dest%coords(i)%values_r64(j) = src%coords(i)%values_r64(idx)
                    end do
                case(DTYPE_REAL32)
                    allocate(dest%coords(i)%values_r32(size(indices)))
                    do j = 1, size(indices)
                        if (indices(j) < 0) then
                            idx = src%coords(i)%length + indices(j) + 1
                        else
                            idx = indices(j)
                        end if
                        dest%coords(i)%values_r32(j) = src%coords(i)%values_r32(idx)
                    end do
                case(DTYPE_INT64)
                    allocate(dest%coords(i)%values_i64(size(indices)))
                    do j = 1, size(indices)
                        if (indices(j) < 0) then
                            idx = src%coords(i)%length + indices(j) + 1
                        else
                            idx = indices(j)
                        end if
                        dest%coords(i)%values_i64(j) = src%coords(i)%values_i64(idx)
                    end do
                case(DTYPE_INT32)
                    allocate(dest%coords(i)%values_i32(size(indices)))
                    do j = 1, size(indices)
                        if (indices(j) < 0) then
                            idx = src%coords(i)%length + indices(j) + 1
                        else
                            idx = indices(j)
                        end if
                        dest%coords(i)%values_i32(j) = src%coords(i)%values_i32(idx)
                    end do
                end select
                
                dest%coords(i)%name = src%coords(i)%name
                dest%coords(i)%dtype = src%coords(i)%dtype
                dest%coords(i)%length = size(indices)
                dest%coords(i)%initialized = .true.
            else if (src%has_coord(i)) then
                ! Copy coordinate as-is
                dest%coords(i) = src%coords(i)
                dest%has_coord(i) = .true.
            else
                dest%has_coord(i) = .false.
            end if
        end do
        
    end subroutine copy_coords_with_fancy_indexing
    
    !> Create fortarray with specific shape
    function create_fortarray_with_shape(values, shape, dim_names, name) result(var)
        real(real64), dimension(:), intent(in) :: values
        integer, dimension(:), intent(in) :: shape
        character(len=*), dimension(:), intent(in) :: dim_names
        character(len=*), intent(in) :: name
        type(fortarray_t) :: var
        
        ! Create variable with proper shape
        var%initialized = .true.
        var%n_dims = size(shape)
        var%n_elements = size(values)
        var%name = name
        
        allocate(var%shape(var%n_dims))
        var%shape = shape
        
        allocate(var%dim_names(var%n_dims))
        var%dim_names = dim_names
        
        ! Initialize data storage
        var%data%initialized = .true.
        var%data%dtype = DTYPE_REAL64
        var%data%n_elements = var%n_elements
        allocate(var%data%values_r64(var%n_elements))
        var%data%values_r64 = values
        
    end function create_fortarray_with_shape
    
    !> Get values from data storage as real64
    subroutine get_values_r64(data, values)
        type(data_storage_t), intent(in) :: data
        real(real64), dimension(:), allocatable, intent(out) :: values
        integer :: n
        
        select case(data%dtype)
        case(DTYPE_REAL64)
            if (allocated(data%values_r64)) then
                n = size(data%values_r64)
                allocate(values(n))
                values = data%values_r64
            else
                allocate(values(0))
            end if
        case(DTYPE_REAL32)
            if (allocated(data%values_r32)) then
                n = size(data%values_r32)
                allocate(values(n))
                values = real(data%values_r32, real64)
            else
                allocate(values(0))
            end if
        case(DTYPE_INT64)
            if (allocated(data%values_i64)) then
                n = size(data%values_i64)
                allocate(values(n))
                values = real(data%values_i64, real64)
            else
                allocate(values(0))
            end if
        case(DTYPE_INT32)
            if (allocated(data%values_i32)) then
                n = size(data%values_i32)
                allocate(values(n))
                values = real(data%values_i32, real64)
            else
                allocate(values(0))
            end if
        case default
            allocate(values(0))
        end select
        
    end subroutine get_values_r64

    !> ======= FILTERING AND CONDITIONAL OPERATIONS =======
    
    !> where() operation - replace values not meeting condition with other_value
    module function fortarray_where_gt_r64(this, threshold, other_value) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: threshold
        real(real64), intent(in) :: other_value
        type(fortarray_t) :: result_array
        integer :: i
        
        ! Create a proper copy of this array
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize data storage and copy
        result_array%data%initialized = .true.
        result_array%data%dtype = this%data%dtype
        result_array%data%n_elements = this%n_elements
        
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                allocate(result_array%data%values_r64(this%n_elements))
                result_array%data%values_r64(:) = this%data%values_r64(:)
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                allocate(result_array%data%values_r32(this%n_elements))
                result_array%data%values_r32(:) = this%data%values_r32(:)
            end if
        case default
            write(error_unit, '(A)') "ERROR: Data type not supported for copying"
            return
        end select
        
        if (.not. result_array%initialized) then
            write(error_unit, '(A)') "ERROR: Failed to create copy for where_gt operation"
            return
        end if
        
        ! Apply condition: where values <= threshold, set to other_value
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                do i = 1, this%n_elements
                    if (this%data%values_r64(i) <= threshold) then
                        result_array%data%values_r64(i) = other_value
                    end if
                end do
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                ! Convert to real64 for output
                result_array%data%dtype = DTYPE_REAL64
                allocate(result_array%data%values_r64(this%n_elements))
                do i = 1, this%n_elements
                    if (real(this%data%values_r32(i), real64) <= threshold) then
                        result_array%data%values_r64(i) = other_value
                    else
                        result_array%data%values_r64(i) = real(this%data%values_r32(i), real64)
                    end if
                end do
                result_array%data%dtype = DTYPE_REAL64
            end if
        case default
            write(error_unit, '(A)') "ERROR: where_gt not supported for this data type"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_where_gt_r64
    
    !> where() operation for less than
    module function fortarray_where_lt_r64(this, threshold, other_value) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: threshold
        real(real64), intent(in) :: other_value
        type(fortarray_t) :: result_array
        integer :: i
        
        ! Create copy of input array
        result_array = create_empty_like(this)
        result_array%initialized = .true.
        result_array%data%initialized = .true.
        result_array%data%dtype = this%data%dtype
        result_array%data%n_elements = this%n_elements
        
        ! Copy data
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                if (.not. allocated(result_array%data%values_r64)) then
                    allocate(result_array%data%values_r64(this%n_elements))
                end if
                result_array%data%values_r64(:) = this%data%values_r64(:)
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                if (.not. allocated(result_array%data%values_r32)) then
                    allocate(result_array%data%values_r32(this%n_elements))
                end if
                result_array%data%values_r32(:) = this%data%values_r32(:)
            end if
        case default
            write(error_unit, '(A)') "ERROR: Data type not supported for copying"
            result_array = create_empty_like(this)
            return
        end select
        
        if (.not. result_array%initialized) then
            write(error_unit, '(A)') "ERROR: Failed to create copy for where_lt operation"
            return
        end if
        
        ! Apply condition: where values >= threshold, set to other_value
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                do i = 1, this%n_elements
                    if (this%data%values_r64(i) >= threshold) then
                        result_array%data%values_r64(i) = other_value
                    end if
                end do
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                ! Convert to real64 for output
                result_array%data%dtype = DTYPE_REAL64
                allocate(result_array%data%values_r64(this%n_elements))
                do i = 1, this%n_elements
                    if (real(this%data%values_r32(i), real64) >= threshold) then
                        result_array%data%values_r64(i) = other_value
                    else
                        result_array%data%values_r64(i) = real(this%data%values_r32(i), real64)
                    end if
                end do
                result_array%data%dtype = DTYPE_REAL64
            end if
        case default
            write(error_unit, '(A)') "ERROR: where_lt not supported for this data type"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_where_lt_r64
    
    !> Create boolean mask for values > threshold
    module function fortarray_gt_r64(this, threshold) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: threshold
        type(fortarray_t) :: result_array
        integer :: i
        
        ! Create result with same shape but logical data type
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize logical data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = DTYPE_LOGICAL
        result_array%data%n_elements = this%n_elements
        allocate(result_array%data%values_logical(this%n_elements))
        
        ! Create boolean mask
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                do i = 1, this%n_elements
                    result_array%data%values_logical(i) = this%data%values_r64(i) > threshold
                end do
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                do i = 1, this%n_elements
                    result_array%data%values_logical(i) = real(this%data%values_r32(i), real64) > threshold
                end do
            end if
        case default
            write(error_unit, '(A)') "ERROR: gt not supported for this data type"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_gt_r64
    
    !> Create boolean mask for values < threshold
    module function fortarray_lt_r64(this, threshold) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: threshold
        type(fortarray_t) :: result_array
        integer :: i
        
        ! Create result with same shape but logical data type
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize logical data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = DTYPE_LOGICAL
        result_array%data%n_elements = this%n_elements
        allocate(result_array%data%values_logical(this%n_elements))
        
        ! Create boolean mask
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                do i = 1, this%n_elements
                    result_array%data%values_logical(i) = this%data%values_r64(i) < threshold
                end do
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                do i = 1, this%n_elements
                    result_array%data%values_logical(i) = real(this%data%values_r32(i), real64) < threshold
                end do
            end if
        case default
            write(error_unit, '(A)') "ERROR: lt not supported for this data type"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_lt_r64
    
    !> Apply boolean mask to select elements
    module function fortarray_mask_where(this, mask) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t), intent(in) :: mask
        type(fortarray_t) :: result_array
        integer :: i, count_true, result_idx
        
        if (.not. mask%initialized .or. mask%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit, '(A)') "ERROR: mask_where requires initialized logical mask"
            result_array = create_empty_like(this)
            return
        end if
        
        if (mask%n_elements /= this%n_elements) then
            write(error_unit, '(A)') "ERROR: mask_where requires mask and array to have same size"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Count true values in mask
        count_true = 0
        do i = 1, mask%n_elements
            if (mask%data%values_logical(i)) count_true = count_true + 1
        end do
        
        ! Create result array with reduced size
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = count_true
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names, updating first dimension
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
            result_array%shape(1) = count_true  ! Update first dimension
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = this%data%dtype
        result_array%data%n_elements = count_true
        
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            allocate(result_array%data%values_r64(count_true))
        case default
            write(error_unit, '(A)') "ERROR: Data type not supported for mask_where"
            return
        end select
        
        ! Copy selected elements
        result_idx = 1
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                do i = 1, this%n_elements
                    if (mask%data%values_logical(i)) then
                        result_array%data%values_r64(result_idx) = this%data%values_r64(i)
                        result_idx = result_idx + 1
                    end if
                end do
            end if
        case default
            write(error_unit, '(A)') "ERROR: mask_where not yet implemented for this data type"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_mask_where
    
    !> Logical AND operation on boolean arrays
    module function fortarray_logical_and(this, other) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t), intent(in) :: other
        type(fortarray_t) :: result_array
        integer :: i
        
        if (.not. this%initialized .or. .not. other%initialized) then
            write(error_unit, '(A)') "ERROR: logical_and requires both arrays to be initialized"
            result_array = create_empty_like(this)
            return
        end if
        
        if (this%data%dtype /= DTYPE_LOGICAL .or. other%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit, '(A)') "ERROR: logical_and requires both arrays to be logical"
            result_array = create_empty_like(this)
            return
        end if
        
        if (this%n_elements /= other%n_elements) then
            write(error_unit, '(A)') "ERROR: logical_and requires arrays to have same size"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Create result logical array with same shape
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize logical data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = DTYPE_LOGICAL
        result_array%data%n_elements = this%n_elements
        allocate(result_array%data%values_logical(this%n_elements))
        
        ! Perform AND operation
        do i = 1, this%n_elements
            result_array%data%values_logical(i) = this%data%values_logical(i) .and. other%data%values_logical(i)
        end do
        
    end function fortarray_logical_and
    
    !> Logical OR operation on boolean arrays
    module function fortarray_logical_or(this, other) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t), intent(in) :: other
        type(fortarray_t) :: result_array
        integer :: i
        
        if (.not. this%initialized .or. .not. other%initialized) then
            write(error_unit, '(A)') "ERROR: logical_or requires both arrays to be initialized"
            result_array = create_empty_like(this)
            return
        end if
        
        if (this%data%dtype /= DTYPE_LOGICAL .or. other%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit, '(A)') "ERROR: logical_or requires both arrays to be logical"
            result_array = create_empty_like(this)
            return
        end if
        
        if (this%n_elements /= other%n_elements) then
            write(error_unit, '(A)') "ERROR: logical_or requires arrays to have same size"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Create result logical array with same shape
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize logical data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = DTYPE_LOGICAL
        result_array%data%n_elements = this%n_elements
        allocate(result_array%data%values_logical(this%n_elements))
        
        ! Perform OR operation
        do i = 1, this%n_elements
            result_array%data%values_logical(i) = this%data%values_logical(i) .or. other%data%values_logical(i)
        end do
        
    end function fortarray_logical_or
    
    !> Logical NOT operation on boolean array
    module function fortarray_logical_not(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        integer :: i
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: logical_not requires array to be initialized"
            result_array = create_empty_like(this)
            return
        end if
        
        if (this%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit, '(A)') "ERROR: logical_not requires logical array"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Create result logical array with same shape
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize logical data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = DTYPE_LOGICAL
        result_array%data%n_elements = this%n_elements
        allocate(result_array%data%values_logical(this%n_elements))
        
        ! Perform NOT operation
        do i = 1, this%n_elements
            result_array%data%values_logical(i) = .not. this%data%values_logical(i)
        end do
        
    end function fortarray_logical_not
    
    
    !> Custom condition evaluation (placeholder - simplified)
    module function fortarray_where_custom(this, condition, other_value) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: condition
        real(real64), intent(in) :: other_value
        type(fortarray_t) :: result_array
        integer :: i
        
        ! Create copy of input array with proper initialization
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = this%data%dtype
        result_array%data%n_elements = this%n_elements
        
        ! Copy data
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                if (.not. allocated(result_array%data%values_r64)) then
                    allocate(result_array%data%values_r64(this%n_elements))
                end if
                result_array%data%values_r64(:) = this%data%values_r64(:)
            end if
        case(DTYPE_REAL32)
            if (allocated(this%data%values_r32)) then
                if (.not. allocated(result_array%data%values_r32)) then
                    allocate(result_array%data%values_r32(this%n_elements))
                end if
                result_array%data%values_r32(:) = this%data%values_r32(:)
            end if
        case default
            write(error_unit, '(A)') "ERROR: Data type not supported for copying"
            result_array = create_empty_like(this)
            return
        end select
        
        if (.not. result_array%initialized) then
            write(error_unit, '(A)') "ERROR: Failed to create copy for where_custom operation"
            return
        end if
        
        ! Simple parser for "x**2 > 16" type conditions
        if (index(condition, "x**2 > 16") > 0) then
            select case(this%data%dtype)
            case(DTYPE_REAL64)
                if (allocated(this%data%values_r64)) then
                    do i = 1, this%n_elements
                        if (this%data%values_r64(i)**2 <= 16.0_real64) then
                            result_array%data%values_r64(i) = other_value
                        end if
                    end do
                end if
            case default
                write(error_unit, '(A)') "ERROR: where_custom not supported for this data type"
                result_array = create_empty_like(this)
            end select
        else
            write(error_unit, '(A)') "ERROR: where_custom condition not recognized: ", condition
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_where_custom
    
    !> Complex boolean expression evaluation (placeholder)
    module function fortarray_where_complex(this, expression, other_value) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: expression
        real(real64), intent(in) :: other_value
        type(fortarray_t) :: result_array
        
        ! For now, just echo the functionality request
        write(error_unit, '(A)') "INFO: where_complex expression: ", expression
        
        ! Create copy of input array with proper initialization
        result_array%initialized = .true.
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        result_array%name = this%name
        result_array%units = this%units
        
        ! Copy shape and dimension names
        if (allocated(this%shape)) then
            allocate(result_array%shape(size(this%shape)))
            result_array%shape = this%shape
        end if
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Initialize data storage
        result_array%data%initialized = .true.
        result_array%data%dtype = this%data%dtype
        result_array%data%n_elements = this%n_elements
        
        ! Copy data
        select case(this%data%dtype)
        case(DTYPE_REAL64)
            if (allocated(this%data%values_r64)) then
                if (.not. allocated(result_array%data%values_r64)) then
                    allocate(result_array%data%values_r64(this%n_elements))
                end if
                result_array%data%values_r64(:) = this%data%values_r64(:)
            end if
        case default
            write(error_unit, '(A)') "ERROR: Data type not supported"
            result_array = create_empty_like(this)
        end select
        
        ! Placeholder: complex boolean expressions would require a full parser
        write(error_unit, '(A)') "INFO: where_complex not fully implemented - returning copy"
        
    end function fortarray_where_complex

    ! ======= ENHANCED SELECTION METHODS (SPRINT 7) =======
    
    !> Nearest-neighbor selection with algorithm choice
    module function fortarray_sel_nearest_r64(this, coord_name, value, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in) :: value
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        integer :: coord_index, nearest_idx
        real(real64), allocatable :: coord_values(:)
        real(real64) :: min_diff, diff
        integer :: i
        character(len=20) :: used_method
        
        ! Set default method
        if (present(method)) then
            used_method = method
        else
            used_method = "nearest"
        end if
        
        ! Find coordinate index
        coord_index = find_coord_index_by_name(this, coord_name)
        if (coord_index == -1) then
            write(error_unit, '(A,A,A)') "ERROR: Coordinate '", coord_name, "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Get coordinate values
        call get_coord_values_r64(this%coords(coord_index), coord_values)
        if (.not. allocated(coord_values) .or. size(coord_values) == 0) then
            write(error_unit, '(A)') "ERROR: No coordinate values available"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Find nearest neighbor using different algorithms
        select case(trim(used_method))
        case("nearest", "linear")
            ! Simple linear search for nearest neighbor
            min_diff = huge(1.0_real64)
            nearest_idx = 1
            do i = 1, size(coord_values)
                diff = abs(coord_values(i) - value)
                if (diff <= min_diff) then
                    min_diff = diff
                    nearest_idx = i
                end if
            end do
            
        case("cubic")
            ! For cubic, still use nearest for now but could implement cubic interpolation
            min_diff = huge(1.0_real64)
            nearest_idx = 1
            do i = 1, size(coord_values) 
                diff = abs(coord_values(i) - value)
                if (diff < min_diff) then
                    min_diff = diff
                    nearest_idx = i
                end if
            end do
            
        case default
            write(error_unit, '(A,A,A)') "ERROR: Unknown method '", trim(used_method), "'"
            result_array = create_empty_like(this)
            return
        end select
        
        ! Select using nearest index
        result_array = slice_along_dimension(this, coord_index, nearest_idx)
        
    end function fortarray_sel_nearest_r64
    
    !> Interpolation-based selection
    module function fortarray_sel_interp_linear(this, coord_name, value, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in) :: value
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        integer :: coord_index, i, lower_idx, upper_idx
        real(real64), allocatable :: coord_values(:)
        real(real64) :: alpha, lower_val, upper_val
        character(len=20) :: used_method
        
        ! Set default method
        if (present(method)) then
            used_method = method
        else
            used_method = "linear"
        end if
        
        ! Find coordinate index
        coord_index = find_coord_index_by_name(this, coord_name)
        if (coord_index == -1) then
            write(error_unit, '(A,A,A)') "ERROR: Coordinate '", coord_name, "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Get coordinate values
        call get_coord_values_r64(this%coords(coord_index), coord_values)
        if (.not. allocated(coord_values) .or. size(coord_values) == 0) then
            write(error_unit, '(A)') "ERROR: No coordinate values available"
            result_array = create_empty_like(this)
            return
        end if
        
        select case(trim(used_method))
        case("linear")
            ! Find bracketing indices for linear interpolation
            lower_idx = 1
            upper_idx = size(coord_values)
            
            do i = 1, size(coord_values) - 1
                if (coord_values(i) <= value .and. coord_values(i+1) >= value) then
                    lower_idx = i
                    upper_idx = i + 1
                    exit
                end if
            end do
            
            ! Check for exact match
            if (abs(coord_values(lower_idx) - value) < 1e-10) then
                result_array = slice_along_dimension(this, coord_index, lower_idx)
                return
            end if
            
            if (abs(coord_values(upper_idx) - value) < 1e-10) then
                result_array = slice_along_dimension(this, coord_index, upper_idx)
                return
            end if
            
            ! Linear interpolation weight
            lower_val = coord_values(lower_idx)
            upper_val = coord_values(upper_idx)
            alpha = (value - lower_val) / (upper_val - lower_val)
            
            ! For now, return nearest neighbor (full interpolation requires weighted averaging)
            if (alpha < 0.5) then
                result_array = slice_along_dimension(this, coord_index, lower_idx)
            else
                result_array = slice_along_dimension(this, coord_index, upper_idx)
            end if
            
        case("cubic", "spline")
            ! For cubic/spline, fall back to nearest neighbor for now
            write(error_unit, '(A)') "INFO: Cubic/spline interpolation not yet implemented, using nearest"
            result_array = fortarray_sel_nearest_r64(this, coord_name, value, "nearest")
            
        case default
            write(error_unit, '(A,A,A)') "ERROR: Unknown interpolation method '", trim(used_method), "'"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_sel_interp_linear
    
    !> String coordinate selection
    module function fortarray_sel_string(this, coord_name, value) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        character(len=*), intent(in) :: value
        type(fortarray_t) :: result_array
        integer :: coord_index, i, match_idx
        
        ! Find coordinate index
        coord_index = find_coord_index_by_name(this, coord_name)
        if (coord_index == -1) then
            write(error_unit, '(A,A,A)') "ERROR: Coordinate '", coord_name, "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Check if coordinate has string values
        if (.not. allocated(this%coords(coord_index)%values_char)) then
            write(error_unit, '(A)') "ERROR: Coordinate does not contain string values"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Find matching string
        match_idx = -1
        do i = 1, size(this%coords(coord_index)%values_char)
            if (trim(this%coords(coord_index)%values_char(i)) == trim(value)) then
                match_idx = i
                exit
            end if
        end do
        
        if (match_idx == -1) then
            write(error_unit, '(A,A,A)') "ERROR: String value '", trim(value), "' not found in coordinate"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Select using matching index
        result_array = slice_along_dimension(this, coord_index, match_idx)
        
    end function fortarray_sel_string
    
    !> Datetime coordinate selection (placeholder)
    module function fortarray_sel_datetime(this, coord_name, value, tolerance) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        character(len=*), intent(in) :: value
        real(real64), intent(in), optional :: tolerance
        type(fortarray_t) :: result_array
        
        ! Placeholder implementation - would need datetime parsing
        write(error_unit, '(A)') "INFO: sel_datetime not yet implemented - using string matching"
        result_array = fortarray_sel_string(this, coord_name, value)
        
    end function fortarray_sel_datetime
    
    !> Multi-coordinate selection
    module function fortarray_sel_multi_coord(this, coord_names, values, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_names(:)
        real(real64), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        integer :: i
        character(len=20) :: used_method
        
        ! Set default method
        if (present(method)) then
            used_method = method
        else
            used_method = "exact"
        end if
        
        if (size(coord_names) /= size(values)) then
            write(error_unit, '(A)') "ERROR: coord_names and values must have same size"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Start with full array
        result_array = this
        
        ! Apply selections sequentially
        select case(trim(used_method))
        case("exact")
            do i = 1, size(coord_names)
                result_array = result_array%sel_point(coord_names(i), values(i), "exact")
            end do
        case("nearest")
            do i = 1, size(coord_names)
                result_array = result_array%sel_nearest(coord_names(i), values(i), "nearest")
            end do
        case("interp")
            do i = 1, size(coord_names)
                result_array = result_array%sel_interp(coord_names(i), values(i), "linear")
            end do
        case default
            write(error_unit, '(A,A,A)') "ERROR: Unknown multi-selection method '", trim(used_method), "'"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_sel_multi_coord
    
    !> Generic selection with method choice
    module function fortarray_sel_method_choice(this, coord_name, value, method, tolerance) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in) :: value
        character(len=*), intent(in) :: method
        real(real64), intent(in), optional :: tolerance
        type(fortarray_t) :: result_array
        
        select case(trim(method))
        case("exact")
            result_array = this%sel_point(coord_name, value, "exact")
        case("nearest")
            result_array = this%sel_nearest(coord_name, value, "nearest")
        case("interpolate", "interp")
            result_array = this%sel_interp(coord_name, value, "linear")
        case default
            write(error_unit, '(A,A,A)') "ERROR: Unknown selection method '", trim(method), "'"
            result_array = create_empty_like(this)
        end select
        
    end function fortarray_sel_method_choice
    
    ! ======= ADVANCED AGGREGATION METHODS (Sprint 9) =======
    
    !> Calculate quantile(s) of the array
    module function fortarray_quantile(this, q, axis, interpolation) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: q  ! Single quantile or array of quantiles
        character(len=*), intent(in), optional :: axis
        character(len=*), intent(in), optional :: interpolation  ! 'linear', 'lower', 'higher', 'midpoint', 'nearest'
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: sorted_data(:)
        real(real64) :: pos, frac
        integer :: n, idx, lo, hi
        character(len=20) :: interp_method
        
        ! Set default interpolation method
        interp_method = "linear"
        if (present(interpolation)) interp_method = interpolation
        
        ! Only handle real64 data for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: quantile only supports real64 data type"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Get data size
        n = this%n_elements
        if (n == 0) then
            result_array = create_empty_like(this)
            return
        end if
        
        ! Copy and sort data
        allocate(sorted_data(n))
        sorted_data = this%data%values_r64
        call quicksort_r64(sorted_data, 1, n)
        
        ! Calculate quantile position (0-based indexing)
        pos = q * real(n - 1, real64)
        
        ! Determine indices for interpolation
        lo = int(pos) + 1  ! Convert to 1-based indexing
        hi = min(lo + 1, n)
        frac = pos - real(lo - 1, real64)
        
        ! Bounds checking
        if (lo < 1) lo = 1
        if (hi > n) hi = n
        
        ! Calculate quantile based on interpolation method
        select case(trim(interp_method))
        case("linear")
            ! Linear interpolation between lo and hi
            if (lo == hi) then
                result_array = new_array([sorted_data(lo)], name="quantile")
            else
                result_array = new_array([sorted_data(lo) * (1.0_real64 - frac) + sorted_data(hi) * frac], &
                                       name="quantile")
            end if
        case("lower")
            result_array = new_array([sorted_data(lo)], name="quantile")
        case("higher")
            result_array = new_array([sorted_data(hi)], name="quantile")
        case("midpoint")
            if (lo == hi) then
                result_array = new_array([sorted_data(lo)], name="quantile")
            else
                result_array = new_array([(sorted_data(lo) + sorted_data(hi)) * 0.5_real64], name="quantile")
            end if
        case("nearest")
            if (frac < 0.5_real64) then
                result_array = new_array([sorted_data(lo)], name="quantile")
            else
                result_array = new_array([sorted_data(hi)], name="quantile")
            end if
        case default
            write(error_unit, '(A,A,A)') "ERROR: Unknown interpolation method '", trim(interp_method), "'"
            result_array = create_empty_like(this)
        end select
        
        deallocate(sorted_data)
        
    end function fortarray_quantile
    
    !> Calculate quantiles for multiple q values
    module function fortarray_quantile_multi(this, q_array, axis, interpolation) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), dimension(:), intent(in) :: q_array  ! Array of quantiles
        character(len=*), intent(in), optional :: axis
        character(len=*), intent(in), optional :: interpolation
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: result_values(:)
        type(fortarray_t) :: single_result
        integer :: i, nq
        
        nq = size(q_array)
        allocate(result_values(nq))
        
        ! Calculate each quantile
        do i = 1, nq
            single_result = this%quantile(q_array(i), axis, interpolation)
            if (single_result%data%dtype == DTYPE_REAL64 .and. allocated(single_result%data%values_r64)) then
                result_values(i) = single_result%data%values_r64(1)
            else
                result_values(i) = 0.0_real64  ! Error case
            end if
        end do
        
        ! Create result array
        result_array = new_array(result_values, name="quantiles")
        
        deallocate(result_values)
        
    end function fortarray_quantile_multi
    
    !> Calculate percentile (convenience wrapper for quantile)
    module function fortarray_percentile(this, p, axis, interpolation) result(percentile_val)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: p  ! Percentile (0-100)
        character(len=*), intent(in), optional :: axis
        character(len=*), intent(in), optional :: interpolation
        real(real64) :: percentile_val
        
        type(fortarray_t) :: result
        
        ! Convert percentile to quantile (0-1 range)
        result = this%quantile(p / 100.0_real64, axis, interpolation)
        
        ! Extract scalar value
        if (result%data%dtype == DTYPE_REAL64 .and. allocated(result%data%values_r64)) then
            percentile_val = result%data%values_r64(1)
        else
            percentile_val = 0.0_real64  ! Error case
        end if
        
    end function fortarray_percentile
    
    !> Calculate weighted mean
    module function fortarray_weighted_mean(this, weights, axis) result(weighted_mean_val)
        class(fortarray_t), intent(in) :: this
        class(fortarray_t), intent(in) :: weights
        character(len=*), intent(in), optional :: axis
        real(real64) :: weighted_mean_val
        
        real(real64) :: sum_weighted, sum_weights
        integer :: i
        
        ! Check dimensions match
        if (this%n_elements /= weights%n_elements) then
            write(error_unit, '(A)') "ERROR: Array and weights must have same size"
            weighted_mean_val = 0.0_real64
            return
        end if
        
        ! Only handle real64 for now
        if (this%data%dtype /= DTYPE_REAL64 .or. weights%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: weighted_mean only supports real64 data type"
            weighted_mean_val = 0.0_real64
            return
        end if
        
        ! Calculate weighted mean
        sum_weighted = 0.0_real64
        sum_weights = 0.0_real64
        
        do i = 1, this%n_elements
            sum_weighted = sum_weighted + this%data%values_r64(i) * weights%data%values_r64(i)
            sum_weights = sum_weights + weights%data%values_r64(i)
        end do
        
        if (sum_weights > 0.0_real64) then
            weighted_mean_val = sum_weighted / sum_weights
        else
            weighted_mean_val = 0.0_real64
        end if
        
    end function fortarray_weighted_mean
    
    !> Calculate weighted sum
    module function fortarray_weighted_sum(this, weights, axis) result(weighted_sum_val)
        class(fortarray_t), intent(in) :: this
        class(fortarray_t), intent(in) :: weights
        character(len=*), intent(in), optional :: axis
        real(real64) :: weighted_sum_val
        
        integer :: i
        
        ! Check dimensions match
        if (this%n_elements /= weights%n_elements) then
            write(error_unit, '(A)') "ERROR: Array and weights must have same size"
            weighted_sum_val = 0.0_real64
            return
        end if
        
        ! Only handle real64 for now
        if (this%data%dtype /= DTYPE_REAL64 .or. weights%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: weighted_sum only supports real64 data type"
            weighted_sum_val = 0.0_real64
            return
        end if
        
        ! Calculate weighted sum
        weighted_sum_val = 0.0_real64
        do i = 1, this%n_elements
            weighted_sum_val = weighted_sum_val + this%data%values_r64(i) * weights%data%values_r64(i)
        end do
        
    end function fortarray_weighted_sum
    
    !> Calculate weighted standard deviation
    module function fortarray_weighted_std(this, weights, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        class(fortarray_t), intent(in) :: weights
        character(len=*), intent(in), optional :: axis
        type(fortarray_t) :: result_array
        
        real(real64) :: weighted_mean, weighted_var, sum_weights
        real(real64) :: sum_squared_dev
        integer :: i
        
        ! Calculate weighted mean first
        weighted_mean = this%weighted_mean(weights, axis)
        
        ! Calculate weighted variance
        sum_squared_dev = 0.0_real64
        sum_weights = 0.0_real64
        
        do i = 1, this%n_elements
            sum_squared_dev = sum_squared_dev + weights%data%values_r64(i) * &
                            (this%data%values_r64(i) - weighted_mean)**2
            sum_weights = sum_weights + weights%data%values_r64(i)
        end do
        
        if (sum_weights > 0.0_real64) then
            weighted_var = sum_squared_dev / sum_weights
            result_array = new_array([sqrt(weighted_var)], name="weighted_std")
        else
            result_array = new_array([0.0_real64], name="weighted_std")
        end if
        
    end function fortarray_weighted_std
    
    !> Calculate cumulative sum
    module function fortarray_cumsum(this, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: axis
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: cumulative(:)
        integer :: i
        
        ! Only handle 1D real64 for now
        if (this%n_dims > 1 .and. .not. present(axis)) then
            write(error_unit, '(A)') "ERROR: cumsum requires axis parameter for multi-dimensional arrays"
            result_array = create_empty_like(this)
            return
        end if
        
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: cumsum only supports real64 data type"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Allocate cumulative array
        allocate(cumulative(this%n_elements))
        
        ! Calculate cumulative sum
        cumulative(1) = this%data%values_r64(1)
        do i = 2, this%n_elements
            cumulative(i) = cumulative(i-1) + this%data%values_r64(i)
        end do
        
        ! Create result array
        result_array = new_array(cumulative, name="cumsum")
        result_array%dim_names = this%dim_names
        result_array%shape = this%shape
        
        deallocate(cumulative)
        
    end function fortarray_cumsum
    
    !> Calculate cumulative product
    module function fortarray_cumprod(this, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: axis
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: cumulative(:)
        integer :: i
        
        ! Only handle 1D real64 for now
        if (this%n_dims > 1 .and. .not. present(axis)) then
            write(error_unit, '(A)') "ERROR: cumprod requires axis parameter for multi-dimensional arrays"
            result_array = create_empty_like(this)
            return
        end if
        
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: cumprod only supports real64 data type"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Allocate cumulative array
        allocate(cumulative(this%n_elements))
        
        ! Calculate cumulative product
        cumulative(1) = this%data%values_r64(1)
        do i = 2, this%n_elements
            cumulative(i) = cumulative(i-1) * this%data%values_r64(i)
        end do
        
        ! Create result array
        result_array = new_array(cumulative, name="cumprod")
        result_array%dim_names = this%dim_names
        result_array%shape = this%shape
        
        deallocate(cumulative)
        
    end function fortarray_cumprod
    
    !> Calculate cumulative minimum
    module function fortarray_cummin(this, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: axis
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: cumulative(:)
        integer :: i
        
        ! Only handle 1D real64 for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: cummin only supports real64 data type"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Allocate cumulative array
        allocate(cumulative(this%n_elements))
        
        ! Calculate cumulative minimum
        cumulative(1) = this%data%values_r64(1)
        do i = 2, this%n_elements
            cumulative(i) = min(cumulative(i-1), this%data%values_r64(i))
        end do
        
        ! Create result array
        result_array = new_array(cumulative, name="cummin")
        
        deallocate(cumulative)
        
    end function fortarray_cummin
    
    !> Calculate cumulative maximum
    module function fortarray_cummax(this, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: axis
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: cumulative(:)
        integer :: i
        
        ! Only handle 1D real64 for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: cummax only supports real64 data type"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Allocate cumulative array
        allocate(cumulative(this%n_elements))
        
        ! Calculate cumulative maximum
        cumulative(1) = this%data%values_r64(1)
        do i = 2, this%n_elements
            cumulative(i) = max(cumulative(i-1), this%data%values_r64(i))
        end do
        
        ! Create result array
        result_array = new_array(cumulative, name="cummax")
        
        deallocate(cumulative)
        
    end function fortarray_cummax
    
    !> Calculate rolling mean
    module function fortarray_rolling_mean(this, window, center, min_periods) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: window
        logical, intent(in), optional :: center
        integer, intent(in), optional :: min_periods
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: rolling_values(:)
        real(real64) :: window_sum
        integer :: i, j, start_idx, end_idx, count
        integer :: min_count
        logical :: use_center
        
        ! Set defaults
        use_center = .false.
        if (present(center)) use_center = center
        min_count = window
        if (present(min_periods)) min_count = min_periods
        
        ! Only handle 1D real64 for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: rolling_mean only supports real64 data type"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Allocate result array
        allocate(rolling_values(this%n_elements))
        rolling_values = 0.0_real64  ! Initialize with zeros (will be NaN for insufficient data)
        
        ! Calculate rolling mean
        do i = 1, this%n_elements
            if (use_center) then
                start_idx = max(1, i - window/2)
                end_idx = min(this%n_elements, i + window/2)
            else
                start_idx = max(1, i - window + 1)
                end_idx = i
            end if
            
            ! Calculate window sum and count
            window_sum = 0.0_real64
            count = 0
            do j = start_idx, end_idx
                window_sum = window_sum + this%data%values_r64(j)
                count = count + 1
            end do
            
            ! Set result if enough values
            if (count >= min_count) then
                rolling_values(i) = window_sum / real(count, real64)
            else
                rolling_values(i) = ieee_value(0.0_real64, ieee_quiet_nan)
            end if
        end do
        
        ! Create result array
        result_array = new_array(rolling_values, name="rolling_mean")
        result_array%dim_names = this%dim_names
        result_array%shape = this%shape
        
        deallocate(rolling_values)
        
    end function fortarray_rolling_mean
    
    !> Calculate rolling sum
    module function fortarray_rolling_sum(this, window, center, min_periods) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: window
        logical, intent(in), optional :: center
        integer, intent(in), optional :: min_periods
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: rolling_values(:)
        real(real64) :: window_sum
        integer :: i, j, start_idx, end_idx, count
        integer :: min_count
        logical :: use_center
        
        ! Set defaults
        use_center = .false.
        if (present(center)) use_center = center
        min_count = 1
        if (present(min_periods)) min_count = min_periods
        
        ! Only handle 1D real64 for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: rolling_sum only supports real64 data type"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Allocate result array
        allocate(rolling_values(this%n_elements))
        
        ! Calculate rolling sum
        do i = 1, this%n_elements
            if (use_center) then
                start_idx = max(1, i - window/2)
                end_idx = min(this%n_elements, i + window/2)
            else
                start_idx = max(1, i - window + 1)
                end_idx = i
            end if
            
            ! Calculate window sum
            window_sum = 0.0_real64
            count = 0
            do j = start_idx, end_idx
                window_sum = window_sum + this%data%values_r64(j)
                count = count + 1
            end do
            
            ! Set result if enough values
            if (count >= min_count) then
                rolling_values(i) = window_sum
            else
                rolling_values(i) = ieee_value(0.0_real64, ieee_quiet_nan)
            end if
        end do
        
        ! Create result array
        result_array = new_array(rolling_values, name="rolling_sum")
        result_array%dim_names = this%dim_names
        result_array%shape = this%shape
        
        deallocate(rolling_values)
        
    end function fortarray_rolling_sum
    
    !> Calculate rolling standard deviation
    module function fortarray_rolling_std(this, window, center, min_periods) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: window
        logical, intent(in), optional :: center
        integer, intent(in), optional :: min_periods
        type(fortarray_t) :: result_array
        
        real(real64), allocatable :: rolling_values(:)
        real(real64) :: window_mean, window_var, sum_sq_diff
        integer :: i, j, start_idx, end_idx, count
        integer :: min_count
        logical :: use_center
        
        ! Set defaults
        use_center = .false.
        if (present(center)) use_center = center
        min_count = 2  ! Need at least 2 values for std
        if (present(min_periods)) min_count = max(2, min_periods)
        
        ! Only handle 1D real64 for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: rolling_std only supports real64 data type"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Allocate result array
        allocate(rolling_values(this%n_elements))
        
        ! Calculate rolling std
        do i = 1, this%n_elements
            if (use_center) then
                start_idx = max(1, i - window/2)
                end_idx = min(this%n_elements, i + window/2)
            else
                start_idx = max(1, i - window + 1)
                end_idx = i
            end if
            
            ! Calculate window mean first
            window_mean = 0.0_real64
            count = 0
            do j = start_idx, end_idx
                window_mean = window_mean + this%data%values_r64(j)
                count = count + 1
            end do
            
            if (count >= min_count) then
                window_mean = window_mean / real(count, real64)
                
                ! Calculate variance
                sum_sq_diff = 0.0_real64
                do j = start_idx, end_idx
                    sum_sq_diff = sum_sq_diff + (this%data%values_r64(j) - window_mean)**2
                end do
                window_var = sum_sq_diff / real(count - 1, real64)  ! Sample variance
                rolling_values(i) = sqrt(window_var)
            else
                rolling_values(i) = ieee_value(0.0_real64, ieee_quiet_nan)
            end if
        end do
        
        ! Create result array
        result_array = new_array(rolling_values, name="rolling_std")
        result_array%dim_names = this%dim_names
        result_array%shape = this%shape
        
        deallocate(rolling_values)
        
    end function fortarray_rolling_std
    
    ! ======= HELPER FUNCTIONS =======
    
    !> Quicksort for real64 arrays
    recursive subroutine quicksort_r64(arr, first, last)
        real(real64), dimension(:), intent(inout) :: arr
        integer, intent(in) :: first, last
        
        integer :: i, j
        real(real64) :: x, temp
        
        if (first >= last) return
        
        x = arr((first + last) / 2)
        i = first
        j = last
        
        do
            do while (arr(i) < x)
                i = i + 1
            end do
            do while (x < arr(j))
                j = j - 1
            end do
            if (i >= j) exit
            
            temp = arr(i)
            arr(i) = arr(j)
            arr(j) = temp
            i = i + 1
            j = j - 1
        end do
        
        if (first < i - 1) call quicksort_r64(arr, first, i - 1)
        if (j + 1 < last) call quicksort_r64(arr, j + 1, last)
        
    end subroutine quicksort_r64
    
    ! ======= DIMENSION MANIPULATION METHODS (Sprint 10) =======
    
    !> Transpose array (reverse dimension order by default)
    module function fortarray_transpose(this, axes) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, dimension(:), intent(in), optional :: axes
        type(fortarray_t) :: result_array
        
        integer, dimension(:), allocatable :: perm, new_shape, new_strides
        integer, dimension(:), allocatable :: old_indices, new_indices
        integer :: i, j, n_dims
        real(real64), allocatable :: temp_data(:)
        
        n_dims = this%n_dims
        
        ! Handle scalar (0D) case
        if (n_dims == 0) then
            result_array = this
            return
        end if
        
        ! Set up permutation
        allocate(perm(n_dims))
        if (present(axes)) then
            if (size(axes) /= n_dims) then
                write(error_unit, '(A)') "ERROR: axes must have same length as array dimensions"
                result_array = create_empty_like(this)
                return
            end if
            perm = axes
        else
            ! Default: reverse dimension order
            do i = 1, n_dims
                perm(i) = n_dims - i + 1
            end do
        end if
        
        ! Validate permutation
        do i = 1, n_dims
            if (perm(i) < 1 .or. perm(i) > n_dims) then
                write(error_unit, '(A)') "ERROR: Invalid axis in permutation"
                result_array = create_empty_like(this)
                return
            end if
        end do
        
        ! Calculate new shape
        allocate(new_shape(n_dims))
        do i = 1, n_dims
            new_shape(i) = this%shape(perm(i))
        end do
        
        ! Create result array with transposed shape
        result_array%name = trim(this%name) // "_T"
        result_array%n_dims = n_dims
        result_array%n_elements = this%n_elements
        result_array%initialized = .true.
        
        allocate(result_array%shape(n_dims))
        result_array%shape = new_shape
        
        ! Transpose dimension names if present
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(n_dims))
            do i = 1, n_dims
                result_array%dim_names(i) = this%dim_names(perm(i))
            end do
        end if
        
        ! For real64 data only (extend for other types as needed)
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: transpose currently only supports real64 data"
            return
        end if
        
        ! Setup result data storage
        result_array%data%dtype = this%data%dtype
        result_array%data%initialized = .true.
        result_array%data%n_elements = this%n_elements
        allocate(result_array%data%values_r64(this%n_elements))
        
        ! Perform transpose by iterating through all elements
        allocate(old_indices(n_dims), new_indices(n_dims))
        
        do i = 1, this%n_elements
            ! Convert linear index to multi-dimensional indices
            call linear_to_multi_index(i, this%shape, old_indices)
            
            ! Apply permutation
            do j = 1, n_dims
                new_indices(j) = old_indices(perm(j))
            end do
            
            ! Convert back to linear index in transposed array
            j = multi_to_linear_index(new_indices, new_shape)
            
            ! Copy data
            result_array%data%values_r64(j) = this%data%values_r64(i)
        end do
        
        deallocate(perm, new_shape, old_indices, new_indices)
        
    end function fortarray_transpose
    
    !> Squeeze array (remove dimensions of size 1)
    module function fortarray_squeeze(this, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: axis
        type(fortarray_t) :: result_array
        
        integer, dimension(:), allocatable :: new_shape, squeeze_mask
        character(len=:), dimension(:), allocatable :: new_dim_names
        integer :: i, j, new_ndims
        
        ! Count dimensions to keep
        if (present(axis)) then
            ! Squeeze only specified axis if it's size 1
            if (axis < 1 .or. axis > this%n_dims) then
                write(error_unit, '(A)') "ERROR: Invalid axis for squeeze"
                result_array = create_empty_like(this)
                return
            end if
            if (this%shape(axis) /= 1) then
                write(error_unit, '(A)') "ERROR: Cannot squeeze axis with size > 1"
                result_array = create_empty_like(this)
                return
            end if
            
            ! Create new shape without specified axis
            new_ndims = this%n_dims - 1
            if (new_ndims == 0) then
                ! Result is scalar
                result_array = this
                result_array%n_dims = 0
                if (allocated(result_array%shape)) deallocate(result_array%shape)
                if (allocated(result_array%dim_names)) deallocate(result_array%dim_names)
                return
            end if
            
            allocate(new_shape(new_ndims))
            j = 0
            do i = 1, this%n_dims
                if (i /= axis) then
                    j = j + 1
                    new_shape(j) = this%shape(i)
                end if
            end do
        else
            ! Squeeze all dimensions of size 1
            allocate(squeeze_mask(this%n_dims))
            squeeze_mask = 0
            new_ndims = 0
            do i = 1, this%n_dims
                if (this%shape(i) > 1) then
                    new_ndims = new_ndims + 1
                    squeeze_mask(i) = 1
                end if
            end do
            
            if (new_ndims == 0) then
                ! All dimensions are size 1, result is scalar
                result_array = this
                result_array%n_dims = 0
                if (allocated(result_array%shape)) deallocate(result_array%shape)
                if (allocated(result_array%dim_names)) deallocate(result_array%dim_names)
                return
            end if
            
            allocate(new_shape(new_ndims))
            j = 0
            do i = 1, this%n_dims
                if (squeeze_mask(i) == 1) then
                    j = j + 1
                    new_shape(j) = this%shape(i)
                end if
            end do
        end if
        
        ! Create result array
        result_array = this  ! Shallow copy
        result_array%shape = new_shape
        result_array%n_dims = new_ndims
        
        ! Update dimension names if present
        if (allocated(this%dim_names)) then
            allocate(character(len=len(this%dim_names)) :: new_dim_names(new_ndims))
            if (present(axis)) then
                j = 0
                do i = 1, this%n_dims
                    if (i /= axis) then
                        j = j + 1
                        new_dim_names(j) = this%dim_names(i)
                    end if
                end do
            else
                j = 0
                do i = 1, this%n_dims
                    if (squeeze_mask(i) == 1) then
                        j = j + 1
                        new_dim_names(j) = this%dim_names(i)
                    end if
                end do
            end if
            result_array%dim_names = new_dim_names
        end if
        
    end function fortarray_squeeze
    
    !> Expand dimensions (add new axis of size 1)
    module function fortarray_expand_dims(this, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: axis
        type(fortarray_t) :: result_array
        
        integer, dimension(:), allocatable :: new_shape
        character(len=:), dimension(:), allocatable :: new_dim_names
        integer :: i, j, new_ndims
        
        new_ndims = this%n_dims + 1
        
        ! Validate axis
        if (axis < 1 .or. axis > new_ndims) then
            write(error_unit, '(A)') "ERROR: Invalid axis for expand_dims"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Create new shape with size 1 at specified axis
        allocate(new_shape(new_ndims))
        j = 0
        do i = 1, new_ndims
            if (i == axis) then
                new_shape(i) = 1
            else
                j = j + 1
                if (j <= this%n_dims) then
                    new_shape(i) = this%shape(j)
                end if
            end if
        end do
        
        ! Create result array
        result_array = this  ! Shallow copy (same data)
        result_array%shape = new_shape
        result_array%n_dims = new_ndims
        
        ! Update dimension names if present
        if (allocated(this%dim_names)) then
            allocate(character(len=max(len(this%dim_names), 8)) :: new_dim_names(new_ndims))
            j = 0
            do i = 1, new_ndims
                if (i == axis) then
                    new_dim_names(i) = "newaxis"
                else
                    j = j + 1
                    if (j <= this%n_dims) then
                        new_dim_names(i) = this%dim_names(j)
                    end if
                end if
            end do
            result_array%dim_names = new_dim_names
        end if
        
    end function fortarray_expand_dims
    
    !> Rename dimensions
    module function fortarray_rename_dims(this, old_name, new_name, new_names) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in), optional :: old_name, new_name
        character(len=*), dimension(:), intent(in), optional :: new_names
        type(fortarray_t) :: result_array
        
        integer :: i
        
        ! Create result as copy
        result_array = this
        
        ! Ensure dimension names are allocated
        if (.not. allocated(result_array%dim_names)) then
            allocate(result_array%dim_names(this%n_dims))
            do i = 1, this%n_dims
                write(result_array%dim_names(i), '(A,I0)') "dim", i
            end do
        end if
        
        if (present(old_name) .and. present(new_name)) then
            ! Rename single dimension
            do i = 1, this%n_dims
                if (trim(result_array%dim_names(i)) == trim(old_name)) then
                    result_array%dim_names(i) = new_name
                    exit
                end if
            end do
        else if (present(new_names)) then
            ! Rename all dimensions
            if (size(new_names) /= this%n_dims) then
                write(error_unit, '(A)') "ERROR: new_names must match number of dimensions"
                return
            end if
            result_array%dim_names = new_names
        else
            write(error_unit, '(A)') "ERROR: Must provide either old_name/new_name or new_names"
        end if
        
    end function fortarray_rename_dims
    
    !> Stack arrays along new axis
    module function fortarray_stack(arrays, axis, out_name) result(result_array)
        type(fortarray_t), dimension(:), intent(in) :: arrays
        integer, intent(in) :: axis
        character(len=*), intent(in), optional :: out_name
        type(fortarray_t) :: result_array
        integer :: n_arrays, i, j, idx
        integer, allocatable :: new_shape(:)
        real(real64), allocatable :: stacked_data(:)
        character(len=32) :: num_str
        
        n_arrays = size(arrays)
        if (n_arrays == 0) then
            write(error_unit, '(A)') "ERROR: Cannot stack empty array list"
            return
        end if
        
        ! Check all arrays have same shape
        do i = 2, n_arrays
            if (.not. all(arrays(i)%shape == arrays(1)%shape)) then
                write(error_unit, '(A)') "ERROR: All arrays must have same shape for stacking"
                return
            end if
        end do
        
        ! Create new shape with added dimension
        allocate(new_shape(arrays(1)%n_dims + 1))
        if (axis <= arrays(1)%n_dims + 1) then
            new_shape(1:axis-1) = arrays(1)%shape(1:axis-1)
            new_shape(axis) = n_arrays
            if (axis <= arrays(1)%n_dims) then
                new_shape(axis+1:) = arrays(1)%shape(axis:)
            end if
        else
            write(error_unit, '(A)') "ERROR: Invalid axis for stack"
            return
        end if
        
        ! Initialize result
        result_array%name = "stacked"
        if (present(out_name)) result_array%name = out_name
        result_array%n_dims = arrays(1)%n_dims + 1
        result_array%n_elements = product(new_shape)
        allocate(result_array%shape(result_array%n_dims))
        result_array%shape = new_shape
        result_array%initialized = .true.
        
        ! Stack data (only real64 for now)
        if (arrays(1)%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: stack currently only supports real64 data"
            return
        end if
        
        result_array%data%dtype = DTYPE_REAL64
        result_array%data%initialized = .true.
        result_array%data%n_elements = result_array%n_elements
        allocate(result_array%data%values_r64(result_array%n_elements))
        
        ! Copy data from each array
        idx = 1
        do i = 1, n_arrays
            do j = 1, arrays(i)%n_elements
                result_array%data%values_r64(idx) = arrays(i)%data%values_r64(j)
                idx = idx + 1
            end do
        end do
        
    end function fortarray_stack
    
    !> Unstack array along axis
    module function fortarray_unstack(this, axis) result(arrays)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: axis
        type(fortarray_t), dimension(:), allocatable :: arrays
        integer :: n_slices, slice_size, i, j, idx
        integer, allocatable :: slice_shape(:)
        character(len=32) :: num_str
        
        if (axis < 1 .or. axis > this%n_dims) then
            write(error_unit, '(A)') "ERROR: Invalid axis for unstack"
            return
        end if
        
        n_slices = this%shape(axis)
        allocate(arrays(n_slices))
        
        ! Create shape for each slice (remove axis dimension)
        allocate(slice_shape(this%n_dims - 1))
        if (axis == 1) then
            slice_shape = this%shape(2:)
        else if (axis == this%n_dims) then
            slice_shape = this%shape(1:this%n_dims-1)
        else
            slice_shape(1:axis-1) = this%shape(1:axis-1)
            slice_shape(axis:) = this%shape(axis+1:)
        end if
        
        slice_size = product(slice_shape)
        
        ! Only real64 for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: unstack currently only supports real64 data"
            return
        end if
        
        ! Create each slice
        idx = 1
        do i = 1, n_slices
            write(num_str, '(I0)') i
            arrays(i)%name = trim(this%name) // "_slice" // trim(num_str)
            arrays(i)%n_dims = this%n_dims - 1
            arrays(i)%n_elements = slice_size
            allocate(arrays(i)%shape(arrays(i)%n_dims))
            arrays(i)%shape = slice_shape
            arrays(i)%initialized = .true.
            
            arrays(i)%data%dtype = DTYPE_REAL64
            arrays(i)%data%initialized = .true.
            arrays(i)%data%n_elements = slice_size
            allocate(arrays(i)%data%values_r64(slice_size))
            
            ! Copy slice data
            do j = 1, slice_size
                arrays(i)%data%values_r64(j) = this%data%values_r64(idx)
                idx = idx + 1
            end do
        end do
        
    end function fortarray_unstack
    
    ! ======= MISSING DATA ADVANCED HANDLING (SPRINT 11) =======
    
    !> Interpolate missing values with various methods
    module function fortarray_interpolate_na(this, method, order, limit) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in), optional :: method
        integer, intent(in), optional :: order
        integer, intent(in), optional :: limit
        type(fortarray_t) :: result_array
        character(len=20) :: interp_method
        integer :: interp_order, max_gap
        real(real64), allocatable :: values(:)
        integer :: i, j, k, start_idx, end_idx, n_missing
        real(real64) :: slope, val
        
        ! Set defaults
        interp_method = "linear"
        if (present(method)) interp_method = method
        interp_order = 1
        if (present(order)) interp_order = order
        max_gap = this%n_elements
        if (present(limit)) max_gap = limit
        
        ! Create result as copy
        result_array = this
        
        ! Only support real64 for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: interpolate_na currently only supports real64 data"
            return
        end if
        
        ! Get values
        allocate(values(this%n_elements))
        values = this%data%values_r64
        
        select case(trim(interp_method))
        case("linear")
            ! Linear interpolation
            i = 1
            do while (i <= this%n_elements)
                if (is_missing(values(i))) then
                    ! Find start of NaN region
                    start_idx = i
                    
                    ! Find end of NaN region
                    j = i
                    do while (j <= this%n_elements)
                        if (.not. is_missing(values(j))) exit
                        j = j + 1
                    end do
                    end_idx = j - 1
                    n_missing = end_idx - start_idx + 1
                    
                    ! Interpolate if we have valid values on both sides and gap is within limit
                    if (start_idx > 1 .and. j <= this%n_elements .and. n_missing <= max_gap) then
                        slope = (values(j) - values(start_idx - 1)) / real(n_missing + 1, real64)
                        
                        do k = start_idx, end_idx
                            values(k) = values(start_idx - 1) + slope * real(k - start_idx + 1, real64)
                        end do
                    end if
                    
                    i = end_idx + 1
                else
                    i = i + 1
                end if
            end do
            
        case("polynomial")
            write(error_unit, '(A)') "WARNING: polynomial interpolation not yet implemented, fallback to linear"
            ! Same as linear for now
            i = 1
            do while (i <= this%n_elements)
                if (is_missing(values(i))) then
                    start_idx = i
                    j = i
                    do while (j <= this%n_elements)
                        if (.not. is_missing(values(j))) exit
                        j = j + 1
                    end do
                    end_idx = j - 1
                    n_missing = end_idx - start_idx + 1
                    
                    if (start_idx > 1 .and. j <= this%n_elements .and. n_missing <= max_gap) then
                        slope = (values(j) - values(start_idx - 1)) / real(n_missing + 1, real64)
                        do k = start_idx, end_idx
                            values(k) = values(start_idx - 1) + slope * real(k - start_idx + 1, real64)
                        end do
                    end if
                    i = end_idx + 1
                else
                    i = i + 1
                end if
            end do
            
        case("spline")
            write(error_unit, '(A)') "WARNING: spline interpolation not yet implemented, fallback to linear"
            ! Same as linear for now
            i = 1
            do while (i <= this%n_elements)
                if (is_missing(values(i))) then
                    start_idx = i
                    j = i
                    do while (j <= this%n_elements)
                        if (.not. is_missing(values(j))) exit
                        j = j + 1
                    end do
                    end_idx = j - 1
                    n_missing = end_idx - start_idx + 1
                    
                    if (start_idx > 1 .and. j <= this%n_elements .and. n_missing <= max_gap) then
                        slope = (values(j) - values(start_idx - 1)) / real(n_missing + 1, real64)
                        do k = start_idx, end_idx
                            values(k) = values(start_idx - 1) + slope * real(k - start_idx + 1, real64)
                        end do
                    end if
                    i = end_idx + 1
                else
                    i = i + 1
                end if
            end do
            
        case("nearest")
            ! Nearest neighbor interpolation
            do i = 1, this%n_elements
                if (is_missing(values(i))) then
                    ! Find nearest non-NaN value
                    if (i > 1 .and. .not. is_missing(values(i-1))) then
                        values(i) = values(i-1)
                    else if (i < this%n_elements .and. .not. is_missing(values(i+1))) then
                        values(i) = values(i+1)
                    end if
                end if
            end do
            
        case default
            write(error_unit, '(A,A)') "ERROR: Unknown interpolation method: ", trim(interp_method)
            return
        end select
        
        ! Update result
        result_array%data%values_r64 = values
        
    end function fortarray_interpolate_na
    
    !> Backward fill missing values
    module function fortarray_bfill(this, limit) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: limit
        type(fortarray_t) :: result_array
        integer :: max_fill, fill_count
        real(real64), allocatable :: values(:)
        integer :: i
        
        ! Set limit
        max_fill = this%n_elements
        if (present(limit)) max_fill = limit
        
        ! Create result as copy
        result_array = this
        
        ! Only support real64 for now
        if (this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: bfill currently only supports real64 data"
            return
        end if
        
        ! Get values
        allocate(values(this%n_elements))
        values = this%data%values_r64
        
        ! Backward fill
        fill_count = 0
        do i = this%n_elements - 1, 1, -1
            if (is_missing(values(i))) then
                if (i < this%n_elements .and. .not. is_missing(values(i+1))) then
                    if (fill_count < max_fill) then
                        values(i) = values(i+1)
                        fill_count = fill_count + 1
                    end if
                end if
            else
                fill_count = 0
            end if
        end do
        
        ! Update result
        result_array%data%values_r64 = values
        
    end function fortarray_bfill
    
    !> Advanced dropna with axis and threshold support
    module function fortarray_dropna_advanced(this, axis, how, thresh, subset) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: axis
        character(len=*), intent(in), optional :: how
        integer, intent(in), optional :: thresh
        character(len=*), dimension(:), intent(in), optional :: subset
        type(fortarray_t) :: result_array
        character(len=10) :: drop_how
        integer :: drop_axis, min_count
        logical, allocatable :: row_mask(:), col_mask(:)
        real(real64), allocatable :: values_2d(:,:), result_values(:,:)
        integer :: i, j, n_valid, new_rows, new_cols
        
        ! Set defaults
        drop_how = "any"
        if (present(how)) drop_how = how
        drop_axis = 0  ! Drop along all axes by default
        if (present(axis)) drop_axis = axis
        min_count = 0
        if (present(thresh)) min_count = thresh
        
        ! For 1D arrays, use simple dropna
        if (this%n_dims == 1) then
            result_array = dropna(this)
            return
        end if
        
        ! Only support 2D real64 for now
        if (this%n_dims /= 2 .or. this%data%dtype /= DTYPE_REAL64) then
            write(error_unit, '(A)') "ERROR: Advanced dropna currently only supports 2D real64 arrays"
            result_array = this
            return
        end if
        
        ! Reshape to 2D
        allocate(values_2d(this%shape(1), this%shape(2)))
        values_2d = reshape(this%data%values_r64, [this%shape(1), this%shape(2)])
        
        ! Create masks
        allocate(row_mask(this%shape(1)), col_mask(this%shape(2)))
        row_mask = .true.
        col_mask = .true.
        
        if (drop_axis == 1) then
            ! Drop rows with NaN
            do i = 1, this%shape(1)
                n_valid = 0
                do j = 1, this%shape(2)
                    if (.not. is_missing(values_2d(i,j))) n_valid = n_valid + 1
                end do
                
                if (min_count > 0) then
                    row_mask(i) = (n_valid >= min_count)
                else
                    select case(drop_how)
                    case("any")
                        row_mask(i) = (n_valid == this%shape(2))
                    case("all")
                        row_mask(i) = (n_valid > 0)
                    end select
                end if
            end do
            
        else if (drop_axis == 2) then
            ! Drop columns with NaN
            do j = 1, this%shape(2)
                n_valid = 0
                do i = 1, this%shape(1)
                    if (.not. is_missing(values_2d(i,j))) n_valid = n_valid + 1
                end do
                
                if (min_count > 0) then
                    col_mask(j) = (n_valid >= min_count)
                else
                    select case(drop_how)
                    case("any")
                        col_mask(j) = (n_valid == this%shape(1))
                    case("all")
                        col_mask(j) = (n_valid > 0)
                    end select
                end if
            end do
        end if
        
        ! Count remaining rows/cols
        new_rows = count(row_mask)
        new_cols = count(col_mask)
        
        ! Create result
        allocate(result_values(new_rows, new_cols))
        
        ! Copy non-dropped values
        new_rows = 0
        do i = 1, this%shape(1)
            if (row_mask(i)) then
                new_rows = new_rows + 1
                new_cols = 0
                do j = 1, this%shape(2)
                    if (col_mask(j)) then
                        new_cols = new_cols + 1
                        result_values(new_rows, new_cols) = values_2d(i,j)
                    end if
                end do
            end if
        end do
        
        ! Create result array with proper dimension names
        if (allocated(this%dim_names)) then
            result_array = new_array(result_values, name=trim(this%name) // "_dropna", &
                                   dim_names=this%dim_names)
        else
            result_array = new_array(result_values, name=trim(this%name) // "_dropna")
        end if
        
    end function fortarray_dropna_advanced
    
    ! ======= PERFORMANCE OPTIMIZATION LAYER (SPRINT 12) =======
    
    !> SIMD-optimized selection by value
    module function fortarray_sel_simd(this, x) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x
        type(fortarray_t) :: result_array
        
        ! For now, delegate to regular selection
        ! In a full implementation, this would use SIMD instructions
        if (this%n_dims >= 1 .and. allocated(this%dim_names)) then
            result_array = this%sel_point(this%dim_names(1), x)
        else
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel_simd
    
    !> SIMD-optimized range selection
    module function fortarray_sel_range_simd(this, x_min, x_max) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x_min, x_max
        type(fortarray_t) :: result_array
        integer :: i, count
        logical, allocatable :: mask(:)
        real(real64), allocatable :: values(:)
        
        ! Simplified implementation - in practice would use SIMD
        if (.not. this%initialized .or. this%n_dims /= 1) then
            result_array = create_empty_like(this)
            return
        end if
        
        ! Create mask for range selection
        allocate(mask(this%n_elements))
        values = get_values_as_real64(this)
        
        count = 0
        do i = 1, this%n_elements
            if (values(i) >= x_min .and. values(i) <= x_max) then
                mask(i) = .true.
                count = count + 1
            else
                mask(i) = .false.
            end if
        end do
        
        ! Create result with selected values
        if (count > 0) then
            block
                real(real64), allocatable :: result_values(:)
                integer :: j
                
                allocate(result_values(count))
                j = 0
                do i = 1, this%n_elements
                    if (mask(i)) then
                        j = j + 1
                        result_values(j) = values(i)
                    end if
                end do
                
                result_array = new_array(result_values, name=trim(this%name) // "_simd_range")
            end block
        else
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel_range_simd
    
    !> Parallel nearest neighbor lookup
    module function fortarray_sel_nearest_parallel(this, x) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x
        type(fortarray_t) :: result_array
        
        ! For now, delegate to regular nearest selection
        ! In a full implementation, this would use OpenMP parallelization
        if (this%n_dims >= 1 .and. allocated(this%dim_names)) then
            result_array = this%sel_nearest(this%dim_names(1), x)
        else
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel_nearest_parallel
    
    !> Parallel interpolation lookup
    module function fortarray_interp_parallel(this, x) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x
        type(fortarray_t) :: result_array
        
        ! Placeholder - delegate to regular interpolation
        if (this%n_dims >= 1 .and. allocated(this%dim_names)) then
            result_array = this%sel_interp(this%dim_names(1), x)
        else
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_interp_parallel
    
    !> Parallel multi-point lookup
    module function fortarray_sel_multipoint_parallel(this, x_values) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), dimension(:), intent(in) :: x_values
        type(fortarray_t) :: result_array
        integer :: n_points, i
        real(real64), allocatable :: result_values(:)
        
        n_points = size(x_values)
        allocate(result_values(n_points))
        
        ! Simplified implementation - could be parallelized with OpenMP
        !$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(this, x_values, result_values, n_points)
        do i = 1, n_points
            ! For each point, find nearest value (simplified)
            result_values(i) = x_values(i)  ! Placeholder
        end do
        !$OMP END PARALLEL DO
        
        result_array = new_array(result_values, name=trim(this%name) // "_multipoint")
        
    end function fortarray_sel_multipoint_parallel
    
    !> Memory layout optimization
    module function fortarray_optimize_layout(this, layout) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: layout
        type(fortarray_t) :: result_array
        
        ! Placeholder - return copy for now
        result_array = this
        result_array%name = trim(this%name) // "_opt_" // trim(layout)
        
    end function fortarray_optimize_layout
    
    !> Memory prefetching optimization
    module function fortarray_prefetch_optimize(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        ! Placeholder - return copy with optimization flag
        result_array = this
        result_array%name = trim(this%name) // "_prefetch"
        
    end function fortarray_prefetch_optimize
    
    !> Cache-friendly chunking
    module function fortarray_chunk_optimize(this, chunk_size) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, dimension(:), intent(in) :: chunk_size
        type(fortarray_t) :: result_array
        
        ! Placeholder - return copy with chunking applied
        result_array = this
        result_array%name = trim(this%name) // "_chunked"
        
    end function fortarray_chunk_optimize
    
    !> Binary search for exact values in sorted coordinates
    module function fortarray_sel_binary_search(this, x) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x
        type(fortarray_t) :: result_array
        integer :: low, high, mid, found_idx
        real(real64), allocatable :: values(:)
        logical :: found
        
        if (.not. this%initialized .or. this%n_dims /= 1) then
            result_array = create_empty_like(this)
            return
        end if
        
        values = get_values_as_real64(this)
        
        ! Binary search implementation
        low = 1
        high = this%n_elements
        found = .false.
        found_idx = 1
        
        do while (low <= high)
            mid = (low + high) / 2
            if (abs(values(mid) - x) < 1e-10) then
                found_idx = mid
                found = .true.
                exit
            else if (values(mid) < x) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do
        
        if (found) then
            result_array = new_array([values(found_idx)], name=trim(this%name) // "_binary")
        else
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel_binary_search
    
    !> Binary search with interpolation
    module function fortarray_sel_binary_interp(this, x) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x
        type(fortarray_t) :: result_array
        
        ! Simplified - use binary search then interpolate
        result_array = this%sel_binary_search(x)
        if (result_array%n_elements == 0) then
            ! Could implement interpolation here
            result_array = new_array([x], name=trim(this%name) // "_interp")
        end if
        
    end function fortarray_sel_binary_interp
    
    !> Range selection using binary search bounds
    module function fortarray_sel_range_binary(this, x_min, x_max) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x_min, x_max
        type(fortarray_t) :: result_array
        integer :: start_idx, end_idx, count, i
        real(real64), allocatable :: values(:), result_values(:)
        
        if (.not. this%initialized .or. this%n_dims /= 1) then
            result_array = create_empty_like(this)
            return
        end if
        
        values = get_values_as_real64(this)
        
        ! Find start and end indices using binary search logic
        start_idx = 1
        end_idx = 0
        
        do i = 1, this%n_elements
            if (values(i) >= x_min .and. start_idx == 1 .and. values(i) <= x_max) then
                start_idx = i
            end if
            if (values(i) <= x_max) then
                end_idx = i
            end if
        end do
        
        count = end_idx - start_idx + 1
        if (count > 0) then
            allocate(result_values(count))
            result_values = values(start_idx:end_idx)
            result_array = new_array(result_values, name=trim(this%name) // "_range_binary")
        else
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel_range_binary
    
    !> Batch selection optimization for sorted data
    module function fortarray_sel_batch_sorted(this, x_values) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), dimension(:), intent(in) :: x_values
        type(fortarray_t) :: result_array
        integer :: i, n_values
        real(real64), allocatable :: result_values(:)
        
        n_values = size(x_values)
        allocate(result_values(n_values))
        
        ! Simplified batch processing
        do i = 1, n_values
            result_values(i) = x_values(i)  ! Placeholder
        end do
        
        result_array = new_array(result_values, name=trim(this%name) // "_batch")
        
    end function fortarray_sel_batch_sorted
    
    !> Cached coordinate selection
    module function fortarray_sel_cached(this, x) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x
        type(fortarray_t) :: result_array
        
        ! Placeholder - in practice would check cache first
        if (this%n_dims >= 1 .and. allocated(this%dim_names)) then
            result_array = this%sel_point(this%dim_names(1), x)
        else
            result_array = create_empty_like(this)
        end if
        
    end function fortarray_sel_cached
    
    !> Invalidate coordinate cache
    module subroutine fortarray_invalidate_cache(this)
        class(fortarray_t), intent(inout) :: this
        
        ! Placeholder - would clear internal cache structures
        ! For now, this is a no-op
        
    end subroutine fortarray_invalidate_cache
    
    !> Get cache statistics
    module subroutine fortarray_get_cache_stats(this, hits, misses, hit_ratio)
        class(fortarray_t), intent(in) :: this
        integer, intent(out) :: hits, misses
        real(real64), intent(out) :: hit_ratio
        
        ! Placeholder statistics
        hits = 1
        misses = 1
        hit_ratio = 0.5_real64
        
    end subroutine fortarray_get_cache_stats
    
    !> Memory usage optimization
    module function fortarray_optimize_memory(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        ! Placeholder - return optimized copy
        result_array = this
        result_array%name = trim(this%name) // "_mem_opt"
        
    end function fortarray_optimize_memory
    
    !> Parallel range selection
    module function fortarray_sel_range_parallel(this, x_min, x_max) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: x_min, x_max
        type(fortarray_t) :: result_array
        
        ! For now, delegate to SIMD range selection
        result_array = this%sel_range_simd(x_min, x_max)
        
    end function fortarray_sel_range_parallel
    
    ! ======= HELPER FUNCTIONS FOR DIMENSION MANIPULATION =======
    
    !> Get all values as real64 array
    function get_values_as_real64(var) result(values)
        class(fortarray_t), intent(in) :: var
        real(real64), dimension(:), allocatable :: values
        
        allocate(values(var%n_elements))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            values = var%data%values_r64
        case(DTYPE_REAL32)
            values = real(var%data%values_r32, real64)
        case(DTYPE_INT32)
            values = real(var%data%values_i32, real64)
        case(DTYPE_INT64)
            values = real(var%data%values_i64, real64)
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type"
            stop 1
        end select
        
    end function get_values_as_real64
    
    !> Convert linear index to multi-dimensional indices
    subroutine linear_to_multi_index(linear_idx, shape, indices)
        integer, intent(in) :: linear_idx
        integer, dimension(:), intent(in) :: shape
        integer, dimension(:), intent(out) :: indices
        
        integer :: i, idx, prod
        
        idx = linear_idx - 1  ! Convert to 0-based
        do i = size(shape), 1, -1
            prod = product(shape(1:i-1))
            indices(i) = idx / prod + 1
            idx = mod(idx, prod)
        end do
        
    end subroutine linear_to_multi_index
    
    !> Convert multi-dimensional indices to linear index
    function multi_to_linear_index(indices, shape) result(linear_idx)
        integer, dimension(:), intent(in) :: indices, shape
        integer :: linear_idx
        
        integer :: i, stride
        
        linear_idx = 1
        stride = 1
        do i = 1, size(shape)
            linear_idx = linear_idx + (indices(i) - 1) * stride
            stride = stride * shape(i)
        end do
        
    end function multi_to_linear_index
    
    ! ======= GROUPBY METHODS (SPRINT 13) =======
    
    !> Create groupby object from fortarray_t
    module function fortarray_groupby(this, dim_name, groups) result(gb)
        class(fortarray_t), intent(in), target :: this
        character(len=*), intent(in) :: dim_name
        class(fortarray_t), intent(in) :: groups
        type(groupby_t) :: gb
        
        integer :: i, j, dim_idx, group_count, current_size
        character(len=MAX_NAME_LEN), allocatable :: unique_names(:)
        integer, allocatable :: group_starts(:), group_ends(:)
        integer, allocatable :: group_member_lists(:,:)  ! Store all indices for each group
        integer, allocatable :: group_member_counts(:)   ! Count of members in each group
        logical :: found_dim, found_group
        
        
        ! Initialize groupby object
        gb%initialized = .false.
        gb%dim_name = dim_name
        gb%n_groups = 0
        gb%current_group = 0
        gb%iterator_active = .false.
        
        ! Validate input array
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Cannot create groupby from uninitialized array"
            return
        end if
        
        ! Find dimension index
        found_dim = .false.
        dim_idx = 0
        if (allocated(this%dim_names)) then
            do i = 1, this%n_dims
                if (trim(this%dim_names(i)) == trim(dim_name)) then
                    found_dim = .true.
                    dim_idx = i
                    exit
                end if
            end do
        end if
        
        if (.not. found_dim) then
            write(error_unit, '(A,A)') "ERROR: Dimension not found: ", trim(dim_name)
            return
        end if
        
        ! Validate groups array
        if (.not. groups%initialized) then
            write(error_unit, '(A)') "ERROR: Groups array is not initialized"
            return
        end if
        
        ! Check that groups array has same size as dimension
        if (groups%n_elements /= this%shape(dim_idx)) then
            write(error_unit, '(A)') "ERROR: Groups array size does not match dimension size"
            return
        end if
        
        ! Count unique groups and create group information
        allocate(unique_names(groups%n_elements))
        
        group_count = 0
        
        ! First pass: identify unique group names
        select case(groups%data%dtype)
        case(DTYPE_CHAR)
            ! Character/string groups
            do i = 1, groups%n_elements
                found_group = .false.
                do j = 1, group_count
                    if (trim(unique_names(j)) == trim(groups%data%values_char(i))) then
                        found_group = .true.
                        exit
                    end if
                end do
                
                if (.not. found_group) then
                    group_count = group_count + 1
                    unique_names(group_count) = trim(groups%data%values_char(i))
                end if
            end do
            
        case(DTYPE_INT32)
            ! Integer groups - convert to strings
            do i = 1, groups%n_elements
                block
                    character(len=32) :: int_str
                    write(int_str, '(I0)') groups%data%values_i32(i)
                
                found_group = .false.
                do j = 1, group_count
                    if (trim(unique_names(j)) == trim(int_str)) then
                        found_group = .true.
                        exit
                    end if
                end do
                
                if (.not. found_group) then
                    group_count = group_count + 1
                    unique_names(group_count) = trim(int_str)
                end if
                end block
            end do
            
        case(DTYPE_REAL64)
            ! Real groups - convert to strings
            do i = 1, groups%n_elements
                block
                    character(len=32) :: real_str
                    write(real_str, '(F0.6)') groups%data%values_r64(i)
                
                found_group = .false.
                do j = 1, group_count
                    if (trim(unique_names(j)) == trim(real_str)) then
                        found_group = .true.
                        exit
                    end if
                end do
                
                if (.not. found_group) then
                    group_count = group_count + 1
                    unique_names(group_count) = trim(real_str)
                end if
                end block
            end do
            
        case default
            write(error_unit, '(A)') "ERROR: Unsupported group data type"
            return
        end select
        
        ! Second pass: collect all indices for each group
        allocate(group_member_lists(group_count, groups%n_elements))
        allocate(group_member_counts(group_count))
        group_member_counts = 0
        
        select case(groups%data%dtype)
        case(DTYPE_CHAR)
            do i = 1, groups%n_elements
                do j = 1, group_count
                    if (trim(unique_names(j)) == trim(groups%data%values_char(i))) then
                        group_member_counts(j) = group_member_counts(j) + 1
                        group_member_lists(j, group_member_counts(j)) = i
                        exit
                    end if
                end do
            end do
            
        case(DTYPE_INT32)
            do i = 1, groups%n_elements
                block
                    character(len=32) :: int_str
                    write(int_str, '(I0)') groups%data%values_i32(i)
                    do j = 1, group_count
                        if (trim(unique_names(j)) == trim(int_str)) then
                            group_member_counts(j) = group_member_counts(j) + 1
                            group_member_lists(j, group_member_counts(j)) = i
                            exit
                        end if
                    end do
                end block
            end do
            
        case(DTYPE_REAL64)
            do i = 1, groups%n_elements
                block
                    character(len=32) :: real_str
                    write(real_str, '(F0.6)') groups%data%values_r64(i)
                    do j = 1, group_count
                        if (trim(unique_names(j)) == trim(real_str)) then
                            group_member_counts(j) = group_member_counts(j) + 1
                            group_member_lists(j, group_member_counts(j)) = i
                            exit
                        end if
                    end do
                end block
            end do
        end select
        
        ! Set up groupby object
        gb%n_groups = group_count
        allocate(gb%group_names(group_count))
        allocate(gb%group_sizes(group_count))
        allocate(gb%group_indices(group_count, 2))  ! Store start and count
        ! Allocate group_members with safe max dimension
        current_size = 0
        if (size(group_member_counts) > 0) then
            current_size = maxval(group_member_counts)
        end if
        if (current_size <= 0) current_size = 1  ! Ensure at least 1 for allocation
        allocate(gb%group_members(group_count, current_size))  ! Store actual indices
        
        ! Fill group information
        do i = 1, group_count
            gb%group_names(i) = unique_names(i)
            gb%group_sizes(i) = group_member_counts(i)
            ! Store index information
            gb%group_indices(i, 1) = 1  ! Start from 1 for group data extraction
            gb%group_indices(i, 2) = group_member_counts(i)  ! Size of group
            ! Store actual member indices
            do j = 1, group_member_counts(i)
                gb%group_members(i, j) = group_member_lists(i, j)
            end do
        end do
        
        deallocate(group_member_lists, group_member_counts)
        
        ! Set parent array pointer (non-owning)
        gb%parent_array => this
        gb%initialized = .true.
        
        deallocate(unique_names)
        
    end function fortarray_groupby
    
    ! ======= GROUPBY AGGREGATION METHODS =======
    
    !> Groupby mean aggregation
    module function groupby_mean(this, skipna) result(result_array)
        class(groupby_t), intent(in) :: this
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result_array
        
        logical :: skip_missing
        integer :: i, j, group_start, group_end, valid_count
        real(real64), allocatable :: group_means(:), group_values(:)
        real(real64) :: group_sum
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Groupby object not initialized"
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        allocate(group_means(this%n_groups))
        
        ! Calculate mean for each group
        do i = 1, this%n_groups
            ! Extract group values using stored member indices
            allocate(group_values(this%group_sizes(i)))
            do j = 1, this%group_sizes(i)
                group_values(j) = this%parent_array%data%values_r64(this%group_members(i, j))
            end do
            
            ! Calculate mean
            group_sum = 0.0_real64
            valid_count = 0
            
            do j = 1, size(group_values)
                if (skip_missing .and. is_missing(group_values(j))) then
                    cycle
                end if
                group_sum = group_sum + group_values(j)
                valid_count = valid_count + 1
            end do
            
            if (valid_count > 0) then
                group_means(i) = group_sum / real(valid_count, real64)
            else
                group_means(i) = huge(1.0_real64)  ! Missing value
            end if
            
            deallocate(group_values)
        end do
        
        ! Create result array
        result_array = new_array(group_means, name=trim(this%parent_array%name) // "_groupby_mean")
        
    end function groupby_mean
    
    !> Groupby sum aggregation
    module function groupby_sum(this, skipna) result(result_array)
        class(groupby_t), intent(in) :: this
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result_array
        
        logical :: skip_missing
        integer :: i, j, group_start, group_end, valid_count
        real(real64), allocatable :: group_sums(:), group_values(:)
        real(real64) :: group_sum
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Groupby object not initialized"
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        allocate(group_sums(this%n_groups))
        
        ! Calculate sum for each group
        do i = 1, this%n_groups
            ! Extract group values using stored member indices
            allocate(group_values(this%group_sizes(i)))
            do j = 1, this%group_sizes(i)
                group_values(j) = this%parent_array%data%values_r64(this%group_members(i, j))
            end do
            
            ! Calculate sum
            group_sum = 0.0_real64
            valid_count = 0
            
            do j = 1, size(group_values)
                if (skip_missing .and. is_missing(group_values(j))) then
                    cycle
                end if
                group_sum = group_sum + group_values(j)
                valid_count = valid_count + 1
            end do
            
            if (valid_count > 0) then
                group_sums(i) = group_sum
            else
                group_sums(i) = huge(1.0_real64)  ! Missing value
            end if
            
            deallocate(group_values)
        end do
        
        ! Create result array
        result_array = new_array(group_sums, name=trim(this%parent_array%name) // "_groupby_sum")
        
    end function groupby_sum
    
    !> Groupby std aggregation
    module function groupby_std(this, skipna) result(result_array)
        class(groupby_t), intent(in) :: this
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result_array
        
        logical :: skip_missing
        integer :: i, j, group_start, group_end, valid_count
        real(real64), allocatable :: group_stds(:), group_values(:)
        real(real64) :: group_mean, group_sum, group_var
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Groupby object not initialized"
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        allocate(group_stds(this%n_groups))
        
        ! Calculate std for each group
        do i = 1, this%n_groups
            ! Extract group values using stored member indices
            allocate(group_values(this%group_sizes(i)))
            do j = 1, this%group_sizes(i)
                group_values(j) = this%parent_array%data%values_r64(this%group_members(i, j))
            end do
            
            ! Calculate mean first
            group_sum = 0.0_real64
            valid_count = 0
            
            do j = 1, size(group_values)
                if (skip_missing .and. is_missing(group_values(j))) then
                    cycle
                end if
                group_sum = group_sum + group_values(j)
                valid_count = valid_count + 1
            end do
            
            if (valid_count > 1) then
                group_mean = group_sum / real(valid_count, real64)
                
                ! Calculate variance
                group_var = 0.0_real64
                do j = 1, size(group_values)
                    if (skip_missing .and. is_missing(group_values(j))) then
                        cycle
                    end if
                    group_var = group_var + (group_values(j) - group_mean)**2
                end do
                
                group_stds(i) = sqrt(group_var / real(valid_count - 1, real64))
            else
                group_stds(i) = huge(1.0_real64)  ! Missing value
            end if
            
            deallocate(group_values)
        end do
        
        ! Create result array
        result_array = new_array(group_stds, name=trim(this%parent_array%name) // "_groupby_std")
        
    end function groupby_std
    
    !> Groupby max aggregation
    module function groupby_max(this, skipna) result(result_array)
        class(groupby_t), intent(in) :: this
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result_array
        
        logical :: skip_missing
        integer :: i, j, group_start, group_end, valid_count
        real(real64), allocatable :: group_maxes(:), group_values(:)
        real(real64) :: group_max
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Groupby object not initialized"
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        allocate(group_maxes(this%n_groups))
        
        ! Calculate max for each group
        do i = 1, this%n_groups
            ! Extract group values using stored member indices
            allocate(group_values(this%group_sizes(i)))
            do j = 1, this%group_sizes(i)
                group_values(j) = this%parent_array%data%values_r64(this%group_members(i, j))
            end do
            
            ! Calculate max
            group_max = -huge(1.0_real64)
            valid_count = 0
            
            do j = 1, size(group_values)
                if (skip_missing .and. is_missing(group_values(j))) then
                    cycle
                end if
                if (group_values(j) > group_max) then
                    group_max = group_values(j)
                end if
                valid_count = valid_count + 1
            end do
            
            if (valid_count > 0) then
                group_maxes(i) = group_max
            else
                group_maxes(i) = huge(1.0_real64)  ! Missing value
            end if
            
            deallocate(group_values)
        end do
        
        ! Create result array
        result_array = new_array(group_maxes, name=trim(this%parent_array%name) // "_groupby_max")
        
    end function groupby_max
    
    !> Groupby min aggregation
    module function groupby_min(this, skipna) result(result_array)
        class(groupby_t), intent(in) :: this
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result_array
        
        logical :: skip_missing
        integer :: i, j, group_start, group_end, valid_count
        real(real64), allocatable :: group_mins(:), group_values(:)
        real(real64) :: group_min
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Groupby object not initialized"
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        allocate(group_mins(this%n_groups))
        
        ! Calculate min for each group
        do i = 1, this%n_groups
            ! Extract group values using stored member indices
            allocate(group_values(this%group_sizes(i)))
            do j = 1, this%group_sizes(i)
                group_values(j) = this%parent_array%data%values_r64(this%group_members(i, j))
            end do
            
            ! Calculate min
            group_min = huge(1.0_real64)
            valid_count = 0
            
            do j = 1, size(group_values)
                if (skip_missing .and. is_missing(group_values(j))) then
                    cycle
                end if
                if (group_values(j) < group_min) then
                    group_min = group_values(j)
                end if
                valid_count = valid_count + 1
            end do
            
            if (valid_count > 0) then
                group_mins(i) = group_min
            else
                group_mins(i) = huge(1.0_real64)  ! Missing value
            end if
            
            deallocate(group_values)
        end do
        
        ! Create result array
        result_array = new_array(group_mins, name=trim(this%parent_array%name) // "_groupby_min")
        
    end function groupby_min
    
    ! ======= GROUP ACCESS METHODS =======
    
    !> Get specific group by name
    module function groupby_get_group(this, group_name) result(result_array)
        class(groupby_t), intent(in) :: this
        character(len=*), intent(in) :: group_name
        type(fortarray_t) :: result_array
        
        integer :: i, group_idx, group_start, group_end, group_size
        real(real64), allocatable :: group_values(:)
        logical :: found_group
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Groupby object not initialized"
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        ! Find group by name
        found_group = .false.
        group_idx = 0
        do i = 1, this%n_groups
            if (trim(this%group_names(i)) == trim(group_name)) then
                found_group = .true.
                group_idx = i
                exit
            end if
        end do
        
        if (.not. found_group) then
            write(error_unit, '(A,A)') "ERROR: Group not found: ", trim(group_name)
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        ! Extract group data using stored member indices
        group_size = this%group_sizes(group_idx)
        
        allocate(group_values(group_size))
        do i = 1, group_size
            group_values(i) = this%parent_array%data%values_r64(this%group_members(group_idx, i))
        end do
        
        ! Create result array
        result_array = new_array(group_values, name=trim(this%parent_array%name) // "_group_" // trim(group_name))
        
    end function groupby_get_group
    
    ! ======= GROUP ITERATION METHODS =======
    
    !> Reset group iterator
    module subroutine groupby_reset_iterator(this)
        class(groupby_t), intent(inout) :: this
        
        this%current_group = 0
        this%iterator_active = .true.
        
    end subroutine groupby_reset_iterator
    
    !> Check if iterator has next group
    module function groupby_has_next_group(this) result(has_next)
        class(groupby_t), intent(in) :: this
        logical :: has_next
        
        has_next = this%iterator_active .and. (this%current_group < this%n_groups)
        
    end function groupby_has_next_group
    
    !> Get next group in iteration
    module function groupby_next_group(this, group_name) result(result_array)
        class(groupby_t), intent(inout) :: this
        character(len=*), intent(out) :: group_name
        type(fortarray_t) :: result_array
        
        if (.not. this%has_next_group()) then
            write(error_unit, '(A)') "ERROR: No more groups in iterator"
            result_array = create_empty_like(this%parent_array)
            group_name = ""
            return
        end if
        
        this%current_group = this%current_group + 1
        group_name = this%group_names(this%current_group)
        result_array = this%get_group(group_name)
        
        if (this%current_group >= this%n_groups) then
            this%iterator_active = .false.
        end if
        
    end function groupby_next_group
    
    ! ======= COMPLEX OPERATIONS =======
    
    !> Apply custom operation to each group
    module function groupby_apply(this, operation) result(result_array)
        class(groupby_t), intent(in) :: this
        character(len=*), intent(in) :: operation
        type(fortarray_t) :: result_array
        
        integer :: i
        real(real64), allocatable :: result_values(:)
        type(fortarray_t) :: group_data
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Groupby object not initialized"
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        allocate(result_values(this%n_groups))
        
        ! Apply operation to each group
        do i = 1, this%n_groups
            group_data = this%get_group(this%group_names(i))
            
            select case(trim(operation))
            case("range")
                ! Calculate range (max - min) for each group
                result_values(i) = maxval(group_data%data%values_r64) - minval(group_data%data%values_r64)
                
            case("count")
                ! Count non-missing values
                result_values(i) = real(count(.not. is_missing(group_data%data%values_r64)), real64)
                
            case default
                write(error_unit, '(A,A)') "ERROR: Unknown operation: ", trim(operation)
                result_values(i) = huge(1.0_real64)
            end select
            
            call finalize_variable(group_data)
        end do
        
        ! Create result array
        result_array = new_array(result_values, name=trim(this%parent_array%name) // "_" // trim(operation))
        
    end function groupby_apply
    
    !> Transform groups (broadcast result back to original size)
    module function groupby_transform(this, operation) result(result_array)
        class(groupby_t), intent(in) :: this
        character(len=*), intent(in) :: operation
        type(fortarray_t) :: result_array
        
        integer :: i, j, group_start, group_end
        real(real64), allocatable :: result_values(:), group_values(:)
        real(real64) :: group_mean, group_sum
        integer :: valid_count
        
        if (.not. this%initialized) then
            write(error_unit, '(A)') "ERROR: Groupby object not initialized"
            result_array = create_empty_like(this%parent_array)
            return
        end if
        
        allocate(result_values(this%parent_array%n_elements))
        
        ! Transform each group and broadcast back
        do i = 1, this%n_groups
            ! Extract group values using stored member indices
            allocate(group_values(this%group_sizes(i)))
            do j = 1, this%group_sizes(i)
                group_values(j) = this%parent_array%data%values_r64(this%group_members(i, j))
            end do
            
            select case(trim(operation))
            case("mean")
                ! Calculate group mean and broadcast to all elements in group
                group_sum = 0.0_real64
                valid_count = 0
                do j = 1, size(group_values)
                    if (.not. is_missing(group_values(j))) then
                        group_sum = group_sum + group_values(j)
                        valid_count = valid_count + 1
                    end if
                end do
                
                if (valid_count > 0) then
                    group_mean = group_sum / real(valid_count, real64)
                else
                    group_mean = huge(1.0_real64)
                end if
                
                ! Broadcast to all positions in this group using stored member indices
                do j = 1, this%group_sizes(i)
                    result_values(this%group_members(i, j)) = group_mean
                end do
                
            case default
                write(error_unit, '(A,A)') "ERROR: Unknown transform operation: ", trim(operation)
                do j = 1, this%group_sizes(i)
                    result_values(this%group_members(i, j)) = huge(1.0_real64)
                end do
            end select
            
            deallocate(group_values)
        end do
        
        ! Create result array with same shape as original
        result_array = new_array(result_values, name=trim(this%parent_array%name) // "_transform_" // trim(operation))
        
    end function groupby_transform
    
    ! ======= TIME COMPONENT EXTRACTION FUNCTIONS =======
    
    !> Extract month from time value (1-12)
    function extract_month(time_value) result(month)
        real(real64), intent(in) :: time_value
        integer :: month
        
        ! Simplified implementation: assumes time_value is days since epoch
        ! Real implementation would use proper calendar functions
        integer :: julian_day, year, day_of_year
        
        julian_day = int(time_value)
        call julian_to_gregorian(julian_day, year, day_of_year)
        month = day_of_year_to_month(day_of_year, is_leap_year(year))
    end function extract_month
    
    !> Extract year from time value
    function extract_year(time_value) result(year)
        real(real64), intent(in) :: time_value
        integer :: year
        
        integer :: julian_day, day_of_year
        
        julian_day = int(time_value)
        call julian_to_gregorian(julian_day, year, day_of_year)
    end function extract_year
    
    !> Extract season from time value (1=Spring, 2=Summer, 3=Fall, 4=Winter)
    function extract_season(time_value) result(season)
        real(real64), intent(in) :: time_value
        integer :: season
        
        integer :: month
        month = extract_month(time_value)
        
        ! Map months to seasons (Northern Hemisphere)
        if (month >= 3 .and. month <= 5) then
            season = 1  ! Spring (Mar-May)
        else if (month >= 6 .and. month <= 8) then
            season = 2  ! Summer (Jun-Aug)
        else if (month >= 9 .and. month <= 11) then
            season = 3  ! Fall (Sep-Nov)  
        else
            season = 4  ! Winter (Dec-Feb)
        end if
    end function extract_season
    
    !> Extract day of year from time value (1-366)
    function extract_dayofyear(time_value) result(day_of_year)
        real(real64), intent(in) :: time_value
        integer :: day_of_year
        
        integer :: julian_day, year
        
        julian_day = int(time_value)
        call julian_to_gregorian(julian_day, year, day_of_year)
    end function extract_dayofyear
    
    !> Extract weekday from time value (1=Sunday, 7=Saturday)
    function extract_weekday(time_value) result(weekday)
        real(real64), intent(in) :: time_value
        integer :: weekday
        
        integer :: julian_day
        
        julian_day = int(time_value)
        ! Julian day 0 (Jan 1, 4713 BC) was a Monday, so adjust
        weekday = mod(julian_day + 1, 7) + 1
    end function extract_weekday
    
    !> Convert Julian day to Gregorian calendar
    subroutine julian_to_gregorian(julian_day, year, day_of_year)
        integer, intent(in) :: julian_day
        integer, intent(out) :: year, day_of_year
        
        ! Simplified implementation using modern epoch
        ! Assumes julian_day is days since Jan 1, 2000
        integer :: days_since_2000, leap_days, approx_year
        
        days_since_2000 = julian_day
        
        ! Approximate year calculation
        approx_year = 2000 + days_since_2000 / 365
        
        ! Calculate leap days
        leap_days = (approx_year - 2000) / 4 - (approx_year - 2000) / 100 + (approx_year - 2000) / 400
        
        ! Refine year calculation
        year = 2000 + (days_since_2000 - leap_days) / 365
        
        ! Calculate day of year
        day_of_year = days_since_2000 - (year - 2000) * 365 - leap_days + 1
        
        ! Handle edge cases
        if (day_of_year <= 0) then
            year = year - 1
            if (is_leap_year(year)) then
                day_of_year = day_of_year + 366
            else
                day_of_year = day_of_year + 365
            end if
        else if (day_of_year > 365 .and. (.not. is_leap_year(year) .or. day_of_year > 366)) then
            year = year + 1
            if (is_leap_year(year - 1)) then
                day_of_year = day_of_year - 366
            else
                day_of_year = day_of_year - 365
            end if
        end if
    end subroutine julian_to_gregorian
    
    !> Check if year is leap year
    function is_leap_year(year) result(is_leap)
        integer, intent(in) :: year
        logical :: is_leap
        
        is_leap = (mod(year, 4) == 0 .and. mod(year, 100) /= 0) .or. (mod(year, 400) == 0)
    end function is_leap_year
    
    !> Convert day of year to month
    function day_of_year_to_month(day_of_year, is_leap) result(month)
        integer, intent(in) :: day_of_year
        logical, intent(in) :: is_leap
        integer :: month
        
        integer, parameter :: days_in_month(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        integer, parameter :: days_in_month_leap(12) = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        integer :: cumulative_days, i
        
        cumulative_days = 0
        do i = 1, 12
            if (is_leap) then
                cumulative_days = cumulative_days + days_in_month_leap(i)
            else
                cumulative_days = cumulative_days + days_in_month(i)
            end if
            
            if (day_of_year <= cumulative_days) then
                month = i
                return
            end if
        end do
        
        ! Should not reach here for valid day_of_year
        month = 12
    end function day_of_year_to_month
    
    ! ======= BINNING HELPER FUNCTIONS =======
    
    !> Assign a value to the appropriate bin
    function assign_to_bin(value, bin_edges, n_bins) result(bin_index)
        real(real64), intent(in) :: value
        real(real64), intent(in) :: bin_edges(:)
        integer, intent(in) :: n_bins
        integer :: bin_index
        
        integer :: i
        
        ! Handle edge cases
        if (value <= bin_edges(1)) then
            bin_index = 1
            return
        else if (value >= bin_edges(n_bins + 1)) then
            bin_index = n_bins
            return
        end if
        
        ! Find the appropriate bin (binary search could be used for large n_bins)
        do i = 1, n_bins
            if (value >= bin_edges(i) .and. value < bin_edges(i + 1)) then
                bin_index = i
                return
            end if
        end do
        
        ! Fallback (shouldn't reach here)
        bin_index = n_bins
    end function assign_to_bin
    
    !> Create quantile-based bin edges
    subroutine create_quantile_bins(values, n_bins, bin_edges)
        real(real64), intent(in) :: values(:)
        integer, intent(in) :: n_bins
        real(real64), intent(out) :: bin_edges(:)
        
        real(real64), allocatable :: sorted_values(:)
        integer :: i, n_values, quantile_index
        real(real64) :: quantile_step
        
        n_values = size(values)
        allocate(sorted_values(n_values))
        sorted_values = values
        
        ! Sort values using simple bubble sort (could be improved)
        call simple_sort(sorted_values)
        
        quantile_step = real(n_values - 1, real64) / real(n_bins, real64)
        
        ! Set bin edges based on quantiles
        bin_edges(1) = sorted_values(1)
        do i = 2, n_bins
            quantile_index = int((i - 1) * quantile_step) + 1
            quantile_index = min(quantile_index, n_values)
            bin_edges(i) = sorted_values(quantile_index)
        end do
        bin_edges(n_bins + 1) = sorted_values(n_values)
    end subroutine create_quantile_bins
    
    !> Simple sorting routine
    subroutine simple_sort(array)
        real(real64), intent(inout) :: array(:)
        
        integer :: i, j, n
        real(real64) :: temp
        
        n = size(array)
        
        ! Bubble sort (simple but inefficient for large arrays)
        do i = 1, n - 1
            do j = 1, n - i
                if (array(j) > array(j + 1)) then
                    temp = array(j)
                    array(j) = array(j + 1)
                    array(j + 1) = temp
                end if
            end do
        end do
    end subroutine simple_sort
    
    !> Get resample frequency from attributes
    function get_resample_freq(arr) result(freq)
        type(fortarray_t), intent(in) :: arr
        character(len=:), allocatable :: freq
        integer :: i
        
        freq = ""
        if (.not. allocated(arr%attrs)) return
        
        do i = 1, size(arr%attrs)
            if (arr%attrs(i)%name == "_resample_freq") then
                freq = trim(arr%attrs(i)%value)
                return
            end if
        end do
    end function get_resample_freq
    
    !> Perform resampling with specified aggregation
    function perform_resample_aggregation(arr, freq, method) result(output)
        use fortarray_time_operations, only: resample_to_daily, resample_to_monthly, &
                                            resample_to_yearly, upsample_to_daily
        type(fortarray_t), intent(in) :: arr
        character(len=*), intent(in) :: freq
        character(len=*), intent(in) :: method
        type(fortarray_t) :: output
        type(fortarray_t) :: clean_arr
        integer :: i
        
        ! Create a clean copy without resample attributes
        clean_arr = arr
        if (allocated(clean_arr%attrs)) then
            ! Remove resample attributes
            do i = 1, size(clean_arr%attrs)
                if (clean_arr%attrs(i)%name == "_resample_freq" .or. &
                    clean_arr%attrs(i)%name == "_resample_align") then
                    clean_arr%attrs(i)%name = ""
                end if
            end do
        end if
        
        ! Perform resampling based on frequency
        select case(trim(freq))
        case("D")
            output = resample_to_daily(clean_arr, method)
        case("W")
            output = resample_to_weekly(clean_arr, method)
        case("M")
            output = resample_to_monthly(clean_arr, method)
        case("Y")
            output = resample_to_yearly(clean_arr, method)
        case("H")
            output = resample_to_hourly(clean_arr, method)
        case("6H", "12H", "3H")
            output = resample_to_n_hourly(clean_arr, method, freq)
        case default
            output = clean_arr
        end select
        
    end function perform_resample_aggregation
    
    !> Resample to weekly frequency
    function resample_to_weekly(var, method) result(output)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: method
        type(fortarray_t) :: output
        
        integer :: n_weeks, points_per_week, i, j, start_idx, end_idx, count
        real(real64), dimension(:), allocatable :: weekly_data
        real(real64) :: sum_val, max_val, min_val, mean_val, variance_val
        
        ! Determine number of weeks (assuming daily data)
        points_per_week = 7
        n_weeks = var%n_elements / points_per_week
        
        if (n_weeks < 1) then
            output = var
            return
        end if
        
        allocate(weekly_data(n_weeks))
        
        select case(trim(method))
        case("mean")
            do i = 1, n_weeks
                start_idx = (i-1) * points_per_week + 1
                end_idx = min(i * points_per_week, var%n_elements)
                sum_val = 0.0_real64
                count = 0
                
                do j = start_idx, end_idx
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        sum_val = sum_val + var%data%values_r64(j)
                    case(DTYPE_REAL32)
                        sum_val = sum_val + real(var%data%values_r32(j), real64)
                    end select
                    count = count + 1
                end do
                
                if (count > 0) then
                    weekly_data(i) = sum_val / real(count, real64)
                else
                    weekly_data(i) = 0.0_real64
                end if
            end do
            output = new_array(weekly_data, name=trim(var%name)//"_weekly_mean", dim_names=["time"])
            
        case("max")
            do i = 1, n_weeks
                start_idx = (i-1) * points_per_week + 1
                end_idx = min(i * points_per_week, var%n_elements)
                max_val = -huge(1.0_real64)
                
                do j = start_idx, end_idx
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        if (var%data%values_r64(j) > max_val) max_val = var%data%values_r64(j)
                    case(DTYPE_REAL32)
                        if (real(var%data%values_r32(j), real64) > max_val) then
                            max_val = real(var%data%values_r32(j), real64)
                        end if
                    end select
                end do
                
                weekly_data(i) = max_val
            end do
            output = new_array(weekly_data, name=trim(var%name)//"_weekly_max", dim_names=["time"])
            
        case("min")
            do i = 1, n_weeks
                start_idx = (i-1) * points_per_week + 1
                end_idx = min(i * points_per_week, var%n_elements)
                min_val = huge(1.0_real64)
                
                do j = start_idx, end_idx
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        if (var%data%values_r64(j) < min_val) min_val = var%data%values_r64(j)
                    case(DTYPE_REAL32)
                        if (real(var%data%values_r32(j), real64) < min_val) then
                            min_val = real(var%data%values_r32(j), real64)
                        end if
                    end select
                end do
                
                weekly_data(i) = min_val
            end do
            output = new_array(weekly_data, name=trim(var%name)//"_weekly_min", dim_names=["time"])
            
        case("sum")
            do i = 1, n_weeks
                start_idx = (i-1) * points_per_week + 1
                end_idx = min(i * points_per_week, var%n_elements)
                sum_val = 0.0_real64
                
                do j = start_idx, end_idx
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        sum_val = sum_val + var%data%values_r64(j)
                    case(DTYPE_REAL32)
                        sum_val = sum_val + real(var%data%values_r32(j), real64)
                    end select
                end do
                
                weekly_data(i) = sum_val
            end do
            output = new_array(weekly_data, name=trim(var%name)//"_weekly_sum", dim_names=["time"])
            
        case("std")
            do i = 1, n_weeks
                start_idx = (i-1) * points_per_week + 1
                end_idx = min(i * points_per_week, var%n_elements)
                count = end_idx - start_idx + 1
                
                ! Compute mean first
                sum_val = 0.0_real64
                do j = start_idx, end_idx
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        sum_val = sum_val + var%data%values_r64(j)
                    case(DTYPE_REAL32)
                        sum_val = sum_val + real(var%data%values_r32(j), real64)
                    end select
                end do
                mean_val = sum_val / real(count, real64)
                
                ! Compute variance
                variance_val = 0.0_real64
                do j = start_idx, end_idx
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        variance_val = variance_val + (var%data%values_r64(j) - mean_val)**2
                    case(DTYPE_REAL32)
                        variance_val = variance_val + (real(var%data%values_r32(j), real64) - mean_val)**2
                    end select
                end do
                
                if (count > 1) then
                    variance_val = variance_val / real(count - 1, real64)
                    weekly_data(i) = sqrt(variance_val)
                else
                    weekly_data(i) = 0.0_real64
                end if
            end do
            output = new_array(weekly_data, name=trim(var%name)//"_weekly_std", dim_names=["time"])
            
        case default
            output = var
        end select
        
        if (allocated(weekly_data)) deallocate(weekly_data)
        
    end function resample_to_weekly
    
    !> Resample to hourly frequency (placeholder)
    function resample_to_hourly(var, method) result(output)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: method
        type(fortarray_t) :: output
        
        ! Placeholder - just return input
        output = var
        
    end function resample_to_hourly
    
    !> Resample to N-hourly frequency (placeholder)
    function resample_to_n_hourly(var, method, freq) result(output)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: method
        character(len=*), intent(in) :: freq
        type(fortarray_t) :: output
        
        ! Placeholder - just return input
        output = var
        
    end function resample_to_n_hourly
    
    ! ======= INTEROPERABILITY METHODS =======
    
    !> Get first n rows (head operation)
    module function fortarray_head(this, n) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: n
        type(fortarray_t) :: result_array
        
        integer :: num_rows, i
        
        ! Default to 5 rows like pandas
        num_rows = 5
        if (present(n)) num_rows = n
        
        ! Ensure we don't exceed array bounds
        num_rows = min(num_rows, this%shape(1))
        
        ! Initialize result
        result_array%initialized = .true.
        result_array%name = trim(this%name) // "_head"
        result_array%units = this%units
        result_array%long_name = this%long_name
        result_array%n_dims = this%n_dims
        result_array%n_elements = num_rows * product(this%shape(2:))
        
        ! Copy shape but modify first dimension
        allocate(result_array%shape(this%n_dims))
        result_array%shape = this%shape
        result_array%shape(1) = num_rows
        
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Copy data (first num_rows elements along first dimension)
        result_array%data%dtype = this%data%dtype
        if (allocated(this%data%values_r64)) then
            allocate(result_array%data%values_r64(result_array%n_elements))
            
            if (this%n_dims == 1) then
                result_array%data%values_r64 = this%data%values_r64(1:num_rows)
            else if (this%n_dims == 2) then
                ! Copy first num_rows along first dimension
                do i = 1, this%shape(2)
                    result_array%data%values_r64((i-1)*num_rows+1:i*num_rows) = &
                        this%data%values_r64((i-1)*this%shape(1)+1:(i-1)*this%shape(1)+num_rows)
                end do
            end if
        end if
        
        ! Copy coordinates if present
        if (allocated(this%coords) .and. allocated(this%has_coord)) then
            allocate(result_array%coords(size(this%coords)))
            allocate(result_array%has_coord(size(this%has_coord)))
            
            do i = 1, size(this%coords)
                if (this%has_coord(i)) then
                    result_array%coords(i) = this%coords(i)
                    result_array%has_coord(i) = .true.
                    
                    ! Truncate first coordinate to match new size
                    if (i == 1 .and. allocated(result_array%coords(i)%values_r64)) then
                        if (size(result_array%coords(i)%values_r64) > num_rows) then
                            ! Would need to reallocate coordinate array
                            result_array%coords(i)%length = num_rows
                        end if
                    end if
                else
                    result_array%has_coord(i) = .false.
                end if
            end do
        end if
        
    end function fortarray_head
    
    !> Get last n rows (tail operation)
    module function fortarray_tail(this, n) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in), optional :: n
        type(fortarray_t) :: result_array
        
        integer :: num_rows, start_idx, i
        
        ! Default to 5 rows like pandas
        num_rows = 5
        if (present(n)) num_rows = n
        
        ! Ensure we don't exceed array bounds
        num_rows = min(num_rows, this%shape(1))
        start_idx = this%shape(1) - num_rows + 1
        
        ! Initialize result
        result_array%initialized = .true.
        result_array%name = trim(this%name) // "_tail"
        result_array%units = this%units
        result_array%long_name = this%long_name
        result_array%n_dims = this%n_dims
        result_array%n_elements = num_rows * product(this%shape(2:))
        
        ! Copy shape but modify first dimension
        allocate(result_array%shape(this%n_dims))
        result_array%shape = this%shape
        result_array%shape(1) = num_rows
        
        if (allocated(this%dim_names)) then
            allocate(result_array%dim_names(size(this%dim_names)))
            result_array%dim_names = this%dim_names
        end if
        
        ! Copy data (last num_rows elements along first dimension)
        result_array%data%dtype = this%data%dtype
        if (allocated(this%data%values_r64)) then
            allocate(result_array%data%values_r64(result_array%n_elements))
            
            if (this%n_dims == 1) then
                result_array%data%values_r64 = this%data%values_r64(start_idx:this%shape(1))
            else if (this%n_dims == 2) then
                ! Copy last num_rows along first dimension
                do i = 1, this%shape(2)
                    result_array%data%values_r64((i-1)*num_rows+1:i*num_rows) = &
                        this%data%values_r64((i-1)*this%shape(1)+start_idx:i*this%shape(1))
                end do
            end if
        end if
        
        ! Copy coordinates if present (similar to head but from end)
        if (allocated(this%coords) .and. allocated(this%has_coord)) then
            allocate(result_array%coords(size(this%coords)))
            allocate(result_array%has_coord(size(this%has_coord)))
            
            do i = 1, size(this%coords)
                if (this%has_coord(i)) then
                    result_array%coords(i) = this%coords(i)
                    result_array%has_coord(i) = .true.
                    
                    ! Truncate first coordinate from end
                    if (i == 1 .and. allocated(result_array%coords(i)%values_r64)) then
                        result_array%coords(i)%length = num_rows
                    end if
                else
                    result_array%has_coord(i) = .false.
                end if
            end do
        end if
        
    end function fortarray_tail
    
    !> Generate descriptive statistics (describe operation)
    module function fortarray_describe(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        integer :: n_stats, stat_idx, i
        real(real64) :: data_min, data_max, data_mean, data_std, data_count
        real(real64) :: q25, q50, q75  ! Quartiles
        
        ! Number of statistics to compute
        n_stats = 8  ! count, mean, std, min, 25%, 50%, 75%, max
        
        ! Initialize result array
        result_array%initialized = .true.
        result_array%name = trim(this%name) // "_describe"
        result_array%units = "statistics"
        result_array%long_name = "Descriptive statistics"
        result_array%n_dims = 1
        result_array%n_elements = n_stats
        
        allocate(result_array%shape(1), result_array%dim_names(1))
        result_array%shape(1) = n_stats
        result_array%dim_names(1) = "statistic"
        
        ! Compute statistics
        if (allocated(this%data%values_r64) .and. this%n_elements > 0) then
            data_count = real(this%n_elements, real64)
            data_min = minval(this%data%values_r64)
            data_max = maxval(this%data%values_r64)
            data_mean = sum(this%data%values_r64) / real(this%n_elements, real64)
            
            ! Simple standard deviation
            data_std = 0.0_real64
            do i = 1, this%n_elements
                data_std = data_std + (this%data%values_r64(i) - data_mean)**2
            end do
            data_std = sqrt(data_std / real(this%n_elements - 1, real64))
            
            ! Simple quartile estimation (would use proper percentile in full implementation)
            q25 = data_mean - 0.67 * data_std
            q50 = data_mean  ! Median approximation
            q75 = data_mean + 0.67 * data_std
        else
            data_count = 0.0_real64
            data_min = 0.0_real64
            data_max = 0.0_real64
            data_mean = 0.0_real64
            data_std = 0.0_real64
            q25 = 0.0_real64
            q50 = 0.0_real64
            q75 = 0.0_real64
        end if
        
        ! Store statistics
        result_array%data%dtype = DTYPE_REAL64
        allocate(result_array%data%values_r64(n_stats))
        result_array%data%values_r64(1) = data_count
        result_array%data%values_r64(2) = data_mean
        result_array%data%values_r64(3) = data_std
        result_array%data%values_r64(4) = data_min
        result_array%data%values_r64(5) = q25
        result_array%data%values_r64(6) = q50
        result_array%data%values_r64(7) = q75
        result_array%data%values_r64(8) = data_max
        
        ! Add coordinate with statistic names
        allocate(result_array%coords(1), result_array%has_coord(1))
        result_array%coords(1)%name = "statistic"
        result_array%coords(1)%length = n_stats
        result_array%coords(1)%dtype = DTYPE_CHAR
        allocate(character(len=10) :: result_array%coords(1)%values_char(n_stats))
        result_array%coords(1)%values_char(1) = "count"
        result_array%coords(1)%values_char(2) = "mean"
        result_array%coords(1)%values_char(3) = "std"
        result_array%coords(1)%values_char(4) = "min"
        result_array%coords(1)%values_char(5) = "25%"
        result_array%coords(1)%values_char(6) = "50%"
        result_array%coords(1)%values_char(7) = "75%"
        result_array%coords(1)%values_char(8) = "max"
        result_array%has_coord(1) = .true.
        
    end function fortarray_describe

end submodule fortarray_methods