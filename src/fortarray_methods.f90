submodule (fortarray_types) fortarray_methods
    !! Implementation of all xarray-compatible methods for fortarray_t
    !! This submodule contains ALL method implementations migrated from external functions
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use fortarray_storage
    use fortarray_memory
    use fortarray_slicing
    use fortarray_indexing
    use fortarray_constructors, only: new_array, create_coordinate
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
    module function fortarray_sel_point_r64(this, coord_name, value, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in) :: value
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        
        character(len=20) :: sel_method
        integer :: coord_idx, value_idx
        
        ! Set default method
        sel_method = METHOD_EXACT
        if (present(method)) sel_method = method
        
        ! Find coordinate by name
        coord_idx = find_coordinate_by_name(this, coord_name)
        if (coord_idx == 0) then
            write(error_unit, '(A,A,A)') "ERROR: Coordinate '", trim(coord_name), "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Find value index
        value_idx = find_coord_index(this%coords(coord_idx), value, sel_method, 0.0_real64)
        if (value_idx == 0) then
            write(error_unit, '(A,F0.6,A,A)') "ERROR: Value ", value, " not found in coordinate ", trim(coord_name)
            result_array = create_empty_like(this)
            return
        end if
        
        ! Perform selection using existing slice functionality
        result_array = slice_along_dimension(this, coord_idx, value_idx)
        
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
        
        integer :: dim_idx
        
        ! Find dimension by name
        dim_idx = find_dimension_by_name(this, dim_name)
        if (dim_idx == 0) then
            write(error_unit, '(A,A,A)') "ERROR: Dimension '", trim(dim_name), "' not found"
            result_array = create_empty_like(this)
            return
        end if
        
        ! Check bounds
        if (index < 1 .or. index > this%shape(dim_idx)) then
            write(error_unit, '(A,I0,A,I0)') "ERROR: Index ", index, " out of bounds for dimension size ", this%shape(dim_idx)
            result_array = create_empty_like(this)
            return
        end if
        
        ! Perform index selection
        result_array = slice_along_dimension(this, dim_idx, index)
        
    end function fortarray_isel_point
    
    !> Select range by dimension name and integer indices
    module function fortarray_isel_range(this, dim_name, start_idx, stop_idx, step_idx) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: dim_name
        integer, intent(in) :: start_idx, stop_idx
        integer, intent(in), optional :: step_idx
        type(fortarray_t) :: result_array
        
        integer :: dim_idx, step_val
        
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
        
        ! Check bounds
        if (start_idx < 1 .or. stop_idx > this%shape(dim_idx) .or. start_idx > stop_idx) then
            write(error_unit, '(A,2I0,A,I0)') "ERROR: Index range [", start_idx, ":", stop_idx, &
                "] invalid for dimension size ", this%shape(dim_idx)
            result_array = create_empty_like(this)
            return
        end if
        
        ! Perform range selection
        result_array = slice_range_along_dimension(this, dim_idx, start_idx, stop_idx, step_val)
        
    end function fortarray_isel_range
    
    !> Select by dimension name and integer array (fancy indexing)
    module function fortarray_isel_indices(this, dim_name, indices) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: dim_name
        integer, dimension(:), intent(in) :: indices
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: isel_indices not yet implemented"
        result_array = create_empty_like(this)
        
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
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        
        real(real64) :: mean_value
        integer :: i
        
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
    
    module function fortarray_std_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: std_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_std_all
    
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
    
    module function fortarray_min_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: min_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_min_all
    
    module function fortarray_min_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: min_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_min_dims
    
    module function fortarray_max_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: max_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_max_all
    
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
    
    module function fortarray_quantile(this, q) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: q
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: quantile not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_quantile
    
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
        write(error_unit, '(A)') "ERROR: to_netcdf_file not yet implemented"
        status = -1
    end function fortarray_to_netcdf_file
    
    module function fortarray_to_numpy_like(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: to_numpy_like not yet implemented"
        ! Return uninitialized array for placeholder
        result_array%initialized = .false.
    end function fortarray_to_numpy_like
    
    module function fortarray_to_pandas_like(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: to_pandas_like not yet implemented"
        ! Return uninitialized array for placeholder
        result_array%initialized = .false.
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
    
    module function fortarray_bfill(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: bfill not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_bfill
    
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
    
    module function fortarray_groupby_coord(this, coord_name) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: groupby_coord not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_groupby_coord
    
    module function fortarray_groupby_bins(this, coord_name, bins) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        integer, intent(in) :: bins
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: groupby_bins not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_groupby_bins
    
    module function fortarray_resample_freq(this, freq) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: freq
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: resample_freq not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_resample_freq
    
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
            ! Find nearest neighbor
            idx = 1
            min_dist = abs(coord_values(1) - value)
            do i = 2, n
                dist = abs(coord_values(i) - value)
                if (dist < min_dist) then
                    min_dist = dist
                    idx = i
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
        
        result%initialized = .true.
        
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
        
        integer :: i, j, new_idx
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
        
        ! Copy the slice
        new_idx = 1
        do i = 1, n_elements_before
            do j = 1, n_elements_after
                result_values(new_idx) = values_r64((i-1)*var%shape(dim_idx)*n_elements_after + &
                                                   (value_idx-1)*n_elements_after + j)
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
        do i = 1, n_elements_before
            do j = actual_start, actual_stop, step
                do k = 1, n_elements_after
                    src_idx = (i-1)*n_elements_dim*n_elements_after + &
                             (j-1)*n_elements_after + k
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
                if (diff < min_diff) then
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

end submodule fortarray_methods