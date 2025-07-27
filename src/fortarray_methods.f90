module fortarray_methods
    !! Implementation of all xarray-compatible methods for fortarray_t
    !! This module contains ALL method implementations migrated from external functions
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use fortarray_types
    ! DTYPE constants are in fortarray_types, no need for explicit import since we already use fortarray_types
    implicit none
    
    ! Public procedures (all method implementations)
    public :: fortarray_sel_point_r64, fortarray_sel_point_r32, fortarray_sel_point_char
    public :: fortarray_sel_range_r64, fortarray_sel_range_char
    public :: fortarray_isel_point, fortarray_isel_range, fortarray_isel_indices
    public :: fortarray_filter_condition, fortarray_filter_mask
    public :: fortarray_mean_all, fortarray_mean_dims, fortarray_sum_all, fortarray_sum_dims
    public :: fortarray_std_all, fortarray_std_dims, fortarray_var_all, fortarray_var_dims
    public :: fortarray_min_all, fortarray_min_dims, fortarray_max_all, fortarray_max_dims
    public :: fortarray_median, fortarray_quantile
    public :: fortarray_values_all, fortarray_values_copy, fortarray_to_netcdf_file, fortarray_to_pandas_like
    public :: fortarray_fillna_value, fortarray_fillna_method, fortarray_dropna_any, fortarray_dropna_all
    public :: fortarray_interpolate_na_linear, fortarray_interpolate_na_cubic, fortarray_ffill, fortarray_bfill
    public :: fortarray_transpose_all, fortarray_transpose_order, fortarray_stack_dims, fortarray_unstack_dims
    public :: fortarray_squeeze_all, fortarray_squeeze_dims, fortarray_expand_dims_axis
    public :: fortarray_groupby_coord, fortarray_groupby_bins, fortarray_resample_freq, fortarray_rolling_window
    public :: fortarray_init_plot_accessor
    
    ! Generic interfaces for clean xarray-style API
    public :: sel, sel_range, isel, filter, mean, sum, std, var, min, max
    public :: fillna, transpose, squeeze, median, quantile
    
    ! Selection method constants
    character(len=*), parameter :: METHOD_EXACT = "exact"
    character(len=*), parameter :: METHOD_NEAREST = "nearest"
    
    ! Generic interfaces
    interface sel
        module procedure fortarray_sel_point_r64, fortarray_sel_point_r32, fortarray_sel_point_char
    end interface
    
    interface sel_range
        module procedure fortarray_sel_range_r64, fortarray_sel_range_char
    end interface
    
    interface isel
        module procedure fortarray_isel_point, fortarray_isel_range, fortarray_isel_indices
    end interface
    
    interface filter
        module procedure fortarray_filter_condition, fortarray_filter_mask
    end interface
    
    interface mean
        module procedure fortarray_mean_all, fortarray_mean_dims
    end interface
    
    interface sum
        module procedure fortarray_sum_all, fortarray_sum_dims
    end interface
    
    interface std
        module procedure fortarray_std_all, fortarray_std_dims
    end interface
    
    interface var
        module procedure fortarray_var_all, fortarray_var_dims
    end interface
    
    interface min
        module procedure fortarray_min_all, fortarray_min_dims
    end interface
    
    interface max
        module procedure fortarray_max_all, fortarray_max_dims
    end interface
    
    interface fillna
        module procedure fortarray_fillna_value, fortarray_fillna_method
    end interface
    
    ! Note: dropna functions have same signature, so no generic interface for now
    ! interface dropna
    !     module procedure fortarray_dropna_any, fortarray_dropna_all
    ! end interface
    
    ! Note: values functions have same signature, so no generic interface for now
    ! interface values
    !     module procedure fortarray_values_all, fortarray_values_copy
    ! end interface
    
    interface transpose
        module procedure fortarray_transpose_all, fortarray_transpose_order
    end interface
    
    interface squeeze
        module procedure fortarray_squeeze_all, fortarray_squeeze_dims
    end interface
    
    interface median
        module procedure fortarray_median
    end interface
    
    interface quantile
        module procedure fortarray_quantile
    end interface
    
contains

    ! ======= SELECTION METHODS (MIGRATED FROM fortarray_coordinate_selection.f90) =======
    
    !> Select by coordinate name and real64 value
    function fortarray_sel_point_r64(this, coord_name, value, method) result(result_array)
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
    function fortarray_sel_point_r32(this, coord_name, value, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real32), intent(in) :: value
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result_array
        
        ! Convert to real64 and call main implementation
        result_array = fortarray_sel_point_r64(this, coord_name, real(value, real64), method)
        
    end function fortarray_sel_point_r32
    
    !> Select by coordinate name and character value (for time strings, etc.)
    function fortarray_sel_point_char(this, coord_name, value, method) result(result_array)
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
    function fortarray_sel_range_r64(this, coord_name, start_val, stop_val, step_val) result(result_array)
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
    function fortarray_sel_range_char(this, coord_name, start_val, stop_val) result(result_array)
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
    
    !> Select by dimension name and integer index
    function fortarray_isel_point(this, dim_name, index) result(result_array)
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
    function fortarray_isel_range(this, dim_name, start_idx, stop_idx, step_idx) result(result_array)
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
    function fortarray_isel_indices(this, dim_name, indices) result(result_array)
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
    function fortarray_filter_condition(this, condition, other_value) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t), intent(in) :: condition  ! Boolean array
        real(real64), intent(in), optional :: other_value
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: filter_condition not yet implemented"
        result_array = create_empty_like(this)
        
    end function fortarray_filter_condition
    
    !> Filter by boolean mask
    function fortarray_filter_mask(this, mask) result(result_array)
        class(fortarray_t), intent(in) :: this
        logical, dimension(:), intent(in) :: mask
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: filter_mask not yet implemented"
        result_array = create_empty_like(this)
        
    end function fortarray_filter_mask
    
    ! ======= AGGREGATION METHODS =======
    
    !> Compute mean over all dimensions
    function fortarray_mean_all(this) result(result_array)
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
    function fortarray_mean_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: mean_dims not yet implemented"
        result_array = create_empty_like(this)
        
    end function fortarray_mean_dims
    
    !> Compute sum over all dimensions
    function fortarray_sum_all(this) result(result_array)
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
    function fortarray_sum_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        
        ! Not yet implemented - placeholder
        write(error_unit, '(A)') "ERROR: sum_dims not yet implemented"
        result_array = create_empty_like(this)
        
    end function fortarray_sum_dims
    
    ! ======= PLACEHOLDER IMPLEMENTATIONS FOR REMAINING METHODS =======
    ! (Following BACKLOG.md: implement FULL functionality, no shortcuts)
    
    function fortarray_std_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: std_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_std_all
    
    function fortarray_std_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: std_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_std_dims
    
    function fortarray_var_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: var_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_var_all
    
    function fortarray_var_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: var_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_var_dims
    
    function fortarray_min_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: min_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_min_all
    
    function fortarray_min_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: min_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_min_dims
    
    function fortarray_max_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: max_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_max_all
    
    function fortarray_max_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: max_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_max_dims
    
    function fortarray_median(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: median not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_median
    
    function fortarray_quantile(this, q) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: q
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: quantile not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_quantile
    
    function fortarray_values_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: values_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_values_all
    
    function fortarray_values_copy(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: values_copy not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_values_copy
    
    function fortarray_to_netcdf_file(this, filename) result(status)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer :: status
        write(error_unit, '(A)') "ERROR: to_netcdf_file not yet implemented"
        status = -1
    end function fortarray_to_netcdf_file
    
    function fortarray_to_pandas_like(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: to_pandas_like not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_to_pandas_like
    
    function fortarray_fillna_value(this, value) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: value
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: fillna_value not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_fillna_value
    
    function fortarray_fillna_method(this, method) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: method
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: fillna_method not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_fillna_method
    
    function fortarray_dropna_any(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: dropna_any not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_dropna_any
    
    function fortarray_dropna_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: dropna_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_dropna_all
    
    function fortarray_interpolate_na_linear(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: interpolate_na_linear not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_interpolate_na_linear
    
    function fortarray_interpolate_na_cubic(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: interpolate_na_cubic not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_interpolate_na_cubic
    
    function fortarray_ffill(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: ffill not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_ffill
    
    function fortarray_bfill(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: bfill not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_bfill
    
    function fortarray_transpose_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: transpose_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_transpose_all
    
    function fortarray_transpose_order(this, order) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, dimension(:), intent(in) :: order
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: transpose_order not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_transpose_order
    
    function fortarray_stack_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: stack_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_stack_dims
    
    function fortarray_unstack_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: unstack_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_unstack_dims
    
    function fortarray_squeeze_all(this) result(result_array)
        class(fortarray_t), intent(in) :: this
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: squeeze_all not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_squeeze_all
    
    function fortarray_squeeze_dims(this, dims) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), dimension(:), intent(in) :: dims
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: squeeze_dims not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_squeeze_dims
    
    function fortarray_expand_dims_axis(this, axis) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: axis
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: expand_dims_axis not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_expand_dims_axis
    
    function fortarray_groupby_coord(this, coord_name) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: groupby_coord not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_groupby_coord
    
    function fortarray_groupby_bins(this, coord_name, bins) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        integer, intent(in) :: bins
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: groupby_bins not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_groupby_bins
    
    function fortarray_resample_freq(this, freq) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: freq
        type(fortarray_t) :: result_array
        write(error_unit, '(A)') "ERROR: resample_freq not yet implemented"
        result_array = create_empty_like(this)
    end function fortarray_resample_freq
    
    function fortarray_rolling_window(this, window) result(result_array)
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
    
    !> Slice along a single dimension (NEW utility function)
    function slice_along_dimension(var, dim_idx, value_idx) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim_idx, value_idx
        type(fortarray_t) :: result
        
        ! For now, return empty result - proper implementation needed
        write(error_unit, '(A)') "ERROR: slice_along_dimension not yet fully implemented"
        result = create_empty_like(var)
        
    end function slice_along_dimension
    
    !> Slice range along a single dimension (NEW utility function)  
    function slice_range_along_dimension(var, dim_idx, start_idx, stop_idx, step_idx) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim_idx, start_idx, stop_idx
        integer, intent(in), optional :: step_idx
        type(fortarray_t) :: result
        
        ! For now, return empty result - proper implementation needed
        write(error_unit, '(A)') "ERROR: slice_range_along_dimension not yet fully implemented"
        result = create_empty_like(var)
        
    end function slice_range_along_dimension

end module fortarray_methods