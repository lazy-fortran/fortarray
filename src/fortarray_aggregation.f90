module fortarray_aggregation
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors
    use fortarray_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic
    implicit none
    private
    
    ! Public interfaces
    public :: sum, mean, minval, maxval
    public :: std, variance
    public :: median, quantile
    
    ! Generic sum interface
    interface sum
        module procedure sum_all
        module procedure sum_along_dim
        module procedure sum_along_dims
        module procedure sum_weighted
    end interface sum
    
    ! Generic mean interface
    interface mean
        module procedure mean_all
        module procedure mean_along_dim
        module procedure mean_along_dims
        module procedure mean_weighted
    end interface mean
    
    ! Generic min interface
    interface minval
        module procedure minval_all
        module procedure minval_along_dim
        module procedure minval_along_dims
    end interface minval
    
    ! Generic max interface
    interface maxval
        module procedure maxval_all
        module procedure maxval_along_dim
        module procedure maxval_along_dims
    end interface maxval
    
    ! Generic std interface
    interface std
        module procedure std_all
        module procedure std_along_dim
        module procedure std_along_dims
    end interface std
    
    ! Generic variance interface
    interface variance
        module procedure variance_all
        module procedure variance_along_dim
        module procedure variance_along_dims
    end interface variance
    
    ! Generic median interface
    interface median
        module procedure median_all
        module procedure median_along_dim
        module procedure median_along_dims
    end interface median
    
    ! Generic quantile interface
    interface quantile
        module procedure quantile_all
        module procedure quantile_along_dim
        module procedure quantile_along_dims
    end interface quantile
    
contains

    !> Check if a value is missing (using huge as sentinel)
    elemental function is_missing(val) result(missing)
        real(real64), intent(in) :: val
        logical :: missing
        real(real64), parameter :: sentinel = huge(1.0_real64)
        
        missing = abs(val - sentinel) < 1e-10
    end function is_missing
    
    !> Get valid (non-missing) values from array
    function get_valid_values(values, skipna) result(valid_values)
        real(real64), dimension(:), intent(in) :: values
        logical, intent(in) :: skipna
        real(real64), dimension(:), allocatable :: valid_values
        integer :: i, n_valid
        
        if (.not. skipna) then
            allocate(valid_values(size(values)))
            valid_values = values
            return
        end if
        
        ! Count valid values
        n_valid = 0
        do i = 1, size(values)
            if (.not. is_missing(values(i))) then
                n_valid = n_valid + 1
            end if
        end do
        
        ! Extract valid values
        allocate(valid_values(n_valid))
        n_valid = 0
        do i = 1, size(values)
            if (.not. is_missing(values(i))) then
                n_valid = n_valid + 1
                valid_values(n_valid) = values(i)
            end if
        end do
        
    end function get_valid_values
    
    !> Convert variable data to real64 for aggregation
    function get_values_as_real64(var) result(values)
        type(fortarray_t), intent(in) :: var
        real(real64), dimension(:), allocatable :: values
        
        allocate(values(var%n_elements))
        
        ! Handle empty arrays
        if (var%n_elements == 0) then
            return
        end if
        
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
            write(error_unit,'(A,I0)') "ERROR: Unsupported data type for aggregation: ", var%data%dtype
            stop 1
        end select
    end function get_values_as_real64
    
    !> Calculate new shape after removing dimensions
    function get_reduced_shape(shape, dims, keepdims) result(new_shape)
        integer, dimension(:), intent(in) :: shape
        integer, dimension(:), intent(in) :: dims
        logical, intent(in) :: keepdims
        integer, dimension(:), allocatable :: new_shape
        integer :: i, j, n_kept
        logical :: is_removed
        
        if (keepdims) then
            ! Keep dimensions but set to 1
            allocate(new_shape(size(shape)))
            new_shape = shape
            do i = 1, size(dims)
                if (dims(i) > 0 .and. dims(i) <= size(shape)) then
                    new_shape(dims(i)) = 1
                end if
            end do
        else
            ! Remove dimensions
            n_kept = size(shape) - size(dims)
            allocate(new_shape(n_kept))
            
            j = 0
            do i = 1, size(shape)
                is_removed = any(dims == i)
                if (.not. is_removed) then
                    j = j + 1
                    new_shape(j) = shape(i)
                end if
            end do
        end if
        
    end function get_reduced_shape
    
    !> Create result variable with appropriate type
    function create_result_variable(result_value, orig_dtype, name, shape) result(var)
        real(real64), intent(in) :: result_value
        integer, intent(in) :: orig_dtype
        character(len=*), intent(in) :: name
        integer, dimension(:), intent(in), optional :: shape
        type(fortarray_t) :: var
        
        ! Always create scalar result
        select case(orig_dtype)
        case(DTYPE_INT32)
            var = variable_scalar(int(result_value, int32), name=name)
        case(DTYPE_INT64)
            var = variable_scalar(int(result_value, int64), name=name)
        case(DTYPE_REAL32)
            var = variable_scalar(real(result_value, real32), name=name)
        case default
            var = variable_scalar(result_value, name=name)
        end select
        
    end function create_result_variable
    
    !======= SUM Functions =======!
    
    !> Sum all elements
    function sum_all(var, skipna) result(result)
        type(fortarray_t), intent(in) :: var
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, valid_values
        real(real64) :: sum_val
        logical :: skip_missing
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        values = get_values_as_real64(var)
        valid_values = get_valid_values(values, skip_missing)
        
        if (size(valid_values) == 0) then
            sum_val = 0.0_real64
        else if (.not. skip_missing .and. any(is_missing(values))) then
            sum_val = huge(1.0_real64)
        else
            sum_val = sum(valid_values)
        end if
        
        result = create_result_variable(sum_val, var%data%dtype, "sum")
        
    end function sum_all
    
    !> Sum along single dimension
    function sum_along_dim(var, dim, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        
        result = sum_along_dims(var, [dim], skipna, keepdims)
        
    end function sum_along_dim
    
    !> Sum along multiple dimensions
    function sum_along_dims(var, dims, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: dims
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer, dimension(:), allocatable :: new_shape, result_shape
        integer :: i, j, n_result
        logical :: skip_missing, keep_dims
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        keep_dims = .false.
        if (present(keepdims)) keep_dims = keepdims
        
        ! Get values and calculate new shape
        values = get_values_as_real64(var)
        new_shape = get_reduced_shape(var%shape, dims, keep_dims)
        n_result = product(new_shape)
        
        ! Perform aggregation along dimensions
        allocate(result_values(n_result))
        
        ! For now, implement simple case - sum along one dimension
        if (size(dims) == 1 .and. dims(1) > 0 .and. dims(1) <= size(var%shape)) then
            call aggregate_along_dimension(values, var%shape, dims(1), 'sum', &
                                         skip_missing, result_values)
        else
            ! Multi-dimension aggregation - not implemented yet
            result_values = sum(values)
        end if
        
        ! Create result variable
        if (size(new_shape) > 0) then
            result = create_variable_array(result_values, var%data%dtype, "sum", new_shape)
        else
            result = create_result_variable(result_values(1), var%data%dtype, "sum")
        end if
        
    end function sum_along_dims
    
    !> Weighted sum
    function sum_weighted(var, weights, skipna) result(result)
        type(fortarray_t), intent(in) :: var, weights
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, weight_vals, valid_values, valid_weights
        real(real64) :: sum_val
        logical :: skip_missing
        integer :: i, n_valid
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        values = get_values_as_real64(var)
        weight_vals = get_values_as_real64(weights)
        
        if (size(values) /= size(weight_vals)) then
            write(error_unit,'(A)') "ERROR: Values and weights must have same size"
            stop 1
        end if
        
        if (skip_missing) then
            ! Count valid pairs
            n_valid = 0
            do i = 1, size(values)
                if (.not. is_missing(values(i)) .and. .not. is_missing(weight_vals(i))) then
                    n_valid = n_valid + 1
                end if
            end do
            
            allocate(valid_values(n_valid), valid_weights(n_valid))
            n_valid = 0
            do i = 1, size(values)
                if (.not. is_missing(values(i)) .and. .not. is_missing(weight_vals(i))) then
                    n_valid = n_valid + 1
                    valid_values(n_valid) = values(i)
                    valid_weights(n_valid) = weight_vals(i)
                end if
            end do
            
            sum_val = sum(valid_values * valid_weights)
        else
            if (any(is_missing(values)) .or. any(is_missing(weight_vals))) then
                sum_val = huge(1.0_real64)
            else
                sum_val = sum(values * weight_vals)
            end if
        end if
        
        result = create_result_variable(sum_val, var%data%dtype, "weighted_sum")
        
    end function sum_weighted
    
    !======= MEAN Functions =======!
    
    !> Mean of all elements
    function mean_all(var, skipna) result(result)
        type(fortarray_t), intent(in) :: var
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, valid_values
        real(real64) :: mean_val
        logical :: skip_missing
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        values = get_values_as_real64(var)
        valid_values = get_valid_values(values, skip_missing)
        
        if (size(valid_values) == 0) then
            mean_val = ieee_value(1.0_real64, ieee_quiet_nan)
        else if (.not. skip_missing .and. any(is_missing(values))) then
            mean_val = huge(1.0_real64)
        else
            mean_val = sum(valid_values) / real(size(valid_values), real64)
        end if
        
        result = create_result_variable(mean_val, var%data%dtype, "mean")
        
    end function mean_all
    
    !> Mean along single dimension
    function mean_along_dim(var, dim, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        
        result = mean_along_dims(var, [dim], skipna, keepdims)
        
    end function mean_along_dim
    
    !> Mean along multiple dimensions
    function mean_along_dims(var, dims, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: dims
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer, dimension(:), allocatable :: new_shape
        integer :: n_result
        logical :: skip_missing, keep_dims
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        keep_dims = .false.
        if (present(keepdims)) keep_dims = keepdims
        
        values = get_values_as_real64(var)
        new_shape = get_reduced_shape(var%shape, dims, keep_dims)
        n_result = product(new_shape)
        
        allocate(result_values(n_result))
        
        if (size(dims) == 1 .and. dims(1) > 0 .and. dims(1) <= size(var%shape)) then
            call aggregate_along_dimension(values, var%shape, dims(1), 'mean', &
                                         skip_missing, result_values)
        else
            result_values = sum(values) / real(size(values), real64)
        end if
        
        if (size(new_shape) > 0) then
            result = create_variable_array(result_values, var%data%dtype, "mean", new_shape)
        else
            result = create_result_variable(result_values(1), var%data%dtype, "mean")
        end if
        
    end function mean_along_dims
    
    !> Weighted mean
    function mean_weighted(var, weights, skipna) result(result)
        type(fortarray_t), intent(in) :: var, weights
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, weight_vals
        real(real64) :: weighted_sum, weight_sum, mean_val
        logical :: skip_missing
        integer :: i
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        values = get_values_as_real64(var)
        weight_vals = get_values_as_real64(weights)
        
        if (size(values) /= size(weight_vals)) then
            write(error_unit,'(A)') "ERROR: Values and weights must have same size"
            stop 1
        end if
        
        weighted_sum = 0.0_real64
        weight_sum = 0.0_real64
        
        do i = 1, size(values)
            if (skip_missing) then
                if (.not. is_missing(values(i)) .and. .not. is_missing(weight_vals(i))) then
                    weighted_sum = weighted_sum + values(i) * weight_vals(i)
                    weight_sum = weight_sum + weight_vals(i)
                end if
            else
                if (is_missing(values(i)) .or. is_missing(weight_vals(i))) then
                    mean_val = huge(1.0_real64)
                    result = create_result_variable(mean_val, var%data%dtype, "weighted_mean")
                    return
                end if
                weighted_sum = weighted_sum + values(i) * weight_vals(i)
                weight_sum = weight_sum + weight_vals(i)
            end if
        end do
        
        if (weight_sum > 0.0_real64) then
            mean_val = weighted_sum / weight_sum
        else
            mean_val = ieee_value(1.0_real64, ieee_quiet_nan)
        end if
        
        result = create_result_variable(mean_val, var%data%dtype, "weighted_mean")
        
    end function mean_weighted
    
    !======= MIN/MAX Functions =======!
    
    !> Minimum of all elements
    function minval_all(var, skipna) result(result)
        type(fortarray_t), intent(in) :: var
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, valid_values
        real(real64) :: min_val
        logical :: skip_missing
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        values = get_values_as_real64(var)
        valid_values = get_valid_values(values, skip_missing)
        
        if (size(valid_values) == 0) then
            min_val = huge(1.0_real64)
        else if (.not. skip_missing .and. any(is_missing(values))) then
            min_val = huge(1.0_real64)
        else
            min_val = minval(valid_values)
        end if
        
        result = create_result_variable(min_val, var%data%dtype, "min")
        
    end function minval_all
    
    !> Minimum along dimension
    function minval_along_dim(var, dim, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        
        result = minval_along_dims(var, [dim], skipna, keepdims)
        
    end function minval_along_dim
    
    !> Minimum along dimensions
    function minval_along_dims(var, dims, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: dims
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer, dimension(:), allocatable :: new_shape
        integer :: n_result
        logical :: skip_missing, keep_dims
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        keep_dims = .false.
        if (present(keepdims)) keep_dims = keepdims
        
        values = get_values_as_real64(var)
        new_shape = get_reduced_shape(var%shape, dims, keep_dims)
        n_result = product(new_shape)
        
        allocate(result_values(n_result))
        
        if (size(dims) == 1 .and. dims(1) > 0 .and. dims(1) <= size(var%shape)) then
            call aggregate_along_dimension(values, var%shape, dims(1), 'min', &
                                         skip_missing, result_values)
        else
            result_values = minval(values)
        end if
        
        if (size(new_shape) > 0) then
            result = create_variable_array(result_values, var%data%dtype, "min", new_shape)
        else
            result = create_result_variable(result_values(1), var%data%dtype, "min")
        end if
        
    end function minval_along_dims
    
    !> Maximum of all elements
    function maxval_all(var, skipna) result(result)
        type(fortarray_t), intent(in) :: var
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, valid_values
        real(real64) :: max_val
        logical :: skip_missing
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        values = get_values_as_real64(var)
        valid_values = get_valid_values(values, skip_missing)
        
        if (size(valid_values) == 0) then
            max_val = -huge(1.0_real64)
        else if (.not. skip_missing .and. any(is_missing(values))) then
            max_val = huge(1.0_real64)
        else
            max_val = maxval(valid_values)
        end if
        
        result = create_result_variable(max_val, var%data%dtype, "max")
        
    end function maxval_all
    
    !> Maximum along dimension
    function maxval_along_dim(var, dim, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        
        result = maxval_along_dims(var, [dim], skipna, keepdims)
        
    end function maxval_along_dim
    
    !> Maximum along dimensions
    function maxval_along_dims(var, dims, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: dims
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer, dimension(:), allocatable :: new_shape
        integer :: n_result
        logical :: skip_missing, keep_dims
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        keep_dims = .false.
        if (present(keepdims)) keep_dims = keepdims
        
        values = get_values_as_real64(var)
        new_shape = get_reduced_shape(var%shape, dims, keep_dims)
        n_result = product(new_shape)
        
        allocate(result_values(n_result))
        
        if (size(dims) == 1 .and. dims(1) > 0 .and. dims(1) <= size(var%shape)) then
            call aggregate_along_dimension(values, var%shape, dims(1), 'max', &
                                         skip_missing, result_values)
        else
            result_values = maxval(values)
        end if
        
        if (size(new_shape) > 0) then
            result = create_variable_array(result_values, var%data%dtype, "max", new_shape)
        else
            result = create_result_variable(result_values(1), var%data%dtype, "max")
        end if
        
    end function maxval_along_dims
    
    !======= VARIANCE/STD Functions =======!
    
    !> Variance of all elements
    function variance_all(var, skipna, ddof) result(result)
        type(fortarray_t), intent(in) :: var
        logical, intent(in), optional :: skipna
        integer, intent(in), optional :: ddof
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, valid_values
        real(real64) :: mean_val, var_val
        logical :: skip_missing
        integer :: degrees_of_freedom, n
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        degrees_of_freedom = 0
        if (present(ddof)) degrees_of_freedom = ddof
        
        values = get_values_as_real64(var)
        valid_values = get_valid_values(values, skip_missing)
        n = size(valid_values)
        
        if (n <= degrees_of_freedom) then
            var_val = ieee_value(1.0_real64, ieee_quiet_nan)
        else if (.not. skip_missing .and. any(is_missing(values))) then
            var_val = huge(1.0_real64)
        else
            mean_val = sum(valid_values) / real(n, real64)
            var_val = sum((valid_values - mean_val)**2) / real(n - degrees_of_freedom, real64)
        end if
        
        result = create_result_variable(var_val, var%data%dtype, "variance")
        
    end function variance_all
    
    !> Variance along dimension
    function variance_along_dim(var, dim, skipna, keepdims, ddof) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        logical, intent(in), optional :: skipna, keepdims
        integer, intent(in), optional :: ddof
        type(fortarray_t) :: result
        
        result = variance_along_dims(var, [dim], skipna, keepdims, ddof)
        
    end function variance_along_dim
    
    !> Variance along dimensions
    function variance_along_dims(var, dims, skipna, keepdims, ddof) result(result)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: dims
        logical, intent(in), optional :: skipna, keepdims
        integer, intent(in), optional :: ddof
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer, dimension(:), allocatable :: new_shape
        integer :: n_result, degrees_of_freedom
        logical :: skip_missing, keep_dims
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        keep_dims = .false.
        if (present(keepdims)) keep_dims = keepdims
        degrees_of_freedom = 0
        if (present(ddof)) degrees_of_freedom = ddof
        
        values = get_values_as_real64(var)
        new_shape = get_reduced_shape(var%shape, dims, keep_dims)
        n_result = product(new_shape)
        
        allocate(result_values(n_result))
        
        if (size(dims) == 1 .and. dims(1) > 0 .and. dims(1) <= size(var%shape)) then
            call aggregate_along_dimension(values, var%shape, dims(1), 'variance', &
                                         skip_missing, result_values, ddof=degrees_of_freedom)
        else
            ! Simple implementation for now
            block
                type(fortarray_t) :: temp_result
                temp_result = variance_all(var, skip_missing, degrees_of_freedom)
                result_values = temp_result%data%values_r64(1)
                call finalize_variable(temp_result)
            end block
        end if
        
        if (size(new_shape) > 0) then
            result = create_variable_array(result_values, var%data%dtype, "variance", new_shape)
        else
            result = create_result_variable(result_values(1), var%data%dtype, "variance")
        end if
        
    end function variance_along_dims
    
    !> Standard deviation of all elements
    function std_all(var, skipna, ddof) result(result)
        type(fortarray_t), intent(in) :: var
        logical, intent(in), optional :: skipna
        integer, intent(in), optional :: ddof
        type(fortarray_t) :: result
        type(fortarray_t) :: var_result
        real(real64) :: std_val
        
        var_result = variance_all(var, skipna, ddof)
        if (var_result%n_elements > 0) then
            std_val = sqrt(var_result%data%values_r64(1))
        else
            std_val = huge(1.0_real64)
        end if
        
        result = create_result_variable(std_val, var%data%dtype, "std")
        call finalize_variable(var_result)
        
    end function std_all
    
    !> Standard deviation along dimension
    function std_along_dim(var, dim, skipna, keepdims, ddof) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        logical, intent(in), optional :: skipna, keepdims
        integer, intent(in), optional :: ddof
        type(fortarray_t) :: result
        
        result = std_along_dims(var, [dim], skipna, keepdims, ddof)
        
    end function std_along_dim
    
    !> Standard deviation along dimensions
    function std_along_dims(var, dims, skipna, keepdims, ddof) result(result)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: dims
        logical, intent(in), optional :: skipna, keepdims
        integer, intent(in), optional :: ddof
        type(fortarray_t) :: result
        type(fortarray_t) :: var_result
        real(real64), dimension(:), allocatable :: std_values
        integer :: i
        
        var_result = variance_along_dims(var, dims, skipna, keepdims, ddof)
        
        allocate(std_values(var_result%n_elements))
        do i = 1, var_result%n_elements
            std_values(i) = sqrt(var_result%data%values_r64(i))
        end do
        
        if (allocated(var_result%shape)) then
            result = create_variable_array(std_values, var%data%dtype, "std", var_result%shape)
        else
            result = create_result_variable(std_values(1), var%data%dtype, "std")
        end if
        
        call finalize_variable(var_result)
        
    end function std_along_dims
    
    !======= MEDIAN Functions =======!
    
    !> Median of all elements
    function median_all(var, skipna) result(result)
        type(fortarray_t), intent(in) :: var
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, valid_values, sorted_values
        real(real64) :: median_val
        logical :: skip_missing
        integer :: n
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        values = get_values_as_real64(var)
        valid_values = get_valid_values(values, skip_missing)
        n = size(valid_values)
        
        if (n == 0) then
            median_val = ieee_value(1.0_real64, ieee_quiet_nan)
        else if (.not. skip_missing .and. any(is_missing(values))) then
            median_val = huge(1.0_real64)
        else
            ! Sort values
            allocate(sorted_values(n))
            sorted_values = valid_values
            call quicksort(sorted_values, 1, n)
            
            ! Calculate median
            if (mod(n, 2) == 1) then
                median_val = sorted_values((n + 1) / 2)
            else
                median_val = (sorted_values(n / 2) + sorted_values(n / 2 + 1)) / 2.0_real64
            end if
        end if
        
        result = create_result_variable(median_val, var%data%dtype, "median")
        
    end function median_all
    
    !> Median along dimension
    function median_along_dim(var, dim, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        
        result = median_along_dims(var, [dim], skipna, keepdims)
        
    end function median_along_dim
    
    !> Median along dimensions
    function median_along_dims(var, dims, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: dims
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer, dimension(:), allocatable :: new_shape
        integer :: n_result
        logical :: skip_missing, keep_dims
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        keep_dims = .false.
        if (present(keepdims)) keep_dims = keepdims
        
        values = get_values_as_real64(var)
        new_shape = get_reduced_shape(var%shape, dims, keep_dims)
        n_result = product(new_shape)
        
        allocate(result_values(n_result))
        
        if (size(dims) == 1 .and. dims(1) > 0 .and. dims(1) <= size(var%shape)) then
            call aggregate_along_dimension(values, var%shape, dims(1), 'median', &
                                         skip_missing, result_values)
        else
            block
                type(fortarray_t) :: temp_result
                temp_result = median_all(var, skip_missing)
                result_values = temp_result%data%values_r64(1)
                call finalize_variable(temp_result)
            end block
        end if
        
        if (size(new_shape) > 0) then
            result = create_variable_array(result_values, var%data%dtype, "median", new_shape)
        else
            result = create_result_variable(result_values(1), var%data%dtype, "median")
        end if
        
    end function median_along_dims
    
    !======= QUANTILE Functions =======!
    
    !> Quantile of all elements
    function quantile_all(var, q, skipna) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: q
        logical, intent(in), optional :: skipna
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, valid_values, sorted_values
        real(real64) :: quantile_val, pos
        logical :: skip_missing
        integer :: n, lower_idx, upper_idx
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        
        if (q < 0.0_real64 .or. q > 1.0_real64) then
            write(error_unit,'(A)') "ERROR: Quantile must be between 0 and 1"
            stop 1
        end if
        
        values = get_values_as_real64(var)
        valid_values = get_valid_values(values, skip_missing)
        n = size(valid_values)
        
        if (n == 0) then
            quantile_val = ieee_value(1.0_real64, ieee_quiet_nan)
        else if (.not. skip_missing .and. any(is_missing(values))) then
            quantile_val = huge(1.0_real64)
        else
            ! Sort values
            allocate(sorted_values(n))
            sorted_values = valid_values
            call quicksort(sorted_values, 1, n)
            
            ! Calculate quantile using linear interpolation
            pos = q * real(n - 1, real64) + 1.0_real64
            lower_idx = int(pos)
            upper_idx = min(lower_idx + 1, n)
            
            if (lower_idx == upper_idx) then
                quantile_val = sorted_values(lower_idx)
            else
                quantile_val = sorted_values(lower_idx) + &
                              (pos - real(lower_idx, real64)) * &
                              (sorted_values(upper_idx) - sorted_values(lower_idx))
            end if
        end if
        
        result = create_result_variable(quantile_val, var%data%dtype, "quantile")
        
    end function quantile_all
    
    !> Quantile along dimension
    function quantile_along_dim(var, q, dim, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: q
        integer, intent(in) :: dim
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        
        result = quantile_along_dims(var, q, [dim], skipna, keepdims)
        
    end function quantile_along_dim
    
    !> Quantile along dimensions
    function quantile_along_dims(var, q, dims, skipna, keepdims) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: q
        integer, dimension(:), intent(in) :: dims
        logical, intent(in), optional :: skipna, keepdims
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer, dimension(:), allocatable :: new_shape
        integer :: n_result
        logical :: skip_missing, keep_dims
        
        skip_missing = .true.
        if (present(skipna)) skip_missing = skipna
        keep_dims = .false.
        if (present(keepdims)) keep_dims = keepdims
        
        values = get_values_as_real64(var)
        new_shape = get_reduced_shape(var%shape, dims, keep_dims)
        n_result = product(new_shape)
        
        allocate(result_values(n_result))
        
        ! For now, simple implementation
        block
            type(fortarray_t) :: temp_result
            temp_result = quantile_all(var, q, skip_missing)
            result_values = temp_result%data%values_r64(1)
            call finalize_variable(temp_result)
        end block
        
        if (size(new_shape) > 0) then
            result = create_variable_array(result_values, var%data%dtype, "quantile", new_shape)
        else
            result = create_result_variable(result_values(1), var%data%dtype, "quantile")
        end if
        
    end function quantile_along_dims
    
    !======= Helper Functions =======!
    
    !> Quicksort for real64 arrays
    recursive subroutine quicksort(arr, left, right)
        real(real64), dimension(:), intent(inout) :: arr
        integer, intent(in) :: left, right
        integer :: i, j
        real(real64) :: pivot, temp
        
        if (left < right) then
            pivot = arr((left + right) / 2)
            i = left
            j = right
            
            do while (i <= j)
                do while (arr(i) < pivot)
                    i = i + 1
                end do
                do while (arr(j) > pivot)
                    j = j - 1
                end do
                if (i <= j) then
                    temp = arr(i)
                    arr(i) = arr(j)
                    arr(j) = temp
                    i = i + 1
                    j = j - 1
                end if
            end do
            
            if (left < j) call quicksort(arr, left, j)
            if (i < right) call quicksort(arr, i, right)
        end if
    end subroutine quicksort
    
    !> Create variable from array with given shape
    function create_variable_array(values, dtype, name, var_shape) result(var)
        real(real64), dimension(:), intent(in) :: values
        integer, intent(in) :: dtype
        character(len=*), intent(in) :: name
        integer, dimension(:), intent(in) :: var_shape
        type(fortarray_t) :: var
        character(len=64), dimension(:), allocatable :: dim_names
        integer :: i
        
        ! Generate dimension names
        allocate(dim_names(size(var_shape)))
        do i = 1, size(var_shape)
            write(dim_names(i), '(A,I0)') "dim", i
        end do
        
        ! Create variable based on dimensions
        select case(size(var_shape))
        case(1)
            select case(dtype)
            case(DTYPE_INT32)
                var = variable(int(values(1:var_shape(1)), int32), name=name, dim_names=dim_names)
            case(DTYPE_INT64)
                var = variable(int(values(1:var_shape(1)), int64), name=name, dim_names=dim_names)
            case(DTYPE_REAL32)
                var = variable(real(values(1:var_shape(1)), real32), name=name, dim_names=dim_names)
            case default
                var = variable(values(1:var_shape(1)), name=name, dim_names=dim_names)
            end select
        case default
            ! For all dimensions >= 2, create manually
            var%name = name
            var%n_elements = product(var_shape)
            var%n_dims = size(var_shape)
            allocate(var%shape(size(var_shape)))
            var%shape = var_shape
            allocate(var%dim_names(size(var_shape)))
            var%dim_names = dim_names
            var%data%dtype = dtype
            
            select case(dtype)
            case(DTYPE_INT32)
                allocate(var%data%values_i32(var%n_elements))
                var%data%values_i32 = int(values, int32)
            case(DTYPE_INT64)
                allocate(var%data%values_i64(var%n_elements))
                var%data%values_i64 = int(values, int64)
            case(DTYPE_REAL32)
                allocate(var%data%values_r32(var%n_elements))
                var%data%values_r32 = real(values, real32)
            case default
                allocate(var%data%values_r64(var%n_elements))
                var%data%values_r64 = values
            end select
        end select
        
    end function create_variable_array
    
    !> Aggregate along a single dimension
    subroutine aggregate_along_dimension(values, shape, dim, operation, skipna, result, ddof)
        real(real64), dimension(:), intent(in) :: values
        integer, dimension(:), intent(in) :: shape
        integer, intent(in) :: dim
        character(len=*), intent(in) :: operation
        logical, intent(in) :: skipna
        real(real64), dimension(:), intent(out) :: result
        integer, intent(in), optional :: ddof
        
        integer :: i, j, k, idx, result_idx
        integer :: n_dims, stride, n_elements_dim
        integer, dimension(:), allocatable :: indices, strides
        real(real64), dimension(:), allocatable :: dim_values
        real(real64) :: agg_val
        
        n_dims = size(shape)
        n_elements_dim = shape(dim)
        
        ! Calculate strides for column-major order
        allocate(strides(n_dims))
        strides(1) = 1
        do i = 2, n_dims
            strides(i) = strides(i-1) * shape(i-1)
        end do
        
        stride = strides(dim)
        
        ! Allocate workspace
        allocate(indices(n_dims))
        allocate(dim_values(n_elements_dim))
        
        ! Iterate over all result elements
        result_idx = 0
        do i = 1, product(shape) / n_elements_dim
            result_idx = result_idx + 1
            
            ! Calculate indices for this result element
            k = i - 1
            do j = n_dims, 1, -1
                if (j /= dim) then
                    if (j < dim) then
                        indices(j) = mod(k, shape(j)) + 1
                        k = k / shape(j)
                    else
                        indices(j) = mod(k, shape(j)) + 1
                        k = k / shape(j)
                    end if
                end if
            end do
            
            ! Collect values along dimension
            do j = 1, n_elements_dim
                indices(dim) = j
                idx = 1
                do k = 1, n_dims
                    idx = idx + (indices(k) - 1) * strides(k)
                end do
                dim_values(j) = values(idx)
            end do
            
            ! Apply aggregation
            select case(operation)
            case('sum')
                if (skipna) then
                    agg_val = 0.0_real64
                    do j = 1, n_elements_dim
                        if (.not. is_missing(dim_values(j))) then
                            agg_val = agg_val + dim_values(j)
                        end if
                    end do
                else
                    if (any(is_missing(dim_values))) then
                        agg_val = huge(1.0_real64)
                    else
                        agg_val = sum(dim_values)
                    end if
                end if
            case('mean')
                if (skipna) then
                    agg_val = 0.0_real64
                    k = 0
                    do j = 1, n_elements_dim
                        if (.not. is_missing(dim_values(j))) then
                            agg_val = agg_val + dim_values(j)
                            k = k + 1
                        end if
                    end do
                    if (k > 0) then
                        agg_val = agg_val / real(k, real64)
                    else
                        agg_val = ieee_value(1.0_real64, ieee_quiet_nan)
                    end if
                else
                    if (any(is_missing(dim_values))) then
                        agg_val = huge(1.0_real64)
                    else
                        agg_val = sum(dim_values) / real(n_elements_dim, real64)
                    end if
                end if
            case('min')
                if (skipna) then
                    agg_val = huge(1.0_real64)
                    do j = 1, n_elements_dim
                        if (.not. is_missing(dim_values(j))) then
                            agg_val = min(agg_val, dim_values(j))
                        end if
                    end do
                else
                    if (any(is_missing(dim_values))) then
                        agg_val = huge(1.0_real64)
                    else
                        agg_val = minval(dim_values)
                    end if
                end if
            case('max')
                if (skipna) then
                    agg_val = -huge(1.0_real64)
                    do j = 1, n_elements_dim
                        if (.not. is_missing(dim_values(j))) then
                            agg_val = max(agg_val, dim_values(j))
                        end if
                    end do
                else
                    if (any(is_missing(dim_values))) then
                        agg_val = huge(1.0_real64)
                    else
                        agg_val = maxval(dim_values)
                    end if
                end if
            case('median')
                block
                    real(real64), dimension(:), allocatable :: valid_vals
                    integer :: n_valid
                    
                    if (skipna) then
                        n_valid = count(.not. is_missing(dim_values))
                        allocate(valid_vals(n_valid))
                        k = 0
                        do j = 1, n_elements_dim
                            if (.not. is_missing(dim_values(j))) then
                                k = k + 1
                                valid_vals(k) = dim_values(j)
                            end if
                        end do
                        
                        if (n_valid > 0) then
                            call quicksort(valid_vals, 1, n_valid)
                            if (mod(n_valid, 2) == 1) then
                                agg_val = valid_vals((n_valid + 1) / 2)
                            else
                                agg_val = (valid_vals(n_valid / 2) + valid_vals(n_valid / 2 + 1)) / 2.0_real64
                            end if
                        else
                            agg_val = ieee_value(1.0_real64, ieee_quiet_nan)
                        end if
                    else
                        if (any(is_missing(dim_values))) then
                            agg_val = huge(1.0_real64)
                        else
                            allocate(valid_vals(n_elements_dim))
                            valid_vals = dim_values
                            call quicksort(valid_vals, 1, n_elements_dim)
                            if (mod(n_elements_dim, 2) == 1) then
                                agg_val = valid_vals((n_elements_dim + 1) / 2)
                            else
                                agg_val = (valid_vals(n_elements_dim / 2) + valid_vals(n_elements_dim / 2 + 1)) / 2.0_real64
                            end if
                        end if
                    end if
                end block
            case('variance')
                block
                    real(real64) :: mean_val
                    integer :: degrees_of_freedom, n_valid
                    
                    degrees_of_freedom = 0
                    if (present(ddof)) degrees_of_freedom = ddof
                    
                    if (skipna) then
                        mean_val = 0.0_real64
                        n_valid = 0
                        do j = 1, n_elements_dim
                            if (.not. is_missing(dim_values(j))) then
                                mean_val = mean_val + dim_values(j)
                                n_valid = n_valid + 1
                            end if
                        end do
                        
                        if (n_valid > degrees_of_freedom) then
                            mean_val = mean_val / real(n_valid, real64)
                            agg_val = 0.0_real64
                            do j = 1, n_elements_dim
                                if (.not. is_missing(dim_values(j))) then
                                    agg_val = agg_val + (dim_values(j) - mean_val)**2
                                end if
                            end do
                            agg_val = agg_val / real(n_valid - degrees_of_freedom, real64)
                        else
                            agg_val = ieee_value(1.0_real64, ieee_quiet_nan)
                        end if
                    else
                        if (any(is_missing(dim_values))) then
                            agg_val = huge(1.0_real64)
                        else if (n_elements_dim > degrees_of_freedom) then
                            mean_val = sum(dim_values) / real(n_elements_dim, real64)
                            agg_val = sum((dim_values - mean_val)**2) / real(n_elements_dim - degrees_of_freedom, real64)
                        else
                            agg_val = ieee_value(1.0_real64, ieee_quiet_nan)
                        end if
                    end if
                end block
            case default
                agg_val = 0.0_real64
            end select
            
            result(result_idx) = agg_val
        end do
        
    end subroutine aggregate_along_dimension
    
end module fortarray_aggregation