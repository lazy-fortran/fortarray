module fortarray_apply_functions
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors, only: new_array, new_dataset, &
        variable_scalar_real64, variable_scalar_real32, variable_scalar_int32, variable_scalar_int64
    use fortarray_memory
    use fortarray_aggregation
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Public interfaces
    public :: apply_along_dim, apply_function, apply_vectorized, apply_cumulative
    public :: apply_elementwise, apply_reduction, apply_broadcast
    
    ! Function pointer interface for user-defined functions
    abstract interface
        function scalar_function_interface(x) result(y)
            import :: real64
            real(real64), intent(in) :: x
            real(real64) :: y
        end function scalar_function_interface
        
        function array_function_interface(x) result(y)
            import :: real64
            real(real64), dimension(:), intent(in) :: x
            real(real64) :: y
        end function array_function_interface
    end interface
    
    public :: scalar_function_interface, array_function_interface
    
    ! Built-in function identifiers
    character(len=*), parameter :: FUNC_SUM = "sum"
    character(len=*), parameter :: FUNC_MEAN = "mean"
    character(len=*), parameter :: FUNC_MIN = "min"
    character(len=*), parameter :: FUNC_MAX = "max"
    character(len=*), parameter :: FUNC_STD = "std"
    character(len=*), parameter :: FUNC_VAR = "var"
    character(len=*), parameter :: FUNC_SQRT = "sqrt"
    character(len=*), parameter :: FUNC_EXP = "exp"
    character(len=*), parameter :: FUNC_LOG = "log"
    character(len=*), parameter :: FUNC_SIN = "sin"
    character(len=*), parameter :: FUNC_COS = "cos"
    character(len=*), parameter :: FUNC_CUMSUM = "cumsum"
    character(len=*), parameter :: FUNC_CUMPROD = "cumprod"
    
    public :: FUNC_SUM, FUNC_MEAN, FUNC_MIN, FUNC_MAX, FUNC_STD, FUNC_VAR
    public :: FUNC_SQRT, FUNC_EXP, FUNC_LOG, FUNC_SIN, FUNC_COS
    public :: FUNC_CUMSUM, FUNC_CUMPROD
    
contains
    
    !> Apply function along specified dimension
    function apply_along_dim(var, dim, func_name) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        character(len=*), intent(in) :: func_name
        type(fortarray_t) :: result
        integer, dimension(:), allocatable :: result_shape
        integer :: result_size, i, j, k
        real(real64), dimension(:), allocatable :: slice_data, result_data
        
        ! Validate input
        if (dim < 1 .or. dim > var%n_dims) then
            write(error_unit,'(A,I0)') "ERROR: Invalid dimension: ", dim
            result = create_empty_like(var)
            return
        end if
        
        ! Calculate result shape (remove specified dimension)
        allocate(result_shape(var%n_dims - 1))
        j = 0
        do i = 1, var%n_dims
            if (i /= dim) then
                j = j + 1
                result_shape(j) = var%shape(i)
            end if
        end do
        
        result_size = product(result_shape)
        if (result_size == 0) result_size = 1
        
        ! Initialize result variable
        result%name = var%name // "_" // trim(func_name)
        result%n_dims = var%n_dims - 1
        result%n_elements = result_size
        
        if (result%n_dims > 0) then
            allocate(result%shape(result%n_dims))
            result%shape = result_shape
            
            allocate(result%dim_names(result%n_dims))
            j = 0
            do i = 1, var%n_dims
                if (i /= dim) then
                    j = j + 1
                    result%dim_names(j) = var%dim_names(i)
                end if
            end do
        end if
        
        ! Copy attributes
        result%units = var%units
        result%long_name = var%long_name
        result%standard_name = var%standard_name
        
        ! Initialize data storage
        result%data%dtype = var%data%dtype
        result%data%n_elements = result_size
        result%data%initialized = .true.
        
        ! Apply function along dimension
        allocate(result_data(result_size))
        
        select case(trim(func_name))
        case(FUNC_SUM)
            call apply_sum_along_dim(var, dim, result_data)
        case(FUNC_MEAN)
            call apply_mean_along_dim(var, dim, result_data)
        case(FUNC_MIN)
            call apply_min_along_dim(var, dim, result_data)
        case(FUNC_MAX)
            call apply_max_along_dim(var, dim, result_data)
        case(FUNC_STD)
            call apply_std_along_dim(var, dim, result_data)
        case(FUNC_VAR)
            call apply_var_along_dim(var, dim, result_data)
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown function: ", func_name
            result_data = 0.0_real64
        end select
        
        ! Store result data
        select case(result%data%dtype)
        case(DTYPE_REAL64)
            allocate(result%data%values_r64(result_size))
            result%data%values_r64 = result_data
        case(DTYPE_REAL32)
            allocate(result%data%values_r32(result_size))
            result%data%values_r32 = real(result_data, real32)
        case(DTYPE_INT32)
            allocate(result%data%values_i32(result_size))
            result%data%values_i32 = int(result_data, int32)
        case(DTYPE_INT64)
            allocate(result%data%values_i64(result_size))
            result%data%values_i64 = int(result_data, int64)
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for apply operation"
            result = create_empty_like(var)
            return
        end select
        
        result%initialized = .true.
        
    end function apply_along_dim
    
    !> Apply user-defined function element-wise
    function apply_function(var, func) result(result)
        type(fortarray_t), intent(in) :: var
        procedure(scalar_function_interface) :: func
        type(fortarray_t) :: result
        integer :: i
        real(real64), dimension(:), allocatable :: input_values, result_values
        
        ! Get input values as real64
        call get_variable_values_r64(var, input_values)
        
        ! Apply function element-wise
        allocate(result_values(var%n_elements))
        do i = 1, var%n_elements
            result_values(i) = func(input_values(i))
        end do
        
        ! Create result variable
        result = new_array(result_values, name=var%name//"_func", dim_names=var%dim_names)
        if (allocated(var%shape)) then
            if (allocated(result%shape)) deallocate(result%shape)
            allocate(result%shape(size(var%shape)))
            result%shape = var%shape
        end if
        
        ! Copy attributes and coordinates
        result%units = var%units
        result%long_name = var%long_name
        result%standard_name = var%standard_name
        
        if (allocated(var%coords) .and. allocated(var%has_coord)) then
            allocate(result%coords(var%n_dims))
            allocate(result%has_coord(var%n_dims))
            result%coords = var%coords
            result%has_coord = var%has_coord
        end if
        
    end function apply_function
    
    !> Apply vectorized operation
    function apply_vectorized(var, operation) result(result)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: operation
        type(fortarray_t) :: result
        integer :: i
        real(real64), dimension(:), allocatable :: input_values, result_values
        
        ! Get input values as real64
        call get_variable_values_r64(var, input_values)
        allocate(result_values(var%n_elements))
        
        ! Apply vectorized operation
        select case(trim(operation))
        case(FUNC_SQRT)
            do i = 1, var%n_elements
                result_values(i) = sqrt(input_values(i))
            end do
        case(FUNC_EXP)
            do i = 1, var%n_elements
                result_values(i) = exp(input_values(i))
            end do
        case(FUNC_LOG)
            do i = 1, var%n_elements
                result_values(i) = log(input_values(i))
            end do
        case(FUNC_SIN)
            do i = 1, var%n_elements
                result_values(i) = sin(input_values(i))
            end do
        case(FUNC_COS)
            do i = 1, var%n_elements
                result_values(i) = cos(input_values(i))
            end do
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown vectorized operation: ", operation
            result_values = input_values
        end select
        
        ! Create result variable
        result = new_array(result_values, name=var%name//"_"//trim(operation), dim_names=var%dim_names)
        if (allocated(var%shape)) then
            if (allocated(result%shape)) deallocate(result%shape)
            allocate(result%shape(size(var%shape)))
            result%shape = var%shape
        end if
        
        result%units = var%units
        result%long_name = var%long_name
        
    end function apply_vectorized
    
    !> Apply cumulative function
    function apply_cumulative(var, operation) result(result)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: operation
        type(fortarray_t) :: result
        integer :: i
        real(real64), dimension(:), allocatable :: input_values, result_values
        
        ! Get input values as real64
        call get_variable_values_r64(var, input_values)
        allocate(result_values(var%n_elements))
        
        ! Apply cumulative operation
        select case(trim(operation))
        case(FUNC_CUMSUM)
            result_values(1) = input_values(1)
            do i = 2, var%n_elements
                result_values(i) = result_values(i-1) + input_values(i)
            end do
        case(FUNC_CUMPROD)
            result_values(1) = input_values(1)
            do i = 2, var%n_elements
                result_values(i) = result_values(i-1) * input_values(i)
            end do
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown cumulative operation: ", operation
            result_values = input_values
        end select
        
        ! Create result variable
        result = new_array(result_values, name=var%name//"_"//trim(operation), dim_names=var%dim_names)
        if (allocated(var%shape)) then
            if (allocated(result%shape)) deallocate(result%shape)
            allocate(result%shape(size(var%shape)))
            result%shape = var%shape
        end if
        
        result%units = var%units
        result%long_name = var%long_name
        
    end function apply_cumulative
    
    !> Apply element-wise operation (alias for apply_vectorized)
    function apply_elementwise(var, operation) result(result)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: operation
        type(fortarray_t) :: result
        
        result = apply_vectorized(var, operation)
        
    end function apply_elementwise
    
    !> Apply reduction operation (alias for apply_along_dim with dim=1)
    function apply_reduction(var, operation) result(result)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: operation
        type(fortarray_t) :: result
        
        result = apply_along_dim(var, 1, operation)
        
    end function apply_reduction
    
    !> Apply function with broadcasting (simplified implementation)
    function apply_broadcast(var, operation) result(result)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: operation
        type(fortarray_t) :: result
        
        ! For now, just apply element-wise
        result = apply_vectorized(var, operation)
        
    end function apply_broadcast
    
    !======= Helper Functions =======!
    
    !> Apply sum along dimension
    subroutine apply_sum_along_dim(var, dim, result_data)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        real(real64), dimension(:), intent(out) :: result_data
        real(real64), dimension(:), allocatable :: input_values
        integer :: i, j, k, stride, offset, slice_size
        
        call get_variable_values_r64(var, input_values)
        
        ! Simplified sum calculation
        if (var%n_dims == 1) then
            result_data(1) = sum(input_values)
        else if (var%n_dims == 2 .and. dim == 1) then
            ! Sum along first dimension (rows)
            do j = 1, var%shape(2)
                result_data(j) = 0.0_real64
                do i = 1, var%shape(1)
                    k = (j-1) * var%shape(1) + i
                    result_data(j) = result_data(j) + input_values(k)
                end do
            end do
        else if (var%n_dims == 2 .and. dim == 2) then
            ! Sum along second dimension (columns)
            do i = 1, var%shape(1)
                result_data(i) = 0.0_real64
                do j = 1, var%shape(2)
                    k = (j-1) * var%shape(1) + i
                    result_data(i) = result_data(i) + input_values(k)
                end do
            end do
        else
            ! Generic case (simplified)
            result_data(1) = sum(input_values)
        end if
        
    end subroutine apply_sum_along_dim
    
    !> Apply mean along dimension
    subroutine apply_mean_along_dim(var, dim, result_data)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        real(real64), dimension(:), intent(out) :: result_data
        
        call apply_sum_along_dim(var, dim, result_data)
        result_data = result_data / real(var%shape(dim), real64)
        
    end subroutine apply_mean_along_dim
    
    !> Apply min along dimension
    subroutine apply_min_along_dim(var, dim, result_data)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        real(real64), dimension(:), intent(out) :: result_data
        real(real64), dimension(:), allocatable :: input_values
        integer :: i, j, k
        
        call get_variable_values_r64(var, input_values)
        
        if (var%n_dims == 1) then
            result_data(1) = minval(input_values)
        else if (var%n_dims == 2 .and. dim == 1) then
            do j = 1, var%shape(2)
                result_data(j) = input_values((j-1) * var%shape(1) + 1)
                do i = 1, var%shape(1)
                    k = (j-1) * var%shape(1) + i
                    result_data(j) = min(result_data(j), input_values(k))
                end do
            end do
        else
            result_data(1) = minval(input_values)
        end if
        
    end subroutine apply_min_along_dim
    
    !> Apply max along dimension
    subroutine apply_max_along_dim(var, dim, result_data)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        real(real64), dimension(:), intent(out) :: result_data
        real(real64), dimension(:), allocatable :: input_values
        integer :: i, j, k
        
        call get_variable_values_r64(var, input_values)
        
        if (var%n_dims == 1) then
            result_data(1) = maxval(input_values)
        else if (var%n_dims == 2 .and. dim == 1) then
            do j = 1, var%shape(2)
                result_data(j) = input_values((j-1) * var%shape(1) + 1)
                do i = 1, var%shape(1)
                    k = (j-1) * var%shape(1) + i
                    result_data(j) = max(result_data(j), input_values(k))
                end do
            end do
        else
            result_data(1) = maxval(input_values)
        end if
        
    end subroutine apply_max_along_dim
    
    !> Apply standard deviation along dimension
    subroutine apply_std_along_dim(var, dim, result_data)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        real(real64), dimension(:), intent(out) :: result_data
        
        call apply_var_along_dim(var, dim, result_data)
        result_data = sqrt(result_data)
        
    end subroutine apply_std_along_dim
    
    !> Apply variance along dimension
    subroutine apply_var_along_dim(var, dim, result_data)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        real(real64), dimension(:), intent(out) :: result_data
        real(real64), dimension(:), allocatable :: input_values, mean_values
        integer :: i, j, k, n
        real(real64) :: mean_val, sum_sq
        
        call get_variable_values_r64(var, input_values)
        
        if (var%n_dims == 1) then
            mean_val = sum(input_values) / real(var%n_elements, real64)
            sum_sq = 0.0_real64
            do i = 1, var%n_elements
                sum_sq = sum_sq + (input_values(i) - mean_val)**2
            end do
            result_data(1) = sum_sq / real(var%n_elements - 1, real64)
        else
            ! Simplified variance calculation
            result_data(1) = 1.0_real64  ! Placeholder
        end if
        
    end subroutine apply_var_along_dim
    
    !> Get variable values as real64
    subroutine get_variable_values_r64(var, values)
        type(fortarray_t), intent(in) :: var
        real(real64), dimension(:), allocatable, intent(out) :: values
        integer :: n
        
        n = var%n_elements
        allocate(values(n))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            values = var%data%values_r64
        case(DTYPE_REAL32)
            values = real(var%data%values_r32, real64)
        case(DTYPE_INT64)
            values = real(var%data%values_i64, real64)
        case(DTYPE_INT32)
            values = real(var%data%values_i32, real64)
        case default
            values = 0.0_real64
        end select
        
    end subroutine get_variable_values_r64
    
    !> Create empty variable like another
    function create_empty_like(var) result(result)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        
        result%name = var%name // "_empty"
        result%n_dims = var%n_dims
        result%n_elements = 0
        
        if (allocated(var%shape)) then
            allocate(result%shape(size(var%shape)))
            result%shape = 0
        end if
        
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(size(var%dim_names)))
            result%dim_names = var%dim_names
        end if
        
        result%data%dtype = var%data%dtype
        result%data%n_elements = 0
        result%data%initialized = .true.
        
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

end module fortarray_apply_functions