module fortarray_arithmetic
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors, only: new_array, new_dataset, &
        variable_scalar_real64, variable_scalar_real32, variable_scalar_int32, variable_scalar_int64
    use fortarray_memory
    use fortarray_broadcasting
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic, only: ieee_is_nan
    implicit none
    private
    
    ! Public operator interfaces
    public :: operator(+), operator(-), operator(*), operator(/), operator(**)
    
    ! Addition operators
    interface operator(+)
        module procedure add_variables
        module procedure add_variable_scalar_r64
        module procedure add_scalar_variable_r64
    end interface operator(+)
    
    ! Subtraction operators
    interface operator(-)
        module procedure subtract_variables
        module procedure subtract_variable_scalar_r64
        module procedure subtract_scalar_variable_r64
    end interface operator(-)
    
    ! Multiplication operators
    interface operator(*)
        module procedure multiply_variables
        module procedure multiply_variable_scalar_r64
        module procedure multiply_scalar_variable_r64
    end interface operator(*)
    
    ! Division operators
    interface operator(/)
        module procedure divide_variables
        module procedure divide_variable_scalar_r64
        module procedure divide_scalar_variable_r64
    end interface operator(/)
    
    ! Power operators
    interface operator(**)
        module procedure power_variables
        module procedure power_variable_scalar_r64
        module procedure power_scalar_variable_r64
    end interface operator(**)
    
contains

    !> Create variable from 1D array with given shape
    function create_variable_with_shape(values, name, shape) result(var)
        real(real64), dimension(:), intent(in) :: values
        character(len=*), intent(in) :: name
        integer, dimension(:), intent(in) :: shape
        type(fortarray_t) :: var
        character(len=64), dimension(:), allocatable :: dim_names
        integer :: i, ndims
        
        ndims = size(shape)
        allocate(dim_names(ndims))
        do i = 1, ndims
            write(dim_names(i), '(A,I0)') "dim", i
        end do
        
        ! Create variable based on number of dimensions
        select case(ndims)
        case(1)
            var = new_array(values(1:shape(1)), name=name, dim_names=dim_names)
        case(2)
            block
                real(real64), dimension(shape(1), shape(2)) :: temp
                temp = reshape(values, [shape(1), shape(2)])
                var = new_array(temp, name=name, dim_names=dim_names)
            end block
        case(3)
            block
                real(real64), dimension(shape(1), shape(2), shape(3)) :: temp
                temp = reshape(values, [shape(1), shape(2), shape(3)])
                var = new_array(temp, name=name, dim_names=dim_names)
            end block
        case default
            ! For higher dimensions, create manually
            var%name = name
            var%n_elements = product(shape)
            var%n_dims = ndims
            allocate(var%shape(ndims))
            var%shape = shape
            allocate(var%dim_names(ndims))
            var%dim_names = dim_names
            var%data%dtype = DTYPE_REAL64
            allocate(var%data%values_r64(var%n_elements))
            var%data%values_r64 = values
        end select
        
    end function create_variable_with_shape

    !> Get values as real64 array
    function get_values_as_real64(var) result(values)
        type(fortarray_t), intent(in) :: var
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
            write(error_unit,'(A)') "ERROR: Unsupported data type for arithmetic"
            stop 1
        end select
    end function get_values_as_real64
    
    !> Handle missing values in binary operations
    function handle_missing_binary(val1, val2, op) result(result)
        real(real64), intent(in) :: val1, val2
        character(len=*), intent(in) :: op
        real(real64) :: result
        real(real64), parameter :: missing = huge(1.0_real64)
        
        ! If either value is missing (huge or NaN), result is missing
        if (abs(val1 - missing) < 1e-10 .or. abs(val2 - missing) < 1e-10 .or. &
            ieee_is_nan(val1) .or. ieee_is_nan(val2)) then
            result = missing
        else
            select case(op)
            case('+')
                result = val1 + val2
            case('-')
                result = val1 - val2
            case('*')
                result = val1 * val2
            case('/')
                result = val1 / val2
            case('**')
                result = val1 ** val2
            end select
        end if
    end function handle_missing_binary
    
    !> Generic binary operation implementation
    function binary_operation(var1, var2, op, result_name) result(result)
        type(fortarray_t), intent(in) :: var1, var2
        character(len=*), intent(in) :: op, result_name
        type(fortarray_t) :: result, bcast1, bcast2
        real(real64), dimension(:), allocatable :: values1, values2, result_values
        integer :: i
        logical :: need_broadcast
        
        ! Check if broadcasting is needed
        if (.not. can_broadcast(var1, var2)) then
            write(error_unit,'(A,A)') "ERROR: Cannot broadcast variables for ", op
            stop 1
        end if
        
        ! Determine if we need broadcasting
        need_broadcast = .false.
        if (allocated(var1%shape) .and. allocated(var2%shape)) then
            if (size(var1%shape) /= size(var2%shape)) then
                need_broadcast = .true.
            else if (any(var1%shape /= var2%shape)) then
                need_broadcast = .true.
            end if
        end if
        
        if (need_broadcast) then
            ! Broadcast to common shape
            bcast1 = broadcast_to_common_shape(var1, var2)
            bcast2 = broadcast_to_common_shape(var2, var1)
            
            values1 = get_values_as_real64(bcast1)
            values2 = get_values_as_real64(bcast2)
            
            allocate(result_values(size(values1)))
            do i = 1, size(values1)
                result_values(i) = handle_missing_binary(values1(i), values2(i), op)
            end do
            
            result = create_variable_with_shape(result_values, result_name, bcast1%shape)
            
            call finalize_variable(bcast1)
            call finalize_variable(bcast2)
        else
            ! Same shape or scalars
            values1 = get_values_as_real64(var1)
            values2 = get_values_as_real64(var2)
            
            allocate(result_values(size(values1)))
            do i = 1, size(values1)
                result_values(i) = handle_missing_binary(values1(i), values2(i), op)
            end do
            
            if (allocated(var1%shape)) then
                result = create_variable_with_shape(result_values, result_name, var1%shape)
            else if (allocated(var2%shape)) then
                result = create_variable_with_shape(result_values, result_name, var2%shape)
            else
                ! Both are scalars
                result = variable_scalar_real64(result_values(1), name=result_name)
            end if
        end if
        
    end function binary_operation
    
    !> Variable-scalar operation implementation
    function var_scalar_operation(var, scalar, op, result_name) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        character(len=*), intent(in) :: op, result_name
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer :: i
        
        values = get_values_as_real64(var)
        allocate(result_values(size(values)))
        
        do i = 1, size(values)
            result_values(i) = handle_missing_binary(values(i), scalar, op)
        end do
        
        if (allocated(var%shape)) then
            result = create_variable_with_shape(result_values, result_name, var%shape)
        else
            result = variable_scalar_real64(result_values(1), name=result_name)
        end if
        
    end function var_scalar_operation
    
    !> Scalar-variable operation implementation
    function scalar_var_operation(scalar, var, op, result_name) result(result)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: op, result_name
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, result_values
        integer :: i
        
        values = get_values_as_real64(var)
        allocate(result_values(size(values)))
        
        do i = 1, size(values)
            result_values(i) = handle_missing_binary(scalar, values(i), op)
        end do
        
        if (allocated(var%shape)) then
            result = create_variable_with_shape(result_values, result_name, var%shape)
        else
            result = variable_scalar_real64(result_values(1), name=result_name)
        end if
        
    end function scalar_var_operation
    
    !======= Addition Operations =======!
    
    function add_variables(var1, var2) result(result)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: result
        result = binary_operation(var1, var2, '+', 'add_result')
    end function add_variables
    
    function add_variable_scalar_r64(var, scalar) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: result
        result = var_scalar_operation(var, scalar, '+', 'add_result')
    end function add_variable_scalar_r64
    
    function add_scalar_variable_r64(scalar, var) result(result)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        result = scalar_var_operation(scalar, var, '+', 'add_result')
    end function add_scalar_variable_r64
    
    !======= Subtraction Operations =======!
    
    function subtract_variables(var1, var2) result(result)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: result
        result = binary_operation(var1, var2, '-', 'subtract_result')
    end function subtract_variables
    
    function subtract_variable_scalar_r64(var, scalar) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: result
        result = var_scalar_operation(var, scalar, '-', 'subtract_result')
    end function subtract_variable_scalar_r64
    
    function subtract_scalar_variable_r64(scalar, var) result(result)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        result = scalar_var_operation(scalar, var, '-', 'subtract_result')
    end function subtract_scalar_variable_r64
    
    !======= Multiplication Operations =======!
    
    function multiply_variables(var1, var2) result(result)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: result
        result = binary_operation(var1, var2, '*', 'multiply_result')
    end function multiply_variables
    
    function multiply_variable_scalar_r64(var, scalar) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: result
        result = var_scalar_operation(var, scalar, '*', 'multiply_result')
    end function multiply_variable_scalar_r64
    
    function multiply_scalar_variable_r64(scalar, var) result(result)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        result = scalar_var_operation(scalar, var, '*', 'multiply_result')
    end function multiply_scalar_variable_r64
    
    !======= Division Operations =======!
    
    function divide_variables(var1, var2) result(result)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: result
        result = binary_operation(var1, var2, '/', 'divide_result')
    end function divide_variables
    
    function divide_variable_scalar_r64(var, scalar) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: result
        result = var_scalar_operation(var, scalar, '/', 'divide_result')
    end function divide_variable_scalar_r64
    
    function divide_scalar_variable_r64(scalar, var) result(result)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        result = scalar_var_operation(scalar, var, '/', 'divide_result')
    end function divide_scalar_variable_r64
    
    !======= Power Operations =======!
    
    function power_variables(var1, var2) result(result)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: result
        result = binary_operation(var1, var2, '**', 'power_result')
    end function power_variables
    
    function power_variable_scalar_r64(var, scalar) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: result
        result = var_scalar_operation(var, scalar, '**', 'power_result')
    end function power_variable_scalar_r64
    
    function power_scalar_variable_r64(scalar, var) result(result)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        result = scalar_var_operation(scalar, var, '**', 'power_result')
    end function power_scalar_variable_r64

end module fortarray_arithmetic