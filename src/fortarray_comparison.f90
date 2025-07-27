module fortarray_comparison
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors
    use fortarray_memory
    use fortarray_broadcasting
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Public comparison operators
    public :: operator(>), operator(<), operator(>=), operator(<=)
    public :: operator(==), operator(/=)
    
    ! Greater than operators
    interface operator(>)
        module procedure greater_than_variable_scalar_r64
        module procedure greater_than_scalar_variable_r64
        module procedure greater_than_variables
    end interface operator(>)
    
    ! Less than operators
    interface operator(<)
        module procedure less_than_variable_scalar_r64
        module procedure less_than_scalar_variable_r64
        module procedure less_than_variables
    end interface operator(<)
    
    ! Greater than or equal operators
    interface operator(>=)
        module procedure greater_equal_variable_scalar_r64
        module procedure greater_equal_scalar_variable_r64
        module procedure greater_equal_variables
    end interface operator(>=)
    
    ! Less than or equal operators
    interface operator(<=)
        module procedure less_equal_variable_scalar_r64
        module procedure less_equal_scalar_variable_r64
        module procedure less_equal_variables
    end interface operator(<=)
    
    ! Equal operators
    interface operator(==)
        module procedure equal_variable_scalar_r64
        module procedure equal_scalar_variable_r64
        module procedure equal_variables
    end interface operator(==)
    
    ! Not equal operators
    interface operator(/=)
        module procedure not_equal_variable_scalar_r64
        module procedure not_equal_scalar_variable_r64
        module procedure not_equal_variables
    end interface operator(/=)
    
contains
    
    !======= Greater Than Functions =======!
    
    !> Variable > scalar
    function greater_than_variable_scalar_r64(var, scalar) result(mask)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: mask
        integer :: i
        
        ! Create logical mask
        mask%name = trim(var%name) // "_gt_mask"
        mask%n_dims = var%n_dims
        mask%n_elements = var%n_elements
        mask%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(mask%shape(size(var%shape)))
            mask%shape = var%shape
        end if
        
        if (allocated(var%dim_names)) then
            allocate(mask%dim_names(size(var%dim_names)))
            mask%dim_names = var%dim_names
        end if
        
        ! Initialize logical data storage
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var%n_elements))
        
        ! Perform comparison based on type
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r64(i) > scalar
            end do
        case(DTYPE_REAL32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r32(i) > real(scalar, real32)
            end do
        case(DTYPE_INT64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i64(i) > int(scalar, int64)
            end do
        case(DTYPE_INT32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i32(i) > int(scalar, int32)
            end do
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for comparison"
            mask%data%values_logical = .false.
        end select
        
    end function greater_than_variable_scalar_r64
    
    !> Scalar > variable
    function greater_than_scalar_variable_r64(scalar, var) result(mask)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: mask
        
        ! scalar > var is equivalent to var < scalar
        mask = less_than_variable_scalar_r64(var, scalar)
        
    end function greater_than_scalar_variable_r64
    
    !> Variable > variable
    function greater_than_variables(var1, var2) result(mask)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: mask
        integer :: i
        
        ! Check if dimensions match
        if (var1%n_elements /= var2%n_elements) then
            write(error_unit,'(A)') "ERROR: Variables must have same size for comparison"
            mask = create_empty_like(var1)
            return
        end if
        
        ! Element-wise comparison
        mask%name = trim(var1%name) // "_gt_" // trim(var2%name)
        mask%n_dims = var1%n_dims
        mask%n_elements = var1%n_elements
        mask%initialized = .true.
        
        if (allocated(var1%shape)) then
            allocate(mask%shape(size(var1%shape)))
            mask%shape = var1%shape
        end if
        
        if (allocated(var1%dim_names)) then
            allocate(mask%dim_names(size(var1%dim_names)))
            mask%dim_names = var1%dim_names
        end if
        
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var1%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var1%n_elements))
        
        ! Perform comparison
        if (var1%data%dtype == DTYPE_REAL64 .and. var2%data%dtype == DTYPE_REAL64) then
            do i = 1, var1%n_elements
                mask%data%values_logical(i) = var1%data%values_r64(i) > var2%data%values_r64(i)
            end do
        else
            ! Handle type conversions if needed
            mask%data%values_logical = .false.
        end if
        
    end function greater_than_variables
    
    !======= Less Than Functions =======!
    
    !> Variable < scalar
    function less_than_variable_scalar_r64(var, scalar) result(mask)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: mask
        integer :: i
        
        mask%name = trim(var%name) // "_lt_mask"
        mask%n_dims = var%n_dims
        mask%n_elements = var%n_elements
        mask%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(mask%shape(size(var%shape)))
            mask%shape = var%shape
        end if
        
        if (allocated(var%dim_names)) then
            allocate(mask%dim_names(size(var%dim_names)))
            mask%dim_names = var%dim_names
        end if
        
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var%n_elements))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r64(i) < scalar
            end do
        case(DTYPE_REAL32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r32(i) < real(scalar, real32)
            end do
        case(DTYPE_INT64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i64(i) < int(scalar, int64)
            end do
        case(DTYPE_INT32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i32(i) < int(scalar, int32)
            end do
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for comparison"
            mask%data%values_logical = .false.
        end select
        
    end function less_than_variable_scalar_r64
    
    !> Scalar < variable
    function less_than_scalar_variable_r64(scalar, var) result(mask)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: mask
        
        mask = greater_than_variable_scalar_r64(var, scalar)
        
    end function less_than_scalar_variable_r64
    
    !> Variable < variable
    function less_than_variables(var1, var2) result(mask)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: mask
        integer :: i
        
        if (var1%n_elements == var2%n_elements) then
            mask%name = trim(var1%name) // "_lt_" // trim(var2%name)
            mask%n_dims = var1%n_dims
            mask%n_elements = var1%n_elements
            mask%initialized = .true.
            
            if (allocated(var1%shape)) then
                allocate(mask%shape(size(var1%shape)))
                mask%shape = var1%shape
            end if
            
            if (allocated(var1%dim_names)) then
                allocate(mask%dim_names(size(var1%dim_names)))
                mask%dim_names = var1%dim_names
            end if
            
            mask%data%dtype = DTYPE_LOGICAL
            mask%data%n_elements = var1%n_elements
            mask%data%initialized = .true.
            allocate(mask%data%values_logical(var1%n_elements))
            
            if (var1%data%dtype == DTYPE_REAL64 .and. var2%data%dtype == DTYPE_REAL64) then
                do i = 1, var1%n_elements
                    mask%data%values_logical(i) = var1%data%values_r64(i) < var2%data%values_r64(i)
                end do
            else
                mask%data%values_logical = .false.
            end if
        else
            write(error_unit,'(A)') "ERROR: Variables must have same size for comparison"
            mask = create_empty_like(var1)
        end if
        
    end function less_than_variables
    
    !======= Greater Than or Equal Functions =======!
    
    !> Variable >= scalar
    function greater_equal_variable_scalar_r64(var, scalar) result(mask)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: mask
        integer :: i
        
        mask%name = trim(var%name) // "_ge_mask"
        mask%n_dims = var%n_dims
        mask%n_elements = var%n_elements
        mask%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(mask%shape(size(var%shape)))
            mask%shape = var%shape
        end if
        
        if (allocated(var%dim_names)) then
            allocate(mask%dim_names(size(var%dim_names)))
            mask%dim_names = var%dim_names
        end if
        
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var%n_elements))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r64(i) >= scalar
            end do
        case(DTYPE_REAL32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r32(i) >= real(scalar, real32)
            end do
        case(DTYPE_INT64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i64(i) >= int(scalar, int64)
            end do
        case(DTYPE_INT32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i32(i) >= int(scalar, int32)
            end do
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for comparison"
            mask%data%values_logical = .false.
        end select
        
    end function greater_equal_variable_scalar_r64
    
    !> Scalar >= variable
    function greater_equal_scalar_variable_r64(scalar, var) result(mask)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: mask
        
        mask = less_equal_variable_scalar_r64(var, scalar)
        
    end function greater_equal_scalar_variable_r64
    
    !> Variable >= variable
    function greater_equal_variables(var1, var2) result(mask)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: mask
        integer :: i
        
        if (var1%n_elements == var2%n_elements) then
            mask%name = trim(var1%name) // "_ge_" // trim(var2%name)
            mask%n_dims = var1%n_dims
            mask%n_elements = var1%n_elements
            mask%initialized = .true.
            
            if (allocated(var1%shape)) then
                allocate(mask%shape(size(var1%shape)))
                mask%shape = var1%shape
            end if
            
            if (allocated(var1%dim_names)) then
                allocate(mask%dim_names(size(var1%dim_names)))
                mask%dim_names = var1%dim_names
            end if
            
            mask%data%dtype = DTYPE_LOGICAL
            mask%data%n_elements = var1%n_elements
            mask%data%initialized = .true.
            allocate(mask%data%values_logical(var1%n_elements))
            
            if (var1%data%dtype == DTYPE_REAL64 .and. var2%data%dtype == DTYPE_REAL64) then
                do i = 1, var1%n_elements
                    mask%data%values_logical(i) = var1%data%values_r64(i) >= var2%data%values_r64(i)
                end do
            else
                mask%data%values_logical = .false.
            end if
        else
            write(error_unit,'(A)') "ERROR: Variables must have same size for comparison"
            mask = create_empty_like(var1)
        end if
        
    end function greater_equal_variables
    
    !======= Less Than or Equal Functions =======!
    
    !> Variable <= scalar
    function less_equal_variable_scalar_r64(var, scalar) result(mask)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: mask
        integer :: i
        
        mask%name = trim(var%name) // "_le_mask"
        mask%n_dims = var%n_dims
        mask%n_elements = var%n_elements
        mask%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(mask%shape(size(var%shape)))
            mask%shape = var%shape
        end if
        
        if (allocated(var%dim_names)) then
            allocate(mask%dim_names(size(var%dim_names)))
            mask%dim_names = var%dim_names
        end if
        
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var%n_elements))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r64(i) <= scalar
            end do
        case(DTYPE_REAL32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r32(i) <= real(scalar, real32)
            end do
        case(DTYPE_INT64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i64(i) <= int(scalar, int64)
            end do
        case(DTYPE_INT32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i32(i) <= int(scalar, int32)
            end do
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for comparison"
            mask%data%values_logical = .false.
        end select
        
    end function less_equal_variable_scalar_r64
    
    !> Scalar <= variable
    function less_equal_scalar_variable_r64(scalar, var) result(mask)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: mask
        
        mask = greater_equal_variable_scalar_r64(var, scalar)
        
    end function less_equal_scalar_variable_r64
    
    !> Variable <= variable
    function less_equal_variables(var1, var2) result(mask)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: mask
        integer :: i
        
        if (var1%n_elements == var2%n_elements) then
            mask%name = trim(var1%name) // "_le_" // trim(var2%name)
            mask%n_dims = var1%n_dims
            mask%n_elements = var1%n_elements
            mask%initialized = .true.
            
            if (allocated(var1%shape)) then
                allocate(mask%shape(size(var1%shape)))
                mask%shape = var1%shape
            end if
            
            if (allocated(var1%dim_names)) then
                allocate(mask%dim_names(size(var1%dim_names)))
                mask%dim_names = var1%dim_names
            end if
            
            mask%data%dtype = DTYPE_LOGICAL
            mask%data%n_elements = var1%n_elements
            mask%data%initialized = .true.
            allocate(mask%data%values_logical(var1%n_elements))
            
            if (var1%data%dtype == DTYPE_REAL64 .and. var2%data%dtype == DTYPE_REAL64) then
                do i = 1, var1%n_elements
                    mask%data%values_logical(i) = var1%data%values_r64(i) <= var2%data%values_r64(i)
                end do
            else
                mask%data%values_logical = .false.
            end if
        else
            write(error_unit,'(A)') "ERROR: Variables must have same size for comparison"
            mask = create_empty_like(var1)
        end if
        
    end function less_equal_variables
    
    !======= Equal Functions =======!
    
    !> Variable == scalar
    function equal_variable_scalar_r64(var, scalar) result(mask)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: mask
        integer :: i
        
        mask%name = trim(var%name) // "_eq_mask"
        mask%n_dims = var%n_dims
        mask%n_elements = var%n_elements
        mask%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(mask%shape(size(var%shape)))
            mask%shape = var%shape
        end if
        
        if (allocated(var%dim_names)) then
            allocate(mask%dim_names(size(var%dim_names)))
            mask%dim_names = var%dim_names
        end if
        
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var%n_elements))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r64(i) == scalar
            end do
        case(DTYPE_REAL32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r32(i) == real(scalar, real32)
            end do
        case(DTYPE_INT64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i64(i) == int(scalar, int64)
            end do
        case(DTYPE_INT32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i32(i) == int(scalar, int32)
            end do
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for comparison"
            mask%data%values_logical = .false.
        end select
        
    end function equal_variable_scalar_r64
    
    !> Scalar == variable
    function equal_scalar_variable_r64(scalar, var) result(mask)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: mask
        
        mask = equal_variable_scalar_r64(var, scalar)
        
    end function equal_scalar_variable_r64
    
    !> Variable == variable
    function equal_variables(var1, var2) result(mask)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: mask
        integer :: i
        
        if (var1%n_elements == var2%n_elements) then
            mask%name = trim(var1%name) // "_eq_" // trim(var2%name)
            mask%n_dims = var1%n_dims
            mask%n_elements = var1%n_elements
            mask%initialized = .true.
            
            if (allocated(var1%shape)) then
                allocate(mask%shape(size(var1%shape)))
                mask%shape = var1%shape
            end if
            
            if (allocated(var1%dim_names)) then
                allocate(mask%dim_names(size(var1%dim_names)))
                mask%dim_names = var1%dim_names
            end if
            
            mask%data%dtype = DTYPE_LOGICAL
            mask%data%n_elements = var1%n_elements
            mask%data%initialized = .true.
            allocate(mask%data%values_logical(var1%n_elements))
            
            if (var1%data%dtype == DTYPE_REAL64 .and. var2%data%dtype == DTYPE_REAL64) then
                do i = 1, var1%n_elements
                    mask%data%values_logical(i) = var1%data%values_r64(i) == var2%data%values_r64(i)
                end do
            else
                mask%data%values_logical = .false.
            end if
        else
            write(error_unit,'(A)') "ERROR: Variables must have same size for comparison"
            mask = create_empty_like(var1)
        end if
        
    end function equal_variables
    
    !======= Not Equal Functions =======!
    
    !> Variable /= scalar
    function not_equal_variable_scalar_r64(var, scalar) result(mask)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: mask
        integer :: i
        
        mask%name = trim(var%name) // "_ne_mask"
        mask%n_dims = var%n_dims
        mask%n_elements = var%n_elements
        mask%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(mask%shape(size(var%shape)))
            mask%shape = var%shape
        end if
        
        if (allocated(var%dim_names)) then
            allocate(mask%dim_names(size(var%dim_names)))
            mask%dim_names = var%dim_names
        end if
        
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var%n_elements))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r64(i) /= scalar
            end do
        case(DTYPE_REAL32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_r32(i) /= real(scalar, real32)
            end do
        case(DTYPE_INT64)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i64(i) /= int(scalar, int64)
            end do
        case(DTYPE_INT32)
            do i = 1, var%n_elements
                mask%data%values_logical(i) = var%data%values_i32(i) /= int(scalar, int32)
            end do
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for comparison"
            mask%data%values_logical = .false.
        end select
        
    end function not_equal_variable_scalar_r64
    
    !> Scalar /= variable
    function not_equal_scalar_variable_r64(scalar, var) result(mask)
        real(real64), intent(in) :: scalar
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: mask
        
        mask = not_equal_variable_scalar_r64(var, scalar)
        
    end function not_equal_scalar_variable_r64
    
    !> Variable /= variable
    function not_equal_variables(var1, var2) result(mask)
        type(fortarray_t), intent(in) :: var1, var2
        type(fortarray_t) :: mask
        integer :: i
        
        if (var1%n_elements == var2%n_elements) then
            mask%name = trim(var1%name) // "_ne_" // trim(var2%name)
            mask%n_dims = var1%n_dims
            mask%n_elements = var1%n_elements
            mask%initialized = .true.
            
            if (allocated(var1%shape)) then
                allocate(mask%shape(size(var1%shape)))
                mask%shape = var1%shape
            end if
            
            if (allocated(var1%dim_names)) then
                allocate(mask%dim_names(size(var1%dim_names)))
                mask%dim_names = var1%dim_names
            end if
            
            mask%data%dtype = DTYPE_LOGICAL
            mask%data%n_elements = var1%n_elements
            mask%data%initialized = .true.
            allocate(mask%data%values_logical(var1%n_elements))
            
            if (var1%data%dtype == DTYPE_REAL64 .and. var2%data%dtype == DTYPE_REAL64) then
                do i = 1, var1%n_elements
                    mask%data%values_logical(i) = var1%data%values_r64(i) /= var2%data%values_r64(i)
                end do
            else
                mask%data%values_logical = .false.
            end if
        else
            write(error_unit,'(A)') "ERROR: Variables must have same size for comparison"
            mask = create_empty_like(var1)
        end if
        
    end function not_equal_variables
    
    !> Create empty variable like another
    function create_empty_like(var) result(result)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        
        result%name = var%name
        result%n_dims = var%n_dims
        result%n_elements = 0
        result%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(result%shape(size(var%shape)))
            result%shape = var%shape
            result%shape(1) = 0
        end if
        
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(size(var%dim_names)))
            result%dim_names = var%dim_names
        end if
        
        result%data%dtype = DTYPE_LOGICAL
        result%data%n_elements = 0
        result%data%initialized = .true.
        allocate(result%data%values_logical(0))
        
    end function create_empty_like

end module fortarray_comparison