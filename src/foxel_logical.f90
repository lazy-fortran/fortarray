module foxel_logical
    use foxel_types
    use foxel_storage
    use foxel_constructors
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Public logical operators
    public :: operator(.and.), operator(.or.), operator(.not.)
    public :: operator(.eqv.), operator(.neqv.)
    
    ! Logical AND
    interface operator(.and.)
        module procedure logical_and_variables
    end interface operator(.and.)
    
    ! Logical OR
    interface operator(.or.)
        module procedure logical_or_variables
    end interface operator(.or.)
    
    ! Logical NOT
    interface operator(.not.)
        module procedure logical_not_variable
    end interface operator(.not.)
    
    ! Logical equivalence
    interface operator(.eqv.)
        module procedure logical_eqv_variables
    end interface operator(.eqv.)
    
    ! Logical non-equivalence
    interface operator(.neqv.)
        module procedure logical_neqv_variables
    end interface operator(.neqv.)
    
contains
    
    !> Logical AND of two logical variables
    function logical_and_variables(var1, var2) result(mask)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: mask
        integer :: i
        
        ! Validate inputs
        if (var1%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: First operand of .and. must be logical type"
            mask = var1
            return
        end if
        
        if (var2%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: Second operand of .and. must be logical type"
            mask = var1
            return
        end if
        
        if (var1%n_elements /= var2%n_elements) then
            write(error_unit,'(A)') "ERROR: Operands of .and. must have same size"
            mask = var1
            return
        end if
        
        ! Create result
        mask%name = trim(var1%name) // "_and_" // trim(var2%name)
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
        
        ! Initialize logical data storage
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var1%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var1%n_elements))
        
        ! Perform logical AND
        do i = 1, var1%n_elements
            mask%data%values_logical(i) = var1%data%values_logical(i) .and. var2%data%values_logical(i)
        end do
        
    end function logical_and_variables
    
    !> Logical OR of two logical variables
    function logical_or_variables(var1, var2) result(mask)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: mask
        integer :: i
        
        ! Validate inputs
        if (var1%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: First operand of .or. must be logical type"
            mask = var1
            return
        end if
        
        if (var2%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: Second operand of .or. must be logical type"
            mask = var1
            return
        end if
        
        if (var1%n_elements /= var2%n_elements) then
            write(error_unit,'(A)') "ERROR: Operands of .or. must have same size"
            mask = var1
            return
        end if
        
        ! Create result
        mask%name = trim(var1%name) // "_or_" // trim(var2%name)
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
        
        ! Initialize logical data storage
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var1%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var1%n_elements))
        
        ! Perform logical OR
        do i = 1, var1%n_elements
            mask%data%values_logical(i) = var1%data%values_logical(i) .or. var2%data%values_logical(i)
        end do
        
    end function logical_or_variables
    
    !> Logical NOT of a logical variable
    function logical_not_variable(var) result(mask)
        type(variable_t), intent(in) :: var
        type(variable_t) :: mask
        integer :: i
        
        ! Validate input
        if (var%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: Operand of .not. must be logical type"
            mask = var
            return
        end if
        
        ! Create result
        mask%name = "not_" // trim(var%name)
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
        
        ! Perform logical NOT
        do i = 1, var%n_elements
            mask%data%values_logical(i) = .not. var%data%values_logical(i)
        end do
        
    end function logical_not_variable
    
    !> Logical equivalence of two logical variables
    function logical_eqv_variables(var1, var2) result(mask)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: mask
        integer :: i
        
        ! Validate inputs
        if (var1%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: First operand of .eqv. must be logical type"
            mask = var1
            return
        end if
        
        if (var2%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: Second operand of .eqv. must be logical type"
            mask = var1
            return
        end if
        
        if (var1%n_elements /= var2%n_elements) then
            write(error_unit,'(A)') "ERROR: Operands of .eqv. must have same size"
            mask = var1
            return
        end if
        
        ! Create result
        mask%name = trim(var1%name) // "_eqv_" // trim(var2%name)
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
        
        ! Initialize logical data storage
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var1%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var1%n_elements))
        
        ! Perform logical equivalence
        do i = 1, var1%n_elements
            mask%data%values_logical(i) = var1%data%values_logical(i) .eqv. var2%data%values_logical(i)
        end do
        
    end function logical_eqv_variables
    
    !> Logical non-equivalence of two logical variables
    function logical_neqv_variables(var1, var2) result(mask)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: mask
        integer :: i
        
        ! Validate inputs
        if (var1%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: First operand of .neqv. must be logical type"
            mask = var1
            return
        end if
        
        if (var2%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: Second operand of .neqv. must be logical type"
            mask = var1
            return
        end if
        
        if (var1%n_elements /= var2%n_elements) then
            write(error_unit,'(A)') "ERROR: Operands of .neqv. must have same size"
            mask = var1
            return
        end if
        
        ! Create result
        mask%name = trim(var1%name) // "_neqv_" // trim(var2%name)
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
        
        ! Initialize logical data storage
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = var1%n_elements
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(var1%n_elements))
        
        ! Perform logical non-equivalence
        do i = 1, var1%n_elements
            mask%data%values_logical(i) = var1%data%values_logical(i) .neqv. var2%data%values_logical(i)
        end do
        
    end function logical_neqv_variables
    
end module foxel_logical