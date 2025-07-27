module foxel_datasets
    use foxel_types
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private
    
    ! Public interfaces
    public :: get_variable
    public :: has_variable
    public :: add_variable
    public :: remove_variable
    public :: list_variables
    public :: select_variables
    
contains

    !> Get a variable from dataset by name
    function get_variable(dset, var_name, stat, error_msg) result(var)
        type(dataset_t), intent(in) :: dset
        character(len=*), intent(in) :: var_name
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        integer :: status, i
        logical :: found
        character(len=256) :: err_msg
        
        status = 0
        err_msg = ""
        found = .false.
        
        ! Initialize empty variable
        var%initialized = .false.
        
        ! Check dataset is initialized
        if (.not. dset%initialized) then
            status = -1
            err_msg = "Dataset not initialized"
            goto 999
        end if
        
        ! Search for variable
        do i = 1, dset%n_vars
            if (trim(dset%variables(i)%name) == trim(var_name)) then
                var = dset%variables(i)
                found = .true.
                exit
            end if
        end do
        
        if (.not. found) then
            status = -2
            write(err_msg, '(A,A,A)') "Variable '", trim(var_name), "' not found in dataset"
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function get_variable
    
    !> Check if dataset has a variable
    function has_variable(dset, var_name) result(exists)
        type(dataset_t), intent(in) :: dset
        character(len=*), intent(in) :: var_name
        logical :: exists
        
        integer :: i
        
        exists = .false.
        
        if (.not. dset%initialized) return
        
        do i = 1, dset%n_vars
            if (trim(dset%variables(i)%name) == trim(var_name)) then
                exists = .true.
                exit
            end if
        end do
    end function has_variable
    
    !> Add a variable to dataset
    subroutine add_variable(dset, var, stat, error_msg)
        type(dataset_t), intent(inout) :: dset
        type(variable_t), intent(in) :: var
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        
        integer :: status, new_size
        type(variable_t), dimension(:), allocatable :: temp_vars
        character(len=256) :: err_msg
        
        status = 0
        err_msg = ""
        
        ! Check dataset is initialized
        if (.not. dset%initialized) then
            status = -1
            err_msg = "Dataset not initialized"
            goto 999
        end if
        
        ! Check variable is initialized
        if (.not. var%initialized) then
            status = -2
            err_msg = "Variable not initialized"
            goto 999
        end if
        
        ! Check if variable name already exists
        if (has_variable(dset, var%name)) then
            status = -3
            write(err_msg, '(A,A,A)') "Variable '", trim(var%name), "' already exists in dataset"
            goto 999
        end if
        
        ! Expand array if needed
        if (dset%n_vars >= size(dset%variables)) then
            new_size = max(2 * size(dset%variables), 10)
            allocate(temp_vars(new_size))
            temp_vars(1:dset%n_vars) = dset%variables(1:dset%n_vars)
            deallocate(dset%variables)
            allocate(dset%variables(new_size))
            dset%variables = temp_vars
            deallocate(temp_vars)
        end if
        
        ! Add variable
        dset%n_vars = dset%n_vars + 1
        dset%variables(dset%n_vars) = var
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end subroutine add_variable
    
    !> Remove a variable from dataset
    subroutine remove_variable(dset, var_name, stat, error_msg)
        type(dataset_t), intent(inout) :: dset
        character(len=*), intent(in) :: var_name
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        
        integer :: status, i, j
        logical :: found
        character(len=256) :: err_msg
        
        status = 0
        err_msg = ""
        found = .false.
        
        ! Check dataset is initialized
        if (.not. dset%initialized) then
            status = -1
            err_msg = "Dataset not initialized"
            goto 999
        end if
        
        ! Find and remove variable
        do i = 1, dset%n_vars
            if (trim(dset%variables(i)%name) == trim(var_name)) then
                ! Finalize the variable being removed
                call finalize_variable(dset%variables(i))
                
                ! Shift remaining variables
                do j = i, dset%n_vars - 1
                    dset%variables(j) = dset%variables(j + 1)
                end do
                
                dset%n_vars = dset%n_vars - 1
                found = .true.
                exit
            end if
        end do
        
        if (.not. found) then
            status = -2
            write(err_msg, '(A,A,A)') "Variable '", trim(var_name), "' not found in dataset"
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end subroutine remove_variable
    
    !> Get list of variable names in dataset
    function list_variables(dset) result(names)
        type(dataset_t), intent(in) :: dset
        character(len=MAX_NAME_LEN), dimension(:), allocatable :: names
        
        integer :: i
        
        if (.not. dset%initialized .or. dset%n_vars == 0) then
            allocate(names(0))
            return
        end if
        
        allocate(names(dset%n_vars))
        do i = 1, dset%n_vars
            names(i) = dset%variables(i)%name
        end do
    end function list_variables
    
    !> Select subset of variables from dataset
    function select_variables(dset, var_names, stat, error_msg) result(subset)
        type(dataset_t), intent(in) :: dset
        character(len=*), dimension(:), intent(in) :: var_names
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataset_t) :: subset
        
        integer :: status, i, j
        type(variable_t) :: var
        character(len=256) :: err_msg
        logical :: found
        
        status = 0
        err_msg = ""
        
        ! Initialize empty dataset
        subset%filename = trim(dset%filename) // "_subset"
        subset%n_vars = 0
        subset%n_dims = 0
        subset%n_attrs = 0
        allocate(subset%variables(size(var_names)))
        allocate(subset%dimensions(0))
        allocate(subset%attr_keys(0))
        allocate(subset%attr_values(0))
        subset%initialized = .true.
        
        ! Check dataset is initialized
        if (.not. dset%initialized) then
            status = -1
            err_msg = "Dataset not initialized"
            goto 999
        end if
        
        ! Add requested variables
        do i = 1, size(var_names)
            found = .false.
            do j = 1, dset%n_vars
                if (trim(dset%variables(j)%name) == trim(var_names(i))) then
                    call add_variable(subset, dset%variables(j))
                    found = .true.
                    exit
                end if
            end do
            
            if (.not. found) then
                status = -2
                write(err_msg, '(A,A,A)') "Variable '", trim(var_names(i)), "' not found in dataset"
                goto 999
            end if
        end do
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        
        if (status /= 0) then
            call finalize_dataset(subset)
        end if
    end function select_variables
    
end module foxel_datasets