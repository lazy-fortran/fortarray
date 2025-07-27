module foxel_boolean_indexing
    use foxel_types
    use foxel_storage
    use foxel_constructors
    use foxel_memory
    use foxel_arithmetic
    use foxel_missing_data
    use ieee_arithmetic, only: ieee_is_nan, ieee_quiet_nan
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Public interfaces
    public :: where_boolean, where_broadcast
    public :: create_mask, apply_mask
    public :: where_mask_only, create_mask_from_logical_array
    
    ! Generic interface for where function
    interface where_boolean
        module procedure where_mask_only
        module procedure where_mask_with_fill_scalar
    end interface where_boolean
    
    ! Generic interface for boolean mask creation
    interface create_mask
        module procedure create_mask_from_condition
        module procedure create_mask_from_logical_array
    end interface create_mask
    
contains
    
    !> Apply boolean mask and return selected values
    function where_mask_only(mask, var) result(result)
        type(variable_t), intent(in) :: mask, var
        type(variable_t) :: result
        integer :: n_true, i, j
        
        ! Validate inputs
        if (mask%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: Mask must be logical type"
            result = create_empty_like(var)
            return
        end if
        
        if (mask%n_elements /= var%n_elements) then
            write(error_unit,'(A)') "ERROR: Mask and variable must have same size"
            result = create_empty_like(var)
            return
        end if
        
        ! Count true values
        n_true = 0
        do i = 1, mask%n_elements
            if (mask%data%values_logical(i)) n_true = n_true + 1
        end do
        
        if (n_true == 0) then
            result = create_empty_like(var)
            return
        end if
        
        ! Extract values where mask is true
        result = extract_masked_values(var, mask, n_true)
        
    end function where_mask_only
    
    !> Apply boolean mask with scalar fill value
    function where_mask_with_fill_scalar(mask, var, fill_value) result(result)
        type(variable_t), intent(in) :: mask, var
        real(real64), intent(in) :: fill_value
        type(variable_t) :: result
        integer :: i
        
        ! Validate inputs
        if (mask%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: Mask must be logical type"
            result = var
            return
        end if
        
        if (mask%n_elements /= var%n_elements) then
            write(error_unit,'(A)') "ERROR: Mask and variable must have same size"
            result = var
            return
        end if
        
        ! Create result as copy of var
        result = var
        
        ! Replace false mask positions with fill value
        select case(result%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, mask%n_elements
                if (.not. mask%data%values_logical(i)) then
                    result%data%values_r64(i) = fill_value
                end if
            end do
        case(DTYPE_REAL32)
            do i = 1, mask%n_elements
                if (.not. mask%data%values_logical(i)) then
                    result%data%values_r32(i) = real(fill_value, real32)
                end if
            end do
        case(DTYPE_INT64)
            do i = 1, mask%n_elements
                if (.not. mask%data%values_logical(i)) then
                    result%data%values_i64(i) = int(fill_value, int64)
                end if
            end do
        case(DTYPE_INT32)
            do i = 1, mask%n_elements
                if (.not. mask%data%values_logical(i)) then
                    result%data%values_i32(i) = int(fill_value, int32)
                end if
            end do
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for fill operation"
        end select
        
    end function where_mask_with_fill_scalar
    
    !> Apply boolean mask with variable fill values
    function where_mask_with_fill_variable(mask, var, fill_var) result(result)
        type(variable_t), intent(in) :: mask, var, fill_var
        type(variable_t) :: result
        integer :: i
        
        ! Validate inputs
        if (mask%data%dtype /= DTYPE_LOGICAL) then
            write(error_unit,'(A)') "ERROR: Mask must be logical type"
            result = var
            return
        end if
        
        if (mask%n_elements /= var%n_elements .or. &
            fill_var%n_elements /= var%n_elements) then
            write(error_unit,'(A)') "ERROR: All variables must have same size"
            result = var
            return
        end if
        
        ! Create result as copy of var
        result = var
        
        ! Replace false mask positions with values from fill_var
        select case(result%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, mask%n_elements
                if (.not. mask%data%values_logical(i)) then
                    result%data%values_r64(i) = get_value_as_real64(fill_var, i)
                end if
            end do
        case(DTYPE_REAL32)
            do i = 1, mask%n_elements
                if (.not. mask%data%values_logical(i)) then
                    result%data%values_r32(i) = real(get_value_as_real64(fill_var, i), real32)
                end if
            end do
        case(DTYPE_INT64)
            do i = 1, mask%n_elements
                if (.not. mask%data%values_logical(i)) then
                    result%data%values_i64(i) = int(get_value_as_real64(fill_var, i), int64)
                end if
            end do
        case(DTYPE_INT32)
            do i = 1, mask%n_elements
                if (.not. mask%data%values_logical(i)) then
                    result%data%values_i32(i) = int(get_value_as_real64(fill_var, i), int32)
                end if
            end do
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for fill operation"
        end select
        
    end function where_mask_with_fill_variable
    
    !> Apply boolean mask with broadcasting
    function where_broadcast(mask, var) result(result)
        type(variable_t), intent(in) :: mask, var
        type(variable_t) :: result
        type(variable_t) :: expanded_mask
        integer :: i, j, mask_size, var_size
        integer :: n_true, idx
        
        mask_size = mask%n_elements
        var_size = var%n_elements
        
        ! Simple case: same size
        if (mask_size == var_size) then
            result = where_mask_only(mask, var)
            return
        end if
        
        ! Broadcasting case: mask is 1D, var is nD
        if (mask%n_dims == 1 .and. var%n_dims > 1) then
            ! Broadcast mask to match var dimensions
            expanded_mask = broadcast_mask_to_var(mask, var)
            result = where_mask_only(expanded_mask, var)
            call finalize_variable(expanded_mask)
        else
            write(error_unit,'(A)') "ERROR: Broadcasting not supported for this combination"
            result = var
        end if
        
    end function where_broadcast
    
    !> Create boolean mask from condition
    function create_mask_from_condition(var, condition) result(mask)
        type(variable_t), intent(in) :: var
        character(len=*), intent(in) :: condition
        type(variable_t) :: mask
        logical, dimension(:), allocatable :: mask_values
        integer :: i
        real(real64) :: val
        
        allocate(mask_values(var%n_elements))
        
        ! Parse simple conditions (this is a simplified implementation)
        select case(trim(condition))
        case(">0")
            do i = 1, var%n_elements
                val = get_value_as_real64(var, i)
                mask_values(i) = val > 0.0_real64
            end do
        case("<0")
            do i = 1, var%n_elements
                val = get_value_as_real64(var, i)
                mask_values(i) = val < 0.0_real64
            end do
        case("==0")
            do i = 1, var%n_elements
                val = get_value_as_real64(var, i)
                mask_values(i) = abs(val) < 1e-10
            end do
        case("isnan")
            do i = 1, var%n_elements
                val = get_value_as_real64(var, i)
                mask_values(i) = ieee_is_nan(val)
            end do
        case("notnan")
            do i = 1, var%n_elements
                val = get_value_as_real64(var, i)
                mask_values(i) = .not. ieee_is_nan(val)
            end do
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown condition: ", condition
            mask_values = .false.
        end select
        
        mask = create_mask_from_logical_array(mask_values, var%dim_names)
        mask%name = var%name // "_mask"
        if (allocated(var%shape)) then
            if (allocated(mask%shape)) deallocate(mask%shape)
            allocate(mask%shape(size(var%shape)))
            mask%shape = var%shape
        end if
        
    end function create_mask_from_condition
    
    !> Create boolean mask from logical array
    function create_mask_from_logical_array(logical_array, dim_names) result(mask)
        logical, dimension(:), intent(in) :: logical_array
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(variable_t) :: mask
        character(len=MAX_NAME_LEN), dimension(1) :: default_names
        integer :: n
        
        default_names(1) = "dim_1"
        n = size(logical_array)
        
        ! Manually create logical variable
        mask%name = "boolean_mask"
        mask%n_dims = 1
        mask%n_elements = n
        
        allocate(mask%dim_names(1))
        if (present(dim_names)) then
            mask%dim_names(1) = dim_names(1)
        else
            mask%dim_names(1) = default_names(1)
        end if
        
        allocate(mask%shape(1))
        mask%shape(1) = n
        
        ! Initialize data storage
        mask%data%dtype = DTYPE_LOGICAL
        mask%data%n_elements = n
        mask%data%initialized = .true.
        allocate(mask%data%values_logical(n))
        mask%data%values_logical = logical_array
        
        mask%initialized = .true.
        
    end function create_mask_from_logical_array
    
    !> Apply pre-created mask to variable
    function apply_mask(var, mask) result(result)
        type(variable_t), intent(in) :: var, mask
        type(variable_t) :: result
        
        result = where_mask_only(mask, var)
        
    end function apply_mask
    
    !======= Helper Functions =======!
    
    !> Extract values where mask is true
    function extract_masked_values(var, mask, n_true) result(result)
        type(variable_t), intent(in) :: var, mask
        integer, intent(in) :: n_true
        type(variable_t) :: result
        integer :: i, j
        
        ! Initialize result with correct size
        result%name = var%name // "_masked"
        result%n_dims = 1  ! Result is always 1D
        result%n_elements = n_true
        
        ! Set up dimension info
        allocate(result%dim_names(1))
        result%dim_names(1) = "masked_" // trim(var%dim_names(1))
        allocate(result%shape(1))
        result%shape(1) = n_true
        
        ! Initialize data storage
        result%data%dtype = var%data%dtype
        result%data%n_elements = n_true
        result%data%initialized = .true.
        
        ! Extract values based on data type
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            allocate(result%data%values_r64(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result%data%values_r64(j) = var%data%values_r64(i)
                end if
            end do
            
        case(DTYPE_REAL32)
            allocate(result%data%values_r32(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result%data%values_r32(j) = var%data%values_r32(i)
                end if
            end do
            
        case(DTYPE_INT64)
            allocate(result%data%values_i64(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result%data%values_i64(j) = var%data%values_i64(i)
                end if
            end do
            
        case(DTYPE_INT32)
            allocate(result%data%values_i32(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result%data%values_i32(j) = var%data%values_i32(i)
                end if
            end do
            
        case(DTYPE_LOGICAL)
            allocate(result%data%values_logical(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result%data%values_logical(j) = var%data%values_logical(i)
                end if
            end do
            
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for masking"
            result = create_empty_like(var)
            return
        end select
        
        ! Copy attributes
        result%units = var%units
        result%long_name = var%long_name
        result%initialized = .true.
        
        ! Set up coordinate tracking
        allocate(result%coords(1))
        allocate(result%has_coord(1))
        result%has_coord(1) = .false.
        
        ! Extract coordinates if present
        if (var%n_dims == 1 .and. allocated(var%coords) .and. var%has_coord(1)) then
            result%coords(1) = extract_coordinate_masked(var%coords(1), mask)
            result%has_coord(1) = .true.
        end if
        
    end function extract_masked_values
    
    !> Extract coordinate values based on mask
    function extract_coordinate_masked(coord, mask) result(result_coord)
        type(coordinate_t), intent(in) :: coord
        type(variable_t), intent(in) :: mask
        type(coordinate_t) :: result_coord
        integer :: i, j, n_true
        
        ! Count true values
        n_true = 0
        do i = 1, mask%n_elements
            if (mask%data%values_logical(i)) n_true = n_true + 1
        end do
        
        ! Initialize result coordinate
        result_coord%name = coord%name
        result_coord%length = n_true
        result_coord%dtype = coord%dtype
        result_coord%initialized = .true.
        
        ! Extract coordinate values based on type
        select case(coord%dtype)
        case(DTYPE_REAL64)
            allocate(result_coord%values_r64(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result_coord%values_r64(j) = coord%values_r64(i)
                end if
            end do
            
        case(DTYPE_REAL32)
            allocate(result_coord%values_r32(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result_coord%values_r32(j) = coord%values_r32(i)
                end if
            end do
            
        case(DTYPE_INT64)
            allocate(result_coord%values_i64(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result_coord%values_i64(j) = coord%values_i64(i)
                end if
            end do
            
        case(DTYPE_INT32)
            allocate(result_coord%values_i32(n_true))
            j = 0
            do i = 1, mask%n_elements
                if (mask%data%values_logical(i)) then
                    j = j + 1
                    result_coord%values_i32(j) = coord%values_i32(i)
                end if
            end do
            
        case default
            write(error_unit,'(A)') "WARNING: Unsupported coordinate type for masking"
        end select
        
    end function extract_coordinate_masked
    
    !> Broadcast 1D mask to match variable dimensions
    function broadcast_mask_to_var(mask, var) result(expanded_mask)
        type(variable_t), intent(in) :: mask, var
        type(variable_t) :: expanded_mask
        integer :: i, j, mask_idx
        integer :: stride
        
        ! Simple broadcasting: repeat mask along other dimensions
        expanded_mask%name = mask%name // "_expanded"
        expanded_mask%n_dims = var%n_dims
        expanded_mask%n_elements = var%n_elements
        
        ! Copy dimension info from var
        allocate(expanded_mask%dim_names(var%n_dims))
        expanded_mask%dim_names = var%dim_names
        if (allocated(var%shape)) then
            allocate(expanded_mask%shape(var%n_dims))
            expanded_mask%shape = var%shape
        end if
        
        ! Initialize data storage
        expanded_mask%data%dtype = DTYPE_LOGICAL
        expanded_mask%data%n_elements = var%n_elements
        expanded_mask%data%initialized = .true.
        allocate(expanded_mask%data%values_logical(var%n_elements))
        
        ! Simple broadcast: repeat mask pattern
        stride = var%n_elements / mask%n_elements
        do i = 1, var%n_elements
            mask_idx = mod((i - 1) / stride, mask%n_elements) + 1
            expanded_mask%data%values_logical(i) = mask%data%values_logical(mask_idx)
        end do
        
        expanded_mask%initialized = .true.
        
    end function broadcast_mask_to_var
    
    !> Get value at index as real64
    function get_value_as_real64(var, idx) result(value)
        type(variable_t), intent(in) :: var
        integer, intent(in) :: idx
        real(real64) :: value
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            value = var%data%values_r64(idx)
        case(DTYPE_REAL32)
            value = real(var%data%values_r32(idx), real64)
        case(DTYPE_INT64)
            value = real(var%data%values_i64(idx), real64)
        case(DTYPE_INT32)
            value = real(var%data%values_i32(idx), real64)
        case(DTYPE_LOGICAL)
            if (var%data%values_logical(idx)) then
                value = 1.0_real64
            else
                value = 0.0_real64
            end if
        case default
            value = 0.0_real64
        end select
        
    end function get_value_as_real64
    
    !> Create empty variable like another
    function create_empty_like(var) result(result)
        type(variable_t), intent(in) :: var
        type(variable_t) :: result
        
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
        
        ! Allocate empty data storage
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
        case(DTYPE_LOGICAL)
            allocate(result%data%values_logical(0))
        end select
        
        result%initialized = .true.
        
    end function create_empty_like

end module foxel_boolean_indexing