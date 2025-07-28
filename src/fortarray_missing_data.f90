module fortarray_missing_data
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors, only: new_array, new_dataset, &
        variable_scalar_real64, variable_scalar_real32, variable_scalar_int32, variable_scalar_int64
    use fortarray_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic
    implicit none
    private
    
    ! Public interfaces
    public :: isnull, isvalid
    public :: where
    public :: fillna
    public :: dropna
    public :: is_missing
    
    ! Generic interfaces
    interface fillna
        module procedure fillna_constant_r64
        module procedure fillna_constant_r32
        module procedure fillna_constant_i32
        module procedure fillna_constant_i64
        module procedure fillna_method
    end interface fillna
    
    interface where
        module procedure where_mask_values
        module procedure where_mask_scalar
    end interface where
    
contains
    
    !> Check if value is missing (huge() or NaN)
    elemental function is_missing(value) result(missing)
        real(real64), intent(in) :: value
        logical :: missing
        
        missing = (abs(value - huge(1.0_real64)) < 1e-10) .or. ieee_is_nan(value)
    end function is_missing
    
    !> Check for null/missing values in variable
    function isnull(var) result(mask)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: mask
        logical, dimension(:), allocatable :: mask_values
        integer :: i
        
        allocate(mask_values(var%n_elements))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                mask_values(i) = is_missing(var%data%values_r64(i))
                ! Also check against fill value if set
                if (var%has_fill_value) then
                    if (abs(var%data%values_r64(i) - var%fill_value_r64) < 1e-10) then
                        mask_values(i) = .true.
                    end if
                end if
            end do
            
        case(DTYPE_REAL32)
            do i = 1, var%n_elements
                mask_values(i) = ieee_is_nan(var%data%values_r32(i)) .or. &
                                (abs(var%data%values_r32(i) - huge(1.0_real32)) < 1e-6)
                if (var%has_fill_value) then
                    if (abs(var%data%values_r32(i) - var%fill_value_r32) < 1e-6) then
                        mask_values(i) = .true.
                    end if
                end if
            end do
            
        case(DTYPE_INT32)
            mask_values = .false.  ! Integers don't have NaN
            if (var%has_fill_value) then
                do i = 1, var%n_elements
                    if (var%data%values_i32(i) == var%fill_value_i32) then
                        mask_values(i) = .true.
                    end if
                end do
            end if
            
        case(DTYPE_INT64)
            mask_values = .false.  ! Integers don't have NaN
            if (var%has_fill_value) then
                do i = 1, var%n_elements
                    if (var%data%values_i64(i) == var%fill_value_i64) then
                        mask_values(i) = .true.
                    end if
                end do
            end if
            
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type in isnull"
            stop 1
        end select
        
        ! Create mask variable with same shape
        mask = create_logical_variable(mask_values, var%shape, var%dim_names, "null_mask")
        
    end function isnull
    
    !> Check for valid (non-missing) values
    function isvalid(var) result(mask)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: mask
        type(fortarray_t) :: null_mask
        integer :: i
        
        null_mask = isnull(var)
        
        ! Invert the null mask
        do i = 1, null_mask%n_elements
            null_mask%data%values_logical(i) = .not. null_mask%data%values_logical(i)
        end do
        
        mask = null_mask
        mask%name = "valid_mask"
        
    end function isvalid
    
    !> Apply where condition
    function where_mask_values(mask, true_values, false_values) result(result)
        type(fortarray_t), intent(in) :: mask, true_values, false_values
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: result_values
        integer :: i
        
        if (mask%n_elements /= true_values%n_elements .or. &
            mask%n_elements /= false_values%n_elements) then
            write(error_unit,'(A)') "ERROR: Shape mismatch in where()"
            stop 1
        end if
        
        allocate(result_values(mask%n_elements))
        
        ! Apply mask
        do i = 1, mask%n_elements
            if (mask%data%values_logical(i)) then
                result_values(i) = get_value_as_real64(true_values, i)
            else
                result_values(i) = get_value_as_real64(false_values, i)
            end if
        end do
        
        ! Create result with same shape as inputs
        if (allocated(true_values%shape)) then
            result = create_real64_variable(result_values, true_values%shape, &
                                          true_values%dim_names, "where_result")
        else
            result = variable_scalar_real64(result_values(1), name="where_result")
        end if
        
    end function where_mask_values
    
    !> Apply where condition with scalar false value
    function where_mask_scalar(mask, true_values, false_scalar) result(result)
        type(fortarray_t), intent(in) :: mask, true_values
        real(real64), intent(in) :: false_scalar
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: result_values
        integer :: i
        
        if (mask%n_elements /= true_values%n_elements) then
            write(error_unit,'(A)') "ERROR: Shape mismatch in where()"
            stop 1
        end if
        
        allocate(result_values(mask%n_elements))
        
        ! Apply mask
        do i = 1, mask%n_elements
            if (mask%data%values_logical(i)) then
                result_values(i) = get_value_as_real64(true_values, i)
            else
                result_values(i) = false_scalar
            end if
        end do
        
        ! Create result with same shape as inputs
        if (allocated(true_values%shape)) then
            result = create_real64_variable(result_values, true_values%shape, &
                                          true_values%dim_names, "where_result")
        else
            result = variable_scalar_real64(result_values(1), name="where_result")
        end if
        
    end function where_mask_scalar
    
    !> Fill missing values with constant
    function fillna_constant_r64(var, fill_value) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: fill_value
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: result_values
        integer :: i
        
        result_values = get_values_as_real64(var)
        
        do i = 1, var%n_elements
            if (is_missing(result_values(i))) then
                result_values(i) = fill_value
            end if
        end do
        
        if (allocated(var%shape)) then
            result = create_real64_variable(result_values, var%shape, &
                                          var%dim_names, var%name // "_filled")
        else
            result = variable_scalar_real64(result_values(1), name=var%name // "_filled")
        end if
        
    end function fillna_constant_r64
    
    !> Fill missing values with constant (real32)
    function fillna_constant_r32(var, fill_value) result(result)
        type(fortarray_t), intent(in) :: var
        real(real32), intent(in) :: fill_value
        type(fortarray_t) :: result
        
        result = fillna_constant_r64(var, real(fill_value, real64))
        
    end function fillna_constant_r32
    
    !> Fill missing values with constant (int32)
    function fillna_constant_i32(var, fill_value) result(result)
        type(fortarray_t), intent(in) :: var
        integer(int32), intent(in) :: fill_value
        type(fortarray_t) :: result
        
        result = fillna_constant_r64(var, real(fill_value, real64))
        
    end function fillna_constant_i32
    
    !> Fill missing values with constant (int64)
    function fillna_constant_i64(var, fill_value) result(result)
        type(fortarray_t), intent(in) :: var
        integer(int64), intent(in) :: fill_value
        type(fortarray_t) :: result
        
        result = fillna_constant_r64(var, real(fill_value, real64))
        
    end function fillna_constant_i64
    
    !> Fill missing values using method
    function fillna_method(var, method, limit) result(result)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: method
        integer, intent(in), optional :: limit
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values
        integer :: i, fill_count, max_fill
        
        values = get_values_as_real64(var)
        max_fill = merge(limit, var%n_elements, present(limit))
        
        select case(method)
        case("forward", "ffill")
            ! Forward fill
            fill_count = 0
            do i = 2, var%n_elements
                if (is_missing(values(i)) .and. .not. is_missing(values(i-1))) then
                    if (fill_count < max_fill) then
                        values(i) = values(i-1)
                        fill_count = fill_count + 1
                    end if
                else if (.not. is_missing(values(i))) then
                    fill_count = 0
                end if
            end do
            
        case("backward", "bfill")
            ! Backward fill
            fill_count = 0
            do i = var%n_elements-1, 1, -1
                if (is_missing(values(i)) .and. .not. is_missing(values(i+1))) then
                    if (fill_count < max_fill) then
                        values(i) = values(i+1)
                        fill_count = fill_count + 1
                    end if
                else if (.not. is_missing(values(i))) then
                    fill_count = 0
                end if
            end do
            
        case("linear")
            ! Linear interpolation
            call linear_interpolate_missing(values)
            
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown fillna method: ", method
            stop 1
        end select
        
        if (allocated(var%shape)) then
            result = create_real64_variable(values, var%shape, &
                                          var%dim_names, var%name // "_filled")
        else
            result = variable_scalar_real64(values(1), name=var%name // "_filled")
        end if
        
    end function fillna_method
    
    !> Drop missing values (1D only for now)
    function dropna(var, dim, how) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in), optional :: dim
        character(len=*), intent(in), optional :: how
        type(fortarray_t) :: result
        real(real64), dimension(:), allocatable :: values, clean_values
        logical, dimension(:), allocatable :: mask
        integer :: i, n_valid
        character(len=10) :: drop_how
        
        drop_how = "any"
        if (present(how)) drop_how = how
        
        if (present(dim) .and. var%n_dims > 1) then
            ! Multi-dimensional dropna not fully implemented
            result = dropna_along_dim(var, dim, drop_how)
        else
            ! 1D dropna
            values = get_values_as_real64(var)
            
            ! Count valid values
            n_valid = 0
            do i = 1, var%n_elements
                if (.not. is_missing(values(i))) then
                    n_valid = n_valid + 1
                end if
            end do
            
            ! Create clean array
            allocate(clean_values(n_valid))
            n_valid = 0
            do i = 1, var%n_elements
                if (.not. is_missing(values(i))) then
                    n_valid = n_valid + 1
                    clean_values(n_valid) = values(i)
                end if
            end do
            
            ! Create result variable
            if (var%n_dims == 1) then
                result = new_array(clean_values, name=var%name // "_dropna", &
                                dim_names=var%dim_names)
            else
                ! For flattened multi-dim, return 1D
                result = new_array(clean_values, name=var%name // "_dropna", &
                                dim_names=["index"])
            end if
        end if
        
    end function dropna
    
    !======= Helper Functions =======!
    
    !> Get single value as real64
    function get_value_as_real64(var, idx) result(value)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: idx
        real(real64) :: value
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            value = var%data%values_r64(idx)
        case(DTYPE_REAL32)
            value = real(var%data%values_r32(idx), real64)
        case(DTYPE_INT32)
            value = real(var%data%values_i32(idx), real64)
        case(DTYPE_INT64)
            value = real(var%data%values_i64(idx), real64)
        case default
            value = huge(1.0_real64)
        end select
        
    end function get_value_as_real64
    
    !> Get all values as real64
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
            write(error_unit,'(A)') "ERROR: Unsupported data type"
            stop 1
        end select
        
    end function get_values_as_real64
    
    !> Create logical variable
    function create_logical_variable(values, shape, dim_names, name) result(var)
        logical, dimension(:), intent(in) :: values
        integer, dimension(:), intent(in), allocatable :: shape
        character(len=*), dimension(:), intent(in), allocatable :: dim_names
        character(len=*), intent(in) :: name
        type(fortarray_t) :: var
        
        var%name = name
        var%n_elements = size(values)
        
        if (allocated(shape)) then
            var%n_dims = size(shape)
            allocate(var%shape(var%n_dims))
            var%shape = shape
            allocate(var%dim_names(var%n_dims))
            if (allocated(dim_names)) then
                var%dim_names = dim_names
            else
                var%dim_names = "dim"
            end if
        else
            var%n_dims = 0  ! Scalar
        end if
        
        var%data%dtype = DTYPE_LOGICAL
        allocate(var%data%values_logical(var%n_elements))
        var%data%values_logical = values
        
        var%initialized = .true.
        
    end function create_logical_variable
    
    !> Create real64 variable
    function create_real64_variable(values, shape, dim_names, name) result(var)
        real(real64), dimension(:), intent(in) :: values
        integer, dimension(:), intent(in), allocatable :: shape
        character(len=*), dimension(:), intent(in), allocatable :: dim_names
        character(len=*), intent(in) :: name
        type(fortarray_t) :: var
        
        var%name = name
        var%n_elements = size(values)
        
        if (allocated(shape)) then
            var%n_dims = size(shape)
            allocate(var%shape(var%n_dims))
            var%shape = shape
            allocate(var%dim_names(var%n_dims))
            if (allocated(dim_names)) then
                var%dim_names = dim_names
            else
                var%dim_names = "dim"
            end if
        else
            var%n_dims = 0  ! Scalar
        end if
        
        var%data%dtype = DTYPE_REAL64
        allocate(var%data%values_r64(var%n_elements))
        var%data%values_r64 = values
        
        var%initialized = .true.
        
    end function create_real64_variable
    
    !> Linear interpolate missing values
    subroutine linear_interpolate_missing(values)
        real(real64), dimension(:), intent(inout) :: values
        integer :: i, j, start_idx, end_idx
        real(real64) :: slope
        
        i = 1
        do while (i <= size(values))
            if (is_missing(values(i))) then
                ! Find start of missing region
                start_idx = i
                
                ! Find end of missing region
                j = i
                do while (j <= size(values))
                    if (.not. is_missing(values(j))) exit
                    j = j + 1
                end do
                end_idx = j - 1
                
                ! Interpolate if we have valid values on both sides
                if (start_idx > 1 .and. end_idx < size(values)) then
                    slope = (values(end_idx + 1) - values(start_idx - 1)) / &
                           real(end_idx - start_idx + 2, real64)
                    
                    do j = start_idx, end_idx
                        values(j) = values(start_idx - 1) + &
                                   slope * real(j - start_idx + 1, real64)
                    end do
                end if
                
                i = end_idx + 1
            else
                i = i + 1
            end if
        end do
        
    end subroutine linear_interpolate_missing
    
    !> Drop NA along dimension (simplified)
    function dropna_along_dim(var, dim, how) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        character(len=*), intent(in) :: how
        type(fortarray_t) :: result
        
        ! Simplified implementation - just return a copy for now
        ! Full implementation would require reshaping based on which slices have missing
        result = var
        result%name = var%name // "_dropna"
        
    end function dropna_along_dim

end module fortarray_missing_data