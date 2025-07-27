module foxel_constructors
    use foxel_types
    use foxel_storage
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Error codes
    integer, parameter :: CONSTRUCTOR_SUCCESS = 0
    integer, parameter :: CONSTRUCTOR_ERROR_DIMS = -1
    integer, parameter :: CONSTRUCTOR_ERROR_COORDS = -2
    integer, parameter :: CONSTRUCTOR_ERROR_SHAPE = -3
    integer, parameter :: CONSTRUCTOR_ERROR_ALLOCATION = -4
    integer, parameter :: CONSTRUCTOR_ERROR_VALIDATION = -5
    integer, parameter :: CONSTRUCTOR_ERROR_IO = -6
    
    ! Generic variable constructor interface
    interface variable
        module procedure variable_from_real64_1d
        module procedure variable_from_real64_2d
        module procedure variable_from_real64_3d
        module procedure variable_from_real32_1d
        module procedure variable_from_real32_2d
        module procedure variable_from_real32_3d
        module procedure variable_from_int32_1d
        module procedure variable_from_int32_2d
        module procedure variable_from_int32_3d
        module procedure variable_from_int64_1d
        module procedure variable_from_int64_2d
        module procedure variable_from_int64_3d
    end interface variable
    
    ! Legacy dataframe interface for compatibility
    interface dataframe
        module procedure dataframe_from_real64_1d
        module procedure dataframe_from_real64_2d
        module procedure dataframe_from_real64_3d
        module procedure dataframe_from_real32_1d
        module procedure dataframe_from_real32_2d
        module procedure dataframe_from_real32_3d
        module procedure dataframe_from_int32_1d
        module procedure dataframe_from_int32_2d
        module procedure dataframe_from_int32_3d
        module procedure dataframe_from_int64_1d
        module procedure dataframe_from_int64_2d
        module procedure dataframe_from_int64_3d
    end interface dataframe
    
    ! Generic from_array interface
    interface variable_from_array
        module procedure variable_from_real64_1d_simple
        module procedure variable_from_real64_2d_simple
        module procedure variable_from_real64_3d_simple
        module procedure variable_from_real32_1d_simple
        module procedure variable_from_real32_2d_simple
        module procedure variable_from_real32_3d_simple
        module procedure variable_from_int32_1d_simple
        module procedure variable_from_int32_2d_simple
        module procedure variable_from_int32_3d_simple
        module procedure variable_from_int64_1d_simple
        module procedure variable_from_int64_2d_simple
        module procedure variable_from_int64_3d_simple
    end interface variable_from_array
    
    ! Legacy interface
    interface dataframe_from_array
        module procedure variable_from_real64_1d_simple
        module procedure variable_from_real64_2d_simple
        module procedure variable_from_real64_3d_simple
        module procedure variable_from_real32_1d_simple
        module procedure variable_from_real32_2d_simple
        module procedure variable_from_real32_3d_simple
        module procedure variable_from_int32_1d_simple
        module procedure variable_from_int32_2d_simple
        module procedure variable_from_int32_3d_simple
        module procedure variable_from_int64_1d_simple
        module procedure variable_from_int64_2d_simple
        module procedure variable_from_int64_3d_simple
    end interface dataframe_from_array
    
    ! Generic scalar constructor
    interface variable_scalar
        module procedure variable_scalar_real64
        module procedure variable_scalar_real32
        module procedure variable_scalar_int32
        module procedure variable_scalar_int64
    end interface variable_scalar
    
    ! Legacy interface
    interface dataframe_scalar
        module procedure variable_scalar_real64
        module procedure variable_scalar_real32
        module procedure variable_scalar_int32
        module procedure variable_scalar_int64
    end interface dataframe_scalar
    
    ! Dataset constructor
    interface dataset
        module procedure dataset_empty
        module procedure dataset_from_variables
    end interface dataset
    
    ! Public interfaces
    public :: variable, dataframe  ! dataframe for backward compatibility
    public :: variable_from_array, dataframe_from_array
    public :: variable_from_csv, dataframe_from_csv
    public :: variable_empty, dataframe_empty
    public :: variable_scalar, dataframe_scalar
    public :: dataset
    public :: create_coordinate
    public :: validate_dimension_names
    public :: validate_coordinates
    
    ! Public error codes
    public :: CONSTRUCTOR_SUCCESS, CONSTRUCTOR_ERROR_DIMS
    public :: CONSTRUCTOR_ERROR_COORDS, CONSTRUCTOR_ERROR_SHAPE
    public :: CONSTRUCTOR_ERROR_ALLOCATION, CONSTRUCTOR_ERROR_VALIDATION
    public :: CONSTRUCTOR_ERROR_IO
    
contains

    !> Create coordinate helper
    subroutine create_coordinate(coord, length, dtype_name, stat, char_len)
        type(coordinate_t), intent(out) :: coord
        integer, intent(in) :: length
        character(len=*), intent(in) :: dtype_name
        integer, intent(out) :: stat
        integer, intent(in), optional :: char_len
        
        integer :: dtype, i
        
        stat = CONSTRUCTOR_SUCCESS
        
        dtype = storage_type_from_name(dtype_name)
        if (dtype == 0) then
            stat = CONSTRUCTOR_ERROR_VALIDATION
            return
        end if
        
        coord%initialized = .true.
        coord%length = length
        coord%dtype = dtype
        
        select case(dtype)
        case(DTYPE_INT32)
            allocate(coord%values_i32(length), stat=stat)
            if (stat /= 0) then
                stat = CONSTRUCTOR_ERROR_ALLOCATION
                return
            end if
            coord%values_i32 = [(i, i=1,length)]
        case(DTYPE_INT64)
            allocate(coord%values_i64(length), stat=stat)
            if (stat /= 0) then
                stat = CONSTRUCTOR_ERROR_ALLOCATION
                return
            end if
            coord%values_i64 = [(int(i, int64), i=1,length)]
        case(DTYPE_REAL32)
            allocate(coord%values_r32(length), stat=stat)
            if (stat /= 0) then
                stat = CONSTRUCTOR_ERROR_ALLOCATION
                return
            end if
            coord%values_r32 = [(real(i, real32), i=1,length)]
        case(DTYPE_REAL64)
            allocate(coord%values_r64(length), stat=stat)
            if (stat /= 0) then
                stat = CONSTRUCTOR_ERROR_ALLOCATION
                return
            end if
            coord%values_r64 = [(real(i, real64), i=1,length)]
        case(DTYPE_CHAR)
            if (present(char_len)) then
                allocate(character(len=char_len) :: coord%values_char(length), stat=stat)
            else
                allocate(character(len=20) :: coord%values_char(length), stat=stat)
            end if
            if (stat /= 0) then
                stat = CONSTRUCTOR_ERROR_ALLOCATION
                return
            end if
        case default
            stat = CONSTRUCTOR_ERROR_VALIDATION
        end select
        
        ! Check monotonicity
        coord%is_monotonic = .true.  ! Default, should be checked properly
    end subroutine create_coordinate
    
    
    !> Validate dimension names
    function validate_dimension_names(dim_names, error_msg) result(is_valid)
        character(len=*), dimension(:), intent(in) :: dim_names
        character(len=*), intent(out) :: error_msg
        logical :: is_valid
        
        integer :: i, j
        
        is_valid = .true.
        error_msg = ""
        
        ! Check for duplicates
        do i = 1, size(dim_names)
            do j = i + 1, size(dim_names)
                if (trim(dim_names(i)) == trim(dim_names(j))) then
                    is_valid = .false.
                    write(error_msg, '(A,A,A)') "Duplicate dimension name: '", &
                                                trim(dim_names(i)), "'"
                    return
                end if
            end do
        end do
        
        ! Check for valid identifiers
        do i = 1, size(dim_names)
            if (len_trim(dim_names(i)) == 0) then
                is_valid = .false.
                write(error_msg, '(A,I0)') "Empty dimension name at position ", i
                return
            end if
            
            ! Check first character is letter
            if (.not. is_letter(dim_names(i)(1:1))) then
                is_valid = .false.
                write(error_msg, '(A,A,A)') "Invalid dimension name: '", &
                                            trim(dim_names(i)), "' (must start with letter)"
                return
            end if
        end do
    end function validate_dimension_names
    
    !> Check if character is a letter
    function is_letter(ch) result(is_let)
        character(len=1), intent(in) :: ch
        logical :: is_let
        
        is_let = (ch >= 'A' .and. ch <= 'Z') .or. (ch >= 'a' .and. ch <= 'z')
    end function is_letter
    
    !> Validate coordinates against shape
    function validate_coordinates(coords, shape, error_msg) result(is_valid)
        type(coordinate_t), dimension(:), intent(in) :: coords
        integer, dimension(:), intent(in) :: shape
        character(len=*), intent(out) :: error_msg
        logical :: is_valid
        
        integer :: i
        
        is_valid = .true.
        error_msg = ""
        
        if (size(coords) /= size(shape)) then
            is_valid = .false.
            write(error_msg, '(A,I0,A,I0)') "Coordinate count (", size(coords), &
                                             ") doesn't match dimension count (", size(shape), ")"
            return
        end if
        
        do i = 1, size(coords)
            if (coords(i)%initialized .and. coords(i)%length /= shape(i)) then
                is_valid = .false.
                write(error_msg, '(A,I0,A,I0,A,I0,A)') &
                    "Coordinate ", i, " has length ", coords(i)%length, &
                    " but dimension has length ", shape(i), ""
                return
            end if
        end do
    end function validate_coordinates
    
    !> Check if coordinate is monotonic (helper function)
    function is_coordinate_monotonic(coord) result(is_mono)
        type(coordinate_t), intent(in) :: coord
        logical :: is_mono
        integer :: i
        
        is_mono = .true.
        
        select case(coord%dtype)
        case(DTYPE_INT32)
            do i = 2, coord%length
                if (coord%values_i32(i) <= coord%values_i32(i-1)) then
                    is_mono = .false.
                    return
                end if
            end do
        case(DTYPE_INT64)
            do i = 2, coord%length
                if (coord%values_i64(i) <= coord%values_i64(i-1)) then
                    is_mono = .false.
                    return
                end if
            end do
        case(DTYPE_REAL32)
            do i = 2, coord%length
                if (coord%values_r32(i) <= coord%values_r32(i-1)) then
                    is_mono = .false.
                    return
                end if
            end do
        case(DTYPE_REAL64)
            do i = 2, coord%length
                if (coord%values_r64(i) <= coord%values_r64(i-1)) then
                    is_mono = .false.
                    return
                end if
            end do
        end select
    end function is_coordinate_monotonic
    
    !> Deep copy a coordinate
    subroutine copy_coordinate(src, dest)
        type(coordinate_t), intent(in) :: src
        type(coordinate_t), intent(out) :: dest
        
        ! Initialize dest to safe defaults first
        dest%initialized = .false.
        dest%name = ""
        dest%dtype = 0
        dest%length = 0
        dest%is_monotonic = .false.
        dest%n_attrs = 0
        
        ! Check if source is initialized
        if (.not. src%initialized) return
        
        ! Now copy fields
        dest%initialized = src%initialized
        dest%name = src%name
        dest%dtype = src%dtype
        dest%length = src%length
        dest%is_monotonic = src%is_monotonic
        dest%n_attrs = src%n_attrs
        
        ! Copy data arrays
        if (src%initialized) then
            select case(src%dtype)
            case(DTYPE_INT32)
                if (allocated(src%values_i32)) then
                    allocate(dest%values_i32(src%length))
                    dest%values_i32 = src%values_i32
                end if
            case(DTYPE_INT64)
                if (allocated(src%values_i64)) then
                    allocate(dest%values_i64(src%length))
                    dest%values_i64 = src%values_i64
                end if
            case(DTYPE_REAL32)
                if (allocated(src%values_r32)) then
                    allocate(dest%values_r32(src%length))
                    dest%values_r32 = src%values_r32
                end if
            case(DTYPE_REAL64)
                if (allocated(src%values_r64)) then
                    allocate(dest%values_r64(src%length))
                    dest%values_r64 = src%values_r64
                end if
            case(DTYPE_CHAR)
                if (allocated(src%values_char)) then
                    allocate(dest%values_char(src%length), source=src%values_char)
                end if
            end select
        end if
        
        ! Copy attributes
        if (src%n_attrs > 0) then
            allocate(dest%attrs(src%n_attrs))
            dest%attrs = src%attrs
        end if
    end subroutine copy_coordinate
    
    !> Create an empty variable with specified dimensions
    function variable_empty(dim_names, shape, dtype, stat, error_msg) result(var)
        character(len=*), dimension(:), intent(in) :: dim_names
        integer, dimension(:), intent(in) :: shape
        character(len=*), intent(in), optional :: dtype
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        integer :: status, total_elements, i
        character(len=256) :: err_msg
        character(len=20) :: dtype_str
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        dtype_str = "real64"
        if (present(dtype)) dtype_str = dtype
        
        ! Validate inputs
        if (size(dim_names) /= size(shape)) then
            status = CONSTRUCTOR_ERROR_DIMS
            err_msg = "Dimension names and shape arrays must have same length"
            goto 999
        end if
        
        ! Validate dimension names
        if (.not. validate_dimension_names(dim_names, err_msg)) then
            status = CONSTRUCTOR_ERROR_DIMS
            goto 999
        end if
        
        ! Check for zero-sized dimensions
        do i = 1, size(shape)
            if (shape(i) <= 0) then
                status = CONSTRUCTOR_ERROR_SHAPE
                write(err_msg, '(A,I0,A,I0)') "Invalid shape at dimension ", i, ": ", shape(i)
                goto 999
            end if
        end do
        
        ! Initialize variable
        var%initialized = .true.
        var%n_dims = size(dim_names)
        
        ! Allocate arrays
        allocate(var%dim_names(var%n_dims), stat=status)
        if (status /= 0) goto 999
        allocate(var%shape(var%n_dims), stat=status)
        if (status /= 0) goto 999
        allocate(var%coords(var%n_dims), stat=status)
        if (status /= 0) goto 999
        allocate(var%has_coord(var%n_dims), stat=status)
        if (status /= 0) goto 999
        
        ! Set values
        var%dim_names = dim_names
        var%shape = shape
        var%has_coord = .false.
        
        ! Calculate total elements
        total_elements = 1
        do i = 1, var%n_dims
            total_elements = total_elements * shape(i)
        end do
        var%n_elements = total_elements
        
        ! Create storage
        call create_storage(var%data, total_elements, dtype_str, status)
        if (status /= 0) then
            err_msg = "Failed to create data storage"
            goto 999
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_empty
    
    ! Legacy name
    function dataframe_empty(dim_names, shape, dtype, stat, error_msg) result(df)
        character(len=*), dimension(:), intent(in) :: dim_names
        integer, dimension(:), intent(in) :: shape
        character(len=*), intent(in), optional :: dtype
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_empty(dim_names, shape, dtype, stat, error_msg)
    end function dataframe_empty
    
    !> Create variable from 1D real64 array
    function variable_from_real64_1d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(var)
        real(real64), dimension(:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(1) :: dims
        integer :: status
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension name
        dims(1) = "dim_1"
        if (present(dim_names)) then
            if (size(dim_names) == 1) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "1D data requires exactly 1 dimension name"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data)], "real64", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data
        var%data%values_r64 = data
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 1) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "1D data requires exactly 1 coordinate array"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords(1) = coords(1)
            var%has_coord(1) = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_real64_1d
    
    !> Create variable from 2D real64 array
    function variable_from_real64_2d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(var)
        real(real64), dimension(:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(2) :: dims
        integer :: status, i
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension names
        dims = ["dim_1", "dim_2"]
        if (present(dim_names)) then
            if (size(dim_names) == 2) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "2D data requires exactly 2 dimension names"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data,1), size(data,2)], "real64", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data (flatten to 1D)
        var%data%values_r64 = reshape(data, [var%n_elements])
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 2) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "2D data requires exactly 2 coordinate arrays"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords = coords
            var%has_coord = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_real64_2d
    
    !> Create variable from 3D real64 array
    function variable_from_real64_3d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(var)
        real(real64), dimension(:,:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(3) :: dims
        integer :: status, i
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension names
        dims = ["dim_1", "dim_2", "dim_3"]
        if (present(dim_names)) then
            if (size(dim_names) == 3) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "3D data requires exactly 3 dimension names"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data,1), size(data,2), size(data,3)], &
                            "real64", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data (flatten to 1D)
        var%data%values_r64 = reshape(data, [var%n_elements])
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 3) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "3D data requires exactly 3 coordinate arrays"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords = coords
            var%has_coord = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_real64_3d
    
    ! Similar implementations for other types (real32, int32, int64)
    ! These follow the same pattern as above
    
    !> Create variable from 1D array with simple interface
    function variable_from_real64_1d_simple(data, dim_name, stat) result(var)
        real(real64), dimension(:), intent(in) :: data
        character(len=*), intent(in), optional :: dim_name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(1) :: dims
        
        dims(1) = "dim_1"
        if (present(dim_name)) dims(1) = dim_name
        
        var = variable_from_real64_1d(data, dims, stat=stat)
    end function variable_from_real64_1d_simple
    
    !> Create scalar variable
    function variable_scalar_real64(value, name, stat) result(var)
        real(real64), intent(in) :: value
        character(len=*), intent(in), optional :: name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        integer :: status
        
        ! Create 0D variable
        var%initialized = .true.
        var%n_dims = 0
        var%n_elements = 1
        if (present(name)) var%name = name
        
        ! Create storage
        call create_storage(var%data, 1, "real64", status)
        if (status == 0) then
            var%data%values_r64(1) = value
        else
            var%initialized = .false.
        end if
        
        if (present(stat)) stat = status
    end function variable_scalar_real64
    
    !> Create variable from 1D real32 array
    function variable_from_real32_1d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(var)
        real(real32), dimension(:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(1) :: dims
        integer :: status
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension name
        dims(1) = "dim_1"
        if (present(dim_names)) then
            if (size(dim_names) == 1) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "1D data requires exactly 1 dimension name"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data)], "real32", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data
        var%data%values_r32 = data
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 1) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "1D data requires exactly 1 coordinate array"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords(1) = coords(1)
            var%has_coord(1) = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_real32_1d
    
    !> Create variable from 2D real32 array
    function variable_from_real32_2d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(var)
        real(real32), dimension(:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(2) :: dims
        integer :: status, i
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension names
        dims = ["dim_1", "dim_2"]
        if (present(dim_names)) then
            if (size(dim_names) == 2) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "2D data requires exactly 2 dimension names"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data,1), size(data,2)], "real32", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data (flatten to 1D)
        var%data%values_r32 = reshape(data, [var%n_elements])
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 2) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "2D data requires exactly 2 coordinate arrays"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords = coords
            var%has_coord = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_real32_2d
    
    !> Create variable from 3D real32 array
    function variable_from_real32_3d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(var)
        real(real32), dimension(:,:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(3) :: dims
        integer :: status, i
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension names
        dims = ["dim_1", "dim_2", "dim_3"]
        if (present(dim_names)) then
            if (size(dim_names) == 3) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "3D data requires exactly 3 dimension names"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data,1), size(data,2), size(data,3)], &
                            "real32", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data (flatten to 1D)
        var%data%values_r32 = reshape(data, [var%n_elements])
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 3) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "3D data requires exactly 3 coordinate arrays"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords = coords
            var%has_coord = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_real32_3d
    
    !> Create variable from 1D int32 array
    function variable_from_int32_1d(data, dim_names, coords, name, &
                                   check_monotonic, stat, error_msg) result(var)
        integer(int32), dimension(:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(1) :: dims
        integer :: status
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension name
        dims(1) = "dim_1"
        if (present(dim_names)) then
            if (size(dim_names) == 1) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "1D data requires exactly 1 dimension name"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data)], "int32", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data
        var%data%values_i32 = data
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 1) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "1D data requires exactly 1 coordinate array"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords(1) = coords(1)
            var%has_coord(1) = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_int32_1d
    
    !> Create variable from 2D int32 array
    function variable_from_int32_2d(data, dim_names, coords, name, &
                                   check_monotonic, stat, error_msg) result(var)
        integer(int32), dimension(:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(2) :: dims
        integer :: status, i
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension names
        dims = ["dim_1", "dim_2"]
        if (present(dim_names)) then
            if (size(dim_names) == 2) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "2D data requires exactly 2 dimension names"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data,1), size(data,2)], "int32", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data (flatten to 1D)
        var%data%values_i32 = reshape(data, [var%n_elements])
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 2) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "2D data requires exactly 2 coordinate arrays"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords = coords
            var%has_coord = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_int32_2d
    
    !> Create variable from 3D int32 array
    function variable_from_int32_3d(data, dim_names, coords, name, &
                                   check_monotonic, stat, error_msg) result(var)
        integer(int32), dimension(:,:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(3) :: dims
        integer :: status, i
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension names
        dims = ["dim_1", "dim_2", "dim_3"]
        if (present(dim_names)) then
            if (size(dim_names) == 3) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "3D data requires exactly 3 dimension names"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data,1), size(data,2), size(data,3)], &
                            "int32", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data (flatten to 1D)
        var%data%values_i32 = reshape(data, [var%n_elements])
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 3) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "3D data requires exactly 3 coordinate arrays"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords = coords
            var%has_coord = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_int32_3d
    
    !> Create variable from 1D int64 array
    function variable_from_int64_1d(data, dim_names, coords, name, &
                                   check_monotonic, stat, error_msg) result(var)
        integer(int64), dimension(:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(1) :: dims
        integer :: status
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension name
        dims(1) = "dim_1"
        if (present(dim_names)) then
            if (size(dim_names) == 1) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "1D data requires exactly 1 dimension name"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data)], "int64", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data
        var%data%values_i64 = data
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 1) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "1D data requires exactly 1 coordinate array"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords(1) = coords(1)
            var%has_coord(1) = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_int64_1d
    
    !> Create variable from 2D int64 array
    function variable_from_int64_2d(data, dim_names, coords, name, &
                                   check_monotonic, stat, error_msg) result(var)
        integer(int64), dimension(:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(2) :: dims
        integer :: status, i
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension names
        dims = ["dim_1", "dim_2"]
        if (present(dim_names)) then
            if (size(dim_names) == 2) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "2D data requires exactly 2 dimension names"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data,1), size(data,2)], "int64", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data (flatten to 1D)
        var%data%values_i64 = reshape(data, [var%n_elements])
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 2) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "2D data requires exactly 2 coordinate arrays"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords = coords
            var%has_coord = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_int64_2d
    
    !> Create variable from 3D int64 array
    function variable_from_int64_3d(data, dim_names, coords, name, &
                                   check_monotonic, stat, error_msg) result(var)
        integer(int64), dimension(:,:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(3) :: dims
        integer :: status, i
        character(len=256) :: err_msg
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Set default dimension names
        dims = ["dim_1", "dim_2", "dim_3"]
        if (present(dim_names)) then
            if (size(dim_names) == 3) then
                dims = dim_names
            else
                status = CONSTRUCTOR_ERROR_DIMS
                err_msg = "3D data requires exactly 3 dimension names"
                goto 999
            end if
        end if
        
        ! Create empty variable
        var = variable_empty(dims, [size(data,1), size(data,2), size(data,3)], &
                            "int64", status, err_msg)
        if (status /= 0) goto 999
        
        ! Copy data (flatten to 1D)
        var%data%values_i64 = reshape(data, [var%n_elements])
        
        ! Set name
        if (present(name)) var%name = name
        
        ! Handle coordinates
        if (present(coords)) then
            if (size(coords) /= 3) then
                status = CONSTRUCTOR_ERROR_COORDS
                err_msg = "3D data requires exactly 3 coordinate arrays"
                goto 999
            end if
            
            if (.not. validate_coordinates(coords, var%shape, err_msg)) then
                status = CONSTRUCTOR_ERROR_COORDS
                goto 999
            end if
            
            ! Fortran automatically deep copies allocatable components
            var%coords = coords
            var%has_coord = .true.
            
            ! Monotonicity already preserved in copy_coordinate
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_int64_3d
    
    !> Create variable from 2D real64 array with simple interface
    function variable_from_real64_2d_simple(data, stat) result(var)
        real(real64), dimension(:,:), intent(in) :: data
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(2) :: dims
        
        dims = ["dim_1", "dim_2"]
        var = variable_from_real64_2d(data, dims, stat=stat)
    end function variable_from_real64_2d_simple
    
    !> Create variable from 3D real64 array with simple interface
    function variable_from_real64_3d_simple(data, stat) result(var)
        real(real64), dimension(:,:,:), intent(in) :: data
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(3) :: dims
        
        dims = ["dim_1", "dim_2", "dim_3"]
        var = variable_from_real64_3d(data, dims, stat=stat)
    end function variable_from_real64_3d_simple
    
    !> Create variable from 1D real32 array with simple interface
    function variable_from_real32_1d_simple(data, dim_name, stat) result(var)
        real(real32), dimension(:), intent(in) :: data
        character(len=*), intent(in), optional :: dim_name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(1) :: dims
        
        dims(1) = "dim_1"
        if (present(dim_name)) dims(1) = dim_name
        
        var = variable_from_real32_1d(data, dims, stat=stat)
    end function variable_from_real32_1d_simple
    
    !> Create variable from 2D real32 array with simple interface
    function variable_from_real32_2d_simple(data, stat) result(var)
        real(real32), dimension(:,:), intent(in) :: data
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(2) :: dims
        
        dims = ["dim_1", "dim_2"]
        var = variable_from_real32_2d(data, dims, stat=stat)
    end function variable_from_real32_2d_simple
    
    !> Create variable from 3D real32 array with simple interface
    function variable_from_real32_3d_simple(data, stat) result(var)
        real(real32), dimension(:,:,:), intent(in) :: data
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(3) :: dims
        
        dims = ["dim_1", "dim_2", "dim_3"]
        var = variable_from_real32_3d(data, dims, stat=stat)
    end function variable_from_real32_3d_simple
    
    !> Create variable from 1D int32 array with simple interface
    function variable_from_int32_1d_simple(data, dim_name, stat) result(var)
        integer(int32), dimension(:), intent(in) :: data
        character(len=*), intent(in), optional :: dim_name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(1) :: dims
        
        dims(1) = "dim_1"
        if (present(dim_name)) dims(1) = dim_name
        
        var = variable_from_int32_1d(data, dims, stat=stat)
    end function variable_from_int32_1d_simple
    
    !> Create variable from 2D int32 array with simple interface
    function variable_from_int32_2d_simple(data, stat) result(var)
        integer(int32), dimension(:,:), intent(in) :: data
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(2) :: dims
        
        dims = ["dim_1", "dim_2"]
        var = variable_from_int32_2d(data, dims, stat=stat)
    end function variable_from_int32_2d_simple
    
    !> Create variable from 3D int32 array with simple interface
    function variable_from_int32_3d_simple(data, stat) result(var)
        integer(int32), dimension(:,:,:), intent(in) :: data
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(3) :: dims
        
        dims = ["dim_1", "dim_2", "dim_3"]
        var = variable_from_int32_3d(data, dims, stat=stat)
    end function variable_from_int32_3d_simple
    
    !> Create variable from 1D int64 array with simple interface
    function variable_from_int64_1d_simple(data, dim_name, stat) result(var)
        integer(int64), dimension(:), intent(in) :: data
        character(len=*), intent(in), optional :: dim_name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(1) :: dims
        
        dims(1) = "dim_1"
        if (present(dim_name)) dims(1) = dim_name
        
        var = variable_from_int64_1d(data, dims, stat=stat)
    end function variable_from_int64_1d_simple
    
    !> Create variable from 2D int64 array with simple interface
    function variable_from_int64_2d_simple(data, stat) result(var)
        integer(int64), dimension(:,:), intent(in) :: data
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(2) :: dims
        
        dims = ["dim_1", "dim_2"]
        var = variable_from_int64_2d(data, dims, stat=stat)
    end function variable_from_int64_2d_simple
    
    !> Create variable from 3D int64 array with simple interface
    function variable_from_int64_3d_simple(data, stat) result(var)
        integer(int64), dimension(:,:,:), intent(in) :: data
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        character(len=MAX_NAME_LEN), dimension(3) :: dims
        
        dims = ["dim_1", "dim_2", "dim_3"]
        var = variable_from_int64_3d(data, dims, stat=stat)
    end function variable_from_int64_3d_simple
    
    !> Create scalar variable from real32
    function variable_scalar_real32(value, name, stat) result(var)
        real(real32), intent(in) :: value
        character(len=*), intent(in), optional :: name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        integer :: status
        
        ! Create 0D variable
        var%initialized = .true.
        var%n_dims = 0
        var%n_elements = 1
        if (present(name)) var%name = name
        
        ! Create storage
        call create_storage(var%data, 1, "real32", status)
        if (status == 0) then
            var%data%values_r32(1) = value
        else
            var%initialized = .false.
        end if
        
        if (present(stat)) stat = status
    end function variable_scalar_real32
    
    !> Create scalar variable from int32
    function variable_scalar_int32(value, name, stat) result(var)
        integer(int32), intent(in) :: value
        character(len=*), intent(in), optional :: name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        integer :: status
        
        ! Create 0D variable
        var%initialized = .true.
        var%n_dims = 0
        var%n_elements = 1
        if (present(name)) var%name = name
        
        ! Create storage
        call create_storage(var%data, 1, "int32", status)
        if (status == 0) then
            var%data%values_i32(1) = value
        else
            var%initialized = .false.
        end if
        
        if (present(stat)) stat = status
    end function variable_scalar_int32
    
    !> Create scalar variable from int64
    function variable_scalar_int64(value, name, stat) result(var)
        integer(int64), intent(in) :: value
        character(len=*), intent(in), optional :: name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        integer :: status
        
        ! Create 0D variable
        var%initialized = .true.
        var%n_dims = 0
        var%n_elements = 1
        if (present(name)) var%name = name
        
        ! Create storage
        call create_storage(var%data, 1, "int64", status)
        if (status == 0) then
            var%data%values_i64(1) = value
        else
            var%initialized = .false.
        end if
        
        if (present(stat)) stat = status
    end function variable_scalar_int64
    
    !> Create variable from CSV file (basic implementation)
    function variable_from_csv(filename, stat, error_msg) result(var)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        integer :: unit, status, iostat
        integer :: n_rows, n_cols, i, j
        character(len=1024) :: line
        character(len=256) :: err_msg
        character(len=256), allocatable :: headers(:)
        real(real64), allocatable :: data_array(:,:)
        
        status = CONSTRUCTOR_SUCCESS
        err_msg = ""
        
        ! Open file
        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            status = CONSTRUCTOR_ERROR_IO
            write(err_msg, '(A,A,A)') "Cannot open file '", trim(filename), "'"
            goto 999
        end if
        
        ! Read header line
        read(unit, '(A)', iostat=iostat) line
        if (iostat /= 0) then
            status = CONSTRUCTOR_ERROR_IO
            err_msg = "Cannot read header line"
            close(unit)
            goto 999
        end if
        
        ! Count columns from header (simple comma count)
        n_cols = 1
        do i = 1, len_trim(line)
            if (line(i:i) == ',') n_cols = n_cols + 1
        end do
        
        ! Allocate space for headers
        allocate(headers(n_cols))
        
        ! Parse headers (simple comma split)
        j = 1
        headers(1) = ""
        do i = 1, len_trim(line)
            if (line(i:i) == ',') then
                j = j + 1
                headers(j) = ""
            else
                headers(j) = trim(headers(j)) // line(i:i)
            end if
        end do
        
        ! Count rows
        n_rows = 0
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) > 0) n_rows = n_rows + 1
        end do
        
        if (n_rows == 0) then
            status = CONSTRUCTOR_ERROR_IO
            err_msg = "No data rows in CSV file"
            close(unit)
            goto 999
        end if
        
        ! Allocate data array
        allocate(data_array(n_rows, n_cols), stat=iostat)
        if (iostat /= 0) then
            status = CONSTRUCTOR_ERROR_ALLOCATION
            err_msg = "Cannot allocate data array"
            close(unit)
            goto 999
        end if
        
        ! Rewind and skip header
        rewind(unit)
        read(unit, '(A)')
        
        ! Read data (simple implementation - assumes numeric data)
        do i = 1, n_rows
            read(unit, *, iostat=iostat) (data_array(i,j), j=1,n_cols)
            if (iostat /= 0) then
                status = CONSTRUCTOR_ERROR_IO
                write(err_msg, '(A,I0)') "Error reading data at row ", i
                close(unit)
                goto 999
            end if
        end do
        
        close(unit)
        
        ! Create variable from data
        var = variable_empty(["rows ", "cols "], [n_rows, n_cols], "real64", status, err_msg)
        if (status == 0) then
            var%data%values_r64 = reshape(data_array, [n_rows * n_cols])
            var%name = filename
            
            ! Store CSV metadata and column names as attributes
            var%n_attrs = n_cols + 1
            allocate(var%attrs(var%n_attrs))
            
            ! Store format
            var%attrs(1)%name = "source_format"
            var%attrs(1)%value = "CSV"
            var%attrs(1)%dtype = ATTR_TYPE_STRING
            
            ! Store column names
            do i = 1, n_cols
                write(var%attrs(i+1)%name, '(A,I0)') "column_", i
                var%attrs(i+1)%value = trim(adjustl(headers(i)))
                var%attrs(i+1)%dtype = ATTR_TYPE_STRING
            end do
        end if
        
        if (allocated(data_array)) deallocate(data_array)
        if (allocated(headers)) deallocate(headers)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        if (status /= 0) var%initialized = .false.
    end function variable_from_csv
    
    function dataframe_from_csv(filename, stat, error_msg) result(df)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_csv(filename, stat, error_msg)
    end function dataframe_from_csv
    
    !> Create empty dataset
    function dataset_empty() result(ds)
        type(dataset_t) :: ds
        
        ds%initialized = .true.
        ds%n_dims = 0
        ds%n_vars = 0
        ds%n_coords = 0
        ds%n_attrs = 0
        
        ! Allocate empty arrays
        allocate(ds%variables(10))  ! Initial capacity
        allocate(ds%dimensions(0))
        allocate(ds%attr_keys(0))
        allocate(ds%attr_values(0))
        allocate(ds%coord_var_indices(0))
    end function dataset_empty
    
    !> Create dataset from variables
    function dataset_from_variables(variables, var_names, stat, error_msg) result(ds)
        type(variable_t), dimension(:), intent(in) :: variables
        character(len=*), dimension(:), intent(in) :: var_names
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataset_t) :: ds
        
        integer :: status
        
        status = CONSTRUCTOR_SUCCESS
        
        if (size(variables) /= size(var_names)) then
            status = CONSTRUCTOR_ERROR_VALIDATION
            if (present(error_msg)) error_msg = "Variable and name arrays must have same size"
            if (present(stat)) stat = status
            return
        end if
        
        ds = dataset_empty()
        ds%n_vars = size(variables)
        
        allocate(ds%variables(ds%n_vars))
        allocate(ds%var_names(ds%n_vars))
        
        ds%variables = variables
        ds%var_names = var_names
        
        if (present(stat)) stat = status
    end function dataset_from_variables
    
    ! Legacy wrapper functions
    function dataframe_from_real64_1d(data, dim_names, coords, name, &
                                     check_monotonic, stat, error_msg) result(df)
        real(real64), dimension(:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_real64_1d(data, dim_names, coords, name, &
                                        check_monotonic, stat, error_msg)
    end function dataframe_from_real64_1d
    
    function dataframe_from_real64_2d(data, dim_names, coords, name, &
                                     check_monotonic, stat, error_msg) result(df)
        real(real64), dimension(:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_real64_2d(data, dim_names, coords, name, &
                                        check_monotonic, stat, error_msg)
    end function dataframe_from_real64_2d
    
    function dataframe_from_real64_3d(data, dim_names, coords, name, &
                                     check_monotonic, stat, error_msg) result(df)
        real(real64), dimension(:,:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_real64_3d(data, dim_names, coords, name, &
                                        check_monotonic, stat, error_msg)
    end function dataframe_from_real64_3d
    
    function dataframe_from_real32_1d(data, dim_names, coords, name, &
                                     check_monotonic, stat, error_msg) result(df)
        real(real32), dimension(:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_real32_1d(data, dim_names, coords, name, &
                                        check_monotonic, stat, error_msg)
    end function dataframe_from_real32_1d
    
    function dataframe_from_real32_2d(data, dim_names, coords, name, &
                                     check_monotonic, stat, error_msg) result(df)
        real(real32), dimension(:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_real32_2d(data, dim_names, coords, name, &
                                        check_monotonic, stat, error_msg)
    end function dataframe_from_real32_2d
    
    function dataframe_from_real32_3d(data, dim_names, coords, name, &
                                     check_monotonic, stat, error_msg) result(df)
        real(real32), dimension(:,:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_real32_3d(data, dim_names, coords, name, &
                                        check_monotonic, stat, error_msg)
    end function dataframe_from_real32_3d
    
    function dataframe_from_int32_1d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(df)
        integer(int32), dimension(:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_int32_1d(data, dim_names, coords, name, &
                                       check_monotonic, stat, error_msg)
    end function dataframe_from_int32_1d
    
    function dataframe_from_int32_2d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(df)
        integer(int32), dimension(:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_int32_2d(data, dim_names, coords, name, &
                                       check_monotonic, stat, error_msg)
    end function dataframe_from_int32_2d
    
    function dataframe_from_int32_3d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(df)
        integer(int32), dimension(:,:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_int32_3d(data, dim_names, coords, name, &
                                       check_monotonic, stat, error_msg)
    end function dataframe_from_int32_3d
    
    function dataframe_from_int64_1d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(df)
        integer(int64), dimension(:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_int64_1d(data, dim_names, coords, name, &
                                       check_monotonic, stat, error_msg)
    end function dataframe_from_int64_1d
    
    function dataframe_from_int64_2d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(df)
        integer(int64), dimension(:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_int64_2d(data, dim_names, coords, name, &
                                       check_monotonic, stat, error_msg)
    end function dataframe_from_int64_2d
    
    function dataframe_from_int64_3d(data, dim_names, coords, name, &
                                    check_monotonic, stat, error_msg) result(df)
        integer(int64), dimension(:,:,:), intent(in) :: data
        character(len=*), dimension(:), intent(in), optional :: dim_names
        type(coordinate_t), dimension(:), intent(in), optional :: coords
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: check_monotonic
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataframe_t) :: df
        
        df%var = variable_from_int64_3d(data, dim_names, coords, name, &
                                       check_monotonic, stat, error_msg)
    end function dataframe_from_int64_3d

end module foxel_constructors