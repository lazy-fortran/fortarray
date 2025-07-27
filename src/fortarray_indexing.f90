module fortarray_indexing
    use fortarray_types
    use fortarray_storage
    use fortarray_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Error codes
    integer, parameter :: INDEXING_SUCCESS = 0
    integer, parameter :: INDEXING_ERROR_BOUNDS = -1
    integer, parameter :: INDEXING_ERROR_DIMS = -2
    integer, parameter :: INDEXING_ERROR_COORDS = -3
    integer, parameter :: INDEXING_ERROR_ALLOCATION = -4
    integer, parameter :: INDEXING_ERROR_TYPE = -5
    integer, parameter :: INDEXING_ERROR_NOT_FOUND = -6
    
    ! Index type for multi-dimensional indexing
    type :: index_t
        integer :: start = 1
        integer :: stop = -1  ! -1 means end
        integer :: stride = 1
        logical :: is_scalar = .false.
        logical :: is_slice = .true.
    end type index_t
    
    ! Label-based index
    type :: label_index_t
        character(len=:), allocatable :: label
        real(real64) :: value  ! For numeric labels
        logical :: is_numeric = .false.
        logical :: is_range = .false.
        real(real64) :: start_value, end_value  ! For ranges
    end type label_index_t
    
    ! Generic interfaces for indexing
    interface get_item
        module procedure get_item_positional
        module procedure get_item_positional_nd
        module procedure get_item_label
        module procedure get_item_label_nd
    end interface get_item
    
    interface set_item
        module procedure set_item_positional
        module procedure set_item_positional_nd
        module procedure set_item_label
        module procedure set_item_label_nd
    end interface set_item
    
    ! Slicing interface
    interface slice
        module procedure slice_variable_1d
        module procedure slice_variable_2d
        module procedure slice_variable_3d
        module procedure slice_variable_nd
    end interface slice
    
    ! Public interfaces
    public :: get_item, set_item, slice
    public :: index_t, label_index_t
    public :: create_index, create_slice, create_label_index
    public :: find_label_position
    public :: INDEXING_SUCCESS, INDEXING_ERROR_BOUNDS, INDEXING_ERROR_DIMS
    public :: INDEXING_ERROR_COORDS, INDEXING_ERROR_ALLOCATION
    public :: INDEXING_ERROR_TYPE, INDEXING_ERROR_NOT_FOUND
    
contains

    !> Create a scalar index
    function create_index(pos) result(idx)
        integer, intent(in) :: pos
        type(index_t) :: idx
        
        idx%start = pos
        idx%stop = pos
        idx%stride = 1
        idx%is_scalar = .true.
        idx%is_slice = .false.
    end function create_index
    
    !> Create a slice index
    function create_slice(start, stop, stride) result(idx)
        integer, intent(in), optional :: start, stop, stride
        type(index_t) :: idx
        
        idx%start = 1
        if (present(start)) idx%start = start
        
        idx%stop = -1  ! Will be resolved to actual end
        if (present(stop)) idx%stop = stop
        
        idx%stride = 1
        if (present(stride)) idx%stride = stride
        
        idx%is_scalar = .false.
        idx%is_slice = .true.
    end function create_slice
    
    !> Create a label-based index
    function create_label_index(label, value, is_range, end_value) result(idx)
        character(len=*), intent(in), optional :: label
        real(real64), intent(in), optional :: value
        logical, intent(in), optional :: is_range
        real(real64), intent(in), optional :: end_value
        type(label_index_t) :: idx
        
        if (present(label)) then
            idx%label = label
            idx%is_numeric = .false.
        else if (present(value)) then
            idx%value = value
            idx%is_numeric = .true.
        end if
        
        idx%is_range = .false.
        if (present(is_range)) idx%is_range = is_range
        
        if (idx%is_range .and. present(value) .and. present(end_value)) then
            idx%start_value = value
            idx%end_value = end_value
        end if
    end function create_label_index
    
    !> Find position of label in coordinate
    function find_label_position(coord, label_idx, stat) result(pos)
        type(coordinate_t), intent(in) :: coord
        type(label_index_t), intent(in) :: label_idx
        integer, intent(out), optional :: stat
        integer :: pos
        
        integer :: i, status
        real(real64) :: eps
        
        status = INDEXING_SUCCESS
        pos = 0
        eps = epsilon(1.0_real64)
        
        if (.not. coord%initialized) then
            status = INDEXING_ERROR_COORDS
            if (present(stat)) stat = status
            return
        end if
        
        if (label_idx%is_numeric) then
            ! Find numeric value in coordinate
            select case(coord%dtype)
            case(DTYPE_REAL64)
                do i = 1, coord%length
                    if (abs(coord%values_r64(i) - label_idx%value) < eps) then
                        pos = i
                        exit
                    end if
                end do
            case(DTYPE_REAL32)
                do i = 1, coord%length
                    if (abs(coord%values_r32(i) - real(label_idx%value, real32)) < real(eps, real32)) then
                        pos = i
                        exit
                    end if
                end do
            case(DTYPE_INT64)
                do i = 1, coord%length
                    if (coord%values_i64(i) == int(label_idx%value, int64)) then
                        pos = i
                        exit
                    end if
                end do
            case(DTYPE_INT32)
                do i = 1, coord%length
                    if (coord%values_i32(i) == int(label_idx%value, int32)) then
                        pos = i
                        exit
                    end if
                end do
            end select
        else
            ! Find string label in coordinate
            if (coord%dtype == DTYPE_CHAR) then
                do i = 1, coord%length
                    if (trim(coord%values_char(i)) == trim(label_idx%label)) then
                        pos = i
                        exit
                    end if
                end do
            end if
        end if
        
        if (pos == 0) status = INDEXING_ERROR_NOT_FOUND
        if (present(stat)) stat = status
    end function find_label_position
    
    !> Get item by positional index (1D, 2D, or 3D)
    function get_item_positional(var, idx1, idx2, idx3, stat, error_msg) result(value)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: idx1
        integer, intent(in), optional :: idx2, idx3
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        real(real64) :: value
        
        integer :: status, linear_idx, n_dims_provided
        character(len=256) :: err_msg
        
        status = INDEXING_SUCCESS
        err_msg = ""
        value = 0.0_real64
        
        ! Check variable is initialized
        if (.not. var%initialized) then
            status = INDEXING_ERROR_TYPE
            err_msg = "Variable not initialized"
            goto 999
        end if
        
        ! Determine number of dimensions provided
        n_dims_provided = 1
        if (present(idx2)) n_dims_provided = 2
        if (present(idx3)) n_dims_provided = 3
        
        ! Check dimensions match
        if (var%n_dims /= n_dims_provided) then
            status = INDEXING_ERROR_DIMS
            write(err_msg, '(A,I0,A,I0)') "Variable has ", var%n_dims, " dimensions, got ", n_dims_provided
            goto 999
        end if
        
        ! Handle based on number of dimensions
        select case(n_dims_provided)
        case(1)
            ! Check bounds
            if (idx1 < 1 .or. idx1 > var%shape(1)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index ", idx1, " out of bounds [1,", var%shape(1), "]"
                goto 999
            end if
            linear_idx = idx1
            
        case(2)
            ! Check bounds
            if (idx1 < 1 .or. idx1 > var%shape(1)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 1: ", idx1, " out of bounds [1,", var%shape(1), "]"
                goto 999
            end if
            if (idx2 < 1 .or. idx2 > var%shape(2)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 2: ", idx2, " out of bounds [1,", var%shape(2), "]"
                goto 999
            end if
            linear_idx = idx1 + (idx2 - 1) * var%shape(1)
            
        case(3)
            ! Check bounds
            if (idx1 < 1 .or. idx1 > var%shape(1)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 1: ", idx1, " out of bounds [1,", var%shape(1), "]"
                goto 999
            end if
            if (idx2 < 1 .or. idx2 > var%shape(2)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 2: ", idx2, " out of bounds [1,", var%shape(2), "]"
                goto 999
            end if
            if (idx3 < 1 .or. idx3 > var%shape(3)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 3: ", idx3, " out of bounds [1,", var%shape(3), "]"
                goto 999
            end if
            linear_idx = idx1 + (idx2 - 1) * var%shape(1) + (idx3 - 1) * var%shape(1) * var%shape(2)
        end select
        
        ! Get value
        call get_value_converted(var%data, linear_idx, value, status)
        if (status /= 0) then
            err_msg = "Failed to convert value"
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function get_item_positional
    
    !> Get item by N-dimensional positional index
    function get_item_positional_nd(var, indices, stat, error_msg) result(value)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: indices
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        real(real64) :: value
        
        integer :: status, linear_idx, i, multiplier
        character(len=256) :: err_msg
        
        status = INDEXING_SUCCESS
        err_msg = ""
        value = 0.0_real64
        
        ! Check variable is initialized
        if (.not. var%initialized) then
            status = INDEXING_ERROR_TYPE
            err_msg = "Variable not initialized"
            goto 999
        end if
        
        ! Check dimensions
        if (size(indices) /= var%n_dims) then
            status = INDEXING_ERROR_DIMS
            write(err_msg, '(A,I0,A,I0,A)') "Got ", size(indices), " indices for ", var%n_dims, "-D variable"
            goto 999
        end if
        
        ! Check bounds and calculate linear index
        linear_idx = 1
        multiplier = 1
        do i = 1, var%n_dims
            if (indices(i) < 1 .or. indices(i) > var%shape(i)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0,A,I0)') &
                    "Index ", i, ": ", indices(i), " out of bounds [1,", var%shape(i), "]"
                goto 999
            end if
            linear_idx = linear_idx + (indices(i) - 1) * multiplier
            if (i < var%n_dims) multiplier = multiplier * var%shape(i)
        end do
        
        call get_value_converted(var%data, linear_idx, value, status)
        if (status /= 0) then
            err_msg = "Failed to convert value"
        end if
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function get_item_positional_nd
    
    !> Get item by label index (1D, 2D, or 3D)
    function get_item_label(var, label_idx1, label_idx2, label_idx3, stat, error_msg) result(value)
        type(fortarray_t), intent(in) :: var
        type(label_index_t), intent(in) :: label_idx1
        type(label_index_t), intent(in), optional :: label_idx2, label_idx3
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        real(real64) :: value
        
        integer :: status, pos1, pos2, pos3, n_dims_provided
        character(len=256) :: err_msg
        
        status = INDEXING_SUCCESS
        err_msg = ""
        value = 0.0_real64
        
        ! Check variable is initialized
        if (.not. var%initialized) then
            status = INDEXING_ERROR_TYPE
            err_msg = "Variable not initialized"
            goto 999
        end if
        
        ! Determine number of dimensions provided
        n_dims_provided = 1
        if (present(label_idx2)) n_dims_provided = 2
        if (present(label_idx3)) n_dims_provided = 3
        
        ! Check dimensions match
        if (var%n_dims /= n_dims_provided) then
            status = INDEXING_ERROR_DIMS
            write(err_msg, '(A,I0,A,I0)') "Variable has ", var%n_dims, " dimensions, got ", n_dims_provided
            goto 999
        end if
        
        ! Check variable has coordinates
        if (.not. var%has_coord(1)) then
            status = INDEXING_ERROR_COORDS
            err_msg = "Variable has no coordinates for dimension 1"
            goto 999
        end if
        
        ! Find position for first dimension
        pos1 = find_label_position(var%coords(1), label_idx1, status)
        if (status /= 0) then
            err_msg = "Label not found in dimension 1"
            goto 999
        end if
        
        ! Handle based on number of dimensions
        select case(n_dims_provided)
        case(1)
            ! Get value using positional index
            value = get_item_positional(var, pos1, stat=status, error_msg=err_msg)
            
        case(2)
            if (.not. var%has_coord(2)) then
                status = INDEXING_ERROR_COORDS
                err_msg = "Variable has no coordinates for dimension 2"
                goto 999
            end if
            pos2 = find_label_position(var%coords(2), label_idx2, status)
            if (status /= 0) then
                err_msg = "Label not found in dimension 2"
                goto 999
            end if
            value = get_item_positional(var, pos1, pos2, stat=status, error_msg=err_msg)
            
        case(3)
            if (.not. var%has_coord(2)) then
                status = INDEXING_ERROR_COORDS
                err_msg = "Variable has no coordinates for dimension 2"
                goto 999
            end if
            if (.not. var%has_coord(3)) then
                status = INDEXING_ERROR_COORDS
                err_msg = "Variable has no coordinates for dimension 3"
                goto 999
            end if
            pos2 = find_label_position(var%coords(2), label_idx2, status)
            if (status /= 0) then
                err_msg = "Label not found in dimension 2"
                goto 999
            end if
            pos3 = find_label_position(var%coords(3), label_idx3, status)
            if (status /= 0) then
                err_msg = "Label not found in dimension 3"
                goto 999
            end if
            value = get_item_positional(var, pos1, pos2, pos3, stat=status, error_msg=err_msg)
        end select
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function get_item_label
    
    !> Get item by N-dimensional label index
    function get_item_label_nd(var, label_indices, stat, error_msg) result(value)
        type(fortarray_t), intent(in) :: var
        type(label_index_t), dimension(:), intent(in) :: label_indices
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        real(real64) :: value
        
        integer :: status, i
        integer, dimension(:), allocatable :: positions
        character(len=256) :: err_msg
        
        status = INDEXING_SUCCESS
        err_msg = ""
        value = 0.0_real64
        
        ! Check dimensions
        if (size(label_indices) /= var%n_dims) then
            status = INDEXING_ERROR_DIMS
            write(err_msg, '(A,I0,A,I0)') "Got ", size(label_indices), " labels for ", var%n_dims, "-D variable"
            goto 999
        end if
        
        ! Convert labels to positions
        allocate(positions(var%n_dims))
        do i = 1, var%n_dims
            if (.not. var%has_coord(i)) then
                status = INDEXING_ERROR_COORDS
                write(err_msg, '(A,I0)') "No coordinate for dimension ", i
                goto 999
            end if
            
            positions(i) = find_label_position(var%coords(i), label_indices(i), status)
            if (status /= 0) then
                write(err_msg, '(A,I0)') "Label not found in dimension ", i
                goto 999
            end if
        end do
        
        ! Get value using positional indices
        value = get_item_positional_nd(var, positions, status, err_msg)
        
        if (allocated(positions)) deallocate(positions)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function get_item_label_nd
    
    !> Set item by positional index (1D, 2D, or 3D)
    subroutine set_item_positional(var, idx1, idx2, idx3, value, stat, error_msg)
        type(fortarray_t), intent(inout) :: var
        integer, intent(in) :: idx1
        integer, intent(in), optional :: idx2, idx3
        real(real64), intent(in) :: value
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        
        integer :: status, linear_idx, n_dims_provided
        character(len=256) :: err_msg
        
        status = INDEXING_SUCCESS
        err_msg = ""
        
        ! Check variable is initialized
        if (.not. var%initialized) then
            status = INDEXING_ERROR_TYPE
            err_msg = "Variable not initialized"
            goto 999
        end if
        
        ! Determine number of dimensions provided
        n_dims_provided = 1
        if (present(idx2)) n_dims_provided = 2
        if (present(idx3)) n_dims_provided = 3
        
        ! Check dimensions match
        if (var%n_dims /= n_dims_provided) then
            status = INDEXING_ERROR_DIMS
            write(err_msg, '(A,I0,A,I0)') "Variable has ", var%n_dims, " dimensions, got ", n_dims_provided
            goto 999
        end if
        
        ! Handle based on number of dimensions
        select case(n_dims_provided)
        case(1)
            ! Check bounds
            if (idx1 < 1 .or. idx1 > var%shape(1)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index ", idx1, " out of bounds [1,", var%shape(1), "]"
                goto 999
            end if
            linear_idx = idx1
            
        case(2)
            ! Check bounds
            if (idx1 < 1 .or. idx1 > var%shape(1)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 1: ", idx1, " out of bounds [1,", var%shape(1), "]"
                goto 999
            end if
            if (idx2 < 1 .or. idx2 > var%shape(2)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 2: ", idx2, " out of bounds [1,", var%shape(2), "]"
                goto 999
            end if
            linear_idx = idx1 + (idx2 - 1) * var%shape(1)
            
        case(3)
            ! Check bounds
            if (idx1 < 1 .or. idx1 > var%shape(1)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 1: ", idx1, " out of bounds [1,", var%shape(1), "]"
                goto 999
            end if
            if (idx2 < 1 .or. idx2 > var%shape(2)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 2: ", idx2, " out of bounds [1,", var%shape(2), "]"
                goto 999
            end if
            if (idx3 < 1 .or. idx3 > var%shape(3)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0)') "Index 3: ", idx3, " out of bounds [1,", var%shape(3), "]"
                goto 999
            end if
            linear_idx = idx1 + (idx2 - 1) * var%shape(1) + (idx3 - 1) * var%shape(1) * var%shape(2)
        end select
        
        ! Set value
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            var%data%values_r64(linear_idx) = value
        case(DTYPE_REAL32)
            var%data%values_r32(linear_idx) = real(value, real32)
        case(DTYPE_INT64)
            var%data%values_i64(linear_idx) = int(value, int64)
        case(DTYPE_INT32)
            var%data%values_i32(linear_idx) = int(value, int32)
        case default
            status = INDEXING_ERROR_TYPE
            err_msg = "Cannot set numeric value in non-numeric array"
        end select
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end subroutine set_item_positional
    
    !> Set item by label index (1D, 2D, or 3D)
    subroutine set_item_label(var, label_idx1, label_idx2, label_idx3, value, stat, error_msg)
        type(fortarray_t), intent(inout) :: var
        type(label_index_t), intent(in) :: label_idx1
        type(label_index_t), intent(in), optional :: label_idx2, label_idx3
        real(real64), intent(in) :: value
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        
        integer :: status, pos1, pos2, pos3, n_dims_provided
        character(len=256) :: err_msg
        
        status = INDEXING_SUCCESS
        err_msg = ""
        
        ! Check variable is initialized
        if (.not. var%initialized) then
            status = INDEXING_ERROR_TYPE
            err_msg = "Variable not initialized"
            goto 999
        end if
        
        ! Determine number of dimensions provided
        n_dims_provided = 1
        if (present(label_idx2)) n_dims_provided = 2
        if (present(label_idx3)) n_dims_provided = 3
        
        ! Check dimensions match
        if (var%n_dims /= n_dims_provided) then
            status = INDEXING_ERROR_DIMS
            write(err_msg, '(A,I0,A,I0)') "Variable has ", var%n_dims, " dimensions, got ", n_dims_provided
            goto 999
        end if
        
        ! Check variable has coordinates
        if (.not. var%has_coord(1)) then
            status = INDEXING_ERROR_COORDS
            err_msg = "Variable has no coordinates for dimension 1"
            goto 999
        end if
        
        ! Find position for first dimension
        pos1 = find_label_position(var%coords(1), label_idx1, status)
        if (status /= 0) then
            err_msg = "Label not found in dimension 1"
            goto 999
        end if
        
        ! Handle based on number of dimensions
        select case(n_dims_provided)
        case(1)
            ! Set value using positional index
            call set_item_positional(var, pos1, value=value, stat=status, error_msg=err_msg)
            
        case(2)
            if (.not. var%has_coord(2)) then
                status = INDEXING_ERROR_COORDS
                err_msg = "Variable has no coordinates for dimension 2"
                goto 999
            end if
            pos2 = find_label_position(var%coords(2), label_idx2, status)
            if (status /= 0) then
                err_msg = "Label not found in dimension 2"
                goto 999
            end if
            call set_item_positional(var, pos1, pos2, value=value, stat=status, error_msg=err_msg)
            
        case(3)
            if (.not. var%has_coord(2)) then
                status = INDEXING_ERROR_COORDS
                err_msg = "Variable has no coordinates for dimension 2"
                goto 999
            end if
            if (.not. var%has_coord(3)) then
                status = INDEXING_ERROR_COORDS
                err_msg = "Variable has no coordinates for dimension 3"
                goto 999
            end if
            pos2 = find_label_position(var%coords(2), label_idx2, status)
            if (status /= 0) then
                err_msg = "Label not found in dimension 2"
                goto 999
            end if
            pos3 = find_label_position(var%coords(3), label_idx3, status)
            if (status /= 0) then
                err_msg = "Label not found in dimension 3"
                goto 999
            end if
            call set_item_positional(var, pos1, pos2, pos3, value=value, stat=status, error_msg=err_msg)
        end select
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end subroutine set_item_label
    
    !> Set item by N-dimensional positional index
    subroutine set_item_positional_nd(var, indices, value, stat, error_msg)
        type(fortarray_t), intent(inout) :: var
        integer, dimension(:), intent(in) :: indices
        real(real64), intent(in) :: value
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        
        integer :: status, linear_idx, i, multiplier
        character(len=256) :: err_msg
        
        status = INDEXING_SUCCESS
        err_msg = ""
        
        ! Check variable is initialized
        if (.not. var%initialized) then
            status = INDEXING_ERROR_TYPE
            err_msg = "Variable not initialized"
            goto 999
        end if
        
        ! Check dimensions
        if (size(indices) /= var%n_dims) then
            status = INDEXING_ERROR_DIMS
            write(err_msg, '(A,I0,A,I0,A)') "Got ", size(indices), " indices for ", var%n_dims, "-D variable"
            goto 999
        end if
        
        ! Check bounds and calculate linear index
        linear_idx = 1
        multiplier = 1
        do i = 1, var%n_dims
            if (indices(i) < 1 .or. indices(i) > var%shape(i)) then
                status = INDEXING_ERROR_BOUNDS
                write(err_msg, '(A,I0,A,I0,A,I0,A,I0)') &
                    "Index ", i, ": ", indices(i), " out of bounds [1,", var%shape(i), "]"
                goto 999
            end if
            linear_idx = linear_idx + (indices(i) - 1) * multiplier
            if (i < var%n_dims) multiplier = multiplier * var%shape(i)
        end do
        
        ! Set value
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            var%data%values_r64(linear_idx) = value
        case(DTYPE_REAL32)
            var%data%values_r32(linear_idx) = real(value, real32)
        case(DTYPE_INT64)
            var%data%values_i64(linear_idx) = int(value, int64)
        case(DTYPE_INT32)
            var%data%values_i32(linear_idx) = int(value, int32)
        case default
            status = INDEXING_ERROR_TYPE
            err_msg = "Cannot set numeric value in non-numeric array"
        end select
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end subroutine set_item_positional_nd
    
    !> Set item by N-dimensional label index
    subroutine set_item_label_nd(var, label_indices, value, stat, error_msg)
        type(fortarray_t), intent(inout) :: var
        type(label_index_t), dimension(:), intent(in) :: label_indices
        real(real64), intent(in) :: value
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        
        integer :: status, i
        integer, dimension(:), allocatable :: positions
        character(len=256) :: err_msg
        
        status = INDEXING_SUCCESS
        err_msg = ""
        
        ! Check dimensions
        if (size(label_indices) /= var%n_dims) then
            status = INDEXING_ERROR_DIMS
            write(err_msg, '(A,I0,A,I0)') "Got ", size(label_indices), " labels for ", var%n_dims, "-D variable"
            goto 999
        end if
        
        ! Convert labels to positions
        allocate(positions(var%n_dims))
        do i = 1, var%n_dims
            if (.not. var%has_coord(i)) then
                status = INDEXING_ERROR_COORDS
                write(err_msg, '(A,I0)') "No coordinate for dimension ", i
                goto 999
            end if
            
            positions(i) = find_label_position(var%coords(i), label_indices(i), status)
            if (status /= 0) then
                write(err_msg, '(A,I0)') "Label not found in dimension ", i
                goto 999
            end if
        end do
        
        ! Set value using positional indices
        call set_item_positional_nd(var, positions, value, status, err_msg)
        
        if (allocated(positions)) deallocate(positions)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end subroutine set_item_label_nd
    
    !> Slice 1D variable
    function slice_variable_1d(var, idx1, stat, error_msg) result(sliced_var)
        type(fortarray_t), intent(in) :: var
        type(index_t), intent(in) :: idx1
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(fortarray_t) :: sliced_var
        
        ! TODO: Implement slicing
        if (present(stat)) stat = INDEXING_SUCCESS
        if (present(error_msg)) error_msg = ""
        sliced_var = var  ! Placeholder
    end function slice_variable_1d
    
    !> Slice 2D variable
    function slice_variable_2d(var, idx1, idx2, stat, error_msg) result(sliced_var)
        type(fortarray_t), intent(in) :: var
        type(index_t), intent(in) :: idx1, idx2
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(fortarray_t) :: sliced_var
        
        ! TODO: Implement slicing
        if (present(stat)) stat = INDEXING_SUCCESS
        if (present(error_msg)) error_msg = ""
        sliced_var = var  ! Placeholder
    end function slice_variable_2d
    
    !> Slice 3D variable
    function slice_variable_3d(var, idx1, idx2, idx3, stat, error_msg) result(sliced_var)
        type(fortarray_t), intent(in) :: var
        type(index_t), intent(in) :: idx1, idx2, idx3
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(fortarray_t) :: sliced_var
        
        ! TODO: Implement slicing
        if (present(stat)) stat = INDEXING_SUCCESS
        if (present(error_msg)) error_msg = ""
        sliced_var = var  ! Placeholder
    end function slice_variable_3d
    
    !> Slice N-dimensional variable
    function slice_variable_nd(var, indices, stat, error_msg) result(sliced_var)
        type(fortarray_t), intent(in) :: var
        type(index_t), dimension(:), intent(in) :: indices
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(fortarray_t) :: sliced_var
        
        ! TODO: Implement slicing
        if (present(stat)) stat = INDEXING_SUCCESS
        if (present(error_msg)) error_msg = ""
        sliced_var = var  ! Placeholder
    end function slice_variable_nd
    
end module fortarray_indexing