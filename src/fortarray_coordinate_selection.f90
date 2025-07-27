module fortarray_coordinate_selection
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors
    use fortarray_memory
    use fortarray_indexing
    use fortarray_slicing
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Public functions
    public :: sel, sel_range, sel_between, isel, loc
    public :: METHOD_EXACT, METHOD_NEAREST
    
    ! Selection method constants
    character(len=*), parameter :: METHOD_EXACT = "exact"
    character(len=*), parameter :: METHOD_NEAREST = "nearest"
    
contains
    
    !> Select by coordinate value (simplified version)
    function sel(var, x, y, z, method, tolerance, drop) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in), optional :: x, y, z
        character(len=*), intent(in), optional :: method
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: drop
        type(fortarray_t) :: result
        type(fortarray_t) :: temp
        integer :: idx
        
        ! Start with original variable
        result = var
        
        ! Apply selections sequentially
        if (present(x) .and. var%n_dims >= 1) then
            idx = loc(result, x, 1, method, tolerance)
            if (idx > 0) then
                temp = slice(result, create_index(idx))
                if (.not. same_variable(result, var)) call finalize_variable(result)
                result = temp
            else
                ! Return empty
                result = create_empty_like(var)
                return
            end if
        end if
        
        if (present(y) .and. var%n_dims >= 2) then
            idx = loc(result, y, 2, method, tolerance)
            if (idx > 0) then
                temp = slice(result, create_slice(), create_index(idx))
                if (.not. same_variable(result, var)) call finalize_variable(result)
                result = temp
            else
                result = create_empty_like(var)
                return
            end if
        end if
        
        if (present(z) .and. var%n_dims >= 3) then
            idx = loc(result, z, 3, method, tolerance)
            if (idx > 0) then
                temp = slice(result, create_slice(), create_slice(), create_index(idx))
                if (.not. same_variable(result, var)) call finalize_variable(result)
                result = temp
            else
                result = create_empty_like(var)
                return
            end if
        end if
        
    end function sel
    
    !> Select range of coordinate values
    function sel_range(var, x, y, z) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), dimension(2), intent(in), optional :: x, y, z
        type(fortarray_t) :: result
        type(fortarray_t) :: temp
        integer :: start_idx, end_idx
        
        result = var
        
        if (present(x) .and. var%n_dims >= 1) then
            call find_range_indices(result, 1, x(1), x(2), start_idx, end_idx)
            if (start_idx > 0 .and. end_idx > 0) then
                temp = slice(result, create_slice(start_idx, end_idx))
                if (.not. same_variable(result, var)) call finalize_variable(result)
                result = temp
            else
                result = create_empty_like(var)
                return
            end if
        end if
        
        if (present(y) .and. var%n_dims >= 2) then
            call find_range_indices(result, 2, y(1), y(2), start_idx, end_idx)
            if (start_idx > 0 .and. end_idx > 0) then
                temp = slice(result, create_slice(), create_slice(start_idx, end_idx))
                if (.not. same_variable(result, var)) call finalize_variable(result)
                result = temp
            else
                result = create_empty_like(var)
                return
            end if
        end if
        
        if (present(z) .and. var%n_dims >= 3) then
            call find_range_indices(result, 3, z(1), z(2), start_idx, end_idx)
            if (start_idx > 0 .and. end_idx > 0) then
                temp = slice(result, create_slice(), create_slice(), create_slice(start_idx, end_idx))
                if (.not. same_variable(result, var)) call finalize_variable(result)
                result = temp
            else
                result = create_empty_like(var)
                return
            end if
        end if
        
    end function sel_range
    
    !> Select between coordinate values (inclusive)
    function sel_between(var, time, time_end, x, x_end, y, y_end, z, z_end) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in), optional :: time, time_end
        real(real64), intent(in), optional :: x, x_end
        real(real64), intent(in), optional :: y, y_end
        real(real64), intent(in), optional :: z, z_end
        type(fortarray_t) :: result
        
        ! Use sel_range with appropriate dimension
        if (present(time) .and. present(time_end)) then
            ! Find time dimension
            result = sel_range(var, x=[time, time_end])
        else if (present(x) .and. present(x_end)) then
            result = sel_range(var, x=[x, x_end])
        else if (present(y) .and. present(y_end)) then
            result = sel_range(var, y=[y, y_end])
        else if (present(z) .and. present(z_end)) then
            result = sel_range(var, z=[z, z_end])
        else
            ! No range specified, return original
            result = var
        end if
        
    end function sel_between
    
    !> Integer location based selection (positional indexing)
    function isel(var, x, y, z) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in), optional :: x, y, z
        type(fortarray_t) :: result
        
        ! Use slice function from fortarray_slicing
        if (present(x) .and. present(y) .and. present(z)) then
            result = slice(var, create_index(x), create_index(y), create_index(z))
        else if (present(x) .and. present(y)) then
            result = slice(var, create_index(x), create_index(y))
        else if (present(x)) then
            result = slice(var, create_index(x))
        else
            result = var
        end if
        
    end function isel
    
    !> Get index for coordinate value
    function loc(var, value, dim, method, tolerance) result(idx)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: value
        integer, intent(in), optional :: dim
        character(len=*), intent(in), optional :: method
        real(real64), intent(in), optional :: tolerance
        integer :: idx
        integer :: dim_idx
        character(len=20) :: sel_method
        real(real64) :: tol
        
        ! Defaults
        dim_idx = 1
        if (present(dim)) dim_idx = dim
        sel_method = METHOD_EXACT
        if (present(method)) sel_method = method
        tol = 0.0_real64
        if (present(tolerance)) tol = tolerance
        
        ! Check bounds
        if (dim_idx < 1 .or. dim_idx > var%n_dims) then
            idx = 0
            return
        end if
        
        ! Check if dimension has coordinates
        if (.not. var%has_coord(dim_idx)) then
            idx = 0
            return
        end if
        
        ! Find index
        idx = find_coord_index(var%coords(dim_idx), value, sel_method, tol)
        
    end function loc
    
    !======= Helper Functions =======!
    
    !> Find coordinate index for a value
    function find_coord_index(coord, value, method, tolerance) result(idx)
        type(coordinate_t), intent(in) :: coord
        real(real64), intent(in) :: value
        character(len=*), intent(in) :: method
        real(real64), intent(in) :: tolerance
        integer :: idx
        integer :: i, n
        real(real64) :: min_dist, dist
        real(real64), dimension(:), allocatable :: coord_values
        
        idx = 0
        
        ! Get coordinate values as real64
        call get_coord_values_r64(coord, coord_values)
        n = size(coord_values)
        
        if (n == 0) return
        
        select case(method)
        case(METHOD_EXACT)
            ! Exact match with tolerance
            do i = 1, n
                if (abs(coord_values(i) - value) <= tolerance) then
                    idx = i
                    return
                end if
            end do
            
        case(METHOD_NEAREST)
            ! Find nearest neighbor
            idx = 1
            min_dist = abs(coord_values(1) - value)
            do i = 2, n
                dist = abs(coord_values(i) - value)
                if (dist < min_dist) then
                    min_dist = dist
                    idx = i
                end if
            end do
            
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown selection method: ", method
        end select
        
    end function find_coord_index
    
    !> Get coordinate values as real64
    subroutine get_coord_values_r64(coord, values)
        type(coordinate_t), intent(in) :: coord
        real(real64), dimension(:), allocatable, intent(out) :: values
        integer :: n
        
        select case(coord%dtype)
        case(DTYPE_REAL64)
            if (allocated(coord%values_r64)) then
                n = size(coord%values_r64)
                allocate(values(n))
                values = coord%values_r64
            else
                allocate(values(0))
            end if
        case(DTYPE_REAL32)
            if (allocated(coord%values_r32)) then
                n = size(coord%values_r32)
                allocate(values(n))
                values = real(coord%values_r32, real64)
            else
                allocate(values(0))
            end if
        case(DTYPE_INT64)
            if (allocated(coord%values_i64)) then
                n = size(coord%values_i64)
                allocate(values(n))
                values = real(coord%values_i64, real64)
            else
                allocate(values(0))
            end if
        case(DTYPE_INT32)
            if (allocated(coord%values_i32)) then
                n = size(coord%values_i32)
                allocate(values(n))
                values = real(coord%values_i32, real64)
            else
                allocate(values(0))
            end if
        case default
            allocate(values(0))
        end select
        
    end subroutine get_coord_values_r64
    
    !> Find range indices
    subroutine find_range_indices(var, dim, start_val, end_val, start_idx, end_idx)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: dim
        real(real64), intent(in) :: start_val, end_val
        integer, intent(out) :: start_idx, end_idx
        real(real64), dimension(:), allocatable :: coord_values
        integer :: i, n
        
        start_idx = 0
        end_idx = 0
        
        if (.not. var%has_coord(dim)) return
        
        call get_coord_values_r64(var%coords(dim), coord_values)
        n = size(coord_values)
        
        do i = 1, n
            if (start_idx == 0 .and. coord_values(i) >= start_val) then
                start_idx = i
            end if
            if (coord_values(i) <= end_val) then
                end_idx = i
            end if
        end do
        
    end subroutine find_range_indices
    
    !> Create empty variable like another
    function create_empty_like(var) result(result)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        
        ! Create variable with zero elements
        result%name = var%name
        result%n_dims = var%n_dims
        result%n_elements = 0
        
        if (allocated(var%shape)) then
            allocate(result%shape(size(var%shape)))
            result%shape = 0
            result%shape(1) = 0  ! First dimension is 0
        end if
        
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(size(var%dim_names)))
            result%dim_names = var%dim_names
        end if
        
        ! Allocate empty data storage
        result%data%dtype = var%data%dtype
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
    
    !> Check if two variables are the same
    function same_variable(var1, var2) result(same)
        type(fortarray_t), intent(in) :: var1, var2
        logical :: same
        
        ! Simple check - compare basic properties
        same = (var1%name == var2%name .and. var1%n_elements == var2%n_elements)
        
    end function same_variable

end module fortarray_coordinate_selection