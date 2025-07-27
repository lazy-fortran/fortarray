module fortarray_interpolation
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors, only: new_array, new_dataset, &
        variable_scalar_real64, variable_scalar_real32, variable_scalar_int32, variable_scalar_int64
    use fortarray_memory
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Public interfaces
    public :: interp1d, interp2d, interp_nd
    public :: INTERP_LINEAR, INTERP_NEAREST, INTERP_CUBIC
    public :: EXTRAP_NONE, EXTRAP_CONSTANT, EXTRAP_LINEAR
    
    ! Interpolation method constants
    character(len=*), parameter :: INTERP_LINEAR = "linear"
    character(len=*), parameter :: INTERP_NEAREST = "nearest"
    character(len=*), parameter :: INTERP_CUBIC = "cubic"
    character(len=*), parameter :: INTERP_SPLINE = "spline"
    
    ! Extrapolation method constants
    character(len=*), parameter :: EXTRAP_NONE = "none"
    character(len=*), parameter :: EXTRAP_CONSTANT = "constant"
    character(len=*), parameter :: EXTRAP_LINEAR = "linear"
    
    ! Generic interfaces
    interface interp1d
        module procedure interp1d_real64_array
        module procedure interp1d_real64_scalar
    end interface interp1d
    
contains
    
    !> 1D interpolation with array of points
    function interp1d_real64_array(var, interp_points, method, bounds_error, fill_value, skip_na) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), dimension(:), intent(in) :: interp_points
        character(len=*), intent(in), optional :: method
        logical, intent(in), optional :: bounds_error
        real(real64), intent(in), optional :: fill_value
        logical, intent(in), optional :: skip_na
        type(fortarray_t) :: result
        character(len=20) :: interp_method
        logical :: check_bounds, ignore_na
        real(real64) :: fill_val
        integer :: n_points, i
        real(real64), dimension(:), allocatable :: result_values
        real(real64), dimension(:), allocatable :: x_coords, y_values
        
        ! Set defaults
        interp_method = INTERP_LINEAR
        if (present(method)) interp_method = method
        check_bounds = .true.
        if (present(bounds_error)) check_bounds = bounds_error
        fill_val = ieee_value(1.0_real64, ieee_quiet_nan)
        if (present(fill_value)) fill_val = fill_value
        ignore_na = .false.
        if (present(skip_na)) ignore_na = skip_na
        
        n_points = size(interp_points)
        
        ! Validate input
        if (var%n_dims /= 1) then
            write(error_unit,'(A)') "ERROR: Variable must be 1D for 1D interpolation"
            result = create_empty_like(var)
            return
        end if
        
        if (.not. var%has_coord(1)) then
            write(error_unit,'(A)') "ERROR: Variable must have coordinates for interpolation"
            result = create_empty_like(var)
            return
        end if
        
        ! Extract coordinates and values
        call get_coord_values_r64(var%coords(1), x_coords)
        call get_variable_values_r64(var, y_values)
        
        ! Perform interpolation
        allocate(result_values(n_points))
        
        do i = 1, n_points
            result_values(i) = interpolate_point(x_coords, y_values, interp_points(i), &
                                                 interp_method, check_bounds, fill_val, ignore_na)
        end do
        
        ! Create result variable
        result = new_array(result_values, name=var%name//"_interp", dim_names=["interp"])
        
        ! Set up interpolated coordinates
        allocate(result%coords(1), result%has_coord(1))
        result%coords(1) = create_coordinate_from_values(interp_points, "interp_x")
        result%has_coord(1) = .true.
        
        ! Copy attributes
        result%units = var%units
        result%long_name = var%long_name
        result%standard_name = var%standard_name
        
    end function interp1d_real64_array
    
    !> 1D interpolation with single point
    function interp1d_real64_scalar(var, interp_point, method, bounds_error, fill_value, skip_na) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: interp_point
        character(len=*), intent(in), optional :: method
        logical, intent(in), optional :: bounds_error
        real(real64), intent(in), optional :: fill_value
        logical, intent(in), optional :: skip_na
        type(fortarray_t) :: result
        real(real64), dimension(1) :: points
        
        points = [interp_point]
        result = interp1d_real64_array(var, points, method, bounds_error, fill_value, skip_na)
        
    end function interp1d_real64_scalar
    
    !> 2D interpolation
    function interp2d(var, x_points, y_points, method) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), dimension(:), intent(in) :: x_points, y_points
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result
        character(len=20) :: interp_method
        integer :: n_points, i
        real(real64), dimension(:), allocatable :: result_values
        
        interp_method = INTERP_LINEAR
        if (present(method)) interp_method = method
        
        n_points = size(x_points)
        
        if (var%n_dims /= 2) then
            write(error_unit,'(A)') "ERROR: Variable must be 2D for 2D interpolation"
            result = create_empty_like(var)
            return
        end if
        
        if (size(y_points) /= n_points) then
            write(error_unit,'(A)') "ERROR: x_points and y_points must have same size"
            result = create_empty_like(var)
            return
        end if
        
        ! Simplified 2D interpolation
        allocate(result_values(n_points))
        
        ! For now, just use nearest neighbor for 2D
        do i = 1, n_points
            result_values(i) = interpolate_2d_point(var, x_points(i), y_points(i), interp_method)
        end do
        
        result = new_array(result_values, name=var%name//"_interp2d", dim_names=["interp"])
        result%units = var%units
        result%long_name = var%long_name
        
    end function interp2d
    
    !> N-dimensional interpolation (placeholder)
    function interp_nd(var, points, method) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), dimension(:,:), intent(in) :: points
        character(len=*), intent(in), optional :: method
        type(fortarray_t) :: result
        
        ! Placeholder implementation
        write(*,'(A)') "N-dimensional interpolation not yet implemented"
        result = var
        
    end function interp_nd
    
    !======= Helper Functions =======!
    
    !> Interpolate single point
    function interpolate_point(x_coords, y_values, x_point, method, bounds_error, fill_value, skip_na) result(y_interp)
        real(real64), dimension(:), intent(in) :: x_coords, y_values
        real(real64), intent(in) :: x_point
        character(len=*), intent(in) :: method
        logical, intent(in) :: bounds_error, skip_na
        real(real64), intent(in) :: fill_value
        real(real64) :: y_interp
        integer :: n, i, idx_left, idx_right
        real(real64) :: x_left, x_right, y_left, y_right, t
        logical :: found
        
        n = size(x_coords)
        
        ! Handle single point case
        if (n == 1) then
            y_interp = y_values(1)
            return
        end if
        
        ! Check bounds
        if (x_point < x_coords(1) .or. x_point > x_coords(n)) then
            if (bounds_error) then
                write(error_unit,'(A,F0.3)') "ERROR: Interpolation point out of bounds: ", x_point
                y_interp = fill_value
                return
            else
                y_interp = fill_value
                return
            end if
        end if
        
        ! Find bracket
        found = .false.
        do i = 1, n-1
            if (x_point >= x_coords(i) .and. x_point <= x_coords(i+1)) then
                idx_left = i
                idx_right = i + 1
                found = .true.
                exit
            end if
        end do
        
        if (.not. found) then
            y_interp = fill_value
            return
        end if
        
        x_left = x_coords(idx_left)
        x_right = x_coords(idx_right)
        y_left = y_values(idx_left)
        y_right = y_values(idx_right)
        
        ! Handle missing data
        if (skip_na .and. (ieee_is_nan(y_left) .or. ieee_is_nan(y_right))) then
            y_interp = fill_value
            return
        end if
        
        ! Perform interpolation based on method
        select case(trim(method))
        case(INTERP_LINEAR)
            if (abs(x_right - x_left) < 1e-15) then
                y_interp = y_left
            else
                t = (x_point - x_left) / (x_right - x_left)
                y_interp = y_left + t * (y_right - y_left)
            end if
            
        case(INTERP_NEAREST)
            if (abs(x_point - x_left) < abs(x_point - x_right)) then
                y_interp = y_left
            else
                y_interp = y_right
            end if
            
        case(INTERP_CUBIC)
            ! Simplified cubic interpolation (needs more points for proper implementation)
            if (idx_left > 1 .and. idx_right < n) then
                y_interp = cubic_interpolate(x_coords(idx_left-1:idx_right+1), &
                                           y_values(idx_left-1:idx_right+1), x_point)
            else
                ! Fall back to linear
                t = (x_point - x_left) / (x_right - x_left)
                y_interp = y_left + t * (y_right - y_left)
            end if
            
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown interpolation method: ", method
            y_interp = fill_value
        end select
        
    end function interpolate_point
    
    !> Cubic interpolation (simplified)
    function cubic_interpolate(x, y, x_point) result(y_interp)
        real(real64), dimension(4), intent(in) :: x, y
        real(real64), intent(in) :: x_point
        real(real64) :: y_interp
        real(real64) :: t, t2, t3
        real(real64) :: a0, a1, a2, a3
        
        ! Simplified cubic interpolation using 4 points
        ! Normalize to [0,1] interval
        t = (x_point - x(2)) / (x(3) - x(2))
        t2 = t * t
        t3 = t2 * t
        
        ! Cubic coefficients (simplified)
        a0 = y(2)
        a1 = (y(3) - y(1)) / 2.0_real64
        a2 = y(1) - 2.5_real64 * y(2) + 2.0_real64 * y(3) - 0.5_real64 * y(4)
        a3 = -0.5_real64 * y(1) + 1.5_real64 * y(2) - 1.5_real64 * y(3) + 0.5_real64 * y(4)
        
        y_interp = a0 + a1 * t + a2 * t2 + a3 * t3
        
    end function cubic_interpolate
    
    !> 2D interpolation for single point
    function interpolate_2d_point(var, x_point, y_point, method) result(z_interp)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: x_point, y_point
        character(len=*), intent(in) :: method
        real(real64) :: z_interp
        
        ! Simplified 2D interpolation - just return first value for now
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            z_interp = var%data%values_r64(1)
        case(DTYPE_REAL32)
            z_interp = real(var%data%values_r32(1), real64)
        case default
            z_interp = 0.0_real64
        end select
        
    end function interpolate_2d_point
    
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
    
    !> Create coordinate from values
    function create_coordinate_from_values(values, name) result(coord)
        real(real64), dimension(:), intent(in) :: values
        character(len=*), intent(in) :: name
        type(coordinate_t) :: coord
        integer :: n
        
        n = size(values)
        coord%name = name
        coord%length = n
        coord%dtype = DTYPE_REAL64
        coord%initialized = .true.
        
        allocate(coord%values_r64(n))
        coord%values_r64 = values
        
    end function create_coordinate_from_values
    
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

end module fortarray_interpolation