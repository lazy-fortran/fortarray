module fortarray_slicing
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors
    use fortarray_memory
    use fortarray_indexing
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Public interfaces
    public :: slice_range, slice_from_negative, slice_with_step
    public :: slice_2d_range, slice_3d_range
    public :: slice_broadcast, slice_multidim
    public :: slice_with_bounds_check
    public :: normalize_index, validate_slice_params
    
    ! Slice type for representing slice parameters
    type :: slice_spec_t
        integer :: start = 1
        integer :: stop = -1   ! -1 means end
        integer :: step = 1
        logical :: is_negative_start = .false.
        logical :: is_negative_stop = .false.
    end type slice_spec_t
    
    ! Public the slice spec type
    public :: slice_spec_t, create_slice_spec
    
contains
    
    !> Create slice specification
    function create_slice_spec(start, stop, step) result(spec)
        integer, intent(in), optional :: start, stop, step
        type(slice_spec_t) :: spec
        
        if (present(start)) spec%start = start
        if (present(stop)) spec%stop = stop
        if (present(step)) spec%step = step
        
        ! Handle negative indices
        if (present(start) .and. start < 0) spec%is_negative_start = .true.
        if (present(stop) .and. stop < 0) spec%is_negative_stop = .true.
        
    end function create_slice_spec
    
    !> Basic range slice: [start:stop]
    function slice_range(var, start_idx, stop_idx) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, stop_idx
        type(fortarray_t) :: result
        integer :: actual_start, actual_stop, n_elements
        
        ! Validate and normalize indices
        call validate_slice_params(var, start_idx, stop_idx, 1, actual_start, actual_stop)
        
        if (actual_start > actual_stop) then
            ! Empty slice
            result = create_empty_like(var)
            return
        end if
        
        n_elements = actual_stop - actual_start + 1
        
        ! Extract slice
        result = extract_slice_1d(var, actual_start, actual_stop, 1)
        
    end function slice_range
    
    !> Slice from negative index: [-n:] (last n elements)
    function slice_from_negative(var, negative_idx) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: negative_idx
        type(fortarray_t) :: result
        integer :: start_idx
        
        if (negative_idx >= 0) then
            write(error_unit,'(A)') "ERROR: Expected negative index"
            result = var
            return
        end if
        
        ! Convert negative index to positive
        start_idx = var%n_elements + negative_idx + 1
        if (start_idx < 1) start_idx = 1
        
        result = slice_range(var, start_idx, var%n_elements)
        
    end function slice_from_negative
    
    !> Slice with step: [start:stop:step]
    function slice_with_step(var, start_idx, stop_idx, step_val) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, stop_idx, step_val
        type(fortarray_t) :: result
        integer :: actual_start, actual_stop
        
        if (step_val <= 0) then
            write(error_unit,'(A)') "ERROR: Step value must be positive"
            result = var
            return
        end if
        
        ! Validate and normalize indices
        call validate_slice_params(var, start_idx, stop_idx, step_val, actual_start, actual_stop)
        
        if (actual_start > actual_stop) then
            result = create_empty_like(var)
            return
        end if
        
        ! Extract slice with step
        result = extract_slice_1d(var, actual_start, actual_stop, step_val)
        
    end function slice_with_step
    
    !> Slice 2D array: [x_start:x_stop, y_start:y_stop]
    function slice_2d_range(var, x_start, x_stop, y_start, y_stop) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: x_start, x_stop, y_start, y_stop
        type(fortarray_t) :: result
        
        if (var%n_dims /= 2) then
            write(error_unit,'(A)') "ERROR: Variable must be 2D for 2D slicing"
            result = var
            return
        end if
        
        result = extract_slice_2d(var, x_start, x_stop, y_start, y_stop)
        
    end function slice_2d_range
    
    !> Slice 3D array: [x_start:x_stop, y_start:y_stop, z_start:z_stop]
    function slice_3d_range(var, x_start, x_stop, y_start, y_stop, z_start, z_stop) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: x_start, x_stop, y_start, y_stop, z_start, z_stop
        type(fortarray_t) :: result
        
        if (var%n_dims /= 3) then
            write(error_unit,'(A)') "ERROR: Variable must be 3D for 3D slicing"
            result = var
            return
        end if
        
        result = extract_slice_3d(var, x_start, x_stop, y_start, y_stop, z_start, z_stop)
        
    end function slice_3d_range
    
    !> Slice with broadcasting: slice one dimension, keep others
    function slice_broadcast(var, start_idx, stop_idx, dim) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, stop_idx, dim
        type(fortarray_t) :: result
        
        if (dim < 1 .or. dim > var%n_dims) then
            write(error_unit,'(A,I0)') "ERROR: Invalid dimension for broadcasting: ", dim
            result = var
            return
        end if
        
        result = extract_slice_broadcast(var, start_idx, stop_idx, dim)
        
    end function slice_broadcast
    
    !> Generic multidimensional slice
    function slice_multidim(var) result(result)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        
        ! Simple implementation: slice each dimension by half
        select case(var%n_dims)
        case(1)
            result = slice_range(var, 1, var%shape(1)/2)
        case(2)
            result = slice_2d_range(var, 1, var%shape(1)/2, 1, var%shape(2)/2)
        case(3)
            result = slice_3d_range(var, 1, var%shape(1)/2, 1, var%shape(2)/2, 1, var%shape(3)/2)
        case default
            result = var  ! Not implemented for higher dimensions
        end select
        
    end function slice_multidim
    
    !> Slice with bounds checking
    function slice_with_bounds_check(var, start_idx, stop_idx) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, stop_idx
        type(fortarray_t) :: result
        integer :: safe_start, safe_stop
        
        ! Clamp indices to valid range
        safe_start = max(1, min(start_idx, var%n_elements))
        safe_stop = max(1, min(stop_idx, var%n_elements))
        
        result = slice_range(var, safe_start, safe_stop)
        
    end function slice_with_bounds_check
    
    !======= Helper Functions =======!
    
    !> Validate and normalize slice parameters
    subroutine validate_slice_params(var, start_idx, stop_idx, step_val, actual_start, actual_stop)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, stop_idx, step_val
        integer, intent(out) :: actual_start, actual_stop
        
        ! Handle negative indices
        actual_start = normalize_index(start_idx, var%n_elements)
        actual_stop = normalize_index(stop_idx, var%n_elements)
        
        ! Bounds checking
        actual_start = max(1, actual_start)
        actual_stop = min(var%n_elements, actual_stop)
        
        ! Ensure start <= stop for positive step
        if (step_val > 0 .and. actual_start > actual_stop) then
            actual_start = actual_stop + 1  ! Creates empty slice
        end if
        
    end subroutine validate_slice_params
    
    !> Normalize index (handle negative indices)
    function normalize_index(idx, array_size) result(normalized)
        integer, intent(in) :: idx, array_size
        integer :: normalized
        
        if (idx < 0) then
            normalized = array_size + idx + 1
        else
            normalized = idx
        end if
        
    end function normalize_index
    
    !> Extract 1D slice
    function extract_slice_1d(var, start_idx, stop_idx, step_val) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, stop_idx, step_val
        type(fortarray_t) :: result
        integer :: n_elements, i, j, src_idx
        
        ! Calculate number of elements in slice
        n_elements = (stop_idx - start_idx) / step_val + 1
        if (n_elements <= 0) then
            result = create_empty_like(var)
            return
        end if
        
        ! Initialize result
        result%name = var%name // "_slice"
        result%n_dims = var%n_dims
        result%n_elements = n_elements
        
        ! Copy dimension info
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(var%n_dims))
            result%dim_names = var%dim_names
        end if
        
        if (allocated(var%shape)) then
            allocate(result%shape(var%n_dims))
            result%shape = var%shape
            result%shape(1) = n_elements  ! Update first dimension
        end if
        
        ! Copy attributes
        result%units = var%units
        result%long_name = var%long_name
        result%standard_name = var%standard_name
        
        ! Initialize data storage
        result%data%dtype = var%data%dtype
        result%data%n_elements = n_elements
        result%data%initialized = .true.
        
        ! Extract data based on type
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            allocate(result%data%values_r64(n_elements))
            j = 0
            do i = start_idx, stop_idx, step_val
                j = j + 1
                result%data%values_r64(j) = var%data%values_r64(i)
            end do
            
        case(DTYPE_REAL32)
            allocate(result%data%values_r32(n_elements))
            j = 0
            do i = start_idx, stop_idx, step_val
                j = j + 1
                result%data%values_r32(j) = var%data%values_r32(i)
            end do
            
        case(DTYPE_INT64)
            allocate(result%data%values_i64(n_elements))
            j = 0
            do i = start_idx, stop_idx, step_val
                j = j + 1
                result%data%values_i64(j) = var%data%values_i64(i)
            end do
            
        case(DTYPE_INT32)
            allocate(result%data%values_i32(n_elements))
            j = 0
            do i = start_idx, stop_idx, step_val
                j = j + 1
                result%data%values_i32(j) = var%data%values_i32(i)
            end do
            
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for slicing"
            result = create_empty_like(var)
            return
        end select
        
        ! Handle coordinates
        if (allocated(var%coords) .and. allocated(var%has_coord)) then
            allocate(result%coords(var%n_dims))
            allocate(result%has_coord(var%n_dims))
            result%has_coord = var%has_coord
            
            ! Slice coordinates for first dimension
            if (var%has_coord(1)) then
                result%coords(1) = slice_coordinate(var%coords(1), start_idx, stop_idx, step_val)
            end if
            
            ! Copy other coordinates unchanged
            do i = 2, var%n_dims
                if (var%has_coord(i)) then
                    result%coords(i) = var%coords(i)
                end if
            end do
        end if
        
        result%initialized = .true.
        
    end function extract_slice_1d
    
    !> Extract 2D slice
    function extract_slice_2d(var, x_start, x_stop, y_start, y_stop) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: x_start, x_stop, y_start, y_stop
        type(fortarray_t) :: result
        integer :: nx, ny, n_elements, i, j, src_idx, dst_idx
        integer :: actual_x_start, actual_x_stop, actual_y_start, actual_y_stop
        
        ! Validate indices
        actual_x_start = max(1, min(x_start, var%shape(1)))
        actual_x_stop = max(1, min(x_stop, var%shape(1)))
        actual_y_start = max(1, min(y_start, var%shape(2)))
        actual_y_stop = max(1, min(y_stop, var%shape(2)))
        
        nx = actual_x_stop - actual_x_start + 1
        ny = actual_y_stop - actual_y_start + 1
        n_elements = nx * ny
        
        if (n_elements <= 0) then
            result = create_empty_like(var)
            return
        end if
        
        ! Initialize result
        result%name = var%name // "_slice2d"
        result%n_dims = 2
        result%n_elements = n_elements
        
        allocate(result%dim_names(2))
        result%dim_names = var%dim_names(1:2)
        
        allocate(result%shape(2))
        result%shape(1) = nx
        result%shape(2) = ny
        
        result%units = var%units
        result%long_name = var%long_name
        
        ! Initialize data storage
        result%data%dtype = var%data%dtype
        result%data%n_elements = n_elements
        result%data%initialized = .true.
        
        ! Extract data (column-major order)
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            allocate(result%data%values_r64(n_elements))
            dst_idx = 0
            do j = actual_y_start, actual_y_stop
                do i = actual_x_start, actual_x_stop
                    dst_idx = dst_idx + 1
                    src_idx = (j-1) * var%shape(1) + i
                    result%data%values_r64(dst_idx) = var%data%values_r64(src_idx)
                end do
            end do
            
        case(DTYPE_REAL32)
            allocate(result%data%values_r32(n_elements))
            dst_idx = 0
            do j = actual_y_start, actual_y_stop
                do i = actual_x_start, actual_x_stop
                    dst_idx = dst_idx + 1
                    src_idx = (j-1) * var%shape(1) + i
                    result%data%values_r32(dst_idx) = var%data%values_r32(src_idx)
                end do
            end do
            
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for 2D slicing"
            result = create_empty_like(var)
            return
        end select
        
        result%initialized = .true.
        
    end function extract_slice_2d
    
    !> Extract 3D slice
    function extract_slice_3d(var, x_start, x_stop, y_start, y_stop, z_start, z_stop) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: x_start, x_stop, y_start, y_stop, z_start, z_stop
        type(fortarray_t) :: result
        integer :: nx, ny, nz, n_elements
        integer :: actual_x_start, actual_x_stop
        integer :: actual_y_start, actual_y_stop
        integer :: actual_z_start, actual_z_stop
        
        ! Validate indices
        actual_x_start = max(1, min(x_start, var%shape(1)))
        actual_x_stop = max(1, min(x_stop, var%shape(1)))
        actual_y_start = max(1, min(y_start, var%shape(2)))
        actual_y_stop = max(1, min(y_stop, var%shape(2)))
        actual_z_start = max(1, min(z_start, var%shape(3)))
        actual_z_stop = max(1, min(z_stop, var%shape(3)))
        
        nx = actual_x_stop - actual_x_start + 1
        ny = actual_y_stop - actual_y_start + 1
        nz = actual_z_stop - actual_z_start + 1
        n_elements = nx * ny * nz
        
        if (n_elements <= 0) then
            result = create_empty_like(var)
            return
        end if
        
        ! Initialize result (simplified implementation)
        result%name = var%name // "_slice3d"
        result%n_dims = 3
        result%n_elements = n_elements
        
        allocate(result%dim_names(3))
        result%dim_names = var%dim_names(1:3)
        
        allocate(result%shape(3))
        result%shape(1) = nx
        result%shape(2) = ny
        result%shape(3) = nz
        
        result%units = var%units
        result%long_name = var%long_name
        
        ! Initialize data storage
        result%data%dtype = var%data%dtype
        result%data%n_elements = n_elements
        result%data%initialized = .true.
        
        ! Simplified extraction (just allocate correct size)
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            allocate(result%data%values_r64(n_elements))
            result%data%values_r64 = 1.0_real64  ! Placeholder
        case(DTYPE_REAL32)
            allocate(result%data%values_r32(n_elements))
            result%data%values_r32 = 1.0_real32  ! Placeholder
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for 3D slicing"
            result = create_empty_like(var)
            return
        end select
        
        result%initialized = .true.
        
    end function extract_slice_3d
    
    !> Extract slice with broadcasting
    function extract_slice_broadcast(var, start_idx, stop_idx, dim) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, stop_idx, dim
        type(fortarray_t) :: result
        integer :: n_elements, slice_size
        
        if (dim == 1) then
            slice_size = stop_idx - start_idx + 1
            n_elements = slice_size * (var%n_elements / var%shape(1))
        else
            n_elements = var%n_elements / 2  ! Simplified
        end if
        
        ! Simplified implementation
        result%name = var%name // "_broadcast"
        result%n_dims = var%n_dims
        result%n_elements = n_elements
        
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(var%n_dims))
            result%dim_names = var%dim_names
        end if
        
        if (allocated(var%shape)) then
            allocate(result%shape(var%n_dims))
            result%shape = var%shape
            result%shape(dim) = stop_idx - start_idx + 1
        end if
        
        result%units = var%units
        result%long_name = var%long_name
        
        result%data%dtype = var%data%dtype
        result%data%n_elements = n_elements
        result%data%initialized = .true.
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            allocate(result%data%values_r64(n_elements))
            result%data%values_r64 = 1.0_real64  ! Placeholder
        case(DTYPE_REAL32)
            allocate(result%data%values_r32(n_elements))
            result%data%values_r32 = 1.0_real32  ! Placeholder
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for broadcast slicing"
            result = create_empty_like(var)
            return
        end select
        
        result%initialized = .true.
        
    end function extract_slice_broadcast
    
    !> Slice coordinate array
    function slice_coordinate(coord, start_idx, stop_idx, step_val) result(result_coord)
        type(coordinate_t), intent(in) :: coord
        integer, intent(in) :: start_idx, stop_idx, step_val
        type(coordinate_t) :: result_coord
        integer :: n_elements, i, j
        
        n_elements = (stop_idx - start_idx) / step_val + 1
        
        result_coord%name = coord%name
        result_coord%length = n_elements
        result_coord%dtype = coord%dtype
        result_coord%initialized = .true.
        
        select case(coord%dtype)
        case(DTYPE_REAL64)
            allocate(result_coord%values_r64(n_elements))
            j = 0
            do i = start_idx, stop_idx, step_val
                j = j + 1
                result_coord%values_r64(j) = coord%values_r64(i)
            end do
            
        case(DTYPE_REAL32)
            allocate(result_coord%values_r32(n_elements))
            j = 0
            do i = start_idx, stop_idx, step_val
                j = j + 1
                result_coord%values_r32(j) = coord%values_r32(i)
            end do
            
        case(DTYPE_INT64)
            allocate(result_coord%values_i64(n_elements))
            j = 0
            do i = start_idx, stop_idx, step_val
                j = j + 1
                result_coord%values_i64(j) = coord%values_i64(i)
            end do
            
        case(DTYPE_INT32)
            allocate(result_coord%values_i32(n_elements))
            j = 0
            do i = start_idx, stop_idx, step_val
                j = j + 1
                result_coord%values_i32(j) = coord%values_i32(i)
            end do
            
        case default
            write(error_unit,'(A)') "WARNING: Unsupported coordinate type for slicing"
        end select
        
    end function slice_coordinate
    
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

end module fortarray_slicing