module foxel_netcdf
    use foxel_types
    use foxel_memory
    use foxel_storage
    use foxel_constructors
    use foxel_datasets
    use netcdf
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Error codes
    integer, parameter :: NC_SUCCESS = 0
    integer, parameter :: NC_ERROR_OPEN = -1
    integer, parameter :: NC_ERROR_READ = -2
    integer, parameter :: NC_ERROR_DIMS = -3
    integer, parameter :: NC_ERROR_VARS = -4
    integer, parameter :: NC_ERROR_ATTRS = -5
    integer, parameter :: NC_ERROR_TYPE = -6
    integer, parameter :: NC_ERROR_MEMORY = -7
    integer, parameter :: NC_ERROR_GROUPS = -8
    
    ! Public interfaces
    public :: read_netcdf
    public :: read_netcdf_variable
    public :: read_netcdf_metadata
    public :: list_netcdf_variables
    public :: list_netcdf_dimensions
    public :: list_netcdf_attributes
    public :: NC_SUCCESS, NC_ERROR_OPEN, NC_ERROR_READ
    public :: NC_ERROR_DIMS, NC_ERROR_VARS, NC_ERROR_ATTRS
    public :: NC_ERROR_TYPE, NC_ERROR_MEMORY, NC_ERROR_GROUPS
    
contains

    !> Read entire NetCDF file into dataset
    function read_netcdf(filename, variables, stat, error_msg) result(dset)
        character(len=*), intent(in) :: filename
        character(len=*), dimension(:), intent(in), optional :: variables
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataset_t) :: dset
        
        integer :: ncid, status, i, j, ndims, nvars, ngatts, unlimdimid
        integer :: dimid, varid, xtype, natts
        integer, dimension(16) :: dimids
        integer, dimension(:), allocatable :: dim_lens, var_dimids
        character(len=256) :: name
        character(len=256) :: err_msg
        type(dimension_t), dimension(:), allocatable :: dimensions
        type(variable_t) :: var
        logical :: load_all, load_var
        
        status = NC_SUCCESS
        err_msg = ""
        dset%initialized = .true.
        dset%filename = filename
        
        ! Open NetCDF file
        status = nf90_open(filename, NF90_NOWRITE, ncid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_OPEN
            write(err_msg, '(A,A,A,A)') "Failed to open NetCDF file '", trim(filename), "': ", &
                                       trim(nf90_strerror(status))
            goto 999
        end if
        
        ! Get file metadata
        status = nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_READ
            err_msg = "Failed to inquire NetCDF file"
            goto 998
        end if
        
        ! Determine which variables to load
        load_all = .not. present(variables)
        
        ! Read dimensions
        allocate(dimensions(ndims))
        allocate(dim_lens(ndims))
        
        do i = 1, ndims
            status = nf90_inquire_dimension(ncid, i, name, dim_lens(i))
            if (status /= NF90_NOERR) then
                status = NC_ERROR_DIMS
                write(err_msg, '(A,I0)') "Failed to read dimension ", i
                goto 998
            end if
            
            dimensions(i)%name = trim(name)
            dimensions(i)%length = dim_lens(i)
            dimensions(i)%is_unlimited = (i == unlimdimid)
        end do
        
        ! Store dimensions in dataset
        dset%n_dims = ndims
        if (allocated(dset%dimensions)) deallocate(dset%dimensions)
        allocate(dset%dimensions(ndims))
        dset%dimensions = dimensions
        
        ! Read global attributes
        if (allocated(dset%attr_keys)) deallocate(dset%attr_keys)
        if (allocated(dset%attr_values)) deallocate(dset%attr_values)
        allocate(dset%attr_keys(ngatts))
        allocate(dset%attr_values(ngatts))
        
        do i = 1, ngatts
            status = nf90_inq_attname(ncid, NF90_GLOBAL, i, name)
            if (status /= NF90_NOERR) cycle
            
            dset%attr_keys(i) = trim(name)
            status = nf90_get_att(ncid, NF90_GLOBAL, trim(name), dset%attr_values(i))
            if (status /= NF90_NOERR) then
                dset%attr_values(i) = "<error reading attribute>"
            end if
        end do
        dset%n_attrs = ngatts
        
        ! Read variables
        do i = 1, nvars
            ! Get variable metadata
            status = nf90_inquire_variable(ncid, i, name, xtype, ndims=j, natts=natts)
            if (status /= NF90_NOERR) cycle
            
            ! Check if we should load this variable
            if (load_all) then
                load_var = .true.
            else
                load_var = .false.
                do j = 1, size(variables)
                    if (trim(name) == trim(variables(j))) then
                        load_var = .true.
                        exit
                    end if
                end do
            end if
            
            if (.not. load_var) cycle
            
            ! Read the variable
            var = read_netcdf_variable_internal(ncid, i, dimensions, dim_lens, status)
            if (status /= NC_SUCCESS) then
                write(err_msg, '(A,A,A)') "Failed to read variable '", trim(name), "'"
                goto 998
            end if
            
            ! Add to dataset
            call add_variable(dset, var)
        end do
        
        ! Clean up
        if (allocated(dimensions)) deallocate(dimensions)
        if (allocated(dim_lens)) deallocate(dim_lens)
        
998     continue
        status = nf90_close(ncid)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        
        if (status /= NC_SUCCESS .and. status /= NF90_NOERR) then
            call finalize_dataset(dset)
        end if
    end function read_netcdf
    
    !> Read a single variable from NetCDF file
    function read_netcdf_variable(filename, varname, stat, error_msg) result(var)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: varname
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        integer :: ncid, status, varid, ndims, nvars, ngatts, unlimdimid
        integer :: i, var_ndims, xtype, natts
        integer, dimension(16) :: dimids
        integer, dimension(:), allocatable :: dim_lens
        character(len=256) :: err_msg
        character(len=256) :: name
        type(dimension_t), dimension(:), allocatable :: dimensions
        
        status = NC_SUCCESS
        err_msg = ""
        var%initialized = .false.
        
        ! Open NetCDF file
        status = nf90_open(filename, NF90_NOWRITE, ncid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_OPEN
            write(err_msg, '(A,A,A,A)') "Failed to open NetCDF file '", trim(filename), "': ", &
                                       trim(nf90_strerror(status))
            goto 999
        end if
        
        ! Get variable ID
        status = nf90_inq_varid(ncid, varname, varid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_VARS
            write(err_msg, '(A,A,A)') "Variable '", trim(varname), "' not found in file"
            goto 998
        end if
        
        ! Get file metadata for dimensions
        status = nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_READ
            err_msg = "Failed to inquire NetCDF file"
            goto 998
        end if
        
        ! Read dimensions
        allocate(dimensions(ndims))
        allocate(dim_lens(ndims))
        
        do i = 1, ndims
            status = nf90_inquire_dimension(ncid, i, name, dim_lens(i))
            if (status /= NF90_NOERR) then
                status = NC_ERROR_DIMS
                write(err_msg, '(A,I0)') "Failed to read dimension ", i
                goto 998
            end if
            
            dimensions(i)%name = trim(name)
            dimensions(i)%length = dim_lens(i)
            dimensions(i)%is_unlimited = (i == unlimdimid)
        end do
        
        ! Read the variable
        var = read_netcdf_variable_internal(ncid, varid, dimensions, dim_lens, status)
        if (status /= NC_SUCCESS) then
            write(err_msg, '(A,A,A)') "Failed to read variable '", trim(varname), "'"
        end if
        
        ! Clean up
        if (allocated(dimensions)) deallocate(dimensions)
        if (allocated(dim_lens)) deallocate(dim_lens)
        
998     continue
        status = nf90_close(ncid)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function read_netcdf_variable
    
    !> Internal function to read a variable given its ID
    function read_netcdf_variable_internal(ncid, varid, dimensions, dim_lens, status) result(var)
        integer, intent(in) :: ncid, varid
        type(dimension_t), dimension(:), intent(in) :: dimensions
        integer, dimension(:), intent(in) :: dim_lens
        integer, intent(out) :: status
        type(variable_t) :: var
        
        integer :: xtype, ndims, natts, i, j
        integer, dimension(16) :: dimids
        integer, dimension(:), allocatable :: var_shape, start, count
        character(len=256) :: name, dim_name, attr_name
        character(len=MAX_NAME_LEN), dimension(:), allocatable :: dim_names
        type(coordinate_t), dimension(:), allocatable :: coords
        real(real64), dimension(:), allocatable :: data_r64
        real(real32), dimension(:), allocatable :: data_r32
        integer(int32), dimension(:), allocatable :: data_i32
        integer(int64), dimension(:), allocatable :: data_i64
        character(len=256), dimension(:), allocatable :: data_char
        integer :: total_size
        logical :: is_coord_var
        
        
        status = NC_SUCCESS
        var%initialized = .false.
        
        ! Get variable metadata
        status = nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids, natts)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_VARS
            return
        end if
        
        
        ! Handle scalar variables (0D)
        if (ndims == 0) then
            select case(xtype)
            case(NF90_DOUBLE)
                allocate(data_r64(1))
                status = nf90_get_var(ncid, varid, data_r64(1))
                if (status == NF90_NOERR) then
                    var = variable_scalar(data_r64(1), name=trim(name))
                end if
            case(NF90_FLOAT)
                allocate(data_r32(1))
                status = nf90_get_var(ncid, varid, data_r32(1))
                if (status == NF90_NOERR) then
                    var = variable_scalar(real(data_r32(1), real64), name=trim(name))
                end if
            case(NF90_INT)
                allocate(data_i32(1))
                status = nf90_get_var(ncid, varid, data_i32(1))
                if (status == NF90_NOERR) then
                    var = variable_scalar(real(data_i32(1), real64), name=trim(name))
                end if
            case(NF90_INT64)
                allocate(data_i64(1))
                status = nf90_get_var(ncid, varid, data_i64(1))
                if (status == NF90_NOERR) then
                    var = variable_scalar(real(data_i64(1), real64), name=trim(name))
                end if
            case default
                status = NC_ERROR_TYPE
                return
            end select
            
            if (status /= NF90_NOERR) then
                status = NC_ERROR_READ
                return
            end if
            
            ! Read variable attributes
            call read_variable_attributes(ncid, varid, var, natts)
            return
        end if
        
        ! Get dimension names and shape
        allocate(dim_names(ndims))
        allocate(var_shape(ndims))
        
        do i = 1, ndims
            dim_names(i) = dimensions(dimids(i))%name
            var_shape(i) = dimensions(dimids(i))%length
        end do
        
        ! Calculate total size
        total_size = 1
        do i = 1, ndims
            total_size = total_size * var_shape(i)
        end do
        
        
        ! Check if this is a coordinate variable (1D with same name as dimension)
        is_coord_var = (ndims == 1 .and. trim(name) == trim(dim_names(1)))
        
        ! Read data based on type
        select case(xtype)
        case(NF90_DOUBLE)
            allocate(data_r64(total_size))
            
            if (ndims > 0) then
                ! For multi-dimensional arrays, read with explicit start/count
                allocate(start(ndims))
                allocate(count(ndims))
                start = 1
                count = var_shape
                
                select case(ndims)
                case(1)
                    status = nf90_get_var(ncid, varid, data_r64, start=start, count=count)
                case(2)
                    block
                        real(real64), dimension(:,:), allocatable :: temp_2d
                        allocate(temp_2d(var_shape(1), var_shape(2)))
                        status = nf90_get_var(ncid, varid, temp_2d)
                        if (status == NF90_NOERR) then
                            data_r64 = reshape(temp_2d, [total_size])
                        end if
                        deallocate(temp_2d)
                    end block
                case(3)
                    block
                        real(real64), dimension(:,:,:), allocatable :: temp_3d
                        allocate(temp_3d(var_shape(1), var_shape(2), var_shape(3)))
                        status = nf90_get_var(ncid, varid, temp_3d)
                        if (status == NF90_NOERR) then
                            data_r64 = reshape(temp_3d, [total_size])
                        end if
                        deallocate(temp_3d)
                    end block
                case default
                    status = nf90_get_var(ncid, varid, data_r64, start=start, count=count)
                end select
                
                deallocate(start)
                deallocate(count)
            else
                ! Scalar
                status = nf90_get_var(ncid, varid, data_r64)
            end if
            
            if (status == NF90_NOERR) then
                select case(ndims)
                case(1)
                    var = variable(data_r64(1:var_shape(1)), name=trim(name), dim_names=dim_names)
                case(2)
                    block
                        real(real64), dimension(:,:), allocatable :: data_2d
                        allocate(data_2d(var_shape(1), var_shape(2)))
                        data_2d = reshape(data_r64, [var_shape(1), var_shape(2)])
                        var = variable(data_2d, name=trim(name), dim_names=dim_names)
                        deallocate(data_2d)
                    end block
                case(3)
                    block
                        real(real64), dimension(:,:,:), allocatable :: data_3d
                        allocate(data_3d(var_shape(1), var_shape(2), var_shape(3)))
                        data_3d = reshape(data_r64, [var_shape(1), var_shape(2), var_shape(3)])
                        var = variable(data_3d, name=trim(name), dim_names=dim_names)
                        deallocate(data_3d)
                    end block
                case default
                    ! For >3D, we need to use variable_from_storage
                    var = create_variable_from_storage(data_r64, var_shape, trim(name), dim_names)
                end select
            end if
            
        case(NF90_FLOAT)
            allocate(data_r32(total_size))
            
            if (ndims > 0) then
                allocate(start(ndims))
                allocate(count(ndims))
                start = 1
                count = var_shape
                
                select case(ndims)
                case(1)
                    status = nf90_get_var(ncid, varid, data_r32, start=start, count=count)
                case(2)
                    block
                        real(real32), dimension(:,:), allocatable :: temp_2d
                        allocate(temp_2d(var_shape(1), var_shape(2)))
                        status = nf90_get_var(ncid, varid, temp_2d)
                        if (status == NF90_NOERR) then
                            data_r32 = reshape(temp_2d, [total_size])
                        end if
                        deallocate(temp_2d)
                    end block
                case(3)
                    block
                        real(real32), dimension(:,:,:), allocatable :: temp_3d
                        allocate(temp_3d(var_shape(1), var_shape(2), var_shape(3)))
                        status = nf90_get_var(ncid, varid, temp_3d)
                        if (status == NF90_NOERR) then
                            data_r32 = reshape(temp_3d, [total_size])
                        end if
                        deallocate(temp_3d)
                    end block
                case default
                    status = nf90_get_var(ncid, varid, data_r32, start=start, count=count)
                end select
                
                deallocate(start)
                deallocate(count)
            else
                status = nf90_get_var(ncid, varid, data_r32)
            end if
            if (status == NF90_NOERR) then
                select case(ndims)
                case(1)
                    var = variable(data_r32(1:var_shape(1)), name=trim(name), dim_names=dim_names)
                case(2)
                    block
                        real(real32), dimension(:,:), allocatable :: data_2d
                        allocate(data_2d(var_shape(1), var_shape(2)))
                        data_2d = reshape(data_r32, [var_shape(1), var_shape(2)])
                        var = variable(data_2d, name=trim(name), dim_names=dim_names)
                        deallocate(data_2d)
                    end block
                case(3)
                    block
                        real(real32), dimension(:,:,:), allocatable :: data_3d
                        allocate(data_3d(var_shape(1), var_shape(2), var_shape(3)))
                        data_3d = reshape(data_r32, [var_shape(1), var_shape(2), var_shape(3)])
                        var = variable(data_3d, name=trim(name), dim_names=dim_names)
                        deallocate(data_3d)
                    end block
                case default
                    var = create_variable_from_storage(data_r32, var_shape, trim(name), dim_names)
                end select
            end if
            
        case(NF90_INT)
            allocate(data_i32(total_size))
            
            if (ndims > 0) then
                allocate(start(ndims))
                allocate(count(ndims))
                start = 1
                count = var_shape
                
                select case(ndims)
                case(1)
                    status = nf90_get_var(ncid, varid, data_i32, start=start, count=count)
                case(2)
                    block
                        integer(int32), dimension(:,:), allocatable :: temp_2d
                        allocate(temp_2d(var_shape(1), var_shape(2)))
                        status = nf90_get_var(ncid, varid, temp_2d)
                        if (status == NF90_NOERR) then
                            data_i32 = reshape(temp_2d, [total_size])
                        end if
                        deallocate(temp_2d)
                    end block
                case(3)
                    block
                        integer(int32), dimension(:,:,:), allocatable :: temp_3d
                        allocate(temp_3d(var_shape(1), var_shape(2), var_shape(3)))
                        status = nf90_get_var(ncid, varid, temp_3d)
                        if (status == NF90_NOERR) then
                            data_i32 = reshape(temp_3d, [total_size])
                        end if
                        deallocate(temp_3d)
                    end block
                case default
                    status = nf90_get_var(ncid, varid, data_i32, start=start, count=count)
                end select
                
                deallocate(start)
                deallocate(count)
            else
                status = nf90_get_var(ncid, varid, data_i32)
            end if
            if (status == NF90_NOERR) then
                select case(ndims)
                case(1)
                    var = variable(data_i32(1:var_shape(1)), name=trim(name), dim_names=dim_names)
                case(2)
                    block
                        integer(int32), dimension(:,:), allocatable :: data_2d
                        allocate(data_2d(var_shape(1), var_shape(2)))
                        data_2d = reshape(data_i32, [var_shape(1), var_shape(2)])
                        var = variable(data_2d, name=trim(name), dim_names=dim_names)
                        deallocate(data_2d)
                    end block
                case(3)
                    block
                        integer(int32), dimension(:,:,:), allocatable :: data_3d
                        allocate(data_3d(var_shape(1), var_shape(2), var_shape(3)))
                        data_3d = reshape(data_i32, [var_shape(1), var_shape(2), var_shape(3)])
                        var = variable(data_3d, name=trim(name), dim_names=dim_names)
                        deallocate(data_3d)
                    end block
                case default
                    var = create_variable_from_storage(data_i32, var_shape, trim(name), dim_names)
                end select
            end if
            
        case(NF90_INT64)
            allocate(data_i64(total_size))
            
            if (ndims > 0) then
                allocate(start(ndims))
                allocate(count(ndims))
                start = 1
                count = var_shape
                
                select case(ndims)
                case(1)
                    status = nf90_get_var(ncid, varid, data_i64, start=start, count=count)
                case(2)
                    block
                        integer(int64), dimension(:,:), allocatable :: temp_2d
                        allocate(temp_2d(var_shape(1), var_shape(2)))
                        status = nf90_get_var(ncid, varid, temp_2d)
                        if (status == NF90_NOERR) then
                            data_i64 = reshape(temp_2d, [total_size])
                        end if
                        deallocate(temp_2d)
                    end block
                case(3)
                    block
                        integer(int64), dimension(:,:,:), allocatable :: temp_3d
                        allocate(temp_3d(var_shape(1), var_shape(2), var_shape(3)))
                        status = nf90_get_var(ncid, varid, temp_3d)
                        if (status == NF90_NOERR) then
                            data_i64 = reshape(temp_3d, [total_size])
                        end if
                        deallocate(temp_3d)
                    end block
                case default
                    status = nf90_get_var(ncid, varid, data_i64, start=start, count=count)
                end select
                
                deallocate(start)
                deallocate(count)
            else
                status = nf90_get_var(ncid, varid, data_i64)
            end if
            if (status == NF90_NOERR) then
                select case(ndims)
                case(1)
                    var = variable(data_i64(1:var_shape(1)), name=trim(name), dim_names=dim_names)
                case(2)
                    block
                        integer(int64), dimension(:,:), allocatable :: data_2d
                        allocate(data_2d(var_shape(1), var_shape(2)))
                        data_2d = reshape(data_i64, [var_shape(1), var_shape(2)])
                        var = variable(data_2d, name=trim(name), dim_names=dim_names)
                        deallocate(data_2d)
                    end block
                case(3)
                    block
                        integer(int64), dimension(:,:,:), allocatable :: data_3d
                        allocate(data_3d(var_shape(1), var_shape(2), var_shape(3)))
                        data_3d = reshape(data_i64, [var_shape(1), var_shape(2), var_shape(3)])
                        var = variable(data_3d, name=trim(name), dim_names=dim_names)
                        deallocate(data_3d)
                    end block
                case default
                    var = create_variable_from_storage(data_i64, var_shape, trim(name), dim_names)
                end select
            end if
            
        case(NF90_CHAR)
            ! String handling - NetCDF stores as char arrays
            allocate(data_char(total_size))
            ! For simplicity, read as 1D character array for now
            ! Full string support would need special handling
            status = NC_ERROR_TYPE  ! TODO: Implement string support
            
        case default
            status = NC_ERROR_TYPE
        end select
        
        if (status /= NF90_NOERR) then
            status = NC_ERROR_READ
            return
        end if
        
        ! Read variable attributes
        if (var%initialized) then
            call read_variable_attributes(ncid, varid, var, natts)
            
            ! Read coordinate variables if this variable has them
            if (.not. is_coord_var) then
                call read_coordinate_variables(ncid, var, dimensions, dim_lens)
            end if
        end if
        
        ! Clean up
        if (allocated(dim_names)) deallocate(dim_names)
        if (allocated(var_shape)) deallocate(var_shape)
        if (allocated(data_r64)) deallocate(data_r64)
        if (allocated(data_r32)) deallocate(data_r32)
        if (allocated(data_i32)) deallocate(data_i32)
        if (allocated(data_i64)) deallocate(data_i64)
        if (allocated(data_char)) deallocate(data_char)
        
    end function read_netcdf_variable_internal
    
    !> Create variable from storage for >3D arrays
    function create_variable_from_storage(data, shape, name, dim_names) result(var)
        class(*), dimension(:), intent(in) :: data
        integer, dimension(:), intent(in) :: shape
        character(len=*), intent(in) :: name
        character(len=*), dimension(:), intent(in) :: dim_names
        type(variable_t) :: var
        
        integer :: status, i
        
        ! Initialize variable structure
        var%name = name
        var%n_dims = size(shape)
        allocate(var%shape(var%n_dims))
        var%shape = shape
        var%n_elements = product(shape)
        
        allocate(var%dim_names(var%n_dims))
        var%dim_names = dim_names
        
        ! Initialize strides (Fortran column-major order)
        allocate(var%strides(var%n_dims))
        var%strides(1) = 1
        do i = 2, var%n_dims
            var%strides(i) = var%strides(i-1) * var%shape(i-1)
        end do
        
        ! Initialize coordinate arrays (empty for now)
        allocate(var%coords(var%n_dims))
        allocate(var%has_coord(var%n_dims))
        var%has_coord = .false.
        
        ! Initialize attributes
        var%n_attrs = 0
        
        ! Create storage based on data type
        select type(data)
        type is (real(real64))
            call create_storage(var%data, var%n_elements, "real64", status)
            if (status == 0) var%data%values_r64 = data
        type is (real(real32))
            call create_storage(var%data, var%n_elements, "real32", status)
            if (status == 0) var%data%values_r32 = data
        type is (integer(int32))
            call create_storage(var%data, var%n_elements, "int32", status)
            if (status == 0) var%data%values_i32 = data
        type is (integer(int64))
            call create_storage(var%data, var%n_elements, "int64", status)
            if (status == 0) var%data%values_i64 = data
        end select
        
        if (status == 0) then
            var%initialized = .true.
        end if
        
    end function create_variable_from_storage
    
    !> Read variable attributes
    subroutine read_variable_attributes(ncid, varid, var, natts)
        integer, intent(in) :: ncid, varid, natts
        type(variable_t), intent(inout) :: var
        
        integer :: i, status, xtype, len
        character(len=256) :: attr_name
        character(len=MAX_ATTR_LEN) :: attr_value
        real(real64) :: r64_val
        real(real32) :: r32_val
        integer :: int_val
        
        var%n_attrs = 0
        if (natts == 0) return
        
        allocate(var%attrs(natts))
        
        do i = 1, natts
            status = nf90_inq_attname(ncid, varid, i, attr_name)
            if (status /= NF90_NOERR) cycle
            
            var%attrs(i)%name = trim(attr_name)
            
            ! Get attribute type and length
            status = nf90_inquire_attribute(ncid, varid, trim(attr_name), xtype, len)
            if (status /= NF90_NOERR) cycle
            
            ! Read attribute based on type
            select case(xtype)
            case(NF90_CHAR)
                status = nf90_get_att(ncid, varid, trim(attr_name), attr_value)
                if (status == NF90_NOERR) then
                    var%attrs(i)%value = trim(attr_value)
                    var%attrs(i)%dtype = ATTR_TYPE_STRING
                end if
                
            case(NF90_DOUBLE)
                status = nf90_get_att(ncid, varid, trim(attr_name), r64_val)
                if (status == NF90_NOERR) then
                    write(var%attrs(i)%value, '(G0)') r64_val
                    var%attrs(i)%dtype = ATTR_TYPE_NUMERIC
                end if
                
            case(NF90_FLOAT)
                status = nf90_get_att(ncid, varid, trim(attr_name), r32_val)
                if (status == NF90_NOERR) then
                    write(var%attrs(i)%value, '(G0)') r32_val
                    var%attrs(i)%dtype = ATTR_TYPE_NUMERIC
                end if
                
            case(NF90_INT, NF90_INT64)
                status = nf90_get_att(ncid, varid, trim(attr_name), int_val)
                if (status == NF90_NOERR) then
                    write(var%attrs(i)%value, '(I0)') int_val
                    var%attrs(i)%dtype = ATTR_TYPE_NUMERIC
                end if
                
            case default
                var%attrs(i)%value = "<unsupported type>"
                var%attrs(i)%dtype = ATTR_TYPE_STRING
            end select
            
            var%n_attrs = var%n_attrs + 1
        end do
        
    end subroutine read_variable_attributes
    
    !> Read coordinate variables for a variable
    subroutine read_coordinate_variables(ncid, var, dimensions, dim_lens)
        integer, intent(in) :: ncid
        type(variable_t), intent(inout) :: var
        type(dimension_t), dimension(:), intent(in) :: dimensions
        integer, dimension(:), intent(in) :: dim_lens
        
        integer :: i, coord_id, status
        type(variable_t) :: coord_var
        
        if (.not. allocated(var%coords)) then
            allocate(var%coords(var%n_dims))
            allocate(var%has_coord(var%n_dims))
            var%has_coord = .false.
        end if
        
        do i = 1, var%n_dims
            ! Try to find coordinate variable with same name as dimension
            status = nf90_inq_varid(ncid, trim(var%dim_names(i)), coord_id)
            if (status == NF90_NOERR) then
                ! Read coordinate variable
                coord_var = read_netcdf_variable_internal(ncid, coord_id, dimensions, dim_lens, status)
                if (status == NC_SUCCESS .and. coord_var%initialized) then
                    ! Convert to coordinate type
                    call variable_to_coordinate(coord_var, var%coords(i))
                    var%has_coord(i) = .true.
                    call finalize_variable(coord_var)
                end if
            end if
        end do
        
    end subroutine read_coordinate_variables
    
    !> Convert variable to coordinate
    subroutine variable_to_coordinate(var, coord)
        type(variable_t), intent(in) :: var
        type(coordinate_t), intent(out) :: coord
        
        integer :: status
        
        if (.not. var%initialized .or. var%n_dims /= 1) return
        
        coord%name = var%name
        coord%length = var%shape(1)
        coord%dtype = var%data%dtype
        coord%initialized = .true.
        
        ! Copy data
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            allocate(coord%values_r64(coord%length))
            coord%values_r64 = var%data%values_r64(1:coord%length)
        case(DTYPE_REAL32)
            allocate(coord%values_r32(coord%length))
            coord%values_r32 = var%data%values_r32(1:coord%length)
        case(DTYPE_INT64)
            allocate(coord%values_i64(coord%length))
            coord%values_i64 = var%data%values_i64(1:coord%length)
        case(DTYPE_INT32)
            allocate(coord%values_i32(coord%length))
            coord%values_i32 = var%data%values_i32(1:coord%length)
        case(DTYPE_CHAR)
            ! TODO: Implement character coordinates
            coord%initialized = .false.
        end select
        
    end subroutine variable_to_coordinate
    
    !> Read NetCDF metadata without loading data
    function read_netcdf_metadata(filename, stat, error_msg) result(metadata)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataset_t) :: metadata
        
        integer :: ncid, status, ndims, nvars, ngatts, unlimdimid
        integer :: i, j, dimid, varid, xtype, var_ndims, natts
        integer, dimension(16) :: dimids
        character(len=256) :: name
        character(len=256) :: err_msg
        
        status = NC_SUCCESS
        err_msg = ""
        metadata%initialized = .true.
        metadata%filename = filename
        
        ! Open NetCDF file
        status = nf90_open(filename, NF90_NOWRITE, ncid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_OPEN
            write(err_msg, '(A,A,A,A)') "Failed to open NetCDF file '", trim(filename), "': ", &
                                       trim(nf90_strerror(status))
            goto 999
        end if
        
        ! Get file metadata
        status = nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_READ
            err_msg = "Failed to inquire NetCDF file"
            goto 998
        end if
        
        ! Read dimensions
        if (allocated(metadata%dimensions)) deallocate(metadata%dimensions)
        allocate(metadata%dimensions(ndims))
        metadata%n_dims = ndims
        
        do i = 1, ndims
            status = nf90_inquire_dimension(ncid, i, name, metadata%dimensions(i)%length)
            if (status /= NF90_NOERR) cycle
            
            metadata%dimensions(i)%name = trim(name)
            metadata%dimensions(i)%is_unlimited = (i == unlimdimid)
        end do
        
        ! Read global attributes
        if (allocated(metadata%attr_keys)) deallocate(metadata%attr_keys)
        if (allocated(metadata%attr_values)) deallocate(metadata%attr_values)
        allocate(metadata%attr_keys(ngatts))
        allocate(metadata%attr_values(ngatts))
        metadata%n_attrs = ngatts
        
        do i = 1, ngatts
            status = nf90_inq_attname(ncid, NF90_GLOBAL, i, name)
            if (status /= NF90_NOERR) cycle
            
            metadata%attr_keys(i) = trim(name)
            status = nf90_get_att(ncid, NF90_GLOBAL, trim(name), metadata%attr_values(i))
            if (status /= NF90_NOERR) then
                metadata%attr_values(i) = "<error reading attribute>"
            end if
        end do
        
        ! Store variable names only (no data)
        if (allocated(metadata%var_names)) deallocate(metadata%var_names)
        allocate(metadata%var_names(nvars))
        j = 0
        do i = 1, nvars
            status = nf90_inquire_variable(ncid, i, name)
            if (status == NF90_NOERR) then
                j = j + 1
                metadata%var_names(j) = trim(name)
            end if
        end do
        metadata%n_vars = j
        
998     continue
        status = nf90_close(ncid)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function read_netcdf_metadata
    
    !> List all variables in a NetCDF file
    function list_netcdf_variables(filename, stat, error_msg) result(varnames)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        character(len=MAX_NAME_LEN), dimension(:), allocatable :: varnames
        
        integer :: ncid, status, ndims, nvars, ngatts, unlimdimid
        integer :: i, j
        character(len=256) :: name
        character(len=256) :: err_msg
        
        status = NC_SUCCESS
        err_msg = ""
        
        ! Open NetCDF file
        status = nf90_open(filename, NF90_NOWRITE, ncid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_OPEN
            write(err_msg, '(A,A,A,A)') "Failed to open NetCDF file '", trim(filename), "': ", &
                                       trim(nf90_strerror(status))
            allocate(varnames(0))
            goto 999
        end if
        
        ! Get number of variables
        status = nf90_inquire(ncid, nDimensions=ndims, nVariables=nvars)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_READ
            err_msg = "Failed to inquire NetCDF file"
            allocate(varnames(0))
            goto 998
        end if
        
        ! Read variable names
        allocate(varnames(nvars))
        j = 0
        do i = 1, nvars
            status = nf90_inquire_variable(ncid, i, name)
            if (status == NF90_NOERR) then
                j = j + 1
                varnames(j) = trim(name)
            end if
        end do
        
        ! Resize if some failed
        if (j < nvars) then
            varnames = varnames(1:j)
        end if
        
998     continue
        status = nf90_close(ncid)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function list_netcdf_variables
    
    !> List all dimensions in a NetCDF file
    function list_netcdf_dimensions(filename, stat, error_msg) result(diminfo)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dimension_t), dimension(:), allocatable :: diminfo
        
        integer :: ncid, status, ndims, nvars, ngatts, unlimdimid
        integer :: i, dimlen
        character(len=256) :: name
        character(len=256) :: err_msg
        
        status = NC_SUCCESS
        err_msg = ""
        
        ! Open NetCDF file
        status = nf90_open(filename, NF90_NOWRITE, ncid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_OPEN
            write(err_msg, '(A,A,A,A)') "Failed to open NetCDF file '", trim(filename), "': ", &
                                       trim(nf90_strerror(status))
            allocate(diminfo(0))
            goto 999
        end if
        
        ! Get file info
        status = nf90_inquire(ncid, ndims, nvars, ngatts, unlimdimid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_READ
            err_msg = "Failed to inquire NetCDF file"
            allocate(diminfo(0))
            goto 998
        end if
        
        ! Read dimensions
        allocate(diminfo(ndims))
        
        do i = 1, ndims
            status = nf90_inquire_dimension(ncid, i, name, dimlen)
            if (status == NF90_NOERR) then
                diminfo(i)%name = trim(name)
                diminfo(i)%length = dimlen
                diminfo(i)%is_unlimited = (i == unlimdimid)
            end if
        end do
        
998     continue
        status = nf90_close(ncid)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function list_netcdf_dimensions
    
    !> List all global attributes in a NetCDF file
    function list_netcdf_attributes(filename, stat, error_msg) result(attrs)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(attribute_t), dimension(:), allocatable :: attrs
        
        integer :: ncid, status, ndims, nvars, ngatts, unlimdimid
        integer :: i, xtype, len
        character(len=256) :: name
        character(len=MAX_ATTR_LEN) :: value
        character(len=256) :: err_msg
        
        status = NC_SUCCESS
        err_msg = ""
        
        ! Open NetCDF file
        status = nf90_open(filename, NF90_NOWRITE, ncid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_OPEN
            write(err_msg, '(A,A,A,A)') "Failed to open NetCDF file '", trim(filename), "': ", &
                                       trim(nf90_strerror(status))
            allocate(attrs(0))
            goto 999
        end if
        
        ! Get number of global attributes
        status = nf90_inquire(ncid, nDimensions=ndims, nVariables=nvars, &
                             nAttributes=ngatts, unlimitedDimId=unlimdimid)
        if (status /= NF90_NOERR) then
            status = NC_ERROR_READ
            err_msg = "Failed to inquire NetCDF file"
            allocate(attrs(0))
            goto 998
        end if
        
        ! Read attributes
        allocate(attrs(ngatts))
        
        do i = 1, ngatts
            status = nf90_inq_attname(ncid, NF90_GLOBAL, i, name)
            if (status /= NF90_NOERR) cycle
            
            attrs(i)%name = trim(name)
            
            ! Get attribute value as string
            status = nf90_get_att(ncid, NF90_GLOBAL, trim(name), value)
            if (status == NF90_NOERR) then
                attrs(i)%value = trim(value)
                attrs(i)%dtype = ATTR_TYPE_STRING
            else
                attrs(i)%value = "<error reading attribute>"
                attrs(i)%dtype = ATTR_TYPE_STRING
            end if
        end do
        
998     continue
        status = nf90_close(ncid)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
    end function list_netcdf_attributes
    
end module foxel_netcdf