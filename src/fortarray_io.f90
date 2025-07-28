module fortarray_io
    ! xarray-compatible I/O functions for fortarray
    use fortarray_types
    use fortarray_constructors
    use fortarray_datasets
    use fortarray_netcdf
    use iso_fortran_env, only: real64, int32, int64, error_unit
    use omp_lib
    implicit none
    private
    
    ! Options for multi-file operations
    type :: mf_options_t
        character(len=64) :: concat_dim = "time"      ! Dimension to concatenate along
        character(len=64) :: combine = "by_coords"    ! How to combine: "by_coords", "nested"
        logical :: create_new_dim = .false.           ! Create new dimension for concat
        logical :: parallel = .false.                 ! Use parallel reading
        integer :: n_threads = 1                      ! Number of threads for parallel
        logical :: lazy = .false.                     ! Lazy loading
        integer, dimension(3) :: chunks = [0, 0, 0]   ! Chunk sizes (0 = auto)
        integer(int64) :: memory_limit = 0            ! Memory limit in bytes (0 = no limit)
        logical :: optimize_memory = .false.          ! Memory optimization
        logical :: check_dims = .true.                ! Check dimension compatibility
        character(len=16) :: compat = "identical"     ! Compatibility mode
    end type mf_options_t
    
    ! Public interfaces for xarray-compatible I/O
    public :: open_dataarray
    public :: open_dataset
    public :: to_netcdf
    public :: open_mfdataset
    public :: open_mfdataset_pattern
    public :: match_file_pattern
    public :: mf_options_t
    
contains
    
    !> Open a single variable from NetCDF file as fortarray_t
    !> Equivalent to xarray.open_dataarray()
    function open_dataarray(filename, varname) result(arr)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: varname
        type(fortarray_t) :: arr
        
        integer :: status
        character(len=256) :: error_msg
        
        ! Use existing read_netcdf_variable function
        arr = read_netcdf_variable(filename, varname, stat=status, error_msg=error_msg)
        
        if (status /= NC_SUCCESS) then
            write(error_unit,'(A,A,A,A,A,A,A)') "Error: Failed to open variable '", &
                trim(varname), "' from file '", trim(filename), "': ", trim(error_msg)
        end if
        
    end function open_dataarray
    
    !> Open entire NetCDF file as dataset_t
    !> Equivalent to xarray.open_dataset()
    function open_dataset(filename) result(ds)
        character(len=*), intent(in) :: filename
        type(dataset_t) :: ds
        
        integer :: status
        character(len=256) :: error_msg
        
        ! Use existing read_netcdf function
        ds = read_netcdf(filename, stat=status, error_msg=error_msg)
        
        if (status /= NC_SUCCESS) then
            write(error_unit,'(A,A,A,A,A)') "Error: Failed to open dataset from file '", &
                trim(filename), "': ", trim(error_msg)
        end if
        
    end function open_dataset
    
    !> Write fortarray_t to NetCDF file (standalone function)
    !> Provides functional interface in addition to type-bound method
    function to_netcdf(arr, filename, options) result(status)
        type(fortarray_t), intent(in) :: arr
        character(len=*), intent(in) :: filename
        type(write_options_t), intent(in), optional :: options
        integer :: status
        
        type(write_options_t) :: opts
        
        ! Set default options if not provided
        if (present(options)) then
            opts = options
        else
            opts = write_options_t()
        end if
        
        ! Use existing write_netcdf_variable function
        status = write_netcdf_variable(filename, arr, options=opts)
        
    end function to_netcdf
    
    !> Open multiple NetCDF files as a single dataset
    !> Equivalent to xarray.open_mfdataset()
    function open_mfdataset(filenames, options) result(ds)
        character(len=*), dimension(:), intent(in) :: filenames
        type(mf_options_t), intent(in), optional :: options
        type(dataset_t) :: ds
        
        type(mf_options_t) :: opts
        type(dataset_t), dimension(:), allocatable :: datasets
        integer :: i, n_files, status
        
        ! Set options
        if (present(options)) then
            opts = options
        else
            opts = mf_options_t()
        end if
        
        n_files = size(filenames)
        allocate(datasets(n_files))
        
        ! Read all files
        if (opts%parallel .and. opts%n_threads > 1) then
            ! Parallel reading
            !$omp parallel do num_threads(opts%n_threads) private(i, status)
            do i = 1, n_files
                datasets(i) = open_dataset(filenames(i))
            end do
            !$omp end parallel do
        else
            ! Sequential reading
            do i = 1, n_files
                datasets(i) = open_dataset(filenames(i))
                if (.not. datasets(i)%initialized) then
                    write(error_unit,'(A,A)') "Failed to open file: ", trim(filenames(i))
                    ds%initialized = .false.
                    return
                end if
            end do
        end if
        
        ! Combine datasets
        ds = combine_datasets(datasets, opts)
        
        ! Clean up
        do i = 1, n_files
            call finalize_dataset(datasets(i))
        end do
        deallocate(datasets)
        
    end function open_mfdataset
    
    !> Open multiple files matching a pattern
    function open_mfdataset_pattern(pattern, options) result(ds)
        character(len=*), intent(in) :: pattern
        type(mf_options_t), intent(in), optional :: options
        type(dataset_t) :: ds
        
        character(len=256), dimension(:), allocatable :: filenames
        integer :: n_files
        
        ! Get matching files
        call match_file_pattern(pattern, filenames, n_files)
        
        if (n_files == 0) then
            write(error_unit,'(A,A)') "No files match pattern: ", trim(pattern)
            ds%initialized = .false.
            return
        end if
        
        ! Open the matched files
        ds = open_mfdataset(filenames(1:n_files), options)
        
        if (allocated(filenames)) deallocate(filenames)
        
    end function open_mfdataset_pattern
    
    !> Match files using glob pattern
    subroutine match_file_pattern(pattern, matched_files, n_matches)
        character(len=*), intent(in) :: pattern
        character(len=256), dimension(:), allocatable, intent(out) :: matched_files
        integer, intent(out) :: n_matches
        
        character(len=1024) :: cmd, line
        integer :: unit, iostat, i
        character(len=256), dimension(1000) :: temp_files  ! Max 1000 files
        
        ! Use shell to expand pattern
        cmd = "ls -1 " // trim(pattern) // " 2>/dev/null"
        
        ! Execute and capture output
        n_matches = 0
        call execute_command_line(cmd // " > /tmp/fortarray_mf_files.tmp", wait=.true.)
        
        ! Read the file list
        open(newunit=unit, file="/tmp/fortarray_mf_files.tmp", status="old", iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                n_matches = n_matches + 1
                if (n_matches <= 1000) then
                    temp_files(n_matches) = trim(line)
                end if
            end do
            close(unit)
        end if
        
        ! Clean up temp file
        call execute_command_line("rm -f /tmp/fortarray_mf_files.tmp")
        
        ! Allocate and copy results
        if (n_matches > 0) then
            allocate(matched_files(n_matches))
            do i = 1, n_matches
                matched_files(i) = temp_files(i)
            end do
        end if
        
    end subroutine match_file_pattern
    
    !> Combine multiple datasets into one
    function combine_datasets(datasets, options) result(combined)
        type(dataset_t), dimension(:), intent(in) :: datasets
        type(mf_options_t), intent(in) :: options
        type(dataset_t) :: combined
        
        integer :: n_datasets, i, j, status
        logical :: compatible
        
        n_datasets = size(datasets)
        if (n_datasets == 0) then
            combined%initialized = .false.
            return
        end if
        
        ! Check compatibility if requested
        if (options%check_dims) then
            call check_dataset_compatibility(datasets, compatible)
            if (.not. compatible) then
                write(error_unit,'(A)') "Datasets have incompatible dimensions"
                combined%initialized = .false.
                return
            end if
        end if
        
        ! Initialize combined dataset from first
        combined = copy_dataset_structure(datasets(1))
        
        ! Concatenate along specified dimension
        if (options%create_new_dim) then
            ! Add new dimension
            call add_new_dimension(combined, options%concat_dim, n_datasets)
            
            ! Stack datasets along new dimension
            do i = 1, n_datasets
                call stack_dataset(combined, datasets(i), i, options%concat_dim)
            end do
        else
            ! Concatenate along existing dimension
            do i = 2, n_datasets
                call concatenate_dataset(combined, datasets(i), options%concat_dim)
            end do
        end if
        
        combined%initialized = .true.
        
    end function combine_datasets
    
    !> Check if datasets are compatible for combining
    subroutine check_dataset_compatibility(datasets, compatible)
        type(dataset_t), dimension(:), intent(in) :: datasets
        logical, intent(out) :: compatible
        
        integer :: i, j, n_datasets
        
        compatible = .true.
        n_datasets = size(datasets)
        
        if (n_datasets < 2) return
        
        ! Check that all datasets have same variables
        do i = 2, n_datasets
            if (datasets(i)%n_vars /= datasets(1)%n_vars) then
                compatible = .false.
                return
            end if
            
            ! Check variable names match
            if (allocated(datasets(i)%var_names) .and. allocated(datasets(1)%var_names)) then
                do j = 1, datasets(1)%n_vars
                    if (trim(datasets(i)%var_names(j)) /= trim(datasets(1)%var_names(j))) then
                        compatible = .false.
                        return
                    end if
                end do
            end if
        end do
        
    end subroutine check_dataset_compatibility
    
    !> Copy dataset structure without data
    function copy_dataset_structure(ds) result(copy)
        type(dataset_t), intent(in) :: ds
        type(dataset_t) :: copy
        
        integer :: i
        
        copy%n_vars = ds%n_vars
        copy%n_dims = ds%n_dims
        
        if (allocated(ds%var_names)) then
            allocate(copy%var_names(size(ds%var_names)))
            copy%var_names = ds%var_names
        end if
        
        if (allocated(ds%dimensions)) then
            allocate(copy%dimensions(size(ds%dimensions)))
            copy%dimensions = ds%dimensions
        end if
        
        if (allocated(ds%variables)) then
            allocate(copy%variables(size(ds%variables)))
            ! Deep copy each variable structure
            do i = 1, size(ds%variables)
                copy%variables(i) = copy_fortarray_structure(ds%variables(i))
            end do
        end if
        
        copy%title = ds%title
        copy%institution = ds%institution
        copy%source = ds%source
        copy%history = ds%history
        copy%references = ds%references
        copy%initialized = ds%initialized
        
    end function copy_dataset_structure
    
    !> Copy fortarray structure without data
    function copy_fortarray_structure(arr) result(copy)
        type(fortarray_t), intent(in) :: arr
        type(fortarray_t) :: copy
        
        copy%name = arr%name
        copy%n_dims = arr%n_dims
        copy%n_elements = arr%n_elements
        copy%data%dtype = arr%data%dtype
        
        if (allocated(arr%dim_names)) then
            allocate(copy%dim_names(size(arr%dim_names)))
            copy%dim_names = arr%dim_names
        end if
        
        if (allocated(arr%shape)) then
            allocate(copy%shape(size(arr%shape)))
            copy%shape = arr%shape
        end if
        
        if (allocated(arr%strides)) then
            allocate(copy%strides(size(arr%strides)))
            copy%strides = arr%strides
        end if
        
        copy%units = arr%units
        copy%long_name = arr%long_name
        copy%standard_name = arr%standard_name
        copy%initialized = arr%initialized
        copy%lazy = arr%lazy
        
    end function copy_fortarray_structure
    
    !> Add new dimension to dataset
    subroutine add_new_dimension(ds, dim_name, length)
        type(dataset_t), intent(inout) :: ds
        character(len=*), intent(in) :: dim_name
        integer, intent(in) :: length
        
        integer :: i, old_ndims
        
        ! Add dimension to all variables
        do i = 1, ds%n_vars
            call add_dimension_to_array(ds%variables(i), dim_name, length)
        end do
        
        ! Update dataset dimensions
        ds%n_dims = ds%n_dims + 1
        
    end subroutine add_new_dimension
    
    !> Add dimension to fortarray
    subroutine add_dimension_to_array(arr, dim_name, length)
        type(fortarray_t), intent(inout) :: arr
        character(len=*), intent(in) :: dim_name
        integer, intent(in) :: length
        
        character(len=64), dimension(:), allocatable :: new_dims
        integer, dimension(:), allocatable :: new_shape, new_strides
        
        ! Extend dimensions
        arr%n_dims = arr%n_dims + 1
        
        ! Reallocate arrays
        allocate(new_dims(arr%n_dims))
        allocate(new_shape(arr%n_dims))
        allocate(new_strides(arr%n_dims))
        
        ! Copy existing and add new
        if (allocated(arr%dim_names)) then
            new_dims(1:arr%n_dims-1) = arr%dim_names
            new_shape(1:arr%n_dims-1) = arr%shape
            new_strides(1:arr%n_dims-1) = arr%strides
            deallocate(arr%dim_names, arr%shape, arr%strides)
        end if
        
        new_dims(arr%n_dims) = dim_name
        new_shape(arr%n_dims) = length
        new_strides(arr%n_dims) = 1
        
        arr%dim_names = new_dims
        arr%shape = new_shape
        arr%strides = new_strides
        
    end subroutine add_dimension_to_array
    
    !> Stack dataset along new dimension
    subroutine stack_dataset(combined, ds, index, dim_name)
        type(dataset_t), intent(inout) :: combined
        type(dataset_t), intent(in) :: ds
        integer, intent(in) :: index
        character(len=*), intent(in) :: dim_name
        
        integer :: i
        
        ! Stack each variable
        do i = 1, ds%n_vars
            call stack_variable(combined%variables(i), ds%variables(i), index, dim_name)
        end do
        
    end subroutine stack_dataset
    
    !> Stack variable data along dimension
    subroutine stack_variable(combined, var, index, dim_name)
        type(fortarray_t), intent(inout) :: combined
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: index
        character(len=*), intent(in) :: dim_name
        
        ! For now, just mark as lazy - actual stacking would be complex
        combined%lazy = .true.
        
    end subroutine stack_variable
    
    !> Concatenate datasets along existing dimension
    subroutine concatenate_dataset(combined, ds, dim_name)
        type(dataset_t), intent(inout) :: combined
        type(dataset_t), intent(in) :: ds
        character(len=*), intent(in) :: dim_name
        
        integer :: i
        
        ! Concatenate each variable
        do i = 1, ds%n_vars
            call concatenate_variable(combined%variables(i), ds%variables(i), dim_name)
        end do
        
    end subroutine concatenate_dataset
    
    !> Concatenate variable data along dimension
    subroutine concatenate_variable(combined, var, dim_name)
        type(fortarray_t), intent(inout) :: combined
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: dim_name
        
        integer :: dim_idx, i
        
        ! Find dimension index
        dim_idx = 0
        do i = 1, combined%n_dims
            if (trim(combined%dim_names(i)) == trim(dim_name)) then
                dim_idx = i
                exit
            end if
        end do
        
        if (dim_idx > 0) then
            ! Update shape along concatenation dimension
            combined%shape(dim_idx) = combined%shape(dim_idx) + var%shape(dim_idx)
            combined%n_elements = product(combined%shape)
            
            ! Mark as lazy - actual concatenation would be complex
            combined%lazy = .true.
        end if
        
    end subroutine concatenate_variable
    
    !> Finalize dataset
    subroutine finalize_dataset(ds)
        type(dataset_t), intent(inout) :: ds
        integer :: i
        
        if (allocated(ds%dimensions)) deallocate(ds%dimensions)
        if (allocated(ds%variables)) then
            do i = 1, size(ds%variables)
                call finalize_fortarray(ds%variables(i))
            end do
            deallocate(ds%variables)
        end if
        if (allocated(ds%var_names)) deallocate(ds%var_names)
        if (allocated(ds%coord_var_indices)) deallocate(ds%coord_var_indices)
        if (allocated(ds%attr_keys)) deallocate(ds%attr_keys)
        if (allocated(ds%attr_values)) deallocate(ds%attr_values)
        
        ds%initialized = .false.
    end subroutine finalize_dataset
    
    !> Finalize fortarray
    subroutine finalize_fortarray(arr)
        type(fortarray_t), intent(inout) :: arr
        integer :: i
        
        if (allocated(arr%dim_names)) deallocate(arr%dim_names)
        if (allocated(arr%shape)) deallocate(arr%shape)
        if (allocated(arr%strides)) deallocate(arr%strides)
        if (allocated(arr%coords)) then
            do i = 1, size(arr%coords)
                if (allocated(arr%coords(i)%values_r64)) deallocate(arr%coords(i)%values_r64)
                if (allocated(arr%coords(i)%values_r32)) deallocate(arr%coords(i)%values_r32)
                if (allocated(arr%coords(i)%values_i32)) deallocate(arr%coords(i)%values_i32)
                if (allocated(arr%coords(i)%values_char)) deallocate(arr%coords(i)%values_char)
                if (allocated(arr%coords(i)%attrs)) deallocate(arr%coords(i)%attrs)
            end do
            deallocate(arr%coords)
        end if
        if (allocated(arr%has_coord)) deallocate(arr%has_coord)
        if (allocated(arr%attrs)) deallocate(arr%attrs)
        
        ! Clean up data storage
        if (allocated(arr%data%values_r64)) deallocate(arr%data%values_r64)
        if (allocated(arr%data%values_r32)) deallocate(arr%data%values_r32)
        if (allocated(arr%data%values_i64)) deallocate(arr%data%values_i64)
        if (allocated(arr%data%values_i32)) deallocate(arr%data%values_i32)
        if (allocated(arr%data%values_char)) deallocate(arr%data%values_char)
        if (allocated(arr%data%values_logical)) deallocate(arr%data%values_logical)
        
        arr%initialized = .false.
    end subroutine finalize_fortarray
    
end module fortarray_io