module fortarray_io
    ! xarray-compatible I/O functions for fortarray
    use fortarray_types
    use fortarray_constructors
    use fortarray_datasets
    use fortarray_netcdf
    use iso_fortran_env, only: real64, int32, error_unit
    implicit none
    private
    
    ! Public interfaces for xarray-compatible I/O
    public :: open_dataarray
    public :: open_dataset
    public :: to_netcdf
    
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
    
end module fortarray_io