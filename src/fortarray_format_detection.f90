module fortarray_format_detection
    use fortarray_types
    use fortarray_constructors, only: new_array, new_dataset, &
        variable_scalar_real64, variable_scalar_real32, variable_scalar_int32, variable_scalar_int64
    use fortarray_netcdf
    use fortarray_csv
    use fortarray_datasets
    use fortarray_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! File format types
    integer, parameter :: FORMAT_UNKNOWN = 0
    integer, parameter :: FORMAT_NETCDF = 1
    integer, parameter :: FORMAT_NETCDF4 = 2
    integer, parameter :: FORMAT_HDF5 = 3
    integer, parameter :: FORMAT_CSV = 4
    integer, parameter :: FORMAT_TEXT = 5
    
    ! Magic bytes for format detection
    character(len=4), parameter :: NETCDF_MAGIC = "CDF"//char(1)
    character(len=4), parameter :: HDF5_MAGIC = char(137)//"HDF"
    character(len=8), parameter :: NETCDF4_MAGIC = char(137)//"HDF"//char(13)//char(10)//char(26)//char(10)
    
    ! Public interfaces
    public :: from_file
    public :: detect_file_format
    
contains

    !> Read file with automatic format detection, returning a variable
    function from_file(filename, variable_name, stat, error_msg) result(var)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: variable_name
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(fortarray_t) :: var
        
        character(len=16) :: detected_format
        integer :: status
        character(len=1024) :: err_msg
        type(dataset_t) :: ds
        character(len=256), dimension(:), allocatable :: var_names
        
        status = 0
        err_msg = ""
        var%initialized = .false.
        
        ! Detect file format
        detected_format = detect_file_format(filename)
        if (detected_format == "unknown") then
            status = -1
            err_msg = "Could not detect file format"
            goto 999
        end if
        
        ! Read based on format
        select case(trim(detected_format))
        case("netcdf", "hdf5")
            ! For NetCDF/HDF5, read as dataset first
            ds = read_netcdf(filename, stat=status, error_msg=err_msg)
            if (status /= 0) then
                goto 999
            end if
            
            ! Extract single variable
            if (present(variable_name)) then
                if (has_variable(ds, variable_name)) then
                    var = get_variable(ds, variable_name)
                else
                    status = -1
                    write(err_msg, '(A,A,A)') "Variable '", trim(variable_name), "' not found in file"
                    call finalize_dataset(ds)
                    goto 999
                end if
            else
                ! Get first non-dimension variable if no name specified
                var_names = list_variables(ds)
                if (size(var_names) > 0) then
                    ! Try to find a non-dimension variable
                    ! For now, just get the first one
                    ! TODO: In a future sprint, add logic to skip dimension variables
                    var = get_variable(ds, var_names(1))
                    if (.not. var%initialized .and. size(var_names) > 1) then
                        ! Try the next variable
                        var = get_variable(ds, var_names(2))
                    end if
                else
                    status = -1
                    err_msg = "No variables found in NetCDF file"
                    call finalize_dataset(ds)
                    goto 999
                end if
            end if
            
            call finalize_dataset(ds)
            
        case("csv")
            ! CSV files return a single 2D variable
            var = read_csv(filename, stat=status, error_msg=err_msg)
            
        case("text")
            ! Try CSV reader for text files
            var = read_csv(filename, stat=status, error_msg=err_msg)
            if (status /= 0) then
                err_msg = "File appears to be text but not valid CSV format"
            end if
            
        case default
            status = -1
            write(err_msg, '(A,A)') "Unsupported file format for: ", trim(filename)
        end select
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        
    end function from_file
    
    !> Read file with automatic format detection, returning a dataset
    function from_file_to_dataset(filename, stat, error_msg) result(ds)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(dataset_t) :: ds
        
        character(len=16) :: detected_format
        integer :: status
        character(len=1024) :: err_msg
        type(fortarray_t) :: var
        
        status = 0
        err_msg = ""
        ds = new_dataset()
        
        ! Detect file format
        detected_format = detect_file_format(filename)
        if (detected_format == "unknown") then
            status = -1
            err_msg = "Could not detect file format"
            goto 999
        end if
        
        ! Read based on format
        select case(trim(detected_format))
        case("netcdf", "hdf5")
            ! NetCDF/HDF5 files naturally contain datasets
            ds = read_netcdf(filename, stat=status, error_msg=err_msg)
            
        case("csv")
            ! CSV files contain single variable, wrap in dataset
            var = read_csv(filename, stat=status, error_msg=err_msg)
            if (status == 0) then
                call add_variable(ds, var)
            end if
            call finalize_variable(var)
            
        case("text")
            ! Try CSV reader for text files
            var = read_csv(filename, stat=status, error_msg=err_msg)
            if (status == 0) then
                call add_variable(ds, var)
            else
                err_msg = "File appears to be text but not valid CSV format"
            end if
            call finalize_variable(var)
            
        case default
            status = -1
            write(err_msg, '(A,A)') "Unsupported file format for: ", trim(filename)
        end select
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        
    end function from_file_to_dataset
    
    !> Detect file format from extension and content (string result)
    function detect_file_format(filename) result(format)
        character(len=*), intent(in) :: filename
        character(len=16) :: format
        
        integer :: unit, io_status
        character(len=256) :: extension
        character(len=8) :: magic_bytes
        integer :: dot_pos
        logical :: file_exists
        
        format = "unknown"
        
        ! Check if file exists
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            return
        end if
        
        ! First try extension-based detection
        dot_pos = index(filename, '.', back=.true.)
        if (dot_pos > 0 .and. dot_pos < len_trim(filename)) then
            extension = filename(dot_pos+1:)
            call lowercase(extension)
            
            select case(trim(extension))
            case('nc', 'nc4', 'netcdf')
                format = "netcdf"
            case('hdf5', 'h5', 'hdf')
                format = "hdf5"
            case('csv')
                format = "csv"
            case('txt', 'dat')
                format = "text"
            case('bin')
                format = "binary"
            end select
        end if
        
        ! If extension didn't help, try content-based detection
        if (format == "unknown" .or. format == "text") then
            ! Read magic bytes
            open(newunit=unit, file=filename, status='old', access='stream', &
                 form='unformatted', iostat=io_status)
            if (io_status == 0) then
                ! Read first 8 bytes  
                magic_bytes = ""
                read(unit, iostat=io_status) magic_bytes
                close(unit)
                
                ! Check magic bytes
                if (magic_bytes(1:4) == HDF5_MAGIC) then
                    format = "hdf5"
                else if (magic_bytes(1:3) == "CDF") then
                    format = "netcdf"
                else if (is_csv_content(filename)) then
                    format = "csv"
                else if (format == "unknown") then
                    format = "text"
                end if
            end if
        end if
        
    end function detect_file_format
    
    !> Check if file content looks like CSV
    function is_csv_content(filename) result(is_csv)
        character(len=*), intent(in) :: filename
        logical :: is_csv
        
        integer :: unit, io_status, n_commas, n_semicolons, n_tabs
        character(len=1024) :: line
        integer :: i, line_count
        
        is_csv = .false.
        line_count = 0
        
        open(newunit=unit, file=filename, status='old', iostat=io_status)
        if (io_status /= 0) return
        
        ! Check first few lines
        do i = 1, 5
            read(unit, '(A)', iostat=io_status) line
            if (io_status /= 0) exit
            
            line_count = line_count + 1
            
            ! Count delimiters
            n_commas = count_char(line, ',')
            n_semicolons = count_char(line, ';')
            n_tabs = count_char(line, char(9))
            
            ! If we have consistent delimiters, likely CSV
            if (n_commas > 0 .or. n_semicolons > 0 .or. n_tabs > 0) then
                is_csv = .true.
            end if
        end do
        
        close(unit)
        
        ! Need at least one line with delimiters
        is_csv = is_csv .and. line_count > 0
        
    end function is_csv_content
    
    !> Count occurrences of character in string
    function count_char(str, ch) result(n)
        character(len=*), intent(in) :: str
        character(len=1), intent(in) :: ch
        integer :: n
        
        integer :: i
        
        n = 0
        do i = 1, len_trim(str)
            if (str(i:i) == ch) n = n + 1
        end do
        
    end function count_char
    
    !> Convert string to lowercase
    subroutine lowercase(str)
        character(len=*), intent(inout) :: str
        integer :: i, ic
        
        do i = 1, len_trim(str)
            ic = iachar(str(i:i))
            if (ic >= 65 .and. ic <= 90) then  ! A-Z
                str(i:i) = achar(ic + 32)      ! Convert to a-z
            end if
        end do
        
    end subroutine lowercase
    
end module fortarray_format_detection