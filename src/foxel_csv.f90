module foxel_csv
    use foxel_types
    use foxel_constructors
    use foxel_storage
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Error codes
    integer, parameter :: CSV_SUCCESS = 0
    integer, parameter :: CSV_ERROR_OPEN = -1
    integer, parameter :: CSV_ERROR_READ = -2
    integer, parameter :: CSV_ERROR_WRITE = -3
    integer, parameter :: CSV_ERROR_FORMAT = -4
    integer, parameter :: CSV_ERROR_TYPE = -5
    integer, parameter :: CSV_ERROR_MEMORY = -6
    integer, parameter :: CSV_ERROR_DIMS = -7
    
    ! CSV parsing options
    type :: csv_options_t
        character(len=1) :: delimiter = ','
        character(len=1) :: quote_char = '"'
        logical :: has_header = .true.
        logical :: skip_empty_lines = .true.
        character(len=32) :: missing_value = "NaN"
        logical :: auto_detect_types = .true.
        character(len=16) :: default_type = "real64"
    end type csv_options_t
    
    ! Public interfaces
    public :: read_csv
    public :: write_csv
    public :: csv_options_t
    public :: CSV_SUCCESS, CSV_ERROR_OPEN, CSV_ERROR_READ
    public :: CSV_ERROR_WRITE, CSV_ERROR_FORMAT, CSV_ERROR_TYPE
    public :: CSV_ERROR_MEMORY, CSV_ERROR_DIMS
    
contains

    !> Read CSV file into a 2D variable
    function read_csv(filename, options, stat, error_msg) result(var)
        character(len=*), intent(in) :: filename
        type(csv_options_t), intent(in), optional :: options
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        type(variable_t) :: var
        
        type(csv_options_t) :: opts
        integer :: unit, status, io_status
        character(len=1024) :: line, err_msg
        character(len=256), dimension(:), allocatable :: headers
        character(len=256), dimension(:,:), allocatable :: data_strings
        real(real64), dimension(:,:), allocatable :: data_r64
        integer :: n_rows, n_cols, row, col, max_rows
        logical :: first_line
        
        status = CSV_SUCCESS
        err_msg = ""
        var%initialized = .false.
        
        ! Set default options
        if (present(options)) then
            opts = options
        end if
        
        ! Open file
        open(newunit=unit, file=filename, status='old', action='read', iostat=io_status)
        if (io_status /= 0) then
            status = CSV_ERROR_OPEN
            write(err_msg, '(A,A,A,I0)') "Failed to open CSV file '", trim(filename), "', iostat=", io_status
            goto 999
        end if
        
        ! First pass: determine dimensions
        n_rows = 0
        n_cols = 0
        first_line = .true.
        
        do
            read(unit, '(A)', iostat=io_status) line
            if (io_status /= 0) exit
            
            if (opts%skip_empty_lines .and. len_trim(line) == 0) cycle
            
            if (first_line) then
                n_cols = count_fields(line, opts%delimiter)
                first_line = .false.
            end if
            
            n_rows = n_rows + 1
        end do
        
        if (n_rows == 0 .or. n_cols == 0) then
            status = CSV_ERROR_FORMAT
            err_msg = "Empty CSV file or no data found"
            goto 998
        end if
        
        ! Adjust for header
        if (opts%has_header) then
            n_rows = n_rows - 1
        end if
        
        if (n_rows <= 0) then
            status = CSV_ERROR_FORMAT
            err_msg = "No data rows found (only header)"
            goto 998
        end if
        
        ! Allocate storage
        allocate(data_strings(n_rows, n_cols))
        if (opts%has_header) then
            allocate(headers(n_cols))
        end if
        
        ! Second pass: read data
        rewind(unit)
        row = 0
        first_line = .true.
        
        do
            read(unit, '(A)', iostat=io_status) line
            if (io_status /= 0) exit
            
            if (opts%skip_empty_lines .and. len_trim(line) == 0) cycle
            
            if (first_line .and. opts%has_header) then
                ! Parse header
                call parse_csv_line(line, headers, n_cols, opts%delimiter, opts%quote_char, status)
                if (status /= CSV_SUCCESS) then
                    err_msg = "Failed to parse CSV header"
                    goto 998
                end if
                first_line = .false.
                cycle
            end if
            first_line = .false.
            
            row = row + 1
            if (row > n_rows) exit
            
            ! Parse data line
            call parse_csv_line(line, data_strings(row, :), n_cols, opts%delimiter, opts%quote_char, status)
            if (status /= CSV_SUCCESS) then
                write(err_msg, '(A,I0)') "Failed to parse CSV line ", row
                goto 998
            end if
        end do
        
        ! Convert to numeric data if possible
        if (opts%auto_detect_types) then
            ! Try to convert to real64
            allocate(data_r64(n_rows, n_cols))
            
            do row = 1, n_rows
                do col = 1, n_cols
                    if (is_missing_value(data_strings(row, col), opts%missing_value)) then
                        data_r64(row, col) = huge(1.0_real64)  ! Missing value marker
                    else
                        read(data_strings(row, col), *, iostat=io_status) data_r64(row, col)
                        if (io_status /= 0) then
                            ! Not numeric - fall back to string storage
                            deallocate(data_r64)
                            status = CSV_ERROR_TYPE
                            err_msg = "String data type not yet supported"
                            goto 998
                        end if
                    end if
                end do
            end do
            
            ! Create variable with numeric data - use generic dimension names to avoid length issues
            var = variable(data_r64, name=trim(filename), dim_names=["rows", "cols"])
            
        else
            ! String data handling - not fully implemented for this sprint
            status = CSV_ERROR_TYPE
            err_msg = "String data type not yet supported"
            goto 998
        end if
        
998     continue
        close(unit)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        
        ! Clean up on error
        if (status /= CSV_SUCCESS) then
            call finalize_variable(var)
        end if
        
    end function read_csv
    
    !> Write 2D variable to CSV file
    function write_csv(filename, var, options, stat, error_msg) result(status)
        character(len=*), intent(in) :: filename
        type(variable_t), intent(in) :: var
        type(csv_options_t), intent(in), optional :: options
        integer, intent(out), optional :: stat
        character(len=*), intent(out), optional :: error_msg
        integer :: status
        
        type(csv_options_t) :: opts
        integer :: unit, io_status, row, col
        character(len=1024) :: err_msg, line
        character(len=32) :: field
        
        status = CSV_SUCCESS
        err_msg = ""
        
        ! Set default options
        if (present(options)) then
            opts = options
        end if
        
        ! Check variable is 2D
        if (.not. var%initialized) then
            status = CSV_ERROR_TYPE
            err_msg = "Variable not initialized"
            goto 999
        end if
        
        if (var%n_dims /= 2) then
            status = CSV_ERROR_DIMS
            write(err_msg, '(A,I0,A)') "Variable must be 2D for CSV export, got ", var%n_dims, "D"
            goto 999
        end if
        
        ! Open file for writing
        open(newunit=unit, file=filename, status='replace', action='write', iostat=io_status)
        if (io_status /= 0) then
            status = CSV_ERROR_OPEN
            write(err_msg, '(A,A,A,I0)') "Failed to open CSV file '", trim(filename), "' for writing, iostat=", io_status
            goto 999
        end if
        
        ! Write header if requested
        if (opts%has_header .and. allocated(var%dim_names)) then
            line = ""
            do col = 1, var%shape(2)
                if (col > 1) line = trim(line) // opts%delimiter
                if (col <= size(var%dim_names)) then
                    line = trim(line) // trim(var%dim_names(col))
                else
                    write(field, '(A,I0)') "col", col
                    line = trim(line) // trim(field)
                end if
            end do
            write(unit, '(A)') trim(line)
        end if
        
        ! Write data
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do row = 1, var%shape(1)
                line = ""
                do col = 1, var%shape(2)
                    if (col > 1) line = trim(line) // opts%delimiter
                    
                    ! Check for missing values
                    if (abs(var%data%values_r64((col-1)*var%shape(1) + row) - huge(1.0_real64)) < 1e-10) then
                        line = trim(line) // trim(opts%missing_value)
                    else
                        write(field, '(G0)') var%data%values_r64((col-1)*var%shape(1) + row)
                        line = trim(line) // trim(field)
                    end if
                end do
                write(unit, '(A)') trim(line)
            end do
            
        case(DTYPE_REAL32)
            do row = 1, var%shape(1)
                line = ""
                do col = 1, var%shape(2)
                    if (col > 1) line = trim(line) // opts%delimiter
                    
                    if (abs(var%data%values_r32((col-1)*var%shape(1) + row) - huge(1.0_real32)) < 1e-6) then
                        line = trim(line) // trim(opts%missing_value)
                    else
                        write(field, '(G0)') var%data%values_r32((col-1)*var%shape(1) + row)
                        line = trim(line) // trim(field)
                    end if
                end do
                write(unit, '(A)') trim(line)
            end do
            
        case(DTYPE_INT64)
            do row = 1, var%shape(1)
                line = ""
                do col = 1, var%shape(2)
                    if (col > 1) line = trim(line) // opts%delimiter
                    
                    if (var%data%values_i64((col-1)*var%shape(1) + row) == huge(1_int64)) then
                        line = trim(line) // trim(opts%missing_value)
                    else
                        write(field, '(I0)') var%data%values_i64((col-1)*var%shape(1) + row)
                        line = trim(line) // trim(field)
                    end if
                end do
                write(unit, '(A)') trim(line)
            end do
            
        case(DTYPE_INT32)
            do row = 1, var%shape(1)
                line = ""
                do col = 1, var%shape(2)
                    if (col > 1) line = trim(line) // opts%delimiter
                    
                    if (var%data%values_i32((col-1)*var%shape(1) + row) == huge(1_int32)) then
                        line = trim(line) // trim(opts%missing_value)
                    else
                        write(field, '(I0)') var%data%values_i32((col-1)*var%shape(1) + row)
                        line = trim(line) // trim(field)
                    end if
                end do
                write(unit, '(A)') trim(line)
            end do
            
        case default
            status = CSV_ERROR_TYPE
            err_msg = "Unsupported data type for CSV export"
            goto 998
        end select
        
998     continue
        close(unit)
        
999     continue
        if (present(stat)) stat = status
        if (present(error_msg)) error_msg = err_msg
        
    end function write_csv
    
    !> Count number of fields in a CSV line
    function count_fields(line, delimiter) result(n_fields)
        character(len=*), intent(in) :: line
        character(len=1), intent(in) :: delimiter
        integer :: n_fields
        
        integer :: i
        logical :: in_quotes
        character(len=1) :: ch
        
        n_fields = 1
        in_quotes = .false.
        
        do i = 1, len_trim(line)
            ch = line(i:i)
            
            if (ch == '"') then
                in_quotes = .not. in_quotes
            else if (ch == delimiter .and. .not. in_quotes) then
                n_fields = n_fields + 1
            end if
        end do
        
    end function count_fields
    
    !> Parse a CSV line into fields
    subroutine parse_csv_line(line, fields, n_fields, delimiter, quote_char, status)
        character(len=*), intent(in) :: line
        character(len=*), dimension(:), intent(out) :: fields
        integer, intent(in) :: n_fields
        character(len=1), intent(in) :: delimiter, quote_char
        integer, intent(out) :: status
        
        integer :: i, field_idx, start_pos, pos
        logical :: in_quotes
        character(len=1) :: ch
        character(len=1024) :: current_field
        
        status = CSV_SUCCESS
        field_idx = 1
        start_pos = 1
        in_quotes = .false.
        current_field = ""
        pos = 0
        
        do i = 1, len_trim(line)
            ch = line(i:i)
            
            if (ch == quote_char) then
                in_quotes = .not. in_quotes
            else if (ch == delimiter .and. .not. in_quotes) then
                ! End of field
                if (field_idx <= n_fields) then
                    fields(field_idx) = trim(current_field)
                    field_idx = field_idx + 1
                    current_field = ""
                    pos = 0
                end if
            else
                ! Add character to current field
                pos = pos + 1
                if (pos <= len(current_field)) then
                    current_field(pos:pos) = ch
                end if
            end if
        end do
        
        ! Handle last field
        if (field_idx <= n_fields) then
            fields(field_idx) = trim(current_field)
        end if
        
        if (field_idx < n_fields) then
            status = CSV_ERROR_FORMAT
        end if
        
    end subroutine parse_csv_line
    
    !> Check if a string represents a missing value
    function is_missing_value(str, missing_pattern) result(is_missing)
        character(len=*), intent(in) :: str, missing_pattern
        logical :: is_missing
        
        character(len=256) :: trimmed_str, trimmed_pattern
        
        trimmed_str = trim(adjustl(str))
        trimmed_pattern = trim(adjustl(missing_pattern))
        
        is_missing = (len_trim(trimmed_str) == 0) .or. &
                    (trimmed_str == trimmed_pattern) .or. &
                    (trimmed_str == "NA") .or. &
                    (trimmed_str == "na") .or. &
                    (trimmed_str == "NULL") .or. &
                    (trimmed_str == "null") .or. &
                    (trimmed_str == "NaN") .or. &
                    (trimmed_str == "nan")
        
    end function is_missing_value
    
end module foxel_csv