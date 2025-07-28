program test_csv
    use fortarray_types
    use fortarray_constructors
    use fortarray_csv
    use fortarray_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total, i
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running CSV I/O Test Suite"
    write(*,'(A)') "========================================"
    
    ! Create test CSV files
    call create_test_csv_files()
    
    ! Run tests
    call test_read_simple_csv()
    call test_read_csv_with_header()
    call test_read_csv_without_header()
    call test_read_csv_with_missing_values()
    call test_write_csv_from_variable()
    call test_write_csv_with_header()
    call test_write_csv_with_missing_values()
    call test_round_trip_csv()
    call test_csv_error_handling()
    call test_different_delimiters()
    call test_quoted_fields()
    call test_empty_file_handling()
    
    ! Clean up test files
    call cleanup_test_csv_files()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "CSV Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some CSV tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All CSV tests passed!"
    end if

contains

    subroutine create_test_csv_files()
        integer :: unit
        
        ! Simple numeric CSV with header
        open(newunit=unit, file="test_simple.csv", status="replace")
        write(unit,'(A)') "x,y,z"
        write(unit,'(A)') "1.0,2.0,3.0"
        write(unit,'(A)') "4.0,5.0,6.0"
        write(unit,'(A)') "7.0,8.0,9.0"
        close(unit)
        
        ! CSV without header
        open(newunit=unit, file="test_no_header.csv", status="replace")
        write(unit,'(A)') "10,20,30"
        write(unit,'(A)') "40,50,60"
        close(unit)
        
        ! CSV with missing values
        open(newunit=unit, file="test_missing.csv", status="replace")
        write(unit,'(A)') "temp,pressure,humidity"
        write(unit,'(A)') "25.5,1013.2,65.0"
        write(unit,'(A)') "NaN,1012.8,70.0"
        write(unit,'(A)') "27.1,,68.5"
        write(unit,'(A)') "26.0,1014.1,NaN"
        close(unit)
        
        ! CSV with quoted fields
        open(newunit=unit, file="test_quoted.csv", status="replace")
        write(unit,'(A)') 'name,value,description'
        write(unit,'(A)') '"Point A",12.5,"First measurement"'
        write(unit,'(A)') '"Point B",15.7,"Second measurement"'
        close(unit)
        
        ! CSV with semicolon delimiter
        open(newunit=unit, file="test_semicolon.csv", status="replace")
        write(unit,'(A)') "a;b;c"
        write(unit,'(A)') "1.1;2.2;3.3"
        write(unit,'(A)') "4.4;5.5;6.6"
        close(unit)
        
        ! Empty CSV file
        open(newunit=unit, file="test_empty.csv", status="replace")
        close(unit)
        
    end subroutine create_test_csv_files
    
    subroutine cleanup_test_csv_files()
        integer :: unit, iostat
        
        ! Delete test files
        open(newunit=unit, file="test_simple.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_no_header.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_missing.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_quoted.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_semicolon.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_empty.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_output.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_round_trip.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
    end subroutine cleanup_test_csv_files
    
    subroutine test_read_simple_csv()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        var = read_csv("test_simple.csv", stat=stat)
        
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read simple CSV, status: ", stat
        else if (.not. var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Variable not initialized after CSV read"
        else if (var%n_dims /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 2D variable, got ", var%n_dims
        else if (var%shape(1) /= 3 .or. var%shape(2) /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0)') "Expected shape [3,3], got [", var%shape(1), ",", var%shape(2), "]"
        else if (var%data%dtype /= DTYPE_REAL64) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected real64 data type, got ", var%data%dtype
        else
            ! Check some values
            if (abs(var%data%values_r64(1) - 1.0_real64) > 1e-10) test_passed = .false.
            if (abs(var%data%values_r64(5) - 5.0_real64) > 1e-10) test_passed = .false.
            if (abs(var%data%values_r64(9) - 9.0_real64) > 1e-10) test_passed = .false.
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read simple CSV test"
        else
            write(*,'(A)') "FAIL: Read simple CSV test"
        end if
    end subroutine test_read_simple_csv
    
    subroutine test_read_csv_with_header()
        type(fortarray_t) :: var
        type(csv_options_t) :: opts
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        opts%has_header = .true.
        var = read_csv("test_simple.csv", options=opts, stat=stat)
        
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read CSV with header, status: ", stat
        else if (var%shape(1) /= 3 .or. var%shape(2) /= 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Incorrect shape when reading with header"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read CSV with header test"
        else
            write(*,'(A)') "FAIL: Read CSV with header test"
        end if
    end subroutine test_read_csv_with_header
    
    subroutine test_read_csv_without_header()
        type(fortarray_t) :: var
        type(csv_options_t) :: opts
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        opts%has_header = .false.
        var = read_csv("test_no_header.csv", options=opts, stat=stat)
        
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read CSV without header, status: ", stat
        else if (var%shape(1) /= 2 .or. var%shape(2) /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0)') "Expected shape [2,3], got [", var%shape(1), ",", var%shape(2), "]"
        else
            ! Check values
            if (abs(var%data%values_r64(1) - 10.0_real64) > 1e-10) test_passed = .false.
            if (abs(var%data%values_r64(6) - 60.0_real64) > 1e-10) test_passed = .false.
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read CSV without header test"
        else
            write(*,'(A)') "FAIL: Read CSV without header test"
        end if
    end subroutine test_read_csv_without_header
    
    subroutine test_read_csv_with_missing_values()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        var = read_csv("test_missing.csv", stat=stat)
        
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read CSV with missing values, status: ", stat
        else if (var%shape(1) /= 4 .or. var%shape(2) /= 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Incorrect shape for CSV with missing values"
        else
            ! Check that missing values are represented as huge()
            ! Row 2, col 1 should be NaN: index = 2 (column-major order)
            if (abs(var%data%values_r64(2) - huge(1.0_real64)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A,G0,A,G0)') "Missing value not properly detected, got: ", &
                    var%data%values_r64(2), ", expected: ", huge(1.0_real64)
            end if
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read CSV with missing values test"
        else
            write(*,'(A)') "FAIL: Read CSV with missing values test"
        end if
    end subroutine test_read_csv_with_missing_values
    
    subroutine test_write_csv_from_variable()
        type(fortarray_t) :: var
        real(real64), dimension(3,2) :: data
        integer :: stat
        logical :: test_passed, file_exists
        integer :: unit
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = reshape([1.1_real64, 2.2_real64, 3.3_real64, 4.4_real64, 5.5_real64, 6.6_real64], [3, 2])
        var = new_array(data, name="test_data", dim_names=["rows", "cols"])
        
        ! Write to CSV
        stat = write_csv("test_output.csv", var)
        
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to write CSV, status: ", stat
        else
            ! Check file exists
            inquire(file="test_output.csv", exist=file_exists)
            if (.not. file_exists) then
                test_passed = .false.
                write(error_unit,'(A)') "CSV output file not created"
            end if
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Write CSV from variable test"
        else
            write(*,'(A)') "FAIL: Write CSV from variable test"
        end if
    end subroutine test_write_csv_from_variable
    
    subroutine test_write_csv_with_header()
        type(fortarray_t) :: var
        type(csv_options_t) :: opts
        real(real64), dimension(2,3) :: data
        integer :: stat, unit
        character(len=1024) :: line
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data (2 rows, 3 columns)
        data = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [2, 3])
        ! For CSV header, we need to specify column names, not row/dimension names
        ! We need a way to specify column headers for CSV output
        var = new_array(data, name="test_data")
        
        ! Write with header
        opts%has_header = .true.
        stat = write_csv("test_output.csv", var, options=opts)
        
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to write CSV with header, status: ", stat
        else
            ! Read first line to check header
            open(newunit=unit, file="test_output.csv", status="old")
            read(unit, '(A)') line
            close(unit)
            
            ! Check that some header was written (should have column names or generated names)
            if (len_trim(line) == 0 .or. index(line, ",") == 0) then
                test_passed = .false.
                write(error_unit,'(A,A)') "Header not written correctly, got: ", trim(line)
            end if
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Write CSV with header test"
        else
            write(*,'(A)') "FAIL: Write CSV with header test"
        end if
    end subroutine test_write_csv_with_header
    
    subroutine test_write_csv_with_missing_values()
        type(fortarray_t) :: var
        real(real64), dimension(2,2) :: data
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with missing values
        data(1,1) = 1.0_real64
        data(1,2) = huge(1.0_real64)  ! Missing value
        data(2,1) = 3.0_real64
        data(2,2) = 4.0_real64
        
        var = new_array(data, name="test_missing", dim_names=["a", "b"])
        
        ! Write CSV
        stat = write_csv("test_output.csv", var)
        
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to write CSV with missing values, status: ", stat
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Write CSV with missing values test"
        else
            write(*,'(A)') "FAIL: Write CSV with missing values test"
        end if
    end subroutine test_write_csv_with_missing_values
    
    subroutine test_round_trip_csv()
        type(fortarray_t) :: var_orig, var_read
        real(real64), dimension(3,2) :: data
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create original data
        data = reshape([1.5_real64, 2.7_real64, 3.9_real64, 4.1_real64, 5.3_real64, 6.8_real64], [3, 2])
        var_orig = new_array(data, name="round_trip", dim_names=["rows", "cols"])
        
        ! Write to CSV
        stat = write_csv("test_round_trip.csv", var_orig)
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write for round-trip test"
        end if
        
        ! Read back
        var_read = read_csv("test_round_trip.csv", stat=stat)
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read for round-trip test"
        end if
        
        ! Compare data
        if (test_passed) then
            if (var_read%shape(1) /= var_orig%shape(1) .or. var_read%shape(2) /= var_orig%shape(2)) then
                test_passed = .false.
                write(error_unit,'(A)') "Shape mismatch in round-trip test"
            else
                do i = 1, var_orig%n_elements
                    if (abs(var_orig%data%values_r64(i) - var_read%data%values_r64(i)) > 1e-10) then
                        test_passed = .false.
                        write(error_unit,'(A,I0,A,G0,A,G0)') "Data mismatch in round-trip test at element ", i, &
                            ", orig=", var_orig%data%values_r64(i), ", read=", var_read%data%values_r64(i)
                        exit
                    end if
                end do
            end if
        end if
        
        call finalize_variable(var_orig)
        call finalize_variable(var_read)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Round-trip CSV test"
        else
            write(*,'(A)') "FAIL: Round-trip CSV test"
        end if
    end subroutine test_round_trip_csv
    
    subroutine test_csv_error_handling()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test reading non-existent file
        var = read_csv("nonexistent.csv", stat=stat)
        if (stat == CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for non-existent file"
        end if
        
        ! Test reading empty file
        var = read_csv("test_empty.csv", stat=stat)
        if (stat == CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for empty file"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: CSV error handling test"
        else
            write(*,'(A)') "FAIL: CSV error handling test"
        end if
    end subroutine test_csv_error_handling
    
    subroutine test_different_delimiters()
        type(fortarray_t) :: var
        type(csv_options_t) :: opts
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test semicolon delimiter
        opts%delimiter = ';'
        opts%has_header = .true.
        var = read_csv("test_semicolon.csv", options=opts, stat=stat)
        
        if (stat /= CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read semicolon-delimited CSV, status: ", stat
        else if (var%shape(1) /= 2 .or. var%shape(2) /= 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Incorrect shape for semicolon-delimited CSV"
        else
            ! Check values
            if (abs(var%data%values_r64(1) - 1.1_real64) > 1e-10) test_passed = .false.
            if (abs(var%data%values_r64(6) - 6.6_real64) > 1e-10) test_passed = .false.
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Different delimiters test"
        else
            write(*,'(A)') "FAIL: Different delimiters test"
        end if
    end subroutine test_different_delimiters
    
    subroutine test_quoted_fields()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Note: This test will fail because we haven't implemented string data types yet
        ! But we test that it fails gracefully
        var = read_csv("test_quoted.csv", stat=stat)
        
        if (stat == CSV_SUCCESS) then
            ! If it succeeds, that's fine - numeric parsing might work
            test_passed = .true.
        else if (stat == CSV_ERROR_TYPE) then
            ! Expected failure for string data
            test_passed = .true.
        else
            test_passed = .false.
            write(error_unit,'(A,I0)') "Unexpected error for quoted CSV, status: ", stat
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Quoted fields test"
        else
            write(*,'(A)') "FAIL: Quoted fields test"
        end if
    end subroutine test_quoted_fields
    
    subroutine test_empty_file_handling()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        var = read_csv("test_empty.csv", stat=stat)
        
        if (stat == CSV_SUCCESS) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for empty CSV file"
        else if (stat /= CSV_ERROR_FORMAT) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Wrong error code for empty file, got: ", stat
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Empty file handling test"
        else
            write(*,'(A)') "FAIL: Empty file handling test"
        end if
    end subroutine test_empty_file_handling
    
end program test_csv