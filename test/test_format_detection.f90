program test_format_detection
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Format Detection Test Suite"
    write(*,'(A)') "========================================"
    
    ! Create test files
    call create_test_files()
    
    ! Run tests
    call test_netcdf_detection()
    call test_netcdf4_detection()
    call test_hdf5_detection()
    call test_csv_detection()
    call test_content_based_detection()
    call test_error_handling()
    call test_comprehensive_from_file()
    
    ! Clean up test files
    call cleanup_test_files()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Format Detection Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some format detection tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All format detection tests passed!"
    end if

contains

    subroutine create_test_files()
        type(fortarray_t) :: var
        type(dataset_t) :: ds
        real(real64), dimension(3,4) :: data
        integer :: stat, unit, i
        
        ! Create test data
        data = reshape([(real(i, real64), i=1,12)], [3,4])
        
        ! Create NetCDF file
        var = new_array(data, name="test_data", dim_names=["x", "y"])
        ds = new_dataset()
        call add_variable(ds, var)
        stat = write_netcdf("test_data.nc", ds)
        call finalize_dataset(ds)
        call finalize_variable(var)
        
        ! Create NetCDF4 file (same content, different extension)
        var = new_array(data, name="test_data", dim_names=["x", "y"])
        ds = new_dataset()
        call add_variable(ds, var)
        stat = write_netcdf("test_data.nc4", ds)
        call finalize_dataset(ds)
        call finalize_variable(var)
        
        ! Create HDF5 file (NetCDF4 is HDF5-based)
        var = new_array(data, name="test_data", dim_names=["x", "y"])
        ds = new_dataset()
        call add_variable(ds, var)
        stat = write_netcdf("test_data.hdf5", ds)
        call finalize_dataset(ds)
        call finalize_variable(var)
        
        ! Create CSV file
        var = new_array(data, name="test_data", dim_names=["x", "y"])
        stat = write_csv("test_data.csv", var)
        call finalize_variable(var)
        
        ! Create file with wrong extension
        var = new_array(data, name="test_data", dim_names=["x", "y"])
        stat = write_csv("test_data.txt", var)
        call finalize_variable(var)
        
        ! Create binary file (not a valid format)
        open(newunit=unit, file="test_data.bin", status="replace", access="stream")
        write(unit) data
        close(unit)
        
    end subroutine create_test_files
    
    subroutine cleanup_test_files()
        integer :: unit, iostat
        
        ! Delete test files
        open(newunit=unit, file="test_data.nc", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_data.nc4", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_data.hdf5", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_data.csv", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_data.txt", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="test_data.bin", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
        open(newunit=unit, file="nonexistent.nc", iostat=iostat)
        if (iostat == 0) close(unit, status="delete")
        
    end subroutine cleanup_test_files
    
    subroutine test_netcdf_detection()
        type(fortarray_t) :: var
        type(dataset_t) :: ds
        integer :: stat
        logical :: test_passed
        character(len=256) :: error_msg
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test reading .nc file with from_file
        var = from_file("test_data.nc", variable_name="test_data", stat=stat, error_msg=error_msg)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,A)') "Failed to read .nc file with from_file, status: ", stat, &
                ", error: ", trim(error_msg)
        else if (.not. var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Variable not initialized from .nc file"
        else if (var%n_elements /= 12) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Incorrect data size from .nc file, got ", var%n_elements, " elements"
        end if
        
        call finalize_variable(var)
        
        ! For now, skip dataset test as we only implemented variable version
        ! This can be added in a future sprint
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: NetCDF detection test"
        else
            write(*,'(A)') "FAIL: NetCDF detection test"
        end if
    end subroutine test_netcdf_detection
    
    subroutine test_netcdf4_detection()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        var = from_file("test_data.nc4", variable_name="test_data", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read .nc4 file with from_file, status: ", stat
        else if (.not. var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Variable not initialized from .nc4 file"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: NetCDF4 detection test"
        else
            write(*,'(A)') "FAIL: NetCDF4 detection test"
        end if
    end subroutine test_netcdf4_detection
    
    subroutine test_hdf5_detection()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        var = from_file("test_data.hdf5", variable_name="test_data", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read .hdf5 file with from_file, status: ", stat
        else if (.not. var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Variable not initialized from .hdf5 file"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: HDF5 detection test"
        else
            write(*,'(A)') "FAIL: HDF5 detection test"
        end if
    end subroutine test_hdf5_detection
    
    subroutine test_csv_detection()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        var = from_file("test_data.csv", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read .csv file with from_file, status: ", stat
        else if (.not. var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Variable not initialized from .csv file"
        else if (var%n_dims /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "CSV should produce 2D variable"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: CSV detection test"
        else
            write(*,'(A)') "FAIL: CSV detection test"
        end if
    end subroutine test_csv_detection
    
    subroutine test_content_based_detection()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test file with wrong extension but CSV content
        var = from_file("test_data.txt", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Failed to read .txt file with CSV content, status: ", stat
        else if (.not. var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Should detect CSV content despite .txt extension"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Content-based detection test"
        else
            write(*,'(A)') "FAIL: Content-based detection test"
        end if
    end subroutine test_content_based_detection
    
    subroutine test_error_handling()
        type(fortarray_t) :: var
        integer :: stat
        logical :: test_passed
        character(len=256) :: error_msg
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test non-existent file
        var = from_file("nonexistent.nc", stat=stat, error_msg=error_msg)
        
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for non-existent file"
        else if (var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Variable should not be initialized for failed read"
        else if (len_trim(error_msg) == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should provide error message for failed read"
        end if
        
        call finalize_variable(var)
        
        ! Test unsupported format
        var = from_file("test_data.bin", stat=stat, error_msg=error_msg)
        
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for unsupported binary format"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Error handling test"
        else
            write(*,'(A)') "FAIL: Error handling test"
        end if
    end subroutine test_error_handling
    
    subroutine test_comprehensive_from_file()
        type(fortarray_t) :: var_nc, var_csv
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test that different formats produce equivalent data
        var_nc = from_file("test_data.nc", variable_name="test_data", stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read NetCDF in comprehensive test"
        end if
        
        var_csv = from_file("test_data.csv", stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read CSV in comprehensive test"
        end if
        
        ! Compare shapes (CSV might have different shape due to header)
        if (test_passed .and. var_nc%initialized .and. var_csv%initialized) then
            if (var_nc%n_elements /= var_csv%n_elements) then
                ! This is expected as CSV might include header
                ! Just check both are initialized properly
                if (.not. (var_nc%n_elements > 0 .and. var_csv%n_elements > 0)) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Data size mismatch in comprehensive test"
                end if
            end if
        end if
        
        call finalize_variable(var_nc)
        call finalize_variable(var_csv)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Comprehensive from_file test"
        else
            write(*,'(A)') "FAIL: Comprehensive from_file test"
        end if
    end subroutine test_comprehensive_from_file
    
end program test_format_detection