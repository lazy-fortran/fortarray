program test_format_support
    use iso_fortran_env, only: real64, int32, int64, error_unit
    use fortarray_types
    use fortarray_constructors
    use fortarray_io
    use fortarray_datasets
    use fortarray_csv, only: write_csv_variable
    use fortarray_format_detection, only: detect_file_format
    use fortarray_netcdf, only: write_netcdf_variable, read_netcdf_variable
    implicit none
    
    logical :: all_tests_passed = .true.
    
    write(*,'(A)') "Testing format support extensions..."
    write(*,'(A)') "===================================="
    
    call test_hdf5_group_support()
    call test_zarr_basic_support()
    call test_csv_metadata_handling()
    call test_binary_format_io()
    call test_format_auto_detection()
    call test_format_conversion()
    call test_hdf5_compression()
    call test_csv_with_coordinates()
    call test_binary_performance()
    call test_format_error_handling()
    
    if (all_tests_passed) then
        write(*,'(A)') "===================================="
        write(*,'(A)') "All format support tests passed!"
    else
        write(error_unit,'(A)') "===================================="
        write(error_unit,'(A)') "Some format support tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_hdf5_group_support()
        type(fortarray_t) :: arr1, arr2, loaded
        type(dataset_t) :: ds
        real(real64), dimension(10, 5) :: data1, data2
        integer :: i, j, status
        logical :: test_passed = .true.
        character(len=256) :: filename = "test_hdf5_groups.h5"
        
        ! Create test data
        do j = 1, 5
            do i = 1, 10
                data1(i, j) = real(i + j, real64)
                data2(i, j) = real(i * j, real64)
            end do
        end do
        
        arr1 = new_array(data1, dim_names=[character(len=10) :: "x", "y"], name="data1")
        arr2 = new_array(data2, dim_names=[character(len=10) :: "x", "y"], name="data2")
        
        ! Test: Write to HDF5 with groups
        status = arr1%to_hdf5(filename, group="/measurements/temperature")
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write to HDF5 group"
        end if
        
        status = arr2%to_hdf5(filename, group="/measurements/pressure", append=.true.)
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to append to HDF5 group"
        end if
        
        ! Test: Read from HDF5 group
        loaded = read_netcdf_variable(filename, "temperature")
        
        if (.not. loaded%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read from HDF5 group"
        end if
        
        ! Test: List groups
        block
            character(len=256), dimension(:), allocatable :: groups
            integer :: n_groups
            
            call list_hdf5_groups(filename, groups, n_groups)
            
            if (n_groups < 1) then
                test_passed = .false.
                write(error_unit,'(A)') "Failed to list HDF5 groups"
            end if
        end block
        
        call execute_command_line("rm -f " // trim(filename))
        
        if (test_passed) then
            write(*,'(A)') "PASS: HDF5 group support"
        else
            write(*,'(A)') "FAIL: HDF5 group support"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr1)
        call finalize_fortarray(arr2)
        call finalize_fortarray(loaded)
    end subroutine test_hdf5_group_support
    
    subroutine test_zarr_basic_support()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(20, 10) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        character(len=256) :: dirname = "test_zarr_store"
        
        ! Create test data
        do j = 1, 10
            do i = 1, 20
                data(i, j) = sin(real(i, real64) * 0.1_real64) * cos(real(j, real64) * 0.2_real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], name="data")
        
        ! Test: Write to Zarr format
        status = arr%to_zarr(dirname, chunks=[10, 5])
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write Zarr format"
        end if
        
        ! Test: Read from Zarr format
        loaded = open_zarr_array(dirname // "/data")
        
        if (.not. loaded%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read Zarr format"
        end if
        
        ! Test: Check metadata
        block
            logical :: file_exists
            inquire(file=trim(dirname) // "/.zarray", exist=file_exists)
            if (.not. file_exists) then
                test_passed = .false.
                write(error_unit,'(A)') "Zarr metadata file not created"
            end if
        end block
        
        call execute_command_line("rm -rf " // trim(dirname))
        
        if (test_passed) then
            write(*,'(A)') "PASS: Zarr basic support"
        else
            write(*,'(A)') "FAIL: Zarr basic support"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded)
    end subroutine test_zarr_basic_support
    
    subroutine test_csv_metadata_handling()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(15, 3) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        character(len=256) :: filename = "test_metadata.csv"
        
        ! Create test data with metadata
        do j = 1, 3
            do i = 1, 15
                data(i, j) = real(i, real64) ** real(j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "time", "variable"], &
                       name="measurements")
        
        ! Add metadata
        arr%units = "meters"
        arr%long_name = "Test measurements with metadata"
        
        ! Add coordinate for time
        block
            type(coordinate_t) :: time_coord
            real(real64), dimension(15) :: time_values
            
            do i = 1, 15
                time_values(i) = real(i-1, real64) * 0.1_real64
            end do
            
            time_coord%name = "time"
            time_coord%length = 15
            time_coord%dtype = DTYPE_REAL64
            allocate(time_coord%values_r64(15))
            time_coord%values_r64 = time_values
            
            arr%coords(1) = time_coord
            arr%has_coord(1) = .true.
        end block
        
        ! Test: Write CSV with metadata
        status = write_csv_variable(filename, arr, include_metadata=.true.)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write CSV with metadata"
        end if
        
        ! Test: Read CSV with metadata
        loaded = read_csv_with_metadata(filename)
        
        if (.not. loaded%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read CSV with metadata"
        else
            ! Check metadata was preserved
            if (trim(loaded%units) /= "meters") then
                test_passed = .false.
                write(error_unit,'(A)') "CSV metadata not preserved"
            end if
        end if
        
        call execute_command_line("rm -f " // trim(filename))
        
        if (test_passed) then
            write(*,'(A)') "PASS: CSV metadata handling"
        else
            write(*,'(A)') "FAIL: CSV metadata handling"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded)
    end subroutine test_csv_metadata_handling
    
    subroutine test_binary_format_io()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(100, 50) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        character(len=256) :: filename = "test_binary.dat"
        real(real64) :: start_time, end_time
        
        ! Create larger test data
        do j = 1, 50
            do i = 1, 100
                data(i, j) = real(i + j, real64) * 1.5_real64
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], name="data")
        
        ! Test: Write binary format
        call cpu_time(start_time)
        status = arr%to_binary(filename)
        call cpu_time(end_time)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write binary format"
        else
            write(*,'(A,F6.4,A)') "Binary write time: ", end_time - start_time, " seconds"
        end if
        
        ! Test: Read binary format
        call cpu_time(start_time)
        loaded = open_binary_array(filename)
        call cpu_time(end_time)
        
        if (.not. loaded%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read binary format"
        else
            write(*,'(A,F6.4,A)') "Binary read time: ", end_time - start_time, " seconds"
            
            ! Verify data integrity
            if (allocated(loaded%data%values_r64) .and. allocated(arr%data%values_r64)) then
                if (any(abs(loaded%data%values_r64 - arr%data%values_r64) > 1.0e-12)) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Binary data corruption detected"
                end if
            end if
        end if
        
        call execute_command_line("rm -f " // trim(filename) // "*")
        
        if (test_passed) then
            write(*,'(A)') "PASS: binary format I/O"
        else
            write(*,'(A)') "FAIL: binary format I/O"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded)
    end subroutine test_binary_format_io
    
    subroutine test_format_auto_detection()
        type(fortarray_t) :: arr
        real(real64), dimension(10, 10) :: data
        character(len=16) :: detected_format
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test data
        data = 1.0_real64
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], name="test")
        
        ! Create files in different formats
        status = write_netcdf_variable("test_detect.nc", arr)
        status = write_csv_variable("test_detect.csv", arr)
        status = arr%to_binary("test_detect.dat")
        
        ! Test: Detect NetCDF
        detected_format = detect_file_format("test_detect.nc")
        if (trim(detected_format) /= "netcdf") then
            test_passed = .false.
            write(error_unit,'(A,A)') "Failed to detect NetCDF, got: ", trim(detected_format)
        end if
        
        ! Test: Detect CSV
        detected_format = detect_file_format("test_detect.csv")
        if (trim(detected_format) /= "csv") then
            test_passed = .false.
            write(error_unit,'(A,A)') "Failed to detect CSV, got: ", trim(detected_format)
        end if
        
        ! Test: Detect binary
        detected_format = detect_file_format("test_detect.dat")
        if (trim(detected_format) /= "binary") then
            test_passed = .false.
            write(error_unit,'(A,A)') "Failed to detect binary, got: ", trim(detected_format)
        end if
        
        ! Test: Unknown format
        call execute_command_line("echo 'random text' > test_detect.txt")
        detected_format = detect_file_format("test_detect.txt")
        if (trim(detected_format) /= "unknown") then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to detect unknown format"
        end if
        
        call execute_command_line("rm -f test_detect.*")
        
        if (test_passed) then
            write(*,'(A)') "PASS: format auto-detection"
        else
            write(*,'(A)') "FAIL: format auto-detection"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_format_auto_detection
    
    subroutine test_format_conversion()
        type(fortarray_t) :: arr
        real(real64), dimension(20, 15) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test data
        do j = 1, 15
            do i = 1, 20
                data(i, j) = real(i, real64) * real(j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "lon", "lat"], name="data")
        
        ! Test: Convert NetCDF to CSV
        status = write_netcdf_variable("test_convert.nc", arr)
        status = convert_format("test_convert.nc", "test_convert.csv", "csv")
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to convert NetCDF to CSV"
        end if
        
        ! Test: Convert CSV to binary
        status = convert_format("test_convert.csv", "test_convert.dat", "binary")
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to convert CSV to binary"
        end if
        
        ! Test: Convert binary to NetCDF
        status = convert_format("test_convert.dat", "test_convert2.nc", "netcdf")
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to convert binary to NetCDF"
        end if
        
        ! Verify data integrity through conversions
        block
            type(fortarray_t) :: loaded
            loaded = open_dataarray("test_convert2.nc", "data")
            
            if (loaded%initialized) then
                if (loaded%shape(1) /= 20 .or. loaded%shape(2) /= 15) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Data dimensions lost in conversion"
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "Failed to verify converted data"
            end if
            
            call finalize_fortarray(loaded)
        end block
        
        call execute_command_line("rm -f test_convert*")
        
        if (test_passed) then
            write(*,'(A)') "PASS: format conversion"
        else
            write(*,'(A)') "FAIL: format conversion"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_format_conversion
    
    subroutine test_hdf5_compression()
        type(fortarray_t) :: arr
        type(write_options_t) :: options
        real(real64), dimension(1000, 500) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        integer(int64) :: uncompressed_size, compressed_size
        
        ! Create compressible data
        do j = 1, 500
            do i = 1, 1000
                data(i, j) = real(mod(i + j, 10), real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], name="data")
        
        ! Write uncompressed
        status = arr%to_hdf5("test_uncompressed.h5")
        inquire(file="test_uncompressed.h5", size=uncompressed_size)
        
        ! Write compressed
        options%compress = .true.
        options%compression = "gzip"
        options%deflate_level = 9
        status = arr%to_hdf5("test_compressed.h5", options=options)
        inquire(file="test_compressed.h5", size=compressed_size)
        
        write(*,'(A,I0,A)') "Uncompressed size: ", uncompressed_size, " bytes"
        write(*,'(A,I0,A)') "Compressed size: ", compressed_size, " bytes"
        write(*,'(A,F5.1,A)') "Compression ratio: ", &
            real(uncompressed_size) / real(compressed_size), ":1"
        
        if (compressed_size >= uncompressed_size) then
            test_passed = .false.
            write(error_unit,'(A)') "HDF5 compression failed to reduce size"
        end if
        
        call execute_command_line("rm -f test_*compressed.h5")
        
        if (test_passed) then
            write(*,'(A)') "PASS: HDF5 compression"
        else
            write(*,'(A)') "FAIL: HDF5 compression"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_hdf5_compression
    
    subroutine test_csv_with_coordinates()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(10, 5) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create data with non-uniform coordinates
        do j = 1, 5
            do i = 1, 10
                data(i, j) = real(i * j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "time", "depth"], &
                       name="temperature")
        
        ! Add non-uniform coordinates
        block
            type(coordinate_t) :: time_coord, depth_coord
            real(real64), dimension(10) :: time_values
            real(real64), dimension(5) :: depth_values
            
            ! Non-uniform time (hours)
            time_values = [0.0, 1.0, 2.5, 4.0, 6.0, 9.0, 12.0, 18.0, 24.0, 36.0]
            
            ! Non-uniform depth (meters)
            depth_values = [0.0, 10.0, 50.0, 100.0, 200.0]
            
            time_coord%name = "time"
            time_coord%length = 10
            time_coord%dtype = DTYPE_REAL64
            allocate(time_coord%values_r64(10))
            time_coord%values_r64 = time_values
            
            depth_coord%name = "depth"
            depth_coord%length = 5
            depth_coord%dtype = DTYPE_REAL64
            allocate(depth_coord%values_r64(5))
            depth_coord%values_r64 = depth_values
            
            arr%coords(1) = time_coord
            arr%coords(2) = depth_coord
            arr%has_coord = [.true., .true.]
        end block
        
        ! Test: Write CSV with coordinates
        status = write_csv_with_coords("test_coords.csv", arr)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write CSV with coordinates"
        end if
        
        ! Test: Read back and verify coordinates
        loaded = read_csv_with_coords("test_coords.csv")
        
        if (loaded%initialized) then
            if (all(loaded%has_coord)) then
                ! Check coordinate values preserved
                if (allocated(loaded%coords(1)%values_r64)) then
                    if (abs(loaded%coords(1)%values_r64(3) - 2.5_real64) > 1.0e-10) then
                        test_passed = .false.
                        write(error_unit,'(A)') "Time coordinates not preserved in CSV"
                    end if
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "Coordinates lost in CSV I/O"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read CSV with coordinates"
        end if
        
        call execute_command_line("rm -f test_coords.csv")
        
        if (test_passed) then
            write(*,'(A)') "PASS: CSV with coordinates"
        else
            write(*,'(A)') "FAIL: CSV with coordinates"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded)
    end subroutine test_csv_with_coordinates
    
    subroutine test_binary_performance()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(1000, 1000) :: data
        real(real64) :: nc_write_time, nc_read_time
        real(real64) :: bin_write_time, bin_read_time
        real(real64) :: start_time, end_time
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create large test data
        do j = 1, 1000
            do i = 1, 1000
                data(i, j) = real(i + j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], name="data")
        
        ! Benchmark NetCDF
        call cpu_time(start_time)
        status = write_netcdf_variable("test_perf.nc", arr)
        call cpu_time(end_time)
        nc_write_time = end_time - start_time
        
        call cpu_time(start_time)
        loaded = open_dataarray("test_perf.nc", "data")
        call cpu_time(end_time)
        nc_read_time = end_time - start_time
        call finalize_fortarray(loaded)
        
        ! Benchmark binary
        call cpu_time(start_time)
        status = arr%to_binary("test_perf.dat")
        call cpu_time(end_time)
        bin_write_time = end_time - start_time
        
        call cpu_time(start_time)
        loaded = open_binary_array("test_perf.dat")
        call cpu_time(end_time)
        bin_read_time = end_time - start_time
        
        write(*,'(A)') "Performance comparison (1000x1000 array):"
        write(*,'(A,F6.4,A)') "  NetCDF write: ", nc_write_time, " seconds"
        write(*,'(A,F6.4,A)') "  Binary write: ", bin_write_time, " seconds"
        write(*,'(A,F6.4,A)') "  NetCDF read:  ", nc_read_time, " seconds"
        write(*,'(A,F6.4,A)') "  Binary read:  ", bin_read_time, " seconds"
        write(*,'(A,F5.1,A)') "  Write speedup: ", nc_write_time / bin_write_time, "x"
        write(*,'(A,F5.1,A)') "  Read speedup:  ", nc_read_time / bin_read_time, "x"
        
        call execute_command_line("rm -f test_perf.*")
        
        if (test_passed) then
            write(*,'(A)') "PASS: binary performance"
        else
            write(*,'(A)') "FAIL: binary performance"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded)
    end subroutine test_binary_performance
    
    subroutine test_format_error_handling()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(5, 5) :: data
        integer :: status
        logical :: test_passed = .true.
        
        data = 1.0_real64
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], name="data")
        
        ! Test: Invalid format conversion
        status = convert_format("nonexistent.nc", "output.csv", "csv")
        if (status == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for nonexistent input file"
        end if
        
        ! Test: Unsupported format
        status = convert_format("test.nc", "test.xyz", "xyz")
        if (status == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for unsupported format"
        end if
        
        ! Test: Corrupted binary file
        block
            integer :: unit
            open(newunit=unit, file="corrupt.dat", form="unformatted", status="replace")
            write(unit) 12345  ! Wrong magic number
            close(unit)
            
            loaded = open_binary_array("corrupt.dat")
            if (loaded%initialized) then
                test_passed = .false.
                write(error_unit,'(A)') "Should fail for corrupted binary file"
            end if
            
            call execute_command_line("rm -f corrupt.dat")
        end block
        
        if (test_passed) then
            write(*,'(A)') "PASS: format error handling"
        else
            write(*,'(A)') "FAIL: format error handling"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded)
    end subroutine test_format_error_handling
    
    ! Helper functions that would need implementation
    
    
    
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
    
end program test_format_support