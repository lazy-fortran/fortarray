program test_multi_file_operations
    use iso_fortran_env, only: real64, int32, int64, error_unit
    use fortarray_types
    use fortarray_constructors
    use fortarray_io
    use fortarray_datasets
    use fortarray_netcdf, only: write_netcdf_variable, write_netcdf
    implicit none
    
    logical :: all_tests_passed = .true.
    character(len=256) :: test_files(4)
    integer :: i
    
    ! Define test file names
    test_files(1) = "test_mf_data_2020.nc"
    test_files(2) = "test_mf_data_2021.nc"
    test_files(3) = "test_mf_data_2022.nc"
    test_files(4) = "test_mf_data_2023.nc"
    
    write(*,'(A)') "Testing multiple file operations..."
    write(*,'(A)') "===================================="
    
    ! Create test files first
    call create_test_files()
    
    ! Run tests
    call test_open_mfdataset_basic()
    call test_open_mfdataset_pattern()
    call test_concatenation_along_time()
    call test_concatenation_along_new_dim()
    call test_parallel_file_reading()
    call test_lazy_loading()
    call test_memory_optimization()
    call test_file_pattern_matching()
    call test_error_handling()
    call test_mixed_dimensions()
    
    ! Clean up test files
    do i = 1, 4
        call execute_command_line("rm -f " // trim(test_files(i)))
    end do
    call execute_command_line("rm -f test_mf_*.nc")
    
    if (all_tests_passed) then
        write(*,'(A)') "===================================="
        write(*,'(A)') "All multi-file tests passed!"
    else
        write(error_unit,'(A)') "===================================="
        write(error_unit,'(A)') "Some multi-file tests failed!"
        stop 1
    end if
    
contains
    
    subroutine create_test_files()
        type(fortarray_t) :: temp, precip
        real(real64), dimension(10, 8, 12) :: temp_data
        real(real64), dimension(10, 8, 12) :: precip_data
        integer :: year, i, j, k, status
        character(len=4) :: year_str
        
        do year = 2020, 2023
            ! Generate data for this year
            do k = 1, 12  ! months
                do j = 1, 8   ! lat
                    do i = 1, 10  ! lon
                        ! Temperature varies by location and month
                        temp_data(i, j, k) = 273.15_real64 + &
                            10.0_real64 * sin(real(k-1, real64) * 3.14159_real64 / 6.0_real64) + &
                            real(year - 2020, real64) * 0.5_real64 + &
                            real(j, real64) * 0.1_real64
                        
                        ! Precipitation varies similarly
                        precip_data(i, j, k) = 50.0_real64 + &
                            30.0_real64 * cos(real(k-1, real64) * 3.14159_real64 / 6.0_real64) + &
                            real(i, real64) * 2.0_real64
                    end do
                end do
            end do
            
            ! Create arrays with time coordinate for this year
            temp = new_array(temp_data, dim_names=[character(len=10) :: "lon", "lat", "time"], &
                           name="temperature")
            precip = new_array(precip_data, dim_names=[character(len=10) :: "lon", "lat", "time"], &
                             name="precipitation")
            
            ! Add time coordinate (months since 2020-01-01)
            block
                type(coordinate_t) :: time_coord
                real(real64), dimension(12) :: time_values
                integer :: m
                
                do m = 1, 12
                    time_values(m) = real((year - 2020) * 12 + m - 1, real64)
                end do
                
                time_coord%name = "time"
                time_coord%length = 12
                time_coord%dtype = DTYPE_REAL64
                allocate(time_coord%values_r64(12))
                time_coord%values_r64 = time_values
                
                temp%coords(3) = time_coord
                temp%has_coord(3) = .true.
                precip%coords(3) = time_coord
                precip%has_coord(3) = .true.
            end block
            
            ! Write to file
            write(year_str, '(I4)') year
            status = write_netcdf_variable(test_files(year - 2019), temp)
            if (status /= 0) then
                write(error_unit, '(A,I0)') "Failed to create test file for year ", year
            end if
            
            ! Also write precipitation to same file
            ! For simplicity, just create single variable files for dataset tests
            status = write_netcdf_variable("test_mf_dataset_" // trim(year_str) // ".nc", temp)
            
            call finalize_fortarray(temp)
            call finalize_fortarray(precip)
        end do
        
    end subroutine create_test_files
    
    subroutine test_open_mfdataset_basic()
        type(dataset_t) :: ds
        character(len=256), dimension(4) :: filenames
        logical :: test_passed = .true.
        integer :: i
        
        ! Test: Open multiple files as single dataset
        do i = 1, 4
            write(filenames(i), '(A,I0,A)') "test_mf_dataset_", 2019 + i, ".nc"
        end do
        
        ds = open_mfdataset(filenames)
        
        if (ds%initialized) then
            ! Check that we have the expected variables
            if (ds%n_vars /= 2) then
                test_passed = .false.
                write(error_unit,'(A,I0)') "Expected 2 variables, got: ", ds%n_vars
            end if
            
            ! Check that time dimension was concatenated (4 years * 12 months = 48)
            if (allocated(ds%variables)) then
                if (ds%variables(1)%shape(3) /= 48) then
                    test_passed = .false.
                    write(error_unit,'(A,I0)') "Expected 48 time steps, got: ", &
                        ds%variables(1)%shape(3)
                end if
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "open_mfdataset failed to initialize dataset"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: open_mfdataset basic"
        else
            write(*,'(A)') "FAIL: open_mfdataset basic"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_open_mfdataset_basic
    
    subroutine test_open_mfdataset_pattern()
        type(dataset_t) :: ds
        logical :: test_passed = .true.
        
        ! Test: Open files using pattern
        ds = open_mfdataset_pattern("test_mf_dataset_*.nc")
        
        if (ds%initialized) then
            if (ds%n_vars /= 2) then
                test_passed = .false.
                write(error_unit,'(A)') "Pattern-based loading failed"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "open_mfdataset_pattern failed"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: open_mfdataset with pattern"
        else
            write(*,'(A)') "FAIL: open_mfdataset with pattern"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_open_mfdataset_pattern
    
    subroutine test_concatenation_along_time()
        type(dataset_t) :: ds
        type(mf_options_t) :: options
        logical :: test_passed = .true.
        
        ! Test: Concatenate along time dimension
        options%concat_dim = "time"
        options%combine = "nested"
        
        ds = open_mfdataset_pattern("test_mf_dataset_*.nc", options)
        
        if (ds%initialized) then
            ! Verify time values are monotonic
            if (allocated(ds%variables(1)%coords(3)%values_r64)) then
                block
                    real(real64), dimension(:), allocatable :: time_vals
                    integer :: n, i
                    
                    time_vals = ds%variables(1)%coords(3)%values_r64
                    n = size(time_vals)
                    
                    do i = 2, n
                        if (time_vals(i) <= time_vals(i-1)) then
                            test_passed = .false.
                            write(error_unit,'(A,I0)') "Time not monotonic at index ", i
                            exit
                        end if
                    end do
                end block
            else
                test_passed = .false.
                write(error_unit,'(A)') "Time coordinate not allocated"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Concatenation along time failed"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: concatenation along time"
        else
            write(*,'(A)') "FAIL: concatenation along time"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_concatenation_along_time
    
    subroutine test_concatenation_along_new_dim()
        type(dataset_t) :: ds
        type(mf_options_t) :: options
        logical :: test_passed = .true.
        
        ! Test: Concatenate along new dimension (e.g., ensemble member)
        options%concat_dim = "ensemble"
        options%combine = "nested"
        options%create_new_dim = .true.
        
        ds = open_mfdataset_pattern("test_mf_dataset_*.nc", options)
        
        if (ds%initialized) then
            ! Check that ensemble dimension was created
            if (ds%variables(1)%n_dims /= 4) then
                test_passed = .false.
                write(error_unit,'(A,I0)') "Expected 4 dimensions, got: ", &
                    ds%variables(1)%n_dims
            else
                ! Check ensemble dimension name
                if (trim(ds%variables(1)%dim_names(4)) /= "ensemble") then
                    test_passed = .false.
                    write(error_unit,'(A)') "New dimension name incorrect"
                end if
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Concatenation along new dimension failed"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: concatenation along new dimension"
        else
            write(*,'(A)') "FAIL: concatenation along new dimension"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_concatenation_along_new_dim
    
    subroutine test_parallel_file_reading()
        type(dataset_t) :: ds
        type(mf_options_t) :: options
        real(real64) :: start_time, end_time
        logical :: test_passed = .true.
        
        ! Test: Parallel file reading
        options%parallel = .true.
        options%n_threads = 4
        
        call cpu_time(start_time)
        ds = open_mfdataset_pattern("test_mf_dataset_*.nc", options)
        call cpu_time(end_time)
        
        if (ds%initialized) then
            write(*,'(A,F6.3,A)') "Parallel read completed in ", &
                end_time - start_time, " seconds"
        else
            test_passed = .false.
            write(error_unit,'(A)') "Parallel file reading failed"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: parallel file reading"
        else
            write(*,'(A)') "FAIL: parallel file reading"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_parallel_file_reading
    
    subroutine test_lazy_loading()
        type(dataset_t) :: ds
        type(mf_options_t) :: options
        logical :: test_passed = .true.
        
        ! Test: Lazy loading (data not loaded until accessed)
        options%lazy = .true.
        options%chunks = [5, 4, 6]  ! Chunk sizes for reading
        
        ds = open_mfdataset_pattern("test_mf_dataset_*.nc", options)
        
        if (ds%initialized) then
            ! Check that data is not yet loaded (lazy)
            if (.not. ds%variables(1)%lazy) then
                test_passed = .false.
                write(error_unit,'(A)') "Data was loaded eagerly, not lazily"
            end if
            
            ! Access a subset to trigger loading
            block
                type(fortarray_t) :: subset
                subset = ds%variables(1)%isel("time", 1)
                
                ! Now data should be partially loaded
                if (.not. subset%initialized) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Lazy loading failed on access"
                end if
                
                call finalize_fortarray(subset)
            end block
        else
            test_passed = .false.
            write(error_unit,'(A)') "Lazy loading setup failed"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: lazy loading"
        else
            write(*,'(A)') "FAIL: lazy loading"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_lazy_loading
    
    subroutine test_memory_optimization()
        type(dataset_t) :: ds
        type(mf_options_t) :: options
        integer(int64) :: mem_before, mem_after
        logical :: test_passed = .true.
        
        ! Test: Memory optimization with chunked reading
        options%memory_limit = 100 * 1024 * 1024  ! 100 MB limit
        options%optimize_memory = .true.
        
        call get_memory_usage(mem_before)
        ds = open_mfdataset_pattern("test_mf_dataset_*.nc", options)
        call get_memory_usage(mem_after)
        
        if (ds%initialized) then
            write(*,'(A,I0,A)') "Memory usage: ", &
                (mem_after - mem_before) / 1024, " KB"
            
            ! Check that memory limit was respected
            if ((mem_after - mem_before) > options%memory_limit) then
                test_passed = .false.
                write(error_unit,'(A)') "Memory limit exceeded"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Memory optimization failed"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: memory optimization"
        else
            write(*,'(A)') "FAIL: memory optimization"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_memory_optimization
    
    subroutine test_file_pattern_matching()
        type(dataset_t) :: ds
        character(len=256), dimension(:), allocatable :: matched_files
        integer :: n_files
        logical :: test_passed = .true.
        
        ! Test: File pattern matching with various patterns
        
        ! Test glob pattern
        call match_file_pattern("test_mf_dataset_*.nc", matched_files, n_files)
        
        if (n_files /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 4 files, got: ", n_files
        end if
        
        ! Test year range pattern
        call match_file_pattern("test_mf_dataset_202[12].nc", matched_files, n_files)
        
        if (n_files /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 2 files for year range, got: ", n_files
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: file pattern matching"
        else
            write(*,'(A)') "FAIL: file pattern matching"
            all_tests_passed = .false.
        end if
        
        if (allocated(matched_files)) deallocate(matched_files)
    end subroutine test_file_pattern_matching
    
    subroutine test_error_handling()
        type(dataset_t) :: ds
        type(mf_options_t) :: options
        logical :: test_passed = .true.
        character(len=256), dimension(2) :: bad_files
        
        ! Test: Error handling for missing files
        bad_files(1) = "nonexistent1.nc"
        bad_files(2) = "nonexistent2.nc"
        
        ds = open_mfdataset(bad_files)
        
        if (ds%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Should have failed for nonexistent files"
        end if
        
        ! Test: Error handling for incompatible dimensions
        ! Create files with different dimensions
        block
            type(fortarray_t) :: arr1, arr2
            real(real64), dimension(10, 8) :: data1
            real(real64), dimension(15, 8) :: data2
            integer :: status
            
            data1 = 1.0_real64
            data2 = 2.0_real64
            
            arr1 = new_array(data1, dim_names=[character(len=10) :: "x", "y"], name="var")
            arr2 = new_array(data2, dim_names=[character(len=10) :: "x", "y"], name="var")
            
            status = write_netcdf_variable("test_incompatible1.nc", arr1)
            status = write_netcdf_variable("test_incompatible2.nc", arr2)
            
            bad_files(1) = "test_incompatible1.nc"
            bad_files(2) = "test_incompatible2.nc"
            
            options%check_dims = .true.
            ds = open_mfdataset(bad_files, options)
            
            if (ds%initialized) then
                test_passed = .false.
                write(error_unit,'(A)') "Should have failed for incompatible dimensions"
            end if
            
            call finalize_fortarray(arr1)
            call finalize_fortarray(arr2)
            call execute_command_line("rm -f test_incompatible*.nc")
        end block
        
        if (test_passed) then
            write(*,'(A)') "PASS: error handling"
        else
            write(*,'(A)') "FAIL: error handling"
            all_tests_passed = .false.
        end if
        
    end subroutine test_error_handling
    
    subroutine test_mixed_dimensions()
        type(dataset_t) :: ds
        type(mf_options_t) :: options
        logical :: test_passed = .true.
        
        ! Test: Handle files with mixed but compatible dimensions
        ! Some files might have extra variables or dimensions
        
        ! Create test files with varying variables
        block
            type(fortarray_t) :: temp, wind
            real(real64), dimension(10, 8) :: temp_data, wind_data
            integer :: status
            
            temp_data = 300.0_real64
            wind_data = 10.0_real64
            
            temp = new_array(temp_data, dim_names=[character(len=10) :: "lon", "lat"], &
                           name="temperature")
            wind = new_array(wind_data, dim_names=[character(len=10) :: "lon", "lat"], &
                           name="wind_speed")
            
            ! First file has only temperature
            status = write_netcdf_variable("test_mixed1.nc", temp)
            
            ! Second file has both temperature and wind
            block
                type(dataset_t) :: ds_mixed
                
                allocate(ds_mixed%variables(2))
                ds_mixed%variables(1) = temp
                ds_mixed%variables(2) = wind
                ds_mixed%n_vars = 2
                allocate(ds_mixed%var_names(2))
                ds_mixed%var_names = [character(len=64) :: "temperature", "wind_speed"]
                ds_mixed%initialized = .true.
                
                status = write_netcdf("test_mixed2.nc", ds_mixed)
                call finalize_dataset(ds_mixed)
            end block
            
            ! Open with compat option
            options%compat = "override"  ! Use superset of all data variables
            ds = open_mfdataset_pattern("test_mixed*.nc", options)
            
            if (ds%initialized) then
                ! Should have union of all variables
                if (ds%n_vars < 2) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Missing variables in mixed dimension handling"
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "Mixed dimension handling failed"
            end if
            
            call finalize_fortarray(temp)
            call finalize_fortarray(wind)
            call finalize_dataset(ds)
            call execute_command_line("rm -f test_mixed*.nc")
        end block
        
        if (test_passed) then
            write(*,'(A)') "PASS: mixed dimensions"
        else
            write(*,'(A)') "FAIL: mixed dimensions"
            all_tests_passed = .false.
        end if
        
    end subroutine test_mixed_dimensions
    
    subroutine get_memory_usage(mem_kb)
        integer(int64), intent(out) :: mem_kb
        character(len=256) :: line
        integer :: unit, iostat
        
        ! Try to read from /proc/self/status (Linux)
        mem_kb = 0
        open(newunit=unit, file='/proc/self/status', status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read(unit, '(A)', iostat=iostat) line
                if (iostat /= 0) exit
                if (index(line, 'VmRSS:') == 1) then
                    read(line(7:), *) mem_kb
                    exit
                end if
            end do
            close(unit)
        else
            ! Fallback: just return a dummy value
            mem_kb = 1024
        end if
    end subroutine get_memory_usage
    
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
    
end program test_multi_file_operations