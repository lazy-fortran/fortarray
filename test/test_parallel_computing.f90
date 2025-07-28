program test_parallel_computing
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    !$ use omp_lib
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================="
    write(*,'(A)') "Running Parallel Computing Test Suite"
    write(*,'(A)') "========================================="
    
    ! Run all parallel computing tests
    call test_openmp_initialization()
    call test_thread_safe_operations()
    call test_parallel_arithmetic()
    call test_parallel_aggregations()
    call test_parallel_apply_functions()
    call test_load_balancing()
    call test_reduction_operations()
    call test_nested_parallelism()
    call test_thread_local_storage()
    call test_parallel_io()
    call test_parallel_broadcasting()
    call test_parallel_missing_data()
    call test_parallel_coordinate_selection()
    call test_parallel_performance()
    call test_parallel_error_handling()
    
    ! Summary
    write(*,'(A)') "========================================="
    write(*,'(A,I0,A,I0,A)') "Parallel Computing Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some parallel computing tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All parallel computing tests passed!"
    end if

contains

    subroutine test_openmp_initialization()
        integer :: num_threads
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        !$ num_threads = omp_get_max_threads()
        !$ if (num_threads < 1) then
        !$     test_passed = .false.
        !$     write(error_unit,'(A,I0)') "Invalid number of threads: ", num_threads
        !$ else
        !$     write(*,'(A,I0)') "OpenMP initialized with max threads: ", num_threads
        !$ end if
        
        ! Test thread ID retrieval
        !$omp parallel
        !$ if (omp_get_thread_num() < 0) then
        !$     test_passed = .false.
        !$     write(error_unit,'(A)') "Invalid thread ID"
        !$ end if
        !$omp end parallel
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: OpenMP initialization"
        else
            write(*,'(A)') "FAIL: OpenMP initialization"
        end if
    end subroutine test_openmp_initialization
    
    subroutine test_thread_safe_operations()
        type(fortarray_t) :: var, result
        real(real64), dimension(1000) :: data
        integer :: i
        logical :: test_passed
        real(real64) :: sum_seq, sum_par
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 1000
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Sequential sum for reference
        sum_seq = sum(data)
        
        ! Parallel sum with thread-safe accumulation
        sum_par = 0.0_real64
        !$omp parallel do reduction(+:sum_par)
        do i = 1, 1000
            sum_par = sum_par + var%data%values_r64(i)
        end do
        !$omp end parallel do
        
        ! Check results match
        if (abs(sum_seq - sum_par) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,2F15.3)') "Sum mismatch: sequential =", sum_seq, ", parallel =", sum_par
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Thread-safe operations"
        else
            write(*,'(A)') "FAIL: Thread-safe operations"
        end if
    end subroutine test_thread_safe_operations
    
    subroutine test_parallel_arithmetic()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(10000) :: data1, data2
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        !$omp parallel do
        do i = 1, 10000
            data1(i) = real(i, real64)
            data2(i) = real(i * 2, real64)
        end do
        !$omp end parallel do
        
        var1 = new_array(data1, name="var1", dim_names=["x"])
        var2 = new_array(data2, name="var2", dim_names=["x"])
        
        ! Parallel addition
        result = add_parallel(var1, var2)
        
        ! Verify results
        !$omp parallel do private(i) shared(test_passed)
        do i = 1, 10000
            if (abs(result%data%values_r64(i) - (data1(i) + data2(i))) > 1e-10) then
                !$omp critical
                test_passed = .false.
                write(error_unit,'(A,I0)') "Addition error at index: ", i
                !$omp end critical
            end if
        end do
        !$omp end parallel do
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel arithmetic"
        else
            write(*,'(A)') "FAIL: Parallel arithmetic"
        end if
    end subroutine test_parallel_arithmetic
    
    subroutine test_parallel_aggregations()
        type(fortarray_t) :: var
        real(real64), dimension(100000) :: data
        real(real64) :: mean_val, min_val, max_val
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 100000
            data(i) = sin(real(i, real64) * 0.01_real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Parallel mean
        mean_val = mean_parallel(var)
        
        ! Parallel min/max
        min_val = min_parallel(var)
        max_val = max_parallel(var)
        
        ! Verify against sequential results
        if (abs(mean_val - sum(data)/size(data)) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean calculation error"
        end if
        
        if (abs(min_val - minval(data)) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Min calculation error"
        end if
        
        if (abs(max_val - maxval(data)) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Max calculation error"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel aggregations"
        else
            write(*,'(A)') "FAIL: Parallel aggregations"
        end if
    end subroutine test_parallel_aggregations
    
    subroutine test_parallel_apply_functions()
        type(fortarray_t) :: var, result
        real(real64), dimension(50000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 50000
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Apply function in parallel
        result = apply_parallel(var, sqrt_function)
        
        ! Verify results
        !$omp parallel do private(i) shared(test_passed)
        do i = 1, 50000
            if (abs(result%data%values_r64(i) - sqrt(data(i))) > 1e-10) then
                !$omp critical
                test_passed = .false.
                !$omp end critical
            end if
        end do
        !$omp end parallel do
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel apply functions"
        else
            write(*,'(A)') "FAIL: Parallel apply functions"
        end if
    end subroutine test_parallel_apply_functions
    
    subroutine test_load_balancing()
        type(fortarray_t) :: var, result
        real(real64), dimension(10000) :: data
        integer :: i, chunk_size
        logical :: test_passed
        real(real64) :: start_time, end_time, static_time, dynamic_time
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with varying computation
        do i = 1, 10000
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test static scheduling
        call cpu_time(start_time)
        result = apply_with_schedule(var, heavy_function, "static", 100)
        call cpu_time(end_time)
        static_time = end_time - start_time
        call finalize_variable(result)
        
        ! Test dynamic scheduling
        call cpu_time(start_time)
        result = apply_with_schedule(var, heavy_function, "dynamic", 10)
        call cpu_time(end_time)
        dynamic_time = end_time - start_time
        
        ! Dynamic should handle load imbalance better
        write(*,'(A,F8.4,A,F8.4,A)') "Static time: ", static_time, "s, Dynamic time: ", dynamic_time, "s"
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Load balancing"
        else
            write(*,'(A)') "FAIL: Load balancing"
        end if
    end subroutine test_load_balancing
    
    subroutine test_reduction_operations()
        type(fortarray_t) :: var
        real(real64), dimension(1000) :: data
        real(real64) :: sum_result, product_result
        integer :: i, count_result
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 1000
            data(i) = real(mod(i, 10) + 1, real64)  ! Values 1-10
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test sum reduction
        sum_result = reduce_parallel(var, "sum")
        if (abs(sum_result - sum(data)) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum reduction error"
        end if
        
        ! Test count reduction (count values > 5)
        count_result = count_parallel(var, 5.0_real64)
        if (count_result /= count(data > 5.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0)') "Count reduction error: expected ", &
                count(data > 5.0_real64), ", got ", count_result
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Reduction operations"
        else
            write(*,'(A)') "FAIL: Reduction operations"
        end if
    end subroutine test_reduction_operations
    
    subroutine test_nested_parallelism()
        type(fortarray_t) :: var
        real(real64), dimension(100, 100) :: data2d
        real(real64), dimension(10000) :: data1d
        integer :: i, j, idx
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D test data
        idx = 0
        do j = 1, 100
            do i = 1, 100
                idx = idx + 1
                data2d(i, j) = real(idx, real64)
                data1d(idx) = data2d(i, j)
            end do
        end do
        var = new_array(data1d, name="test_data", dim_names=["idx"])
        
        ! Test nested parallel regions
        !$omp parallel num_threads(2)
        !$omp single
        !$omp task
        call process_chunk(var, 1, 5000)
        !$omp end task
        !$omp task
        call process_chunk(var, 5001, 10000)
        !$omp end task
        !$omp end single
        !$omp end parallel
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Nested parallelism"
        else
            write(*,'(A)') "FAIL: Nested parallelism"
        end if
    end subroutine test_nested_parallelism
    
    subroutine test_thread_local_storage()
        type(fortarray_t) :: var
        real(real64), dimension(1000) :: data
        real(real64), dimension(:), allocatable :: thread_sums
        integer :: i, tid, num_threads
        logical :: test_passed
        real(real64) :: total_sum
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 1000
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Get number of threads
        !$ num_threads = omp_get_max_threads()
        if (num_threads == 0) num_threads = 1
        allocate(thread_sums(0:num_threads-1))
        thread_sums = 0.0_real64
        
        ! Each thread computes partial sum
        !$omp parallel private(i, tid) shared(thread_sums, var)
        !$ tid = omp_get_thread_num()
        if (tid < 0) tid = 0
        
        !$omp do
        do i = 1, var%n_elements
            thread_sums(tid) = thread_sums(tid) + var%data%values_r64(i)
        end do
        !$omp end do
        !$omp end parallel
        
        ! Combine thread-local results
        total_sum = sum(thread_sums)
        
        if (abs(total_sum - sum(data)) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Thread-local storage sum mismatch"
        end if
        
        deallocate(thread_sums)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Thread local storage"
        else
            write(*,'(A)') "FAIL: Thread local storage"
        end if
    end subroutine test_thread_local_storage
    
    subroutine test_parallel_io()
        type(dataset_t) :: ds
        type(fortarray_t) :: var, var_read
        real(real64), dimension(10000) :: data
        character(len=256) :: filename
        integer :: i, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        !$omp parallel do
        do i = 1, 10000
            data(i) = real(i, real64)
        end do
        !$omp end parallel do
        
        var = new_array(data, name="test_var", dim_names=["x"])
        
        ! Create dataset
        ds = new_dataset()
        call add_variable(ds, var)
        
        ! Write with parallel-aware settings
        filename = "test_parallel_io.nc"
        stat = write_netcdf_parallel(filename, ds)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write parallel NetCDF"
        else
            ! Read back
            ds = read_netcdf(filename)
            var_read = get_variable(ds, "test_var")
            
            ! Verify data
            !$omp parallel do private(i) shared(test_passed)
            do i = 1, 10000
                if (abs(var_read%data%values_r64(i) - data(i)) > 1e-10) then
                    !$omp critical
                    test_passed = .false.
                    !$omp end critical
                end if
            end do
            !$omp end parallel do
            
            call finalize_variable(var_read)
        end if
        
        ! Clean up
        call delete_file(filename)
        call finalize_variable(var)
        call finalize_dataset(ds)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel I/O"
        else
            write(*,'(A)') "FAIL: Parallel I/O"
        end if
    end subroutine test_parallel_io
    
    subroutine test_parallel_broadcasting()
        type(fortarray_t) :: var_small, var_large, result
        real(real64), dimension(10) :: small_data
        real(real64), dimension(10000) :: large_data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        small_data = [(real(i, real64), i=1,10)]
        var_small = new_array(small_data, name="small", dim_names=["x"])
        
        !$omp parallel do
        do i = 1, 10000
            large_data(i) = real(i, real64)
        end do
        !$omp end parallel do
        var_large = new_array(large_data, name="large", dim_names=["x"])
        
        ! Broadcast and add in parallel
        result = broadcast_add_parallel(var_small, var_large)
        
        ! Verify - each element should be large_data(i) + small_data(mod(i-1,10)+1)
        !$omp parallel do private(i) shared(test_passed)
        do i = 1, 10000
            if (abs(result%data%values_r64(i) - &
                (large_data(i) + small_data(mod(i-1,10)+1))) > 1e-10) then
                !$omp critical
                test_passed = .false.
                !$omp end critical
            end if
        end do
        !$omp end parallel do
        
        call finalize_variable(var_small)
        call finalize_variable(var_large)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel broadcasting"
        else
            write(*,'(A)') "FAIL: Parallel broadcasting"
        end if
    end subroutine test_parallel_broadcasting
    
    subroutine test_parallel_missing_data()
        type(fortarray_t) :: var, result
        real(real64), dimension(1000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with NaN values
        do i = 1, 1000
            if (mod(i, 7) == 0) then
                data(i) = ieee_value(1.0_real64, ieee_quiet_nan)
            else
                data(i) = real(i, real64)
            end if
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Fill missing values in parallel
        result = fillna_parallel(var, 0.0_real64)
        
        ! Verify
        !$omp parallel do private(i) shared(test_passed)
        do i = 1, 1000
            if (mod(i, 7) == 0) then
                if (result%data%values_r64(i) /= 0.0_real64) then
                    !$omp critical
                    test_passed = .false.
                    !$omp end critical
                end if
            else
                if (abs(result%data%values_r64(i) - real(i, real64)) > 1e-10) then
                    !$omp critical
                    test_passed = .false.
                    !$omp end critical
                end if
            end if
        end do
        !$omp end parallel do
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel missing data"
        else
            write(*,'(A)') "FAIL: Parallel missing data"
        end if
    end subroutine test_parallel_missing_data
    
    subroutine test_parallel_coordinate_selection()
        type(fortarray_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(10000) :: data, coord_vals
        integer :: i, n_selected, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with coordinates
        !$omp parallel do
        do i = 1, 10000
            data(i) = real(i, real64)
            coord_vals(i) = real(i, real64) * 0.1_real64
        end do
        !$omp end parallel do
        
        var = new_array(data, name="test_data", dim_names=["x"])
        call create_coordinate(x_coord, 10000, "real64", stat)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        
        allocate(var%coords(1))
        allocate(var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Select range in parallel
        result = select_range_parallel(var, "x", 100.0_real64, 500.0_real64)
        
        ! Count selected elements
        n_selected = 0
        do i = 1, 10000
            if (coord_vals(i) >= 100.0_real64 .and. coord_vals(i) <= 500.0_real64) then
                n_selected = n_selected + 1
            end if
        end do
        
        if (result%n_elements /= n_selected) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0)') "Expected ", n_selected, " elements, got ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel coordinate selection"
        else
            write(*,'(A)') "FAIL: Parallel coordinate selection"
        end if
    end subroutine test_parallel_coordinate_selection
    
    subroutine test_parallel_performance()
        type(fortarray_t) :: var, result
        real(real64), dimension(1000000) :: data
        real(real64) :: start_time, end_time, seq_time, par_time
        real(real64) :: speedup, efficiency
        integer :: i, num_threads
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Get number of threads
        !$ num_threads = omp_get_max_threads()
        if (num_threads == 0) num_threads = 1
        
        ! Create large test data
        do i = 1, 1000000
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Time sequential operation
        call cpu_time(start_time)
        ! Apply function sequentially for baseline  
        call disable_parallel()
        result = apply_function(var, expensive_function)
        call enable_parallel()
        call cpu_time(end_time)
        seq_time = end_time - start_time
        call finalize_variable(result)
        
        ! Time parallel operation
        call cpu_time(start_time)
        result = apply_parallel(var, expensive_function)
        call cpu_time(end_time)
        par_time = end_time - start_time
        
        ! Calculate performance metrics
        if (par_time > 0.0_real64) then
            speedup = seq_time / par_time
            efficiency = speedup / real(num_threads, real64)
            
            write(*,'(A,I0)') "Number of threads: ", num_threads
            write(*,'(A,F8.4,A)') "Sequential time: ", seq_time, " seconds"
            write(*,'(A,F8.4,A)') "Parallel time: ", par_time, " seconds"
            write(*,'(A,F8.4)') "Speedup: ", speedup
            write(*,'(A,F8.2,A)') "Efficiency: ", efficiency * 100.0_real64, "%"
            
            ! Expect at least some speedup with multiple threads
            if (num_threads > 1 .and. speedup < 1.2_real64) then
                test_passed = .false.
                write(error_unit,'(A)') "Insufficient speedup"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Invalid timing"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel performance"
        else
            write(*,'(A)') "FAIL: Parallel performance"
        end if
    end subroutine test_parallel_performance
    
    subroutine test_parallel_error_handling()
        type(fortarray_t) :: var
        real(real64), dimension(100) :: data
        integer :: i, error_count
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 100
            data(i) = real(i - 50, real64)  ! Some negative values
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test error handling in parallel region
        error_count = 0
        
        !$omp parallel private(i) shared(error_count)
        !$omp do
        do i = 1, 100
            if (var%data%values_r64(i) < 0.0_real64) then
                ! Simulate error condition
                !$omp atomic
                error_count = error_count + 1
            end if
        end do
        !$omp end do
        !$omp end parallel
        
        ! Verify error count
        if (error_count /= count(data < 0.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A)') "Error count mismatch in parallel region"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel error handling"
        else
            write(*,'(A)') "FAIL: Parallel error handling"
        end if
    end subroutine test_parallel_error_handling
    
    ! Helper functions
    
    function sqrt_function(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y
        y = sqrt(x)
    end function sqrt_function
    
    function heavy_function(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y
        integer :: i
        y = x
        ! Simulate variable computation load
        do i = 1, int(x) / 100
            y = sin(y) + cos(y)
        end do
    end function heavy_function
    
    function expensive_function(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y
        integer :: i
        y = x
        ! Expensive computation
        do i = 1, 100
            y = sqrt(abs(sin(y) * cos(y) + 1.0_real64))
        end do
    end function expensive_function
    
    subroutine process_chunk(var, start_idx, end_idx)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, end_idx
        integer :: i
        real(real64) :: sum
        
        sum = 0.0_real64
        !$omp parallel do reduction(+:sum)
        do i = start_idx, end_idx
            sum = sum + var%data%values_r64(i)
        end do
        !$omp end parallel do
    end subroutine process_chunk
    
end program test_parallel_computing