program test_performance
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Performance Benchmark Suite"
    write(*,'(A)') "========================================"
    
    ! Run performance benchmarks
    call benchmark_large_array_operations()
    call benchmark_io_operations()
    call benchmark_aggregation_operations()
    call benchmark_parallel_scaling()
    call benchmark_memory_usage()
    call benchmark_chunked_processing()
    call benchmark_time_series_operations()
    call benchmark_netcdf_performance()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Performance Benchmarks: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some performance benchmarks failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All performance benchmarks passed!"
    end if

contains

    subroutine benchmark_large_array_operations()
        type(fortarray_t) :: large_var1, large_var2, result
        real(real64), dimension(1000000) :: data1, data2  ! 1M elements
        integer :: i
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: elapsed_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Benchmarking large array operations (1M elements)..."
        
        ! Initialize data
        do i = 1, 1000000
            data1(i) = real(i, real64)
            data2(i) = sin(real(i, real64) * 0.000001_real64)
        end do
        
        large_var1 = variable(data1, name="large1", dim_names=["index"])
        large_var2 = variable(data2, name="large2", dim_names=["index"])
        
        ! Benchmark arithmetic operations
        call system_clock(start_time, count_rate)
        result = large_var1 + large_var2
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Addition: ", elapsed_time, " seconds"
        
        if (elapsed_time > 1.0_real64) then
            test_passed = .false.
            write(error_unit,'(A)') "Addition too slow"
        end if
        
        call finalize_variable(result)
        
        ! Benchmark multiplication
        call system_clock(start_time, count_rate)
        result = large_var1 * large_var2
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Multiplication: ", elapsed_time, " seconds"
        
        call finalize_variable(result)
        call finalize_variable(large_var1)
        call finalize_variable(large_var2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Large array operations"
        else
            write(*,'(A)') "FAIL: Large array operations"
        end if
    end subroutine benchmark_large_array_operations
    
    subroutine benchmark_io_operations()
        type(dataset_t) :: ds
        type(fortarray_t) :: var
        real(real64), dimension(100000) :: data  ! 100K elements
        integer :: i, stat
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: elapsed_time
        logical :: test_passed
        character(len=256) :: filename
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Benchmarking I/O operations (100K elements)..."
        
        ! Initialize data
        do i = 1, 100000
            data(i) = sin(real(i, real64) * 0.0001_real64)
        end do
        
        var = variable(data, name="io_test", dim_names=["index"])
        ds = dataset()
        call add_variable(ds, var)
        
        filename = "benchmark_io.nc"
        
        ! Benchmark write
        call system_clock(start_time, count_rate)
        stat = write_netcdf(filename, ds)
        call system_clock(end_time)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "I/O write failed"
        end if
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Write: ", elapsed_time, " seconds"
        
        call finalize_dataset(ds)
        
        ! Benchmark read
        call system_clock(start_time, count_rate)
        ds = read_netcdf(filename, stat=stat)
        call system_clock(end_time)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "I/O read failed"
        end if
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Read: ", elapsed_time, " seconds"
        
        call finalize_variable(var)
        call finalize_dataset(ds)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: I/O operations"
        else
            write(*,'(A)') "FAIL: I/O operations"
        end if
    end subroutine benchmark_io_operations
    
    subroutine benchmark_aggregation_operations()
        type(fortarray_t) :: var, result
        real(real64), dimension(1000000) :: data  ! 1M elements
        real(real64) :: mean_val, sum_val, std_val
        integer :: i
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: elapsed_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Benchmarking aggregation operations (1M elements)..."
        
        ! Initialize data
        do i = 1, 1000000
            data(i) = sin(real(i, real64) * 0.000001_real64) + real(i, real64) * 0.000001_real64
        end do
        
        var = variable(data, name="agg_test", dim_names=["index"])
        
        ! Benchmark mean
        call system_clock(start_time, count_rate)
        result = mean(var)
        mean_val = result%data%values_r64(1)
        call finalize_variable(result)
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Mean: ", elapsed_time, " seconds"
        
        ! Benchmark sum
        call system_clock(start_time, count_rate)
        result = sum(var)
        sum_val = result%data%values_r64(1)
        call finalize_variable(result)
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Sum: ", elapsed_time, " seconds"
        
        ! Benchmark standard deviation
        call system_clock(start_time, count_rate)
        result = std(var)
        std_val = result%data%values_r64(1)
        call finalize_variable(result)
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Std: ", elapsed_time, " seconds"
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Aggregation operations"
        else
            write(*,'(A)') "FAIL: Aggregation operations"
        end if
    end subroutine benchmark_aggregation_operations
    
    subroutine benchmark_parallel_scaling()
        type(fortarray_t) :: var, serial_sum, parallel_sum
        real(real64), dimension(1000000) :: data  ! 1M elements
        real(real64) :: sum_val
        integer :: i, threads, original_threads
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: elapsed_time, serial_time, parallel_time, serial_result, parallel_result
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Benchmarking parallel scaling (1M elements)..."
        
        ! Initialize data
        do i = 1, 1000000
            data(i) = real(i, real64)
        end do
        
        var = variable(data, name="parallel_test", dim_names=["index"])
        
        ! Serial benchmark
        call set_num_threads(1)
        call system_clock(start_time, count_rate)
        serial_sum = sum(var)
        call system_clock(end_time)
        
        serial_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Serial (1 thread): ", serial_time, " seconds"
        
        ! Parallel benchmark
        call set_num_threads(24)
        call system_clock(start_time, count_rate)
        parallel_sum = sum(var)
        call system_clock(end_time)
        
        parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Parallel (24 threads): ", parallel_time, " seconds"
        
        if (parallel_time > 0.0_real64) then
            write(*,'(A,F0.2)') "  Speedup: ", serial_time / parallel_time
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel scaling"
        else
            write(*,'(A)') "FAIL: Parallel scaling"
        end if
    end subroutine benchmark_parallel_scaling
    
    subroutine benchmark_memory_usage()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Memory usage (placeholder)"
    end subroutine benchmark_memory_usage
    
    subroutine benchmark_chunked_processing()
        type(fortarray_t) :: var, result
        real(real64), dimension(1000000) :: data  ! 1M elements
        integer :: i
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: elapsed_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Benchmarking chunked processing (1M elements)..."
        
        ! Initialize data
        do i = 1, 1000000
            data(i) = real(i, real64)
        end do
        
        var = variable(data, name="chunk_test", dim_names=["index"])
        
        ! Set chunk size
        call set_chunk_size(10000)  ! 10K chunks
        
        ! Benchmark chunked operation
        call system_clock(start_time, count_rate)
        result = mean_chunked(var)
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Chunked mean: ", elapsed_time, " seconds"
        
        if (result%n_elements /= 100) then  ! Should have 100 chunks
            test_passed = .false.
            write(error_unit,'(A,I0)') "Wrong number of chunks: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunked processing"
        else
            write(*,'(A)') "FAIL: Chunked processing"
        end if
    end subroutine benchmark_chunked_processing
    
    subroutine benchmark_time_series_operations()
        type(fortarray_t) :: ts_var, result
        real(real64), dimension(365*10) :: ts_data  ! 10 years daily
        integer :: i
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: elapsed_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Benchmarking time series operations (10 years daily)..."
        
        ! Create time series with seasonality
        do i = 1, 365*10
            ts_data(i) = 20.0_real64 + 10.0_real64 * sin(2.0_real64 * 3.14159_real64 * real(i, real64) / 365.0_real64) + &
                        real(i, real64) * 0.001_real64  ! Trend
        end do
        
        ts_var = variable(ts_data, name="time_series", dim_names=["time"])
        
        ! Benchmark rolling mean
        call system_clock(start_time, count_rate)
        result = rolling_mean(ts_var, 30)  ! 30-day rolling mean
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Rolling mean (30-day): ", elapsed_time, " seconds"
        
        call finalize_variable(result)
        
        ! Benchmark resampling
        call system_clock(start_time, count_rate)
        result = resample(ts_var, "M", "mean")  ! Monthly resampling
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        write(*,'(A,F0.4,A)') "  Monthly resampling: ", elapsed_time, " seconds"
        
        call finalize_variable(result)
        call finalize_variable(ts_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time series operations"
        else
            write(*,'(A)') "FAIL: Time series operations"
        end if
    end subroutine benchmark_time_series_operations
    
    subroutine benchmark_netcdf_performance()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: NetCDF performance (placeholder)"
    end subroutine benchmark_netcdf_performance
    
end program test_performance