program test_chunked_operations
    use foxel
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================="
    write(*,'(A)') "Running Chunked Operations Test Suite"
    write(*,'(A)') "========================================="
    
    ! Run all chunked operations tests
    call test_chunk_definition()
    call test_chunk_iteration()
    call test_chunk_processing()
    call test_out_of_core_operations()
    call test_chunked_aggregations()
    call test_chunked_arithmetic()
    call test_chunk_boundary_handling()
    call test_multidimensional_chunking()
    call test_adaptive_chunk_sizing()
    call test_chunk_caching()
    call test_parallel_chunk_processing()
    call test_chunk_memory_management()
    call test_chunked_io_operations()
    call test_chunk_performance()
    call test_chunk_error_handling()
    
    ! Summary
    write(*,'(A)') "========================================="
    write(*,'(A,I0,A,I0,A)') "Chunked Operations Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some chunked operations tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All chunked operations tests passed!"
    end if

contains

    subroutine test_chunk_definition()
        type(variable_t) :: var
        type(chunk_info_t) :: chunk_info
        real(real64), dimension(10000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create large dataset
        do i = 1, 10000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="large_data", dim_names=["x"])
        
        ! Define chunk size
        chunk_info = define_chunks(var, chunk_size=1000)
        
        ! Check chunk definition
        if (chunk_info%n_chunks /= 10) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 10 chunks, got: ", chunk_info%n_chunks
        end if
        
        if (chunk_info%chunk_size /= 1000) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected chunk size 1000, got: ", chunk_info%chunk_size
        end if
        
        ! Check chunk shape for multidimensional
        if (size(chunk_info%chunk_shape) /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Chunk shape should have 1 dimension"
        end if
        
        call finalize_variable(var)
        call finalize_chunk_info(chunk_info)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunk definition"
        else
            write(*,'(A)') "FAIL: Chunk definition"
        end if
    end subroutine test_chunk_definition
    
    subroutine test_chunk_iteration()
        type(variable_t) :: var
        type(chunk_iterator_t) :: iterator
        type(variable_t) :: chunk
        real(real64), dimension(100) :: data
        integer :: i, chunk_count
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 100
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Create chunk iterator
        iterator = create_chunk_iterator(var, chunk_size=25)
        
        ! Iterate over chunks
        chunk_count = 0
        do while (has_next_chunk(iterator))
            chunk = get_next_chunk(iterator)
            chunk_count = chunk_count + 1
            
            ! Verify chunk size
            if (chunk_count < 4 .and. chunk%n_elements /= 25) then
                test_passed = .false.
                write(error_unit,'(A,I0,A,I0)') "Chunk ", chunk_count, " has wrong size: ", chunk%n_elements
            else if (chunk_count == 4 .and. chunk%n_elements /= 25) then
                test_passed = .false.
                write(error_unit,'(A)') "Last chunk has wrong size"
            end if
            
            call finalize_variable(chunk)
        end do
        
        if (chunk_count /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 4 chunks, got: ", chunk_count
        end if
        
        call finalize_variable(var)
        call finalize_chunk_iterator(iterator)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunk iteration"
        else
            write(*,'(A)') "FAIL: Chunk iteration"
        end if
    end subroutine test_chunk_iteration
    
    subroutine test_chunk_processing()
        type(variable_t) :: var, result
        type(chunked_operation_t) :: op
        real(real64), dimension(1000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 1000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Define chunked operation (square each element)
        op = create_chunked_operation("square", chunk_size=100)
        
        ! Process in chunks
        result = apply_chunked(op, var, square_function)
        
        ! Verify result
        if (result%n_elements /= 1000) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result has wrong size: ", result%n_elements
        else
            ! Check first and last values
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "First value incorrect"
            end if
            if (abs(result%data%values_r64(1000) - 1000000.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Last value incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        call finalize_chunked_operation(op)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunk processing"
        else
            write(*,'(A)') "FAIL: Chunk processing"
        end if
    end subroutine test_chunk_processing
    
    subroutine test_out_of_core_operations()
        type(variable_t) :: var, result
        type(out_of_core_config_t) :: config
        real(real64), dimension(10000) :: data
        character(len=256) :: temp_file
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create large dataset
        do i = 1, 10000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="large_data", dim_names=["x"])
        
        ! Configure out-of-core processing
        temp_file = "test_chunked_temp.nc"
        config = create_out_of_core_config(temp_file, memory_limit=1000_int64)
        
        ! Process out-of-core (compute mean)
        result = compute_out_of_core(var, "mean", config)
        
        ! Verify result
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean should be scalar"
        else
            ! Mean of 1..10000 is 5000.5
            if (abs(result%data%values_r64(1) - 5000.5_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A,F0.1)') "Mean value incorrect: ", result%data%values_r64(1)
            end if
        end if
        
        ! Clean up temp file
        call delete_temp_file(temp_file)
        
        call finalize_variable(var)
        call finalize_variable(result)
        call finalize_out_of_core_config(config)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Out-of-core operations"
        else
            write(*,'(A)') "FAIL: Out-of-core operations"
        end if
    end subroutine test_out_of_core_operations
    
    subroutine test_chunked_aggregations()
        type(variable_t) :: var, result
        real(real64), dimension(5000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 5000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test chunked sum
        result = sum_chunked(var, chunk_size=500)
        
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum should be scalar"
        else
            ! Sum of 1..5000 = 5000 * 5001 / 2 = 12502500
            if (abs(result%data%values_r64(1) - 12502500.0_real64) > 1e-6) then
                test_passed = .false.
                write(error_unit,'(A,F0.1)') "Sum value incorrect: ", result%data%values_r64(1)
            end if
        end if
        
        call finalize_variable(result)
        
        ! Test chunked mean
        result = mean_chunked(var, chunk_size=500)
        
        if (abs(result%data%values_r64(1) - 2500.5_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F0.1)') "Mean value incorrect: ", result%data%values_r64(1)
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunked aggregations"
        else
            write(*,'(A)') "FAIL: Chunked aggregations"
        end if
    end subroutine test_chunked_aggregations
    
    subroutine test_chunked_arithmetic()
        type(variable_t) :: var1, var2, result
        real(real64), dimension(1000) :: data1, data2
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 1000
            data1(i) = real(i, real64)
            data2(i) = real(i * 2, real64)
        end do
        var1 = variable(data1, name="var1", dim_names=["x"])
        var2 = variable(data2, name="var2", dim_names=["x"])
        
        ! Test chunked addition
        result = add_chunked(var1, var2, chunk_size=100)
        
        if (result%n_elements /= 1000) then
            test_passed = .false.
            write(error_unit,'(A)') "Result has wrong size"
        else
            ! Check some values: var1[i] + var2[i] = i + 2*i = 3*i
            if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "First value incorrect"
            end if
            if (abs(result%data%values_r64(500) - 1500.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Middle value incorrect"
            end if
            if (abs(result%data%values_r64(1000) - 3000.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Last value incorrect"
            end if
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunked arithmetic"
        else
            write(*,'(A)') "FAIL: Chunked arithmetic"
        end if
    end subroutine test_chunked_arithmetic
    
    subroutine test_chunk_boundary_handling()
        type(variable_t) :: var, result
        real(real64), dimension(97) :: data  ! Prime number size
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with non-divisible size
        do i = 1, 97
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Process with chunk size that doesn't divide evenly
        result = multiply_chunked(var, 2.0_real64, chunk_size=10)
        
        if (result%n_elements /= 97) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result has wrong size: ", result%n_elements
        else
            ! Check last element
            if (abs(result%data%values_r64(97) - 194.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Last element incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunk boundary handling"
        else
            write(*,'(A)') "FAIL: Chunk boundary handling"
        end if
    end subroutine test_chunk_boundary_handling
    
    subroutine test_multidimensional_chunking()
        type(variable_t) :: var, result
        type(chunk_info_t) :: chunk_info
        real(real64), dimension(100, 100) :: data
        integer :: i, j
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D test data
        do j = 1, 100
            do i = 1, 100
                data(i, j) = real(i + (j-1)*100, real64)
            end do
        end do
        var = variable(data, name="test_2d", dim_names=["x", "y"])
        
        ! Define 2D chunks
        chunk_info = define_chunks(var, chunk_shape=[25, 25])
        
        ! Check chunk definition
        if (chunk_info%n_chunks /= 16) then  ! 4x4 chunks
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 16 chunks, got: ", chunk_info%n_chunks
        end if
        
        ! Process in 2D chunks
        result = apply_chunked_2d(var, chunk_info, sqrt_function)
        
        if (result%n_elements /= 10000) then
            test_passed = .false.
            write(error_unit,'(A)') "Result has wrong size"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        call finalize_chunk_info(chunk_info)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Multidimensional chunking"
        else
            write(*,'(A)') "FAIL: Multidimensional chunking"
        end if
    end subroutine test_multidimensional_chunking
    
    subroutine test_adaptive_chunk_sizing()
        type(variable_t) :: var
        type(adaptive_chunk_config_t) :: config
        integer :: optimal_size
        real(real64), dimension(10000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 10000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Configure adaptive chunking
        config = create_adaptive_config(min_size=100, max_size=2000, &
                                      target_memory=8000_int64)  ! bytes
        
        ! Determine optimal chunk size
        optimal_size = determine_optimal_chunk_size(var, config)
        
        ! Check that size is within bounds
        if (optimal_size < 100 .or. optimal_size > 2000) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Optimal size out of bounds: ", optimal_size
        end if
        
        ! For real64 data, 8000 bytes = 1000 elements
        if (optimal_size /= 1000) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected optimal size 1000, got: ", optimal_size
        end if
        
        call finalize_variable(var)
        call finalize_adaptive_config(config)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Adaptive chunk sizing"
        else
            write(*,'(A)') "FAIL: Adaptive chunk sizing"
        end if
    end subroutine test_adaptive_chunk_sizing
    
    subroutine test_chunk_caching()
        type(variable_t) :: var
        type(chunk_cache_t) :: cache
        type(variable_t) :: chunk1, chunk2
        real(real64), dimension(1000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 1000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Create chunk cache
        cache = create_chunk_cache(max_chunks=4, chunk_size=250)
        
        ! Get chunk (should cache it)
        chunk1 = get_cached_chunk(cache, var, chunk_id=1)
        
        ! Get same chunk again (should come from cache)
        chunk2 = get_cached_chunk(cache, var, chunk_id=1)
        
        ! Verify cache hit
        if (.not. cache%last_was_hit) then
            test_passed = .false.
            write(error_unit,'(A)') "Second access should be cache hit"
        end if
        
        ! Verify chunks are identical
        if (chunk1%n_elements /= chunk2%n_elements) then
            test_passed = .false.
            write(error_unit,'(A)') "Cached chunks have different sizes"
        end if
        
        call finalize_variable(var)
        call finalize_variable(chunk1)
        call finalize_variable(chunk2)
        call finalize_chunk_cache(cache)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunk caching"
        else
            write(*,'(A)') "FAIL: Chunk caching"
        end if
    end subroutine test_chunk_caching
    
    subroutine test_parallel_chunk_processing()
        type(variable_t) :: var, result
        type(parallel_chunk_config_t) :: config
        real(real64), dimension(4000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 4000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Configure parallel processing
        config = create_parallel_config(n_threads=4, chunk_size=1000)
        
        ! Process chunks in parallel
        result = process_parallel_chunks(var, config, double_function)
        
        ! Verify result
        if (result%n_elements /= 4000) then
            test_passed = .false.
            write(error_unit,'(A)') "Result has wrong size"
        else
            ! Check some values
            if (abs(result%data%values_r64(1) - 2.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "First value incorrect"
            end if
            if (abs(result%data%values_r64(4000) - 8000.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Last value incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        call finalize_parallel_config(config)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel chunk processing"
        else
            write(*,'(A)') "FAIL: Parallel chunk processing"
        end if
    end subroutine test_parallel_chunk_processing
    
    subroutine test_chunk_memory_management()
        type(variable_t) :: var
        type(memory_monitor_t) :: monitor
        integer :: initial_memory, peak_memory
        real(real64), dimension(10000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 10000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Start memory monitoring
        monitor = create_memory_monitor()
        call start_monitoring(monitor)
        
        ! Process with small chunks (should use less memory)
        call process_with_chunks(var, chunk_size=100)
        
        ! Get memory statistics
        initial_memory = get_initial_memory(monitor)
        peak_memory = get_peak_memory(monitor)
        
        ! Peak memory should not be much more than chunk size
        if (peak_memory - initial_memory > 200 * 8) then  ! 200 elements * 8 bytes
            test_passed = .false.
            write(error_unit,'(A,I0)') "Peak memory too high: ", peak_memory - initial_memory
        end if
        
        call finalize_variable(var)
        call finalize_memory_monitor(monitor)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunk memory management"
        else
            write(*,'(A)') "FAIL: Chunk memory management"
        end if
    end subroutine test_chunk_memory_management
    
    subroutine test_chunked_io_operations()
        type(variable_t) :: var, result
        type(chunked_io_config_t) :: config
        character(len=256) :: filename
        real(real64), dimension(5000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 5000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Configure chunked I/O
        filename = "test_chunked_io.nc"
        config = create_chunked_io_config(chunk_size=1000, compression=.true.)
        
        ! Write in chunks
        call write_chunked(var, filename, config)
        
        ! Read back in chunks
        result = read_chunked(filename, "test_data", config)
        
        ! Verify data
        if (result%n_elements /= 5000) then
            test_passed = .false.
            write(error_unit,'(A)') "Read data has wrong size"
        else
            ! Check some values
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "First value incorrect after read"
            end if
            if (abs(result%data%values_r64(5000) - 5000.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Last value incorrect after read"
            end if
        end if
        
        ! Clean up
        call delete_file(filename)
        
        call finalize_variable(var)
        call finalize_variable(result)
        call finalize_chunked_io_config(config)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunked I/O operations"
        else
            write(*,'(A)') "FAIL: Chunked I/O operations"
        end if
    end subroutine test_chunked_io_operations
    
    subroutine test_chunk_performance()
        type(variable_t) :: var, result1, result2
        real(real64), dimension(100000) :: data
        real(real64) :: start_time, end_time, chunked_time, regular_time
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create large test data
        do i = 1, 100000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Time regular processing
        call cpu_time(start_time)
        result1 = apply_sqrt(var)
        call cpu_time(end_time)
        regular_time = end_time - start_time
        
        ! Time chunked processing
        call cpu_time(start_time)
        result2 = sqrt_chunked(var, chunk_size=10000)
        call cpu_time(end_time)
        chunked_time = end_time - start_time
        
        ! Chunked should not be too much slower (within 2x)
        if (chunked_time > regular_time * 2.0) then
            test_passed = .false.
            write(error_unit,'(A,F0.3,A,F0.3)') "Chunked too slow: ", chunked_time, " vs ", regular_time
        end if
        
        ! Verify results are identical
        if (result1%n_elements /= result2%n_elements) then
            test_passed = .false.
            write(error_unit,'(A)') "Results have different sizes"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result1)
        call finalize_variable(result2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunk performance"
        else
            write(*,'(A)') "FAIL: Chunk performance"
        end if
    end subroutine test_chunk_performance
    
    subroutine test_chunk_error_handling()
        type(variable_t) :: var, result
        type(chunk_info_t) :: chunk_info
        real(real64), dimension(100) :: data
        integer :: i, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 100
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test invalid chunk size
        chunk_info = define_chunks(var, chunk_size=0, stat=stat)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail with chunk_size=0"
        end if
        
        ! Test chunk size larger than data
        chunk_info = define_chunks(var, chunk_size=1000, stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should succeed with large chunk size"
        else
            if (chunk_info%n_chunks /= 1) then
                test_passed = .false.
                write(error_unit,'(A)') "Should have 1 chunk for oversized chunk"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_chunk_info(chunk_info)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chunk error handling"
        else
            write(*,'(A)') "FAIL: Chunk error handling"
        end if
    end subroutine test_chunk_error_handling
    
    ! Helper functions
    
    function square_function(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y
        y = x * x
    end function square_function
    
    function double_function(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y
        y = x * 2.0_real64
    end function double_function
    
    function sqrt_function(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y
        y = sqrt(x)
    end function sqrt_function
    
    !> Apply sqrt elementwise
    function apply_sqrt(var) result(result)
        type(variable_t), intent(in) :: var
        type(variable_t) :: result
        integer :: i
        
        ! Create result variable
        result%name = trim(var%name) // "_sqrt"
        result%n_dims = var%n_dims
        result%n_elements = var%n_elements
        result%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(result%shape(size(var%shape)))
            result%shape = var%shape
        end if
        
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(size(var%dim_names)))
            result%dim_names = var%dim_names
        end if
        
        ! Initialize data storage
        result%data%dtype = var%data%dtype
        result%data%n_elements = var%n_elements
        result%data%initialized = .true.
        allocate(result%data%values_r64(var%n_elements))
        
        ! Apply sqrt
        do i = 1, var%n_elements
            result%data%values_r64(i) = sqrt(var%data%values_r64(i))
        end do
        
    end function apply_sqrt
    
end program test_chunked_operations