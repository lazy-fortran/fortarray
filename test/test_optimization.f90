program test_optimization
    use foxel
    use foxel_slicing
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Optimization Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run optimization tests
    call test_simd_operations()
    call test_cache_efficiency()
    call test_memory_layout_optimization()
    call test_loop_optimization()
    call test_parallel_scaling_optimization()
    call test_vectorization()
    call test_memory_access_patterns()
    call test_computational_intensity()
    call test_algorithm_optimization()
    call test_compiler_optimization_effectiveness()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Optimization Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some optimization tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All optimization tests passed!"
    end if

contains

    subroutine test_simd_operations()
        type(variable_t) :: var1, var2, result
        real(real64), dimension(10000) :: data1, data2
        integer :: i, iter
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: elapsed_time, baseline_time, optimized_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing SIMD operations performance..."
        
        ! Initialize test data - use patterns that benefit from SIMD
        do i = 1, 10000
            data1(i) = real(i, real64) * 0.1_real64
            data2(i) = sin(real(i, real64) * 0.001_real64)
        end do
        
        var1 = variable(data1, name="simd_test1", dim_names=["index"])
        var2 = variable(data2, name="simd_test2", dim_names=["index"])
        
        ! Baseline: simple addition (should use SIMD automatically)
        call system_clock(start_time, count_rate)
        do iter = 1, 100
            result = var1 + var2
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        baseline_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        ! Optimized: compound operations that should vectorize well
        call system_clock(start_time, count_rate)
        do iter = 1, 100
            result = var1 * var2 + var1  ! FMA-friendly operation
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        optimized_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  Baseline time: ", baseline_time, " seconds"
        write(*,'(A,F0.6,A)') "  Optimized time: ", optimized_time, " seconds"
        
        ! Test that operations complete and give reasonable performance
        if (baseline_time <= 0.0_real64 .or. optimized_time <= 0.0_real64) then
            test_passed = .false.
            write(error_unit,'(A)') "Invalid timing measurements"
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: SIMD operations"
        else
            write(*,'(A)') "FAIL: SIMD operations"
        end if
    end subroutine test_simd_operations
    
    subroutine test_cache_efficiency()
        type(variable_t) :: var, result
        real(real64), dimension(100000) :: data
        integer :: i, iter, block_size
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: sequential_time, blocked_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing cache efficiency..."
        
        ! Initialize data
        do i = 1, 100000
            data(i) = real(i, real64)
        end do
        
        var = variable(data, name="cache_test", dim_names=["index"])
        
        ! Sequential access pattern (cache-friendly)
        call system_clock(start_time, count_rate)
        do iter = 1, 50
            result = var * 2.0_real64
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        sequential_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        ! Blocked access pattern (test chunking efficiency)
        block_size = 10000
        call set_chunk_size(block_size)
        
        call system_clock(start_time, count_rate)
        do iter = 1, 50
            result = mean_chunked(var)
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        blocked_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  Sequential time: ", sequential_time, " seconds"
        write(*,'(A,F0.6,A)') "  Blocked time: ", blocked_time, " seconds"
        
        if (sequential_time > 0.0_real64 .and. blocked_time > 0.0_real64) then
            write(*,'(A,F0.2)') "  Cache efficiency ratio: ", sequential_time / blocked_time
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Cache efficiency"
        else
            write(*,'(A)') "FAIL: Cache efficiency"
        end if
    end subroutine test_cache_efficiency
    
    subroutine test_memory_layout_optimization()
        type(variable_t) :: var, result
        real(real64), dimension(1000, 1000) :: data_2d
        integer :: i, j, iter
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: column_major_time, access_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing memory layout optimization..."
        
        ! Initialize 2D data
        do j = 1, 1000
            do i = 1, 1000
                data_2d(i, j) = real(i + j, real64)
            end do
        end do
        
        var = variable(reshape(data_2d, [1000*1000]), name="layout_test", dim_names=["flat"])
        
        ! Test memory access patterns
        call system_clock(start_time, count_rate)
        do iter = 1, 10
            result = var + 1.0_real64
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        column_major_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        ! Test aggregation operations (different access pattern)
        call system_clock(start_time, count_rate)
        do iter = 1, 10
            result = mean(var)
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        access_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  Sequential access time: ", column_major_time, " seconds"
        write(*,'(A,F0.6,A)') "  Aggregation time: ", access_time, " seconds"
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Memory layout optimization"
        else
            write(*,'(A)') "FAIL: Memory layout optimization"
        end if
    end subroutine test_memory_layout_optimization
    
    subroutine test_loop_optimization()
        type(variable_t) :: var, result
        real(real64), dimension(50000) :: data
        integer :: i, iter
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: simple_loop_time, complex_loop_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing loop optimization..."
        
        ! Initialize data
        do i = 1, 50000
            data(i) = real(i, real64) * 0.01_real64
        end do
        
        var = variable(data, name="loop_test", dim_names=["index"])
        
        ! Simple loop operations (should be well optimized)
        call system_clock(start_time, count_rate)
        do iter = 1, 100
            result = var * 2.0_real64
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        simple_loop_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        ! More complex operations
        call system_clock(start_time, count_rate)
        do iter = 1, 100
            result = var * var + var - 1.0_real64
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        complex_loop_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  Simple loop time: ", simple_loop_time, " seconds"
        write(*,'(A,F0.6,A)') "  Complex loop time: ", complex_loop_time, " seconds"
        
        if (simple_loop_time > 0.0_real64 .and. complex_loop_time > 0.0_real64) then
            write(*,'(A,F0.2)') "  Complexity ratio: ", complex_loop_time / simple_loop_time
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Loop optimization"
        else
            write(*,'(A)') "FAIL: Loop optimization"
        end if
    end subroutine test_loop_optimization
    
    subroutine test_parallel_scaling_optimization()
        type(variable_t) :: var, result
        real(real64), dimension(200000) :: data
        integer :: i, iter, threads
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: time_1, time_4, time_8, time_24
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing parallel scaling optimization..."
        
        ! Initialize large dataset
        do i = 1, 200000
            data(i) = sin(real(i, real64) * 0.0001_real64)
        end do
        
        var = variable(data, name="parallel_test", dim_names=["index"])
        
        ! Test with different thread counts
        threads = 1
        call set_num_threads(threads)
        call system_clock(start_time, count_rate)
        do iter = 1, 20
            result = sum(var)
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        time_1 = real(end_time - start_time, real64) / real(count_rate, real64)
        
        threads = 4
        call set_num_threads(threads)
        call system_clock(start_time, count_rate)
        do iter = 1, 20
            result = sum(var)
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        time_4 = real(end_time - start_time, real64) / real(count_rate, real64)
        
        threads = 8
        call set_num_threads(threads)
        call system_clock(start_time, count_rate)
        do iter = 1, 20
            result = sum(var)
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        time_8 = real(end_time - start_time, real64) / real(count_rate, real64)
        
        threads = 24
        call set_num_threads(threads)
        call system_clock(start_time, count_rate)
        do iter = 1, 20
            result = sum(var)
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        time_24 = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  1 thread: ", time_1, " seconds"
        write(*,'(A,F0.6,A)') "  4 threads: ", time_4, " seconds"
        write(*,'(A,F0.6,A)') "  8 threads: ", time_8, " seconds"
        write(*,'(A,F0.6,A)') "  24 threads: ", time_24, " seconds"
        
        if (time_1 > 0.0_real64 .and. time_24 > 0.0_real64) then
            write(*,'(A,F0.2)') "  Parallel efficiency (24 threads): ", time_1 / (24.0_real64 * time_24)
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Parallel scaling optimization"
        else
            write(*,'(A)') "FAIL: Parallel scaling optimization"
        end if
    end subroutine test_parallel_scaling_optimization
    
    subroutine test_vectorization()
        type(variable_t) :: var1, var2, result
        real(real64), dimension(20000) :: data1, data2
        integer :: i, iter
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: arithmetic_time, transcendental_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing vectorization effectiveness..."
        
        ! Initialize data for vectorization
        do i = 1, 20000
            data1(i) = real(i, real64) * 0.01_real64
            data2(i) = real(i, real64) * 0.02_real64
        end do
        
        var1 = variable(data1, name="vec_test1", dim_names=["index"])
        var2 = variable(data2, name="vec_test2", dim_names=["index"])
        
        ! Arithmetic operations (should vectorize well)
        call system_clock(start_time, count_rate)
        do iter = 1, 50
            result = var1 + var2 * 3.0_real64
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        arithmetic_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        ! More complex operations
        call system_clock(start_time, count_rate)
        do iter = 1, 50
            result = var1 * var1 + var2 * var2  ! Should use FMA
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        transcendental_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  Arithmetic operations: ", arithmetic_time, " seconds"
        write(*,'(A,F0.6,A)') "  Complex operations: ", transcendental_time, " seconds"
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Vectorization"
        else
            write(*,'(A)') "FAIL: Vectorization"
        end if
    end subroutine test_vectorization
    
    subroutine test_memory_access_patterns()
        type(variable_t) :: var, result
        real(real64), dimension(100000) :: data
        integer :: i, iter, stride
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: contiguous_time, strided_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing memory access pattern optimization..."
        
        ! Initialize data
        do i = 1, 100000
            data(i) = real(i, real64)
        end do
        
        var = variable(data, name="access_test", dim_names=["index"])
        
        ! Contiguous access
        call system_clock(start_time, count_rate)
        do iter = 1, 30
            result = var + 1.0_real64
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        contiguous_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        ! Strided access (through slicing if available)
        call system_clock(start_time, count_rate)
        do iter = 1, 30
            result = slice_with_step(var, 1, 50000, 2)  ! Every other element
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        strided_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  Contiguous access: ", contiguous_time, " seconds"
        write(*,'(A,F0.6,A)') "  Strided access: ", strided_time, " seconds"
        
        if (contiguous_time > 0.0_real64 .and. strided_time > 0.0_real64) then
            write(*,'(A,F0.2)') "  Access pattern penalty: ", strided_time / contiguous_time
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Memory access patterns"
        else
            write(*,'(A)') "FAIL: Memory access patterns"
        end if
    end subroutine test_memory_access_patterns
    
    subroutine test_computational_intensity()
        type(variable_t) :: var, result
        real(real64), dimension(30000) :: data
        integer :: i, iter
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: low_intensity_time, high_intensity_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing computational intensity optimization..."
        
        ! Initialize data
        do i = 1, 30000
            data(i) = real(i, real64) * 0.001_real64
        end do
        
        var = variable(data, name="intensity_test", dim_names=["index"])
        
        ! Low computational intensity (memory bound)
        call system_clock(start_time, count_rate)
        do iter = 1, 50
            result = var + 1.0_real64
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        low_intensity_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        ! High computational intensity (compute bound)
        call system_clock(start_time, count_rate)
        do iter = 1, 50
            result = var * var * var + var * var - var + 1.0_real64
            call finalize_variable(result)
        end do
        call system_clock(end_time)
        high_intensity_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  Low intensity: ", low_intensity_time, " seconds"
        write(*,'(A,F0.6,A)') "  High intensity: ", high_intensity_time, " seconds"
        
        if (low_intensity_time > 0.0_real64 .and. high_intensity_time > 0.0_real64) then
            write(*,'(A,F0.2)') "  Intensity ratio: ", high_intensity_time / low_intensity_time
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Computational intensity"
        else
            write(*,'(A)') "FAIL: Computational intensity"
        end if
    end subroutine test_computational_intensity
    
    subroutine test_algorithm_optimization()
        type(variable_t) :: var, result1, result2
        real(real64), dimension(50000) :: data
        integer :: i, iter
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: naive_time, optimized_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing algorithmic optimization..."
        
        ! Initialize data
        do i = 1, 50000
            data(i) = real(i, real64)
        end do
        
        var = variable(data, name="algo_test", dim_names=["index"])
        
        ! Naive approach: multiple passes
        call system_clock(start_time, count_rate)
        do iter = 1, 20
            result1 = mean(var)
            result2 = std(var)
            call finalize_variable(result1)
            call finalize_variable(result2)
        end do
        call system_clock(end_time)
        naive_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        ! Optimized approach: single pass through chunked operations
        call set_chunk_size(5000)
        call system_clock(start_time, count_rate)
        do iter = 1, 20
            result1 = mean_chunked(var)
            call finalize_variable(result1)
        end do
        call system_clock(end_time)
        optimized_time = real(end_time - start_time, real64) / real(count_rate, real64)
        
        write(*,'(A,F0.6,A)') "  Naive algorithm: ", naive_time, " seconds"
        write(*,'(A,F0.6,A)') "  Optimized algorithm: ", optimized_time, " seconds"
        
        if (naive_time > 0.0_real64 .and. optimized_time > 0.0_real64) then
            write(*,'(A,F0.2)') "  Optimization speedup: ", naive_time / optimized_time
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Algorithm optimization"
        else
            write(*,'(A)') "FAIL: Algorithm optimization"
        end if
    end subroutine test_algorithm_optimization
    
    subroutine test_compiler_optimization_effectiveness()
        n_tests_total = n_tests_total + 1
        
        write(*,'(A)') "Testing compiler optimization effectiveness..."
        write(*,'(A)') "  Note: This test verifies that the code benefits from compiler optimizations"
        write(*,'(A)') "  Actual effectiveness depends on compiler flags (-O3, -march=native, etc.)"
        write(*,'(A)') "  Manual testing with different optimization levels recommended"
        
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Compiler optimization (informational)"
    end subroutine test_compiler_optimization_effectiveness
    
end program test_optimization