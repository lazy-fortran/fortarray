!> Example 7: Parallel Processing with OpenMP
!> This example demonstrates using Foxel's parallel computing capabilities
program parallel_processing
    use foxel
    use iso_fortran_env, only: real64, int64, error_unit
    implicit none
    
    type(variable_t) :: large_var, result_var, mean_result, sum_result
    real(real64), dimension(1000000) :: large_data  ! 1M elements
    integer :: i, original_threads, target_threads
    integer(int64) :: start_time, end_time, count_rate
    real(real64) :: serial_time, parallel_time, speedup
    real(real64) :: serial_result, parallel_result
    
    write(*,'(A)') "=== Foxel Example 7: Parallel Processing ==="
    write(*,'(A)') ""
    
    ! 1. Initialize large dataset
    write(*,'(A)') "1. Creating large dataset (1M elements)..."
    do i = 1, 1000000
        large_data(i) = sin(real(i, real64) * 0.000001_real64) + &
                       cos(real(i, real64) * 0.000002_real64) + &
                       real(i, real64) * 0.000001_real64
    end do
    
    large_var = variable(large_data, name="large_dataset", dim_names=["index"])
    large_var%units = "arbitrary_units"
    large_var%long_name = "Large synthetic dataset for parallel processing demo"
    
    write(*,'(A,I0,A)') "   Created variable with ", large_var%n_elements, " elements"
    write(*,'(A)') ""
    
    ! 2. Check current parallel configuration
    write(*,'(A)') "2. Parallel processing configuration..."
    original_threads = get_num_threads()
    write(*,'(A,I0)') "   Current number of threads: ", original_threads
    write(*,'(A)') ""
    
    ! 3. Serial processing benchmark
    write(*,'(A)') "3. Serial processing benchmark..."
    call set_num_threads(1)
    
    call system_clock(start_time, count_rate)
    sum_result = sum(large_var)
    serial_result = sum_result%data%values_r64(1)
    call system_clock(end_time)
    serial_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Serial sum time: ", serial_time, " seconds"
    write(*,'(A,F0.3)') "   Serial sum result: ", serial_result
    call finalize_variable(sum_result)
    
    ! Mean calculation (serial)
    call system_clock(start_time, count_rate)
    mean_result = mean(large_var)
    call system_clock(end_time)
    write(*,'(A,F0.6,A)') "   Serial mean time: ", &
        real(end_time - start_time, real64) / real(count_rate, real64), " seconds"
    write(*,'(A,F0.6)') "   Serial mean result: ", mean_result%data%values_r64(1)
    call finalize_variable(mean_result)
    write(*,'(A)') ""
    
    ! 4. Parallel processing with different thread counts
    write(*,'(A)') "4. Parallel processing benchmarks..."
    
    ! Test with 4 threads
    target_threads = 4
    call set_num_threads(target_threads)
    call system_clock(start_time, count_rate)
    sum_result = sum(large_var)
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    parallel_result = sum_result%data%values_r64(1)
    speedup = serial_time / parallel_time
    
    write(*,'(A,I0,A)') "   With ", target_threads, " threads:"
    write(*,'(A,F0.6,A)') "     Sum time: ", parallel_time, " seconds"
    write(*,'(A,F0.2)') "     Speedup: ", speedup
    write(*,'(A,F0.3)') "     Result: ", parallel_result
    write(*,'(A,F0.6)') "     Result difference: ", abs(parallel_result - serial_result)
    call finalize_variable(sum_result)
    
    ! Test with 8 threads
    target_threads = 8
    call set_num_threads(target_threads)
    call system_clock(start_time, count_rate)
    sum_result = sum(large_var)
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    parallel_result = sum_result%data%values_r64(1)
    speedup = serial_time / parallel_time
    
    write(*,'(A,I0,A)') "   With ", target_threads, " threads:"
    write(*,'(A,F0.6,A)') "     Sum time: ", parallel_time, " seconds"
    write(*,'(A,F0.2)') "     Speedup: ", speedup
    write(*,'(A,F0.3)') "     Result: ", parallel_result
    call finalize_variable(sum_result)
    
    ! Test with maximum threads (24)
    target_threads = 24
    call set_num_threads(target_threads)
    call system_clock(start_time, count_rate)
    sum_result = sum(large_var)
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    parallel_result = sum_result%data%values_r64(1)
    speedup = serial_time / parallel_time
    
    write(*,'(A,I0,A)') "   With ", target_threads, " threads:"
    write(*,'(A,F0.6,A)') "     Sum time: ", parallel_time, " seconds"
    write(*,'(A,F0.2)') "     Speedup: ", speedup
    write(*,'(A,F0.3)') "     Result: ", parallel_result
    write(*,'(A,F0.1,A)') "     Parallel efficiency: ", (speedup / real(target_threads, real64)) * 100.0_real64, "%"
    call finalize_variable(sum_result)
    write(*,'(A)') ""
    
    ! 5. Parallel arithmetic operations
    write(*,'(A)') "5. Parallel arithmetic operations..."
    call set_num_threads(24)
    
    ! Complex arithmetic operation
    call system_clock(start_time, count_rate)
    result_var = large_var * large_var + large_var - 1.0_real64
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Complex arithmetic (x²+x-1) time: ", parallel_time, " seconds"
    write(*,'(A,F0.3)') "   First result value: ", result_var%data%values_r64(1)
    call finalize_variable(result_var)
    
    ! Broadcasting operation
    call system_clock(start_time, count_rate)
    result_var = large_var + 100.0_real64
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Broadcasting (x+100) time: ", parallel_time, " seconds"
    call finalize_variable(result_var)
    
    ! Element-wise operations
    call system_clock(start_time, count_rate)
    result_var = large_var * 2.5_real64
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Scalar multiplication (x*2.5) time: ", parallel_time, " seconds"
    call finalize_variable(result_var)
    write(*,'(A)') ""
    
    ! 6. Parallel aggregation operations
    write(*,'(A)') "6. Parallel aggregation operations..."
    
    ! Standard deviation
    call system_clock(start_time, count_rate)
    result_var = std(large_var)
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Standard deviation time: ", parallel_time, " seconds"
    write(*,'(A,F0.6)') "   Standard deviation: ", result_var%data%values_r64(1)
    call finalize_variable(result_var)
    
    ! Min/Max operations
    call system_clock(start_time, count_rate)
    result_var = minval(large_var)
    call system_clock(end_time)
    write(*,'(A,F0.6,A)') "   Minimum value time: ", &
        real(end_time - start_time, real64) / real(count_rate, real64), " seconds"
    write(*,'(A,F0.6)') "   Minimum value: ", result_var%data%values_r64(1)
    call finalize_variable(result_var)
    
    call system_clock(start_time, count_rate)
    result_var = maxval(large_var)
    call system_clock(end_time)
    write(*,'(A,F0.6,A)') "   Maximum value time: ", &
        real(end_time - start_time, real64) / real(count_rate, real64), " seconds"
    write(*,'(A,F0.6)') "   Maximum value: ", result_var%data%values_r64(1)
    call finalize_variable(result_var)
    write(*,'(A)') ""
    
    ! 7. Chunked parallel operations
    write(*,'(A)') "7. Chunked parallel operations..."
    
    ! Set chunk size for optimal cache usage
    call set_chunk_size(50000)  ! 50K elements per chunk
    
    call system_clock(start_time, count_rate)
    result_var = mean_chunked(large_var)
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Chunked mean time: ", parallel_time, " seconds"
    write(*,'(A,F0.6)') "   Chunked mean result: ", result_var%data%values_r64(1)
    call finalize_variable(result_var)
    
    call system_clock(start_time, count_rate)
    result_var = sum_chunked(large_var)
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Chunked sum time: ", parallel_time, " seconds"
    write(*,'(A,F0.3)') "   Chunked sum result: ", result_var%data%values_r64(1)
    call finalize_variable(result_var)
    write(*,'(A)') ""
    
    ! 8. Memory-intensive operations
    write(*,'(A)') "8. Memory-intensive parallel operations..."
    
    ! Create second large variable for operations
    result_var = large_var * 0.5_real64  ! Create copy with different values
    
    ! Variable-to-variable operations
    call system_clock(start_time, count_rate)
    sum_result = large_var + result_var
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Large variable addition time: ", parallel_time, " seconds"
    write(*,'(A,F0.3)') "   Addition result (first element): ", sum_result%data%values_r64(1)
    call finalize_variable(sum_result)
    
    call system_clock(start_time, count_rate)
    sum_result = large_var * result_var
    call system_clock(end_time)
    parallel_time = real(end_time - start_time, real64) / real(count_rate, real64)
    
    write(*,'(A,F0.6,A)') "   Large variable multiplication time: ", parallel_time, " seconds"
    call finalize_variable(sum_result)
    call finalize_variable(result_var)
    write(*,'(A)') ""
    
    ! 9. Performance analysis summary
    write(*,'(A)') "9. Performance analysis summary..."
    write(*,'(A)') "   ╔═══════════════════════════════════════════════════════╗"
    write(*,'(A)') "   ║              PARALLEL PERFORMANCE SUMMARY             ║"
    write(*,'(A)') "   ╠═══════════════════════════════════════════════════════╣"
    write(*,'(A)') "   ║ Dataset size:           1,000,000 elements            ║"
    write(*,'(A)') "   ║ Memory usage:           ~8 MB per variable            ║"
    write(*,'(A)') "   ║ Operations tested:      Sum, Mean, Std, Min, Max      ║"
    write(*,'(A)') "   ║ Threading:              OpenMP parallel regions       ║"
    write(*,'(A)') "   ║ Chunking:               Cache-optimized processing    ║"
    write(*,'(A)') "   ║ Arithmetic:             SIMD-optimized where possible ║"
    write(*,'(A)') "   ╚═══════════════════════════════════════════════════════╝"
    write(*,'(A)') ""
    
    ! 10. Optimization recommendations
    write(*,'(A)') "10. Performance optimization tips:"
    write(*,'(A)') "    • Use 8-24 threads for best performance on most systems"
    write(*,'(A)') "    • Set chunk size to optimize cache usage"
    write(*,'(A)') "    • Enable compiler optimizations (-O3 -march=native)"
    write(*,'(A)') "    • Use chunked operations for very large datasets"
    write(*,'(A)') "    • Monitor memory bandwidth for memory-bound operations"
    write(*,'(A)') ""
    
    ! Restore original thread count
    call set_num_threads(original_threads)
    write(*,'(A,I0)') "   Restored original thread count: ", original_threads
    
    ! Clean up memory
    write(*,'(A)') "11. Cleaning up memory..."
    call finalize_variable(large_var)
    
    write(*,'(A)') "=== Parallel processing example completed successfully! ==="
    
end program parallel_processing