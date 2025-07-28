program test_performance_optimization_simple
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================="
    write(*,'(A)') "Testing Performance Optimization Layer..."
    write(*,'(A)') "========================================="
    
    ! Run tests
    call test_simd_methods()
    call test_parallel_methods()
    call test_binary_search_methods()
    call test_cache_methods()
    call test_optimization_methods()
    
    ! Summary
    write(*,'(A)') "========================================="
    write(*,'(A,I0,A,I0,A)') "Performance Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some performance optimization tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All performance optimization tests passed!"
    end if

contains

    subroutine test_simd_methods()
        type(fortarray_t) :: var, result
        real(real64), dimension(100) :: data
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') " Testing SIMD-optimized methods..."
        
        ! Create test data
        do i = 1, 100
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test SIMD selection
        write(*,'(A)') "   Test 1: SIMD selection"
        result = var%sel_simd(50.0_real64)
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "SIMD selection failed"
        else
            write(*,'(A)') "     PASS: SIMD selection"
        end if
        call finalize_variable(result)
        
        ! Test SIMD range selection
        write(*,'(A)') "   Test 2: SIMD range selection"
        result = var%sel_range_simd(10.0_real64, 20.0_real64)
        if (result%n_elements /= 11) then
            test_passed = .false.
            write(error_unit,'(A)') "SIMD range selection failed"
        else
            write(*,'(A)') "     PASS: SIMD range selection"
        end if
        call finalize_variable(result)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') " SUCCESS: SIMD methods passed!"
        else
            write(*,'(A)') " FAIL: SIMD methods failed!"
        end if
    end subroutine test_simd_methods

    subroutine test_parallel_methods()
        type(fortarray_t) :: var, result
        real(real64), dimension(50) :: data, lookup_points
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') " Testing parallel methods..."
        
        ! Create test data
        do i = 1, 50
            data(i) = real(i, real64) * 2.0_real64
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test parallel nearest neighbor
        write(*,'(A)') "   Test 1: Parallel nearest neighbor"
        result = var%sel_nearest_parallel(25.0_real64)
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Parallel nearest failed"
        else
            write(*,'(A)') "     PASS: Parallel nearest neighbor"
        end if
        call finalize_variable(result)
        
        ! Test parallel interpolation
        write(*,'(A)') "   Test 2: Parallel interpolation"
        result = var%interp_parallel(25.5_real64)
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Parallel interpolation failed"
        else
            write(*,'(A)') "     PASS: Parallel interpolation"
        end if
        call finalize_variable(result)
        
        ! Test multi-point lookup
        write(*,'(A)') "   Test 3: Multi-point lookup"
        do i = 1, 10
            lookup_points(i) = real(i * 5, real64)
        end do
        result = var%sel_multipoint_parallel(lookup_points(1:10))
        if (result%n_elements /= 10) then
            test_passed = .false.
            write(error_unit,'(A)') "Multi-point lookup failed"
        else
            write(*,'(A)') "     PASS: Multi-point lookup"
        end if
        call finalize_variable(result)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') " SUCCESS: Parallel methods passed!"
        else
            write(*,'(A)') " FAIL: Parallel methods failed!"
        end if
    end subroutine test_parallel_methods

    subroutine test_binary_search_methods()
        type(fortarray_t) :: var, result
        real(real64), dimension(100) :: data, batch_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') " Testing binary search methods..."
        
        ! Create sorted test data
        do i = 1, 100
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="sorted_data", dim_names=["x"])
        
        ! Test binary search
        write(*,'(A)') "   Test 1: Binary search"
        result = var%sel_binary_search(50.0_real64)
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Binary search failed"
        else
            write(*,'(A)') "     PASS: Binary search"
        end if
        call finalize_variable(result)
        
        ! Test binary interpolation
        write(*,'(A)') "   Test 2: Binary interpolation"
        result = var%sel_binary_interp(50.5_real64)
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Binary interpolation failed"
        else
            write(*,'(A)') "     PASS: Binary interpolation"
        end if
        call finalize_variable(result)
        
        ! Test binary range selection
        write(*,'(A)') "   Test 3: Binary range selection"
        result = var%sel_range_binary(10.0_real64, 20.0_real64)
        if (result%n_elements /= 11) then
            test_passed = .false.
            write(error_unit,'(A)') "Binary range selection failed"
        else
            write(*,'(A)') "     PASS: Binary range selection"
        end if
        call finalize_variable(result)
        
        ! Test batch selection
        write(*,'(A)') "   Test 4: Batch selection"
        do i = 1, 5
            batch_values(i) = real(i * 10, real64)
        end do
        result = var%sel_batch_sorted(batch_values(1:5))
        if (result%n_elements /= 5) then
            test_passed = .false.
            write(error_unit,'(A)') "Batch selection failed"
        else
            write(*,'(A)') "     PASS: Batch selection"
        end if
        call finalize_variable(result)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') " SUCCESS: Binary search methods passed!"
        else
            write(*,'(A)') " FAIL: Binary search methods failed!"
        end if
    end subroutine test_binary_search_methods

    subroutine test_cache_methods()
        type(fortarray_t) :: var, result1, result2
        real(real64), dimension(50) :: data
        logical :: test_passed
        integer :: i, hits, misses
        real(real64) :: hit_ratio
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') " Testing caching methods..."
        
        ! Create test data
        do i = 1, 50
            data(i) = real(i, real64) * 0.1_real64
        end do
        var = new_array(data, name="cached_data", dim_names=["x"])
        
        ! Test cached selection
        write(*,'(A)') "   Test 1: Cached selection"
        result1 = var%sel_cached(2.5_real64)
        if (result1%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Cached selection failed"
        else
            write(*,'(A)') "     PASS: Cached selection"
        end if
        call finalize_variable(result1)
        
        ! Test cache hit
        write(*,'(A)') "   Test 2: Cache hit"
        result2 = var%sel_cached(2.5_real64)
        if (result2%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Cache hit failed"
        else
            write(*,'(A)') "     PASS: Cache hit"
        end if
        call finalize_variable(result2)
        
        ! Test cache invalidation
        write(*,'(A)') "   Test 3: Cache invalidation"
        call var%invalidate_cache()
        write(*,'(A)') "     PASS: Cache invalidation"
        
        ! Test cache statistics
        write(*,'(A)') "   Test 4: Cache statistics"
        call var%get_cache_stats(hits, misses, hit_ratio)
        write(*,'(A,I0,A,I0,A,F5.2,A)') "     PASS: Cache stats - Hits: ", hits, &
            ", Misses: ", misses, ", Hit ratio: ", hit_ratio * 100, "%"
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') " SUCCESS: Cache methods passed!"
        else
            write(*,'(A)') " FAIL: Cache methods failed!"
        end if
    end subroutine test_cache_methods

    subroutine test_optimization_methods()
        type(fortarray_t) :: var, result
        real(real64), dimension(10, 10) :: data
        logical :: test_passed
        integer :: i, j
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') " Testing optimization methods..."
        
        ! Create test data
        do i = 1, 10
            do j = 1, 10
                data(i, j) = real(i*10 + j, real64)
            end do
        end do
        var = new_array(data, name="test_data", dim_names=["x", "y"])
        
        ! Test layout optimization
        write(*,'(A)') "   Test 1: Layout optimization"
        result = var%optimize_layout("column_major")
        if (result%n_elements /= 100) then
            test_passed = .false.
            write(error_unit,'(A)') "Layout optimization failed"
        else
            write(*,'(A)') "     PASS: Layout optimization"
        end if
        call finalize_variable(result)
        
        ! Test prefetch optimization
        write(*,'(A)') "   Test 2: Prefetch optimization"
        result = var%prefetch_optimize()
        if (result%n_elements /= 100) then
            test_passed = .false.
            write(error_unit,'(A)') "Prefetch optimization failed"
        else
            write(*,'(A)') "     PASS: Prefetch optimization"
        end if
        call finalize_variable(result)
        
        ! Test chunk optimization
        write(*,'(A)') "   Test 3: Chunk optimization"
        result = var%chunk_optimize([5, 5])
        if (result%n_elements /= 100) then
            test_passed = .false.
            write(error_unit,'(A)') "Chunk optimization failed"
        else
            write(*,'(A)') "     PASS: Chunk optimization"
        end if
        call finalize_variable(result)
        
        ! Test memory optimization
        write(*,'(A)') "   Test 4: Memory optimization"
        result = var%optimize_memory()
        if (result%n_elements /= 100) then
            test_passed = .false.
            write(error_unit,'(A)') "Memory optimization failed"
        else
            write(*,'(A)') "     PASS: Memory optimization"
        end if
        call finalize_variable(result)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') " SUCCESS: Optimization methods passed!"
        else
            write(*,'(A)') " FAIL: Optimization methods failed!"
        end if
    end subroutine test_optimization_methods

end program test_performance_optimization_simple