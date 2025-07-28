program test_filtering_operations
    use fortarray
    use fortarray_types
    use fortarray_constructors, only: new_array
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    logical :: all_passed = .true.
    
    call test_where_condition()
    call test_boolean_mask_creation()
    call test_boolean_mask_application()
    call test_condition_chaining()
    call test_fillna_operations()
    call test_vectorized_conditions()
    call test_complex_boolean_expressions()
    call test_performance_filtering()
    
    if (all_passed) then
        print *, "SUCCESS: All filtering operations tests passed!"
    else
        error stop "FAILURE: Some filtering operations tests failed!"
    end if
    
contains
    
    subroutine test_where_condition()
        type(fortarray_t) :: arr, result
        real(real64), dimension(5) :: data_1d
        real(real64), dimension(5) :: expected
        integer :: i
        
        print *, "Testing where() condition filtering..."
        
        ! Create test data: [1, 2, 3, 4, 5]
        do i = 1, 5
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: where(condition > 3, other_value=-999)
        print *, "  Test 1: where(condition > 3, other_value=-999)"
        result = arr%where_gt(3.0_real64, -999.0_real64)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "    FAIL: where - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 5) then
            print *, "    FAIL: where - wrong size"
            all_passed = .false.
            return
        end if
        
        ! Expected: [-999, -999, -999, 4, 5]
        expected = [-999.0_real64, -999.0_real64, -999.0_real64, 4.0_real64, 5.0_real64]
        do i = 1, 5
            if (abs(result%data%values_r64(i) - expected(i)) > 1e-10) then
                print *, "    FAIL: where - wrong value at", i
                print *, "      Expected:", expected(i)
                print *, "      Got:", result%data%values_r64(i)
                all_passed = .false.
                return
            end if
        end do
        
        print *, "    PASS: where() condition filtering"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_where_condition
    
    subroutine test_boolean_mask_creation()
        type(fortarray_t) :: arr, mask
        real(real64), dimension(6) :: data_1d
        integer :: i
        
        print *, "Testing boolean mask creation..."
        
        ! Create test data: [1, 2, 3, 4, 5, 6]
        do i = 1, 6
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: Create mask for values > 3
        print *, "  Test 1: Create mask for values > 3"
        mask = arr%gt(3.0_real64)
        
        if (.not. allocated(mask%data%values_logical)) then
            print *, "    FAIL: mask creation - no logical data allocated"
            all_passed = .false.
            return
        end if
        
        if (mask%n_elements /= 6) then
            print *, "    FAIL: mask creation - wrong size"
            all_passed = .false.
            return
        end if
        
        ! Expected: [F, F, F, T, T, T]
        do i = 1, 6
            if (i <= 3) then
                if (mask%data%values_logical(i)) then
                    print *, "    FAIL: mask - should be false at", i
                    all_passed = .false.
                    return
                end if
            else
                if (.not. mask%data%values_logical(i)) then
                    print *, "    FAIL: mask - should be true at", i
                    all_passed = .false.
                    return
                end if
            end if
        end do
        
        print *, "    PASS: boolean mask creation"
        
        call finalize_variable(arr)
        call finalize_variable(mask)
        
    end subroutine test_boolean_mask_creation
    
    subroutine test_boolean_mask_application()
        type(fortarray_t) :: arr, mask, result
        real(real64), dimension(6) :: data_1d
        logical, dimension(6) :: mask_data
        integer :: i
        
        print *, "Testing boolean mask application..."
        
        ! Create test data: [10, 20, 30, 40, 50, 60]
        do i = 1, 6
            data_1d(i) = real(i * 10, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Create mask: [T, F, T, F, T, F] (select odd indices)
        mask_data = [.true., .false., .true., .false., .true., .false.]
        mask = new_array(mask_data, dim_names=["x"])
        
        ! Test 1: Apply boolean mask
        print *, "  Test 1: Apply boolean mask"
        result = arr%mask_where(mask)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "    FAIL: mask application - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 3) then
            print *, "    FAIL: mask application - wrong size"
            print *, "      Expected: 3"
            print *, "      Got:", result%n_elements
            all_passed = .false.
            return
        end if
        
        ! Expected: [10, 30, 50]
        if (abs(result%data%values_r64(1) - 10.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(2) - 30.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(3) - 50.0_real64) > 1e-10) then
            print *, "    FAIL: mask application - wrong values"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: boolean mask application"
        
        call finalize_variable(arr)
        call finalize_variable(mask)
        call finalize_variable(result)
        
    end subroutine test_boolean_mask_application
    
    subroutine test_condition_chaining()
        type(fortarray_t) :: arr, mask1, mask2, combined_mask, result
        real(real64), dimension(10) :: data_1d
        integer :: i
        
        print *, "Testing condition chaining (AND, OR, NOT)..."
        
        ! Create test data: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        do i = 1, 10
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: AND operation (values > 3 AND values < 8)
        print *, "  Test 1: AND operation (values > 3 AND values < 8)"
        mask1 = arr%gt(3.0_real64)
        mask2 = arr%lt(8.0_real64)
        combined_mask = mask1%logical_and(mask2)
        result = arr%mask_where(combined_mask)
        
        if (result%n_elements /= 4) then
            print *, "    FAIL: AND operation - wrong size"
            print *, "      Expected: 4 (values 4,5,6,7)"
            print *, "      Got:", result%n_elements
            all_passed = .false.
            return
        end if
        
        ! Expected: [4, 5, 6, 7]
        do i = 1, 4
            if (abs(result%data%values_r64(i) - real(i + 3, real64)) > 1e-10) then
                print *, "    FAIL: AND operation - wrong value at", i
                all_passed = .false.
                return
            end if
        end do
        
        print *, "    PASS: AND operation"
        
        ! Test 2: OR operation (values < 3 OR values > 8)
        print *, "  Test 2: OR operation (values < 3 OR values > 8)"
        mask1 = arr%lt(3.0_real64)
        mask2 = arr%gt(8.0_real64)
        combined_mask = mask1%logical_or(mask2)
        result = arr%mask_where(combined_mask)
        
        if (result%n_elements /= 4) then
            print *, "    FAIL: OR operation - wrong size"
            print *, "      Expected: 4 (values 1,2,9,10)"
            print *, "      Got:", result%n_elements
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: OR operation"
        
        ! Test 3: NOT operation (NOT values > 5)
        print *, "  Test 3: NOT operation (NOT values > 5)"
        mask1 = arr%gt(5.0_real64)
        combined_mask = mask1%logical_not()
        result = arr%mask_where(combined_mask)
        
        if (result%n_elements /= 5) then
            print *, "    FAIL: NOT operation - wrong size"
            print *, "      Expected: 5 (values 1,2,3,4,5)"
            print *, "      Got:", result%n_elements
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: NOT operation"
        
        call finalize_variable(arr)
        call finalize_variable(mask1)
        call finalize_variable(mask2)
        call finalize_variable(combined_mask)
        call finalize_variable(result)
        
    end subroutine test_condition_chaining
    
    subroutine test_fillna_operations()
        type(fortarray_t) :: arr, result
        real(real64), dimension(5) :: data_1d
        
        print *, "Testing fillna operations..."
        
        ! Create test data with NaN values: [1, NaN, 3, NaN, 5]
        data_1d(1) = 1.0_real64
        data_1d(2) = huge(1.0_real64)  ! Use huge as NaN placeholder
        data_1d(3) = 3.0_real64
        data_1d(4) = huge(1.0_real64)  ! Use huge as NaN placeholder
        data_1d(5) = 5.0_real64
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: fillna with constant value
        print *, "  Test 1: fillna with constant value -999"
        result = arr%fillna_value(-999.0_real64)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "    FAIL: fillna - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 5) then
            print *, "    FAIL: fillna - wrong size"
            all_passed = .false.
            return
        end if
        
        ! Expected: [1, -999, 3, -999, 5]
        if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(2) - (-999.0_real64)) > 1e-10 .or. &
            abs(result%data%values_r64(3) - 3.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(4) - (-999.0_real64)) > 1e-10 .or. &
            abs(result%data%values_r64(5) - 5.0_real64) > 1e-10) then
            print *, "    FAIL: fillna - wrong values"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: fillna with constant value"
        
        ! Test 2: forward fill (ffill)
        print *, "  Test 2: forward fill (ffill)"
        result = arr%ffill()
        
        ! Expected: [1, 1, 3, 3, 5] (forward fill)
        if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(2) - 1.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(3) - 3.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(4) - 3.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(5) - 5.0_real64) > 1e-10) then
            print *, "    FAIL: ffill - wrong values"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: forward fill"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_fillna_operations
    
    subroutine test_vectorized_conditions()
        type(fortarray_t) :: arr, result
        real(real64), dimension(8) :: data_1d
        integer :: i
        
        print *, "Testing vectorized condition evaluation..."
        
        ! Create test data: [1, 2, 3, 4, 5, 6, 7, 8]
        do i = 1, 8
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: Vectorized comparison and filtering
        print *, "  Test 1: Vectorized condition (x^2 > 16)"
        result = arr%where_custom("x**2 > 16", 0.0_real64)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "    FAIL: vectorized condition - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 8) then
            print *, "    FAIL: vectorized condition - wrong size"
            all_passed = .false.
            return
        end if
        
        ! Expected: [0, 0, 0, 0, 5, 6, 7, 8] (only values 5-8 have x^2 > 16)
        do i = 1, 8
            if (i <= 4) then
                if (abs(result%data%values_r64(i) - 0.0_real64) > 1e-10) then
                    print *, "    FAIL: vectorized condition - wrong value at", i
                    all_passed = .false.
                    return
                end if
            else
                if (abs(result%data%values_r64(i) - real(i, real64)) > 1e-10) then
                    print *, "    FAIL: vectorized condition - wrong value at", i
                    all_passed = .false.
                    return
                end if
            end if
        end do
        
        print *, "    PASS: vectorized condition evaluation"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_vectorized_conditions
    
    subroutine test_complex_boolean_expressions()
        type(fortarray_t) :: arr, result
        real(real64), dimension(12) :: data_1d
        integer :: i
        
        print *, "Testing complex boolean expressions..."
        
        ! Create test data: [1, 2, 3, ..., 12]
        do i = 1, 12
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: Complex expression ((x > 3) AND (x < 9)) OR (x > 10)
        print *, "  Test 1: Complex expression ((x > 3) AND (x < 9)) OR (x > 10)"
        result = arr%where_complex("((x > 3) & (x < 9)) | (x > 10)", -1.0_real64)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "    FAIL: complex boolean - no data allocated"
            all_passed = .false.
            return
        end if
        
        ! Expected: [-1, -1, -1, 4, 5, 6, 7, 8, -1, -1, 11, 12]
        ! Values that satisfy: 4,5,6,7,8 (3 < x < 9) and 11,12 (x > 10)
        
        print *, "    PASS: complex boolean expressions"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_complex_boolean_expressions
    
    subroutine test_performance_filtering()
        type(fortarray_t) :: arr, result
        real(real64), dimension(10000) :: data_1d
        real(real64) :: t_start, t_end
        integer :: i
        
        print *, "Testing performance of filtering operations..."
        
        ! Create large test data
        do i = 1, 10000
            data_1d(i) = real(mod(i, 100), real64)  ! Values 0-99 repeated
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: Performance test for large array filtering
        print *, "  Test 1: Filter 10,000 elements (values > 50)"
        call cpu_time(t_start)
        result = arr%where_gt(50.0_real64, -999.0_real64)
        call cpu_time(t_end)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "    FAIL: performance test - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 10000) then
            print *, "    FAIL: performance test - wrong size"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: Performance test completed in", (t_end - t_start), "seconds"
        
        ! Test 2: Count filtered elements
        ! Should have ~4900 elements with values > 50 (51-99 out of 0-99 = 49 values per 100)
        ! 10000 / 100 * 49 = 4900
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_performance_filtering
    
end program test_filtering_operations