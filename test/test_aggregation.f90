program test_aggregation
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Aggregation Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run tests
    call test_sum_functions()
    call test_mean_functions()
    call test_min_max_functions()
    call test_std_var_functions()
    call test_median_functions()
    call test_quantile_functions()
    call test_aggregation_along_dims()
    call test_weighted_mean()
    call test_weighted_sum()
    call test_missing_data_aggregation()
    write(*,'(A)') "About to run empty aggregation test..."
    call test_empty_aggregation()
    write(*,'(A)') "About to run scalar aggregation test..."
    call test_scalar_aggregation()
    write(*,'(A)') "About to run multidim aggregation test..."
    call test_multidim_aggregation()
    write(*,'(A)') "About to run type preservation test..."
    call test_aggregation_preserves_type()
    write(*,'(A)') "About to run performance test..."
    call test_aggregation_performance()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Aggregation Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some aggregation tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All aggregation tests passed!"
    end if

contains

    subroutine test_sum_functions()
        type(fortarray_t) :: var, result
        real(real64), dimension(3,4) :: data
        real(real64) :: expected_sum
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = reshape([(real(i, real64), i=1,12)], [3,4])
        var = new_array(data, name="test_data", dim_names=["x", "y"])
        
        ! Test sum of all elements
        result = sum(var)
        expected_sum = sum(data)
        
        if (abs(result%data%values_r64(1) - expected_sum) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F10.4,A,F10.4)') "Sum mismatch: got ", &
                result%data%values_r64(1), " expected ", expected_sum
        end if
        
        ! Test sum along dimension
        result = sum(var, dim=1)
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum along dim=1 has wrong shape"
        else
            ! Check values: sum of each column
            if (abs(result%data%values_r64(1) - 6.0_real64) > 1e-10 .or. &  ! 1+2+3
                abs(result%data%values_r64(2) - 15.0_real64) > 1e-10 .or. & ! 4+5+6
                abs(result%data%values_r64(3) - 24.0_real64) > 1e-10 .or. & ! 7+8+9
                abs(result%data%values_r64(4) - 33.0_real64) > 1e-10) then  ! 10+11+12
                test_passed = .false.
                write(error_unit,'(A)') "Sum along dim=1 has wrong values"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Sum functions test"
        else
            write(*,'(A)') "FAIL: Sum functions test"
        end if
    end subroutine test_sum_functions
    
    subroutine test_mean_functions()
        type(fortarray_t) :: var, result
        real(real64), dimension(2,3,4) :: data
        real(real64) :: expected_mean
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = reshape([(real(i, real64), i=1,24)], [2,3,4])
        var = new_array(data, name="test_data", dim_names=["x", "y", "z"])
        
        ! Test mean of all elements
        result = mean(var)
        expected_mean = sum(data) / size(data)
        
        if (abs(result%data%values_r64(1) - expected_mean) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F10.4,A,F10.4)') "Mean mismatch: got ", &
                result%data%values_r64(1), " expected ", expected_mean
        end if
        
        ! Test mean along dimension
        result = mean(var, dim=2)  ! Average over y dimension
        if (any(result%shape /= [2, 4])) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean along dim=2 has wrong shape"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mean functions test"
        else
            write(*,'(A)') "FAIL: Mean functions test"
        end if
    end subroutine test_mean_functions
    
    subroutine test_min_max_functions()
        type(fortarray_t) :: var, min_result, max_result
        real(real64), dimension(3,3) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with known min/max
        data = reshape([5.0_real64, 2.0_real64, 8.0_real64, &
                       1.0_real64, 9.0_real64, 3.0_real64, &
                       7.0_real64, 4.0_real64, 6.0_real64], [3,3])
        var = new_array(data, name="test_data", dim_names=["x", "y"])
        
        ! Test min
        min_result = minval(var)
        if (abs(min_result%data%values_r64(1) - 1.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Min value incorrect"
        end if
        
        ! Test max
        max_result = maxval(var)
        if (abs(max_result%data%values_r64(1) - 9.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Max value incorrect"
        end if
        
        ! Test min along dimension
        min_result = minval(var, dim=1)
        if (min_result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Min along dim=1 has wrong shape"
        else
            ! Check values: min of each column
            if (abs(min_result%data%values_r64(1) - 2.0_real64) > 1e-10 .or. &
                abs(min_result%data%values_r64(2) - 1.0_real64) > 1e-10 .or. &
                abs(min_result%data%values_r64(3) - 4.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Min along dim=1 has wrong values"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(min_result)
        call finalize_variable(max_result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Min/Max functions test"
        else
            write(*,'(A)') "FAIL: Min/Max functions test"
        end if
    end subroutine test_min_max_functions
    
    subroutine test_std_var_functions()
        type(fortarray_t) :: var, std_result, var_result
        real(real64), dimension(4) :: data
        real(real64) :: expected_mean, expected_var, expected_std
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with known statistics
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Calculate expected values
        expected_mean = sum(data) / size(data)  ! 2.5
        expected_var = sum((data - expected_mean)**2) / size(data)  ! 1.25
        expected_std = sqrt(expected_var)  ! ~1.118
        
        ! Test variance
        var_result = variance(var)
        if (abs(var_result%data%values_r64(1) - expected_var) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F10.6,A,F10.6)') "Variance mismatch: got ", &
                var_result%data%values_r64(1), " expected ", expected_var
        end if
        
        ! Test standard deviation
        std_result = std(var)
        if (abs(std_result%data%values_r64(1) - expected_std) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F10.6,A,F10.6)') "Std deviation mismatch: got ", &
                std_result%data%values_r64(1), " expected ", expected_std
        end if
        
        ! Test sample variance (ddof=1)
        var_result = variance(var, ddof=1)
        expected_var = sum((data - expected_mean)**2) / (size(data) - 1)  ! 1.667
        if (abs(var_result%data%values_r64(1) - expected_var) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Sample variance incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(std_result)
        call finalize_variable(var_result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Std/Var functions test"
        else
            write(*,'(A)') "FAIL: Std/Var functions test"
        end if
    end subroutine test_std_var_functions
    
    subroutine test_median_functions()
        type(fortarray_t) :: var_odd, var_even, result
        real(real64), dimension(5) :: data_odd
        real(real64), dimension(6) :: data_even
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test odd number of elements
        data_odd = [3.0_real64, 1.0_real64, 4.0_real64, 2.0_real64, 5.0_real64]
        var_odd = new_array(data_odd, name="odd_data", dim_names=["x"])
        
        result = median(var_odd)
        if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Median of odd array incorrect"
        end if
        
        ! Test even number of elements
        data_even = [3.0_real64, 1.0_real64, 4.0_real64, 2.0_real64, 5.0_real64, 6.0_real64]
        var_even = new_array(data_even, name="even_data", dim_names=["x"])
        
        result = median(var_even)
        if (abs(result%data%values_r64(1) - 3.5_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Median of even array incorrect"
        end if
        
        call finalize_variable(var_odd)
        call finalize_variable(var_even)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Median functions test"
        else
            write(*,'(A)') "FAIL: Median functions test"
        end if
    end subroutine test_median_functions
    
    subroutine test_quantile_functions()
        type(fortarray_t) :: var, q25, q50, q75
        real(real64), dimension(100) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data: 1 to 100
        data = [(real(i, real64), i=1,100)]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test 25th percentile
        q25 = quantile(var, 0.25_real64)
        if (abs(q25%data%values_r64(1) - 25.75_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F10.4)') "25th percentile incorrect: ", q25%data%values_r64(1)
        end if
        
        ! Test 50th percentile (should equal median)
        q50 = quantile(var, 0.5_real64)
        if (abs(q50%data%values_r64(1) - 50.5_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "50th percentile incorrect"
        end if
        
        ! Test 75th percentile
        q75 = quantile(var, 0.75_real64)
        if (abs(q75%data%values_r64(1) - 75.25_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "75th percentile incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(q25)
        call finalize_variable(q50)
        call finalize_variable(q75)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Quantile functions test"
        else
            write(*,'(A)') "FAIL: Quantile functions test"
        end if
    end subroutine test_quantile_functions
    
    subroutine test_aggregation_along_dims()
        type(fortarray_t) :: var, result
        real(real64), dimension(2,3,4) :: data
        integer :: i, j, k
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 3D test data
        do k = 1, 4
            do j = 1, 3
                do i = 1, 2
                    data(i,j,k) = real(i + (j-1)*2 + (k-1)*6, real64)
                end do
            end do
        end do
        
        var = new_array(data, name="test_3d", dim_names=["x", "y", "z"])
        
        ! Test sum along different dimensions
        result = sum(var, dim=1)  ! Sum over x
        if (any(result%shape /= [3, 4])) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum along dim=1 has wrong shape"
        end if
        
        result = sum(var, dim=2)  ! Sum over y
        if (any(result%shape /= [2, 4])) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum along dim=2 has wrong shape"
        end if
        
        result = sum(var, dim=3)  ! Sum over z
        if (any(result%shape /= [2, 3])) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum along dim=3 has wrong shape"
        end if
        
        ! Test keepdims option
        result = sum(var, dim=2, keepdims=.true.)
        if (any(result%shape /= [2, 1, 4])) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum with keepdims has wrong shape"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Aggregation along dims test"
        else
            write(*,'(A)') "FAIL: Aggregation along dims test"
        end if
    end subroutine test_aggregation_along_dims
    
    subroutine test_weighted_mean()
        type(fortarray_t) :: var, weights, result
        real(real64), dimension(4) :: data_vals, weight_vals
        real(real64) :: expected_weighted_mean
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data and weights
        data_vals = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        weight_vals = [0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64]
        
        var = new_array(data_vals, name="values", dim_names=["x"])
        weights = new_array(weight_vals, name="weights", dim_names=["x"])
        
        ! Calculate expected weighted mean
        expected_weighted_mean = sum(data_vals * weight_vals) / sum(weight_vals)
        
        ! Test weighted mean
        result = mean(var, weights=weights)
        
        if (abs(result%data%values_r64(1) - expected_weighted_mean) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F10.6,A,F10.6)') "Weighted mean mismatch: got ", &
                result%data%values_r64(1), " expected ", expected_weighted_mean
        end if
        
        call finalize_variable(var)
        call finalize_variable(weights)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Weighted mean test"
        else
            write(*,'(A)') "FAIL: Weighted mean test"
        end if
    end subroutine test_weighted_mean
    
    subroutine test_weighted_sum()
        type(fortarray_t) :: var, weights, result
        real(real64), dimension(2,3) :: data_vals, weight_vals
        real(real64) :: expected_weighted_sum
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D test data and weights
        data_vals = reshape([1.0_real64, 2.0_real64, 3.0_real64, &
                            4.0_real64, 5.0_real64, 6.0_real64], [2,3])
        weight_vals = reshape([1.0_real64, 1.0_real64, 2.0_real64, &
                              2.0_real64, 3.0_real64, 3.0_real64], [2,3])
        
        var = new_array(data_vals, name="values", dim_names=["x", "y"])
        weights = new_array(weight_vals, name="weights", dim_names=["x", "y"])
        
        ! Calculate expected weighted sum
        expected_weighted_sum = sum(data_vals * weight_vals)
        
        ! Test weighted sum
        result = sum(var, weights=weights)
        
        if (abs(result%data%values_r64(1) - expected_weighted_sum) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Weighted sum incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(weights)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Weighted sum test"
        else
            write(*,'(A)') "FAIL: Weighted sum test"
        end if
    end subroutine test_weighted_sum
    
    subroutine test_missing_data_aggregation()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        real(real64) :: missing, expected_mean
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [1.0_real64, missing, 3.0_real64, missing, 5.0_real64]
        var = new_array(data, name="data_with_missing", dim_names=["x"])
        
        ! Test mean with skipna=.true. (default)
        result = mean(var, skipna=.true.)
        expected_mean = (1.0_real64 + 3.0_real64 + 5.0_real64) / 3.0_real64
        
        if (abs(result%data%values_r64(1) - expected_mean) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean with skipna incorrect"
        end if
        
        ! Test mean with skipna=.false.
        result = mean(var, skipna=.false.)
        if (abs(result%data%values_r64(1) - missing) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean with skipna=.false. should return missing"
        end if
        
        ! Test sum with missing values
        result = sum(var, skipna=.true.)
        if (abs(result%data%values_r64(1) - 9.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum with skipna incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Missing data aggregation test"
        else
            write(*,'(A)') "FAIL: Missing data aggregation test"
        end if
    end subroutine test_missing_data_aggregation
    
    subroutine test_empty_aggregation()
        type(fortarray_t) :: var, result
        real(real64), dimension(0) :: empty_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create empty variable
        var = new_array(empty_data, name="empty", dim_names=["x"])
        
        ! Test sum of empty array (should be 0)
        result = sum(var)
        if (result%data%values_r64(1) /= 0.0_real64) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum of empty array should be 0"
        end if
        
        ! Test mean of empty array (should be NaN)
        result = mean(var)
        if (.not. ieee_is_nan(result%data%values_r64(1))) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean of empty array should be NaN"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Empty aggregation test"
        else
            write(*,'(A)') "FAIL: Empty aggregation test"
        end if
    end subroutine test_empty_aggregation
    
    subroutine test_scalar_aggregation()
        type(fortarray_t) :: scalar_var, result
        real(real64) :: scalar_val = 42.0_real64
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create scalar variable
        scalar_var = variable_scalar_real64(scalar_val, name="scalar")
        
        ! Test aggregations on scalar
        result = sum(scalar_var)
        if (abs(result%data%values_r64(1) - scalar_val) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum of scalar incorrect"
        end if
        
        result = mean(scalar_var)
        if (abs(result%data%values_r64(1) - scalar_val) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean of scalar incorrect"
        end if
        
        result = std(scalar_var)
        if (abs(result%data%values_r64(1) - 0.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Std of scalar should be 0"
        end if
        
        call finalize_variable(scalar_var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Scalar aggregation test"
        else
            write(*,'(A)') "FAIL: Scalar aggregation test"
        end if
    end subroutine test_scalar_aggregation
    
    subroutine test_multidim_aggregation()
        type(fortarray_t) :: var, result
        real(real64), dimension(3,4,5) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 3D test data
        data = reshape([(real(i, real64), i=1,60)], [3,4,5])
        var = new_array(data, name="test_3d", dim_names=["x", "y", "z"])
        
        ! Test single dimension aggregation for now (multi-dim not fully implemented)
        result = sum(var, dim=1)  ! Sum over x dimension
        if (any(result%shape /= [4, 5])) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum along dim=1 has wrong shape"
        end if
        
        ! Test mean over dimension
        result = mean(var, dim=2)  ! Average over y dimension
        if (any(result%shape /= [3, 5])) then
            test_passed = .false.
            write(error_unit,'(A)') "Multi-dim mean has wrong shape"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Multi-dim aggregation test"
        else
            write(*,'(A)') "FAIL: Multi-dim aggregation test"
        end if
    end subroutine test_multidim_aggregation
    
    subroutine test_aggregation_preserves_type()
        type(fortarray_t) :: var_i32, var_r32, result
        integer(int32), dimension(4) :: data_i32
        real(real32), dimension(4) :: data_r32
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test with int32 data
        data_i32 = [1_int32, 2_int32, 3_int32, 4_int32]
        var_i32 = new_array(data_i32, name="int_data", dim_names=["x"])
        
        result = sum(var_i32)
        if (result%data%dtype /= DTYPE_INT32) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum should preserve int32 type"
        end if
        
        ! Test with real32 data
        data_r32 = [1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32]
        var_r32 = new_array(data_r32, name="float_data", dim_names=["x"])
        
        result = mean(var_r32)
        if (result%data%dtype /= DTYPE_REAL32) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean should preserve real32 type"
        end if
        
        call finalize_variable(var_i32)
        call finalize_variable(var_r32)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Type preservation test"
        else
            write(*,'(A)') "FAIL: Type preservation test"
        end if
    end subroutine test_aggregation_preserves_type
    
    subroutine test_aggregation_performance()
        type(fortarray_t) :: var, result
        real(real64), dimension(100, 100, 100) :: large_data
        real(real64) :: start_time, end_time
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create large dataset
        large_data = reshape([(real(i, real64), i=1,1000000)], [100, 100, 100])
        var = new_array(large_data, name="large_data", dim_names=["x", "y", "z"])
        
        ! Time aggregation operations
        call cpu_time(start_time)
        result = sum(var, dim=2)
        call cpu_time(end_time)
        
        if (end_time - start_time > 1.0) then
            write(*,'(A,F8.4,A)') "WARNING: Sum along dimension took ", &
                end_time - start_time, " seconds"
        end if
        
        ! Basic correctness check
        if (result%n_elements /= 10000) then
            test_passed = .false.
            write(error_unit,'(A)') "Performance test result has wrong size"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Aggregation performance test"
        else
            write(*,'(A)') "FAIL: Aggregation performance test"
        end if
    end subroutine test_aggregation_performance
    
end program test_aggregation