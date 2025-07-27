program test_missing_data
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Missing Data Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run tests
    call test_fill_value_semantics()
    call test_where_masking()
    call test_fillna_constant()
    call test_fillna_forward()
    call test_fillna_backward()
    call test_fillna_interpolate()
    call test_dropna_1d()
    call test_dropna_2d()
    call test_dropna_along_dim()
    call test_isnull_isvalid()
    call test_missing_in_calculations()
    call test_missing_propagation()
    call test_missing_with_aggregations()
    call test_nan_handling()
    call test_mixed_missing_types()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Missing Data Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some missing data tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All missing data tests passed!"
    end if

contains

    subroutine test_fill_value_semantics()
        type(fortarray_t) :: var
        real(real64), dimension(5) :: data
        real(real64) :: fill_val
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with specific fill value
        fill_val = -999.0_real64
        data = [1.0_real64, fill_val, 3.0_real64, fill_val, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        var%has_fill_value = .true.
        var%fill_value_r64 = fill_val
        
        ! Check fill value is stored
        if (abs(var%fill_value_r64 - fill_val) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Fill value not stored correctly"
        end if
        
        ! Check isnull detection
        block
            type(fortarray_t) :: null_mask
            null_mask = isnull(var)
            if (.not. null_mask%data%values_logical(2) .or. &
                .not. null_mask%data%values_logical(4)) then
                test_passed = .false.
                write(error_unit,'(A)') "Fill value not detected as null"
            end if
            call finalize_variable(null_mask)
        end block
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Fill value semantics test"
        else
            write(*,'(A)') "FAIL: Fill value semantics test"
        end if
    end subroutine test_fill_value_semantics
    
    subroutine test_where_masking()
        type(fortarray_t) :: var, mask, result
        real(real64), dimension(6) :: data
        logical, dimension(6) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Create mask (select even values)
        mask_data = [.false., .true., .false., .true., .false., .true.]
        ! Create mask variable manually
        mask%name = "mask"
        mask%n_elements = 6
        mask%n_dims = 1
        allocate(mask%shape(1))
        mask%shape = [6]
        allocate(mask%dim_names(1))
        mask%dim_names = ["x"]
        mask%data%dtype = DTYPE_LOGICAL
        allocate(mask%data%values_logical(6))
        mask%data%values_logical = mask_data
        mask%initialized = .true.
        
        ! Apply where masking
        result = where(mask, var, huge(1.0_real64))
        
        ! Check results
        if (abs(result%data%values_r64(1) - huge(1.0_real64)) > 1e-10 .or. &
            abs(result%data%values_r64(2) - 2.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(3) - huge(1.0_real64)) > 1e-10 .or. &
            abs(result%data%values_r64(4) - 4.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Where masking incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(mask)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Where masking test"
        else
            write(*,'(A)') "FAIL: Where masking test"
        end if
    end subroutine test_where_masking
    
    subroutine test_fillna_constant()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [1.0_real64, missing, 3.0_real64, missing, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Fill missing with constant
        result = fillna(var, 0.0_real64)
        
        ! Check results
        if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(2) - 0.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(3) - 3.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(4) - 0.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(5) - 5.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Fillna constant incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Fillna constant test"
        else
            write(*,'(A)') "FAIL: Fillna constant test"
        end if
    end subroutine test_fillna_constant
    
    subroutine test_fillna_forward()
        type(fortarray_t) :: var, result
        real(real64), dimension(7) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [missing, 2.0_real64, missing, 4.0_real64, missing, missing, 7.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Forward fill
        result = fillna(var, method="forward")
        
        ! Check results
        if (abs(result%data%values_r64(1) - missing) > 1e-10 .or. &  ! First stays missing
            abs(result%data%values_r64(2) - 2.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(3) - 2.0_real64) > 1e-10 .or. &  ! Filled with previous
            abs(result%data%values_r64(4) - 4.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(5) - 4.0_real64) > 1e-10 .or. &  ! Filled with previous
            abs(result%data%values_r64(6) - 4.0_real64) > 1e-10) then   ! Filled with previous
            test_passed = .false.
            write(error_unit,'(A)') "Fillna forward incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Fillna forward test"
        else
            write(*,'(A)') "FAIL: Fillna forward test"
        end if
    end subroutine test_fillna_forward
    
    subroutine test_fillna_backward()
        type(fortarray_t) :: var, result
        real(real64), dimension(7) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [1.0_real64, missing, missing, 4.0_real64, missing, 6.0_real64, missing]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Backward fill
        result = fillna(var, method="backward")
        
        ! Check results
        if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(2) - 4.0_real64) > 1e-10 .or. &  ! Filled from next valid
            abs(result%data%values_r64(3) - 4.0_real64) > 1e-10 .or. &  ! Filled from next valid
            abs(result%data%values_r64(4) - 4.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(5) - 6.0_real64) > 1e-10 .or. &  ! Filled from next valid
            abs(result%data%values_r64(6) - 6.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(7) - missing) > 1e-10) then      ! Last stays missing
            test_passed = .false.
            write(error_unit,'(A)') "Fillna backward incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Fillna backward test"
        else
            write(*,'(A)') "FAIL: Fillna backward test"
        end if
    end subroutine test_fillna_backward
    
    subroutine test_fillna_interpolate()
        type(fortarray_t) :: var, result
        real(real64), dimension(7) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [1.0_real64, missing, missing, 4.0_real64, missing, 10.0_real64, missing]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Linear interpolation
        result = fillna(var, method="linear")
        
        ! Check results
        if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(2) - 2.0_real64) > 1e-10 .or. &  ! Linear interp
            abs(result%data%values_r64(3) - 3.0_real64) > 1e-10 .or. &  ! Linear interp
            abs(result%data%values_r64(4) - 4.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(5) - 7.0_real64) > 1e-10 .or. &  ! Linear interp
            abs(result%data%values_r64(6) - 10.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Fillna interpolate incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Fillna interpolate test"
        else
            write(*,'(A)') "FAIL: Fillna interpolate test"
        end if
    end subroutine test_fillna_interpolate
    
    subroutine test_dropna_1d()
        type(fortarray_t) :: var, result
        real(real64), dimension(6) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [1.0_real64, missing, 3.0_real64, missing, 5.0_real64, 6.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Drop missing values
        result = dropna(var)
        
        ! Check results
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Dropna 1D wrong size"
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 5.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(4) - 6.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Dropna 1D values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dropna 1D test"
        else
            write(*,'(A)') "FAIL: Dropna 1D test"
        end if
    end subroutine test_dropna_1d
    
    subroutine test_dropna_2d()
        type(fortarray_t) :: var, result
        real(real64), dimension(3,4) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create 2D data with missing values
        data = reshape([1.0_real64, 2.0_real64, 3.0_real64, &
                       missing, 5.0_real64, 6.0_real64, &
                       7.0_real64, 8.0_real64, 9.0_real64, &
                       10.0_real64, missing, 12.0_real64], [3,4])
        var = variable(data, name="test_data", dim_names=["x", "y"])
        
        ! Drop rows with any missing
        result = dropna(var, dim=1, how="any")
        
        ! Should drop row 2 (index 2), keeping rows 1 and 3
        if (any(result%shape /= [2, 4])) then
            test_passed = .false.
            write(error_unit,'(A)') "Dropna 2D wrong shape"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dropna 2D test"
        else
            write(*,'(A)') "FAIL: Dropna 2D test"
        end if
    end subroutine test_dropna_2d
    
    subroutine test_dropna_along_dim()
        type(fortarray_t) :: var, result
        real(real64), dimension(3,4) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create 2D data with missing values
        data = reshape([1.0_real64, missing, 3.0_real64, &
                       4.0_real64, 5.0_real64, missing, &
                       7.0_real64, 8.0_real64, 9.0_real64, &
                       10.0_real64, 11.0_real64, 12.0_real64], [3,4])
        var = variable(data, name="test_data", dim_names=["x", "y"])
        
        ! Drop columns with any missing
        result = dropna(var, dim=2, how="any")
        
        ! Should drop columns 1 and 2 (indices 1 and 2), keeping columns 3 and 4
        if (any(result%shape /= [3, 2])) then
            test_passed = .false.
            write(error_unit,'(A)') "Dropna along dim wrong shape"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dropna along dim test"
        else
            write(*,'(A)') "FAIL: Dropna along dim test"
        end if
    end subroutine test_dropna_along_dim
    
    subroutine test_isnull_isvalid()
        type(fortarray_t) :: var, null_mask, valid_mask
        real(real64), dimension(5) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [1.0_real64, missing, 3.0_real64, missing, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test isnull
        null_mask = isnull(var)
        if (.not. null_mask%data%values_logical(2) .or. &
            .not. null_mask%data%values_logical(4) .or. &
            null_mask%data%values_logical(1) .or. &
            null_mask%data%values_logical(3) .or. &
            null_mask%data%values_logical(5)) then
            test_passed = .false.
            write(error_unit,'(A)') "isnull incorrect"
        end if
        
        ! Test isvalid
        valid_mask = isvalid(var)
        if (valid_mask%data%values_logical(2) .or. &
            valid_mask%data%values_logical(4) .or. &
            .not. valid_mask%data%values_logical(1) .or. &
            .not. valid_mask%data%values_logical(3) .or. &
            .not. valid_mask%data%values_logical(5)) then
            test_passed = .false.
            write(error_unit,'(A)') "isvalid incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(null_mask)
        call finalize_variable(valid_mask)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: isnull/isvalid test"
        else
            write(*,'(A)') "FAIL: isnull/isvalid test"
        end if
    end subroutine test_isnull_isvalid
    
    subroutine test_missing_in_calculations()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(4) :: data1, data2
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data1 = [1.0_real64, missing, 3.0_real64, 4.0_real64]
        data2 = [5.0_real64, 6.0_real64, missing, 8.0_real64]
        
        var1 = variable(data1, name="data1", dim_names=["x"])
        var2 = variable(data2, name="data2", dim_names=["x"])
        
        ! Test arithmetic with missing values
        result = var1 + var2
        
        ! Check that missing propagates
        if (abs(result%data%values_r64(1) - 6.0_real64) > 1e-10 .or. &
            abs(result%data%values_r64(2) - missing) < 1e-10 .or. &  ! Should be missing
            abs(result%data%values_r64(3) - missing) < 1e-10 .or. &  ! Should be missing
            abs(result%data%values_r64(4) - 12.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Missing value propagation incorrect"
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Missing in calculations test"
        else
            write(*,'(A)') "FAIL: Missing in calculations test"
        end if
    end subroutine test_missing_in_calculations
    
    subroutine test_missing_propagation()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with one missing value
        data = [1.0_real64, 2.0_real64, missing, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Operations should propagate missing
        result = var * 2.0_real64
        
        if (abs(result%data%values_r64(3) - missing) < 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Missing should propagate in multiplication"
        end if
        
        result = var + 10.0_real64
        
        if (abs(result%data%values_r64(3) - missing) < 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Missing should propagate in addition"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Missing propagation test"
        else
            write(*,'(A)') "FAIL: Missing propagation test"
        end if
    end subroutine test_missing_propagation
    
    subroutine test_missing_with_aggregations()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [1.0_real64, missing, 3.0_real64, missing, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test aggregations with skipna=.true. (default)
        result = mean(var)
        if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean with skipna incorrect"
        end if
        
        ! Test aggregations with skipna=.false.
        result = mean(var, skipna=.false.)
        if (abs(result%data%values_r64(1) - missing) < 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean without skipna should return missing"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Missing with aggregations test"
        else
            write(*,'(A)') "FAIL: Missing with aggregations test"
        end if
    end subroutine test_missing_with_aggregations
    
    subroutine test_nan_handling()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        real(real64) :: nan_val
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        nan_val = ieee_value(1.0_real64, ieee_quiet_nan)
        
        ! Create data with NaN values
        data = [1.0_real64, nan_val, 3.0_real64, nan_val, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test NaN detection
        result = isnull(var)
        if (.not. result%data%values_logical(2) .or. &
            .not. result%data%values_logical(4)) then
            test_passed = .false.
            write(error_unit,'(A)') "NaN not detected as null"
        end if
        
        ! Test fillna with NaN
        result = fillna(var, 0.0_real64)
        if (ieee_is_nan(result%data%values_r64(2)) .or. &
            ieee_is_nan(result%data%values_r64(4))) then
            test_passed = .false.
            write(error_unit,'(A)') "NaN not filled correctly"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: NaN handling test"
        else
            write(*,'(A)') "FAIL: NaN handling test"
        end if
    end subroutine test_nan_handling
    
    subroutine test_mixed_missing_types()
        type(fortarray_t) :: var
        real(real64), dimension(6) :: data
        real(real64) :: missing, nan_val, inf_val
        type(fortarray_t) :: null_mask
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        nan_val = ieee_value(1.0_real64, ieee_quiet_nan)
        inf_val = ieee_value(1.0_real64, ieee_positive_inf)
        
        ! Create data with different types of missing/special values
        data = [1.0_real64, missing, nan_val, 4.0_real64, inf_val, 6.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test that huge() and NaN are detected as missing, but not infinity
        null_mask = isnull(var)
        if (.not. null_mask%data%values_logical(2) .or. &  ! huge() should be null
            .not. null_mask%data%values_logical(3) .or. &  ! NaN should be null
            null_mask%data%values_logical(5)) then         ! Inf should NOT be null
            test_passed = .false.
            write(error_unit,'(A)') "Mixed missing type detection incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(null_mask)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mixed missing types test"
        else
            write(*,'(A)') "FAIL: Mixed missing types test"
        end if
    end subroutine test_mixed_missing_types
    
end program test_missing_data