program test_missing_data_simple
    use foxel
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Missing Data Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run simplified tests
    call test_isnull_basic()
    call test_fillna_basic()
    call test_dropna_basic()
    
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

    subroutine test_isnull_basic()
        type(variable_t) :: var, null_mask
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
        
        ! Check results
        if (.not. null_mask%data%values_logical(2) .or. &
            .not. null_mask%data%values_logical(4) .or. &
            null_mask%data%values_logical(1) .or. &
            null_mask%data%values_logical(3) .or. &
            null_mask%data%values_logical(5)) then
            test_passed = .false.
            write(error_unit,'(A)') "isnull detection incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(null_mask)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: isnull basic test"
        else
            write(*,'(A)') "FAIL: isnull basic test"
        end if
    end subroutine test_isnull_basic
    
    subroutine test_fillna_basic()
        type(variable_t) :: var, result
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
            write(*,'(A)') "PASS: fillna basic test"
        else
            write(*,'(A)') "FAIL: fillna basic test"
        end if
    end subroutine test_fillna_basic
    
    subroutine test_dropna_basic()
        type(variable_t) :: var, result
        real(real64), dimension(5) :: data
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data = [1.0_real64, missing, 3.0_real64, missing, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Drop missing values
        result = dropna(var)
        
        ! Check results
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Dropna wrong size: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 5.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Dropna values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: dropna basic test"
        else
            write(*,'(A)') "FAIL: dropna basic test"
        end if
    end subroutine test_dropna_basic
    
end program test_missing_data_simple