program test_apply_simple
    use foxel
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Apply Functions Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run basic tests
    call test_apply_along_dim()
    call test_apply_vectorized()
    call test_apply_cumulative()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Apply Functions Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some apply functions tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All apply functions tests passed!"
    end if

contains

    subroutine test_apply_along_dim()
        type(variable_t) :: var, result
        real(real64), dimension(6) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2x3 test data
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        var = variable(data, name="test_data", dim_names=["x", "y"])
        var%shape = [2, 3]
        
        ! Test apply sum along first dimension
        result = apply_along_dim(var, 1, "sum")
        
        ! Should result in 3 values (sum of each column)
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            ! First column sum should be 1+2 = 3
            if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A,F0.1)') "First column sum should be 3, got: ", result%data%values_r64(1)
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Apply along dimension"
        else
            write(*,'(A)') "FAIL: Apply along dimension"
        end if
    end subroutine test_apply_along_dim
    
    subroutine test_apply_vectorized()
        type(variable_t) :: var, result
        real(real64), dimension(4) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 4.0_real64, 9.0_real64, 16.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test vectorized square root
        result = apply_vectorized(var, "sqrt")
        
        ! Should give [1, 2, 3, 4]
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 4 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 2.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(4) - 4.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Sqrt results incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Apply vectorized"
        else
            write(*,'(A)') "FAIL: Apply vectorized"
        end if
    end subroutine test_apply_vectorized
    
    subroutine test_apply_cumulative()
        type(variable_t) :: var, result
        real(real64), dimension(4) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test cumulative sum
        result = apply_cumulative(var, "cumsum")
        
        ! Should give [1, 3, 6, 10]
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 4 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 6.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(4) - 10.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Cumsum results incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Apply cumulative"
        else
            write(*,'(A)') "FAIL: Apply cumulative"
        end if
    end subroutine test_apply_cumulative
    
end program test_apply_simple