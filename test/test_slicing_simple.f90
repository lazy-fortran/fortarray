program test_slicing_simple
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Slicing Operations Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run basic tests
    call test_basic_slice()
    call test_negative_slice()
    call test_step_slice()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Slicing Operations Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some slicing operations tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All slicing operations tests passed!"
    end if

contains

    subroutine test_basic_slice()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test basic slice: [2:4]
        result = slice_range(var, 2, 4)
        
        ! Should return [2, 3, 4]
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 2.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 4.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Basic slice result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic slice"
        else
            write(*,'(A)') "FAIL: Basic slice"
        end if
    end subroutine test_basic_slice
    
    subroutine test_negative_slice()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test negative slice: last 2 elements
        result = slice_from_negative(var, -2)
        
        ! Should return [4, 5]
        if (result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 2 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 4.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 5.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Negative slice result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Negative slice"
        else
            write(*,'(A)') "FAIL: Negative slice"
        end if
    end subroutine test_negative_slice
    
    subroutine test_step_slice()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test step slice: [1:5:2] (every 2nd element)
        result = slice_with_step(var, 1, 5, 2)
        
        ! Should return [1, 3, 5]
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 5.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Step slice result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Step slice"
        else
            write(*,'(A)') "FAIL: Step slice"
        end if
    end subroutine test_step_slice
    
end program test_slicing_simple