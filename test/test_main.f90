program test_main
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    
    integer :: total_passed, total_tests
    integer :: status
    character(len=100) :: test_name
    
    total_passed = 0
    total_tests = 0
    
    write(output_unit, '(A)') "="//repeat("=", 70)
    write(output_unit, '(A)') "Running Foxel Test Suite"
    write(output_unit, '(A)') "="//repeat("=", 70)
    
    ! Run each test program and collect results
    
    ! Test 1: Basic type tests
    test_name = "test_variable_type"
    call run_test(test_name, status)
    call update_stats(test_name, status, total_tests, total_passed)
    
    ! Test 2: Finalizer tests
    test_name = "test_finalizers"
    call run_test(test_name, status)
    call update_stats(test_name, status, total_tests, total_passed)
    
    ! Summary
    write(output_unit, '(A)') "="//repeat("=", 70)
    write(output_unit, '(A,I0,A,I0,A)') "Total: ", total_passed, " / ", total_tests, " test programs passed"
    
    if (total_passed /= total_tests) then
        write(error_unit, '(A)') "FAILURE: Some tests failed!"
        error stop 1
    else
        write(output_unit, '(A)') "SUCCESS: All tests passed!"
    end if
    
contains

    subroutine run_test(test_name, status)
        character(len=*), intent(in) :: test_name
        integer, intent(out) :: status
        character(len=200) :: cmd
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A,A,A)') "Running ", trim(test_name), "..."
        
        ! Build command to run test
        write(cmd, '(A,A)') "./build/*/test/", trim(test_name)
        call execute_command_line(cmd, exitstat=status, wait=.true.)
    end subroutine run_test
    
    subroutine update_stats(test_name, status, total_tests, total_passed)
        character(len=*), intent(in) :: test_name
        integer, intent(in) :: status
        integer, intent(inout) :: total_tests, total_passed
        
        total_tests = total_tests + 1
        if (status == 0) then
            total_passed = total_passed + 1
            write(output_unit, '(A,A,A)') "[PASS] ", trim(test_name), " completed successfully"
        else
            write(error_unit, '(A,A,A,I0)') "[FAIL] ", trim(test_name), " failed with status ", status
        end if
    end subroutine update_stats

end program test_main