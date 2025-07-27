program test_boolean_indexing_simple
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Boolean Indexing Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run basic tests
    call test_create_logical_mask()
    call test_apply_basic_mask()
    call test_mask_with_fill_value()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Boolean Indexing Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some boolean indexing tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All boolean indexing tests passed!"
    end if

contains

    subroutine test_create_logical_mask()
        type(fortarray_t) :: var, mask
        real(real64), dimension(5) :: data
        logical, dimension(5) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Create logical mask manually
        mask_data = [.true., .false., .true., .false., .true.]
        mask = create_mask(mask_data, ["x"])
        
        if (mask%data%dtype /= DTYPE_LOGICAL) then
            test_passed = .false.
            write(error_unit,'(A)') "Mask should have logical dtype"
        end if
        
        if (mask%n_elements /= 5) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Mask should have 5 elements, got: ", mask%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(mask)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Create logical mask"
        else
            write(*,'(A)') "FAIL: Create logical mask"
        end if
    end subroutine test_create_logical_mask
    
    subroutine test_apply_basic_mask()
        type(fortarray_t) :: var, mask, result
        real(real64), dimension(5) :: data
        logical, dimension(5) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        mask_data = [.false., .true., .false., .true., .true.]
        mask = create_mask(mask_data, ["x"])
        
        ! Apply mask
        result = where_boolean(mask, var)
        
        ! Should return values [2, 4, 5]
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 2.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 4.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 5.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(mask)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Apply basic mask"
        else
            write(*,'(A)') "FAIL: Apply basic mask"
        end if
    end subroutine test_apply_basic_mask
    
    subroutine test_mask_with_fill_value()
        type(fortarray_t) :: var, mask, result
        real(real64), dimension(5) :: data
        logical, dimension(5) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        mask_data = [.true., .false., .true., .false., .true.]
        mask = create_mask(mask_data, ["x"])
        
        ! Apply mask with fill value
        result = where_boolean(mask, var, -99.0_real64)
        
        ! Should return [1, -99, 3, -99, 5]
        if (result%n_elements /= 5) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 5 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - (-99.0_real64)) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(4) - (-99.0_real64)) > 1e-10 .or. &
                abs(result%data%values_r64(5) - 5.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(mask)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mask with fill value"
        else
            write(*,'(A)') "FAIL: Mask with fill value"
        end if
    end subroutine test_mask_with_fill_value
    
end program test_boolean_indexing_simple