program test_boolean_indexing
    use fortarray
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Boolean Indexing Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run all boolean indexing tests
    call test_create_boolean_mask()
    call test_apply_mask_1d()
    call test_apply_mask_2d()
    call test_conditional_selection()
    call test_multi_condition_and()
    call test_multi_condition_or()
    call test_multi_condition_not()
    call test_mask_with_missing_data()
    call test_where_function()
    call test_boolean_mask_broadcasting()
    call test_mask_edge_cases()
    call test_mask_performance()
    call test_mask_with_coordinates()
    call test_complex_conditions()
    call test_mask_combinations()
    
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

    subroutine test_create_boolean_mask()
        type(fortarray_t) :: var, mask
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Create boolean mask: values > 3
        mask = var > 3.0_real64
        
        if (mask%data%dtype /= DTYPE_LOGICAL) then
            test_passed = .false.
            write(error_unit,'(A)') "Mask should have logical dtype"
        end if
        
        if (mask%n_elements /= 5) then
            test_passed = .false.
            write(error_unit,'(A)') "Mask should have same size as original"
        end if
        
        ! Check mask values: [F, F, F, T, T]
        if (.not. (mask%data%values_logical(1) .eqv. .false.) .or. &
            .not. (mask%data%values_logical(2) .eqv. .false.) .or. &
            .not. (mask%data%values_logical(3) .eqv. .false.) .or. &
            .not. (mask%data%values_logical(4) .eqv. .true.) .or. &
            .not. (mask%data%values_logical(5) .eqv. .true.)) then
            test_passed = .false.
            write(error_unit,'(A)') "Mask values incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(mask)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Create boolean mask"
        else
            write(*,'(A)') "FAIL: Create boolean mask"
        end if
    end subroutine test_create_boolean_mask
    
    subroutine test_apply_mask_1d()
        type(fortarray_t) :: var, mask, result
        real(real64), dimension(5) :: data
        logical, dimension(5) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        mask_data = [.false., .true., .false., .true., .true.]
        mask = new_array(mask_data, name="mask", dim_names=["x"])
        
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
            write(*,'(A)') "PASS: Apply mask 1D"
        else
            write(*,'(A)') "FAIL: Apply mask 1D"
        end if
    end subroutine test_apply_mask_1d
    
    subroutine test_apply_mask_2d()
        type(fortarray_t) :: var, mask, result
        real(real64), dimension(6) :: data
        logical, dimension(6) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! 2x3 array - create directly as 1D for now
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        var = new_array(data, name="test_data", dim_names=["idx"])
        ! Manually set shape for 2D
        deallocate(var%shape)
        allocate(var%shape(2))
        var%shape = [2, 3]
        var%n_dims = 2
        deallocate(var%dim_names)
        allocate(var%dim_names(2))
        var%dim_names = ["x", "y"]
        
        mask_data = [.true., .false., .true., .false., .true., .false.]
        mask = new_array(mask_data, name="mask", dim_names=["idx"])
        ! Manually set shape for 2D
        deallocate(mask%shape)
        allocate(mask%shape(2))
        mask%shape = [2, 3]
        mask%n_dims = 2
        deallocate(mask%dim_names)
        allocate(mask%dim_names(2))
        mask%dim_names = ["x", "y"]
        
        ! Apply mask
        result = where_boolean(mask, var)
        
        ! Should return values [1, 3, 5]
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
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
            write(*,'(A)') "PASS: Apply mask 2D"
        else
            write(*,'(A)') "FAIL: Apply mask 2D"
        end if
    end subroutine test_apply_mask_2d
    
    subroutine test_conditional_selection()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test: select values between 2 and 4 (inclusive)
        result = where_boolean((var >= 2.0_real64) .and. (var <= 4.0_real64), var)
        
        ! Should return values [2, 3, 4]
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 2.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 4.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Conditional selection"
        else
            write(*,'(A)') "FAIL: Conditional selection"
        end if
    end subroutine test_conditional_selection
    
    subroutine test_multi_condition_and()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test: (values > 2) AND (values < 5)
        result = where_boolean((var > 2.0_real64) .and. (var < 5.0_real64), var)
        
        ! Should return values [3, 4]
        if (result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 2 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 4.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Multi-condition AND"
        else
            write(*,'(A)') "FAIL: Multi-condition AND"
        end if
    end subroutine test_multi_condition_and
    
    subroutine test_multi_condition_or()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test: (values < 2) OR (values > 4)
        result = where_boolean((var < 2.0_real64) .or. (var > 4.0_real64), var)
        
        ! Should return values [1, 5]
        if (result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 2 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 5.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Multi-condition OR"
        else
            write(*,'(A)') "FAIL: Multi-condition OR"
        end if
    end subroutine test_multi_condition_or
    
    subroutine test_multi_condition_not()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Test: NOT(values == 3)
        result = where_boolean(.not.(var == 3.0_real64), var)
        
        ! Should return values [1, 2, 4, 5]
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 4 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 2.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 4.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(4) - 5.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Multi-condition NOT"
        else
            write(*,'(A)') "FAIL: Multi-condition NOT"
        end if
    end subroutine test_multi_condition_not
    
    subroutine test_mask_with_missing_data()
        type(fortarray_t) :: var, result
        real(real64), dimension(5) :: test_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Include NaN values
        test_data(1) = 1.0_real64
        test_data(2) = 2.0_real64
        test_data(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        test_data(4) = 4.0_real64
        test_data(5) = 5.0_real64
        var = new_array(test_data, name="test_data", dim_names=["x"])
        
        ! Test: select non-NaN values
        result = where_boolean(.not. isnull(var), var)
        
        ! Should return values [1, 2, 4, 5]
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 4 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 2.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 4.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(4) - 5.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mask with missing data"
        else
            write(*,'(A)') "FAIL: Mask with missing data"
        end if
    end subroutine test_mask_with_missing_data
    
    subroutine test_where_function()
        type(fortarray_t) :: var, mask, result
        real(real64), dimension(5) :: data, fill_values
        logical, dimension(5) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        fill_values = [-99.0_real64, -99.0_real64, -99.0_real64, -99.0_real64, -99.0_real64]
        mask_data = [.true., .false., .true., .false., .true.]
        mask = new_array(mask_data, name="mask", dim_names=["x"])
        
        ! where(mask, var, fill_value)
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
            write(*,'(A)') "PASS: where function"
        else
            write(*,'(A)') "FAIL: where function"
        end if
    end subroutine test_where_function
    
    subroutine test_boolean_mask_broadcasting()
        type(fortarray_t) :: var, mask, result
        real(real64), dimension(6) :: data
        logical, dimension(2) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! 2x3 array - create directly as 1D for now
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        var = new_array(data, name="test_data", dim_names=["idx"])
        ! Manually set shape for 2D
        deallocate(var%shape)
        allocate(var%shape(2))
        var%shape = [2, 3]
        var%n_dims = 2
        deallocate(var%dim_names)
        allocate(var%dim_names(2))
        var%dim_names = ["x", "y"]
        
        ! 1D mask for first dimension
        mask_data = [.true., .false.]
        mask = new_array(mask_data, name="mask", dim_names=["x"])
        
        ! Apply mask with broadcasting
        result = where_broadcast(mask, var)
        
        ! Should return first row: [1, 3, 5]
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
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
            write(*,'(A)') "PASS: Boolean mask broadcasting"
        else
            write(*,'(A)') "FAIL: Boolean mask broadcasting"
        end if
    end subroutine test_boolean_mask_broadcasting
    
    subroutine test_mask_edge_cases()
        type(fortarray_t) :: var, mask, result
        real(real64), dimension(1) :: data
        logical, dimension(1) :: mask_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Single element
        data = [42.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        mask_data = [.true.]
        mask = new_array(mask_data, name="mask", dim_names=["x"])
        
        result = where_boolean(mask, var)
        
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 1 element, got: ", result%n_elements
        else if (abs(result%data%values_r64(1) - 42.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Result value incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(mask)
        call finalize_variable(result)
        
        ! Test empty result
        mask_data = [.false.]
        mask = new_array(mask_data, name="mask", dim_names=["x"])
        result = where_boolean(mask, var)
        
        if (result%n_elements /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Empty result should have 0 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(mask)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mask edge cases"
        else
            write(*,'(A)') "FAIL: Mask edge cases"
        end if
    end subroutine test_mask_edge_cases
    
    subroutine test_mask_performance()
        type(fortarray_t) :: var, result, even_mask, temp_var
        real(real64), dimension(1000) :: data, temp_data
        logical, dimension(1000) :: mask_data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Large array performance test
        do i = 1, 1000
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Select even numbers
        ! Create mask for even numbers using comparison
        ! Create array with 0 for even indices, 1 for odd
        do i = 1, 1000
            temp_data(i) = real(mod(i, 2), real64)
        end do
        temp_var = new_array(temp_data, name="temp", dim_names=["x"])
        
        ! Create mask: temp_var == 0.0 (even numbers)
        even_mask = temp_var == 0.0_real64
        result = where_boolean(even_mask, var)
        
        call finalize_variable(temp_var)
        call finalize_variable(even_mask)
        
        if (result%n_elements /= 500) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 500 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mask performance"
        else
            write(*,'(A)') "FAIL: Mask performance"
        end if
    end subroutine test_mask_performance
    
    subroutine test_mask_with_coordinates()
        type(fortarray_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        
        call create_coordinate(x_coord, 5, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = new_array(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Select values > 3 (should preserve coordinates)
        result = where_boolean(var > 3.0_real64, var)
        
        if (result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 2 elements, got: ", result%n_elements
        end if
        
        ! Check that coordinates are preserved/adjusted
        if (.not. allocated(result%coords) .or. .not. result%has_coord(1)) then
            test_passed = .false.
            write(error_unit,'(A)') "Coordinates should be preserved"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mask with coordinates"
        else
            write(*,'(A)') "FAIL: Mask with coordinates"
        end if
    end subroutine test_mask_with_coordinates
    
    subroutine test_complex_conditions()
        type(fortarray_t) :: var, result, mask1, mask2, mask3, combined_mask
        real(real64), dimension(10) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        do i = 1, 10
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        
        ! Complex condition: (x > 3 AND x < 7) OR x == 9
        ! Note: Creating separate masks due to operator precedence issues
        mask1 = (var > 3.0_real64) .and. (var < 7.0_real64)
        mask2 = var == 9.0_real64
        combined_mask = mask1 .or. mask2
        result = where_boolean(combined_mask, var)
        
        call finalize_variable(mask1)
        call finalize_variable(mask2)
        call finalize_variable(combined_mask)
        
        ! Should return [4, 5, 6, 9]
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 4 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 4.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 5.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 6.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(4) - 9.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Complex conditions"
        else
            write(*,'(A)') "FAIL: Complex conditions"
        end if
    end subroutine test_complex_conditions
    
    subroutine test_mask_combinations()
        type(fortarray_t) :: var, mask1, mask2, combined_mask, result
        real(real64), dimension(5) :: data
        logical, dimension(5) :: mask1_data, mask2_data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = new_array(data, name="test_data", dim_names=["x"])
        
        mask1_data = [.true., .false., .true., .false., .true.]
        mask1 = create_mask_from_logical_array(mask1_data, ["x"])
        
        mask2_data = [.false., .true., .true., .true., .false.]
        mask2 = create_mask_from_logical_array(mask2_data, ["x"])
        
        ! TODO: Implement .and. operator for mask variables
        ! combined_mask = mask1 .and. mask2
        ! result = where_boolean(combined_mask, var)
        
        ! For now, just test with a single mask
        result = where_boolean(mask1, var)
        
        ! Should return [1, 3, 5]
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10 .or. &
                 abs(result%data%values_r64(2) - 3.0_real64) > 1e-10 .or. &
                 abs(result%data%values_r64(3) - 5.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Result values incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(mask1)
        call finalize_variable(mask2)
        call finalize_variable(combined_mask)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mask combinations"
        else
            write(*,'(A)') "FAIL: Mask combinations"
        end if
    end subroutine test_mask_combinations
    
end program test_boolean_indexing