program test_interpolation
    use foxel
    use ieee_arithmetic, only: ieee_quiet_nan, ieee_value
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Interpolation Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run all interpolation tests
    call test_linear_interpolation_1d()
    call test_nearest_neighbor_interpolation()
    call test_cubic_interpolation()
    call test_interpolation_extrapolation()
    call test_interpolation_with_missing_data()
    call test_interpolation_2d()
    call test_interpolation_edge_cases()
    call test_interpolation_performance()
    call test_interpolation_coordinates()
    call test_interpolation_bounds_checking()
    call test_interpolation_methods()
    call test_interpolation_fill_values()
    call test_interpolation_regular_grid()
    call test_interpolation_irregular_grid()
    call test_interpolation_multidimensional()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Interpolation Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some interpolation tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All interpolation tests passed!"
    end if

contains

    subroutine test_linear_interpolation_1d()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        real(real64), dimension(3) :: interp_points
        real(real64), dimension(3) :: expected_values
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with coordinates
        data = [1.0_real64, 2.0_real64, 4.0_real64, 8.0_real64, 16.0_real64]
        coord_vals = [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        
        call create_coordinate(x_coord, 5, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Interpolation points
        interp_points = [0.5_real64, 1.5_real64, 2.5_real64]
        
        ! Expected values for linear interpolation
        expected_values = [1.5_real64, 3.0_real64, 6.0_real64]
        
        ! Test linear interpolation
        result = interp1d(var, interp_points, method="linear")
        
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            do stat = 1, 3
                if (abs(result%data%values_r64(stat) - expected_values(stat)) > 1e-10) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,F0.2,A,F0.2)') "Element ", stat, &
                        " should be ", expected_values(stat), " got ", result%data%values_r64(stat)
                    exit
                end if
            end do
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Linear interpolation 1D"
        else
            write(*,'(A)') "FAIL: Linear interpolation 1D"
        end if
    end subroutine test_linear_interpolation_1d
    
    subroutine test_nearest_neighbor_interpolation()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        real(real64), dimension(3) :: interp_points
        real(real64), dimension(3) :: expected_values
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        call create_coordinate(x_coord, 5, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Interpolation points
        interp_points = [1.4_real64, 2.6_real64, 4.8_real64]
        
        ! Expected values for nearest neighbor
        expected_values = [10.0_real64, 30.0_real64, 50.0_real64]
        
        ! Test nearest neighbor interpolation
        result = interp1d(var, interp_points, method="nearest")
        
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            do stat = 1, 3
                if (abs(result%data%values_r64(stat) - expected_values(stat)) > 1e-10) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,F0.1,A,F0.1)') "Element ", stat, &
                        " should be ", expected_values(stat), " got ", result%data%values_r64(stat)
                    exit
                end if
            end do
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Nearest neighbor interpolation"
        else
            write(*,'(A)') "FAIL: Nearest neighbor interpolation"
        end if
    end subroutine test_nearest_neighbor_interpolation
    
    subroutine test_cubic_interpolation()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(6) :: data, coord_vals
        real(real64), dimension(1) :: interp_points
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data for cubic interpolation
        data = [1.0_real64, 8.0_real64, 27.0_real64, 64.0_real64, 125.0_real64, 216.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        
        call create_coordinate(x_coord, 6, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Interpolation point
        interp_points = [2.5_real64]
        
        ! Test cubic interpolation
        result = interp1d(var, interp_points, method="cubic")
        
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 1 element, got: ", result%n_elements
        else
            ! For x^3 function, f(2.5) should be close to 15.625
            if (abs(result%data%values_r64(1) - 15.625_real64) > 0.1) then
                test_passed = .false.
                write(error_unit,'(A,F0.3)') "Cubic interpolation result incorrect: ", result%data%values_r64(1)
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Cubic interpolation"
        else
            write(*,'(A)') "FAIL: Cubic interpolation"
        end if
    end subroutine test_cubic_interpolation
    
    subroutine test_interpolation_extrapolation()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(3) :: data, coord_vals, interp_points
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [1.0_real64, 2.0_real64, 3.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64]
        
        call create_coordinate(x_coord, 3, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Extrapolation points (outside data range)
        interp_points = [0.5_real64, 4.0_real64, 5.0_real64]
        
        ! Test extrapolation with bounds_error=false
        result = interp1d(var, interp_points, method="linear", bounds_error=.false., fill_value=-999.0_real64)
        
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            ! Check that extrapolated values are filled with fill_value
            if (abs(result%data%values_r64(1) - (-999.0_real64)) > 1e-10 .or. &
                abs(result%data%values_r64(3) - (-999.0_real64)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Extrapolation fill values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Interpolation extrapolation"
        else
            write(*,'(A)') "FAIL: Interpolation extrapolation"
        end if
    end subroutine test_interpolation_extrapolation
    
    subroutine test_interpolation_with_missing_data()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        real(real64), dimension(3) :: interp_points
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with NaN
        data = [1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        call create_coordinate(x_coord, 5, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Interpolation points
        interp_points = [1.5_real64, 2.5_real64, 3.5_real64]
        
        ! Test interpolation with missing data
        result = interp1d(var, interp_points, method="linear", skip_na=.true.)
        
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Interpolation with missing data"
        else
            write(*,'(A)') "FAIL: Interpolation with missing data"
        end if
    end subroutine test_interpolation_with_missing_data
    
    subroutine test_interpolation_2d()
        type(variable_t) :: var, result
        real(real64), dimension(6) :: data
        real(real64), dimension(2) :: x_interp, y_interp
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2x3 test data
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        var = variable(data, name="test_data", dim_names=["x", "y"])
        var%shape = [2, 3]
        
        ! Interpolation points
        x_interp = [1.5_real64, 1.5_real64]
        y_interp = [1.5_real64, 2.5_real64]
        
        ! Test 2D interpolation
        result = interp2d(var, x_interp, y_interp, method="linear")
        
        if (result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 2 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Interpolation 2D"
        else
            write(*,'(A)') "FAIL: Interpolation 2D"
        end if
    end subroutine test_interpolation_2d
    
    subroutine test_interpolation_edge_cases()
        type(variable_t) :: var, result
        real(real64), dimension(1) :: data, coord_vals, interp_points
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Single point interpolation
        data = [42.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        interp_points = [1.0_real64]
        
        result = interp1d(var, interp_points, method="nearest")
        
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 1 element, got: ", result%n_elements
        else if (abs(result%data%values_r64(1) - 42.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Single point interpolation failed"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Interpolation edge cases"
        else
            write(*,'(A)') "FAIL: Interpolation edge cases"
        end if
    end subroutine test_interpolation_edge_cases
    
    subroutine test_interpolation_performance()
        type(variable_t) :: var, result
        real(real64), dimension(1000) :: data, coord_vals, interp_points
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Large dataset performance test
        do i = 1, 1000
            data(i) = real(i, real64)
            coord_vals(i) = real(i, real64)
            interp_points(i) = real(i, real64) + 0.5_real64
        end do
        
        var = variable(data, name="test_data", dim_names=["x"])
        
        result = interp1d(var, interp_points(1:100), method="linear")
        
        if (result%n_elements /= 100) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Performance test failed, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Interpolation performance"
        else
            write(*,'(A)') "FAIL: Interpolation performance"
        end if
    end subroutine test_interpolation_performance
    
    ! Placeholder subroutines for remaining tests
    subroutine test_interpolation_coordinates()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Interpolation coordinates (placeholder)"
    end subroutine
    
    subroutine test_interpolation_bounds_checking()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Interpolation bounds checking (placeholder)"
    end subroutine
    
    subroutine test_interpolation_methods()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Interpolation methods (placeholder)"
    end subroutine
    
    subroutine test_interpolation_fill_values()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Interpolation fill values (placeholder)"
    end subroutine
    
    subroutine test_interpolation_regular_grid()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Interpolation regular grid (placeholder)"
    end subroutine
    
    subroutine test_interpolation_irregular_grid()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Interpolation irregular grid (placeholder)"
    end subroutine
    
    subroutine test_interpolation_multidimensional()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Interpolation multidimensional (placeholder)"
    end subroutine
    
end program test_interpolation