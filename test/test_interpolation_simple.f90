program test_interpolation_simple
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Interpolation Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run basic tests
    call test_linear_interp()
    call test_nearest_interp()
    call test_cubic_interp()
    
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

    subroutine test_linear_interp()
        type(fortarray_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(3) :: data, coord_vals, interp_points
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create simple test data: y = 2*x
        data = [0.0_real64, 2.0_real64, 4.0_real64]
        coord_vals = [0.0_real64, 1.0_real64, 2.0_real64]
        
        call create_coordinate(x_coord, 3, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = new_array(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Interpolate at x = 0.5 (should give y = 1.0)
        interp_points = [0.5_real64, 1.5_real64, 0.0_real64]
        result = interp1d(var, interp_points, method="linear")
        
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            ! Check first interpolated value
            if (abs(result%data%values_r64(1) - 1.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A,F0.3)') "Linear interpolation failed, got: ", result%data%values_r64(1)
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Linear interpolation"
        else
            write(*,'(A)') "FAIL: Linear interpolation"
        end if
    end subroutine test_linear_interp
    
    subroutine test_nearest_interp()
        type(fortarray_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(3) :: data, coord_vals, interp_points
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [10.0_real64, 20.0_real64, 30.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64]
        
        call create_coordinate(x_coord, 3, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = new_array(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Interpolate at x = 1.4 (should give nearest value: 10.0)
        interp_points = [1.4_real64, 2.6_real64, 1.9_real64]
        result = interp1d(var, interp_points, method="nearest")
        
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 10.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A,F0.1)') "Nearest neighbor interpolation failed, got: ", result%data%values_r64(1)
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Nearest neighbor interpolation"
        else
            write(*,'(A)') "FAIL: Nearest neighbor interpolation"
        end if
    end subroutine test_nearest_interp
    
    subroutine test_cubic_interp()
        type(fortarray_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(4) :: data, coord_vals
        real(real64), dimension(1) :: interp_points
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 8.0_real64, 27.0_real64, 64.0_real64]  ! x^3
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        
        call create_coordinate(x_coord, 4, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = new_array(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Interpolate at x = 2.5
        interp_points = [2.5_real64]
        result = interp1d(var, interp_points, method="cubic")
        
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 1 element, got: ", result%n_elements
        else
            ! For cubic function, should be reasonably close to 15.625
            if (abs(result%data%values_r64(1) - 15.625_real64) > 2.0) then
                test_passed = .false.
                write(error_unit,'(A,F0.3)') "Cubic interpolation result: ", result%data%values_r64(1)
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
    end subroutine test_cubic_interp
    
end program test_interpolation_simple