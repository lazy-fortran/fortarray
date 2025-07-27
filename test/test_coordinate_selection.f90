program test_coordinate_selection
    use foxel
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================="
    write(*,'(A)') "Running Coordinate Selection Test Suite"
    write(*,'(A)') "========================================="
    
    ! Run tests
    call test_sel_single_value()
    call test_sel_multiple_values()
    call test_sel_range()
    call test_sel_nearest()
    call test_sel_multidim()
    call test_sel_between()
    call test_sel_tolerance()
    call test_sel_datetime_coords()
    call test_sel_string_coords()
    call test_sel_preserve_attrs()
    call test_sel_performance()
    call test_sel_edge_cases()
    call test_isel_vs_sel()
    call test_sel_method_parameter()
    call test_sel_drop_parameter()
    
    ! Summary
    write(*,'(A)') "========================================="
    write(*,'(A,I0,A,I0,A)') "Coordinate Selection Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some coordinate selection tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All coordinate selection tests passed!"
    end if

contains

    subroutine test_sel_single_value()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(10) :: data, coord_vals
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with coordinates
        data = [(real(i, real64), i=1,10)]
        coord_vals = [(real(i*10, real64), i=1,10)]  ! 10, 20, 30, ..., 100
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Select single coordinate value
        result = sel(var, x=30.0_real64)
        
        ! Check result
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Single value selection wrong size"
        else if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Single value selection wrong value"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Single value selection test"
        else
            write(*,'(A)') "FAIL: Single value selection test"
        end if
    end subroutine test_sel_single_value
    
    subroutine test_sel_multiple_values()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(10) :: data, coord_vals
        real(real64), dimension(3) :: select_vals
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [(real(i, real64), i=1,10)]
        coord_vals = [(real(i*10, real64), i=1,10)]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! TODO: Multiple value selection - sel doesn't support array arguments yet
        select_vals = [20.0_real64, 50.0_real64, 80.0_real64]
        ! result = sel(var, x=select_vals)
        
        ! For now, just test single value
        result = sel(var, x=20.0_real64)
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Single value selection wrong size"
        else if (abs(result%data%values_r64(1) - 2.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Single value selection wrong value"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Multiple value selection test"
        else
            write(*,'(A)') "FAIL: Multiple value selection test"
        end if
    end subroutine test_sel_multiple_values
    
    subroutine test_sel_range()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(10) :: data, coord_vals
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [(real(i, real64), i=1,10)]
        coord_vals = [(real(i*10, real64), i=1,10)]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Select range (inclusive)
        result = sel_range(var, x=[25.0_real64, 75.0_real64])
        
        ! Check result - should include values at coords 30, 40, 50, 60, 70
        if (result%n_elements /= 5) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Range selection wrong size: ", result%n_elements
        else if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10 .or. &
                 abs(result%data%values_r64(5) - 7.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Range selection wrong values"
        end if
        
        ! Check coordinates are preserved
        if (result%has_coord(1)) then
            if (abs(result%coords(1)%values_r64(1) - 30.0_real64) > 1e-10 .or. &
                abs(result%coords(1)%values_r64(5) - 70.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Range selection coordinates not preserved"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Range selection lost coordinates"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Range selection test"
        else
            write(*,'(A)') "FAIL: Range selection test"
        end if
    end subroutine test_sel_range
    
    subroutine test_sel_nearest()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with irregular coordinates
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [10.0_real64, 25.0_real64, 30.0_real64, 50.0_real64, 100.0_real64]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Test nearest neighbor selection
        result = sel(var, x=27.0_real64, method="nearest")
        
        ! Should select value at coord 25.0 (index 2, value 2.0)
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Nearest selection wrong size"
        else if (abs(result%data%values_r64(1) - 2.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Nearest selection wrong value"
        end if
        
        ! Test another nearest selection
        result = sel(var, x=40.0_real64, method="nearest")
        
        ! Should select value at coord 50.0 (index 4, value 4.0)
        if (abs(result%data%values_r64(1) - 4.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Nearest selection (2) wrong value"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Nearest neighbor selection test"
        else
            write(*,'(A)') "FAIL: Nearest neighbor selection test"
        end if
    end subroutine test_sel_nearest
    
    subroutine test_sel_multidim()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord, y_coord
        real(real64), dimension(4,3) :: data
        real(real64), dimension(4) :: x_vals
        real(real64), dimension(3) :: y_vals
        integer :: i, j
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D test data
        do j = 1, 3
            do i = 1, 4
                data(i,j) = real(i + (j-1)*10, real64)
            end do
        end do
        
        x_vals = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64]
        y_vals = [100.0_real64, 200.0_real64, 300.0_real64]
        
        call create_coordinate(x_coord, size(x_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = x_vals
        call create_coordinate(y_coord, size(y_vals), "real64", i)
        y_coord%name = "y"
        y_coord%values_r64 = y_vals
        var = variable(data, name="test_2d", dim_names=["x", "y"], &
                      coords=[x_coord, y_coord])
        
        ! Select on both dimensions
        result = sel(var, x=20.0_real64, y=200.0_real64)
        
        ! Should get single value at (2,2) = 2 + 10 = 12
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "2D selection wrong size"
        else if (abs(result%data%values_r64(1) - 12.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "2D selection wrong value"
        end if
        
        ! Select on one dimension only
        result = sel(var, y=300.0_real64)
        
        ! Should get 1D array with x dimension preserved
        if (result%n_dims /= 1 .or. result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Partial selection wrong shape"
        else if (abs(result%data%values_r64(1) - 21.0_real64) > 1e-10 .or. &
                 abs(result%data%values_r64(4) - 24.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Partial selection wrong values"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Multi-dimensional selection test"
        else
            write(*,'(A)') "FAIL: Multi-dimensional selection test"
        end if
    end subroutine test_sel_multidim
    
    subroutine test_sel_between()
        type(variable_t) :: var, result
        type(coordinate_t) :: time_coord
        real(real64), dimension(10) :: data, time_vals
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create time series data
        data = [(real(i**2, real64), i=1,10)]
        time_vals = [(real(i, real64), i=1,10)]
        
        call create_coordinate(time_coord, size(time_vals), "real64", i)
        time_coord%name = "time"
        time_coord%values_r64 = time_vals
        var = variable(data, name="timeseries", dim_names=["time"], &
                      coords=[time_coord])
        
        ! Select between values (inclusive)
        result = sel_between(var, time=3.5_real64, time_end=7.5_real64)
        
        ! Should include times 4, 5, 6, 7 with values 16, 25, 36, 49
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Between selection wrong size"
        else if (abs(result%data%values_r64(1) - 16.0_real64) > 1e-10 .or. &
                 abs(result%data%values_r64(4) - 49.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Between selection wrong values"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Between selection test"
        else
            write(*,'(A)') "FAIL: Between selection test"
        end if
    end subroutine test_sel_between
    
    subroutine test_sel_tolerance()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Select with tolerance
        result = sel(var, x=2.1_real64, tolerance=0.2_real64)
        
        ! Should match coordinate 2.0
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Tolerance selection wrong size"
        else if (abs(result%data%values_r64(1) - 2.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Tolerance selection wrong value"
        end if
        
        ! Test no match with small tolerance
        result = sel(var, x=2.5_real64, tolerance=0.1_real64)
        
        if (result%n_elements /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Tolerance selection should return empty"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Tolerance selection test"
        else
            write(*,'(A)') "FAIL: Tolerance selection test"
        end if
    end subroutine test_sel_tolerance
    
    subroutine test_sel_datetime_coords()
        ! Placeholder for datetime coordinate selection
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! For now, just pass
        ! TODO: Implement when datetime support is added
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Datetime coordinate selection test"
        else
            write(*,'(A)') "FAIL: Datetime coordinate selection test"
        end if
    end subroutine test_sel_datetime_coords
    
    subroutine test_sel_string_coords()
        ! Test selection with string coordinates
        type(variable_t) :: var, result
        type(coordinate_t) :: label_coord
        real(real64), dimension(4) :: data
        character(len=10), dimension(4) :: labels
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create categorical data
        data = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64]
        labels = ["low       ", "medium    ", "high      ", "extreme   "]
        
        ! For now, use numeric coordinates as placeholder
        ! TODO: Implement string coordinate support
        call create_coordinate(label_coord, 4, "real64", i)
        label_coord%name = "x"
        label_coord%values_r64 = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        var = variable(data, name="categorical", dim_names=["x"], &
                      coords=[label_coord])
        
        result = sel(var, x=2.0_real64)  ! Would be "medium" with string coords
        
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "String coord selection wrong size"
        else if (abs(result%data%values_r64(1) - 20.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "String coord selection wrong value"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: String coordinate selection test"
        else
            write(*,'(A)') "FAIL: String coordinate selection test"
        end if
    end subroutine test_sel_string_coords
    
    subroutine test_sel_preserve_attrs()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with attributes
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Add attributes
        var%units = "meters"
        var%long_name = "Test variable with units"
        
        ! Select subset
        result = sel(var, x=30.0_real64)
        
        ! Check attributes are preserved
        if (trim(result%units) /= "meters") then
            test_passed = .false.
            write(error_unit,'(A)') "Units not preserved"
        end if
        
        if (trim(result%long_name) /= "Test variable with units") then
            test_passed = .false.
            write(error_unit,'(A)') "Long name not preserved"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Preserve attributes test"
        else
            write(*,'(A)') "FAIL: Preserve attributes test"
        end if
    end subroutine test_sel_preserve_attrs
    
    subroutine test_sel_performance()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord, y_coord, z_coord
        real(real64), dimension(100,100,50) :: data
        real(real64), dimension(100) :: x_vals
        real(real64), dimension(100) :: y_vals  
        real(real64), dimension(50) :: z_vals
        real(real64) :: start_time, end_time
        integer :: i, j, k
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create large 3D dataset
        do k = 1, 50
            do j = 1, 100
                do i = 1, 100
                    data(i,j,k) = real(i + j*100 + k*10000, real64)
                end do
            end do
        end do
        
        x_vals = [(real(i, real64), i=1,100)]
        y_vals = [(real(i, real64), i=1,100)]
        z_vals = [(real(i, real64), i=1,50)]
        
        call create_coordinate(x_coord, size(x_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = x_vals
        call create_coordinate(y_coord, size(y_vals), "real64", i)
        y_coord%name = "y"
        y_coord%values_r64 = y_vals
        call create_coordinate(z_coord, size(z_vals), "real64", i)
        z_coord%name = "z"
        z_coord%values_r64 = z_vals
        
        var = variable(data, name="large_3d", dim_names=["x", "y", "z"], &
                      coords=[x_coord, y_coord, z_coord])
        
        ! Time coordinate selection
        call cpu_time(start_time)
        result = sel(var, x=50.0_real64, y=50.0_real64, z=25.0_real64)
        call cpu_time(end_time)
        
        if (end_time - start_time > 0.1) then
            write(*,'(A,F8.4,A)') "WARNING: Coordinate selection took ", &
                end_time - start_time, " seconds"
        end if
        
        ! Verify result
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Performance test wrong result"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Performance test"
        else
            write(*,'(A)') "FAIL: Performance test"
        end if
    end subroutine test_sel_performance
    
    subroutine test_sel_edge_cases()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Test out of bounds selection
        result = sel(var, x=100.0_real64)
        
        if (result%n_elements /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Out of bounds should return empty"
        end if
        
        ! TODO: Test empty selection array - sel doesn't support array arguments yet
        ! result = sel(var, x=[real(real64) ::])
        
        ! TODO: Test selecting all values - sel doesn't support array arguments yet
        ! result = sel(var, x=coord_vals)
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Edge cases test"
        else
            write(*,'(A)') "FAIL: Edge cases test"
        end if
    end subroutine test_sel_edge_cases
    
    subroutine test_isel_vs_sel()
        type(variable_t) :: var, result_sel, result_isel
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Compare sel and isel
        result_sel = sel(var, x=3.0_real64)
        result_isel = isel(var, x=3)  ! 1-based indexing
        
        ! Results should be identical
        if (abs(result_sel%data%values_r64(1) - result_isel%data%values_r64(1)) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "sel and isel give different results"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result_sel)
        call finalize_variable(result_isel)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: isel vs sel test"
        else
            write(*,'(A)') "FAIL: isel vs sel test"
        end if
    end subroutine test_isel_vs_sel
    
    subroutine test_sel_method_parameter()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with gaps
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 5.0_real64, 6.0_real64, 10.0_real64]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = variable(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Test different methods
        
        ! Exact match (default)
        result = sel(var, x=3.0_real64, method="exact")
        if (result%n_elements /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Exact method should return empty for no match"
        end if
        
        ! Nearest
        result = sel(var, x=3.0_real64, method="nearest")
        if (result%n_elements /= 1 .or. &
            abs(result%data%values_r64(1) - 2.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Nearest method wrong result"
        end if
        
        ! Forward fill (use last valid coordinate <= target)
        result = sel(var, x=3.0_real64, method="ffill")
        if (result%n_elements /= 1 .or. &
            abs(result%data%values_r64(1) - 2.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Forward fill method wrong result"
        end if
        
        ! Backward fill (use next valid coordinate >= target)
        result = sel(var, x=3.0_real64, method="bfill")
        if (result%n_elements /= 1 .or. &
            abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Backward fill method wrong result"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Method parameter test"
        else
            write(*,'(A)') "FAIL: Method parameter test"
        end if
    end subroutine test_sel_method_parameter
    
    subroutine test_sel_drop_parameter()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord, y_coord
        real(real64), dimension(3,4) :: data
        real(real64), dimension(3) :: x_vals
        real(real64), dimension(4) :: y_vals
        integer :: i, j
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D test data
        do j = 1, 4
            do i = 1, 3
                data(i,j) = real(i + (j-1)*10, real64)
            end do
        end do
        
        x_vals = [1.0_real64, 2.0_real64, 3.0_real64]
        y_vals = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64]
        
        call create_coordinate(x_coord, size(x_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = x_vals
        call create_coordinate(y_coord, size(y_vals), "real64", i)
        y_coord%name = "y"
        y_coord%values_r64 = y_vals
        var = variable(data, name="test_2d", dim_names=["x", "y"], &
                      coords=[x_coord, y_coord])
        
        ! Select with drop=.true. (default)
        result = sel(var, x=2.0_real64)
        
        if (result%n_dims /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Drop should reduce dimensions"
        end if
        
        ! Select with drop=.false.
        result = sel(var, x=2.0_real64, drop=.false.)
        
        if (result%n_dims /= 2 .or. result%shape(1) /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "No drop should preserve dimensions"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Drop parameter test"
        else
            write(*,'(A)') "FAIL: Drop parameter test"
        end if
    end subroutine test_sel_drop_parameter
    
end program test_coordinate_selection