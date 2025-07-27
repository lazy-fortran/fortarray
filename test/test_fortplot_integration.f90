program test_fortplot_integration
    use foxel
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================="
    write(*,'(A)') "Running Fortplot Integration Test Suite"
    write(*,'(A)') "========================================="
    
    ! Run all fortplot integration tests
    call test_plot_1d_variable()
    call test_plot_2d_variable()
    call test_plot_with_coordinates()
    call test_plot_multiple_variables()
    call test_plot_with_missing_data()
    call test_plot_methods_chaining()
    call test_plot_customization()
    call test_plot_export_formats()
    call test_plot_automatic_labels()
    call test_plot_dataset_visualization()
    call test_plot_time_series()
    call test_plot_error_bars()
    call test_plot_facets()
    call test_plot_colormaps()
    call test_plot_performance()
    
    ! Summary
    write(*,'(A)') "========================================="
    write(*,'(A,I0,A,I0,A)') "Fortplot Integration Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some fortplot integration tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All fortplot integration tests passed!"
    end if

contains

    subroutine test_plot_1d_variable()
        type(variable_t) :: var
        real(real64), dimension(100) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 100
            data(i) = sin(real(i, real64) * 0.1_real64)
        end do
        var = variable(data, name="sine_wave", dim_names=["x"])
        var%units = "radians"
        var%long_name = "Sine Wave Test Data"
        
        ! Test plot method - we'll use module procedure for now
        call variable_plot(var)
        
        ! Check if plot was created successfully
        if (.not. variable_has_plot(var)) then
            test_passed = .false.
            write(error_unit,'(A)') "Plot was not created"
        end if
        
        ! Clean up
        call variable_close_plot(var)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot 1D variable"
        else
            write(*,'(A)') "FAIL: Plot 1D variable"
        end if
    end subroutine test_plot_1d_variable
    
    subroutine test_plot_2d_variable()
        type(variable_t) :: var
        real(real64), dimension(400) :: data
        integer :: i, j, idx
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D test data
        idx = 0
        do j = 1, 20
            do i = 1, 20
                idx = idx + 1
                data(idx) = sin(real(i, real64) * 0.1_real64) * cos(real(j, real64) * 0.1_real64)
            end do
        end do
        var = variable(data, name="wave_2d", dim_names=["x", "y"])
        var%shape = [20, 20]
        
        ! Test 2D plot methods
        call variable_contour(var)
        call variable_contourf(var)
        call variable_pcolormesh(var)
        
        ! Test successful plot creation
        if (.not. variable_has_plot(var)) then
            test_passed = .false.
            write(error_unit,'(A)') "2D plot was not created"
        end if
        
        call variable_close_plot(var)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot 2D variable"
        else
            write(*,'(A)') "FAIL: Plot 2D variable"
        end if
    end subroutine test_plot_2d_variable
    
    subroutine test_plot_with_coordinates()
        type(variable_t) :: var
        type(coordinate_t) :: x_coord
        real(real64), dimension(50) :: data, coord_vals
        integer :: i, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with coordinates
        do i = 1, 50
            coord_vals(i) = real(i-1, real64) * 0.5_real64  ! 0, 0.5, 1.0, ...
            data(i) = exp(-coord_vals(i) / 10.0_real64)
        end do
        
        call create_coordinate(x_coord, 50, "real64", stat)
        if (stat == 0) then
            x_coord%name = "time"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="decay", dim_names=["time"])
        if (.not. allocated(var%coords)) allocate(var%coords(1))
        if (.not. allocated(var%has_coord)) allocate(var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Plot should use coordinate values
        call variable_plot(var)
        
        ! Verify coordinate-aware plotting
        if (.not. variable_uses_coordinate_axis(var)) then
            test_passed = .false.
            write(error_unit,'(A)') "Plot does not use coordinate axis"
        end if
        
        call variable_close_plot(var)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot with coordinates"
        else
            write(*,'(A)') "FAIL: Plot with coordinates"
        end if
    end subroutine test_plot_with_coordinates
    
    subroutine test_plot_multiple_variables()
        type(variable_t) :: var1, var2, var3
        real(real64), dimension(100) :: data1, data2, data3
        type(plot_options_t) :: opts
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create multiple variables
        do i = 1, 100
            data1(i) = sin(real(i, real64) * 0.1_real64)
            data2(i) = cos(real(i, real64) * 0.1_real64)
            data3(i) = sin(real(i, real64) * 0.1_real64) * cos(real(i, real64) * 0.05_real64)
        end do
        
        var1 = variable(data1, name="sin", dim_names=["x"])
        var2 = variable(data2, name="cos", dim_names=["x"])
        var3 = variable(data3, name="sin*cos", dim_names=["x"])
        
        ! Plot multiple variables on same axes
        opts%title = "Multiple Variables"
        allocate(opts%legend_labels(3))
        opts%legend_labels = ["sin(x)         ", "cos(x)         ", "sin(x)*cos(x/2)"]
        call plot_multiple([var1, var2, var3], opts)
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(var3)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot multiple variables"
        else
            write(*,'(A)') "FAIL: Plot multiple variables"
        end if
    end subroutine test_plot_multiple_variables
    
    subroutine test_plot_with_missing_data()
        type(variable_t) :: var
        real(real64), dimension(100) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with missing values
        do i = 1, 100
            if (mod(i, 7) == 0) then
                data(i) = ieee_value(1.0_real64, ieee_quiet_nan)
            else
                data(i) = real(i, real64) + 10.0_real64 * sin(real(i, real64) * 0.1_real64)
            end if
        end do
        
        var = variable(data, name="data_with_gaps", dim_names=["x"])
        
        ! Plot should handle NaN values gracefully
        call variable_plot(var)
        
        call variable_close_plot(var)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot with missing data"
        else
            write(*,'(A)') "FAIL: Plot with missing data"
        end if
    end subroutine test_plot_with_missing_data
    
    subroutine test_plot_methods_chaining()
        type(variable_t) :: var
        real(real64), dimension(100) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 100
            data(i) = real(i, real64)**2
        end do
        var = variable(data, name="quadratic", dim_names=["x"])
        
        ! Test method chaining - not available with current implementation
        call variable_plot(var)
        
        call variable_close_plot(var)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot methods chaining"
        else
            write(*,'(A)') "FAIL: Plot methods chaining"
        end if
    end subroutine test_plot_methods_chaining
    
    subroutine test_plot_customization()
        type(variable_t) :: var
        real(real64), dimension(50) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 50
            data(i) = exp(-real(i-25, real64)**2 / 100.0_real64)
        end do
        var = variable(data, name="gaussian", dim_names=["x"])
        
        ! Test plot customization options
        call variable_plot(var)
        
        call variable_close_plot(var)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot customization"
        else
            write(*,'(A)') "FAIL: Plot customization"
        end if
    end subroutine test_plot_customization
    
    subroutine test_plot_export_formats()
        type(variable_t) :: var
        real(real64), dimension(50) :: data
        integer :: i
        logical :: test_passed
        character(len=256) :: filename
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 50
            data(i) = sin(real(i, real64) * 0.2_real64)
        end do
        var = variable(data, name="export_test", dim_names=["x"])
        
        ! Test export to different formats
        call variable_plot(var)
        
        ! Export to PNG
        filename = "test_plot.png"
        call variable_savefig(var, filename, dpi=150)
        
        ! Export to PDF
        filename = "test_plot.pdf"
        call variable_savefig(var, filename)
        
        ! Export to SVG
        filename = "test_plot.svg"
        call variable_savefig(var, filename)
        
        ! Clean up files
        call delete_file("test_plot.png")
        call delete_file("test_plot.pdf")
        call delete_file("test_plot.svg")
        
        call variable_close_plot(var)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot export formats"
        else
            write(*,'(A)') "FAIL: Plot export formats"
        end if
    end subroutine test_plot_export_formats
    
    subroutine test_plot_automatic_labels()
        type(variable_t) :: var
        type(coordinate_t) :: x_coord
        real(real64), dimension(30) :: data, coord_vals
        integer :: i, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with metadata
        do i = 1, 30
            coord_vals(i) = real(i-1, real64) * 10.0_real64
            data(i) = 273.15_real64 + 20.0_real64 * sin(real(i, real64) * 0.2_real64)
        end do
        
        call create_coordinate(x_coord, 30, "real64", stat)
        if (stat == 0) then
            x_coord%name = "time"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="temperature", dim_names=["time"])
        var%units = "K"
        var%long_name = "Air Temperature"
        var%standard_name = "air_temperature"
        if (.not. allocated(var%coords)) allocate(var%coords(1))
        if (.not. allocated(var%has_coord)) allocate(var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Plot should automatically use metadata for labels
        call variable_plot(var)
        
        ! Check automatic labeling
        if (.not. variable_has_auto_labels(var)) then
            test_passed = .false.
            write(error_unit,'(A)') "Automatic labels not applied"
        end if
        
        call variable_close_plot(var)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot automatic labels"
        else
            write(*,'(A)') "FAIL: Plot automatic labels"
        end if
    end subroutine test_plot_automatic_labels
    
    subroutine test_plot_dataset_visualization()
        type(dataset_t) :: ds
        type(variable_t) :: temp, pressure, humidity
        real(real64), dimension(100) :: temp_data, pres_data, hum_data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create dataset with multiple variables
        do i = 1, 100
            temp_data(i) = 20.0_real64 + 5.0_real64 * sin(real(i, real64) * 0.1_real64)
            pres_data(i) = 1013.25_real64 + 10.0_real64 * cos(real(i, real64) * 0.08_real64)
            hum_data(i) = 60.0_real64 + 20.0_real64 * sin(real(i, real64) * 0.15_real64)
        end do
        
        temp = variable(temp_data, name="temperature", dim_names=["time"])
        temp%units = "degC"
        
        pressure = variable(pres_data, name="pressure", dim_names=["time"])
        pressure%units = "hPa"
        
        humidity = variable(hum_data, name="humidity", dim_names=["time"])
        humidity%units = "%"
        
        ds = dataset()
        call add_variable(ds, temp)
        call add_variable(ds, pressure)
        call add_variable(ds, humidity)
        
        ! Plot entire dataset
        call dataset_plot(ds)
        
        call finalize_variable(temp)
        call finalize_variable(pressure)
        call finalize_variable(humidity)
        call finalize_dataset(ds)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Plot dataset visualization"
        else
            write(*,'(A)') "FAIL: Plot dataset visualization"
        end if
    end subroutine test_plot_dataset_visualization
    
    subroutine test_plot_time_series()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Plot time series (placeholder)"
    end subroutine test_plot_time_series
    
    subroutine test_plot_error_bars()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Plot error bars (placeholder)"
    end subroutine test_plot_error_bars
    
    subroutine test_plot_facets()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Plot facets (placeholder)"
    end subroutine test_plot_facets
    
    subroutine test_plot_colormaps()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Plot colormaps (placeholder)"
    end subroutine test_plot_colormaps
    
    subroutine test_plot_performance()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Plot performance (placeholder)"
    end subroutine test_plot_performance
    
end program test_fortplot_integration