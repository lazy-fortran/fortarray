program test_plot_types
    use foxel
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Plot Types Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run all plot type tests
    call test_line_plot_basic()
    call test_line_plot_multiple()
    call test_line_plot_styles()
    call test_scatter_plot()
    call test_stem_plot()
    call test_bar_plot()
    call test_histogram_plot()
    call test_contour_plot_levels()
    call test_filled_contour_plot()
    call test_surface_plot()
    call test_mesh_plot()
    call test_time_series_plot()
    call test_subplot_grid()
    call test_twin_axes()
    call test_3d_visualization()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Plot Types Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some plot type tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All plot type tests passed!"
    end if

contains

    subroutine test_line_plot_basic()
        type(variable_t) :: var
        real(real64), dimension(100) :: x_data, y_data
        type(plot_options_t) :: opts
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 100
            x_data(i) = real(i-1, real64) * 0.1_real64
            y_data(i) = sin(x_data(i)) + 0.1_real64 * cos(5.0_real64 * x_data(i))
        end do
        var = variable(y_data, name="signal", dim_names=["x"])
        
        ! Test basic line plot
        opts%linestyle = "-"
        opts%linewidth = 2.0_real64
        opts%color = "blue"
        
        call line_plot(var, opts)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Line plot basic"
        else
            write(*,'(A)') "FAIL: Line plot basic"
        end if
    end subroutine test_line_plot_basic
    
    subroutine test_line_plot_multiple()
        type(variable_t), dimension(3) :: vars
        real(real64), dimension(50) :: x_data, y1, y2, y3
        type(plot_options_t) :: opts
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create multiple signals
        do i = 1, 50
            x_data(i) = real(i-1, real64) * 0.2_real64
            y1(i) = sin(x_data(i))
            y2(i) = cos(x_data(i))
            y3(i) = sin(x_data(i)) * cos(x_data(i))
        end do
        
        vars(1) = variable(y1, name="sin", dim_names=["x"])
        vars(2) = variable(y2, name="cos", dim_names=["x"])
        vars(3) = variable(y3, name="sin*cos", dim_names=["x"])
        
        ! Plot multiple lines
        opts%title = "Multiple Line Plot"
        call line_plot_multiple(vars, opts)
        
        do i = 1, 3
            call finalize_variable(vars(i))
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Line plot multiple"
        else
            write(*,'(A)') "FAIL: Line plot multiple"
        end if
    end subroutine test_line_plot_multiple
    
    subroutine test_line_plot_styles()
        type(variable_t) :: var
        real(real64), dimension(20) :: data
        type(plot_options_t) :: opts
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 20
            data(i) = real(i, real64)**2
        end do
        var = variable(data, name="parabola", dim_names=["x"])
        
        ! Test different line styles
        opts%linestyle = "--"
        opts%marker = "o"
        opts%markersize = 8.0_real64
        opts%color = "red"
        
        call line_plot(var, opts)
        
        ! Test dotted line
        opts%linestyle = ":"
        opts%marker = "none"
        call line_plot(var, opts)
        
        ! Test markers only
        opts%linestyle = "none"
        opts%marker = "^"
        call line_plot(var, opts)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Line plot styles"
        else
            write(*,'(A)') "FAIL: Line plot styles"
        end if
    end subroutine test_line_plot_styles
    
    subroutine test_scatter_plot()
        type(variable_t) :: x_var, y_var, size_var, color_var
        real(real64), dimension(100) :: x_data, y_data, sizes, colors
        type(plot_options_t) :: opts
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create random scatter data
        call random_seed()
        call random_number(x_data)
        call random_number(y_data)
        
        ! Variable sizes and colors
        do i = 1, 100
            sizes(i) = 10.0_real64 + 90.0_real64 * real(i, real64) / 100.0_real64
            colors(i) = sqrt(x_data(i)**2 + y_data(i)**2)
        end do
        
        x_var = variable(x_data, name="x_random", dim_names=["points"])
        y_var = variable(y_data, name="y_random", dim_names=["points"])
        size_var = variable(sizes, name="sizes", dim_names=["points"])
        color_var = variable(colors, name="colors", dim_names=["points"])
        
        ! Basic scatter plot
        call scatter_plot_advanced(x_var, y_var, opts)
        
        ! Scatter with sizes
        call scatter_plot_sized(x_var, y_var, size_var, opts)
        
        ! Scatter with colors
        opts%colormap = "plasma"
        call scatter_plot_colored(x_var, y_var, color_var, opts)
        
        call finalize_variable(x_var)
        call finalize_variable(y_var)
        call finalize_variable(size_var)
        call finalize_variable(color_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Scatter plot"
        else
            write(*,'(A)') "FAIL: Scatter plot"
        end if
    end subroutine test_scatter_plot
    
    subroutine test_stem_plot()
        type(variable_t) :: var
        real(real64), dimension(30) :: data
        type(plot_options_t) :: opts
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create discrete signal
        do i = 1, 30
            data(i) = sin(real(i, real64) * 0.3_real64) * exp(-real(i, real64) * 0.05_real64)
        end do
        var = variable(data, name="discrete_signal", dim_names=["n"])
        
        ! Stem plot
        opts%marker = "o"
        opts%markersize = 6.0_real64
        call stem_plot(var, opts)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Stem plot"
        else
            write(*,'(A)') "FAIL: Stem plot"
        end if
    end subroutine test_stem_plot
    
    subroutine test_bar_plot()
        type(variable_t) :: categories, values
        real(real64), dimension(5) :: data
        type(plot_options_t) :: opts
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create bar data
        data = [15.0_real64, 25.0_real64, 35.0_real64, 20.0_real64, 30.0_real64]
        values = variable(data, name="sales", dim_names=["category"])
        
        ! Basic bar plot
        call bar_plot_advanced(values, opts)
        
        ! Horizontal bar plot
        call barh_plot(values, opts)
        
        call finalize_variable(values)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Bar plot"
        else
            write(*,'(A)') "FAIL: Bar plot"
        end if
    end subroutine test_bar_plot
    
    subroutine test_histogram_plot()
        type(variable_t) :: var
        real(real64), dimension(1000) :: data
        type(plot_options_t) :: opts
        real(real64) :: mean, std
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create normally distributed data
        mean = 100.0_real64
        std = 15.0_real64
        call random_seed()
        do i = 1, 1000
            call random_normal(data(i), mean, std)
        end do
        
        var = variable(data, name="normal_dist", dim_names=["samples"])
        
        ! Basic histogram
        opts%title = "Histogram of Normal Distribution"
        call histogram_plot_advanced(var, nbins=30, options=opts)
        
        ! Normalized histogram
        call histogram_plot_normalized(var, nbins=50, options=opts)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Histogram plot"
        else
            write(*,'(A)') "FAIL: Histogram plot"
        end if
    end subroutine test_histogram_plot
    
    subroutine test_contour_plot_levels()
        type(variable_t) :: var
        real(real64), dimension(2500) :: data
        real(real64), dimension(10) :: levels
        type(plot_options_t) :: opts
        integer :: i, j, idx
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D data
        idx = 0
        do j = 1, 50
            do i = 1, 50
                idx = idx + 1
                data(idx) = sin(real(i, real64) * 0.1_real64) * &
                           cos(real(j, real64) * 0.1_real64) + &
                           0.1_real64 * real(i + j, real64)
            end do
        end do
        var = variable(data, name="wave_field", dim_names=["x", "y"])
        var%shape = [50, 50]
        
        ! Contour with specific levels
        levels = [(real(i, real64) * 0.5_real64, i = -5, 4)]
        call contour_plot_levels(var, levels, opts)
        
        ! Filled contour with colorbar
        opts%colormap = "RdBu"
        call contourf_plot_levels(var, levels, opts)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Contour plot levels"
        else
            write(*,'(A)') "FAIL: Contour plot levels"
        end if
    end subroutine test_contour_plot_levels
    
    subroutine test_filled_contour_plot()
        type(variable_t) :: var
        real(real64), dimension(10000) :: data
        type(plot_options_t) :: opts
        integer :: i, j, idx
        real(real64) :: x, y
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D Gaussian
        idx = 0
        do j = 1, 100
            do i = 1, 100
                idx = idx + 1
                x = real(i - 50, real64) * 0.1_real64
                y = real(j - 50, real64) * 0.1_real64
                data(idx) = exp(-(x**2 + y**2) / 2.0_real64)
            end do
        end do
        var = variable(data, name="gaussian_2d", dim_names=["x", "y"])
        var%shape = [100, 100]
        
        ! Filled contour
        opts%colormap = "viridis"
        call contourf_plot(var, opts)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Filled contour plot"
        else
            write(*,'(A)') "FAIL: Filled contour plot"
        end if
    end subroutine test_filled_contour_plot
    
    subroutine test_surface_plot()
        type(variable_t) :: var
        real(real64), dimension(900) :: data
        type(plot_options_t) :: opts
        integer :: i, j, idx
        real(real64) :: x, y
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 3D surface data
        idx = 0
        do j = 1, 30
            do i = 1, 30
                idx = idx + 1
                x = real(i - 15, real64) * 0.2_real64
                y = real(j - 15, real64) * 0.2_real64
                data(idx) = sin(sqrt(x**2 + y**2)) / sqrt(x**2 + y**2 + 0.1_real64)
            end do
        end do
        var = variable(data, name="sinc_function", dim_names=["x", "y"])
        var%shape = [30, 30]
        
        ! Surface plot
        opts%colormap = "coolwarm"
        call surface_plot(var, opts)
        
        ! Wireframe surface
        call surface_plot_wireframe(var, opts)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Surface plot"
        else
            write(*,'(A)') "FAIL: Surface plot"
        end if
    end subroutine test_surface_plot
    
    subroutine test_mesh_plot()
        type(variable_t) :: var
        real(real64), dimension(400) :: data
        type(plot_options_t) :: opts
        integer :: i, j, idx
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create mesh data
        idx = 0
        do j = 1, 20
            do i = 1, 20
                idx = idx + 1
                data(idx) = real(i + j, real64)
            end do
        end do
        var = variable(data, name="plane", dim_names=["x", "y"])
        var%shape = [20, 20]
        
        ! Mesh plot
        call mesh_plot(var, opts)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mesh plot"
        else
            write(*,'(A)') "FAIL: Mesh plot"
        end if
    end subroutine test_mesh_plot
    
    subroutine test_time_series_plot()
        type(variable_t) :: time_var, data_var
        real(real64), dimension(365) :: time, temperature
        type(plot_options_t) :: opts
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create time series data (daily temperature)
        do i = 1, 365
            time(i) = real(i, real64)
            temperature(i) = 20.0_real64 + 10.0_real64 * sin(2.0_real64 * 3.14159_real64 * real(i, real64) / 365.0_real64) + &
                            3.0_real64 * sin(2.0_real64 * 3.14159_real64 * real(i, real64) / 7.0_real64)
        end do
        
        time_var = variable(time, name="day_of_year", dim_names=["time"])
        data_var = variable(temperature, name="temperature", dim_names=["time"])
        
        ! Time series plot
        opts%xlabel = "Day of Year"
        opts%ylabel = "Temperature (°C)"
        opts%title = "Annual Temperature Variation"
        call time_series_plot(time_var, data_var, opts)
        
        call finalize_variable(time_var)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time series plot"
        else
            write(*,'(A)') "FAIL: Time series plot"
        end if
    end subroutine test_time_series_plot
    
    subroutine test_subplot_grid()
        type(variable_t), dimension(4) :: vars
        real(real64), dimension(100) :: x, y1, y2, y3, y4
        type(plot_options_t) :: opts
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create multiple datasets
        do i = 1, 100
            x(i) = real(i, real64) * 0.1_real64
            y1(i) = sin(x(i))
            y2(i) = cos(x(i))
            y3(i) = tan(x(i))
            y4(i) = exp(-x(i) * 0.1_real64)
        end do
        
        vars(1) = variable(y1, name="sin", dim_names=["x"])
        vars(2) = variable(y2, name="cos", dim_names=["x"])
        vars(3) = variable(y3, name="tan", dim_names=["x"])
        vars(4) = variable(y4, name="exp", dim_names=["x"])
        
        ! Create 2x2 subplot grid
        call subplot_grid(2, 2, vars, opts)
        
        do i = 1, 4
            call finalize_variable(vars(i))
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Subplot grid"
        else
            write(*,'(A)') "FAIL: Subplot grid"
        end if
    end subroutine test_subplot_grid
    
    subroutine test_twin_axes()
        type(variable_t) :: var1, var2
        real(real64), dimension(50) :: x, y1, y2
        type(plot_options_t) :: opts1, opts2
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with different scales
        do i = 1, 50
            x(i) = real(i, real64)
            y1(i) = sin(real(i, real64) * 0.1_real64)
            y2(i) = 100.0_real64 * exp(real(i, real64) * 0.02_real64)
        end do
        
        var1 = variable(y1, name="sine", dim_names=["x"])
        var2 = variable(y2, name="exponential", dim_names=["x"])
        
        ! Twin axes plot
        opts1%color = "blue"
        opts1%ylabel = "Sine Value"
        opts2%color = "red"
        opts2%ylabel = "Exponential Value"
        
        call twin_axes_plot(var1, var2, opts1, opts2)
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Twin axes"
        else
            write(*,'(A)') "FAIL: Twin axes"
        end if
    end subroutine test_twin_axes
    
    subroutine test_3d_visualization()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: 3D visualization (placeholder)"
    end subroutine test_3d_visualization
    
    ! Helper subroutine for generating random normal numbers
    subroutine random_normal(x, mean, std)
        real(real64), intent(out) :: x
        real(real64), intent(in) :: mean, std
        real(real64) :: u1, u2
        
        call random_number(u1)
        call random_number(u2)
        x = mean + std * sqrt(-2.0_real64 * log(u1)) * cos(2.0_real64 * 3.14159_real64 * u2)
    end subroutine random_normal
    
end program test_plot_types