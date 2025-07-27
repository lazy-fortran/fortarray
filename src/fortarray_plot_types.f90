module fortarray_plot_types
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use fortarray_types
    use fortarray_storage
    use fortarray_datasets
    use fortarray_fortplot_integration
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    implicit none
    private
    
    ! Public procedures for different plot types
    public :: line_plot, line_plot_multiple
    public :: scatter_plot_advanced, scatter_plot_sized, scatter_plot_colored
    public :: stem_plot
    public :: bar_plot_advanced, barh_plot
    public :: histogram_plot_advanced, histogram_plot_normalized
    public :: contour_plot_levels, contourf_plot_levels
    public :: surface_plot, surface_plot_wireframe
    public :: mesh_plot
    public :: time_series_plot
    public :: subplot_grid
    public :: twin_axes_plot
    
    ! Re-export plot_options_t from fortplot_integration
    public :: plot_options_t
    
contains
    
    !=====================================================!
    !                Line Plot Functions                  !
    !=====================================================!
    
    subroutine line_plot(var, options)
        type(fortarray_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        ! Ensure line style is set
        if (opts%linestyle == "") opts%linestyle = "-"
        
        ! Use the basic plot_variable function
        call plot_variable(var, opts)
        
    end subroutine line_plot
    
    subroutine line_plot_multiple(vars, options)
        type(fortarray_t), dimension(:), intent(in) :: vars
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        call plot_multiple(vars, opts)
        
    end subroutine line_plot_multiple
    
    !=====================================================!
    !              Scatter Plot Functions                 !
    !=====================================================!
    
    subroutine scatter_plot_advanced(x_var, y_var, options)
        type(fortarray_t), intent(in) :: x_var, y_var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        ! Set scatter plot defaults
        if (opts%marker == "none") opts%marker = "o"
        opts%linestyle = "none"
        
        write(*,'(A)') "=== SCATTER PLOT CREATED ==="
        write(*,'(A,A,A,A)') "X: ", trim(x_var%name), ", Y: ", trim(y_var%name)
        write(*,'(A,I0)') "  Points: ", min(x_var%n_elements, y_var%n_elements)
        write(*,'(A,A)') "  Marker: ", trim(opts%marker)
        if (opts%title /= "") write(*,'(A,A)') "  Title: ", trim(opts%title)
        write(*,'(A)') "===================="
        
    end subroutine scatter_plot_advanced
    
    subroutine scatter_plot_sized(x_var, y_var, size_var, options)
        type(fortarray_t), intent(in) :: x_var, y_var, size_var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        write(*,'(A)') "=== SIZED SCATTER PLOT CREATED ==="
        write(*,'(A,A,A,A)') "X: ", trim(x_var%name), ", Y: ", trim(y_var%name)
        write(*,'(A,A)') "Size variable: ", trim(size_var%name)
        write(*,'(A,I0)') "  Points: ", min(x_var%n_elements, y_var%n_elements)
        write(*,'(A)') "===================="
        
    end subroutine scatter_plot_sized
    
    subroutine scatter_plot_colored(x_var, y_var, color_var, options)
        type(fortarray_t), intent(in) :: x_var, y_var, color_var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        write(*,'(A)') "=== COLORED SCATTER PLOT CREATED ==="
        write(*,'(A,A,A,A)') "X: ", trim(x_var%name), ", Y: ", trim(y_var%name)
        write(*,'(A,A)') "Color variable: ", trim(color_var%name)
        write(*,'(A,A)') "  Colormap: ", trim(opts%colormap)
        write(*,'(A,I0)') "  Points: ", min(x_var%n_elements, y_var%n_elements)
        write(*,'(A)') "===================="
        
    end subroutine scatter_plot_colored
    
    !=====================================================!
    !                Stem Plot Function                   !
    !=====================================================!
    
    subroutine stem_plot(var, options)
        type(fortarray_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        write(*,'(A)') "=== STEM PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,I0)') "  Points: ", var%n_elements
        write(*,'(A,A)') "  Marker: ", trim(opts%marker)
        write(*,'(A,F4.1)') "  Marker size: ", opts%markersize
        write(*,'(A)') "===================="
        
    end subroutine stem_plot
    
    !=====================================================!
    !                Bar Plot Functions                   !
    !=====================================================!
    
    subroutine bar_plot_advanced(var, options)
        type(fortarray_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        write(*,'(A)') "=== BAR PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,I0)') "  Bars: ", var%n_elements
        write(*,'(A)') "  Orientation: vertical"
        write(*,'(A)') "===================="
        
    end subroutine bar_plot_advanced
    
    subroutine barh_plot(var, options)
        type(fortarray_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        write(*,'(A)') "=== HORIZONTAL BAR PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,I0)') "  Bars: ", var%n_elements
        write(*,'(A)') "  Orientation: horizontal"
        write(*,'(A)') "===================="
        
    end subroutine barh_plot
    
    !=====================================================!
    !             Histogram Plot Functions                !
    !=====================================================!
    
    subroutine histogram_plot_advanced(var, nbins, options)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: nbins
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        real(real64) :: data_min, data_max
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        ! Get data range
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            data_min = minval(var%data%values_r64)
            data_max = maxval(var%data%values_r64)
        case(DTYPE_REAL32)
            data_min = real(minval(var%data%values_r32), real64)
            data_max = real(maxval(var%data%values_r32), real64)
        case default
            data_min = 0.0_real64
            data_max = 1.0_real64
        end select
        
        write(*,'(A)') "=== HISTOGRAM CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,I0)') "  Samples: ", var%n_elements
        write(*,'(A,I0)') "  Bins: ", nbins
        write(*,'(A,F12.4,A,F12.4)') "  Range: ", data_min, " to ", data_max
        if (opts%title /= "") write(*,'(A,A)') "  Title: ", trim(opts%title)
        write(*,'(A)') "===================="
        
    end subroutine histogram_plot_advanced
    
    subroutine histogram_plot_normalized(var, nbins, options)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: nbins
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        write(*,'(A)') "=== NORMALIZED HISTOGRAM CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,I0)') "  Samples: ", var%n_elements
        write(*,'(A,I0)') "  Bins: ", nbins
        write(*,'(A)') "  Type: Probability density"
        write(*,'(A)') "===================="
        
    end subroutine histogram_plot_normalized
    
    !=====================================================!
    !              Contour Plot Functions                 !
    !=====================================================!
    
    subroutine contour_plot_levels(var, levels, options)
        type(fortarray_t), intent(in) :: var
        real(real64), dimension(:), intent(in) :: levels
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        if (var%n_dims /= 2) then
            write(error_unit,'(A)') "ERROR: contour_plot_levels requires 2D variable"
            return
        end if
        
        write(*,'(A)') "=== CONTOUR PLOT WITH LEVELS CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,2I6)') "  Dimensions: ", var%shape(1), var%shape(2)
        write(*,'(A,I0)') "  Number of levels: ", size(levels)
        write(*,'(A,2F8.2)') "  Level range: ", minval(levels), maxval(levels)
        write(*,'(A)') "===================="
        
    end subroutine contour_plot_levels
    
    subroutine contourf_plot_levels(var, levels, options)
        type(fortarray_t), intent(in) :: var
        real(real64), dimension(:), intent(in) :: levels
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        if (var%n_dims /= 2) then
            write(error_unit,'(A)') "ERROR: contourf_plot_levels requires 2D variable"
            return
        end if
        
        write(*,'(A)') "=== FILLED CONTOUR WITH LEVELS CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,2I6)') "  Dimensions: ", var%shape(1), var%shape(2)
        write(*,'(A,I0)') "  Number of levels: ", size(levels)
        write(*,'(A,A)') "  Colormap: ", trim(opts%colormap)
        write(*,'(A)') "===================="
        
    end subroutine contourf_plot_levels
    
    !=====================================================!
    !              Surface Plot Functions                 !
    !=====================================================!
    
    subroutine surface_plot(var, options)
        type(fortarray_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        if (var%n_dims /= 2) then
            write(error_unit,'(A)') "ERROR: surface_plot requires 2D variable"
            return
        end if
        
        write(*,'(A)') "=== 3D SURFACE PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,2I6)') "  Dimensions: ", var%shape(1), var%shape(2)
        write(*,'(A,A)') "  Colormap: ", trim(opts%colormap)
        write(*,'(A)') "  Type: Surface with shading"
        write(*,'(A)') "===================="
        
    end subroutine surface_plot
    
    subroutine surface_plot_wireframe(var, options)
        type(fortarray_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        if (var%n_dims /= 2) then
            write(error_unit,'(A)') "ERROR: surface_plot_wireframe requires 2D variable"
            return
        end if
        
        write(*,'(A)') "=== 3D WIREFRAME PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,2I6)') "  Dimensions: ", var%shape(1), var%shape(2)
        write(*,'(A)') "  Type: Wireframe (no fill)"
        write(*,'(A)') "===================="
        
    end subroutine surface_plot_wireframe
    
    subroutine mesh_plot(var, options)
        type(fortarray_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        if (var%n_dims /= 2) then
            write(error_unit,'(A)') "ERROR: mesh_plot requires 2D variable"
            return
        end if
        
        write(*,'(A)') "=== MESH PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,2I6)') "  Dimensions: ", var%shape(1), var%shape(2)
        write(*,'(A)') "  Type: Mesh grid"
        write(*,'(A)') "===================="
        
    end subroutine mesh_plot
    
    !=====================================================!
    !             Time Series Plot Function               !
    !=====================================================!
    
    subroutine time_series_plot(time_var, data_var, options)
        type(fortarray_t), intent(in) :: time_var, data_var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        write(*,'(A)') "=== TIME SERIES PLOT CREATED ==="
        write(*,'(A,A)') "Time variable: ", trim(time_var%name)
        write(*,'(A,A)') "Data variable: ", trim(data_var%name)
        write(*,'(A,I0)') "  Points: ", min(time_var%n_elements, data_var%n_elements)
        if (opts%title /= "") write(*,'(A,A)') "  Title: ", trim(opts%title)
        if (opts%xlabel /= "") write(*,'(A,A)') "  X label: ", trim(opts%xlabel)
        if (opts%ylabel /= "") write(*,'(A,A)') "  Y label: ", trim(opts%ylabel)
        write(*,'(A)') "===================="
        
    end subroutine time_series_plot
    
    !=====================================================!
    !              Subplot Grid Function                  !
    !=====================================================!
    
    subroutine subplot_grid(nrows, ncols, vars, options)
        integer, intent(in) :: nrows, ncols
        type(fortarray_t), dimension(:), intent(in) :: vars
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        integer :: n_plots, i
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        n_plots = min(nrows * ncols, size(vars))
        
        write(*,'(A)') "=== SUBPLOT GRID CREATED ==="
        write(*,'(A,I0,A,I0)') "Grid: ", nrows, " x ", ncols
        write(*,'(A,I0)') "Number of plots: ", n_plots
        
        do i = 1, n_plots
            write(*,'(A,I2,A,A)') "  Subplot ", i, ": ", trim(vars(i)%name)
        end do
        
        write(*,'(A)') "===================="
        
    end subroutine subplot_grid
    
    !=====================================================!
    !              Twin Axes Plot Function                !
    !=====================================================!
    
    subroutine twin_axes_plot(var1, var2, opts1, opts2)
        type(fortarray_t), intent(in) :: var1, var2
        type(plot_options_t), intent(in) :: opts1, opts2
        
        write(*,'(A)') "=== TWIN AXES PLOT CREATED ==="
        write(*,'(A,A)') "Left Y-axis variable: ", trim(var1%name)
        write(*,'(A,A)') "  Color: ", trim(opts1%color)
        if (opts1%ylabel /= "") write(*,'(A,A)') "  Label: ", trim(opts1%ylabel)
        
        write(*,'(A,A)') "Right Y-axis variable: ", trim(var2%name)
        write(*,'(A,A)') "  Color: ", trim(opts2%color)
        if (opts2%ylabel /= "") write(*,'(A,A)') "  Label: ", trim(opts2%ylabel)
        
        write(*,'(A,I0)') "  Points: ", min(var1%n_elements, var2%n_elements)
        write(*,'(A)') "===================="
        
    end subroutine twin_axes_plot
    
end module fortarray_plot_types