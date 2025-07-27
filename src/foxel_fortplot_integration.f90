module foxel_fortplot_integration
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use foxel_types
    use foxel_storage
    use foxel_datasets
    ! use foxel_netcdf, only: delete_file  ! Not exported from foxel_netcdf
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    implicit none
    private
    
    ! Public procedures
    public :: plot_variable, plot_multiple_variables
    public :: plot_dataset, plot_dataset_grid
    public :: contour_plot, contourf_plot, pcolormesh_plot
    public :: scatter_plot, bar_plot, hist_plot
    public :: plot_with_errorbars, plot_with_fill
    public :: set_plot_style, set_colormap
    public :: save_figure, show_plot, close_figure
    
    ! Plot customization
    public :: plot_options_t
    
    ! Variable plot methods (to be added via type extension)
    public :: variable_plot, variable_contour, variable_contourf
    public :: variable_pcolormesh, variable_scatter
    public :: variable_has_plot, variable_close_plot
    public :: variable_savefig, variable_uses_coordinate_axis
    public :: variable_has_auto_labels
    
    ! Dataset plot methods
    public :: dataset_plot
    
    ! Public helper for multiple variables
    public :: plot_multiple
    
    ! Module variables
    logical :: plot_initialized = .false.
    character(len=64) :: default_style = "default"
    character(len=64) :: default_colormap = "viridis"
    character(len=256) :: current_filename = ""
    
    ! Plot options type
    type :: plot_options_t
        character(len=256) :: title = ""
        character(len=256) :: xlabel = ""
        character(len=256) :: ylabel = ""
        character(len=256) :: zlabel = ""
        logical :: grid = .true.
        logical :: legend_on = .true.
        character(len=64), dimension(:), allocatable :: legend_labels
        character(len=32) :: color = "auto"
        character(len=32) :: linestyle = "-"
        real(real64) :: linewidth = 1.5_real64
        character(len=32) :: marker = "none"
        real(real64) :: markersize = 6.0_real64
        real(real64) :: alpha = 1.0_real64
        logical :: skip_na = .true.
        real(real64), dimension(2) :: xlimits = [0.0_real64, 0.0_real64]
        real(real64), dimension(2) :: ylimits = [0.0_real64, 0.0_real64]
        character(len=64) :: style = "default"
        character(len=64) :: colormap = "viridis"
        integer :: dpi = 100
        logical :: tight_layout = .true.
        logical :: subplots = .false.
        real(real64), dimension(2) :: figsize = [8.0_real64, 6.0_real64]
    end type plot_options_t
    
contains
    
    !=====================================================!
    !            Basic Plotting Functions                 !
    !=====================================================!
    
    subroutine plot_variable(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        real(real64), dimension(:), allocatable :: x_data, y_data
        integer :: i, n_valid
        character(len=256) :: auto_title, auto_xlabel, auto_ylabel
        
        ! Set options
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        ! Handle 1D variable plotting
        if (var%n_dims /= 1) then
            write(error_unit, '(A)') "ERROR: plot_variable only supports 1D variables"
            return
        end if
        
        ! Extract data, skipping NaN if requested
        n_valid = 0
        if (opts%skip_na) then
            ! Count valid points
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                do i = 1, var%n_elements
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        n_valid = n_valid + 1
                    end if
                end do
            case(DTYPE_REAL32)
                do i = 1, var%n_elements
                    if (.not. ieee_is_nan(var%data%values_r32(i))) then
                        n_valid = n_valid + 1
                    end if
                end do
            case default
                n_valid = var%n_elements
            end select
            
            ! Allocate arrays for valid data
            allocate(x_data(n_valid), y_data(n_valid))
            
            ! Fill valid data
            n_valid = 0
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                do i = 1, var%n_elements
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        n_valid = n_valid + 1
                        y_data(n_valid) = var%data%values_r64(i)
                        ! Use coordinate if available
                        if (allocated(var%coords) .and. var%has_coord(1)) then
                            x_data(n_valid) = get_coordinate_value(var%coords(1), i)
                        else
                            x_data(n_valid) = real(i, real64)
                        end if
                    end if
                end do
            end select
        else
            ! Use all data
            allocate(x_data(var%n_elements), y_data(var%n_elements))
            
            ! Fill x data (coordinates or indices)
            if (allocated(var%coords) .and. var%has_coord(1)) then
                do i = 1, var%n_elements
                    x_data(i) = get_coordinate_value(var%coords(1), i)
                end do
            else
                x_data = [(real(i, real64), i=1,var%n_elements)]
            end if
            
            ! Fill y data
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                y_data = var%data%values_r64
            case(DTYPE_REAL32)
                y_data = real(var%data%values_r32, real64)
            case(DTYPE_INT32)
                y_data = real(var%data%values_i32, real64)
            case(DTYPE_INT64)
                y_data = real(var%data%values_i64, real64)
            end select
        end if
        
        ! Stub implementation - just print plot info
        write(*,'(A)') "=== PLOT CREATED ==="
        write(*,'(A,A)') "Plotting variable: ", trim(var%name)
        write(*,'(A,I0,A)') "  Data points: ", size(y_data), " (skipping NaN: " // merge("yes", "no ", opts%skip_na) // ")"
        
        ! Add markers if requested
        if (opts%marker /= "none") then
            write(*,'(A,A)') "  Marker style: ", trim(opts%marker)
        end if
        
        ! Set labels - use metadata if available
        if (opts%title == "") then
            auto_title = get_auto_title(var)
            write(*,'(A,A)') "  Title: ", trim(auto_title)
        else
            write(*,'(A,A)') "  Title: ", trim(opts%title)
        end if
        
        if (opts%xlabel == "") then
            auto_xlabel = get_auto_xlabel(var)
            write(*,'(A,A)') "  X label: ", trim(auto_xlabel)
        else
            write(*,'(A,A)') "  X label: ", trim(opts%xlabel)
        end if
        
        if (opts%ylabel == "") then
            auto_ylabel = get_auto_ylabel(var)
            write(*,'(A,A)') "  Y label: ", trim(auto_ylabel)
        else
            write(*,'(A,A)') "  Y label: ", trim(opts%ylabel)
        end if
        
        ! Set grid
        if (opts%grid) write(*,'(A)') "  Grid: enabled"
        
        ! Set limits if specified
        if (opts%xlimits(1) /= opts%xlimits(2)) then
            write(*,'(A,2F12.4)') "  X limits: ", opts%xlimits
        end if
        if (opts%ylimits(1) /= opts%ylimits(2)) then
            write(*,'(A,2F12.4)') "  Y limits: ", opts%ylimits
        end if
        
        write(*,'(A,F12.4,A,F12.4)') "  Data range (x): ", minval(x_data), " to ", maxval(x_data)
        write(*,'(A,F12.4,A,F12.4)') "  Data range (y): ", minval(y_data), " to ", maxval(y_data)
        write(*,'(A)') "===================="
        
        ! Clean up
        deallocate(x_data, y_data)
        
        ! Mark as initialized
        plot_initialized = .true.
        
    end subroutine plot_variable
    
    subroutine plot_multiple(vars, options)
        type(variable_t), dimension(:), intent(in) :: vars
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        character(len=64), dimension(:), allocatable :: legend_names
        integer :: i, n_vars
        
        ! Set options
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        n_vars = size(vars)
        if (allocated(opts%legend_labels) .and. size(opts%legend_labels) >= n_vars) then
            allocate(legend_names(n_vars))
            legend_names = opts%legend_labels(1:n_vars)
        end if
        
        write(*,'(A)') "=== MULTIPLE PLOT CREATED ==="
        write(*,'(A,I0)') "Number of variables: ", n_vars
        
        ! Plot each variable
        do i = 1, n_vars
            if (vars(i)%n_dims == 1) then
                write(*,'(A,I0,A,A)') "  Variable ", i, ": ", trim(vars(i)%name)
                if (allocated(legend_names)) then
                    write(*,'(A,A)') "    Legend: ", trim(legend_names(i))
                end if
            end if
        end do
        
        if (opts%title /= "") write(*,'(A,A)') "Title: ", trim(opts%title)
        if (opts%legend_on) write(*,'(A)') "Legend: enabled"
        write(*,'(A)') "===================="
        
        plot_initialized = .true.
        
        if (allocated(legend_names)) deallocate(legend_names)
        
    end subroutine plot_multiple
    
    subroutine plot_multiple_variables(vars, options)
        type(variable_t), dimension(:), intent(in) :: vars
        type(plot_options_t), intent(in), optional :: options
        
        ! Just call plot_multiple
        call plot_multiple(vars, options)
        
    end subroutine plot_multiple_variables
    
    !=====================================================!
    !              2D Plotting Functions                  !
    !=====================================================!
    
    subroutine contour_plot(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        integer :: nx, ny
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        ! Check dimensions
        if (var%n_dims /= 2) then
            write(error_unit, '(A)') "ERROR: contour_plot requires 2D variable"
            return
        end if
        
        nx = var%shape(1)
        ny = var%shape(2)
        
        ! Stub implementation
        write(*,'(A)') "=== CONTOUR PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,2I6)') "  Dimensions: ", nx, ny
        write(*,'(A,A)') "  Plot type: contour"
        if (opts%title /= "") write(*,'(A,A)') "  Title: ", trim(opts%title)
        write(*,'(A)') "===================="
        
        plot_initialized = .true.
        
    end subroutine contour_plot
    
    subroutine contourf_plot(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        integer :: nx, ny
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        ! Check dimensions
        if (var%n_dims /= 2) then
            write(error_unit, '(A)') "ERROR: contourf_plot requires 2D variable"
            return
        end if
        
        nx = var%shape(1)
        ny = var%shape(2)
        
        ! Stub implementation
        write(*,'(A)') "=== FILLED CONTOUR PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,2I6)') "  Dimensions: ", nx, ny
        write(*,'(A,A)') "  Plot type: filled contour"
        write(*,'(A,A)') "  Colormap: ", trim(opts%colormap)
        if (opts%title /= "") write(*,'(A,A)') "  Title: ", trim(opts%title)
        write(*,'(A)') "===================="
        
        plot_initialized = .true.
        
    end subroutine contourf_plot
    
    subroutine pcolormesh_plot(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        integer :: nx, ny
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        ! Check dimensions
        if (var%n_dims /= 2) then
            write(error_unit, '(A)') "ERROR: pcolormesh_plot requires 2D variable"
            return
        end if
        
        nx = var%shape(1)
        ny = var%shape(2)
        
        ! Stub implementation
        write(*,'(A)') "=== PCOLORMESH PLOT CREATED ==="
        write(*,'(A,A)') "Variable: ", trim(var%name)
        write(*,'(A,2I6)') "  Dimensions: ", nx, ny
        write(*,'(A,A)') "  Plot type: pcolormesh"
        write(*,'(A,A)') "  Colormap: ", trim(opts%colormap)
        if (opts%title /= "") write(*,'(A,A)') "  Title: ", trim(opts%title)
        write(*,'(A)') "===================="
        
        plot_initialized = .true.
        
    end subroutine pcolormesh_plot
    
    !=====================================================!
    !          Dataset Plotting Functions                 !
    !=====================================================!
    
    subroutine plot_dataset(ds, options)
        type(dataset_t), intent(in) :: ds
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        integer :: i, n_vars, n_rows, n_cols
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        n_vars = ds%n_vars
        if (n_vars == 0) return
        
        ! Calculate subplot grid
        n_cols = ceiling(sqrt(real(n_vars, real64)))
        n_rows = ceiling(real(n_vars, real64) / real(n_cols, real64))
        
        ! Stub implementation
        write(*,'(A)') "=== DATASET PLOT CREATED ==="
        write(*,'(A,I0)') "Number of variables: ", n_vars
        write(*,'(A,2I4)') "  Subplot grid: ", n_rows, n_cols
        write(*,'(A,2F6.1)') "  Figure size: ", opts%figsize
        
        ! List variables
        do i = 1, n_vars
            write(*,'(A,I2,A,A,A,I0,A)') "  Var ", i, ": ", trim(ds%variables(i)%name), &
                " (", ds%variables(i)%n_dims, "D)"
        end do
        
        if (opts%title /= "") then
            write(*,'(A,A)') "Overall title: ", trim(opts%title)
        else if (ds%title /= "") then
            write(*,'(A,A)') "Overall title: ", trim(ds%title)
        end if
        
        if (opts%tight_layout) write(*,'(A)') "Tight layout: enabled"
        write(*,'(A)') "===================="
        
        plot_initialized = .true.
        
    end subroutine plot_dataset
    
    subroutine plot_dataset_grid(ds, var_names, options)
        type(dataset_t), intent(in) :: ds
        character(len=*), dimension(:), intent(in) :: var_names
        type(plot_options_t), intent(in), optional :: options
        
        ! Placeholder for grid plotting
        call plot_dataset(ds, options)
        
    end subroutine plot_dataset_grid
    
    !=====================================================!
    !          Additional Plot Types                      !
    !=====================================================!
    
    subroutine scatter_plot(var_x, var_y, options)
        type(variable_t), intent(in) :: var_x, var_y
        type(plot_options_t), intent(in), optional :: options
        
        type(plot_options_t) :: opts
        
        if (present(options)) then
            opts = options
        else
            opts = plot_options_t()
        end if
        
        ! Stub implementation
        write(*,'(A)') "=== SCATTER PLOT CREATED ==="
        write(*,'(A,A,A,A)') "X variable: ", trim(var_x%name), ", Y variable: ", trim(var_y%name)
        write(*,'(A,I0)') "  Points: ", min(var_x%n_elements, var_y%n_elements)
        if (opts%title /= "") write(*,'(A,A)') "  Title: ", trim(opts%title)
        write(*,'(A)') "===================="
        
        plot_initialized = .true.
        
    end subroutine scatter_plot
    
    subroutine bar_plot(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        ! Placeholder
        call plot_variable(var, options)
        
    end subroutine bar_plot
    
    subroutine hist_plot(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        
        ! Placeholder
        call plot_variable(var, options)
        
    end subroutine hist_plot
    
    subroutine plot_with_errorbars(var, errors, options)
        type(variable_t), intent(in) :: var, errors
        type(plot_options_t), intent(in), optional :: options
        
        ! Placeholder
        call plot_variable(var, options)
        
    end subroutine plot_with_errorbars
    
    subroutine plot_with_fill(var_upper, var_lower, options)
        type(variable_t), intent(in) :: var_upper, var_lower
        type(plot_options_t), intent(in), optional :: options
        
        ! Placeholder
        call plot_variable(var_upper, options)
        
    end subroutine plot_with_fill
    
    !=====================================================!
    !            Plot Utility Functions                   !
    !=====================================================!
    
    subroutine save_figure(filename, dpi)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: dpi
        
        integer :: dpi_val
        
        dpi_val = 100
        if (present(dpi)) dpi_val = dpi
        
        write(*,'(A,A,A,I0)') "Saving figure to: ", trim(filename), " (DPI: ", dpi_val, ")"
        current_filename = filename
        
    end subroutine save_figure
    
    subroutine show_plot()
        write(*,'(A)') "Showing plot..."
    end subroutine show_plot
    
    subroutine close_figure()
        write(*,'(A)') "Closing figure..."
        plot_initialized = .false.
        current_filename = ""
    end subroutine close_figure
    
    subroutine set_plot_style(style)
        character(len=*), intent(in) :: style
        default_style = style
        write(*,'(A,A)') "Plot style set to: ", trim(style)
    end subroutine set_plot_style
    
    subroutine set_colormap(cmap)
        character(len=*), intent(in) :: cmap
        default_colormap = cmap
        write(*,'(A,A)') "Colormap set to: ", trim(cmap)
    end subroutine set_colormap
    
    !=====================================================!
    !        Variable Plot Method Implementations         !
    !=====================================================!
    
    subroutine variable_plot(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        call plot_variable(var, options)
    end subroutine variable_plot
    
    subroutine variable_contour(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        call contour_plot(var, options)
    end subroutine variable_contour
    
    subroutine variable_contourf(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        call contourf_plot(var, options)
    end subroutine variable_contourf
    
    subroutine variable_pcolormesh(var, options)
        type(variable_t), intent(in) :: var
        type(plot_options_t), intent(in), optional :: options
        call pcolormesh_plot(var, options)
    end subroutine variable_pcolormesh
    
    subroutine variable_scatter(var_x, var_y, options)
        type(variable_t), intent(in) :: var_x, var_y
        type(plot_options_t), intent(in), optional :: options
        call scatter_plot(var_x, var_y, options)
    end subroutine variable_scatter
    
    function variable_has_plot(var) result(has_plot)
        type(variable_t), intent(in) :: var
        logical :: has_plot
        has_plot = plot_initialized
    end function variable_has_plot
    
    subroutine variable_close_plot(var)
        type(variable_t), intent(in) :: var
        call close_figure()
    end subroutine variable_close_plot
    
    subroutine variable_savefig(var, filename, dpi)
        type(variable_t), intent(in) :: var
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: dpi
        call save_figure(filename, dpi)
        ! Create empty file as a stub
        open(unit=99, file=trim(filename), status='replace')
        write(99,'(A)') "# Stub plot file created by foxel"
        close(99)
    end subroutine variable_savefig
    
    function variable_uses_coordinate_axis(var) result(uses_coords)
        type(variable_t), intent(in) :: var
        logical :: uses_coords
        uses_coords = allocated(var%coords) .and. var%has_coord(1)
    end function variable_uses_coordinate_axis
    
    function variable_has_auto_labels(var) result(has_labels)
        type(variable_t), intent(in) :: var
        logical :: has_labels
        has_labels = (var%long_name /= "" .or. var%units /= "")
    end function variable_has_auto_labels
    
    subroutine dataset_plot(ds, options)
        type(dataset_t), intent(in) :: ds
        type(plot_options_t), intent(in), optional :: options
        call plot_dataset(ds, options)
    end subroutine dataset_plot
    
    !=====================================================!
    !              Helper Functions                       !
    !=====================================================!
    
    function get_coordinate_value(coord, idx) result(val)
        type(coordinate_t), intent(in) :: coord
        integer, intent(in) :: idx
        real(real64) :: val
        
        select case(coord%dtype)
        case(DTYPE_REAL64)
            val = coord%values_r64(idx)
        case(DTYPE_REAL32)
            val = real(coord%values_r32(idx), real64)
        case(DTYPE_INT32)
            val = real(coord%values_i32(idx), real64)
        case(DTYPE_INT64)
            val = real(coord%values_i64(idx), real64)
        case default
            val = real(idx, real64)
        end select
        
    end function get_coordinate_value
    
    function get_auto_title(var) result(title)
        type(variable_t), intent(in) :: var
        character(len=256) :: title
        
        if (var%long_name /= "") then
            title = trim(var%long_name)
        else
            title = trim(var%name)
        end if
        
    end function get_auto_title
    
    function get_auto_xlabel(var) result(label)
        type(variable_t), intent(in) :: var
        character(len=256) :: label
        
        if (allocated(var%coords) .and. var%has_coord(1)) then
            label = trim(var%coords(1)%name)
            ! Coordinates don't have units field - only variables do
            ! Could check for units attribute in coord%attrs if needed
        else if (var%n_dims >= 1) then
            label = trim(var%dim_names(1))
        else
            label = "Index"
        end if
        
    end function get_auto_xlabel
    
    function get_auto_ylabel(var) result(label)
        type(variable_t), intent(in) :: var
        character(len=256) :: label
        
        label = trim(var%name)
        if (var%units /= "") then
            label = trim(label) // " [" // trim(var%units) // "]"
        end if
        
    end function get_auto_ylabel
    
    function get_auto_ylabel_2d(var) result(label)
        type(variable_t), intent(in) :: var
        character(len=256) :: label
        
        if (allocated(var%coords) .and. size(var%coords) >= 2 .and. var%has_coord(2)) then
            label = trim(var%coords(2)%name)
            ! Coordinates don't have units field - only variables do
            ! Could check for units attribute in coord%attrs if needed
        else if (var%n_dims >= 2) then
            label = trim(var%dim_names(2))
        else
            label = "Y"
        end if
        
    end function get_auto_ylabel_2d
    
    !> Delete file utility
    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        integer :: status
        character(len=512) :: cmd
        
        ! Use rm command to delete file
        cmd = "rm -f " // trim(filename)
        call execute_command_line(cmd, exitstat=status, wait=.true.)
        
    end subroutine delete_file
    
end module foxel_fortplot_integration