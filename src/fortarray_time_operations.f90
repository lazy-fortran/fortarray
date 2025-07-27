module fortarray_time_operations
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors
    use fortarray_indexing
    use fortarray_coordinate_selection
    use fortarray_interpolation
    use fortarray_aggregation
    use fortarray_missing_data
    use fortarray_time_coordinates
    use fortarray_slicing
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    implicit none
    private
    
    ! Public procedures
    public :: select_time_index
    public :: select_time_range
    public :: select_time_nearest
    public :: resample
    public :: rolling_mean
    public :: rolling_sum
    public :: rolling_std
    public :: rolling_min
    public :: rolling_max
    public :: seasonal_mean
    public :: seasonal_decompose
    public :: time_shift
    public :: time_diff
    public :: time_cumsum
    public :: time_cumprod
    
    ! Resampling frequencies
    character(len=*), parameter :: FREQ_DAILY = "D"
    character(len=*), parameter :: FREQ_WEEKLY = "W"
    character(len=*), parameter :: FREQ_MONTHLY = "M"
    character(len=*), parameter :: FREQ_QUARTERLY = "Q"
    character(len=*), parameter :: FREQ_YEARLY = "Y"
    character(len=*), parameter :: FREQ_HOURLY = "H"
    
contains
    
    !=====================================================!
    !           Time-based Indexing Functions             !
    !=====================================================!
    
    function select_time_index(var, start_idx, end_idx) result(output)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, end_idx
        type(fortarray_t) :: output
        
        integer :: actual_start, actual_end
        
        ! Validate indices
        actual_start = max(1, start_idx)
        actual_end = min(var%n_elements, end_idx)
        
        if (actual_start > actual_end) then
            ! Return empty variable
            output = var
            output%n_elements = 0
            return
        end if
        
        ! Use slicing to select range
        output = slice_range(var, actual_start, actual_end)
        
    end function select_time_index
    
    function select_time_range(var, start_time, end_time) result(output)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: start_time, end_time
        type(fortarray_t) :: output
        
        integer :: i, start_idx, end_idx
        real(real64), dimension(:), allocatable :: time_vals
        
        ! Get time coordinate values
        if (allocated(var%coords) .and. var%has_coord(1)) then
            ! Extract time values from coordinate
            select case(var%coords(1)%dtype)
            case(DTYPE_REAL64)
                time_vals = var%coords(1)%values_r64
            case(DTYPE_REAL32)
                allocate(time_vals(var%coords(1)%length))
                time_vals = real(var%coords(1)%values_r32, real64)
            case default
                ! Use indices as time
                allocate(time_vals(var%n_elements))
                time_vals = [(real(i, real64), i = 1, var%n_elements)]
            end select
        else
            ! Use indices as time
            allocate(time_vals(var%n_elements))
            time_vals = [(real(i, real64), i = 1, var%n_elements)]
        end if
        
        ! Find indices for time range
        start_idx = 0
        end_idx = 0
        
        do i = 1, size(time_vals)
            if (start_idx == 0 .and. time_vals(i) >= start_time) then
                start_idx = i
            end if
            if (time_vals(i) <= end_time) then
                end_idx = i
            end if
        end do
        
        if (start_idx == 0) start_idx = 1
        if (end_idx == 0) end_idx = size(time_vals)
        
        output = select_time_index(var, start_idx, end_idx)
        
        if (allocated(time_vals)) deallocate(time_vals)
        
    end function select_time_range
    
    function select_time_nearest(var, target_time) result(output)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: target_time
        type(fortarray_t) :: output
        
        integer :: i, nearest_idx
        real(real64) :: min_diff, diff
        real(real64), dimension(:), allocatable :: time_vals
        
        ! Get time coordinate values
        if (allocated(var%coords) .and. var%has_coord(1)) then
            select case(var%coords(1)%dtype)
            case(DTYPE_REAL64)
                time_vals = var%coords(1)%values_r64
            case(DTYPE_REAL32)
                allocate(time_vals(var%coords(1)%length))
                time_vals = real(var%coords(1)%values_r32, real64)
            case default
                allocate(time_vals(var%n_elements))
                time_vals = [(real(i, real64), i = 1, var%n_elements)]
            end select
        else
            allocate(time_vals(var%n_elements))
            time_vals = [(real(i, real64), i = 1, var%n_elements)]
        end if
        
        ! Find nearest time
        nearest_idx = 1
        min_diff = abs(time_vals(1) - target_time)
        
        do i = 2, size(time_vals)
            diff = abs(time_vals(i) - target_time)
            if (diff < min_diff) then
                min_diff = diff
                nearest_idx = i
            end if
        end do
        
        output = select_time_index(var, nearest_idx, nearest_idx)
        
        if (allocated(time_vals)) deallocate(time_vals)
        
    end function select_time_nearest
    
    !=====================================================!
    !              Resampling Functions                   !
    !=====================================================!
    
    function resample(var, freq, method) result(output)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: freq
        character(len=*), intent(in) :: method
        type(fortarray_t) :: output
        
        select case(trim(freq))
        case(FREQ_DAILY)
            output = resample_to_daily(var, method)
        case(FREQ_MONTHLY)
            output = resample_to_monthly(var, method)
        case(FREQ_YEARLY)
            output = resample_to_yearly(var, method)
        case default
            ! Unsupported frequency, return original
            output = var
        end select
        
    end function resample
    
    function resample_to_daily(var, method) result(output)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: method
        type(fortarray_t) :: output
        
        integer :: n_days, hours_per_day, i, j, start_idx, end_idx, count
        real(real64), dimension(:), allocatable :: daily_data
        real(real64) :: sum_val, max_val, min_val
        
        ! Check if this is monthly data (upsampling case)
        if (var%n_elements <= 12 .and. trim(method) == "interpolate") then
            output = upsample_to_daily(var)
            return
        end if
        
        ! Determine number of days from hours
        hours_per_day = 24
        n_days = var%n_elements / hours_per_day
        
        if (n_days < 1) then
            ! Less than a day of data
            output = var
            return
        end if
        
        allocate(daily_data(n_days))
        
        select case(trim(method))
        case("mean")
            do i = 1, n_days
                start_idx = (i-1) * hours_per_day + 1
                end_idx = min(i * hours_per_day, var%n_elements)
                sum_val = 0.0_real64
                count = 0
                
                do j = start_idx, end_idx
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        sum_val = sum_val + var%data%values_r64(j)
                    case(DTYPE_REAL32)
                        sum_val = sum_val + real(var%data%values_r32(j), real64)
                    end select
                    count = count + 1
                end do
                
                if (count > 0) then
                    daily_data(i) = sum_val / real(count, real64)
                else
                    daily_data(i) = 0.0_real64
                end if
            end do
            output = variable(daily_data, name=trim(var%name)//"_daily_mean", dim_names=["time"])
            
        case("max")
            do i = 1, n_days
                start_idx = (i-1) * hours_per_day + 1
                end_idx = min(i * hours_per_day, var%n_elements)
                max_val = -huge(1.0_real64)
                
                do j = start_idx, end_idx
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        if (var%data%values_r64(j) > max_val) max_val = var%data%values_r64(j)
                    case(DTYPE_REAL32)
                        if (real(var%data%values_r32(j), real64) > max_val) then
                            max_val = real(var%data%values_r32(j), real64)
                        end if
                    end select
                end do
                
                daily_data(i) = max_val
            end do
            output = variable(daily_data, name=trim(var%name)//"_daily_max", dim_names=["time"])
            
        case("interpolate")
            ! For upsampling - approximate 365 days
            output = upsample_to_daily(var)
            write(*,'(A)') "Upsampling to daily with interpolation"
            
        case default
            output = var
        end select
        
        if (allocated(daily_data)) deallocate(daily_data)
        
    end function resample_to_daily
    
    function resample_to_monthly(var, method) result(output)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: method
        type(fortarray_t) :: output
        
        real(real64), dimension(12) :: monthly_data
        integer :: i, j, month_idx, n_per_month
        real(real64) :: sum_val
        
        ! Simple monthly aggregation
        monthly_data = 0.0_real64
        
        select case(trim(method))
        case("mean")
            ! Divide year into 12 equal parts
            n_per_month = var%n_elements / 12
            
            do i = 1, 12
                sum_val = 0.0_real64
                month_idx = 0
                do j = (i-1)*n_per_month + 1, min(i*n_per_month, var%n_elements)
                    select case(var%data%dtype)
                    case(DTYPE_REAL64)
                        sum_val = sum_val + var%data%values_r64(j)
                    case(DTYPE_REAL32)
                        sum_val = sum_val + real(var%data%values_r32(j), real64)
                    end select
                    month_idx = month_idx + 1
                end do
                if (month_idx > 0) then
                    monthly_data(i) = sum_val / real(month_idx, real64)
                end if
            end do
            
            output = variable(monthly_data, name=trim(var%name)//"_monthly", dim_names=["time"])
            
        case default
            output = var
            output%n_elements = 12
        end select
        
    end function resample_to_monthly
    
    function resample_to_yearly(var, method) result(output)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: method
        type(fortarray_t) :: output
        
        ! Placeholder
        output = var
        output%n_elements = 1
        write(*,'(A)') "Resampling to yearly"
        
    end function resample_to_yearly
    
    !=====================================================!
    !           Rolling Window Functions                  !
    !=====================================================!
    
    function rolling_mean(var, window_size) result(output)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: window_size
        type(fortarray_t) :: output
        
        real(real64), dimension(:), allocatable :: result_data
        real(real64) :: sum_val
        integer :: i, j, half_window, count
        
        allocate(result_data(var%n_elements))
        half_window = window_size / 2
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                sum_val = 0.0_real64
                count = 0
                do j = max(1, i - half_window), min(var%n_elements, i + half_window)
                    if (.not. ieee_is_nan(var%data%values_r64(j))) then
                        sum_val = sum_val + var%data%values_r64(j)
                        count = count + 1
                    end if
                end do
                if (count > 0) then
                    result_data(i) = sum_val / real(count, real64)
                else
                    result_data(i) = ieee_value(1.0_real64, ieee_quiet_nan)
                end if
            end do
        case default
            result_data = 0.0_real64
        end select
        
        output = variable(result_data, name=trim(var%name)//"_rolling_mean", dim_names=var%dim_names)
        
        deallocate(result_data)
        
    end function rolling_mean
    
    function rolling_sum(var, window_size) result(output)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: window_size
        type(fortarray_t) :: output
        
        real(real64), dimension(:), allocatable :: result_data
        real(real64) :: sum_val
        integer :: i, j, half_window
        
        allocate(result_data(var%n_elements))
        half_window = window_size / 2
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                sum_val = 0.0_real64
                do j = max(1, i - half_window), min(var%n_elements, i + half_window)
                    if (.not. ieee_is_nan(var%data%values_r64(j))) then
                        sum_val = sum_val + var%data%values_r64(j)
                    end if
                end do
                result_data(i) = sum_val
            end do
        case default
            result_data = 0.0_real64
        end select
        
        output = variable(result_data, name=trim(var%name)//"_rolling_sum", dim_names=var%dim_names)
        
        deallocate(result_data)
        
    end function rolling_sum
    
    function rolling_std(var, window_size) result(output)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: window_size
        type(fortarray_t) :: output
        
        ! Placeholder
        output = var
        write(*,'(A)') "Rolling standard deviation"
        
    end function rolling_std
    
    function rolling_min(var, window_size) result(output)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: window_size
        type(fortarray_t) :: output
        
        ! Placeholder
        output = var
        write(*,'(A)') "Rolling minimum"
        
    end function rolling_min
    
    function rolling_max(var, window_size) result(output)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: window_size
        type(fortarray_t) :: output
        
        ! Placeholder
        output = var
        write(*,'(A)') "Rolling maximum"
        
    end function rolling_max
    
    !=====================================================!
    !            Seasonal Functions                       !
    !=====================================================!
    
    function seasonal_mean(var) result(output)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: output
        
        real(real64), dimension(4) :: seasonal_data
        integer :: days_per_season
        
        ! Simple 4-season split
        days_per_season = var%n_elements / 4
        
        ! Calculate seasonal means
        seasonal_data(1) = mean_subset(var, 1, days_per_season)  ! DJF
        seasonal_data(2) = mean_subset(var, days_per_season+1, 2*days_per_season)  ! MAM
        seasonal_data(3) = mean_subset(var, 2*days_per_season+1, 3*days_per_season)  ! JJA
        seasonal_data(4) = mean_subset(var, 3*days_per_season+1, var%n_elements)  ! SON
        
        output = variable(seasonal_data, name=trim(var%name)//"_seasonal", dim_names=["season"])
        
    end function seasonal_mean
    
    function seasonal_decompose(var) result(output)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: output
        
        ! Placeholder for seasonal decomposition
        output = var
        write(*,'(A)') "Seasonal decomposition"
        
    end function seasonal_decompose
    
    !=====================================================!
    !           Time Transformation Functions             !
    !=====================================================!
    
    function time_shift(var, periods) result(output)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: periods
        type(fortarray_t) :: output
        
        real(real64), dimension(:), allocatable :: shifted_data
        integer :: i, src_idx
        
        allocate(shifted_data(var%n_elements))
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                src_idx = i - periods
                if (src_idx >= 1 .and. src_idx <= var%n_elements) then
                    shifted_data(i) = var%data%values_r64(src_idx)
                else
                    shifted_data(i) = ieee_value(1.0_real64, ieee_quiet_nan)
                end if
            end do
        case default
            shifted_data = 0.0_real64
        end select
        
        output = variable(shifted_data, name=trim(var%name)//"_shifted", dim_names=var%dim_names)
        
        deallocate(shifted_data)
        
    end function time_shift
    
    function time_diff(var, periods) result(output)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: periods
        type(fortarray_t) :: output
        
        real(real64), dimension(:), allocatable :: diff_data
        integer :: i
        
        if (var%n_elements <= periods) then
            allocate(diff_data(1))
            diff_data = 0.0_real64
        else
            allocate(diff_data(var%n_elements - periods))
            
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                do i = periods + 1, var%n_elements
                    diff_data(i - periods) = var%data%values_r64(i) - var%data%values_r64(i - periods)
                end do
            case default
                diff_data = 0.0_real64
            end select
        end if
        
        output = variable(diff_data, name=trim(var%name)//"_diff", dim_names=var%dim_names)
        
        deallocate(diff_data)
        
    end function time_diff
    
    function time_cumsum(var) result(output)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: output
        
        real(real64), dimension(:), allocatable :: cumsum_data
        real(real64) :: running_sum
        integer :: i
        
        allocate(cumsum_data(var%n_elements))
        running_sum = 0.0_real64
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                if (.not. ieee_is_nan(var%data%values_r64(i))) then
                    running_sum = running_sum + var%data%values_r64(i)
                end if
                cumsum_data(i) = running_sum
            end do
        case default
            cumsum_data = 0.0_real64
        end select
        
        output = variable(cumsum_data, name=trim(var%name)//"_cumsum", dim_names=var%dim_names)
        
        deallocate(cumsum_data)
        
    end function time_cumsum
    
    function time_cumprod(var) result(output)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: output
        
        ! Placeholder
        output = var
        write(*,'(A)') "Cumulative product"
        
    end function time_cumprod
    
    !=====================================================!
    !              Helper Functions                       !
    !=====================================================!
    
    function mean_subset(var, start_idx, end_idx) result(mean_val)
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: start_idx, end_idx
        real(real64) :: mean_val
        
        real(real64) :: sum_val
        integer :: i, count
        
        sum_val = 0.0_real64
        count = 0
        
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = max(1, start_idx), min(var%n_elements, end_idx)
                if (.not. ieee_is_nan(var%data%values_r64(i))) then
                    sum_val = sum_val + var%data%values_r64(i)
                    count = count + 1
                end if
            end do
        case(DTYPE_REAL32)
            do i = max(1, start_idx), min(var%n_elements, end_idx)
                if (.not. ieee_is_nan(var%data%values_r32(i))) then
                    sum_val = sum_val + real(var%data%values_r32(i), real64)
                    count = count + 1
                end if
            end do
        case default
            count = 1
        end select
        
        if (count > 0) then
            mean_val = sum_val / real(count, real64)
        else
            mean_val = ieee_value(1.0_real64, ieee_quiet_nan)
        end if
        
    end function mean_subset
    
    function upsample_to_daily(var) result(output)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: output
        
        real(real64), dimension(:), allocatable :: daily_data
        integer :: n_days, i
        
        ! Approximate 30 days per month
        n_days = var%n_elements * 30
        
        allocate(daily_data(n_days))
        
        ! Simple upsampling - repeat each monthly value 30 times
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            do i = 1, var%n_elements
                daily_data((i-1)*30+1:i*30) = var%data%values_r64(i)
            end do
        case(DTYPE_REAL32)
            do i = 1, var%n_elements
                daily_data((i-1)*30+1:i*30) = real(var%data%values_r32(i), real64)
            end do
        case default
            daily_data = 0.0_real64
        end select
        
        output = variable(daily_data, name=trim(var%name)//"_daily", dim_names=["time"])
        
        deallocate(daily_data)
        
    end function upsample_to_daily
    
end module fortarray_time_operations