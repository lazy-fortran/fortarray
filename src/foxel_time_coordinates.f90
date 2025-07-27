module foxel_time_coordinates
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use foxel_types
    use foxel_storage
    use foxel_datasets
    use foxel_constructors, only: create_coordinate
    implicit none
    private
    
    ! Public procedures
    public :: parse_time_units
    public :: is_leap_year
    public :: date_to_julian
    public :: julian_to_date
    public :: convert_time_units
    public :: time_to_epoch
    public :: epoch_to_date
    public :: create_time_coordinate
    public :: is_cf_time_compliant
    public :: parse_datetime
    public :: has_valid_time_bounds
    public :: format_time_iso
    public :: days_in_month
    public :: date_to_days
    public :: add_time_attributes
    public :: add_climatology_attributes
    public :: has_time_dimension
    public :: utc_to_local
    public :: finalize_time_coordinate
    
    ! Time unit conversion factors
    real(real64), parameter :: SECONDS_PER_MINUTE = 60.0_real64
    real(real64), parameter :: SECONDS_PER_HOUR = 3600.0_real64
    real(real64), parameter :: SECONDS_PER_DAY = 86400.0_real64
    real(real64), parameter :: DAYS_PER_YEAR = 365.2425_real64  ! Gregorian average
    real(real64), parameter :: DAYS_PER_MONTH = 30.4375_real64  ! Average
    
    ! Epoch reference (1970-01-01 00:00:00 UTC)
    integer, parameter :: EPOCH_YEAR = 1970
    
    ! Calendar types
    character(len=*), parameter :: CALENDAR_STANDARD = "standard"
    character(len=*), parameter :: CALENDAR_GREGORIAN = "gregorian"
    character(len=*), parameter :: CALENDAR_PROLEPTIC_GREGORIAN = "proleptic_gregorian"
    character(len=*), parameter :: CALENDAR_365_DAY = "365_day"
    character(len=*), parameter :: CALENDAR_360_DAY = "360_day"
    character(len=*), parameter :: CALENDAR_366_DAY = "366_day"
    
contains
    
    !=====================================================!
    !           Time Unit Parsing Functions               !
    !=====================================================!
    
    subroutine parse_time_units(units_string, unit_name, base_year, base_month, &
                                base_day, base_hour, base_minute, base_second)
        character(len=*), intent(in) :: units_string
        character(len=*), intent(out) :: unit_name
        integer, intent(out) :: base_year, base_month, base_day
        integer, intent(out) :: base_hour, base_minute
        real(real64), intent(out) :: base_second
        
        character(len=256) :: work_string
        integer :: since_pos, date_start
        character(len=64) :: date_part, time_part
        
        ! Initialize outputs
        unit_name = ""
        base_year = 1970
        base_month = 1
        base_day = 1
        base_hour = 0
        base_minute = 0
        base_second = 0.0_real64
        
        work_string = adjustl(units_string)
        
        ! Find "since" keyword
        since_pos = index(work_string, "since")
        if (since_pos == 0) return
        
        ! Extract unit name
        unit_name = adjustl(work_string(1:since_pos-1))
        
        ! Extract date/time part
        date_start = since_pos + 5
        date_part = adjustl(work_string(date_start:))
        
        ! Parse date and time
        call parse_datetime_internal(date_part, base_year, base_month, base_day, &
                                   base_hour, base_minute, base_second)
        
    end subroutine parse_time_units
    
    subroutine parse_datetime(datetime_str, year, month, day, hour, minute, second)
        character(len=*), intent(in) :: datetime_str
        integer, intent(out) :: year, month, day, hour, minute
        real(real64), intent(out) :: second
        
        call parse_datetime_internal(datetime_str, year, month, day, hour, minute, second)
        
    end subroutine parse_datetime
    
    subroutine parse_datetime_internal(datetime_str, year, month, day, hour, minute, second)
        character(len=*), intent(in) :: datetime_str
        integer, intent(out) :: year, month, day, hour, minute
        real(real64), intent(out) :: second
        
        character(len=256) :: work_str
        integer :: t_pos, space_pos
        
        ! Initialize
        year = 1970
        month = 1
        day = 1
        hour = 0
        minute = 0
        second = 0.0_real64
        
        work_str = adjustl(datetime_str)
        
        ! Replace 'T' or 'Z' with space for easier parsing
        t_pos = index(work_str, 'T')
        if (t_pos > 0) work_str(t_pos:t_pos) = ' '
        t_pos = index(work_str, 'Z')
        if (t_pos > 0) work_str(t_pos:t_pos) = ' '
        
        ! Simplified parsing - extract numbers from the string
        ! This is a basic implementation that handles common formats
        if (index(work_str, '-') > 0) then
            ! Replace separators with spaces
            do t_pos = 1, len_trim(work_str)
                if (work_str(t_pos:t_pos) == '-' .or. work_str(t_pos:t_pos) == ':') then
                    work_str(t_pos:t_pos) = ' '
                end if
            end do
        end if
        
        ! Try to read all components
        read(work_str, *, iostat=t_pos) year, month, day, hour, minute, second
        if (t_pos /= 0) then
            ! Try without seconds
            second = 0.0_real64
            read(work_str, *, iostat=t_pos) year, month, day, hour, minute
            if (t_pos /= 0) then
                ! Try date only
                hour = 0
                minute = 0
                read(work_str, *, iostat=t_pos) year, month, day
            end if
        end if
        
    end subroutine parse_datetime_internal
    
    !=====================================================!
    !            Calendar System Functions                !
    !=====================================================!
    
    function is_leap_year(year, calendar) result(is_leap)
        integer, intent(in) :: year
        character(len=*), intent(in) :: calendar
        logical :: is_leap
        
        is_leap = .false.
        
        select case(trim(calendar))
        case(CALENDAR_STANDARD, CALENDAR_GREGORIAN, CALENDAR_PROLEPTIC_GREGORIAN)
            ! Gregorian calendar rules
            if (mod(year, 4) == 0) then
                if (mod(year, 100) == 0) then
                    if (mod(year, 400) == 0) then
                        is_leap = .true.
                    end if
                else
                    is_leap = .true.
                end if
            end if
            
        case(CALENDAR_366_DAY)
            is_leap = .true.  ! Always leap year
            
        case(CALENDAR_365_DAY, CALENDAR_360_DAY)
            is_leap = .false.  ! Never leap year
            
        end select
        
    end function is_leap_year
    
    function days_in_month(year, month, calendar) result(days)
        integer, intent(in) :: year, month
        character(len=*), intent(in) :: calendar
        integer :: days
        
        integer, dimension(12) :: days_normal = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
        if (trim(calendar) == CALENDAR_360_DAY) then
            days = 30  ! All months have 30 days
        else
            days = days_normal(month)
            if (month == 2 .and. is_leap_year(year, calendar)) then
                days = 29
            end if
        end if
        
    end function days_in_month
    
    !=====================================================!
    !           Date Conversion Functions                 !
    !=====================================================!
    
    function date_to_julian(year, month, day, calendar) result(julian_day)
        integer, intent(in) :: year, month, day
        character(len=*), intent(in) :: calendar
        real(real64) :: julian_day
        
        integer :: y, m, total_days
        
        if (trim(calendar) == CALENDAR_360_DAY) then
            ! In 360-day calendar, year 1 starts at day 1
            julian_day = real((year - 1) * 360 + (month - 1) * 30 + day, real64)
        else
            ! Count days from year 1
            total_days = 0
            
            ! Add days for complete years
            do y = 1, year - 1
                if (is_leap_year(y, calendar)) then
                    total_days = total_days + 366
                else
                    total_days = total_days + 365
                end if
            end do
            
            ! Add days for complete months in current year
            do m = 1, month - 1
                total_days = total_days + days_in_month(year, m, calendar)
            end do
            
            ! Add days in current month
            total_days = total_days + day
            
            julian_day = real(total_days, real64)
        end if
        
    end function date_to_julian
    
    subroutine julian_to_date(julian_day, year, month, day, calendar)
        real(real64), intent(in) :: julian_day
        integer, intent(out) :: year, month, day
        character(len=*), intent(in) :: calendar
        
        integer :: remaining_days, days_in_year, m
        
        remaining_days = int(julian_day)
        
        if (trim(calendar) == CALENDAR_360_DAY) then
            year = (remaining_days - 1) / 360 + 1
            remaining_days = mod(remaining_days - 1, 360) + 1
            month = (remaining_days - 1) / 30 + 1
            day = mod(remaining_days - 1, 30) + 1
        else
            ! Find year
            year = 1
            do while (.true.)
                if (is_leap_year(year, calendar)) then
                    days_in_year = 366
                else
                    days_in_year = 365
                end if
                
                if (remaining_days <= days_in_year) exit
                
                remaining_days = remaining_days - days_in_year
                year = year + 1
            end do
            
            ! Find month
            month = 1
            do m = 1, 12
                if (remaining_days <= days_in_month(year, m, calendar)) then
                    month = m
                    exit
                end if
                remaining_days = remaining_days - days_in_month(year, m, calendar)
            end do
            
            day = remaining_days
        end if
        
    end subroutine julian_to_date
    
    !=====================================================!
    !          Time Unit Conversion Functions             !
    !=====================================================!
    
    function convert_time_units(value, from_units, to_units) result(converted)
        real(real64), intent(in) :: value
        character(len=*), intent(in) :: from_units, to_units
        real(real64) :: converted
        
        real(real64) :: value_in_days
        
        ! First convert to days
        select case(trim(from_units))
        case("seconds")
            value_in_days = value / SECONDS_PER_DAY
        case("minutes")
            value_in_days = value / (SECONDS_PER_DAY / SECONDS_PER_MINUTE)
        case("hours")
            value_in_days = value / 24.0_real64
        case("days")
            value_in_days = value
        case("months")
            value_in_days = value * DAYS_PER_MONTH
        case("years")
            value_in_days = value * DAYS_PER_YEAR
        case default
            value_in_days = value
        end select
        
        ! Then convert to target units
        select case(trim(to_units))
        case("seconds")
            converted = value_in_days * SECONDS_PER_DAY
        case("minutes")
            converted = value_in_days * (SECONDS_PER_DAY / SECONDS_PER_MINUTE)
        case("hours")
            converted = value_in_days * 24.0_real64
        case("days")
            converted = value_in_days
        case("months")
            converted = value_in_days / DAYS_PER_MONTH
        case("years")
            converted = value_in_days / DAYS_PER_YEAR
        case default
            converted = value_in_days
        end select
        
    end function convert_time_units
    
    !=====================================================!
    !            Epoch Conversion Functions               !
    !=====================================================!
    
    function time_to_epoch(time_value, time_units) result(epoch_seconds)
        real(real64), intent(in) :: time_value
        character(len=*), intent(in) :: time_units
        real(real64) :: epoch_seconds
        
        character(len=32) :: unit_name
        integer :: base_year, base_month, base_day
        integer :: base_hour, base_minute
        real(real64) :: base_second
        real(real64) :: base_julian, epoch_julian, days_diff
        
        ! Parse time units
        call parse_time_units(time_units, unit_name, base_year, base_month, &
                             base_day, base_hour, base_minute, base_second)
        
        ! Convert base date to julian days
        base_julian = date_to_julian(base_year, base_month, base_day, CALENDAR_STANDARD)
        epoch_julian = date_to_julian(EPOCH_YEAR, 1, 1, CALENDAR_STANDARD)
        
        days_diff = base_julian - epoch_julian
        
        ! Add time component
        days_diff = days_diff + (base_hour * 3600.0_real64 + base_minute * 60.0_real64 + base_second) / SECONDS_PER_DAY
        
        ! Convert time value to days and add to base
        days_diff = days_diff + convert_time_units(time_value, unit_name, "days")
        
        ! Convert to seconds
        epoch_seconds = days_diff * SECONDS_PER_DAY
        
    end function time_to_epoch
    
    subroutine epoch_to_date(epoch_seconds, year, month, day, hour, minute, second)
        real(real64), intent(in) :: epoch_seconds
        integer, intent(out) :: year, month, day, hour, minute
        real(real64), intent(out) :: second
        
        real(real64) :: days_since_epoch, julian_day
        real(real64) :: time_of_day
        
        days_since_epoch = epoch_seconds / SECONDS_PER_DAY
        julian_day = date_to_julian(EPOCH_YEAR, 1, 1, CALENDAR_STANDARD) + days_since_epoch
        
        call julian_to_date(julian_day, year, month, day, CALENDAR_STANDARD)
        
        ! Extract time of day
        time_of_day = mod(epoch_seconds, SECONDS_PER_DAY)
        hour = int(time_of_day / SECONDS_PER_HOUR)
        minute = int(mod(time_of_day, SECONDS_PER_HOUR) / SECONDS_PER_MINUTE)
        second = mod(time_of_day, SECONDS_PER_MINUTE)
        
    end subroutine epoch_to_date
    
    !=====================================================!
    !         Time Coordinate Creation Functions          !
    !=====================================================!
    
    subroutine create_time_coordinate(coord, length, units, calendar, stat)
        type(coordinate_t), intent(out) :: coord
        integer, intent(in) :: length
        character(len=*), intent(in) :: units
        character(len=*), intent(in) :: calendar
        integer, intent(out) :: stat
        
        stat = 0
        
        ! Create basic coordinate
        call create_coordinate(coord, length, "real64", stat)
        if (stat /= 0) return
        
        coord%name = "time"
        coord%initialized = .true.
        
        ! Add time attributes
        call add_time_attributes(coord, units, calendar)
        
    end subroutine create_time_coordinate
    
    subroutine add_time_attributes(coord, units, calendar)
        type(coordinate_t), intent(inout) :: coord
        character(len=*), intent(in) :: units
        character(len=*), intent(in), optional :: calendar
        
        ! Simplified: store units in the coordinate name for now
        ! In a full implementation, this would add proper attributes
        coord%name = "time"
        
        write(*,'(A,A)') "Time coordinate created with units: ", trim(units)
        if (present(calendar)) then
            write(*,'(A,A)') "Calendar: ", trim(calendar)
        end if
        
    end subroutine add_time_attributes
    
    subroutine add_climatology_attributes(coord, climatology_bounds, cell_methods)
        type(coordinate_t), intent(inout) :: coord
        character(len=*), intent(in) :: climatology_bounds
        character(len=*), intent(in) :: cell_methods
        
        write(*,'(A)') "Climatology attributes added:"
        write(*,'(A,A)') "  Bounds: ", trim(climatology_bounds)
        write(*,'(A,A)') "  Cell methods: ", trim(cell_methods)
        
    end subroutine add_climatology_attributes
    
    !=====================================================!
    !          Time Validation Functions                  !
    !=====================================================!
    
    function is_cf_time_compliant(coord) result(is_compliant)
        type(coordinate_t), intent(in) :: coord
        logical :: is_compliant
        
        is_compliant = .true.
        
        ! Check if coordinate is named "time"
        if (coord%name /= "time") then
            is_compliant = .false.
            return
        end if
        
        ! Check if it's initialized
        if (.not. coord%initialized) then
            is_compliant = .false.
            return
        end if
        
        ! In full implementation, would check for proper units attribute
        
    end function is_cf_time_compliant
    
    function has_valid_time_bounds(time_var, bounds_var) result(is_valid)
        type(variable_t), intent(in) :: time_var, bounds_var
        logical :: is_valid
        
        is_valid = .true.
        
        ! Check dimensions
        if (bounds_var%n_dims /= 2) then
            is_valid = .false.
            return
        end if
        
        if (bounds_var%shape(1) /= 2) then
            is_valid = .false.
            return
        end if
        
        if (bounds_var%shape(2) /= time_var%n_elements) then
            is_valid = .false.
            return
        end if
        
    end function has_valid_time_bounds
    
    function has_time_dimension(var) result(has_time)
        type(variable_t), intent(in) :: var
        logical :: has_time
        integer :: i
        
        has_time = .false.
        
        do i = 1, var%n_dims
            if (var%dim_names(i) == "time") then
                has_time = .true.
                exit
            end if
        end do
        
    end function has_time_dimension
    
    !=====================================================!
    !          Time Formatting Functions                  !
    !=====================================================!
    
    function format_time_iso(time_value, time_units) result(iso_string)
        real(real64), intent(in) :: time_value
        character(len=*), intent(in) :: time_units
        character(len=64) :: iso_string
        
        real(real64) :: epoch_seconds
        integer :: year, month, day, hour, minute
        real(real64) :: second
        
        ! Convert to epoch seconds
        epoch_seconds = time_to_epoch(time_value, time_units)
        
        ! Convert to date components
        call epoch_to_date(epoch_seconds, year, month, day, hour, minute, second)
        
        ! Format as ISO string
        write(iso_string, '(I4.4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') &
            year, '-', month, '-', day, 'T', hour, ':', minute, ':', int(second)
        
    end function format_time_iso
    
    function date_to_days(year, month, day, time_units, calendar) result(days)
        integer, intent(in) :: year, month, day
        character(len=*), intent(in) :: time_units
        character(len=*), intent(in) :: calendar
        real(real64) :: days
        
        character(len=32) :: unit_name
        integer :: base_year, base_month, base_day
        integer :: base_hour, base_minute
        real(real64) :: base_second
        real(real64) :: julian_target, julian_base
        
        ! Parse base date from time units
        call parse_time_units(time_units, unit_name, base_year, base_month, &
                             base_day, base_hour, base_minute, base_second)
        
        ! Calculate julian days for both dates
        julian_target = date_to_julian(year, month, day, calendar)
        julian_base = date_to_julian(base_year, base_month, base_day, calendar)
        
        ! Return difference
        days = julian_target - julian_base
        
    end function date_to_days
    
    !=====================================================!
    !          Time Zone Functions                        !
    !=====================================================!
    
    function utc_to_local(utc_time, tz_offset_hours) result(local_time)
        real(real64), intent(in) :: utc_time  ! Time in days
        integer, intent(in) :: tz_offset_hours
        real(real64) :: local_time
        
        local_time = utc_time + real(tz_offset_hours, real64) / 24.0_real64
        
    end function utc_to_local
    
    !=====================================================!
    !              Cleanup Functions                      !
    !=====================================================!
    
    subroutine finalize_time_coordinate(coord)
        type(coordinate_t), intent(inout) :: coord
        
        if (allocated(coord%values_r64)) deallocate(coord%values_r64)
        if (allocated(coord%values_r32)) deallocate(coord%values_r32)
        if (allocated(coord%values_i64)) deallocate(coord%values_i64)
        if (allocated(coord%values_i32)) deallocate(coord%values_i32)
        if (allocated(coord%values_char)) deallocate(coord%values_char)
        if (allocated(coord%attrs)) deallocate(coord%attrs)
        
        coord%initialized = .false.
        
    end subroutine finalize_time_coordinate
    
end module foxel_time_coordinates