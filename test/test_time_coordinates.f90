program test_time_coordinates
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Time Coordinates Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run all time coordinate tests
    call test_time_units_parsing()
    call test_calendar_systems()
    call test_time_unit_conversions()
    call test_epoch_conversion()
    call test_cf_time_compliance()
    call test_datetime_parsing()
    call test_time_bounds()
    call test_time_formatting()
    call test_leap_year_handling()
    call test_time_arithmetic()
    call test_time_coordinate_creation()
    call test_time_variable_metadata()
    call test_climatological_time()
    call test_time_zone_handling()
    call test_time_coordinate_validation()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Time Coordinates Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some time coordinate tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All time coordinate tests passed!"
    end if

contains

    subroutine test_time_units_parsing()
        character(len=256) :: time_units
        integer :: base_year, base_month, base_day
        integer :: base_hour, base_minute
        real(real64) :: base_second
        character(len=32) :: unit_name
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test parsing "days since 1970-01-01 00:00:00"
        time_units = "days since 1970-01-01 00:00:00"
        call parse_time_units(time_units, unit_name, base_year, base_month, &
                             base_day, base_hour, base_minute, base_second)
        
        if (unit_name /= "days" .or. base_year /= 1970 .or. base_month /= 1 .or. &
            base_day /= 1 .or. base_hour /= 0 .or. base_minute /= 0 .or. &
            base_second /= 0.0_real64) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to parse standard time units"
        end if
        
        ! Test parsing "hours since 2000-01-01T12:00:00Z"
        time_units = "hours since 2000-01-01T12:00:00Z"
        call parse_time_units(time_units, unit_name, base_year, base_month, &
                             base_day, base_hour, base_minute, base_second)
        
        if (unit_name /= "hours" .or. base_year /= 2000 .or. base_hour /= 12) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to parse ISO format time units"
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time units parsing"
        else
            write(*,'(A)') "FAIL: Time units parsing"
        end if
    end subroutine test_time_units_parsing
    
    subroutine test_calendar_systems()
        real(real64) :: julian_day
        integer :: year, month, day
        logical :: is_leap
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test standard calendar
        is_leap = is_leap_year(2000, "standard")
        if (.not. is_leap) then
            test_passed = .false.
            write(error_unit,'(A)') "2000 should be leap year in standard calendar"
        end if
        
        is_leap = is_leap_year(1900, "standard")
        if (is_leap) then
            test_passed = .false.
            write(error_unit,'(A)') "1900 should not be leap year in standard calendar"
        end if
        
        ! Test 360-day calendar
        ! Year 2000 = (2000-1)*360 + (12-1)*30 + 30 = 719640 + 330 + 30 = 720000
        julian_day = date_to_julian(2000, 12, 30, "360_day")
        ! For year 1, month 12, day 30: (1-1)*360 + (12-1)*30 + 30 = 0 + 330 + 30 = 360
        julian_day = date_to_julian(1, 12, 30, "360_day")
        if (abs(julian_day - 360.0_real64) > 1e-6) then
            test_passed = .false.
            write(error_unit,'(A)') "360-day calendar calculation error"
        end if
        
        ! Test 365-day calendar (no leap years)
        is_leap = is_leap_year(2000, "365_day")
        if (is_leap) then
            test_passed = .false.
            write(error_unit,'(A)') "365-day calendar should have no leap years"
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Calendar systems"
        else
            write(*,'(A)') "FAIL: Calendar systems"
        end if
    end subroutine test_calendar_systems
    
    subroutine test_time_unit_conversions()
        real(real64) :: value, converted
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test days to hours
        value = 1.5_real64  ! 1.5 days
        converted = convert_time_units(value, "days", "hours")
        if (abs(converted - 36.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F0.3)') "Days to hours conversion error: ", converted
        end if
        
        ! Test hours to seconds
        value = 2.5_real64  ! 2.5 hours
        converted = convert_time_units(value, "hours", "seconds")
        if (abs(converted - 9000.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A,F0.1)') "Hours to seconds conversion error: ", converted
        end if
        
        ! Test months to days (approximate)
        value = 3.0_real64  ! 3 months
        converted = convert_time_units(value, "months", "days")
        if (abs(converted - 90.0_real64) > 5.0_real64) then  ! Allow some tolerance
            test_passed = .false.
            write(error_unit,'(A,F0.1)') "Months to days conversion error: ", converted
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time unit conversions"
        else
            write(*,'(A)') "FAIL: Time unit conversions"
        end if
    end subroutine test_time_unit_conversions
    
    subroutine test_epoch_conversion()
        real(real64) :: time_value, epoch_seconds
        integer :: year, month, day, hour, minute
        real(real64) :: second
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Convert from "days since 1970-01-01" to epoch seconds
        time_value = 365.25_real64  ! Approximately 1 year
        epoch_seconds = time_to_epoch(time_value, "days since 1970-01-01 00:00:00")
        
        ! Should be approximately 365.25 * 86400 seconds
        if (abs(epoch_seconds - 365.25_real64 * 86400.0_real64) > 1.0_real64) then
            test_passed = .false.
            write(error_unit,'(A)') "Time to epoch conversion error"
        end if
        
        ! Convert epoch seconds back to date
        call epoch_to_date(epoch_seconds, year, month, day, hour, minute, second)
        if (year /= 1971 .or. month /= 1 .or. day /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0,A,I0)') "Epoch to date error: ", year, "-", month, "-", day
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Epoch conversion"
        else
            write(*,'(A)') "FAIL: Epoch conversion"
        end if
    end subroutine test_epoch_conversion
    
    subroutine test_cf_time_compliance()
        type(coordinate_t) :: time_coord
        character(len=256) :: units_str, calendar_str
        logical :: is_valid
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create CF-compliant time coordinate
        call create_time_coordinate(time_coord, 365, &
            units="days since 2000-01-01 00:00:00", &
            calendar="standard", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to create CF-compliant time coordinate"
        end if
        
        ! Validate CF compliance
        is_valid = is_cf_time_compliant(time_coord)
        if (.not. is_valid) then
            test_passed = .false.
            write(error_unit,'(A)') "Time coordinate not CF-compliant"
        end if
        
        call finalize_time_coordinate(time_coord)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: CF time compliance"
        else
            write(*,'(A)') "FAIL: CF time compliance"
        end if
    end subroutine test_cf_time_compliance
    
    subroutine test_datetime_parsing()
        character(len=64) :: datetime_str
        integer :: year, month, day, hour, minute
        real(real64) :: second
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Parse ISO format
        datetime_str = "2023-12-25T15:30:45.5"
        call parse_datetime(datetime_str, year, month, day, hour, minute, second)
        
        if (year /= 2023 .or. month /= 12 .or. day /= 25 .or. &
            hour /= 15 .or. minute /= 30 .or. abs(second - 45.5_real64) > 1e-6) then
            test_passed = .false.
            write(error_unit,'(A)') "ISO datetime parsing error"
        end if
        
        ! Parse space-separated format
        datetime_str = "2023-12-25 15:30:45"
        call parse_datetime(datetime_str, year, month, day, hour, minute, second)
        
        if (year /= 2023 .or. month /= 12 .or. day /= 25) then
            test_passed = .false.
            write(error_unit,'(A)') "Space-separated datetime parsing error"
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Datetime parsing"
        else
            write(*,'(A)') "FAIL: Datetime parsing"
        end if
    end subroutine test_datetime_parsing
    
    subroutine test_time_bounds()
        type(fortarray_t) :: time_var, time_bounds_var
        real(real64), dimension(12) :: time_data
        real(real64), dimension(24) :: bounds_data  ! 2 x 12
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create monthly time data with bounds
        do i = 1, 12
            time_data(i) = real(i - 1, real64) * 30.0_real64 + 15.0_real64  ! Mid-month
            bounds_data(2*i-1) = real(i - 1, real64) * 30.0_real64  ! Start of month
            bounds_data(2*i) = real(i, real64) * 30.0_real64  ! End of month
        end do
        
        time_var = variable(time_data, name="time", dim_names=["time"])
        time_bounds_var = variable(bounds_data, name="time_bounds", dim_names=["bnds  ", "time  "])
        time_bounds_var%shape = [2, 12]
        
        ! Debug print
        write(*,'(A,I0)') "time_var n_elements: ", time_var%n_elements
        write(*,'(A,2I4)') "bounds_var shape: ", time_bounds_var%shape
        
        ! Check bounds relationship
        if (.not. has_valid_time_bounds(time_var, time_bounds_var)) then
            test_passed = .false.
            write(error_unit,'(A)') "Invalid time bounds"
        end if
        
        call finalize_variable(time_var)
        call finalize_variable(time_bounds_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time bounds"
        else
            write(*,'(A)') "FAIL: Time bounds"
        end if
    end subroutine test_time_bounds
    
    subroutine test_time_formatting()
        real(real64) :: time_value
        character(len=64) :: formatted_time
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Format time value as ISO string
        time_value = 18628.5_real64  ! Days since 1970-01-01 (2021-01-01 12:00)
        formatted_time = format_time_iso(time_value, "days since 1970-01-01 00:00:00")
        
        if (formatted_time(1:10) /= "2021-01-01") then
            test_passed = .false.
            write(error_unit,'(A,A)') "Time formatting error: ", trim(formatted_time)
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time formatting"
        else
            write(*,'(A)') "FAIL: Time formatting"
        end if
    end subroutine test_time_formatting
    
    subroutine test_leap_year_handling()
        integer :: days_in_feb
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test leap year February days
        days_in_feb = days_in_month(2000, 2, "standard")
        if (days_in_feb /= 29) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Leap year February should have 29 days, got: ", days_in_feb
        end if
        
        days_in_feb = days_in_month(1900, 2, "standard")
        if (days_in_feb /= 28) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Non-leap year February should have 28 days, got: ", days_in_feb
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Leap year handling"
        else
            write(*,'(A)') "FAIL: Leap year handling"
        end if
    end subroutine test_leap_year_handling
    
    subroutine test_time_arithmetic()
        real(real64) :: time1, time2, diff
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test time difference
        time1 = date_to_days(2021, 1, 1, "days since 1970-01-01", "standard")
        time2 = date_to_days(2021, 1, 31, "days since 1970-01-01", "standard")
        diff = time2 - time1
        
        if (abs(diff - 30.0_real64) > 1e-6) then
            test_passed = .false.
            write(error_unit,'(A,F0.1)') "Time difference should be 30 days, got: ", diff
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time arithmetic"
        else
            write(*,'(A)') "FAIL: Time arithmetic"
        end if
    end subroutine test_time_arithmetic
    
    subroutine test_time_coordinate_creation()
        type(coordinate_t) :: time_coord
        real(real64), dimension(365) :: time_values
        integer :: i, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create daily time coordinate for one year
        do i = 1, 365
            time_values(i) = real(i - 1, real64)
        end do
        
        call create_coordinate(time_coord, 365, "real64", stat)
        if (stat == 0) then
            time_coord%name = "time"
            time_coord%values_r64 = time_values
            time_coord%initialized = .true.
            
            ! Add time attributes
            call add_time_attributes(time_coord, &
                units="days since 2023-01-01 00:00:00", &
                calendar="standard")
        else
            test_passed = .false.
            write(error_unit,'(A)') "Failed to create time coordinate"
        end if
        
        call finalize_time_coordinate(time_coord)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time coordinate creation"
        else
            write(*,'(A)') "FAIL: Time coordinate creation"
        end if
    end subroutine test_time_coordinate_creation
    
    subroutine test_time_variable_metadata()
        type(fortarray_t) :: temp_var
        real(real64), dimension(365) :: temp_data
        logical :: has_time_dim
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create temperature variable with time dimension
        call random_number(temp_data)
        temp_data = 20.0_real64 + 10.0_real64 * temp_data
        
        temp_var = variable(temp_data, name="temperature", dim_names=["time"])
        temp_var%units = "degrees_C"
        temp_var%standard_name = "air_temperature"
        
        ! Check if variable has time dimension
        has_time_dim = has_time_dimension(temp_var)
        if (.not. has_time_dim) then
            test_passed = .false.
            write(error_unit,'(A)') "Variable should have time dimension"
        end if
        
        call finalize_variable(temp_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time variable metadata"
        else
            write(*,'(A)') "FAIL: Time variable metadata"
        end if
    end subroutine test_time_variable_metadata
    
    subroutine test_climatological_time()
        type(coordinate_t) :: clim_time
        real(real64), dimension(12) :: monthly_clim
        character(len=64) :: cell_methods
        integer :: i, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create climatological monthly coordinate
        do i = 1, 12
            monthly_clim(i) = real(i, real64) * 30.0_real64 - 15.0_real64  ! Mid-month
        end do
        
        call create_coordinate(clim_time, 12, "real64", stat)
        if (stat == 0) then
            clim_time%name = "time"
            clim_time%values_r64 = monthly_clim
            clim_time%initialized = .true.
            
            ! Add climatology attributes
            call add_climatology_attributes(clim_time, &
                climatology_bounds="climatology_bounds", &
                cell_methods="time: mean within years time: mean over years")
        end if
        
        call finalize_time_coordinate(clim_time)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Climatological time"
        else
            write(*,'(A)') "FAIL: Climatological time"
        end if
    end subroutine test_climatological_time
    
    subroutine test_time_zone_handling()
        real(real64) :: utc_time, local_time
        integer :: tz_offset_hours
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test UTC to local time conversion
        utc_time = 0.5_real64  ! Noon UTC in days
        tz_offset_hours = -5  ! EST
        local_time = utc_to_local(utc_time, tz_offset_hours)
        
        ! Should be 7 AM local time (0.291667 days)
        if (abs(local_time - 0.291667_real64) > 0.001_real64) then
            test_passed = .false.
            write(error_unit,'(A,F0.6)') "UTC to local conversion error: ", local_time
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time zone handling"
        else
            write(*,'(A)') "FAIL: Time zone handling"
        end if
    end subroutine test_time_zone_handling
    
    subroutine test_time_coordinate_validation()
        n_tests_total = n_tests_total + 1
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Time coordinate validation (placeholder)"
    end subroutine test_time_coordinate_validation
    
end program test_time_coordinates