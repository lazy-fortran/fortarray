program test_time_groupby
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Time-based Groupby Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run tests
    call test_month_extraction()
    call test_year_extraction()
    call test_season_extraction()
    call test_dayofyear_extraction()
    call test_weekday_extraction()
    call test_time_groupby_operations()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Time-based Groupby Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some time-based groupby tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All time-based groupby tests passed!"
    end if

contains

    subroutine test_month_extraction()
        type(fortarray_t) :: data_var, time_coord
        type(groupby_t) :: gb
        real(real64), dimension(12) :: temperature_data, time_values
        real(real64), dimension(4) :: month_counts_expected
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing month extraction from time coordinates..."
        
        ! Create monthly temperature data (days since Jan 1, 2000)
        ! Day 15 of each month for year 2000
        time_values = [15.0_real64, 46.0_real64, 75.0_real64, 106.0_real64, &  ! Jan-Apr
                      136.0_real64, 167.0_real64, 197.0_real64, 228.0_real64, & ! May-Aug
                      259.0_real64, 289.0_real64, 320.0_real64, 350.0_real64]   ! Sep-Dec
        
        ! Create temperature data (some pattern)
        do i = 1, 12
            temperature_data(i) = 20.0_real64 + 10.0_real64 * sin(real(i-1, real64) * 3.14159_real64 / 6.0_real64)
        end do
        
        ! Create fortarray with time coordinate
        data_var = new_array(temperature_data, name="temperature", dim_names=["time"])
        time_coord = new_array(time_values, name="time", dim_names=["time"])
        
        ! Add coordinate to data variable
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "time"
        data_var%coords(1)%length = 12
        allocate(data_var%coords(1)%values_r64(12))
        data_var%coords(1)%values_r64 = time_values
        data_var%has_coord(1) = .true.
        
        ! Test groupby by month
        write(*,'(A)') "   Test 1: Groupby by time.month"
        gb = data_var%groupby_coord("time.month")
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Month groupby failed to initialize"
        else if (gb%n_groups /= 12) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 12 month groups, got ", gb%n_groups, " groups"
        else
            write(*,'(A)') "     PASS: Month groupby created successfully"
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        call finalize_variable(time_coord)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Month extraction test"
        else
            write(*,'(A)') "FAIL: Month extraction test"
        end if
    end subroutine test_month_extraction
    
    subroutine test_year_extraction()
        type(fortarray_t) :: data_var, time_coord
        type(groupby_t) :: gb
        real(real64), dimension(24) :: temperature_data, time_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing year extraction from time coordinates..."
        
        ! Create 2-year monthly data (24 months)
        ! Day 15 of each month for years 2000-2001
        do i = 1, 12
            time_values(i) = real(15 + (i-1)*30, real64)  ! 2000
            time_values(i+12) = real(365 + 15 + (i-1)*30, real64)  ! 2001
            temperature_data(i) = 20.0_real64 + real(i, real64)
            temperature_data(i+12) = 25.0_real64 + real(i, real64)
        end do
        
        ! Create fortarray with time coordinate
        data_var = new_array(temperature_data, name="temperature", dim_names=["time"])
        
        ! Add coordinate to data variable
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "time"
        data_var%coords(1)%length = 24
        allocate(data_var%coords(1)%values_r64(24))
        data_var%coords(1)%values_r64 = time_values
        data_var%has_coord(1) = .true.
        
        ! Test groupby by year
        write(*,'(A)') "   Test 1: Groupby by time.year"
        gb = data_var%groupby_coord("time.year")
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Year groupby failed to initialize"
        else if (gb%n_groups /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 2 year groups, got ", gb%n_groups, " groups"
        else
            write(*,'(A)') "     PASS: Year groupby created successfully"
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Year extraction test"
        else
            write(*,'(A)') "FAIL: Year extraction test"
        end if
    end subroutine test_year_extraction
    
    subroutine test_season_extraction()
        type(fortarray_t) :: data_var
        type(groupby_t) :: gb
        real(real64), dimension(12) :: temperature_data, time_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing season extraction from time coordinates..."
        
        ! Create seasonal data: 3 months per season
        time_values = [75.0_real64, 105.0_real64, 135.0_real64, &   ! Spring (Mar-May)
                      166.0_real64, 196.0_real64, 227.0_real64, &  ! Summer (Jun-Aug)
                      258.0_real64, 288.0_real64, 319.0_real64, &  ! Fall (Sep-Nov)
                      15.0_real64, 46.0_real64, 349.0_real64]      ! Winter (Jan, Feb, Dec)
        
        do i = 1, 12
            temperature_data(i) = 15.0_real64 + real(i, real64)
        end do
        
        data_var = new_array(temperature_data, name="temperature", dim_names=["time"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "time"
        data_var%coords(1)%length = 12
        allocate(data_var%coords(1)%values_r64(12))
        data_var%coords(1)%values_r64 = time_values
        data_var%has_coord(1) = .true.
        
        ! Test groupby by season
        write(*,'(A)') "   Test 1: Groupby by time.season"
        gb = data_var%groupby_coord("time.season")
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Season groupby failed to initialize"
        else if (gb%n_groups /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 4 season groups, got ", gb%n_groups, " groups"
        else
            write(*,'(A)') "     PASS: Season groupby created successfully"
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Season extraction test"
        else
            write(*,'(A)') "FAIL: Season extraction test"
        end if
    end subroutine test_season_extraction
    
    subroutine test_dayofyear_extraction()
        type(fortarray_t) :: data_var
        type(groupby_t) :: gb
        real(real64), dimension(4) :: temperature_data, time_values
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing day-of-year extraction from time coordinates..."
        
        ! Create data for specific days of year
        time_values = [59.0_real64, 152.0_real64, 213.0_real64, 334.0_real64] ! Different days
        temperature_data = [5.0_real64, 25.0_real64, 30.0_real64, 0.0_real64]
        
        data_var = new_array(temperature_data, name="temperature", dim_names=["time"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "time"
        data_var%coords(1)%length = 4
        allocate(data_var%coords(1)%values_r64(4))
        data_var%coords(1)%values_r64 = time_values
        data_var%has_coord(1) = .true.
        
        ! Test groupby by day of year
        write(*,'(A)') "   Test 1: Groupby by time.dayofyear"
        gb = data_var%groupby_coord("time.dayofyear")
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Day-of-year groupby failed to initialize"
        else if (gb%n_groups /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 4 day groups, got ", gb%n_groups, " groups"
        else
            write(*,'(A)') "     PASS: Day-of-year groupby created successfully"
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Day-of-year extraction test"
        else
            write(*,'(A)') "FAIL: Day-of-year extraction test"
        end if
    end subroutine test_dayofyear_extraction
    
    subroutine test_weekday_extraction()
        type(fortarray_t) :: data_var
        type(groupby_t) :: gb
        real(real64), dimension(7) :: temperature_data, time_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing weekday extraction from time coordinates..."
        
        ! Create data for 7 consecutive days (one week)
        do i = 1, 7
            time_values(i) = real(i, real64)  ! Days 1-7
            temperature_data(i) = 20.0_real64 + real(i, real64)
        end do
        
        data_var = new_array(temperature_data, name="temperature", dim_names=["time"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "time"
        data_var%coords(1)%length = 7
        allocate(data_var%coords(1)%values_r64(7))
        data_var%coords(1)%values_r64 = time_values
        data_var%has_coord(1) = .true.
        
        ! Test groupby by weekday
        write(*,'(A)') "   Test 1: Groupby by time.weekday"
        gb = data_var%groupby_coord("time.weekday")
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Weekday groupby failed to initialize"
        else if (gb%n_groups /= 7) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 7 weekday groups, got ", gb%n_groups, " groups"
        else
            write(*,'(A)') "     PASS: Weekday groupby created successfully"
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Weekday extraction test"
        else
            write(*,'(A)') "FAIL: Weekday extraction test"
        end if
    end subroutine test_weekday_extraction
    
    subroutine test_time_groupby_operations()
        type(fortarray_t) :: data_var, monthly_mean
        type(groupby_t) :: gb
        real(real64), dimension(12) :: temperature_data, time_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing time-based groupby aggregation operations..."
        
        ! Create monthly temperature data
        time_values = [15.0_real64, 46.0_real64, 75.0_real64, 106.0_real64, &
                      136.0_real64, 167.0_real64, 197.0_real64, 228.0_real64, &
                      259.0_real64, 289.0_real64, 320.0_real64, 350.0_real64]
        
        do i = 1, 12
            temperature_data(i) = 15.0_real64 + 5.0_real64 * sin(real(i-1, real64) * 3.14159_real64 / 6.0_real64)
        end do
        
        data_var = new_array(temperature_data, name="temperature", dim_names=["time"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "time"
        data_var%coords(1)%length = 12
        allocate(data_var%coords(1)%values_r64(12))
        data_var%coords(1)%values_r64 = time_values
        data_var%has_coord(1) = .true.
        
        ! Test seasonal aggregation
        write(*,'(A)') "   Test 1: Seasonal mean aggregation"
        gb = data_var%groupby_coord("time.season")
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Seasonal groupby failed"
        else
            monthly_mean = gb%mean()
            if (monthly_mean%n_elements /= 4) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 4 seasonal means, got ", monthly_mean%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Seasonal mean computed successfully"
            end if
            call finalize_variable(monthly_mean)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Time-based groupby operations test"
        else
            write(*,'(A)') "FAIL: Time-based groupby operations test"
        end if
    end subroutine test_time_groupby_operations

end program test_time_groupby