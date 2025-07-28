program test_resample_methods
    use iso_fortran_env, only: real64, error_unit
    use fortarray_types
    use fortarray_constructors
    implicit none
    
    logical :: all_tests_passed = .true.
    
    write(*,'(A)') "Testing resample methods..."
    write(*,'(A)') "========================================="
    
    call test_resample_daily_mean()
    call test_resample_monthly_sum()
    call test_resample_yearly_max()
    call test_resample_with_frequency_string()
    call test_resample_upsampling()
    call test_resample_downsampling()
    call test_resample_aggregation_methods()
    call test_resample_time_alignment()
    call test_resample_interpolation()
    call test_resample_method_chaining()
    
    if (all_tests_passed) then
        write(*,'(A)') "========================================="
        write(*,'(A)') "All resample tests passed!"
    else
        write(error_unit,'(A)') "========================================="
        write(error_unit,'(A)') "Some resample tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_resample_daily_mean()
        type(fortarray_t) :: hourly_data, daily_mean, resampled
        type(coordinate_t) :: time_coord
        real(real64), dimension(168) :: data_values, time_values  ! 7 days * 24 hours
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create hourly data for a week
        do i = 1, 168
            data_values(i) = real(mod(i-1, 24) + 1, real64)  ! 1-24 repeating
            time_values(i) = real(i-1, real64)  ! Hours from start
        end do
        
        ! Create hourly array with time coordinate
        time_coord%name = "time"
        time_coord%length = 168
        allocate(time_coord%values_r64(168))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        hourly_data = new_array(data_values, dim_names=["time"], name="temperature")
        hourly_data%coords(1) = time_coord
        hourly_data%has_coord(1) = .true.
        
        ! Test: Resample to daily mean using type-bound method
        resampled = hourly_data%resample("D")
        daily_mean = resampled%mean()
        
        ! Check result
        if (daily_mean%n_elements /= 7) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0)') "Expected 7 daily values, got: ", daily_mean%n_elements
        end if
        
        ! Check that each day's mean is 12.5 (mean of 1-24)
        if (allocated(daily_mean%data%values_r64)) then
            do i = 1, daily_mean%n_elements
                if (abs(daily_mean%data%values_r64(i) - 12.5_real64) > 1.0e-10) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,F10.4)') "Day ", i, " mean incorrect: ", &
                        daily_mean%data%values_r64(i)
                end if
            end do
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample daily mean"
        else
            write(*,'(A)') "FAIL: Resample daily mean"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(hourly_data)
        call finalize_variable(daily_mean)
    end subroutine test_resample_daily_mean
    
    subroutine test_resample_monthly_sum()
        type(fortarray_t) :: daily_data, monthly_sum, resampled
        type(coordinate_t) :: time_coord
        real(real64), dimension(365) :: data_values, time_values
        integer :: i
        logical :: test_passed = .true.
        
        ! Create daily data for a year
        do i = 1, 365
            data_values(i) = 1.0_real64  ! Constant value for easy sum checking
            time_values(i) = real(i-1, real64)  ! Days from start
        end do
        
        ! Create daily array
        time_coord%name = "time"
        ! time_coord%units = "days since 2024-01-01"
        time_coord%length = 365
        allocate(time_coord%values_r64(365))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        daily_data = new_array(data_values, dim_names=["time"], name="precipitation")
        daily_data%coords(1) = time_coord
        daily_data%has_coord(1) = .true.
        
        ! Test: Resample to monthly sum
        resampled = daily_data%resample("M")
        monthly_sum = resampled%sum()
        
        ! Check result
        if (monthly_sum%n_elements /= 12) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 12 monthly values, got: ", monthly_sum%n_elements
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample monthly sum"
        else
            write(*,'(A)') "FAIL: Resample monthly sum"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(daily_data)
        call finalize_variable(monthly_sum)
    end subroutine test_resample_monthly_sum
    
    subroutine test_resample_yearly_max()
        type(fortarray_t) :: monthly_data, yearly_max, resampled
        type(coordinate_t) :: time_coord
        real(real64), dimension(36) :: data_values, time_values  ! 3 years
        integer :: i
        logical :: test_passed = .true.
        
        ! Create monthly data for 3 years
        do i = 1, 36
            data_values(i) = real(mod(i-1, 12) + 1, real64)  ! 1-12 repeating
            time_values(i) = real((i-1) * 30, real64)  ! Approximate days
        end do
        
        ! Create monthly array
        time_coord%name = "time"
        ! time_coord%units = "days since 2022-01-01"
        time_coord%length = 36
        allocate(time_coord%values_r64(36))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        monthly_data = new_array(data_values, dim_names=["time"], name="max_temp")
        monthly_data%coords(1) = time_coord
        monthly_data%has_coord(1) = .true.
        
        ! Test: Resample to yearly max
        resampled = monthly_data%resample("Y")
        yearly_max = resampled%max()
        
        ! Check result
        if (yearly_max%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 3 yearly values, got: ", yearly_max%n_elements
        end if
        
        ! Each year's max should be 12
        if (allocated(yearly_max%data%values_r64)) then
            do i = 1, yearly_max%n_elements
                if (abs(yearly_max%data%values_r64(i) - 12.0_real64) > 1.0e-10) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,F10.4)') "Year ", i, " max incorrect: ", &
                        yearly_max%data%values_r64(i)
                end if
            end do
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample yearly max"
        else
            write(*,'(A)') "FAIL: Resample yearly max"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(monthly_data)
        call finalize_variable(yearly_max)
    end subroutine test_resample_yearly_max
    
    subroutine test_resample_with_frequency_string()
        type(fortarray_t) :: input_data, resampled
        type(coordinate_t) :: time_coord
        real(real64), dimension(100) :: data_values, time_values
        integer :: i
        logical :: test_passed = .true.
        
        ! Create test data
        do i = 1, 100
            data_values(i) = real(i, real64)
            time_values(i) = real(i-1, real64) * 0.25  ! Quarter-day intervals
        end do
        
        time_coord%name = "time"
        ! time_coord%units = "days since 2024-01-01"
        time_coord%length = 100
        allocate(time_coord%values_r64(100))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        input_data = new_array(data_values, dim_names=["time"], name="data")
        input_data%coords(1) = time_coord
        input_data%has_coord(1) = .true.
        
        ! Test various frequency strings
        resampled = input_data%resample("W")  ! Weekly
        if (resampled%n_elements < 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Weekly resample failed"
        end if
        call finalize_variable(resampled)
        
        resampled = input_data%resample("H")  ! Hourly (upsampling)
        if (resampled%n_elements <= input_data%n_elements) then
            test_passed = .false.
            write(error_unit,'(A)') "Hourly upsampling failed"
        end if
        call finalize_variable(resampled)
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample with frequency string"
        else
            write(*,'(A)') "FAIL: Resample with frequency string"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(input_data)
    end subroutine test_resample_with_frequency_string
    
    subroutine test_resample_upsampling()
        type(fortarray_t) :: monthly_data, daily_data, resampled
        type(coordinate_t) :: time_coord
        real(real64), dimension(12) :: data_values, time_values
        integer :: i
        logical :: test_passed = .true.
        
        ! Create monthly data
        do i = 1, 12
            data_values(i) = real(i * 10, real64)
            time_values(i) = real((i-1) * 30, real64)  ! Approximate monthly
        end do
        
        time_coord%name = "time"
        ! time_coord%units = "days since 2024-01-01"
        time_coord%length = 12
        allocate(time_coord%values_r64(12))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        monthly_data = new_array(data_values, dim_names=["time"], name="monthly_avg")
        monthly_data%coords(1) = time_coord
        monthly_data%has_coord(1) = .true.
        
        ! Test: Upsample to daily
        resampled = monthly_data%resample("D")
        daily_data = resampled%interpolate()
        
        ! Check that we have more data points
        if (daily_data%n_elements <= monthly_data%n_elements) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0)') "Upsampling failed: ", &
                monthly_data%n_elements, " -> ", daily_data%n_elements
        end if
        
        ! Should have approximately 360 days
        if (daily_data%n_elements < 350 .or. daily_data%n_elements > 370) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected ~360 daily values, got: ", daily_data%n_elements
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample upsampling"
        else
            write(*,'(A)') "FAIL: Resample upsampling"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(monthly_data)
        call finalize_variable(daily_data)
    end subroutine test_resample_upsampling
    
    subroutine test_resample_downsampling()
        type(fortarray_t) :: hourly_data, weekly_data, resampled
        type(coordinate_t) :: time_coord
        real(real64), dimension(720) :: data_values, time_values  ! 30 days hourly
        integer :: i
        logical :: test_passed = .true.
        
        ! Create hourly data
        do i = 1, 720
            data_values(i) = sin(real(i, real64) * 0.1_real64) * 100.0_real64
            time_values(i) = real(i-1, real64) / 24.0_real64  ! Hours to days
        end do
        
        time_coord%name = "time"
        ! time_coord%units = "days since 2024-01-01"
        time_coord%length = 720
        allocate(time_coord%values_r64(720))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        hourly_data = new_array(data_values, dim_names=["time"], name="hourly_temp")
        hourly_data%coords(1) = time_coord
        hourly_data%has_coord(1) = .true.
        
        ! Test: Downsample to weekly
        resampled = hourly_data%resample("W")
        weekly_data = resampled%mean()
        
        ! Check that we have fewer data points
        if (weekly_data%n_elements >= hourly_data%n_elements) then
            test_passed = .false.
            write(error_unit,'(A)') "Downsampling failed"
        end if
        
        ! Should have about 4 weeks
        if (weekly_data%n_elements < 4 .or. weekly_data%n_elements > 5) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 4-5 weekly values, got: ", weekly_data%n_elements
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample downsampling"
        else
            write(*,'(A)') "FAIL: Resample downsampling"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(hourly_data)
        call finalize_variable(weekly_data)
    end subroutine test_resample_downsampling
    
    subroutine test_resample_aggregation_methods()
        type(fortarray_t) :: input_data, resampled, temp
        type(coordinate_t) :: time_coord
        real(real64), dimension(48) :: data_values, time_values  ! 2 days hourly
        integer :: i
        logical :: test_passed = .true.
        
        ! Create test data with known pattern
        do i = 1, 48
            data_values(i) = real(i, real64)
            time_values(i) = real(i-1, real64) / 24.0_real64  ! Hours to days
        end do
        
        time_coord%name = "time"
        ! time_coord%units = "days since 2024-01-01"
        time_coord%length = 48
        allocate(time_coord%values_r64(48))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        input_data = new_array(data_values, dim_names=["time"], name="test_data")
        input_data%coords(1) = time_coord
        input_data%has_coord(1) = .true.
        
        ! Test different aggregation methods
        temp = input_data%resample("D")
        resampled = temp%min()
        if (resampled%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Daily min resample failed"
        end if
        ! First day min should be 1, second day min should be 25
        if (allocated(resampled%data%values_r64)) then
            if (abs(resampled%data%values_r64(1) - 1.0_real64) > 1.0e-10 .or. &
                abs(resampled%data%values_r64(2) - 25.0_real64) > 1.0e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Daily min values incorrect"
            end if
        end if
        call finalize_variable(resampled)
        
        temp = input_data%resample("D")
        resampled = temp%max()
        if (resampled%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Daily max resample failed"
        end if
        ! First day max should be 24, second day max should be 48
        if (allocated(resampled%data%values_r64)) then
            if (abs(resampled%data%values_r64(1) - 24.0_real64) > 1.0e-10 .or. &
                abs(resampled%data%values_r64(2) - 48.0_real64) > 1.0e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Daily max values incorrect"
            end if
        end if
        call finalize_variable(resampled)
        
        temp = input_data%resample("D")
        resampled = temp%std()
        if (resampled%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Daily std resample failed"
        end if
        call finalize_variable(resampled)
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample aggregation methods"
        else
            write(*,'(A)') "FAIL: Resample aggregation methods"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(input_data)
    end subroutine test_resample_aggregation_methods
    
    subroutine test_resample_time_alignment()
        type(fortarray_t) :: input_data, resampled, temp
        type(coordinate_t) :: time_coord
        real(real64), dimension(100) :: data_values, time_values
        integer :: i
        logical :: test_passed = .true.
        
        ! Create data with irregular time spacing
        do i = 1, 100
            data_values(i) = real(i, real64)
            ! Irregular time spacing
            time_values(i) = real(i-1, real64) + 0.1_real64 * sin(real(i, real64))
        end do
        
        time_coord%name = "time"
        ! time_coord%units = "hours since 2024-01-01"
        time_coord%length = 100
        allocate(time_coord%values_r64(100))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        input_data = new_array(data_values, dim_names=["time"], name="irregular_data")
        input_data%coords(1) = time_coord
        input_data%has_coord(1) = .true.
        
        ! Test: Resample with alignment
        temp = input_data%resample("6H", align="left")
        resampled = temp%mean()
        
        ! Check that time coordinates are aligned
        if (allocated(resampled%coords(1)%values_r64)) then
            do i = 1, resampled%n_elements
                if (mod(nint(resampled%coords(1)%values_r64(i)), 6) /= 0) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,F10.4)') "Time point ", i, &
                        " not aligned to 6H boundary: ", resampled%coords(1)%values_r64(i)
                end if
            end do
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample time alignment"
        else
            write(*,'(A)') "FAIL: Resample time alignment"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(input_data)
        call finalize_variable(resampled)
    end subroutine test_resample_time_alignment
    
    subroutine test_resample_interpolation()
        type(fortarray_t) :: sparse_data, dense_data, temp
        type(coordinate_t) :: time_coord
        real(real64), dimension(5) :: sparse_values, sparse_times
        integer :: i
        logical :: test_passed = .true.
        
        ! Create sparse data
        sparse_values = [10.0_real64, 20.0_real64, 15.0_real64, 25.0_real64, 30.0_real64]
        sparse_times = [0.0_real64, 7.0_real64, 14.0_real64, 21.0_real64, 28.0_real64]
        
        time_coord%name = "time"
        ! time_coord%units = "days since 2024-01-01"
        time_coord%length = 5
        allocate(time_coord%values_r64(5))
        time_coord%values_r64 = sparse_times
        time_coord%dtype = DTYPE_REAL64
        
        sparse_data = new_array(sparse_values, dim_names=["time"], name="sparse_data")
        sparse_data%coords(1) = time_coord
        sparse_data%has_coord(1) = .true.
        
        ! Test: Resample with interpolation
        temp = sparse_data%resample("D")
        dense_data = temp%interpolate("linear")
        
        ! Check that we have daily data
        if (dense_data%n_elements < 28) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected at least 28 daily values, got: ", &
                dense_data%n_elements
        end if
        
        ! Check that interpolated values are reasonable
        if (allocated(dense_data%data%values_r64)) then
            ! Check value at day 3.5 (should be between 10 and 20)
            if (dense_data%n_elements > 3) then
                if (dense_data%data%values_r64(4) < 10.0_real64 .or. &
                    dense_data%data%values_r64(4) > 20.0_real64) then
                    test_passed = .false.
                    write(error_unit,'(A,F10.4)') "Interpolated value out of range: ", &
                        dense_data%data%values_r64(4)
                end if
            end if
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample interpolation"
        else
            write(*,'(A)') "FAIL: Resample interpolation"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(sparse_data)
        call finalize_variable(dense_data)
    end subroutine test_resample_interpolation
    
    subroutine test_resample_method_chaining()
        type(fortarray_t) :: input_data, result, temp1, temp2
        type(coordinate_t) :: time_coord
        real(real64), dimension(168) :: data_values, time_values  ! 1 week hourly
        integer :: i
        logical :: test_passed = .true.
        
        ! Create test data
        do i = 1, 168
            data_values(i) = real(i, real64) + 10.0_real64 * sin(real(i, real64) * 0.1_real64)
            time_values(i) = real(i-1, real64) / 24.0_real64  ! Hours to days
        end do
        
        time_coord%name = "time"
        ! time_coord%units = "days since 2024-01-01"
        time_coord%length = 168
        allocate(time_coord%values_r64(168))
        time_coord%values_r64 = time_values
        time_coord%dtype = DTYPE_REAL64
        
        input_data = new_array(data_values, dim_names=["time"], name="chain_test")
        input_data%coords(1) = time_coord
        input_data%has_coord(1) = .true.
        
        ! Test: Chain resample with other operations
        temp1 = input_data%resample("D")
        temp2 = temp1%mean()
        result = temp2%fillna_value(0.0_real64)
        
        if (result%n_elements /= 7) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Chained resample failed, expected 7 days, got: ", &
                result%n_elements
        end if
        
        ! Test another chain
        call finalize_variable(result)
        temp1 = input_data%sel_range("time", 1.0_real64, 3.0_real64)
        temp2 = temp1%resample("12H")
        result = temp2%max()
        
        if (result%n_elements < 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Chained selection and resample failed"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Resample method chaining"
        else
            write(*,'(A)') "FAIL: Resample method chaining"
            all_tests_passed = .false.
        end if
        
        call finalize_variable(input_data)
        call finalize_variable(result)
    end subroutine test_resample_method_chaining
    
    subroutine finalize_variable(var)
        type(fortarray_t), intent(inout) :: var
        integer :: i
        
        if (allocated(var%dim_names)) deallocate(var%dim_names)
        if (allocated(var%shape)) deallocate(var%shape)
        if (allocated(var%strides)) deallocate(var%strides)
        if (allocated(var%coords)) then
            do i = 1, size(var%coords)
                if (allocated(var%coords(i)%values_r64)) deallocate(var%coords(i)%values_r64)
                if (allocated(var%coords(i)%values_r32)) deallocate(var%coords(i)%values_r32)
                if (allocated(var%coords(i)%values_i32)) deallocate(var%coords(i)%values_i32)
                if (allocated(var%coords(i)%values_char)) deallocate(var%coords(i)%values_char)
            end do
            deallocate(var%coords)
        end if
        if (allocated(var%has_coord)) deallocate(var%has_coord)
        if (allocated(var%attrs)) deallocate(var%attrs)
        
        ! Clean up data storage
        if (allocated(var%data%values_r64)) deallocate(var%data%values_r64)
        if (allocated(var%data%values_r32)) deallocate(var%data%values_r32)
        if (allocated(var%data%values_i64)) deallocate(var%data%values_i64)
        if (allocated(var%data%values_i32)) deallocate(var%data%values_i32)
        if (allocated(var%data%values_char)) deallocate(var%data%values_char)
        if (allocated(var%data%values_logical)) deallocate(var%data%values_logical)
        
        var%initialized = .false.
    end subroutine finalize_variable
    
end program test_resample_methods