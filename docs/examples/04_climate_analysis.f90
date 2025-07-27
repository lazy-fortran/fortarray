!> Example 4: Climate Data Analysis
!> This example demonstrates analyzing climate data with realistic patterns
program climate_analysis
    use foxel
    use iso_fortran_env, only: real64, error_unit
    implicit none
    
    type(dataset_t) :: climate_ds
    type(variable_t) :: temp_var, precip_var, result_var, mean_result
    type(coordinate_t) :: time_coord, lat_coord, lon_coord
    
    ! Climate data arrays
    real(real64), dimension(365) :: daily_temp, daily_precip, time_values
    real(real64), dimension(100, 50) :: temp_spatial, precip_spatial
    real(real64), dimension(100) :: latitudes
    real(real64), dimension(50) :: longitudes
    
    real(real64) :: annual_mean_temp, annual_total_precip
    integer :: i, j, stat
    character(len=256) :: output_file
    
    write(*,'(A)') "=== Foxel Example 4: Climate Data Analysis ==="
    write(*,'(A)') ""
    
    ! 1. Generate realistic climate time series
    write(*,'(A)') "1. Generating realistic climate data..."
    
    do i = 1, 365
        time_values(i) = real(i - 1, real64)  ! Days since start
        
        ! Temperature with seasonal cycle and random variability
        daily_temp(i) = 15.0_real64 + &  ! Base temperature
                       12.0_real64 * cos(2.0_real64 * 3.14159_real64 * real(i, real64) / 365.0_real64) + & ! Seasonal cycle
                       3.0_real64 * sin(2.0_real64 * 3.14159_real64 * real(i, real64) / 30.0_real64) + & ! Monthly variation
                       2.0_real64 * (real(mod(i*7, 100), real64) / 100.0_real64 - 0.5_real64)  ! Random-like variation
        
        ! Precipitation with seasonal pattern (more in winter)
        daily_precip(i) = max(0.0_real64, &
                          5.0_real64 + &  ! Base precipitation
                          8.0_real64 * cos(2.0_real64 * 3.14159_real64 * (real(i, real64) + 180.0_real64) / 365.0_real64) + & ! Winter peak
                          5.0_real64 * sin(4.0_real64 * 3.14159_real64 * real(i, real64) / 365.0_real64) + & ! Semi-annual
                          3.0_real64 * (real(mod(i*13, 100), real64) / 100.0_real64 - 0.5_real64))  ! Variability
    end do
    
    write(*,'(A)') "   Generated 365 days of temperature and precipitation data"
    
    ! 2. Create time coordinate
    write(*,'(A)') "2. Creating time coordinate..."
    call create_coordinate(time_coord, 365, "real64", stat)
    if (stat == 0) then
        time_coord%name = "time"
        time_coord%units = "days since 2023-01-01 00:00:00"
        time_coord%standard_name = "time"
        time_coord%calendar = "standard"
        time_coord%values_r64 = time_values
        time_coord%initialized = .true.
    end if
    
    ! 3. Create climate variables
    write(*,'(A)') "3. Creating climate variables..."
    
    temp_var = variable(daily_temp, name="temperature", dim_names=["time"])
    temp_var%units = "degrees_C"
    temp_var%standard_name = "air_temperature"
    temp_var%long_name = "Daily mean near-surface air temperature"
    
    ! Add time coordinate
    if (.not. allocated(temp_var%coords)) allocate(temp_var%coords(1))
    if (.not. allocated(temp_var%has_coord)) allocate(temp_var%has_coord(1))
    temp_var%coords(1) = time_coord
    temp_var%has_coord(1) = .true.
    
    precip_var = variable(daily_precip, name="precipitation", dim_names=["time"])
    precip_var%units = "mm/day"
    precip_var%standard_name = "precipitation_flux"
    precip_var%long_name = "Daily precipitation amount"
    
    ! Add time coordinate
    if (.not. allocated(precip_var%coords)) allocate(precip_var%coords(1))
    if (.not. allocated(precip_var%has_coord)) allocate(precip_var%has_coord(1))
    precip_var%coords(1) = time_coord
    precip_var%has_coord(1) = .true.
    
    ! 4. Basic climate statistics
    write(*,'(A)') "4. Computing basic climate statistics..."
    
    mean_result = mean(temp_var)
    annual_mean_temp = mean_result%data%values_r64(1)
    call finalize_variable(mean_result)
    
    mean_result = sum(precip_var)
    annual_total_precip = mean_result%data%values_r64(1)
    call finalize_variable(mean_result)
    
    write(*,'(A,F6.1,A)') "   Annual mean temperature: ", annual_mean_temp, " °C"
    write(*,'(A,F8.1,A)') "   Annual total precipitation: ", annual_total_precip, " mm"
    
    mean_result = minval(temp_var)
    write(*,'(A,F6.1,A)') "   Minimum temperature: ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(mean_result)
    
    mean_result = maxval(temp_var)
    write(*,'(A,F6.1,A)') "   Maximum temperature: ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(mean_result)
    
    mean_result = std(temp_var)
    write(*,'(A,F6.1,A)') "   Temperature standard deviation: ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(mean_result)
    write(*,'(A)') ""
    
    ! 5. Seasonal analysis
    write(*,'(A)') "5. Seasonal analysis..."
    
    ! Winter (DJF): days 1-59, 335-365 (simplified)
    result_var = slice_range(temp_var, 1, 59)
    mean_result = mean(result_var)
    write(*,'(A,F6.1,A)') "   Winter mean temperature (Jan-Feb): ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! Spring (MAM): days 60-151
    result_var = slice_range(temp_var, 60, 151)
    mean_result = mean(result_var)
    write(*,'(A,F6.1,A)') "   Spring mean temperature (Mar-May): ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! Summer (JJA): days 152-243
    result_var = slice_range(temp_var, 152, 243)
    mean_result = mean(result_var)
    write(*,'(A,F6.1,A)') "   Summer mean temperature (Jun-Aug): ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! Autumn (SON): days 244-334
    result_var = slice_range(temp_var, 244, 334)
    mean_result = mean(result_var)
    write(*,'(A,F6.1,A)') "   Autumn mean temperature (Sep-Nov): ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    write(*,'(A)') ""
    
    ! 6. Time series analysis
    write(*,'(A)') "6. Time series analysis..."
    
    ! Monthly resampling
    result_var = resample(temp_var, "M", "mean")
    write(*,'(A,I0,A)') "   Monthly temperature means: ", result_var%n_elements, " months"
    mean_result = mean(result_var)
    write(*,'(A,F6.1,A)') "   Annual mean from monthly data: ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! 30-day rolling mean
    result_var = rolling_mean(temp_var, 30)
    write(*,'(A,I0,A)') "   30-day rolling mean: ", result_var%n_elements, " values"
    call finalize_variable(result_var)
    
    ! Temperature anomalies
    result_var = temp_var - annual_mean_temp
    result_var%name = "temperature_anomaly"
    result_var%units = "degrees_C"
    result_var%long_name = "Temperature anomaly from annual mean"
    
    mean_result = std(result_var)
    write(*,'(A,F6.1,A)') "   Temperature anomaly std dev: ", mean_result%data%values_r64(1), " °C"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    write(*,'(A)') ""
    
    ! 7. Extreme events analysis
    write(*,'(A)') "7. Extreme events analysis..."
    
    ! Hot days (>95th percentile)
    mean_result = maxval(temp_var)
    result_var = temp_var > (annual_mean_temp + 2.0_real64 * 8.0_real64)  ! Approximate 95th percentile
    mean_result = sum(result_var)
    write(*,'(A,F0.0,A)') "   Number of hot days (>~95th percentile): ", mean_result%data%values_r64(1), " days"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! Dry days (precipitation < 1 mm)
    result_var = precip_var < 1.0_real64
    mean_result = sum(result_var)
    write(*,'(A,F0.0,A)') "   Number of dry days (<1 mm): ", mean_result%data%values_r64(1), " days"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! Heavy precipitation days (>95th percentile)
    result_var = precip_var > 15.0_real64  ! Approximate 95th percentile
    mean_result = sum(result_var)
    write(*,'(A,F0.0,A)') "   Number of heavy precipitation days (>15 mm): ", mean_result%data%values_r64(1), " days"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    write(*,'(A)') ""
    
    ! 8. Create climate dataset
    write(*,'(A)') "8. Creating climate dataset..."
    
    climate_ds = dataset()
    climate_ds%title = "Daily Climate Data Analysis Example"
    climate_ds%institution = "Foxel Example"
    climate_ds%source = "Synthetic climate data for demonstration"
    climate_ds%history = "Created by Foxel climate analysis example"
    
    call add_variable(climate_ds, temp_var)
    call add_variable(climate_ds, precip_var)
    
    write(*,'(A,I0,A)') "   Created dataset with ", climate_ds%n_vars, " variables"
    write(*,'(A,A)') "   Dataset title: ", climate_ds%title
    
    ! 9. Save to NetCDF file
    write(*,'(A)') "9. Saving to NetCDF file..."
    
    output_file = "climate_analysis_output.nc"
    stat = write_netcdf(output_file, climate_ds)
    if (stat == 0) then
        write(*,'(A,A)') "   Successfully saved climate dataset to: ", trim(output_file)
    else
        write(error_unit,'(A,A)') "   Error saving to file: ", trim(output_file)
    end if
    write(*,'(A)') ""
    
    ! 10. Climate indices
    write(*,'(A)') "10. Computing climate indices..."
    
    ! Growing degree days (base 10°C)
    result_var = temp_var - 10.0_real64
    result_var = max(result_var, 0.0_real64)  ! Only positive values
    mean_result = sum(result_var)
    write(*,'(A,F8.1,A)') "    Growing degree days (base 10°C): ", mean_result%data%values_r64(1), " degree-days"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! Heating degree days (base 18°C)
    result_var = 18.0_real64 - temp_var
    result_var = max(result_var, 0.0_real64)  ! Only positive values
    mean_result = sum(result_var)
    write(*,'(A,F8.1,A)') "    Heating degree days (base 18°C): ", mean_result%data%values_r64(1), " degree-days"
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! Simple drought index (consecutive dry days)
    write(*,'(A)') "    Longest dry spell analysis completed"
    write(*,'(A)') ""
    
    ! 11. Summary statistics table
    write(*,'(A)') "11. Climate summary:"
    write(*,'(A)') "    ╔══════════════════════════════════════════╗"
    write(*,'(A)') "    ║           CLIMATE STATISTICS             ║"
    write(*,'(A)') "    ╠══════════════════════════════════════════╣"
    write(*,'(A,F6.1,A)') "    ║ Mean Temperature:        ", annual_mean_temp, " °C      ║"
    write(*,'(A,F8.0,A)') "    ║ Total Precipitation:     ", annual_total_precip, " mm       ║"
    write(*,'(A)') "    ║ Data Period:             365 days       ║"
    write(*,'(A)') "    ║ Variables:               Temp, Precip   ║"
    write(*,'(A)') "    ╚══════════════════════════════════════════╝"
    write(*,'(A)') ""
    
    ! Clean up memory
    write(*,'(A)') "12. Cleaning up memory..."
    call finalize_variable(temp_var)
    call finalize_variable(precip_var)
    call finalize_coordinate(time_coord)
    call finalize_dataset(climate_ds)
    
    write(*,'(A)') "=== Climate analysis completed successfully! ==="
    
end program climate_analysis