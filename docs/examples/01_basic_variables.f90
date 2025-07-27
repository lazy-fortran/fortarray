!> Example 1: Basic Variable Operations
!> This example demonstrates creating variables, basic operations, and memory management
program basic_variables
    use foxel
    use iso_fortran_env, only: real64, error_unit
    implicit none
    
    type(variable_t) :: temp_var, pressure_var, result_var, mean_result
    real(real64), dimension(10) :: temperatures, pressures
    real(real64) :: mean_temp
    integer :: i
    
    write(*,'(A)') "=== Foxel Example 1: Basic Variables ==="
    write(*,'(A)') ""
    
    ! Generate sample data
    write(*,'(A)') "1. Creating sample data..."
    do i = 1, 10
        temperatures(i) = 20.0_real64 + 5.0_real64 * sin(real(i, real64) * 0.5_real64)
        pressures(i) = 1013.25_real64 + 10.0_real64 * cos(real(i, real64) * 0.3_real64)
    end do
    
    ! Create variables with metadata
    write(*,'(A)') "2. Creating variables with metadata..."
    temp_var = variable(temperatures, name="temperature", dim_names=["time"])
    temp_var%units = "degrees_C"
    temp_var%standard_name = "air_temperature"
    temp_var%long_name = "Near-surface air temperature"
    
    pressure_var = variable(pressures, name="pressure", dim_names=["time"])
    pressure_var%units = "hPa"
    pressure_var%standard_name = "air_pressure"
    pressure_var%long_name = "Sea level pressure"
    
    ! Display variable information
    write(*,'(A)') "3. Variable information:"
    write(*,'(A,A)') "   Temperature variable: ", temp_var%name
    write(*,'(A,A)') "   Units: ", temp_var%units
    write(*,'(A,I0)') "   Number of elements: ", temp_var%n_elements
    write(*,'(A,I0)') "   Number of dimensions: ", temp_var%n_dims
    write(*,'(A)') ""
    
    ! Basic arithmetic operations
    write(*,'(A)') "4. Performing arithmetic operations..."
    
    ! Convert temperature to Kelvin
    result_var = temp_var + 273.15_real64
    result_var%name = "temperature_K"
    result_var%units = "K"
    write(*,'(A)') "   Converted temperature to Kelvin"
    
    ! Display some values
    write(*,'(A)') "   First 5 temperatures (°C -> K):"
    do i = 1, 5
        write(*,'(A,I0,A,F6.2,A,F7.2)') "     Point ", i, ": ", &
            temp_var%data%values_r64(i), " °C -> ", result_var%data%values_r64(i), " K"
    end do
    write(*,'(A)') ""
    
    call finalize_variable(result_var)
    
    ! Compute derived quantity (temperature difference from average)
    mean_result = mean(temp_var)
    mean_temp = mean_result%data%values_r64(1)
    result_var = temp_var - mean_temp
    result_var%name = "temp_anomaly"
    result_var%units = "degrees_C"
    result_var%long_name = "Temperature anomaly from mean"
    
    write(*,'(A,F6.2,A)') "   Mean temperature: ", mean_temp, " °C"
    write(*,'(A)') "   Temperature anomalies:"
    do i = 1, 10
        write(*,'(A,I0,A,F6.2)') "     Point ", i, ": ", result_var%data%values_r64(i)
    end do
    write(*,'(A)') ""
    
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
    
    ! Variable combination
    write(*,'(A)') "5. Combining variables..."
    result_var = temp_var * pressure_var
    result_var%name = "temp_pressure_product"
    result_var%units = "degrees_C*hPa"
    write(*,'(A)') "   Created temperature-pressure product"
    write(*,'(A,F8.2)') "   First value: ", result_var%data%values_r64(1)
    
    call finalize_variable(result_var)
    
    ! Comparison operations
    write(*,'(A)') "6. Logical operations..."
    result_var = temp_var > mean_temp
    write(*,'(A,F6.2,A)') "   Points where temperature > ", mean_temp, " °C:"
    do i = 1, 10
        if (result_var%data%values_r64(i) > 0.5_real64) then  ! True values are 1.0
            write(*,'(A,I0,A,F6.2,A)') "     Point ", i, ": ", temp_var%data%values_r64(i), " °C"
        end if
    end do
    write(*,'(A)') ""
    
    call finalize_variable(result_var)
    
    ! Indexing operations
    write(*,'(A)') "7. Indexing operations..."
    write(*,'(A,F6.2)') "   Temperature at point 5: ", get_item(temp_var, 5)
    write(*,'(A,F6.2)') "   Pressure at point 3: ", get_item(pressure_var, 3)
    write(*,'(A)') ""
    
    ! Slicing operations
    write(*,'(A)') "8. Slicing operations..."
    result_var = slice_range(temp_var, 3, 7)
    write(*,'(A)') "   Temperature slice [3:7]:"
    do i = 1, result_var%n_elements
        write(*,'(A,I0,A,F6.2)') "     Element ", i, ": ", result_var%data%values_r64(i)
    end do
    write(*,'(A)') ""
    
    call finalize_variable(result_var)
    
    ! Statistical operations
    write(*,'(A)') "9. Statistical operations..."
    mean_result = mean(temp_var)
    write(*,'(A,F6.2)') "   Mean temperature: ", mean_result%data%values_r64(1)
    call finalize_variable(mean_result)
    
    mean_result = std(temp_var)
    write(*,'(A,F6.2)') "   Temperature std dev: ", mean_result%data%values_r64(1)
    call finalize_variable(mean_result)
    
    mean_result = minval(temp_var)
    write(*,'(A,F6.2)') "   Minimum temperature: ", mean_result%data%values_r64(1)
    call finalize_variable(mean_result)
    
    mean_result = maxval(temp_var)
    write(*,'(A,F6.2)') "   Maximum temperature: ", mean_result%data%values_r64(1)
    call finalize_variable(mean_result)
    write(*,'(A)') ""
    
    ! Clean up memory
    write(*,'(A)') "10. Cleaning up memory..."
    call finalize_variable(temp_var)
    call finalize_variable(pressure_var)
    
    write(*,'(A)') "=== Example completed successfully! ==="
    
end program basic_variables