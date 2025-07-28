# FortArray Getting Started Guide

FortArray is a modern Fortran library providing xarray-compatible labeled multi-dimensional arrays and datasets, following NetCDF data model conventions. This guide will help you get started with FortArray for scientific data analysis.

## Installation

### Prerequisites

- Modern Fortran compiler (gfortran 9+ or Intel Fortran)
- NetCDF-Fortran library
- HDF5 library (usually bundled with NetCDF4)
- Fortran Package Manager (fpm)

### Building with FPM

```bash
git clone https://github.com/krystophny/foxel.git
cd foxel
fpm build
fpm test
```

### Building with Make

```bash
make
make test
```

## Basic Concepts

FortArray follows the NetCDF data model with these core types:

- **`fortarray_t`**: Multi-dimensional labeled arrays (0D to nD) with xarray-compatible methods
- **`dataset_t`**: Collection of arrays with shared dimensions
- **`coordinate_t`**: 1D variables defining axes
- **`dimension_t`**: Named axes with lengths

## Quick Start

### Creating Variables

```fortran
program quick_start
    use fortarray
    implicit none
    
    type(fortarray_t) :: temp_var, pressure_var
    real(real64), dimension(365) :: temperature_data, pressure_data
    integer :: i
    
    ! Generate sample data
    do i = 1, 365
        temperature_data(i) = 20.0 + 15.0 * sin(2.0 * 3.14159 * real(i) / 365.0)
        pressure_data(i) = 1013.25 + 50.0 * cos(2.0 * 3.14159 * real(i) / 365.0)
    end do
    
    ! Create variables with xarray-style constructor
    temp_var = new_array(temperature_data, dim_names=["time"])
    temp_var%name = "temperature"
    temp_var%units = "degrees_C"
    temp_var%standard_name = "air_temperature"
    
    pressure_var = new_array(pressure_data, dim_names=["time"])
    pressure_var%name = "pressure"
    pressure_var%units = "hPa"
    pressure_var%standard_name = "air_pressure"
    
    ! Print basic information
    write(*,'(A,A)') "Temperature variable: ", temp_var%name
    write(*,'(A,I0)') "Number of elements: ", temp_var%n_elements
    write(*,'(A,A)') "Units: ", temp_var%units
    
    ! Clean up
    call finalize_variable(temp_var)
    call finalize_variable(pressure_var)
end program quick_start
```

### Working with Datasets

```fortran
program dataset_example
    use fortarray
    implicit none
    
    type(dataset_t) :: climate_data
    type(fortarray_t) :: temp_var, humid_var
    real(real64), dimension(100, 50) :: temp_2d, humid_2d
    integer :: i, j
    
    ! Generate 2D data (lat x lon)
    do j = 1, 50  ! longitude
        do i = 1, 100  ! latitude
            temp_2d(i, j) = 15.0 + 20.0 * cos(3.14159 * real(i-50) / 100.0)
            humid_2d(i, j) = 60.0 + 30.0 * sin(3.14159 * real(j) / 50.0)
        end do
    end do
    
    ! Create variables
    temp_var = variable(reshape(temp_2d, [100*50]), name="temperature", &
                       dim_names=["lat", "lon"])
    humid_var = variable(reshape(humid_2d, [100*50]), name="humidity", &
                        dim_names=["lat", "lon"])
    
    ! Create dataset
    climate_data = dataset()
    call add_variable(climate_data, temp_var)
    call add_variable(climate_data, humid_var)
    
    write(*,'(A,I0)') "Dataset contains ", climate_data%n_vars, " variables"
    
    ! Clean up
    call finalize_variable(temp_var)
    call finalize_variable(humid_var)
    call finalize_dataset(climate_data)
end program dataset_example
```

### Reading and Writing NetCDF Files

```fortran
program netcdf_io
    use foxel
    implicit none
    
    type(dataset_t) :: ds, loaded_ds
    type(variable_t) :: data_var
    real(real64), dimension(1000) :: data
    integer :: i, stat
    
    ! Generate data
    do i = 1, 1000
        data(i) = sin(real(i) * 0.01) + 0.1 * real(i)
    end do
    
    ! Create variable and dataset
    data_var = variable(data, name="signal", dim_names=["time"])
    data_var%units = "volts"
    data_var%long_name = "Measured signal"
    
    ds = dataset()
    call add_variable(ds, data_var)
    
    ! Write to NetCDF
    stat = write_netcdf("output.nc", ds)
    if (stat == 0) then
        write(*,'(A)') "Successfully wrote output.nc"
    end if
    
    ! Read back
    loaded_ds = read_netcdf("output.nc", stat=stat)
    if (stat == 0) then
        write(*,'(A,I0)') "Loaded dataset with ", loaded_ds%n_vars, " variables"
    end if
    
    ! Clean up
    call finalize_variable(data_var)
    call finalize_dataset(ds)
    call finalize_dataset(loaded_ds)
end program netcdf_io
```

### Basic Operations

```fortran
program operations_example
    use foxel
    implicit none
    
    type(variable_t) :: data_var, result_var, mean_result
    real(real64), dimension(100) :: data
    real(real64) :: mean_value
    integer :: i
    
    ! Generate data
    do i = 1, 100
        data(i) = real(i) * 0.1 + sin(real(i) * 0.1)
    end do
    
    data_var = variable(data, name="measurements", dim_names=["index"])
    
    ! Arithmetic operations
    result_var = data_var * 2.0_real64 + 1.0_real64
    write(*,'(A)') "Applied transformation: y = 2x + 1"
    
    ! Aggregation
    mean_result = mean(data_var)
    mean_value = mean_result%data%values_r64(1)
    write(*,'(A,F0.3)') "Mean value: ", mean_value
    
    ! Slicing
    call finalize_variable(result_var)
    result_var = slice_range(data_var, 10, 20)
    write(*,'(A,I0)') "Slice [10:20] has ", result_var%n_elements, " elements"
    
    ! Clean up
    call finalize_variable(data_var)
    call finalize_variable(result_var)
    call finalize_variable(mean_result)
end program operations_example
```

### Time Series Analysis

```fortran
program time_series_example
    use foxel
    implicit none
    
    type(variable_t) :: ts_var, monthly_var, rolling_var
    real(real64), dimension(365) :: daily_data
    integer :: i
    
    ! Generate daily time series
    do i = 1, 365
        daily_data(i) = 100.0 + 50.0 * sin(2.0 * 3.14159 * real(i) / 365.0) + &
                       20.0 * sin(2.0 * 3.14159 * real(i) / 30.0)  ! Monthly variation
    end do
    
    ts_var = variable(daily_data, name="daily_values", dim_names=["time"])
    
    ! Monthly resampling
    monthly_var = resample(ts_var, "M", "mean")
    write(*,'(A,I0)') "Monthly resampled data has ", monthly_var%n_elements, " points"
    
    ! Rolling mean
    rolling_var = rolling_mean(ts_var, 30)  ! 30-day rolling mean
    write(*,'(A,I0)') "30-day rolling mean has ", rolling_var%n_elements, " points"
    
    ! Clean up
    call finalize_variable(ts_var)
    call finalize_variable(monthly_var)
    call finalize_variable(rolling_var)
end program time_series_example
```

### Parallel Computing

```fortran
program parallel_example
    use foxel
    implicit none
    
    type(variable_t) :: large_var, result_var
    real(real64), dimension(1000000) :: large_data
    integer :: i
    
    ! Generate large dataset
    do i = 1, 1000000
        large_data(i) = sin(real(i) * 0.000001)
    end do
    
    large_var = variable(large_data, name="large_dataset", dim_names=["index"])
    
    ! Set number of threads
    call set_num_threads(8)
    
    ! Parallel operations automatically use available threads
    result_var = mean(large_var)
    write(*,'(A)') "Computed mean using parallel processing"
    
    ! Clean up
    call finalize_variable(large_var)
    call finalize_variable(result_var)
end program parallel_example
```

## Key Features

### Data Types
- Support for real32, real64, int32, int64
- 0D scalars through nD arrays
- Automatic type conversion and promotion

### NetCDF Integration
- Full NetCDF4/HDF5 compatibility
- CF conventions compliance
- Automatic metadata preservation
- Compression and chunking support

### Operations
- **Arithmetic**: +, -, *, /, ** with broadcasting
- **Aggregation**: mean, sum, min, max, std, var
- **Indexing**: Positional and label-based
- **Slicing**: Range selection with steps
- **Time Series**: Resampling, rolling windows, seasonal analysis

### Performance
- OpenMP parallel computing
- SIMD optimizations
- Cache-efficient algorithms
- Memory layout optimization
- Chunked operations for large datasets

### Visualization
- Integration with fortplot
- Automatic axis labeling from coordinates
- Multiple plot types (line, contour, surface)
- Publication-ready output

## Common Patterns

### Error Handling
```fortran
integer :: stat
type(variable_t) :: var

var = read_netcdf_variable("data.nc", "temperature", stat=stat)
if (stat /= 0) then
    write(*,'(A)') "Error reading NetCDF file"
    stop 1
end if
```

### Memory Management
```fortran
! Always finalize variables and datasets
call finalize_variable(var)
call finalize_dataset(ds)
call finalize_coordinate(coord)
```

### Performance Optimization
```fortran
! Use chunking for large operations
call set_chunk_size(10000)
result = mean_chunked(large_var)

! Enable optimization
call enable_optimization()
```

## Next Steps

- Read the [API Reference](api_reference.md) for detailed function documentation
- Explore the [Example Gallery](examples/) for more complex use cases
- Check [Migration Guides](migration_guides.md) if coming from other tools
- See [Performance Guide](performance_guide.md) for optimization tips

## Getting Help

- Check the documentation in the `docs/` directory
- Look at test files in `test/` for usage examples
- Report issues on GitHub: https://github.com/krystophny/foxel/issues