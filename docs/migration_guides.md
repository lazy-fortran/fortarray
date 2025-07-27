# Migration Guides

This document helps users migrate to Foxel from other scientific data analysis tools.

## Migrating from CDO (Climate Data Operators)

CDO is a popular command-line tool for climate data processing. Here's how to translate common CDO operations to Foxel:

### Basic Operations

| CDO Command | Foxel Equivalent |
|-------------|------------------|
| `cdo info file.nc` | `ds = read_netcdf("file.nc"); print dataset info` |
| `cdo select,name=temp file.nc` | `var = get_variable(ds, "temp")` |
| `cdo sellevel,850 file.nc` | `var = select_by_value(var, "level", 850.0)` |
| `cdo seldate,2020-01-01,2020-12-31` | `var = select_by_range(var, "time", start, end)` |

### Statistical Operations

| CDO Command | Foxel Equivalent |
|-------------|------------------|
| `cdo timmean file.nc` | `result = mean(var, dim=time_dim)` |
| `cdo timsum file.nc` | `result = sum(var, dim=time_dim)` |
| `cdo timstd file.nc` | `result = std(var, dim=time_dim)` |
| `cdo fldmean file.nc` | `result = mean(var, dim=[lat_dim, lon_dim])` |

### Example Migration

**CDO workflow:**
```bash
cdo timmean input.nc temp_mean.nc
cdo sub input.nc temp_mean.nc anomaly.nc
cdo timstd anomaly.nc std.nc
```

**Foxel equivalent:**
```fortran
program cdo_to_foxel
    use foxel
    implicit none
    
    type(dataset_t) :: ds
    type(variable_t) :: temp_var, temp_mean, anomaly, std_result
    integer :: stat
    
    ! Read input data
    ds = read_netcdf("input.nc", stat=stat)
    temp_var = get_variable(ds, "temperature")
    
    ! Time mean
    temp_mean = mean(temp_var)  ! Default: mean over all dimensions
    
    ! Compute anomalies
    anomaly = temp_var - temp_mean
    
    ! Time standard deviation
    std_result = std(anomaly)
    
    ! Save results
    stat = write_netcdf_variable("temp_mean.nc", temp_mean)
    stat = write_netcdf_variable("anomaly.nc", anomaly)
    stat = write_netcdf_variable("std.nc", std_result)
    
    ! Cleanup
    call finalize_variable(temp_var)
    call finalize_variable(temp_mean)
    call finalize_variable(anomaly)
    call finalize_variable(std_result)
    call finalize_dataset(ds)
end program cdo_to_foxel
```

## Migrating from NCL (NCAR Command Language)

NCL is widely used for atmospheric sciences. Here are common patterns:

### Data Reading and Selection

**NCL:**
```ncl
f = addfile("data.nc", "r")
temp = f->temperature
temp_subset = temp({time|start_time:end_time}, {lat|start_lat:end_lat})
```

**Foxel:**
```fortran
ds = read_netcdf("data.nc", stat=stat)
temp = get_variable(ds, "temperature")
temp_subset = select_by_range(temp, "time", start_time, end_time)
temp_subset = select_by_range(temp_subset, "lat", start_lat, end_lat)
```

### Statistics and Analysis

**NCL:**
```ncl
temp_avg = dim_avg_n_Wrap(temp, 0)  ; average over time dimension
temp_anom = temp - conform(temp, temp_avg, (/1,2/))
```

**Foxel:**
```fortran
temp_avg = mean(temp, dim=1)  ! Average over first dimension (time)
temp_anom = temp - temp_avg   ! Broadcasting handled automatically
```

### Plotting

**NCL:**
```ncl
res = True
res@cnFillOn = True
plot = gsn_csm_contour_map(wks, temp(0,:,:), res)
```

**Foxel:**
```fortran
call contour_plot(temp_slice)  ! temp_slice is 2D slice
```

## Migrating from xarray (Python)

xarray is a popular Python library for labeled arrays. Here's how to translate common operations:

### Basic Operations

| xarray (Python) | Foxel (Fortran) |
|-----------------|-----------------|
| `ds = xr.open_dataset("file.nc")` | `ds = read_netcdf("file.nc", stat=stat)` |
| `temp = ds['temperature']` | `temp = get_variable(ds, "temperature")` |
| `temp.sel(time=slice('2020', '2021'))` | `temp = select_by_range(temp, "time", start, end)` |
| `temp.mean(dim='time')` | `result = mean(temp, dim=time_dim)` |

### Advanced Operations

**xarray:**
```python
import xarray as xr
import numpy as np

# Load data
ds = xr.open_dataset('climate.nc')
temp = ds['temperature']

# Compute seasonal means
seasonal = temp.groupby('time.season').mean('time')

# Compute anomalies
climatology = temp.groupby('time.month').mean('time')
anomalies = temp.groupby('time.month') - climatology

# Weighted mean
weights = np.cos(np.deg2rad(temp.lat))
global_mean = temp.weighted(weights).mean(['lat', 'lon'])
```

**Foxel:**
```fortran
program xarray_to_foxel
    use foxel
    implicit none
    
    type(dataset_t) :: ds
    type(variable_t) :: temp, seasonal, climatology, anomalies, global_mean
    type(variable_t) :: weights
    integer :: stat
    
    ! Load data
    ds = read_netcdf("climate.nc", stat=stat)
    temp = get_variable(ds, "temperature")
    
    ! Compute seasonal means
    seasonal = seasonal_mean(temp)
    
    ! Compute anomalies (simplified - would need month grouping)
    climatology = mean(temp)  ! Overall mean
    anomalies = temp - climatology
    
    ! Weighted mean (would need to create weights)
    ! weights = cos(lat_in_radians)
    ! global_mean = weighted_mean(temp, weights, dim=[lat_dim, lon_dim])
    global_mean = mean(temp)  ! Simplified
    
    ! Cleanup
    call finalize_variable(temp)
    call finalize_variable(seasonal)
    call finalize_variable(climatology)
    call finalize_variable(anomalies)
    call finalize_variable(global_mean)
    call finalize_dataset(ds)
end program xarray_to_foxel
```

## Migrating from MATLAB

MATLAB is commonly used for data analysis. Here are equivalent operations:

### Array Operations

| MATLAB | Foxel |
|--------|-------|
| `data = ncread('file.nc', 'temp')` | `var = read_netcdf_variable("file.nc", "temp", stat=stat)` |
| `mean_data = mean(data, 1)` | `result = mean(var, dim=1)` |
| `data_subset = data(1:10, :)` | `result = slice_range(var, 1, 10)` |
| `filtered = data(data > threshold)` | `result = filter_by_condition(var, var > threshold)` |

### Statistical Analysis

**MATLAB:**
```matlab
% Load and analyze data
data = ncread('climate.nc', 'temperature');
time = ncread('climate.nc', 'time');

% Compute statistics
mean_temp = mean(data, 1);
std_temp = std(data, 0, 1);
trend = polyfit(time, mean_temp, 1);

% Find extremes
[max_val, max_idx] = max(data);
[min_val, min_idx] = min(data);
```

**Foxel:**
```fortran
program matlab_to_foxel
    use foxel
    implicit none
    
    type(variable_t) :: data_var, time_var, mean_temp, std_temp, max_val, min_val
    integer :: stat
    
    ! Load data
    data_var = read_netcdf_variable("climate.nc", "temperature", stat=stat)
    time_var = read_netcdf_variable("climate.nc", "time", stat=stat)
    
    ! Compute statistics
    mean_temp = mean(data_var, dim=1)
    std_temp = std(data_var, dim=1)
    
    ! Find extremes
    max_val = maxval(data_var)
    min_val = minval(data_var)
    
    ! Cleanup
    call finalize_variable(data_var)
    call finalize_variable(time_var)
    call finalize_variable(mean_temp)
    call finalize_variable(std_temp)
    call finalize_variable(max_val)
    call finalize_variable(min_val)
end program matlab_to_foxel
```

## Key Differences and Considerations

### Memory Management
- **Other tools**: Automatic garbage collection
- **Foxel**: Explicit memory management with `finalize_*` procedures

```fortran
! Always clean up
call finalize_variable(var)
call finalize_dataset(ds)
call finalize_coordinate(coord)
```

### Error Handling
- **Other tools**: Exception-based error handling
- **Foxel**: Status codes and optional error messages

```fortran
integer :: stat
character(len=256) :: error_msg

var = read_netcdf_variable("file.nc", "temp", stat=stat)
if (stat /= 0) then
    write(*,'(A)') "Error reading variable"
    stop 1
end if
```

### Data Types
- **Other tools**: Dynamic typing or type inference
- **Foxel**: Explicit Fortran typing

```fortran
! Specify precision explicitly
real(real64), dimension(100) :: data_array
type(variable_t) :: var

var = variable(data_array, name="temperature")
```

### Broadcasting
- **Other tools**: Automatic broadcasting
- **Foxel**: Manual broadcasting or use built-in operations

```fortran
! Automatic broadcasting in arithmetic
result = var1 + var2  ! Works if dimensions are compatible
result = var + scalar ! Always works
```

## Performance Considerations

### Parallel Processing
**Foxel advantage**: Built-in OpenMP parallelization
```fortran
call set_num_threads(8)
result = mean(large_var)  ! Automatically parallelized
```

### Memory Efficiency
**Foxel advantage**: Control over memory layout and chunking
```fortran
call set_chunk_size(10000)
result = mean_chunked(large_var)  ! Process in chunks
```

### Optimization
**Foxel advantage**: Compiler optimizations and SIMD
```fortran
call enable_optimization()
result = vectorized_add(var1, var2)  ! SIMD-optimized
```

## Common Patterns

### Reading Multiple Files
```fortran
subroutine process_multiple_files()
    character(len=*), parameter :: files(*) = ["file1.nc", "file2.nc", "file3.nc"]
    type(variable_t) :: var, combined
    integer :: i, stat
    
    do i = 1, size(files)
        var = read_netcdf_variable(files(i), "temperature", stat=stat)
        if (stat == 0) then
            ! Process variable
            ! Concatenate or analyze
        end if
        call finalize_variable(var)
    end do
end subroutine
```

### Time Series Processing
```fortran
subroutine analyze_time_series(var)
    type(variable_t), intent(in) :: var
    type(variable_t) :: monthly, anomaly, trend
    
    ! Monthly means
    monthly = resample(var, "M", "mean")
    
    ! Compute anomalies
    anomaly = var - mean(var)
    
    ! Rolling mean for trend
    trend = rolling_mean(var, 365)  ! Annual rolling mean
    
    call finalize_variable(monthly)
    call finalize_variable(anomaly)
    call finalize_variable(trend)
end subroutine
```

### Error-Safe File Operations
```fortran
function safe_read_variable(filename, varname) result(var)
    character(len=*), intent(in) :: filename, varname
    type(variable_t) :: var
    integer :: stat
    
    var = read_netcdf_variable(filename, varname, stat=stat)
    if (stat /= 0) then
        write(*,'(A,A,A,A)') "Warning: Could not read ", varname, " from ", filename
        ! Create empty variable or handle error appropriately
    end if
end function
```

## Migration Checklist

### Before Migration
- [ ] Identify core operations used in your workflow
- [ ] Check if equivalent Foxel functions exist
- [ ] Plan memory management strategy
- [ ] Consider performance requirements

### During Migration
- [ ] Start with simple operations
- [ ] Add error handling early
- [ ] Test with small datasets first
- [ ] Verify numerical accuracy

### After Migration
- [ ] Profile performance and optimize
- [ ] Add parallel processing where beneficial
- [ ] Document any workflow-specific patterns
- [ ] Create reusable subroutines for common operations

## Getting Help

If you need assistance migrating from a specific tool:

1. Check the example gallery for similar use cases
2. Consult the API reference for function details
3. Look at test files for usage patterns
4. Report issues or ask questions on GitHub