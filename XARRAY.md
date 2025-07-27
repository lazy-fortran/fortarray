# FortArray: Fortran xarray Compatibility Plan

This document outlines a comprehensive plan to rebrand and redesign the library as **FortArray** with a more xarray-compatible API, enabling xarray users to feel at home immediately while maintaining Fortran's performance advantages.

## Executive Summary

The goal is to create a "Fortran xarray" that provides 90%+ API compatibility with xarray while leveraging Fortran's performance. This involves:

1. **API Harmonization**: Make method names, signatures, and behaviors match xarray
2. **Syntax Compatibility**: Support xarray-like selection and indexing syntax
3. **Method Chaining**: Enable fluent interface patterns common in xarray
4. **Namespace Organization**: Mirror xarray's module structure and imports

## Current State Analysis

### Similarities (Already Good)
- ✅ Multi-dimensional labeled arrays (`variable_t` → `fortarray_t` ≈ `DataArray`)
- ✅ Dataset collections (`dataset_t` ≈ `Dataset`)
- ✅ NetCDF I/O support
- ✅ Coordinate-based indexing
- ✅ Broadcasting and aggregation

### Major Differences (Need Changes)
- ❌ Method naming conventions
- ❌ Selection syntax (`.sel()`, `.isel()`, `.where()`)
- ❌ Method chaining
- ❌ Groupby operations
- ❌ Time/datetime handling
- ❌ Plotting integration

## Phase 1: Core API Harmonization

### 1.1 Rename Core Types and Library

**Library Rebranding:**
- **Foxel** → **FortArray**
- **variable_t** → **fortarray_t** 
- Module names: `foxel_*` → `fortarray_*`

**Current → Target:**
```fortran
! Current (Foxel)
use foxel
type(variable_t) :: var
type(dataset_t) :: ds

! Target (FortArray)
use fortarray
type(fortarray_t) :: arr
type(dataset_t) :: ds  ! Keep same name

! Enhanced central type
type :: fortarray_t
    ! Same content as variable_t but with enhanced methods
    character(len=:), allocatable :: name
    character(len=:), allocatable :: units
    type(storage_t) :: data
    integer :: n_dims
    character(len=64), dimension(:), allocatable :: dims
    type(coordinate_t), dimension(:), allocatable :: coords
contains
    ! xarray-compatible methods
    procedure :: sel, isel, filter, mean, sum, std, plot
end type fortarray_t

! Provide aliases for backward compatibility
! Note: Cannot alias types in Fortran, need to use inheritance or generic interfaces
type, extends(fortarray_t) :: variable_t  ! Backward compatibility via inheritance
end type variable_t
```

### 1.2 Constructor Harmonization

**Target API:**
```fortran
! xarray: xr.DataArray(data, coords, dims, name, attrs)
! FortArray: fa.array(data, coords, dims, name, attrs)
! Note: Fortran doesn't support keyword arguments like Python
arr = new_array(data, dim_names, coords, "temperature")

! xarray: xr.Dataset(data_vars, coords, attrs)  
! FortArray: fa.dataset(data_vars, coords, attrs)
ds = new_dataset(arrays, coordinates, global_attrs)
```

**Implementation:**
```fortran
interface new_array
    module procedure :: fortarray_from_array
    module procedure :: fortarray_from_scalar
    module procedure :: fortarray_with_coords
end interface new_array

interface new_dataset
    module procedure :: dataset_from_fortrarrays
    module procedure :: dataset_empty
    module procedure :: dataset_with_metadata
end interface new_dataset
```

### 1.3 Method Name Mapping

Create xarray-compatible method names:

```fortran
! Current → xarray-style methods
type :: fortarray_t
contains
    ! Selection methods
    procedure :: sel => select_by_coordinate
    procedure :: isel => select_by_index  
    procedure :: filter => select_by_condition  ! avoid 'where' keyword
    procedure :: drop_sel => drop_by_coordinate
    
    ! Aggregation methods  
    procedure :: mean => compute_mean
    procedure :: sum => compute_sum
    procedure :: std => compute_std
    procedure :: var => compute_variance
    procedure :: min => compute_min
    procedure :: max => compute_max
    procedure :: median => compute_median
    procedure :: quantile => compute_quantile
    
    ! Dimension operations
    procedure :: transpose => transpose_dims
    procedure :: stack => stack_dimensions
    procedure :: unstack => unstack_dimensions
    procedure :: squeeze => squeeze_dims
    procedure :: expand_dims => expand_dimensions
    
    ! Data manipulation
    procedure :: fillna => fill_missing
    procedure :: dropna => drop_missing
    procedure :: interpolate_na => interpolate_missing
    procedure :: ffill => forward_fill
    procedure :: bfill => backward_fill
    procedure :: values => extract_values  ! avoid 'data' keyword
    
    ! Resampling and groupby
    procedure :: resample => resample_time
    procedure :: groupby => group_by_coordinate
    procedure :: rolling => rolling_window
    
    ! I/O methods
    procedure :: to_netcdf => write_to_netcdf
    procedure :: to_pandas => convert_to_pandas_style
    procedure :: to_numpy => extract_numpy_values
    procedure :: store => save_to_file  ! avoid 'save' keyword
    
    ! Plotting (via fortplot integration)
    procedure :: plot => create_plot
    procedure :: plot_line => plot_line_chart
    procedure :: plot_contour => plot_contour_map
    procedure :: plot_surface => plot_surface_3d
end type fortarray_t
```

## Phase 2: Selection Syntax Enhancement

### 2.1 Coordinate Selection (.sel method)

**Target xarray syntax:**
```python
# xarray
temp.sel(time='2020-01-01')
temp.sel(time=slice('2020', '2021'))
temp.sel(lat=45.0, method='nearest')
temp.sel(lat=slice(40, 50), lon=slice(-10, 10))
```

**FortArray implementation:**
```fortran
! Single coordinate selection
result = temp%sel(time='2020-01-01')
result = temp%sel(lat=45.0_real64, method='nearest')

! Range selection (need slice_t type)
type :: slice_t
    ! Note: class(*) not ideal - use specific types for performance
    real(real64) :: start_val, stop_val, step_val
    character(len=32) :: slice_type = 'range'
    logical :: has_start = .false., has_stop = .false., has_step = .false.
end type slice_t

! Usage (simplified - Fortran doesn't support Python-like slice syntax)
result = temp%sel_range('time', '2020', '2021')
result = temp%sel_range('lat', 40.0_real64, 50.0_real64)
result = temp%sel_point('lat', 45.0_real64)

! Implementation
! Simplified interface - avoid class(*) polymorphism for performance
function sel_point(this, coord_name, coord_value, method) result(result_da)
    class(fortarray_t), intent(in) :: this
    character(len=*), intent(in) :: coord_name
    real(real64), intent(in) :: coord_value
    character(len=*), intent(in), optional :: method
    type(fortarray_t) :: result_da
    
    ! Point selection implementation
end function

function sel_range(this, coord_name, start_val, stop_val, step_val) result(result_da)
    class(fortarray_t), intent(in) :: this
    character(len=*), intent(in) :: coord_name
    real(real64), intent(in) :: start_val, stop_val
    real(real64), intent(in), optional :: step_val
    type(fortarray_t) :: result_da
    
    ! Range selection implementation
end function
```

### 2.2 Index Selection (.isel method)

**Target xarray syntax:**
```python
# xarray
temp.isel(time=0)
temp.isel(time=slice(0, 10))
temp.isel(time=[0, 5, 10])
```

**FortArray implementation:**
```fortran
! Index selection (simplified - avoid complex polymorphism)
result = temp%isel_point('time', 1)  ! Fortran 1-based indexing
result = temp%isel_range('time', 1, 10)
result = temp%isel_indices('time', [1, 5, 10])

function isel_point(this, dim_name, index) result(result_da)
    class(fortarray_t), intent(in) :: this
    character(len=*), intent(in) :: dim_name
    integer, intent(in) :: index
    type(fortarray_t) :: result_da
    ! Implementation
end function

function isel_range(this, dim_name, start_idx, stop_idx, step_idx) result(result_da)
    class(fortarray_t), intent(in) :: this
    character(len=*), intent(in) :: dim_name
    integer, intent(in) :: start_idx, stop_idx
    integer, intent(in), optional :: step_idx
    type(fortarray_t) :: result_da
    ! Implementation
end function
```

### 2.3 Conditional Selection (.filter method)

**Target xarray syntax:**
```python
# xarray
temp.where(temp > 273.15)
temp.where(temp > 273.15, other=0)
```

**FortArray implementation:**
```fortran
! Conditional selection (using 'filter' to avoid 'where' keyword)
result = temp%filter(temp > 273.15_real64)
result = temp%filter(temp > 273.15_real64, other=0.0_real64)

function select_by_condition(this, condition, other_val) result(result_da)
    class(fortarray_t), intent(in) :: this
    type(fortarray_t), intent(in) :: condition  ! Boolean array
    class(*), intent(in), optional :: other_val  ! avoid 'other' potential conflict
    type(fortarray_t) :: result_da
    
    ! Apply condition mask
end function
```

## Phase 3: Method Chaining Support

### 3.1 Fluent Interface Design

Enable xarray-style method chaining:

```fortran
! Target: method chaining like xarray
result = temp%sel(time=slice('2020', '2021'))%mean(dim='time')%filter(result > 273.15)

! Implementation approach: all methods return fortarray_t
type :: fortarray_t
contains
    procedure :: sel => select_coordinate  ! Returns fortarray_t
    procedure :: mean => compute_mean     ! Returns fortarray_t  
    procedure :: filter => filter_condition ! Returns fortarray_t
end type
```

### 3.2 Lazy Evaluation (Optional)

For advanced users, support lazy evaluation:

```fortran
type :: lazy_fortarray_t
    type(computation_graph_t) :: graph
    logical :: is_computed = .false.
    type(fortarray_t) :: cached_result
contains
    procedure :: compute => force_computation
    procedure :: sel => lazy_select
    procedure :: mean => lazy_mean
end type

! Usage
lazy_result = temp%lazy()%sel(time='2020')%mean(dim='time')
actual_result = lazy_result%compute()  ! Execute computation graph
```

## Phase 4: Groupby Operations

### 4.1 Groupby Interface

**Target xarray syntax:**
```python
# xarray
temp.groupby('time.season').mean()
temp.groupby_bins('temperature', bins=10).count()
```

**FortArray implementation:**
```fortran
type :: groupby_t
    type(fortarray_t) :: source_data
    character(len=:), allocatable :: group_coord
    character(len=:), allocatable :: group_type  ! 'coordinate', 'bins', 'function'
contains
    procedure :: mean => groupby_mean
    procedure :: sum => groupby_sum
    procedure :: count => groupby_count
    procedure :: std => groupby_std
    procedure :: apply => groupby_apply
end type groupby_t

! Usage
result = temp%groupby('time.season')%mean()
result = temp%groupby_bins('temperature', bins=10)%count()

function group_by_coordinate(this, coord_name, group_type) result(groupby_obj)
    class(fortarray_t), intent(in) :: this
    character(len=*), intent(in) :: coord_name
    character(len=*), intent(in), optional :: group_type
    type(groupby_t) :: groupby_obj
    
    groupby_obj%source_data = this
    groupby_obj%group_coord = coord_name
    if (present(group_type)) then
        groupby_obj%group_type = group_type
    else
        groupby_obj%group_type = 'coordinate'
    end if
end function
```

### 4.2 Time-based Groupby

Support time-based grouping operations:

```fortran
! Time groupby extensions
result = temp%groupby('time.month')%mean()
result = temp%groupby('time.season')%std()
result = temp%groupby('time.year')%sum()

! Implementation in time module
function extract_time_component(coord, component) result(time_groups)
    type(coordinate_t), intent(in) :: coord
    character(len=*), intent(in) :: component  ! 'month', 'season', 'year', etc.
    integer, allocatable :: time_groups(:)
    
    ! Extract time component from coordinate
end function
```

## Phase 5: Enhanced I/O and Interoperability

### 5.1 I/O Method Harmonization

**Target xarray syntax:**
```python
# xarray
ds = xr.open_dataset('file.nc')
ds.to_netcdf('output.nc')
da = xr.open_dataarray('file.nc')
```

**FortArray implementation:**
```fortran
! Module-level functions (like xarray)
ds = open_dataset('file.nc')
da = open_dataarray('file.nc', var_name='temperature')

! Method-based I/O
call ds%to_netcdf('output.nc')
call da%to_netcdf('output.nc', mode='w')

! Enhanced options
ds = open_dataset('file.nc', chunks={'time': 100, 'lat': 50})
ds = open_mfdataset(['file1.nc', 'file2.nc'], concat_dim='time')
```

### 5.2 Pandas Integration

Provide conversion utilities:

```fortran
! Convert to pandas-like format (for interoperability)
type :: pandas_like_t
    character(len=:), allocatable :: data(:,:)  ! String representation
    character(len=:), allocatable :: columns(:)
    character(len=:), allocatable :: index(:)
end type

function to_pandas(this) result(pandas_data)
    class(fortarray_t), intent(in) :: this
    type(pandas_like_t) :: pandas_data
    
    ! Convert to pandas-compatible format
end function
```

## Phase 6: Plotting Integration

### 6.1 xarray-style Plotting

**Target xarray syntax:**
```python
# xarray
temp.plot()
temp.plot.line()
temp.plot.contour()
temp.plot.contourf()
```

**FortArray implementation:**
```fortran
type :: plot_accessor_t
    type(dataarray_t), pointer :: data => null()
contains
    procedure :: line => plot_line
    procedure :: contour => plot_contour
    procedure :: contourf => plot_contourf
    procedure :: surface => plot_surface
    procedure :: histogram => plot_histogram
end type plot_accessor_t

type, extends(dataarray_t) :: dataarray_t
    type(plot_accessor_t) :: plot
contains
    procedure :: init_plot_accessor
end type

! Usage
call temp%plot%line()
call temp%plot%contour(levels=20)
call temp%plot%surface()
```

### 6.2 Automatic Plot Configuration

```fortran
subroutine plot_line(this, x, y, hue, col, row, fig_size, title)
    class(plot_accessor_t), intent(in) :: this
    character(len=*), intent(in), optional :: x, y, hue, col, row
    real, intent(in), optional :: fig_size(2)
    character(len=*), intent(in), optional :: title
    
    ! Auto-configure plot based on data dimensions
    if (.not. present(x)) then
        ! Use first coordinate as x-axis
    end if
    
    ! Create plot using fortplot backend
end subroutine
```

## Phase 7: Fortran-Specific Constraints and Solutions

### 7.1 Fortran Language Limitations and Workarounds

**Key Limitations:**

1. **No Keyword Arguments**
   ```fortran
   ! Python/xarray style (NOT possible in Fortran)
   arr = DataArray(data, dims=['x', 'y'], name='temp')
   
   ! Fortran solution: positional arguments with overloading
   arr = new_array(data, ['x', 'y'], 'temp')
   ```

2. **No Duck Typing / class(*) Performance Issues**
   ```fortran
   ! Avoid: class(*) for performance
   ! Instead: specific method overloads
   procedure :: sel_point   ! for single values
   procedure :: sel_range   ! for ranges  
   procedure :: sel_indices ! for index arrays
   ```

3. **No Type Aliases**
   ```fortran
   ! NOT possible: type aliases
   ! integer, parameter :: variable_t = fortarray_t
   
   ! Solution: inheritance for backward compatibility
   type, extends(fortarray_t) :: variable_t
   end type variable_t
   ```

4. **Case Insensitivity**
   ```fortran
   ! Must avoid conflicts between module and constructor names
   module fortarray        ! module name
   interface new_array     ! constructor (not 'fortarray')
   ```

5. **Keyword Conflicts**
   ```fortran
   ! Avoid Fortran keywords:
   procedure :: filter     ! NOT 'where'
   procedure :: values     ! NOT 'data'  
   procedure :: store      ! NOT 'save'
   ```

**Performance Optimizations:**

1. **Avoid Unlimited Polymorphism**
   ```fortran
   ! Slower: class(*) polymorphism
   function sel(this, coord_value) 
       class(*), intent(in) :: coord_value
   
   ! Faster: specific overloads
   function sel_point(this, coord_value)
       real(real64), intent(in) :: coord_value
   
   function sel_range(this, start_val, stop_val)
       real(real64), intent(in) :: start_val, stop_val
   ```

2. **Static Memory Layout**
   ```fortran
   ! Use specific types instead of class(*) for better optimization
   type :: slice_t
       real(real64) :: start_val, stop_val, step_val
       logical :: has_start, has_stop, has_step
   end type slice_t
   ```

## Phase 8: Advanced Features

### 8.1 Coordinate System Enhancements

```fortran
! Enhanced coordinate handling
type, extends(coordinate_t) :: enhanced_coordinate_t
contains
    procedure :: to_datetime => convert_to_datetime
    procedure :: to_timedelta => convert_to_timedelta
    procedure :: to_period => convert_to_period
end type

! xarray-like coordinate operations
coords = da%coords
time_coord = coords('time')
time_values = time_coord%to_datetime()
```

### 7.2 Arithmetic and Math Functions

```fortran
! Enhanced arithmetic (more xarray-like)
result = da1 + da2  ! Already supported
result = da1.add(da2, fill_value=0)  ! xarray-style with options
result = da1.subtract(da2, fill_value=0)
result = da1.multiply(da2)
result = da1.divide(da2)

! Math functions
result = da%abs()
result = da%sqrt()
result = da%sin()
result = da%cos()
result = da%log()
result = da%exp()
```

## Implementation Timeline

### Phase 1: Core API (2-3 months)
1. Create type aliases and basic method renaming
2. Implement sel/isel/where methods
3. Add basic method chaining support

### Phase 2: Selection & Aggregation (2 months)
1. Enhanced selection syntax with slice support
2. Complete aggregation method set
3. Dimension manipulation methods

### Phase 3: Groupby & Resampling (3 months)
1. Basic groupby operations
2. Time-based groupby
3. Resampling methods

### Phase 4: I/O & Plotting (2 months)
1. Enhanced I/O methods
2. Plot accessor implementation
3. Integration testing

### Phase 5: Advanced Features (3 months)
1. Lazy evaluation (optional)
2. Enhanced coordinate handling
3. Mathematical function library

## Migration Strategy

### 6.1 Backward Compatibility

Maintain existing Foxel API while adding xarray compatibility:

```fortran
! Old style still works
var = variable(data, name="temp")
result = mean(var)

! New xarray style also works
da = DataArray(data, name="temp")
result = da%mean()
```

### 6.2 Documentation and Examples

Create comprehensive examples showing xarray → Foxel translation:

```fortran
! docs/xarray_compatibility.md
! Side-by-side examples for all major operations

! docs/examples/xarray_users/
! Complete examples for common xarray workflows
```

## Success Metrics

1. **API Coverage**: 90%+ of common xarray operations supported
2. **Syntax Similarity**: Method names and signatures match xarray
3. **User Experience**: xarray users can adapt within 1 day
4. **Performance**: Maintain Fortran performance advantages
5. **Documentation**: Complete migration guide for xarray users

## Why FortArray is Better Than "Foxel"

### **Brand Clarity**
- **FortArray** immediately communicates "Fortran arrays" 
- Avoids confusion with other scientific libraries
- Clear positioning as "Fortran's xarray"

### **API Elegance**
- `fortarray_t` is clean and unambiguous
- No awkward double-a like `dataarray_t`
- Natural abbreviation: `arr` vs awkward `da`

### **Usage Examples**
```fortran
! Clean and intuitive
use fortarray
type(fortarray_t) :: temperature, pressure
type(dataset_t) :: climate_data

! Natural method calls
result = temperature%sel(time='2020')%mean(dim='time')
call temperature%plot%contour()

! Clear constructors (Fortran syntax)
temp = new_array(temp_data, ['lat', 'lon'], 'temperature')
```

### **Marketing Position**
- **"FortArray: High-Performance xarray for Fortran"**
- **"The missing link between xarray and HPC"**
- **"xarray syntax, Fortran speed"**

## Conclusion

This plan transforms the library into **FortArray** - a "Fortran xarray" that provides familiar syntax and methods while maintaining the performance benefits of compiled Fortran code. The phased approach ensures gradual adoption while maintaining backward compatibility.

**FortArray** will be positioned as:
> *The high-performance scientific array library that brings xarray's intuitive API to Fortran, enabling seamless migration from Python to compiled performance without sacrificing developer experience.*

The result will be a library that allows xarray users to transition smoothly to high-performance Fortran-based scientific computing without losing the familiar API they're accustomed to, while the **FortArray** name clearly communicates its purpose and target audience.

## Fortran Validity Assessment

### ✅ **Fully Achievable (100% Valid Fortran)**

1. **Method chaining**: `result = temp%sel_point('time', 1)%mean()%filter(condition)`
2. **Type-bound procedures**: All `.sel()`, `.mean()`, `.plot()` methods 
3. **Generic interfaces**: Overloaded constructors `new_array()`, `new_dataset()`
4. **Inheritance**: Backward compatibility via `type, extends(fortarray_t) :: variable_t`
5. **Aggregation methods**: `.mean()`, `.sum()`, `.std()`, etc.
6. **I/O methods**: `.to_netcdf()`, `.store()`, etc.
7. **Plotting accessors**: `temp%plot%contour()` via contained plot object

### 🟡 **Achievable with Syntax Compromises**

1. **Selection syntax**: 
   - xarray: `temp.sel(time='2020', method='nearest')`
   - FortArray: `temp%sel_point('time', '2020', 'nearest')`

2. **Constructor calls**:
   - xarray: `DataArray(data, dims=['x', 'y'], name='temp')`  
   - FortArray: `new_array(data, ['x', 'y'], 'temp')`

3. **Slicing**:
   - xarray: `temp.sel(time=slice('2020', '2021'))`
   - FortArray: `temp%sel_range('time', '2020', '2021')`

### ❌ **Not Possible in Fortran**

1. **Python keyword arguments**: `sel(time='2020', method='nearest')`
2. **Duck typing with `class(*)`**: Performance penalty too high
3. **Python slice objects**: `slice(start, stop, step)`
4. **Type aliases**: `variable_t = fortarray_t` (use inheritance instead)
5. **Module/constructor name conflicts**: Need `new_array` not `fortarray`

### **Final API Comparison**

| Feature | xarray (Python) | FortArray (Fortran) | Status |
|---------|----------------|---------------------|---------|
| **Basic selection** | `temp.sel(time='2020')` | `temp%sel_point('time', '2020')` | ✅ Very close |
| **Method chaining** | `temp.sel().mean().plot()` | `temp%sel_point()%mean()%plot()` | ✅ Identical |
| **Aggregation** | `temp.mean(dim='time')` | `temp%mean(dim='time')` | ✅ Identical |
| **Array creation** | `xr.DataArray(data, dims=['x'])` | `new_array(data, ['x'])` | 🟡 Good |
| **I/O operations** | `temp.to_netcdf('file.nc')` | `temp%to_netcdf('file.nc')` | ✅ Identical |
| **Plotting** | `temp.plot.line()` | `temp%plot%line()` | ✅ Identical |

**Overall Assessment**: ~85% syntax similarity achievable while maintaining full Fortran performance and type safety.