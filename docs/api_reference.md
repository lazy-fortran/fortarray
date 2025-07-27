# Foxel API Reference

Complete reference for all public interfaces in the Foxel library.

## Core Types

### `variable_t`

Multi-dimensional labeled array following NetCDF conventions.

**Components:**
```fortran
type :: variable_t
    character(len=:), allocatable :: name
    character(len=:), allocatable :: units
    character(len=:), allocatable :: standard_name
    character(len=:), allocatable :: long_name
    type(storage_t) :: data
    integer :: n_elements
    integer :: n_dims
    integer, dimension(:), allocatable :: shape
    character(len=64), dimension(:), allocatable :: dim_names
    type(coordinate_t), dimension(:), allocatable :: coords
    logical, dimension(:), allocatable :: has_coord
end type variable_t
```

### `dataset_t`

Collection of variables with shared dimensions.

**Components:**
```fortran
type :: dataset_t
    character(len=:), allocatable :: name
    type(variable_t), dimension(:), allocatable :: variables
    integer :: n_vars
    character(len=:), allocatable :: title
    character(len=:), allocatable :: institution
    character(len=:), allocatable :: source
    character(len=:), allocatable :: history
    character(len=:), allocatable :: references
    character(len=:), allocatable :: comment
end type dataset_t
```

### `coordinate_t`

1D variable defining an axis.

**Components:**
```fortran
type :: coordinate_t
    character(len=:), allocatable :: name
    character(len=:), allocatable :: units
    character(len=:), allocatable :: standard_name
    character(len=:), allocatable :: calendar
    integer :: length
    character(len=16) :: data_type
    real(real64), dimension(:), allocatable :: values_r64
    real(real32), dimension(:), allocatable :: values_r32
    integer(int64), dimension(:), allocatable :: values_i64
    integer(int32), dimension(:), allocatable :: values_i32
    logical :: initialized = .false.
end type coordinate_t
```

## Constructor Functions

### Variables

#### `variable(data, name, dim_names, units, coords)`

Create a variable from data array.

**Parameters:**
- `data`: Real or integer array (any rank)
- `name`: Variable name (optional)
- `dim_names`: Dimension names (optional)
- `units`: Physical units (optional)
- `coords`: Coordinate arrays (optional)

**Returns:** `variable_t`

**Examples:**
```fortran
! 1D variable
var = variable([1.0, 2.0, 3.0], name="temperature", units="K")

! 2D variable with dimensions
var = variable(data_2d, name="pressure", dim_names=["lat", "lon"])

! With coordinates
var = variable(data, name="temp", coords=[time_coord, lat_coord])
```

#### `variable_from_scalar(value, name)`

Create scalar (0D) variable.

**Parameters:**
- `value`: Scalar value (real or integer)
- `name`: Variable name (optional)

**Returns:** `variable_t`

### Datasets

#### `dataset(name)`

Create empty dataset.

**Parameters:**
- `name`: Dataset name (optional)

**Returns:** `dataset_t`

#### `dataset_from_variables(vars, name)`

Create dataset from variable array.

**Parameters:**
- `vars`: Array of variables
- `name`: Dataset name (optional)

**Returns:** `dataset_t`

### Coordinates

#### `create_coordinate(coord, length, data_type, stat)`

Create coordinate variable.

**Parameters:**
- `coord`: Coordinate object (output)
- `length`: Number of coordinate points
- `data_type`: "real64", "real32", "int64", "int32"
- `stat`: Status code (output, optional)

## I/O Operations

### NetCDF

#### `write_netcdf(filename, dataset, stat)`

Write dataset to NetCDF file.

**Parameters:**
- `filename`: Output file path
- `dataset`: Dataset to write
- `stat`: Status code (output, optional)

**Returns:** Integer status (0 = success)

#### `read_netcdf(filename, stat, error_msg)`

Read dataset from NetCDF file.

**Parameters:**
- `filename`: Input file path
- `stat`: Status code (output, optional)
- `error_msg`: Error message (output, optional)

**Returns:** `dataset_t`

#### `write_netcdf_variable(filename, variable, stat)`

Write single variable to NetCDF file.

**Parameters:**
- `filename`: Output file path
- `variable`: Variable to write
- `stat`: Status code (output, optional)

**Returns:** Integer status

#### `read_netcdf_variable(filename, varname, stat)`

Read single variable from NetCDF file.

**Parameters:**
- `filename`: Input file path
- `varname`: Variable name to read
- `stat`: Status code (output, optional)

**Returns:** `variable_t`

### CSV

#### `write_csv(filename, variable, stat)`

Write 2D variable to CSV file.

**Parameters:**
- `filename`: Output file path
- `variable`: 2D variable to write
- `stat`: Status code (output, optional)

**Returns:** Integer status

#### `read_csv(filename, stat)`

Read CSV file as 2D variable.

**Parameters:**
- `filename`: Input file path
- `stat`: Status code (output, optional)

**Returns:** `variable_t`

### Format Detection

#### `from_file(filename, varname, stat)`

Auto-detect format and read file.

**Parameters:**
- `filename`: Input file path
- `varname`: Variable name (for NetCDF, optional)
- `stat`: Status code (output, optional)

**Returns:** `variable_t` or `dataset_t`

## Dataset Operations

#### `add_variable(dataset, variable)`

Add variable to dataset.

**Parameters:**
- `dataset`: Dataset object
- `variable`: Variable to add

#### `get_variable(dataset, name)`

Get variable by name.

**Parameters:**
- `dataset`: Dataset object
- `name`: Variable name

**Returns:** `variable_t`

#### `has_variable(dataset, name)`

Check if variable exists.

**Parameters:**
- `dataset`: Dataset object
- `name`: Variable name

**Returns:** `logical`

#### `remove_variable(dataset, name)`

Remove variable from dataset.

**Parameters:**
- `dataset`: Dataset object
- `name`: Variable name

#### `list_variables(dataset)`

Get array of variable names.

**Parameters:**
- `dataset`: Dataset object

**Returns:** `character` array

## Indexing Operations

#### `get_item(variable, indices...)`

Get element(s) by position.

**Parameters:**
- `variable`: Variable object
- `indices`: Positional indices (1 to 3 dimensions)

**Returns:** `real(real64)` or `variable_t`

**Examples:**
```fortran
! Single element
value = get_item(var, 5)

! 2D indexing
value = get_item(var_2d, 10, 20)

! 3D indexing
value = get_item(var_3d, 5, 10, 15)
```

#### `set_item(variable, indices..., value)`

Set element by position.

**Parameters:**
- `variable`: Variable object
- `indices`: Positional indices
- `value`: New value

#### `create_index(position)`

Create positional index.

**Parameters:**
- `position`: Array position

**Returns:** `index_t`

#### `create_slice(start, stop, stride)`

Create slice index.

**Parameters:**
- `start`: Start position (optional)
- `stop`: Stop position (optional)  
- `stride`: Step size (optional)

**Returns:** `index_t`

## Slicing Operations

#### `slice_range(variable, start, stop)`

Slice variable by range.

**Parameters:**
- `variable`: Variable object
- `start`: Start index
- `stop`: Stop index

**Returns:** `variable_t`

#### `slice_with_step(variable, start, stop, step)`

Slice with step size.

**Parameters:**
- `variable`: Variable object
- `start`: Start index
- `stop`: Stop index
- `step`: Step size

**Returns:** `variable_t`

#### `slice_from_negative(variable, negative_idx)`

Slice from end (negative indexing).

**Parameters:**
- `variable`: Variable object
- `negative_idx`: Negative index

**Returns:** `variable_t`

## Arithmetic Operations

All arithmetic operations support broadcasting and return new variables.

#### Binary Operators

- `var1 + var2`: Addition
- `var1 - var2`: Subtraction  
- `var1 * var2`: Multiplication
- `var1 / var2`: Division
- `var1 ** var2`: Exponentiation

#### Scalar Operations

- `var + scalar`: Add scalar to all elements
- `var - scalar`: Subtract scalar from all elements
- `var * scalar`: Multiply by scalar
- `var / scalar`: Divide by scalar
- `var ** scalar`: Raise to scalar power

#### Comparison Operations

- `var1 > var2`: Greater than
- `var1 < var2`: Less than
- `var1 >= var2`: Greater than or equal
- `var1 <= var2`: Less than or equal
- `var1 == var2`: Equal to
- `var1 /= var2`: Not equal to

**Returns:** `variable_t` with logical values

## Aggregation Functions

#### `mean(variable, dim)`

Compute mean along dimension.

**Parameters:**
- `variable`: Variable object
- `dim`: Dimension number (optional, default: all)

**Returns:** `variable_t`

#### `sum(variable, dim)`

Compute sum along dimension.

**Parameters:**
- `variable`: Variable object
- `dim`: Dimension number (optional)

**Returns:** `variable_t`

#### `minval(variable, dim)`

Find minimum value.

**Parameters:**
- `variable`: Variable object
- `dim`: Dimension number (optional)

**Returns:** `variable_t`

#### `maxval(variable, dim)`

Find maximum value.

**Parameters:**
- `variable`: Variable object
- `dim`: Dimension number (optional)

**Returns:** `variable_t`

#### `std(variable, dim)`

Compute standard deviation.

**Parameters:**
- `variable`: Variable object
- `dim`: Dimension number (optional)

**Returns:** `variable_t`

#### `var(variable, dim)`

Compute variance.

**Parameters:**
- `variable`: Variable object
- `dim`: Dimension number (optional)

**Returns:** `variable_t`

#### Weighted Aggregations

#### `weighted_mean(variable, weights, dim)`

Compute weighted mean.

**Parameters:**
- `variable`: Variable object
- `weights`: Weight array
- `dim`: Dimension number (optional)

**Returns:** `variable_t`

## Missing Data Operations

#### `fillna(variable, method, value)`

Fill missing values.

**Parameters:**
- `variable`: Variable object
- `method`: Fill method ("constant", "forward", "backward", "interpolate")
- `value`: Fill value (for "constant" method, optional)

**Returns:** `variable_t`

#### `dropna(variable, dim)`

Remove missing values.

**Parameters:**
- `variable`: Variable object
- `dim`: Dimension to drop along (optional)

**Returns:** `variable_t`

#### `where(condition, x, y)`

Select elements based on condition.

**Parameters:**
- `condition`: Logical variable
- `x`: Value for true elements
- `y`: Value for false elements

**Returns:** `variable_t`

#### `isnan(variable)`

Check for NaN values.

**Parameters:**
- `variable`: Variable object

**Returns:** `variable_t` with logical values

## Coordinate Selection

#### `select_by_value(variable, coord_name, value, method)`

Select by coordinate value.

**Parameters:**
- `variable`: Variable object
- `coord_name`: Coordinate name
- `value`: Target value
- `method`: Selection method ("nearest", "exact", optional)

**Returns:** `variable_t`

#### `select_by_range(variable, coord_name, start_val, end_val)`

Select by coordinate range.

**Parameters:**
- `variable`: Variable object
- `coord_name`: Coordinate name
- `start_val`: Range start
- `end_val`: Range end

**Returns:** `variable_t`

## Boolean Indexing

#### `mask(variable, condition)`

Apply boolean mask.

**Parameters:**
- `variable`: Variable object
- `condition`: Logical variable

**Returns:** `variable_t`

#### `filter_by_condition(variable, condition)`

Filter elements by condition.

**Parameters:**
- `variable`: Variable object
- `condition`: Logical condition

**Returns:** `variable_t`

## Apply Functions

#### `apply_function(variable, func, dim)`

Apply user function along dimension.

**Parameters:**
- `variable`: Variable object
- `func`: Function pointer
- `dim`: Dimension number (optional)

**Returns:** `variable_t`

#### `apply_ufunc(func, variables...)`

Apply universal function to variables.

**Parameters:**
- `func`: Function pointer
- `variables`: Input variables

**Returns:** `variable_t`

## Time Operations

#### `resample(variable, frequency, method)`

Resample time series.

**Parameters:**
- `variable`: Time series variable
- `frequency`: Target frequency ("D", "M", "Y", etc.)
- `method`: Aggregation method ("mean", "sum", etc.)

**Returns:** `variable_t`

#### `rolling_mean(variable, window)`

Compute rolling mean.

**Parameters:**
- `variable`: Variable object
- `window`: Window size

**Returns:** `variable_t`

#### `rolling_sum(variable, window)`

Compute rolling sum.

**Parameters:**
- `variable`: Variable object
- `window`: Window size

**Returns:** `variable_t`

#### `seasonal_mean(variable)`

Compute seasonal means.

**Parameters:**
- `variable`: Time series variable

**Returns:** `variable_t`

#### `seasonal_decompose(variable)`

Decompose into trend/seasonal/residual.

**Parameters:**
- `variable`: Time series variable

**Returns:** `dataset_t` with components

## Parallel Computing

#### `set_num_threads(n_threads)`

Set number of OpenMP threads.

**Parameters:**
- `n_threads`: Number of threads

#### `get_num_threads()`

Get current number of threads.

**Returns:** Integer

#### Parallel Operations

Most operations automatically use parallel processing:
- Arithmetic operations
- Aggregations
- Apply functions
- Time series operations

## Chunked Operations

#### `set_chunk_size(size)`

Set default chunk size.

**Parameters:**
- `size`: Chunk size in elements

#### `mean_chunked(variable)`

Compute mean using chunked processing.

**Parameters:**
- `variable`: Variable object

**Returns:** `variable_t`

#### `sum_chunked(variable)`

Compute sum using chunked processing.

**Parameters:**
- `variable`: Variable object

**Returns:** `variable_t`

## Optimization

#### `enable_optimization()`

Enable performance optimizations.

#### `disable_optimization()`

Disable optimizations (for debugging).

#### `vectorized_add(var1, var2)`

SIMD-optimized addition.

**Parameters:**
- `var1`, `var2`: Variable objects

**Returns:** `variable_t`

#### `vectorized_multiply(var1, var2)`

SIMD-optimized multiplication.

**Parameters:**
- `var1`, `var2`: Variable objects

**Returns:** `variable_t`

## Plotting

#### `plot(variable, x_coord, title)`

Create line plot.

**Parameters:**
- `variable`: 1D variable
- `x_coord`: X coordinate name (optional)
- `title`: Plot title (optional)

#### `contour_plot(variable, levels)`

Create contour plot.

**Parameters:**
- `variable`: 2D variable
- `levels`: Contour levels (optional)

#### `surface_plot(variable)`

Create 3D surface plot.

**Parameters:**
- `variable`: 2D variable

## Memory Management

#### `finalize_variable(variable)`

Free variable memory.

**Parameters:**
- `variable`: Variable object

#### `finalize_dataset(dataset)`

Free dataset memory.

**Parameters:**
- `dataset`: Dataset object

#### `finalize_coordinate(coordinate)`

Free coordinate memory.

**Parameters:**
- `coordinate`: Coordinate object

## Error Codes

- `0`: Success
- `-1`: General error
- `-2`: File not found
- `-3`: Invalid dimensions
- `-4`: Type mismatch
- `-5`: Memory allocation error
- `-6`: NetCDF error
- `-7`: Index out of bounds

## Constants

```fortran
! Data type constants
integer, parameter :: DTYPE_REAL64 = 1
integer, parameter :: DTYPE_REAL32 = 2
integer, parameter :: DTYPE_INT64 = 3
integer, parameter :: DTYPE_INT32 = 4

! Fill values
real(real64), parameter :: FILL_VALUE_R64 = -9999.0_real64
real(real32), parameter :: FILL_VALUE_R32 = -9999.0_real32
integer(int64), parameter :: FILL_VALUE_I64 = -9999_int64
integer(int32), parameter :: FILL_VALUE_I32 = -9999_int32
```