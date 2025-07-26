# Foxel Types Reference

## Overview

This document provides detailed documentation of all types defined in the `foxel_types` module, which forms the foundation of the Foxel library.

## Core Types

### dataframe_t

The main type representing a multi-dimensional labeled array with coordinates and attributes.

```fortran
type :: dataframe_t
```

#### Fields

##### Basic Properties
- `initialized` (logical): Whether the dataframe has been properly initialized
- `n_dims` (integer): Number of dimensions
- `n_elements` (integer): Total number of elements in the data array

##### Dimension Information
- `dim_names(n_dims)` (character(256)): Names of each dimension
- `shape(n_dims)` (integer): Size along each dimension
- `strides(n_dims)` (integer): Memory strides for efficient indexing

##### Coordinate Arrays
- `coords(n_dims)` (coordinate_t): Coordinate values for each dimension

##### Data Storage
- `data` (data_storage_t): The actual data values

##### Attributes
- `n_attrs` (integer): Number of attributes
- `attr_keys(n_attrs)` (character(256)): Attribute names
- `attr_values(n_attrs)` (character(1024)): Attribute values

##### Memory Layout Flags
- `is_c_order` (logical): True for C-style row-major order, False for Fortran column-major (default)
- `is_view` (logical): True if this is a view of another dataframe
- `owns_memory` (logical): True if this dataframe owns its memory

##### Missing Value Handling
- `has_missing` (logical): Whether the data contains missing values
- `missing_value_*` : Missing value sentinels for each data type

##### Other
- `var_name` (character(256)): Variable name for single-variable dataframes
- `parent_id` (integer): Reference to parent dataframe (for views)

#### Example Usage

```fortran
program example_dataframe
    use foxel_types
    implicit none
    
    type(dataframe_t) :: df
    
    ! Initialize a 2D dataframe
    df%initialized = .true.
    df%n_dims = 2
    
    ! Set dimension names
    allocate(df%dim_names(2))
    df%dim_names(1) = "time"
    df%dim_names(2) = "station"
    
    ! Set shape
    allocate(df%shape(2))
    df%shape = [365, 10]  ! 365 days, 10 stations
    
    ! The dataframe will be automatically cleaned up when it goes out of scope
end program
```

### coordinate_t

Represents coordinate values along a dimension, supporting multiple data types.

```fortran
type :: coordinate_t
```

#### Fields

- `initialized` (logical): Whether the coordinate is initialized
- `length` (integer): Number of coordinate values
- `dtype` (integer): Data type (1=int32, 2=int64, 3=real32, 4=real64, 5=char)
- `values_*` (allocatable array): The actual coordinate values
- `is_monotonic` (logical): Whether values are monotonically increasing
- `is_regular` (logical): Whether spacing is regular
- `spacing` (real64): Spacing for regular coordinates

#### Example Usage

```fortran
! Create a time coordinate with daily values
type(coordinate_t) :: time_coord

time_coord%initialized = .true.
time_coord%dtype = 4  ! real64
time_coord%length = 365
allocate(time_coord%values_r64(365))

! Fill with day numbers
time_coord%values_r64 = [(real(i, real64), i=1,365)]
time_coord%is_monotonic = .true.
time_coord%is_regular = .true.
time_coord%spacing = 1.0_real64
```

### data_storage_t

Generic data storage supporting multiple data types.

```fortran
type :: data_storage_t
```

#### Fields

- `initialized` (logical): Whether storage is initialized
- `dtype` (integer): Data type (same encoding as coordinate_t)
- `n_elements` (integer): Total number of elements
- `values_*` (allocatable array): Flattened data array

#### Example Usage

```fortran
! Create storage for temperature data
type(data_storage_t) :: temp_data

temp_data%initialized = .true.
temp_data%dtype = 3  ! real32
temp_data%n_elements = 3650  ! 10 years of daily data
allocate(temp_data%values_r32(3650))

! Fill with temperature values
call random_number(temp_data%values_r32)
temp_data%values_r32 = temp_data%values_r32 * 30.0 + 10.0  ! Scale to 10-40°C
```

## Constants

### MAX_NAME_LEN
Maximum length for dimension names and attribute keys (256 characters).

### MAX_ATTR_LEN
Maximum length for attribute values (1024 characters).

## Data Type Encoding

The following integer codes are used throughout to identify data types:

1. `int32` - 32-bit integer
2. `int64` - 64-bit integer  
3. `real32` - 32-bit floating point (single precision)
4. `real64` - 64-bit floating point (double precision)
5. `char` - Character strings

## Memory Management

All types include automatic memory management through finalizers:

- `dataframe_t`: Cleans up all allocated arrays and nested types
- `coordinate_t`: Deallocates coordinate value arrays
- `data_storage_t`: Deallocates data arrays

Finalizers are called automatically when objects go out of scope.

## Best Practices

1. Always set `initialized = .true.` after setting up a type
2. Use the appropriate `dtype` code for your data
3. For views, set `owns_memory = .false.` to prevent double-deallocation
4. Dimension names should be unique within a dataframe
5. Coordinate lengths must match the corresponding dimension size

## Thread Safety

The types themselves are not thread-safe. When using in parallel regions:
- Use private copies for each thread
- Synchronize access to shared dataframes
- Consider creating views for read-only access

## Future Extensions

The type system is designed to be extensible:
- Additional data types can be added by extending the dtype encoding
- Complex numbers support planned
- Bit/boolean type planned
- Variable-length string support planned