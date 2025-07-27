# Foxel Architecture Overview

This document provides a comprehensive overview of the Foxel library architecture, design principles, and internal structure.

## Design Philosophy

Foxel follows the **NetCDF data model** with strict adherence to **CF (Climate and Forecast) conventions**. The library is designed around these core principles:

1. **NetCDF-First Design**: All types and operations mirror the NetCDF data model
2. **Type Safety**: Compile-time checks and runtime validation
3. **Memory Efficiency**: Explicit memory management with clear ownership semantics
4. **Performance**: Optimized for scientific computing with SIMD and OpenMP
5. **Interoperability**: Native NetCDF4/HDF5 compatibility

## Core Data Model

### Type Hierarchy

```
foxel_types (Core Types)
├── variable_t          - Multi-dimensional labeled array (NetCDF variable)
├── dataset_t           - Collection of variables (NetCDF file)
├── coordinate_t        - 1D variable defining an axis
├── dimension_t         - Named axis with length
├── attribute_t         - Key-value metadata
└── storage_t           - Generic data storage backend
```

### NetCDF Mapping

| NetCDF Concept | Foxel Type | Description |
|----------------|------------|-------------|
| Variable | `variable_t` | Multi-dimensional array with metadata |
| Dataset/File | `dataset_t` | Collection of variables sharing dimensions |
| Dimension | `dimension_t` | Named axis with size |
| Coordinate Variable | `coordinate_t` | 1D variable defining axis values |
| Attribute | `attribute_t` | Metadata key-value pairs |

## Module Architecture

### Layer 1: Core Types (`foxel_types`)
```fortran
module foxel_types
    ! Core type definitions following NetCDF model
    type :: variable_t
        type(storage_t) :: data               ! Generic data storage
        character(len=:), allocatable :: name ! Variable name
        type(dimension_t), allocatable :: dims(:) ! Dimension definitions
        type(coordinate_t), allocatable :: coords(:) ! Coordinate arrays
        type(attribute_t), allocatable :: attrs(:) ! Metadata
    end type

    type :: dataset_t
        type(variable_t), allocatable :: vars(:) ! Variables
        type(dimension_t), allocatable :: dims(:) ! Shared dimensions
        type(attribute_t), allocatable :: attrs(:) ! Global attributes
    end type
end module
```

### Layer 2: Storage Backend (`foxel_storage`)
```fortran
module foxel_storage
    ! Generic type-safe data storage with runtime type information
    type :: storage_t
        integer :: data_type    ! Type identifier (real64, real32, int32, etc.)
        integer :: n_elements   ! Total number of elements
        integer, allocatable :: shape(:) ! Array dimensions
        
        ! Type-specific storage (only one allocated at a time)
        real(real64), allocatable :: values_r64(:)
        real(real32), allocatable :: values_r32(:)
        integer(int32), allocatable :: values_i32(:)
        integer(int64), allocatable :: values_i64(:)
        logical, allocatable :: values_logical(:)
    end type
end module
```

### Layer 3: I/O Operations
```
foxel_netcdf     - NetCDF4 file I/O
foxel_csv        - CSV text file I/O
foxel_formats    - Format detection and dispatch
```

### Layer 4: Data Operations
```
foxel_indexing    - Label and positional indexing
foxel_arithmetic  - Variable arithmetic operations
foxel_aggregation - Statistical aggregation functions
foxel_broadcasting - Dimension alignment and broadcasting
foxel_missing     - Missing data handling
```

### Layer 5: Advanced Features
```
foxel_compute     - Apply functions and lazy evaluation
foxel_parallel    - OpenMP parallel operations
foxel_optimization - SIMD and cache optimizations
foxel_plotting    - Visualization via fortplot
foxel_time        - Time coordinate handling
```

## Memory Management

### Ownership Model

1. **Variables own their data**: Each `variable_t` owns its `storage_t`
2. **Datasets own variables**: Each `dataset_t` owns its `variable_t` array
3. **Explicit finalization**: All types have `finalize_*` procedures
4. **Deep copying**: Coordinates and attributes are deep-copied during construction

### Memory Layout

```fortran
! Column-major (Fortran native) layout
real(real64) :: data(lon, lat, time)  ! [longitude, latitude, time]

! Linear indexing calculation
linear_index = (time-1)*lat*lon + (lat-1)*lon + (lon-1) + 1
```

### Allocation Strategy

- **Stack allocation**: Small arrays and metadata
- **Heap allocation**: Large data arrays via `allocatable`
- **Chunked processing**: For datasets larger than memory
- **Reference counting**: Planned for shared coordinates

## Type System

### Generic Storage

The `storage_t` type provides type-safe storage with runtime type identification:

```fortran
! Type identifiers
integer, parameter :: TYPE_REAL64 = 1
integer, parameter :: TYPE_REAL32 = 2
integer, parameter :: TYPE_INT32 = 3
integer, parameter :: TYPE_INT64 = 4
integer, parameter :: TYPE_LOGICAL = 5

! Only one array is allocated per storage instance
type(storage_t) :: storage
storage%data_type = TYPE_REAL64
allocate(storage%values_r64(n_elements))
```

### Type Promotion Rules

For arithmetic operations between different types:

1. `real64` + `real32` → `real64`
2. `real64` + `integer` → `real64`
3. `real32` + `integer` → `real32`
4. `int64` + `int32` → `int64`
5. Any type + `logical` → Same type (logical used as mask)

## Error Handling

### Status Code Convention

All I/O operations return integer status codes:

```fortran
integer, parameter :: SUCCESS = 0
integer, parameter :: ERROR_FILE_NOT_FOUND = 1
integer, parameter :: ERROR_INVALID_VARIABLE = 2
integer, parameter :: ERROR_DIMENSION_MISMATCH = 3
integer, parameter :: ERROR_TYPE_MISMATCH = 4
integer, parameter :: ERROR_MEMORY_ALLOCATION = 5
```

### Error Propagation

```fortran
! Library functions
function read_netcdf_variable(filename, varname, stat) result(var)
    character(len=*), intent(in) :: filename, varname
    integer, intent(out), optional :: stat
    type(variable_t) :: var
    
    ! Implementation sets stat on error
    if (present(stat)) stat = ERROR_FILE_NOT_FOUND
end function

! User code
var = read_netcdf_variable("data.nc", "temperature", stat=status)
if (status /= SUCCESS) then
    write(*,*) "Error reading variable"
end if
```

## Performance Architecture

### SIMD Optimization

Vectorized operations use compiler directives:

```fortran
!$OMP SIMD ALIGNED(result_data, var1_data, var2_data: 64)
do i = 1, n
    result_data(i) = var1_data(i) + var2_data(i)
end do
```

### OpenMP Parallelization

```fortran
!$OMP PARALLEL DO REDUCTION(+:sum_val) SCHEDULE(STATIC)
do i = 1, n_elements
    sum_val = sum_val + data(i)
end do
!$OMP END PARALLEL DO
```

### Cache Optimization

- **Chunk processing**: Process data in cache-friendly blocks
- **Memory alignment**: 64-byte alignment for SIMD
- **Loop tiling**: For multi-dimensional operations
- **Data locality**: Minimize memory bandwidth usage

## Extensibility Points

### Adding New Data Types

1. Add type identifier to `foxel_storage`
2. Add storage array to `storage_t`
3. Update constructors and accessors
4. Add type promotion rules
5. Update I/O routines

### Adding New File Formats

1. Create new I/O module (e.g., `foxel_hdf5`)
2. Implement `read_*` and `write_*` functions
3. Add format detection logic
4. Update `from_file()` dispatcher

### Adding New Operations

1. Create operation module (e.g., `foxel_signal_processing`)
2. Follow existing patterns for type safety
3. Add comprehensive tests
4. Document in API reference

## Thread Safety

### Thread-Safe Components

- **Data structures**: Immutable after construction
- **Read operations**: Concurrent reads are safe
- **Mathematical operations**: Stateless functions

### Thread-Unsafe Components

- **Construction/destruction**: Requires synchronization
- **I/O operations**: File handle management
- **Global state**: Thread counts, optimization settings

### Parallelization Strategy

```fortran
! OpenMP parallel regions for data processing
!$OMP PARALLEL
    !$OMP DO
    do chunk = 1, n_chunks
        call process_chunk(data, chunk, chunk_size)
    end do
    !$OMP END DO
!$OMP END PARALLEL
```

## Build System Integration

### FPM Configuration

```toml
[build]
auto-executables = true
auto-tests = true
auto-examples = true

[dependencies]
netcdf-fortran = "*"
fortplot = { git = "https://github.com/fortls/fortplot.git" }

[dev-dependencies]
funit = "*"
```

### Compiler Requirements

- **Fortran 2018** standard compliance
- **OpenMP 4.5+** for parallelization
- **IEEE arithmetic** for floating-point operations
- **C interoperability** for NetCDF interface

## Testing Architecture

### Test Organization

```
test/
├── test_types.f90           - Core type functionality
├── test_constructors.f90    - Variable/dataset construction
├── test_indexing.f90        - All indexing operations
├── test_io.f90             - File I/O operations
├── test_arithmetic.f90      - Mathematical operations
├── test_aggregation.f90     - Statistical functions
├── test_integration.f90     - End-to-end workflows
├── test_performance.f90     - Performance benchmarks
├── test_memory_leaks.f90    - Memory management
└── test_api_coverage.f90    - API completeness
```

### Test Principles

1. **100% API coverage**: Every public procedure tested
2. **Edge case testing**: Boundary conditions and error cases
3. **Performance benchmarks**: Automated performance regression detection
4. **Memory leak detection**: Valgrind integration
5. **Integration testing**: Real-world scientific workflows

## Future Architecture Evolution

### Planned Extensions

1. **Distributed computing**: MPI support for large clusters
2. **GPU acceleration**: CUDA/HIP kernels for arithmetic operations
3. **Lazy evaluation**: Computation graphs for complex workflows
4. **Metadata standards**: Extended CF convention support
5. **Compression**: Advanced compression algorithms

### API Stability

- **Core types**: Stable, breaking changes will increment major version
- **I/O operations**: Stable, extensions will be backward compatible
- **Mathematical operations**: Stable API, implementation may be optimized
- **Advanced features**: May evolve based on user feedback

## Contributing Guidelines

### Code Organization

- One module per file
- Clear separation of concerns
- Consistent naming conventions
- Comprehensive documentation

### Performance Requirements

- **Memory usage**: O(data_size) for core operations
- **Computational complexity**: Optimal algorithms for all operations
- **I/O performance**: Efficient NetCDF4 usage with compression
- **Scalability**: Linear speedup with OpenMP threads

This architecture ensures Foxel remains maintainable, performant, and extensible while providing a clean, type-safe interface for scientific data analysis in Fortran.