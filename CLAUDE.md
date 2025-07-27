# Foxel Development Guidelines

## Project Overview
Foxel is a Fortran library for handling multi-dimensional labeled arrays and datasets, following NetCDF data model conventions. The library provides a clean, type-safe interface for scientific data analysis in Fortran, with NetCDF4/HDF5 as the primary data formats.

## Core Design Principles
- Follow NetCDF data model: Variables, Dimensions, Coordinates, Attributes
- Maintain consistency with CF (Climate and Forecast) conventions
- Follow modern Fortran standards (Fortran 2018)
- Prioritize type safety and compile-time checks
- Support both in-memory and out-of-core operations
- Enable seamless NetCDF4/HDF5 interoperability

## Type System Architecture
```fortran
! Core types following NetCDF model
variable_t    - Single multi-dimensional array (0D to nD)
dataset_t     - Collection of variables with shared dimensions
coordinate_t  - 1D variable defining an axis
dimension_t   - Named axis with length
attribute_t   - Key-value metadata
```

## Coding Standards
- Use implicit none throughout
- Descriptive variable and procedure names following NetCDF terminology
- Module-based organization with clear interfaces
- Comprehensive error handling with informative messages
- NO SHORTCUTS, NO SIMPLIFICATIONS, NO CHEATING
- Full implementation of all features as specified

## Testing Requirements
- Unit tests for ALL public procedures
- Integration tests for NetCDF I/O operations
- Test with real scientific datasets
- Always run tests with: `OMP_NUM_THREADS=24 fpm test`
- Test 0D scalars through nD arrays
- Test edge cases and error conditions

## Module Structure
```
foxel_types       - Core type definitions (variable_t, dataset_t, etc.)
foxel_storage     - Generic data storage interfaces
foxel_netcdf      - NetCDF4 I/O operations
foxel_hdf5        - HDF5 I/O operations (via NetCDF4)
foxel_indexing    - Label and positional indexing
foxel_compute     - Computational routines
foxel_coords      - Coordinate handling and transformations
foxel_plotting    - Visualization interface (using fortplot)
```

## Memory Management
- Use allocatable arrays for dynamic data
- Implement proper finalizers for all types
- Avoid move_alloc (as per global guidelines)
- Support chunked operations for datasets larger than memory
- Clear ownership semantics for all allocations

## Parallel Computing
- OpenMP for shared-memory parallelism
- Thread-safe data structures
- Parallel I/O where supported by NetCDF4
- Default to OMP_NUM_THREADS=24 for testing
- Document any non-thread-safe operations

## API Design Guidelines
- NetCDF-oriented naming conventions
- Methods on variables: `var%slice()`, `var%mean()`, `var%interp()`
- Methods on datasets: `ds%variables()`, `ds%select()`, `ds%merge()`
- Optional arguments for flexibility
- Support method chaining where appropriate
- Clear distinction between in-place and copying operations

## I/O Conventions
- Primary format: NetCDF4 (with HDF5 backend)
- Support CF conventions by default
- Preserve all metadata on read/write
- Handle unlimited dimensions properly
- Support compression and chunking options

## Documentation
- Document all public interfaces with examples
- Follow NetCDF/CF terminology consistently
- Include performance considerations
- Provide migration guides from CDO/NCL/xarray
- Maintain comprehensive API reference

## Dependencies
- NetCDF-Fortran for file I/O
- HDF5 (via NetCDF4)
- fortplot for visualization
- OpenMP for parallel computing

## Development Process
1. Write comprehensive tests FIRST
2. Implement full functionality (no placeholders)
3. Ensure all tests pass
4. Update documentation
5. Run performance benchmarks
6. Code review before merge

## Version Control
- Atomic commits with clear messages
- Reference NetCDF/CF conventions in commits
- No commented-out code
- Full test coverage for all changes