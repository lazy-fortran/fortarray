# Foxel Development Guidelines

## Project Overview
Foxel is a Fortran library for handling multi-dimensional labeled data, inspired by xarray and pandas. The library centers around NetCDF4 format and provides DataFrame-like functionality for scientific computing in Fortran.

## Core Design Principles
- Follow modern Fortran standards (Fortran 2018)
- Prioritize type safety and compile-time checks
- Maintain API consistency with xarray/pandas where reasonable
- Focus on performance without sacrificing usability
- Support both in-memory and out-of-core operations

## Coding Standards
- Use implicit none throughout
- Descriptive variable and procedure names
- Module-based organization
- Clear separation between public and private interfaces
- Comprehensive error handling with informative messages

## Testing Requirements
- Unit tests for all public procedures
- Integration tests for I/O operations
- Performance benchmarks for core operations
- Test with various NetCDF4 file formats
- Always run tests with: `OMP_NUM_THREADS=24 fpm test`

## Module Structure
```
foxel_types      - Core data type definitions
foxel_io         - I/O operations (NetCDF, CSV, etc.)
foxel_indexing   - Indexing and selection operations
foxel_compute    - Computational routines
foxel_utils      - Utility functions
foxel_plotting   - Visualization interface (using fortplot)
```

## Memory Management
- Use allocatable arrays for dynamic data
- Implement proper cleanup in finalizers
- Avoid move_alloc (as per global guidelines)
- Support chunked operations for large datasets

## Parallel Computing
- OpenMP for shared-memory parallelism
- Design thread-safe data structures
- Document any non-thread-safe operations
- Default to OMP_NUM_THREADS=24 for testing

## API Design Guidelines
- Consistent naming: `dataframe%select()`, `dataframe%mean()`, etc.
- Optional arguments for flexibility
- Return new objects rather than modifying in-place (functional style)
- Support method chaining where appropriate

## Documentation
- Document all public interfaces
- Provide usage examples in comments
- Maintain API reference documentation
- Include performance considerations

## Dependencies
- NetCDF-Fortran for file I/O
- fortplot for visualization capabilities
- OpenMP for parallel computing

## Version Control
- Atomic commits with clear messages
- Feature branches for new functionality
- Comprehensive PR descriptions
- No commented-out code in commits