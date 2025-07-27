# Foxel Development Roadmap

## Overview
Foxel is a modern Fortran library for handling multi-dimensional labeled arrays and datasets, following NetCDF data model conventions. It provides a clean, type-safe interface for working with NetCDF4/HDF5 data in scientific computing applications.

## Core Concepts
- **Variable**: A named multi-dimensional array (0D scalar to nD array) with associated metadata
- **Dataset**: A collection of variables sharing coordinate systems
- **Coordinate**: A 1D variable that defines an axis
- **Dimension**: A named axis with a length

## Phase 1: Core Data Structures (Months 1-2)
- [ ] Define `variable_t` type for single NetCDF variables
  - [ ] Support 0D (scalar) through nD arrays
  - [ ] Dimension names and coordinate association
  - [ ] Attributes/metadata storage
- [ ] Define `dataset_t` type for collections of variables
  - [ ] Shared dimension and coordinate management
  - [ ] Global attributes
  - [ ] Variable collection with name-based access
- [ ] Support for all NetCDF data types:
  - [ ] Integer types (int8, int16, int32, int64)
  - [ ] Float types (real32, real64)
  - [ ] Character/string type
  - [ ] Logical/boolean type
- [ ] Memory management with automatic finalization

## Phase 2: I/O Operations (Months 2-3)
- [ ] NetCDF4 reader implementation
  - [ ] Read complete datasets with all variables
  - [ ] Selective variable reading
  - [ ] Support for groups (hierarchical structure)
  - [ ] Handle unlimited dimensions
- [ ] NetCDF4 writer implementation
  - [ ] Write complete datasets
  - [ ] Support compression and chunking
  - [ ] CF-convention compliance
- [ ] HDF5 support (using NetCDF4/HDF5 backend)
- [ ] CSV import/export for 2D slices
- [ ] JSON metadata import/export

## Phase 3: Data Access & Manipulation (Months 3-4)
- [ ] Positional indexing for variables
- [ ] Label-based selection using coordinates
- [ ] Slicing operations preserving coordinates
- [ ] Broadcasting operations between variables
- [ ] Arithmetic operations (+, -, *, /, **)
- [ ] Aggregation functions (mean, sum, min, max, std)
  - [ ] Along specific dimensions
  - [ ] Weighted operations
- [ ] Missing data handling (fill values, NaN)

## Phase 4: Advanced Operations (Month 4)
- [ ] Multi-dimensional interpolation
- [ ] Coordinate transformations
- [ ] Regridding operations
- [ ] Dataset merging and concatenation
- [ ] Variable alignment and reindexing

## Phase 5: Computation Engine (Months 5-6)
- [ ] Lazy evaluation for large datasets
- [ ] Chunked operations for out-of-core computation
- [ ] Parallel execution with OpenMP
- [ ] Memory-efficient algorithms
- [ ] User-defined functions along dimensions

## Phase 6: Visualization (Month 6)
- [ ] Integration with fortplot library
- [ ] Automatic plot type selection based on dimensionality
- [ ] Quick plot methods for variables
- [ ] Contour/surface plots for 2D slices
- [ ] Time series plots for 1D data

## Phase 7: Time Series & Geospatial (Month 7)
- [ ] CF-compliant time coordinate handling
- [ ] Time-based selection and resampling
- [ ] Rolling window operations
- [ ] Geospatial coordinate support (lat/lon grids)
- [ ] Map projections via proj4 bindings

## Phase 8: Quality & Performance (Month 8)
- [ ] Comprehensive test coverage
- [ ] Performance benchmarks vs. other tools
- [ ] Memory profiling and optimization
- [ ] Complete API documentation
- [ ] User guide with examples
- [ ] Migration guide from CDO/NCL

## Future Enhancements
- Zarr format support
- GRIB format support
- Distributed computing with coarrays
- Python bindings for interoperability
- Integration with climate/weather models