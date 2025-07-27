# Foxel Development Backlog

## Development Principles
- **NO SHORTCUTS**: Every feature must be fully implemented with proper error handling
- **NO SIMPLIFICATIONS**: Full functionality as described, no "minimal viable" versions
- **NO CHEATING**: Proper algorithms, no placeholder implementations
- **COMPLETE TESTING**: Every public API must have comprehensive tests
- **FULL DOCUMENTATION**: Every module, type, and procedure must be documented
- **NETCDF COMPLIANCE**: Follow NetCDF data model and CF conventions

## Architecture Refactoring (IMMEDIATE PRIORITY)

### Sprint 0: Refactor to NetCDF Model ⚠️ IN PROGRESS
- [ ] Rename `dataframe_t` to `variable_t` throughout codebase
- [ ] Update all references from "dataframe" to "variable"
- [ ] Create `dataset_t` type for multi-variable collections
- [ ] Update module names to reflect NetCDF terminology
- [ ] Refactor tests to use new type names
- [ ] Update all documentation

## Phase 1: Core Data Structures (Sprint 1-4)

### Sprint 1: Basic Type Definitions ✓ (NEEDS REFACTORING)
- [x] Define core types (currently as dataframe_t, needs rename to variable_t)
- [ ] Refactor to variable_t with NetCDF semantics
- [ ] Add dimension_t type for named dimensions
- [ ] Add dataset_t type for variable collections
- [ ] Update finalizers for new types
- [ ] Refactor test suite for new type names

### Sprint 2: Generic Data Storage ✓ (COMPATIBLE)
- [x] Implement generic interfaces for data types
- [x] Create type-specific storage modules
- [x] Implement proper type conversion routines
- [x] Add bounds checking for all array operations
- [x] Test all type combinations exhaustively

### Sprint 3: Constructor Functions (NEEDS REFACTORING)
- [ ] Implement `variable()` constructor (rename from dataframe())
  - [ ] Support 0D scalars through nD arrays
  - [ ] Validate dimension names (no duplicates, valid identifiers)
  - [ ] Validate coordinate arrays (correct lengths, monotonic if needed)
  - [ ] Handle optional parameters properly
- [ ] Implement `dataset()` constructor
  - [ ] Manage shared dimensions across variables
  - [ ] Validate variable consistency
  - [ ] Handle global attributes
- [ ] Create convenience constructors
  - [ ] From arrays with auto-generated coordinates
  - [ ] From NetCDF files
  - [ ] Empty variables/datasets with specified dimensions
- [ ] Test edge cases (scalars, empty data, huge arrays)

### Sprint 4: Basic Indexing
- [ ] Implement positional indexing for variables
- [ ] Implement label-based indexing using coordinates
- [ ] Support multi-dimensional indexing
- [ ] Handle scalar variable indexing (0D)
- [ ] Create dataset variable selection methods
- [ ] Test all indexing combinations

## Phase 2: I/O Operations (Sprint 5-8)

### Sprint 5: NetCDF4 Reader
- [ ] Implement complete NetCDF4 file reading
  - [ ] Read all dimensions and their lengths
  - [ ] Read all variables with proper types
  - [ ] Handle coordinate variables specially
  - [ ] Read all attributes (global and variable)
  - [ ] Support NetCDF4 groups
- [ ] Handle special cases
  - [ ] Scalar variables (0D)
  - [ ] Unlimited dimensions
  - [ ] String variables
  - [ ] Missing values and fill values
- [ ] Create dataset from NetCDF file
- [ ] Selective variable loading
- [ ] Test with real NetCDF files

### Sprint 6: NetCDF4 Writer
- [ ] Implement complete NetCDF4 writing
  - [ ] Define dimensions (including unlimited)
  - [ ] Define variables with proper types
  - [ ] Write coordinate variables
  - [ ] Write all attributes with type safety
  - [ ] Support compression and chunking
- [ ] Dataset to NetCDF file conversion
- [ ] Atomic writes (temp file + rename)
- [ ] CF-convention compliance checking
- [ ] Test round-trip fidelity

### Sprint 7: CSV I/O
- [ ] CSV to 2D variable conversion
- [ ] Variable to CSV export (2D only)
- [ ] Handle headers and data types
- [ ] Support missing values
- [ ] Test with real CSV files

### Sprint 8: Format Detection
- [ ] Implement `from_file()` with auto-detection
- [ ] Support .nc, .nc4, .hdf5, .csv extensions
- [ ] Content-based format detection
- [ ] Comprehensive error messages

## Phase 3: Data Manipulation (Sprint 9-12)

### Sprint 9: Broadcasting Engine
- [ ] Implement dimension alignment
- [ ] Support operations between variables
- [ ] Handle scalar broadcasting
- [ ] Optimize common patterns
- [ ] Test all scenarios

### Sprint 10: Arithmetic Operations
- [ ] Variable arithmetic (+, -, *, /, **)
- [ ] Scalar-variable operations
- [ ] Type promotion rules
- [ ] NaN/missing value handling
- [ ] Operator overloading

### Sprint 11: Aggregation Functions
- [ ] Statistical functions on variables
  - [ ] Mean, sum, min, max
  - [ ] Standard deviation, variance
  - [ ] Quantiles, median
- [ ] Operations along dimensions
- [ ] Weighted aggregations
- [ ] Handle missing data properly

### Sprint 12: Missing Data
- [ ] Define fill value semantics
- [ ] Implement where() for masking
- [ ] fillna() with various methods
- [ ] dropna() along dimensions
- [ ] Interpolation for missing values

## Phase 4: Advanced Indexing (Sprint 13-16)

### Sprint 13: Coordinate-based Selection
- [ ] Select by coordinate values
- [ ] Range selection (inclusive)
- [ ] Nearest-neighbor selection
- [ ] Multi-dimensional selection
- [ ] Performance optimization

### Sprint 14: Boolean Indexing
- [ ] Create boolean masks
- [ ] Apply masks to variables
- [ ] Conditional selection
- [ ] Multi-condition support

### Sprint 15: Slicing Operations
- [ ] Implement slice syntax
- [ ] Support negative indices
- [ ] Step values in slices
- [ ] Preserve coordinates

### Sprint 16: Interpolation
- [ ] Linear interpolation
- [ ] Nearest-neighbor
- [ ] Higher-order methods
- [ ] Extrapolation options

## Phase 5: Computation Engine (Sprint 17-20)

### Sprint 17: Apply Functions
- [ ] Apply along dimensions
- [ ] User-defined functions
- [ ] Vectorized operations
- [ ] Result type inference

### Sprint 18: Lazy Evaluation
- [ ] Computation graph
- [ ] Deferred execution
- [ ] Memory optimization
- [ ] Automatic chunking

### Sprint 19: Chunked Operations
- [ ] Define chunk sizes
- [ ] Iterate over chunks
- [ ] Parallel chunk processing
- [ ] Out-of-core algorithms

### Sprint 20: Parallel Computing
- [ ] OpenMP integration
- [ ] Thread-safe operations
- [ ] Load balancing
- [ ] Reduction operations

## Phase 6: Visualization (Sprint 21-22)

### Sprint 21: Fortplot Integration
- [ ] Variable plot methods
- [ ] Automatic axis labels
- [ ] Coordinate-aware plotting
- [ ] Dataset visualization

### Sprint 22: Plot Types
- [ ] Line plots (1D)
- [ ] Contour plots (2D)
- [ ] Surface plots (2D)
- [ ] Time series plots

## Phase 7: Time Series Support (Sprint 23-24)

### Sprint 23: Time Coordinates
- [ ] CF-compliant time handling
- [ ] Calendar support
- [ ] Time unit conversions
- [ ] Datetime parsing

### Sprint 24: Time Operations
- [ ] Time-based indexing
- [ ] Resampling methods
- [ ] Rolling windows
- [ ] Seasonal statistics

## Phase 8: Quality Assurance (Sprint 25-26)

### Sprint 25: Testing
- [ ] 100% public API coverage
- [ ] Integration test suite
- [ ] Performance benchmarks
- [ ] Memory leak checks

### Sprint 26: Optimization
- [ ] Profile critical paths
- [ ] SIMD optimizations
- [ ] Cache efficiency
- [ ] Parallel scaling

## Phase 9: Documentation (Sprint 27-28)

### Sprint 27: User Guide
- [ ] Getting started guide
- [ ] API reference
- [ ] Example gallery
- [ ] Migration guides

### Sprint 28: Developer Docs
- [ ] Architecture overview
- [ ] Contributing guide
- [ ] Performance guide
- [ ] Extension guide

## Acceptance Criteria
1. **Functionality**: Correct behavior for all cases
2. **Performance**: Meets benchmark targets
3. **Memory**: No leaks, efficient usage
4. **Testing**: Comprehensive coverage
5. **Documentation**: Complete and clear
6. **NetCDF Compliance**: Follows conventions

## Definition of Done
- [ ] Code compiles without warnings
- [ ] All tests pass with OMP_NUM_THREADS=24
- [ ] Memory leak check passes
- [ ] Code coverage > 95%
- [ ] Documentation complete
- [ ] Performance targets met
- [ ] Code reviewed