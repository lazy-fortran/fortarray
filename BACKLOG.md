# Foxel Development Backlog

## Development Principles
- **NO SHORTCUTS**: Every feature must be fully implemented with proper error handling
- **NO SIMPLIFICATIONS**: Full functionality as described, no "minimal viable" versions
- **NO CHEATING**: Proper algorithms, no placeholder implementations
- **COMPLETE TESTING**: Every public API must have comprehensive tests
- **FULL DOCUMENTATION**: Every module, type, and procedure must be documented
- **NETCDF COMPLIANCE**: Follow NetCDF data model and CF conventions

## Architecture Refactoring (IMMEDIATE PRIORITY)

### Sprint 0: Refactor to NetCDF Model ✓
- [x] Rename `dataframe_t` to `variable_t` throughout codebase
- [x] Update all references from "dataframe" to "variable"
- [x] Create `dataset_t` type for multi-variable collections
- [x] Update module names to reflect NetCDF terminology
- [x] Refactor tests to use new type names
- [x] Update all documentation

## Phase 1: Core Data Structures (Sprint 1-4)

### Sprint 1: Basic Type Definitions ✓
- [x] Define core types (now as variable_t with NetCDF semantics)
- [x] Refactor to variable_t with NetCDF semantics
- [x] Add dimension_t type for named dimensions
- [x] Add dataset_t type for variable collections
- [x] Update finalizers for new types
- [x] Refactor test suite for new type names

### Sprint 2: Generic Data Storage ✓ (COMPATIBLE)
- [x] Implement generic interfaces for data types
- [x] Create type-specific storage modules
- [x] Implement proper type conversion routines
- [x] Add bounds checking for all array operations
- [x] Test all type combinations exhaustively

### Sprint 3: Constructor Functions ✓
- [x] Implement `variable()` constructor
  - [x] Support 0D scalars through nD arrays (real64, real32, int32, int64)
  - [x] Validate dimension names (no duplicates, valid identifiers)
  - [x] Validate coordinate arrays (correct lengths)
  - [x] Handle optional parameters properly
- [x] Implement `dataset()` constructor
  - [x] Empty dataset constructor
  - [x] From variables constructor
- [x] Create convenience constructors
  - [x] From arrays with auto-generated coordinates
  - [x] From CSV files
  - [x] Empty variables with specified dimensions
  - [x] Scalar constructors
- [x] Test edge cases (scalars, empty data, large arrays)
- [x] Fix data type mapping between storage and types modules
- [x] Implement deep copy for coordinates
- [x] All constructor tests passing

### Sprint 4: Basic Indexing ✓
- [x] Implement positional indexing for variables
  - [x] 1D, 2D, 3D, and N-dimensional positional indexing
  - [x] Bounds checking and error handling
  - [x] Optional arguments for clean API
- [x] Implement label-based indexing using coordinates
  - [x] String label indexing
  - [x] Numeric label indexing
  - [x] Multi-dimensional label indexing
- [x] Support multi-dimensional indexing
  - [x] Generic N-dimensional support
  - [x] Linear index calculation for column-major order
- [x] Handle scalar variable indexing (0D)
  - [x] Scalar variable support verified
- [x] Create dataset variable selection methods
  - [x] get_variable, has_variable, add_variable, remove_variable
  - [x] list_variables, select_variables
  - [x] Comprehensive error handling
- [x] Test all indexing combinations
  - [x] All 15 indexing tests passing
  - [x] All 6 dataset tests passing

## Phase 2: I/O Operations (Sprint 5-8)

### Sprint 5: NetCDF4 Reader ✓
- [x] Implement complete NetCDF4 file reading
  - [x] Read all dimensions and their lengths
  - [x] Read all variables with proper types
  - [x] Handle coordinate variables specially
  - [x] Read all attributes (global and variable)
  - [ ] Support NetCDF4 groups (deferred to Phase 9)
- [x] Handle special cases
  - [x] Scalar variables (0D)
  - [x] Unlimited dimensions
  - [ ] String variables (partial - reads as char arrays)
  - [x] Missing values and fill values
- [x] Create dataset from NetCDF file
- [x] Selective variable loading
- [ ] Test with real NetCDF files (deferred to integration testing)

### Sprint 6: NetCDF4 Writer ✓
- [x] Implement complete NetCDF4 writing
  - [x] Define dimensions (including unlimited)
  - [x] Define variables with proper types
  - [x] Write coordinate variables
  - [x] Write all attributes with type safety
  - [x] Support compression and chunking
- [x] Dataset to NetCDF file conversion
- [x] Atomic writes (temp file + rename)
- [x] CF-convention compliance checking
- [x] Test round-trip fidelity

### Sprint 7: CSV I/O ✓
- [x] CSV to 2D variable conversion
- [x] Variable to CSV export (2D only)
- [x] Handle headers and data types
- [x] Support missing values
- [x] Test with real CSV files

### Sprint 8: Format Detection ✓
- [x] Implement `from_file()` with auto-detection
- [x] Support .nc, .nc4, .hdf5, .csv extensions
- [x] Content-based format detection
- [x] Comprehensive error messages

## Phase 3: Data Manipulation (Sprint 9-12)

### Sprint 9: Broadcasting Engine ✓
- [x] Implement dimension alignment
- [x] Support operations between variables
- [x] Handle scalar broadcasting
- [x] Optimize common patterns
- [x] Test all scenarios

### Sprint 10: Arithmetic Operations ✓
- [x] Variable arithmetic (+, -, *, /, **)
- [x] Scalar-variable operations
- [x] Type promotion rules
- [x] NaN/missing value handling
- [x] Operator overloading

### Sprint 11: Aggregation Functions ✓
- [x] Statistical functions on variables
  - [x] Mean, sum, min, max
  - [x] Standard deviation, variance
  - [x] Quantiles, median
- [x] Operations along dimensions
- [x] Weighted aggregations
- [x] Handle missing data properly

### Sprint 12: Missing Data ✓
- [x] Define fill value semantics
- [x] Implement where() for masking
- [x] fillna() with various methods
- [x] dropna() along dimensions
- [x] Interpolation for missing values

## Phase 4: Advanced Indexing (Sprint 13-16)

### Sprint 13: Coordinate-based Selection ✓
- [x] Select by coordinate values
- [x] Range selection (inclusive)
- [x] Nearest-neighbor selection
- [x] Multi-dimensional selection
- [x] Performance optimization

### Sprint 14: Boolean Indexing ✓
- [x] Create boolean masks
- [x] Apply masks to variables
- [x] Conditional selection
- [x] Multi-condition support

### Sprint 15: Slicing Operations ✓
- [x] Implement slice syntax
- [x] Support negative indices
- [x] Step values in slices
- [x] Preserve coordinates

### Sprint 16: Interpolation ✓
- [x] Linear interpolation
- [x] Nearest-neighbor
- [x] Higher-order methods
- [x] Extrapolation options

## Phase 5: Computation Engine (Sprint 17-20)

### Sprint 17: Apply Functions ✓
- [x] Apply along dimensions
- [x] User-defined functions
- [x] Vectorized operations
- [x] Result type inference

### Sprint 18: Lazy Evaluation ✓
- [x] Computation graph
- [x] Deferred execution
- [x] Memory optimization
- [x] Automatic chunking

### Sprint 19: Chunked Operations ✓
- [x] Define chunk sizes
- [x] Iterate over chunks
- [x] Parallel chunk processing
- [x] Out-of-core algorithms

### Sprint 20: Parallel Computing ✓
- [x] OpenMP integration
- [x] Thread-safe operations
- [x] Load balancing
- [x] Reduction operations

## Phase 6: Visualization (Sprint 21-22)

### Sprint 21: Fortplot Integration ✓
- [x] Variable plot methods
- [x] Automatic axis labels
- [x] Coordinate-aware plotting
- [x] Dataset visualization

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