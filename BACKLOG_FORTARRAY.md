# FortArray Development Backlog

## Development Principles
- **NO SHORTCUTS**: Every feature must be fully implemented with proper error handling
- **NO SIMPLIFICATIONS**: Full functionality as described, no "minimal viable" versions
- **NO CHEATING**: Proper algorithms, no placeholder implementations
- **COMPLETE TESTING**: Every public API must have comprehensive tests
- **FULL DOCUMENTATION**: Every module, type, and procedure must be documented
- **NETCDF COMPLIANCE**: Follow NetCDF data model and CF conventions
- **XARRAY COMPATIBILITY**: Achieve 85%+ API similarity to xarray while maintaining Fortran performance

## Architecture Transformation (IMMEDIATE PRIORITY)

### Sprint 0: Global Rename Foxel → FortArray ✓
- [x] Rename all modules from `foxel_*` to `fortarray_*`
- [x] Rename `variable_t` to `fortarray_t` throughout codebase
- [x] Update all references from "foxel" to "fortarray" (case insensitive)
- [x] Update documentation and README
- [x] Update repository name and URLs
- [x] Update all import statements and module names

## Phase 1: Core API Harmonization (Sprint 1-6)

### Sprint 1: Type System Redesign ✓
- [x] Rename `variable_t` → `fortarray_t` as central type
- [x] Add backward compatibility via `type, extends(fortarray_t) :: variable_t`
- [x] Update all constructor interfaces
- [x] Add xarray-compatible method signatures to `fortarray_t`
- [x] Test type compatibility and inheritance

### Sprint 2: Constructor Harmonization ✓
- [x] Create `new_array()` constructor interface (avoid case conflict)
- [x] Create `new_dataset()` constructor interface
- [x] Implement positional argument constructors (no keyword args in Fortran)
- [x] Add overloaded constructors for different data types
- [x] Test all constructor combinations

### Sprint 3: Method Name Mapping ✓
- [x] Add xarray-compatible method names to `fortarray_t`:
  - [x] Selection: `sel_point`, `sel_range`, `sel_indices`
  - [x] Index selection: `isel_point`, `isel_range`, `isel_indices`
  - [x] Filtering: `filter` (avoid `where` keyword)
  - [x] Aggregation: `mean`, `sum`, `std`, `var`, `min`, `max`
  - [x] Data access: `values` (avoid `data` keyword)
- [x] Implement method forwarding from old names
- [x] Test all method aliases

### Sprint 4: Selection Syntax Enhancement ✓
- [x] Implement coordinate-based selection (`sel_*` methods)
- [x] Implement index-based selection (`isel_*` methods)
- [x] Add method selection with optional arguments
- [x] Create `slice_t` type for range operations
- [x] Avoid `class(*)` polymorphism for performance
- [x] Test all selection patterns

### Sprint 5: Method Chaining Support ✓
- [x] Ensure all methods return `fortarray_t` for chaining
- [x] Test complex chaining: `temp%sel_point()%mean()%filter()`
- [x] Optimize memory management in chained operations
- [x] Add performance tests for chaining
- [x] Document chaining patterns

### Sprint 6: Backward Compatibility Layer ✓
- [x] Maintain all existing Foxel APIs
- [x] Add deprecation warnings for old method names
- [x] Create migration documentation
- [x] Test dual API support
- [x] Performance comparison old vs new API

## Phase 2: Advanced Selection and Aggregation (Sprint 7-12)

### Sprint 7: Enhanced Selection Methods
- [ ] Implement nearest-neighbor selection
- [ ] Add interpolation-based selection
- [ ] Support string/datetime coordinate selection
- [ ] Implement multi-dimensional selection
- [ ] Add selection validation and error handling
- [ ] Performance optimization for large selections

### Sprint 8: Index Selection Enhancements
- [ ] Support negative indexing (Python-style)
- [ ] Implement fancy indexing with arrays
- [ ] Add step-based indexing
- [ ] Boolean mask selection
- [ ] Optimize index calculation algorithms
- [ ] Memory-efficient index operations

### Sprint 9: Conditional Selection (filter methods)
- [ ] Implement boolean mask creation
- [ ] Add condition chaining (AND, OR, NOT)
- [ ] Support multiple conditions
- [ ] Implement `fillna` style replacement
- [ ] Optimize condition evaluation
- [ ] Add vectorized condition operations

### Sprint 10: Advanced Aggregation Functions
- [ ] Add quantile and percentile functions
- [ ] Implement weighted aggregations
- [ ] Add cumulative operations (cumsum, cumprod)
- [ ] Rolling window aggregations
- [ ] Multi-dimensional aggregations
- [ ] Statistical significance tests

### Sprint 11: Dimension Manipulation
- [ ] Implement `transpose` method
- [ ] Add `stack` and `unstack` operations
- [ ] Implement `squeeze` and `expand_dims`
- [ ] Support dimension reordering
- [ ] Add dimension renaming
- [ ] Optimize dimension operations

### Sprint 12: Missing Data Advanced Handling
- [ ] Implement `interpolate_na` method
- [ ] Add forward fill (`ffill`) and backward fill (`bfill`)
- [ ] Support different interpolation methods
- [ ] Add `dropna` with axis support
- [ ] Implement advanced filling strategies
- [ ] Handle NaN propagation correctly

## Phase 3: Groupby and Resampling (Sprint 13-16)

### Sprint 13: Basic Groupby Implementation
- [ ] Create `groupby_t` type
- [ ] Implement `groupby()` method on `fortarray_t`
- [ ] Add coordinate-based grouping
- [ ] Support groupby aggregations (mean, sum, std)
- [ ] Implement group iteration
- [ ] Add group validation and error handling

### Sprint 14: Time-based Groupby
- [ ] Implement time component extraction (year, month, season)
- [ ] Add `groupby('time.month')` functionality
- [ ] Support seasonal grouping
- [ ] Add custom time period grouping
- [ ] Implement time zone handling
- [ ] Calendar-aware grouping operations

### Sprint 15: Binning and Advanced Groupby
- [ ] Implement `groupby_bins` functionality
- [ ] Add histogram-style binning
- [ ] Support custom binning functions
- [ ] Add quantile-based binning
- [ ] Multi-variable grouping
- [ ] Optimize groupby performance

### Sprint 16: Resampling Methods
- [ ] Implement `resample()` method
- [ ] Add time frequency conversion
- [ ] Support upsampling and downsampling
- [ ] Add resampling aggregations
- [ ] Implement time series alignment
- [ ] Add resampling interpolation

## Phase 4: I/O and Interoperability (Sprint 17-20)

### Sprint 17: Enhanced NetCDF I/O
- [ ] Update I/O to use `fortarray_t` types
- [ ] Add xarray-compatible I/O methods
- [ ] Implement `open_dataset()` function
- [ ] Add `open_dataarray()` function
- [ ] Support chunked reading/writing
- [ ] Add compression options

### Sprint 18: Multiple File I/O
- [ ] Implement `open_mfdataset()` for multiple files
- [ ] Add concatenation along dimensions
- [ ] Support parallel file reading
- [ ] Add file pattern matching
- [ ] Implement lazy loading for large datasets
- [ ] Add memory usage optimization

### Sprint 19: Additional Format Support
- [ ] Enhance HDF5 support
- [ ] Add Zarr format support (future)
- [ ] Improve CSV I/O with metadata
- [ ] Add binary format support
- [ ] Implement format auto-detection
- [ ] Add format conversion utilities

### Sprint 20: Pandas-style Interoperability
- [ ] Implement `to_pandas()` method
- [ ] Add pandas-compatible export formats
- [ ] Create data exchange utilities
- [ ] Add CSV export with proper headers
- [ ] Implement table-style operations
- [ ] Add data frame compatibility layer

## Phase 5: Plotting and Visualization (Sprint 21-24)

### Sprint 21: Plot Accessor Implementation
- [ ] Create `plot_accessor_t` type
- [ ] Add `plot` component to `fortarray_t`
- [ ] Implement basic plotting methods
- [ ] Add automatic plot configuration
- [ ] Support different backends (fortplot)
- [ ] Add plot customization options

### Sprint 22: Advanced Plot Types
- [ ] Implement line plots (`plot%line()`)
- [ ] Add contour plots (`plot%contour()`)
- [ ] Support surface plots (`plot%surface()`)
- [ ] Add histogram plotting
- [ ] Implement scatter plots
- [ ] Add statistical plots (box, violin)

### Sprint 23: Automatic Plot Configuration
- [ ] Auto-detect plot types from data dimensions
- [ ] Add intelligent axis labeling
- [ ] Support coordinate-aware plotting
- [ ] Implement automatic colormaps
- [ ] Add legend generation
- [ ] Support multiple subplot layouts

### Sprint 24: Interactive Plotting
- [ ] Add plot saving capabilities
- [ ] Implement plot animation for time series
- [ ] Support interactive features
- [ ] Add plot styling options
- [ ] Implement plot templates
- [ ] Add export to different formats

## Phase 6: Performance and Optimization (Sprint 25-28)

### Sprint 25: Method Chaining Optimization
- [ ] Optimize memory usage in chained operations
- [ ] Implement copy-on-write semantics
- [ ] Add lazy evaluation for complex chains
- [ ] Optimize temporary object creation
- [ ] Add memory pooling for chains
- [ ] Performance benchmarking

### Sprint 26: Selection Performance
- [ ] Optimize coordinate lookup algorithms
- [ ] Add indexing caches
- [ ] Implement parallel selection
- [ ] Optimize memory layout for selections
- [ ] Add SIMD optimization for conditions
- [ ] Benchmark against xarray performance

### Sprint 27: Aggregation Performance
- [ ] Optimize parallel aggregations
- [ ] Add SIMD vectorization
- [ ] Implement cache-friendly algorithms
- [ ] Add chunked aggregation support
- [ ] Optimize memory bandwidth usage
- [ ] Add specialized algorithms for common cases

### Sprint 28: I/O Performance
- [ ] Optimize NetCDF reading/writing
- [ ] Add parallel I/O support
- [ ] Implement memory mapping
- [ ] Add compression optimization
- [ ] Support streaming I/O
- [ ] Add I/O performance monitoring

## Phase 7: Testing and Quality Assurance (Sprint 29-32)

### Sprint 29: xarray Compatibility Testing
- [ ] Create comprehensive xarray comparison tests
- [ ] Test API compatibility across all methods
- [ ] Add numerical accuracy verification
- [ ] Test error handling compatibility
- [ ] Add performance comparison benchmarks
- [ ] Create migration test suite

### Sprint 30: Edge Case Testing
- [ ] Test boundary conditions
- [ ] Add stress tests for large datasets
- [ ] Test memory limit scenarios
- [ ] Add error recovery testing
- [ ] Test concurrent access
- [ ] Add numerical stability tests

### Sprint 31: Performance Regression Testing
- [ ] Create automated performance benchmarks
- [ ] Add memory usage monitoring
- [ ] Test scaling behavior
- [ ] Add compilation time monitoring
- [ ] Create performance CI pipeline
- [ ] Add performance regression alerts

### Sprint 32: Integration Testing
- [ ] Test with real scientific workflows
- [ ] Add end-to-end scenario testing
- [ ] Test interoperability with other libraries
- [ ] Add long-running stability tests
- [ ] Test different compiler compatibility
- [ ] Add platform compatibility testing

## Phase 8: Documentation and Migration (Sprint 33-36)

### Sprint 33: xarray Migration Guide
- [ ] Create comprehensive xarray → FortArray guide
- [ ] Add side-by-side API comparisons
- [ ] Document syntax differences
- [ ] Add migration examples
- [ ] Create automated migration tools
- [ ] Add migration best practices

### Sprint 34: Enhanced API Documentation
- [ ] Update all API documentation for new methods
- [ ] Add xarray-style examples
- [ ] Document method chaining patterns
- [ ] Add performance guidance
- [ ] Create interactive documentation
- [ ] Add API reference search

### Sprint 35: Tutorial and Examples
- [ ] Create getting started tutorial
- [ ] Add comprehensive example gallery
- [ ] Document common workflows
- [ ] Add performance optimization guide
- [ ] Create video tutorials
- [ ] Add community examples

### Sprint 36: User Migration Support
- [ ] Create migration assistance tools
- [ ] Add compatibility checking utilities
- [ ] Implement automated code conversion
- [ ] Add deprecation management
- [ ] Create community support resources
- [ ] Add FAQ and troubleshooting

## Acceptance Criteria
1. **xarray Compatibility**: 85%+ API similarity achieved
2. **Performance**: Maintains or improves Fortran performance advantages
3. **Backward Compatibility**: All existing Foxel code continues to work
4. **Testing**: Comprehensive test coverage including xarray comparisons
5. **Documentation**: Complete migration guides and API documentation
6. **User Experience**: xarray users can adapt within 1-2 days

## Definition of Done
- [ ] All new APIs implemented with full functionality
- [ ] Code compiles without warnings
- [ ] All tests pass with OMP_NUM_THREADS=24
- [ ] Memory leak check passes
- [ ] xarray compatibility tests pass
- [ ] Performance benchmarks meet targets
- [ ] Documentation complete with examples
- [ ] Migration guide tested with real users
- [ ] Code reviewed and approved

## Success Metrics
1. **API Coverage**: 90%+ of common xarray operations supported
2. **Syntax Similarity**: Method names and signatures match xarray patterns
3. **Performance**: 5-10x speedup over xarray for computational operations
4. **Adoption**: Smooth migration path for existing xarray users
5. **Maintainability**: Clean, well-documented, tested codebase