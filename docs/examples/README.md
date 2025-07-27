# Foxel Example Gallery

This directory contains practical examples demonstrating Foxel's capabilities for scientific data analysis.

## Example Categories

### Basic Operations
- [Basic Variables](01_basic_variables.f90) - Creating and manipulating variables
- [Dataset Operations](02_dataset_operations.f90) - Working with datasets
- [File I/O](03_file_io.f90) - Reading and writing NetCDF files

### Data Analysis
- [Climate Data Analysis](04_climate_analysis.f90) - Analyzing temperature and precipitation data
- [Time Series Analysis](05_time_series.f90) - Working with temporal data
- [Statistical Operations](06_statistics.f90) - Computing statistics and aggregations

### Advanced Features
- [Parallel Processing](07_parallel_processing.f90) - Using OpenMP parallelization
- [Performance Optimization](08_optimization.f90) - SIMD and cache optimizations
- [Custom Functions](09_custom_functions.f90) - Applying user-defined functions

### Visualization
- [Basic Plotting](10_basic_plotting.f90) - Creating plots with fortplot
- [Advanced Visualization](11_advanced_plots.f90) - Contours, surfaces, and subplots

### Real-World Applications
- [Weather Data Processing](12_weather_processing.f90) - Processing meteorological data
- [Ocean Data Analysis](13_ocean_analysis.f90) - Analyzing oceanographic datasets
- [Atmospheric Modeling](14_atmospheric_modeling.f90) - Working with model output

## Running Examples

Each example is a complete Fortran program. To run:

```bash
# Compile and run with fpm
fpm run example_name

# Or compile manually
gfortran -I/path/to/foxel/include example.f90 -L/path/to/foxel/lib -lfoxel -lnetcdff -o example
./example
```

## Data Files

Some examples use sample data files located in the `data/` subdirectory. These demonstrate real-world usage patterns with actual scientific datasets.

## Contributing Examples

To contribute new examples:

1. Follow the naming convention: `NN_descriptive_name.f90`
2. Include comprehensive comments explaining each step
3. Add error handling and memory cleanup
4. Provide sample output or expected results
5. Update this README with a brief description