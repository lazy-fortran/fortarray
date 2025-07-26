# Foxel

A modern Fortran library for handling multi-dimensional labeled arrays and datasets, inspired by Python's xarray and pandas.

## Features

- **Labeled dimensions and coordinates** - Work with meaningful names instead of array indices
- **NetCDF4 support** - Native reading and writing of NetCDF files
- **Intuitive API** - Familiar operations for users of xarray/pandas
- **High performance** - Leverages Fortran's computational efficiency
- **Flexible indexing** - Label-based, positional, and boolean indexing
- **Built-in computations** - Aggregations, reductions, and transformations
- **Missing data handling** - Proper NaN support and propagation
- **Parallel operations** - OpenMP support for computationally intensive tasks
- **Visualization** - Built-in plotting capabilities via fortplot integration

## Installation

```bash
git clone https://github.com/yourusername/foxel.git
cd foxel
fpm build --release
```

## Quick Start

```fortran
program example
    use foxel
    implicit none
    
    type(dataframe_t) :: df
    real, allocatable :: values(:,:)
    
    ! Create a DataFrame from array
    allocate(values(100, 50))
    call random_number(values)
    
    df = dataframe(values, &
                   dims=['time', 'station'], &
                   coords=[time_coords, station_coords])
    
    ! Write to file (auto-detects format from extension)
    call df%to_file('output.nc')
    
    ! Read from file (auto-detects format from extension)
    df = from_file('input.nc')
    
    ! Or specify format explicitly
    call df%to_file('output.dat', format='csv')
    df = from_file('input.dat', format='csv')
    
    ! Filter data by label
    df_subset = df%filter(time='2024-01-01', station='A001')
    
    ! Select specific variables
    df_vars = df%vars(['temperature', 'pressure'])
    
    ! Compute mean along dimension
    df_mean = df%mean(dim='time')
    
    ! Plot the data
    call df%plot(x='time', y='station')
    
end program example
```

## Core Concepts

### DataFrame
The central data structure in Foxel, containing:
- **Data array**: The actual numerical values
- **Dimensions**: Named axes (e.g., 'time', 'lat', 'lon')
- **Coordinates**: Label arrays for each dimension
- **Attributes**: Metadata (units, descriptions, etc.)

### Operations
- **Selection**: Extract subsets using labels or indices
- **Aggregation**: Compute statistics along dimensions
- **Arithmetic**: Element-wise and broadcasting operations
- **I/O**: Read/write various formats (NetCDF, CSV)
- **Visualization**: Plot DataFrames directly using fortplot

## Requirements

- Modern Fortran compiler (gfortran 9+, ifort 2021+)
- NetCDF-Fortran library
- fortplot (installed automatically via fpm)
- OpenMP support (optional)
- fpm (Fortran Package Manager)

## Documentation

Detailed documentation and API reference available at: [coming soon]

## Contributing

Contributions are welcome! Please read our contributing guidelines and submit pull requests to our repository.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

Inspired by the excellent Python libraries:
- [xarray](https://xarray.pydata.org/)
- [pandas](https://pandas.pydata.org/)

## Status

This project is under active development. See [ROADMAP.md](ROADMAP.md) for planned features.