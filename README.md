# Foxel

A modern Fortran library for handling multi-dimensional labeled arrays and datasets, following NetCDF data model conventions.

## Features

- **NetCDF data model** - Variables, dimensions, coordinates, and attributes
- **Multi-dimensional arrays** - Support for 0D scalars through nD arrays
- **Dataset collections** - Group multiple variables with shared dimensions
- **NetCDF4/HDF5 I/O** - Native support for scientific data formats
- **CF conventions** - Climate and Forecast metadata standards
- **Label-based indexing** - Work with coordinate values, not just indices
- **High performance** - Optimized Fortran implementations
- **Parallel operations** - OpenMP support for large datasets
- **Missing data handling** - Proper fill values and NaN support
- **Visualization** - Integrated plotting via fortplot

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
    
    type(dataset_t) :: ds
    type(variable_t) :: temp, pres
    real(real64), allocatable :: temp_data(:,:,:)
    
    ! Read a NetCDF file
    ds = from_netcdf('weather_data.nc')
    
    ! Access variables
    temp = ds%var('temperature')
    pres = ds%var('pressure')
    
    ! Select data by coordinate values
    temp_subset = temp%sel(time='2024-01-01', lat=45.0:55.0)
    
    ! Compute statistics along dimensions
    temp_mean = temp%mean(dim='time')
    
    ! Create new variable from array
    allocate(temp_data(360, 180, 24))
    call random_number(temp_data)
    
    temp = variable(temp_data, &
                   dims=['lon', 'lat', 'time'], &
                   name='temperature', &
                   units='kelvin')
    
    ! Add to dataset
    call ds%add_var(temp)
    
    ! Write to NetCDF
    call ds%to_netcdf('output.nc')
    
    ! Plot using fortplot
    call temp%plot(lon=0.0, lat=45.0)  ! Time series at point
    
end program example
```

## Core Types

### Variable
A named multi-dimensional array (NetCDF variable):
- **Data array**: The actual values (0D to nD)
- **Dimensions**: Named axes with sizes
- **Coordinates**: Optional 1D arrays defining axis values
- **Attributes**: Metadata (units, long_name, etc.)

### Dataset
A collection of variables (NetCDF file):
- **Variables**: Named collection of arrays
- **Dimensions**: Shared dimension definitions
- **Coordinates**: Shared coordinate variables
- **Attributes**: Global metadata

### Operations
- **Selection**: Extract data using coordinates or indices
- **Slicing**: Multi-dimensional array subsets
- **Aggregation**: Statistical operations along dimensions
- **Arithmetic**: Operations between variables
- **I/O**: Read/write NetCDF4, HDF5, CSV formats
- **Visualization**: Direct plotting of variables

## NetCDF Example

```fortran
! Working with real climate data
type(dataset_t) :: climate
type(variable_t) :: temp, precip
real(real64) :: global_mean

! Load ERA5 reanalysis data
climate = from_netcdf('era5_2023.nc')

! Extract variables
temp = climate%var('t2m')  ! 2-meter temperature
precip = climate%var('tp')  ! Total precipitation

! Compute annual mean temperature
temp_annual = temp%mean(dim='time')

! Select a region
europe = temp%sel(lat=35.0:70.0, lon=-10.0:40.0)

! Compute area-weighted global mean
global_mean = temp%weighted_mean(weights=area_weights)

! Write subset to new file
call europe%to_netcdf('europe_temperature.nc')
```

## Requirements

- Modern Fortran compiler (gfortran 9+, ifort 2021+)
- NetCDF-Fortran library
- HDF5 library (usually comes with NetCDF4)
- fortplot (installed automatically via fpm)
- OpenMP support (optional but recommended)
- fpm (Fortran Package Manager)

## Documentation

- [User Guide](docs/user_guide.md) - Getting started and examples
- [API Reference](docs/api_reference.md) - Complete API documentation
- [NetCDF Primer](docs/netcdf_primer.md) - Understanding the data model
- [Migration Guide](docs/migration.md) - Coming from xarray/CDO/NCL

## Testing

Run the test suite with:
```bash
export OMP_NUM_THREADS=24
fpm test
```

## Contributing

Contributions are welcome! Please:
1. Follow the coding standards in [CLAUDE.md](CLAUDE.md)
2. Add tests for new features
3. Update documentation
4. Submit pull requests

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Follows [NetCDF](https://www.unidata.ucar.edu/software/netcdf/) data model
- Complies with [CF Conventions](https://cfconventions.org/)
- Inspired by [xarray](https://xarray.pydata.org/) API design

## Status

This project is under active development. See [ROADMAP.md](ROADMAP.md) for planned features and [BACKLOG.md](BACKLOG.md) for detailed task tracking.