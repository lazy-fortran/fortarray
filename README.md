# FortArray

A modern Fortran library providing xarray-compatible labeled multi-dimensional arrays and datasets, following NetCDF data model conventions.

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
git clone https://github.com/krystophny/foxel.git
cd foxel
fpm build --release
```

## Quick Start

```fortran
program example
    use fortarray
    implicit none
    
    type(dataset_t) :: ds
    type(fortarray_t) :: temp, pres, temp_subset, temp_mean
    real(real64), allocatable :: temp_data(:,:)
    
    ! Create array with xarray-style constructors
    temp_data = reshape([20.0, 21.0, 22.0, 23.0, 24.0, 25.0], [3, 2])
    temp = new_array(temp_data, dim_names=["lat", "lon"])
    
    ! Select data by coordinate values (xarray-compatible)
    temp_subset = temp%sel_point("lat", 2.0)
    
    ! Compute statistics along dimensions
    temp_mean = temp%mean()
    
    ! Method chaining support
    pres = temp%sel_point("lat", 1.0)%mean()
    
    ! Create 3D variable from array  
    allocate(temp_data(360, 180, 24))
    call random_number(temp_data)
    
    temp = new_array(temp_data, &
                     dim_names=['lon', 'lat', 'time'], &
                     name='temperature')
    temp%units = 'kelvin'
    
    ! Add to dataset
    ds = new_dataset()
    call add_variable(ds, temp)
    
    ! Write to NetCDF
    call write_netcdf('output.nc', ds)
    
    ! Plot using fortplot
    call temp%plot(x='time')  ! Time series plot
    
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
type(fortarray_t) :: temp, precip
real(real64) :: global_mean

! Load ERA5 reanalysis data
climate = read_netcdf_dataset('era5_2023.nc')

! Extract variables using xarray-style access
temp = climate%get_variable('t2m')  ! 2-meter temperature
precip = climate%get_variable('tp')  ! Total precipitation

! Compute annual mean temperature
temp_annual = temp%mean(dim_name='time')

! Select a region using coordinate-based selection
europe = temp%sel_range('lat', 35.0_real64, 70.0_real64)%sel_range('lon', -10.0_real64, 40.0_real64)

! Compute area-weighted global mean
global_mean = temp%mean()%data%values_r64(1)

! Write subset to new file  
call write_netcdf('europe_temperature.nc', europe)
```

## Requirements

- Modern Fortran compiler (gfortran 9+, ifort 2021+)
- NetCDF-Fortran library
- HDF5 library (usually comes with NetCDF4)
- fortplot (installed automatically via fpm)
- OpenMP support (optional but recommended)
- fpm (Fortran Package Manager)

## Documentation

- **[Getting Started Guide](docs/getting_started.md)** - Learn the basics with examples
- **[API Reference](docs/api_reference.md)** - Complete function documentation  
- **[Example Gallery](docs/examples/)** - Practical usage examples
- **[Migration Guides](docs/migration_guides.md)** - Transition from CDO/NCL/xarray/MATLAB

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