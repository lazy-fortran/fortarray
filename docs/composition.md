---
title: I/O and plotting composition
---

# I/O and plotting composition

Fortarray owns labeled computation. Fortio owns physical file formats.
Fortplot owns rendering.

```text
application
  ├─ fortarray_core  labeled arrays and operations
  ├─ fortarray_io    conversion between Fortio and labeled arrays
  └─ fortplot        rendering procedures accepting labeled arrays
```

This keeps `data_array_t` usable without file-format or graphics dependencies.
It also makes control explicit: the application chooses when to read, compute,
and render.

## NetCDF adapter

```fortran
use fortarray, only: data_array_t, FORTARRAY_SUCCESS
use fortarray_io, only: read_netcdf, write_netcdf

type(data_array_t) :: field
integer :: stat

field = read_netcdf("input.nc", "potential", stat)
if (stat /= FORTARRAY_SUCCESS) error stop "cannot read potential"
call write_netcdf("output.nc", field, stat)
if (stat /= FORTARRAY_SUCCESS) error stop "cannot write potential"
```

The adapter transfers named dimensions, dimension coordinates, the array name,
and text attributes. Fortarray does not expose NetCDF handles.

## Why plotting is not a method

An xarray-style `array%plot()` would make the array module depend on a plotting
policy or require a global backend registry. Fortarray instead supports
composition: Fortplot can provide `plot(array)` without changing the array
type or forcing non-plotting programs to carry graphics code.
