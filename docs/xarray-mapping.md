---
title: From xarray to Fortarray
---

# From xarray to Fortarray

Fortarray preserves xarray's useful ideas—named dimensions, dimension
coordinates, labeled selection, alignment, and datasets—without copying
Python's dynamic dispatch or attaching every ecosystem feature to one object.

| xarray | Fortarray |
|---|---|
| `xr.DataArray(data, dims=..., name=...)` | `data_array(data, dims, name)` |
| `a.sel(radius=0.5, method="nearest")` | `sel(a, "radius", 0.5_dp, method="nearest")` |
| `a.isel(radius=3)` | `isel(a, "radius", 4)` |
| `a.mean("theta")` | `mean(a, "theta")` |
| `a + b` | `a + b` |
| `Dataset({"a": a})` | `call dataset%set("a", a)` |
| `xr.open_dataset(...)` | `read_netcdf(...)` from `fortarray_io` |
| `a.plot()` | a Fortplot procedure that accepts a Fortarray value |

Fortran indices are one-based. Exact coordinate selection is the default;
`method="nearest"` selects the closest stored coordinate.

```fortran
use fortarray, only: dp, data_array_t, data_array, mean, sel

type(data_array_t) :: field, surface
real(dp) :: values(64, 128, 16)

field = data_array(values, ["radius", "theta ", "zeta  "], name="potential")
call field%set_coord("radius", [(real(i - 1, dp)/63.0_dp, i=1, 64)])
surface = mean(sel(field, "radius", 0.5_dp, method="nearest"), "zeta")
```

Unlike xarray, v0.1.0 is deliberately static:

- payloads are dense `real(real64)` arrays of rank zero through four;
- dimension coordinates are one-dimensional `real(real64)` arrays;
- attributes are text;
- there is no lazy task graph, Dask backend, plotting method, or dynamic
  backend registry.
