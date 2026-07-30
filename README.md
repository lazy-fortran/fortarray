# fortarray

Fortarray provides dense, labeled arrays for modern Fortran. Its data model is
inspired by xarray, its storage rules by xtensor, and its composed generic
procedures by Julia.

The first implementation is intentionally limited to operations used by the
ITP plasma codes:

- dense column-major `real(real64)` arrays through rank four;
- named dimensions, one-dimensional dimension coordinates, and text metadata;
- positional and exact/nearest coordinate selection;
- dimension reductions;
- arithmetic aligned and broadcast by dimension name;
- collections of named arrays.

Fortarray owns labeled computation. Fortio owns physical file formats.
Fortplot owns rendering. Plotting and I/O are composed procedures rather than
capabilities attached to `data_array_t`.

CMake consumers that need only labeled computation can configure
`FORTARRAY_BUILD_IO=OFF`; that build has no Fortio dependency and exports only
`fortarray::core` plus the core-only `fortarray::fortarray` convenience target.
The fpm dependency remains package-wide because fpm does not provide
target-level dependency declarations.

```fortran
use fortarray, only: dp, data_array_t, data_array, mean, sel

type(data_array_t) :: field, surface
real(dp) :: values(64, 128, 16)

field = data_array(values, ["radius", "theta ", "zeta  "], name="potential")
surface = mean(sel(field, "radius", 0.5_dp, method="nearest"), "zeta")
```

Both allocating functions and destination-taking routines are provided for
hot paths:

```fortran
surface = mean(field, "zeta")
call mean_into(field, "zeta", surface)
```

The latter reuses `surface` storage when its size is already correct.
