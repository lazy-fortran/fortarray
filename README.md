# Fortarray

Fortarray provides dense labeled arrays for modern Fortran. Its data model is
inspired by xarray, its contiguous storage by Fortran and xtensor, and its
composable generic procedures by Julia.

Version 0.1.0 supports:

- dense `real(real64)` arrays through rank four;
- unique named dimensions and one-dimensional dimension coordinates;
- positional and exact or nearest coordinate selection;
- reductions and arithmetic aligned by dimension name;
- allocation-reusing destination routines for hot paths;
- collections of named arrays;
- optional Fortio-backed NetCDF I/O.

```fortran
use fortarray, only: dp, data_array_t, data_array, mean, sel

type(data_array_t) :: field, surface
real(dp) :: values(64, 128, 16)
integer :: i

field = data_array(values, ["radius", "theta ", "zeta  "], name="potential")
call field%set_coord("radius", [(real(i - 1, dp)/63.0_dp, i=1, 64)])
surface = mean(sel(field, "radius", 0.5_dp, method="nearest"), "zeta")
```

Fortarray owns labeled computation. [Fortio](https://lazy-fortran.github.io/fortio/)
owns physical formats, and Fortplot owns rendering. I/O and plotting are
composed procedures rather than methods attached to `data_array_t`.

Documentation: <https://lazy-fortran.github.io/fortarray/>

- [Translate xarray concepts to Fortarray](docs/xarray-mapping.md)
- [Understand storage and metadata invariants](docs/data-model.md)
- [Use allocation-aware operations](docs/operations.md)
- [Compose I/O and plotting](docs/composition.md)
- [Install with fpm or CMake](docs/installation.md)
