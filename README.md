# Fortarray

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24223511.svg)](https://doi.org/10.5281/zenodo.24223511)

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
program example
    use fortarray, only: dp, data_array_t, data_array, mean, sel
    implicit none

    type(data_array_t) :: field, surface
    real(dp) :: values(64, 128, 16)
    integer :: i

    values = 1.0_dp
    field = data_array(values, ["radius", "theta ", "zeta  "], name="potential")
    call field%set_coord("radius", [(real(i - 1, dp)/63.0_dp, i=1, 64)])
    surface = mean(sel(field, "radius", 0.5_dp, method="nearest"), "zeta")
    print *, surface%shape, surface%values(1)
end program example
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

## Status and verification

The tests exercise storage and metadata invariants, selection, reductions,
dimension-aligned arithmetic and concurrent calls with separate objects. The optional I/O adapter has
an independent file-format oracle. See [status](docs/status.md) for scope and
[the module overview](docs/data-model.md#modules) for interfaces.

## Contributing, citation and licence

See [CONTRIBUTING.md](CONTRIBUTING.md), [CITATION.cff](CITATION.cff) and the
MIT [LICENSE](LICENSE).

## Citation

Cite fortarray through its Zenodo record,
[doi:10.5281/zenodo.24223511](https://doi.org/10.5281/zenodo.24223511), which
always resolves to the latest release; release 0.2.0 is
[doi:10.5281/zenodo.24223512](https://doi.org/10.5281/zenodo.24223512).
`CITATION.cff` holds the full citation metadata.
