---
title: Operations and allocation
---

# Operations and allocation

The same core operations are available as free generic procedures and, where
useful, type-bound calls:

```fortran
surface = mean(field, "zeta")
surface = field%mean("zeta")
point = isel(field, "radius", 4)
point = sel(field, "radius", 0.5_dp, method="nearest")
sum = left + right
```

Allocating functions favor clarity. Destination-taking procedures let hot
loops retain storage:

```fortran
call mean_into(field, "zeta", surface, stat)
call add_into(left, right, sum, stat)
```

`mean_into` and `add_into` reuse the destination value allocation when its
size and labeled layout already match. They do not allocate inside the
elementwise hot loop.

Status values are:

| Constant | Meaning |
|---|---|
| `FORTARRAY_SUCCESS` | Operation completed |
| `FORTARRAY_EINVAL` | Invalid argument or array |
| `FORTARRAY_ENOTFOUND` | Dimension, coordinate, or variable was absent |
| `FORTARRAY_ESHAPE` | Labeled shapes could not be aligned |
