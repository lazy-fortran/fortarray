---
title: Data model and invariants
---

# Data model and invariants

`data_array_t` owns a flat contiguous value buffer, shape, Fortran-order
strides, unique dimension names, optional dimension coordinates, text
attributes, and a name.

`array%valid()` is true only when:

- values, shape, strides, and dimensions are allocated;
- shape, stride, and dimension counts agree;
- dimensions are unique and extents are nonnegative;
- the product of the extents equals the value count.

The leftmost index is contiguous. A `shape=[nr, nt, nz]` array uses strides
`[1, nr, nr*nt]`, matching ordinary Fortran arrays.

`dataset_t` is a named collection of data arrays with dataset-level text
attributes. It owns data; it is not a file handle or plotting context.

## Metadata behavior

Selection and reduction preserve the array name and text attributes. Metadata
for the removed dimension is dropped; coordinates for remaining dimensions
are retained. Addition aligns dimensions by name. Shared dimensions must have
equal extents; a dimension present on only one operand is broadcast across
the result.
