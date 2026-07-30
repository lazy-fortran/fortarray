project: Fortarray
project_github: https://github.com/lazy-fortran/fortarray
project_website: https://lazy-fortran.github.io/fortarray/
summary: Dense labeled arrays for Fortran
author: Fortarray contributors
license: MIT
src_dir: src
page_dir: docs
output_dir: site
display: public
         protected
source: false
graph: false
search: false
print_creation_date: false
warn: false
hide_undoc: true

# Fortarray

Fortarray supplies dense labeled arrays for Fortran. Its user model follows
xarray, its contiguous column-major storage follows Fortran and xtensor, and
its operations use composable generic procedures rather than a monolithic
dataset object.

Start with the [xarray mapping](page/xarray-mapping.html) and
[data model](page/data-model.html). I/O is an optional adapter over
[Fortio](https://lazy-fortran.github.io/fortio/); plotting is composed with
Fortplot rather than embedded in the array type.
