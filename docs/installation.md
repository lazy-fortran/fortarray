---
title: Installation
---

# Installation

## fpm

```toml
[dependencies]
fortarray = { git = "https://github.com/lazy-fortran/fortarray.git", tag = "v0.1.0" }
```

The fpm package includes the Fortio-backed I/O adapter.

## CMake with I/O

```cmake
include(FetchContent)
FetchContent_Declare(
  fortarray
  GIT_REPOSITORY https://github.com/lazy-fortran/fortarray.git
  GIT_TAG v0.1.0
  GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(fortarray)
target_link_libraries(my_program PRIVATE fortarray::fortarray)
```

Link `fortarray::core` when only labeled computation is needed, or
`fortarray::io` for the Fortio adapter.

## Core-only CMake build

```sh
cmake -S . -B build \
  -DFORTARRAY_BUILD_IO=OFF \
  -DFORTARRAY_BUILD_TESTS=ON
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

The core-only build has no Fortio dependency. OpenMP is optional and controlled
by `FORTARRAY_ENABLE_OPENMP`.
