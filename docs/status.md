---
title: Status and verification
---

# Status and verification

Fortarray supports dense `real(real64)` arrays through rank four, named
dimensions, one-dimensional coordinates and collections of named arrays.
It is an early interface and applications should test their own combinations
of operations and metadata.

The native tests check storage and metadata invariants, coordinate selection,
dimension alignment, arithmetic, reductions and concurrent calls with separate
objects. The core-only CMake build exercises these without an I/O dependency.
The optional Fortio adapter is checked against an independent file-format
oracle. The performance benchmark checks allocation reuse as well as values;
its timing is specific to the compiler and hardware used.

Fortarray owns in-memory labeled computation. File-format support belongs to
[Fortio](https://github.com/lazy-fortran/fortio), and rendering belongs to
[Fortplot](https://github.com/lazy-fortran/fortplot). Their optional integrations
need their own dependencies and checks.
