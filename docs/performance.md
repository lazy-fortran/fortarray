---
title: Thread safety and performance
---

# Thread safety and performance

Fortarray arrays own their storage and contain no global mutable state.
Independent arrays may be operated on concurrently. Do not mutate the same
array from multiple threads without application-level synchronization.

Large same-layout additions and reductions may use OpenMP. Small operations
stay serial to avoid launch overhead. Synchronization and I/O guarantees come
from Fortio when the optional adapter is used.

CI checks:

- independent behavioral results for selection, reduction, alignment, and I/O;
- OpenMP concurrent kernels under ThreadSanitizer;
- allocation-reusing destination routines;
- release-mode kernel performance thresholds;
- CMake builds with and without Fortio.
