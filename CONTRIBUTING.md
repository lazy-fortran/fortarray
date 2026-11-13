# Contributing

Preserve the [data model and invariants](docs/data-model.md) and keep changes
small. Behavior changes need an independent oracle rather than a check that
repeats the implementation. Keep generated documentation, data and binaries out
of source control.

Run the native builds, tests and documentation checks in
[the development guide](docs/development.md). Exercise both the core-only
CMake build and the Fortio-backed adapter when a change affects their boundary.
State the compiler, commands and any untested optional feature in the pull request.
