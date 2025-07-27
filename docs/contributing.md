# Contributing to Foxel

Thank you for your interest in contributing to Foxel! This guide will help you get started with development and ensure your contributions align with the project's standards.

## Development Principles

Foxel follows strict development principles that must be adhered to in all contributions:

### Core Principles

- **NO SHORTCUTS**: Every feature must be fully implemented with proper error handling
- **NO SIMPLIFICATIONS**: Full functionality as described, no "minimal viable" versions  
- **NO CHEATING**: Proper algorithms, no placeholder implementations
- **COMPLETE TESTING**: Every public API must have comprehensive tests
- **FULL DOCUMENTATION**: Every module, type, and procedure must be documented
- **NETCDF COMPLIANCE**: Follow NetCDF data model and CF conventions

### Code Quality Standards

- **TDD (Test-Driven Development)**: Write tests first, then implementation
- **SOLID Principles**: Single responsibility, open/closed, Liskov substitution, interface segregation, dependency inversion
- **KISS (Keep It Simple, Stupid)**: Prefer simple, clear solutions
- **DRY (Don't Repeat Yourself)**: Eliminate code duplication
- **SRP (Single Responsibility Principle)**: Each module has one clear purpose

## Getting Started

### Prerequisites

1. **Fortran Compiler**: Modern Fortran 2018 compiler (gfortran 9+, ifort 2021+)
2. **Build Tools**: Fortran Package Manager (fpm)
3. **Dependencies**: NetCDF-Fortran, HDF5, OpenMP
4. **Optional**: Valgrind for memory leak detection

### Setting Up Development Environment

```bash
# Clone the repository
git clone https://github.com/krystophny/foxel.git
cd foxel

# Install dependencies (Ubuntu/Debian)
sudo apt-get install gfortran libnetcdff-dev libhdf5-dev

# Install fpm
curl -fsSL https://raw.githubusercontent.com/fortran-lang/fpm/main/install.sh | sh

# Build the project
fpm build --profile release

# Run tests
export OMP_NUM_THREADS=24
fpm test
```

### Project Structure

```
foxel/
├── src/                    # Source code
│   ├── foxel_types.f90    # Core type definitions
│   ├── foxel_storage.f90  # Generic data storage
│   ├── foxel_netcdf.f90   # NetCDF I/O operations
│   ├── foxel_*.f90        # Feature modules
│   └── foxel.f90          # Main module interface
├── test/                   # Test suite
│   ├── test_*.f90         # Unit and integration tests
│   └── data/              # Test data files
├── docs/                   # Documentation
│   ├── getting_started.md # User documentation
│   ├── api_reference.md   # API documentation
│   ├── examples/          # Code examples
│   └── *.md               # Developer guides
├── fpm.toml               # Build configuration
├── BACKLOG.md             # Development backlog
└── CLAUDE.md              # Project guidelines
```

## Contribution Workflow

### 1. Issue Discussion

Before starting work:

1. **Check existing issues**: Search for related issues or feature requests
2. **Create new issue**: If none exists, create one describing your proposal
3. **Discuss approach**: Wait for maintainer feedback before implementation
4. **Get approval**: Ensure your approach aligns with project goals

### 2. Development Process

#### Branching Strategy

```bash
# Create feature branch from main
git checkout main
git pull origin main
git checkout -b feature/your-feature-name

# For bug fixes
git checkout -b bugfix/issue-number-description
```

#### Test-Driven Development

**ALWAYS write tests before implementation:**

```bash
# 1. Write failing test
echo 'program test_new_feature
    use foxel
    use test_framework
    implicit none
    
    ! Write test that defines expected behavior
    call test_new_feature_works()
contains
    subroutine test_new_feature_works()
        ! Test implementation
        call assert_true(.false., "Not implemented yet")
    end subroutine
end program' > test/test_new_feature.f90

# 2. Verify test fails
fpm test test_new_feature
# Should fail with "Not implemented yet"

# 3. Implement feature
# Edit src/foxel_*.f90 files

# 4. Verify test passes
fpm test test_new_feature
# Should pass
```

#### Implementation Guidelines

**Module Structure:**
```fortran
!> Module for [functionality description]
!> 
!> This module implements [detailed description following NetCDF conventions]
module foxel_new_feature
    use foxel_types, only: variable_t, dataset_t
    use iso_fortran_env, only: real64, int32
    implicit none
    private
    
    ! Public interfaces
    public :: new_feature_function
    public :: new_feature_type
    
    ! Type definitions
    type :: new_feature_type
        ! Member variables with clear documentation
    end type
    
contains
    
    !> Brief description of function
    !>
    !> Detailed description explaining:
    !> - Purpose and behavior
    !> - Input parameter meanings
    !> - Return value description
    !> - Error conditions
    !> - Usage examples
    !>
    !> @param input_var Input variable description
    !> @param optional_param Optional parameter (default: value)
    !> @returns Result description
    function new_feature_function(input_var, optional_param) result(output_var)
        type(variable_t), intent(in) :: input_var
        logical, intent(in), optional :: optional_param
        type(variable_t) :: output_var
        
        ! Implementation with error handling
        
    end function new_feature_function
    
end module foxel_new_feature
```

**Error Handling Pattern:**
```fortran
function safe_operation(input, stat) result(output)
    type(variable_t), intent(in) :: input
    integer, intent(out), optional :: stat
    type(variable_t) :: output
    
    integer :: local_stat
    
    ! Initialize status
    local_stat = SUCCESS
    
    ! Validate inputs
    if (.not. allocated(input%data%values_r64)) then
        local_stat = ERROR_INVALID_VARIABLE
        if (present(stat)) stat = local_stat
        return
    end if
    
    ! Perform operation with error checking
    ! ...
    
    if (present(stat)) stat = local_stat
end function
```

### 3. Testing Requirements

#### Test Coverage

**Every public procedure must have tests covering:**

1. **Normal operation**: Typical use cases
2. **Edge cases**: Boundary conditions (empty arrays, single elements, maximum sizes)
3. **Error conditions**: Invalid inputs, file errors, memory issues
4. **Performance**: Benchmarks for critical operations
5. **Memory**: No leaks, proper cleanup

#### Test Organization

```fortran
program test_comprehensive_feature
    use foxel
    use test_framework
    implicit none
    
    call test_normal_operation()
    call test_edge_cases()
    call test_error_conditions()
    call test_performance()
    call test_memory_management()
    
contains
    
    subroutine test_normal_operation()
        ! Test typical usage patterns
    end subroutine
    
    subroutine test_edge_cases()
        ! Test boundary conditions
        call test_empty_arrays()
        call test_single_elements()
        call test_maximum_sizes()
    end subroutine
    
    subroutine test_error_conditions()
        ! Test error handling
        call test_invalid_inputs()
        call test_file_errors()
    end subroutine
    
    subroutine test_performance()
        ! Benchmark critical operations
    end subroutine
    
    subroutine test_memory_management()
        ! Test for memory leaks
    end subroutine
    
end program
```

#### Running Tests

```bash
# Run all tests
export OMP_NUM_THREADS=24
fpm test

# Run specific test
fpm test test_new_feature

# Run with memory leak detection (if available)
valgrind --leak-check=full fpm test test_new_feature

# Performance testing
fpm test test_performance --profile release
```

### 4. Documentation

#### Code Documentation

**Every public interface must be documented:**

```fortran
!> Compute weighted mean of variable along specified dimension
!>
!> This function computes the weighted mean of a variable along the
!> specified dimension, following CF conventions for weighted statistics.
!> Missing values (NaN, fill_value) are automatically excluded.
!>
!> Example:
!> ```fortran
!> ! Compute area-weighted global mean temperature
!> area_weights = compute_area_weights(temp%coords(2)%values_r64, &  ! lat
!>                                   temp%coords(3)%values_r64)    ! lon
!> global_mean = weighted_mean(temp, area_weights, dim_name='spatial')
!> ```
!>
!> @param var Input variable with named dimensions
!> @param weights Weights array matching the dimension to reduce
!> @param dim_name Name of dimension to compute mean along
!> @param fill_value Value to use for missing data (optional)
!> @param stat Status code: SUCCESS=0, ERROR_*=positive values
!> @returns Variable with reduced dimensionality containing weighted means
!>
!> @note Weights are automatically normalized to sum to 1.0
!> @warning Input variable must have coordinate information for dim_name
function weighted_mean(var, weights, dim_name, fill_value, stat) result(mean_var)
```

#### API Documentation

Update `docs/api_reference.md` when adding new public interfaces.

#### Examples

Add practical examples to `docs/examples/` showing how to use new features.

### 5. Code Review Process

#### Before Submitting

**Run complete validation:**

```bash
# 1. Clean build
fpm clean
fpm build --profile release

# 2. All tests pass
export OMP_NUM_THREADS=24
fpm test

# 3. No compiler warnings
fpm build --flag "-Wall -Wextra -Werror"

# 4. Memory leak check (if available)
fpm test test_memory_leaks

# 5. Performance regression check
fpm test test_performance --profile release
```

#### Pull Request Guidelines

**Title Format:**
- `feat: Add [feature description]` for new features
- `fix: Resolve [issue description]` for bug fixes  
- `docs: Update [documentation section]` for documentation
- `refactor: Improve [component description]` for refactoring
- `test: Add tests for [feature/component]` for test additions

**Description Template:**
```markdown
## Summary
Brief description of changes and motivation.

## Changes
- [ ] Core implementation
- [ ] Comprehensive tests  
- [ ] Documentation updates
- [ ] Performance benchmarks

## Testing
- [ ] All existing tests pass
- [ ] New tests added for this feature
- [ ] Memory leak check passed
- [ ] Performance impact assessed

## Breaking Changes
List any breaking changes to existing APIs.

## Checklist
- [ ] Code follows project style guidelines
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] Tests added/updated
- [ ] No compiler warnings
- [ ] Memory leaks checked
```

## Coding Standards

### Fortran Style Guide

#### Naming Conventions

```fortran
! Module names: lowercase with underscores
module foxel_feature_name

! Type names: lowercase with _t suffix  
type :: variable_t

! Procedure names: lowercase with underscores
function compute_mean(var) result(mean_var)

! Variable names: descriptive, lowercase with underscores
integer :: n_elements
real(real64) :: temperature_data(100)

! Constants: uppercase with underscores
integer, parameter :: SUCCESS = 0
real(real64), parameter :: PI = 3.14159265358979323846_real64
```

#### Code Organization

```fortran
module foxel_example
    ! Use statements first
    use foxel_types, only: variable_t
    use iso_fortran_env, only: real64
    
    ! Implicit none always
    implicit none
    
    ! Private by default
    private
    
    ! Public interfaces
    public :: public_function
    public :: public_type
    
    ! Parameter definitions
    integer, parameter :: LOCAL_CONSTANT = 42
    
    ! Type definitions
    type :: example_type
        ! Type components
    end type
    
    ! Interface blocks (if needed)
    interface public_function
        module procedure :: specific_implementation
    end interface
    
contains
    
    ! Procedures in logical order
    
end module foxel_example
```

#### Memory Management

```fortran
! Always use allocatable for dynamic arrays
real(real64), allocatable :: data(:, :, :)

! Check allocation status
if (allocated(data)) deallocate(data)
allocate(data(nx, ny, nz))

! Implement finalizers for types
type :: managed_type
    real(real64), allocatable :: data(:)
contains
    final :: finalize_managed_type
end type

subroutine finalize_managed_type(obj)
    type(managed_type), intent(inout) :: obj
    if (allocated(obj%data)) deallocate(obj%data)
end subroutine
```

### Performance Guidelines

#### OpenMP Usage

```fortran
! Use OpenMP for large arrays (>10k elements)
!$OMP PARALLEL DO REDUCTION(+:sum_val) SCHEDULE(STATIC)
do i = 1, n_elements
    sum_val = sum_val + data(i)
end do
!$OMP END PARALLEL DO

! SIMD optimization for vectorizable loops
!$OMP SIMD ALIGNED(result_data, input_data: 64)
do i = 1, n
    result_data(i) = sqrt(input_data(i))
end do
```

#### Memory Optimization

```fortran
! Process in chunks for large datasets
integer, parameter :: CHUNK_SIZE = 10000

do chunk_start = 1, n_elements, CHUNK_SIZE
    chunk_end = min(chunk_start + CHUNK_SIZE - 1, n_elements)
    call process_chunk(data(chunk_start:chunk_end))
end do
```

## Common Pitfalls

### 1. Memory Management

**Don't:**
```fortran
! Memory leak - missing finalization
var = create_variable(data)
! var goes out of scope without cleanup
```

**Do:**
```fortran
! Proper cleanup
var = create_variable(data)
! ... use var ...
call finalize_variable(var)
```

### 2. Error Handling

**Don't:**
```fortran
! Silent failure
var = read_netcdf_variable("file.nc", "temp")
! No error checking
```

**Do:**
```fortran
! Explicit error handling
var = read_netcdf_variable("file.nc", "temp", stat=status)
if (status /= SUCCESS) then
    write(*,*) "Error reading variable"
    return
end if
```

### 3. Type Safety

**Don't:**
```fortran
! Unsafe type casting
real_data = transfer(int_data, real_data)
```

**Do:**
```fortran
! Explicit type conversion with validation
if (storage%data_type == TYPE_INT32) then
    real_data = real(storage%values_i32, real64)
else
    ! Handle error
end if
```

## Release Process

### Version Numbering

Foxel uses semantic versioning (MAJOR.MINOR.PATCH):

- **MAJOR**: Breaking API changes
- **MINOR**: New features, backward compatible
- **PATCH**: Bug fixes, backward compatible

### Pre-Release Checklist

- [ ] All tests pass with OMP_NUM_THREADS=24
- [ ] No memory leaks detected
- [ ] Performance benchmarks meet targets
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] Version numbers updated

### Release Notes

Include in release notes:

1. **New Features**: User-facing improvements
2. **Bug Fixes**: Issues resolved
3. **Performance**: Optimization improvements
4. **Breaking Changes**: API modifications requiring user action
5. **Migration Guide**: How to adapt existing code

## Support and Communication

### Getting Help

1. **Documentation**: Check existing docs first
2. **Issues**: Search existing GitHub issues
3. **Discussions**: Use GitHub Discussions for questions
4. **Examples**: Look at test files and examples

### Reporting Issues

Include in bug reports:

1. **Minimal example**: Reproducible test case
2. **Environment**: Compiler, OS, dependencies
3. **Expected behavior**: What should happen
4. **Actual behavior**: What actually happens
5. **Debug information**: Error messages, stack traces

### Feature Requests

Include in feature requests:

1. **Use case**: Why is this needed?
2. **Proposed API**: How should it work?
3. **Alternatives**: Other solutions considered
4. **NetCDF compliance**: How does it fit the data model?

## Recognition

Contributors will be recognized in:

- **AUTHORS**: All contributors listed
- **CHANGELOG**: Contributions noted in releases
- **Git history**: Proper attribution in commits
- **Documentation**: Examples and improvements credited

Thank you for contributing to Foxel! Your efforts help make scientific computing in Fortran more accessible and powerful.