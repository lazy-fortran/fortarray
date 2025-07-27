# Foxel Performance Guide

This guide covers performance optimization strategies, benchmarking, and best practices for achieving optimal performance with Foxel.

## Performance Overview

Foxel is designed for high-performance scientific computing with these key optimizations:

- **SIMD Vectorization**: Automatic vectorization for arithmetic operations
- **OpenMP Parallelization**: Multi-threaded operations on large datasets
- **Cache Optimization**: Memory layout and chunking for cache efficiency
- **Lazy Evaluation**: Deferred computation to minimize memory usage
- **Optimized Algorithms**: Efficient implementations of statistical operations

## Performance Targets

### Computational Performance

| Operation | Target Performance | Scaling |
|-----------|------------------|---------|
| Element-wise arithmetic | >10 GB/s memory bandwidth | Linear with threads |
| Aggregation (sum, mean) | >5 GB/s memory bandwidth | Near-linear with threads |
| Matrix operations | >50% peak FLOPS | Good thread scaling |
| I/O operations | >500 MB/s (SSD) | Limited by storage |
| Memory allocation | <1ms for 100MB | Constant time |

### Memory Efficiency

| Metric | Target | Notes |
|--------|--------|-------|
| Memory overhead | <5% of data size | Type metadata and coordinates |
| Memory alignment | 64-byte aligned | For SIMD operations |
| Memory fragmentation | <1% waste | Contiguous allocations |
| Cache hit rate | >95% (L1), >85% (L2) | For sequential operations |

## Optimization Strategies

### 1. Compiler Optimization

#### Recommended Compiler Flags

**GFortran:**
```bash
# Debug build
fpm build --flag "-g -Wall -Wextra -fbounds-check"

# Release build  
fpm build --profile release --flag "-O3 -march=native -funroll-loops -ffast-math"

# High-performance build
fpm build --profile release --flag "-O3 -march=native -mtune=native -funroll-loops -ffast-math -fopenmp -ftree-vectorize"
```

**Intel Fortran (ifort):**
```bash
# Release build
fpm build --profile release --flag "-O3 -xHost -qopenmp -fast"

# High-performance build
fpm build --profile release --flag "-O3 -xHost -qopenmp -fast -qopt-report=5"
```

#### Profile-Guided Optimization (PGO)

```bash
# 1. Build with profiling
fmp build --profile release --flag "-O3 -march=native -fprofile-generate"

# 2. Run representative workload
./build/gfortran_*/app/foxel_benchmark

# 3. Rebuild with profile data  
fpm build --profile release --flag "-O3 -march=native -fprofile-use"
```

### 2. OpenMP Configuration

#### Thread Count Optimization

```bash
# Test different thread counts
for threads in 1 2 4 8 16 24; do
    echo "Testing with $threads threads"
    export OMP_NUM_THREADS=$threads
    fpm run benchmark --profile release
done
```

#### OpenMP Environment Variables

```bash
# Optimal settings for most workloads
export OMP_NUM_THREADS=24              # Match physical cores
export OMP_PROC_BIND=true              # Bind threads to cores
export OMP_PLACES=cores                # One thread per core
export OMP_SCHEDULE="static,1000"      # Static scheduling with chunk size
export OMP_NESTED=false               # Disable nested parallelism
```

#### NUMA Optimization

```bash
# Check NUMA topology
numactl --hardware

# Run with NUMA binding
numactl --cpunodebind=0 --membind=0 fpm run app --profile release
```

### 3. Memory Optimization

#### Chunked Processing

For datasets larger than available memory:

```fortran
program chunked_processing
    use foxel
    implicit none
    
    type(variable_t) :: large_var, chunk_var, result_var
    integer, parameter :: CHUNK_SIZE = 1000000  ! 1M elements
    integer :: i, n_chunks
    real(real64) :: chunk_result, total_result
    
    ! Set optimal chunk size based on cache size
    call set_chunk_size(CHUNK_SIZE)
    
    ! Process large dataset in chunks
    n_chunks = (large_var%n_elements + CHUNK_SIZE - 1) / CHUNK_SIZE
    total_result = 0.0_real64
    
    do i = 1, n_chunks
        chunk_var = get_chunk(large_var, i)
        result_var = mean(chunk_var)
        chunk_result = result_var%data%values_r64(1)
        total_result = total_result + chunk_result
        
        call finalize_variable(chunk_var)
        call finalize_variable(result_var)
    end do
    
    total_result = total_result / real(n_chunks, real64)
end program
```

#### Memory Layout Optimization

```fortran
! Prefer column-major (Fortran native) layout
real(real64) :: data(nx, ny, nz)  ! Optimal
real(real64) :: data(nz, ny, nx)  ! Suboptimal for Fortran

! Access patterns: innermost dimension first
do k = 1, nz
    do j = 1, ny
        do i = 1, nx
            data(i, j, k) = compute_value(i, j, k)  ! Cache-friendly
        end do
    end do
end do
```

#### Cache-Conscious Algorithms

```fortran
! Tiled matrix operations for cache efficiency
subroutine tiled_matrix_multiply(A, B, C, n, tile_size)
    real(real64), intent(in) :: A(n, n), B(n, n)
    real(real64), intent(out) :: C(n, n)
    integer, intent(in) :: n, tile_size
    
    integer :: i, j, k, ii, jj, kk
    
    do ii = 1, n, tile_size
        do jj = 1, n, tile_size  
            do kk = 1, n, tile_size
                ! Process tile
                do i = ii, min(ii + tile_size - 1, n)
                    do j = jj, min(jj + tile_size - 1, n)
                        do k = kk, min(kk + tile_size - 1, n)
                            C(i, j) = C(i, j) + A(i, k) * B(k, j)
                        end do
                    end do
                end do
            end do
        end do
    end do
end subroutine
```

### 4. SIMD Optimization

#### Vectorizable Loop Patterns

```fortran
! SIMD-friendly operations
subroutine vectorized_operations(var1, var2, result_var)
    type(variable_t), intent(in) :: var1, var2
    type(variable_t), intent(out) :: result_var
    
    integer :: i, n
    real(real64), pointer :: data1(:), data2(:), result_data(:)
    
    n = var1%n_elements
    data1 => var1%data%values_r64
    data2 => var2%data%values_r64
    result_data => result_var%data%values_r64
    
    ! Compiler will automatically vectorize this loop
    !$OMP SIMD ALIGNED(data1, data2, result_data: 64)
    do i = 1, n
        result_data(i) = data1(i) + data2(i)
    end do
end subroutine
```

#### Memory Alignment

```fortran
! Ensure 64-byte alignment for SIMD
type :: aligned_storage_t
    real(real64), allocatable :: values(:)
end type

subroutine allocate_aligned(storage, n)
    type(aligned_storage_t), intent(out) :: storage
    integer, intent(in) :: n
    
    ! Allocate with padding for alignment
    allocate(storage%values(n + 8))  ! Extra elements for alignment
    
    ! Manual alignment (compiler-dependent)
    ! Modern compilers often handle this automatically
end subroutine
```

### 5. I/O Optimization

#### NetCDF4 Performance Settings

```fortran
! Optimal NetCDF4 settings for performance
subroutine configure_netcdf_performance()
    ! Enable chunking for large datasets
    call nf90_def_var_chunking(ncid, varid, NF90_CHUNKED, chunk_sizes)
    
    ! Enable compression (balance between speed and size)
    call nf90_def_var_deflate(ncid, varid, shuffle=1, deflate=1, deflate_level=1)
    
    ! Set buffer size for I/O operations  
    call nf90_set_chunk_cache(size=64*1024*1024, nelems=1009, preemption=0.75)
end subroutine
```

#### Parallel I/O

```fortran
! Use parallel NetCDF for large-scale applications
program parallel_io
    use mpi
    use netcdf
    implicit none
    
    integer :: ierr, rank, nprocs
    
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
    
    ! Open file for parallel access
    call nf90_open_par("data.nc", NF90_WRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
    
    ! Each process writes its portion
    ! ...
    
    call MPI_Finalize(ierr)
end program
```

## Performance Monitoring

### 1. Built-in Profiling

```fortran
program performance_monitoring
    use foxel
    use foxel_optimization, only: profiler_t
    implicit none
    
    type(profiler_t) :: profiler
    type(variable_t) :: large_var, result_var
    
    ! Start profiling
    call start_profiling(profiler)
    
    ! Perform operations
    result_var = mean(large_var)
    
    ! Stop profiling and get results
    call stop_profiling(profiler)
    call print_profile_report(profiler)
    
    ! Profile report includes:
    ! - Total execution time
    ! - Memory bandwidth achieved
    ! - Cache hit rates
    ! - Thread utilization
end program
```

### 2. External Profiling Tools

#### Perf (Linux)

```bash
# Profile CPU usage
perf record -g fpm run benchmark --profile release
perf report

# Monitor cache performance
perf stat -e cache-references,cache-misses,instructions,cycles fmp run benchmark
```

#### Intel VTune

```bash
# Profile hotspots
vtune -collect hotspots -result-dir vtune_results fpm run benchmark

# Analyze memory access patterns
vtune -collect memory-access -result-dir vtune_memory fpm run benchmark
```

#### Valgrind

```bash
# Memory usage profiling
valgrind --tool=massif fpm run benchmark

# Cache simulation
valgrind --tool=cachegrind fpm run benchmark
```

### 3. Performance Benchmarks

```fortran
program comprehensive_benchmark
    use foxel
    use iso_fortran_env, only: real64, int64
    implicit none
    
    call benchmark_arithmetic_operations()
    call benchmark_aggregation_functions()
    call benchmark_io_operations()
    call benchmark_memory_operations()
    
contains
    
    subroutine benchmark_arithmetic_operations()
        type(variable_t) :: var1, var2, result_var
        integer(int64) :: start_time, end_time, count_rate
        real(real64) :: elapsed_time, bandwidth
        integer, parameter :: N = 10000000  ! 10M elements
        
        ! Create test data
        var1 = create_random_variable(N)
        var2 = create_random_variable(N)
        
        ! Benchmark addition
        call system_clock(start_time, count_rate)
        result_var = var1 + var2
        call system_clock(end_time)
        
        elapsed_time = real(end_time - start_time, real64) / real(count_rate, real64)
        bandwidth = 3.0_real64 * N * 8 / elapsed_time / 1e9  ! GB/s (3 arrays, 8 bytes each)
        
        write(*,'(A,F6.2,A,F6.1,A)') "Addition: ", elapsed_time, " seconds, ", bandwidth, " GB/s"
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result_var)
    end subroutine
    
    subroutine benchmark_aggregation_functions()
        ! Benchmark statistical operations
    end subroutine
    
    subroutine benchmark_io_operations()
        ! Benchmark NetCDF I/O performance
    end subroutine
    
    subroutine benchmark_memory_operations()
        ! Benchmark memory allocation/deallocation
    end subroutine
    
end program
```

## Common Performance Issues

### 1. Memory Bandwidth Limitations

**Problem**: Operations become memory-bound rather than compute-bound.

**Solutions**:
- Use chunked processing to fit in cache
- Optimize memory access patterns
- Consider data layout reorganization

```fortran
! Memory-bound operation optimization
subroutine optimize_memory_bound_operation(var)
    type(variable_t), intent(inout) :: var
    integer, parameter :: CHUNK_SIZE = 100000  ! Fit in L2 cache
    
    call set_chunk_size(CHUNK_SIZE)
    result_var = process_in_chunks(var, chunk_operation)
end subroutine
```

### 2. Load Imbalance

**Problem**: Some threads finish before others in parallel operations.

**Solutions**:
- Use dynamic scheduling for irregular workloads
- Balance work distribution
- Consider work-stealing algorithms

```fortran
! Dynamic scheduling for load balancing
!$OMP PARALLEL DO SCHEDULE(DYNAMIC, 1000)
do i = 1, n_items
    call process_item(items(i))
end do
!$OMP END PARALLEL DO
```

### 3. False Sharing

**Problem**: Multiple threads accessing nearby memory locations cause cache conflicts.

**Solutions**:
- Pad data structures to cache line boundaries
- Use thread-local storage
- Reorganize data layout

```fortran
! Avoid false sharing with padding
type :: thread_local_data_t
    real(real64) :: local_sum
    integer(int8) :: padding(56)  ! Pad to 64 bytes (cache line)
end type

type(thread_local_data_t) :: thread_data(max_threads)
```

### 4. NUMA Effects

**Problem**: Memory access costs vary based on memory location relative to CPU.

**Solutions**:
- Use NUMA-aware memory allocation
- Bind threads to specific NUMA nodes
- First-touch memory policy

```bash
# NUMA-aware execution
numactl --cpunodebind=0 --membind=0 fpm run app
```

## Platform-Specific Optimizations

### Intel Architecture

```fortran
! Intel-specific optimizations
!DIR$ VECTOR ALIGNED
!DIR$ UNROLL(4)
do i = 1, n
    result(i) = a(i) * b(i) + c(i)
end do
```

### ARM Architecture

```fortran
! ARM NEON optimizations (compiler-dependent)
!$OMP SIMD SAFELEN(4)
do i = 1, n
    result(i) = sqrt(data(i))
end do
```

### GPU Acceleration (Future)

```fortran
! OpenMP offload (planned feature)
!$OMP TARGET MAP(to: input_data) MAP(from: result_data)
!$OMP TEAMS DISTRIBUTE PARALLEL DO
do i = 1, n
    result_data(i) = compute_intensive_function(input_data(i))
end do
!$OMP END TARGET
```

## Performance Testing Framework

### Automated Benchmarks

```fortran
program automated_performance_test
    use foxel
    implicit none
    
    ! Performance regression testing
    call run_performance_suite()
    
contains
    
    subroutine run_performance_suite()
        real(real64) :: baseline_time, current_time, regression_threshold
        logical :: performance_ok
        
        baseline_time = get_baseline_performance()
        current_time = measure_current_performance()
        
        regression_threshold = 1.1  ! Allow 10% regression
        performance_ok = (current_time / baseline_time) < regression_threshold
        
        if (performance_ok) then
            write(*,*) "PASS: Performance within acceptable range"
        else
            write(*,*) "FAIL: Performance regression detected"
            call exit(1)
        end if
    end subroutine
    
end program
```

### Continuous Integration

```yaml
# .github/workflows/performance.yml
name: Performance Testing
on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
    - name: Build optimized
      run: fpm build --profile release --flag "-O3 -march=native"
    - name: Run benchmarks
      run: |
        export OMP_NUM_THREADS=2
        fpm run benchmark --profile release
    - name: Check performance
      run: python scripts/check_performance_regression.py
```

## Best Practices Summary

### Code Level

1. **Use appropriate data types**: `real64` for precision, `real32` for memory-constrained applications
2. **Minimize memory allocations**: Reuse variables when possible
3. **Write cache-friendly loops**: Access memory in stride-1 patterns
4. **Use compiler intrinsics**: For platform-specific optimizations
5. **Profile regularly**: Identify and fix performance bottlenecks

### Build Level

1. **Enable optimization**: Always use `-O3` for production builds
2. **Target architecture**: Use `-march=native` for best performance
3. **Link-time optimization**: Enable LTO for cross-module optimizations
4. **Profile-guided optimization**: Use PGO for critical applications

### Runtime Level

1. **Set optimal thread count**: Usually equal to physical cores
2. **Configure OpenMP**: Use appropriate scheduling and binding
3. **Monitor resources**: Watch memory usage and CPU utilization
4. **Use performance tools**: Profile regularly to identify issues

This performance guide provides the foundation for achieving optimal performance with Foxel. Regular profiling and benchmarking are essential for maintaining high performance as the codebase evolves.