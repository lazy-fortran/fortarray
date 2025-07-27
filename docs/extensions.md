# Foxel Extension Guide

This guide covers how to extend Foxel with new functionality, including adding new data types, file formats, mathematical operations, and visualization capabilities.

## Extension Architecture

Foxel is designed with extensibility in mind, following a modular architecture that allows for clean integration of new features while maintaining compatibility with the core NetCDF data model.

### Core Extension Points

1. **Data Types**: Add support for new numeric and non-numeric types
2. **File Formats**: Integrate additional scientific data formats
3. **Mathematical Operations**: Implement new computational algorithms
4. **Visualization**: Add new plot types and visualization backends
5. **I/O Backends**: Support different storage systems and protocols

## Adding New Data Types

### 1. Extending the Storage System

The `storage_t` type in `foxel_storage` provides generic, type-safe data storage. To add a new data type:

#### Step 1: Define Type Identifier

```fortran
! In src/foxel_storage.f90
module foxel_storage
    implicit none
    
    ! Existing type identifiers
    integer, parameter :: TYPE_REAL64 = 1
    integer, parameter :: TYPE_REAL32 = 2
    integer, parameter :: TYPE_INT32 = 3
    integer, parameter :: TYPE_INT64 = 4
    integer, parameter :: TYPE_LOGICAL = 5
    
    ! New type identifier
    integer, parameter :: TYPE_COMPLEX64 = 6
    
    type :: storage_t
        integer :: data_type
        integer :: n_elements
        integer, allocatable :: shape(:)
        
        ! Existing storage arrays
        real(real64), allocatable :: values_r64(:)
        real(real32), allocatable :: values_r32(:)
        integer(int32), allocatable :: values_i32(:)
        integer(int64), allocatable :: values_i64(:)
        logical, allocatable :: values_logical(:)
        
        ! New storage array
        complex(real64), allocatable :: values_c64(:)
    end type storage_t
```

#### Step 2: Update Constructor

```fortran
! Constructor for complex data
function create_storage_complex64(data, shape_array) result(storage)
    complex(real64), intent(in) :: data(:)
    integer, intent(in) :: shape_array(:)
    type(storage_t) :: storage
    
    storage%data_type = TYPE_COMPLEX64
    storage%n_elements = size(data)
    storage%shape = shape_array
    allocate(storage%values_c64(size(data)))
    storage%values_c64 = data
end function create_storage_complex64
```

#### Step 3: Update Access Functions

```fortran
! Getter for complex data
function get_complex64_values(storage) result(values)
    type(storage_t), intent(in) :: storage
    complex(real64), allocatable :: values(:)
    
    if (storage%data_type /= TYPE_COMPLEX64) then
        ! Handle type mismatch error
        return
    end if
    
    allocate(values(storage%n_elements))
    values = storage%values_c64
end function get_complex64_values
```

### 2. Update Type Promotion Rules

```fortran
! In src/foxel_arithmetic.f90
function promote_types(type1, type2) result(result_type)
    integer, intent(in) :: type1, type2
    integer :: result_type
    
    ! Add complex promotion rules
    if (type1 == TYPE_COMPLEX64 .or. type2 == TYPE_COMPLEX64) then
        result_type = TYPE_COMPLEX64
    else if (type1 == TYPE_REAL64 .or. type2 == TYPE_REAL64) then
        result_type = TYPE_REAL64
    ! ... other rules
    end if
end function promote_types
```

### 3. Implement Arithmetic Operations

```fortran
! Complex arithmetic operations
function add_complex64(var1, var2) result(result_var)
    type(variable_t), intent(in) :: var1, var2
    type(variable_t) :: result_var
    
    complex(real64), allocatable :: data1(:), data2(:), result_data(:)
    integer :: i
    
    data1 = get_complex64_values(var1%data)
    data2 = get_complex64_values(var2%data)
    allocate(result_data(size(data1)))
    
    !$OMP PARALLEL DO
    do i = 1, size(data1)
        result_data(i) = data1(i) + data2(i)
    end do
    !$OMP END PARALLEL DO
    
    result_var = create_variable_from_complex64(result_data, var1%dims)
end function add_complex64
```

### 4. Add NetCDF I/O Support

```fortran
! In src/foxel_netcdf.f90
subroutine write_complex64_variable(ncid, varid, var, stat)
    integer, intent(in) :: ncid, varid
    type(variable_t), intent(in) :: var
    integer, intent(out) :: stat
    
    complex(real64), allocatable :: complex_data(:)
    real(real64), allocatable :: real_part(:), imag_part(:)
    
    complex_data = get_complex64_values(var%data)
    real_part = real(complex_data)
    imag_part = aimag(complex_data)
    
    ! Write as two real variables
    stat = nf90_put_var(ncid, varid, real_part)
    if (stat /= NF90_NOERR) return
    
    stat = nf90_put_var(ncid, varid+1, imag_part)
end subroutine write_complex64_variable
```

## Adding New File Formats

### 1. Create Format Module

Create a new module following the pattern of existing I/O modules:

```fortran
! src/foxel_hdf5.f90
module foxel_hdf5
    use foxel_types, only: variable_t, dataset_t
    use hdf5
    implicit none
    private
    
    public :: read_hdf5_dataset
    public :: write_hdf5_dataset
    public :: from_hdf5
    public :: to_hdf5
    
contains
    
    function read_hdf5_dataset(filename, dataset_name, stat) result(var)
        character(len=*), intent(in) :: filename, dataset_name
        integer, intent(out), optional :: stat
        type(variable_t) :: var
        
        integer(HID_T) :: file_id, dataset_id, dataspace_id, datatype_id
        integer :: h5_error, local_stat
        
        local_stat = 0
        
        ! Open HDF5 file
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, h5_error)
        if (h5_error < 0) then
            local_stat = ERROR_FILE_NOT_FOUND
            if (present(stat)) stat = local_stat
            return
        end if
        
        ! Open dataset
        call h5dopen_f(file_id, dataset_name, dataset_id, h5_error)
        if (h5_error < 0) then
            local_stat = ERROR_INVALID_VARIABLE
            call h5fclose_f(file_id, h5_error)
            if (present(stat)) stat = local_stat
            return
        end if
        
        ! Read dataset properties
        call h5dget_space_f(dataset_id, dataspace_id, h5_error)
        call h5dget_type_f(dataset_id, datatype_id, h5_error)
        
        ! Read data based on type and dimensions
        call read_hdf5_data(dataset_id, datatype_id, dataspace_id, var)
        
        ! Clean up
        call h5dclose_f(dataset_id, h5_error)
        call h5fclose_f(file_id, h5_error)
        
        if (present(stat)) stat = local_stat
    end function read_hdf5_dataset
    
    function from_hdf5(filename, stat) result(ds)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        type(dataset_t) :: ds
        
        ! Implementation to read entire HDF5 file into dataset
    end function from_hdf5
    
    subroutine to_hdf5(ds, filename, stat)
        type(dataset_t), intent(in) :: ds
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        
        ! Implementation to write dataset to HDF5 file
    end subroutine to_hdf5
    
end module foxel_hdf5
```

### 2. Update Format Detection

```fortran
! In src/foxel_formats.f90
function detect_file_format(filename) result(format_type)
    character(len=*), intent(in) :: filename
    character(len=32) :: format_type
    
    character(len=10) :: extension
    integer :: dot_pos
    
    ! Get file extension
    dot_pos = index(filename, '.', back=.true.)
    if (dot_pos > 0) then
        extension = filename(dot_pos+1:)
        call to_lowercase(extension)
        
        select case (extension)
        case ('nc', 'nc4', 'netcdf')
            format_type = 'netcdf'
        case ('h5', 'hdf5')
            format_type = 'hdf5'
        case ('csv', 'txt')
            format_type = 'csv'
        case ('zarr')
            format_type = 'zarr'
        case default
            format_type = 'unknown'
        end select
    else
        ! Content-based detection
        format_type = detect_by_content(filename)
    end if
end function detect_file_format
```

### 3. Update Main Interface

```fortran
! In src/foxel.f90
function from_file(filename, stat) result(ds)
    character(len=*), intent(in) :: filename
    integer, intent(out), optional :: stat
    type(dataset_t) :: ds
    
    character(len=32) :: format_type
    
    format_type = detect_file_format(filename)
    
    select case (format_type)
    case ('netcdf')
        ds = from_netcdf(filename, stat=stat)
    case ('hdf5')
        ds = from_hdf5(filename, stat=stat)
    case ('csv')
        ds = from_csv(filename, stat=stat)
    case default
        if (present(stat)) stat = ERROR_UNSUPPORTED_FORMAT
    end select
end function from_file
```

## Adding Mathematical Operations

### 1. Create Operation Module

```fortran
! src/foxel_signal_processing.f90
module foxel_signal_processing
    use foxel_types, only: variable_t
    use iso_fortran_env, only: real64
    implicit none
    private
    
    public :: fft, ifft, filter, convolve
    public :: hilbert_transform, power_spectrum
    
contains
    
    function fft(var, stat) result(result_var)
        type(variable_t), intent(in) :: var
        integer, intent(out), optional :: stat
        type(variable_t) :: result_var
        
        real(real64), allocatable :: input_data(:)
        complex(real64), allocatable :: output_data(:)
        integer :: n, local_stat
        
        local_stat = 0
        
        ! Validate input
        if (var%n_dims /= 1) then
            local_stat = ERROR_DIMENSION_MISMATCH
            if (present(stat)) stat = local_stat
            return
        end if
        
        n = var%dims(1)%length
        input_data = get_real64_values(var%data)
        allocate(output_data(n))
        
        ! Perform FFT (using external library like FFTW)
        call perform_fft(input_data, output_data, n)
        
        ! Create result variable with complex type
        result_var = create_variable_from_complex64(output_data, var%dims)
        result_var%name = trim(var%name) // "_fft"
        result_var%units = "frequency_domain"
        
        if (present(stat)) stat = local_stat
    end function fft
    
    function filter(var, filter_type, cutoff_freq, stat) result(result_var)
        type(variable_t), intent(in) :: var
        character(len=*), intent(in) :: filter_type
        real(real64), intent(in) :: cutoff_freq
        integer, intent(out), optional :: stat
        type(variable_t) :: result_var
        
        ! Implement filtering algorithms
        select case (filter_type)
        case ('lowpass')
            result_var = lowpass_filter(var, cutoff_freq)
        case ('highpass')
            result_var = highpass_filter(var, cutoff_freq)
        case ('bandpass')
            result_var = bandpass_filter(var, cutoff_freq)
        case default
            if (present(stat)) stat = ERROR_INVALID_ARGUMENT
            return
        end select
        
        if (present(stat)) stat = 0
    end function filter
    
end module foxel_signal_processing
```

### 2. Add to Main Interface

```fortran
! Update src/foxel.f90
module foxel
    ! Existing modules
    use foxel_types
    use foxel_arithmetic
    use foxel_aggregation
    
    ! New signal processing module
    use foxel_signal_processing, only: fft, ifft, filter, convolve
    
    implicit none
    
    ! Re-export new functions
    public :: fft, ifft, filter, convolve
    
end module foxel
```

### 3. Add Comprehensive Tests

```fortran
! test/test_signal_processing.f90
program test_signal_processing
    use foxel
    use test_framework
    implicit none
    
    call test_fft_basic()
    call test_fft_inverse()
    call test_filter_operations()
    call test_convolution()
    
contains
    
    subroutine test_fft_basic()
        type(variable_t) :: signal, fft_result
        real(real64), parameter :: pi = 3.14159265358979323846_real64
        real(real64) :: time_data(1024), freq
        integer :: i
        
        ! Create test signal (sine wave)
        do i = 1, 1024
            time_data(i) = sin(2.0_real64 * pi * 5.0_real64 * real(i-1, real64) / 1024.0_real64)
        end do
        
        signal = variable(time_data, name="sine_wave", dim_names=["time"])
        fft_result = fft(signal)
        
        ! Verify FFT properties
        call assert_equal(fft_result%data%data_type, TYPE_COMPLEX64, "FFT returns complex data")
        call assert_equal(fft_result%n_elements, 1024, "FFT preserves size")
        
        call finalize_variable(signal)
        call finalize_variable(fft_result)
    end subroutine test_fft_basic
    
end program test_signal_processing
```

## Adding Visualization Capabilities

### 1. Create Visualization Module

```fortran
! src/foxel_plotting_advanced.f90
module foxel_plotting_advanced
    use foxel_types, only: variable_t
    use fortplot, only: plot_t
    implicit none
    private
    
    public :: contour3d, surface3d, animation, interactive_plot
    
contains
    
    function contour3d(var, levels, stat) result(plot)
        type(variable_t), intent(in) :: var
        real(real64), intent(in), optional :: levels(:)
        integer, intent(out), optional :: stat
        type(plot_t) :: plot
        
        ! Validate 3D data
        if (var%n_dims /= 3) then
            if (present(stat)) stat = ERROR_DIMENSION_MISMATCH
            return
        end if
        
        ! Create 3D contour visualization
        call setup_3d_plot(plot)
        call add_contour_data(plot, var, levels)
        call configure_3d_axes(plot, var)
        
        if (present(stat)) stat = 0
    end function contour3d
    
    function animation(var, time_dim, stat) result(plot)
        type(variable_t), intent(in) :: var
        character(len=*), intent(in) :: time_dim
        integer, intent(out), optional :: stat
        type(plot_t) :: plot
        
        integer :: time_dim_index, n_frames
        
        ! Find time dimension
        time_dim_index = find_dimension_index(var, time_dim)
        if (time_dim_index == 0) then
            if (present(stat)) stat = ERROR_DIMENSION_NOT_FOUND
            return
        end if
        
        n_frames = var%dims(time_dim_index)%length
        
        ! Create animated plot
        call setup_animation(plot, n_frames)
        call add_animation_data(plot, var, time_dim_index)
        
        if (present(stat)) stat = 0
    end function animation
    
end module foxel_plotting_advanced
```

### 2. Integration with External Libraries

```fortran
! Integration with matplotlib via Python interface
module foxel_matplotlib
    use iso_c_binding
    use foxel_types, only: variable_t
    implicit none
    private
    
    public :: matplotlib_plot, matplotlib_save
    
    ! C interface to Python matplotlib
    interface
        function py_matplotlib_plot(data, n_data, plot_type) bind(C, name="matplotlib_plot")
            import :: c_double, c_int, c_char
            real(c_double), intent(in) :: data(*)
            integer(c_int), value :: n_data
            character(c_char), intent(in) :: plot_type(*)
            integer(c_int) :: py_matplotlib_plot
        end function py_matplotlib_plot
    end interface
    
contains
    
    function matplotlib_plot(var, plot_type) result(status)
        type(variable_t), intent(in) :: var
        character(len=*), intent(in) :: plot_type
        integer :: status
        
        real(real64), allocatable :: data(:)
        character(len=len(plot_type)+1, kind=c_char) :: c_plot_type
        integer :: i
        
        data = get_real64_values(var%data)
        
        ! Convert Fortran string to C string
        do i = 1, len(plot_type)
            c_plot_type(i:i) = plot_type(i:i)
        end do
        c_plot_type(len(plot_type)+1:len(plot_type)+1) = c_null_char
        
        status = py_matplotlib_plot(data, size(data), c_plot_type)
    end function matplotlib_plot
    
end module foxel_matplotlib
```

## Adding I/O Backends

### 1. Cloud Storage Integration

```fortran
! src/foxel_cloud.f90
module foxel_cloud
    use foxel_types, only: dataset_t
    implicit none
    private
    
    public :: from_s3, to_s3, from_azure, to_azure
    
contains
    
    function from_s3(bucket, key, credentials, stat) result(ds)
        character(len=*), intent(in) :: bucket, key, credentials
        integer, intent(out), optional :: stat
        type(dataset_t) :: ds
        
        character(len=512) :: temp_file, download_url
        integer :: local_stat
        
        ! Construct S3 URL
        download_url = "https://" // trim(bucket) // ".s3.amazonaws.com/" // trim(key)
        
        ! Download to temporary file
        temp_file = create_temp_filename()
        call download_file(download_url, temp_file, credentials, local_stat)
        
        if (local_stat == 0) then
            ! Read from temporary file
            ds = from_file(temp_file, stat=local_stat)
            call delete_file(temp_file)
        end if
        
        if (present(stat)) stat = local_stat
    end function from_s3
    
    subroutine to_s3(ds, bucket, key, credentials, stat)
        type(dataset_t), intent(in) :: ds
        character(len=*), intent(in) :: bucket, key, credentials
        integer, intent(out), optional :: stat
        
        character(len=512) :: temp_file, upload_url
        integer :: local_stat
        
        ! Write to temporary file
        temp_file = create_temp_filename()
        call to_netcdf(ds, temp_file, stat=local_stat)
        
        if (local_stat == 0) then
            ! Upload temporary file
            upload_url = "https://" // trim(bucket) // ".s3.amazonaws.com/" // trim(key)
            call upload_file(temp_file, upload_url, credentials, local_stat)
            call delete_file(temp_file)
        end if
        
        if (present(stat)) stat = local_stat
    end subroutine to_s3
    
end module foxel_cloud
```

### 2. Database Integration

```fortran
! src/foxel_database.f90
module foxel_database
    use foxel_types, only: variable_t, dataset_t
    use iso_c_binding
    implicit none
    private
    
    public :: from_database, to_database
    public :: database_connection_t
    
    type :: database_connection_t
        type(c_ptr) :: connection_handle
        character(len=256) :: connection_string
        logical :: is_connected
    end type database_connection_t
    
contains
    
    function connect_database(connection_string) result(conn)
        character(len=*), intent(in) :: connection_string
        type(database_connection_t) :: conn
        
        conn%connection_string = connection_string
        ! Initialize database connection (implementation depends on database type)
        call init_db_connection(conn)
    end function connect_database
    
    function from_database(conn, query, stat) result(ds)
        type(database_connection_t), intent(in) :: conn
        character(len=*), intent(in) :: query
        integer, intent(out), optional :: stat
        type(dataset_t) :: ds
        
        ! Execute query and convert results to dataset
        call execute_query_to_dataset(conn, query, ds, stat)
    end function from_database
    
end module foxel_database
```

## Testing Extension Points

### 1. Extension Test Framework

```fortran
! test/test_extension_framework.f90
module test_extension_framework
    use foxel
    implicit none
    private
    
    public :: test_new_extension
    
contains
    
    subroutine test_new_extension()
        ! Template for testing new extensions
        call test_type_safety()
        call test_memory_management()
        call test_error_handling()
        call test_performance()
        call test_compatibility()
    end subroutine test_new_extension
    
    subroutine test_type_safety()
        ! Ensure new types interact correctly with existing types
    end subroutine
    
    subroutine test_memory_management()
        ! Verify no memory leaks in new functionality
    end subroutine
    
    subroutine test_error_handling()
        ! Test error conditions and recovery
    end subroutine
    
    subroutine test_performance()
        ! Benchmark new operations
    end subroutine
    
    subroutine test_compatibility()
        ! Ensure backward compatibility
    end subroutine
    
end module test_extension_framework
```

### 2. Integration Testing

```fortran
! test/test_full_integration.f90
program test_full_integration
    use foxel
    implicit none
    
    ! Test complete workflows with extensions
    call test_complex_scientific_workflow()
    call test_multi_format_pipeline()
    call test_cloud_to_visualization_pipeline()
    
contains
    
    subroutine test_complex_scientific_workflow()
        type(dataset_t) :: climate_data
        type(variable_t) :: temperature, fft_result, filtered_data
        
        ! Read from cloud storage
        climate_data = from_s3("climate-bucket", "era5_2023.nc", "credentials")
        
        ! Extract and process variable
        temperature = get_variable(climate_data, "temperature")
        fft_result = fft(temperature)
        filtered_data = filter(temperature, "lowpass", 0.1_real64)
        
        ! Visualize results
        call animation(filtered_data, "time")
        
        ! Save to database
        call to_database(climate_data, db_connection, "INSERT INTO climate_data ...")
        
        ! Cleanup
        call finalize_variable(temperature)
        call finalize_variable(fft_result)
        call finalize_variable(filtered_data)
        call finalize_dataset(climate_data)
    end subroutine test_complex_scientific_workflow
    
end program test_full_integration
```

## Documentation for Extensions

### 1. Extension API Documentation

```fortran
!> Signal Processing Extension
!>
!> This module provides signal processing capabilities for Foxel variables,
!> including FFT, filtering, and spectral analysis operations.
!>
!> Example usage:
!> ```fortran
!> use foxel
!> use foxel_signal_processing
!>
!> type(variable_t) :: signal, filtered_signal, spectrum
!> 
!> ! Apply low-pass filter
!> filtered_signal = filter(signal, "lowpass", 0.1_real64)
!> 
!> ! Compute power spectrum
!> spectrum = power_spectrum(signal)
!> ```
!>
!> @author Your Name
!> @date 2024-01-01
!> @version 1.0
module foxel_signal_processing
```

### 2. Usage Examples

Create comprehensive examples showing how to use extensions:

```fortran
! docs/examples/extension_examples/signal_processing_example.f90
program signal_processing_example
    use foxel
    use foxel_signal_processing
    implicit none
    
    ! Demonstrate complete signal processing workflow
    call demonstrate_fft_analysis()
    call demonstrate_filtering()
    call demonstrate_spectral_analysis()
    
contains
    
    subroutine demonstrate_fft_analysis()
        ! Complete example with explanation
    end subroutine
    
end program signal_processing_example
```

## Best Practices for Extensions

### 1. Design Principles

- **Follow NetCDF Model**: All extensions should respect the core data model
- **Type Safety**: Maintain compile-time and runtime type checking
- **Error Handling**: Use consistent error reporting patterns
- **Performance**: Optimize for large-scale scientific computing
- **Documentation**: Provide comprehensive API documentation and examples

### 2. Code Organization

```
src/
├── foxel_core/          # Core functionality (stable)
├── foxel_extensions/    # Extension modules
│   ├── foxel_signal_processing.f90
│   ├── foxel_cloud.f90
│   ├── foxel_database.f90
│   └── foxel_advanced_plotting.f90
└── foxel.f90           # Main interface (selectively exports extensions)

test/
├── test_core/          # Core functionality tests
└── test_extensions/    # Extension tests
    ├── test_signal_processing.f90
    ├── test_cloud.f90
    └── test_database.f90

docs/
├── core/               # Core documentation
└── extensions/         # Extension documentation
    ├── signal_processing.md
    ├── cloud_storage.md
    └── database_integration.md
```

### 3. Backward Compatibility

- **Versioned APIs**: Use semantic versioning for extension APIs
- **Deprecation Warnings**: Provide clear migration paths for API changes
- **Extension Registry**: Central registry of available extensions
- **Optional Dependencies**: Extensions should not break core functionality

### 4. Community Extensions

- **Extension Template**: Provide template for community-developed extensions
- **Plugin System**: Allow loading extensions at runtime
- **Extension Manager**: Tool for discovering and installing extensions
- **Quality Standards**: Guidelines for community extension acceptance

This extension guide provides the framework for extending Foxel while maintaining consistency, performance, and reliability. Extensions should enhance the core NetCDF-based functionality without compromising the library's scientific computing focus.