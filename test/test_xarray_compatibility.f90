program test_xarray_compatibility
    use iso_fortran_env, only: real64, int32, int64, error_unit
    use fortarray_types
    use fortarray_constructors
    use fortarray_io
    use fortarray_datasets
    use fortarray_csv, only: write_csv_variable
    use fortarray_interoperability
    implicit none
    
    logical :: all_tests_passed = .true.
    
    write(*,'(A)') "Testing xarray compatibility..."
    write(*,'(A)') "================================="
    
    call test_dataarray_construction()
    call test_dataset_operations()
    call test_indexing_compatibility()
    call test_arithmetic_operations()
    call test_aggregation_methods()
    call test_selection_methods()
    call test_io_compatibility()
    call test_coordinate_handling()
    call test_attribute_handling()
    call test_error_handling_compatibility()
    call test_numerical_accuracy()
    call test_performance_comparison()
    
    if (all_tests_passed) then
        write(*,'(A)') "================================="
        write(*,'(A)') "All xarray compatibility tests passed!"
    else
        write(error_unit,'(A)') "================================="
        write(error_unit,'(A)') "Some xarray compatibility tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_dataarray_construction()
        type(fortarray_t) :: arr
        real(real64), dimension(3, 4) :: data
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create test data similar to xarray.DataArray construction
        do j = 1, 4
            do i = 1, 3
                data(i, j) = real(i + j, real64)
            end do
        end do
        
        ! Test: DataArray-style construction with coordinates
        arr = new_array(data, &
                       dim_names=[character(len=10) :: "x", "y"], &
                       name="temperature")
        
        ! Add xarray-style coordinates
        block
            type(coordinate_t) :: x_coord, y_coord
            real(real64), dimension(3) :: x_values = [1.0, 2.0, 3.0]
            real(real64), dimension(4) :: y_values = [10.0, 20.0, 30.0, 40.0]
            
            x_coord%name = "x"
            x_coord%length = 3
            x_coord%dtype = DTYPE_REAL64
            allocate(x_coord%values_r64(3))
            x_coord%values_r64 = x_values
            
            y_coord%name = "y"
            y_coord%length = 4
            y_coord%dtype = DTYPE_REAL64
            allocate(y_coord%values_r64(4))
            y_coord%values_r64 = y_values
            
            arr%coords(1) = x_coord
            arr%coords(2) = y_coord
            arr%has_coord = [.true., .true.]
        end block
        
        ! Add xarray-style attributes
        arr%units = "degrees_celsius"
        arr%long_name = "Temperature measurements"
        arr%standard_name = "air_temperature"
        
        ! Verify xarray-compatible structure
        if (.not. arr%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "DataArray construction failed"
        else if (arr%n_dims /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Incorrect number of dimensions"
        else if (.not. all(arr%has_coord)) then
            test_passed = .false.
            write(error_unit,'(A)') "Coordinates not properly set"
        else if (trim(arr%name) /= "temperature") then
            test_passed = .false.
            write(error_unit,'(A)') "Name not preserved"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: DataArray construction compatibility"
        else
            write(*,'(A)') "FAIL: DataArray construction compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_dataarray_construction
    
    subroutine test_dataset_operations()
        type(dataset_t) :: ds
        type(fortarray_t) :: temp_var, humidity_var
        real(real64), dimension(5, 3) :: temp_data, humidity_data
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create test data for multi-variable dataset
        do j = 1, 3
            do i = 1, 5
                temp_data(i, j) = 20.0 + real(i + j, real64)
                humidity_data(i, j) = 50.0 + real(i * j, real64)
            end do
        end do
        
        ! Create variables with shared coordinates
        temp_var = new_array(temp_data, &
                            dim_names=[character(len=10) :: "time", "location"], &
                            name="temperature")
        temp_var%units = "celsius"
        
        humidity_var = new_array(humidity_data, &
                                dim_names=[character(len=10) :: "time", "location"], &
                                name="humidity")
        humidity_var%units = "percent"
        
        ! Test: xarray-style Dataset creation
        ds = new_dataset()
        allocate(ds%variables(2))
        ds%variables(1) = temp_var
        ds%variables(2) = humidity_var
        ds%n_vars = 2
        allocate(ds%var_names(2))
        ds%var_names = [character(len=64) :: "temperature", "humidity"]
        ds%initialized = .true.
        
        ! Verify dataset structure matches xarray conventions
        if (.not. ds%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Dataset initialization failed"
        else if (ds%n_vars /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Incorrect number of variables"
        else if (trim(ds%var_names(1)) /= "temperature") then
            test_passed = .false.
            write(error_unit,'(A)') "Variable names not preserved"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Dataset operations compatibility"
        else
            write(*,'(A)') "FAIL: Dataset operations compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_dataset_operations
    
    subroutine test_indexing_compatibility()
        type(fortarray_t) :: arr, sliced
        real(real64), dimension(6, 4) :: data
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create test data
        do j = 1, 4
            do i = 1, 6
                data(i, j) = real(i * 10 + j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], &
                       name="test_data")
        
        ! Test: xarray-style slicing arr.sel(x=slice(2, 4))
        ! This would be equivalent to arr%slice([2, 4], [1, -1])
        sliced = arr%slice(start_indices=[2, 1], end_indices=[4, 4])
        
        if (.not. sliced%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Slicing operation failed"
        else if (sliced%shape(1) /= 3) then  ! Should be indices 2, 3, 4
            test_passed = .false.
            write(error_unit,'(A,I0)') "Incorrect slice size, got: ", sliced%shape(1)
        else if (sliced%shape(2) /= 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Second dimension not preserved"
        end if
        
        call finalize_fortarray(sliced)
        
        ! Test: xarray-style isel indexing arr.isel(x=[0, 2, 4])
        block
            integer, dimension(3) :: indices = [1, 3, 5]  ! Fortran 1-based
            sliced = arr%select_indices(dim=1, indices=indices)
            
            if (.not. sliced%initialized) then
                test_passed = .false.
                write(error_unit,'(A)') "Index selection failed"
            else if (sliced%shape(1) /= 3) then
                test_passed = .false.
                write(error_unit,'(A)') "Index selection wrong size"
            end if
            
            call finalize_fortarray(sliced)
        end block
        
        if (test_passed) then
            write(*,'(A)') "PASS: Indexing compatibility"
        else
            write(*,'(A)') "FAIL: Indexing compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_indexing_compatibility
    
    subroutine test_arithmetic_operations()
        type(fortarray_t) :: arr1, arr2, result
        real(real64), dimension(3, 3) :: data1, data2
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create test data
        do j = 1, 3
            do i = 1, 3
                data1(i, j) = real(i + j, real64)
                data2(i, j) = real(i * j, real64)
            end do
        end do
        
        arr1 = new_array(data1, dim_names=[character(len=10) :: "x", "y"], &
                        name="array1")
        arr2 = new_array(data2, dim_names=[character(len=10) :: "x", "y"], &
                        name="array2")
        
        ! Test: xarray-style arithmetic arr1 + arr2
        result = arr1%add(arr2)
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Addition operation failed"
        else if (result%shape(1) /= 3 .or. result%shape(2) /= 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Result shape incorrect"
        else
            ! Check a specific value: data1(1,1) + data2(1,1) = 2 + 1 = 3
            if (abs(result%data%values_r64(1) - 3.0_real64) > 1.0e-10) then
                test_passed = .false.
                write(error_unit,'(A,F0.6)') "Arithmetic result incorrect, got: ", &
                    result%data%values_r64(1)
            end if
        end if
        
        call finalize_fortarray(result)
        
        ! Test: xarray-style scalar operations arr1 * 2.0
        result = arr1%multiply_scalar(2.0_real64)
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Scalar multiplication failed"
        else if (abs(result%data%values_r64(1) - 4.0_real64) > 1.0e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Scalar multiplication result incorrect"
        end if
        
        call finalize_fortarray(result)
        
        if (test_passed) then
            write(*,'(A)') "PASS: Arithmetic operations compatibility"
        else
            write(*,'(A)') "FAIL: Arithmetic operations compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr1)
        call finalize_fortarray(arr2)
    end subroutine test_arithmetic_operations
    
    subroutine test_aggregation_methods()
        type(fortarray_t) :: arr, result
        real(real64), dimension(4, 3) :: data
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create test data with known statistics
        do j = 1, 3
            do i = 1, 4
                data(i, j) = real(i + j - 1, real64)  ! Values: 1,2,3,4; 2,3,4,5; 3,4,5,6
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], &
                       name="test_data")
        
        ! Test: xarray-style reduction arr.mean(dim="x")
        result = arr%mean(dim=1)  ! Average along first dimension
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean reduction failed"
        else if (result%n_dims /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean result should be 1D"
        else if (result%shape(1) /= 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean result wrong size"
        else
            ! Check first column mean: (1+2+3+4)/4 = 2.5
            if (abs(result%data%values_r64(1) - 2.5_real64) > 1.0e-10) then
                test_passed = .false.
                write(error_unit,'(A,F0.6)') "Mean value incorrect, got: ", &
                    result%data%values_r64(1)
            end if
        end if
        
        call finalize_fortarray(result)
        
        ! Test: xarray-style sum arr.sum()
        result = arr%sum()  ! Total sum
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum reduction failed"
        else if (result%n_dims /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum result should be scalar (0D)"
        else
            ! Total sum: (1+2+3+4) + (2+3+4+5) + (3+4+5+6) = 10 + 14 + 18 = 42
            if (abs(result%data%values_r64(1) - 42.0_real64) > 1.0e-10) then
                test_passed = .false.
                write(error_unit,'(A,F0.6)') "Sum value incorrect, got: ", &
                    result%data%values_r64(1)
            end if
        end if
        
        call finalize_fortarray(result)
        
        if (test_passed) then
            write(*,'(A)') "PASS: Aggregation methods compatibility"
        else
            write(*,'(A)') "FAIL: Aggregation methods compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_aggregation_methods
    
    subroutine test_selection_methods()
        type(fortarray_t) :: arr, result
        real(real64), dimension(5, 4) :: data
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create test data
        do j = 1, 4
            do i = 1, 5
                data(i, j) = real(i + j * 10, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "time", "space"], &
                       name="measurements")
        
        ! Add coordinate values for selection
        block
            type(coordinate_t) :: time_coord
            real(real64), dimension(5) :: time_values = [1.0, 2.0, 3.0, 4.0, 5.0]
            
            time_coord%name = "time"
            time_coord%length = 5
            time_coord%dtype = DTYPE_REAL64
            allocate(time_coord%values_r64(5))
            time_coord%values_r64 = time_values
            
            arr%coords(1) = time_coord
            arr%has_coord(1) = .true.
        end block
        
        ! Test: xarray-style selection arr.sel(time=3.0)
        result = arr%select_coord_value(coord_name="time", value=3.0_real64)
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Coordinate selection failed"
        else if (result%n_dims /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Selection result should be 1D"
        else if (result%shape(1) /= 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Selection result wrong size"
        end if
        
        call finalize_fortarray(result)
        
        if (test_passed) then
            write(*,'(A)') "PASS: Selection methods compatibility"
        else
            write(*,'(A)') "FAIL: Selection methods compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_selection_methods
    
    subroutine test_io_compatibility()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(3, 4) :: data
        character(len=256) :: filename = "test_xarray_compat.nc"
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test data
        do j = 1, 4
            do i = 1, 3
                data(i, j) = sin(real(i, real64)) * cos(real(j, real64))
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], &
                       name="field")
        arr%units = "meters"
        arr%long_name = "Test field for xarray compatibility"
        
        ! Test: xarray-style NetCDF I/O arr.to_netcdf("file.nc")
        status = write_netcdf_variable(filename, arr)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "NetCDF export failed"
        else
            ! Test: xarray-style loading xr.open_dataarray("file.nc")
            loaded = read_netcdf_variable(filename, "field")
            
            if (.not. loaded%initialized) then
                test_passed = .false.
                write(error_unit,'(A)') "NetCDF import failed"
            else if (loaded%shape(1) /= 3 .or. loaded%shape(2) /= 4) then
                test_passed = .false.
                write(error_unit,'(A)') "Loaded data shape mismatch"
            else if (trim(loaded%units) /= "meters") then
                test_passed = .false.
                write(error_unit,'(A)') "Metadata not preserved"
            end if
            
            call finalize_fortarray(loaded)
        end if
        
        call execute_command_line("rm -f " // trim(filename))
        
        if (test_passed) then
            write(*,'(A)') "PASS: I/O compatibility"
        else
            write(*,'(A)') "FAIL: I/O compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_io_compatibility
    
    subroutine test_coordinate_handling()
        type(fortarray_t) :: arr
        real(real64), dimension(4, 3) :: data
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create test data
        do j = 1, 3
            do i = 1, 4
                data(i, j) = real(i + j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "lat", "lon"], &
                       name="temperature")
        
        ! Test: xarray-style coordinate assignment
        block
            type(coordinate_t) :: lat_coord, lon_coord
            real(real64), dimension(4) :: lat_values = [10.0, 20.0, 30.0, 40.0]
            real(real64), dimension(3) :: lon_values = [-120.0, -110.0, -100.0]
            
            lat_coord%name = "lat"
            lat_coord%length = 4
            lat_coord%dtype = DTYPE_REAL64
            lat_coord%units = "degrees_north"
            allocate(lat_coord%values_r64(4))
            lat_coord%values_r64 = lat_values
            
            lon_coord%name = "lon"
            lon_coord%length = 3
            lon_coord%dtype = DTYPE_REAL64
            lon_coord%units = "degrees_east"
            allocate(lon_coord%values_r64(3))
            lon_coord%values_r64 = lon_values
            
            arr%coords(1) = lat_coord
            arr%coords(2) = lon_coord
            arr%has_coord = [.true., .true.]
        end block
        
        ! Verify coordinate handling matches xarray conventions
        if (.not. all(arr%has_coord)) then
            test_passed = .false.
            write(error_unit,'(A)') "Coordinates not properly assigned"
        else if (trim(arr%coords(1)%name) /= "lat") then
            test_passed = .false.
            write(error_unit,'(A)') "Coordinate names not preserved"
        else if (abs(arr%coords(1)%values_r64(2) - 20.0_real64) > 1.0e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Coordinate values not preserved"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Coordinate handling compatibility"
        else
            write(*,'(A)') "FAIL: Coordinate handling compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_coordinate_handling
    
    subroutine test_attribute_handling()
        type(fortarray_t) :: arr
        real(real64), dimension(3, 3) :: data
        integer :: i, j
        logical :: test_passed = .true.
        
        ! Create test data
        data = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0], [3, 3])
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], &
                       name="test_var")
        
        ! Test: xarray-style attribute assignment
        arr%units = "kg/m^3"
        arr%long_name = "Density measurements"
        arr%standard_name = "sea_water_density"
        ! Verify attributes are handled like xarray
        if (trim(arr%units) /= "kg/m^3") then
            test_passed = .false.
            write(error_unit,'(A)') "Units attribute not preserved"
        else if (trim(arr%long_name) /= "Density measurements") then
            test_passed = .false.
            write(error_unit,'(A)') "Long name attribute not preserved"
        else if (trim(arr%standard_name) /= "sea_water_density") then
            test_passed = .false.
            write(error_unit,'(A)') "Standard name attribute not preserved"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: Attribute handling compatibility"
        else
            write(*,'(A)') "FAIL: Attribute handling compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_attribute_handling
    
    subroutine test_error_handling_compatibility()
        type(fortarray_t) :: arr, result
        real(real64), dimension(3, 3) :: data
        logical :: test_passed = .true.
        
        data = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0], [3, 3])
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], &
                       name="test_data")
        
        ! Test: Invalid dimension slicing (like xarray behavior)
        result = arr%slice(start_indices=[1, 1], end_indices=[5, 3])  ! x=5 is out of bounds
        
        if (result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Should have failed for out-of-bounds slice"
            call finalize_fortarray(result)
        end if
        
        ! Test: Invalid coordinate selection
        block
            integer, dimension(5) :: bad_indices = [1, 2, 3, 4, 5]  ! Index 5 is out of bounds
            result = arr%select_indices(dim=1, indices=bad_indices)
            
            if (result%initialized) then
                test_passed = .false.
                write(error_unit,'(A)') "Should have failed for invalid indices"
                call finalize_fortarray(result)
            end if
        end block
        
        if (test_passed) then
            write(*,'(A)') "PASS: Error handling compatibility"
        else
            write(*,'(A)') "FAIL: Error handling compatibility"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_error_handling_compatibility
    
    subroutine test_numerical_accuracy()
        type(fortarray_t) :: arr, result
        real(real64), dimension(100) :: data
        integer :: i
        logical :: test_passed = .true.
        real(real64) :: expected_mean, computed_mean
        
        ! Create test data with known statistics
        do i = 1, 100
            data(i) = real(i, real64)  ! 1, 2, 3, ..., 100
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "index"], &
                       name="sequence")
        
        ! Test: Numerical accuracy of mean calculation
        result = arr%mean()
        expected_mean = 50.5_real64  ! Mean of 1 to 100
        computed_mean = result%data%values_r64(1)
        
        if (abs(computed_mean - expected_mean) > 1.0e-10) then
            test_passed = .false.
            write(error_unit,'(A,F0.10,A,F0.10)') "Mean accuracy test failed: got ", &
                computed_mean, ", expected ", expected_mean
        end if
        
        call finalize_fortarray(result)
        
        ! Test: Numerical accuracy of standard deviation
        result = arr%std()
        ! Standard deviation of 1 to 100: sqrt(sum((i-50.5)^2)/99) ≈ 29.01
        if (abs(result%data%values_r64(1) - 29.01149423592240_real64) > 1.0e-10) then
            test_passed = .false.
            write(error_unit,'(A,F0.10)') "Std accuracy test failed: got ", &
                result%data%values_r64(1)
        end if
        
        call finalize_fortarray(result)
        
        if (test_passed) then
            write(*,'(A)') "PASS: Numerical accuracy verification"
        else
            write(*,'(A)') "FAIL: Numerical accuracy verification"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_numerical_accuracy
    
    subroutine test_performance_comparison()
        type(fortarray_t) :: arr, result
        real(real64), dimension(1000, 100) :: data
        integer :: i, j, start_time, end_time
        logical :: test_passed = .true.
        real(real64) :: computation_time
        
        ! Create large test data
        do j = 1, 100
            do i = 1, 1000
                data(i, j) = sin(real(i, real64)) * cos(real(j, real64))
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], &
                       name="large_array")
        
        ! Test: Performance benchmark for aggregation
        call system_clock(start_time)
        result = arr%mean(dim=1)  ! Average along first dimension
        call system_clock(end_time)
        
        computation_time = real(end_time - start_time, real64) / 1000.0_real64
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Performance test computation failed"
        else if (computation_time > 10.0_real64) then  ! Should complete in <10 seconds
            test_passed = .false.
            write(error_unit,'(A,F0.3,A)') "Performance too slow: ", computation_time, " seconds"
        end if
        
        call finalize_fortarray(result)
        
        if (test_passed) then
            write(*,'(A,F0.3,A)') "PASS: Performance comparison (", computation_time, " seconds)"
        else
            write(*,'(A)') "FAIL: Performance comparison"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_performance_comparison
    
    subroutine finalize_fortarray(arr)
        type(fortarray_t), intent(inout) :: arr
        integer :: i
        
        if (allocated(arr%dim_names)) deallocate(arr%dim_names)
        if (allocated(arr%shape)) deallocate(arr%shape)
        if (allocated(arr%strides)) deallocate(arr%strides)
        if (allocated(arr%coords)) then
            do i = 1, size(arr%coords)
                if (allocated(arr%coords(i)%values_r64)) deallocate(arr%coords(i)%values_r64)
                if (allocated(arr%coords(i)%values_r32)) deallocate(arr%coords(i)%values_r32)
                if (allocated(arr%coords(i)%values_i32)) deallocate(arr%coords(i)%values_i32)
                if (allocated(arr%coords(i)%values_char)) deallocate(arr%coords(i)%values_char)
                if (allocated(arr%coords(i)%attrs)) deallocate(arr%coords(i)%attrs)
            end do
            deallocate(arr%coords)
        end if
        if (allocated(arr%has_coord)) deallocate(arr%has_coord)
        if (allocated(arr%attrs)) deallocate(arr%attrs)
        
        ! Clean up data storage
        if (allocated(arr%data%values_r64)) deallocate(arr%data%values_r64)
        if (allocated(arr%data%values_r32)) deallocate(arr%data%values_r32)
        if (allocated(arr%data%values_i64)) deallocate(arr%data%values_i64)
        if (allocated(arr%data%values_i32)) deallocate(arr%data%values_i32)
        if (allocated(arr%data%values_char)) deallocate(arr%data%values_char)
        if (allocated(arr%data%values_logical)) deallocate(arr%data%values_logical)
        
        arr%initialized = .false.
    end subroutine finalize_fortarray
    
    subroutine finalize_dataset(ds)
        type(dataset_t), intent(inout) :: ds
        integer :: i
        
        if (allocated(ds%dimensions)) deallocate(ds%dimensions)
        if (allocated(ds%variables)) then
            do i = 1, size(ds%variables)
                call finalize_fortarray(ds%variables(i))
            end do
            deallocate(ds%variables)
        end if
        if (allocated(ds%var_names)) deallocate(ds%var_names)
        if (allocated(ds%coord_var_indices)) deallocate(ds%coord_var_indices)
        if (allocated(ds%attr_keys)) deallocate(ds%attr_keys)
        if (allocated(ds%attr_values)) deallocate(ds%attr_values)
        
        ds%initialized = .false.
    end subroutine finalize_dataset
    
end program test_xarray_compatibility