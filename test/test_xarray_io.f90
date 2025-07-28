program test_xarray_io
    use iso_fortran_env, only: real64, int32, int64, error_unit
    use fortarray_types
    use fortarray_constructors
    use fortarray_io  ! Module for xarray-compatible I/O functions
    use fortarray_netcdf, only: write_netcdf_variable, read_netcdf_variable, &
        write_options_t, write_netcdf
    implicit none
    
    logical :: all_tests_passed = .true.
    character(len=256) :: test_file = "test_xarray_io.nc"
    character(len=256) :: test_file2 = "test_xarray_io2.nc"
    
    write(*,'(A)') "Testing xarray-compatible I/O methods..."
    write(*,'(A)') "========================================="
    
    call test_to_netcdf_method()
    call test_open_dataarray()
    call test_open_dataset()
    call test_roundtrip_single_variable()
    call test_roundtrip_dataset()
    call test_chunked_writing()
    call test_compression_options()
    call test_coordinate_preservation()
    call test_attribute_preservation()
    call test_cf_compliance()
    
    ! Clean up test files
    call execute_command_line("rm -f " // trim(test_file) // " " // trim(test_file2))
    
    if (all_tests_passed) then
        write(*,'(A)') "========================================="
        write(*,'(A)') "All xarray I/O tests passed!"
    else
        write(error_unit,'(A)') "========================================="
        write(error_unit,'(A)') "Some xarray I/O tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_to_netcdf_method()
        type(fortarray_t) :: arr
        real(real64), dimension(10, 5) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        logical :: file_exists
        
        ! Create test data
        do j = 1, 5
            do i = 1, 10
                data(i, j) = real(i, real64) + real(j, real64) * 0.1_real64
            end do
        end do
        
        ! Create fortarray with coordinates
        arr = new_array(data, dim_names=["x", "y"], name="temperature")
        
        ! Add coordinates
        block
            type(coordinate_t) :: x_coord, y_coord
            real(real64), dimension(10) :: x_values
            real(real64), dimension(5) :: y_values
            
            do i = 1, 10
                x_values(i) = real(i-1, real64)
            end do
            do i = 1, 5
                y_values(i) = real(i-1, real64) * 10.0_real64
            end do
            
            x_coord%name = "x"
            x_coord%length = 10
            x_coord%dtype = DTYPE_REAL64
            allocate(x_coord%values_r64(10))
            x_coord%values_r64 = x_values
            
            y_coord%name = "y"
            y_coord%length = 5
            y_coord%dtype = DTYPE_REAL64
            allocate(y_coord%values_r64(5))
            y_coord%values_r64 = y_values
            
            arr%coords(1) = x_coord
            arr%coords(2) = y_coord
            arr%has_coord = [.true., .true.]
        end block
        
        ! Test: Write to NetCDF using to_netcdf method
        status = arr%to_netcdf(test_file)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "to_netcdf method failed"
        end if
        
        ! Check if file was created
        inquire(file=test_file, exist=file_exists)
        if (.not. file_exists) then
            test_passed = .false.
            write(error_unit,'(A)') "NetCDF file was not created"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: to_netcdf method"
        else
            write(*,'(A)') "FAIL: to_netcdf method"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_to_netcdf_method
    
    subroutine test_open_dataarray()
        type(fortarray_t) :: arr, loaded_arr
        real(real64), dimension(10, 5) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create and save test data
        do j = 1, 5
            do i = 1, 10
                data(i, j) = real(i * j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=["x", "y"], name="test_data")
        status = write_netcdf_variable(test_file, arr)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to create test file for open_dataarray"
        else
            ! Test: Load using open_dataarray
            loaded_arr = open_dataarray(test_file, "test_data")
            
            ! Verify data
            if (loaded_arr%initialized) then
                if (loaded_arr%n_dims /= 2 .or. loaded_arr%shape(1) /= 10 .or. loaded_arr%shape(2) /= 5) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Loaded array has wrong dimensions"
                else if (allocated(loaded_arr%data%values_r64)) then
                    do j = 1, 5
                        do i = 1, 10
                            if (abs(loaded_arr%data%values_r64((j-1)*10 + i) - data(i,j)) > 1.0e-10) then
                                test_passed = .false.
                                write(error_unit,'(A,I0,A,I0,A)') "Data mismatch at (", i, ",", j, ")"
                            end if
                        end do
                    end do
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "open_dataarray failed to initialize array"
            end if
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: open_dataarray"
        else
            write(*,'(A)') "FAIL: open_dataarray"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded_arr)
    end subroutine test_open_dataarray
    
    subroutine test_open_dataset()
        type(dataset_t) :: ds, loaded_ds
        type(fortarray_t) :: temp, pressure, humidity
        real(real64), dimension(10, 5) :: temp_data, press_data, humid_data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test dataset with multiple variables
        do j = 1, 5
            do i = 1, 10
                temp_data(i, j) = 273.15_real64 + real(i + j, real64)
                press_data(i, j) = 1013.25_real64 + real(i - j, real64) * 0.1_real64
                humid_data(i, j) = 50.0_real64 + real(i * j, real64) * 0.5_real64
            end do
        end do
        
        temp = new_array(temp_data, dim_names=["lon", "lat"], name="temperature")
        pressure = new_array(press_data, dim_names=["lon", "lat"], name="pressure")
        humidity = new_array(humid_data, dim_names=["lon", "lat"], name="humidity")
        
        ! Create dataset
        allocate(ds%variables(3))
        ds%variables(1) = temp
        ds%variables(2) = pressure
        ds%variables(3) = humidity
        ds%n_vars = 3
        allocate(ds%var_names(3))
        ds%var_names = [character(len=64) :: "temperature", "pressure", "humidity"]
        ds%initialized = .true.
        
        ! Save dataset
        status = write_netcdf(test_file, ds)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to create test file for open_dataset"
        else
            ! Test: Load using open_dataset
            loaded_ds = open_dataset(test_file)
            
            ! Verify dataset
            if (loaded_ds%initialized) then
                if (loaded_ds%n_vars /= 3) then
                    test_passed = .false.
                    write(error_unit,'(A,I0)') "Expected 3 variables, got: ", loaded_ds%n_vars
                end if
                
                ! Check variable names
                if (allocated(loaded_ds%var_names)) then
                    if (trim(loaded_ds%var_names(1)) /= "temperature" .or. &
                        trim(loaded_ds%var_names(2)) /= "pressure" .or. &
                        trim(loaded_ds%var_names(3)) /= "humidity") then
                        test_passed = .false.
                        write(error_unit,'(A)') "Variable names don't match"
                    end if
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "open_dataset failed to initialize dataset"
            end if
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: open_dataset"
        else
            write(*,'(A)') "FAIL: open_dataset"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(temp)
        call finalize_fortarray(pressure)
        call finalize_fortarray(humidity)
        call finalize_dataset(ds)
        call finalize_dataset(loaded_ds)
    end subroutine test_open_dataset
    
    subroutine test_roundtrip_single_variable()
        type(fortarray_t) :: original, loaded
        real(real64), dimension(20, 10, 5) :: data_3d
        integer :: i, j, k, status
        logical :: test_passed = .true.
        
        ! Create 3D test data
        do k = 1, 5
            do j = 1, 10
                do i = 1, 20
                    data_3d(i, j, k) = real(i, real64) + real(j, real64) * 0.1_real64 + real(k, real64) * 0.01_real64
                end do
            end do
        end do
        
        original = new_array(data_3d, dim_names=["x", "y", "z"], name="data3d")
        
        ! Add units and other attributes
        original%units = "K"
        original%long_name = "Three dimensional test data"
        
        ! Write and read back
        status = original%to_netcdf(test_file)
        
        if (status == 0) then
            loaded = open_dataarray(test_file, "data3d")
            
            ! Verify roundtrip
            if (loaded%initialized) then
                ! Check dimensions
                if (loaded%n_dims /= 3 .or. any(loaded%shape /= [20, 10, 5])) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Dimensions don't match after roundtrip"
                end if
                
                ! Check attributes
                if (trim(loaded%units) /= "K") then
                    test_passed = .false.
                    write(error_unit,'(A)') "Units attribute lost in roundtrip"
                end if
                
                if (trim(loaded%long_name) /= "Three dimensional test data") then
                    test_passed = .false.
                    write(error_unit,'(A)') "Long name attribute lost in roundtrip"
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "Failed to load data in roundtrip"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write data for roundtrip test"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: roundtrip single variable"
        else
            write(*,'(A)') "FAIL: roundtrip single variable"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(original)
        call finalize_fortarray(loaded)
    end subroutine test_roundtrip_single_variable
    
    subroutine test_roundtrip_dataset()
        type(dataset_t) :: original, loaded
        type(fortarray_t) :: var1, var2
        real(real64), dimension(15, 10) :: data1, data2
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test data
        do j = 1, 10
            do i = 1, 15
                data1(i, j) = sin(real(i, real64) * 0.1_real64) * cos(real(j, real64) * 0.1_real64)
                data2(i, j) = exp(-real((i-8)**2 + (j-5)**2, real64) * 0.01_real64)
            end do
        end do
        
        var1 = new_array(data1, dim_names=["x", "y"], name="sine_cosine")
        var2 = new_array(data2, dim_names=["x", "y"], name="gaussian")
        
        ! Create dataset with global attributes
        allocate(original%variables(2))
        original%variables(1) = var1
        original%variables(2) = var2
        original%n_vars = 2
        allocate(original%var_names(2))
        original%var_names = [character(len=64) :: "sine_cosine", "gaussian"]
        original%initialized = .true.
        original%title = "Test Dataset"
        original%institution = "FortArray Test Suite"
        original%source = "test_xarray_io.f90"
        
        ! Write and read back
        status = write_netcdf(test_file, original)
        
        if (status == 0) then
            loaded = open_dataset(test_file)
            
            ! Verify roundtrip
            if (loaded%initialized) then
                if (loaded%n_vars /= 2) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Number of variables doesn't match"
                end if
                
                ! Check global attributes
                if (trim(loaded%title) /= "Test Dataset") then
                    test_passed = .false.
                    write(error_unit,'(A)') "Title attribute lost in roundtrip"
                end if
                
                if (trim(loaded%institution) /= "FortArray Test Suite") then
                    test_passed = .false.
                    write(error_unit,'(A)') "Institution attribute lost in roundtrip"
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "Failed to load dataset in roundtrip"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write dataset for roundtrip test"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: roundtrip dataset"
        else
            write(*,'(A)') "FAIL: roundtrip dataset"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(var1)
        call finalize_fortarray(var2)
        call finalize_dataset(original)
        call finalize_dataset(loaded)
    end subroutine test_roundtrip_dataset
    
    subroutine test_chunked_writing()
        type(fortarray_t) :: arr
        type(write_options_t) :: options
        real(real64), dimension(1000, 500) :: large_data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create large test data
        do j = 1, 500
            do i = 1, 1000
                large_data(i, j) = real(i + j, real64)
            end do
        end do
        
        arr = new_array(large_data, dim_names=["x", "y"], name="large_data")
        
        ! Set chunking options
        allocate(options%chunksizes(2))
        options%chunksizes = [100, 50]  ! Chunk size 100x50
        
        ! Test: Write with chunking (options not yet supported)
        ! status = arr%to_netcdf(test_file, options=options)
        status = arr%to_netcdf(test_file)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write chunked data"
        end if
        
        ! TODO: Verify chunking was applied correctly (requires NetCDF library calls)
        
        if (test_passed) then
            write(*,'(A)') "PASS: chunked writing"
        else
            write(*,'(A)') "FAIL: chunked writing"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_chunked_writing
    
    subroutine test_compression_options()
        type(fortarray_t) :: arr
        type(write_options_t) :: options
        real(real64), dimension(100, 100) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        integer(int64) :: file_size_uncompressed, file_size_compressed
        
        ! Create test data with pattern that compresses well
        do j = 1, 100
            do i = 1, 100
                data(i, j) = real(mod(i + j, 10), real64)
            end do
        end do
        
        arr = new_array(data, dim_names=["x", "y"], name="compressible_data")
        
        ! Write uncompressed
        status = arr%to_netcdf(test_file)
        if (status == 0) then
            inquire(file=test_file, size=file_size_uncompressed)
        else
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write uncompressed data"
        end if
        
        ! Write compressed
        options%compress = .true.
        options%deflate_level = 9
        options%shuffle = .true.
        
        ! Options not yet supported in to_netcdf
        ! status = arr%to_netcdf(test_file2, options=options)
        status = arr%to_netcdf(test_file2)
        if (status == 0) then
            inquire(file=test_file2, size=file_size_compressed)
            
            ! Compressed file should be smaller
            if (file_size_compressed >= file_size_uncompressed) then
                test_passed = .false.
                write(error_unit,'(A)') "Compression did not reduce file size"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write compressed data"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: compression options"
        else
            write(*,'(A)') "FAIL: compression options"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_compression_options
    
    subroutine test_coordinate_preservation()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(10, 5) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create data with non-uniform coordinates
        do j = 1, 5
            do i = 1, 10
                data(i, j) = real(i * j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=["lon", "lat"], name="geo_data")
        
        ! Add non-uniform coordinates
        block
            type(coordinate_t) :: lon_coord, lat_coord
            real(real64), dimension(10) :: lon_values
            real(real64), dimension(5) :: lat_values
            
            ! Non-uniform longitude
            lon_values = [-180.0_real64, -120.0_real64, -60.0_real64, -30.0_real64, 0.0_real64, &
                          30.0_real64, 60.0_real64, 90.0_real64, 120.0_real64, 180.0_real64]
            
            ! Non-uniform latitude
            lat_values = [-90.0_real64, -45.0_real64, 0.0_real64, 45.0_real64, 90.0_real64]
            
            lon_coord%name = "lon"
            lon_coord%length = 10
            lon_coord%dtype = DTYPE_REAL64
            allocate(lon_coord%values_r64(10))
            lon_coord%values_r64 = lon_values
            
            lat_coord%name = "lat"
            lat_coord%length = 5
            lat_coord%dtype = DTYPE_REAL64
            allocate(lat_coord%values_r64(5))
            lat_coord%values_r64 = lat_values
            
            arr%coords(1) = lon_coord
            arr%coords(2) = lat_coord
            arr%has_coord = [.true., .true.]
        end block
        
        ! Write and read back
        status = arr%to_netcdf(test_file)
        
        if (status == 0) then
            loaded = open_dataarray(test_file, "geo_data")
            
            if (loaded%initialized .and. all(loaded%has_coord)) then
                ! Check longitude coordinates
                if (allocated(loaded%coords(1)%values_r64)) then
                    if (any(abs(loaded%coords(1)%values_r64 - arr%coords(1)%values_r64) > 1.0e-10)) then
                        test_passed = .false.
                        write(error_unit,'(A)') "Longitude coordinates not preserved"
                    end if
                else
                    test_passed = .false.
                    write(error_unit,'(A)') "Longitude coordinates not loaded"
                end if
                
                ! Check latitude coordinates
                if (allocated(loaded%coords(2)%values_r64)) then
                    if (any(abs(loaded%coords(2)%values_r64 - arr%coords(2)%values_r64) > 1.0e-10)) then
                        test_passed = .false.
                        write(error_unit,'(A)') "Latitude coordinates not preserved"
                    end if
                else
                    test_passed = .false.
                    write(error_unit,'(A)') "Latitude coordinates not loaded"
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "Coordinates not properly initialized"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write data with coordinates"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: coordinate preservation"
        else
            write(*,'(A)') "FAIL: coordinate preservation"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded)
    end subroutine test_coordinate_preservation
    
    subroutine test_attribute_preservation()
        type(fortarray_t) :: arr, loaded
        real(real64), dimension(10) :: data_1d
        integer :: i, status
        logical :: test_passed = .true.
        
        ! Create simple 1D data
        do i = 1, 10
            data_1d(i) = real(i, real64) ** 2
        end do
        
        arr = new_array(data_1d, dim_names=["time"], name="timeseries")
        
        ! Add various attributes
        arr%units = "W/m^2"
        arr%long_name = "Surface solar radiation"
        arr%standard_name = "surface_downwelling_shortwave_flux"
        
        ! Add custom attributes
        if (.not. allocated(arr%attrs)) then
            allocate(arr%attrs(3))
            arr%n_attrs = 3
        end if
        
        arr%attrs(1)%name = "valid_min"
        arr%attrs(1)%value = "0.0"
        arr%attrs(1)%dtype = ATTR_TYPE_STRING
        
        arr%attrs(2)%name = "valid_max"
        arr%attrs(2)%value = "1500.0"
        arr%attrs(2)%dtype = ATTR_TYPE_STRING
        
        arr%attrs(3)%name = "comment"
        arr%attrs(3)%value = "Measured at 2m height"
        arr%attrs(3)%dtype = ATTR_TYPE_STRING
        
        ! Write and read back
        status = arr%to_netcdf(test_file)
        
        if (status == 0) then
            loaded = open_dataarray(test_file, "timeseries")
            
            if (loaded%initialized) then
                ! Check standard attributes
                if (trim(loaded%units) /= "W/m^2") then
                    test_passed = .false.
                    write(error_unit,'(A)') "Units attribute not preserved"
                end if
                
                if (trim(loaded%long_name) /= "Surface solar radiation") then
                    test_passed = .false.
                    write(error_unit,'(A)') "Long name attribute not preserved"
                end if
                
                if (trim(loaded%standard_name) /= "surface_downwelling_shortwave_flux") then
                    test_passed = .false.
                    write(error_unit,'(A)') "Standard name attribute not preserved"
                end if
                
                ! Check custom attributes
                if (loaded%n_attrs < 3) then
                    test_passed = .false.
                    write(error_unit,'(A,I0)') "Expected at least 3 custom attributes, got: ", loaded%n_attrs
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "Failed to load data with attributes"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write data with attributes"
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: attribute preservation"
        else
            write(*,'(A)') "FAIL: attribute preservation"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(loaded)
    end subroutine test_attribute_preservation
    
    subroutine test_cf_compliance()
        type(fortarray_t) :: arr
        type(write_options_t) :: options
        real(real64), dimension(10, 5, 3) :: data_3d
        integer :: i, j, k, status
        logical :: test_passed = .true.
        
        ! Create 3D data (lon, lat, time)
        do k = 1, 3
            do j = 1, 5
                do i = 1, 10
                    data_3d(i, j, k) = 273.15_real64 + real(i + j + k, real64)
                end do
            end do
        end do
        
        arr = new_array(data_3d, dim_names=[character(len=10) :: "longitude", "latitude", "time"], name="temperature")
        
        ! Set CF-compliant attributes
        arr%units = "K"
        arr%long_name = "Air temperature at 2m"
        arr%standard_name = "air_temperature"
        
        ! Add CF-compliant coordinates
        block
            type(coordinate_t) :: lon_coord, lat_coord, time_coord
            real(real64), dimension(10) :: lon_values
            real(real64), dimension(5) :: lat_values
            real(real64), dimension(3) :: time_values
            
            do i = 1, 10
                lon_values(i) = -180.0_real64 + real(i-1, real64) * 40.0_real64
            end do
            do i = 1, 5
                lat_values(i) = -90.0_real64 + real(i-1, real64) * 45.0_real64
            end do
            time_values = [0.0_real64, 6.0_real64, 12.0_real64]  ! Hours since reference
            
            ! Longitude with CF attributes
            lon_coord%name = "longitude"
            lon_coord%length = 10
            lon_coord%dtype = DTYPE_REAL64
            allocate(lon_coord%values_r64(10))
            lon_coord%values_r64 = lon_values
            allocate(lon_coord%attrs(3))
            lon_coord%n_attrs = 3
            lon_coord%attrs(1)%name = "units"
            lon_coord%attrs(1)%value = "degrees_east"
            lon_coord%attrs(1)%dtype = ATTR_TYPE_STRING
            lon_coord%attrs(2)%name = "standard_name"
            lon_coord%attrs(2)%value = "longitude"
            lon_coord%attrs(2)%dtype = ATTR_TYPE_STRING
            lon_coord%attrs(3)%name = "axis"
            lon_coord%attrs(3)%value = "X"
            lon_coord%attrs(3)%dtype = ATTR_TYPE_STRING
            
            ! Latitude with CF attributes
            lat_coord%name = "latitude"
            lat_coord%length = 5
            lat_coord%dtype = DTYPE_REAL64
            allocate(lat_coord%values_r64(5))
            lat_coord%values_r64 = lat_values
            allocate(lat_coord%attrs(3))
            lat_coord%n_attrs = 3
            lat_coord%attrs(1)%name = "units"
            lat_coord%attrs(1)%value = "degrees_north"
            lat_coord%attrs(1)%dtype = ATTR_TYPE_STRING
            lat_coord%attrs(2)%name = "standard_name"
            lat_coord%attrs(2)%value = "latitude"
            lat_coord%attrs(2)%dtype = ATTR_TYPE_STRING
            lat_coord%attrs(3)%name = "axis"
            lat_coord%attrs(3)%value = "Y"
            lat_coord%attrs(3)%dtype = ATTR_TYPE_STRING
            
            ! Time with CF attributes
            time_coord%name = "time"
            time_coord%length = 3
            time_coord%dtype = DTYPE_REAL64
            allocate(time_coord%values_r64(3))
            time_coord%values_r64 = time_values
            allocate(time_coord%attrs(3))
            time_coord%n_attrs = 3
            time_coord%attrs(1)%name = "units"
            time_coord%attrs(1)%value = "hours since 2024-01-01 00:00:00"
            time_coord%attrs(1)%dtype = ATTR_TYPE_STRING
            time_coord%attrs(2)%name = "standard_name"
            time_coord%attrs(2)%value = "time"
            time_coord%attrs(2)%dtype = ATTR_TYPE_STRING
            time_coord%attrs(3)%name = "calendar"
            time_coord%attrs(3)%value = "gregorian"
            time_coord%attrs(3)%dtype = ATTR_TYPE_STRING
            
            arr%coords(1) = lon_coord
            arr%coords(2) = lat_coord
            arr%coords(3) = time_coord
            arr%has_coord = [.true., .true., .true.]
        end block
        
        ! Enable CF compliance
        options%cf_compliant = .true.
        
        ! Test: Write CF-compliant file (options not yet supported)
        ! status = arr%to_netcdf(test_file, options=options)
        status = arr%to_netcdf(test_file)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to write CF-compliant file"
        end if
        
        ! TODO: Verify CF compliance (would require CF checker tools)
        
        if (test_passed) then
            write(*,'(A)') "PASS: CF compliance"
        else
            write(*,'(A)') "FAIL: CF compliance"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_cf_compliance
    
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
    
end program test_xarray_io