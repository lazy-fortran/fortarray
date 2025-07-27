program test_netcdf
    use foxel_types
    use foxel_constructors
    use foxel_datasets
    use foxel_netcdf
    use foxel_indexing
    use foxel_memory
    use netcdf
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    character(len=256) :: test_file
    
    n_tests_passed = 0
    n_tests_total = 0
    
    ! Create test file name
    test_file = "test_data.nc"
    
    ! Create test NetCDF files
    call create_test_files()
    
    ! Test NetCDF operations
    call test_read_scalar_variable()
    call test_read_1d_variable()
    call test_read_2d_variable()
    call test_read_3d_variable()
    call test_read_with_coordinates()
    call test_read_with_attributes()
    call test_read_entire_dataset()
    call test_selective_reading()
    call test_metadata_reading()
    call test_list_operations()
    call test_unlimited_dimensions()
    call test_missing_values()
    call test_error_handling()
    
    ! Clean up test files
    call cleanup_test_files()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine create_test_files()
        integer :: ncid, status
        integer :: x_dimid, y_dimid, z_dimid, time_dimid
        integer :: temp_varid, pres_varid, vel_varid, scalar_varid
        integer :: lat_varid, lon_varid, time_varid
        real(real64), dimension(10) :: lat_data, time_data
        real(real64), dimension(15) :: lon_data
        real(real64), dimension(10, 15) :: temp_data
        real(real64), dimension(10, 15, 5) :: pres_data
        real(real32), dimension(10) :: vel_data
        real(real64) :: scalar_data
        integer :: i, j, k
        
        ! Create test data
        do i = 1, 10
            lat_data(i) = -90.0_real64 + (i-1) * 20.0_real64
            time_data(i) = real(i-1, real64) * 3600.0_real64  ! Hours since start
            vel_data(i) = real(i, real32) * 0.5_real32
        end do
        
        do i = 1, 15
            lon_data(i) = -180.0_real64 + (i-1) * 24.0_real64
        end do
        
        do j = 1, 15
            do i = 1, 10
                temp_data(i,j) = 273.15_real64 + real(i + j, real64)
            end do
        end do
        
        do k = 1, 5
            do j = 1, 15
                do i = 1, 10
                    pres_data(i,j,k) = 1013.25_real64 + real(i + j + k, real64) * 0.1_real64
                end do
            end do
        end do
        
        scalar_data = 42.0_real64
        
        ! Create NetCDF file
        status = nf90_create(test_file, NF90_CLOBBER, ncid)
        if (status /= NF90_NOERR) then
            write(error_unit,*) "Failed to create test file: ", trim(nf90_strerror(status))
            return
        end if
        
        ! Define dimensions
        status = nf90_def_dim(ncid, "lat", 10, x_dimid)
        status = nf90_def_dim(ncid, "lon", 15, y_dimid)
        status = nf90_def_dim(ncid, "level", 5, z_dimid)
        status = nf90_def_dim(ncid, "time", NF90_UNLIMITED, time_dimid)
        
        ! Define coordinate variables
        status = nf90_def_var(ncid, "lat", NF90_DOUBLE, x_dimid, lat_varid)
        status = nf90_def_var(ncid, "lon", NF90_DOUBLE, y_dimid, lon_varid)
        status = nf90_def_var(ncid, "time", NF90_DOUBLE, time_dimid, time_varid)
        
        ! Define data variables
        status = nf90_def_var(ncid, "temperature", NF90_DOUBLE, [x_dimid, y_dimid], temp_varid)
        status = nf90_def_var(ncid, "pressure", NF90_DOUBLE, [x_dimid, y_dimid, z_dimid], pres_varid)
        status = nf90_def_var(ncid, "velocity", NF90_FLOAT, x_dimid, vel_varid)
        status = nf90_def_var(ncid, "scalar_value", NF90_DOUBLE, scalar_varid)
        
        ! Add attributes
        status = nf90_put_att(ncid, NF90_GLOBAL, "title", "Test NetCDF file for foxel")
        status = nf90_put_att(ncid, NF90_GLOBAL, "version", "1.0")
        status = nf90_put_att(ncid, NF90_GLOBAL, "created_by", "test_netcdf")
        
        status = nf90_put_att(ncid, lat_varid, "units", "degrees_north")
        status = nf90_put_att(ncid, lat_varid, "long_name", "Latitude")
        
        status = nf90_put_att(ncid, lon_varid, "units", "degrees_east")
        status = nf90_put_att(ncid, lon_varid, "long_name", "Longitude")
        
        status = nf90_put_att(ncid, time_varid, "units", "seconds since 2025-01-01 00:00:00")
        status = nf90_put_att(ncid, time_varid, "calendar", "standard")
        
        status = nf90_put_att(ncid, temp_varid, "units", "K")
        status = nf90_put_att(ncid, temp_varid, "long_name", "Temperature")
        status = nf90_put_att(ncid, temp_varid, "missing_value", -999.0_real64)
        
        status = nf90_put_att(ncid, pres_varid, "units", "hPa")
        status = nf90_put_att(ncid, pres_varid, "long_name", "Pressure")
        
        status = nf90_put_att(ncid, vel_varid, "units", "m/s")
        status = nf90_put_att(ncid, vel_varid, "long_name", "Velocity")
        
        ! End define mode
        status = nf90_enddef(ncid)
        
        ! Write data
        status = nf90_put_var(ncid, lat_varid, lat_data)
        status = nf90_put_var(ncid, lon_varid, lon_data)
        status = nf90_put_var(ncid, time_varid, time_data(1:5))  ! Write 5 time steps
        status = nf90_put_var(ncid, temp_varid, temp_data)
        status = nf90_put_var(ncid, pres_varid, pres_data)
        status = nf90_put_var(ncid, vel_varid, vel_data)
        status = nf90_put_var(ncid, scalar_varid, scalar_data)
        
        ! Close file
        status = nf90_close(ncid)
        
        ! Create additional test files as needed
        call create_string_test_file()
        call create_group_test_file()
        
    end subroutine create_test_files
    
    subroutine create_string_test_file()
        ! Create a file with string variables (NetCDF4 feature)
        ! For now, skip if not supported
    end subroutine create_string_test_file
    
    subroutine create_group_test_file()
        ! Create a file with groups (NetCDF4 feature)
        ! For now, skip if not supported
    end subroutine create_group_test_file
    
    subroutine cleanup_test_files()
        integer :: unit, iostat
        
        ! Delete test files
        open(newunit=unit, file=test_file, status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
    end subroutine cleanup_test_files
    
    subroutine test_read_scalar_variable()
        type(variable_t) :: var
        logical :: test_passed
        integer :: stat
        character(len=256) :: error_msg
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read scalar variable
        var = read_netcdf_variable(test_file, "scalar_value", stat=stat, error_msg=error_msg)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A,A)') "Failed to read scalar variable: ", trim(error_msg)
        else if (.not. var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Scalar variable not initialized"
        else if (var%n_dims /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Scalar should have 0 dimensions, got ", var%n_dims
        else if (abs(var%data%values_r64(1) - 42.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A,F0.1)') "Wrong scalar value, got ", var%data%values_r64(1)
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read scalar variable test"
        else
            write(*,'(A)') "FAIL: Read scalar variable test"
        end if
    end subroutine test_read_scalar_variable
    
    subroutine test_read_1d_variable()
        type(variable_t) :: var
        logical :: test_passed
        integer :: stat, i
        real(real64) :: value, expected
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read 1D variable
        var = read_netcdf_variable(test_file, "velocity", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read 1D variable"
        else if (var%n_dims /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Should have 1 dimension, got ", var%n_dims
        else if (var%shape(1) /= 10) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Wrong shape, expected 10, got ", var%shape(1)
        else
            ! Check some values
            do i = 1, 3
                value = get_item(var, i)
                expected = real(i, real64) * 0.5_real64
                if (abs(value - expected) > 1.0e-5_real64) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,F0.3,A,F0.3)') &
                        "Wrong value at index ", i, ": expected ", expected, ", got ", value
                end if
            end do
        end if
        
        ! Check attributes
        if (var%n_attrs < 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Missing attributes"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read 1D variable test"
        else
            write(*,'(A)') "FAIL: Read 1D variable test"
        end if
    end subroutine test_read_1d_variable
    
    subroutine test_read_2d_variable()
        type(variable_t) :: var
        logical :: test_passed
        integer :: stat
        real(real64) :: value, expected
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read 2D variable
        var = read_netcdf_variable(test_file, "temperature", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read 2D variable"
        else if (.not. var%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Variable not initialized"
        else if (var%n_dims /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Should have 2 dimensions, got ", var%n_dims
            write(error_unit,'(A,L1)') "Initialized: ", var%initialized
            write(error_unit,'(A,A)') "Name: ", trim(var%name)
        else if (var%shape(1) /= 10 .or. var%shape(2) /= 15) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0)') "Wrong shape, expected (10,15), got (", &
                var%shape(1), ",", var%shape(2), ")"
        else
            ! Check a value
            value = get_item(var, 1, 1)
            expected = 273.15_real64 + 2.0_real64
            if (abs(value - expected) > epsilon(1.0_real64)) then
                test_passed = .false.
                write(error_unit,'(A,F0.3,A,F0.3)') "Wrong value at (1,1): expected ", &
                    expected, ", got ", value
            end if
        end if
        
        ! Check dimension names
        if (var%n_dims == 2) then
            if (trim(var%dim_names(1)) /= "lat" .or. trim(var%dim_names(2)) /= "lon") then
                test_passed = .false.
                write(error_unit,'(A)') "Wrong dimension names"
            end if
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read 2D variable test"
        else
            write(*,'(A)') "FAIL: Read 2D variable test"
        end if
    end subroutine test_read_2d_variable
    
    subroutine test_read_3d_variable()
        type(variable_t) :: var
        logical :: test_passed
        integer :: stat
        real(real64) :: value, expected
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read 3D variable
        var = read_netcdf_variable(test_file, "pressure", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read 3D variable"
        else if (var%n_dims /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Should have 3 dimensions, got ", var%n_dims
        else if (var%shape(1) /= 10 .or. var%shape(2) /= 15 .or. var%shape(3) /= 5) then
            test_passed = .false.
            write(error_unit,'(A)') "Wrong shape for 3D variable"
        else
            ! Check a value
            value = get_item(var, 1, 1, 1)
            expected = 1013.25_real64 + 3.0_real64 * 0.1_real64
            if (abs(value - expected) > epsilon(1.0_real64)) then
                test_passed = .false.
                write(error_unit,'(A,F0.3,A,F0.3)') "Wrong value at (1,1,1): expected ", &
                    expected, ", got ", value
            end if
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read 3D variable test"
        else
            write(*,'(A)') "FAIL: Read 3D variable test"
        end if
    end subroutine test_read_3d_variable
    
    subroutine test_read_with_coordinates()
        type(variable_t) :: var
        type(label_index_t) :: label_idx
        logical :: test_passed
        integer :: stat, i
        real(real64) :: value
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read variable with coordinates
        var = read_netcdf_variable(test_file, "temperature", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read variable"
        else
            ! Check if coordinates were loaded
            if (.not. var%has_coord(1) .or. .not. var%has_coord(2)) then
                test_passed = .false.
                write(error_unit,'(A)') "Coordinates not loaded"
            else
                ! Check coordinate values
                if (var%coords(1)%length /= 10) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Wrong coordinate length for dimension 1"
                end if
                
                ! Try label-based indexing using coordinate value
                label_idx = create_label_index(value=-90.0_real64)
                value = get_item(var, label_idx, create_label_index(value=-180.0_real64), stat=stat)
                if (stat /= 0) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Failed to use label-based indexing with coordinates"
                end if
            end if
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read with coordinates test"
        else
            write(*,'(A)') "FAIL: Read with coordinates test"
        end if
    end subroutine test_read_with_coordinates
    
    subroutine test_read_with_attributes()
        type(variable_t) :: var
        logical :: test_passed
        integer :: stat, i
        logical :: found_units, found_longname
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read variable
        var = read_netcdf_variable(test_file, "temperature", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read variable"
        else
            ! Check attributes
            found_units = .false.
            found_longname = .false.
            
            do i = 1, var%n_attrs
                if (trim(var%attrs(i)%name) == "units") then
                    found_units = .true.
                    if (trim(var%attrs(i)%value) /= "K") then
                        test_passed = .false.
                        write(error_unit,'(A)') "Wrong units attribute"
                    end if
                else if (trim(var%attrs(i)%name) == "long_name") then
                    found_longname = .true.
                    if (trim(var%attrs(i)%value) /= "Temperature") then
                        test_passed = .false.
                        write(error_unit,'(A)') "Wrong long_name attribute"
                    end if
                end if
            end do
            
            if (.not. found_units .or. .not. found_longname) then
                test_passed = .false.
                write(error_unit,'(A)') "Missing expected attributes"
            end if
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read with attributes test"
        else
            write(*,'(A)') "FAIL: Read with attributes test"
        end if
    end subroutine test_read_with_attributes
    
    subroutine test_read_entire_dataset()
        type(dataset_t) :: dset
        type(variable_t) :: var
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read entire file
        dset = read_netcdf(test_file, stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read dataset"
        else
            ! Check number of variables
            if (dset%n_vars < 4) then
                test_passed = .false.
                write(error_unit,'(A,I0)') "Expected at least 4 variables, got ", dset%n_vars
            end if
            
            ! Check dimensions
            if (dset%n_dims /= 4) then
                test_passed = .false.
                write(error_unit,'(A,I0)') "Expected 4 dimensions, got ", dset%n_dims
            end if
            
            ! Check global attributes
            if (dset%n_attrs < 3) then
                test_passed = .false.
                write(error_unit,'(A)') "Missing global attributes"
            end if
            
            ! Try to get a variable
            if (has_variable(dset, "temperature")) then
                var = get_variable(dset, "temperature", stat=stat)
                if (stat /= 0 .or. .not. var%initialized) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Failed to get temperature variable from dataset"
                end if
            else
                test_passed = .false.
                write(error_unit,'(A)') "Temperature variable not found in dataset"
            end if
        end if
        
        call finalize_dataset(dset)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Read entire dataset test"
        else
            write(*,'(A)') "FAIL: Read entire dataset test"
        end if
    end subroutine test_read_entire_dataset
    
    subroutine test_selective_reading()
        type(dataset_t) :: dset
        character(len=MAX_NAME_LEN), dimension(2) :: selected_vars
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read only selected variables
        selected_vars = ["temperature", "velocity   "]
        dset = read_netcdf(test_file, variables=selected_vars, stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read dataset with selected variables"
        else
            ! Check that only selected variables were loaded
            if (dset%n_vars /= 2) then
                test_passed = .false.
                write(error_unit,'(A,I0)') "Expected 2 variables, got ", dset%n_vars
            end if
            
            if (.not. has_variable(dset, "temperature")) then
                test_passed = .false.
                write(error_unit,'(A)') "Temperature not loaded"
            end if
            
            if (.not. has_variable(dset, "velocity")) then
                test_passed = .false.
                write(error_unit,'(A)') "Velocity not loaded"
            end if
            
            if (has_variable(dset, "pressure")) then
                test_passed = .false.
                write(error_unit,'(A)') "Pressure should not have been loaded"
            end if
        end if
        
        call finalize_dataset(dset)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Selective reading test"
        else
            write(*,'(A)') "FAIL: Selective reading test"
        end if
    end subroutine test_selective_reading
    
    subroutine test_metadata_reading()
        type(dataset_t) :: metadata
        logical :: test_passed
        integer :: stat, i
        logical :: found_dim
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read metadata only
        metadata = read_netcdf_metadata(test_file, stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read metadata"
        else
            ! Check dimensions
            found_dim = .false.
            do i = 1, metadata%n_dims
                if (trim(metadata%dimensions(i)%name) == "time") then
                    found_dim = .true.
                    if (.not. metadata%dimensions(i)%is_unlimited) then
                        test_passed = .false.
                        write(error_unit,'(A)') "Time dimension should be unlimited"
                    end if
                end if
            end do
            
            if (.not. found_dim) then
                test_passed = .false.
                write(error_unit,'(A)') "Time dimension not found"
            end if
            
            ! Check that no actual data was loaded
            if (allocated(metadata%variables)) then
                if (size(metadata%variables) > 0) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Data should not be loaded in metadata-only mode"
                end if
            end if
        end if
        
        call finalize_dataset(metadata)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Metadata reading test"
        else
            write(*,'(A)') "FAIL: Metadata reading test"
        end if
    end subroutine test_metadata_reading
    
    subroutine test_list_operations()
        character(len=MAX_NAME_LEN), dimension(:), allocatable :: varnames
        type(dimension_t), dimension(:), allocatable :: dims
        type(attribute_t), dimension(:), allocatable :: attrs
        logical :: test_passed
        integer :: stat, i
        logical :: found_var, found_attr
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! List variables
        varnames = list_netcdf_variables(test_file, stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to list variables"
        else if (size(varnames) < 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Too few variables listed"
        else
            found_var = .false.
            do i = 1, size(varnames)
                if (trim(varnames(i)) == "temperature") found_var = .true.
            end do
            if (.not. found_var) then
                test_passed = .false.
                write(error_unit,'(A)') "Temperature variable not in list"
            end if
        end if
        
        ! List dimensions
        dims = list_netcdf_dimensions(test_file, stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to list dimensions"
        else if (size(dims) /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Expected 4 dimensions, got ", size(dims)
        end if
        
        ! List attributes
        attrs = list_netcdf_attributes(test_file, stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to list attributes"
        else
            found_attr = .false.
            do i = 1, size(attrs)
                if (trim(attrs(i)%name) == "title") then
                    found_attr = .true.
                    if (trim(attrs(i)%value) /= "Test NetCDF file for foxel") then
                        test_passed = .false.
                        write(error_unit,'(A)') "Wrong title attribute value"
                    end if
                end if
            end do
            if (.not. found_attr) then
                test_passed = .false.
                write(error_unit,'(A)') "Title attribute not found"
            end if
        end if
        
        ! Clean up
        if (allocated(varnames)) deallocate(varnames)
        if (allocated(dims)) deallocate(dims)
        if (allocated(attrs)) deallocate(attrs)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: List operations test"
        else
            write(*,'(A)') "FAIL: List operations test"
        end if
    end subroutine test_list_operations
    
    subroutine test_unlimited_dimensions()
        type(dataset_t) :: dset
        logical :: test_passed
        integer :: stat, i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read dataset
        dset = read_netcdf(test_file, stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read dataset"
        else
            ! Check for unlimited dimension
            do i = 1, dset%n_dims
                if (trim(dset%dimensions(i)%name) == "time") then
                    if (.not. dset%dimensions(i)%is_unlimited) then
                        test_passed = .false.
                        write(error_unit,'(A)') "Time dimension should be unlimited"
                    end if
                    ! Note: Actual length might be less than declared unlimited
                    if (dset%dimensions(i)%length /= 5) then
                        test_passed = .false.
                        write(error_unit,'(A,I0)') "Expected 5 time steps, got ", &
                            dset%dimensions(i)%length
                    end if
                end if
            end do
        end if
        
        call finalize_dataset(dset)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Unlimited dimensions test"
        else
            write(*,'(A)') "FAIL: Unlimited dimensions test"
        end if
    end subroutine test_unlimited_dimensions
    
    subroutine test_missing_values()
        type(variable_t) :: var
        logical :: test_passed
        integer :: stat, i
        logical :: found_missing_attr
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Read variable with missing value attribute
        var = read_netcdf_variable(test_file, "temperature", stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to read variable"
        else
            ! Check for missing_value attribute
            found_missing_attr = .false.
            do i = 1, var%n_attrs
                if (trim(var%attrs(i)%name) == "missing_value") then
                    found_missing_attr = .true.
                    ! Check if the value starts with "-999" (may have decimal point)
                    if (index(var%attrs(i)%value, "-999") /= 1) then
                        test_passed = .false.
                        write(error_unit,'(A,A,A)') "Wrong missing_value attribute: expected '-999', got '", &
                            trim(var%attrs(i)%value), "'"
                    end if
                end if
            end do
            
            if (.not. found_missing_attr) then
                test_passed = .false.
                write(error_unit,'(A)') "Missing value attribute not found"
            end if
            
            ! Note: Actual missing value handling would be implemented in Phase 3
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Missing values test"
        else
            write(*,'(A)') "FAIL: Missing values test"
        end if
    end subroutine test_missing_values
    
    subroutine test_error_handling()
        type(variable_t) :: var
        type(dataset_t) :: dset
        logical :: test_passed
        integer :: stat
        character(len=256) :: error_msg
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Try to read non-existent file
        var = read_netcdf_variable("nonexistent.nc", "temperature", stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for non-existent file"
        end if
        
        ! Try to read non-existent variable
        var = read_netcdf_variable(test_file, "nonexistent", stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Should fail for non-existent variable, stat=", stat
        end if
        
        ! Try selective read with non-existent variable
        dset = read_netcdf(test_file, variables=["nonexistent"], stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for non-existent variable in selection"
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Error handling test"
        else
            write(*,'(A)') "FAIL: Error handling test"
        end if
    end subroutine test_error_handling
    
end program test_netcdf