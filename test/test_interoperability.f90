program test_interoperability
    use iso_fortran_env, only: real64, int32, int64, error_unit
    use fortarray_types
    use fortarray_constructors
    use fortarray_io
    use fortarray_datasets
    use fortarray_csv, only: write_csv_variable
    use fortarray_interoperability
    implicit none
    
    logical :: all_tests_passed = .true.
    
    write(*,'(A)') "Testing interoperability layer..."
    write(*,'(A)') "=================================="
    
    call test_to_pandas_basic()
    call test_to_pandas_with_index()
    call test_to_pandas_multiindex()
    call test_pandas_compatible_csv()
    call test_data_exchange_utilities()
    call test_table_style_operations()
    call test_metadata_preservation()
    call test_coordinate_export()
    call test_datetime_interop()
    call test_mixed_types_export()
    
    if (all_tests_passed) then
        write(*,'(A)') "=================================="
        write(*,'(A)') "All interoperability tests passed!"
    else
        write(error_unit,'(A)') "=================================="
        write(error_unit,'(A)') "Some interoperability tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_to_pandas_basic()
        type(fortarray_t) :: arr, pandas_arr
        real(real64), dimension(5, 3) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test data
        do j = 1, 3
            do i = 1, 5
                data(i, j) = real(i + j, real64)
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "rows", "cols"], &
                       name="test_data")
        
        ! Add metadata
        arr%units = "meters"
        arr%long_name = "Test measurement data"
        
        ! Test: Convert to pandas-compatible format
        pandas_arr = arr%to_pandas()
        
        if (.not. pandas_arr%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to convert to pandas format"
        else
            ! Check pandas-specific attributes were added
            if (.not. has_pandas_attributes(pandas_arr)) then
                test_passed = .false.
                write(error_unit,'(A)') "Missing pandas-specific attributes"
            end if
            
            ! Check data integrity
            if (pandas_arr%shape(1) /= 5 .or. pandas_arr%shape(2) /= 3) then
                test_passed = .false.
                write(error_unit,'(A)') "Shape mismatch in pandas conversion"
            end if
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: to_pandas basic conversion"
        else
            write(*,'(A)') "FAIL: to_pandas basic conversion"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(pandas_arr)
    end subroutine test_to_pandas_basic
    
    subroutine test_to_pandas_with_index()
        type(fortarray_t) :: arr, pandas_arr
        real(real64), dimension(10) :: data, index_values
        integer :: i, status
        logical :: test_passed = .true.
        
        ! Create test data with custom index
        do i = 1, 10
            data(i) = real(i, real64) ** 2
            index_values(i) = real(i, real64) * 0.5_real64
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "time"], name="timeseries")
        
        ! Add time coordinate
        block
            type(coordinate_t) :: time_coord
            
            time_coord%name = "time"
            time_coord%length = 10
            time_coord%dtype = DTYPE_REAL64
            allocate(time_coord%values_r64(10))
            time_coord%values_r64 = index_values
            
            arr%coords(1) = time_coord
            arr%has_coord(1) = .true.
        end block
        
        ! Test: Convert with index
        pandas_arr = arr%to_pandas(index_from_coords=.true.)
        
        if (.not. pandas_arr%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to convert with index"
        else
            ! Check index was preserved
            if (.not. pandas_arr%has_coord(1)) then
                test_passed = .false.
                write(error_unit,'(A)') "Index coordinates lost in conversion"
            else if (abs(pandas_arr%coords(1)%values_r64(5) - 2.5_real64) > 1.0e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Index values not preserved"
            end if
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: to_pandas with index"
        else
            write(*,'(A)') "FAIL: to_pandas with index"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(pandas_arr)
    end subroutine test_to_pandas_with_index
    
    subroutine test_to_pandas_multiindex()
        type(fortarray_t) :: arr, pandas_arr
        real(real64), dimension(4, 3, 2) :: data
        integer :: i, j, k, status
        logical :: test_passed = .true.
        
        ! Create 3D test data
        do k = 1, 2
            do j = 1, 3
                do i = 1, 4
                    data(i, j, k) = real(i + j + k, real64)
                end do
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y", "z"], &
                       name="volume_data")
        
        ! Test: Convert to pandas with MultiIndex
        pandas_arr = arr%to_pandas(flatten_multiindex=.true.)
        
        if (.not. pandas_arr%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to convert with MultiIndex"
        else
            ! Check data was flattened appropriately
            if (pandas_arr%n_dims /= 2) then
                test_passed = .false.
                write(error_unit,'(A)') "MultiIndex not flattened to 2D"
            end if
            
            ! Check total elements preserved
            if (pandas_arr%n_elements /= 24) then
                test_passed = .false.
                write(error_unit,'(A,I0)') "Element count mismatch, got: ", pandas_arr%n_elements
            end if
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: to_pandas with MultiIndex"
        else
            write(*,'(A)') "FAIL: to_pandas with MultiIndex"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(pandas_arr)
    end subroutine test_to_pandas_multiindex
    
    subroutine test_pandas_compatible_csv()
        type(fortarray_t) :: arr
        real(real64), dimension(5, 4) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        character(len=256) :: filename = "test_pandas_compat.csv"
        
        ! Create test data
        do j = 1, 4
            do i = 1, 5
                data(i, j) = real(i, real64) + real(j, real64) * 0.1_real64
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "index", "A"], name="dataframe")
        
        ! Add column names and row index
        block
            type(coordinate_t) :: row_coord, col_coord
            character(len=10), dimension(5) :: row_names
            character(len=10), dimension(4) :: col_names
            
            ! Row names
            do i = 1, 5
                write(row_names(i), '(A,I0)') "row_", i
            end do
            
            ! Column names  
            col_names = [character(len=10) :: "A", "B", "C", "D"]
            
            row_coord%name = "index"
            row_coord%length = 5
            row_coord%dtype = DTYPE_CHAR
            allocate(character(len=10) :: row_coord%values_char(5))
            row_coord%values_char = row_names
            
            col_coord%name = "columns"
            col_coord%length = 4
            col_coord%dtype = DTYPE_CHAR
            allocate(character(len=10) :: col_coord%values_char(4))
            col_coord%values_char = col_names
            
            arr%coords(1) = row_coord
            arr%coords(2) = col_coord
            arr%has_coord = [.true., .true.]
        end block
        
        ! Test: Export as pandas-compatible CSV
        status = export_pandas_csv(filename, arr)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to export pandas-compatible CSV"
        else
            ! Check file was created and has expected format
            block
                logical :: file_exists
                integer :: unit, iostat
                character(len=256) :: line
                
                inquire(file=filename, exist=file_exists)
                if (.not. file_exists) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Pandas CSV file not created"
                else
                    ! Check header format
                    open(newunit=unit, file=filename, status="old", iostat=iostat)
                    if (iostat == 0) then
                        read(unit, '(A)', iostat=iostat) line
                        close(unit)
                        
                        if (index(line, "index,A,B,C,D") == 0) then
                            test_passed = .false.
                            write(error_unit,'(A)') "Incorrect CSV header format"
                        end if
                    end if
                end if
            end block
        end if
        
        call execute_command_line("rm -f " // trim(filename))
        
        if (test_passed) then
            write(*,'(A)') "PASS: pandas-compatible CSV export"
        else
            write(*,'(A)') "FAIL: pandas-compatible CSV export"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_pandas_compatible_csv
    
    subroutine test_data_exchange_utilities()
        type(fortarray_t) :: arr
        real(real64), dimension(3, 3) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test data
        data = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0], [3, 3])
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], name="matrix")
        
        ! Test: Export to various exchange formats
        
        ! JSON export
        status = export_json("test_exchange.json", arr)
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to export JSON"
        end if
        
        ! XML export
        status = export_xml("test_exchange.xml", arr)
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to export XML"
        end if
        
        ! HDF5 exchange format
        status = export_hdf5_exchange("test_exchange.h5", arr)
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to export HDF5 exchange format"
        end if
        
        ! Test: Import from exchange format
        block
            type(fortarray_t) :: imported
            imported = import_json("test_exchange.json")
            
            if (.not. imported%initialized) then
                test_passed = .false.
                write(error_unit,'(A)') "Failed to import from JSON"
            else if (imported%shape(1) /= 3 .or. imported%shape(2) /= 3) then
                test_passed = .false.
                write(error_unit,'(A)') "Imported data shape mismatch"
            end if
            
            call finalize_fortarray(imported)
        end block
        
        call execute_command_line("rm -f test_exchange.*")
        
        if (test_passed) then
            write(*,'(A)') "PASS: data exchange utilities"
        else
            write(*,'(A)') "FAIL: data exchange utilities"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_data_exchange_utilities
    
    subroutine test_table_style_operations()
        type(fortarray_t) :: arr, result
        real(real64), dimension(6, 3) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create tabular test data
        do j = 1, 3
            do i = 1, 6
                data(i, j) = real(i, real64) + real(j, real64) * 10.0_real64
            end do
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "rows", "cols"], &
                       name="table")
        
        ! Test: Table-style head operation
        result = arr%head(n=3)
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "head() operation failed"
        else if (result%shape(1) /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "head() wrong size, got: ", result%shape(1)
        end if
        
        call finalize_fortarray(result)
        
        ! Test: Table-style tail operation
        result = arr%tail(n=2)
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "tail() operation failed"
        else if (result%shape(1) /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "tail() wrong size, got: ", result%shape(1)
        end if
        
        call finalize_fortarray(result)
        
        ! Test: Table-style describe operation
        result = arr%describe()
        
        if (.not. result%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "describe() operation failed"
        else
            ! Check describe output has expected statistics
            if (result%shape(1) < 5) then  ! count, mean, std, min, max at minimum
                test_passed = .false.
                write(error_unit,'(A)') "describe() missing statistics"
            end if
        end if
        
        call finalize_fortarray(result)
        
        if (test_passed) then
            write(*,'(A)') "PASS: table-style operations"
        else
            write(*,'(A)') "FAIL: table-style operations"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_table_style_operations
    
    subroutine test_metadata_preservation()
        type(fortarray_t) :: arr, converted
        real(real64), dimension(4, 2) :: data
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test data with rich metadata
        data = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], [4, 2])
        arr = new_array(data, dim_names=[character(len=10) :: "time", "space"], &
                       name="sensor_data")
        
        ! Add comprehensive metadata
        arr%units = "temperature_celsius"
        arr%long_name = "Hourly temperature measurements"
        arr%standard_name = "air_temperature"
        
        ! Test: Metadata preservation in pandas conversion
        converted = arr%to_pandas(preserve_metadata=.true.)
        
        if (.not. converted%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Metadata preservation conversion failed"
        else
            ! Check all metadata was preserved
            if (trim(converted%units) /= "temperature_celsius") then
                test_passed = .false.
                write(error_unit,'(A)') "Units metadata lost"
            end if
            
            if (trim(converted%long_name) /= "Hourly temperature measurements") then
                test_passed = .false.
                write(error_unit,'(A)') "Long name metadata lost"
            end if
            
            if (trim(converted%standard_name) /= "air_temperature") then
                test_passed = .false.
                write(error_unit,'(A)') "Standard name metadata lost"
            end if
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: metadata preservation"
        else
            write(*,'(A)') "FAIL: metadata preservation"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(converted)
    end subroutine test_metadata_preservation
    
    subroutine test_coordinate_export()
        type(fortarray_t) :: arr
        real(real64), dimension(5, 3) :: data
        real(real64), dimension(5) :: x_coords
        real(real64), dimension(3) :: y_coords
        integer :: i, j, status
        logical :: test_passed = .true.
        
        ! Create test data with coordinates
        do j = 1, 3
            do i = 1, 5
                data(i, j) = sin(real(i, real64)) * cos(real(j, real64))
                x_coords(i) = real(i, real64) * 0.5_real64
            end do
            y_coords(j) = real(j, real64) * 1.5_real64
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "x", "y"], name="field")
        
        ! Add coordinates
        block
            type(coordinate_t) :: x_coord, y_coord
            
            x_coord%name = "x"
            x_coord%length = 5
            x_coord%dtype = DTYPE_REAL64
            allocate(x_coord%values_r64(5))
            x_coord%values_r64 = x_coords
            
            y_coord%name = "y"
            y_coord%length = 3
            y_coord%dtype = DTYPE_REAL64
            allocate(y_coord%values_r64(3))
            y_coord%values_r64 = y_coords
            
            arr%coords(1) = x_coord
            arr%coords(2) = y_coord
            arr%has_coord = [.true., .true.]
        end block
        
        ! Test: Export coordinates to separate files
        status = export_coordinates_csv("test_coords.csv", arr)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to export coordinates"
        else
            ! Check coordinate file was created
            block
                logical :: file_exists
                inquire(file="test_coords.csv", exist=file_exists)
                if (.not. file_exists) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Coordinate file not created"
                end if
            end block
        end if
        
        ! Test: Export as long-form data (melted)
        status = export_long_form_csv("test_long.csv", arr)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to export long-form CSV"
        end if
        
        call execute_command_line("rm -f test_coords.csv test_long.csv")
        
        if (test_passed) then
            write(*,'(A)') "PASS: coordinate export"
        else
            write(*,'(A)') "FAIL: coordinate export"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
    end subroutine test_coordinate_export
    
    subroutine test_datetime_interop()
        type(fortarray_t) :: arr, converted
        real(real64), dimension(7) :: data
        integer :: i, status
        logical :: test_passed = .true.
        
        ! Create time series data
        do i = 1, 7
            data(i) = sin(real(i, real64) * 0.5_real64)
        end do
        
        arr = new_array(data, dim_names=[character(len=10) :: "time"], name="timeseries")
        
        ! Add datetime coordinate (days since epoch)
        block
            type(coordinate_t) :: time_coord
            real(real64), dimension(7) :: time_values
            
            do i = 1, 7
                time_values(i) = real(18000 + i, real64)  ! Days since 1970-01-01
            end do
            
            time_coord%name = "time"
            time_coord%length = 7
            time_coord%dtype = DTYPE_REAL64
            allocate(time_coord%values_r64(7))
            time_coord%values_r64 = time_values
            
            arr%coords(1) = time_coord
            arr%has_coord(1) = .true.
        end block
        
        ! Test: Convert datetime for pandas compatibility
        converted = arr%to_pandas(datetime_index=.true.)
        
        if (.not. converted%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Datetime conversion failed"
        else
            ! Check datetime formatting was applied
            if (.not. has_datetime_formatting(converted)) then
                test_passed = .false.
                write(error_unit,'(A)') "Datetime formatting not applied"
            end if
        end if
        
        if (test_passed) then
            write(*,'(A)') "PASS: datetime interoperability"
        else
            write(*,'(A)') "FAIL: datetime interoperability"
            all_tests_passed = .false.
        end if
        
        call finalize_fortarray(arr)
        call finalize_fortarray(converted)
    end subroutine test_datetime_interop
    
    subroutine test_mixed_types_export()
        type(dataset_t) :: ds
        type(fortarray_t) :: numeric_var, string_var
        real(real64), dimension(4) :: numeric_data
        character(len=10), dimension(4) :: string_data
        integer :: status
        logical :: test_passed = .true.
        
        ! Create mixed-type dataset
        numeric_data = [1.0_real64, 2.5_real64, 3.7_real64, 4.2_real64]
        string_data = [character(len=10) :: "apple", "banana", "cherry", "date"]
        
        numeric_var = new_array(numeric_data, dim_names=[character(len=10) :: "index"], &
                               name="values")
        
        ! Create string array (would need implementation)
        string_var%initialized = .true.
        string_var%name = "labels"
        string_var%n_dims = 1
        string_var%n_elements = 4
        allocate(string_var%dim_names(1), string_var%shape(1))
        string_var%dim_names(1) = "index"
        string_var%shape(1) = 4
        string_var%data%dtype = DTYPE_CHAR
        allocate(character(len=10) :: string_var%data%values_char(4))
        string_var%data%values_char = string_data
        
        ! Create dataset
        ds = new_dataset()
        allocate(ds%variables(2))
        ds%variables(1) = numeric_var
        ds%variables(2) = string_var
        ds%n_vars = 2
        allocate(ds%var_names(2))
        ds%var_names = [character(len=64) :: "values", "labels"]
        ds%initialized = .true.
        
        ! Test: Export mixed types to pandas-compatible format
        status = export_dataset_pandas_csv("test_mixed.csv", ds)
        
        if (status /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to export mixed-type dataset"
        else
            ! Check file was created
            block
                logical :: file_exists
                inquire(file="test_mixed.csv", exist=file_exists)
                if (.not. file_exists) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Mixed-type CSV file not created"
                end if
            end block
        end if
        
        call execute_command_line("rm -f test_mixed.csv")
        
        if (test_passed) then
            write(*,'(A)') "PASS: mixed types export"
        else
            write(*,'(A)') "FAIL: mixed types export"
            all_tests_passed = .false.
        end if
        
        call finalize_dataset(ds)
    end subroutine test_mixed_types_export
    
    
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
    
end program test_interoperability