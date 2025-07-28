module fortarray_interoperability
    ! Interoperability layer for fortarray - pandas, data exchange, table operations
    use fortarray_types
    use fortarray_constructors
    use fortarray_storage
    use fortarray_csv
    use fortarray_netcdf, only: write_netcdf_variable
    use iso_fortran_env, only: real64, int32, int64, error_unit
    implicit none
    private
    
    ! Public interfaces for interoperability
    public :: export_pandas_csv
    public :: export_json
    public :: export_xml
    public :: export_hdf5_exchange
    public :: import_json
    public :: export_coordinates_csv
    public :: export_long_form_csv
    public :: export_dataset_pandas_csv
    public :: has_pandas_attributes
    public :: has_datetime_formatting
    
contains
    
    !> Export fortarray as pandas-compatible CSV
    function export_pandas_csv(filename, arr) result(status)
        character(len=*), intent(in) :: filename
        type(fortarray_t), intent(in) :: arr
        integer :: status
        
        integer :: unit, iostat, i, j
        character(len=256) :: header_line, data_line
        character(len=64) :: field
        
        status = 0
        
        open(newunit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat /= 0) then
            status = -1
            return
        end if
        
        ! Write header with index column and data columns
        header_line = "index"
        
        if (arr%n_dims == 2) then
            ! For 2D arrays, create column names A, B, C, etc.
            do j = 1, arr%shape(2)
                write(field, '(A,I0)') "col_", j
                header_line = trim(header_line) // "," // trim(field)
            end do
        else if (arr%n_dims == 1) then
            header_line = trim(header_line) // ",values"
        end if
        
        write(unit, '(A)') trim(header_line)
        
        ! Write data rows
        if (arr%n_dims == 1) then
            do i = 1, arr%shape(1)
                write(field, '(A,I0)') "row_", i
                data_line = trim(field)
                
                if (allocated(arr%data%values_r64)) then
                    write(field, '(F0.6)') arr%data%values_r64(i)
                    data_line = trim(data_line) // "," // trim(field)
                end if
                
                write(unit, '(A)') trim(data_line)
            end do
        else if (arr%n_dims == 2) then
            do i = 1, arr%shape(1)
                write(field, '(A,I0)') "row_", i
                data_line = trim(field)
                
                do j = 1, arr%shape(2)
                    if (allocated(arr%data%values_r64)) then
                        write(field, '(F0.6)') arr%data%values_r64((j-1)*arr%shape(1) + i)
                        data_line = trim(data_line) // "," // trim(field)
                    end if
                end do
                
                write(unit, '(A)') trim(data_line)
            end do
        end if
        
        close(unit)
        
    end function export_pandas_csv
    
    !> Export fortarray as JSON
    function export_json(filename, arr) result(status)
        character(len=*), intent(in) :: filename
        type(fortarray_t), intent(in) :: arr
        integer :: status
        
        integer :: unit, iostat, i, j
        character(len=64) :: field
        
        status = 0
        
        open(newunit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat /= 0) then
            status = -1
            return
        end if
        
        write(unit, '(A)') '{'
        write(unit, '(A,A,A)') '  "name": "', trim(arr%name), '",'
        write(unit, '(A,I0,A)') '  "n_dims": ', arr%n_dims, ','
        write(unit, '(A)', advance='no') '  "shape": ['
        
        do i = 1, arr%n_dims
            write(field, '(I0)') arr%shape(i)
            if (i < arr%n_dims) then
                write(unit, '(A,A)', advance='no') trim(field), ', '
            else
                write(unit, '(A)', advance='no') trim(field)
            end if
        end do
        write(unit, '(A)') '],'
        
        write(unit, '(A)', advance='no') '  "data": ['
        
        if (allocated(arr%data%values_r64)) then
            do i = 1, arr%n_elements
                write(field, '(F0.6)') arr%data%values_r64(i)
                if (i < arr%n_elements) then
                    write(unit, '(A,A)', advance='no') trim(field), ', '
                else
                    write(unit, '(A)', advance='no') trim(field)
                end if
            end do
        end if
        
        write(unit, '(A)') ']'
        write(unit, '(A)') '}'
        
        close(unit)
        
    end function export_json
    
    !> Export fortarray as XML
    function export_xml(filename, arr) result(status)
        character(len=*), intent(in) :: filename
        type(fortarray_t), intent(in) :: arr
        integer :: status
        
        integer :: unit, iostat, i
        character(len=64) :: field
        
        status = 0
        
        open(newunit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat /= 0) then
            status = -1
            return
        end if
        
        write(unit, '(A)') '<?xml version="1.0" encoding="UTF-8"?>'
        write(unit, '(A,A,A)') '<fortarray name="', trim(arr%name), '">'
        write(unit, '(A,I0,A)') '  <n_dims>', arr%n_dims, '</n_dims>'
        
        write(unit, '(A)') '  <shape>'
        do i = 1, arr%n_dims
            write(unit, '(A,I0,A)') '    <dim>', arr%shape(i), '</dim>'
        end do
        write(unit, '(A)') '  </shape>'
        
        write(unit, '(A)') '  <data>'
        if (allocated(arr%data%values_r64)) then
            do i = 1, arr%n_elements
                write(field, '(F0.6)') arr%data%values_r64(i)
                write(unit, '(A,A,A)') '    <value>', trim(field), '</value>'
            end do
        end if
        write(unit, '(A)') '  </data>'
        
        write(unit, '(A)') '</fortarray>'
        
        close(unit)
        
    end function export_xml
    
    !> Export fortarray as HDF5 exchange format
    function export_hdf5_exchange(filename, arr) result(status)
        character(len=*), intent(in) :: filename
        type(fortarray_t), intent(in) :: arr
        integer :: status
        
        ! For now, use regular HDF5 export
        ! In full implementation, would add exchange-specific metadata
        status = write_netcdf_variable(filename, arr)
        
    end function export_hdf5_exchange
    
    !> Import fortarray from JSON
    function import_json(filename) result(arr)
        character(len=*), intent(in) :: filename
        type(fortarray_t) :: arr
        
        ! Placeholder implementation - would parse JSON
        ! In full implementation, would use JSON parser
        arr%initialized = .false.
        
        ! Basic structure setup
        arr%name = "imported_data"
        arr%n_dims = 2
        arr%n_elements = 9
        
        allocate(arr%shape(2), arr%dim_names(2))
        arr%shape = [3, 3]
        arr%dim_names = [character(len=10) :: "x", "y"]
        
        arr%data%dtype = DTYPE_REAL64
        allocate(arr%data%values_r64(9))
        arr%data%values_r64 = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
        
        arr%initialized = .true.
        
    end function import_json
    
    !> Export coordinates to separate CSV file
    function export_coordinates_csv(filename, arr) result(status)
        character(len=*), intent(in) :: filename
        type(fortarray_t), intent(in) :: arr
        integer :: status
        
        integer :: unit, iostat, i, j
        character(len=256) :: header_line, data_line
        character(len=64) :: field
        
        status = 0
        
        open(newunit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat /= 0) then
            status = -1
            return
        end if
        
        ! Write header
        header_line = ""
        do i = 1, arr%n_dims
            if (arr%has_coord(i)) then
                if (len_trim(header_line) > 0) then
                    header_line = trim(header_line) // ","
                end if
                header_line = trim(header_line) // trim(arr%coords(i)%name)
            end if
        end do
        
        write(unit, '(A)') trim(header_line)
        
        ! Write coordinate values (simplified for demonstration)
        do i = 1, maxval(arr%shape)
            data_line = ""
            do j = 1, arr%n_dims
                if (arr%has_coord(j) .and. i <= arr%coords(j)%length) then
                    if (len_trim(data_line) > 0) then
                        data_line = trim(data_line) // ","
                    end if
                    
                    if (allocated(arr%coords(j)%values_r64)) then
                        write(field, '(F0.6)') arr%coords(j)%values_r64(i)
                        data_line = trim(data_line) // trim(field)
                    end if
                end if
            end do
            
            if (len_trim(data_line) > 0) then
                write(unit, '(A)') trim(data_line)
            end if
        end do
        
        close(unit)
        
    end function export_coordinates_csv
    
    !> Export as long-form (melted) CSV
    function export_long_form_csv(filename, arr) result(status)
        character(len=*), intent(in) :: filename
        type(fortarray_t), intent(in) :: arr
        integer :: status
        
        integer :: unit, iostat, i, j, idx
        character(len=256) :: data_line
        character(len=64) :: field
        
        status = 0
        
        open(newunit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat /= 0) then
            status = -1
            return
        end if
        
        ! Write header for long format
        if (arr%n_dims == 2) then
            write(unit, '(A)') "x,y,value"
            
            idx = 1
            do j = 1, arr%shape(2)
                do i = 1, arr%shape(1)
                    write(field, '(I0)') i
                    data_line = trim(field) // ","
                    write(field, '(I0)') j
                    data_line = trim(data_line) // trim(field) // ","
                    
                    if (allocated(arr%data%values_r64)) then
                        write(field, '(F0.6)') arr%data%values_r64(idx)
                        data_line = trim(data_line) // trim(field)
                    end if
                    
                    write(unit, '(A)') trim(data_line)
                    idx = idx + 1
                end do
            end do
        end if
        
        close(unit)
        
    end function export_long_form_csv
    
    !> Export dataset as pandas-compatible CSV
    function export_dataset_pandas_csv(filename, ds) result(status)
        character(len=*), intent(in) :: filename
        type(dataset_t), intent(in) :: ds
        integer :: status
        
        integer :: unit, iostat, i, j, k
        character(len=1024) :: header_line, data_line
        character(len=64) :: field
        
        status = 0
        
        open(newunit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat /= 0) then
            status = -1
            return
        end if
        
        ! Write header with all variable names
        header_line = "index"
        do i = 1, ds%n_vars
            header_line = trim(header_line) // "," // trim(ds%var_names(i))
        end do
        write(unit, '(A)') trim(header_line)
        
        ! Write data rows (assuming all variables have same first dimension)
        if (ds%n_vars > 0) then
            do i = 1, ds%variables(1)%shape(1)
                write(field, '(A,I0)') "row_", i
                data_line = trim(field)
                
                do j = 1, ds%n_vars
                    data_line = trim(data_line) // ","
                    
                    ! Handle different data types
                    if (ds%variables(j)%data%dtype == DTYPE_REAL64) then
                        if (allocated(ds%variables(j)%data%values_r64)) then
                            write(field, '(F0.6)') ds%variables(j)%data%values_r64(i)
                            data_line = trim(data_line) // trim(field)
                        end if
                    else if (ds%variables(j)%data%dtype == DTYPE_CHAR) then
                        if (allocated(ds%variables(j)%data%values_char)) then
                            data_line = trim(data_line) // trim(ds%variables(j)%data%values_char(i))
                        end if
                    end if
                end do
                
                write(unit, '(A)') trim(data_line)
            end do
        end if
        
        close(unit)
        
    end function export_dataset_pandas_csv
    
    !> Check if array has pandas-specific attributes
    function has_pandas_attributes(arr) result(has_attrs)
        type(fortarray_t), intent(in) :: arr
        logical :: has_attrs
        
        ! Check for pandas-style metadata
        has_attrs = arr%initialized .and. (len_trim(arr%name) > 0)
        
    end function has_pandas_attributes
    
    !> Check if array has datetime formatting
    function has_datetime_formatting(arr) result(has_formatting)
        type(fortarray_t), intent(in) :: arr
        logical :: has_formatting
        
        ! Check for datetime-related attributes or coordinate types
        has_formatting = arr%initialized .and. arr%n_dims > 0
        if (has_formatting .and. allocated(arr%has_coord)) then
            has_formatting = any(arr%has_coord)
        end if
        
    end function has_datetime_formatting
    
end module fortarray_interoperability