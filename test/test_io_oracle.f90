program test_io_oracle
    use fortarray, only: dp, data_array_t, FORTARRAY_SUCCESS
    use fortarray_io, only: read_netcdf, write_netcdf
    implicit none

    type(data_array_t) :: field
    integer :: command_status, stat

    call execute_command_line( &
        "ncgen -o fortarray-oracle-input.nc test/fixtures/itp_field.cdl", &
        exitstat=command_status)
    if (command_status /= 0) error stop "ncgen could not create the independent fixture"

    field = read_netcdf("fortarray-oracle-input.nc", "potential", stat)
    if (stat /= FORTARRAY_SUCCESS) error stop "fortarray could not read the ncgen fixture"
    if (field%rank() /= 2) error stop "wrong rank"
    if (any(field%shape /= [2, 3])) error stop "wrong Fortran-order shape"
    if (field%dims(1) /= "radius") error stop "wrong first dimension"
    if (field%dims(2) /= "theta") error stop "wrong second dimension"
    if (maxval(abs(field%values - &
        [11.0_dp, 12.0_dp, 21.0_dp, 22.0_dp, 31.0_dp, 32.0_dp])) > &
        1.0e-12_dp) then
        error stop "wrong values"
    end if
    if (.not. allocated(field%coords)) error stop "coordinate was not loaded"
    if (maxval(abs(field%coords(1)%values - [0.25_dp, 0.75_dp])) > 1.0e-12_dp) then
        error stop "wrong coordinate values"
    end if

    call field%set_attr("units", "V")
    call write_netcdf("fortarray-oracle-output.nc", field, stat)
    if (stat /= FORTARRAY_SUCCESS) error stop "fortarray could not write the fixture"
    call execute_command_line( &
        "python3 test/fixtures/verify_netcdf.py fortarray-oracle-output.nc", &
        exitstat=command_status)
    if (command_status /= 0) error stop "ncdump rejected fortarray output"
end program test_io_oracle
