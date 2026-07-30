module fortarray_io
    use fortarray_core, only: dp, data_array_t, data_array, FORTARRAY_SUCCESS, &
        FORTARRAY_EINVAL
    use netcdf, only: NF90_NOERR, NF90_NOWRITE, NF90_CLOBBER, NF90_DOUBLE, &
        nf90_open, nf90_create, nf90_close, nf90_inq_varid, nf90_inquire_variable, &
        nf90_inquire_dimension, nf90_def_dim, nf90_def_var, nf90_enddef, &
        nf90_get_var, nf90_put_var, nf90_put_att
    implicit none
    private

    public :: read_netcdf, write_netcdf

contains

    function read_netcdf(filename, variable, stat) result(array)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: variable
        integer, intent(out), optional :: stat
        type(data_array_t) :: array
        character(len=256), allocatable :: dims(:)
        integer, allocatable :: dimids(:), array_shape(:)
        integer :: code, i, ncid, rank, varid

        if (present(stat)) stat = FORTARRAY_EINVAL
        code = nf90_open(filename, NF90_NOWRITE, ncid)
        if (code /= NF90_NOERR) return
        code = nf90_inq_varid(ncid, variable, varid)
        if (code /= NF90_NOERR) then
            code = nf90_close(ncid)
            return
        end if
        code = nf90_inquire_variable(ncid, varid, ndims=rank)
        if (code /= NF90_NOERR .or. rank > 4) then
            code = nf90_close(ncid)
            return
        end if
        allocate(dimids(rank), array_shape(rank), dims(rank))
        code = nf90_inquire_variable(ncid, varid, dimids=dimids)
        if (code /= NF90_NOERR) then
            code = nf90_close(ncid)
            return
        end if
        do i = 1, rank
            code = nf90_inquire_dimension(ncid, dimids(i), dims(i), array_shape(i))
            if (code /= NF90_NOERR) then
                code = nf90_close(ncid)
                return
            end if
        end do
        call read_values(ncid, varid, variable, dims, array_shape, array, code)
        if (code == NF90_NOERR) call read_dimension_coordinates(ncid, array)
        code = merge(nf90_close(ncid), code, code == NF90_NOERR)
        if (present(stat)) then
            if (code == NF90_NOERR) stat = FORTARRAY_SUCCESS
        end if
    end function read_netcdf

    subroutine read_values(ncid, varid, name, dims, array_shape, array, code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: dims(:)
        integer, intent(in) :: array_shape(:)
        type(data_array_t), intent(out) :: array
        integer, intent(out) :: code
        real(dp) :: scalar
        real(dp), allocatable :: rank1(:), rank2(:, :), rank3(:, :, :), rank4(:, :, :, :)

        select case (size(array_shape))
        case (0)
            code = nf90_get_var(ncid, varid, scalar)
            if (code == NF90_NOERR) array = data_array(scalar, name)
        case (1)
            allocate(rank1(array_shape(1)))
            code = nf90_get_var(ncid, varid, rank1)
            if (code == NF90_NOERR) array = data_array(rank1, dims, name)
        case (2)
            allocate(rank2(array_shape(1), array_shape(2)))
            code = nf90_get_var(ncid, varid, rank2)
            if (code == NF90_NOERR) array = data_array(rank2, dims, name)
        case (3)
            allocate(rank3(array_shape(1), array_shape(2), array_shape(3)))
            code = nf90_get_var(ncid, varid, rank3)
            if (code == NF90_NOERR) array = data_array(rank3, dims, name)
        case (4)
            allocate(rank4(array_shape(1), array_shape(2), array_shape(3), array_shape(4)))
            code = nf90_get_var(ncid, varid, rank4)
            if (code == NF90_NOERR) array = data_array(rank4, dims, name)
        case default
            code = FORTARRAY_EINVAL
        end select
    end subroutine read_values

    subroutine read_dimension_coordinates(ncid, array)
        integer, intent(in) :: ncid
        type(data_array_t), intent(inout) :: array
        real(dp), allocatable :: values(:)
        integer :: code, i, varid

        do i = 1, array%rank()
            code = nf90_inq_varid(ncid, trim(array%dims(i)), varid)
            if (code /= NF90_NOERR) cycle
            allocate(values(array%shape(i)))
            code = nf90_get_var(ncid, varid, values)
            if (code == NF90_NOERR) call array%set_coord(trim(array%dims(i)), values)
            deallocate(values)
        end do
    end subroutine read_dimension_coordinates

    subroutine write_netcdf(filename, array, stat)
        character(len=*), intent(in) :: filename
        type(data_array_t), intent(in) :: array
        integer, intent(out), optional :: stat
        integer, allocatable :: coord_varids(:), dimids(:)
        integer :: code, i, ncid, varid

        if (present(stat)) stat = FORTARRAY_EINVAL
        if (.not. array%valid()) return
        code = nf90_create(filename, NF90_CLOBBER, ncid)
        if (code /= NF90_NOERR) return
        allocate(dimids(array%rank()), coord_varids(array%rank()), source=-1)
        do i = 1, array%rank()
            code = nf90_def_dim(ncid, trim(array%dims(i)), array%shape(i), dimids(i))
            if (code /= NF90_NOERR) exit
            if (has_coordinate(array, trim(array%dims(i)))) then
                code = nf90_def_var(ncid, trim(array%dims(i)), NF90_DOUBLE, dimids(i), &
                    coord_varids(i))
                if (code /= NF90_NOERR) exit
            end if
        end do
        if (code == NF90_NOERR) then
            if (array%rank() == 0) then
                code = nf90_def_var(ncid, trim(array%name), NF90_DOUBLE, varid)
            else
                code = nf90_def_var(ncid, trim(array%name), NF90_DOUBLE, dimids, varid)
            end if
        end if
        if (code == NF90_NOERR .and. allocated(array%attrs)) then
            do i = 1, size(array%attrs)
                code = nf90_put_att(ncid, varid, trim(array%attrs(i)%name), &
                    trim(array%attrs(i)%value))
                if (code /= NF90_NOERR) exit
            end do
        end if
        if (code == NF90_NOERR) code = nf90_enddef(ncid)
        if (code == NF90_NOERR) then
            do i = 1, array%rank()
                if (coord_varids(i) >= 0) then
                    code = write_coordinate(ncid, coord_varids(i), array, &
                        trim(array%dims(i)))
                    if (code /= NF90_NOERR) exit
                end if
            end do
        end if
        if (code == NF90_NOERR) code = nf90_put_var(ncid, varid, array%values)
        if (nf90_close(ncid) /= NF90_NOERR) code = FORTARRAY_EINVAL
        if (present(stat)) then
            if (code == NF90_NOERR) stat = FORTARRAY_SUCCESS
        end if
    end subroutine write_netcdf

    pure logical function has_coordinate(array, name)
        type(data_array_t), intent(in) :: array
        character(len=*), intent(in) :: name
        integer :: i

        has_coordinate = .false.
        if (.not. allocated(array%coords)) return
        do i = 1, size(array%coords)
            if (array%coords(i)%name == name) then
                has_coordinate = .true.
                return
            end if
        end do
    end function has_coordinate

    integer function write_coordinate(ncid, varid, array, name) result(code)
        integer, intent(in) :: ncid, varid
        type(data_array_t), intent(in) :: array
        character(len=*), intent(in) :: name
        integer :: i

        code = FORTARRAY_EINVAL
        do i = 1, size(array%coords)
            if (array%coords(i)%name == name) then
                code = nf90_put_var(ncid, varid, array%coords(i)%values)
                return
            end if
        end do
    end function write_coordinate

end module fortarray_io
