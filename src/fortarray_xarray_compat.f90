submodule (fortarray_types) fortarray_xarray_compat
    ! xarray compatibility layer methods for FortArray
    ! Implements xarray-style API methods for seamless migration
    use fortarray_constructors
    use iso_fortran_env, only: real64, int32, int64, error_unit
    implicit none

contains

    !> Select values along a coordinate (xarray .sel() equivalent)
    module function fortarray_select_coord_value(this, coord_name, value) result(result_array)
        class(fortarray_t), intent(in) :: this
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in) :: value
        type(fortarray_t) :: result_array
        
        integer :: coord_dim, i, closest_idx
        real(real64) :: min_diff, diff
        integer, dimension(:), allocatable :: start_indices, end_indices
        
        result_array%initialized = .false.
        
        if (.not. this%initialized) return
        
        ! Find the coordinate dimension
        coord_dim = 0
        do i = 1, this%n_dims
            if (this%has_coord(i) .and. trim(this%coords(i)%name) == trim(coord_name)) then
                coord_dim = i
                exit
            end if
        end do
        
        if (coord_dim == 0) return
        
        ! Find closest coordinate value
        closest_idx = 1
        min_diff = huge(1.0_real64)
        do i = 1, this%coords(coord_dim)%length
            if (allocated(this%coords(coord_dim)%values_r64)) then
                diff = abs(this%coords(coord_dim)%values_r64(i) - value)
                if (diff < min_diff) then
                    min_diff = diff
                    closest_idx = i
                end if
            end if
        end do
        
        ! Create slice indices
        allocate(start_indices(this%n_dims), end_indices(this%n_dims))
        do i = 1, this%n_dims
            if (i == coord_dim) then
                start_indices(i) = closest_idx
                end_indices(i) = closest_idx
            else
                start_indices(i) = 1
                end_indices(i) = this%shape(i)
            end if
        end do
        
        ! Perform the slice using isel_range and squeeze the selected dimension
        result_array = this%isel_range(dim_name=trim(this%dim_names(coord_dim)), &
                                      start_idx=closest_idx, stop_idx=closest_idx)
        if (result_array%initialized) then
            result_array = result_array%squeeze(axis=coord_dim)
        end if
        
    end function fortarray_select_coord_value

    !> Select indices along a dimension (xarray .isel() equivalent)
    module function fortarray_select_indices(this, dim, indices) result(result_array)
        class(fortarray_t), intent(in) :: this
        integer, intent(in) :: dim
        integer, dimension(:), intent(in) :: indices
        type(fortarray_t) :: result_array
        
        result_array%initialized = .false.
        
        if (.not. this%initialized) return
        if (dim < 1 .or. dim > this%n_dims) return
        
        ! Use existing isel_indices method with dimension name
        result_array = this%isel_indices(dim_name=trim(this%dim_names(dim)), indices=indices)
        
    end function fortarray_select_indices

    !> Multiply array by scalar (xarray scalar multiplication)
    module function fortarray_multiply_scalar(this, scalar) result(result_array)
        class(fortarray_t), intent(in) :: this
        real(real64), intent(in) :: scalar
        type(fortarray_t) :: result_array
        
        integer :: i
        
        result_array%initialized = .false.
        
        if (.not. this%initialized) return
        
        ! Copy structure
        result_array%n_dims = this%n_dims
        result_array%n_elements = this%n_elements
        allocate(result_array%shape(this%n_dims))
        result_array%shape = this%shape
        
        allocate(result_array%dim_names(this%n_dims))
        result_array%dim_names = this%dim_names
        
        ! Copy coordinates
        if (allocated(this%coords)) then
            allocate(result_array%coords(this%n_dims))
            allocate(result_array%has_coord(this%n_dims))
            result_array%has_coord = this%has_coord
            result_array%coords = this%coords
        end if
        
        ! Multiply data by scalar
        result_array%data%dtype = this%data%dtype
        if (allocated(this%data%values_r64)) then
            allocate(result_array%data%values_r64(this%n_elements))
            do i = 1, this%n_elements
                result_array%data%values_r64(i) = this%data%values_r64(i) * scalar
            end do
        end if
        
        ! Copy metadata (units might need updating for multiplication) 
        result_array%name = this%name
        result_array%units = this%units
        result_array%long_name = this%long_name
        result_array%standard_name = this%standard_name
        
        result_array%initialized = .true.
        
    end function fortarray_multiply_scalar

end submodule fortarray_xarray_compat