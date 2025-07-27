module foxel_broadcasting
    use foxel_types
    use foxel_storage
    use foxel_constructors
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    public :: can_broadcast
    public :: broadcast_to_shape
    public :: broadcast_to_common_shape
    
contains

    !> Check if two variables can be broadcast together
    function can_broadcast(var1, var2) result(compatible)
        type(variable_t), intent(in) :: var1, var2
        logical :: compatible
        integer :: ndim1, ndim2, i, dim1, dim2
        
        ! Get dimensions, handling scalar case
        ndim1 = 0
        ndim2 = 0
        if (allocated(var1%shape)) ndim1 = size(var1%shape)
        if (allocated(var2%shape)) ndim2 = size(var2%shape)
        
        ! Scalars can broadcast to anything
        if (ndim1 == 0 .or. ndim2 == 0) then
            compatible = .true.
            return
        end if
        
        ! Start from the rightmost dimensions and work backwards
        compatible = .true.
        i = 0
        do while (i < min(ndim1, ndim2) .and. compatible)
            i = i + 1
            dim1 = var1%shape(ndim1 - i + 1)
            dim2 = var2%shape(ndim2 - i + 1)
            
            ! Dimensions are compatible if they're equal or one is 1
            if (dim1 /= dim2 .and. dim1 /= 1 .and. dim2 /= 1) then
                compatible = .false.
            end if
        end do
        
    end function can_broadcast
    
    !> Broadcast a variable to a specific shape
    function broadcast_to_shape(var, target_shape) result(broadcast_var)
        type(variable_t), intent(in) :: var
        integer, dimension(:), intent(in) :: target_shape
        type(variable_t) :: broadcast_var
        integer :: i, j, k, ndim_in, ndim_out
        integer :: linear_idx_in, linear_idx_out
        integer, dimension(:), allocatable :: idx_in, idx_out, strides_in, strides_out
        real(real64), dimension(:), allocatable :: broadcast_data
        character(len=64), dimension(:), allocatable :: new_dim_names
        logical :: compatible
        
        ndim_in = 0
        if (allocated(var%shape)) ndim_in = size(var%shape)
        ndim_out = size(target_shape)
        
        ! Check if broadcasting is valid
        compatible = .true.
        
        ! For scalar (0D) variables, can broadcast to any shape
        if (ndim_in == 0) then
            ! Scalar case - replicate the value
            allocate(broadcast_data(product(target_shape)))
            if (allocated(var%data%values_r64)) then
                broadcast_data = var%data%values_r64(1)
            else
                ! Handle other data types if needed
                write(error_unit,'(A)') "ERROR: Only real64 scalars supported for now"
                stop 1
            end if
            
            ! Generate dimension names
            allocate(new_dim_names(ndim_out))
            do i = 1, ndim_out
                write(new_dim_names(i), '(A,I0)') "dim", i
            end do
            
            ! Create broadcast variable by constructing directly
            broadcast_var%name = var%name
            broadcast_var%n_elements = product(target_shape)
            broadcast_var%n_dims = ndim_out
            allocate(broadcast_var%shape(ndim_out))
            broadcast_var%shape = target_shape
            allocate(broadcast_var%dim_names(ndim_out))
            broadcast_var%dim_names = new_dim_names
            
            ! Allocate and copy data
            broadcast_var%data%dtype = DTYPE_REAL64
            allocate(broadcast_var%data%values_r64(broadcast_var%n_elements))
            broadcast_var%data%values_r64 = broadcast_data
            
            return
        end if
        
        ! Check compatibility from right to left
        do i = 1, min(ndim_in, ndim_out)
            j = ndim_in - i + 1
            k = ndim_out - i + 1
            if (var%shape(j) /= target_shape(k) .and. var%shape(j) /= 1) then
                compatible = .false.
                exit
            end if
        end do
        
        ! Additional check: input can't have more non-1 dimensions than output
        if (ndim_in > ndim_out) then
            compatible = .false.
        end if
        
        if (.not. compatible) then
            write(error_unit,'(A)') "ERROR: Cannot broadcast to target shape"
            stop 1
        end if
        
        ! Allocate output data
        allocate(broadcast_data(product(target_shape)))
        
        ! Calculate strides
        allocate(strides_in(ndim_in), strides_out(ndim_out))
        allocate(idx_in(ndim_in), idx_out(ndim_out))
        
        ! Calculate strides for column-major order
        strides_in(1) = 1
        do i = 2, ndim_in
            strides_in(i) = strides_in(i-1) * var%shape(i-1)
        end do
        
        strides_out(1) = 1
        do i = 2, ndim_out
            strides_out(i) = strides_out(i-1) * target_shape(i-1)
        end do
        
        ! Broadcast the data
        do linear_idx_out = 1, product(target_shape)
            ! Convert linear index to multi-dimensional index (column-major order)
            idx_out = 0
            k = linear_idx_out - 1
            do i = 1, ndim_out
                idx_out(i) = mod(k, target_shape(i)) + 1
                k = k / target_shape(i)
            end do
            
            ! Map output index to input index
            idx_in = 1  ! Default to 1 for missing dimensions
            do i = 1, ndim_in
                ! Map from the right - align trailing dimensions
                j = ndim_out - ndim_in + i
                if (j > 0) then
                    if (var%shape(i) == 1) then
                        idx_in(i) = 1
                    else
                        idx_in(i) = idx_out(j)
                    end if
                else
                    idx_in(i) = 1
                end if
            end do
            
            ! Calculate linear index for input
            linear_idx_in = 1
            do i = 1, ndim_in
                linear_idx_in = linear_idx_in + (idx_in(i) - 1) * strides_in(i)
            end do
            
            ! Copy the value (linear_idx_out is already set by the loop)
            broadcast_data(linear_idx_out) = var%data%values_r64(linear_idx_in)
        end do
        
        ! Create new dimension names
        allocate(new_dim_names(ndim_out))
        if (allocated(var%dim_names)) then
            ! Try to preserve dimension names where possible
            do i = 1, ndim_out
                if (i <= ndim_in .and. ndim_out - i < ndim_in) then
                    j = ndim_in - (ndim_out - i)
                    if (j > 0 .and. j <= size(var%dim_names)) then
                        new_dim_names(i) = var%dim_names(j)
                    else
                        write(new_dim_names(i), '(A,I0)') "dim", i
                    end if
                else
                    write(new_dim_names(i), '(A,I0)') "dim", i
                end if
            end do
        else
            do i = 1, ndim_out
                write(new_dim_names(i), '(A,I0)') "dim", i
            end do
        end if
        
        ! Create the broadcast variable by constructing directly
        broadcast_var%name = var%name
        broadcast_var%n_elements = product(target_shape)
        broadcast_var%n_dims = ndim_out
        allocate(broadcast_var%shape(ndim_out))
        broadcast_var%shape = target_shape
        allocate(broadcast_var%dim_names(ndim_out))
        broadcast_var%dim_names = new_dim_names
        
        ! Allocate and copy data
        broadcast_var%data%dtype = DTYPE_REAL64
        allocate(broadcast_var%data%values_r64(broadcast_var%n_elements))
        broadcast_var%data%values_r64 = broadcast_data
        
    end function broadcast_to_shape
    
    !> Broadcast two variables to their common shape
    function broadcast_to_common_shape(var1, var2) result(common_var)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: common_var
        integer, dimension(:), allocatable :: common_shape
        integer :: ndim1, ndim2, ndim_common, i, j, k
        
        ndim1 = 0
        ndim2 = 0
        if (allocated(var1%shape)) ndim1 = size(var1%shape)
        if (allocated(var2%shape)) ndim2 = size(var2%shape)
        ndim_common = max(ndim1, ndim2)
        
        allocate(common_shape(ndim_common))
        
        ! Determine common shape
        do i = 1, ndim_common
            if (i <= ndim1 .and. i <= ndim2) then
                ! Both have this dimension
                j = ndim1 - ndim_common + i
                k = ndim2 - ndim_common + i
                if (j > 0 .and. k > 0) then
                    if (var1%shape(j) == 1) then
                        common_shape(i) = var2%shape(k)
                    else if (var2%shape(k) == 1) then
                        common_shape(i) = var1%shape(j)
                    else if (var1%shape(j) == var2%shape(k)) then
                        common_shape(i) = var1%shape(j)
                    else
                        write(error_unit,'(A)') "ERROR: Incompatible shapes for broadcasting"
                        stop 1
                    end if
                else if (j > 0) then
                    common_shape(i) = var1%shape(j)
                else
                    common_shape(i) = var2%shape(k)
                end if
            else if (i <= ndim1) then
                ! Only var1 has this dimension
                j = ndim1 - ndim_common + i
                common_shape(i) = var1%shape(j)
            else
                ! Only var2 has this dimension
                k = ndim2 - ndim_common + i
                common_shape(i) = var2%shape(k)
            end if
        end do
        
        ! Broadcast the first variable to common shape
        common_var = broadcast_to_shape(var1, common_shape)
        
    end function broadcast_to_common_shape

end module foxel_broadcasting