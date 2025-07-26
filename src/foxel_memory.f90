module foxel_memory
    use foxel_types
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private
    
    ! Public interfaces for finalizers
    public :: finalize_dataframe
    public :: finalize_coordinate
    public :: finalize_data_storage
    
contains

    !> Finalizer for coordinate_t type
    !> Deallocates all allocated arrays based on dtype
    subroutine finalize_coordinate(coord)
        type(coordinate_t), intent(inout) :: coord
        
        if (.not. coord%initialized) return
        
        ! Deallocate based on data type
        select case(coord%dtype)
        case(1)  ! int32
            if (allocated(coord%values_i32)) deallocate(coord%values_i32)
        case(2)  ! int64
            if (allocated(coord%values_i64)) deallocate(coord%values_i64)
        case(3)  ! real32
            if (allocated(coord%values_r32)) deallocate(coord%values_r32)
        case(4)  ! real64
            if (allocated(coord%values_r64)) deallocate(coord%values_r64)
        case(5)  ! char
            if (allocated(coord%values_char)) deallocate(coord%values_char)
        end select
        
        coord%initialized = .false.
        coord%length = 0
        coord%dtype = 0
    end subroutine finalize_coordinate
    
    !> Finalizer for data_storage_t type
    !> Deallocates all allocated arrays based on dtype
    subroutine finalize_data_storage(storage)
        type(data_storage_t), intent(inout) :: storage
        
        if (.not. storage%initialized) return
        
        ! Deallocate based on data type
        select case(storage%dtype)
        case(1)  ! int32
            if (allocated(storage%values_i32)) deallocate(storage%values_i32)
        case(2)  ! int64
            if (allocated(storage%values_i64)) deallocate(storage%values_i64)
        case(3)  ! real32
            if (allocated(storage%values_r32)) deallocate(storage%values_r32)
        case(4)  ! real64
            if (allocated(storage%values_r64)) deallocate(storage%values_r64)
        case(5)  ! char
            if (allocated(storage%values_char)) deallocate(storage%values_char)
        end select
        
        storage%initialized = .false.
        storage%n_elements = 0
        storage%dtype = 0
    end subroutine finalize_data_storage
    
    !> Finalizer for dataframe_t type
    !> Deallocates all allocated components
    subroutine finalize_dataframe(df)
        type(dataframe_t), intent(inout) :: df
        integer :: i
        
        if (.not. df%initialized) return
        
        ! Deallocate dimension-related arrays
        if (allocated(df%dim_names)) deallocate(df%dim_names)
        if (allocated(df%shape)) deallocate(df%shape)
        if (allocated(df%strides)) deallocate(df%strides)
        
        ! Finalize and deallocate coordinates
        if (allocated(df%coords)) then
            do i = 1, size(df%coords)
                call finalize_coordinate(df%coords(i))
            end do
            deallocate(df%coords)
        end if
        
        ! Finalize data storage only if we own the memory
        if (df%owns_memory) then
            call finalize_data_storage(df%data)
        end if
        
        ! Deallocate attributes
        if (allocated(df%attr_keys)) deallocate(df%attr_keys)
        if (allocated(df%attr_values)) deallocate(df%attr_values)
        
        ! Reset all values
        df%initialized = .false.
        df%n_dims = 0
        df%n_elements = 0
        df%n_attrs = 0
        df%is_c_order = .false.
        df%is_view = .false.
        df%owns_memory = .true.
        df%parent_id = -1
        df%has_missing = .false.
        df%var_name = ""
        df%shape_cached = .false.
        df%strides_cached = .false.
    end subroutine finalize_dataframe
    
end module foxel_memory