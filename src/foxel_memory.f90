module foxel_memory
    use foxel_types
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private
    
    ! Public interfaces for finalizers
    public :: finalize_variable
    public :: finalize_dataframe  ! Legacy support
    public :: finalize_coordinate
    public :: finalize_data_storage
    public :: finalize_dataset
    
contains

    !> Finalizer for coordinate_t type
    !> Deallocates all allocated arrays based on dtype
    subroutine finalize_coordinate(coord)
        type(coordinate_t), intent(inout) :: coord
        
        if (.not. coord%initialized) return
        
        ! Deallocate based on data type
        select case(coord%dtype)
        case(DTYPE_INT8, DTYPE_INT16, DTYPE_INT32)
            if (allocated(coord%values_i32)) deallocate(coord%values_i32)
        case(DTYPE_INT64)
            if (allocated(coord%values_i64)) deallocate(coord%values_i64)
        case(DTYPE_REAL32)
            if (allocated(coord%values_r32)) deallocate(coord%values_r32)
        case(DTYPE_REAL64)
            if (allocated(coord%values_r64)) deallocate(coord%values_r64)
        case(DTYPE_CHAR)
            if (allocated(coord%values_char)) deallocate(coord%values_char)
        end select
        
        ! Deallocate attributes
        if (allocated(coord%attr_keys)) deallocate(coord%attr_keys)
        if (allocated(coord%attr_values)) deallocate(coord%attr_values)
        
        coord%initialized = .false.
        coord%length = 0
        coord%dtype = 0
        coord%n_attrs = 0
    end subroutine finalize_coordinate
    
    !> Finalizer for data_storage_t type
    !> Deallocates all allocated arrays based on dtype
    subroutine finalize_data_storage(storage)
        type(data_storage_t), intent(inout) :: storage
        
        if (.not. storage%initialized) return
        
        ! Deallocate based on data type
        select case(storage%dtype)
        case(DTYPE_INT8, DTYPE_INT16, DTYPE_INT32)
            if (allocated(storage%values_i32)) deallocate(storage%values_i32)
        case(DTYPE_INT64)
            if (allocated(storage%values_i64)) deallocate(storage%values_i64)
        case(DTYPE_REAL32)
            if (allocated(storage%values_r32)) deallocate(storage%values_r32)
        case(DTYPE_REAL64)
            if (allocated(storage%values_r64)) deallocate(storage%values_r64)
        case(DTYPE_CHAR)
            if (allocated(storage%values_char)) deallocate(storage%values_char)
        case(DTYPE_LOGICAL)
            if (allocated(storage%values_logical)) deallocate(storage%values_logical)
        end select
        
        storage%initialized = .false.
        storage%n_elements = 0
        storage%dtype = 0
    end subroutine finalize_data_storage
    
    !> Finalizer for variable_t type
    !> Deallocates all allocated components
    subroutine finalize_variable(var)
        type(variable_t), intent(inout) :: var
        integer :: i
        
        if (.not. var%initialized) return
        
        ! Deallocate dimension-related arrays
        if (allocated(var%dim_names)) deallocate(var%dim_names)
        if (allocated(var%shape)) deallocate(var%shape)
        if (allocated(var%strides)) deallocate(var%strides)
        
        ! Finalize and deallocate coordinates
        if (allocated(var%coords)) then
            do i = 1, size(var%coords)
                call finalize_coordinate(var%coords(i))
            end do
            deallocate(var%coords)
        end if
        
        if (allocated(var%has_coord)) deallocate(var%has_coord)
        
        ! Finalize data storage only if we own the memory
        if (var%owns_memory) then
            call finalize_data_storage(var%data)
        end if
        
        ! Deallocate attributes
        if (allocated(var%attr_keys)) deallocate(var%attr_keys)
        if (allocated(var%attr_values)) deallocate(var%attr_values)
        
        ! Reset all values
        var%initialized = .false.
        var%n_dims = 0
        var%n_elements = 0
        var%n_attrs = 0
        var%is_c_order = .false.
        var%is_view = .false.
        var%owns_memory = .true.
        var%has_fill_value = .false.
        var%name = ""
        var%units = ""
        var%long_name = ""
        var%standard_name = ""
        var%shape_cached = .false.
        var%strides_cached = .false.
    end subroutine finalize_variable
    
    !> Legacy finalizer for dataframe_t type
    !> Delegates to variable finalizer
    subroutine finalize_dataframe(df)
        type(dataframe_t), intent(inout) :: df
        
        call finalize_variable(df%var)
    end subroutine finalize_dataframe
    
    !> Finalizer for dataset_t type
    !> Deallocates all allocated components
    subroutine finalize_dataset(ds)
        type(dataset_t), intent(inout) :: ds
        integer :: i
        
        if (.not. ds%initialized) return
        
        ! Finalize and deallocate dimensions
        if (allocated(ds%dimensions)) deallocate(ds%dimensions)
        
        ! Finalize and deallocate variables
        if (allocated(ds%variables)) then
            do i = 1, size(ds%variables)
                call finalize_variable(ds%variables(i))
            end do
            deallocate(ds%variables)
        end if
        
        if (allocated(ds%var_names)) deallocate(ds%var_names)
        if (allocated(ds%coord_var_indices)) deallocate(ds%coord_var_indices)
        
        ! Deallocate attributes
        if (allocated(ds%attr_keys)) deallocate(ds%attr_keys)
        if (allocated(ds%attr_values)) deallocate(ds%attr_values)
        
        ! Reset all values
        ds%initialized = .false.
        ds%filename = ""
        ds%n_dims = 0
        ds%n_vars = 0
        ds%n_coords = 0
        ds%n_attrs = 0
        ds%ncid = -1
        ds%is_open = .false.
        ds%read_only = .true.
    end subroutine finalize_dataset
    
end module foxel_memory