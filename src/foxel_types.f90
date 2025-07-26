module foxel_types
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private
    
    ! Maximum length for dimension names and attribute keys
    integer, parameter :: MAX_NAME_LEN = 256
    integer, parameter :: MAX_ATTR_LEN = 1024
    
    ! Coordinate storage type - will be extended later for different data types
    type :: coordinate_t
        logical :: initialized = .false.
        integer :: length = 0
        integer :: dtype = 0  ! 1=int32, 2=int64, 3=real32, 4=real64, 5=char
        real(real64), allocatable :: values_r64(:)
        real(real32), allocatable :: values_r32(:)
        integer(int64), allocatable :: values_i64(:)
        integer(int32), allocatable :: values_i32(:)
        character(len=:), allocatable :: values_char(:)
        logical :: is_monotonic = .false.
        logical :: is_regular = .false.  ! Regular spacing
        real(real64) :: spacing = 0.0_real64  ! For regular coords
    contains
        final :: coordinate_finalizer
    end type coordinate_t
    
    ! Data storage type - will be extended for generic data
    type :: data_storage_t
        logical :: initialized = .false.
        integer :: dtype = 0  ! 1=int32, 2=int64, 3=real32, 4=real64, 5=char
        integer :: n_elements = 0
        ! Flattened storage for all types
        real(real64), allocatable :: values_r64(:)
        real(real32), allocatable :: values_r32(:)
        integer(int64), allocatable :: values_i64(:)
        integer(int32), allocatable :: values_i32(:)
        character(len=:), allocatable :: values_char(:)
    contains
        final :: data_storage_finalizer
    end type data_storage_t
    
    ! Main DataFrame type
    type :: dataframe_t
        ! Basic properties
        logical :: initialized = .false.
        integer :: n_dims = 0
        integer :: n_elements = 0  ! Total number of elements
        
        ! Dimension information
        character(len=MAX_NAME_LEN), allocatable :: dim_names(:)
        integer, allocatable :: shape(:)
        integer, allocatable :: strides(:)  ! For efficient indexing
        
        ! Coordinate arrays
        type(coordinate_t), allocatable :: coords(:)
        
        ! Data storage
        type(data_storage_t) :: data
        
        ! Attributes (key-value pairs)
        integer :: n_attrs = 0
        character(len=MAX_NAME_LEN), allocatable :: attr_keys(:)
        character(len=MAX_ATTR_LEN), allocatable :: attr_values(:)
        
        ! Memory layout flags
        logical :: is_c_order = .false.  ! False = Fortran order (default)
        logical :: is_view = .false.      ! True if this is a view of another dataframe
        logical :: owns_memory = .true.   ! True if this dataframe owns its memory
        
        ! Parent reference for views (not used yet, placeholder)
        integer :: parent_id = -1
        
        ! Missing value handling
        logical :: has_missing = .false.
        real(real64) :: missing_value_r64 = huge(1.0_real64)
        real(real32) :: missing_value_r32 = huge(1.0_real32)
        integer(int64) :: missing_value_i64 = huge(1_int64)
        integer(int32) :: missing_value_i32 = huge(1_int32)
        character(len=1) :: missing_value_char = ""
        
        ! Variable name (for single-variable dataframes)
        character(len=MAX_NAME_LEN) :: var_name = ""
        
        ! Cache for commonly computed values
        logical :: shape_cached = .false.
        logical :: strides_cached = .false.
    contains
        final :: dataframe_finalizer
    end type dataframe_t
    
    ! Make types public
    public :: dataframe_t, coordinate_t, data_storage_t
    public :: MAX_NAME_LEN, MAX_ATTR_LEN
    
contains

    !> Finalizer for coordinate_t type
    subroutine coordinate_finalizer(coord)
        type(coordinate_t), intent(inout) :: coord
        
        if (allocated(coord%values_i32)) deallocate(coord%values_i32)
        if (allocated(coord%values_i64)) deallocate(coord%values_i64)
        if (allocated(coord%values_r32)) deallocate(coord%values_r32)
        if (allocated(coord%values_r64)) deallocate(coord%values_r64)
        if (allocated(coord%values_char)) deallocate(coord%values_char)
    end subroutine coordinate_finalizer
    
    !> Finalizer for data_storage_t type
    subroutine data_storage_finalizer(storage)
        type(data_storage_t), intent(inout) :: storage
        
        if (allocated(storage%values_i32)) deallocate(storage%values_i32)
        if (allocated(storage%values_i64)) deallocate(storage%values_i64)
        if (allocated(storage%values_r32)) deallocate(storage%values_r32)
        if (allocated(storage%values_r64)) deallocate(storage%values_r64)
        if (allocated(storage%values_char)) deallocate(storage%values_char)
    end subroutine data_storage_finalizer
    
    !> Finalizer for dataframe_t type
    subroutine dataframe_finalizer(df)
        type(dataframe_t), intent(inout) :: df
        
        if (allocated(df%dim_names)) deallocate(df%dim_names)
        if (allocated(df%shape)) deallocate(df%shape)
        if (allocated(df%strides)) deallocate(df%strides)
        if (allocated(df%coords)) deallocate(df%coords)
        if (allocated(df%attr_keys)) deallocate(df%attr_keys)
        if (allocated(df%attr_values)) deallocate(df%attr_values)
    end subroutine dataframe_finalizer
    
end module foxel_types