module fortarray_types
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private
    
    
    ! Maximum length for names and attributes
    integer, parameter :: MAX_NAME_LEN = 256
    integer, parameter :: MAX_ATTR_LEN = 1024
    
    ! Data type enumerations
    integer, parameter :: DTYPE_INT8 = 1
    integer, parameter :: DTYPE_INT16 = 2
    integer, parameter :: DTYPE_INT32 = 3
    integer, parameter :: DTYPE_INT64 = 4
    integer, parameter :: DTYPE_REAL32 = 5
    integer, parameter :: DTYPE_REAL64 = 6
    integer, parameter :: DTYPE_CHAR = 7
    integer, parameter :: DTYPE_LOGICAL = 8
    
    ! Attribute type constants
    integer, parameter :: ATTR_TYPE_STRING = 1
    integer, parameter :: ATTR_TYPE_NUMERIC = 2
    integer, parameter :: ATTR_TYPE_LOGICAL = 3
    
    ! Attribute type
    type :: attribute_t
        character(len=MAX_NAME_LEN) :: name = ""
        character(len=MAX_ATTR_LEN) :: value = ""
        integer :: dtype = ATTR_TYPE_STRING
    end type attribute_t
    
    ! Dimension type - represents a NetCDF dimension
    type :: dimension_t
        character(len=MAX_NAME_LEN) :: name = ""
        integer :: length = 0
        logical :: is_unlimited = .false.
        integer :: current_length = 0  ! For unlimited dimensions
    end type dimension_t
    
    ! Coordinate type - 1D variable that defines an axis
    type :: coordinate_t
        logical :: initialized = .false.
        character(len=MAX_NAME_LEN) :: name = ""
        integer :: length = 0
        integer :: dtype = 0
        real(real64), allocatable :: values_r64(:)
        real(real32), allocatable :: values_r32(:)
        integer(int64), allocatable :: values_i64(:)
        integer(int32), allocatable :: values_i32(:)
        character(len=:), allocatable :: values_char(:)
        logical :: is_monotonic = .false.
        logical :: is_regular = .false.  ! Regular spacing
        real(real64) :: spacing = 0.0_real64  ! For regular coords
        ! Attributes
        integer :: n_attrs = 0
        type(attribute_t), allocatable :: attrs(:)
    contains
        final :: coordinate_finalizer
    end type coordinate_t
    
    ! Data storage type - generic storage for variable data
    type :: data_storage_t
        logical :: initialized = .false.
        integer :: dtype = 0
        integer :: n_elements = 0
        ! Flattened storage for all types
        real(real64), allocatable :: values_r64(:)
        real(real32), allocatable :: values_r32(:)
        integer(int64), allocatable :: values_i64(:)
        integer(int32), allocatable :: values_i32(:)
        character(len=:), allocatable :: values_char(:)
        logical, allocatable :: values_logical(:)
    contains
        final :: data_storage_finalizer
    end type data_storage_t
    
    ! Variable type - represents a NetCDF variable (was dataframe_t)
    type :: fortarray_t
        ! Basic properties
        logical :: initialized = .false.
        character(len=MAX_NAME_LEN) :: name = ""
        integer :: n_dims = 0
        integer :: n_elements = 0  ! Total number of elements
        
        ! Dimension information
        character(len=MAX_NAME_LEN), allocatable :: dim_names(:)
        integer, allocatable :: shape(:)
        integer, allocatable :: strides(:)  ! For efficient indexing
        
        ! Coordinate arrays (may be empty for some dimensions)
        type(coordinate_t), allocatable :: coords(:)
        logical, allocatable :: has_coord(:)  ! Track which dims have coords
        
        ! Data storage
        type(data_storage_t) :: data
        
        ! Attributes (NetCDF attributes)
        integer :: n_attrs = 0
        type(attribute_t), allocatable :: attrs(:)
        
        ! Memory layout flags
        logical :: is_c_order = .false.  ! False = Fortran order (default)
        logical :: is_view = .false.      ! True if this is a view
        logical :: owns_memory = .true.   ! True if owns its memory
        
        ! Missing value handling (NetCDF fill values)
        logical :: has_fill_value = .false.
        real(real64), allocatable :: fill_value  ! Unified fill value as real64
        real(real64) :: fill_value_r64 = huge(1.0_real64)
        real(real32) :: fill_value_r32 = huge(1.0_real32)
        integer(int64) :: fill_value_i64 = huge(1_int64)
        integer(int32) :: fill_value_i32 = huge(1_int32)
        character(len=1) :: fill_value_char = ""
        
        ! Common NetCDF attributes stored separately for quick access
        character(len=MAX_ATTR_LEN) :: units = ""
        character(len=MAX_ATTR_LEN) :: long_name = ""
        character(len=MAX_ATTR_LEN) :: standard_name = ""
        
        ! Cache flags
        logical :: shape_cached = .false.
        logical :: strides_cached = .false.
    contains
        ! Finalizer
        final :: variable_finalizer
        
        ! ======= XARRAY-COMPATIBLE METHODS (TO BE IMPLEMENTED) =======
        ! Note: Method implementations will be added in fortarray_methods.f90
        
        ! Selection methods
        procedure :: sel_point => fortarray_sel_point_r64
        procedure :: sel_range => fortarray_sel_range_r64
        procedure :: isel_point => fortarray_isel_point
        procedure :: isel_range => fortarray_isel_range
        
        ! Aggregation methods
        procedure :: mean => fortarray_mean_all
        procedure :: sum => fortarray_sum_all
        
    end type fortarray_t
    
    ! Dataset type - represents a NetCDF file with multiple variables
    type :: dataset_t
        ! Basic properties
        logical :: initialized = .false.
        character(len=MAX_NAME_LEN) :: filename = ""
        
        ! Dimensions (shared across variables)
        integer :: n_dims = 0
        type(dimension_t), allocatable :: dimensions(:)
        
        ! Variables
        integer :: n_vars = 0
        type(fortarray_t), allocatable :: variables(:)
        character(len=MAX_NAME_LEN), allocatable :: var_names(:)
        
        ! Coordinate variables (subset of variables that are 1D)
        integer :: n_coords = 0
        integer, allocatable :: coord_var_indices(:)  ! Indices into variables array
        
        ! Global attributes
        integer :: n_attrs = 0
        character(len=MAX_NAME_LEN), allocatable :: attr_keys(:)
        character(len=MAX_ATTR_LEN), allocatable :: attr_values(:)
        
        ! NetCDF specific
        integer :: ncid = -1  ! NetCDF file ID when open
        logical :: is_open = .false.
        logical :: read_only = .true.
        
        ! CF convention info
        character(len=MAX_ATTR_LEN) :: conventions = "CF-1.8"
        character(len=MAX_ATTR_LEN) :: title = ""
        character(len=MAX_ATTR_LEN) :: institution = ""
        character(len=MAX_ATTR_LEN) :: source = ""
        character(len=MAX_ATTR_LEN) :: history = ""
        character(len=MAX_ATTR_LEN) :: references = ""
    contains
        final :: dataset_finalizer
    end type dataset_t
    
    ! Legacy support - dataframe_t is now an alias for fortarray_t
    type :: dataframe_t
        type(fortarray_t) :: var
    end type dataframe_t
    
    ! Make types public
    public :: fortarray_t, dataset_t, coordinate_t, data_storage_t, dimension_t
    public :: dataframe_t  ! For backward compatibility
    public :: MAX_NAME_LEN, MAX_ATTR_LEN
    public :: DTYPE_INT8, DTYPE_INT16, DTYPE_INT32, DTYPE_INT64
    public :: DTYPE_REAL32, DTYPE_REAL64, DTYPE_CHAR, DTYPE_LOGICAL
    public :: ATTR_TYPE_STRING, ATTR_TYPE_NUMERIC, ATTR_TYPE_LOGICAL
    public :: attribute_t
    
    ! Interface block for external procedures
    interface
        module function fortarray_sel_point_r64(this, coord_name, value, method) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            real(real64), intent(in) :: value
            character(len=*), intent(in), optional :: method
            type(fortarray_t) :: result_array
        end function fortarray_sel_point_r64
        
        module function fortarray_sel_range_r64(this, coord_name, start_val, stop_val, step_val) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            real(real64), intent(in) :: start_val, stop_val
            real(real64), intent(in), optional :: step_val
            type(fortarray_t) :: result_array
        end function fortarray_sel_range_r64
        
        module function fortarray_isel_point(this, dim_name, index) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: dim_name
            integer, intent(in) :: index
            type(fortarray_t) :: result_array
        end function fortarray_isel_point
        
        module function fortarray_isel_range(this, dim_name, start_idx, stop_idx, step_idx) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: dim_name
            integer, intent(in) :: start_idx, stop_idx
            integer, intent(in), optional :: step_idx
            type(fortarray_t) :: result_array
        end function fortarray_isel_range
        
        module function fortarray_mean_all(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_mean_all
        
        module function fortarray_sum_all(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_sum_all
    end interface
    
contains

    !> Finalizer for coordinate_t type
    subroutine coordinate_finalizer(coord)
        type(coordinate_t), intent(inout) :: coord
        
        if (allocated(coord%values_i32)) deallocate(coord%values_i32)
        if (allocated(coord%values_i64)) deallocate(coord%values_i64)
        if (allocated(coord%values_r32)) deallocate(coord%values_r32)
        if (allocated(coord%values_r64)) deallocate(coord%values_r64)
        if (allocated(coord%values_char)) deallocate(coord%values_char)
        if (allocated(coord%attrs)) deallocate(coord%attrs)
    end subroutine coordinate_finalizer
    
    !> Finalizer for data_storage_t type
    subroutine data_storage_finalizer(storage)
        type(data_storage_t), intent(inout) :: storage
        
        if (allocated(storage%values_i32)) deallocate(storage%values_i32)
        if (allocated(storage%values_i64)) deallocate(storage%values_i64)
        if (allocated(storage%values_r32)) deallocate(storage%values_r32)
        if (allocated(storage%values_r64)) deallocate(storage%values_r64)
        if (allocated(storage%values_char)) deallocate(storage%values_char)
        if (allocated(storage%values_logical)) deallocate(storage%values_logical)
    end subroutine data_storage_finalizer
    
    !> Finalizer for fortarray_t type (was dataframe_finalizer)
    subroutine variable_finalizer(var)
        type(fortarray_t), intent(inout) :: var
        
        if (allocated(var%dim_names)) deallocate(var%dim_names)
        if (allocated(var%shape)) deallocate(var%shape)
        if (allocated(var%strides)) deallocate(var%strides)
        if (allocated(var%coords)) deallocate(var%coords)
        if (allocated(var%has_coord)) deallocate(var%has_coord)
        if (allocated(var%attrs)) deallocate(var%attrs)
    end subroutine variable_finalizer
    
    !> Finalizer for dataset_t type
    subroutine dataset_finalizer(ds)
        type(dataset_t), intent(inout) :: ds
        
        if (allocated(ds%dimensions)) deallocate(ds%dimensions)
        if (allocated(ds%variables)) deallocate(ds%variables)
        if (allocated(ds%var_names)) deallocate(ds%var_names)
        if (allocated(ds%coord_var_indices)) deallocate(ds%coord_var_indices)
        if (allocated(ds%attr_keys)) deallocate(ds%attr_keys)
        if (allocated(ds%attr_values)) deallocate(ds%attr_values)
    end subroutine dataset_finalizer
    
end module fortarray_types