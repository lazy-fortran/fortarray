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
    
    ! Write options type for various formats
    type :: write_options_t
        logical :: compress = .false.
        character(len=16) :: compression = "gzip"
        integer :: deflate_level = 6
        logical :: shuffle = .false.
        logical :: fletcher32 = .false.
        character(len=16) :: format = "netcdf4"
        integer, dimension(:), allocatable :: chunksizes
        logical :: unlimited_dims = .false.
        logical :: cf_compliant = .true.
        logical :: atomic_write = .true.
        character(len=256) :: temp_suffix = ".tmp"
    end type write_options_t
    
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
        
        ! Lazy loading flag
        logical :: lazy = .false.
    contains
        ! Finalizer
        final :: variable_finalizer
        
        ! ======= XARRAY-COMPATIBLE METHODS (TO BE IMPLEMENTED) =======
        ! Note: Method implementations will be added in fortarray_methods.f90
        
        ! Selection methods
        procedure :: sel => fortarray_sel  ! Generic sel for both point and range
        procedure :: sel_point => fortarray_sel_point_r64
        procedure :: sel_range => fortarray_sel_range_r64
        procedure :: isel => fortarray_isel  ! Generic isel for both point and range
        procedure :: isel_point => fortarray_isel_point
        procedure :: isel_range => fortarray_isel_range
        procedure :: isel_indices => fortarray_isel_indices
        
        ! Aggregation methods
        procedure :: mean => fortarray_mean_all
        procedure :: sum => fortarray_sum_all
        procedure :: max => fortarray_max_all
        procedure :: min => fortarray_min_all
        procedure :: std => fortarray_std_all
        
        ! Advanced aggregation methods (Sprint 9)
        procedure :: quantile => fortarray_quantile
        procedure :: percentile => fortarray_percentile
        procedure :: weighted_mean => fortarray_weighted_mean
        procedure :: weighted_sum => fortarray_weighted_sum
        procedure :: weighted_std => fortarray_weighted_std
        procedure :: cumsum => fortarray_cumsum
        procedure :: cumprod => fortarray_cumprod
        procedure :: cummin => fortarray_cummin
        procedure :: cummax => fortarray_cummax
        procedure :: rolling_mean => fortarray_rolling_mean
        procedure :: rolling_sum => fortarray_rolling_sum
        procedure :: rolling_std => fortarray_rolling_std
        
        ! Filtering and conditional operations
        procedure :: where_gt => fortarray_where_gt_r64
        procedure :: where_lt => fortarray_where_lt_r64
        procedure :: gt => fortarray_gt_r64
        procedure :: lt => fortarray_lt_r64
        procedure :: mask_where => fortarray_mask_where
        procedure :: logical_and => fortarray_logical_and
        procedure :: logical_or => fortarray_logical_or
        procedure :: logical_not => fortarray_logical_not
        procedure :: fillna_value => fortarray_fillna_value
        procedure :: ffill => fortarray_ffill
        procedure :: where_custom => fortarray_where_custom
        procedure :: where_complex => fortarray_where_complex
        
        ! Data access and conversion methods
        procedure :: values => fortarray_values_all
        procedure :: values_copy => fortarray_values_copy
        procedure :: to_netcdf => fortarray_to_netcdf_file
        procedure :: to_hdf5 => fortarray_to_hdf5
        procedure :: to_zarr => fortarray_to_zarr
        procedure :: to_binary => fortarray_to_binary
        procedure :: to_numpy => fortarray_to_numpy_like
        procedure :: to_pandas => fortarray_to_pandas_like
        
        ! Table-style operations for interoperability
        procedure :: head => fortarray_head
        procedure :: tail => fortarray_tail
        procedure :: describe => fortarray_describe
        
        ! Enhanced selection methods (Sprint 7)
        procedure :: sel_nearest => fortarray_sel_nearest_r64
        procedure :: sel_interp => fortarray_sel_interp_linear
        procedure :: sel_string => fortarray_sel_string
        procedure :: sel_datetime => fortarray_sel_datetime
        procedure :: sel_multi => fortarray_sel_multi_coord
        procedure :: sel_method => fortarray_sel_method_choice
        
        ! Dimension manipulation methods (Sprint 10)
        procedure :: transpose => fortarray_transpose
        procedure :: squeeze => fortarray_squeeze
        procedure :: expand_dims => fortarray_expand_dims
        procedure :: rename_dims => fortarray_rename_dims
        procedure :: unstack => fortarray_unstack
        
        ! Missing data advanced handling (Sprint 11)
        procedure :: interpolate_na => fortarray_interpolate_na
        procedure :: bfill => fortarray_bfill
        procedure :: dropna => fortarray_dropna_advanced
        
        ! Performance optimization layer (Sprint 12)
        procedure :: sel_simd => fortarray_sel_simd
        procedure :: sel_range_simd => fortarray_sel_range_simd
        procedure :: sel_nearest_parallel => fortarray_sel_nearest_parallel
        procedure :: interp_parallel => fortarray_interp_parallel
        procedure :: sel_multipoint_parallel => fortarray_sel_multipoint_parallel
        procedure :: optimize_layout => fortarray_optimize_layout
        procedure :: prefetch_optimize => fortarray_prefetch_optimize
        procedure :: chunk_optimize => fortarray_chunk_optimize
        procedure :: sel_binary_search => fortarray_sel_binary_search
        procedure :: sel_binary_interp => fortarray_sel_binary_interp
        procedure :: sel_range_binary => fortarray_sel_range_binary
        procedure :: sel_batch_sorted => fortarray_sel_batch_sorted
        procedure :: sel_cached => fortarray_sel_cached
        procedure :: invalidate_cache => fortarray_invalidate_cache
        procedure :: get_cache_stats => fortarray_get_cache_stats
        procedure :: optimize_memory => fortarray_optimize_memory
        procedure :: sel_range_parallel => fortarray_sel_range_parallel
        
        ! Groupby methods (Sprint 13-15)
        procedure :: groupby => fortarray_groupby
        procedure :: groupby_coord => fortarray_groupby_coord
        procedure :: groupby_bins => fortarray_groupby_bins
        procedure :: groupby_quantiles => fortarray_groupby_quantiles
        
        ! Resampling methods (Sprint 16)
        procedure :: resample => fortarray_resample
        procedure :: interpolate => fortarray_interpolate_resample
        
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
    
    ! Groupby type for groupby operations (Sprint 13)
    type :: groupby_t
        logical :: initialized = .false.
        character(len=MAX_NAME_LEN) :: dim_name = ""
        integer :: n_groups = 0
        
        ! Group information
        character(len=MAX_NAME_LEN), allocatable :: group_names(:)
        integer, allocatable :: group_sizes(:)
        integer, allocatable :: group_indices(:, :)  ! Start and end indices for each group
        integer, allocatable :: group_members(:, :)   ! Actual member indices for each group
        
        ! Parent array reference (not owning)
        class(fortarray_t), pointer :: parent_array => null()
        
        ! Group iterator state
        integer :: current_group = 0
        logical :: iterator_active = .false.
    contains
        final :: groupby_finalizer
        
        ! Groupby aggregation methods
        procedure :: mean => groupby_mean
        procedure :: sum => groupby_sum
        procedure :: std => groupby_std
        procedure :: max => groupby_max
        procedure :: min => groupby_min
        
        ! Group access methods
        procedure :: get_group => groupby_get_group
        
        ! Group iteration methods
        procedure :: reset_iterator => groupby_reset_iterator
        procedure :: has_next_group => groupby_has_next_group
        procedure :: next_group => groupby_next_group
        
        ! Complex operations
        procedure :: apply => groupby_apply
        procedure :: transform => groupby_transform
    end type groupby_t
    
    ! Legacy support - dataframe_t is now an alias for fortarray_t
    type :: dataframe_t
        type(fortarray_t) :: var
    end type dataframe_t
    
    ! Make types public
    public :: fortarray_t, dataset_t, coordinate_t, data_storage_t, dimension_t, write_options_t
    public :: groupby_t  ! Groupby type for Sprint 13
    public :: dataframe_t  ! For backward compatibility
    public :: MAX_NAME_LEN, MAX_ATTR_LEN
    public :: DTYPE_INT8, DTYPE_INT16, DTYPE_INT32, DTYPE_INT64
    public :: DTYPE_REAL32, DTYPE_REAL64, DTYPE_CHAR, DTYPE_LOGICAL
    public :: ATTR_TYPE_STRING, ATTR_TYPE_NUMERIC, ATTR_TYPE_LOGICAL
    public :: attribute_t
    public :: fortarray_stack  ! Stack function
    
    ! Interface block for external procedures
    interface
        module function fortarray_sel(this, coord_name, value, start_val, stop_val, method) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            real(real64), intent(in), optional :: value      ! For point selection
            real(real64), intent(in), optional :: start_val, stop_val  ! For range selection
            character(len=*), intent(in), optional :: method
            type(fortarray_t) :: result_array
        end function fortarray_sel
        
        module function fortarray_sel_point_r64(this, coord_name, value, method, drop) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            real(real64), intent(in) :: value
            character(len=*), intent(in), optional :: method
            logical, intent(in), optional :: drop
            type(fortarray_t) :: result_array
        end function fortarray_sel_point_r64
        
        module function fortarray_sel_range_r64(this, coord_name, start_val, stop_val, step_val) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            real(real64), intent(in) :: start_val, stop_val
            real(real64), intent(in), optional :: step_val
            type(fortarray_t) :: result_array
        end function fortarray_sel_range_r64
        
        module function fortarray_isel(this, dim_name, index, start_idx, stop_idx, step_idx) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: dim_name
            integer, intent(in), optional :: index      ! For point selection
            integer, intent(in), optional :: start_idx, stop_idx, step_idx  ! For range selection
            type(fortarray_t) :: result_array
        end function fortarray_isel
        
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
        
        module function fortarray_isel_indices(this, dim_name, indices) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: dim_name
            integer, dimension(:), intent(in) :: indices
            type(fortarray_t) :: result_array
        end function fortarray_isel_indices
        
        module function fortarray_mean_all(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_mean_all
        
        module function fortarray_sum_all(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_sum_all
        
        module function fortarray_max_all(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_max_all
        
        module function fortarray_min_all(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_min_all
        
        module function fortarray_std_all(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_std_all
        
        ! Advanced aggregation methods (Sprint 9)
        module function fortarray_quantile(this, q, axis, interpolation) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: q
            character(len=*), intent(in), optional :: axis
            character(len=*), intent(in), optional :: interpolation
            type(fortarray_t) :: result_array
        end function fortarray_quantile
        
        module function fortarray_percentile(this, p, axis, interpolation) result(percentile_val)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: p
            character(len=*), intent(in), optional :: axis
            character(len=*), intent(in), optional :: interpolation
            real(real64) :: percentile_val
        end function fortarray_percentile
        
        module function fortarray_weighted_mean(this, weights, axis) result(weighted_mean_val)
            class(fortarray_t), intent(in) :: this
            class(fortarray_t), intent(in) :: weights
            character(len=*), intent(in), optional :: axis
            real(real64) :: weighted_mean_val
        end function fortarray_weighted_mean
        
        module function fortarray_weighted_sum(this, weights, axis) result(weighted_sum_val)
            class(fortarray_t), intent(in) :: this
            class(fortarray_t), intent(in) :: weights
            character(len=*), intent(in), optional :: axis
            real(real64) :: weighted_sum_val
        end function fortarray_weighted_sum
        
        module function fortarray_weighted_std(this, weights, axis) result(result_array)
            class(fortarray_t), intent(in) :: this
            class(fortarray_t), intent(in) :: weights
            character(len=*), intent(in), optional :: axis
            type(fortarray_t) :: result_array
        end function fortarray_weighted_std
        
        module function fortarray_cumsum(this, axis) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: axis
            type(fortarray_t) :: result_array
        end function fortarray_cumsum
        
        module function fortarray_cumprod(this, axis) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: axis
            type(fortarray_t) :: result_array
        end function fortarray_cumprod
        
        module function fortarray_cummin(this, axis) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: axis
            type(fortarray_t) :: result_array
        end function fortarray_cummin
        
        module function fortarray_cummax(this, axis) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: axis
            type(fortarray_t) :: result_array
        end function fortarray_cummax
        
        module function fortarray_rolling_mean(this, window, center, min_periods) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in) :: window
            logical, intent(in), optional :: center
            integer, intent(in), optional :: min_periods
            type(fortarray_t) :: result_array
        end function fortarray_rolling_mean
        
        module function fortarray_rolling_sum(this, window, center, min_periods) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in) :: window
            logical, intent(in), optional :: center
            integer, intent(in), optional :: min_periods
            type(fortarray_t) :: result_array
        end function fortarray_rolling_sum
        
        module function fortarray_rolling_std(this, window, center, min_periods) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in) :: window
            logical, intent(in), optional :: center
            integer, intent(in), optional :: min_periods
            type(fortarray_t) :: result_array
        end function fortarray_rolling_std
        
        ! Filtering and conditional operations
        module function fortarray_where_gt_r64(this, threshold, other_value) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: threshold
            real(real64), intent(in) :: other_value
            type(fortarray_t) :: result_array
        end function fortarray_where_gt_r64
        
        module function fortarray_where_lt_r64(this, threshold, other_value) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: threshold
            real(real64), intent(in) :: other_value
            type(fortarray_t) :: result_array
        end function fortarray_where_lt_r64
        
        module function fortarray_gt_r64(this, threshold) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: threshold
            type(fortarray_t) :: result_array
        end function fortarray_gt_r64
        
        module function fortarray_lt_r64(this, threshold) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: threshold
            type(fortarray_t) :: result_array
        end function fortarray_lt_r64
        
        module function fortarray_mask_where(this, mask) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t), intent(in) :: mask
            type(fortarray_t) :: result_array
        end function fortarray_mask_where
        
        module function fortarray_logical_and(this, other) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t), intent(in) :: other
            type(fortarray_t) :: result_array
        end function fortarray_logical_and
        
        module function fortarray_logical_or(this, other) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t), intent(in) :: other
            type(fortarray_t) :: result_array
        end function fortarray_logical_or
        
        module function fortarray_logical_not(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_logical_not
        
        module function fortarray_fillna_value(this, fill_value) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: fill_value
            type(fortarray_t) :: result_array
        end function fortarray_fillna_value
        
        module function fortarray_ffill(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_ffill
        
        module function fortarray_where_custom(this, condition, other_value) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: condition
            real(real64), intent(in) :: other_value
            type(fortarray_t) :: result_array
        end function fortarray_where_custom
        
        module function fortarray_where_complex(this, expression, other_value) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: expression
            real(real64), intent(in) :: other_value
            type(fortarray_t) :: result_array
        end function fortarray_where_complex
        
        ! Data access and conversion methods
        module function fortarray_values_all(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_values_all
        
        module function fortarray_values_copy(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_values_copy
        
        module function fortarray_to_netcdf_file(this, filename) result(status)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: filename
            integer :: status
        end function fortarray_to_netcdf_file
        
        module function fortarray_to_hdf5(this, filename, group, append, options) result(status)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: filename
            character(len=*), intent(in), optional :: group
            logical, intent(in), optional :: append
            type(write_options_t), intent(in), optional :: options
            integer :: status
        end function fortarray_to_hdf5
        
        module function fortarray_to_zarr(this, dirname, chunks) result(status)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: dirname
            integer, dimension(:), intent(in), optional :: chunks
            integer :: status
        end function fortarray_to_zarr
        
        module function fortarray_to_binary(this, filename) result(status)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: filename
            integer :: status
        end function fortarray_to_binary
        
        module function fortarray_to_numpy_like(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_to_numpy_like
        
        module function fortarray_to_pandas_like(this, index_from_coords, flatten_multiindex, &
                                           preserve_metadata, datetime_index) result(result_array)
            class(fortarray_t), intent(in) :: this
            logical, intent(in), optional :: index_from_coords
            logical, intent(in), optional :: flatten_multiindex
            logical, intent(in), optional :: preserve_metadata
            logical, intent(in), optional :: datetime_index
            type(fortarray_t) :: result_array
        end function fortarray_to_pandas_like
        
        module function fortarray_head(this, n) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: n
            type(fortarray_t) :: result_array
        end function fortarray_head
        
        module function fortarray_tail(this, n) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: n
            type(fortarray_t) :: result_array
        end function fortarray_tail
        
        module function fortarray_describe(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_describe
        
        ! Enhanced selection methods (Sprint 7)
        module function fortarray_sel_nearest_r64(this, coord_name, value, method) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            real(real64), intent(in) :: value
            character(len=*), intent(in), optional :: method  ! 'linear', 'cubic', 'nearest'
            type(fortarray_t) :: result_array
        end function fortarray_sel_nearest_r64
        
        module function fortarray_sel_interp_linear(this, coord_name, value, method) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            real(real64), intent(in) :: value
            character(len=*), intent(in), optional :: method  ! 'linear', 'cubic', 'spline'
            type(fortarray_t) :: result_array
        end function fortarray_sel_interp_linear
        
        module function fortarray_sel_string(this, coord_name, value) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            character(len=*), intent(in) :: value
            type(fortarray_t) :: result_array
        end function fortarray_sel_string
        
        module function fortarray_sel_datetime(this, coord_name, value, tolerance) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            character(len=*), intent(in) :: value  ! ISO 8601 format
            real(real64), intent(in), optional :: tolerance
            type(fortarray_t) :: result_array
        end function fortarray_sel_datetime
        
        module function fortarray_sel_multi_coord(this, coord_names, values, method) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_names(:)
            real(real64), intent(in) :: values(:)
            character(len=*), intent(in), optional :: method  ! 'exact', 'nearest', 'interp'
            type(fortarray_t) :: result_array
        end function fortarray_sel_multi_coord
        
        module function fortarray_sel_method_choice(this, coord_name, value, method, tolerance) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            real(real64), intent(in) :: value
            character(len=*), intent(in) :: method  ! 'exact', 'nearest', 'interpolate'
            real(real64), intent(in), optional :: tolerance
            type(fortarray_t) :: result_array
        end function fortarray_sel_method_choice
        
        ! Dimension manipulation methods (Sprint 10)
        module function fortarray_transpose(this, axes) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, dimension(:), intent(in), optional :: axes
            type(fortarray_t) :: result_array
        end function fortarray_transpose
        
        module function fortarray_squeeze(this, axis) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: axis
            type(fortarray_t) :: result_array
        end function fortarray_squeeze
        
        module function fortarray_expand_dims(this, axis) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in) :: axis
            type(fortarray_t) :: result_array
        end function fortarray_expand_dims
        
        module function fortarray_rename_dims(this, old_name, new_name, new_names) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in), optional :: old_name, new_name
            character(len=*), dimension(:), intent(in), optional :: new_names
            type(fortarray_t) :: result_array
        end function fortarray_rename_dims
        
        module function fortarray_stack(arrays, axis, out_name) result(result_array)
            type(fortarray_t), dimension(:), intent(in) :: arrays
            integer, intent(in) :: axis
            character(len=*), intent(in), optional :: out_name
            type(fortarray_t) :: result_array
        end function fortarray_stack
        
        module function fortarray_unstack(this, axis) result(arrays)
            class(fortarray_t), intent(in) :: this
            integer, intent(in) :: axis
            type(fortarray_t), dimension(:), allocatable :: arrays
        end function fortarray_unstack
        
        ! Missing data advanced handling (Sprint 11)
        module function fortarray_interpolate_na(this, method, order, limit) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in), optional :: method
            integer, intent(in), optional :: order
            integer, intent(in), optional :: limit
            type(fortarray_t) :: result_array
        end function fortarray_interpolate_na
        
        module function fortarray_bfill(this, limit) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: limit
            type(fortarray_t) :: result_array
        end function fortarray_bfill
        
        module function fortarray_dropna_advanced(this, axis, how, thresh, subset) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, intent(in), optional :: axis
            character(len=*), intent(in), optional :: how
            integer, intent(in), optional :: thresh
            character(len=*), dimension(:), intent(in), optional :: subset
            type(fortarray_t) :: result_array
        end function fortarray_dropna_advanced
        
        ! Performance optimization layer (Sprint 12)
        module function fortarray_sel_simd(this, x) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x
            type(fortarray_t) :: result_array
        end function fortarray_sel_simd
        
        module function fortarray_sel_range_simd(this, x_min, x_max) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x_min, x_max
            type(fortarray_t) :: result_array
        end function fortarray_sel_range_simd
        
        module function fortarray_sel_nearest_parallel(this, x) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x
            type(fortarray_t) :: result_array
        end function fortarray_sel_nearest_parallel
        
        module function fortarray_interp_parallel(this, x) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x
            type(fortarray_t) :: result_array
        end function fortarray_interp_parallel
        
        module function fortarray_sel_multipoint_parallel(this, x_values) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), dimension(:), intent(in) :: x_values
            type(fortarray_t) :: result_array
        end function fortarray_sel_multipoint_parallel
        
        module function fortarray_optimize_layout(this, layout) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: layout
            type(fortarray_t) :: result_array
        end function fortarray_optimize_layout
        
        module function fortarray_prefetch_optimize(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_prefetch_optimize
        
        module function fortarray_chunk_optimize(this, chunk_size) result(result_array)
            class(fortarray_t), intent(in) :: this
            integer, dimension(:), intent(in) :: chunk_size
            type(fortarray_t) :: result_array
        end function fortarray_chunk_optimize
        
        module function fortarray_sel_binary_search(this, x) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x
            type(fortarray_t) :: result_array
        end function fortarray_sel_binary_search
        
        module function fortarray_sel_binary_interp(this, x) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x
            type(fortarray_t) :: result_array
        end function fortarray_sel_binary_interp
        
        module function fortarray_sel_range_binary(this, x_min, x_max) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x_min, x_max
            type(fortarray_t) :: result_array
        end function fortarray_sel_range_binary
        
        module function fortarray_sel_batch_sorted(this, x_values) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), dimension(:), intent(in) :: x_values
            type(fortarray_t) :: result_array
        end function fortarray_sel_batch_sorted
        
        module function fortarray_sel_cached(this, x) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x
            type(fortarray_t) :: result_array
        end function fortarray_sel_cached
        
        module subroutine fortarray_invalidate_cache(this)
            class(fortarray_t), intent(inout) :: this
        end subroutine fortarray_invalidate_cache
        
        module subroutine fortarray_get_cache_stats(this, hits, misses, hit_ratio)
            class(fortarray_t), intent(in) :: this
            integer, intent(out) :: hits, misses
            real(real64), intent(out) :: hit_ratio
        end subroutine fortarray_get_cache_stats
        
        module function fortarray_optimize_memory(this) result(result_array)
            class(fortarray_t), intent(in) :: this
            type(fortarray_t) :: result_array
        end function fortarray_optimize_memory
        
        module function fortarray_sel_range_parallel(this, x_min, x_max) result(result_array)
            class(fortarray_t), intent(in) :: this
            real(real64), intent(in) :: x_min, x_max
            type(fortarray_t) :: result_array
        end function fortarray_sel_range_parallel
        
        ! Groupby methods (Sprint 13)
        module function fortarray_groupby(this, dim_name, groups) result(gb)
            class(fortarray_t), intent(in), target :: this
            character(len=*), intent(in) :: dim_name
            class(fortarray_t), intent(in) :: groups
            type(groupby_t) :: gb
        end function fortarray_groupby
        
        module function fortarray_groupby_coord(this, coord_name) result(gb)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            type(groupby_t) :: gb
        end function fortarray_groupby_coord
        
        module function fortarray_groupby_bins(this, coord_name, bins) result(gb)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            integer, intent(in) :: bins
            type(groupby_t) :: gb
        end function fortarray_groupby_bins
        
        module function fortarray_groupby_quantiles(this, coord_name, n_quantiles) result(gb)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: coord_name
            integer, intent(in) :: n_quantiles
            type(groupby_t) :: gb
        end function fortarray_groupby_quantiles
        
        ! Resampling methods (Sprint 16)
        module function fortarray_resample(this, freq, align) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in) :: freq
            character(len=*), intent(in), optional :: align
            type(fortarray_t) :: result_array
        end function fortarray_resample
        
        module function fortarray_interpolate_resample(this, method) result(result_array)
            class(fortarray_t), intent(in) :: this
            character(len=*), intent(in), optional :: method
            type(fortarray_t) :: result_array
        end function fortarray_interpolate_resample
        
        ! Groupby aggregation methods
        module function groupby_mean(this, skipna) result(result_array)
            class(groupby_t), intent(in) :: this
            logical, intent(in), optional :: skipna
            type(fortarray_t) :: result_array
        end function groupby_mean
        
        module function groupby_sum(this, skipna) result(result_array)
            class(groupby_t), intent(in) :: this
            logical, intent(in), optional :: skipna
            type(fortarray_t) :: result_array
        end function groupby_sum
        
        module function groupby_std(this, skipna) result(result_array)
            class(groupby_t), intent(in) :: this
            logical, intent(in), optional :: skipna
            type(fortarray_t) :: result_array
        end function groupby_std
        
        module function groupby_max(this, skipna) result(result_array)
            class(groupby_t), intent(in) :: this
            logical, intent(in), optional :: skipna
            type(fortarray_t) :: result_array
        end function groupby_max
        
        module function groupby_min(this, skipna) result(result_array)
            class(groupby_t), intent(in) :: this
            logical, intent(in), optional :: skipna
            type(fortarray_t) :: result_array
        end function groupby_min
        
        ! Group access methods
        module function groupby_get_group(this, group_name) result(result_array)
            class(groupby_t), intent(in) :: this
            character(len=*), intent(in) :: group_name
            type(fortarray_t) :: result_array
        end function groupby_get_group
        
        ! Group iteration methods
        module subroutine groupby_reset_iterator(this)
            class(groupby_t), intent(inout) :: this
        end subroutine groupby_reset_iterator
        
        module function groupby_has_next_group(this) result(has_next)
            class(groupby_t), intent(in) :: this
            logical :: has_next
        end function groupby_has_next_group
        
        module function groupby_next_group(this, group_name) result(result_array)
            class(groupby_t), intent(inout) :: this
            character(len=*), intent(out) :: group_name
            type(fortarray_t) :: result_array
        end function groupby_next_group
        
        ! Complex operations
        module function groupby_apply(this, operation) result(result_array)
            class(groupby_t), intent(in) :: this
            character(len=*), intent(in) :: operation
            type(fortarray_t) :: result_array
        end function groupby_apply
        
        module function groupby_transform(this, operation) result(result_array)
            class(groupby_t), intent(in) :: this
            character(len=*), intent(in) :: operation
            type(fortarray_t) :: result_array
        end function groupby_transform
        
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
    
    !> Finalizer for groupby_t type
    subroutine groupby_finalizer(gb)
        type(groupby_t), intent(inout) :: gb
        
        if (allocated(gb%group_names)) deallocate(gb%group_names)
        if (allocated(gb%group_sizes)) deallocate(gb%group_sizes)
        if (allocated(gb%group_indices)) deallocate(gb%group_indices)
        if (allocated(gb%group_members)) deallocate(gb%group_members)
        ! Note: parent_array is not owned, so we don't deallocate it
        if (associated(gb%parent_array)) gb%parent_array => null()
    end subroutine groupby_finalizer
    
end module fortarray_types