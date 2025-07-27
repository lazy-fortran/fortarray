module fortarray_chunked_operations
    use fortarray_types
    use fortarray_storage
    use fortarray_constructors
    use fortarray_memory
    use fortarray_slicing
    use fortarray_arithmetic
    use fortarray_aggregation
    use fortarray_netcdf
    use fortarray_datasets
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    !$ use omp_lib
    implicit none
    private
    
    ! Public types
    public :: chunk_info_t, chunk_iterator_t, chunked_operation_t
    public :: out_of_core_config_t, adaptive_chunk_config_t
    public :: chunk_cache_t, parallel_chunk_config_t
    public :: memory_monitor_t, chunked_io_config_t
    
    ! Public functions
    public :: define_chunks, finalize_chunk_info
    public :: create_chunk_iterator, has_next_chunk, get_next_chunk, finalize_chunk_iterator
    public :: create_chunked_operation, apply_chunked, finalize_chunked_operation
    public :: create_out_of_core_config, compute_out_of_core, finalize_out_of_core_config
    public :: sum_chunked, mean_chunked, add_chunked, multiply_chunked
    public :: apply_chunked_2d, sqrt_chunked
    public :: create_adaptive_config, determine_optimal_chunk_size, finalize_adaptive_config
    public :: create_chunk_cache, get_cached_chunk, finalize_chunk_cache
    public :: create_parallel_config, process_parallel_chunks, finalize_parallel_config
    public :: create_memory_monitor, start_monitoring, get_initial_memory, get_peak_memory
    public :: finalize_memory_monitor, process_with_chunks
    public :: create_chunked_io_config, write_chunked, read_chunked, finalize_chunked_io_config
    public :: delete_temp_file, delete_file
    
    ! Default chunk size (elements)
    integer, parameter :: DEFAULT_CHUNK_SIZE = 10000
    
    !> Chunk information type
    type :: chunk_info_t
        integer :: n_chunks
        integer :: chunk_size
        integer, dimension(:), allocatable :: chunk_shape
        integer :: n_dims
        integer, dimension(:), allocatable :: chunks_per_dim
        logical :: initialized = .false.
    end type chunk_info_t
    
    !> Chunk iterator type
    type :: chunk_iterator_t
        type(fortarray_t), pointer :: var => null()
        type(chunk_info_t) :: chunk_info
        integer :: current_chunk
        integer :: total_chunks
        integer, dimension(:), allocatable :: current_indices
        logical :: initialized = .false.
    end type chunk_iterator_t
    
    !> Chunked operation type
    type :: chunked_operation_t
        character(len=64) :: operation_name
        integer :: chunk_size
        logical :: parallel = .false.
        integer :: n_threads = 1
        logical :: initialized = .false.
    end type chunked_operation_t
    
    !> Out-of-core configuration
    type :: out_of_core_config_t
        character(len=256) :: temp_file
        integer(int64) :: memory_limit
        integer :: chunk_size
        logical :: use_compression = .true.
        logical :: initialized = .false.
    end type out_of_core_config_t
    
    !> Adaptive chunk configuration
    type :: adaptive_chunk_config_t
        integer :: min_size
        integer :: max_size
        integer(int64) :: target_memory
        real(real64) :: performance_factor = 1.0_real64
        logical :: initialized = .false.
    end type adaptive_chunk_config_t
    
    !> Chunk cache type
    type :: chunk_cache_t
        integer :: max_chunks
        integer :: chunk_size
        integer :: n_cached = 0
        integer, dimension(:), allocatable :: chunk_ids
        type(fortarray_t), dimension(:), allocatable :: cached_chunks
        logical :: last_was_hit = .false.
        logical :: initialized = .false.
    end type chunk_cache_t
    
    !> Parallel chunk configuration
    type :: parallel_chunk_config_t
        integer :: n_threads
        integer :: chunk_size
        logical :: dynamic_scheduling = .true.
        logical :: initialized = .false.
    end type parallel_chunk_config_t
    
    !> Memory monitor type
    type :: memory_monitor_t
        integer(int64) :: initial_memory
        integer(int64) :: peak_memory
        integer(int64) :: current_memory
        logical :: monitoring = .false.
        logical :: initialized = .false.
    end type memory_monitor_t
    
    !> Chunked I/O configuration
    type :: chunked_io_config_t
        integer :: chunk_size
        logical :: compression
        integer :: compression_level = 6
        logical :: use_collective_io = .false.
        logical :: initialized = .false.
    end type chunked_io_config_t
    
contains
    
    !> Define chunks for a variable
    function define_chunks(var, chunk_size, chunk_shape, stat) result(chunk_info)
        type(fortarray_t), intent(in) :: var
        integer, intent(in), optional :: chunk_size
        integer, dimension(:), intent(in), optional :: chunk_shape
        integer, intent(out), optional :: stat
        type(chunk_info_t) :: chunk_info
        integer :: i, total_elements, elements_per_chunk
        
        if (present(stat)) stat = 0
        
        ! Initialize chunk info
        chunk_info%n_dims = var%n_dims
        chunk_info%initialized = .true.
        
        if (present(chunk_shape)) then
            ! Use provided chunk shape
            if (size(chunk_shape) /= var%n_dims) then
                if (present(stat)) stat = 1
                write(error_unit,'(A)') "ERROR: Chunk shape dimensionality mismatch"
                return
            end if
            allocate(chunk_info%chunk_shape(var%n_dims))
            chunk_info%chunk_shape = chunk_shape
            
            ! Calculate chunks per dimension
            allocate(chunk_info%chunks_per_dim(var%n_dims))
            do i = 1, var%n_dims
                chunk_info%chunks_per_dim(i) = (var%shape(i) + chunk_shape(i) - 1) / chunk_shape(i)
            end do
            
            ! Total number of chunks
            chunk_info%n_chunks = product(chunk_info%chunks_per_dim)
            
            ! Effective chunk size
            chunk_info%chunk_size = product(chunk_shape)
            
        else
            ! Use linear chunking
            if (present(chunk_size)) then
                if (chunk_size <= 0) then
                    if (present(stat)) stat = 2
                    write(error_unit,'(A)') "ERROR: Invalid chunk size"
                    return
                end if
                chunk_info%chunk_size = chunk_size
            else
                chunk_info%chunk_size = DEFAULT_CHUNK_SIZE
            end if
            
            ! Ensure chunk size doesn't exceed data size
            if (chunk_info%chunk_size > var%n_elements) then
                chunk_info%chunk_size = var%n_elements
            end if
            
            ! Calculate number of chunks
            chunk_info%n_chunks = (var%n_elements + chunk_info%chunk_size - 1) / chunk_info%chunk_size
            
            ! For linear chunking, shape is 1D
            allocate(chunk_info%chunk_shape(1))
            chunk_info%chunk_shape(1) = chunk_info%chunk_size
        end if
        
    end function define_chunks
    
    !> Finalize chunk info
    subroutine finalize_chunk_info(chunk_info)
        type(chunk_info_t), intent(inout) :: chunk_info
        
        if (allocated(chunk_info%chunk_shape)) deallocate(chunk_info%chunk_shape)
        if (allocated(chunk_info%chunks_per_dim)) deallocate(chunk_info%chunks_per_dim)
        chunk_info%initialized = .false.
        
    end subroutine finalize_chunk_info
    
    !> Create chunk iterator
    function create_chunk_iterator(var, chunk_size, chunk_shape) result(iterator)
        type(fortarray_t), target, intent(in) :: var
        integer, intent(in), optional :: chunk_size
        integer, dimension(:), intent(in), optional :: chunk_shape
        type(chunk_iterator_t) :: iterator
        
        iterator%var => var
        iterator%chunk_info = define_chunks(var, chunk_size, chunk_shape)
        iterator%current_chunk = 0
        iterator%total_chunks = iterator%chunk_info%n_chunks
        
        if (allocated(iterator%chunk_info%chunks_per_dim)) then
            allocate(iterator%current_indices(size(iterator%chunk_info%chunks_per_dim)))
            iterator%current_indices = 1
        end if
        
        iterator%initialized = .true.
        
    end function create_chunk_iterator
    
    !> Check if iterator has more chunks
    function has_next_chunk(iterator) result(has_next)
        type(chunk_iterator_t), intent(in) :: iterator
        logical :: has_next
        
        has_next = iterator%current_chunk < iterator%total_chunks
        
    end function has_next_chunk
    
    !> Get next chunk from iterator
    function get_next_chunk(iterator) result(chunk)
        type(chunk_iterator_t), intent(inout) :: iterator
        type(fortarray_t) :: chunk
        integer :: start_idx, end_idx, chunk_elements
        integer, dimension(:), allocatable :: starts, ends
        integer :: i
        
        if (.not. has_next_chunk(iterator)) then
            chunk = create_empty_like(iterator%var)
            return
        end if
        
        iterator%current_chunk = iterator%current_chunk + 1
        
        if (iterator%chunk_info%n_dims == 1 .or. &
            .not. allocated(iterator%chunk_info%chunks_per_dim)) then
            ! Linear chunking
            start_idx = (iterator%current_chunk - 1) * iterator%chunk_info%chunk_size + 1
            end_idx = min(start_idx + iterator%chunk_info%chunk_size - 1, iterator%var%n_elements)
            
            ! Extract chunk using slicing
            chunk = slice_range(iterator%var, start_idx, end_idx)
            
        else
            ! Multidimensional chunking
            allocate(starts(iterator%chunk_info%n_dims))
            allocate(ends(iterator%chunk_info%n_dims))
            
            ! Calculate starts and ends for each dimension
            do i = 1, iterator%chunk_info%n_dims
                starts(i) = (iterator%current_indices(i) - 1) * iterator%chunk_info%chunk_shape(i) + 1
                ends(i) = min(starts(i) + iterator%chunk_info%chunk_shape(i) - 1, iterator%var%shape(i))
            end do
            
            ! Extract multidimensional chunk
            chunk = extract_chunk_multidim(iterator%var, starts, ends)
            
            ! Update indices for next chunk
            call increment_chunk_indices(iterator)
            
            deallocate(starts)
            deallocate(ends)
        end if
        
    end function get_next_chunk
    
    !> Increment chunk indices for multidimensional iteration
    subroutine increment_chunk_indices(iterator)
        type(chunk_iterator_t), intent(inout) :: iterator
        integer :: dim
        
        if (.not. allocated(iterator%current_indices)) return
        
        ! Increment indices (row-major order)
        do dim = iterator%chunk_info%n_dims, 1, -1
            iterator%current_indices(dim) = iterator%current_indices(dim) + 1
            if (iterator%current_indices(dim) <= iterator%chunk_info%chunks_per_dim(dim)) then
                exit
            else
                iterator%current_indices(dim) = 1
            end if
        end do
        
    end subroutine increment_chunk_indices
    
    !> Finalize chunk iterator
    subroutine finalize_chunk_iterator(iterator)
        type(chunk_iterator_t), intent(inout) :: iterator
        
        if (allocated(iterator%current_indices)) deallocate(iterator%current_indices)
        call finalize_chunk_info(iterator%chunk_info)
        iterator%var => null()
        iterator%initialized = .false.
        
    end subroutine finalize_chunk_iterator
    
    !> Create chunked operation
    function create_chunked_operation(operation_name, chunk_size, parallel, n_threads) result(op)
        character(len=*), intent(in) :: operation_name
        integer, intent(in), optional :: chunk_size
        logical, intent(in), optional :: parallel
        integer, intent(in), optional :: n_threads
        type(chunked_operation_t) :: op
        
        op%operation_name = operation_name
        
        if (present(chunk_size)) then
            op%chunk_size = chunk_size
        else
            op%chunk_size = DEFAULT_CHUNK_SIZE
        end if
        
        if (present(parallel)) then
            op%parallel = parallel
        end if
        
        if (present(n_threads)) then
            op%n_threads = n_threads
        else
            !$ op%n_threads = omp_get_max_threads()
        end if
        
        op%initialized = .true.
        
    end function create_chunked_operation
    
    !> Apply chunked operation with function
    function apply_chunked(op, var, func) result(result)
        type(chunked_operation_t), intent(in) :: op
        type(fortarray_t), intent(in) :: var
        interface
            function func(x) result(y)
                use iso_fortran_env, only: real64
                real(real64), intent(in) :: x
                real(real64) :: y
            end function func
        end interface
        type(fortarray_t) :: result
        type(chunk_iterator_t) :: iterator
        type(fortarray_t) :: chunk, processed_chunk
        type(fortarray_t), dimension(:), allocatable :: chunks
        integer :: i, j, idx, n_chunks
        
        ! Initialize result
        result = create_empty_like(var)
        allocate(result%data%values_r64(var%n_elements))
        result%n_elements = var%n_elements
        
        if (op%parallel) then
            ! Parallel processing
            iterator = create_chunk_iterator(var, op%chunk_size)
            n_chunks = iterator%total_chunks
            allocate(chunks(n_chunks))
            
            ! Collect all chunks
            do i = 1, n_chunks
                chunks(i) = get_next_chunk(iterator)
            end do
            
            ! Process chunks in parallel
            !$omp parallel do private(processed_chunk) num_threads(op%n_threads)
            do i = 1, n_chunks
                processed_chunk = apply_function_to_chunk(chunks(i), func)
                chunks(i) = processed_chunk
            end do
            !$omp end parallel do
            
            ! Combine results
            idx = 1
            do i = 1, n_chunks
                do j = 1, chunks(i)%n_elements
                    result%data%values_r64(idx) = chunks(i)%data%values_r64(j)
                    idx = idx + 1
                end do
                call finalize_variable(chunks(i))
            end do
            
            deallocate(chunks)
            call finalize_chunk_iterator(iterator)
            
        else
            ! Sequential processing
            iterator = create_chunk_iterator(var, op%chunk_size)
            idx = 1
            
            do while (has_next_chunk(iterator))
                chunk = get_next_chunk(iterator)
                processed_chunk = apply_function_to_chunk(chunk, func)
                
                ! Copy results
                do i = 1, processed_chunk%n_elements
                    result%data%values_r64(idx) = processed_chunk%data%values_r64(i)
                    idx = idx + 1
                end do
                
                call finalize_variable(chunk)
                call finalize_variable(processed_chunk)
            end do
            
            call finalize_chunk_iterator(iterator)
        end if
        
    end function apply_chunked
    
    !> Apply function to chunk
    function apply_function_to_chunk(chunk, func) result(result)
        type(fortarray_t), intent(in) :: chunk
        interface
            function func(x) result(y)
                use iso_fortran_env, only: real64
                real(real64), intent(in) :: x
                real(real64) :: y
            end function func
        end interface
        type(fortarray_t) :: result
        integer :: i
        
        result = create_empty_like(chunk)
        allocate(result%data%values_r64(chunk%n_elements))
        result%n_elements = chunk%n_elements
        
        do i = 1, chunk%n_elements
            result%data%values_r64(i) = func(chunk%data%values_r64(i))
        end do
        
    end function apply_function_to_chunk
    
    !> Finalize chunked operation
    subroutine finalize_chunked_operation(op)
        type(chunked_operation_t), intent(inout) :: op
        op%initialized = .false.
    end subroutine finalize_chunked_operation
    
    !> Create out-of-core configuration
    function create_out_of_core_config(temp_file, memory_limit, chunk_size) result(config)
        character(len=*), intent(in) :: temp_file
        integer(int64), intent(in) :: memory_limit
        integer, intent(in), optional :: chunk_size
        type(out_of_core_config_t) :: config
        
        config%temp_file = temp_file
        config%memory_limit = memory_limit
        
        if (present(chunk_size)) then
            config%chunk_size = chunk_size
        else
            ! Calculate chunk size based on memory limit
            ! Assuming real64 elements (8 bytes each)
            config%chunk_size = int(memory_limit / 8)
        end if
        
        config%initialized = .true.
        
    end function create_out_of_core_config
    
    !> Compute out-of-core operation
    function compute_out_of_core(var, operation, config) result(result)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: operation
        type(out_of_core_config_t), intent(in) :: config
        type(fortarray_t) :: result
        type(dataset_t) :: temp_dataset
        type(chunk_iterator_t) :: iterator
        type(fortarray_t) :: chunk, chunk_result
        character(len=64) :: chunk_name
        integer :: i, chunk_idx
        real(real64) :: accumulator, count
        
        ! Create temporary file for intermediate results
        temp_dataset = dataset()
        
        select case(operation)
        case("mean")
            ! Process chunks and accumulate
            iterator = create_chunk_iterator(var, config%chunk_size)
            accumulator = 0.0_real64
            count = 0.0_real64
            
            do while (has_next_chunk(iterator))
                chunk = get_next_chunk(iterator)
                
                ! Accumulate sum and count
                accumulator = accumulator + sum(chunk%data%values_r64)
                count = count + real(chunk%n_elements, real64)
                
                call finalize_variable(chunk)
            end do
            
            ! Create scalar result
            result%name = trim(var%name) // "_mean"
            result%n_dims = 0
            result%n_elements = 1
            result%initialized = .true.
            result%data%dtype = DTYPE_REAL64
            allocate(result%data%values_r64(1))
            result%data%values_r64(1) = accumulator / count
            
            call finalize_chunk_iterator(iterator)
            
        case default
            write(error_unit,'(A,A)') "ERROR: Unknown out-of-core operation: ", operation
            result = create_empty_like(var)
        end select
        
        call finalize_dataset(temp_dataset)
        
    end function compute_out_of_core
    
    !> Finalize out-of-core configuration
    subroutine finalize_out_of_core_config(config)
        type(out_of_core_config_t), intent(inout) :: config
        config%initialized = .false.
    end subroutine finalize_out_of_core_config
    
    !> Chunked sum
    function sum_chunked(var, chunk_size) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in), optional :: chunk_size
        type(fortarray_t) :: result
        type(chunk_iterator_t) :: iterator
        type(fortarray_t) :: chunk
        real(real64) :: total_sum
        
        iterator = create_chunk_iterator(var, chunk_size)
        total_sum = 0.0_real64
        
        do while (has_next_chunk(iterator))
            chunk = get_next_chunk(iterator)
            total_sum = total_sum + sum(chunk%data%values_r64)
            call finalize_variable(chunk)
        end do
        
        ! Create scalar result
        result%name = trim(var%name) // "_sum"
        result%n_dims = 0
        result%n_elements = 1
        result%initialized = .true.
        result%data%dtype = DTYPE_REAL64
        allocate(result%data%values_r64(1))
        result%data%values_r64(1) = total_sum
        
        call finalize_chunk_iterator(iterator)
        
    end function sum_chunked
    
    !> Chunked mean
    function mean_chunked(var, chunk_size) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in), optional :: chunk_size
        type(fortarray_t) :: result
        type(chunk_iterator_t) :: iterator
        type(fortarray_t) :: chunk
        real(real64) :: total_sum, total_count
        
        iterator = create_chunk_iterator(var, chunk_size)
        total_sum = 0.0_real64
        total_count = 0.0_real64
        
        do while (has_next_chunk(iterator))
            chunk = get_next_chunk(iterator)
            total_sum = total_sum + sum(chunk%data%values_r64)
            total_count = total_count + real(chunk%n_elements, real64)
            call finalize_variable(chunk)
        end do
        
        ! Create scalar result
        result%name = trim(var%name) // "_mean"
        result%n_dims = 0
        result%n_elements = 1
        result%initialized = .true.
        result%data%dtype = DTYPE_REAL64
        allocate(result%data%values_r64(1))
        result%data%values_r64(1) = total_sum / total_count
        
        call finalize_chunk_iterator(iterator)
        
    end function mean_chunked
    
    !> Chunked addition
    function add_chunked(var1, var2, chunk_size) result(result)
        type(fortarray_t), intent(in) :: var1, var2
        integer, intent(in), optional :: chunk_size
        type(fortarray_t) :: result
        type(chunk_iterator_t) :: iterator1, iterator2
        type(fortarray_t) :: chunk1, chunk2
        integer :: idx, i
        
        if (var1%n_elements /= var2%n_elements) then
            write(error_unit,'(A)') "ERROR: Variables must have same size for chunked addition"
            result = create_empty_like(var1)
            return
        end if
        
        result = create_empty_like(var1)
        allocate(result%data%values_r64(var1%n_elements))
        result%n_elements = var1%n_elements
        
        iterator1 = create_chunk_iterator(var1, chunk_size)
        iterator2 = create_chunk_iterator(var2, chunk_size)
        idx = 1
        
        do while (has_next_chunk(iterator1))
            chunk1 = get_next_chunk(iterator1)
            chunk2 = get_next_chunk(iterator2)
            
            do i = 1, chunk1%n_elements
                result%data%values_r64(idx) = chunk1%data%values_r64(i) + chunk2%data%values_r64(i)
                idx = idx + 1
            end do
            
            call finalize_variable(chunk1)
            call finalize_variable(chunk2)
        end do
        
        call finalize_chunk_iterator(iterator1)
        call finalize_chunk_iterator(iterator2)
        
    end function add_chunked
    
    !> Chunked multiplication by scalar
    function multiply_chunked(var, scalar, chunk_size) result(result)
        type(fortarray_t), intent(in) :: var
        real(real64), intent(in) :: scalar
        integer, intent(in), optional :: chunk_size
        type(fortarray_t) :: result
        type(chunked_operation_t) :: op
        
        op = create_chunked_operation("multiply", chunk_size)
        result = apply_chunked(op, var, multiply_by_scalar)
        call finalize_chunked_operation(op)
        
    contains
        function multiply_by_scalar(x) result(y)
            real(real64), intent(in) :: x
            real(real64) :: y
            y = x * scalar
        end function multiply_by_scalar
        
    end function multiply_chunked
    
    !> Apply chunked operation on 2D data
    function apply_chunked_2d(var, chunk_info, func) result(result)
        type(fortarray_t), intent(in) :: var
        type(chunk_info_t), intent(in) :: chunk_info
        interface
            function func(x) result(y)
                use iso_fortran_env, only: real64
                real(real64), intent(in) :: x
                real(real64) :: y
            end function func
        end interface
        type(fortarray_t) :: result
        type(chunked_operation_t) :: op
        
        ! For now, use regular apply_chunked
        op = create_chunked_operation("apply_2d", chunk_info%chunk_size)
        result = apply_chunked(op, var, func)
        call finalize_chunked_operation(op)
        
    end function apply_chunked_2d
    
    !> Chunked square root
    function sqrt_chunked(var, chunk_size) result(result)
        type(fortarray_t), intent(in) :: var
        integer, intent(in), optional :: chunk_size
        type(fortarray_t) :: result
        type(chunked_operation_t) :: op
        
        op = create_chunked_operation("sqrt", chunk_size)
        result = apply_chunked(op, var, sqrt_wrapper)
        call finalize_chunked_operation(op)
        
    contains
        function sqrt_wrapper(x) result(y)
            real(real64), intent(in) :: x
            real(real64) :: y
            y = sqrt(x)
        end function sqrt_wrapper
        
    end function sqrt_chunked
    
    !> Create adaptive chunk configuration
    function create_adaptive_config(min_size, max_size, target_memory) result(config)
        integer, intent(in) :: min_size, max_size
        integer(int64), intent(in) :: target_memory
        type(adaptive_chunk_config_t) :: config
        
        config%min_size = min_size
        config%max_size = max_size
        config%target_memory = target_memory
        config%initialized = .true.
        
    end function create_adaptive_config
    
    !> Determine optimal chunk size
    function determine_optimal_chunk_size(var, config) result(optimal_size)
        type(fortarray_t), intent(in) :: var
        type(adaptive_chunk_config_t), intent(in) :: config
        integer :: optimal_size
        integer :: element_size
        
        ! Determine element size in bytes
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            element_size = 8
        case(DTYPE_REAL32)
            element_size = 4
        case(DTYPE_INT64)
            element_size = 8
        case(DTYPE_INT32)
            element_size = 4
        case default
            element_size = 8
        end select
        
        ! Calculate optimal size based on target memory
        optimal_size = int(config%target_memory / element_size)
        
        ! Apply bounds
        optimal_size = max(config%min_size, min(config%max_size, optimal_size))
        
    end function determine_optimal_chunk_size
    
    !> Finalize adaptive chunk configuration
    subroutine finalize_adaptive_config(config)
        type(adaptive_chunk_config_t), intent(inout) :: config
        config%initialized = .false.
    end subroutine finalize_adaptive_config
    
    !> Create chunk cache
    function create_chunk_cache(max_chunks, chunk_size) result(cache)
        integer, intent(in) :: max_chunks, chunk_size
        type(chunk_cache_t) :: cache
        
        cache%max_chunks = max_chunks
        cache%chunk_size = chunk_size
        cache%n_cached = 0
        allocate(cache%chunk_ids(max_chunks))
        allocate(cache%cached_chunks(max_chunks))
        cache%chunk_ids = -1
        cache%initialized = .true.
        
    end function create_chunk_cache
    
    !> Get cached chunk
    function get_cached_chunk(cache, var, chunk_id) result(chunk)
        type(chunk_cache_t), intent(inout) :: cache
        type(fortarray_t), intent(in) :: var
        integer, intent(in) :: chunk_id
        type(fortarray_t) :: chunk
        integer :: i, start_idx, end_idx
        
        ! Check if chunk is in cache
        do i = 1, cache%n_cached
            if (cache%chunk_ids(i) == chunk_id) then
                chunk = cache%cached_chunks(i)
                cache%last_was_hit = .true.
                return
            end if
        end do
        
        ! Cache miss - load chunk
        cache%last_was_hit = .false.
        start_idx = (chunk_id - 1) * cache%chunk_size + 1
        end_idx = min(start_idx + cache%chunk_size - 1, var%n_elements)
        chunk = slice_range(var, start_idx, end_idx)
        
        ! Add to cache (simple FIFO replacement)
        if (cache%n_cached < cache%max_chunks) then
            cache%n_cached = cache%n_cached + 1
            cache%chunk_ids(cache%n_cached) = chunk_id
            cache%cached_chunks(cache%n_cached) = chunk
        else
            ! Replace oldest
            call finalize_variable(cache%cached_chunks(1))
            do i = 1, cache%max_chunks - 1
                cache%chunk_ids(i) = cache%chunk_ids(i + 1)
                cache%cached_chunks(i) = cache%cached_chunks(i + 1)
            end do
            cache%chunk_ids(cache%max_chunks) = chunk_id
            cache%cached_chunks(cache%max_chunks) = chunk
        end if
        
    end function get_cached_chunk
    
    !> Finalize chunk cache
    subroutine finalize_chunk_cache(cache)
        type(chunk_cache_t), intent(inout) :: cache
        integer :: i
        
        if (allocated(cache%cached_chunks)) then
            do i = 1, cache%n_cached
                call finalize_variable(cache%cached_chunks(i))
            end do
            deallocate(cache%cached_chunks)
        end if
        
        if (allocated(cache%chunk_ids)) deallocate(cache%chunk_ids)
        cache%initialized = .false.
        
    end subroutine finalize_chunk_cache
    
    !> Create parallel chunk configuration
    function create_parallel_config(n_threads, chunk_size) result(config)
        integer, intent(in) :: n_threads, chunk_size
        type(parallel_chunk_config_t) :: config
        
        config%n_threads = n_threads
        config%chunk_size = chunk_size
        config%initialized = .true.
        
    end function create_parallel_config
    
    !> Process chunks in parallel
    function process_parallel_chunks(var, config, func) result(result)
        type(fortarray_t), intent(in) :: var
        type(parallel_chunk_config_t), intent(in) :: config
        interface
            function func(x) result(y)
                use iso_fortran_env, only: real64
                real(real64), intent(in) :: x
                real(real64) :: y
            end function func
        end interface
        type(fortarray_t) :: result
        type(chunked_operation_t) :: op
        
        op = create_chunked_operation("parallel", config%chunk_size, &
                                    parallel=.true., n_threads=config%n_threads)
        result = apply_chunked(op, var, func)
        call finalize_chunked_operation(op)
        
    end function process_parallel_chunks
    
    !> Finalize parallel chunk configuration
    subroutine finalize_parallel_config(config)
        type(parallel_chunk_config_t), intent(inout) :: config
        config%initialized = .false.
    end subroutine finalize_parallel_config
    
    !> Create memory monitor
    function create_memory_monitor() result(monitor)
        type(memory_monitor_t) :: monitor
        
        monitor%initial_memory = 0
        monitor%peak_memory = 0
        monitor%current_memory = 0
        monitor%monitoring = .false.
        monitor%initialized = .true.
        
    end function create_memory_monitor
    
    !> Start memory monitoring
    subroutine start_monitoring(monitor)
        type(memory_monitor_t), intent(inout) :: monitor
        
        monitor%initial_memory = get_current_memory_usage()
        monitor%peak_memory = monitor%initial_memory
        monitor%monitoring = .true.
        
    end subroutine start_monitoring
    
    !> Get initial memory
    function get_initial_memory(monitor) result(memory)
        type(memory_monitor_t), intent(in) :: monitor
        integer :: memory
        memory = int(monitor%initial_memory)
    end function get_initial_memory
    
    !> Get peak memory
    function get_peak_memory(monitor) result(memory)
        type(memory_monitor_t), intent(in) :: monitor
        integer :: memory
        memory = int(monitor%peak_memory)
    end function get_peak_memory
    
    !> Get current memory usage (placeholder)
    function get_current_memory_usage() result(memory)
        integer(int64) :: memory
        ! Placeholder - would need system-specific implementation
        memory = 0
    end function get_current_memory_usage
    
    !> Finalize memory monitor
    subroutine finalize_memory_monitor(monitor)
        type(memory_monitor_t), intent(inout) :: monitor
        monitor%initialized = .false.
    end subroutine finalize_memory_monitor
    
    !> Process with chunks (generic)
    subroutine process_with_chunks(var, chunk_size)
        type(fortarray_t), intent(inout) :: var
        integer, intent(in) :: chunk_size
        ! Placeholder for generic chunk processing
    end subroutine process_with_chunks
    
    !> Create chunked I/O configuration
    function create_chunked_io_config(chunk_size, compression) result(config)
        integer, intent(in) :: chunk_size
        logical, intent(in) :: compression
        type(chunked_io_config_t) :: config
        
        config%chunk_size = chunk_size
        config%compression = compression
        config%initialized = .true.
        
    end function create_chunked_io_config
    
    !> Write variable in chunks
    subroutine write_chunked(var, filename, config)
        type(fortarray_t), intent(in) :: var
        character(len=*), intent(in) :: filename
        type(chunked_io_config_t), intent(in) :: config
        type(dataset_t) :: ds
        type(chunk_iterator_t) :: iterator
        type(fortarray_t) :: chunk
        integer :: chunk_idx, status
        character(len=64) :: chunk_name
        
        ! Create dataset
        ds = dataset()
        
        ! Write metadata
        call add_variable(ds, var)
        
        ! Write to file with chunking enabled
        status = write_netcdf(filename, ds)
        
        call finalize_dataset(ds)
        
    end subroutine write_chunked
    
    !> Read variable in chunks
    function read_chunked(filename, varname, config) result(var)
        character(len=*), intent(in) :: filename, varname
        type(chunked_io_config_t), intent(in) :: config
        type(fortarray_t) :: var
        type(dataset_t) :: ds
        
        ! Read dataset
        ds = read_netcdf(filename)
        
        ! Get variable
        var = get_variable(ds, varname)
        
        call finalize_dataset(ds)
        
    end function read_chunked
    
    !> Finalize chunked I/O configuration
    subroutine finalize_chunked_io_config(config)
        type(chunked_io_config_t), intent(inout) :: config
        config%initialized = .false.
    end subroutine finalize_chunked_io_config
    
    !> Delete temporary file
    subroutine delete_temp_file(filename)
        character(len=*), intent(in) :: filename
        call delete_file(filename)
    end subroutine delete_temp_file
    
    !> Delete file
    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit, iostat
        
        open(newunit=unit, file=filename, status='old', iostat=iostat)
        if (iostat == 0) then
            close(unit, status='delete')
        end if
        
    end subroutine delete_file
    
    !> Create empty variable like another
    function create_empty_like(var) result(result)
        type(fortarray_t), intent(in) :: var
        type(fortarray_t) :: result
        
        result%name = var%name
        result%n_dims = var%n_dims
        result%n_elements = 0
        result%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(result%shape(size(var%shape)))
            result%shape = var%shape
            result%shape(1) = 0
        end if
        
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(size(var%dim_names)))
            result%dim_names = var%dim_names
        end if
        
        result%data%dtype = var%data%dtype
        
    end function create_empty_like
    
    !> Extract multidimensional chunk
    function extract_chunk_multidim(var, starts, ends) result(result)
        type(fortarray_t), intent(in) :: var
        integer, dimension(:), intent(in) :: starts, ends
        type(fortarray_t) :: result
        integer :: i, j, k, n_elements
        integer :: idx, src_idx
        integer, dimension(:), allocatable :: indices, new_shape
        
        ! Calculate new shape and total elements
        allocate(new_shape(var%n_dims))
        n_elements = 1
        do i = 1, var%n_dims
            new_shape(i) = ends(i) - starts(i) + 1
            n_elements = n_elements * new_shape(i)
        end do
        
        ! Initialize result
        result%name = trim(var%name) // "_chunk"
        result%n_dims = var%n_dims
        result%n_elements = n_elements
        result%initialized = .true.
        
        if (allocated(var%shape)) then
            allocate(result%shape(var%n_dims))
            result%shape = new_shape
        end if
        
        if (allocated(var%dim_names)) then
            allocate(result%dim_names(var%n_dims))
            result%dim_names = var%dim_names
        end if
        
        ! Initialize data storage
        result%data%dtype = var%data%dtype
        result%data%n_elements = n_elements
        result%data%initialized = .true.
        
        ! Extract data based on type
        select case(var%data%dtype)
        case(DTYPE_REAL64)
            allocate(result%data%values_r64(n_elements))
            allocate(indices(var%n_dims))
            idx = 0
            
            ! Iterate through the chunk region
            call iterate_chunk_region()
            
        case(DTYPE_REAL32)
            allocate(result%data%values_r32(n_elements))
            allocate(indices(var%n_dims))
            idx = 0
            
            ! Iterate through the chunk region
            call iterate_chunk_region_r32()
            
        case default
            write(error_unit,'(A)') "ERROR: Unsupported data type for chunking"
            result = create_empty_like(var)
        end select
        
        deallocate(new_shape)
        if (allocated(indices)) deallocate(indices)
        
    contains
        
        subroutine iterate_chunk_region()
            integer :: d
            
            ! Initialize indices to start values
            do d = 1, var%n_dims
                indices(d) = starts(d)
            end do
            
            ! Iterate through all elements in chunk
            do while(.true.)
                idx = idx + 1
                src_idx = calculate_linear_index(indices, var%shape)
                result%data%values_r64(idx) = var%data%values_r64(src_idx)
                
                ! Increment indices
                d = 1
                do while(d <= var%n_dims)
                    indices(d) = indices(d) + 1
                    if (indices(d) <= ends(d)) exit
                    if (d == var%n_dims) return  ! Done
                    indices(d) = starts(d)
                    d = d + 1
                end do
            end do
        end subroutine iterate_chunk_region
        
        subroutine iterate_chunk_region_r32()
            integer :: d
            
            ! Initialize indices to start values
            do d = 1, var%n_dims
                indices(d) = starts(d)
            end do
            
            ! Iterate through all elements in chunk
            do while(.true.)
                idx = idx + 1
                src_idx = calculate_linear_index(indices, var%shape)
                result%data%values_r32(idx) = var%data%values_r32(src_idx)
                
                ! Increment indices
                d = 1
                do while(d <= var%n_dims)
                    indices(d) = indices(d) + 1
                    if (indices(d) <= ends(d)) exit
                    if (d == var%n_dims) return  ! Done
                    indices(d) = starts(d)
                    d = d + 1
                end do
            end do
        end subroutine iterate_chunk_region_r32
        
    end function extract_chunk_multidim
    
    !> Calculate linear index from multidimensional indices (column-major)
    function calculate_linear_index(indices, shape) result(linear_idx)
        integer, dimension(:), intent(in) :: indices, shape
        integer :: linear_idx
        integer :: i, stride
        
        linear_idx = indices(1)
        stride = 1
        
        do i = 2, size(indices)
            stride = stride * shape(i-1)
            linear_idx = linear_idx + (indices(i) - 1) * stride
        end do
        
    end function calculate_linear_index
    
end module fortarray_chunked_operations