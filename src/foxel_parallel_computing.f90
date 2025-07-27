module foxel_parallel_computing
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use foxel_types
    use foxel_storage
    use foxel_datasets
    use foxel_netcdf
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
    !$ use omp_lib
    implicit none
    private
    
    ! Public API
    public :: set_num_threads
    public :: get_num_threads
    public :: enable_parallel
    public :: disable_parallel
    public :: is_parallel_enabled
    
    ! Parallel arithmetic operations
    public :: add_parallel
    public :: multiply_parallel
    public :: subtract_parallel
    public :: divide_parallel
    public :: power_parallel
    
    ! Parallel aggregation functions
    public :: sum_parallel
    public :: mean_parallel
    public :: min_parallel
    public :: max_parallel
    public :: std_parallel
    public :: var_parallel
    
    ! Parallel apply functions
    public :: apply_parallel
    public :: apply_along_dim_parallel
    public :: apply_with_schedule
    
    ! Parallel utilities
    public :: reduce_parallel
    public :: broadcast_parallel
    public :: fillna_parallel
    public :: where_parallel
    public :: count_parallel
    
    ! Advanced parallel operations
    public :: parallel_chunk_iterator
    public :: parallel_partition
    public :: parallel_merge
    
    ! Parallel I/O
    public :: write_netcdf_parallel
    public :: read_netcdf_parallel
    
    ! Scheduling and load balancing
    public :: set_schedule_type
    public :: get_schedule_type
    public :: set_parallel_chunk_size
    public :: get_parallel_chunk_size
    
    ! Thread-safe operations
    public :: thread_safe_accumulate
    public :: thread_safe_update
    
    ! Performance monitoring
    public :: get_parallel_efficiency
    public :: get_speedup
    public :: reset_performance_stats
    
    ! Module variables
    logical :: parallel_enabled = .true.
    integer :: default_num_threads = 0  ! 0 means use OMP_NUM_THREADS
    character(len=16) :: schedule_type = "dynamic"
    integer :: chunk_size = 0  ! 0 means automatic
    
    ! Performance tracking
    real(real64) :: total_serial_time = 0.0_real64
    real(real64) :: total_parallel_time = 0.0_real64
    integer :: parallel_call_count = 0
    
    ! Generic interfaces
    interface apply_parallel
        module procedure apply_parallel_real64
        module procedure apply_parallel_real32
        module procedure apply_parallel_int32
        module procedure apply_parallel_int64
    end interface apply_parallel
    
    ! Remove generic interface for reduce_parallel - use single function
    
contains
    
    !=====================================================!
    !           Thread Management Functions               !
    !=====================================================!
    
    subroutine set_num_threads(n)
        integer, intent(in) :: n
        
        if (n > 0) then
            default_num_threads = n
            !$ call omp_set_num_threads(n)
        else
            write(error_unit, '(A)') "WARNING: Invalid thread count, using default"
        end if
    end subroutine set_num_threads
    
    function get_num_threads() result(n)
        integer :: n
        
        !$ if (default_num_threads > 0) then
        !$     n = default_num_threads
        !$ else
        !$     n = omp_get_max_threads()
        !$ end if
        
        ! Fallback for non-OpenMP builds
        if (n == 0) n = 1
    end function get_num_threads
    
    subroutine enable_parallel()
        parallel_enabled = .true.
    end subroutine enable_parallel
    
    subroutine disable_parallel()
        parallel_enabled = .false.
    end subroutine disable_parallel
    
    function is_parallel_enabled() result(enabled)
        logical :: enabled
        enabled = parallel_enabled
    end function is_parallel_enabled
    
    !=====================================================!
    !           Parallel Arithmetic Operations            !
    !=====================================================!
    
    function add_parallel(var1, var2) result(output)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: output
        integer :: i, n
        real(real64) :: start_time, end_time
        
        ! Track performance
        call cpu_time(start_time)
        
        ! Initialize output
        output = var1  ! Copy structure
        
        n = var1%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            ! Parallel execution for large arrays
            select case(var1%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do schedule(runtime) if(parallel_enabled)
                do i = 1, n
                    output%data%values_r64(i) = var1%data%values_r64(i) + var2%data%values_r64(i)
                end do
                !$omp end parallel do
                
            case(DTYPE_REAL32)
                !$omp parallel do schedule(runtime) if(parallel_enabled)
                do i = 1, n
                    output%data%values_r32(i) = var1%data%values_r32(i) + var2%data%values_r32(i)
                end do
                !$omp end parallel do
                
            case(DTYPE_INT32)
                !$omp parallel do schedule(runtime) if(parallel_enabled)
                do i = 1, n
                    output%data%values_i32(i) = var1%data%values_i32(i) + var2%data%values_i32(i)
                end do
                !$omp end parallel do
                
            case(DTYPE_INT64)
                !$omp parallel do schedule(runtime) if(parallel_enabled)
                do i = 1, n
                    output%data%values_i64(i) = var1%data%values_i64(i) + var2%data%values_i64(i)
                end do
                !$omp end parallel do
            end select
        else
            ! Serial execution for small arrays
            select case(var1%data%dtype)
            case(DTYPE_REAL64)
                output%data%values_r64 = var1%data%values_r64 + var2%data%values_r64
            case(DTYPE_REAL32)
                output%data%values_r32 = var1%data%values_r32 + var2%data%values_r32
            case(DTYPE_INT32)
                output%data%values_i32 = var1%data%values_i32 + var2%data%values_i32
            case(DTYPE_INT64)
                output%data%values_i64 = var1%data%values_i64 + var2%data%values_i64
            end select
        end if
        
        ! Track performance
        call cpu_time(end_time)
        if (parallel_enabled .and. n > 1000) then
            total_parallel_time = total_parallel_time + (end_time - start_time)
            parallel_call_count = parallel_call_count + 1
        else
            total_serial_time = total_serial_time + (end_time - start_time)
        end if
        
    end function add_parallel
    
    function multiply_parallel(var1, var2) result(output)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: output
        integer :: i, n
        
        output = var1  ! Copy structure
        n = var1%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var1%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_r64(i) = var1%data%values_r64(i) * var2%data%values_r64(i)
                end do
                !$omp end parallel do
                
            case(DTYPE_REAL32)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_r32(i) = var1%data%values_r32(i) * var2%data%values_r32(i)
                end do
                !$omp end parallel do
                
            case(DTYPE_INT32)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_i32(i) = var1%data%values_i32(i) * var2%data%values_i32(i)
                end do
                !$omp end parallel do
                
            case(DTYPE_INT64)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_i64(i) = var1%data%values_i64(i) * var2%data%values_i64(i)
                end do
                !$omp end parallel do
            end select
        else
            select case(var1%data%dtype)
            case(DTYPE_REAL64)
                output%data%values_r64 = var1%data%values_r64 * var2%data%values_r64
            case(DTYPE_REAL32)
                output%data%values_r32 = var1%data%values_r32 * var2%data%values_r32
            case(DTYPE_INT32)
                output%data%values_i32 = var1%data%values_i32 * var2%data%values_i32
            case(DTYPE_INT64)
                output%data%values_i64 = var1%data%values_i64 * var2%data%values_i64
            end select
        end if
        
    end function multiply_parallel
    
    function subtract_parallel(var1, var2) result(output)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: output
        integer :: i, n
        
        output = var1
        n = var1%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var1%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_r64(i) = var1%data%values_r64(i) - var2%data%values_r64(i)
                end do
                !$omp end parallel do
            case(DTYPE_REAL32)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_r32(i) = var1%data%values_r32(i) - var2%data%values_r32(i)
                end do
                !$omp end parallel do
            case(DTYPE_INT32)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_i32(i) = var1%data%values_i32(i) - var2%data%values_i32(i)
                end do
                !$omp end parallel do
            case(DTYPE_INT64)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_i64(i) = var1%data%values_i64(i) - var2%data%values_i64(i)
                end do
                !$omp end parallel do
            end select
        else
            select case(var1%data%dtype)
            case(DTYPE_REAL64)
                output%data%values_r64 = var1%data%values_r64 - var2%data%values_r64
            case(DTYPE_REAL32)
                output%data%values_r32 = var1%data%values_r32 - var2%data%values_r32
            case(DTYPE_INT32)
                output%data%values_i32 = var1%data%values_i32 - var2%data%values_i32
            case(DTYPE_INT64)
                output%data%values_i64 = var1%data%values_i64 - var2%data%values_i64
            end select
        end if
        
    end function subtract_parallel
    
    function divide_parallel(var1, var2) result(output)
        type(variable_t), intent(in) :: var1, var2
        type(variable_t) :: output
        integer :: i, n
        
        output = var1
        n = var1%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var1%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    if (abs(var2%data%values_r64(i)) > epsilon(1.0_real64)) then
                        output%data%values_r64(i) = var1%data%values_r64(i) / var2%data%values_r64(i)
                    else
                        output%data%values_r64(i) = ieee_value(1.0_real64, ieee_quiet_nan)
                    end if
                end do
                !$omp end parallel do
            case(DTYPE_REAL32)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    if (abs(var2%data%values_r32(i)) > epsilon(1.0_real32)) then
                        output%data%values_r32(i) = var1%data%values_r32(i) / var2%data%values_r32(i)
                    else
                        output%data%values_r32(i) = ieee_value(1.0_real32, ieee_quiet_nan)
                    end if
                end do
                !$omp end parallel do
            end select
        else
            select case(var1%data%dtype)
            case(DTYPE_REAL64)
                where (abs(var2%data%values_r64) > epsilon(1.0_real64))
                    output%data%values_r64 = var1%data%values_r64 / var2%data%values_r64
                elsewhere
                    output%data%values_r64 = ieee_value(1.0_real64, ieee_quiet_nan)
                end where
            case(DTYPE_REAL32)
                where (abs(var2%data%values_r32) > epsilon(1.0_real32))
                    output%data%values_r32 = var1%data%values_r32 / var2%data%values_r32
                elsewhere
                    output%data%values_r32 = ieee_value(1.0_real32, ieee_quiet_nan)
                end where
            end select
        end if
        
    end function divide_parallel
    
    function power_parallel(var, exponent) result(output)
        type(variable_t), intent(in) :: var
        real(real64), intent(in) :: exponent
        type(variable_t) :: output
        integer :: i, n
        
        output = var
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_r64(i) = var%data%values_r64(i) ** exponent
                end do
                !$omp end parallel do
            case(DTYPE_REAL32)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    output%data%values_r32(i) = var%data%values_r32(i) ** real(exponent, real32)
                end do
                !$omp end parallel do
            end select
        else
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                output%data%values_r64 = var%data%values_r64 ** exponent
            case(DTYPE_REAL32)
                output%data%values_r32 = var%data%values_r32 ** real(exponent, real32)
            end select
        end if
        
    end function power_parallel
    
    !=====================================================!
    !          Parallel Aggregation Functions             !
    !=====================================================!
    
    function sum_parallel(var) result(sum_val)
        type(variable_t), intent(in) :: var
        real(real64) :: sum_val
        integer :: i, n
        
        sum_val = 0.0_real64
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do reduction(+:sum_val) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        sum_val = sum_val + var%data%values_r64(i)
                    end if
                end do
                !$omp end parallel do
                
            case(DTYPE_REAL32)
                !$omp parallel do reduction(+:sum_val) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r32(i))) then
                        sum_val = sum_val + real(var%data%values_r32(i), real64)
                    end if
                end do
                !$omp end parallel do
                
            case(DTYPE_INT32)
                !$omp parallel do reduction(+:sum_val) schedule(runtime)
                do i = 1, n
                    sum_val = sum_val + real(var%data%values_i32(i), real64)
                end do
                !$omp end parallel do
                
            case(DTYPE_INT64)
                !$omp parallel do reduction(+:sum_val) schedule(runtime)
                do i = 1, n
                    sum_val = sum_val + real(var%data%values_i64(i), real64)
                end do
                !$omp end parallel do
            end select
        else
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                sum_val = sum(var%data%values_r64, mask=.not. ieee_is_nan(var%data%values_r64))
            case(DTYPE_REAL32)
                sum_val = real(sum(var%data%values_r32, mask=.not. ieee_is_nan(var%data%values_r32)), real64)
            case(DTYPE_INT32)
                sum_val = real(sum(var%data%values_i32), real64)
            case(DTYPE_INT64)
                sum_val = real(sum(var%data%values_i64), real64)
            end select
        end if
        
    end function sum_parallel
    
    function mean_parallel(var) result(mean_val)
        type(variable_t), intent(in) :: var
        real(real64) :: mean_val, sum_val
        integer :: count_valid
        
        sum_val = sum_parallel(var)
        count_valid = count_valid_parallel(var)
        
        if (count_valid > 0) then
            mean_val = sum_val / real(count_valid, real64)
        else
            mean_val = ieee_value(1.0_real64, ieee_quiet_nan)
        end if
        
    end function mean_parallel
    
    function min_parallel(var) result(min_val)
        type(variable_t), intent(in) :: var
        real(real64) :: min_val
        integer :: i, n
        
        min_val = huge(1.0_real64)
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do reduction(min:min_val) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        min_val = min(min_val, var%data%values_r64(i))
                    end if
                end do
                !$omp end parallel do
                
            case(DTYPE_REAL32)
                !$omp parallel do reduction(min:min_val) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r32(i))) then
                        min_val = min(min_val, real(var%data%values_r32(i), real64))
                    end if
                end do
                !$omp end parallel do
                
            case(DTYPE_INT32)
                !$omp parallel do reduction(min:min_val) schedule(runtime)
                do i = 1, n
                    min_val = min(min_val, real(var%data%values_i32(i), real64))
                end do
                !$omp end parallel do
                
            case(DTYPE_INT64)
                !$omp parallel do reduction(min:min_val) schedule(runtime)
                do i = 1, n
                    min_val = min(min_val, real(var%data%values_i64(i), real64))
                end do
                !$omp end parallel do
            end select
        else
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                min_val = minval(var%data%values_r64, mask=.not. ieee_is_nan(var%data%values_r64))
            case(DTYPE_REAL32)
                min_val = real(minval(var%data%values_r32, mask=.not. ieee_is_nan(var%data%values_r32)), real64)
            case(DTYPE_INT32)
                min_val = real(minval(var%data%values_i32), real64)
            case(DTYPE_INT64)
                min_val = real(minval(var%data%values_i64), real64)
            end select
        end if
        
    end function min_parallel
    
    function max_parallel(var) result(max_val)
        type(variable_t), intent(in) :: var
        real(real64) :: max_val
        integer :: i, n
        
        max_val = -huge(1.0_real64)
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do reduction(max:max_val) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        max_val = max(max_val, var%data%values_r64(i))
                    end if
                end do
                !$omp end parallel do
                
            case(DTYPE_REAL32)
                !$omp parallel do reduction(max:max_val) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r32(i))) then
                        max_val = max(max_val, real(var%data%values_r32(i), real64))
                    end if
                end do
                !$omp end parallel do
                
            case(DTYPE_INT32)
                !$omp parallel do reduction(max:max_val) schedule(runtime)
                do i = 1, n
                    max_val = max(max_val, real(var%data%values_i32(i), real64))
                end do
                !$omp end parallel do
                
            case(DTYPE_INT64)
                !$omp parallel do reduction(max:max_val) schedule(runtime)
                do i = 1, n
                    max_val = max(max_val, real(var%data%values_i64(i), real64))
                end do
                !$omp end parallel do
            end select
        else
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                max_val = maxval(var%data%values_r64, mask=.not. ieee_is_nan(var%data%values_r64))
            case(DTYPE_REAL32)
                max_val = real(maxval(var%data%values_r32, mask=.not. ieee_is_nan(var%data%values_r32)), real64)
            case(DTYPE_INT32)
                max_val = real(maxval(var%data%values_i32), real64)
            case(DTYPE_INT64)
                max_val = real(maxval(var%data%values_i64), real64)
            end select
        end if
        
    end function max_parallel
    
    function std_parallel(var) result(std_val)
        type(variable_t), intent(in) :: var
        real(real64) :: std_val, mean_val, sum_sq
        integer :: i, n, count_valid
        
        mean_val = mean_parallel(var)
        if (ieee_is_nan(mean_val)) then
            std_val = mean_val
            return
        end if
        
        sum_sq = 0.0_real64
        count_valid = 0
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do reduction(+:sum_sq,count_valid) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        sum_sq = sum_sq + (var%data%values_r64(i) - mean_val)**2
                        count_valid = count_valid + 1
                    end if
                end do
                !$omp end parallel do
            end select
        else
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        sum_sq = sum_sq + (var%data%values_r64(i) - mean_val)**2
                        count_valid = count_valid + 1
                    end if
                end do
            end select
        end if
        
        if (count_valid > 1) then
            std_val = sqrt(sum_sq / real(count_valid - 1, real64))
        else
            std_val = ieee_value(1.0_real64, ieee_quiet_nan)
        end if
        
    end function std_parallel
    
    function var_parallel(var) result(var_val)
        type(variable_t), intent(in) :: var
        real(real64) :: var_val, std_val
        
        std_val = std_parallel(var)
        if (.not. ieee_is_nan(std_val)) then
            var_val = std_val**2
        else
            var_val = std_val
        end if
        
    end function var_parallel
    
    !=====================================================!
    !             Parallel Apply Functions                !
    !=====================================================!
    
    function apply_parallel_real64(var, func) result(output)
        type(variable_t), intent(in) :: var
        interface
            function func(x) result(y)
                use iso_fortran_env, only: real64
                real(real64), intent(in) :: x
                real(real64) :: y
            end function func
        end interface
        type(variable_t) :: output
        integer :: i, n
        
        output = var
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            !$omp parallel do schedule(runtime)
            do i = 1, n
                output%data%values_r64(i) = func(var%data%values_r64(i))
            end do
            !$omp end parallel do
        else
            do i = 1, n
                output%data%values_r64(i) = func(var%data%values_r64(i))
            end do
        end if
        
    end function apply_parallel_real64
    
    function apply_parallel_real32(var, func) result(output)
        type(variable_t), intent(in) :: var
        interface
            function func(x) result(y)
                use iso_fortran_env, only: real32
                real(real32), intent(in) :: x
                real(real32) :: y
            end function func
        end interface
        type(variable_t) :: output
        integer :: i, n
        
        output = var
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            !$omp parallel do schedule(runtime)
            do i = 1, n
                output%data%values_r32(i) = func(var%data%values_r32(i))
            end do
            !$omp end parallel do
        else
            do i = 1, n
                output%data%values_r32(i) = func(var%data%values_r32(i))
            end do
        end if
        
    end function apply_parallel_real32
    
    function apply_parallel_int32(var, func) result(output)
        type(variable_t), intent(in) :: var
        interface
            function func(x) result(y)
                use iso_fortran_env, only: int32
                integer(int32), intent(in) :: x
                integer(int32) :: y
            end function func
        end interface
        type(variable_t) :: output
        integer :: i, n
        
        output = var
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            !$omp parallel do schedule(runtime)
            do i = 1, n
                output%data%values_i32(i) = func(var%data%values_i32(i))
            end do
            !$omp end parallel do
        else
            do i = 1, n
                output%data%values_i32(i) = func(var%data%values_i32(i))
            end do
        end if
        
    end function apply_parallel_int32
    
    function apply_parallel_int64(var, func) result(output)
        type(variable_t), intent(in) :: var
        interface
            function func(x) result(y)
                use iso_fortran_env, only: int64
                integer(int64), intent(in) :: x
                integer(int64) :: y
            end function func
        end interface
        type(variable_t) :: output
        integer :: i, n
        
        output = var
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            !$omp parallel do schedule(runtime)
            do i = 1, n
                output%data%values_i64(i) = func(var%data%values_i64(i))
            end do
            !$omp end parallel do
        else
            do i = 1, n
                output%data%values_i64(i) = func(var%data%values_i64(i))
            end do
        end if
        
    end function apply_parallel_int64
    
    function apply_with_schedule(var, func, sched_type, chunk) result(output)
        type(variable_t), intent(in) :: var
        interface
            function func(x) result(y)
                use iso_fortran_env, only: real64
                real(real64), intent(in) :: x
                real(real64) :: y
            end function func
        end interface
        character(len=*), intent(in) :: sched_type
        integer, intent(in) :: chunk
        type(variable_t) :: output
        integer :: i, n
        
        output = var
        n = var%n_elements
        
        select case(trim(sched_type))
        case("static")
            !$omp parallel do schedule(static, chunk)
            do i = 1, n
                output%data%values_r64(i) = func(var%data%values_r64(i))
            end do
            !$omp end parallel do
            
        case("dynamic")
            !$omp parallel do schedule(dynamic, chunk)
            do i = 1, n
                output%data%values_r64(i) = func(var%data%values_r64(i))
            end do
            !$omp end parallel do
            
        case("guided")
            !$omp parallel do schedule(guided, chunk)
            do i = 1, n
                output%data%values_r64(i) = func(var%data%values_r64(i))
            end do
            !$omp end parallel do
            
        case default
            !$omp parallel do schedule(runtime)
            do i = 1, n
                output%data%values_r64(i) = func(var%data%values_r64(i))
            end do
            !$omp end parallel do
        end select
        
    end function apply_with_schedule
    
    !=====================================================!
    !            Parallel Utility Functions               !
    !=====================================================!
    
    function reduce_parallel(var, operation) result(result_val)
        type(variable_t), intent(in) :: var
        character(len=*), intent(in) :: operation
        real(real64) :: result_val
        integer :: i, n
        
        select case(trim(operation))
        case("sum")
            result_val = sum_parallel(var)
        case("mean")
            result_val = mean_parallel(var)
        case("min")
            result_val = min_parallel(var)
        case("max")
            result_val = max_parallel(var)
        case("product")
            result_val = 1.0_real64
            n = var%n_elements
            
            if (parallel_enabled .and. n > 1000) then
                !$omp parallel do reduction(*:result_val) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        result_val = result_val * var%data%values_r64(i)
                    end if
                end do
                !$omp end parallel do
            else
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        result_val = result_val * var%data%values_r64(i)
                    end if
                end do
            end if
        case default
            result_val = ieee_value(1.0_real64, ieee_quiet_nan)
        end select
        
    end function reduce_parallel
    
    function count_parallel(var, threshold) result(count)
        type(variable_t), intent(in) :: var
        real(real64), intent(in) :: threshold
        integer :: count, i, n
        
        count = 0
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            !$omp parallel do reduction(+:count) schedule(runtime)
            do i = 1, n
                if (var%data%values_r64(i) > threshold) then
                    count = count + 1
                end if
            end do
            !$omp end parallel do
        else
            do i = 1, n
                if (var%data%values_r64(i) > threshold) then
                    count = count + 1
                end if
            end do
        end if
        
    end function count_parallel
    
    function fillna_parallel(var, fill_value) result(output)
        type(variable_t), intent(in) :: var
        real(real64), intent(in) :: fill_value
        type(variable_t) :: output
        integer :: i, n
        
        output = var
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    if (ieee_is_nan(var%data%values_r64(i))) then
                        output%data%values_r64(i) = fill_value
                    end if
                end do
                !$omp end parallel do
            case(DTYPE_REAL32)
                !$omp parallel do schedule(runtime)
                do i = 1, n
                    if (ieee_is_nan(var%data%values_r32(i))) then
                        output%data%values_r32(i) = real(fill_value, real32)
                    end if
                end do
                !$omp end parallel do
            end select
        else
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                where (ieee_is_nan(var%data%values_r64))
                    output%data%values_r64 = fill_value
                end where
            case(DTYPE_REAL32)
                where (ieee_is_nan(var%data%values_r32))
                    output%data%values_r32 = real(fill_value, real32)
                end where
            end select
        end if
        
    end function fillna_parallel
    
    !=====================================================!
    !         Advanced Parallel Operations                !
    !=====================================================!
    
    function broadcast_add_parallel(small_var, large_var) result(output)
        type(variable_t), intent(in) :: small_var, large_var
        type(variable_t) :: output
        integer :: i, n, small_n
        
        output = large_var
        n = large_var%n_elements
        small_n = small_var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            !$omp parallel do schedule(runtime)
            do i = 1, n
                output%data%values_r64(i) = large_var%data%values_r64(i) + &
                    small_var%data%values_r64(mod(i-1, small_n) + 1)
            end do
            !$omp end parallel do
        else
            do i = 1, n
                output%data%values_r64(i) = large_var%data%values_r64(i) + &
                    small_var%data%values_r64(mod(i-1, small_n) + 1)
            end do
        end if
        
    end function broadcast_add_parallel
    
    function broadcast_parallel(var, target_shape) result(output)
        type(variable_t), intent(in) :: var
        integer, dimension(:), intent(in) :: target_shape
        type(variable_t) :: output
        ! Simplified implementation
        output = var
    end function broadcast_parallel
    
    function where_parallel(condition, true_vals, false_vals) result(output)
        type(variable_t), intent(in) :: condition, true_vals, false_vals
        type(variable_t) :: output
        integer :: i, n
        
        output = true_vals
        n = condition%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            !$omp parallel do schedule(runtime)
            do i = 1, n
                if (condition%data%values_logical(i)) then
                    output%data%values_r64(i) = true_vals%data%values_r64(i)
                else
                    output%data%values_r64(i) = false_vals%data%values_r64(i)
                end if
            end do
            !$omp end parallel do
        else
            do i = 1, n
                if (condition%data%values_logical(i)) then
                    output%data%values_r64(i) = true_vals%data%values_r64(i)
                else
                    output%data%values_r64(i) = false_vals%data%values_r64(i)
                end if
            end do
        end if
        
    end function where_parallel
    
    !=====================================================!
    !              Parallel I/O Operations                !
    !=====================================================!
    
    function write_netcdf_parallel(filename, ds) result(stat)
        character(len=*), intent(in) :: filename
        type(dataset_t), intent(in) :: ds
        integer :: stat
        
        ! For now, use regular NetCDF write
        ! In the future, could use parallel NetCDF if available
        stat = write_netcdf(filename, ds)
        
    end function write_netcdf_parallel
    
    function read_netcdf_parallel(filename) result(ds)
        character(len=*), intent(in) :: filename
        type(dataset_t) :: ds
        
        ! For now, use regular NetCDF read
        ! In the future, could use parallel NetCDF if available
        ds = read_netcdf(filename)
        
    end function read_netcdf_parallel
    
    !=====================================================!
    !         Scheduling and Load Balancing               !
    !=====================================================!
    
    subroutine set_schedule_type(sched_type)
        character(len=*), intent(in) :: sched_type
        
        select case(trim(sched_type))
        case("static", "dynamic", "guided", "auto")
            schedule_type = trim(sched_type)
            !$ call omp_set_schedule(get_omp_schedule_kind(sched_type), chunk_size)
        case default
            write(error_unit, '(A)') "WARNING: Invalid schedule type, using dynamic"
            schedule_type = "dynamic"
        end select
        
    end subroutine set_schedule_type
    
    function get_schedule_type() result(sched_type)
        character(len=16) :: sched_type
        sched_type = schedule_type
    end function get_schedule_type
    
    subroutine set_parallel_chunk_size(size)
        integer, intent(in) :: size
        
        if (size >= 0) then
            chunk_size = size
            !$ call omp_set_schedule(get_omp_schedule_kind(schedule_type), chunk_size)
        else
            write(error_unit, '(A)') "WARNING: Invalid chunk size, using automatic"
            chunk_size = 0
        end if
        
    end subroutine set_parallel_chunk_size
    
    function get_parallel_chunk_size() result(size)
        integer :: size
        size = chunk_size
    end function get_parallel_chunk_size
    
    !=====================================================!
    !           Thread-Safe Operations                    !
    !=====================================================!
    
    subroutine thread_safe_accumulate(target, value)
        real(real64), intent(inout) :: target
        real(real64), intent(in) :: value
        
        !$omp atomic
        target = target + value
        
    end subroutine thread_safe_accumulate
    
    subroutine thread_safe_update(target, value)
        real(real64), intent(inout) :: target
        real(real64), intent(in) :: value
        
        !$omp critical
        target = value
        !$omp end critical
        
    end subroutine thread_safe_update
    
    !=====================================================!
    !           Performance Monitoring                    !
    !=====================================================!
    
    function get_parallel_efficiency() result(efficiency)
        real(real64) :: efficiency
        integer :: num_threads
        real(real64) :: speedup
        
        speedup = get_speedup()
        num_threads = get_num_threads()
        
        if (num_threads > 0) then
            efficiency = speedup / real(num_threads, real64)
        else
            efficiency = 1.0_real64
        end if
        
    end function get_parallel_efficiency
    
    function get_speedup() result(speedup)
        real(real64) :: speedup
        
        if (total_parallel_time > 0.0_real64 .and. total_serial_time > 0.0_real64) then
            speedup = total_serial_time / total_parallel_time
        else
            speedup = 1.0_real64
        end if
        
    end function get_speedup
    
    subroutine reset_performance_stats()
        total_serial_time = 0.0_real64
        total_parallel_time = 0.0_real64
        parallel_call_count = 0
    end subroutine reset_performance_stats
    
    !=====================================================!
    !              Helper Functions                       !
    !=====================================================!
    
    function count_valid_parallel(var) result(count)
        type(variable_t), intent(in) :: var
        integer :: count, i, n
        
        count = 0
        n = var%n_elements
        
        if (parallel_enabled .and. n > 1000) then
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                !$omp parallel do reduction(+:count) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        count = count + 1
                    end if
                end do
                !$omp end parallel do
            case(DTYPE_REAL32)
                !$omp parallel do reduction(+:count) schedule(runtime)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r32(i))) then
                        count = count + 1
                    end if
                end do
                !$omp end parallel do
            case default
                count = n
            end select
        else
            select case(var%data%dtype)
            case(DTYPE_REAL64)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r64(i))) then
                        count = count + 1
                    end if
                end do
            case(DTYPE_REAL32)
                do i = 1, n
                    if (.not. ieee_is_nan(var%data%values_r32(i))) then
                        count = count + 1
                    end if
                end do
            case default
                count = n
            end select
        end if
        
    end function count_valid_parallel
    
    function get_omp_schedule_kind(sched_str) result(kind)
        character(len=*), intent(in) :: sched_str
        integer :: kind
        
        !$ select case(trim(sched_str))
        !$ case("static")
        !$     kind = omp_sched_static
        !$ case("dynamic")
        !$     kind = omp_sched_dynamic
        !$ case("guided")
        !$     kind = omp_sched_guided
        !$ case("auto")
        !$     kind = omp_sched_auto
        !$ case default
        !$     kind = omp_sched_dynamic
        !$ end select
        
        ! Non-OpenMP fallback
        kind = 1
        
    end function get_omp_schedule_kind
    
    ! Stub implementations for advanced features
    
    function parallel_chunk_iterator(var, chunk_size) result(iter)
        type(variable_t), intent(in) :: var
        integer, intent(in) :: chunk_size
        integer :: iter
        iter = 1
    end function parallel_chunk_iterator
    
    function parallel_partition(var, n_partitions) result(partitions)
        type(variable_t), intent(in) :: var
        integer, intent(in) :: n_partitions
        type(variable_t), dimension(:), allocatable :: partitions
        allocate(partitions(1))
        partitions(1) = var
    end function parallel_partition
    
    function parallel_merge(partitions) result(merged)
        type(variable_t), dimension(:), intent(in) :: partitions
        type(variable_t) :: merged
        merged = partitions(1)
    end function parallel_merge
    
    function apply_along_dim_parallel(var, dim, func_name) result(output)
        type(variable_t), intent(in) :: var
        integer, intent(in) :: dim
        character(len=*), intent(in) :: func_name
        type(variable_t) :: output
        ! Simplified implementation
        output = var
    end function apply_along_dim_parallel
    
    function apply_sequential(var, func) result(output)
        type(variable_t), intent(in) :: var
        interface
            function func(x) result(y)
                use iso_fortran_env, only: real64
                real(real64), intent(in) :: x
                real(real64) :: y
            end function func
        end interface
        type(variable_t) :: output
        integer :: i
        
        output = var
        do i = 1, var%n_elements
            output%data%values_r64(i) = func(var%data%values_r64(i))
        end do
        
    end function apply_sequential
    
    function select_range_parallel(var, coord_name, start_val, end_val) result(output)
        type(variable_t), intent(in) :: var
        character(len=*), intent(in) :: coord_name
        real(real64), intent(in) :: start_val, end_val
        type(variable_t) :: output
        ! Simplified implementation
        output = var
    end function select_range_parallel
    
end module foxel_parallel_computing