module fortarray_storage_real64
    use fortarray_types
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private
    
    ! Public procedures
    public :: create_real64_storage
    public :: resize_real64_storage
    public :: fill_real64_storage
    public :: copy_real64_storage
    public :: slice_real64_storage
    public :: find_real64_value
    public :: sort_real64_storage
    public :: unique_real64_values
    public :: real64_statistics
    public :: real64_advanced_stats
    
contains

    !> Create real64 storage with optional initial value
    subroutine create_real64_storage(storage, n_elements, initial_value, stat)
        type(data_storage_t), intent(out) :: storage
        integer, intent(in) :: n_elements
        real(real64), intent(in), optional :: initial_value
        integer, intent(out) :: stat
        
        real(real64) :: init_val
        
        stat = 0
        init_val = 0.0_real64
        if (present(initial_value)) init_val = initial_value
        
        storage%initialized = .true.
        storage%dtype = DTYPE_REAL64  ! real64
        storage%n_elements = n_elements
        
        allocate(storage%values_r64(n_elements), stat=stat)
        if (stat /= 0) return
        
        storage%values_r64 = init_val
    end subroutine create_real64_storage
    
    !> Resize real64 storage preserving existing data
    subroutine resize_real64_storage(storage, new_size, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: new_size
        integer, intent(out) :: stat
        
        real(real64), allocatable :: temp(:)
        integer :: copy_size
        
        stat = 0
        
        if (storage%dtype /= DTYPE_REAL64) then
            stat = -1
            return
        end if
        
        if (new_size == storage%n_elements) return
        
        ! Save existing data
        copy_size = min(new_size, storage%n_elements)
        allocate(temp(copy_size), stat=stat)
        if (stat /= 0) return
        
        temp = storage%values_r64(1:copy_size)
        
        ! Reallocate
        deallocate(storage%values_r64)
        allocate(storage%values_r64(new_size), stat=stat)
        if (stat /= 0) return
        
        ! Restore data
        storage%values_r64(1:copy_size) = temp
        if (new_size > copy_size) then
            storage%values_r64(copy_size+1:new_size) = 0.0_real64
        end if
        
        storage%n_elements = new_size
        deallocate(temp)
    end subroutine resize_real64_storage
    
    !> Fill storage with a value or pattern
    subroutine fill_real64_storage(storage, pattern, start_idx, end_idx, stat, params)
        type(data_storage_t), intent(inout) :: storage
        character(len=*), intent(in) :: pattern
        integer, intent(in), optional :: start_idx, end_idx
        integer, intent(out) :: stat
        real(real64), dimension(:), intent(in), optional :: params
        
        integer :: i, start_i, end_i
        real(real64) :: rand_val, mean, std_dev, range_min, range_max
        real(real64), parameter :: pi = 3.141592653589793238_real64
        
        stat = 0
        
        if (storage%dtype /= DTYPE_REAL64) then
            stat = -1
            return
        end if
        
        start_i = 1
        end_i = storage%n_elements
        if (present(start_idx)) start_i = max(1, start_idx)
        if (present(end_idx)) end_i = min(storage%n_elements, end_idx)
        
        select case(trim(pattern))
        case("zeros")
            storage%values_r64(start_i:end_i) = 0.0_real64
            
        case("ones")
            storage%values_r64(start_i:end_i) = 1.0_real64
            
        case("sequence")
            storage%values_r64(start_i:end_i) = [(real(i, real64), i=start_i,end_i)]
            
        case("random_uniform")
            range_min = 0.0_real64
            range_max = 1.0_real64
            if (present(params)) then
                if (size(params) >= 1) range_min = params(1)
                if (size(params) >= 2) range_max = params(2)
            end if
            do i = start_i, end_i
                call random_number(rand_val)
                storage%values_r64(i) = range_min + rand_val * (range_max - range_min)
            end do
            
        case("random_normal")
            mean = 0.0_real64
            std_dev = 1.0_real64
            if (present(params)) then
                if (size(params) >= 1) mean = params(1)
                if (size(params) >= 2) std_dev = params(2)
            end if
            ! Box-Muller transform for normal distribution
            do i = start_i, end_i, 2
                block
                    real(real64) :: u1, u2, r, theta
                    call random_number(u1)
                    call random_number(u2)
                    ! Ensure u1 is not exactly 0 to avoid log(0)
                    u1 = max(tiny(1.0_real64), u1)
                    r = sqrt(-2.0_real64 * log(u1))
                    theta = 2.0_real64 * pi * u2
                    storage%values_r64(i) = mean + std_dev * r * cos(theta)
                    if (i < end_i) then
                        storage%values_r64(i+1) = mean + std_dev * r * sin(theta)
                    end if
                end block
            end do
            
        case("linspace")
            if (present(params) .and. size(params) >= 2) then
                range_min = params(1)
                range_max = params(2)
                do i = start_i, end_i
                    storage%values_r64(i) = range_min + (range_max - range_min) * &
                                           real(i - start_i, real64) / real(end_i - start_i, real64)
                end do
            else
                stat = -3  ! Missing parameters
            end if
            
        case default
            stat = -2  ! Unknown pattern
        end select
    end subroutine fill_real64_storage
    
    !> Copy real64 storage
    subroutine copy_real64_storage(storage_in, storage_out, stat)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        integer, intent(out) :: stat
        
        stat = 0
        
        if (storage_in%dtype /= DTYPE_REAL64) then
            stat = -1
            return
        end if
        
        call create_real64_storage(storage_out, storage_in%n_elements, stat=stat)
        if (stat /= 0) return
        
        storage_out%values_r64 = storage_in%values_r64
    end subroutine copy_real64_storage
    
    !> Extract slice from storage
    subroutine slice_real64_storage(storage_in, storage_out, start_idx, end_idx, stride, stat)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        integer, intent(in) :: start_idx, end_idx
        integer, intent(in), optional :: stride
        integer, intent(out) :: stat
        
        integer :: step, n_out, i, j
        
        stat = 0
        step = 1
        if (present(stride)) step = stride
        
        if (storage_in%dtype /= DTYPE_REAL64) then
            stat = -1
            return
        end if
        
        if (start_idx < 1 .or. end_idx > storage_in%n_elements .or. step < 1) then
            stat = -2
            return
        end if
        
        ! Calculate output size
        n_out = (end_idx - start_idx) / step + 1
        
        call create_real64_storage(storage_out, n_out, stat=stat)
        if (stat /= 0) return
        
        ! Copy strided data
        j = 1
        do i = start_idx, end_idx, step
            storage_out%values_r64(j) = storage_in%values_r64(i)
            j = j + 1
        end do
    end subroutine slice_real64_storage
    
    !> Find value in storage with tolerance (returns first index or 0 if not found)
    function find_real64_value(storage, value, start_idx, tolerance) result(index)
        type(data_storage_t), intent(in) :: storage
        real(real64), intent(in) :: value
        integer, intent(in), optional :: start_idx
        real(real64), intent(in), optional :: tolerance
        integer :: index
        
        integer :: i, start_i
        real(real64) :: tol
        
        index = 0
        start_i = 1
        tol = epsilon(1.0_real64)
        if (present(start_idx)) start_i = max(1, start_idx)
        if (present(tolerance)) tol = tolerance
        
        if (storage%dtype /= DTYPE_REAL64) return
        
        do i = start_i, storage%n_elements
            if (abs(storage%values_r64(i) - value) <= tol) then
                index = i
                return
            end if
        end do
    end function find_real64_value
    
    !> Sort real64 storage in place using quicksort
    subroutine sort_real64_storage(storage, ascending, stat)
        type(data_storage_t), intent(inout) :: storage
        logical, intent(in), optional :: ascending
        integer, intent(out) :: stat
        
        logical :: asc
        
        stat = 0
        asc = .true.
        if (present(ascending)) asc = ascending
        
        if (storage%dtype /= DTYPE_REAL64) then
            stat = -1
            return
        end if
        
        if (storage%n_elements > 1) then
            call quicksort_real64(storage%values_r64, 1, storage%n_elements, asc)
        end if
    end subroutine sort_real64_storage
    
    !> Quicksort implementation for real64
    recursive subroutine quicksort_real64(arr, left, right, ascending)
        real(real64), dimension(:), intent(inout) :: arr
        integer, intent(in) :: left, right
        logical, intent(in) :: ascending
        
        integer :: i, j
        real(real64) :: pivot, temp
        
        if (left < right) then
            pivot = arr((left + right) / 2)
            i = left
            j = right
            
            do while (i <= j)
                if (ascending) then
                    do while (arr(i) < pivot)
                        i = i + 1
                    end do
                    do while (arr(j) > pivot)
                        j = j - 1
                    end do
                else
                    do while (arr(i) > pivot)
                        i = i + 1
                    end do
                    do while (arr(j) < pivot)
                        j = j - 1
                    end do
                end if
                
                if (i <= j) then
                    temp = arr(i)
                    arr(i) = arr(j)
                    arr(j) = temp
                    i = i + 1
                    j = j - 1
                end if
            end do
            
            call quicksort_real64(arr, left, j, ascending)
            call quicksort_real64(arr, i, right, ascending)
        end if
    end subroutine quicksort_real64
    
    !> Get unique values from storage with tolerance
    subroutine unique_real64_values(storage_in, storage_out, counts, stat, tolerance)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        integer, dimension(:), allocatable, intent(out), optional :: counts
        integer, intent(out) :: stat
        real(real64), intent(in), optional :: tolerance
        
        real(real64), allocatable :: unique_vals(:)
        integer, allocatable :: unique_counts(:)
        integer :: n_unique, i, j
        logical :: found
        real(real64) :: tol
        
        stat = 0
        tol = epsilon(1.0_real64)
        if (present(tolerance)) tol = tolerance
        
        if (storage_in%dtype /= DTYPE_REAL64) then
            stat = -1
            return
        end if
        
        ! First pass: count unique values
        allocate(unique_vals(storage_in%n_elements))
        allocate(unique_counts(storage_in%n_elements))
        n_unique = 0
        
        do i = 1, storage_in%n_elements
            found = .false.
            do j = 1, n_unique
                if (abs(storage_in%values_r64(i) - unique_vals(j)) <= tol) then
                    unique_counts(j) = unique_counts(j) + 1
                    found = .true.
                    exit
                end if
            end do
            
            if (.not. found) then
                n_unique = n_unique + 1
                unique_vals(n_unique) = storage_in%values_r64(i)
                unique_counts(n_unique) = 1
            end if
        end do
        
        ! Create output storage
        call create_real64_storage(storage_out, n_unique, stat=stat)
        if (stat /= 0) return
        
        storage_out%values_r64 = unique_vals(1:n_unique)
        
        if (present(counts)) then
            allocate(counts(n_unique))
            counts = unique_counts(1:n_unique)
        end if
        
        deallocate(unique_vals, unique_counts)
    end subroutine unique_real64_values
    
    !> Calculate basic statistics for real64 storage
    subroutine real64_statistics(storage, min_val, max_val, sum_val, mean_val, stat)
        type(data_storage_t), intent(in) :: storage
        real(real64), intent(out), optional :: min_val, max_val, sum_val, mean_val
        integer, intent(out) :: stat
        
        integer :: i
        real(real64) :: sum_tmp
        
        stat = 0
        
        if (storage%dtype /= DTYPE_REAL64 .or. storage%n_elements == 0) then
            stat = -1
            return
        end if
        
        if (present(min_val)) min_val = storage%values_r64(1)
        if (present(max_val)) max_val = storage%values_r64(1)
        sum_tmp = storage%values_r64(1)
        
        do i = 2, storage%n_elements
            if (present(min_val)) min_val = min(min_val, storage%values_r64(i))
            if (present(max_val)) max_val = max(max_val, storage%values_r64(i))
            sum_tmp = sum_tmp + storage%values_r64(i)
        end do
        
        if (present(sum_val)) sum_val = sum_tmp
        if (present(mean_val)) mean_val = sum_tmp / real(storage%n_elements, real64)
    end subroutine real64_statistics
    
    !> Calculate advanced statistics (variance, std dev, etc.)
    subroutine real64_advanced_stats(storage, variance, std_dev, skewness, kurtosis, stat)
        type(data_storage_t), intent(in) :: storage
        real(real64), intent(out), optional :: variance, std_dev, skewness, kurtosis
        integer, intent(out) :: stat
        
        real(real64) :: mean, m2, m3, m4, delta, delta_n
        integer :: i, n
        
        stat = 0
        
        if (storage%dtype /= DTYPE_REAL64 .or. storage%n_elements == 0) then
            stat = -1
            return
        end if
        
        ! Single-pass algorithm for moments
        n = 0
        mean = 0.0_real64
        m2 = 0.0_real64
        m3 = 0.0_real64
        m4 = 0.0_real64
        
        do i = 1, storage%n_elements
            n = n + 1
            delta = storage%values_r64(i) - mean
            delta_n = delta / real(n, real64)
            mean = mean + delta_n
            m4 = m4 + delta**4 * (n - 1) * ((n - 2) * (n - 3) + 3) / real(n**3, real64) + &
                 6.0_real64 * m2 * delta**2 / real(n**2, real64) - 4.0_real64 * m3 * delta / real(n, real64)
            m3 = m3 + delta**3 * (n - 1) * (n - 2) / real(n**2, real64) - 3.0_real64 * m2 * delta / real(n, real64)
            m2 = m2 + delta * delta_n * real(n - 1, real64)
        end do
        
        if (present(variance)) variance = m2 / real(n - 1, real64)
        if (present(std_dev)) std_dev = sqrt(m2 / real(n - 1, real64))
        if (present(skewness) .and. m2 > 0.0_real64) then
            skewness = sqrt(real(n, real64)) * m3 / (m2**1.5_real64)
        end if
        if (present(kurtosis) .and. m2 > 0.0_real64) then
            kurtosis = real(n, real64) * m4 / (m2**2) - 3.0_real64
        end if
    end subroutine real64_advanced_stats
    
end module fortarray_storage_real64