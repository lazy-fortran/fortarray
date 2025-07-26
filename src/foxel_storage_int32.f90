module foxel_storage_int32
    use foxel_types
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private
    
    ! Public procedures
    public :: create_int32_storage
    public :: resize_int32_storage
    public :: fill_int32_storage
    public :: copy_int32_storage
    public :: slice_int32_storage
    public :: find_int32_value
    public :: sort_int32_storage
    public :: unique_int32_values
    public :: int32_statistics
    
contains

    !> Create int32 storage with optional initial value
    subroutine create_int32_storage(storage, n_elements, initial_value, stat)
        type(data_storage_t), intent(out) :: storage
        integer, intent(in) :: n_elements
        integer(int32), intent(in), optional :: initial_value
        integer, intent(out) :: stat
        
        integer(int32) :: init_val
        
        stat = 0
        init_val = 0_int32
        if (present(initial_value)) init_val = initial_value
        
        storage%initialized = .true.
        storage%dtype = 1  ! int32
        storage%n_elements = n_elements
        
        allocate(storage%values_i32(n_elements), stat=stat)
        if (stat /= 0) return
        
        storage%values_i32 = init_val
    end subroutine create_int32_storage
    
    !> Resize int32 storage preserving existing data
    subroutine resize_int32_storage(storage, new_size, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: new_size
        integer, intent(out) :: stat
        
        integer(int32), allocatable :: temp(:)
        integer :: copy_size
        
        stat = 0
        
        if (storage%dtype /= 1) then
            stat = -1
            return
        end if
        
        if (new_size == storage%n_elements) return
        
        ! Save existing data
        copy_size = min(new_size, storage%n_elements)
        allocate(temp(copy_size), stat=stat)
        if (stat /= 0) return
        
        temp = storage%values_i32(1:copy_size)
        
        ! Reallocate
        deallocate(storage%values_i32)
        allocate(storage%values_i32(new_size), stat=stat)
        if (stat /= 0) return
        
        ! Restore data
        storage%values_i32(1:copy_size) = temp
        if (new_size > copy_size) then
            storage%values_i32(copy_size+1:new_size) = 0_int32
        end if
        
        storage%n_elements = new_size
        deallocate(temp)
    end subroutine resize_int32_storage
    
    !> Fill storage with a value or pattern
    subroutine fill_int32_storage(storage, pattern, start_idx, end_idx, stat)
        type(data_storage_t), intent(inout) :: storage
        character(len=*), intent(in) :: pattern
        integer, intent(in), optional :: start_idx, end_idx
        integer, intent(out) :: stat
        
        integer :: i, start_i, end_i
        real :: rand_val
        
        stat = 0
        
        if (storage%dtype /= 1) then
            stat = -1
            return
        end if
        
        start_i = 1
        end_i = storage%n_elements
        if (present(start_idx)) start_i = max(1, start_idx)
        if (present(end_idx)) end_i = min(storage%n_elements, end_idx)
        
        select case(trim(pattern))
        case("zeros")
            storage%values_i32(start_i:end_i) = 0_int32
            
        case("ones")
            storage%values_i32(start_i:end_i) = 1_int32
            
        case("sequence")
            storage%values_i32(start_i:end_i) = [(int(i, int32), i=start_i,end_i)]
            
        case("random")
            do i = start_i, end_i
                call random_number(rand_val)
                storage%values_i32(i) = int(rand_val * huge(1_int32), int32)
            end do
            
        case default
            stat = -2  ! Unknown pattern
        end select
    end subroutine fill_int32_storage
    
    !> Copy int32 storage
    subroutine copy_int32_storage(storage_in, storage_out, stat)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        integer, intent(out) :: stat
        
        stat = 0
        
        if (storage_in%dtype /= 1) then
            stat = -1
            return
        end if
        
        call create_int32_storage(storage_out, storage_in%n_elements, stat=stat)
        if (stat /= 0) return
        
        storage_out%values_i32 = storage_in%values_i32
    end subroutine copy_int32_storage
    
    !> Extract slice from storage
    subroutine slice_int32_storage(storage_in, storage_out, start_idx, end_idx, stride, stat)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        integer, intent(in) :: start_idx, end_idx
        integer, intent(in), optional :: stride
        integer, intent(out) :: stat
        
        integer :: step, n_out, i, j
        
        stat = 0
        step = 1
        if (present(stride)) step = stride
        
        if (storage_in%dtype /= 1) then
            stat = -1
            return
        end if
        
        if (start_idx < 1 .or. end_idx > storage_in%n_elements .or. step < 1) then
            stat = -2
            return
        end if
        
        ! Calculate output size
        n_out = (end_idx - start_idx) / step + 1
        
        call create_int32_storage(storage_out, n_out, stat=stat)
        if (stat /= 0) return
        
        ! Copy strided data
        j = 1
        do i = start_idx, end_idx, step
            storage_out%values_i32(j) = storage_in%values_i32(i)
            j = j + 1
        end do
    end subroutine slice_int32_storage
    
    !> Find value in storage (returns first index or 0 if not found)
    function find_int32_value(storage, value, start_idx) result(index)
        type(data_storage_t), intent(in) :: storage
        integer(int32), intent(in) :: value
        integer, intent(in), optional :: start_idx
        integer :: index
        
        integer :: i, start_i
        
        index = 0
        start_i = 1
        if (present(start_idx)) start_i = max(1, start_idx)
        
        if (storage%dtype /= 1) return
        
        do i = start_i, storage%n_elements
            if (storage%values_i32(i) == value) then
                index = i
                return
            end if
        end do
    end function find_int32_value
    
    !> Sort int32 storage in place
    subroutine sort_int32_storage(storage, ascending, stat)
        type(data_storage_t), intent(inout) :: storage
        logical, intent(in), optional :: ascending
        integer, intent(out) :: stat
        
        logical :: asc
        integer :: i, j
        integer(int32) :: temp
        
        stat = 0
        asc = .true.
        if (present(ascending)) asc = ascending
        
        if (storage%dtype /= 1) then
            stat = -1
            return
        end if
        
        ! Simple bubble sort (should use quicksort for production)
        do i = 1, storage%n_elements - 1
            do j = 1, storage%n_elements - i
                if ((asc .and. storage%values_i32(j) > storage%values_i32(j+1)) .or. &
                    (.not. asc .and. storage%values_i32(j) < storage%values_i32(j+1))) then
                    temp = storage%values_i32(j)
                    storage%values_i32(j) = storage%values_i32(j+1)
                    storage%values_i32(j+1) = temp
                end if
            end do
        end do
    end subroutine sort_int32_storage
    
    !> Get unique values from storage
    subroutine unique_int32_values(storage_in, storage_out, counts, stat)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        integer, dimension(:), allocatable, intent(out), optional :: counts
        integer, intent(out) :: stat
        
        integer(int32), allocatable :: unique_vals(:)
        integer, allocatable :: unique_counts(:)
        integer :: n_unique, i, j
        logical :: found
        
        stat = 0
        
        if (storage_in%dtype /= 1) then
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
                if (storage_in%values_i32(i) == unique_vals(j)) then
                    unique_counts(j) = unique_counts(j) + 1
                    found = .true.
                    exit
                end if
            end do
            
            if (.not. found) then
                n_unique = n_unique + 1
                unique_vals(n_unique) = storage_in%values_i32(i)
                unique_counts(n_unique) = 1
            end if
        end do
        
        ! Create output storage
        call create_int32_storage(storage_out, n_unique, stat=stat)
        if (stat /= 0) return
        
        storage_out%values_i32 = unique_vals(1:n_unique)
        
        if (present(counts)) then
            allocate(counts(n_unique))
            counts = unique_counts(1:n_unique)
        end if
        
        deallocate(unique_vals, unique_counts)
    end subroutine unique_int32_values
    
    !> Calculate basic statistics for int32 storage
    subroutine int32_statistics(storage, min_val, max_val, sum_val, mean_val, stat)
        type(data_storage_t), intent(in) :: storage
        integer(int32), intent(out), optional :: min_val, max_val
        integer(int64), intent(out), optional :: sum_val
        real(real64), intent(out), optional :: mean_val
        integer, intent(out) :: stat
        
        integer :: i
        integer(int64) :: sum_tmp
        
        stat = 0
        
        if (storage%dtype /= 1 .or. storage%n_elements == 0) then
            stat = -1
            return
        end if
        
        if (present(min_val)) min_val = storage%values_i32(1)
        if (present(max_val)) max_val = storage%values_i32(1)
        sum_tmp = int(storage%values_i32(1), int64)
        
        do i = 2, storage%n_elements
            if (present(min_val)) min_val = min(min_val, storage%values_i32(i))
            if (present(max_val)) max_val = max(max_val, storage%values_i32(i))
            sum_tmp = sum_tmp + int(storage%values_i32(i), int64)
        end do
        
        if (present(sum_val)) sum_val = sum_tmp
        if (present(mean_val)) mean_val = real(sum_tmp, real64) / real(storage%n_elements, real64)
    end subroutine int32_statistics
    
end module foxel_storage_int32