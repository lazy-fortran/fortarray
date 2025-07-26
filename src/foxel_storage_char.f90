module foxel_storage_char
    use foxel_types
    use iso_fortran_env, only: int32
    implicit none
    private
    
    ! Public procedures
    public :: create_char_storage
    public :: resize_char_storage
    public :: copy_char_storage
    public :: set_char_value
    public :: get_char_value
    public :: find_char_value
    public :: sort_char_storage
    public :: unique_char_values
    public :: char_to_lower
    public :: char_to_upper
    public :: char_trim_storage
    
contains

    !> Create character storage with specified length
    subroutine create_char_storage(storage, n_elements, char_length, initial_value, stat)
        type(data_storage_t), intent(out) :: storage
        integer, intent(in) :: n_elements
        integer, intent(in) :: char_length
        character(len=*), intent(in), optional :: initial_value
        integer, intent(out) :: stat
        
        character(len=char_length) :: init_val
        integer :: i
        
        stat = 0
        init_val = ""
        if (present(initial_value)) init_val = initial_value
        
        storage%initialized = .true.
        storage%dtype = 5  ! char
        storage%n_elements = n_elements
        
        allocate(character(len=char_length) :: storage%values_char(n_elements), stat=stat)
        if (stat /= 0) return
        
        do i = 1, n_elements
            storage%values_char(i) = init_val
        end do
    end subroutine create_char_storage
    
    !> Resize character storage preserving existing data
    subroutine resize_char_storage(storage, new_size, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: new_size
        integer, intent(out) :: stat
        
        character(len=:), allocatable :: temp(:)
        integer :: copy_size, char_len
        
        stat = 0
        
        if (storage%dtype /= 5) then
            stat = -1
            return
        end if
        
        if (new_size == storage%n_elements) return
        
        ! Get character length
        char_len = len(storage%values_char)
        
        ! Save existing data
        copy_size = min(new_size, storage%n_elements)
        allocate(character(len=char_len) :: temp(copy_size), stat=stat)
        if (stat /= 0) return
        
        temp = storage%values_char(1:copy_size)
        
        ! Reallocate
        deallocate(storage%values_char)
        allocate(character(len=char_len) :: storage%values_char(new_size), stat=stat)
        if (stat /= 0) return
        
        ! Restore data
        storage%values_char(1:copy_size) = temp
        if (new_size > copy_size) then
            storage%values_char(copy_size+1:new_size) = ""
        end if
        
        storage%n_elements = new_size
        deallocate(temp)
    end subroutine resize_char_storage
    
    !> Copy character storage
    subroutine copy_char_storage(storage_in, storage_out, stat)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        integer, intent(out) :: stat
        
        integer :: char_len
        
        stat = 0
        
        if (storage_in%dtype /= 5) then
            stat = -1
            return
        end if
        
        char_len = len(storage_in%values_char)
        call create_char_storage(storage_out, storage_in%n_elements, char_len, stat=stat)
        if (stat /= 0) return
        
        storage_out%values_char = storage_in%values_char
    end subroutine copy_char_storage
    
    !> Set character value with bounds checking
    subroutine set_char_value(storage, index, value, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: index
        character(len=*), intent(in) :: value
        integer, intent(out) :: stat
        
        stat = 0
        
        if (storage%dtype /= 5) then
            stat = -1
            return
        end if
        
        if (index < 1 .or. index > storage%n_elements) then
            stat = -2
            return
        end if
        
        storage%values_char(index) = value
    end subroutine set_char_value
    
    !> Get character value with bounds checking
    subroutine get_char_value(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        character(len=:), allocatable, intent(out) :: value
        integer, intent(out) :: stat
        
        stat = 0
        
        if (storage%dtype /= 5) then
            stat = -1
            return
        end if
        
        if (index < 1 .or. index > storage%n_elements) then
            stat = -2
            return
        end if
        
        value = trim(storage%values_char(index))
    end subroutine get_char_value
    
    !> Find character value in storage
    function find_char_value(storage, value, start_idx, case_sensitive) result(index)
        type(data_storage_t), intent(in) :: storage
        character(len=*), intent(in) :: value
        integer, intent(in), optional :: start_idx
        logical, intent(in), optional :: case_sensitive
        integer :: index
        
        integer :: i, start_i
        logical :: case_sens
        character(len=:), allocatable :: val_lower, storage_lower
        
        index = 0
        start_i = 1
        case_sens = .true.
        if (present(start_idx)) start_i = max(1, start_idx)
        if (present(case_sensitive)) case_sens = case_sensitive
        
        if (storage%dtype /= 5) return
        
        if (case_sens) then
            do i = start_i, storage%n_elements
                if (trim(storage%values_char(i)) == trim(value)) then
                    index = i
                    return
                end if
            end do
        else
            val_lower = to_lower(value)
            do i = start_i, storage%n_elements
                storage_lower = to_lower(storage%values_char(i))
                if (trim(storage_lower) == trim(val_lower)) then
                    index = i
                    return
                end if
            end do
        end if
    end function find_char_value
    
    !> Sort character storage in place
    subroutine sort_char_storage(storage, ascending, stat)
        type(data_storage_t), intent(inout) :: storage
        logical, intent(in), optional :: ascending
        integer, intent(out) :: stat
        
        logical :: asc
        integer :: i, j
        character(len=:), allocatable :: temp
        
        stat = 0
        asc = .true.
        if (present(ascending)) asc = ascending
        
        if (storage%dtype /= 5) then
            stat = -1
            return
        end if
        
        ! Simple bubble sort
        do i = 1, storage%n_elements - 1
            do j = 1, storage%n_elements - i
                if ((asc .and. storage%values_char(j) > storage%values_char(j+1)) .or. &
                    (.not. asc .and. storage%values_char(j) < storage%values_char(j+1))) then
                    temp = storage%values_char(j)
                    storage%values_char(j) = storage%values_char(j+1)
                    storage%values_char(j+1) = temp
                end if
            end do
        end do
    end subroutine sort_char_storage
    
    !> Get unique character values
    subroutine unique_char_values(storage_in, storage_out, counts, stat)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        integer, dimension(:), allocatable, intent(out), optional :: counts
        integer, intent(out) :: stat
        
        character(len=:), allocatable :: unique_vals(:)
        integer, allocatable :: unique_counts(:)
        integer :: n_unique, i, j, char_len
        logical :: found
        
        stat = 0
        
        if (storage_in%dtype /= 5) then
            stat = -1
            return
        end if
        
        char_len = len(storage_in%values_char)
        
        ! First pass: count unique values
        allocate(character(len=char_len) :: unique_vals(storage_in%n_elements))
        allocate(unique_counts(storage_in%n_elements))
        n_unique = 0
        
        do i = 1, storage_in%n_elements
            found = .false.
            do j = 1, n_unique
                if (trim(storage_in%values_char(i)) == trim(unique_vals(j))) then
                    unique_counts(j) = unique_counts(j) + 1
                    found = .true.
                    exit
                end if
            end do
            
            if (.not. found) then
                n_unique = n_unique + 1
                unique_vals(n_unique) = storage_in%values_char(i)
                unique_counts(n_unique) = 1
            end if
        end do
        
        ! Create output storage
        call create_char_storage(storage_out, n_unique, char_len, stat=stat)
        if (stat /= 0) return
        
        storage_out%values_char = unique_vals(1:n_unique)
        
        if (present(counts)) then
            allocate(counts(n_unique))
            counts = unique_counts(1:n_unique)
        end if
        
        deallocate(unique_vals, unique_counts)
    end subroutine unique_char_values
    
    !> Convert all strings to lowercase
    subroutine char_to_lower(storage, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(out) :: stat
        
        integer :: i
        
        stat = 0
        
        if (storage%dtype /= 5) then
            stat = -1
            return
        end if
        
        do i = 1, storage%n_elements
            storage%values_char(i) = to_lower(storage%values_char(i))
        end do
    end subroutine char_to_lower
    
    !> Convert all strings to uppercase
    subroutine char_to_upper(storage, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(out) :: stat
        
        integer :: i
        
        stat = 0
        
        if (storage%dtype /= 5) then
            stat = -1
            return
        end if
        
        do i = 1, storage%n_elements
            storage%values_char(i) = to_upper(storage%values_char(i))
        end do
    end subroutine char_to_upper
    
    !> Trim all strings in storage
    subroutine char_trim_storage(storage, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(out) :: stat
        
        integer :: i
        
        stat = 0
        
        if (storage%dtype /= 5) then
            stat = -1
            return
        end if
        
        do i = 1, storage%n_elements
            storage%values_char(i) = trim(adjustl(storage%values_char(i)))
        end do
    end subroutine char_trim_storage
    
    !> Convert string to lowercase
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        
        integer :: i
        integer, parameter :: offset = iachar('a') - iachar('A')
        
        lower_str = str
        do i = 1, len_trim(str)
            if (str(i:i) >= 'A' .and. str(i:i) <= 'Z') then
                lower_str(i:i) = achar(iachar(str(i:i)) + offset)
            end if
        end do
    end function to_lower
    
    !> Convert string to uppercase
    function to_upper(str) result(upper_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: upper_str
        
        integer :: i
        integer, parameter :: offset = iachar('A') - iachar('a')
        
        upper_str = str
        do i = 1, len_trim(str)
            if (str(i:i) >= 'a' .and. str(i:i) <= 'z') then
                upper_str(i:i) = achar(iachar(str(i:i)) + offset)
            end if
        end do
    end function to_upper
    
end module foxel_storage_char