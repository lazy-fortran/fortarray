module foxel_storage
    use foxel_types
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    private
    
    ! Error codes
    integer, parameter :: STORAGE_SUCCESS = 0
    integer, parameter :: STORAGE_ERROR_ALLOCATION = -1
    integer, parameter :: STORAGE_ERROR_BOUNDS = -2
    integer, parameter :: STORAGE_ERROR_TYPE = -3
    integer, parameter :: STORAGE_ERROR_CONVERSION = -4
    
    ! Generic interfaces for storage operations
    interface set_value
        module procedure set_value_i32, set_value_i64
        module procedure set_value_r32, set_value_r64
        module procedure set_value_char
    end interface set_value
    
    interface get_value
        module procedure get_value_i32, get_value_i64
        module procedure get_value_r32, get_value_r64
        module procedure get_value_char
    end interface get_value
    
    interface set_array
        module procedure set_array_i32, set_array_i64
        module procedure set_array_r32, set_array_r64
    end interface set_array
    
    interface get_array
        module procedure get_array_i32, get_array_i64
        module procedure get_array_r32, get_array_r64
    end interface get_array
    
    interface get_value_converted
        module procedure get_value_i32_from_storage
        module procedure get_value_i64_from_storage
        module procedure get_value_r32_from_storage
        module procedure get_value_r64_from_storage
    end interface get_value_converted
    
    ! Public procedures
    public :: create_storage, finalize_storage
    public :: set_value, get_value
    public :: set_array, get_array
    public :: get_value_converted
    public :: convert_storage
    public :: storage_type_name, storage_type_from_name
    
    ! Public error codes
    public :: STORAGE_SUCCESS, STORAGE_ERROR_ALLOCATION
    public :: STORAGE_ERROR_BOUNDS, STORAGE_ERROR_TYPE
    public :: STORAGE_ERROR_CONVERSION
    
contains

    !> Create storage with specified type and size
    subroutine create_storage(storage, n_elements, type_name, stat, char_len)
        type(data_storage_t), intent(out) :: storage
        integer, intent(in) :: n_elements
        character(len=*), intent(in) :: type_name
        integer, intent(out) :: stat
        integer, intent(in), optional :: char_len
        
        integer :: dtype, clen
        
        stat = STORAGE_SUCCESS
        
        ! Get data type from name
        dtype = storage_type_from_name(type_name)
        if (dtype == 0) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        ! Initialize storage
        storage%initialized = .true.
        storage%dtype = dtype
        storage%n_elements = n_elements
        
        ! Allocate based on type
        select case(dtype)
        case(1)  ! int32
            allocate(storage%values_i32(n_elements), stat=stat)
            if (stat /= 0) then
                stat = STORAGE_ERROR_ALLOCATION
                return
            end if
            storage%values_i32 = 0_int32
            
        case(2)  ! int64
            allocate(storage%values_i64(n_elements), stat=stat)
            if (stat /= 0) then
                stat = STORAGE_ERROR_ALLOCATION
                return
            end if
            storage%values_i64 = 0_int64
            
        case(3)  ! real32
            allocate(storage%values_r32(n_elements), stat=stat)
            if (stat /= 0) then
                stat = STORAGE_ERROR_ALLOCATION
                return
            end if
            storage%values_r32 = 0.0_real32
            
        case(4)  ! real64
            allocate(storage%values_r64(n_elements), stat=stat)
            if (stat /= 0) then
                stat = STORAGE_ERROR_ALLOCATION
                return
            end if
            storage%values_r64 = 0.0_real64
            
        case(5)  ! char
            clen = 80  ! default
            if (present(char_len)) clen = char_len
            allocate(character(len=clen) :: storage%values_char(n_elements), stat=stat)
            if (stat /= 0) then
                stat = STORAGE_ERROR_ALLOCATION
                return
            end if
            storage%values_char = ""
        end select
        
    end subroutine create_storage
    
    !> Finalize storage (explicit cleanup)
    subroutine finalize_storage(storage)
        type(data_storage_t), intent(inout) :: storage
        
        if (.not. storage%initialized) return
        
        select case(storage%dtype)
        case(1)
            if (allocated(storage%values_i32)) deallocate(storage%values_i32)
        case(2)
            if (allocated(storage%values_i64)) deallocate(storage%values_i64)
        case(3)
            if (allocated(storage%values_r32)) deallocate(storage%values_r32)
        case(4)
            if (allocated(storage%values_r64)) deallocate(storage%values_r64)
        case(5)
            if (allocated(storage%values_char)) deallocate(storage%values_char)
        end select
        
        storage%initialized = .false.
        storage%dtype = 0
        storage%n_elements = 0
    end subroutine finalize_storage
    
    !> Set single value - int32
    subroutine set_value_i32(storage, index, value, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: index
        integer(int32), intent(in) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 1) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        storage%values_i32(index) = value
    end subroutine set_value_i32
    
    !> Set single value - int64
    subroutine set_value_i64(storage, index, value, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: index
        integer(int64), intent(in) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 2) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        storage%values_i64(index) = value
    end subroutine set_value_i64
    
    !> Set single value - real32
    subroutine set_value_r32(storage, index, value, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: index
        real(real32), intent(in) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 3) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        storage%values_r32(index) = value
    end subroutine set_value_r32
    
    !> Set single value - real64
    subroutine set_value_r64(storage, index, value, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: index
        real(real64), intent(in) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 4) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        storage%values_r64(index) = value
    end subroutine set_value_r64
    
    !> Set single value - character
    subroutine set_value_char(storage, index, value, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: index
        character(len=*), intent(in) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 5) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        storage%values_char(index) = value
    end subroutine set_value_char
    
    !> Get single value - int32
    subroutine get_value_i32(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        integer(int32), intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 1) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        value = storage%values_i32(index)
    end subroutine get_value_i32
    
    !> Get single value - int64
    subroutine get_value_i64(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        integer(int64), intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 2) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        value = storage%values_i64(index)
    end subroutine get_value_i64
    
    !> Get single value - real32
    subroutine get_value_r32(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        real(real32), intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 3) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        value = storage%values_r32(index)
    end subroutine get_value_r32
    
    !> Get single value - real64
    subroutine get_value_r64(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        real(real64), intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 4) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        value = storage%values_r64(index)
    end subroutine get_value_r64
    
    !> Get single value - character
    subroutine get_value_char(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        character(len=:), allocatable, intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 5) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        value = trim(storage%values_char(index))
    end subroutine get_value_char
    
    !> Set array values - int32
    subroutine set_array_i32(storage, start_idx, end_idx, values, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: start_idx, end_idx
        integer(int32), dimension(:), intent(in) :: values
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, start_idx) .or. &
            .not. check_bounds(storage, end_idx)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 1) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        if (size(values) /= end_idx - start_idx + 1) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        storage%values_i32(start_idx:end_idx) = values
    end subroutine set_array_i32
    
    !> Set array values - int64
    subroutine set_array_i64(storage, start_idx, end_idx, values, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: start_idx, end_idx
        integer(int64), dimension(:), intent(in) :: values
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, start_idx) .or. &
            .not. check_bounds(storage, end_idx)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 2) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        if (size(values) /= end_idx - start_idx + 1) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        storage%values_i64(start_idx:end_idx) = values
    end subroutine set_array_i64
    
    !> Set array values - real32
    subroutine set_array_r32(storage, start_idx, end_idx, values, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: start_idx, end_idx
        real(real32), dimension(:), intent(in) :: values
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, start_idx) .or. &
            .not. check_bounds(storage, end_idx)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 3) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        if (size(values) /= end_idx - start_idx + 1) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        storage%values_r32(start_idx:end_idx) = values
    end subroutine set_array_r32
    
    !> Set array values - real64
    subroutine set_array_r64(storage, start_idx, end_idx, values, stat)
        type(data_storage_t), intent(inout) :: storage
        integer, intent(in) :: start_idx, end_idx
        real(real64), dimension(:), intent(in) :: values
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, start_idx) .or. &
            .not. check_bounds(storage, end_idx)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 4) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        if (size(values) /= end_idx - start_idx + 1) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        storage%values_r64(start_idx:end_idx) = values
    end subroutine set_array_r64
    
    !> Get array values - int32
    subroutine get_array_i32(storage, start_idx, end_idx, values, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: start_idx, end_idx
        integer(int32), dimension(:), intent(out) :: values
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, start_idx) .or. &
            .not. check_bounds(storage, end_idx)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 1) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        if (size(values) /= end_idx - start_idx + 1) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        values = storage%values_i32(start_idx:end_idx)
    end subroutine get_array_i32
    
    !> Get array values - int64
    subroutine get_array_i64(storage, start_idx, end_idx, values, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: start_idx, end_idx
        integer(int64), dimension(:), intent(out) :: values
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, start_idx) .or. &
            .not. check_bounds(storage, end_idx)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 2) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        if (size(values) /= end_idx - start_idx + 1) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        values = storage%values_i64(start_idx:end_idx)
    end subroutine get_array_i64
    
    !> Get array values - real32
    subroutine get_array_r32(storage, start_idx, end_idx, values, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: start_idx, end_idx
        real(real32), dimension(:), intent(out) :: values
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, start_idx) .or. &
            .not. check_bounds(storage, end_idx)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 3) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        if (size(values) /= end_idx - start_idx + 1) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        values = storage%values_r32(start_idx:end_idx)
    end subroutine get_array_r32
    
    !> Get array values - real64
    subroutine get_array_r64(storage, start_idx, end_idx, values, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: start_idx, end_idx
        real(real64), dimension(:), intent(out) :: values
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, start_idx) .or. &
            .not. check_bounds(storage, end_idx)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        if (storage%dtype /= 4) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        if (size(values) /= end_idx - start_idx + 1) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        values = storage%values_r64(start_idx:end_idx)
    end subroutine get_array_r64
    
    !> Get value with type conversion - int32
    subroutine get_value_i32_from_storage(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        integer(int32), intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        select case(storage%dtype)
        case(1)  ! int32
            value = storage%values_i32(index)
        case(2)  ! int64
            value = int(storage%values_i64(index), int32)
        case(3)  ! real32
            value = int(storage%values_r32(index), int32)
        case(4)  ! real64
            value = int(storage%values_r64(index), int32)
        case default
            stat = STORAGE_ERROR_CONVERSION
        end select
    end subroutine get_value_i32_from_storage
    
    !> Get value with type conversion - int64
    subroutine get_value_i64_from_storage(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        integer(int64), intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        select case(storage%dtype)
        case(1)  ! int32
            value = int(storage%values_i32(index), int64)
        case(2)  ! int64
            value = storage%values_i64(index)
        case(3)  ! real32
            value = int(storage%values_r32(index), int64)
        case(4)  ! real64
            value = int(storage%values_r64(index), int64)
        case default
            stat = STORAGE_ERROR_CONVERSION
        end select
    end subroutine get_value_i64_from_storage
    
    !> Get value with type conversion - real32
    subroutine get_value_r32_from_storage(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        real(real32), intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        select case(storage%dtype)
        case(1)  ! int32
            value = real(storage%values_i32(index), real32)
        case(2)  ! int64
            value = real(storage%values_i64(index), real32)
        case(3)  ! real32
            value = storage%values_r32(index)
        case(4)  ! real64
            value = real(storage%values_r64(index), real32)
        case default
            stat = STORAGE_ERROR_CONVERSION
        end select
    end subroutine get_value_r32_from_storage
    
    !> Get value with type conversion - real64
    subroutine get_value_r64_from_storage(storage, index, value, stat)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        real(real64), intent(out) :: value
        integer, intent(out) :: stat
        
        stat = STORAGE_SUCCESS
        
        if (.not. check_bounds(storage, index)) then
            stat = STORAGE_ERROR_BOUNDS
            return
        end if
        
        select case(storage%dtype)
        case(1)  ! int32
            value = real(storage%values_i32(index), real64)
        case(2)  ! int64
            value = real(storage%values_i64(index), real64)
        case(3)  ! real32
            value = real(storage%values_r32(index), real64)
        case(4)  ! real64
            value = storage%values_r64(index)
        case default
            stat = STORAGE_ERROR_CONVERSION
        end select
    end subroutine get_value_r64_from_storage
    
    !> Convert entire storage to different type
    subroutine convert_storage(storage_in, storage_out, type_name, stat)
        type(data_storage_t), intent(in) :: storage_in
        type(data_storage_t), intent(out) :: storage_out
        character(len=*), intent(in) :: type_name
        integer, intent(out) :: stat
        
        integer :: i, dtype_out
        
        stat = STORAGE_SUCCESS
        
        ! Get output type
        dtype_out = storage_type_from_name(type_name)
        if (dtype_out == 0) then
            stat = STORAGE_ERROR_TYPE
            return
        end if
        
        ! Create output storage
        call create_storage(storage_out, storage_in%n_elements, type_name, stat)
        if (stat /= 0) return
        
        ! Convert values
        select case(dtype_out)
        case(1)  ! to int32
            do i = 1, storage_in%n_elements
                call get_value_converted(storage_in, i, storage_out%values_i32(i), stat)
                if (stat /= 0) return
            end do
            
        case(2)  ! to int64
            do i = 1, storage_in%n_elements
                call get_value_converted(storage_in, i, storage_out%values_i64(i), stat)
                if (stat /= 0) return
            end do
            
        case(3)  ! to real32
            do i = 1, storage_in%n_elements
                call get_value_converted(storage_in, i, storage_out%values_r32(i), stat)
                if (stat /= 0) return
            end do
            
        case(4)  ! to real64
            do i = 1, storage_in%n_elements
                call get_value_converted(storage_in, i, storage_out%values_r64(i), stat)
                if (stat /= 0) return
            end do
            
        case(5)  ! to char
            stat = STORAGE_ERROR_CONVERSION  ! Not implemented
            return
        end select
        
    end subroutine convert_storage
    
    !> Check bounds
    function check_bounds(storage, index) result(ok)
        type(data_storage_t), intent(in) :: storage
        integer, intent(in) :: index
        logical :: ok
        
        ok = (index >= 1 .and. index <= storage%n_elements)
    end function check_bounds
    
    !> Get type name from dtype code
    function storage_type_name(dtype) result(name)
        integer, intent(in) :: dtype
        character(len=10) :: name
        
        select case(dtype)
        case(1)
            name = "int32"
        case(2)
            name = "int64"
        case(3)
            name = "real32"
        case(4)
            name = "real64"
        case(5)
            name = "char"
        case default
            name = "unknown"
        end select
    end function storage_type_name
    
    !> Get dtype code from type name
    function storage_type_from_name(name) result(dtype)
        character(len=*), intent(in) :: name
        integer :: dtype
        
        select case(trim(adjustl(name)))
        case("int32", "i32", "integer32")
            dtype = 1
        case("int64", "i64", "integer64")
            dtype = 2
        case("real32", "r32", "float", "single")
            dtype = 3
        case("real64", "r64", "double")
            dtype = 4
        case("char", "character", "string")
            dtype = 5
        case default
            dtype = 0  ! Unknown
        end select
    end function storage_type_from_name
    
end module foxel_storage