program test_generic_storage
    use fortarray_types
    use fortarray_storage
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    call test_storage_creation()
    call test_integer_storage()
    call test_real_storage()
    call test_character_storage()
    call test_storage_access()
    call test_storage_conversion()
    call test_bounds_checking()
    call test_mixed_type_operations()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_storage_creation()
        type(data_storage_t) :: storage
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test creation for each type
        call create_storage(storage, 100, "int32", stat)
        if (stat /= 0) test_passed = .false.
        if (storage%dtype /= 1) test_passed = .false.
        if (storage%n_elements /= 100) test_passed = .false.
        if (.not. allocated(storage%values_i32)) test_passed = .false.
        call finalize_storage(storage)
        
        call create_storage(storage, 50, "real64", stat)
        if (stat /= 0) test_passed = .false.
        if (storage%dtype /= 4) test_passed = .false.
        if (.not. allocated(storage%values_r64)) test_passed = .false.
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Storage creation test"
        else
            write(*,'(A)') "FAIL: Storage creation test"
        end if
    end subroutine test_storage_creation
    
    subroutine test_integer_storage()
        type(data_storage_t) :: storage
        integer(int32) :: val_i32
        integer(int64) :: val_i64
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test int32
        call create_storage(storage, 10, "int32", stat)
        call set_value(storage, 5, 42_int32, stat)
        call get_value(storage, 5, val_i32, stat)
        if (val_i32 /= 42_int32) test_passed = .false.
        call finalize_storage(storage)
        
        ! Test int64
        call create_storage(storage, 10, "int64", stat)
        call set_value(storage, 3, 123456789_int64, stat)
        call get_value(storage, 3, val_i64, stat)
        if (val_i64 /= 123456789_int64) test_passed = .false.
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Integer storage test"
        else
            write(*,'(A)') "FAIL: Integer storage test"
        end if
    end subroutine test_integer_storage
    
    subroutine test_real_storage()
        type(data_storage_t) :: storage
        real(real32) :: val_r32
        real(real64) :: val_r64
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test real32
        call create_storage(storage, 10, "real32", stat)
        call set_value(storage, 7, 3.14159_real32, stat)
        call get_value(storage, 7, val_r32, stat)
        if (abs(val_r32 - 3.14159_real32) > epsilon(1.0_real32)) test_passed = .false.
        call finalize_storage(storage)
        
        ! Test real64
        call create_storage(storage, 10, "real64", stat)
        call set_value(storage, 2, 2.718281828_real64, stat)
        call get_value(storage, 2, val_r64, stat)
        if (abs(val_r64 - 2.718281828_real64) > epsilon(1.0_real64)) test_passed = .false.
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Real storage test"
        else
            write(*,'(A)') "FAIL: Real storage test"
        end if
    end subroutine test_real_storage
    
    subroutine test_character_storage()
        type(data_storage_t) :: storage
        character(len=:), allocatable :: val_char
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test character storage
        call create_storage(storage, 5, "char", stat, char_len=20)
        call set_value(storage, 1, "Hello World", stat)
        call set_value(storage, 2, "Foxel Library", stat)
        
        call get_value(storage, 1, val_char, stat)
        if (trim(val_char) /= "Hello World") test_passed = .false.
        
        call get_value(storage, 2, val_char, stat)
        if (trim(val_char) /= "Foxel Library") test_passed = .false.
        
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Character storage test"
        else
            write(*,'(A)') "FAIL: Character storage test"
        end if
    end subroutine test_character_storage
    
    subroutine test_storage_access()
        type(data_storage_t) :: storage
        real(real64), dimension(5) :: arr_in, arr_out
        logical :: test_passed
        integer :: stat, i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test array access
        arr_in = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        call create_storage(storage, 5, "real64", stat)
        call set_array(storage, 1, 5, arr_in, stat)
        call get_array(storage, 1, 5, arr_out, stat)
        
        do i = 1, 5
            if (abs(arr_out(i) - arr_in(i)) > epsilon(1.0_real64)) test_passed = .false.
        end do
        
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Storage access test"
        else
            write(*,'(A)') "FAIL: Storage access test"
        end if
    end subroutine test_storage_access
    
    subroutine test_storage_conversion()
        type(data_storage_t) :: storage_i32, storage_r64
        integer(int32) :: val_i32
        real(real64) :: val_r64
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create int32 storage
        call create_storage(storage_i32, 5, "int32", stat)
        call set_value(storage_i32, 1, 42_int32, stat)
        
        ! Convert to real64
        call convert_storage(storage_i32, storage_r64, "real64", stat)
        if (stat /= 0) test_passed = .false.
        
        call get_value(storage_r64, 1, val_r64, stat)
        if (abs(val_r64 - 42.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        call finalize_storage(storage_i32)
        call finalize_storage(storage_r64)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Storage conversion test"
        else
            write(*,'(A)') "FAIL: Storage conversion test"
        end if
    end subroutine test_storage_conversion
    
    subroutine test_bounds_checking()
        type(data_storage_t) :: storage
        real(real64) :: val
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        call create_storage(storage, 10, "real64", stat)
        
        ! Test out of bounds access
        call get_value(storage, 0, val, stat)  ! Below bounds
        if (stat == 0) test_passed = .false.
        
        call get_value(storage, 11, val, stat)  ! Above bounds
        if (stat == 0) test_passed = .false.
        
        call set_value(storage, -5, 1.0_real64, stat)  ! Negative index
        if (stat == 0) test_passed = .false.
        
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Bounds checking test"
        else
            write(*,'(A)') "FAIL: Bounds checking test"
        end if
    end subroutine test_bounds_checking
    
    subroutine test_mixed_type_operations()
        type(data_storage_t) :: storage
        integer(int32) :: ival
        real(real64) :: rval
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create real64 storage
        call create_storage(storage, 5, "real64", stat)
        call set_value(storage, 1, 3.14_real64, stat)
        
        ! Try to get as integer (should convert)
        call get_value_converted(storage, 1, ival, stat)
        if (stat /= 0) test_passed = .false.
        if (ival /= 3_int32) test_passed = .false.
        
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mixed type operations test"
        else
            write(*,'(A)') "FAIL: Mixed type operations test"
        end if
    end subroutine test_mixed_type_operations

end program test_generic_storage