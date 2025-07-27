program test_type_combinations
    use fortarray_types
    use fortarray_storage
    use fortarray_storage_int32
    use fortarray_storage_real64
    use fortarray_storage_char
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    ! Test all conversion combinations
    call test_int32_to_all()
    call test_int64_to_all()
    call test_real32_to_all()
    call test_real64_to_all()
    
    ! Test type-specific operations
    call test_int32_operations()
    call test_real64_operations()
    call test_char_operations()
    
    ! Test edge cases
    call test_conversion_edge_cases()
    call test_large_storage()
    call test_performance_patterns()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_int32_to_all()
        type(data_storage_t) :: storage_i32, storage_out
        integer(int32) :: val_i32
        integer(int64) :: val_i64
        real(real32) :: val_r32
        real(real64) :: val_r64
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create int32 storage
        call create_storage(storage_i32, 5, "int32", stat)
        call set_value(storage_i32, 1, -42_int32, stat)
        call set_value(storage_i32, 2, 0_int32, stat)
        call set_value(storage_i32, 3, 42_int32, stat)
        call set_value(storage_i32, 4, huge(1_int32), stat)
        call set_value(storage_i32, 5, -huge(1_int32)-1, stat)
        
        ! Convert to int64
        call convert_storage(storage_i32, storage_out, "int64", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 1, val_i64, stat)
        if (val_i64 /= -42_int64) test_passed = .false.
        call get_value(storage_out, 4, val_i64, stat)
        if (val_i64 /= int(huge(1_int32), int64)) test_passed = .false.
        call finalize_storage(storage_out)
        
        ! Convert to real32
        call convert_storage(storage_i32, storage_out, "real32", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 3, val_r32, stat)
        if (abs(val_r32 - 42.0_real32) > epsilon(1.0_real32)) test_passed = .false.
        call finalize_storage(storage_out)
        
        ! Convert to real64
        call convert_storage(storage_i32, storage_out, "real64", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 1, val_r64, stat)
        if (abs(val_r64 - (-42.0_real64)) > epsilon(1.0_real64)) test_passed = .false.
        call finalize_storage(storage_out)
        
        call finalize_storage(storage_i32)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: int32 to all conversions"
        else
            write(*,'(A)') "FAIL: int32 to all conversions"
        end if
    end subroutine test_int32_to_all
    
    subroutine test_int64_to_all()
        type(data_storage_t) :: storage_i64, storage_out
        integer(int32) :: val_i32
        integer(int64) :: val_i64
        real(real32) :: val_r32
        real(real64) :: val_r64
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create int64 storage
        call create_storage(storage_i64, 4, "int64", stat)
        call set_value(storage_i64, 1, -1000000000000_int64, stat)
        call set_value(storage_i64, 2, 0_int64, stat)
        call set_value(storage_i64, 3, 1000000000000_int64, stat)
        call set_value(storage_i64, 4, huge(1_int64), stat)
        
        ! Convert to int32 (with truncation)
        call convert_storage(storage_i64, storage_out, "int32", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 2, val_i32, stat)
        if (val_i32 /= 0_int32) test_passed = .false.
        call finalize_storage(storage_out)
        
        ! Convert to real64
        call convert_storage(storage_i64, storage_out, "real64", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 3, val_r64, stat)
        if (abs(val_r64 - 1.0e12_real64) > 1.0_real64) test_passed = .false.
        call finalize_storage(storage_out)
        
        call finalize_storage(storage_i64)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: int64 to all conversions"
        else
            write(*,'(A)') "FAIL: int64 to all conversions"
        end if
    end subroutine test_int64_to_all
    
    subroutine test_real32_to_all()
        type(data_storage_t) :: storage_r32, storage_out
        integer(int32) :: val_i32
        real(real32) :: val_r32
        real(real64) :: val_r64
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create real32 storage
        call create_storage(storage_r32, 5, "real32", stat)
        call set_value(storage_r32, 1, -3.14159_real32, stat)
        call set_value(storage_r32, 2, 0.0_real32, stat)
        call set_value(storage_r32, 3, 2.71828_real32, stat)
        call set_value(storage_r32, 4, tiny(1.0_real32), stat)
        call set_value(storage_r32, 5, huge(1.0_real32), stat)
        
        ! Convert to int32
        call convert_storage(storage_r32, storage_out, "int32", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 1, val_i32, stat)
        if (val_i32 /= -3_int32) test_passed = .false.
        call get_value(storage_out, 3, val_i32, stat)
        if (val_i32 /= 2_int32) test_passed = .false.
        call finalize_storage(storage_out)
        
        ! Convert to real64
        call convert_storage(storage_r32, storage_out, "real64", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 3, val_r64, stat)
        if (abs(val_r64 - 2.71828_real64) > 1.0e-5_real64) test_passed = .false.
        call finalize_storage(storage_out)
        
        call finalize_storage(storage_r32)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: real32 to all conversions"
        else
            write(*,'(A)') "FAIL: real32 to all conversions"
        end if
    end subroutine test_real32_to_all
    
    subroutine test_real64_to_all()
        type(data_storage_t) :: storage_r64, storage_out
        integer(int32) :: val_i32
        integer(int64) :: val_i64
        real(real32) :: val_r32
        real(real64) :: val_r64
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create real64 storage
        call create_storage(storage_r64, 4, "real64", stat)
        call set_value(storage_r64, 1, -1234.5678_real64, stat)
        call set_value(storage_r64, 2, 0.0_real64, stat)
        call set_value(storage_r64, 3, 9876.5432_real64, stat)
        call set_value(storage_r64, 4, 1.0e100_real64, stat)
        
        ! Convert to int32
        call convert_storage(storage_r64, storage_out, "int32", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 1, val_i32, stat)
        if (val_i32 /= -1234_int32) test_passed = .false.
        call finalize_storage(storage_out)
        
        ! Convert to int64
        call convert_storage(storage_r64, storage_out, "int64", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 3, val_i64, stat)
        if (val_i64 /= 9876_int64) test_passed = .false.
        call finalize_storage(storage_out)
        
        ! Convert to real32
        call convert_storage(storage_r64, storage_out, "real32", stat)
        if (stat /= 0) test_passed = .false.
        call get_value(storage_out, 2, val_r32, stat)
        if (abs(val_r32) > epsilon(1.0_real32)) test_passed = .false.
        call finalize_storage(storage_out)
        
        call finalize_storage(storage_r64)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: real64 to all conversions"
        else
            write(*,'(A)') "FAIL: real64 to all conversions"
        end if
    end subroutine test_real64_to_all
    
    subroutine test_int32_operations()
        type(data_storage_t) :: storage, storage_sorted, storage_unique
        integer, allocatable :: counts(:)
        integer(int32) :: min_val, max_val
        integer(int64) :: sum_val
        real(real64) :: mean_val
        logical :: test_passed
        integer :: stat, idx
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test int32-specific operations
        call create_int32_storage(storage, 10, stat=stat)
        call fill_int32_storage(storage, "sequence", stat=stat)
        
        ! Test statistics
        call int32_statistics(storage, min_val, max_val, sum_val, mean_val, stat)
        if (min_val /= 1_int32) test_passed = .false.
        if (max_val /= 10_int32) test_passed = .false.
        if (sum_val /= 55_int64) test_passed = .false.
        if (abs(mean_val - 5.5_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        ! Test find
        idx = find_int32_value(storage, 7_int32)
        if (idx /= 7) test_passed = .false.
        
        ! Test sort
        call set_value(storage, 5, 100_int32, stat)
        call copy_int32_storage(storage, storage_sorted, stat)
        call sort_int32_storage(storage_sorted, ascending=.true., stat=stat)
        call get_value(storage_sorted, 10, max_val, stat)
        if (max_val /= 100_int32) test_passed = .false.
        
        ! Test unique
        call set_value(storage, 3, 7_int32, stat)  ! Duplicate
        call unique_int32_values(storage, storage_unique, counts, stat)
        if (storage_unique%n_elements /= 9) test_passed = .false.
        
        call finalize_storage(storage)
        call finalize_storage(storage_sorted)
        call finalize_storage(storage_unique)
        if (allocated(counts)) deallocate(counts)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: int32 operations"
        else
            write(*,'(A)') "FAIL: int32 operations"
        end if
    end subroutine test_int32_operations
    
    subroutine test_real64_operations()
        type(data_storage_t) :: storage, storage_sorted
        real(real64) :: min_val, max_val, sum_val, mean_val
        real(real64) :: variance, std_dev, skewness, kurtosis
        real(real64) :: params(2)
        logical :: test_passed
        integer :: stat, idx
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test real64-specific operations
        call create_real64_storage(storage, 1000, stat=stat)
        
        ! Fill with normal distribution
        params = [0.0_real64, 1.0_real64]  ! mean=0, std=1
        call fill_real64_storage(storage, "random_normal", params=params, stat=stat)
        
        ! Test basic statistics
        call real64_statistics(storage, min_val, max_val, sum_val, mean_val, stat)
        if (abs(mean_val) > 0.1_real64) test_passed = .false.  ! Should be near 0
        
        ! Test advanced statistics
        call real64_advanced_stats(storage, variance, std_dev, skewness, kurtosis, stat)
        if (abs(std_dev - 1.0_real64) > 0.1_real64) test_passed = .false.  ! Should be near 1
        
        ! Test sort
        call sort_real64_storage(storage, ascending=.true., stat=stat)
        call get_value(storage, 1, min_val, stat)
        call get_value(storage, storage%n_elements, max_val, stat)
        if (min_val > max_val) test_passed = .false.
        
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: real64 operations"
        else
            write(*,'(A)') "FAIL: real64 operations"
        end if
    end subroutine test_real64_operations
    
    subroutine test_char_operations()
        type(data_storage_t) :: storage, storage_unique
        integer, allocatable :: counts(:)
        character(len=:), allocatable :: val
        logical :: test_passed
        integer :: stat, idx
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test character-specific operations
        call create_char_storage(storage, 5, 20, stat=stat)
        call set_char_value(storage, 1, "Hello", stat)
        call set_char_value(storage, 2, "World", stat)
        call set_char_value(storage, 3, "HELLO", stat)
        call set_char_value(storage, 4, "world", stat)
        call set_char_value(storage, 5, "Foxel", stat)
        
        ! Test case-insensitive find
        idx = find_char_value(storage, "hello", case_sensitive=.false.)
        if (idx /= 1) test_passed = .false.
        
        ! Test conversion to lower
        call char_to_lower(storage, stat)
        call get_char_value(storage, 3, val, stat)
        if (val /= "hello") test_passed = .false.
        
        ! Test unique values
        call unique_char_values(storage, storage_unique, counts, stat)
        if (storage_unique%n_elements /= 3) test_passed = .false.  ! hello, world, foxel
        
        ! Test sort
        call sort_char_storage(storage, ascending=.true., stat=stat)
        call get_char_value(storage, 1, val, stat)
        if (val /= "foxel") test_passed = .false.  ! 'f' comes before 'h'
        
        call finalize_storage(storage)
        call finalize_storage(storage_unique)
        if (allocated(counts)) deallocate(counts)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: char operations"
        else
            write(*,'(A)') "FAIL: char operations"
        end if
    end subroutine test_char_operations
    
    subroutine test_conversion_edge_cases()
        type(data_storage_t) :: storage_in, storage_out
        real(real64) :: val_r64
        integer(int32) :: val_i32
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test extreme value conversions
        call create_storage(storage_in, 4, "real64", stat)
        call set_value(storage_in, 1, tiny(1.0_real64), stat)  ! Smallest positive
        call set_value(storage_in, 2, huge(1.0_real64), stat)  ! Max value
        call set_value(storage_in, 3, -huge(1.0_real64), stat)  ! Min value
        call set_value(storage_in, 4, 0.0_real64, stat)  ! Zero
        
        ! Convert to int32
        call convert_storage(storage_in, storage_out, "int32", stat)
        if (stat /= 0) test_passed = .false.
        
        ! Check zero converts correctly
        call get_value(storage_out, 4, val_i32, stat)
        if (val_i32 /= 0_int32) test_passed = .false.
        
        ! Tiny should round to 0
        call get_value(storage_out, 1, val_i32, stat)
        if (val_i32 /= 0_int32) test_passed = .false.
        
        call finalize_storage(storage_in)
        call finalize_storage(storage_out)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Conversion edge cases"
        else
            write(*,'(A)') "FAIL: Conversion edge cases"
        end if
    end subroutine test_conversion_edge_cases
    
    subroutine test_large_storage()
        type(data_storage_t) :: storage
        real(real64) :: val
        logical :: test_passed
        integer :: stat
        integer, parameter :: large_size = 1000000
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test with large storage
        call create_storage(storage, large_size, "real64", stat)
        if (stat /= 0) then
            test_passed = .false.
        else
            ! Set and get at various positions
            call set_value(storage, 1, 1.0_real64, stat)
            call set_value(storage, large_size/2, 2.0_real64, stat)
            call set_value(storage, large_size, 3.0_real64, stat)
            
            call get_value(storage, large_size/2, val, stat)
            if (abs(val - 2.0_real64) > epsilon(1.0_real64)) test_passed = .false.
            
            call finalize_storage(storage)
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Large storage test"
        else
            write(*,'(A)') "FAIL: Large storage test"
        end if
    end subroutine test_large_storage
    
    subroutine test_performance_patterns()
        type(data_storage_t) :: storage
        real(real64) :: params(2), mean_val
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test various fill patterns
        call create_real64_storage(storage, 100, stat=stat)
        
        ! Test zeros
        call fill_real64_storage(storage, "zeros", stat=stat)
        call real64_statistics(storage, mean_val=mean_val, stat=stat)
        if (abs(mean_val) > epsilon(1.0_real64)) test_passed = .false.
        
        ! Test ones
        call fill_real64_storage(storage, "ones", stat=stat)
        call real64_statistics(storage, mean_val=mean_val, stat=stat)
        if (abs(mean_val - 1.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        ! Test linspace
        params = [0.0_real64, 99.0_real64]
        call fill_real64_storage(storage, "linspace", params=params, stat=stat)
        call real64_statistics(storage, mean_val=mean_val, stat=stat)
        if (abs(mean_val - 49.5_real64) > 0.1_real64) test_passed = .false.
        
        call finalize_storage(storage)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Performance patterns test"
        else
            write(*,'(A)') "FAIL: Performance patterns test"
        end if
    end subroutine test_performance_patterns

end program test_type_combinations