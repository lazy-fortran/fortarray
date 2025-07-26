program test_edge_cases
    use foxel_types
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    call test_empty_dataframe()
    call test_single_element_dataframe()
    call test_large_dimensions()
    call test_zero_dimensions()
    call test_max_attributes()
    call test_long_names()
    call test_mixed_coordinate_types()
    call test_memory_ownership()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_empty_dataframe()
        type(dataframe_t) :: df
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test completely empty dataframe
        if (df%initialized) test_passed = .false.
        if (df%n_dims /= 0) test_passed = .false.
        if (df%n_elements /= 0) test_passed = .false.
        if (allocated(df%dim_names)) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Empty dataframe test"
        else
            write(*,'(A)') "FAIL: Empty dataframe test"
        end if
    end subroutine test_empty_dataframe
    
    subroutine test_single_element_dataframe()
        type(dataframe_t) :: df
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create a 1x1 dataframe
        df%initialized = .true.
        df%n_dims = 1
        df%n_elements = 1
        allocate(df%dim_names(1))
        df%dim_names(1) = "single"
        allocate(df%shape(1))
        df%shape(1) = 1
        
        ! Initialize data
        df%data%initialized = .true.
        df%data%dtype = 4  ! real64
        df%data%n_elements = 1
        allocate(df%data%values_r64(1))
        df%data%values_r64(1) = 42.0_real64
        
        if (df%shape(1) /= 1) test_passed = .false.
        if (df%data%n_elements /= 1) test_passed = .false.
        if (abs(df%data%values_r64(1) - 42.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Single element dataframe test"
        else
            write(*,'(A)') "FAIL: Single element dataframe test"
        end if
        
        call finalize_dataframe(df)
    end subroutine test_single_element_dataframe
    
    subroutine test_large_dimensions()
        type(dataframe_t) :: df
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test with many dimensions
        df%initialized = .true.
        df%n_dims = 10
        allocate(df%dim_names(10))
        allocate(df%shape(10))
        allocate(df%coords(10))
        
        do i = 1, 10
            write(df%dim_names(i), '(A,I0)') "dim_", i
            df%shape(i) = i * 10
            df%coords(i)%initialized = .true.
            df%coords(i)%length = i * 10
            df%coords(i)%dtype = 4
            allocate(df%coords(i)%values_r64(i * 10))
        end do
        
        ! Verify
        if (size(df%dim_names) /= 10) test_passed = .false.
        if (df%shape(10) /= 100) test_passed = .false.
        if (.not. df%coords(5)%initialized) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Large dimensions test"
        else
            write(*,'(A)') "FAIL: Large dimensions test"
        end if
        
        call finalize_dataframe(df)
    end subroutine test_large_dimensions
    
    subroutine test_zero_dimensions()
        type(dataframe_t) :: df
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test scalar (0-dimensional) dataframe
        df%initialized = .true.
        df%n_dims = 0
        df%n_elements = 1  ! Scalar has 1 element
        ! No dimension names or shape for scalar
        
        df%data%initialized = .true.
        df%data%dtype = 2  ! int64
        df%data%n_elements = 1
        allocate(df%data%values_i64(1))
        df%data%values_i64(1) = 12345_int64
        
        if (df%n_dims /= 0) test_passed = .false.
        if (df%n_elements /= 1) test_passed = .false.
        if (allocated(df%dim_names)) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Zero dimensions test"
        else
            write(*,'(A)') "FAIL: Zero dimensions test"
        end if
        
        call finalize_dataframe(df)
    end subroutine test_zero_dimensions
    
    subroutine test_max_attributes()
        type(dataframe_t) :: df
        logical :: test_passed
        integer :: i
        character(len=20) :: key, val
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test with many attributes
        df%initialized = .true.
        df%n_attrs = 100
        allocate(df%attr_keys(100))
        allocate(df%attr_values(100))
        
        do i = 1, 100
            write(key, '(A,I0)') "attr_", i
            write(val, '(A,I0)') "value_", i
            df%attr_keys(i) = trim(key)
            df%attr_values(i) = trim(val)
        end do
        
        ! Verify some attributes
        if (df%n_attrs /= 100) test_passed = .false.
        if (df%attr_keys(50) /= "attr_50") test_passed = .false.
        if (df%attr_values(99) /= "value_99") test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Max attributes test"
        else
            write(*,'(A)') "FAIL: Max attributes test"
        end if
        
        call finalize_dataframe(df)
    end subroutine test_max_attributes
    
    subroutine test_long_names()
        type(dataframe_t) :: df
        logical :: test_passed
        character(len=MAX_NAME_LEN) :: long_name
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create a very long name
        long_name = repeat("a", MAX_NAME_LEN)
        
        df%initialized = .true.
        df%n_dims = 1
        allocate(df%dim_names(1))
        df%dim_names(1) = long_name
        df%var_name = long_name
        
        ! Verify
        if (len_trim(df%dim_names(1)) /= MAX_NAME_LEN) test_passed = .false.
        if (df%dim_names(1) /= long_name) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Long names test"
        else
            write(*,'(A)') "FAIL: Long names test"
        end if
        
        call finalize_dataframe(df)
    end subroutine test_long_names
    
    subroutine test_mixed_coordinate_types()
        type(dataframe_t) :: df
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test different coordinate types
        df%initialized = .true.
        df%n_dims = 3
        allocate(df%dim_names(3))
        df%dim_names = ["int_coord ", "real_coord", "char_coord"]
        allocate(df%coords(3))
        
        ! Integer coordinate
        df%coords(1)%initialized = .true.
        df%coords(1)%dtype = 1  ! int32
        df%coords(1)%length = 5
        allocate(df%coords(1)%values_i32(5))
        df%coords(1)%values_i32 = [1, 2, 3, 4, 5]
        
        ! Real coordinate
        df%coords(2)%initialized = .true.
        df%coords(2)%dtype = 4  ! real64
        df%coords(2)%length = 3
        allocate(df%coords(2)%values_r64(3))
        df%coords(2)%values_r64 = [1.0_real64, 2.5_real64, 5.0_real64]
        
        ! Character coordinate
        df%coords(3)%initialized = .true.
        df%coords(3)%dtype = 5  ! char
        df%coords(3)%length = 2
        allocate(character(len=10) :: df%coords(3)%values_char(2))
        df%coords(3)%values_char = ["station_1 ", "station_2 "]
        
        ! Verify
        if (df%coords(1)%dtype /= 1) test_passed = .false.
        if (df%coords(2)%dtype /= 4) test_passed = .false.
        if (df%coords(3)%dtype /= 5) test_passed = .false.
        if (df%coords(1)%values_i32(3) /= 3) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mixed coordinate types test"
        else
            write(*,'(A)') "FAIL: Mixed coordinate types test"
        end if
        
        call finalize_dataframe(df)
    end subroutine test_mixed_coordinate_types
    
    subroutine test_memory_ownership()
        type(dataframe_t) :: df_parent, df_view
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create parent
        df_parent%initialized = .true.
        df_parent%owns_memory = .true.
        df_parent%is_view = .false.
        
        ! Create view
        df_view%initialized = .true.
        df_view%owns_memory = .false.
        df_view%is_view = .true.
        df_view%parent_id = 1  ! Dummy parent ID
        
        ! Verify flags
        if (.not. df_parent%owns_memory) test_passed = .false.
        if (df_parent%is_view) test_passed = .false.
        if (df_view%owns_memory) test_passed = .false.
        if (.not. df_view%is_view) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Memory ownership test"
        else
            write(*,'(A)') "FAIL: Memory ownership test"
        end if
    end subroutine test_memory_ownership

end program test_edge_cases