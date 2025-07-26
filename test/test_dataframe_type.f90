program test_dataframe_type
    use foxel_types
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    call test_type_creation()
    call test_type_fields()
    call test_memory_layout()
    call test_attribute_storage()
    call test_type_assignment()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_type_creation()
        type(dataframe_t) :: df
        
        n_tests_total = n_tests_total + 1
        
        ! Test that type can be created
        df%n_dims = 0
        df%initialized = .false.
        
        if (.not. df%initialized) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Type creation test"
        else
            write(*,'(A)') "FAIL: Type creation test"
        end if
    end subroutine test_type_creation
    
    subroutine test_type_fields()
        type(dataframe_t) :: df
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test all required fields exist and can be accessed
        df%n_dims = 2
        df%initialized = .true.
        
        ! Test dimension names allocation
        allocate(df%dim_names(2))
        df%dim_names(1) = "time"
        df%dim_names(2) = "station"
        
        ! Test shape allocation
        allocate(df%shape(2))
        df%shape = [100, 50]
        
        ! Test coordinate storage allocation
        allocate(df%coords(2))
        
        ! Verify fields
        if (df%n_dims /= 2) test_passed = .false.
        if (.not. df%initialized) test_passed = .false.
        if (size(df%dim_names) /= 2) test_passed = .false.
        if (df%dim_names(1) /= "time") test_passed = .false.
        if (any(df%shape /= [100, 50])) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Type fields test"
        else
            write(*,'(A)') "FAIL: Type fields test"
        end if
        
        ! Clean up
        if (allocated(df%dim_names)) deallocate(df%dim_names)
        if (allocated(df%shape)) deallocate(df%shape)
        if (allocated(df%coords)) deallocate(df%coords)
    end subroutine test_type_fields
    
    subroutine test_memory_layout()
        type(dataframe_t) :: df
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test memory layout flags
        df%is_c_order = .false.  ! Fortran order by default
        df%is_view = .false.
        df%owns_memory = .true.
        
        if (df%is_c_order) test_passed = .false.
        if (df%is_view) test_passed = .false.
        if (.not. df%owns_memory) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Memory layout test"
        else
            write(*,'(A)') "FAIL: Memory layout test"
        end if
    end subroutine test_memory_layout
    
    subroutine test_attribute_storage()
        type(dataframe_t) :: df
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test attribute storage
        df%n_attrs = 2
        allocate(df%attr_keys(2))
        allocate(df%attr_values(2))
        
        df%attr_keys(1) = "units"
        df%attr_values(1) = "degrees_celsius"
        df%attr_keys(2) = "long_name"
        df%attr_values(2) = "Surface Temperature"
        
        ! Verify attributes
        if (df%n_attrs /= 2) test_passed = .false.
        if (df%attr_keys(1) /= "units") test_passed = .false.
        if (df%attr_values(2) /= "Surface Temperature") test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Attribute storage test"
        else
            write(*,'(A)') "FAIL: Attribute storage test"
        end if
        
        ! Clean up
        if (allocated(df%attr_keys)) deallocate(df%attr_keys)
        if (allocated(df%attr_values)) deallocate(df%attr_values)
    end subroutine test_attribute_storage
    
    subroutine test_type_assignment()
        type(dataframe_t) :: df1, df2
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Set up df1
        df1%n_dims = 1
        df1%initialized = .true.
        allocate(df1%dim_names(1))
        df1%dim_names(1) = "x"
        allocate(df1%shape(1))
        df1%shape(1) = 10
        
        ! Test that assignment doesn't work by default (need custom assignment)
        ! This should fail without proper assignment operator
        ! For now, we test manual copy
        df2%n_dims = df1%n_dims
        df2%initialized = df1%initialized
        allocate(df2%dim_names(1))
        df2%dim_names = df1%dim_names
        allocate(df2%shape(1))
        df2%shape = df1%shape
        
        if (df2%n_dims /= 1) test_passed = .false.
        if (.not. df2%initialized) test_passed = .false.
        if (df2%dim_names(1) /= "x") test_passed = .false.
        if (df2%shape(1) /= 10) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Type assignment test"
        else
            write(*,'(A)') "FAIL: Type assignment test"
        end if
        
        ! Clean up
        if (allocated(df1%dim_names)) deallocate(df1%dim_names)
        if (allocated(df1%shape)) deallocate(df1%shape)
        if (allocated(df2%dim_names)) deallocate(df2%dim_names)
        if (allocated(df2%shape)) deallocate(df2%shape)
    end subroutine test_type_assignment

end program test_dataframe_type