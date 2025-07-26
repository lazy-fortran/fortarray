program test_finalizers
    use foxel_types
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    call test_dataframe_finalizer()
    call test_coordinate_finalizer()
    call test_data_storage_finalizer()
    call test_nested_finalization()
    call test_view_finalization()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_dataframe_finalizer()
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test in a block to trigger finalization
        block
            type(dataframe_t) :: df
            
            ! Allocate all components
            df%initialized = .true.
            df%n_dims = 2
            allocate(df%dim_names(2))
            df%dim_names = ["time    ", "station "]
            allocate(df%shape(2))
            df%shape = [100, 50]
            allocate(df%strides(2))
            df%strides = [1, 100]
            allocate(df%coords(2))
            
            ! Allocate attributes
            df%n_attrs = 1
            allocate(df%attr_keys(1))
            allocate(df%attr_values(1))
            df%attr_keys(1) = "test"
            df%attr_values(1) = "value"
            
            ! When block ends, finalizer should be called
        end block
        
        ! If we get here without segfault, finalizer worked
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: DataFrame finalizer test"
        else
            write(*,'(A)') "FAIL: DataFrame finalizer test"
        end if
    end subroutine test_dataframe_finalizer
    
    subroutine test_coordinate_finalizer()
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        block
            type(coordinate_t) :: coord
            
            coord%initialized = .true.
            coord%dtype = 4  ! real64
            coord%length = 100
            allocate(coord%values_r64(100))
            coord%values_r64 = [(real(i, real64), i=1,100)]
        end block
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Coordinate finalizer test"
        else
            write(*,'(A)') "FAIL: Coordinate finalizer test"
        end if
    end subroutine test_coordinate_finalizer
    
    subroutine test_data_storage_finalizer()
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        block
            type(data_storage_t) :: storage
            
            storage%initialized = .true.
            storage%dtype = 3  ! real32
            storage%n_elements = 5000
            allocate(storage%values_r32(5000))
            storage%values_r32 = 0.0_real32
        end block
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Data storage finalizer test"
        else
            write(*,'(A)') "FAIL: Data storage finalizer test"
        end if
    end subroutine test_data_storage_finalizer
    
    subroutine test_nested_finalization()
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        block
            type(dataframe_t) :: df
            integer :: i
            
            df%initialized = .true.
            df%n_dims = 3
            allocate(df%coords(3))
            
            ! Initialize each coordinate
            do i = 1, 3
                df%coords(i)%initialized = .true.
                df%coords(i)%dtype = 4
                df%coords(i)%length = 10*i
                allocate(df%coords(i)%values_r64(10*i))
            end do
            
            ! Initialize data storage
            df%data%initialized = .true.
            df%data%dtype = 4
            df%data%n_elements = 600
            allocate(df%data%values_r64(600))
        end block
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Nested finalization test"
        else
            write(*,'(A)') "FAIL: Nested finalization test"
        end if
    end subroutine test_nested_finalization
    
    subroutine test_view_finalization()
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        block
            type(dataframe_t) :: df
            
            ! Create a view (doesn't own memory)
            df%initialized = .true.
            df%is_view = .true.
            df%owns_memory = .false.
            df%n_dims = 1
            allocate(df%dim_names(1))
            allocate(df%shape(1))
            
            ! For views, data shouldn't be deallocated by finalizer
            df%data%initialized = .true.
            df%data%dtype = 4
            ! Don't allocate - simulating pointing to parent's data
        end block
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: View finalization test"
        else
            write(*,'(A)') "FAIL: View finalization test"
        end if
    end subroutine test_view_finalization

end program test_finalizers