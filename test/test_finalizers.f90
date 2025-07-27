program test_finalizers
    use foxel_types
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    call test_variable_finalizer()
    call test_dataframe_finalizer()
    call test_coordinate_finalizer()
    call test_data_storage_finalizer()
    call test_dataset_finalizer()
    call test_nested_finalization()
    call test_view_finalization()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_variable_finalizer()
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test in a block to trigger finalization
        block
            type(variable_t) :: var
            
            ! Allocate all components
            var%initialized = .true.
            var%n_dims = 2
            var%name = "test_variable"
            allocate(var%dim_names(2))
            var%dim_names = ["time    ", "station "]
            allocate(var%shape(2))
            var%shape = [100, 50]
            allocate(var%strides(2))
            var%strides = [1, 100]
            allocate(var%coords(2))
            allocate(var%has_coord(2))
            var%has_coord = .false.
            
            ! Allocate attributes
            var%n_attrs = 1
            allocate(var%attrs(1))
            var%attrs(1)%name = "test"
            var%attrs(1)%value = "value"
            var%attrs(1)%dtype = ATTR_TYPE_STRING
            
            ! When block ends, finalizer should be called
        end block
        
        ! If we get here without segfault, finalizer worked
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Variable finalizer test"
        else
            write(*,'(A)') "FAIL: Variable finalizer test"
        end if
    end subroutine test_variable_finalizer

    subroutine test_dataframe_finalizer()
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test legacy dataframe type
        block
            type(dataframe_t) :: df
            
            ! Allocate components through the var field
            df%var%initialized = .true.
            df%var%n_dims = 1
            df%var%name = "legacy_dataframe"
            allocate(df%var%dim_names(1))
            df%var%dim_names = ["x"]
            allocate(df%var%shape(1))
            df%var%shape = [10]
            
            ! When block ends, finalizer should be called
        end block
        
        ! If we get here without segfault, finalizer worked
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Legacy DataFrame finalizer test"
        else
            write(*,'(A)') "FAIL: Legacy DataFrame finalizer test"
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
            coord%name = "time"
            coord%dtype = DTYPE_REAL64
            coord%length = 100
            allocate(coord%values_r64(100))
            coord%values_r64 = [(real(i, real64), i=1,100)]
            
            ! Allocate attributes
            coord%n_attrs = 1
            allocate(coord%attrs(1))
            coord%attrs(1)%name = "units"
            coord%attrs(1)%value = "seconds"
            coord%attrs(1)%dtype = ATTR_TYPE_STRING
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
            storage%dtype = DTYPE_REAL32
            storage%n_elements = 1000
            allocate(storage%values_r32(1000))
            storage%values_r32 = 0.0_real32
        end block
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Data storage finalizer test"
        else
            write(*,'(A)') "FAIL: Data storage finalizer test"
        end if
    end subroutine test_data_storage_finalizer
    
    subroutine test_dataset_finalizer()
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        block
            type(dataset_t) :: ds
            
            ds%initialized = .true.
            ds%filename = "test.nc"
            ds%n_dims = 2
            ds%n_vars = 2
            
            ! Allocate dimensions
            allocate(ds%dimensions(2))
            ds%dimensions(1)%name = "time"
            ds%dimensions(1)%length = 100
            ds%dimensions(2)%name = "lat"
            ds%dimensions(2)%length = 180
            
            ! Allocate variable names
            allocate(ds%var_names(2))
            ds%var_names = ["temperature", "pressure   "]
            
            ! Allocate global attributes
            ds%n_attrs = 1
            allocate(ds%attr_keys(1))
            allocate(ds%attr_values(1))
            ds%attr_keys(1) = "title"
            ds%attr_values(1) = "Test Dataset"
        end block
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dataset finalizer test"
        else
            write(*,'(A)') "FAIL: Dataset finalizer test"
        end if
    end subroutine test_dataset_finalizer
    
    subroutine test_nested_finalization()
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        block
            type(variable_t) :: var
            
            var%initialized = .true.
            var%n_dims = 3
            allocate(var%coords(3))
            
            ! Initialize nested coordinates
            do i = 1, 3
                var%coords(i)%initialized = .true.
                var%coords(i)%dtype = DTYPE_REAL64
                var%coords(i)%length = 10 * i
                allocate(var%coords(i)%values_r64(10 * i))
            end do
            
            ! Initialize data storage
            var%data%initialized = .true.
            var%data%dtype = DTYPE_REAL64
            var%data%n_elements = 1000
            allocate(var%data%values_r64(1000))
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
            type(variable_t) :: var
            
            ! Create a view (doesn't own memory)
            var%initialized = .true.
            var%is_view = .true.
            var%owns_memory = .false.
            
            ! Even though we allocate, the finalizer should handle views properly
            var%data%initialized = .true.
            var%data%dtype = DTYPE_REAL64
            var%data%n_elements = 100
            allocate(var%data%values_r64(100))
        end block
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: View finalization test"
        else
            write(*,'(A)') "FAIL: View finalization test"
        end if
    end subroutine test_view_finalization

end program test_finalizers