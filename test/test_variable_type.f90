program test_variable_type
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
    call test_dataset_type()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_type_creation()
        type(variable_t) :: var
        type(dataframe_t) :: df  ! Legacy type
        
        n_tests_total = n_tests_total + 1
        
        ! Test that type can be created
        var%n_dims = 0
        var%initialized = .false.
        
        ! Test legacy dataframe type
        df%var%initialized = .false.
        
        if (.not. var%initialized .and. .not. df%var%initialized) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Type creation test"
        else
            write(*,'(A)') "FAIL: Type creation test"
        end if
    end subroutine test_type_creation
    
    subroutine test_type_fields()
        type(variable_t) :: var
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test all required fields exist and can be accessed
        var%n_dims = 2
        var%initialized = .true.
        var%name = "temperature"
        
        ! Test dimension names allocation
        allocate(var%dim_names(2))
        var%dim_names(1) = "time"
        var%dim_names(2) = "station"
        
        ! Test shape allocation
        allocate(var%shape(2))
        var%shape = [100, 50]
        
        ! Test coordinate storage allocation
        allocate(var%coords(2))
        allocate(var%has_coord(2))
        var%has_coord = .false.
        
        ! Verify fields
        if (var%n_dims /= 2) test_passed = .false.
        if (.not. var%initialized) test_passed = .false.
        if (size(var%dim_names) /= 2) test_passed = .false.
        if (var%dim_names(1) /= "time") test_passed = .false.
        if (any(var%shape /= [100, 50])) test_passed = .false.
        if (var%name /= "temperature") test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Type fields test"
        else
            write(*,'(A)') "FAIL: Type fields test"
        end if
        
        ! Clean up
        if (allocated(var%dim_names)) deallocate(var%dim_names)
        if (allocated(var%shape)) deallocate(var%shape)
        if (allocated(var%coords)) deallocate(var%coords)
        if (allocated(var%has_coord)) deallocate(var%has_coord)
    end subroutine test_type_fields
    
    subroutine test_memory_layout()
        type(variable_t) :: var
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test memory layout flags
        var%is_c_order = .false.  ! Fortran order by default
        var%is_view = .false.
        var%owns_memory = .true.
        
        if (var%is_c_order) test_passed = .false.
        if (var%is_view) test_passed = .false.
        if (.not. var%owns_memory) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Memory layout test"
        else
            write(*,'(A)') "FAIL: Memory layout test"
        end if
    end subroutine test_memory_layout
    
    subroutine test_attribute_storage()
        type(variable_t) :: var
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test attribute storage
        var%n_attrs = 2
        allocate(var%attr_keys(2))
        allocate(var%attr_values(2))
        
        var%attr_keys(1) = "units"
        var%attr_values(1) = "degrees_celsius"
        var%attr_keys(2) = "long_name"
        var%attr_values(2) = "Surface Temperature"
        
        ! Test direct access to common attributes
        var%units = "K"
        var%long_name = "Air Temperature"
        var%standard_name = "air_temperature"
        
        ! Verify attributes
        if (var%n_attrs /= 2) test_passed = .false.
        if (var%attr_keys(1) /= "units") test_passed = .false.
        if (var%attr_values(2) /= "Surface Temperature") test_passed = .false.
        if (var%units /= "K") test_passed = .false.
        if (var%long_name /= "Air Temperature") test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Attribute storage test"
        else
            write(*,'(A)') "FAIL: Attribute storage test"
        end if
        
        ! Clean up
        if (allocated(var%attr_keys)) deallocate(var%attr_keys)
        if (allocated(var%attr_values)) deallocate(var%attr_values)
    end subroutine test_attribute_storage
    
    subroutine test_type_assignment()
        type(variable_t) :: var1, var2
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Set up var1
        var1%n_dims = 1
        var1%initialized = .true.
        var1%name = "x_coordinate"
        allocate(var1%dim_names(1))
        var1%dim_names(1) = "x"
        allocate(var1%shape(1))
        var1%shape(1) = 10
        
        ! Test that assignment doesn't work by default (need custom assignment)
        ! This should fail without proper assignment operator
        ! For now, we test manual copy
        var2%n_dims = var1%n_dims
        var2%initialized = var1%initialized
        var2%name = var1%name
        allocate(var2%dim_names(1))
        var2%dim_names = var1%dim_names
        allocate(var2%shape(1))
        var2%shape = var1%shape
        
        if (var2%n_dims /= 1) test_passed = .false.
        if (.not. var2%initialized) test_passed = .false.
        if (var2%dim_names(1) /= "x") test_passed = .false.
        if (var2%shape(1) /= 10) test_passed = .false.
        if (var2%name /= "x_coordinate") test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Type assignment test"
        else
            write(*,'(A)') "FAIL: Type assignment test"
        end if
        
        ! Clean up
        if (allocated(var1%dim_names)) deallocate(var1%dim_names)
        if (allocated(var1%shape)) deallocate(var1%shape)
        if (allocated(var2%dim_names)) deallocate(var2%dim_names)
        if (allocated(var2%shape)) deallocate(var2%shape)
    end subroutine test_type_assignment
    
    subroutine test_dataset_type()
        type(dataset_t) :: ds
        type(dimension_t) :: dim
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test dataset creation
        ds%initialized = .true.
        ds%filename = "test_data.nc"
        ds%n_dims = 2
        ds%n_vars = 3
        
        ! Test dimension allocation
        allocate(ds%dimensions(2))
        ds%dimensions(1)%name = "time"
        ds%dimensions(1)%length = 100
        ds%dimensions(1)%is_unlimited = .true.
        
        ds%dimensions(2)%name = "lat"
        ds%dimensions(2)%length = 180
        ds%dimensions(2)%is_unlimited = .false.
        
        ! Test variable name allocation
        allocate(ds%var_names(3))
        ds%var_names = ["temperature", "pressure   ", "humidity   "]
        
        ! Test CF convention fields
        ds%conventions = "CF-1.8"
        ds%title = "Test Dataset"
        ds%institution = "Test Institute"
        
        ! Verify fields
        if (.not. ds%initialized) test_passed = .false.
        if (ds%n_dims /= 2) test_passed = .false.
        if (ds%n_vars /= 3) test_passed = .false.
        if (ds%dimensions(1)%name /= "time") test_passed = .false.
        if (.not. ds%dimensions(1)%is_unlimited) test_passed = .false.
        if (ds%var_names(1) /= "temperature") test_passed = .false.
        if (ds%conventions /= "CF-1.8") test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dataset type test"
        else
            write(*,'(A)') "FAIL: Dataset type test"
        end if
        
        ! Clean up
        if (allocated(ds%dimensions)) deallocate(ds%dimensions)
        if (allocated(ds%var_names)) deallocate(ds%var_names)
    end subroutine test_dataset_type

end program test_variable_type