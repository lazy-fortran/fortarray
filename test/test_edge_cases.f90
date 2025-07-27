program test_edge_cases
    use foxel_types
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    call test_empty_variable()
    call test_single_element_variable()
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

    subroutine test_empty_variable()
        type(variable_t) :: var
        type(dataframe_t) :: df  ! Legacy
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test completely empty variable
        if (var%initialized) test_passed = .false.
        if (var%n_dims /= 0) test_passed = .false.
        if (var%n_elements /= 0) test_passed = .false.
        if (allocated(var%dim_names)) test_passed = .false.
        
        ! Test legacy dataframe
        if (df%var%initialized) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Empty variable test"
        else
            write(*,'(A)') "FAIL: Empty variable test"
        end if
    end subroutine test_empty_variable
    
    subroutine test_single_element_variable()
        type(variable_t) :: var
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create scalar (0D) variable
        var%initialized = .true.
        var%n_dims = 0
        var%n_elements = 1
        var%name = "scalar_var"
        
        ! Initialize data storage
        var%data%initialized = .true.
        var%data%dtype = DTYPE_REAL64
        var%data%n_elements = 1
        allocate(var%data%values_r64(1))
        var%data%values_r64(1) = 42.0_real64
        
        ! Test scalar properties
        if (var%n_dims /= 0) test_passed = .false.
        if (var%n_elements /= 1) test_passed = .false.
        if (allocated(var%shape)) test_passed = .false.  ! Scalars have no shape
        if (abs(var%data%values_r64(1) - 42.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Single element variable test"
        else
            write(*,'(A)') "FAIL: Single element variable test"
        end if
        
        ! Clean up
        call finalize_variable(var)
    end subroutine test_single_element_variable
    
    subroutine test_large_dimensions()
        type(variable_t) :: var
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test with maximum reasonable dimensions (e.g., 10D)
        var%initialized = .true.
        var%n_dims = 10
        allocate(var%dim_names(10))
        allocate(var%shape(10))
        
        do i = 1, 10
            write(var%dim_names(i), '(A,I0)') "dim_", i
            var%shape(i) = 2  ! Small size per dimension to avoid overflow
        end do
        
        ! Calculate total elements (2^10 = 1024)
        var%n_elements = 1
        do i = 1, 10
            var%n_elements = var%n_elements * var%shape(i)
        end do
        
        if (var%n_dims /= 10) test_passed = .false.
        if (var%n_elements /= 1024) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Large dimensions test"
        else
            write(*,'(A)') "FAIL: Large dimensions test"
        end if
        
        ! Clean up
        if (allocated(var%dim_names)) deallocate(var%dim_names)
        if (allocated(var%shape)) deallocate(var%shape)
    end subroutine test_large_dimensions
    
    subroutine test_zero_dimensions()
        type(variable_t) :: var
        type(dataset_t) :: ds
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test scalar variable (0 dimensions)
        var%initialized = .true.
        var%n_dims = 0
        var%n_elements = 1
        
        ! Test empty dataset
        ds%initialized = .true.
        ds%n_dims = 0
        ds%n_vars = 0
        
        if (var%n_dims /= 0) test_passed = .false.
        if (var%n_elements /= 1) test_passed = .false.
        if (ds%n_dims /= 0) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Zero dimensions test"
        else
            write(*,'(A)') "FAIL: Zero dimensions test"
        end if
    end subroutine test_zero_dimensions
    
    subroutine test_max_attributes()
        type(variable_t) :: var
        integer, parameter :: max_attrs = 1000
        logical :: test_passed
        integer :: i
        character(len=20) :: key
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test with many attributes
        var%initialized = .true.
        var%n_attrs = max_attrs
        allocate(var%attrs(max_attrs))
        
        do i = 1, max_attrs
            write(key, '(A,I0)') "attr_", i
            var%attrs(i)%name = key
            var%attrs(i)%value = "test_value"
            var%attrs(i)%dtype = ATTR_TYPE_STRING
        end do
        
        if (var%n_attrs /= max_attrs) test_passed = .false.
        if (size(var%attrs) /= max_attrs) test_passed = .false.
        if (var%attrs(500)%name /= "attr_500") test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Max attributes test"
        else
            write(*,'(A)') "FAIL: Max attributes test"
        end if
        
        ! Clean up
        if (allocated(var%attrs)) deallocate(var%attrs)
    end subroutine test_max_attributes
    
    subroutine test_long_names()
        type(variable_t) :: var
        character(len=MAX_NAME_LEN) :: long_name
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create maximum length name
        long_name = repeat("a", MAX_NAME_LEN)
        
        var%initialized = .true.
        var%name = long_name
        var%n_dims = 1
        allocate(var%dim_names(1))
        var%dim_names(1) = long_name
        
        ! Test attribute with long name
        var%long_name = repeat("Long description ", 16)  ! Fill MAX_ATTR_LEN
        
        if (len_trim(var%name) /= MAX_NAME_LEN) test_passed = .false.
        if (var%dim_names(1) /= long_name) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Long names test"
        else
            write(*,'(A)') "FAIL: Long names test"
        end if
        
        ! Clean up
        if (allocated(var%dim_names)) deallocate(var%dim_names)
    end subroutine test_long_names
    
    subroutine test_mixed_coordinate_types()
        type(variable_t) :: var
        type(coordinate_t) :: coord_time, coord_station
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create variable with mixed coordinate types
        var%initialized = .true.
        var%n_dims = 2
        allocate(var%dim_names(2))
        var%dim_names = ["time   ", "station"]
        allocate(var%shape(2))
        var%shape = [10, 5]
        allocate(var%coords(2))
        allocate(var%has_coord(2))
        
        ! Time coordinate (real64)
        var%coords(1)%initialized = .true.
        var%coords(1)%name = "time"
        var%coords(1)%dtype = DTYPE_REAL64
        var%coords(1)%length = 10
        allocate(var%coords(1)%values_r64(10))
        var%coords(1)%values_r64 = [(real(i, real64), i=1,10)]
        
        ! Station coordinate (character)
        var%coords(2)%initialized = .true.
        var%coords(2)%name = "station"
        var%coords(2)%dtype = DTYPE_CHAR
        var%coords(2)%length = 5
        allocate(character(len=10) :: var%coords(2)%values_char(5))
        var%coords(2)%values_char = ["Station_01", "Station_02", &
                                     "Station_03", "Station_04", "Station_05"]
        
        var%has_coord = .true.
        
        if (var%coords(1)%dtype /= DTYPE_REAL64) test_passed = .false.
        if (var%coords(2)%dtype /= DTYPE_CHAR) test_passed = .false.
        if (var%coords(2)%values_char(3) /= "Station_03") test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Mixed coordinate types test"
        else
            write(*,'(A)') "FAIL: Mixed coordinate types test"
        end if
        
        ! Clean up
        call finalize_variable(var)
    end subroutine test_mixed_coordinate_types
    
    subroutine test_memory_ownership()
        type(variable_t) :: var_owner, var_view
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create owner variable
        var_owner%initialized = .true.
        var_owner%owns_memory = .true.
        var_owner%is_view = .false.
        var_owner%n_elements = 100
        
        var_owner%data%initialized = .true.
        var_owner%data%dtype = DTYPE_REAL64
        var_owner%data%n_elements = 100
        allocate(var_owner%data%values_r64(100))
        var_owner%data%values_r64 = 1.0_real64
        
        ! Create view variable
        var_view%initialized = .true.
        var_view%owns_memory = .false.
        var_view%is_view = .true.
        var_view%n_elements = 100
        
        ! View points to same data (in real implementation)
        var_view%data = var_owner%data
        
        if (.not. var_owner%owns_memory) test_passed = .false.
        if (var_view%owns_memory) test_passed = .false.
        if (.not. var_view%is_view) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Memory ownership test"
        else
            write(*,'(A)') "FAIL: Memory ownership test"
        end if
        
        ! Clean up only the owner
        if (var_owner%owns_memory) then
            deallocate(var_owner%data%values_r64)
        end if
    end subroutine test_memory_ownership

end program test_edge_cases