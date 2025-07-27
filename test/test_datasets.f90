program test_datasets
    use foxel_types
    use foxel_constructors
    use foxel_datasets
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    ! Test dataset operations
    call test_add_get_variable()
    call test_has_variable()
    call test_remove_variable()
    call test_list_variables()
    call test_select_variables()
    call test_error_handling()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_add_get_variable()
        type(dataset_t) :: dset
        type(variable_t) :: var1, var2, var_retrieved
        real(real64), dimension(5) :: data1
        real(real64), dimension(3,3) :: data2
        logical :: test_passed
        integer :: stat
        character(len=256) :: error_msg
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create dataset
        dset = dataset()
        dset%filename = "test_dataset.nc"
        
        ! Create variables
        data1 = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var1 = variable(data1, name="temperature", dim_names=["x"])
        
        data2 = reshape([1.0_real64, 2.0_real64, 3.0_real64, &
                        4.0_real64, 5.0_real64, 6.0_real64, &
                        7.0_real64, 8.0_real64, 9.0_real64], [3, 3])
        var2 = variable(data2, name="pressure", dim_names=["x", "y"])
        
        ! Add variables to dataset
        call add_variable(dset, var1, stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to add first variable"
        end if
        
        call add_variable(dset, var2, stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to add second variable"
        end if
        
        ! Get variables back
        var_retrieved = get_variable(dset, "temperature", stat=stat)
        if (stat /= 0 .or. .not. var_retrieved%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to retrieve temperature variable"
        else if (trim(var_retrieved%name) /= "temperature") then
            test_passed = .false.
            write(error_unit,'(A)') "Retrieved variable has wrong name"
        end if
        
        var_retrieved = get_variable(dset, "pressure", stat=stat)
        if (stat /= 0 .or. .not. var_retrieved%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to retrieve pressure variable"
        end if
        
        ! Test non-existent variable
        var_retrieved = get_variable(dset, "nonexistent", stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should have failed to retrieve non-existent variable"
        end if
        
        ! Clean up
        call finalize_dataset(dset)
        call finalize_variable(var1)
        call finalize_variable(var2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Add/get variable test"
        else
            write(*,'(A)') "FAIL: Add/get variable test"
        end if
    end subroutine test_add_get_variable
    
    subroutine test_has_variable()
        type(dataset_t) :: dset
        type(variable_t) :: var
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create dataset and variable
        dset = dataset()
        dset%filename = "test_dataset.nc"
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_var", dim_names=["x"])
        
        ! Initially should not have variable
        if (has_variable(dset, "test_var")) then
            test_passed = .false.
            write(error_unit,'(A)') "Dataset should not have variable before adding"
        end if
        
        ! Add variable
        call add_variable(dset, var)
        
        ! Now should have variable
        if (.not. has_variable(dset, "test_var")) then
            test_passed = .false.
            write(error_unit,'(A)') "Dataset should have variable after adding"
        end if
        
        ! Should not have non-existent variable
        if (has_variable(dset, "nonexistent")) then
            test_passed = .false.
            write(error_unit,'(A)') "Dataset should not have non-existent variable"
        end if
        
        ! Clean up
        call finalize_dataset(dset)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Has variable test"
        else
            write(*,'(A)') "FAIL: Has variable test"
        end if
    end subroutine test_has_variable
    
    subroutine test_remove_variable()
        type(dataset_t) :: dset
        type(variable_t) :: var1, var2
        real(real64), dimension(5) :: data
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create dataset and variables
        dset = dataset()
        dset%filename = "test_dataset.nc"
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var1 = variable(data, name="var1", dim_names=["x"])
        var2 = variable(data, name="var2", dim_names=["x"])
        
        ! Add variables
        call add_variable(dset, var1)
        call add_variable(dset, var2)
        
        ! Check both exist
        if (dset%n_vars /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Should have 2 variables"
        end if
        
        ! Remove var1
        call remove_variable(dset, "var1", stat=stat)
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to remove variable"
        end if
        
        ! Check only var2 exists
        if (dset%n_vars /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Should have 1 variable after removal"
        end if
        
        if (has_variable(dset, "var1")) then
            test_passed = .false.
            write(error_unit,'(A)') "var1 should not exist after removal"
        end if
        
        if (.not. has_variable(dset, "var2")) then
            test_passed = .false.
            write(error_unit,'(A)') "var2 should still exist"
        end if
        
        ! Try to remove non-existent variable
        call remove_variable(dset, "nonexistent", stat=stat)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail when removing non-existent variable"
        end if
        
        ! Clean up
        call finalize_dataset(dset)
        call finalize_variable(var1)
        call finalize_variable(var2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Remove variable test"
        else
            write(*,'(A)') "FAIL: Remove variable test"
        end if
    end subroutine test_remove_variable
    
    subroutine test_list_variables()
        type(dataset_t) :: dset
        type(variable_t) :: var1, var2, var3
        real(real64), dimension(5) :: data
        character(len=MAX_NAME_LEN), dimension(:), allocatable :: names
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create dataset and variables
        dset = dataset()
        dset%filename = "test_dataset.nc"
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var1 = variable(data, name="temperature", dim_names=["x"])
        var2 = variable(data, name="pressure", dim_names=["x"])
        var3 = variable(data, name="velocity", dim_names=["x"])
        
        ! Test empty dataset
        names = list_variables(dset)
        if (size(names) /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Empty dataset should return empty list"
        end if
        if (allocated(names)) deallocate(names)
        
        ! Add variables
        call add_variable(dset, var1)
        call add_variable(dset, var2)
        call add_variable(dset, var3)
        
        ! List variables
        names = list_variables(dset)
        if (size(names) /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Should have 3 variable names, got ", size(names)
        else
            ! Check names (order should be preserved)
            if (trim(names(1)) /= "temperature") then
                test_passed = .false.
                write(error_unit,'(A)') "First variable should be temperature"
            end if
            if (trim(names(2)) /= "pressure") then
                test_passed = .false.
                write(error_unit,'(A)') "Second variable should be pressure"
            end if
            if (trim(names(3)) /= "velocity") then
                test_passed = .false.
                write(error_unit,'(A)') "Third variable should be velocity"
            end if
        end if
        
        ! Clean up
        if (allocated(names)) deallocate(names)
        call finalize_dataset(dset)
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(var3)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: List variables test"
        else
            write(*,'(A)') "FAIL: List variables test"
        end if
    end subroutine test_list_variables
    
    subroutine test_select_variables()
        type(dataset_t) :: dset, subset
        type(variable_t) :: var1, var2, var3, var_check
        real(real64), dimension(5) :: data
        character(len=MAX_NAME_LEN), dimension(2) :: select_names
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create dataset and variables
        dset = dataset()
        dset%filename = "test_dataset.nc"
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var1 = variable(data, name="temperature", dim_names=["x"])
        var2 = variable(data * 2.0_real64, name="pressure", dim_names=["x"])
        var3 = variable(data * 3.0_real64, name="velocity", dim_names=["x"])
        
        ! Add all variables
        call add_variable(dset, var1)
        call add_variable(dset, var2)
        call add_variable(dset, var3)
        
        ! Select subset
        select_names = ["temperature", "velocity   "]
        subset = select_variables(dset, select_names, stat=stat)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to select variables"
        else
            ! Check subset
            if (subset%n_vars /= 2) then
                test_passed = .false.
                write(error_unit,'(A,I0)') "Subset should have 2 variables, got ", subset%n_vars
            end if
            
            if (.not. has_variable(subset, "temperature")) then
                test_passed = .false.
                write(error_unit,'(A)') "Subset should have temperature"
            end if
            
            if (.not. has_variable(subset, "velocity")) then
                test_passed = .false.
                write(error_unit,'(A)') "Subset should have velocity"
            end if
            
            if (has_variable(subset, "pressure")) then
                test_passed = .false.
                write(error_unit,'(A)') "Subset should not have pressure"
            end if
            
            ! Check data integrity
            var_check = get_variable(subset, "velocity", stat=stat)
            if (stat == 0 .and. var_check%initialized) then
                if (abs(var_check%data%values_r64(1) - 3.0_real64) > epsilon(1.0_real64)) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Velocity data not preserved correctly"
                end if
            end if
        end if
        
        ! Clean up
        call finalize_dataset(subset)
        call finalize_dataset(dset)
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(var3)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Select variables test"
        else
            write(*,'(A)') "FAIL: Select variables test"
        end if
    end subroutine test_select_variables
    
    subroutine test_error_handling()
        type(dataset_t) :: dset, uninit_dset
        type(variable_t) :: var, uninit_var
        real(real64), dimension(5) :: data
        character(len=MAX_NAME_LEN), dimension(1) :: bad_names
        logical :: test_passed
        integer :: stat
        character(len=256) :: error_msg
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create initialized dataset and variable
        dset = dataset()
        dset%filename = "test_dataset.nc"
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_var", dim_names=["x"])
        
        ! Test operations on uninitialized dataset
        uninit_dset%initialized = .false.
        
        if (has_variable(uninit_dset, "anything")) then
            test_passed = .false.
            write(error_unit,'(A)') "Uninitialized dataset should not have any variables"
        end if
        
        call add_variable(uninit_dset, var, stat=stat)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail to add to uninitialized dataset"
        end if
        
        ! Test adding uninitialized variable
        uninit_var%initialized = .false.
        call add_variable(dset, uninit_var, stat=stat)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail to add uninitialized variable"
        end if
        
        ! Test duplicate variable name
        call add_variable(dset, var)
        call add_variable(dset, var, stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail to add duplicate variable name"
        end if
        
        ! Test selecting non-existent variable
        bad_names = ["nonexistent"]
        dset = select_variables(dset, bad_names, stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail to select non-existent variable"
        end if
        
        ! Clean up
        call finalize_dataset(dset)
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Error handling test"
        else
            write(*,'(A)') "FAIL: Error handling test"
        end if
    end subroutine test_error_handling
    
end program test_datasets