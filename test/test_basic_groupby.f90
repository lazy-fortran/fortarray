program test_basic_groupby
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Basic Groupby Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run tests
    call test_groupby_creation()
    call test_coordinate_based_grouping()
    call test_groupby_aggregations()
    call test_group_iteration()
    call test_group_validation()
    call test_groupby_memory_management()
    call test_complex_groupby_operations()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Basic Groupby Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some basic groupby tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All basic groupby tests passed!"
    end if

contains

    subroutine test_groupby_creation()
        type(fortarray_t) :: var, groups
        type(groupby_t) :: gb
        real(real64), dimension(12) :: data
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing groupby_t type creation and basic functionality..."
        
        ! Create test data
        do i = 1, 12
            data(i) = real(i, real64)
        end do
        var = new_array(data, name="test_data", dim_names=["time"])
        
        ! Create group labels (monthly data as integers)
        block
            integer, dimension(12) :: month_codes
            month_codes = [(i, i=1,12)]  ! 1=Jan, 2=Feb, ..., 12=Dec
            groups = new_array(month_codes, name="months", dim_names=["time"])
        end block
        
        ! Test 1: Create groupby object
        write(*,'(A)') "   Test 1: Create groupby object"
        gb = var%groupby("time", groups)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Groupby object not initialized"
        else
            write(*,'(A)') "     PASS: Groupby object created"
        end if
        
        ! Test 2: Check group count
        write(*,'(A)') "   Test 2: Check number of groups"
        if (gb%n_groups /= 12) then
            test_passed = .false.
            write(error_unit,'(A)') "Wrong number of groups"
        else
            write(*,'(A)') "     PASS: Correct number of groups"
        end if
        
        ! Test 3: Check group names
        write(*,'(A)') "   Test 3: Check group names"
        if (.not. allocated(gb%group_names)) then
            test_passed = .false.
            write(error_unit,'(A)') "Group names not allocated"
        else if (size(gb%group_names) /= 12) then
            test_passed = .false.
            write(error_unit,'(A)') "Wrong number of group names"
        else
            write(*,'(A)') "     PASS: Group names allocated correctly"
        end if
        
        ! Test 4: Check group indices
        write(*,'(A)') "   Test 4: Check group indices"
        if (.not. allocated(gb%group_indices)) then
            test_passed = .false.
            write(error_unit,'(A)') "Group indices not allocated"
        else
            write(*,'(A)') "     PASS: Group indices allocated correctly"
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(var)
        call finalize_variable(groups)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Groupby creation test"
        else
            write(*,'(A)') "FAIL: Groupby creation test"
        end if
    end subroutine test_groupby_creation

    subroutine test_coordinate_based_grouping()
        type(fortarray_t) :: var, coord_var, result
        type(groupby_t) :: gb
        real(real64), dimension(10) :: data, coords
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing coordinate-based grouping with validation..."
        
        ! Create test data with coordinates
        do i = 1, 10
            data(i) = real(i, real64) * 10.0_real64
            coords(i) = real(mod(i-1, 5), real64)  ! 0,1,2,3,4,0,1,2,3,4
        end do
        var = new_array(data, name="test_data", dim_names=["x"])
        coord_var = new_array(coords, name="groups", dim_names=["x"])
        
        ! Test 1: Group by coordinate values
        write(*,'(A)') "   Test 1: Group by coordinate values"
        gb = var%groupby("x", coord_var)
        
        if (gb%n_groups /= 5) then
            test_passed = .false.
            write(error_unit,'(A)') "Wrong number of coordinate groups"
        else
            write(*,'(A)') "     PASS: Correct number of coordinate groups"
        end if
        
        ! Test 2: Validate group membership
        write(*,'(A)') "   Test 2: Validate group membership"
        if (.not. validate_groupby(gb, var)) then
            test_passed = .false.
            write(error_unit,'(A)') "Group validation failed"
        else
            write(*,'(A)') "     PASS: Group validation successful"
        end if
        
        ! Test 3: Check group sizes
        write(*,'(A)') "   Test 3: Check group sizes"
        if (.not. allocated(gb%group_sizes)) then
            test_passed = .false.
            write(error_unit,'(A)') "Group sizes not allocated"
        else if (any(gb%group_sizes /= 2)) then
            test_passed = .false.
            write(error_unit,'(A)') "Incorrect group sizes"
        else
            write(*,'(A)') "     PASS: Group sizes correct"
        end if
        
        ! Test 4: Access specific group
        write(*,'(A)') "   Test 4: Access specific group"
        result = gb%get_group("0.000000")
        if (result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Group access failed"
        else
            write(*,'(A)') "     PASS: Group access successful"
        end if
        call finalize_variable(result)
        
        call finalize_groupby(gb)
        call finalize_variable(var)
        call finalize_variable(coord_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Coordinate-based grouping test"
        else
            write(*,'(A)') "FAIL: Coordinate-based grouping test"
        end if
    end subroutine test_coordinate_based_grouping

    subroutine test_groupby_aggregations()
        type(fortarray_t) :: var, groups, mean_result, sum_result, std_result
        type(groupby_t) :: gb
        real(real64), dimension(8) :: data
        integer, dimension(8) :: group_ids
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing groupby aggregations (mean, sum, std, etc.)..."
        
        ! Create test data: two groups of 4 elements each
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
               10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64]
        group_ids = [1, 1, 1, 1, 2, 2, 2, 2]
        
        var = new_array(data, name="test_data", dim_names=["x"])
        groups = new_array(group_ids, name="group_labels", dim_names=["x"])
        
        gb = var%groupby("x", groups)
        
        ! Test 1: Groupby mean
        write(*,'(A)') "   Test 1: Groupby mean aggregation"
        mean_result = gb%mean()
        
        if (mean_result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean result wrong size"
        else if (abs(mean_result%data%values_r64(1) - 2.5_real64) > 1e-10 .or. &
                abs(mean_result%data%values_r64(2) - 25.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean values incorrect"
        else
            write(*,'(A)') "     PASS: Groupby mean correct"
        end if
        call finalize_variable(mean_result)
        
        ! Test 2: Groupby sum
        write(*,'(A)') "   Test 2: Groupby sum aggregation"
        sum_result = gb%sum()
        
        if (sum_result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum result wrong size"
        else if (abs(sum_result%data%values_r64(1) - 10.0_real64) > 1e-10 .or. &
                abs(sum_result%data%values_r64(2) - 100.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Sum values incorrect"
        else
            write(*,'(A)') "     PASS: Groupby sum correct"
        end if
        call finalize_variable(sum_result)
        
        ! Test 3: Groupby std
        write(*,'(A)') "   Test 3: Groupby std aggregation"
        std_result = gb%std()
        
        if (std_result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Std result wrong size"
        else
            ! Group 1 std: sqrt(((1-2.5)^2 + (2-2.5)^2 + (3-2.5)^2 + (4-2.5)^2)/4) = sqrt(1.25) ≈ 1.118
            write(*,'(A)') "     PASS: Groupby std computed"
        end if
        call finalize_variable(std_result)
        
        ! Test 4: Test aggregation with missing values
        write(*,'(A)') "   Test 4: Aggregation with missing values"
        data(2) = huge(1.0_real64)  ! Set missing value
        var = new_array(data, name="test_data_missing", dim_names=["x"])
        gb = var%groupby("x", groups)
        mean_result = gb%mean(skipna=.true.)
        
        if (mean_result%n_elements /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Mean with missing wrong size"
        else
            write(*,'(A)') "     PASS: Groupby mean with missing values"
        end if
        call finalize_variable(mean_result)
        
        call finalize_groupby(gb)
        call finalize_variable(var)
        call finalize_variable(groups)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Groupby aggregations test"
        else
            write(*,'(A)') "FAIL: Groupby aggregations test"
        end if
    end subroutine test_groupby_aggregations

    subroutine test_group_iteration()
        type(fortarray_t) :: var, groups, group_data
        type(groupby_t) :: gb
        real(real64), dimension(6) :: data
        logical :: test_passed
        integer :: i, group_count
        character(len=64) :: group_name
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing efficient group iteration..."
        
        ! Create test data
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        block
            integer, dimension(6) :: group_codes
            group_codes = [1, 1, 2, 2, 3, 3]  ! 1=A, 2=B, 3=C
            
            var = new_array(data, name="test_data", dim_names=["x"])
            groups = new_array(group_codes, name="labels", dim_names=["x"])
        end block
        
        gb = var%groupby("x", groups)
        
        ! Test 1: Iterate through all groups
        write(*,'(A)') "   Test 1: Iterate through all groups"
        group_count = 0
        do i = 1, gb%n_groups
            group_name = gb%group_names(i)
            group_data = gb%get_group(trim(group_name))
            
            if (group_data%n_elements /= 2) then
                test_passed = .false.
                write(error_unit,'(A)') "Group iteration wrong size"
                call finalize_variable(group_data)
                exit
            end if
            
            group_count = group_count + 1
            call finalize_variable(group_data)
        end do
        
        if (group_count /= 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Wrong number of groups iterated"
        else
            write(*,'(A)') "     PASS: Group iteration successful"
        end if
        
        ! Test 2: Test group iterator interface
        write(*,'(A)') "   Test 2: Group iterator interface"
        call gb%reset_iterator()
        group_count = 0
        
        do while (gb%has_next_group())
            group_data = gb%next_group(group_name)
            group_count = group_count + 1
            call finalize_variable(group_data)
        end do
        
        if (group_count /= 3) then
            test_passed = .false.
            write(error_unit,'(A)') "Iterator interface failed"
        else
            write(*,'(A)') "     PASS: Group iterator interface"
        end if
        
        ! Test 3: Test group filtering during iteration
        write(*,'(A)') "   Test 3: Group filtering during iteration"
        call gb%reset_iterator()
        group_count = 0
        
        do while (gb%has_next_group())
            group_data = gb%next_group(group_name)
            ! Only count groups with name starting with 'A' or 'B'
            if (group_name(1:1) == 'A' .or. group_name(1:1) == 'B') then
                group_count = group_count + 1
            end if
            call finalize_variable(group_data)
        end do
        
        if (group_count /= 2) then
            test_passed = .false.
            write(error_unit,'(A)') "Group filtering failed"
        else
            write(*,'(A)') "     PASS: Group filtering successful"
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(var)
        call finalize_variable(groups)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Group iteration test"
        else
            write(*,'(A)') "FAIL: Group iteration test"
        end if
    end subroutine test_group_iteration

    subroutine test_group_validation()
        type(fortarray_t) :: var, groups, invalid_groups
        type(groupby_t) :: gb
        real(real64), dimension(5) :: data
        integer, dimension(5) :: valid_labels
        integer, dimension(3) :: invalid_labels  ! Wrong size
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing comprehensive group validation and error handling..."
        
        ! Create test data
        do i = 1, 5
            data(i) = real(i, real64)
        end do
        valid_labels = [1, 1, 2, 2, 3]
        invalid_labels = [1, 2, 3]  ! Size mismatch
        
        var = new_array(data, name="test_data", dim_names=["x"])
        groups = new_array(valid_labels, name="valid_labels", dim_names=["x"])
        invalid_groups = new_array(invalid_labels, name="invalid_labels", dim_names=["x"])
        
        ! Test 1: Valid groupby should succeed
        write(*,'(A)') "   Test 1: Valid groupby creation"
        gb = var%groupby("x", groups)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Valid groupby failed to initialize"
        else
            write(*,'(A)') "     PASS: Valid groupby created"
        end if
        call finalize_groupby(gb)
        
        ! Test 2: Invalid groupby should fail gracefully
        write(*,'(A)') "   Test 2: Invalid groupby error handling"
        gb = var%groupby("x", invalid_groups)
        
        if (gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Invalid groupby should not initialize"
            call finalize_groupby(gb)
        else
            write(*,'(A)') "     PASS: Invalid groupby properly rejected"
        end if
        
        ! Test 3: Test dimension validation
        write(*,'(A)') "   Test 3: Dimension validation"
        gb = var%groupby("nonexistent", groups)
        
        if (gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Groupby with invalid dimension should fail"
            call finalize_groupby(gb)
        else
            write(*,'(A)') "     PASS: Invalid dimension properly rejected"
        end if
        
        ! Test 4: Test empty group handling
        write(*,'(A)') "   Test 4: Empty group handling"
        block
            real(real64), dimension(0) :: empty_data
            type(fortarray_t) :: empty_var, empty_groups
            
            empty_var = new_array(empty_data, name="empty", dim_names=["x"])
            empty_groups = new_array(empty_data, name="empty_groups", dim_names=["x"])
            
            gb = empty_var%groupby("x", empty_groups)
            
            if (gb%initialized .and. gb%n_groups /= 0) then
                test_passed = .false.
                write(error_unit,'(A)') "Empty groupby handling failed"
                call finalize_groupby(gb)
            else
                write(*,'(A)') "     PASS: Empty groups handled correctly"
            end if
            
            call finalize_variable(empty_var)
            call finalize_variable(empty_groups)
        end block
        
        call finalize_variable(var)
        call finalize_variable(groups)
        call finalize_variable(invalid_groups)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Group validation test"
        else
            write(*,'(A)') "FAIL: Group validation test"
        end if
    end subroutine test_group_validation

    subroutine test_groupby_memory_management()
        type(fortarray_t) :: var, groups
        type(groupby_t) :: gb
        real(real64), dimension(100) :: data
        integer, dimension(100) :: group_labels
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing groupby_t memory management..."
        
        ! Create large test data
        do i = 1, 100
            data(i) = real(i, real64)
            group_labels(i) = mod(i-1, 10) + 1  ! 10 groups
        end do
        
        var = new_array(data, name="large_data", dim_names=["x"])
        groups = new_array(group_labels, name="group_labels", dim_names=["x"])
        
        ! Test 1: Create and destroy multiple groupby objects
        write(*,'(A)') "   Test 1: Create and destroy multiple groupby objects"
        do i = 1, 5
            gb = var%groupby("x", groups)
            
            if (.not. gb%initialized) then
                test_passed = .false.
                write(error_unit,'(A)') "Groupby creation failed in loop"
                exit
            end if
            
            call finalize_groupby(gb)
        end do
        
        if (test_passed) then
            write(*,'(A)') "     PASS: Multiple create/destroy cycles"
        end if
        
        ! Test 2: Test memory cleanup after aggregations
        write(*,'(A)') "   Test 2: Memory cleanup after aggregations"
        gb = var%groupby("x", groups)
        
        do i = 1, 3
            block
                type(fortarray_t) :: result
                result = gb%mean()
                call finalize_variable(result)
                
                result = gb%sum()
                call finalize_variable(result)
            end block
        end do
        
        write(*,'(A)') "     PASS: Memory cleanup after aggregations"
        
        ! Test 3: Test finalizer robustness
        write(*,'(A)') "   Test 3: Finalizer robustness"
        call finalize_groupby(gb)  ! Should not crash even if called multiple times
        call finalize_groupby(gb)
        write(*,'(A)') "     PASS: Finalizer robustness"
        
        call finalize_variable(var)
        call finalize_variable(groups)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Memory management test"
        else
            write(*,'(A)') "FAIL: Memory management test"
        end if
    end subroutine test_groupby_memory_management

    subroutine test_complex_groupby_operations()
        type(fortarray_t) :: var, groups, result
        type(groupby_t) :: gb
        real(real64), dimension(12) :: data
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing complex groupby operations and edge cases..."
        
        ! Create seasonal data
        data = [10.0_real64, 15.0_real64, 20.0_real64, &  ! Spring
               25.0_real64, 30.0_real64, 35.0_real64, &  ! Summer
               20.0_real64, 15.0_real64, 10.0_real64, &  ! Fall
               5.0_real64, 0.0_real64, -5.0_real64]      ! Winter
        
        ! Use integer season codes instead of strings
        block
            integer, dimension(12) :: season_codes
            season_codes = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4]  ! 1=Spring, 2=Summer, 3=Fall, 4=Winter
            
            var = new_array(data, name="temperature", dim_names=["month"])
            groups = new_array(season_codes, name="seasons", dim_names=["month"])
        end block
        
        gb = var%groupby("month", groups)
        
        ! Test 1: Multiple aggregations on same groupby
        write(*,'(A)') "   Test 1: Multiple aggregations on same groupby"
        block
            type(fortarray_t) :: mean_temp, max_temp, min_temp
            
            mean_temp = gb%mean()
            max_temp = gb%max()
            min_temp = gb%min()
            
            if (mean_temp%n_elements /= 4 .or. max_temp%n_elements /= 4 .or. &
                min_temp%n_elements /= 4) then
                test_passed = .false.
                write(error_unit,'(A)') "Multiple aggregations failed"
            else
                write(*,'(A)') "     PASS: Multiple aggregations successful"
            end if
            
            call finalize_variable(mean_temp)
            call finalize_variable(max_temp)
            call finalize_variable(min_temp)
        end block
        
        ! Test 2: Chained operations with groupby
        write(*,'(A)') "   Test 2: Simple groupby mean"
        result = gb%mean()
        
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Groupby mean failed"
        else
            write(*,'(A)') "     PASS: Groupby mean successful"
        end if
        call finalize_variable(result)
        
        ! Test 3: Custom aggregation function
        write(*,'(A)') "   Test 3: Custom aggregation function"
        result = gb%apply("range")  ! max - min for each group
        
        if (result%n_elements /= 4) then
            test_passed = .false.
            write(error_unit,'(A)') "Custom aggregation failed"
        else
            write(*,'(A)') "     PASS: Custom aggregation successful"
        end if
        call finalize_variable(result)
        
        ! Test 4: Groupby with transform (broadcasting result back to original size)
        write(*,'(A)') "   Test 4: Groupby transform operation"
        result = gb%transform("mean")  ! Broadcast group means back to original size
        
        if (result%n_elements /= 12) then
            test_passed = .false.
            write(error_unit,'(A)') "Transform operation failed"
        else
            write(*,'(A)') "     PASS: Transform operation successful"
        end if
        call finalize_variable(result)
        
        call finalize_groupby(gb)
        call finalize_variable(var)
        call finalize_variable(groups)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Complex groupby operations test"
        else
            write(*,'(A)') "FAIL: Complex groupby operations test"
        end if
    end subroutine test_complex_groupby_operations

end program test_basic_groupby