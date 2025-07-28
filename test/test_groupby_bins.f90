program test_groupby_bins
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Advanced Groupby Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run tests
    call test_uniform_binning()
    call test_edge_cases()
    call test_bin_aggregations()
    call test_uneven_data_distribution()
    call test_single_bin()
    call test_many_bins()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Advanced Groupby Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some advanced groupby tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All advanced groupby tests passed!"
    end if

contains

    subroutine test_uniform_binning()
        type(fortarray_t) :: data_var, binned_result
        type(groupby_t) :: gb
        real(real64), dimension(10) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing uniform binning with groupby_bins..."
        
        ! Create data with values from 1.0 to 10.0
        do i = 1, 10
            coord_values(i) = real(i, real64)
            data_values(i) = real(i * 2, real64)  ! Simple pattern: 2, 4, 6, 8, ...
        end do
        
        ! Create fortarray with coordinate
        data_var = new_array(data_values, name="data", dim_names=["x"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "x"
        data_var%coords(1)%length = 10
        allocate(data_var%coords(1)%values_r64(10))
        data_var%coords(1)%values_r64 = coord_values
        data_var%has_coord(1) = .true.
        
        ! Test groupby_bins with 3 bins
        write(*,'(A)') "   Test 1: Groupby with 3 uniform bins"
        gb = data_var%groupby_bins("x", 3)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Uniform binning failed to initialize"
        else if (gb%n_groups /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 3 bins, got ", gb%n_groups, " bins"
        else
            write(*,'(A)') "     PASS: Uniform binning created successfully"
            
            ! Test bin aggregation
            binned_result = gb%mean()
            if (binned_result%n_elements /= 3) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 3 bin means, got ", binned_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Bin aggregation computed successfully"
            end if
            call finalize_variable(binned_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Uniform binning test"
        else
            write(*,'(A)') "FAIL: Uniform binning test"
        end if
    end subroutine test_uniform_binning
    
    subroutine test_edge_cases()
        type(fortarray_t) :: data_var
        type(groupby_t) :: gb
        real(real64), dimension(5) :: data_values, coord_values
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing edge cases for groupby_bins..."
        
        ! Create data with edge values
        coord_values = [1.0_real64, 1.0_real64, 5.0_real64, 5.0_real64, 3.0_real64]
        data_values = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 25.0_real64]
        
        data_var = new_array(data_values, name="data", dim_names=["x"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "x"
        data_var%coords(1)%length = 5
        allocate(data_var%coords(1)%values_r64(5))
        data_var%coords(1)%values_r64 = coord_values
        data_var%has_coord(1) = .true.
        
        ! Test binning with duplicate edge values
        write(*,'(A)') "   Test 1: Binning with duplicate edge values"
        gb = data_var%groupby_bins("x", 2)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Edge case binning failed to initialize"
        else
            write(*,'(A)') "     PASS: Edge case binning handled correctly"
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Edge cases test"
        else
            write(*,'(A)') "FAIL: Edge cases test"
        end if
    end subroutine test_edge_cases
    
    subroutine test_bin_aggregations()
        type(fortarray_t) :: data_var, mean_result, sum_result
        type(groupby_t) :: gb
        real(real64), dimension(8) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing bin aggregation operations..."
        
        ! Create data suitable for testing aggregations
        coord_values = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
                       5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64]
        do i = 1, 8
            data_values(i) = real(i * 10, real64)  ! 10, 20, 30, ..., 80
        end do
        
        data_var = new_array(data_values, name="data", dim_names=["x"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "x"
        data_var%coords(1)%length = 8
        allocate(data_var%coords(1)%values_r64(8))
        data_var%coords(1)%values_r64 = coord_values
        data_var%has_coord(1) = .true.
        
        ! Test binning with 4 bins
        gb = data_var%groupby_bins("x", 4)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Bin aggregation test setup failed"
        else
            ! Test mean aggregation
            write(*,'(A)') "   Test 1: Bin mean aggregation"
            mean_result = gb%mean()
            if (mean_result%n_elements /= 4) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 4 bin means, got ", mean_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Bin mean computed successfully"
            end if
            call finalize_variable(mean_result)
            
            ! Test sum aggregation
            write(*,'(A)') "   Test 2: Bin sum aggregation"
            sum_result = gb%sum()
            if (sum_result%n_elements /= 4) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 4 bin sums, got ", sum_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Bin sum computed successfully"
            end if
            call finalize_variable(sum_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Bin aggregations test"
        else
            write(*,'(A)') "FAIL: Bin aggregations test"
        end if
    end subroutine test_bin_aggregations
    
    subroutine test_uneven_data_distribution()
        type(fortarray_t) :: data_var, binned_result
        type(groupby_t) :: gb
        real(real64), dimension(12) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing binning with uneven data distribution..."
        
        ! Create data with uneven distribution (clustered at edges)
        coord_values = [1.0_real64, 1.1_real64, 1.2_real64, 1.3_real64, &  ! Cluster at low end
                       5.0_real64, 6.0_real64, &                          ! Middle values
                       9.8_real64, 9.9_real64, 9.95_real64, 9.98_real64, & ! Cluster at high end
                       2.0_real64, 8.0_real64]                            ! Scattered values
        
        do i = 1, 12
            data_values(i) = real(i, real64)
        end do
        
        data_var = new_array(data_values, name="data", dim_names=["x"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "x"
        data_var%coords(1)%length = 12
        allocate(data_var%coords(1)%values_r64(12))
        data_var%coords(1)%values_r64 = coord_values
        data_var%has_coord(1) = .true.
        
        ! Test binning with uneven distribution
        write(*,'(A)') "   Test 1: Binning uneven distribution"
        gb = data_var%groupby_bins("x", 3)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Uneven distribution binning failed"
        else
            binned_result = gb%mean()
            if (binned_result%n_elements /= 3) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 3 bins, got ", binned_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Uneven distribution handled correctly"
            end if
            call finalize_variable(binned_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Uneven data distribution test"
        else
            write(*,'(A)') "FAIL: Uneven data distribution test"
        end if
    end subroutine test_uneven_data_distribution
    
    subroutine test_single_bin()
        type(fortarray_t) :: data_var, binned_result
        type(groupby_t) :: gb
        real(real64), dimension(5) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing single bin case..."
        
        do i = 1, 5
            coord_values(i) = real(i, real64)
            data_values(i) = real(i * 5, real64)
        end do
        
        data_var = new_array(data_values, name="data", dim_names=["x"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "x"
        data_var%coords(1)%length = 5
        allocate(data_var%coords(1)%values_r64(5))
        data_var%coords(1)%values_r64 = coord_values
        data_var%has_coord(1) = .true.
        
        ! Test single bin
        write(*,'(A)') "   Test 1: Single bin groupby"
        gb = data_var%groupby_bins("x", 1)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Single bin groupby failed"
        else if (gb%n_groups /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 1 bin, got ", gb%n_groups, " bins"
        else
            binned_result = gb%mean()
            if (binned_result%n_elements /= 1) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 1 result, got ", binned_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Single bin handled correctly"
            end if
            call finalize_variable(binned_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Single bin test"
        else
            write(*,'(A)') "FAIL: Single bin test"
        end if
    end subroutine test_single_bin
    
    subroutine test_many_bins()
        type(fortarray_t) :: data_var, binned_result
        type(groupby_t) :: gb
        real(real64), dimension(20) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing many bins case..."
        
        do i = 1, 20
            coord_values(i) = real(i, real64)
            data_values(i) = real(i * 3, real64)
        end do
        
        data_var = new_array(data_values, name="data", dim_names=["x"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "x"
        data_var%coords(1)%length = 20
        allocate(data_var%coords(1)%values_r64(20))
        data_var%coords(1)%values_r64 = coord_values
        data_var%has_coord(1) = .true.
        
        ! Test many bins (10 bins for 20 data points)
        write(*,'(A)') "   Test 1: Many bins groupby"
        gb = data_var%groupby_bins("x", 10)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Many bins groupby failed"
        else if (gb%n_groups /= 10) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 10 bins, got ", gb%n_groups, " bins"
        else
            binned_result = gb%mean()
            if (binned_result%n_elements /= 10) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 10 results, got ", binned_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Many bins handled correctly"
            end if
            call finalize_variable(binned_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Many bins test"
        else
            write(*,'(A)') "FAIL: Many bins test"
        end if
    end subroutine test_many_bins

end program test_groupby_bins