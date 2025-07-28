program test_quantile_groupby
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Quantile-based Groupby Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run tests
    call test_quartile_binning()
    call test_decile_binning()
    call test_quantile_aggregations()
    call test_skewed_distribution()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Quantile Groupby Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some quantile groupby tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All quantile groupby tests passed!"
    end if

contains

    subroutine test_quartile_binning()
        type(fortarray_t) :: data_var, quartile_result
        type(groupby_t) :: gb
        real(real64), dimension(16) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing quartile binning with groupby_quantiles..."
        
        ! Create data with known distribution for quartiles
        do i = 1, 16
            coord_values(i) = real(i, real64)  ! 1, 2, 3, ..., 16
            data_values(i) = real(i * 5, real64)  ! 5, 10, 15, ..., 80
        end do
        
        ! Create fortarray with coordinate
        data_var = new_array(data_values, name="data", dim_names=["x"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "x"
        data_var%coords(1)%length = 16
        allocate(data_var%coords(1)%values_r64(16))
        data_var%coords(1)%values_r64 = coord_values
        data_var%has_coord(1) = .true.
        
        ! Test quartile binning (4 quantiles)
        write(*,'(A)') "   Test 1: Quartile binning (4 quantiles)"
        gb = data_var%groupby_quantiles("x", 4)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Quartile binning failed to initialize"
        else if (gb%n_groups /= 4) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 4 quartiles, got ", gb%n_groups, " groups"
        else
            write(*,'(A)') "     PASS: Quartile binning created successfully"
            
            ! Test quartile aggregation
            quartile_result = gb%mean()
            if (quartile_result%n_elements /= 4) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 4 quartile means, got ", quartile_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Quartile aggregation computed successfully"
            end if
            call finalize_variable(quartile_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Quartile binning test"
        else
            write(*,'(A)') "FAIL: Quartile binning test"
        end if
    end subroutine test_quartile_binning
    
    subroutine test_decile_binning()
        type(fortarray_t) :: data_var, decile_result
        type(groupby_t) :: gb
        real(real64), dimension(20) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing decile binning with groupby_quantiles..."
        
        ! Create data for decile analysis
        do i = 1, 20
            coord_values(i) = real(i, real64)
            data_values(i) = real(i * 2, real64)
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
        
        ! Test decile binning (10 quantiles)
        write(*,'(A)') "   Test 1: Decile binning (10 quantiles)"
        gb = data_var%groupby_quantiles("x", 10)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Decile binning failed to initialize"
        else if (gb%n_groups /= 10) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 10 deciles, got ", gb%n_groups, " groups"
        else
            write(*,'(A)') "     PASS: Decile binning created successfully"
            
            decile_result = gb%sum()
            if (decile_result%n_elements /= 10) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 10 decile sums, got ", decile_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Decile aggregation computed successfully"
            end if
            call finalize_variable(decile_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Decile binning test"
        else
            write(*,'(A)') "FAIL: Decile binning test"
        end if
    end subroutine test_decile_binning
    
    subroutine test_quantile_aggregations()
        type(fortarray_t) :: data_var, mean_result, std_result
        type(groupby_t) :: gb
        real(real64), dimension(12) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing quantile aggregation operations..."
        
        ! Create data with variation for testing statistics
        do i = 1, 12
            coord_values(i) = real(i, real64)
            data_values(i) = real(i, real64) ** 1.5_real64  ! Non-linear growth
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
        
        ! Test tertile binning with multiple aggregations
        write(*,'(A)') "   Test 1: Tertile binning with multiple aggregations"
        gb = data_var%groupby_quantiles("x", 3)
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Tertile binning failed"
        else
            ! Test mean aggregation
            mean_result = gb%mean()
            if (mean_result%n_elements /= 3) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 3 tertile means, got ", mean_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Tertile mean computed successfully"
            end if
            call finalize_variable(mean_result)
            
            ! Test standard deviation aggregation
            std_result = gb%std()
            if (std_result%n_elements /= 3) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 3 tertile stds, got ", std_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Tertile std computed successfully"
            end if
            call finalize_variable(std_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Quantile aggregations test"
        else
            write(*,'(A)') "FAIL: Quantile aggregations test"
        end if
    end subroutine test_quantile_aggregations
    
    subroutine test_skewed_distribution()
        type(fortarray_t) :: data_var, quantile_result
        type(groupby_t) :: gb
        real(real64), dimension(15) :: data_values, coord_values
        logical :: test_passed
        integer :: i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing quantile binning with skewed distribution..."
        
        ! Create skewed data (exponential-like growth)
        do i = 1, 15
            coord_values(i) = real(i, real64) ** 2.0_real64  ! Highly skewed: 1, 4, 9, 16, 25, ...
            data_values(i) = real(i, real64)
        end do
        
        data_var = new_array(data_values, name="data", dim_names=["x"])
        
        ! Add coordinate
        if (.not. allocated(data_var%coords)) then
            allocate(data_var%coords(1))
            allocate(data_var%has_coord(1))
        end if
        data_var%coords(1)%name = "x"
        data_var%coords(1)%length = 15
        allocate(data_var%coords(1)%values_r64(15))
        data_var%coords(1)%values_r64 = coord_values
        data_var%has_coord(1) = .true.
        
        ! Test quantile binning with skewed data
        write(*,'(A)') "   Test 1: Quantile binning with skewed distribution"
        gb = data_var%groupby_quantiles("x", 5)  ! Quintiles
        
        if (.not. gb%initialized) then
            test_passed = .false.
            write(error_unit,'(A)') "Skewed distribution quantile binning failed"
        else if (gb%n_groups /= 5) then
            test_passed = .false.
            write(error_unit,'(A,I0,A)') "Expected 5 quintiles, got ", gb%n_groups, " groups"
        else
            write(*,'(A)') "     PASS: Skewed distribution quantile binning successful"
            
            quantile_result = gb%mean()
            if (quantile_result%n_elements /= 5) then
                test_passed = .false.
                write(error_unit,'(A,I0,A)') "Expected 5 quintile means, got ", quantile_result%n_elements, " values"
            else
                write(*,'(A)') "     PASS: Skewed distribution aggregation successful"
            end if
            call finalize_variable(quantile_result)
        end if
        
        call finalize_groupby(gb)
        call finalize_variable(data_var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Skewed distribution test"
        else
            write(*,'(A)') "FAIL: Skewed distribution test"
        end if
    end subroutine test_skewed_distribution

end program test_quantile_groupby