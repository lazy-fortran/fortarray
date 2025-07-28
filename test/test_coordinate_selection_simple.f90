program test_coordinate_selection_simple
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================="
    write(*,'(A)') "Running Coordinate Selection Test Suite"
    write(*,'(A)') "========================================="
    
    ! Run simplified tests
    call test_basic_selection()
    call test_range_selection()
    call test_nearest_selection()
    
    ! Summary
    write(*,'(A)') "========================================="
    write(*,'(A,I0,A,I0,A)') "Coordinate Selection Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some coordinate selection tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All coordinate selection tests passed!"
    end if

contains

    subroutine test_basic_selection()
        type(fortarray_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        integer :: idx
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with coordinates
        data = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        coord_vals = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", idx)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = new_array(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Test basic location finding
        idx = loc(var, 3.0_real64, 1)
        
        if (idx /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "loc() returned wrong index: ", idx
        end if
        
        ! Test selection using slice
        result = slice(var, create_index(idx))
        
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A)') "Selection wrong size"
        else if (abs(result%data%values_r64(1) - 30.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Selection wrong value"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic selection test"
        else
            write(*,'(A)') "FAIL: Basic selection test"
        end if
    end subroutine test_basic_selection
    
    subroutine test_range_selection()
        type(fortarray_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(10) :: data, coord_vals
        integer :: i, start_idx, end_idx
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = [(real(i, real64), i=1,10)]
        coord_vals = [(real(i*10, real64), i=1,10)]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = new_array(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Find indices for range manually
        start_idx = 0
        end_idx = 0
        do i = 1, 10
            if (coord_vals(i) >= 25.0_real64 .and. start_idx == 0) start_idx = i
            if (coord_vals(i) <= 75.0_real64) end_idx = i
        end do
        
        ! Use slice with range
        result = slice(var, create_slice(start_idx, end_idx))
        
        ! Check result
        if (result%n_elements /= (end_idx - start_idx + 1)) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Range selection wrong size: ", result%n_elements
        else if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Range selection wrong first value"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Range selection test"
        else
            write(*,'(A)') "FAIL: Range selection test"
        end if
    end subroutine test_range_selection
    
    subroutine test_nearest_selection()
        type(fortarray_t) :: var
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        integer :: idx
        real(real64) :: target, min_dist, dist
        integer :: i, nearest_idx
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with irregular coordinates
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [10.0_real64, 25.0_real64, 30.0_real64, 50.0_real64, 100.0_real64]
        
        call create_coordinate(x_coord, size(coord_vals), "real64", i)
        x_coord%name = "x"
        x_coord%values_r64 = coord_vals
        var = new_array(data, name="test_data", dim_names=["x"], coords=[x_coord])
        
        ! Find nearest to 27.0 manually
        target = 27.0_real64
        nearest_idx = 1
        min_dist = abs(coord_vals(1) - target)
        
        do i = 2, 5
            dist = abs(coord_vals(i) - target)
            if (dist < min_dist) then
                min_dist = dist
                nearest_idx = i
            end if
        end do
        
        ! Should be index 2 (value 25.0)
        if (nearest_idx /= 2) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Nearest calculation wrong: ", nearest_idx
        end if
        
        ! Test with method="nearest"
        idx = loc(var, target, 1, method="nearest")
        
        if (idx /= nearest_idx) then
            test_passed = .false.
            write(error_unit,'(A,I0,A,I0)') "loc(method=nearest) wrong: ", idx, " expected ", nearest_idx
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Nearest selection test"
        else
            write(*,'(A)') "FAIL: Nearest selection test"
        end if
    end subroutine test_nearest_selection
    
end program test_coordinate_selection_simple