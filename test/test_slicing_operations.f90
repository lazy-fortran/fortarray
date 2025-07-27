program test_slicing_operations
    use foxel
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Slicing Operations Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run all slicing tests
    call test_basic_slice_syntax()
    call test_negative_indices()
    call test_step_values()
    call test_slice_preserve_coordinates()
    call test_slice_1d_array()
    call test_slice_2d_array()
    call test_slice_3d_array()
    call test_slice_edge_cases()
    call test_slice_with_broadcasting()
    call test_slice_performance()
    call test_slice_multidimensional()
    call test_slice_bounds_checking()
    call test_slice_empty_result()
    call test_slice_coordinate_update()
    call test_slice_attributes_preservation()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Slicing Operations Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some slicing operations tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All slicing operations tests passed!"
    end if

contains

    subroutine test_basic_slice_syntax()
        type(variable_t) :: var, result
        real(real64), dimension(10) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 10
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test basic slice: [3:7]
        result = slice_range(var, 3, 7)
        
        ! Should return [3, 4, 5, 6, 7]
        if (result%n_elements /= 5) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 5 elements, got: ", result%n_elements
        else
            do i = 1, 5
                if (abs(result%data%values_r64(i) - real(i + 2, real64)) > 1e-10) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,F0.1)') "Element ", i, " should be ", real(i + 2, real64)
                    exit
                end if
            end do
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic slice syntax"
        else
            write(*,'(A)') "FAIL: Basic slice syntax"
        end if
    end subroutine test_basic_slice_syntax
    
    subroutine test_negative_indices()
        type(variable_t) :: var, result
        real(real64), dimension(10) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 10
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test negative indices: [-3:]  (last 3 elements)
        result = slice_from_negative(var, -3)
        
        ! Should return [8, 9, 10]
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 8.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 9.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 10.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Negative index result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Negative indices"
        else
            write(*,'(A)') "FAIL: Negative indices"
        end if
    end subroutine test_negative_indices
    
    subroutine test_step_values()
        type(variable_t) :: var, result
        real(real64), dimension(10) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do i = 1, 10
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test step values: [1:10:2] (every 2nd element)
        result = slice_with_step(var, 1, 10, 2)
        
        ! Should return [1, 3, 5, 7, 9]
        if (result%n_elements /= 5) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 5 elements, got: ", result%n_elements
        else
            do i = 1, 5
                if (abs(result%data%values_r64(i) - real(2*i-1, real64)) > 1e-10) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,F0.1)') "Element ", i, " should be ", real(2*i-1, real64)
                    exit
                end if
            end do
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Step values"
        else
            write(*,'(A)') "FAIL: Step values"
        end if
    end subroutine test_step_values
    
    subroutine test_slice_preserve_coordinates()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(10) :: data, coord_vals
        integer :: i, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with coordinates
        do i = 1, 10
            data(i) = real(i, real64)
            coord_vals(i) = real(i * 10, real64)  ! [10, 20, 30, ..., 100]
        end do
        
        call create_coordinate(x_coord, 10, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Test slice: [3:7] should preserve coordinates
        result = slice_range(var, 3, 7)
        
        ! Check coordinates are preserved and sliced
        if (.not. allocated(result%coords) .or. .not. result%has_coord(1)) then
            test_passed = .false.
            write(error_unit,'(A)') "Coordinates should be preserved"
        else
            ! Check coordinate values: should be [30, 40, 50, 60, 70]
            if (result%coords(1)%length /= 5) then
                test_passed = .false.
                write(error_unit,'(A,I0)') "Coordinate length should be 5, got: ", result%coords(1)%length
            else
                do i = 1, 5
                    if (abs(result%coords(1)%values_r64(i) - real((i+2)*10, real64)) > 1e-10) then
                        test_passed = .false.
                        write(error_unit,'(A,I0)') "Coordinate value incorrect at position: ", i
                        exit
                    end if
                end do
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice preserve coordinates"
        else
            write(*,'(A)') "FAIL: Slice preserve coordinates"
        end if
    end subroutine test_slice_preserve_coordinates
    
    subroutine test_slice_1d_array()
        type(variable_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test slice: [2:4]
        result = slice_range(var, 2, 4)
        
        if (result%n_elements /= 3) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 3 elements, got: ", result%n_elements
        else
            if (abs(result%data%values_r64(1) - 20.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(2) - 30.0_real64) > 1e-10 .or. &
                abs(result%data%values_r64(3) - 40.0_real64) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "1D slice result values incorrect"
            end if
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice 1D array"
        else
            write(*,'(A)') "FAIL: Slice 1D array"
        end if
    end subroutine test_slice_1d_array
    
    subroutine test_slice_2d_array()
        type(variable_t) :: var, result
        real(real64), dimension(12) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 3x4 array
        do i = 1, 12
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x", "y"], shape=[3, 4])
        
        ! Test slice: [2:3, 1:3] 
        result = slice_2d_range(var, 2, 3, 1, 3)
        
        ! Should be 2x3 subarray
        if (result%n_elements /= 6) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 6 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice 2D array"
        else
            write(*,'(A)') "FAIL: Slice 2D array"
        end if
    end subroutine test_slice_2d_array
    
    subroutine test_slice_3d_array()
        type(variable_t) :: var, result
        real(real64), dimension(24) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2x3x4 array
        do i = 1, 24
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x", "y", "z"], shape=[2, 3, 4])
        
        ! Test slice: [1:2, 2:3, 1:2]
        result = slice_3d_range(var, 1, 2, 2, 3, 1, 2)
        
        ! Should be 2x2x2 subarray
        if (result%n_elements /= 8) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Result should have 8 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice 3D array"
        else
            write(*,'(A)') "FAIL: Slice 3D array"
        end if
    end subroutine test_slice_3d_array
    
    subroutine test_slice_edge_cases()
        type(variable_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test edge case: empty slice [3:2]
        result = slice_range(var, 3, 2)
        
        if (result%n_elements /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Empty slice should have 0 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(result)
        
        ! Test edge case: single element [3:3]
        result = slice_range(var, 3, 3)
        
        if (result%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Single element slice should have 1 element, got: ", result%n_elements
        else if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Single element slice value incorrect"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice edge cases"
        else
            write(*,'(A)') "FAIL: Slice edge cases"
        end if
    end subroutine test_slice_edge_cases
    
    subroutine test_slice_with_broadcasting()
        type(variable_t) :: var, result
        real(real64), dimension(12) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 3x4 array and slice along one dimension
        do i = 1, 12
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x", "y"], shape=[3, 4])
        
        ! Test slicing with broadcasting: [2:3, :]
        result = slice_broadcast(var, 2, 3, 1)
        
        ! Should slice only first dimension
        if (result%n_elements /= 8) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Broadcast slice should have 8 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice with broadcasting"
        else
            write(*,'(A)') "FAIL: Slice with broadcasting"
        end if
    end subroutine test_slice_with_broadcasting
    
    subroutine test_slice_performance()
        type(variable_t) :: var, result
        real(real64), dimension(1000) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Large array performance test
        do i = 1, 1000
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test large slice: [100:900]
        result = slice_range(var, 100, 900)
        
        if (result%n_elements /= 801) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Large slice should have 801 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice performance"
        else
            write(*,'(A)') "FAIL: Slice performance"
        end if
    end subroutine test_slice_performance
    
    subroutine test_slice_multidimensional()
        type(variable_t) :: var, result
        real(real64), dimension(60) :: data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 3x4x5 array
        do i = 1, 60
            data(i) = real(i, real64)
        end do
        var = variable(data, name="test_data", dim_names=["x", "y", "z"], shape=[3, 4, 5])
        
        ! Test complex multidimensional slice
        result = slice_multidim(var)
        
        ! Basic check that slicing works
        if (result%n_elements <= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Multidimensional slice failed"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice multidimensional"
        else
            write(*,'(A)') "FAIL: Slice multidimensional"
        end if
    end subroutine test_slice_multidimensional
    
    subroutine test_slice_bounds_checking()
        type(variable_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test bounds checking: slice beyond array bounds should handle gracefully
        result = slice_with_bounds_check(var, 1, 10)
        
        ! Should clamp to valid range [1:5]
        if (result%n_elements /= 5) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Bounds check slice should have 5 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice bounds checking"
        else
            write(*,'(A)') "FAIL: Slice bounds checking"
        end if
    end subroutine test_slice_bounds_checking
    
    subroutine test_slice_empty_result()
        type(variable_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        
        ! Test empty result case
        result = slice_range(var, 6, 10)  ! Beyond array bounds
        
        if (result%n_elements /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Empty result should have 0 elements, got: ", result%n_elements
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice empty result"
        else
            write(*,'(A)') "FAIL: Slice empty result"
        end if
    end subroutine test_slice_empty_result
    
    subroutine test_slice_coordinate_update()
        type(variable_t) :: var, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(5) :: data, coord_vals
        integer :: stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coord_vals = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        
        call create_coordinate(x_coord, 5, "real64", stat)
        if (stat == 0) then
            x_coord%name = "x"
            x_coord%values_r64 = coord_vals
            x_coord%initialized = .true.
        end if
        
        var = variable(data, name="test_data", dim_names=["x"])
        allocate(var%coords(1), var%has_coord(1))
        var%coords(1) = x_coord
        var%has_coord(1) = .true.
        
        ! Test slice with coordinate update
        result = slice_range(var, 2, 4)
        
        ! Check that coordinates are updated correctly
        if (allocated(result%coords) .and. result%has_coord(1)) then
            if (result%coords(1)%length /= 3) then
                test_passed = .false.
                write(error_unit,'(A)') "Coordinate length not updated correctly"
            end if
        else
            test_passed = .false.
            write(error_unit,'(A)') "Coordinates not preserved in slice"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice coordinate update"
        else
            write(*,'(A)') "FAIL: Slice coordinate update"
        end if
    end subroutine test_slice_coordinate_update
    
    subroutine test_slice_attributes_preservation()
        type(variable_t) :: var, result
        real(real64), dimension(5) :: data
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        var = variable(data, name="test_data", dim_names=["x"])
        var%units = "meters"
        var%long_name = "Test data for slicing"
        
        ! Test slice preserves attributes
        result = slice_range(var, 2, 4)
        
        if (result%units /= var%units) then
            test_passed = .false.
            write(error_unit,'(A)') "Units not preserved in slice"
        end if
        
        if (result%long_name /= var%long_name) then
            test_passed = .false.
            write(error_unit,'(A)') "Long name not preserved in slice"
        end if
        
        call finalize_variable(var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Slice attributes preservation"
        else
            write(*,'(A)') "FAIL: Slice attributes preservation"
        end if
    end subroutine test_slice_attributes_preservation
    
end program test_slicing_operations