program test_enhanced_selection
    use fortarray
    use fortarray_types
    use fortarray_constructors, only: new_array
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    logical :: all_passed = .true.
    
    write(*,'(A)') "Testing enhanced selection methods..."
    
    call test_nearest_neighbor_selection()
    call test_interpolation_selection()
    call test_string_coordinate_selection()
    call test_datetime_coordinate_selection()
    call test_multi_coordinate_selection()
    call test_method_choice_selection()
    call test_performance_enhanced_selection()
    call test_error_handling_enhanced()
    
    if (all_passed) then
        print *, "SUCCESS: All enhanced selection tests passed!"
    else
        error stop "FAILURE: Some enhanced selection tests failed!"
    end if
    
contains
    
    subroutine test_nearest_neighbor_selection()
        type(fortarray_t) :: arr, coord, result
        real(real64), dimension(10) :: data_1d, coord_values
        integer :: i
        
        print *, "Testing nearest-neighbor selection..."
        
        ! Create test data: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        do i = 1, 10
            data_1d(i) = real(i, real64)
            coord_values(i) = real(i * 2, real64)  ! [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        coord = new_array(coord_values, dim_names=["x"])
        
        ! Add coordinate to array
        if (allocated(arr%coords)) deallocate(arr%coords)
        allocate(arr%coords(1))
        arr%coords(1) = coord%coords(1)
        
        ! Test 1: Nearest neighbor to 7.0 (should find index 4 with coord value 8)
        print *, "  Test 1: sel_nearest for value 7.0"
        result = arr%sel_nearest("x", 7.0_real64, "nearest")
        
        if (.not. result%initialized) then
            print *, "    FAIL: sel_nearest result not initialized"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 1) then
            print *, "    FAIL: sel_nearest wrong size"
            all_passed = .false.
            return
        end if
        
        ! Nearest to 7 should be coordinate value 8 (index 4), so data value should be 4
        if (allocated(result%data%values_r64)) then
            if (abs(result%data%values_r64(1) - 4.0_real64) > 1e-10) then
                print *, "    FAIL: sel_nearest wrong value"
                print *, "      Expected: 4.0"
                print *, "      Got:", result%data%values_r64(1)
                all_passed = .false.
                return
            end if
        else
            print *, "    FAIL: sel_nearest no data allocated"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: nearest-neighbor selection"
        
        ! Test 2: Linear method (should behave same as nearest for now)
        print *, "  Test 2: sel_nearest with linear method"
        result = arr%sel_nearest("x", 11.0_real64, "linear")
        
        if (result%initialized .and. result%n_elements == 1) then
            print *, "    PASS: linear method working"
        else
            print *, "    FAIL: linear method failed"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(coord)
        call finalize_variable(result)
        
    end subroutine test_nearest_neighbor_selection
    
    subroutine test_interpolation_selection()
        type(fortarray_t) :: arr, coord, result
        real(real64), dimension(5) :: data_1d, coord_values
        integer :: i
        
        print *, "Testing interpolation-based selection..."
        
        ! Create test data: [10, 20, 30, 40, 50] with coordinates [1, 2, 3, 4, 5]
        do i = 1, 5
            data_1d(i) = real(i * 10, real64)
            coord_values(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        coord = new_array(coord_values, dim_names=["x"])
        
        ! Add coordinate to array
        if (allocated(arr%coords)) deallocate(arr%coords)
        allocate(arr%coords(1))
        arr%coords(1) = coord%coords(1)
        
        ! Test 1: Linear interpolation between points
        print *, "  Test 1: sel_interp for value 2.5"
        result = arr%sel_interp("x", 2.5_real64, "linear")
        
        if (.not. result%initialized) then
            print *, "    FAIL: sel_interp result not initialized"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 1) then
            print *, "    FAIL: sel_interp wrong size"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: interpolation selection (basic functionality)"
        
        ! Test 2: Exact match should work
        print *, "  Test 2: sel_interp for exact match (3.0)"
        result = arr%sel_interp("x", 3.0_real64, "linear")
        
        if (result%initialized .and. result%n_elements == 1) then
            if (allocated(result%data%values_r64)) then
                if (abs(result%data%values_r64(1) - 30.0_real64) < 1e-10) then
                    print *, "    PASS: exact match interpolation"
                else
                    print *, "    FAIL: exact match wrong value"
                    all_passed = .false.
                end if
            else
                print *, "    FAIL: exact match no data"
                all_passed = .false.
            end if
        else
            print *, "    FAIL: exact match failed"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(coord)
        call finalize_variable(result)
        
    end subroutine test_interpolation_selection
    
    subroutine test_string_coordinate_selection()
        type(fortarray_t) :: arr, result
        real(real64), dimension(3) :: data_1d
        character(len=10), dimension(3) :: string_coords
        integer :: i
        
        print *, "Testing string coordinate selection..."
        
        ! Create test data with string coordinates
        data_1d = [100.0_real64, 200.0_real64, 300.0_real64]
        string_coords = ["jan", "feb", "mar"]
        
        arr = new_array(data_1d, dim_names=["month"])
        
        ! Manually set up string coordinate (simplified for test)
        if (allocated(arr%coords)) deallocate(arr%coords)
        allocate(arr%coords(1))
        arr%coords(1)%initialized = .true.
        arr%coords(1)%name = "month"
        arr%coords(1)%length = 3
        arr%coords(1)%dtype = DTYPE_CHAR
        allocate(character(len=10) :: arr%coords(1)%values_char(3))
        arr%coords(1)%values_char = string_coords
        
        ! Test 1: Select by string value
        print *, "  Test 1: sel_string for 'feb'"
        result = arr%sel_string("month", "feb")
        
        if (.not. result%initialized) then
            print *, "    FAIL: sel_string result not initialized"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 1) then
            print *, "    FAIL: sel_string wrong size"
            all_passed = .false.
            return
        end if
        
        if (allocated(result%data%values_r64)) then
            if (abs(result%data%values_r64(1) - 200.0_real64) < 1e-10) then
                print *, "    PASS: string coordinate selection"
            else
                print *, "    FAIL: string selection wrong value"
                all_passed = .false.
            end if
        else
            print *, "    FAIL: string selection no data"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_string_coordinate_selection
    
    subroutine test_datetime_coordinate_selection()
        type(fortarray_t) :: arr, result
        real(real64), dimension(3) :: data_1d
        character(len=20), dimension(3) :: datetime_coords
        
        print *, "Testing datetime coordinate selection..."
        
        ! Create test data with datetime-like coordinates
        data_1d = [10.0_real64, 20.0_real64, 30.0_real64]
        datetime_coords = ["2023-01-01", "2023-01-02", "2023-01-03"]
        
        arr = new_array(data_1d, dim_names=["time"])
        
        ! Set up string coordinate for datetime test
        if (allocated(arr%coords)) deallocate(arr%coords)
        allocate(arr%coords(1))
        arr%coords(1)%initialized = .true.
        arr%coords(1)%name = "time"
        arr%coords(1)%length = 3
        arr%coords(1)%dtype = DTYPE_CHAR
        allocate(character(len=20) :: arr%coords(1)%values_char(3))
        arr%coords(1)%values_char = datetime_coords
        
        ! Test 1: Datetime selection (currently uses string matching)
        print *, "  Test 1: sel_datetime for '2023-01-02'"
        result = arr%sel_datetime("time", "2023-01-02")
        
        if (.not. result%initialized) then
            print *, "    FAIL: sel_datetime result not initialized"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 1) then
            print *, "    FAIL: sel_datetime wrong size"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: datetime coordinate selection (placeholder)"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_datetime_coordinate_selection
    
    subroutine test_multi_coordinate_selection()
        type(fortarray_t) :: arr, result
        real(real64), dimension(6) :: data_1d, x_coords, y_coords
        character(len=10), dimension(2) :: coord_names
        real(real64), dimension(2) :: coord_values
        integer :: i
        
        print *, "Testing multi-coordinate selection..."
        
        ! Create test data with multiple coordinates
        do i = 1, 6
            data_1d(i) = real(i * 100, real64)
            x_coords(i) = real(i, real64)
            y_coords(i) = real(i * 2, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x", "y"])
        
        ! Set up coordinates
        if (allocated(arr%coords)) deallocate(arr%coords)
        allocate(arr%coords(2))
        
        ! X coordinate
        arr%coords(1)%initialized = .true.
        arr%coords(1)%name = "x"
        arr%coords(1)%length = 6
        arr%coords(1)%dtype = DTYPE_REAL64
        allocate(arr%coords(1)%values_r64(6))
        arr%coords(1)%values_r64 = x_coords
        
        ! Y coordinate
        arr%coords(2)%initialized = .true.
        arr%coords(2)%name = "y"
        arr%coords(2)%length = 6
        arr%coords(2)%dtype = DTYPE_REAL64
        allocate(arr%coords(2)%values_r64(6))
        arr%coords(2)%values_r64 = y_coords
        
        ! Test 1: Multi-coordinate exact selection
        print *, "  Test 1: sel_multi with exact method"
        coord_names = ["x", "y"]
        coord_values = [3.0_real64, 6.0_real64]
        result = arr%sel_multi(coord_names, coord_values, "exact")
        
        if (result%initialized) then
            print *, "    PASS: multi-coordinate selection (basic functionality)"
        else
            print *, "    FAIL: multi-coordinate selection failed"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_multi_coordinate_selection
    
    subroutine test_method_choice_selection()
        type(fortarray_t) :: arr, coord, result
        real(real64), dimension(5) :: data_1d, coord_values
        integer :: i
        
        print *, "Testing generic method choice selection..."
        
        ! Create test data
        do i = 1, 5
            data_1d(i) = real(i * 10, real64)
            coord_values(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        coord = new_array(coord_values, dim_names=["x"])
        
        ! Add coordinate to array
        if (allocated(arr%coords)) deallocate(arr%coords)
        allocate(arr%coords(1))
        arr%coords(1) = coord%coords(1)
        
        ! Test 1: Exact method
        print *, "  Test 1: sel_method with exact method"
        result = arr%sel_method("x", 3.0_real64, "exact")
        
        if (result%initialized .and. result%n_elements == 1) then
            print *, "    PASS: exact method selection"
        else
            print *, "    FAIL: exact method selection"
            all_passed = .false.
        end if
        
        ! Test 2: Nearest method
        print *, "  Test 2: sel_method with nearest method"
        result = arr%sel_method("x", 2.7_real64, "nearest")
        
        if (result%initialized .and. result%n_elements == 1) then
            print *, "    PASS: nearest method selection"
        else
            print *, "    FAIL: nearest method selection"
            all_passed = .false.
        end if
        
        ! Test 3: Interpolate method
        print *, "  Test 3: sel_method with interpolate method"
        result = arr%sel_method("x", 2.5_real64, "interpolate")
        
        if (result%initialized .and. result%n_elements == 1) then
            print *, "    PASS: interpolate method selection"
        else
            print *, "    FAIL: interpolate method selection"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(coord)
        call finalize_variable(result)
        
    end subroutine test_method_choice_selection
    
    subroutine test_performance_enhanced_selection()
        type(fortarray_t) :: arr, coord, result
        real(real64), dimension(1000) :: data_1d, coord_values
        real(real64) :: t_start, t_end
        integer :: i
        
        print *, "Testing performance of enhanced selection methods..."
        
        ! Create large test data
        do i = 1, 1000
            data_1d(i) = real(i, real64)
            coord_values(i) = real(i * 0.1, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        coord = new_array(coord_values, dim_names=["x"])
        
        ! Add coordinate to array
        if (allocated(arr%coords)) deallocate(arr%coords)
        allocate(arr%coords(1))
        arr%coords(1) = coord%coords(1)
        
        ! Test 1: Performance test for nearest neighbor
        print *, "  Test 1: Performance test - nearest neighbor on 1000 elements"
        call cpu_time(t_start)
        result = arr%sel_nearest("x", 50.0_real64, "nearest")
        call cpu_time(t_end)
        
        if (result%initialized .and. result%n_elements == 1) then
            print *, "    PASS: Performance test completed in", (t_end - t_start), "seconds"
        else
            print *, "    FAIL: Performance test failed"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(coord)
        call finalize_variable(result)
        
    end subroutine test_performance_enhanced_selection
    
    subroutine test_error_handling_enhanced()
        type(fortarray_t) :: arr, result
        real(real64), dimension(3) :: data_1d
        
        print *, "Testing error handling for enhanced selection..."
        
        data_1d = [1.0_real64, 2.0_real64, 3.0_real64]
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: Non-existent coordinate
        print *, "  Test 1: sel_nearest with non-existent coordinate"
        result = arr%sel_nearest("nonexistent", 1.0_real64, "nearest")
        
        if (.not. result%initialized) then
            print *, "    PASS: Proper error handling for non-existent coordinate"
        else
            print *, "    FAIL: Should have failed for non-existent coordinate"
            all_passed = .false.
        end if
        
        ! Test 2: Invalid method
        print *, "  Test 2: sel_method with invalid method"
        result = arr%sel_method("x", 1.0_real64, "invalid_method")
        
        if (.not. result%initialized) then
            print *, "    PASS: Proper error handling for invalid method"
        else
            print *, "    FAIL: Should have failed for invalid method"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_error_handling_enhanced
    
end program test_enhanced_selection