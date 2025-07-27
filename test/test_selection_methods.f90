program test_selection_methods
    use fortarray
    use fortarray_types
    use fortarray_constructors, only: new_array, create_coordinate
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    logical :: all_passed = .true.
    
    call test_sel_point_exact()
    call test_sel_point_nearest()
    call test_sel_range()
    call test_isel_point()
    call test_isel_range()
    call test_method_chaining()
    call test_multidimensional_selection()
    call test_error_cases()
    
    if (all_passed) then
        print *, "SUCCESS: All selection method tests passed!"
    else
        error stop "FAILURE: Some selection method tests failed!"
    end if
    
contains
    
    subroutine test_sel_point_exact()
        type(fortarray_t) :: arr, result
        type(coordinate_t) :: time_coord
        real(real64) :: expected_value
        integer :: stat
        
        print *, "Testing sel_point with exact match..."
        
        ! Create test data with time coordinate
        arr = new_array([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                       dim_names=["time"])
        
        ! Allocate coordinate arrays
        allocate(arr%coords(1))
        allocate(arr%has_coord(1))
        
        ! Create time coordinate
        call create_coordinate(time_coord, 5, "real64", stat)
        time_coord%values_r64 = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        time_coord%name = "time"
        arr%coords(1) = time_coord
        arr%has_coord(1) = .true.
        
        ! Test exact selection
        result = arr%sel_point("time", 30.0_real64)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "FAIL: sel_point exact - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_dims /= 0) then
            print *, "FAIL: sel_point exact - should return scalar (0D)"
            all_passed = .false.
            return
        end if
        
        expected_value = 3.0_real64
        if (abs(result%data%values_r64(1) - expected_value) > 1e-10) then
            print *, "FAIL: sel_point exact - wrong value"
            print *, "  Expected:", expected_value
            print *, "  Got:", result%data%values_r64(1)
            all_passed = .false.
            return
        end if
        
        print *, "PASS: sel_point exact match"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_sel_point_exact
    
    subroutine test_sel_point_nearest()
        type(fortarray_t) :: arr, result
        type(coordinate_t) :: time_coord
        real(real64) :: expected_value
        integer :: stat
        
        print *, "Testing sel_point with nearest match..."
        
        ! Create test data
        arr = new_array([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                       dim_names=["time"])
        
        ! Allocate coordinate arrays
        allocate(arr%coords(1))
        allocate(arr%has_coord(1))
        
        ! Create time coordinate
        call create_coordinate(time_coord, 5, "real64", stat)
        time_coord%values_r64 = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        time_coord%name = "time"
        arr%coords(1) = time_coord
        arr%has_coord(1) = .true.
        
        ! Test nearest selection (value not in coordinate)
        result = arr%sel_point("time", 35.0_real64, method="nearest")
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "FAIL: sel_point nearest - no data allocated"
            all_passed = .false.
            return
        end if
        
        ! Should select 40.0 (index 4) which has value 4.0
        expected_value = 4.0_real64
        if (abs(result%data%values_r64(1) - expected_value) > 1e-10) then
            print *, "FAIL: sel_point nearest - wrong value"
            print *, "  Expected:", expected_value
            print *, "  Got:", result%data%values_r64(1)
            all_passed = .false.
            return
        end if
        
        print *, "PASS: sel_point nearest match"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_sel_point_nearest
    
    subroutine test_sel_range()
        type(fortarray_t) :: arr, result
        type(coordinate_t) :: time_coord
        integer :: stat, i
        
        print *, "Testing sel_range..."
        
        ! Create test data
        arr = new_array([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                       dim_names=["time"])
        
        ! Allocate coordinate arrays
        allocate(arr%coords(1))
        allocate(arr%has_coord(1))
        
        ! Create time coordinate
        call create_coordinate(time_coord, 5, "real64", stat)
        time_coord%values_r64 = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        time_coord%name = "time"
        arr%coords(1) = time_coord
        arr%has_coord(1) = .true.
        
        ! Test range selection
        result = arr%sel_range("time", 20.0_real64, 40.0_real64)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "FAIL: sel_range - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 3) then
            print *, "FAIL: sel_range - wrong size"
            print *, "  Expected: 3"
            print *, "  Got:", result%n_elements
            all_passed = .false.
            return
        end if
        
        ! Check values
        do i = 1, 3
            if (abs(result%data%values_r64(i) - real(i+1, real64)) > 1e-10) then
                print *, "FAIL: sel_range - wrong value at", i
                all_passed = .false.
                return
            end if
        end do
        
        ! Check coordinate preservation
        if (.not. result%has_coord(1)) then
            print *, "FAIL: sel_range - coordinate not preserved"
            all_passed = .false.
            return
        end if
        
        if (size(result%coords(1)%values_r64) /= 3) then
            print *, "FAIL: sel_range - coordinate wrong size"
            all_passed = .false.
            return
        end if
        
        print *, "PASS: sel_range"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_sel_range
    
    subroutine test_isel_point()
        type(fortarray_t) :: arr, result
        integer :: stat
        
        print *, "Testing isel_point..."
        
        ! Create test data
        arr = new_array([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                       dim_names=["x"])
        
        ! Test index selection
        result = arr%isel_point("x", 3)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "FAIL: isel_point - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_dims /= 0) then
            print *, "FAIL: isel_point - should return scalar"
            all_passed = .false.
            return
        end if
        
        if (abs(result%data%values_r64(1) - 3.0_real64) > 1e-10) then
            print *, "FAIL: isel_point - wrong value"
            all_passed = .false.
            return
        end if
        
        print *, "PASS: isel_point"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_isel_point
    
    subroutine test_isel_range()
        type(fortarray_t) :: arr, result
        integer :: i
        
        print *, "Testing isel_range..."
        
        ! Create test data
        arr = new_array([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
                       dim_names=["x"])
        
        ! Test index range selection
        result = arr%isel_range("x", 2, 4)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "FAIL: isel_range - no data allocated"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements /= 3) then
            print *, "FAIL: isel_range - wrong size"
            all_passed = .false.
            return
        end if
        
        ! Check values (should be 2.0, 3.0, 4.0)
        do i = 1, 3
            if (abs(result%data%values_r64(i) - real(i+1, real64)) > 1e-10) then
                print *, "FAIL: isel_range - wrong value at", i
                all_passed = .false.
                return
            end if
        end do
        
        print *, "PASS: isel_range"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_isel_range
    
    subroutine test_method_chaining()
        type(fortarray_t) :: arr, temp, result
        type(coordinate_t) :: x_coord, y_coord
        real(real64), dimension(5,4) :: data_2d
        integer :: i, j, stat
        
        print *, "Testing method chaining with selections..."
        
        ! Create 2D test data
        do i = 1, 5
            do j = 1, 4
                data_2d(i,j) = real(i*10 + j, real64)
            end do
        end do
        
        arr = new_array(data_2d, dim_names=["x", "y"])
        
        ! Create coordinates
        call create_coordinate(x_coord, 5, "real64", stat)
        x_coord%values_r64 = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        x_coord%name = "x"
        
        call create_coordinate(y_coord, 4, "real64", stat)  
        y_coord%values_r64 = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64]
        y_coord%name = "y"
        
        arr%coords(1) = x_coord
        arr%coords(2) = y_coord
        arr%has_coord = .true.
        
        ! Test chained selection: arr.sel(x=3.0).sel(y=20.0)
        temp = arr%sel_point("x", 3.0_real64)
        result = temp%sel_point("y", 20.0_real64)
        
        if (.not. allocated(result%data%values_r64)) then
            print *, "FAIL: method chaining - no data allocated"
            all_passed = .false.
            return
        end if
        
        ! Should get value at (3,2) = 32.0
        if (abs(result%data%values_r64(1) - 32.0_real64) > 1e-10) then
            print *, "FAIL: method chaining - wrong value"
            print *, "  Expected: 32.0"
            print *, "  Got:", result%data%values_r64(1)
            all_passed = .false.
            return
        end if
        
        print *, "PASS: method chaining"
        
        call finalize_variable(arr)
        call finalize_variable(temp)
        call finalize_variable(result)
        
    end subroutine test_method_chaining
    
    subroutine test_multidimensional_selection()
        type(fortarray_t) :: arr, result
        type(coordinate_t) :: x_coord, y_coord, z_coord
        real(real64), dimension(3,4,5) :: data_3d
        integer :: i, j, k, stat
        
        print *, "Testing multidimensional selection..."
        
        ! Create 3D test data
        do i = 1, 3
            do j = 1, 4
                do k = 1, 5
                    data_3d(i,j,k) = real(i*100 + j*10 + k, real64)
                end do
            end do
        end do
        
        arr = new_array(data_3d, dim_names=["x", "y", "z"])
        
        ! Create coordinates
        call create_coordinate(x_coord, 3, "real64", stat)
        x_coord%values_r64 = [1.0_real64, 2.0_real64, 3.0_real64]
        x_coord%name = "x"
        
        call create_coordinate(y_coord, 4, "real64", stat)
        y_coord%values_r64 = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64]
        y_coord%name = "y"
        
        call create_coordinate(z_coord, 5, "real64", stat)
        z_coord%values_r64 = [100.0_real64, 200.0_real64, 300.0_real64, 400.0_real64, 500.0_real64]
        z_coord%name = "z"
        
        arr%coords(1) = x_coord
        arr%coords(2) = y_coord  
        arr%coords(3) = z_coord
        arr%has_coord = .true.
        
        ! Test selecting on middle dimension
        result = arr%sel_point("y", 30.0_real64)
        
        if (result%n_dims /= 2) then
            print *, "FAIL: multidimensional selection - wrong dims"
            print *, "  Expected: 2"
            print *, "  Got:", result%n_dims
            all_passed = .false.
            return
        end if
        
        if (result%shape(1) /= 3 .or. result%shape(2) /= 5) then
            print *, "FAIL: multidimensional selection - wrong shape"
            all_passed = .false.
            return
        end if
        
        print *, "PASS: multidimensional selection"
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_multidimensional_selection
    
    subroutine test_error_cases()
        type(fortarray_t) :: arr, result
        
        print *, "Testing selection error cases..."
        
        ! Create test data without coordinates
        arr = new_array([1.0_real64, 2.0_real64, 3.0_real64], dim_names=["x"])
        
        ! Test selection on non-existent coordinate
        result = arr%sel_point("time", 10.0_real64)
        
        if (result%initialized .and. result%n_elements > 0) then
            print *, "FAIL: error case - should return empty for missing coord"
            all_passed = .false.
            return
        end if
        
        ! Test out of bounds index
        result = arr%isel_point("x", 10)
        
        if (result%initialized .and. result%n_elements > 0) then
            print *, "FAIL: error case - should return empty for out of bounds"
            all_passed = .false.
            return
        end if
        
        print *, "PASS: error cases"
        
        call finalize_variable(arr)
        
    end subroutine test_error_cases
    
end program test_selection_methods