program test_index_selection_enhancements
    use fortarray
    use fortarray_types
    use fortarray_constructors, only: new_array, create_coordinate
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    logical :: all_passed = .true.
    
    write(*,'(A)') "Testing index selection enhancements..."
    
    call test_negative_indexing()
    call test_fancy_indexing()
    call test_step_based_indexing()
    call test_boolean_mask_integration()
    call test_nd_index_optimization()
    call test_memory_efficiency()
    
    if (all_passed) then
        print *, "SUCCESS: All index selection enhancement tests passed!"
    else
        error stop "FAILURE: Some index selection enhancement tests failed!"
    end if
    
contains
    
    subroutine test_negative_indexing()
        type(fortarray_t) :: arr, result
        type(coordinate_t) :: x_coord
        real(real64), dimension(10) :: data_1d, coord_values
        integer :: i, stat
        
        print *, "Testing negative indexing (Python-style)..."
        
        ! Create test data: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        do i = 1, 10
            data_1d(i) = real(i, real64)
            coord_values(i) = real(i * 10, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Create and set coordinate
        call create_coordinate(x_coord, 10, "real64", stat)
        x_coord%values_r64 = coord_values
        x_coord%name = "x"
        arr%coords(1) = x_coord
        arr%has_coord(1) = .true.
        
        ! Test 1: Negative index -1 (last element)
        print *, "  Test 1: isel with index=-1 (last element)"
        result = arr%isel("x", index=-1)
        
        if (.not. result%initialized) then
            print *, "    FAIL: Negative indexing not implemented"
            all_passed = .false.
            return
        end if
        
        if (result%n_dims == 0 .and. abs(result%data%values_r64(1) - 10.0_real64) < 1e-10) then
            print *, "    PASS: Negative index -1 returns last element"
        else
            print *, "    FAIL: Negative index -1 wrong value"
            all_passed = .false.
        end if
        
        ! Test 2: Negative index -3 (third from last)
        print *, "  Test 2: isel with index=-3 (third from last)"
        result = arr%isel("x", index=-3)
        
        if (result%n_dims == 0 .and. abs(result%data%values_r64(1) - 8.0_real64) < 1e-10) then
            print *, "    PASS: Negative index -3 returns correct element"
        else
            print *, "    FAIL: Negative index -3 wrong value"
            all_passed = .false.
        end if
        
        ! Test 3: Negative range selection
        print *, "  Test 3: isel with negative range [-3:-1]"
        result = arr%isel("x", start_idx=-3, stop_idx=-1)
        
        if (result%n_elements == 3) then
            print *, "    PASS: Negative range selection"
        else
            print *, "    FAIL: Negative range selection wrong size"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_negative_indexing
    
    subroutine test_fancy_indexing()
        type(fortarray_t) :: arr, result
        real(real64), dimension(10) :: data_1d
        integer, dimension(4) :: indices
        integer :: i
        
        print *, "Testing fancy indexing with integer arrays..."
        
        ! Create test data
        do i = 1, 10
            data_1d(i) = real(i * 10, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: Select specific indices [2, 5, 7, 3]
        print *, "  Test 1: isel_indices with [2, 5, 7, 3]"
        indices = [2, 5, 7, 3]
        result = arr%isel_indices("x", indices)
        
        if (.not. result%initialized) then
            print *, "    FAIL: Fancy indexing not working"
            all_passed = .false.
            return
        end if
        
        if (result%n_elements == 4) then
            if (abs(result%data%values_r64(1) - 20.0_real64) < 1e-10 .and. &
                abs(result%data%values_r64(2) - 50.0_real64) < 1e-10 .and. &
                abs(result%data%values_r64(3) - 70.0_real64) < 1e-10 .and. &
                abs(result%data%values_r64(4) - 30.0_real64) < 1e-10) then
                print *, "    PASS: Fancy indexing returns correct values"
            else
                print *, "    FAIL: Fancy indexing wrong values"
                all_passed = .false.
            end if
        else
            print *, "    FAIL: Fancy indexing wrong size"
            all_passed = .false.
        end if
        
        ! Test 2: Fancy indexing with negative indices
        print *, "  Test 2: isel_indices with negative indices [-1, -3, 1, -5]"
        indices = [-1, -3, 1, -5]
        result = arr%isel_indices("x", indices)
        
        if (result%n_elements == 4) then
            print *, "    PASS: Fancy indexing with negative indices"
        else
            print *, "    FAIL: Fancy indexing with negative indices"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_fancy_indexing
    
    subroutine test_step_based_indexing()
        type(fortarray_t) :: arr, result
        real(real64), dimension(20) :: data_1d
        integer :: i
        
        print *, "Testing step-based indexing..."
        
        ! Create test data
        do i = 1, 20
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: Every 2nd element
        print *, "  Test 1: isel with step=2 (every 2nd element)"
        result = arr%isel("x", start_idx=1, stop_idx=20, step_idx=2)
        
        if (result%n_elements == 10) then
            print *, "    PASS: Step-based selection size correct"
            if (abs(result%data%values_r64(1) - 1.0_real64) < 1e-10 .and. &
                abs(result%data%values_r64(2) - 3.0_real64) < 1e-10) then
                print *, "    PASS: Step-based selection values correct"
            else
                print *, "    FAIL: Step-based selection values wrong"
                all_passed = .false.
            end if
        else
            print *, "    FAIL: Step-based selection size wrong"
            all_passed = .false.
        end if
        
        ! Test 2: Every 3rd element starting from 2
        print *, "  Test 2: isel with start=2, step=3"
        result = arr%isel("x", start_idx=2, stop_idx=20, step_idx=3)
        
        if (result%n_elements == 7) then
            print *, "    PASS: Complex step selection"
        else
            print *, "    FAIL: Complex step selection wrong size"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_step_based_indexing
    
    subroutine test_boolean_mask_integration()
        type(fortarray_t) :: arr, mask, result
        real(real64), dimension(10) :: data_1d
        logical, dimension(10) :: mask_values
        integer :: i
        
        print *, "Testing boolean mask selection integration..."
        
        ! Create test data
        do i = 1, 10
            data_1d(i) = real(i, real64)
            mask_values(i) = mod(i, 2) == 0  ! Even indices
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Create boolean mask
        ! This needs integration with boolean indexing
        print *, "  Test 1: Boolean mask selection"
        print *, "    INFO: Boolean mask integration with isel not yet implemented"
        
        call finalize_variable(arr)
        
    end subroutine test_boolean_mask_integration
    
    subroutine test_nd_index_optimization()
        type(fortarray_t) :: arr, result
        real(real64), dimension(10,10,10) :: data_3d
        integer :: i, j, k, val
        
        print *, "Testing nD index optimization..."
        
        ! Create 3D test data
        val = 0
        do k = 1, 10
            do j = 1, 10
                do i = 1, 10
                    val = val + 1
                    data_3d(i,j,k) = real(val, real64)
                end do
            end do
        end do
        
        arr = new_array(data_3d, dim_names=["x", "y", "z"])
        
        ! Test 1: Multi-dimensional slicing
        print *, "  Test 1: 3D slicing performance"
        result = arr%isel("x", start_idx=3, stop_idx=7) ! Should be fast
        
        if (result%shape(1) == 5 .and. result%shape(2) == 10 .and. result%shape(3) == 10) then
            print *, "    PASS: 3D slicing dimensions correct"
        else
            print *, "    FAIL: 3D slicing dimensions wrong"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_nd_index_optimization
    
    subroutine test_memory_efficiency()
        type(fortarray_t) :: arr, result
        real(real64), dimension(1000) :: data_1d
        real(real64) :: t_start, t_end
        integer :: i
        
        print *, "Testing memory-efficient index operations..."
        
        ! Create large test data
        do i = 1, 1000
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: Large stride selection
        print *, "  Test 1: Large stride selection (every 10th element)"
        call cpu_time(t_start)
        result = arr%isel("x", start_idx=1, stop_idx=1000, step_idx=10)
        call cpu_time(t_end)
        
        if (result%n_elements == 100) then
            print *, "    PASS: Memory-efficient selection completed in", (t_end - t_start), "seconds"
        else
            print *, "    FAIL: Memory-efficient selection wrong size"
            all_passed = .false.
        end if
        
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_memory_efficiency
    
end program test_index_selection_enhancements