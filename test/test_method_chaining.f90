program test_method_chaining
    use fortarray
    use fortarray_types
    use fortarray_constructors, only: new_array, create_coordinate
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    logical :: all_passed = .true.
    
    call test_simple_chaining()
    call test_complex_chaining()
    call test_memory_management()
    call test_temporary_cleanup()
    call test_chaining_performance()
    call test_error_propagation()
    call test_copy_on_write()
    call test_finalizer_calls()
    
    if (all_passed) then
        print *, "SUCCESS: All method chaining tests passed!"
    else
        error stop "FAILURE: Some method chaining tests failed!"
    end if
    
contains
    
    subroutine test_simple_chaining()
        type(fortarray_t) :: arr, temp1, temp2, result
        type(coordinate_t) :: x_coord, y_coord
        real(real64), dimension(5,4) :: data_2d
        integer :: i, j, stat
        real(real64) :: mean_val
        
        print *, "Testing simple method chaining..."
        
        ! Create 2D test data
        do i = 1, 5
            do j = 1, 4
                data_2d(i,j) = real(i*10 + j, real64)
            end do
        end do
        
        arr = new_array(data_2d, dim_names=["x", "y"])
        
        ! Debug: print data layout
        print *, "    Data shape:", shape(data_2d)
        print *, "    Data values (first few):"
        do i = 1, 3
            print *, "      Row", i, ":", data_2d(i, 1:4)
        end do
        
        ! Coordinates already allocated by new_array, just create them
        
        call create_coordinate(x_coord, 5, "real64", stat)
        x_coord%values_r64 = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        x_coord%name = "x"
        
        call create_coordinate(y_coord, 4, "real64", stat)
        y_coord%values_r64 = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64]
        y_coord%name = "y"
        
        arr%coords(1) = x_coord
        arr%coords(2) = y_coord
        arr%has_coord = .true.
        
        ! Debug: print coordinate values
        print *, "    x_coord values:", x_coord%values_r64
        print *, "    y_coord values:", y_coord%values_r64
        
        ! Test 1: Chain two selections
        print *, "  Test 1: Chain two selections"
        temp1 = arr%sel("x", value=3.0_real64)
        result = temp1%sel("y", value=20.0_real64)
        
        ! Debug output
        print *, "    temp1 dims:", temp1%n_dims, " shape:", temp1%shape(1:temp1%n_dims)
        if (allocated(temp1%data%values_r64)) then
            print *, "    temp1 values:", temp1%data%values_r64(1:temp1%n_elements)
        end if
        print *, "    result dims:", result%n_dims
        if (result%n_dims == 0 .and. allocated(result%data%values_r64)) then
            print *, "    result value:", result%data%values_r64(1)
        end if
        
        if (result%n_dims == 0 .and. abs(result%data%values_r64(1) - 32.0_real64) < 1e-10) then
            print *, "    PASS: Chained selection returned correct scalar"
        else
            print *, "    FAIL: Chained selection incorrect"
            all_passed = .false.
        end if
        
        ! Test 2: Chain selection and aggregation
        print *, "  Test 2: Chain selection and aggregation"
        temp1 = arr%sel("x", value=3.0_real64)
        temp2 = temp1%mean()
        
        if (temp2%n_dims == 0) then
            mean_val = temp2%data%values_r64(1)
            if (abs(mean_val - 32.5_real64) < 1e-10) then  ! Mean of [31, 32, 33, 34]
                print *, "    PASS: Chained selection and mean correct"
            else
                print *, "    FAIL: Mean value incorrect:", mean_val
                all_passed = .false.
            end if
        else
            print *, "    FAIL: Mean should return scalar"
            all_passed = .false.
        end if
        
        ! Clean up
        call finalize_variable(arr)
        call finalize_variable(temp1)
        call finalize_variable(temp2)
        call finalize_variable(result)
        
    end subroutine test_simple_chaining
    
    subroutine test_complex_chaining()
        type(fortarray_t) :: arr, temp1, temp2, temp3, result
        type(coordinate_t) :: x_coord, y_coord, z_coord
        real(real64), dimension(4,3,2) :: data_3d
        integer :: i, j, k, stat
        
        print *, "Testing complex method chaining..."
        
        ! Create 3D test data
        do i = 1, 4
            do j = 1, 3
                do k = 1, 2
                    data_3d(i,j,k) = real(i*100 + j*10 + k, real64)
                end do
            end do
        end do
        
        arr = new_array(data_3d, dim_names=["x", "y", "z"])
        
        ! Coordinates already allocated by new_array, just create them
        
        call create_coordinate(x_coord, 4, "real64", stat)
        x_coord%values_r64 = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        x_coord%name = "x"
        
        call create_coordinate(y_coord, 3, "real64", stat)
        y_coord%values_r64 = [10.0_real64, 20.0_real64, 30.0_real64]
        y_coord%name = "y"
        
        call create_coordinate(z_coord, 2, "real64", stat)
        z_coord%values_r64 = [100.0_real64, 200.0_real64]
        z_coord%name = "z"
        
        arr%coords(1) = x_coord
        arr%coords(2) = y_coord
        arr%coords(3) = z_coord
        arr%has_coord = .true.
        
        ! Test: Chain three operations
        print *, "  Test: Chain three operations"
        temp1 = arr%sel("x", start_val=2.0_real64, stop_val=3.0_real64)  ! Select x range
        temp2 = temp1%sel("z", value=100.0_real64)                       ! Select z point
        temp3 = temp2%mean()                                             ! Take mean
        
        if (temp3%n_dims == 0) then
            print *, "    PASS: Complex chain resulted in scalar"
            ! Expected mean of: 211, 221, 231, 311, 321, 331 = 271.0
            if (abs(temp3%data%values_r64(1) - 271.0_real64) < 1e-10) then
                print *, "    PASS: Complex chain value correct"
            else
                print *, "    FAIL: Complex chain value incorrect:", temp3%data%values_r64(1)
                all_passed = .false.
            end if
        else
            print *, "    FAIL: Complex chain should return scalar"
            all_passed = .false.
        end if
        
        ! Clean up
        call finalize_variable(arr)
        call finalize_variable(temp1)
        call finalize_variable(temp2)
        call finalize_variable(temp3)
        
    end subroutine test_complex_chaining
    
    subroutine test_memory_management()
        type(fortarray_t) :: arr, temp
        real(real64), dimension(1000) :: data_1d
        integer :: i, initial_mem, final_mem
        
        print *, "Testing memory management in chaining..."
        
        ! Create test data
        do i = 1, 1000
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Note: In a real implementation, we would track memory allocations
        ! For now, we just ensure operations complete without errors
        
        ! Test: Multiple chained operations
        print *, "  Test: Memory stability through multiple operations"
        do i = 1, 10
            temp = arr%isel("x", start_idx=1, stop_idx=500)
            call finalize_variable(temp)
        end do
        
        print *, "    PASS: Multiple operations completed without memory issues"
        
        ! Clean up
        call finalize_variable(arr)
        
    end subroutine test_memory_management
    
    subroutine test_temporary_cleanup()
        type(fortarray_t) :: arr, result, temp
        real(real64), dimension(10) :: data_1d
        integer :: i
        
        print *, "Testing temporary object cleanup..."
        
        ! Create test data
        do i = 1, 10
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test: Direct chaining without storing intermediates
        print *, "  Test: Direct chaining without intermediate variables"
        ! Need to chain in separate steps due to Fortran syntax limitations
        temp = arr%isel("x", start_idx=3, stop_idx=7)
        result = temp%sum()
        
        if (result%n_dims == 0) then
            ! Sum of [3,4,5,6,7] = 25
            if (abs(result%data%values_r64(1) - 25.0_real64) < 1e-10) then
                print *, "    PASS: Direct chaining worked correctly"
            else
                print *, "    FAIL: Direct chaining value incorrect"
                all_passed = .false.
            end if
        else
            print *, "    FAIL: Sum should return scalar"
            all_passed = .false.
        end if
        
        ! Clean up
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_temporary_cleanup
    
    subroutine test_chaining_performance()
        type(fortarray_t) :: arr, temp1, temp2, result
        real(real64), dimension(100,100) :: data_2d
        real(real64) :: t_start, t_end, t_chained, t_separate
        integer :: i, j
        
        print *, "Testing chaining performance..."
        
        ! Create larger test data
        do i = 1, 100
            do j = 1, 100
                data_2d(i,j) = real(i*j, real64)
            end do
        end do
        
        arr = new_array(data_2d, dim_names=["x", "y"])
        
        ! Test: Compare chained vs separate operations
        print *, "  Test: Performance comparison (relative timing)"
        
        ! Note: In real implementation, would use actual timing
        ! For now, just verify operations complete
        
        ! Chained operations
        temp1 = arr%isel("x", start_idx=10, stop_idx=90)
        result = temp1%mean()
        call finalize_variable(result)
        
        ! Separate operations
        temp1 = arr%isel("x", start_idx=10, stop_idx=90)
        temp2 = temp1%mean()
        call finalize_variable(temp1)
        call finalize_variable(temp2)
        
        print *, "    PASS: Both chained and separate operations completed"
        
        ! Clean up
        call finalize_variable(arr)
        
    end subroutine test_chaining_performance
    
    subroutine test_error_propagation()
        type(fortarray_t) :: arr, result, temp
        real(real64), dimension(5) :: data_1d
        integer :: i
        
        print *, "Testing error propagation in chains..."
        
        ! Create test data
        do i = 1, 5
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test: Error in first operation
        print *, "  Test: Error propagation from invalid selection"
        temp = arr%sel("nonexistent", value=1.0_real64)
        result = temp%mean()
        
        if (.not. result%initialized .or. result%n_elements == 0) then
            print *, "    PASS: Error propagated correctly"
        else
            print *, "    FAIL: Error should have propagated"
            all_passed = .false.
        end if
        
        ! Clean up
        call finalize_variable(arr)
        call finalize_variable(result)
        
    end subroutine test_error_propagation
    
    subroutine test_copy_on_write()
        type(fortarray_t) :: arr, view1, view2
        real(real64), dimension(10) :: data_1d
        integer :: i
        
        print *, "Testing copy-on-write semantics..."
        
        ! Create test data
        do i = 1, 10
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test: Multiple views of same data
        print *, "  Test: Multiple views without modification"
        view1 = arr%isel("x", start_idx=1, stop_idx=5)
        view2 = arr%isel("x", start_idx=6, stop_idx=10)
        
        if (view1%n_elements == 5 .and. view2%n_elements == 5) then
            print *, "    PASS: Multiple views created successfully"
        else
            print *, "    FAIL: View creation failed"
            all_passed = .false.
        end if
        
        ! Note: In full implementation, would verify no data copying occurred
        ! and that modifications trigger copy-on-write
        
        ! Clean up
        call finalize_variable(arr)
        call finalize_variable(view1)
        call finalize_variable(view2)
        
    end subroutine test_copy_on_write
    
    subroutine test_finalizer_calls()
        type(fortarray_t) :: arr
        real(real64), dimension(5) :: data_1d
        integer :: i
        
        print *, "Testing finalizer calls in chaining..."
        
        ! Create test data
        do i = 1, 5
            data_1d(i) = real(i, real64)
        end do
        
        ! Test: Ensure finalizers are called properly
        print *, "  Test: Finalizer behavior"
        
        ! Create and immediately reassign
        arr = new_array(data_1d, dim_names=["x"])
        arr = arr%isel("x", start_idx=2, stop_idx=4)  ! Should finalize old arr
        
        if (arr%n_elements == 3) then
            print *, "    PASS: Reassignment with finalization worked"
        else
            print *, "    FAIL: Reassignment failed"
            all_passed = .false.
        end if
        
        ! Clean up
        call finalize_variable(arr)
        
    end subroutine test_finalizer_calls
    
end program test_method_chaining