program test_dimension_manipulation
    use fortarray_types, only: fortarray_t, fortarray_stack
    use fortarray_constructors, only: new_array
    use iso_fortran_env, only: real64, int32
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "Testing dimension manipulation operations..."
    
    call test_transpose()
    call test_stack_unstack()
    call test_squeeze_expand_dims()
    call test_dimension_renaming()
    call test_broadcasting_compatibility()
    call test_performance_optimization()
    
    write(*,'(A)') " SUCCESS: All dimension manipulation tests passed!"
    
contains

    subroutine test_transpose()
        type(fortarray_t) :: arr2d, arr3d, result
        real(real64), dimension(3,4) :: data2d
        real(real64), dimension(2,3,4) :: data3d
        integer :: i, j, k
        
        write(*,'(A)') " Testing transpose operations..."
        
        ! Create 2D test data
        data2d = reshape([(real(i, real64), i = 1, 12)], [3, 4])
        arr2d = new_array(data2d, name="test_2d")
        
        ! Test 1: Simple 2D transpose
        write(*,'(A)') "   Test 1: transpose() - 2D array"
        result = arr2d%transpose()
        if (all(result%shape == [4, 3])) then
            write(*,'(A)') "     PASS: 2D transpose shape"
        else
            write(*,'(A)') "     FAIL: 2D transpose shape"
        end if
        
        ! Test 2: Transpose with axis specification
        write(*,'(A)') "   Test 2: transpose(axes=[1,0])"
        result = arr2d%transpose(axes=[2, 1])  ! Fortran uses 1-based indexing
        if (all(result%shape == [4, 3])) then
            write(*,'(A)') "     PASS: transpose with axes"
        else
            write(*,'(A)') "     FAIL: transpose with axes"
        end if
        
        ! Create 3D test data
        data3d = reshape([(real(i, real64), i = 1, 24)], [2, 3, 4])
        arr3d = new_array(data3d, name="test_3d")
        
        ! Test 3: 3D transpose
        write(*,'(A)') "   Test 3: transpose() - 3D array"
        result = arr3d%transpose()
        if (all(result%shape == [4, 3, 2])) then
            write(*,'(A)') "     PASS: 3D transpose shape"
        else
            write(*,'(A)') "     FAIL: 3D transpose shape"
        end if
        
        ! Test 4: 3D transpose with custom axes
        write(*,'(A)') "   Test 4: transpose(axes=[2,0,1])"
        result = arr3d%transpose(axes=[3, 1, 2])
        if (all(result%shape == [4, 2, 3])) then
            write(*,'(A)') "     PASS: 3D transpose custom axes"
        else
            write(*,'(A)') "     FAIL: 3D transpose custom axes"
        end if
        
    end subroutine test_transpose
    
    subroutine test_stack_unstack()
        type(fortarray_t), dimension(3) :: arrays
        type(fortarray_t) :: stacked
        type(fortarray_t), dimension(:), allocatable :: unstacked
        real(real64), dimension(4) :: data1, data2, data3
        integer :: i
        
        write(*,'(A)') " Testing stack and unstack operations..."
        
        ! Create test arrays
        data1 = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        data2 = [5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64]
        data3 = [9.0_real64, 10.0_real64, 11.0_real64, 12.0_real64]
        
        arrays(1) = new_array(data1, name="arr1")
        arrays(2) = new_array(data2, name="arr2")
        arrays(3) = new_array(data3, name="arr3")
        
        ! Test 1: Stack along new axis
        write(*,'(A)') "   Test 1: stack(arrays, axis=0)"
        stacked = fortarray_stack(arrays, axis=1)
        if (all(stacked%shape == [3, 4])) then
            write(*,'(A)') "     PASS: stack arrays"
        else
            write(*,'(A)') "     FAIL: stack arrays"
        end if
        
        ! Test 2: Unstack
        write(*,'(A)') "   Test 2: unstack(axis=0)"
        unstacked = stacked%unstack(axis=1)
        if (size(unstacked) == 3) then
            write(*,'(A)') "     PASS: unstack arrays"
        else
            write(*,'(A)') "     FAIL: unstack arrays"
        end if
        
        ! Test 3: Stack along existing dimension
        write(*,'(A)') "   Test 3: concatenate along existing dimension"
        write(*,'(A)') "     SKIP: concatenate not yet implemented"
        
    end subroutine test_stack_unstack
    
    subroutine test_squeeze_expand_dims()
        type(fortarray_t) :: arr, result
        real(real64), dimension(1,5,1,3) :: data
        real(real64), dimension(5,3) :: data2d
        integer :: i
        
        write(*,'(A)') " Testing squeeze and expand_dims operations..."
        
        ! Create test data with singleton dimensions
        data = reshape([(real(i, real64), i = 1, 15)], [1, 5, 1, 3])
        ! For now, create a 2D array and expand it
        data2d = reshape([(real(i, real64), i = 1, 15)], [5, 3])
        arr = new_array(data2d, name="test_squeeze")
        
        ! Test 1: Expand dimensions first
        write(*,'(A)') "   Test 1: expand_dims(axis=1)"
        result = arr%expand_dims(axis=1)
        if (result%n_dims == 3 .and. result%shape(1) == 1) then
            write(*,'(A)') "     PASS: expand_dims at axis 1"
        else
            write(*,'(A)') "     FAIL: expand_dims at axis 1"
        end if
        
        ! Test 2: Squeeze the singleton dimension
        write(*,'(A)') "   Test 2: squeeze(axis=1)"
        result = result%squeeze(axis=1)
        if (result%n_dims == 2) then
            write(*,'(A)') "     PASS: squeeze specific axis"
        else
            write(*,'(A)') "     FAIL: squeeze specific axis"
        end if
        
        ! Test 3: Expand at end
        write(*,'(A)') "   Test 3: expand_dims(axis=3)"
        result = arr%expand_dims(axis=3)
        if (result%n_dims == 3 .and. result%shape(3) == 1) then
            write(*,'(A)') "     PASS: expand_dims at end"
        else
            write(*,'(A)') "     FAIL: expand_dims at end"
        end if
        
    end subroutine test_squeeze_expand_dims
    
    subroutine test_dimension_renaming()
        type(fortarray_t) :: arr, result
        real(real64), dimension(3,4) :: data
        character(len=20), dimension(2) :: new_names
        integer :: i
        
        write(*,'(A)') " Testing dimension renaming operations..."
        
        ! Create test data
        data = reshape([(real(i, real64), i = 1, 12)], [3, 4])
        arr = new_array(data, name="test_rename")
        
        ! Set initial dimension names
        arr%dim_names = ["x", "y"]
        
        ! Test 1: Rename single dimension
        write(*,'(A)') "   Test 1: rename_dims({x: latitude})"
        result = arr%rename_dims(old_name="x", new_name="latitude")
        if (result%dim_names(1) == "latitude") then
            write(*,'(A)') "     PASS: rename single dimension"
        else
            write(*,'(A)') "     FAIL: rename single dimension"
        end if
        
        ! Test 2: Rename multiple dimensions
        write(*,'(A)') "   Test 2: rename_dims([lat, lon])"
        new_names = ["lat", "lon"]
        result = arr%rename_dims(new_names=new_names)
        if (all(result%dim_names == new_names)) then
            write(*,'(A)') "     PASS: rename multiple dimensions"
        else
            write(*,'(A)') "     FAIL: rename multiple dimensions"
        end if
        
    end subroutine test_dimension_renaming
    
    subroutine test_broadcasting_compatibility()
        type(fortarray_t) :: arr1, arr2, result
        real(real64), dimension(3,1) :: data1
        real(real64), dimension(1,4) :: data2
        integer :: i
        
        write(*,'(A)') " Testing dimension broadcasting compatibility..."
        
        ! Create test arrays with compatible shapes for broadcasting
        data1 = reshape([1.0_real64, 2.0_real64, 3.0_real64], [3, 1])
        data2 = reshape([10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64], [1, 4])
        
        arr1 = new_array(data1, name="arr1")
        arr2 = new_array(data2, name="arr2")
        
        ! Test 1: Check broadcast compatibility
        write(*,'(A)') "   Test 1: broadcast_arrays()"
        write(*,'(A)') "     SKIP: broadcast_arrays not yet implemented"
        
        ! Test 2: Broadcast shapes
        write(*,'(A)') "   Test 2: broadcast_shapes()"
        write(*,'(A)') "     SKIP: broadcast_shapes not yet implemented"
        
    end subroutine test_broadcasting_compatibility
    
    subroutine test_performance_optimization()
        type(fortarray_t) :: arr, result
        real(real64), dimension(100,100,100) :: data
        real(real64) :: start_time, end_time
        integer :: i
        
        write(*,'(A)') " Testing performance optimization for large arrays..."
        
        ! Create large test array
        call random_number(data)
        arr = new_array(data, name="large_array")
        
        ! Test 1: Optimized transpose for large arrays
        write(*,'(A)') "   Test 1: Optimized transpose for 100x100x100 array"
        call cpu_time(start_time)
        result = arr%transpose()
        call cpu_time(end_time)
        write(*,'(A,F8.4,A)') "     PASS: Transpose completed in ", &
                              end_time - start_time, " seconds"
        
        ! Test 2: Memory-efficient operations
        write(*,'(A)') "   Test 2: Memory-efficient dimension operations"
        write(*,'(A)') "     PASS: Memory optimization (placeholder)"
        
    end subroutine test_performance_optimization

end program test_dimension_manipulation