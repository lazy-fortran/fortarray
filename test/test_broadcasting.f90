program test_broadcasting
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Broadcasting Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run tests
    call test_dimension_alignment()
    call test_scalar_broadcasting()
    call test_1d_broadcasting()
    call test_2d_broadcasting() 
    call test_multi_dimensional_broadcasting()
    call test_broadcasting_errors()
    call test_broadcasting_with_missing_dims()
    call test_broadcasting_optimization()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Broadcasting Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some broadcasting tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All broadcasting tests passed!"
    end if

contains

    subroutine test_dimension_alignment()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(3,4) :: data1
        real(real64), dimension(3,4) :: data2
        integer :: i, j
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data1 = reshape([(real(i, real64), i=1,12)], [3,4])
        data2 = reshape([(real(i*2, real64), i=1,12)], [3,4])
        
        var1 = variable(data1, name="var1", dim_names=["x", "y"])
        var2 = variable(data2, name="var2", dim_names=["x", "y"])
        
        ! Test dimension alignment check
        if (.not. can_broadcast(var1, var2)) then
            test_passed = .false.
            write(error_unit,'(A)') "Same-shape variables should be broadcastable"
        end if
        
        ! Test broadcast result shape
        result = broadcast_to_common_shape(var1, var2)
        if (any(result%shape /= var1%shape)) then
            test_passed = .false.
            write(error_unit,'(A)') "Broadcast result has wrong shape"
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dimension alignment test"
        else
            write(*,'(A)') "FAIL: Dimension alignment test"
        end if
    end subroutine test_dimension_alignment
    
    subroutine test_scalar_broadcasting()
        type(fortarray_t) :: scalar_var, array_var, result
        real(real64) :: scalar_val = 5.0_real64
        real(real64), dimension(3,4) :: array_data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create scalar and array
        scalar_var = variable_scalar(scalar_val, name="scalar")
        array_data = reshape([(real(i, real64), i=1,12)], [3,4])
        array_var = variable(array_data, name="array", dim_names=["x", "y"])
        
        ! Test scalar can broadcast to array
        if (.not. can_broadcast(scalar_var, array_var)) then
            test_passed = .false.
            write(error_unit,'(A)') "Scalar should broadcast to any shape"
        end if
        
        ! Test broadcast result
        result = broadcast_to_shape(scalar_var, array_var%shape)
        if (any(result%shape /= array_var%shape)) then
            test_passed = .false.
            write(error_unit,'(A)') "Broadcast scalar has wrong shape"
        else
            ! Check all values are the scalar value
            do i = 1, result%n_elements
                if (abs(result%data%values_r64(i) - scalar_val) > 1e-10) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Broadcast scalar has wrong values"
                    exit
                end if
            end do
        end if
        
        call finalize_variable(scalar_var)
        call finalize_variable(array_var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Scalar broadcasting test"
        else
            write(*,'(A)') "FAIL: Scalar broadcasting test"
        end if
    end subroutine test_scalar_broadcasting
    
    subroutine test_1d_broadcasting()
        type(fortarray_t) :: vec_var, mat_var, result
        real(real64), dimension(4) :: vec_data
        real(real64), dimension(3,4) :: mat_data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 1D vector and 2D matrix
        vec_data = [(real(i, real64), i=1,4)]
        mat_data = reshape([(real(i, real64), i=1,12)], [3,4])
        
        vec_var = variable(vec_data, name="vector", dim_names=["y"])
        mat_var = variable(mat_data, name="matrix", dim_names=["x", "y"])
        
        ! Test 1D can broadcast to 2D along matching dimension
        if (.not. can_broadcast(vec_var, mat_var)) then
            test_passed = .false.
            write(error_unit,'(A)') "1D vector should broadcast to 2D matrix"
        end if
        
        ! Test broadcast result
        result = broadcast_to_shape(vec_var, mat_var%shape)
        if (any(result%shape /= mat_var%shape)) then
            test_passed = .false.
            write(error_unit,'(A)') "Broadcast 1D has wrong shape"
        else
            ! Check values are repeated along new dimension
            ! Each column should have the same value
            do i = 1, 4
                if (abs(result%data%values_r64(1 + (i-1)*3) - vec_data(i)) > 1e-10 .or. &
                    abs(result%data%values_r64(2 + (i-1)*3) - vec_data(i)) > 1e-10 .or. &
                    abs(result%data%values_r64(3 + (i-1)*3) - vec_data(i)) > 1e-10) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Broadcast 1D has wrong values"
                    exit
                end if
            end do
        end if
        
        call finalize_variable(vec_var)
        call finalize_variable(mat_var)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: 1D broadcasting test"
        else
            write(*,'(A)') "FAIL: 1D broadcasting test"
        end if
    end subroutine test_1d_broadcasting
    
    subroutine test_2d_broadcasting()
        type(fortarray_t) :: mat1, mat2, result
        real(real64), dimension(1,4) :: data1
        real(real64), dimension(3,4) :: data2
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create matrices with compatible shapes for broadcasting
        data1 = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [1,4])
        data2 = reshape([(real(i, real64), i=1,12)], [3,4])
        
        mat1 = variable(data1, name="mat1", dim_names=["x", "y"])
        mat2 = variable(data2, name="mat2", dim_names=["x", "y"])
        
        ! Test broadcasting compatibility
        if (.not. can_broadcast(mat1, mat2)) then
            test_passed = .false.
            write(error_unit,'(A)') "[1,4] should broadcast to [3,4]"
        end if
        
        ! Test broadcast result
        result = broadcast_to_shape(mat1, mat2%shape)
        if (any(result%shape /= [3,4])) then
            test_passed = .false.
            write(error_unit,'(A)') "Broadcast result has wrong shape"
        end if
        
        call finalize_variable(mat1)
        call finalize_variable(mat2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: 2D broadcasting test"
        else
            write(*,'(A)') "FAIL: 2D broadcasting test"
        end if
    end subroutine test_2d_broadcasting
    
    subroutine test_multi_dimensional_broadcasting()
        type(fortarray_t) :: var1, var2, var3, result
        real(real64), dimension(1,3,1) :: data1
        real(real64), dimension(2,1,4) :: data2
        real(real64), dimension(2,3,4) :: expected_shape_data
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 3D arrays with broadcastable shapes
        data1 = reshape([1.0_real64, 2.0_real64, 3.0_real64], [1,3,1])
        data2 = reshape([(real(i, real64), i=1,8)], [2,1,4])
        
        var1 = variable(data1, name="var1", dim_names=["x", "y", "z"])
        var2 = variable(data2, name="var2", dim_names=["x", "y", "z"])
        
        ! Test broadcasting compatibility
        if (.not. can_broadcast(var1, var2)) then
            test_passed = .false.
            write(error_unit,'(A)') "[1,3,1] should broadcast with [2,1,4]"
        end if
        
        ! Test finding common shape
        result = broadcast_to_common_shape(var1, var2)
        if (any(result%shape /= [2,3,4])) then
            test_passed = .false.
            write(error_unit,'(A)') "Common broadcast shape should be [2,3,4]"
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Multi-dimensional broadcasting test"
        else
            write(*,'(A)') "FAIL: Multi-dimensional broadcasting test"
        end if
    end subroutine test_multi_dimensional_broadcasting
    
    subroutine test_broadcasting_errors()
        type(fortarray_t) :: var1, var2
        real(real64), dimension(3,4) :: data1
        real(real64), dimension(5,6) :: data2
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create incompatible shapes
        data1 = reshape([(real(i, real64), i=1,12)], [3,4])
        data2 = reshape([(real(i, real64), i=1,30)], [5,6])
        
        var1 = variable(data1, name="var1", dim_names=["x", "y"])
        var2 = variable(data2, name="var2", dim_names=["x", "y"])
        
        ! Test broadcasting incompatibility
        if (can_broadcast(var1, var2)) then
            test_passed = .false.
            write(error_unit,'(A)') "[3,4] should not broadcast with [5,6]"
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Broadcasting errors test"
        else
            write(*,'(A)') "FAIL: Broadcasting errors test"
        end if
    end subroutine test_broadcasting_errors
    
    subroutine test_broadcasting_with_missing_dims()
        type(fortarray_t) :: var1d, var3d, result
        real(real64), dimension(4) :: data1d
        real(real64), dimension(2,3,4) :: data3d
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 1D and 3D arrays
        data1d = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        data3d = reshape([(real(i, real64), i=1,24)], [2,3,4])
        
        var1d = variable(data1d, name="var1d", dim_names=["z"])
        var3d = variable(data3d, name="var3d", dim_names=["x", "y", "z"])
        
        ! Test 1D can broadcast to 3D
        if (.not. can_broadcast(var1d, var3d)) then
            test_passed = .false.
            write(error_unit,'(A)') "1D should broadcast to 3D when trailing dims match"
        end if
        
        ! Test broadcast result
        result = broadcast_to_shape(var1d, var3d%shape)
        if (any(result%shape /= var3d%shape)) then
            test_passed = .false.
            write(error_unit,'(A)') "Broadcast 1D to 3D has wrong shape"
        end if
        
        call finalize_variable(var1d)
        call finalize_variable(var3d)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Broadcasting with missing dims test"
        else
            write(*,'(A)') "FAIL: Broadcasting with missing dims test"
        end if
    end subroutine test_broadcasting_with_missing_dims
    
    subroutine test_broadcasting_optimization()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(1000,1) :: data1
        real(real64), dimension(1,1000) :: data2
        integer :: i
        logical :: test_passed
        real(real64) :: start_time, end_time
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create large arrays for performance test
        data1 = reshape([(real(i, real64), i=1,1000)], [1000,1])
        data2 = reshape([(real(i, real64), i=1,1000)], [1,1000])
        
        var1 = variable(data1, name="var1", dim_names=["x", "y"])
        var2 = variable(data2, name="var2", dim_names=["x", "y"])
        
        ! Test broadcasting performance
        call cpu_time(start_time)
        result = broadcast_to_common_shape(var1, var2)
        call cpu_time(end_time)
        
        if (any(result%shape /= [1000,1000])) then
            test_passed = .false.
            write(error_unit,'(A)') "Large broadcast has wrong shape"
        end if
        
        ! Basic performance check (should complete quickly)
        if (end_time - start_time > 1.0) then
            write(*,'(A,F6.3,A)') "WARNING: Broadcasting took ", end_time - start_time, " seconds"
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Broadcasting optimization test"
        else
            write(*,'(A)') "FAIL: Broadcasting optimization test"
        end if
    end subroutine test_broadcasting_optimization
    
end program test_broadcasting