program test_arithmetic
    use fortarray
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Arithmetic Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run tests
    call test_basic_addition()
    call test_basic_subtraction()
    call test_basic_multiplication()
    call test_basic_division()
    call test_basic_power()
    call test_scalar_operations()
    call test_broadcasting_arithmetic()
    call test_type_promotion()
    call test_nan_handling()
    call test_missing_value_arithmetic()
    call test_operator_overloading()
    call test_chained_operations()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Arithmetic Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some arithmetic tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All arithmetic tests passed!"
    end if

contains

    subroutine test_basic_addition()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(3,4) :: data1, data2, expected
        real(real64), dimension(:), allocatable :: expected_flat
        integer :: i, j
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data1 = reshape([(real(i, real64), i=1,12)], [3,4])
        data2 = reshape([(real(i*2, real64), i=1,12)], [3,4])
        expected = data1 + data2
        allocate(expected_flat(12))
        expected_flat = reshape(expected, [12])
        
        var1 = variable(data1, name="var1", dim_names=["x", "y"])
        var2 = variable(data2, name="var2", dim_names=["x", "y"])
        
        ! Test addition
        result = var1 + var2
        
        ! Check result
        if (any(result%shape /= var1%shape)) then
            test_passed = .false.
            write(error_unit,'(A)') "Addition result has wrong shape"
        else
            do i = 1, result%n_elements
                if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                    test_passed = .false.
                    write(error_unit,'(A)') "Addition result has wrong values"
                    exit
                end if
            end do
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic addition test"
        else
            write(*,'(A)') "FAIL: Basic addition test"
        end if
    end subroutine test_basic_addition
    
    subroutine test_basic_subtraction()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(3,4) :: data1, data2, expected
        real(real64), dimension(:), allocatable :: expected_flat
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data1 = reshape([(real(i*3, real64), i=1,12)], [3,4])
        data2 = reshape([(real(i, real64), i=1,12)], [3,4])
        expected = data1 - data2
        allocate(expected_flat(12))
        expected_flat = reshape(expected, [12])
        
        var1 = variable(data1, name="var1", dim_names=["x", "y"])
        var2 = variable(data2, name="var2", dim_names=["x", "y"])
        
        ! Test subtraction
        result = var1 - var2
        
        ! Check result
        do i = 1, result%n_elements
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Subtraction result has wrong values"
                exit
            end if
        end do
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic subtraction test"
        else
            write(*,'(A)') "FAIL: Basic subtraction test"
        end if
    end subroutine test_basic_subtraction
    
    subroutine test_basic_multiplication()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(2,3) :: data1, data2, expected
        real(real64), dimension(:), allocatable :: expected_flat
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data1 = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [2,3])
        data2 = reshape([2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64], [2,3])
        expected = data1 * data2
        allocate(expected_flat(6))
        expected_flat = reshape(expected, [6])
        
        var1 = variable(data1, name="var1", dim_names=["x", "y"])
        var2 = variable(data2, name="var2", dim_names=["x", "y"])
        
        ! Test multiplication
        result = var1 * var2
        
        ! Check result
        do i = 1, result%n_elements
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Multiplication result has wrong values"
                exit
            end if
        end do
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic multiplication test"
        else
            write(*,'(A)') "FAIL: Basic multiplication test"
        end if
    end subroutine test_basic_multiplication
    
    subroutine test_basic_division()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(2,2) :: data1, data2, expected
        real(real64), dimension(:), allocatable :: expected_flat
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data1 = reshape([10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64], [2,2])
        data2 = reshape([2.0_real64, 4.0_real64, 5.0_real64, 8.0_real64], [2,2])
        expected = data1 / data2
        allocate(expected_flat(4))
        expected_flat = reshape(expected, [4])
        
        var1 = variable(data1, name="var1", dim_names=["x", "y"])
        var2 = variable(data2, name="var2", dim_names=["x", "y"])
        
        ! Test division
        result = var1 / var2
        
        ! Check result
        do i = 1, result%n_elements
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Division result has wrong values"
                exit
            end if
        end do
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic division test"
        else
            write(*,'(A)') "FAIL: Basic division test"
        end if
    end subroutine test_basic_division
    
    subroutine test_basic_power()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(2,2) :: data1, data2, expected
        real(real64), dimension(:), allocatable :: expected_flat
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data1 = reshape([2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], [2,2])
        data2 = reshape([2.0_real64, 3.0_real64, 2.0_real64, 2.0_real64], [2,2])
        expected = data1 ** data2
        allocate(expected_flat(4))
        expected_flat = reshape(expected, [4])
        
        var1 = variable(data1, name="base", dim_names=["x", "y"])
        var2 = variable(data2, name="exponent", dim_names=["x", "y"])
        
        ! Test power operation
        result = var1 ** var2
        
        ! Check result
        do i = 1, result%n_elements
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A,I0,A,F10.6,A,F10.6)') &
                    "Power result wrong at ", i, ": got ", result%data%values_r64(i), &
                    " expected ", expected_flat(i)
                exit
            end if
        end do
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic power test"
        else
            write(*,'(A)') "FAIL: Basic power test"
        end if
    end subroutine test_basic_power
    
    subroutine test_scalar_operations()
        type(fortarray_t) :: var, result
        real(real64), dimension(2,3) :: data, expected
        real(real64), dimension(:), allocatable :: expected_flat
        real(real64) :: scalar_val = 2.0_real64
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data = reshape([(real(i, real64), i=1,6)], [2,3])
        var = variable(data, name="var", dim_names=["x", "y"])
        
        ! Test scalar addition
        result = var + scalar_val
        expected = data + scalar_val
        allocate(expected_flat(6))
        expected_flat = reshape(expected, [6])
        do i = 1, result%n_elements
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Scalar addition failed"
                exit
            end if
        end do
        call finalize_variable(result)
        
        ! Test scalar multiplication
        result = var * scalar_val
        expected = data * scalar_val
        expected_flat = reshape(expected, [6])
        do i = 1, result%n_elements
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Scalar multiplication failed"
                exit
            end if
        end do
        call finalize_variable(result)
        
        ! Test scalar on left side
        result = scalar_val * var
        do i = 1, result%n_elements
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Left scalar multiplication failed"
                exit
            end if
        end do
        call finalize_variable(result)
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Scalar operations test"
        else
            write(*,'(A)') "FAIL: Scalar operations test"
        end if
    end subroutine test_scalar_operations
    
    subroutine test_broadcasting_arithmetic()
        type(fortarray_t) :: vec, mat, result
        real(real64), dimension(4) :: vec_data
        real(real64), dimension(3,4) :: mat_data
        integer :: i, j
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 1D and 2D variables
        vec_data = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        mat_data = reshape([(real(i, real64), i=1,12)], [3,4])
        
        vec = variable(vec_data, name="vector", dim_names=["y"])
        mat = variable(mat_data, name="matrix", dim_names=["x", "y"])
        
        ! Test broadcasting addition
        result = vec + mat
        
        ! Check shape
        if (any(result%shape /= mat%shape)) then
            test_passed = .false.
            write(error_unit,'(A)') "Broadcasting addition has wrong shape"
        else
            ! Check values - each column should have vec value added
            do j = 1, 4
                do i = 1, 3
                    if (abs(result%data%values_r64(i + (j-1)*3) - &
                           (mat_data(i,j) + vec_data(j))) > 1e-10) then
                        test_passed = .false.
                        write(error_unit,'(A)') "Broadcasting addition has wrong values"
                        exit
                    end if
                end do
            end do
        end if
        
        call finalize_variable(vec)
        call finalize_variable(mat)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Broadcasting arithmetic test"
        else
            write(*,'(A)') "FAIL: Broadcasting arithmetic test"
        end if
    end subroutine test_broadcasting_arithmetic
    
    subroutine test_type_promotion()
        type(fortarray_t) :: var_r32, var_r64, var_i32, result
        real(real32), dimension(2,2) :: data_r32
        real(real64), dimension(2,2) :: data_r64
        integer(int32), dimension(2,2) :: data_i32
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create variables of different types
        data_r32 = reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32], [2,2])
        data_r64 = reshape([2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], [2,2])
        data_i32 = reshape([1_int32, 2_int32, 3_int32, 4_int32], [2,2])
        
        var_r32 = variable(data_r32, name="r32")
        var_r64 = variable(data_r64, name="r64")
        var_i32 = variable(data_i32, name="i32")
        
        ! Test real32 + real64 -> real64
        result = var_r32 + var_r64
        if (result%data%dtype /= DTYPE_REAL64) then
            test_passed = .false.
            write(error_unit,'(A)') "Type promotion r32+r64 failed"
        end if
        call finalize_variable(result)
        
        ! Test int32 + real64 -> real64
        result = var_i32 + var_r64
        if (result%data%dtype /= DTYPE_REAL64) then
            test_passed = .false.
            write(error_unit,'(A)') "Type promotion i32+r64 failed"
        end if
        call finalize_variable(result)
        
        call finalize_variable(var_r32)
        call finalize_variable(var_r64)
        call finalize_variable(var_i32)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Type promotion test"
        else
            write(*,'(A)') "FAIL: Type promotion test"
        end if
    end subroutine test_type_promotion
    
    subroutine test_nan_handling()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(2,2) :: data1, data2
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with NaN
        data1 = reshape([1.0_real64, 0.0_real64, 3.0_real64, 4.0_real64], [2,2])
        data2 = reshape([2.0_real64, 0.0_real64, 5.0_real64, 6.0_real64], [2,2])
        
        var1 = variable(data1, name="var1")
        var2 = variable(data2, name="var2")
        
        ! Test division by zero produces NaN/Inf
        result = var1 / var2
        
        ! Check that 0/0 produces NaN
        if (.not. isnan(result%data%values_r64(2))) then
            test_passed = .false.
            write(error_unit,'(A)') "0/0 should produce NaN"
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: NaN handling test"
        else
            write(*,'(A)') "FAIL: NaN handling test"
        end if
    end subroutine test_nan_handling
    
    subroutine test_missing_value_arithmetic()
        type(fortarray_t) :: var1, var2, result
        real(real64), dimension(2,2) :: data1, data2
        real(real64) :: missing
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        missing = huge(1.0_real64)
        
        ! Create data with missing values
        data1 = reshape([1.0_real64, missing, 3.0_real64, 4.0_real64], [2,2])
        data2 = reshape([2.0_real64, 3.0_real64, missing, 6.0_real64], [2,2])
        
        var1 = variable(data1, name="var1")
        var2 = variable(data2, name="var2")
        
        ! Test that operations with missing values produce missing values
        result = var1 + var2
        
        ! Check missing value propagation
        if (abs(result%data%values_r64(2) - missing) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Missing + value should be missing"
        end if
        
        if (abs(result%data%values_r64(3) - missing) > 1e-10) then
            test_passed = .false.
            write(error_unit,'(A)') "Value + missing should be missing"
        end if
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Missing value arithmetic test"
        else
            write(*,'(A)') "FAIL: Missing value arithmetic test"
        end if
    end subroutine test_missing_value_arithmetic
    
    subroutine test_operator_overloading()
        type(fortarray_t) :: a, b, c, result
        real(real64), dimension(2,2) :: data_a, data_b, data_c
        real(real64), dimension(:), allocatable :: expected_flat
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data_a = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2,2])
        data_b = reshape([2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], [2,2])
        data_c = reshape([1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2,2])
        allocate(expected_flat(4))
        expected_flat = reshape(data_a + data_b * data_c, [4])
        
        a = variable(data_a, name="a")
        b = variable(data_b, name="b")
        c = variable(data_c, name="c")
        
        ! Test operator precedence and overloading
        result = a + b * c  ! Should be a + (b * c)
        
        ! Check values
        do i = 1, 4
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Operator precedence incorrect"
                exit
            end if
        end do
        
        call finalize_variable(a)
        call finalize_variable(b)
        call finalize_variable(c)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Operator overloading test"
        else
            write(*,'(A)') "FAIL: Operator overloading test"
        end if
    end subroutine test_operator_overloading
    
    subroutine test_chained_operations()
        type(fortarray_t) :: a, b, c, d, result
        real(real64), dimension(2,2) :: data_a, data_b, data_c, data_d, expected
        real(real64), dimension(:), allocatable :: expected_flat
        integer :: i
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data_a = reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2,2])
        data_b = reshape([2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], [2,2])
        data_c = reshape([0.5_real64, 0.5_real64, 0.5_real64, 0.5_real64], [2,2])
        data_d = reshape([1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2,2])
        
        a = variable(data_a, name="a")
        b = variable(data_b, name="b")
        c = variable(data_c, name="c")
        d = variable(data_d, name="d")
        
        ! Test chained operations
        result = (a + b) * c - d
        expected = (data_a + data_b) * data_c - data_d
        allocate(expected_flat(4))
        expected_flat = reshape(expected, [4])
        
        ! Check values
        do i = 1, 4
            if (abs(result%data%values_r64(i) - expected_flat(i)) > 1e-10) then
                test_passed = .false.
                write(error_unit,'(A)') "Chained operations incorrect"
                exit
            end if
        end do
        
        call finalize_variable(a)
        call finalize_variable(b)
        call finalize_variable(c)
        call finalize_variable(d)
        call finalize_variable(result)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Chained operations test"
        else
            write(*,'(A)') "FAIL: Chained operations test"
        end if
    end subroutine test_chained_operations
    
end program test_arithmetic