program test_indexing
    use foxel_types
    use foxel_constructors
    use foxel_indexing
    use foxel_memory
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    ! Test positional indexing
    call test_1d_positional_indexing()
    call test_2d_positional_indexing()
    call test_3d_positional_indexing()
    call test_nd_positional_indexing()
    call test_scalar_variable_indexing()
    
    ! Test label-based indexing
    call test_1d_label_indexing()
    call test_nd_label_indexing()
    call test_numeric_label_indexing()
    
    ! Test set operations
    call test_set_item_1d()
    call test_set_item_2d()
    call test_set_item_3d()
    call test_set_item_nd()
    
    ! Test error cases
    call test_bounds_checking()
    call test_dimension_mismatch()
    call test_missing_coordinates()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_1d_positional_indexing()
        type(variable_t) :: var
        real(real64), dimension(10) :: data_1d
        real(real64) :: value
        logical :: test_passed
        integer :: stat, i
        character(len=256) :: error_msg
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data_1d = [(real(i, real64), i=1,10)]
        var = variable(data_1d, dim_names=["x"])
        
        ! Test getting values
        do i = 1, 10
            value = get_item(var, i, stat=stat)
            if (stat /= 0 .or. abs(value - real(i, real64)) > epsilon(1.0_real64)) then
                test_passed = .false.
                write(error_unit,'(A,I0,A,F0.1,A,F0.1)') &
                    "Failed to get item at ", i, ": expected ", real(i, real64), ", got ", value
            end if
        end do
        
        ! Test out of bounds
        value = get_item(var, 0, stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should have failed for index 0"
        end if
        
        value = get_item(var, 11, stat=stat)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should have failed for index 11"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: 1D positional indexing test"
        else
            write(*,'(A)') "FAIL: 1D positional indexing test"
        end if
    end subroutine test_1d_positional_indexing
    
    subroutine test_2d_positional_indexing()
        type(variable_t) :: var
        real(real64), dimension(3, 4) :: data_2d
        real(real64) :: value, expected
        logical :: test_passed
        integer :: stat, i, j
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do j = 1, 4
            do i = 1, 3
                data_2d(i,j) = real(i + (j-1)*10, real64)
            end do
        end do
        var = variable(data_2d, dim_names=["row", "col"])
        
        ! Test getting values
        do j = 1, 4
            do i = 1, 3
                value = get_item(var, i, j, stat=stat)
                expected = real(i + (j-1)*10, real64)
                if (stat /= 0 .or. abs(value - expected) > epsilon(1.0_real64)) then
                    test_passed = .false.
                    write(error_unit,'(A,I0,A,I0,A,F0.1,A,F0.1)') &
                        "Failed at (", i, ",", j, "): expected ", expected, ", got ", value
                end if
            end do
        end do
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: 2D positional indexing test"
        else
            write(*,'(A)') "FAIL: 2D positional indexing test"
        end if
    end subroutine test_2d_positional_indexing
    
    subroutine test_3d_positional_indexing()
        type(variable_t) :: var
        real(real64), dimension(2, 3, 4) :: data_3d
        real(real64) :: value, expected
        logical :: test_passed
        integer :: stat, i, j, k
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        do k = 1, 4
            do j = 1, 3
                do i = 1, 2
                    data_3d(i,j,k) = real(i + (j-1)*10 + (k-1)*100, real64)
                end do
            end do
        end do
        var = variable(data_3d, dim_names=["x", "y", "z"])
        
        ! Test specific values
        value = get_item(var, 1, 1, 1, stat=stat)
        expected = 1.0_real64
        if (stat /= 0 .or. abs(value - expected) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A,F0.1,A,F0.1)') "At (1,1,1): expected ", expected, ", got ", value
        end if
        
        value = get_item(var, 2, 3, 4, stat=stat)
        expected = 322.0_real64
        if (stat /= 0 .or. abs(value - expected) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A,F0.1,A,F0.1)') "At (2,3,4): expected ", expected, ", got ", value
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: 3D positional indexing test"
        else
            write(*,'(A)') "FAIL: 3D positional indexing test"
        end if
    end subroutine test_3d_positional_indexing
    
    subroutine test_nd_positional_indexing()
        type(variable_t) :: var
        real(real64), dimension(2, 3, 4) :: data_3d
        real(real64) :: value
        logical :: test_passed
        integer :: stat, i, j, k, idx
        integer, dimension(3) :: indices
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data with known pattern
        idx = 1
        do k = 1, 4
            do j = 1, 3
                do i = 1, 2
                    data_3d(i,j,k) = real(idx, real64)
                    idx = idx + 1
                end do
            end do
        end do
        var = variable(data_3d, dim_names=["x  ", "y  ", "z  "])
        
        ! Test N-dimensional indexing
        indices = [1, 1, 1]
        value = get_item(var, indices, stat=stat)
        if (stat /= 0 .or. abs(value - 1.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed at [1,1,1]"
        end if
        
        indices = [2, 3, 4]
        value = get_item(var, indices, stat=stat)
        if (stat /= 0 .or. abs(value - 24.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed at [2,3,4]"
        end if
        
        ! Test with different indices
        indices = [1, 2, 3]
        value = get_item(var, indices, stat=stat)
        if (stat /= 0 .or. abs(value - 15.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A,F0.1)') "Failed at [1,2,3], got ", value
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: N-dimensional positional indexing test"
        else
            write(*,'(A)') "FAIL: N-dimensional positional indexing test"
        end if
    end subroutine test_nd_positional_indexing
    
    subroutine test_scalar_variable_indexing()
        type(variable_t) :: var
        real(real64) :: value
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create scalar variable
        var = variable_scalar(42.0_real64, name="scalar_test")
        
        ! For scalar variables, we might want to support get_item with no indices
        ! For now, test that it's a 0D variable
        if (var%n_dims /= 0) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Scalar should have 0 dimensions, got ", var%n_dims
        end if
        
        if (var%n_elements /= 1) then
            test_passed = .false.
            write(error_unit,'(A,I0)') "Scalar should have 1 element, got ", var%n_elements
        end if
        
        ! Direct access to scalar value
        if (abs(var%data%values_r64(1) - 42.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A)') "Scalar value incorrect"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Scalar variable indexing test"
        else
            write(*,'(A)') "FAIL: Scalar variable indexing test"
        end if
    end subroutine test_scalar_variable_indexing
    
    subroutine test_1d_label_indexing()
        type(variable_t) :: var
        type(coordinate_t) :: coord
        type(label_index_t) :: label_idx
        real(real64), dimension(5) :: data_1d
        real(real64) :: value
        logical :: test_passed
        integer :: stat, i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with string coordinate
        data_1d = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]
        
        ! Create string coordinate
        call create_coordinate(coord, 5, "char", stat, char_len=10)
        coord%name = "station"
        coord%values_char = ["Station_A ", "Station_B ", "Station_C ", "Station_D ", "Station_E "]
        
        ! Create variable with coordinate
        var = variable(data_1d, dim_names=["station"], coords=[coord])
        
        ! Test label indexing
        label_idx = create_label_index(label="Station_C")
        value = get_item(var, label_idx, stat=stat)
        if (stat /= 0 .or. abs(value - 30.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to get Station_C"
        end if
        
        ! Test non-existent label
        label_idx = create_label_index(label="Station_Z")
        value = get_item(var, label_idx, stat=stat)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should have failed for non-existent label"
        end if
        
        call finalize_variable(var)
        call finalize_coordinate(coord)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: 1D label indexing test"
        else
            write(*,'(A)') "FAIL: 1D label indexing test"
        end if
    end subroutine test_1d_label_indexing
    
    subroutine test_nd_label_indexing()
        type(variable_t) :: var
        type(coordinate_t), dimension(2) :: coords
        type(label_index_t), dimension(2) :: label_indices
        real(real64), dimension(3, 2) :: data_2d
        real(real64) :: value
        logical :: test_passed
        integer :: stat, i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create test data
        data_2d = reshape([1.0_real64, 2.0_real64, 3.0_real64, &
                          4.0_real64, 5.0_real64, 6.0_real64], [3, 2])
        
        ! Create coordinates
        call create_coordinate(coords(1), 3, "char", stat, char_len=5)
        coords(1)%name = "row"
        coords(1)%values_char = ["row1 ", "row2 ", "row3 "]
        
        call create_coordinate(coords(2), 2, "char", stat, char_len=5)
        coords(2)%name = "col"
        coords(2)%values_char = ["col1 ", "col2 "]
        
        var = variable(data_2d, dim_names=[("row "), ("col ")], coords=coords)
        
        ! Test multi-dimensional label indexing
        label_indices(1) = create_label_index(label="row2")
        label_indices(2) = create_label_index(label="col2")
        value = get_item(var, label_indices, stat=stat)
        if (stat /= 0 .or. abs(value - 5.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A,F0.1)') "Failed at (row2,col2), got ", value
        end if
        
        call finalize_variable(var)
        do i = 1, 2
            call finalize_coordinate(coords(i))
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: N-dimensional label indexing test"
        else
            write(*,'(A)') "FAIL: N-dimensional label indexing test"
        end if
    end subroutine test_nd_label_indexing
    
    subroutine test_numeric_label_indexing()
        type(variable_t) :: var
        type(coordinate_t) :: coord
        type(label_index_t) :: label_idx
        real(real64), dimension(5) :: data_1d
        real(real64) :: value
        logical :: test_passed
        integer :: stat, i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create data with numeric coordinate
        data_1d = [100.0_real64, 200.0_real64, 300.0_real64, 400.0_real64, 500.0_real64]
        
        ! Create numeric coordinate
        call create_coordinate(coord, 5, "real64", stat)
        coord%name = "time"
        coord%values_r64 = [0.0_real64, 0.5_real64, 1.0_real64, 1.5_real64, 2.0_real64]
        
        var = variable(data_1d, dim_names=["time"], coords=[coord])
        
        ! Test numeric label indexing
        label_idx = create_label_index(value=1.0_real64)
        value = get_item(var, label_idx, stat=stat)
        if (stat /= 0 .or. abs(value - 300.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
            write(error_unit,'(A)') "Failed to get value at time=1.0"
        end if
        
        call finalize_variable(var)
        call finalize_coordinate(coord)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Numeric label indexing test"
        else
            write(*,'(A)') "FAIL: Numeric label indexing test"
        end if
    end subroutine test_numeric_label_indexing
    
    subroutine test_set_item_1d()
        type(variable_t) :: var
        real(real64), dimension(5) :: data_1d
        real(real64) :: value
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create variable
        data_1d = 0.0_real64
        var = variable(data_1d, dim_names=["x"])
        
        ! Set values
        call set_item(var, 1, value=10.0_real64, stat=stat)
        call set_item(var, 3, value=30.0_real64, stat=stat)
        call set_item(var, 5, value=50.0_real64, stat=stat)
        
        ! Check values
        value = get_item(var, 1, stat=stat)
        if (abs(value - 10.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        value = get_item(var, 2, stat=stat)
        if (abs(value - 0.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        value = get_item(var, 3, stat=stat)
        if (abs(value - 30.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Set item 1D test"
        else
            write(*,'(A)') "FAIL: Set item 1D test"
        end if
    end subroutine test_set_item_1d
    
    subroutine test_set_item_2d()
        type(variable_t) :: var
        real(real64), dimension(3, 3) :: data_2d
        real(real64) :: value
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create variable
        data_2d = 0.0_real64
        var = variable(data_2d, dim_names=["row", "col"])
        
        ! Set diagonal
        call set_item(var, 1, 1, value=1.0_real64, stat=stat)
        call set_item(var, 2, 2, value=2.0_real64, stat=stat)
        call set_item(var, 3, 3, value=3.0_real64, stat=stat)
        
        ! Check values
        value = get_item(var, 1, 1, stat=stat)
        if (abs(value - 1.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        value = get_item(var, 2, 2, stat=stat)
        if (abs(value - 2.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        value = get_item(var, 1, 2, stat=stat)
        if (abs(value - 0.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Set item 2D test"
        else
            write(*,'(A)') "FAIL: Set item 2D test"
        end if
    end subroutine test_set_item_2d
    
    subroutine test_set_item_3d()
        type(variable_t) :: var
        real(real64), dimension(2, 2, 2) :: data_3d
        real(real64) :: value
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create variable
        data_3d = 0.0_real64
        var = variable(data_3d, dim_names=["x", "y", "z"])
        
        ! Set corners
        call set_item(var, 1, 1, 1, value=111.0_real64, stat=stat)
        call set_item(var, 2, 2, 2, value=222.0_real64, stat=stat)
        
        ! Check values
        value = get_item(var, 1, 1, 1, stat=stat)
        if (abs(value - 111.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        value = get_item(var, 2, 2, 2, stat=stat)
        if (abs(value - 222.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Set item 3D test"
        else
            write(*,'(A)') "FAIL: Set item 3D test"
        end if
    end subroutine test_set_item_3d
    
    subroutine test_set_item_nd()
        type(variable_t) :: var
        real(real64), dimension(3, 2, 3) :: data_3d
        real(real64) :: value
        logical :: test_passed
        integer :: stat
        integer, dimension(3) :: indices
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create variable
        data_3d = 0.0_real64
        var = variable(data_3d, dim_names=["a  ", "b  ", "c  "])
        
        ! Set specific values
        indices = [1, 2, 1]
        call set_item(var, indices, 121.0_real64, stat=stat)
        
        indices = [3, 1, 3]
        call set_item(var, indices, 313.0_real64, stat=stat)
        
        indices = [2, 2, 2]
        call set_item(var, indices, 222.0_real64, stat=stat)
        
        ! Check values
        indices = [1, 2, 1]
        value = get_item(var, indices, stat=stat)
        if (abs(value - 121.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        indices = [3, 1, 3]
        value = get_item(var, indices, stat=stat)
        if (abs(value - 313.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        indices = [2, 2, 2]
        value = get_item(var, indices, stat=stat)
        if (abs(value - 222.0_real64) > epsilon(1.0_real64)) test_passed = .false.
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Set item N-dimensional test"
        else
            write(*,'(A)') "FAIL: Set item N-dimensional test"
        end if
    end subroutine test_set_item_nd
    
    subroutine test_bounds_checking()
        type(variable_t) :: var
        real(real64), dimension(5) :: data_1d
        real(real64) :: value
        logical :: test_passed
        integer :: stat
        character(len=256) :: error_msg
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create variable
        data_1d = 1.0_real64
        var = variable(data_1d, dim_names=["x"])
        
        ! Test lower bound
        value = get_item(var, 0, stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for index 0"
        end if
        
        ! Test upper bound
        value = get_item(var, 6, stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for index 6"
        end if
        
        ! Test negative index
        value = get_item(var, -1, stat=stat, error_msg=error_msg)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for negative index"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Bounds checking test"
        else
            write(*,'(A)') "FAIL: Bounds checking test"
        end if
    end subroutine test_bounds_checking
    
    subroutine test_dimension_mismatch()
        type(variable_t) :: var
        real(real64), dimension(3, 3) :: data_2d
        real(real64) :: value
        logical :: test_passed
        integer :: stat
        integer, dimension(3) :: indices
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create 2D variable
        data_2d = 1.0_real64
        var = variable(data_2d, dim_names=["x", "y"])
        
        ! Try to index with wrong number of dimensions
        value = get_item(var, 1, stat=stat)  ! Only 1 index for 2D
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail with 1 index for 2D variable"
        end if
        
        ! Try with 3 indices
        indices = [1, 1, 1]
        value = get_item(var, indices, stat=stat)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail with 3 indices for 2D variable"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dimension mismatch test"
        else
            write(*,'(A)') "FAIL: Dimension mismatch test"
        end if
    end subroutine test_dimension_mismatch
    
    subroutine test_missing_coordinates()
        type(variable_t) :: var
        type(label_index_t) :: label_idx
        real(real64), dimension(5) :: data_1d
        real(real64) :: value
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create variable without coordinates
        data_1d = 1.0_real64
        var = variable(data_1d, dim_names=["x"])
        
        ! Try label-based indexing without coordinates
        label_idx = create_label_index(label="test")
        value = get_item(var, label_idx, stat=stat)
        if (stat == 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Should fail for label indexing without coordinates"
        end if
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Missing coordinates test"
        else
            write(*,'(A)') "FAIL: Missing coordinates test"
        end if
    end subroutine test_missing_coordinates
    
end program test_indexing