program test_constructors
    use foxel_types
    use foxel_constructors
    use foxel_storage
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    ! Test main constructor
    call test_basic_constructor()
    call test_constructor_validation()
    call test_dimension_validation()
    call test_coordinate_validation()
    call test_shape_validation()
    
    ! Test convenience constructors
    call test_from_array_constructors()
    call test_from_csv_constructor()
    call test_empty_constructor()
    call test_scalar_constructor()
    
    ! Test error handling
    call test_error_messages()
    call test_edge_cases()
    
    write(*,'(A,I0,A,I0,A)') "Passed ", n_tests_passed, " out of ", n_tests_total, " tests."
    if (n_tests_passed /= n_tests_total) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_basic_constructor()
        type(dataframe_t) :: df
        real(real64), dimension(10, 5) :: data_2d
        character(len=20), dimension(2) :: dim_names
        type(coordinate_t), dimension(2) :: coords
        character(len=256) :: error_msg
        logical :: test_passed
        integer :: stat, i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Prepare test data
        call random_number(data_2d)
        dim_names = ["time    ", "station "]
        
        ! Create coordinates
        call create_coordinate(coords(1), 10, "real64", stat)
        coords(1)%values_r64 = [(real(i, real64), i=1,10)]
        
        call create_coordinate(coords(2), 5, "char", stat, char_len=10)
        coords(2)%values_char = ["Station_1 ", "Station_2 ", "Station_3 ", &
                                 "Station_4 ", "Station_5 "]
        
        ! Create dataframe
        df = dataframe(data_2d, dim_names=dim_names, coords=coords, &
                      var_name="temperature", stat=stat, error_msg=error_msg)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "Constructor failed: " // trim(error_msg)
        end if
        
        ! Verify construction
        if (df%n_dims /= 2) test_passed = .false.
        if (df%shape(1) /= 10 .or. df%shape(2) /= 5) test_passed = .false.
        if (df%n_elements /= 50) test_passed = .false.
        if (trim(df%var_name) /= "temperature") test_passed = .false.
        if (.not. df%initialized) test_passed = .false.
        
        ! Verify coordinates were copied
        if (df%coords(1)%length /= 10) test_passed = .false.
        if (df%coords(2)%length /= 5) test_passed = .false.
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Basic constructor test"
        else
            write(*,'(A)') "FAIL: Basic constructor test"
        end if
        
        ! Clean up
        call finalize_dataframe(df)
        call finalize_coordinate(coords(1))
        call finalize_coordinate(coords(2))
    end subroutine test_basic_constructor
    
    subroutine test_constructor_validation()
        type(dataframe_t) :: df
        real(real64), dimension(10, 5) :: data_2d
        character(len=20), dimension(2) :: dim_names
        type(coordinate_t), dimension(2) :: coords
        character(len=256) :: error_msg
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test with mismatched dimensions
        dim_names = ["x", "y"]
        
        ! Create wrong-sized coordinates
        call create_coordinate(coords(1), 10, "real64", stat)
        call create_coordinate(coords(2), 10, "real64", stat)  ! Wrong size!
        
        df = dataframe(data_2d, dim_names=dim_names, coords=coords, &
                      stat=stat, error_msg=error_msg)
        
        if (stat == 0) then
            test_passed = .false.  ! Should have failed
            write(error_unit,'(A)') "Constructor should have failed for mismatched coords"
        end if
        
        ! Clean up
        call finalize_coordinate(coords(1))
        call finalize_coordinate(coords(2))
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Constructor validation test"
        else
            write(*,'(A)') "FAIL: Constructor validation test"
        end if
    end subroutine test_constructor_validation
    
    subroutine test_dimension_validation()
        type(dataframe_t) :: df
        real(real64), dimension(10, 5, 3) :: data_3d
        character(len=20), dimension(3) :: dim_names
        character(len=256) :: error_msg
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test duplicate dimension names
        dim_names = ["time", "time", "level"]  ! Duplicate!
        
        df = dataframe(data_3d, dim_names=dim_names, stat=stat, error_msg=error_msg)
        
        if (stat == 0) then
            test_passed = .false.  ! Should have failed
        else
            ! Check error message mentions duplicates
            if (index(error_msg, "duplicate") == 0 .and. &
                index(error_msg, "Duplicate") == 0) then
                test_passed = .false.
                write(error_unit,'(A)') "Error message should mention duplicate dims"
            end if
        end if
        
        ! Test invalid dimension names
        dim_names = ["time", "123abc", "level"]  ! Invalid identifier
        
        df = dataframe(data_3d, dim_names=dim_names, stat=stat, error_msg=error_msg)
        
        if (stat == 0) then
            test_passed = .false.  ! Should have failed
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dimension validation test"
        else
            write(*,'(A)') "FAIL: Dimension validation test"
        end if
    end subroutine test_dimension_validation
    
    subroutine test_coordinate_validation()
        type(dataframe_t) :: df
        real(real64), dimension(10, 5) :: data_2d
        character(len=20), dimension(2) :: dim_names
        type(coordinate_t), dimension(2) :: coords
        character(len=256) :: error_msg
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        dim_names = ["x", "y"]
        
        ! Create non-monotonic coordinate
        call create_coordinate(coords(1), 10, "real64", stat)
        coords(1)%values_r64 = [1.0_real64, 3.0_real64, 2.0_real64, 4.0_real64, &
                                5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64, &
                                9.0_real64, 10.0_real64]
        coords(1)%is_monotonic = .false.
        
        call create_coordinate(coords(2), 5, "real64", stat)
        coords(2)%values_r64 = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        coords(2)%is_monotonic = .true.
        
        ! Constructor with check_monotonic=.true. should validate
        df = dataframe(data_2d, dim_names=dim_names, coords=coords, &
                      check_monotonic=.true., stat=stat, error_msg=error_msg)
        
        ! For now, we might allow non-monotonic coords with a warning
        ! But let's verify the coordinate was properly copied
        if (stat == 0) then
            if (df%coords(1)%is_monotonic) then
                test_passed = .false.  ! Should preserve the flag
            end if
        end if
        
        ! Clean up
        call finalize_dataframe(df)
        call finalize_coordinate(coords(1))
        call finalize_coordinate(coords(2))
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Coordinate validation test"
        else
            write(*,'(A)') "FAIL: Coordinate validation test"
        end if
    end subroutine test_coordinate_validation
    
    subroutine test_shape_validation()
        type(dataframe_t) :: df
        real(real64), dimension(:,:), allocatable :: data_2d
        character(len=20), dimension(2) :: dim_names
        character(len=256) :: error_msg
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test with zero-sized dimension
        allocate(data_2d(10, 0))  ! Zero columns!
        dim_names = ["rows", "cols"]
        
        df = dataframe(data_2d, dim_names=dim_names, stat=stat, error_msg=error_msg)
        
        if (stat == 0) then
            test_passed = .false.  ! Should reject zero-sized dimensions
        end if
        
        deallocate(data_2d)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Shape validation test"
        else
            write(*,'(A)') "FAIL: Shape validation test"
        end if
    end subroutine test_shape_validation
    
    subroutine test_from_array_constructors()
        type(dataframe_t) :: df
        real(real64), dimension(10, 5) :: data_2d
        real(real32), dimension(20) :: data_1d
        integer(int32), dimension(3, 3, 3) :: data_3d
        logical :: test_passed
        integer :: stat, i
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test 1D array constructor
        call random_number(data_1d)
        df = dataframe_from_array(data_1d, dim_name="samples", stat=stat)
        
        if (stat /= 0) test_passed = .false.
        if (df%n_dims /= 1) test_passed = .false.
        if (df%shape(1) /= 20) test_passed = .false.
        if (trim(df%dim_names(1)) /= "samples") test_passed = .false.
        call finalize_dataframe(df)
        
        ! Test 2D array constructor with auto-generated names
        call random_number(data_2d)
        df = dataframe_from_array(data_2d, stat=stat)
        
        if (stat /= 0) test_passed = .false.
        if (df%n_dims /= 2) test_passed = .false.
        if (df%shape(1) /= 10 .or. df%shape(2) /= 5) test_passed = .false.
        ! Should have auto-generated dimension names
        if (len_trim(df%dim_names(1)) == 0) test_passed = .false.
        call finalize_dataframe(df)
        
        ! Test 3D integer array
        data_3d = reshape([(i, i=1,27)], [3, 3, 3])
        df = dataframe_from_array(data_3d, stat=stat)
        
        if (stat /= 0) test_passed = .false.
        if (df%n_dims /= 3) test_passed = .false.
        if (df%data%dtype /= 1) test_passed = .false.  ! Should be int32
        call finalize_dataframe(df)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: From array constructors test"
        else
            write(*,'(A)') "FAIL: From array constructors test"
        end if
    end subroutine test_from_array_constructors
    
    subroutine test_from_csv_constructor()
        type(dataframe_t) :: df
        character(len=256) :: error_msg
        logical :: test_passed
        integer :: stat, unit
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create a test CSV file
        open(newunit=unit, file="test_data.csv", status="replace", action="write")
        write(unit,'(A)') "time,temperature,pressure"
        write(unit,'(A)') "1.0,20.5,1013.25"
        write(unit,'(A)') "2.0,21.0,1013.30"
        write(unit,'(A)') "3.0,21.5,1013.35"
        close(unit)
        
        ! Load from CSV
        df = dataframe_from_csv("test_data.csv", stat=stat, error_msg=error_msg)
        
        if (stat /= 0) then
            test_passed = .false.
            write(error_unit,'(A)') "CSV load failed: " // trim(error_msg)
        else
            ! Verify data
            if (df%n_dims /= 2) test_passed = .false.  ! rows x columns
            if (df%shape(1) /= 3) test_passed = .false.  ! 3 rows
            if (df%shape(2) /= 3) test_passed = .false.  ! 3 columns
            
            ! Check column names were read
            if (df%n_attrs < 3) test_passed = .false.
        end if
        
        ! Clean up
        call finalize_dataframe(df)
        open(newunit=unit, file="test_data.csv", status="old")
        close(unit, status="delete")
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: From CSV constructor test"
        else
            write(*,'(A)') "FAIL: From CSV constructor test"
        end if
    end subroutine test_from_csv_constructor
    
    subroutine test_empty_constructor()
        type(dataframe_t) :: df
        character(len=20), dimension(2) :: dim_names
        integer, dimension(2) :: shape
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create empty dataframe with specified dimensions
        dim_names = ["time", "vars"]
        shape = [100, 5]
        
        df = dataframe_empty(dim_names, shape, dtype="real64", stat=stat)
        
        if (stat /= 0) test_passed = .false.
        if (.not. df%initialized) test_passed = .false.
        if (df%n_dims /= 2) test_passed = .false.
        if (any(df%shape /= shape)) test_passed = .false.
        if (df%data%dtype /= 4) test_passed = .false.  ! real64
        if (df%data%n_elements /= 500) test_passed = .false.
        
        ! Data should be initialized to zero
        if (any(df%data%values_r64 /= 0.0_real64)) test_passed = .false.
        
        call finalize_dataframe(df)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Empty constructor test"
        else
            write(*,'(A)') "FAIL: Empty constructor test"
        end if
    end subroutine test_empty_constructor
    
    subroutine test_scalar_constructor()
        type(dataframe_t) :: df
        real(real64) :: scalar_value
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Create scalar dataframe
        scalar_value = 42.0_real64
        df = dataframe_scalar(scalar_value, var_name="answer", stat=stat)
        
        if (stat /= 0) test_passed = .false.
        if (df%n_dims /= 0) test_passed = .false.  ! Scalar has 0 dimensions
        if (df%n_elements /= 1) test_passed = .false.
        if (allocated(df%shape)) test_passed = .false.  ! No shape for scalar
        if (trim(df%var_name) /= "answer") test_passed = .false.
        if (abs(df%data%values_r64(1) - 42.0_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
        end if
        
        call finalize_dataframe(df)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Scalar constructor test"
        else
            write(*,'(A)') "FAIL: Scalar constructor test"
        end if
    end subroutine test_scalar_constructor
    
    subroutine test_error_messages()
        type(dataframe_t) :: df
        real(real64), dimension(5, 5) :: data_2d
        character(len=20), dimension(3) :: dim_names  ! Wrong size!
        character(len=256) :: error_msg
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test dimension count mismatch
        dim_names = ["x", "y", "z"]
        df = dataframe(data_2d, dim_names=dim_names, stat=stat, error_msg=error_msg)
        
        if (stat == 0) then
            test_passed = .false.
        else
            ! Verify error message is informative
            if (len_trim(error_msg) < 10) test_passed = .false.
            if (index(error_msg, "dimension") == 0) test_passed = .false.
        end if
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Error messages test"
        else
            write(*,'(A)') "FAIL: Error messages test"
        end if
    end subroutine test_error_messages
    
    subroutine test_edge_cases()
        type(dataframe_t) :: df
        real(real64), dimension(:), allocatable :: data_1d
        real(real64), dimension(1, 1) :: single_element
        logical :: test_passed
        integer :: stat
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        ! Test single element dataframe
        single_element(1,1) = 3.14_real64
        df = dataframe_from_array(single_element, stat=stat)
        
        if (stat /= 0) test_passed = .false.
        if (df%n_elements /= 1) test_passed = .false.
        if (abs(df%data%values_r64(1) - 3.14_real64) > epsilon(1.0_real64)) then
            test_passed = .false.
        end if
        call finalize_dataframe(df)
        
        ! Test large array (but not too large for testing)
        allocate(data_1d(100000))
        call random_number(data_1d)
        df = dataframe_from_array(data_1d, dim_name="large", stat=stat)
        
        if (stat /= 0) test_passed = .false.
        if (df%shape(1) /= 100000) test_passed = .false.
        call finalize_dataframe(df)
        deallocate(data_1d)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Edge cases test"
        else
            write(*,'(A)') "FAIL: Edge cases test"
        end if
    end subroutine test_edge_cases

end program test_constructors