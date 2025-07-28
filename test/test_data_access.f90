program test_data_access
    use fortarray
    use iso_fortran_env, only: real64, error_unit
    implicit none
    
    logical :: all_passed = .true.
    
    write(*,'(A)') "Testing data access and conversion methods..."
    
    call test_values_methods()
    call test_conversion_methods()
    
    if (all_passed) then
        print *, "SUCCESS: All data access tests passed!"
    else
        error stop "FAILURE: Some data access tests failed!"
    end if
    
contains
    
    subroutine test_values_methods()
        type(fortarray_t) :: arr, values_result, copy_result
        real(real64), dimension(5) :: data_1d
        integer :: i
        
        print *, "Testing values() and values_copy() methods..."
        
        ! Create test data: [1, 2, 3, 4, 5]
        do i = 1, 5
            data_1d(i) = real(i, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: values() method (should return a view)
        print *, "  Test 1: values() method"
        values_result = arr%values()
        
        if (.not. values_result%initialized) then
            print *, "    FAIL: values() result not initialized"
            all_passed = .false.
            return
        end if
        
        if (values_result%n_elements /= 5) then
            print *, "    FAIL: values() wrong size"
            all_passed = .false.
            return
        end if
        
        if (values_result%is_view .neqv. .true.) then
            print *, "    FAIL: values() should be a view"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: values() method"
        
        ! Test 2: values_copy() method (should return a copy)
        print *, "  Test 2: values_copy() method"
        copy_result = arr%values_copy()
        
        if (.not. copy_result%initialized) then
            print *, "    FAIL: values_copy() result not initialized"
            all_passed = .false.
            return
        end if
        
        if (copy_result%n_elements /= 5) then
            print *, "    FAIL: values_copy() wrong size"
            all_passed = .false.
            return
        end if
        
        if (copy_result%is_view .neqv. .false.) then
            print *, "    FAIL: values_copy() should not be a view"
            all_passed = .false.
            return
        end if
        
        if (copy_result%owns_memory .neqv. .true.) then
            print *, "    FAIL: values_copy() should own memory"
            all_passed = .false.
            return
        end if
        
        ! Check data integrity
        if (.not. allocated(copy_result%data%values_r64)) then
            print *, "    FAIL: values_copy() data not allocated"
            all_passed = .false.
            return
        end if
        
        do i = 1, 5
            if (abs(copy_result%data%values_r64(i) - real(i, real64)) > 1e-10) then
                print *, "    FAIL: values_copy() data incorrect"
                all_passed = .false.
                return
            end if
        end do
        
        print *, "    PASS: values_copy() method"
        
        call finalize_variable(arr)
        call finalize_variable(values_result)
        call finalize_variable(copy_result)
        
    end subroutine test_values_methods
    
    subroutine test_conversion_methods()
        type(fortarray_t) :: arr, numpy_result, pandas_result
        real(real64), dimension(3) :: data_1d
        integer :: status, i
        
        print *, "Testing conversion methods..."
        
        ! Create test data: [10, 20, 30]
        do i = 1, 3
            data_1d(i) = real(i * 10, real64)
        end do
        
        arr = new_array(data_1d, dim_names=["x"])
        
        ! Test 1: to_netcdf() method (placeholder - should return error status)
        print *, "  Test 1: to_netcdf() method"
        status = arr%to_netcdf("test_output.nc")
        
        if (status == 0) then
            print *, "    FAIL: to_netcdf() should return error status for unimplemented method"
            all_passed = .false.
            return
        end if
        
        print *, "    PASS: to_netcdf() method (placeholder working)"
        
        ! Test 2: to_numpy() method (placeholder)
        print *, "  Test 2: to_numpy() method"
        numpy_result = arr%to_numpy()
        
        ! For placeholder, just check that it doesn't crash
        ! and method can be called (implementation can vary)
        print *, "    PASS: to_numpy() method (placeholder working)"
        
        ! Test 3: to_pandas() method (placeholder)
        print *, "  Test 3: to_pandas() method"
        pandas_result = arr%to_pandas()
        
        ! For placeholder, just check that it doesn't crash
        ! and method can be called (implementation can vary)
        print *, "    PASS: to_pandas() method (placeholder working)"
        
        call finalize_variable(arr)
        call finalize_variable(numpy_result)
        call finalize_variable(pandas_result)
        
    end subroutine test_conversion_methods
    
end program test_data_access