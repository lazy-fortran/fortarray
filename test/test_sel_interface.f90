program test_sel_interface
    use fortarray
    use fortarray_types
    use fortarray_constructors, only: new_array, create_coordinate
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    type(fortarray_t) :: arr, result
    type(coordinate_t) :: time_coord
    integer :: stat
    logical :: all_passed = .true.
    
    print *, "Testing unified sel/isel interface..."
    
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
    
    print *, ""
    print *, "1. Testing sel() for point selection:"
    ! Point selection using generic sel
    result = arr%sel("time", value=30.0_real64)
    
    if (allocated(result%data%values_r64)) then
        if (abs(result%data%values_r64(1) - 3.0_real64) < 1e-10) then
            print *, "   PASS: sel(time, value=30.0) returned correct value"
        else
            print *, "   FAIL: Wrong value returned"
            all_passed = .false.
        end if
    else
        print *, "   FAIL: No data allocated"
        all_passed = .false.
    end if
    
    print *, ""
    print *, "2. Testing sel() for range selection:"
    ! Range selection using generic sel
    result = arr%sel("time", start_val=20.0_real64, stop_val=40.0_real64)
    
    if (result%n_elements == 3) then
        print *, "   PASS: sel(time, start_val=20.0, stop_val=40.0) returned 3 elements"
    else
        print *, "   FAIL: Wrong number of elements"
        all_passed = .false.
    end if
    
    print *, ""
    print *, "3. Testing isel() for point selection:"
    ! Index point selection using generic isel
    result = arr%isel("time", index=3)
    
    if (allocated(result%data%values_r64)) then
        if (abs(result%data%values_r64(1) - 3.0_real64) < 1e-10) then
            print *, "   PASS: isel(time, index=3) returned correct value"
        else
            print *, "   FAIL: Wrong value returned"
            all_passed = .false.
        end if
    else
        print *, "   FAIL: No data allocated"
        all_passed = .false.
    end if
    
    print *, ""
    print *, "4. Testing isel() for range selection:"
    ! Index range selection using generic isel
    result = arr%isel("time", start_idx=2, stop_idx=4)
    
    if (result%n_elements == 3) then
        print *, "   PASS: isel(time, start_idx=2, stop_idx=4) returned 3 elements"
    else
        print *, "   FAIL: Wrong number of elements"
        all_passed = .false.
    end if
    
    print *, ""
    print *, "5. API improvements over separate sel_point/sel_range:"
    print *, "   OLD: result = arr%sel_point('time', 30.0)"
    print *, "   NEW: result = arr%sel('time', value=30.0)"
    print *, ""
    print *, "   OLD: result = arr%sel_range('time', 20.0, 40.0)"  
    print *, "   NEW: result = arr%sel('time', start_val=20.0, stop_val=40.0)"
    print *, ""
    print *, "   Chaining: result = arr%sel('x', value=3.0)%sel('y', value=20.0)"
    
    ! Clean up
    call finalize_variable(arr)
    
    print *, ""
    if (all_passed) then
        print *, "SUCCESS: All unified interface tests passed!"
    else
        error stop "FAILURE: Some tests failed!"
    end if
    
end program test_sel_interface