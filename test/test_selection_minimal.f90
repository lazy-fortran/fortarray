program test_selection_minimal
    use fortarray
    use iso_fortran_env, only: real64
    implicit none
    
    type(fortarray_t) :: arr, result
    
    print *, "Testing minimal selection functionality..."
    
    ! Create test array
    arr = new_array([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64])
    
    ! Test if it was created properly
    if (arr%n_elements == 5) then
        print *, "PASS: Array creation"
    else
        print *, "FAIL: Array creation"
    end if
    
    ! Clean up
    call finalize_variable(arr)
    
    print *, "Test complete."
    
end program test_selection_minimal