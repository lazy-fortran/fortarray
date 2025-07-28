program test_boolean_minimal
    use fortarray_boolean_indexing, only: create_mask_from_logical_array, where_mask_only
    use fortarray_types, only: fortarray_t
    use fortarray_constructors, only: new_array
    use iso_fortran_env, only: real64, error_unit
    implicit none
    
    type(fortarray_t) :: var, mask, result
    real(real64), dimension(3) :: data
    logical, dimension(3) :: mask_data
    logical :: success
    
    success = .true.
    
    ! Create simple test data
    data = [1.0_real64, 2.0_real64, 3.0_real64]
    var = new_array(data, name="test", dim_names=["x"])
    
    ! Create mask
    mask_data = [.true., .false., .true.]
    mask = create_mask_from_logical_array(mask_data, ["x"])
    
    ! Apply mask
    result = where_mask_only(mask, var)
    
    ! Check result
    if (result%n_elements /= 2) then
        success = .false.
        write(error_unit,'(A,I0)') "Expected 2 elements, got: ", result%n_elements
    else
        write(*,'(A)') "SUCCESS: Boolean indexing basic test passed"
    end if
    
    if (.not. success) then
        error stop 1
    end if
    
end program test_boolean_minimal