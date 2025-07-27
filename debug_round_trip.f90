program debug_round_trip
    use foxel_types
    use foxel_constructors
    use foxel_csv
    use iso_fortran_env, only: real64
    implicit none
    
    type(variable_t) :: var_orig, var_read
    real(real64), dimension(3,2) :: data
    integer :: stat, i
    
    ! Create original data (same as test)
    data = reshape([1.5_real64, 2.7_real64, 3.9_real64, 4.1_real64, 5.3_real64, 6.8_real64], [3, 2])
    var_orig = variable(data, name="round_trip", dim_names=["rows", "cols"])
    
    write(*,'(A)') "Original data:"
    do i = 1, var_orig%n_elements
        write(*,'(A,I0,A,G20.15)') "  Element ", i, ": ", var_orig%data%values_r64(i)
    end do
    
    ! Write to CSV
    stat = write_csv("debug_round_trip.csv", var_orig)
    write(*,'(A,I0)') "Write status: ", stat
    
    ! Read back
    var_read = read_csv("debug_round_trip.csv", stat=stat)
    write(*,'(A,I0)') "Read status: ", stat
    write(*,'(A,I0,A,I0)') "Read shape: [", var_read%shape(1), ",", var_read%shape(2), "]"
    
    write(*,'(A)') "Read data:"
    do i = 1, var_read%n_elements
        write(*,'(A,I0,A,G20.15)') "  Element ", i, ": ", var_read%data%values_r64(i)
    end do
    
    write(*,'(A)') "Differences:"
    do i = 1, min(var_orig%n_elements, var_read%n_elements)
        write(*,'(A,I0,A,G20.15)') "  Element ", i, " diff: ", abs(var_orig%data%values_r64(i) - var_read%data%values_r64(i))
    end do
    
end program debug_round_trip