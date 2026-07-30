program test_thread_safety
    use fortarray, only: dp, data_array_t, data_array, mean_into
    implicit none

    integer, parameter :: nfields = 32
    type(data_array_t) :: inputs(nfields)
    real(dp) :: values(64, 128)
    integer :: failure_count, i, field

    values = reshape([(real(i, dp), i=1, size(values))], shape(values))
    do field = 1, nfields
        inputs(field) = data_array(values + real(field, dp), ["radius", "theta "])
    end do

    failure_count = 0
    !$omp parallel do default(none) shared(inputs, failure_count)
    do field = 1, nfields
        call verify_mean(inputs(field), field, failure_count)
    end do
    !$omp end parallel do
    if (failure_count /= 0) error stop "concurrent reduction produced a wrong value"

contains

    subroutine verify_mean(input, field_number, failures)
        type(data_array_t), intent(in) :: input
        integer, intent(in) :: field_number
        integer, intent(inout) :: failures
        type(data_array_t) :: output
        logical :: correct
        integer :: index

        call mean_into(input, "theta", output)
        correct = .true.
        do index = 1, 64
            if (abs(output%values(index) - &
                (real(index, dp) + 4064.0_dp + real(field_number, dp))) > &
                1.0e-12_dp) then
                correct = .false.
            end if
        end do
        if (.not. correct) then
            !$omp atomic update
            failures = failures + 1
        end if
    end subroutine verify_mean

end program test_thread_safety
