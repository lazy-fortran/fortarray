program benchmark_core_kernels
    use fortarray, only: dp, data_array_t, data_array, mean_into, add_into
    implicit none

    integer, parameter :: ninner = 512, nreduce = 8192, repetitions = 6
    integer, parameter :: trials = 7
    type(data_array_t) :: input, right, reduced, added
    real(dp), allocatable :: values(:, :), reference(:), sum_reference(:)
    real(dp) :: fortarray_mean_time, fortarray_add_time
    real(dp) :: raw_mean_time, raw_add_time, checksum
    integer :: i

    allocate(values(ninner, nreduce), reference(ninner))
    allocate(sum_reference(ninner*nreduce))
    values = reshape([(sin(real(i, dp)*1.0e-4_dp), i=1, size(values))], &
        shape(values))
    input = data_array(values, ["radius", "theta "], name="field")
    right = data_array(0.5_dp*values, ["radius", "theta "], name="other")

    call benchmark_raw_mean(values, reference, raw_mean_time)
    call benchmark_fortarray_mean(input, reduced, fortarray_mean_time)
    if (maxval(abs(reference - reduced%values)) > 1.0e-12_dp) then
        error stop "mean benchmark results differ"
    end if

    call benchmark_add_pair(input, right, sum_reference, added, raw_add_time, &
        fortarray_add_time)
    if (maxval(abs(sum_reference - added%values)) > 1.0e-12_dp) then
        error stop "addition benchmark results differ"
    end if

    checksum = sum(reduced%values) + sum(added%values)
    write (*, '(a, f8.3)') "mean fortarray/raw: ", fortarray_mean_time/raw_mean_time
    write (*, '(a, f8.3)') "add  fortarray/raw: ", fortarray_add_time/raw_add_time
    write (*, '(a, es12.4)') "checksum: ", checksum
    ! A small allowance covers timer and dispatch noise; the file-I/O benchmarks
    ! enforce the strict native-library performance requirement in fortio.
    if (fortarray_mean_time > 1.15_dp*raw_mean_time) then
        error stop "fortarray mean kernel is slower than the direct Fortran oracle"
    end if
    if (fortarray_add_time > 1.25_dp*raw_add_time) then
        error stop "fortarray add kernel is slower than the direct Fortran oracle"
    end if

contains

    subroutine benchmark_raw_mean(source, destination, elapsed)
        real(dp), intent(in) :: source(:, :)
        real(dp), intent(out) :: destination(:)
        real(dp), intent(out) :: elapsed
        real(dp) :: started, trial_time
        integer :: repeat, trial

        elapsed = huge(elapsed)
        do trial = 1, trials
            call cpu_time(started)
            do repeat = 1, repetitions
                call raw_mean(source, destination)
            end do
            call cpu_time(trial_time)
            elapsed = min(elapsed, trial_time - started)
        end do
    end subroutine benchmark_raw_mean

    subroutine benchmark_fortarray_mean(source, destination, elapsed)
        type(data_array_t), intent(in) :: source
        type(data_array_t), intent(inout) :: destination
        real(dp), intent(out) :: elapsed
        real(dp) :: started, trial_time
        integer :: repeat, trial

        elapsed = huge(elapsed)
        do trial = 1, trials
            call cpu_time(started)
            do repeat = 1, repetitions
                call mean_into(source, "theta", destination)
            end do
            call cpu_time(trial_time)
            elapsed = min(elapsed, trial_time - started)
        end do
    end subroutine benchmark_fortarray_mean

    subroutine raw_mean(source, destination)
        real(dp), intent(in) :: source(:, :)
        real(dp), intent(out) :: destination(:)
        integer :: inner_index, reduce_index
        real(dp) :: total

        do inner_index = 1, size(source, 1)
            total = 0.0_dp
            do reduce_index = 1, size(source, 2)
                total = total + source(inner_index, reduce_index)
            end do
            destination(inner_index) = total/real(size(source, 2), dp)
        end do
    end subroutine raw_mean

    subroutine benchmark_add_pair(source, right_operand, raw_destination, destination, &
            raw_elapsed, fortarray_elapsed)
        type(data_array_t), intent(in) :: source, right_operand
        real(dp), intent(out) :: raw_destination(:)
        type(data_array_t), intent(inout) :: destination
        real(dp), intent(out) :: raw_elapsed, fortarray_elapsed
        real(dp) :: started, trial_time
        real(dp), volatile :: sink
        integer :: repeat, trial

        raw_elapsed = 0.0_dp
        fortarray_elapsed = 0.0_dp
        sink = 0.0_dp
        do trial = 1, trials
            if (mod(trial, 2) == 0) then
                call cpu_time(started)
                do repeat = 1, repetitions
                    call add_into(source, right_operand, destination)
                    sink = sink + destination%values(repeat)
                end do
                call cpu_time(trial_time)
                fortarray_elapsed = fortarray_elapsed + trial_time - started
                call cpu_time(started)
                do repeat = 1, repetitions
                    raw_destination = source%values + right_operand%values
                    sink = sink + raw_destination(repeat)
                end do
                call cpu_time(trial_time)
                raw_elapsed = raw_elapsed + trial_time - started
            else
                call cpu_time(started)
                do repeat = 1, repetitions
                    raw_destination = source%values + right_operand%values
                    sink = sink + raw_destination(repeat)
                end do
                call cpu_time(trial_time)
                raw_elapsed = raw_elapsed + trial_time - started
                call cpu_time(started)
                do repeat = 1, repetitions
                    call add_into(source, right_operand, destination)
                    sink = sink + destination%values(repeat)
                end do
                call cpu_time(trial_time)
                fortarray_elapsed = fortarray_elapsed + trial_time - started
            end if
        end do
        if (sink == huge(sink)) error stop "unreachable benchmark sink"
    end subroutine benchmark_add_pair

end program benchmark_core_kernels
