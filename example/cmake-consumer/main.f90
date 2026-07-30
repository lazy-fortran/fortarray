program consumer
    use fortarray, only: dp, data_array_t, data_array, mean
    implicit none

    type(data_array_t) :: field, average
    real(dp) :: values(2, 2)

    values = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], shape(values))
    field = data_array(values, ["radius", "theta "])
    average = mean(field, "theta")
    if (any(abs(average%values - [2.0_dp, 3.0_dp]) > 1.0e-12_dp)) error stop 1
end program consumer

