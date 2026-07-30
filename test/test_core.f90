program test_core
    use fortarray, only: dp, data_array_t, dataset_t, data_array, mean, mean_into, &
        sel, operator(+), FORTARRAY_SUCCESS
    implicit none

    type(data_array_t) :: field, radial_slice, angular_mean, sum_field
    type(dataset_t) :: dataset
    real(dp) :: values(2, 3, 2)
    real(dp) :: radial(2), expected_slice(6), expected_mean(4)
    integer :: i, j, k, stat

    do k = 1, 2
        do j = 1, 3
            do i = 1, 2
                values(i, j, k) = real(100*k + 10*j + i, dp)
            end do
        end do
    end do
    radial = [0.25_dp, 0.75_dp]
    field = data_array(values, ["radius", "theta ", "zeta  "], name="potential")
    call assert_true(field%valid(), "constructed array is valid")
    call assert_true(field%rank() == 3, "rank")
    call assert_true(all(field%shape == [2, 3, 2]), "shape")
    call field%set_coord("radius", radial, stat)
    call assert_true(stat == FORTARRAY_SUCCESS, "coordinate accepted")
    call assert_true(allocated(field%coords), "coordinate allocated")
    call assert_true(field%coords(1)%name == "radius", "coordinate name")
    radial_slice = sel(field, "radius", 0.7_dp, method="nearest", stat=stat)
    call assert_true(stat == FORTARRAY_SUCCESS, "nearest selection status")
    expected_slice = [112.0_dp, 122.0_dp, 132.0_dp, 212.0_dp, 222.0_dp, 232.0_dp]
    call assert_close(radial_slice%values, expected_slice, "nearest selection values")
    call assert_true(radial_slice%dims(1) == "theta", "selection first dim")
    call assert_true(radial_slice%dims(2) == "zeta", "selection second dim")

    angular_mean = mean(field, "theta", stat)
    call assert_true(stat == FORTARRAY_SUCCESS, "mean status")
    expected_mean = [121.0_dp, 122.0_dp, 221.0_dp, 222.0_dp]
    call assert_close(angular_mean%values, expected_mean, "dimension mean")
    call assert_true(angular_mean%dims(1) == "radius", "mean first dim")
    call assert_true(angular_mean%dims(2) == "zeta", "mean second dim")

    call mean_into(field, "theta", angular_mean, stat)
    call assert_close(angular_mean%values, expected_mean, "mean_into reuses output")

    sum_field = field + data_array([1.0_dp, 2.0_dp, 3.0_dp], ["theta"])
    call assert_true(all(sum_field%shape == [2, 3, 2]), "named broadcasting shape")
    call assert_close(sum_field%values(1:6), &
        [112.0_dp, 113.0_dp, 123.0_dp, 124.0_dp, 134.0_dp, 135.0_dp], &
        "named broadcasting values")
    call field%set_attr("units", "V")

    call dataset%set("potential", field)
    call assert_true(dataset%has("potential"), "dataset lookup")
    call assert_true(dataset%size() == 1, "dataset size")
    radial_slice = dataset%get("potential", stat)
    call assert_true(stat == FORTARRAY_SUCCESS, "dataset get status")
    call assert_close(radial_slice%values, field%values, "dataset values")

contains

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            write (*, '(a)') "FAIL: "//message
            error stop 1
        end if
    end subroutine assert_true

    subroutine assert_close(actual, expected, message)
        real(dp), intent(in) :: actual(:), expected(:)
        character(len=*), intent(in) :: message

        call assert_true(size(actual) == size(expected), message//" size")
        call assert_true(maxval(abs(actual - expected)) < 1.0e-12_dp, message)
    end subroutine assert_close

end program test_core
