module fortarray_core
    use, intrinsic :: iso_c_binding, only: c_double, c_int64_t
    use, intrinsic :: iso_fortran_env, only: int64, real64
    !$ use omp_lib, only: omp_get_max_threads
    implicit none
    private

    integer, parameter, public :: dp = real64
    integer, parameter, public :: FORTARRAY_SUCCESS = 0
    integer, parameter, public :: FORTARRAY_EINVAL = 1
    integer, parameter, public :: FORTARRAY_ENOTFOUND = 2
    integer, parameter, public :: FORTARRAY_ESHAPE = 3
    integer, parameter :: NAME_LENGTH = 256
    integer, parameter :: ATTRIBUTE_LENGTH = 1024

    type, public :: coordinate_t
        character(len=NAME_LENGTH) :: name = ""
        real(dp), allocatable :: values(:)
    end type coordinate_t

    type, public :: attribute_t
        character(len=NAME_LENGTH) :: name = ""
        character(len=ATTRIBUTE_LENGTH) :: value = ""
    end type attribute_t

    type, public :: data_array_t
        character(len=NAME_LENGTH) :: name = ""
        real(dp), allocatable :: values(:)
        integer, allocatable :: shape(:)
        integer, allocatable :: strides(:)
        character(len=:), allocatable :: dims(:)
        type(coordinate_t), allocatable :: coords(:)
        type(attribute_t), allocatable :: attrs(:)
    contains
        procedure :: valid => array_valid
        procedure :: size => array_size
        procedure :: rank => array_rank
        procedure :: dim_index
        procedure :: set_coord
        procedure :: set_attr
        procedure :: isel => array_isel
        procedure :: sel => array_sel
        procedure :: mean => array_mean
    end type data_array_t

    type, public :: dataset_t
        type(data_array_t), allocatable :: variables(:)
        type(attribute_t), allocatable :: attrs(:)
    contains
        procedure :: set => dataset_set
        procedure :: get => dataset_get
        procedure :: has => dataset_has
        procedure :: size => dataset_size
    end type dataset_t

    interface data_array
        module procedure data_array_scalar
        module procedure data_array_rank1
        module procedure data_array_rank2
        module procedure data_array_rank3
        module procedure data_array_rank4
    end interface data_array

    interface mean
        module procedure array_mean_function
    end interface mean

    interface isel
        module procedure array_isel_function
    end interface isel

    interface sel
        module procedure array_sel_function
    end interface sel

    interface operator(+)
        module procedure add_arrays
    end interface operator(+)

    interface
        subroutine add_r64_kernel(count, left, right, output) &
                bind(C, name="fortarray_add_r64")
            import :: c_double, c_int64_t
            integer(c_int64_t), value, intent(in) :: count
            real(c_double), intent(in) :: left(*), right(*)
            real(c_double), intent(out) :: output(*)
        end subroutine add_r64_kernel
    end interface

    public :: data_array, mean, mean_into, isel, sel, add_into, operator(+)

contains

    function data_array_scalar(values, name) result(array)
        real(dp), intent(in) :: values
        character(len=*), intent(in), optional :: name
        type(data_array_t) :: array

        allocate(array%values(1), array%shape(0), array%strides(0))
        allocate(character(len=1) :: array%dims(0))
        array%values(1) = values
        call set_name(array, name)
    end function data_array_scalar

    function data_array_rank1(values, dims, name) result(array)
        real(dp), intent(in) :: values(:)
        character(len=*), intent(in) :: dims(:)
        character(len=*), intent(in), optional :: name
        type(data_array_t) :: array

        call initialize_array(array, values, shape(values), dims, name)
    end function data_array_rank1

    function data_array_rank2(values, dims, name) result(array)
        real(dp), intent(in) :: values(:, :)
        character(len=*), intent(in) :: dims(:)
        character(len=*), intent(in), optional :: name
        type(data_array_t) :: array

        call initialize_array(array, reshape(values, [size(values)]), shape(values), &
            dims, name)
    end function data_array_rank2

    function data_array_rank3(values, dims, name) result(array)
        real(dp), intent(in) :: values(:, :, :)
        character(len=*), intent(in) :: dims(:)
        character(len=*), intent(in), optional :: name
        type(data_array_t) :: array

        call initialize_array(array, reshape(values, [size(values)]), shape(values), &
            dims, name)
    end function data_array_rank3

    function data_array_rank4(values, dims, name) result(array)
        real(dp), intent(in) :: values(:, :, :, :)
        character(len=*), intent(in) :: dims(:)
        character(len=*), intent(in), optional :: name
        type(data_array_t) :: array

        call initialize_array(array, reshape(values, [size(values)]), shape(values), &
            dims, name)
    end function data_array_rank4

    subroutine initialize_array(array, values, array_shape, dims, name)
        type(data_array_t), intent(out) :: array
        real(dp), intent(in) :: values(:)
        integer, intent(in) :: array_shape(:)
        character(len=*), intent(in) :: dims(:)
        character(len=*), intent(in), optional :: name

        if (size(dims) /= size(array_shape)) return
        if (has_duplicate_names(dims)) return
        array%values = values
        array%shape = array_shape
        array%strides = make_strides(array_shape)
        array%dims = dims
        call set_name(array, name)
    end subroutine initialize_array

    subroutine set_name(array, name)
        type(data_array_t), intent(inout) :: array
        character(len=*), intent(in), optional :: name

        if (present(name)) then
            array%name = name
        else
            array%name = ""
        end if
    end subroutine set_name

    pure function make_strides(array_shape) result(strides)
        integer, intent(in) :: array_shape(:)
        integer, allocatable :: strides(:)
        integer :: i

        allocate(strides(size(array_shape)))
        if (size(strides) == 0) return
        strides(1) = 1
        do i = 2, size(strides)
            strides(i) = strides(i - 1)*array_shape(i - 1)
        end do
    end function make_strides

    pure logical function has_duplicate_names(names)
        character(len=*), intent(in) :: names(:)
        integer :: i, j

        has_duplicate_names = .false.
        do i = 1, size(names)
            do j = i + 1, size(names)
                if (names(i) == names(j)) then
                    has_duplicate_names = .true.
                    return
                end if
            end do
        end do
    end function has_duplicate_names

    pure logical function array_valid(this)
        class(data_array_t), intent(in) :: this
        integer(int64) :: expected

        array_valid = .false.
        if (.not. allocated(this%values)) return
        if (.not. allocated(this%shape)) return
        if (.not. allocated(this%strides)) return
        if (.not. allocated(this%dims)) return
        if (size(this%shape) /= size(this%dims)) return
        if (size(this%shape) /= size(this%strides)) return
        if (any(this%shape < 0)) return
        if (has_duplicate_names(this%dims)) return
        expected = product(int(this%shape, int64))
        if (size(this%shape) == 0) expected = 1_int64
        array_valid = expected == size(this%values, kind=int64)
    end function array_valid

    pure integer function array_size(this)
        class(data_array_t), intent(in) :: this

        if (allocated(this%values)) then
            array_size = size(this%values)
        else
            array_size = 0
        end if
    end function array_size

    pure integer function array_rank(this)
        class(data_array_t), intent(in) :: this

        if (allocated(this%shape)) then
            array_rank = size(this%shape)
        else
            array_rank = -1
        end if
    end function array_rank

    pure integer function dim_index(this, name)
        class(data_array_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: i

        dim_index = 0
        if (.not. allocated(this%dims)) return
        do i = 1, size(this%dims)
            if (this%dims(i) == name) then
                dim_index = i
                return
            end if
        end do
    end function dim_index

    subroutine set_coord(this, name, values, stat)
        class(data_array_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(dp), intent(in) :: values(:)
        integer, intent(out), optional :: stat
        type(coordinate_t), allocatable :: next(:)
        integer :: axis, i

        if (present(stat)) stat = FORTARRAY_EINVAL
        axis = this%dim_index(name)
        if (axis == 0) return
        if (size(values) /= this%shape(axis)) return
        if (allocated(this%coords)) then
            do i = 1, size(this%coords)
                if (this%coords(i)%name == name) then
                    this%coords(i)%values = values
                    if (present(stat)) stat = FORTARRAY_SUCCESS
                    return
                end if
            end do
            allocate(next(size(this%coords) + 1))
            next(1:size(this%coords)) = this%coords
            call move_alloc(next, this%coords)
        else
            allocate(this%coords(1))
        end if
        this%coords(size(this%coords))%values = values
        this%coords(size(this%coords))%name = name
        if (present(stat)) stat = FORTARRAY_SUCCESS
    end subroutine set_coord

    subroutine set_attr(this, name, value)
        class(data_array_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: value
        type(attribute_t), allocatable :: next(:)
        integer :: i

        if (allocated(this%attrs)) then
            do i = 1, size(this%attrs)
                if (this%attrs(i)%name == name) then
                    this%attrs(i)%value = value
                    return
                end if
            end do
            allocate(next(size(this%attrs) + 1))
            next(1:size(this%attrs)) = this%attrs
            call move_alloc(next, this%attrs)
        else
            allocate(this%attrs(1))
        end if
        this%attrs(size(this%attrs))%value = value
        this%attrs(size(this%attrs))%name = name
    end subroutine set_attr

    function array_isel(this, dim, index, stat) result(output)
        class(data_array_t), intent(in) :: this
        character(len=*), intent(in) :: dim
        integer, intent(in) :: index
        integer, intent(out), optional :: stat
        type(data_array_t) :: output

        call isel_into(this, dim, index, output, stat)
    end function array_isel

    function array_isel_function(input, dim, index, stat) result(output)
        type(data_array_t), intent(in) :: input
        character(len=*), intent(in) :: dim
        integer, intent(in) :: index
        integer, intent(out), optional :: stat
        type(data_array_t) :: output

        call isel_into(input, dim, index, output, stat)
    end function array_isel_function

    subroutine isel_into(input, dim, index, output, stat)
        type(data_array_t), intent(in) :: input
        character(len=*), intent(in) :: dim
        integer, intent(in) :: index
        type(data_array_t), intent(out) :: output
        integer, intent(out), optional :: stat
        integer, allocatable :: out_index(:), in_index(:)
        integer :: axis, linear, j

        if (present(stat)) stat = FORTARRAY_EINVAL
        axis = input%dim_index(dim)
        if (axis == 0) return
        if (index < 1 .or. index > input%shape(axis)) return
        output%name = input%name
        output%shape = [input%shape(:axis - 1), input%shape(axis + 1:)]
        output%strides = make_strides(output%shape)
        allocate(character(len=len(input%dims)) :: output%dims(size(input%dims) - 1))
        if (axis > 1) output%dims(:axis - 1) = input%dims(:axis - 1)
        if (axis < size(input%dims)) output%dims(axis:) = input%dims(axis + 1:)
        allocate(output%values(product(output%shape)))
        if (size(output%shape) == 0) then
            deallocate(output%values)
            allocate(output%values(1))
        end if
        allocate(out_index(size(output%shape)), in_index(size(input%shape)))
        do linear = 1, size(output%values)
            call decode_index(linear, output%shape, output%strides, out_index)
            in_index(axis) = index
            do j = 1, axis - 1
                in_index(j) = out_index(j)
            end do
            do j = axis + 1, size(in_index)
                in_index(j) = out_index(j - 1)
            end do
            output%values(linear) = input%values(encode_index(in_index, input%strides))
        end do
        call copy_remaining_metadata(input, output, removed_dim=dim)
        if (present(stat)) stat = FORTARRAY_SUCCESS
    end subroutine isel_into

    function array_sel(this, dim, value, method, stat) result(output)
        class(data_array_t), intent(in) :: this
        character(len=*), intent(in) :: dim
        real(dp), intent(in) :: value
        character(len=*), intent(in), optional :: method
        integer, intent(out), optional :: stat
        type(data_array_t) :: output

        call sel_into(this, dim, value, output, method, stat)
    end function array_sel

    function array_sel_function(input, dim, value, method, stat) result(output)
        type(data_array_t), intent(in) :: input
        character(len=*), intent(in) :: dim
        real(dp), intent(in) :: value
        character(len=*), intent(in), optional :: method
        integer, intent(out), optional :: stat
        type(data_array_t) :: output

        call sel_into(input, dim, value, output, method, stat)
    end function array_sel_function

    subroutine sel_into(input, dim, value, output, method, stat)
        type(data_array_t), intent(in) :: input
        character(len=*), intent(in) :: dim
        real(dp), intent(in) :: value
        type(data_array_t), intent(out) :: output
        character(len=*), intent(in), optional :: method
        integer, intent(out), optional :: stat
        character(len=:), allocatable :: choice
        integer :: coordinate_index, i, selected

        if (present(stat)) stat = FORTARRAY_ENOTFOUND
        if (.not. allocated(input%coords)) return
        coordinate_index = 0
        do i = 1, size(input%coords)
            if (input%coords(i)%name == dim) coordinate_index = i
        end do
        if (coordinate_index == 0) return
        choice = "exact"
        if (present(method)) choice = method
        selected = 0
        if (choice == "nearest") then
            selected = minloc(abs(input%coords(coordinate_index)%values - value), dim=1)
        else
            do i = 1, size(input%coords(coordinate_index)%values)
                if (input%coords(coordinate_index)%values(i) == value) then
                    selected = i
                    exit
                end if
            end do
        end if
        if (selected == 0) return
        call isel_into(input, dim, selected, output, stat)
    end subroutine sel_into

    function array_mean(this, dim, stat) result(output)
        class(data_array_t), intent(in) :: this
        character(len=*), intent(in) :: dim
        integer, intent(out), optional :: stat
        type(data_array_t) :: output

        call mean_into(this, dim, output, stat)
    end function array_mean

    function array_mean_function(input, dim, stat) result(output)
        type(data_array_t), intent(in) :: input
        character(len=*), intent(in) :: dim
        integer, intent(out), optional :: stat
        type(data_array_t) :: output

        call mean_into(input, dim, output, stat)
    end function array_mean_function

    subroutine mean_into(input, dim, output, stat)
        type(data_array_t), intent(in) :: input
        character(len=*), intent(in) :: dim
        type(data_array_t), intent(inout) :: output
        integer, intent(out), optional :: stat
        integer :: axis, base, inner, inner_index, k, outer, outer_index, required
        logical :: layout_matches, use_parallel
        real(dp) :: total

        if (present(stat)) stat = FORTARRAY_ENOTFOUND
        axis = input%dim_index(dim)
        if (axis == 0) return
        required = size(input%values)/input%shape(axis)
        layout_matches = reduced_layout_matches(input, axis, output)
        if (.not. layout_matches) then
            if (allocated(output%shape)) deallocate(output%shape)
            if (allocated(output%strides)) deallocate(output%strides)
            if (allocated(output%dims)) deallocate(output%dims)
            allocate(output%shape(size(input%shape) - 1))
            if (axis > 1) output%shape(:axis - 1) = input%shape(:axis - 1)
            if (axis < size(input%shape)) output%shape(axis:) = input%shape(axis + 1:)
            output%strides = make_strides(output%shape)
            allocate(character(len=len(input%dims)) :: output%dims(size(input%dims) - 1))
            if (axis > 1) output%dims(:axis - 1) = input%dims(:axis - 1)
            if (axis < size(input%dims)) output%dims(axis:) = input%dims(axis + 1:)
        end if
        if (allocated(output%values)) then
            if (size(output%values) /= required) deallocate(output%values)
        end if
        if (.not. allocated(output%values)) allocate(output%values(required))
        output%name = input%name
        inner = input%strides(axis)
        outer = size(input%values)/(inner*input%shape(axis))
        use_parallel = .false.
        !$ use_parallel = omp_get_max_threads() > 1
        if (required <= 16384) use_parallel = .false.
        if (use_parallel) then
            !$omp parallel do simd collapse(2) default(none) shared(input, output, &
            !$omp& axis, inner, outer) &
            !$omp& private(outer_index, inner_index, base, k, total)
            do outer_index = 1, outer
                do inner_index = 1, inner
                    base = (outer_index - 1)*inner*input%shape(axis) + inner_index
                    total = 0.0_dp
                    do k = 0, input%shape(axis) - 1
                        total = total + input%values(base + k*inner)
                    end do
                    output%values((outer_index - 1)*inner + inner_index) = &
                        total/real(input%shape(axis), dp)
                end do
            end do
            !$omp end parallel do simd
        else
            !$omp simd collapse(2) private(base, k, total)
            do outer_index = 1, outer
                do inner_index = 1, inner
                    base = (outer_index - 1)*inner*input%shape(axis) + inner_index
                    total = 0.0_dp
                    do k = 0, input%shape(axis) - 1
                        total = total + input%values(base + k*inner)
                    end do
                    output%values((outer_index - 1)*inner + inner_index) = &
                        total/real(input%shape(axis), dp)
                end do
            end do
            !$omp end simd
        end if
        call copy_remaining_metadata(input, output, removed_dim=dim)
        if (present(stat)) stat = FORTARRAY_SUCCESS
    end subroutine mean_into

    pure logical function reduced_layout_matches(input, axis, output)
        type(data_array_t), intent(in) :: input
        integer, intent(in) :: axis
        type(data_array_t), intent(in) :: output

        reduced_layout_matches = .false.
        if (.not. allocated(output%shape) .or. .not. allocated(output%dims)) return
        if (size(output%shape) /= size(input%shape) - 1) return
        if (axis > 1) then
            if (any(output%shape(:axis - 1) /= input%shape(:axis - 1))) return
            if (any(output%dims(:axis - 1) /= input%dims(:axis - 1))) return
        end if
        if (axis < size(input%shape)) then
            if (any(output%shape(axis:) /= input%shape(axis + 1:))) return
            if (any(output%dims(axis:) /= input%dims(axis + 1:))) return
        end if
        reduced_layout_matches = .true.
    end function reduced_layout_matches

    function add_arrays(left, right) result(output)
        type(data_array_t), intent(in) :: left, right
        type(data_array_t) :: output

        call add_into(left, right, output)
    end function add_arrays

    subroutine add_into(left, right, output, stat)
        type(data_array_t), intent(in) :: left, right
        type(data_array_t), intent(inout) :: output
        integer, intent(out), optional :: stat
        integer, allocatable :: left_map(:), right_map(:), result_index(:)
        integer, allocatable :: left_index(:), right_index(:), result_shape(:)
        character(len=:), allocatable :: result_dims(:)
        integer :: i, axis, linear, required
        logical :: use_parallel

        if (present(stat)) stat = FORTARRAY_ESHAPE
        if (same_layout(left, right)) then
            required = size(left%values)
            call prepare_output_layout(output, left%name, left%dims, left%shape, required)
            use_parallel = .false.
            !$ use_parallel = omp_get_max_threads() > 1
            if (required <= 16384) use_parallel = .false.
            if (use_parallel) then
                !$omp parallel do simd
                do linear = 1, required
                    output%values(linear) = left%values(linear) + right%values(linear)
                end do
                !$omp end parallel do simd
            else
                call add_r64_kernel(int(required, c_int64_t), left%values, right%values, &
                    output%values)
            end if
            if (present(stat)) stat = FORTARRAY_SUCCESS
            return
        end if
        call aligned_shape(left, right, result_dims, result_shape, left_map, right_map)
        if (.not. allocated(result_shape)) return
        required = product(result_shape)
        if (size(result_shape) == 0) required = 1
        call prepare_output_layout(output, left%name, result_dims, result_shape, required)
        allocate(result_index(size(result_shape)))
        allocate(left_index(size(left%shape)), right_index(size(right%shape)))
        do linear = 1, required
            call decode_index(linear, output%shape, output%strides, result_index)
            do i = 1, size(left_map)
                axis = left_map(i)
                left_index(i) = result_index(axis)
            end do
            do i = 1, size(right_map)
                axis = right_map(i)
                right_index(i) = result_index(axis)
            end do
            output%values(linear) = &
                left%values(encode_index(left_index, left%strides)) + &
                right%values(encode_index(right_index, right%strides))
        end do
        if (present(stat)) stat = FORTARRAY_SUCCESS
    end subroutine add_into

    subroutine prepare_output_layout(output, name, dims, array_shape, required)
        type(data_array_t), intent(inout) :: output
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: dims(:)
        integer, intent(in) :: array_shape(:), required
        logical :: layout_matches

        layout_matches = allocated(output%dims) .and. allocated(output%shape)
        if (layout_matches) layout_matches = size(output%shape) == size(array_shape)
        if (layout_matches) layout_matches = all(output%shape == array_shape)
        if (layout_matches) layout_matches = size(output%dims) == size(dims)
        if (layout_matches) layout_matches = all(output%dims == dims)
        if (.not. layout_matches) then
            output%dims = dims
            output%shape = array_shape
            output%strides = make_strides(array_shape)
        end if
        if (allocated(output%values)) then
            if (size(output%values) /= required) deallocate(output%values)
        end if
        if (.not. allocated(output%values)) allocate(output%values(required))
        output%name = name
    end subroutine prepare_output_layout

    subroutine aligned_shape(left, right, dims, array_shape, left_map, right_map)
        type(data_array_t), intent(in) :: left, right
        character(len=:), allocatable, intent(out) :: dims(:)
        integer, allocatable, intent(out) :: array_shape(:), left_map(:), right_map(:)
        integer :: i, j, n, width

        n = size(left%dims)
        do i = 1, size(right%dims)
            if (left%dim_index(right%dims(i)) == 0) n = n + 1
        end do
        width = 1
        if (size(left%dims) > 0) width = max(width, len(left%dims))
        if (size(right%dims) > 0) width = max(width, len(right%dims))
        allocate(character(len=width) :: dims(n))
        allocate(array_shape(n), left_map(size(left%dims)), right_map(size(right%dims)))
        dims(:size(left%dims)) = left%dims
        array_shape(:size(left%shape)) = left%shape
        do i = 1, size(left%dims)
            left_map(i) = i
        end do
        j = size(left%dims)
        do i = 1, size(right%dims)
            right_map(i) = find_name(dims(:j), right%dims(i))
            if (right_map(i) == 0) then
                j = j + 1
                dims(j) = right%dims(i)
                array_shape(j) = right%shape(i)
                right_map(i) = j
            else if (array_shape(right_map(i)) /= right%shape(i)) then
                deallocate(dims, array_shape, left_map, right_map)
                return
            end if
        end do
    end subroutine aligned_shape

    pure integer function find_name(names, name)
        character(len=*), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: i

        find_name = 0
        do i = 1, size(names)
            if (names(i) == name) then
                find_name = i
                return
            end if
        end do
    end function find_name

    pure logical function same_layout(left, right)
        type(data_array_t), intent(in) :: left, right

        same_layout = .false.
        if (size(left%shape) /= size(right%shape)) return
        if (any(left%shape /= right%shape)) return
        if (any(left%dims /= right%dims)) return
        same_layout = .true.
    end function same_layout

    pure subroutine decode_index(linear, array_shape, strides, index)
        integer, intent(in) :: linear
        integer, intent(in) :: array_shape(:), strides(:)
        integer, intent(out) :: index(:)
        integer :: i, remainder

        remainder = linear - 1
        do i = size(array_shape), 1, -1
            index(i) = remainder/strides(i) + 1
            remainder = mod(remainder, strides(i))
        end do
    end subroutine decode_index

    pure integer function encode_index(index, strides)
        integer, intent(in) :: index(:), strides(:)

        encode_index = 1 + sum((index - 1)*strides)
    end function encode_index

    subroutine copy_remaining_metadata(input, output, removed_dim)
        type(data_array_t), intent(in) :: input
        type(data_array_t), intent(inout) :: output
        character(len=*), intent(in) :: removed_dim
        integer :: i, n

        if (allocated(output%coords)) deallocate(output%coords)
        if (allocated(output%attrs)) deallocate(output%attrs)
        if (allocated(input%attrs)) output%attrs = input%attrs
        if (.not. allocated(input%coords)) return
        n = 0
        do i = 1, size(input%coords)
            if (input%coords(i)%name /= removed_dim) n = n + 1
        end do
        if (n == 0) return
        allocate(output%coords(n))
        n = 0
        do i = 1, size(input%coords)
            if (input%coords(i)%name /= removed_dim) then
                n = n + 1
                output%coords(n) = input%coords(i)
            end if
        end do
    end subroutine copy_remaining_metadata

    subroutine dataset_set(this, name, array)
        class(dataset_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(data_array_t), intent(in) :: array
        type(data_array_t), allocatable :: next(:)
        integer :: i

        if (allocated(this%variables)) then
            do i = 1, size(this%variables)
                if (this%variables(i)%name == name) then
                    this%variables(i) = array
                    this%variables(i)%name = name
                    return
                end if
            end do
            allocate(next(size(this%variables) + 1))
            next(1:size(this%variables)) = this%variables
            call move_alloc(next, this%variables)
        else
            allocate(this%variables(1))
        end if
        this%variables(size(this%variables)) = array
        this%variables(size(this%variables))%name = name
    end subroutine dataset_set

    function dataset_get(this, name, stat) result(array)
        class(dataset_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(out), optional :: stat
        type(data_array_t) :: array
        integer :: i

        if (present(stat)) stat = FORTARRAY_ENOTFOUND
        if (.not. allocated(this%variables)) return
        do i = 1, size(this%variables)
            if (this%variables(i)%name == name) then
                array = this%variables(i)
                if (present(stat)) stat = FORTARRAY_SUCCESS
                return
            end if
        end do
    end function dataset_get

    pure logical function dataset_has(this, name)
        class(dataset_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: i

        dataset_has = .false.
        if (.not. allocated(this%variables)) return
        do i = 1, size(this%variables)
            if (this%variables(i)%name == name) then
                dataset_has = .true.
                return
            end if
        end do
    end function dataset_has

    pure integer function dataset_size(this)
        class(dataset_t), intent(in) :: this

        if (allocated(this%variables)) then
            dataset_size = size(this%variables)
        else
            dataset_size = 0
        end if
    end function dataset_size

end module fortarray_core
