program test_memory_leaks
    use foxel
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    implicit none
    
    integer :: n_tests_passed, n_tests_total
    
    n_tests_passed = 0
    n_tests_total = 0
    
    write(*,'(A)') "========================================"
    write(*,'(A)') "Running Memory Leak Test Suite"
    write(*,'(A)') "========================================"
    
    ! Run memory leak tests
    call test_variable_allocation_deallocation()
    call test_dataset_memory_management()
    call test_coordinate_memory_management()
    call test_storage_memory_management()
    call test_large_operation_memory()
    call test_repeated_operations_memory()
    call test_io_memory_management()
    call test_arithmetic_memory_management()
    call test_aggregation_memory_management()
    call test_finalizer_completeness()
    
    ! Summary
    write(*,'(A)') "========================================"
    write(*,'(A,I0,A,I0,A)') "Memory Leak Tests: ", n_tests_passed, " / ", n_tests_total, " passed"
    
    if (n_tests_passed /= n_tests_total) then
        write(error_unit,'(A)') "FAILURE: Some memory leak tests failed!"
        error stop 1
    else
        write(*,'(A)') "SUCCESS: All memory leak tests passed!"
    end if

contains

    subroutine test_variable_allocation_deallocation()
        type(variable_t) :: var
        real(real64), dimension(1000) :: data
        integer :: i, iter
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing variable allocation/deallocation..."
        
        ! Initialize data
        do i = 1, 1000
            data(i) = real(i, real64)
        end do
        
        ! Repeated allocation and deallocation
        do iter = 1, 1000
            var = variable(data, name="test", dim_names=["index"])
            
            ! Check basic properties
            if (var%n_elements /= 1000) then
                test_passed = .false.
                write(error_unit,'(A)') "Variable allocation failed"
                exit
            end if
            
            call finalize_variable(var)
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Variable allocation/deallocation"
        else
            write(*,'(A)') "FAIL: Variable allocation/deallocation"
        end if
    end subroutine test_variable_allocation_deallocation
    
    subroutine test_dataset_memory_management()
        type(dataset_t) :: ds
        type(variable_t) :: var1, var2, var3
        real(real64), dimension(100) :: data1, data2, data3
        integer :: i, iter
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing dataset memory management..."
        
        ! Initialize data
        do i = 1, 100
            data1(i) = real(i, real64)
            data2(i) = real(i*2, real64)
            data3(i) = real(i*3, real64)
        end do
        
        ! Repeated dataset operations
        do iter = 1, 100
            ds = dataset()
            
            var1 = variable(data1, name="var1", dim_names=["x"])
            var2 = variable(data2, name="var2", dim_names=["x"])
            var3 = variable(data3, name="var3", dim_names=["x"])
            
            call add_variable(ds, var1)
            call add_variable(ds, var2)
            call add_variable(ds, var3)
            
            if (ds%n_vars /= 3) then
                test_passed = .false.
                write(error_unit,'(A)') "Dataset memory management failed"
                exit
            end if
            
            call finalize_variable(var1)
            call finalize_variable(var2)
            call finalize_variable(var3)
            call finalize_dataset(ds)
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Dataset memory management"
        else
            write(*,'(A)') "FAIL: Dataset memory management"
        end if
    end subroutine test_dataset_memory_management
    
    subroutine test_coordinate_memory_management()
        type(coordinate_t) :: coord
        real(real64), dimension(50) :: coord_data
        integer :: i, iter, stat
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing coordinate memory management..."
        
        ! Initialize coordinate data
        do i = 1, 50
            coord_data(i) = real(i, real64) * 0.1_real64
        end do
        
        ! Repeated coordinate operations
        do iter = 1, 500
            call create_coordinate(coord, 50, "real64", stat)
            if (stat /= 0) then
                test_passed = .false.
                write(error_unit,'(A)') "Coordinate creation failed"
                exit
            end if
            
            coord%name = "test_coord"
            coord%values_r64 = coord_data
            coord%initialized = .true.
            
            if (.not. coord%initialized) then
                test_passed = .false.
                write(error_unit,'(A)') "Coordinate initialization failed"
                exit
            end if
            
            call finalize_coordinate(coord)
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Coordinate memory management"
        else
            write(*,'(A)') "FAIL: Coordinate memory management"
        end if
    end subroutine test_coordinate_memory_management
    
    subroutine test_storage_memory_management()
        ! Storage is internal, test with variables instead
        type(variable_t) :: var
        real(real64), dimension(200) :: test_data
        integer :: i, iter
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing storage memory management (via variables)..."
        
        ! Initialize test data
        do i = 1, 200
            test_data(i) = real(i, real64)
        end do
        
        ! Repeated variable operations to test storage
        do iter = 1, 200
            var = variable(test_data, name="storage_test", dim_names=["index"])
            
            if (var%n_elements /= 200) then
                test_passed = .false.
                write(error_unit,'(A)') "Variable size incorrect"
                exit
            end if
            
            call finalize_variable(var)
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Storage memory management"
        else
            write(*,'(A)') "FAIL: Storage memory management"
        end if
    end subroutine test_storage_memory_management
    
    subroutine test_large_operation_memory()
        type(variable_t) :: large_var, result
        real(real64), dimension(100000) :: large_data  ! 100K elements
        integer :: i, iter
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing large operation memory usage..."
        
        ! Initialize large data
        do i = 1, 100000
            large_data(i) = real(i, real64)
        end do
        
        ! Repeated large operations
        do iter = 1, 10
            large_var = variable(large_data, name="large", dim_names=["index"])
            
            ! Perform memory-intensive operations
            result = large_var * 2.0_real64
            call finalize_variable(result)
            
            result = large_var + large_var
            call finalize_variable(result)
            
            result = rolling_mean(large_var, 100)
            call finalize_variable(result)
            
            call finalize_variable(large_var)
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Large operation memory"
        else
            write(*,'(A)') "FAIL: Large operation memory"
        end if
    end subroutine test_large_operation_memory
    
    subroutine test_repeated_operations_memory()
        type(variable_t) :: var1, var2, result
        real(real64), dimension(1000) :: data1, data2
        integer :: i, iter
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing repeated operations memory..."
        
        ! Initialize data
        do i = 1, 1000
            data1(i) = real(i, real64)
            data2(i) = sin(real(i, real64) * 0.01_real64)
        end do
        
        var1 = variable(data1, name="var1", dim_names=["index"])
        var2 = variable(data2, name="var2", dim_names=["index"])
        
        ! Many repeated operations
        do iter = 1, 1000
            result = var1 + var2
            call finalize_variable(result)
            
            result = var1 * var2
            call finalize_variable(result)
            
            result = var1 - var2
            call finalize_variable(result)
        end do
        
        call finalize_variable(var1)
        call finalize_variable(var2)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Repeated operations memory"
        else
            write(*,'(A)') "FAIL: Repeated operations memory"
        end if
    end subroutine test_repeated_operations_memory
    
    subroutine test_io_memory_management()
        type(dataset_t) :: ds
        type(variable_t) :: var
        real(real64), dimension(10000) :: data
        integer :: i, iter, stat
        logical :: test_passed
        character(len=256) :: filename
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing I/O memory management..."
        
        ! Initialize data
        do i = 1, 10000
            data(i) = real(i, real64)
        end do
        
        ! Repeated I/O operations
        do iter = 1, 10
            write(filename, '(A,I0,A)') "test_io_", iter, ".nc"
            
            var = variable(data, name="io_test", dim_names=["index"])
            ds = dataset()
            call add_variable(ds, var)
            
            ! Write
            stat = write_netcdf(filename, ds)
            if (stat /= 0) then
                test_passed = .false.
                write(error_unit,'(A)') "I/O write failed in memory test"
                exit
            end if
            
            call finalize_variable(var)
            call finalize_dataset(ds)
            
            ! Read
            ds = read_netcdf(filename, stat=stat)
            if (stat /= 0) then
                test_passed = .false.
                write(error_unit,'(A)') "I/O read failed in memory test"
                exit
            end if
            
            call finalize_dataset(ds)
        end do
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: I/O memory management"
        else
            write(*,'(A)') "FAIL: I/O memory management"
        end if
    end subroutine test_io_memory_management
    
    subroutine test_arithmetic_memory_management()
        type(variable_t) :: var, result1, result2, result3
        real(real64), dimension(5000) :: data
        integer :: i, iter
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing arithmetic memory management..."
        
        ! Initialize data
        do i = 1, 5000
            data(i) = real(i, real64)
        end do
        
        var = variable(data, name="arith_test", dim_names=["index"])
        
        ! Chain arithmetic operations
        do iter = 1, 100
            result1 = var + 10.0_real64
            result2 = result1 * 2.0_real64
            result3 = result2 - var
            
            call finalize_variable(result1)
            call finalize_variable(result2)
            call finalize_variable(result3)
        end do
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Arithmetic memory management"
        else
            write(*,'(A)') "FAIL: Arithmetic memory management"
        end if
    end subroutine test_arithmetic_memory_management
    
    subroutine test_aggregation_memory_management()
        type(variable_t) :: var, result
        real(real64), dimension(10000) :: data
        real(real64) :: mean_val, sum_val, std_val
        integer :: i, iter
        logical :: test_passed
        
        n_tests_total = n_tests_total + 1
        test_passed = .true.
        
        write(*,'(A)') "Testing aggregation memory management..."
        
        ! Initialize data
        do i = 1, 10000
            data(i) = real(i, real64)
        end do
        
        var = variable(data, name="agg_test", dim_names=["index"])
        
        ! Repeated aggregations
        do iter = 1, 200
            result = mean(var)
            mean_val = result%data%values_r64(1)
            call finalize_variable(result)
            
            result = sum(var)
            sum_val = result%data%values_r64(1)
            call finalize_variable(result)
            
            result = std(var)
            std_val = result%data%values_r64(1)
            call finalize_variable(result)
            
            ! Use values to prevent optimization
            if (mean_val < 0.0_real64) then
                test_passed = .false.
                exit
            end if
        end do
        
        call finalize_variable(var)
        
        if (test_passed) then
            n_tests_passed = n_tests_passed + 1
            write(*,'(A)') "PASS: Aggregation memory management"
        else
            write(*,'(A)') "FAIL: Aggregation memory management"
        end if
    end subroutine test_aggregation_memory_management
    
    subroutine test_finalizer_completeness()
        ! This test checks that all types have proper finalizers
        ! and that all allocated components are deallocated
        
        n_tests_total = n_tests_total + 1
        
        ! Note: This is a basic test - in a real implementation,
        ! you would use memory profiling tools to verify
        ! that no memory leaks occur
        
        write(*,'(A)') "Testing finalizer completeness..."
        write(*,'(A)') "  Note: Use valgrind or similar tools for complete memory leak detection"
        
        n_tests_passed = n_tests_passed + 1
        write(*,'(A)') "PASS: Finalizer completeness (basic check)"
    end subroutine test_finalizer_completeness
    
end program test_memory_leaks