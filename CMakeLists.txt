cmake_minimum_required(VERSION 3.22)
project(fortarray VERSION 0.1.0 LANGUAGES C Fortran)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
include(FetchContent)

option(FORTARRAY_ENABLE_OPENMP "Enable OpenMP kernels" ON)
option(FORTARRAY_BUILD_TESTS "Build fortarray tests" ${PROJECT_IS_TOP_LEVEL})
option(FORTARRAY_BUILD_BENCHMARKS "Build performance regression benchmarks" OFF)
option(FORTARRAY_INSTALL "Install fortarray targets and package files" ${PROJECT_IS_TOP_LEVEL})

find_package(fortio CONFIG QUIET)
if(NOT fortio_FOUND)
    set(_fortarray_build_testing "${BUILD_TESTING}")
    set(BUILD_TESTING OFF)
    set(FORTIO_INSTALL "${FORTARRAY_INSTALL}" CACHE BOOL "" FORCE)
    FetchContent_Declare(fortio
        GIT_REPOSITORY https://github.com/lazy-fortran/fortio.git
        GIT_TAG 46e16e7507550bb70a4c392073e20118d554aff5
        GIT_SHALLOW FALSE
    )
    FetchContent_MakeAvailable(fortio)
    set(BUILD_TESTING "${_fortarray_build_testing}")
endif()

if(FORTARRAY_BUILD_BENCHMARKS)
    add_executable(benchmark_core_kernels benchmark/core_kernels.f90)
    target_link_libraries(benchmark_core_kernels PRIVATE fortarray_core)
    if(FORTARRAY_BUILD_TESTS)
        add_test(NAME performance_core COMMAND benchmark_core_kernels)
        set_tests_properties(performance_core PROPERTIES
            ENVIRONMENT "OMP_NUM_THREADS=1"
            RUN_SERIAL TRUE
        )
    endif()
endif()

add_library(fortarray_core
    src/fortarray_kernels.c
    src/fortarray_core.f90
    src/fortarray.f90
)
add_library(fortarray::core ALIAS fortarray_core)
set_target_properties(fortarray_core PROPERTIES
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules"
    OUTPUT_NAME fortarray
    EXPORT_NAME core
)
target_include_directories(fortarray_core PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/fortarray>
)

add_library(fortarray_io src/fortarray_io.f90)
add_library(fortarray::io ALIAS fortarray_io)
set_target_properties(fortarray_io PROPERTIES
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules"
    EXPORT_NAME io
)
target_include_directories(fortarray_io PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/fortarray>
)
target_link_libraries(fortarray_io PUBLIC fortarray_core fortio::fortio)

add_library(fortarray_all INTERFACE)
add_library(fortarray::fortarray ALIAS fortarray_all)
set_target_properties(fortarray_all PROPERTIES EXPORT_NAME fortarray)
target_link_libraries(fortarray_all INTERFACE fortarray_core fortarray_io)

if(FORTARRAY_ENABLE_OPENMP)
    find_package(OpenMP COMPONENTS Fortran)
    if(OpenMP_Fortran_FOUND)
        target_link_libraries(fortarray_core PUBLIC OpenMP::OpenMP_Fortran)
    endif()
endif()

if(FORTARRAY_BUILD_TESTS)
    enable_testing()
    foreach(test_name IN ITEMS core thread_safety io_oracle)
        add_executable(test_${test_name} test/test_${test_name}.f90)
        target_link_libraries(test_${test_name} PRIVATE fortarray_all)
        add_test(NAME ${test_name} COMMAND test_${test_name})
    endforeach()
    set_tests_properties(io_oracle PROPERTIES
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    )
endif()

if(FORTARRAY_INSTALL)
    install(TARGETS fortarray_core fortarray_io fortarray_all
        EXPORT fortarray-targets
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    )
    install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/"
        DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/fortarray"
    )
    install(EXPORT fortarray-targets
        FILE fortarray-targets.cmake
        NAMESPACE fortarray::
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortarray"
    )
    configure_package_config_file(
        cmake/fortarray-config.cmake.in
        "${CMAKE_CURRENT_BINARY_DIR}/fortarray-config.cmake"
        INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortarray"
    )
    write_basic_package_version_file(
        "${CMAKE_CURRENT_BINARY_DIR}/fortarray-config-version.cmake"
        VERSION "${PROJECT_VERSION}"
        COMPATIBILITY SameMajorVersion
    )
    install(FILES
        "${CMAKE_CURRENT_BINARY_DIR}/fortarray-config.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/fortarray-config-version.cmake"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortarray"
    )
    install(FILES LICENSE DESTINATION "${CMAKE_INSTALL_DATADIR}/licenses/fortarray")
endif()
