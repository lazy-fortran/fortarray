cmake_minimum_required(VERSION 3.22)
project(fortarray_consumer LANGUAGES Fortran)

find_package(fortarray CONFIG REQUIRED)
add_executable(consumer main.f90)
target_link_libraries(consumer PRIVATE fortarray::core)

