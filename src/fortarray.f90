module fortarray
    use fortarray_types
    use fortarray_memory
    use fortarray_storage
    use fortarray_constructors
    use fortarray_indexing
    use fortarray_datasets
    use fortarray_netcdf
    use fortarray_csv
    use fortarray_format_detection
    use fortarray_broadcasting
    use fortarray_arithmetic
    use fortarray_comparison
    use fortarray_logical
    use fortarray_aggregation
    use fortarray_missing_data
    use fortarray_coordinate_selection
    use fortarray_boolean_indexing
    use fortarray_slicing
    use fortarray_interpolation
    use fortarray_apply_functions
    use fortarray_lazy_evaluation
    use fortarray_chunked_operations
    use fortarray_parallel_computing
    use fortarray_fortplot_integration
    use fortarray_plot_types
    use fortarray_time_coordinates
    use fortarray_time_operations
    use fortarray_optimization
    use fortarray_groupby_methods
    implicit none
    
    ! Re-export everything from submodules
    public
    
end module fortarray