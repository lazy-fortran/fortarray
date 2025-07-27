module foxel
    use foxel_types
    use foxel_memory
    use foxel_storage
    use foxel_constructors
    use foxel_indexing
    use foxel_datasets
    use foxel_netcdf
    use foxel_csv
    use foxel_format_detection
    use foxel_broadcasting
    use foxel_arithmetic
    use foxel_comparison
    use foxel_logical
    use foxel_aggregation
    use foxel_missing_data
    use foxel_coordinate_selection
    use foxel_boolean_indexing
    use foxel_slicing
    use foxel_interpolation
    use foxel_apply_functions
    use foxel_lazy_evaluation
    use foxel_chunked_operations
    use foxel_parallel_computing
    use foxel_fortplot_integration
    use foxel_plot_types
    use foxel_time_coordinates
    implicit none
    
    ! Re-export everything from submodules
    public
    
end module foxel