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
    use foxel_aggregation
    use foxel_missing_data
    implicit none
    
    ! Re-export everything from submodules
    public
    
end module foxel