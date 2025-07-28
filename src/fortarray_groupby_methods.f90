module fortarray_groupby_methods
    !! Implementation of groupby_t methods for Sprint 13
    use fortarray_types
    use iso_fortran_env, only: int32, int64, real32, real64, error_unit
    use fortarray_constructors, only: new_array
    use fortarray_missing_data, only: is_missing
    implicit none
    
    private
    public :: finalize_groupby, validate_groupby
    
contains

    !> Finalizer for groupby_t - clean implementation
    subroutine finalize_groupby(gb)
        type(groupby_t), intent(inout) :: gb
        
        if (allocated(gb%group_names)) deallocate(gb%group_names)
        if (allocated(gb%group_sizes)) deallocate(gb%group_sizes)
        if (allocated(gb%group_indices)) deallocate(gb%group_indices)
        if (associated(gb%parent_array)) gb%parent_array => null()
        
        gb%initialized = .false.
        gb%n_groups = 0
        gb%current_group = 0
        gb%iterator_active = .false.
        
    end subroutine finalize_groupby
    
    !> Validate groupby object with parent array
    function validate_groupby(gb, parent_array) result(is_valid)
        type(groupby_t), intent(in) :: gb
        class(fortarray_t), intent(in) :: parent_array
        logical :: is_valid
        
        is_valid = .true.
        
        ! Check initialization
        if (.not. gb%initialized) then
            is_valid = .false.
            return
        end if
        
        ! Check parent array association
        if (.not. associated(gb%parent_array)) then
            is_valid = .false.
            return
        end if
        
        ! Check group counts
        if (gb%n_groups <= 0) then
            is_valid = .false.
            return
        end if
        
        ! Check allocations
        if (.not. allocated(gb%group_names) .or. &
            .not. allocated(gb%group_sizes) .or. &
            .not. allocated(gb%group_indices)) then
            is_valid = .false.
            return
        end if
        
    end function validate_groupby

end module fortarray_groupby_methods