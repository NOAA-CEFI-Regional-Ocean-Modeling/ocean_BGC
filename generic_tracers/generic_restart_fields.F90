!> Describes fields that a generic tracer package keeps in its own arrays, but that must be saved
!! in restart files because they carry information from one time step to the next.
!!
!! These fields are not tracers, so they are not added to the generic tracer list. The ocean model
!! coupling layer (e.g. MOM_generic_tracer) registers them directly with its restart infrastructure,
!! and on a new run initializes them from their source files in the same way as the generic tracers.
module generic_restart_fields

  implicit none ; private

  public g_restart_field_2d_type, g_restart_field_2d_add

  !> A 2D field to be saved in and restored from restart files
  type g_restart_field_2d_type
    character(len=64)  :: name = ''        !< The name of the field in the restart file
    character(len=128) :: longname = ''    !< The long name of the field
    character(len=32)  :: units = ''       !< The units of the field
    character(len=64)  :: legacy_name = '' !< The name of a 3D restart field written by older versions
                                           !! whose top level holds the same data, or blank if there is none
    real, dimension(:,:), pointer :: field => NULL() !< The array that is saved and restored, in [units]
    character(len=256) :: src_file = 'NULL'         !< The file with the initial condition, or 'NULL' if
                                                    !! the field keeps the values set by its package
    character(len=256) :: src_var_name = 'NULL'     !< The name of the initial condition in src_file
    character(len=256) :: src_var_gridspec = 'NULL' !< The name of a gridspec file for src_file, or 'NULL'
    integer :: src_var_record = -1                  !< The record to read from src_file
    real :: src_var_unit_conversion = 1.0           !< A factor that converts the values in src_file to [units]
    real :: src_var_valid_min = -99.0               !< The minimum value of the initial condition, in [units]
  end type g_restart_field_2d_type

contains

  !> Append a 2D field to a list of restart fields. The optional source file arguments give the initial
  !! condition, which is read in the same way as for a 3D generic tracer, keeping only its top level.
  subroutine g_restart_field_2d_add(fields, field, name, longname, units, legacy_name, src_file, &
                                    src_var_name, src_var_gridspec, src_var_record, &
                                    src_var_unit_conversion, src_var_valid_min)
    type(g_restart_field_2d_type), dimension(:), allocatable, intent(inout) :: fields !< The list of restart fields
    real, dimension(:,:), pointer, intent(in) :: field    !< The array to save and restore, in [units]
    character(len=*),             intent(in) :: name     !< The name of the field in the restart file
    character(len=*),             intent(in) :: longname !< The long name of the field
    character(len=*),             intent(in) :: units    !< The units of the field
    character(len=*),   optional, intent(in) :: legacy_name !< The name of a 3D restart field written by older
                                                         !! versions whose top level holds the same data
    character(len=*),   optional, intent(in) :: src_file !< The file with the initial condition
    character(len=*),   optional, intent(in) :: src_var_name !< The name of the initial condition in src_file
    character(len=*),   optional, intent(in) :: src_var_gridspec !< The name of a gridspec file for src_file
    integer,            optional, intent(in) :: src_var_record !< The record to read from src_file
    real,               optional, intent(in) :: src_var_unit_conversion !< A factor that converts the values
                                                         !! in src_file to [units]
    real,               optional, intent(in) :: src_var_valid_min !< The minimum value of the initial
                                                         !! condition, in [units]

    type(g_restart_field_2d_type), dimension(:), allocatable :: new_fields
    integer :: n

    if (.not. allocated(fields)) allocate(fields(0))
    n = size(fields)
    allocate(new_fields(n+1))
    new_fields(1:n) = fields(1:n)
    new_fields(n+1)%name = name
    new_fields(n+1)%longname = longname
    new_fields(n+1)%units = units
    if (present(legacy_name)) new_fields(n+1)%legacy_name = legacy_name
    new_fields(n+1)%field => field
    if (present(src_file)) new_fields(n+1)%src_file = src_file
    if (present(src_var_name)) new_fields(n+1)%src_var_name = src_var_name
    if (present(src_var_gridspec)) new_fields(n+1)%src_var_gridspec = src_var_gridspec
    if (present(src_var_record)) new_fields(n+1)%src_var_record = src_var_record
    if (present(src_var_unit_conversion)) new_fields(n+1)%src_var_unit_conversion = src_var_unit_conversion
    if (present(src_var_valid_min)) new_fields(n+1)%src_var_valid_min = src_var_valid_min
    call move_alloc(new_fields, fields)
  end subroutine g_restart_field_2d_add

end module generic_restart_fields
