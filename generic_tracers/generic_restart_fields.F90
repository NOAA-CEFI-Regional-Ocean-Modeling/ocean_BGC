module generic_restart_fields
!! Describes fields that a generic tracer package keeps in its own arrays, but that must be saved
!! in restart files because they carry information from one time step to the next.
!!
!! These fields are not tracers, so they are not added to the generic tracer list. The ocean model
!! coupling layer (e.g. MOM_generic_tracer) registers them directly with its restart infrastructure,
!! and on a new run initializes them from their source files in the same way as the generic tracers.
!!
!! A package builds its whole list with g_restart_field_add before any field is registered. Each call
!! moves the elements of the list to a new array, so no pointer to an element may be kept while the
!! list is being built.
!!
!! The initial conditions are read from the field_table entries of the package, in the same way as
!! for the generic tracers, so the package must call g_tracer_start_param_list before adding fields.

  use field_manager_mod, only : fm_string_len
  use mpp_mod,           only : mpp_error, FATAL
  use g_tracer_utils,    only : g_tracer_add_param

  implicit none ; private

  public g_restart_field_type, g_restart_field_add

  type g_restart_field_type
    !! A 2D or 3D field to be saved in and restored from restart files
    character(len=64)  :: name = ''        !! The name of the field in the restart file
    character(len=128) :: longname = ''    !! The long name of the field
    character(len=32)  :: units = ''       !! The units of the field
    character(len=64)  :: legacy_name = '' !! The name of a 3D restart field written by older versions
                                           !! whose top level holds the same data, or blank if there is none.
                                           !! Only used for 2D fields.
    ! Exactly one of field_2d and field_3d is associated. They point to arrays owned by the package and
    ! must stay pointers: appending to the list copies its elements, which would copy allocatable
    ! components and leave the fields registered for restarts pointing to deallocated memory.
    real, dimension(:,:),   pointer :: field_2d => NULL() !! The 2D array that is saved and restored, in [units]
    real, dimension(:,:,:), pointer :: field_3d => NULL() !! The 3D array that is saved and restored, in [units]
    logical :: init_from_ic = .true.                !! If false, a new run keeps the values set by the package
                                                    !! unless src_file is set
    character(len=256) :: src_file = 'NULL'         !! The file with the initial condition, or 'NULL' if
                                                    !! there is none
    character(len=256) :: src_var_name = 'NULL'     !! The name of the initial condition in src_file
    character(len=256) :: src_var_gridspec = 'NULL' !! The name of a gridspec file for src_file, or 'NULL'
    integer :: src_var_record = -1                  !! The record to read from src_file
    real :: src_var_unit_conversion = 1.0           !! A factor that converts the values in src_file to [units]
    real :: src_var_valid_min = -99.0               !! The minimum value of the initial condition, in [units]
  end type g_restart_field_type

  interface g_restart_field_add
    !! Append a 2D or 3D field to a list of restart fields
    module procedure g_restart_field_add_2d, g_restart_field_add_3d
  end interface g_restart_field_add

contains

  subroutine g_restart_field_add_2d(fields, field, name, longname, units, legacy_name)
    !! Append a 2D field to a list of restart fields. Its initial condition is set in the field_table
    !! under legacy_name if it is present, or else under name, and is read in the same way as for a
    !! 3D generic tracer, keeping only its top level.
    type(g_restart_field_type), dimension(:), allocatable, intent(inout) :: fields !! The list of restart fields
    real, dimension(:,:), pointer, intent(in) :: field    !! The array to save and restore, in [units]
    character(len=*),             intent(in) :: name     !! The name of the field in the restart file
    character(len=*),             intent(in) :: longname !! The long name of the field
    character(len=*),             intent(in) :: units    !! The units of the field
    character(len=*),   optional, intent(in) :: legacy_name !! The name of a 3D restart field written by older
                                                         !! versions whose top level holds the same data

    if (present(legacy_name)) then
      call append(fields, name, longname, units, legacy_name)
      fields(size(fields))%legacy_name = legacy_name
    else
      call append(fields, name, longname, units, name)
    endif
    fields(size(fields))%field_2d => field
  end subroutine g_restart_field_add_2d

  subroutine g_restart_field_add_3d(fields, field, name, longname, units)
    !! Append a 3D field to a list of restart fields. Its initial condition is set in the field_table
    !! under name, and is read in the same way as for a 3D generic tracer.
    type(g_restart_field_type), dimension(:), allocatable, intent(inout) :: fields !! The list of restart fields
    real, dimension(:,:,:), pointer, intent(in) :: field  !! The array to save and restore, in [units]
    character(len=*),             intent(in) :: name     !! The name of the field in the restart file
    character(len=*),             intent(in) :: longname !! The long name of the field
    character(len=*),             intent(in) :: units    !! The units of the field

    call append(fields, name, longname, units, name)
    fields(size(fields))%field_3d => field
  end subroutine g_restart_field_add_3d

  subroutine append(fields, name, longname, units, ic_name)
    !! Append a restart field with the given description, but no array, to a list of restart fields
    type(g_restart_field_type), dimension(:), allocatable, intent(inout) :: fields !! The list of restart fields
    character(len=*), intent(in) :: name     !! The name of the field in the restart file
    character(len=*), intent(in) :: longname !! The long name of the field
    character(len=*), intent(in) :: units    !! The units of the field
    character(len=*), intent(in) :: ic_name  !! The name under which the field_table sets the initial condition

    type(g_restart_field_type), dimension(:), allocatable :: new_fields
    integer :: n

    if (.not. allocated(fields)) allocate(fields(0))
    n = size(fields)
    allocate(new_fields(n+1))
    new_fields(1:n) = fields(1:n)
    new_fields(n+1)%name = name
    new_fields(n+1)%longname = longname
    new_fields(n+1)%units = units
    call read_ic_params(new_fields(n+1), ic_name)
    call move_alloc(new_fields, fields)
  end subroutine append

  subroutine read_ic_params(rf, name)
    !! Read the initial condition parameters that the field_table sets for a tracer called name into
    !! the restart field rf. This follows g_tracer_add and g_tracer_init.
    type(g_restart_field_type), intent(inout) :: rf   !! The restart field to set
    character(len=*),           intent(in)    :: name !! The name under which the field_table sets
                                                      !! the initial condition

    character(len=fm_string_len) :: src_var_unit
    logical :: enforce_src_info  ! The package default for whether initial conditions must be set
    logical :: requires_src_info ! If true, this field must have its initial condition set

    call g_tracer_add_param('enforce_src_info', enforce_src_info, .true., do_not_log=.true.)
    call g_tracer_add_param(trim(name)//"_requires_src_info", requires_src_info, enforce_src_info)
    call g_tracer_add_param(trim(name)//"_requires_restart", rf%init_from_ic, .true.)
    if (.not. requires_src_info) return

    call g_tracer_add_param(trim(name)//"_src_file",         rf%src_file,          'NULL')
    call g_tracer_add_param(trim(name)//"_src_var_name",     rf%src_var_name,      'NULL')
    call g_tracer_add_param(trim(name)//"_src_var_unit",     src_var_unit,         'NULL')
    call g_tracer_add_param(trim(name)//"_src_var_record",   rf%src_var_record,    -1)
    call g_tracer_add_param(trim(name)//"_src_var_gridspec", rf%src_var_gridspec,  'NULL')
    call g_tracer_add_param(trim(name)//"_valid_min",        rf%src_var_valid_min, -99.0)

    ! These checks reproduce the checks that g_tracer_print_info performed after registering tracers
    if (rf%src_file == 'NULL') call mpp_error(FATAL, 'g_restart_field_add: '// &
        trim(name)//' : src_file is not set in the field_table')
    if (rf%src_var_name == 'NULL') call mpp_error(FATAL, 'g_restart_field_add: '// &
        trim(name)//' : src_var_name is not set in the field_table')
    select case (trim(src_var_unit))
      case ('none') ; rf%src_var_unit_conversion = 1.0
      case ('ppt')  ; rf%src_var_unit_conversion = 1.0e-12
      case ('NULL') ; call mpp_error(FATAL, 'g_restart_field_add: '// &
                          trim(name)//' : src_var_unit is not set in the field_table')
      case default  ; call mpp_error(FATAL, 'g_restart_field_add: '// &
                          trim(name)//' : cannot determine src_var_unit_conversion')
    end select
  end subroutine read_ic_params

end module generic_restart_fields
