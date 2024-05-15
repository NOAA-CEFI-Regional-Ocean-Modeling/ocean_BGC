! this module will contain a few main routines: one to get the
module cobalt_param_doc 

use MOM_cpu_clock,     only : cpu_clock_id, cpu_clock_begin, cpu_clock_end, CLOCK_COMPONENT
use MOM_data_override, only : data_override
use MOM_domains,       only : domain2D, same_domain
use MOM_error_handler, only : MOM_error, FATAL, WARNING, callTree_enter, callTree_leave
use MOM_file_parser,   only : param_file_type, open_param_file, close_param_file
use MOM_file_parser,   only : read_param, get_param, log_param, log_version
use MOM_io,            only : file_exists, close_file, slasher, ensembler
use MOM_io,            only : open_namelist_file, check_nml_error
use MOM_time_manager,  only : time_type, time_type_to_real, real_to_time_type
use MOM_time_manager,  only : operator(+), operator(-), operator(>)
use posix, only : mkdir, stat, stat_buf

implicit none ; private

public get_COBALT_param_file

!> Container for paths and parameter file names.
!type, public :: directories
!  character(len=240) :: &
!    restart_input_dir = ' ',& !< The directory to read restart and input files.
!    restart_output_dir = ' ',&!< The directory into which to write restart files.
!    output_directory = ' '    !< The directory to use to write the model output.
!  character(len=2048) :: &
!    input_filename  = ' '     !< A string that indicates the input files or how
!                              !! the run segment should be started.
!end type directories

contains
!! This subroutine is analagous to the get_mom_input routine within the MOM_get_input module. There are additional
!> arguments used in the MOM version which are commented out and not used here. These features may be needed as we 
!> add more complexity to the parameter files in COBALT so I have commented them out rather than deleting them.
subroutine get_COBALT_param_file(param_file)
  type(param_file_type), optional, intent(out) :: param_file   !< A structure to parse for run-time parameters.
  !type(directories),     optional, intent(out) :: dirs         !< Container for paths and parameter filenames.
  !logical,               optional, intent(in)  :: check_params !< If present and False will stop error checking for
  !                                                             !! run-time parameters.
  !character(len=*),      optional, intent(in)  :: default_input_filename !< If present, is the value assumed for
  !                                                             !! input_filename if input_filename is no listed
  !                                                             !! in the namelist MOM_input_nml.
  ! Local variables
  integer, parameter :: npf = 5 ! Maximum number of parameter files

  character(len=240) :: &
    output_directory = ' ', &      ! Directory to use to write the model output.
    parameter_filename(npf) = ' '  ! List of files containing parameters.

  character(len=2048) :: &
    input_filename             ! A string that indicates the input files or how
                               ! the run segment should be started.
  character(len=240) :: output_dir
  integer :: unit, io, ierr, valid_param_files

  type(stat_buf) :: buf

  namelist /cobalt_input_nml/ parameter_filename

  ! Open namelist
  if (file_exists('input.nml')) then
    unit = open_namelist_file(file='input.nml')
  else
    call MOM_error(FATAL,'Required namelist file input.nml does not exist.')
  endif

  ! Read namelist parameters
  ierr=1 ; do while (ierr /= 0)
    read(unit, nml=cobalt_input_nml, iostat=io, end=10)
    ierr = check_nml_error(io, 'cobalt_input_nml')
  enddo
10 call close_file(unit)

  output_dir = trim(slasher(ensembler(output_directory)))
  valid_param_files = 0
  do io=1,npf
     if (len_trim(trim(parameter_filename(io))) > 0) then
       call open_param_file(trim(parameter_filename(io)), param_file, &
                            component="COBALT", doc_file_dir=output_dir)
        valid_param_files = valid_param_files + 1
     endif
   enddo

end subroutine get_COBALT_param_file

end module cobalt_param_doc 
