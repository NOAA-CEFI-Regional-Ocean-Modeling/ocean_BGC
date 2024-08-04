! <CONTACT EMAIL="Charles.Stock@noaa.gov"> Charles Stock
! </CONTACT>
!
! <OVERVIEW>
! This module contains the generic version of the version 3 of the Carbon Ocean
! Biogeochemistry and Lower Trophics model (COBALTv3.0)
! </OVERVIEW>
!<DESCRIPTION>
!  COBALT simulates plankton food webs and the cycling of carbon, nitrogen,
!  phosphorus, iron, silica, calcium carbonate, and lithogenic material in ocean
!  and coastal environments.  COBALT's construction attempts to balance the need
!  for sufficient complexity to resolve prominent global and regional patterns
!  in these processes, while being mindful of computational efficiency and
!  the desire to rely on dynamics which are well supported by empirical studies.
!  In some cases, this means forgoing less understood interactions of uncertain
!  quantitative importance to establish a robustly supported baseline simulation
!  until constraints improve.  The COBALT code releases reflect this approach:
!
!  COBALTv1 (Stock et al., 2014) augmented the plankton foodweb dynamics of
!  GFDL's prior biogeochemical model (TOPAZ, Dunne et al., 2013) by adding 3
!  zooplankton groups, free living bacteria, and expanding the resolution of
!  dissolved organic material to include labile, semi-labile and semi-refractory
!  forms.  This produced quantitatively credible estimates of energy flows through
!  plankton foodwebs.
!
!  COBALTv2 served as the ocean biogeochemical component of GFDL's CMIP6 earth
!  system model (Stock et al., 2020).  The structure of COBALTv2 was largely
!  analogous to COBALTv1, but notable adjustments included: 1) the addition of
!  temperature and O2-dependent particle remineralization following Laufkotter
!  et al. (2017); 2) retuning of ammonia dynamics and the addition air-sea ammonia
!  exchanges (Paulot et al., 2020); 3) adjustments to iron sources and
!  scavenging; and 4) movement of the default carbon chemistry to MOCSY.
!
!  COBALTv3 (Stock et al., submitted) was developed to enhance the robustness of COBALT
!  across open ocean and coastal regions.  It now divides the phytoplankton community
!  into 3 rather than 2 size classes to better resolve phytoplankton communities from
!  oligotrophic gyres to highly productive coastal regions (Van Oostende et al., 2018).
!  Thus, COBALTv3 includes small, medium, and large phytoplankton, and a large diazotroph.
!  The flexibility of plankton foodweb links has been increased and relevant parameters
!  recalibrated to better reflect observed constraints.  Dynamic P:N stoichiometry has
!  been added using a truncated variant of the Galbraith and Martiny (2015) scheme
!  to allow COBALT to better integrate riverine inputs with highly perturbed stoichiometry
!  (Ross et al., 2023).  Photoacclimation and photoadaptation dynamics have been enhanced
!  to better represent observed chlorophyll patterns (Stock et al., submitted).
!  Direct phytoplankton sinking has been added and phytoplankton loss terms generalized.
!  Temperature dependence has been added to the decay of dissolved organic material to
!  improve seasonal dynamics.
!
!  COBALTv3 now includes 40 prognostic state variables:
!  
!       alk: alkalinity
!       cadet_arag: calcium carbonate detritus (aragonite)
!       cadet_calc: calcium carbonate detritus (calcite)
!       dic: dissolved inorganic carbon
!       fed: dissolved iron
!       fedi: diazotroph iron
!       felg: large phytoplankton iron
!       femd: medium phytoplankton iron
!       fesm: small phytoplankton iron
!       fedet: iron detritus
!       ldon: labile dissolved organic nitrogen
!       ldop: labile dissolved organic phosphorous
!       lith: lithogenic aluminosilicate particles
!       lithdet: lithogenic detritus
!       nbact: bacteria
!       ndet: nitrogen detritus
!       ndi: diazotroph nitrogen
!       nlg: large, coastal or chain-forming, phytoplankton nitrogen
!       nmd: medium phytoplankton nitrogen
!       nsm: high-light adapted small phytoplankton nitrogen
!       nh4: ammonia
!       no3: nitrate
!       o2: oxygen
!       pdet: phosphorous detritus
!       pdi: diazotroph phosphorus 
!       plg: large phytoplankton phosphorus 
!       pmd: medium phytoplankton phosphorus 
!       psm: small phytoplankton phosphorus 
!       po4: phosphate
!       srdon: semi-refractory dissolved organic nitrogen
!       srdop: semi-refractory dissolved organic phosphorous
!       sldon: semi-labile dissolved organic nitrogen
!       sldop: semi-labile dissolved organic phosphorous
!       sidet: silica detritus
!       silg: large phytoplankton silica
!       simd: medium phytoplankton silica
!       sio4: silicate
!       nsmz: small zooplankton nitrogen
!       nmdz: medium zooplankton nitrogen
!       nlgz: large zooplankton nitrogen
!
!<NAMELIST NAME="generic_COBALT_nml">
!
!  <DATA NAME="do_14c" TYPE="logical">
!  If true, then simulate radiocarbon. Includes 2 prognostic tracers, DI14C
! and DO14C. Requires that do_carbon = .true. Note that 14C is not taken up
! by CaCO3 at the current time, but cycles only through the soft tissue.
! This is a mistake that will be fixed later.
!  </DATA>
!
!</NAMELIST>
!
!</DESCRIPTION>
!
! <INFO>
! <REFERENCE>
!
! Stock et al. (submitted).  Photoacclimation and photoadaptation sensitivity
! in a global ocean ecosystem model.  JAMES 
!
! Stock et al. (2020). https://doi.org/10.1029/2019MS002043
! Stock, Dunne and John (2014). https://doi.org/10.1016/j.pocean.2013.07.001
! Dunne et al. (2013). https://doi.org/10.1175/JCLI-D-12-00150.1
! Ross et al. (2023) https://doi.org/10.5194/gmd-16-6943-2023
! Van Oostende et al. (2018) https://doi.org/10.1016/j.pocean.2018.10.009
! Laufkotter et al. (2017)  https://doi.org/10.1002/2017GB005643 
! Paulot et al. (2020)  https://doi.org/10.1029/2019MS002026
! Galbraith and Martiny (2015) https://doi.org/10.1073/pnas.1423917112
! 
! </REFERENCE>
! <DEVELOPER_NOTES>
! </DEVELOPER_NOTES>
! </INFO>
!----------------------------------------------------------------

module generic_COBALT

  use coupler_types_mod, only: coupler_2d_bc_type
  use field_manager_mod, only: fm_string_len, fm_path_name_len
  use mpp_mod,           only: mpp_clock_id, mpp_clock_begin, mpp_clock_end
  use mpp_mod,           only: CLOCK_COMPONENT, CLOCK_SUBCOMPONENT, CLOCK_MODULE
  use mpp_mod,           only: input_nml_file, mpp_error, stdlog, NOTE, WARNING, FATAL, stdout, mpp_chksum
  use time_manager_mod,  only: time_type, day_of_year
  use fm_util_mod,       only: fm_util_start_namelist, fm_util_end_namelist
  use constants_mod,     only: WTMCO2, WTMO2,WTMN,rdgas,wtmair
  use data_override_mod, only: data_override
  use fms_mod,           only: write_version_number, FATAL, WARNING, stdout, stdlog,mpp_pe,mpp_root_pe
  use fms_mod,           only: check_nml_error
  use MOM_EOS,           only: calculate_density, EOS_type

  use g_tracer_utils, only : g_tracer_type,g_tracer_start_param_list,g_tracer_end_param_list
  use g_tracer_utils, only : g_tracer_add,g_tracer_add_param, g_tracer_set_files
  use g_tracer_utils, only : g_tracer_set_values,g_tracer_get_pointer
  use g_tracer_utils, only : g_tracer_get_common,g_tracer_set_common
  use g_tracer_utils, only : g_tracer_coupler_set,g_tracer_coupler_get
  use g_tracer_utils, only : g_tracer_get_values
  use g_tracer_utils, only : g_diag_type, g_diag_field_add
  use g_tracer_utils, only : register_diag_field=>g_register_diag_field
  use g_tracer_utils, only : g_send_data, is_root_pe
  use g_tracer_utils, only : g_tracer_is_prog, g_tracer_vertfill, g_tracer_get_next

  use cobalt_types 
  use cobalt_send_diag, only : cobalt_send_diagnostics
  use cobalt_reg_diag, only : cobalt_reg_diagnostics
  use cobalt_param_doc, only : get_COBALT_param_file

  use MOM_file_parser,   only : read_param, get_param, log_version, param_file_type

  use FMS_co2calc_mod, only : FMS_co2calc, CO2_dope_vector

  implicit none ; private

  public do_generic_COBALT
  public generic_COBALT_register
  public generic_COBALT_init
  public generic_COBALT_register_diag
  public generic_COBALT_update_from_coupler
  public generic_COBALT_update_from_source
  public generic_COBALT_update_from_bottom
  public generic_COBALT_set_boundary_values
  public generic_COBALT_end
  public as_param_cobalt

  !The following variables for using this module
  ! are overwritten by generic_tracer_nml namelist
  logical, save :: do_generic_COBALT = .false.       !< Activate the generic_COBALT module if it is set to true.
  character(len=10), save :: as_param_cobalt !< air-sea flux parameter settings for COBALT. Set from
                                             !! as_param in generic_tracer_nml by generic_tracer.F90, 
                                             !! but can be replaced by setting as_param_cobalt
                                             !! in generic_COBALT_nml.

  namelist /generic_COBALT_nml/ co2_calc, do_14c, do_nh3_atm_ocean_exchange, scheme_nitrif, debug, &
     o2_min_nit,k_o2_nit,irr_inhibit,k_nh3_nitrif,gamma_nitrif,do_vertfill_pre,imbalance_tolerance, &
     as_param_cobalt
  
  !
  ! Array allocations and flux calculations assume that phyto(1) is the
  ! only phytoplankton group cabable of nitrogen uptake by N2 fixation while phyto(2:NUM_PHYTO)
  ! are only cabable of nitrgen uptake by NH4 and NO3 uptake
  !
  type(phytoplankton), dimension(NUM_PHYTO) :: phyto

  ! define three zooplankton types
  type(zooplankton), dimension(NUM_ZOO) :: zoo

  type(bacteria), dimension(NUM_BACT) :: bact

  type(generic_COBALT_type) :: cobalt

  type(CO2_dope_vector) :: CO2_dope_vec

  ! identification numbers for mpp clocks
  integer :: id_clock_carbon_calculations
  integer :: id_clock_phyto_growth
  integer :: id_clock_bacteria_growth
  integer :: id_clock_zooplankton_calculations
  integer :: id_clock_other_losses
  integer :: id_clock_production_loop
  integer :: id_clock_ballast_loops
  integer :: id_clock_source_sink_loop1
  integer :: id_clock_source_sink_loop2
  integer :: id_clock_source_sink_loop3
  integer :: id_clock_source_sink_loop4
  integer :: id_clock_source_sink_loop5
  integer :: id_clock_source_sink_loop6
  integer :: id_clock_cobalt_send_diagnostics
  integer :: id_clock_cobalt_calc_diagnostics

contains

  subroutine generic_COBALT_register(tracer_list)
    type(g_tracer_type), pointer :: tracer_list
    integer                                                 :: ioun
    integer                                                 :: ierr
    integer                                                 :: io_status
    integer                                                 :: stdoutunit,stdlogunit
    !-----------------------------------------------------------------------
    !     local parameters
    !-----------------------------------------------------------------------
    !
    character(len=fm_string_len), parameter :: sub_name = 'generic_COBALT_register'
    character(len=256), parameter   :: error_header =                               &
      '==>Error from ' // trim(mod_name) // '(' // trim(sub_name) // '): '
    character(len=256), parameter   :: warn_header =                                &
      '==>Warning from ' // trim(mod_name) // '(' // trim(sub_name) // '): '
    character(len=256), parameter   :: note_header =                                &
      '==>Note from ' // trim(mod_name) // '(' // trim(sub_name) // '): '



    ! provide for namelist over-ride
    ! This needs to go before the add_tracers in order to allow the namelist
    ! settings to switch tracers on and off.
    !
    stdoutunit=stdout();stdlogunit=stdlog()

    read (input_nml_file, nml=generic_COBALT_nml, iostat=io_status)
    ierr = check_nml_error(io_status,'generic_COBALT_nml')

    write (stdoutunit,'(/)')
    write (stdoutunit, generic_COBALT_nml)
    write (stdlogunit, generic_COBALT_nml)

    if (do_14c) then
      write (stdoutunit,*) trim(note_header), 'Simulating radiocarbon'
    endif

    if (trim(co2_calc) == 'mocsy') then
      write (stdoutunit,*) trim(note_header), 'Using Mocsy CO2 routine'
    else
      call mpp_error(FATAL,"Unknown co2_calc option specified in generic_COBALT_nml")
    endif

    !Specify all prognostic and diagnostic tracers of this modules.
    call user_add_tracers(tracer_list)

  end subroutine generic_COBALT_register

  !  <SUBROUTINE NAME="generic_COBALT_init">
  !  <OVERVIEW>
  !   Initialize the generic COBALT module
  !  </OVERVIEW>
  !  <DESCRIPTION>
  !   This subroutine:
  !       Adds all the COBALT Tracers to the list of generic Tracers
  !       passed to it via utility subroutine g_tracer_add().
  !
  !       Adds all the parameters used by this module via utility
  !       subroutine g_tracer_add_param().
  !
  !       Allocates all work arrays used in the module.
  !  </DESCRIPTION>
  !  <TEMPLATE>
  !   call generic_COBALT_init(tracer_list)
  !  </TEMPLATE>
  !  <IN NAME="tracer_list" TYPE="type(g_tracer_type), pointer">
  !   Pointer to the head of generic tracer list.
  !  </IN>
  ! </SUBROUTINE>
  subroutine generic_COBALT_init(tracer_list, force_update_fluxes)
    type(g_tracer_type), pointer :: tracer_list
    logical          ,intent(in) :: force_update_fluxes

    character(len=fm_string_len), parameter :: sub_name = 'generic_COBALT_init'

    type(param_file_type) :: param_file  !< structure indicating parameter file to parse

    ! This include declares and sets the variable "version". (use of include statement copied from MOM6)
# include "version_variable.h"

    !There are situations where the column_physics (update_from_source) is not called every timestep
    ! such as when MOM6 THERMO_SPANS_COUPLING=True , yet we want the fluxes to be updated every timestep
    ! In that case we can force an update by setting the namelist generic_tracer_nml:force_update_fluxes=.true.
    cobalt%force_update_fluxes = force_update_fluxes

    if (do_nh3_atm_ocean_exchange .or. scheme_nitrif.eq.2 .or. scheme_nitrif.eq.3) then
       do_nh3_diag=.true.
    else
       do_nh3_diag=.false.
    end if

    ! add MOM6-style interfaces for a parameter file
    call get_COBALT_param_file(param_file)
    call log_version(param_file, "COBALT", version, "", log_to_all=.true., debugging=.true.)
    !Specify and initialize all parameters used by this package
    call user_add_params(param_file)

    !Allocate all the private work arrays used by this module.
    call user_allocate_arrays

    id_clock_carbon_calculations = mpp_clock_id('(Cobalt: carbon calcs)' ,grain=CLOCK_MODULE)
    id_clock_phyto_growth = mpp_clock_id('(Cobalt: phytoplankton growth calcs)',grain=CLOCK_MODULE)
    id_clock_bacteria_growth = mpp_clock_id('(Cobalt: bacteria growth calcs)',grain=CLOCK_MODULE)
    id_clock_zooplankton_calculations = mpp_clock_id('(Cobalt: zooplankton calculations)',grain=CLOCK_MODULE)
    id_clock_other_losses = mpp_clock_id('(Cobalt: other losses)',grain=CLOCK_MODULE)
    id_clock_production_loop = mpp_clock_id('(Cobalt: production loop)',grain=CLOCK_MODULE)
    id_clock_ballast_loops = mpp_clock_id('(Cobalt: ballasting loops)',grain=CLOCK_MODULE)
    id_clock_source_sink_loop1 = mpp_clock_id('(Cobalt: source/sink loop 1)',grain=CLOCK_MODULE)
    id_clock_source_sink_loop2 = mpp_clock_id('(Cobalt: source/sink loop 2)',grain=CLOCK_MODULE)
    id_clock_source_sink_loop3 = mpp_clock_id('(Cobalt: source/sink loop 3)',grain=CLOCK_MODULE)
    id_clock_source_sink_loop4 = mpp_clock_id('(Cobalt: source/sink loop 4)',grain=CLOCK_MODULE)
    id_clock_source_sink_loop5 = mpp_clock_id('(Cobalt: source/sink loop 5)',grain=CLOCK_MODULE)
    id_clock_source_sink_loop6 = mpp_clock_id('(Cobalt: source/sink loop 6)',grain=CLOCK_MODULE)
    id_clock_cobalt_send_diagnostics = mpp_clock_id('(Cobalt: send diagnostics)',grain=CLOCK_MODULE)
    id_clock_cobalt_calc_diagnostics = mpp_clock_id('(Cobalt: calculate diagnostics)',grain=CLOCK_MODULE)

  end subroutine generic_COBALT_init

  !>   Register diagnostic fields to be used in this module.
  !!   Note that the tracer fields are automatically registered in user_add_tracers
  !!   User adds only diagnostics for fields that are not a member of g_tracer_type
  !
  subroutine generic_COBALT_register_diag(diag_list)
    type(g_diag_type), pointer :: diag_list
    integer        :: isc,iec,jsc,jec,isd,ied,jsd,jed,nk,ntau, axes(3), axesTi(3)
    type(time_type):: init_time
    call g_tracer_get_common(isc,iec,jsc,jec,isd,ied,jsd,jed,nk,ntau,axes=axes,init_time=init_time)
    !
    call cobalt_reg_diagnostics(diag_list,axes,init_time,phyto,zoo,bact,cobalt)    
  end subroutine generic_COBALT_register_diag  

  !
  !   This is an internal sub, not a public interface.
  !   Add all the parameters to be used in this module.
  !
  subroutine user_add_params(param_file)

    type(param_file_type), intent(in)   :: param_file  !< structure indicating parameter file to parse

    !Specify all parameters used in this modules.
    !===============================@===============================
    !User adds one call for each parameter below!
    !User also adds the definition of each parameter in generic_COBALT_params type
    !==============================================================

    integer :: stdoutunit 

    !=============
    !Block Starts: g_tracer_add_param
    !=============
    !Add the known experimental parameters used for calculations
    !in this module.
    !All the g_tracer_add_param calls must happen between
    !g_tracer_start_param_list and g_tracer_end_param_list  calls.
    !This implementation enables runtime overwrite via field_table.

    stdoutunit=stdout()
    if (is_root_pe()) write(stdoutunit,*) '!-----------------------START-------------------------------------------'
    if (is_root_pe()) write(stdoutunit,*) '! ', trim(package_name), ' parameter check'
    if (is_root_pe()) write(stdoutunit,*) '!-----------------------START-------------------------------------------'

    call g_tracer_start_param_list(package_name)
    call get_param(param_file, "generic_COBALT", "init", cobalt%init, "init", default=.false.)

    call get_param(param_file, "generic_COBALT", "htotal_scale_lo", cobalt%htotal_scale_lo, &
                   "scaling factor for initializing carbon chemistry solver", units=" ", default=0.01)
    call get_param(param_file, "generic_COBALT", "htotal_scale_hi", cobalt%htotal_scale_hi, &
                   "scaling factor for initializing carbon chemistry solver", units=" ", default=100.0)

    call get_param(param_file, "generic_COBALT", "RHO_0", cobalt%Rho_0, "reference density", &
                   units="kg m-3", default=1035.0)
    !------------------------------------------------------------------------------------------------------------------
    ! Coefficients for O2 saturation based on Garcia and Gordon, 1992.  Oxygen solubility in seawater: Better fitting  
    ! equations.  Limnol. Oceanogr., 37(6), pp. 1307-1312. 
    ! https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.4319/lo.1992.37.6.1307
    ! Coefficients fit the Benson and Krause data in Table 1 were used.
    !------------------------------------------------------------------------------------------------------------------
    call get_param(param_file, "generic_COBALT", "a_0", cobalt%a_0, "O2 sat. regression coefficient", &
                   units="ml L-1", default=2.00907)
    call get_param(param_file, "generic_COBALT", "a_1", cobalt%a_1, "O2 sat. regression coefficient", &
                   units="ml L-1", default=3.22014)
    call get_param(param_file, "generic_COBALT", "a_2", cobalt%a_2, "O2 sat. regression coefficient", &
                   units="ml L-1", default=4.05010)
    call get_param(param_file, "generic_COBALT", "a_3", cobalt%a_3, "O2 sat. regression coefficient", &
                   units="ml L-1", default= 4.94457)
    call get_param(param_file, "generic_COBALT", "a_4", cobalt%a_4, "O2 sat. regression coefficient", &
                   units="ml L-1", default= -2.56847e-01)
    call get_param(param_file, "generic_COBALT", "a_5", cobalt%a_5, "O2 sat. regression coefficient", &
                   units="ml L-1", default= 3.88767)
    call get_param(param_file, "generic_COBALT", "b_0", cobalt%b_0, "O2 sat. regression coefficient", &
                   units="ml L-1 (ppt salt)-1)", default= -6.24523e-03)
    call get_param(param_file, "generic_COBALT", "b_1", cobalt%b_1, "O2 sat. regression coefficient", &
                   units="ml L-1 (ppt salt)-1", default= -7.37614e-03)
    call get_param(param_file, "generic_COBALT", "b_2", cobalt%b_2, "O2 sat. regression coefficient", &
                   units="ml L-1 (ppt salt)-1", default= -1.03410e-02)
    call get_param(param_file, "generic_COBALT", "b_3", cobalt%b_3, "O2 sat. regression coefficient", &
                   units="ml L-1 (ppt salt)-1", default= -8.17083e-03)
    call get_param(param_file, "generic_COBALT", "c_0", cobalt%c_0, "O2 sat. regression coefficient", &
                   units="ml L-1 (ppt salt)-2", default= -4.88682e-07)
    !------------------------------------------------------------------------------------------------------------------
    !     Schmidt number coefficients for calculating air-sea exchanges
    !------------------------------------------------------------------------------------------------------------------
    if (trim(as_param_cobalt) == "W92") then
        !  Compute the Schmidt number of CO2 in seawater using a modified version of the coefficients presented by 
        !  Wanninkhof, 1992a Relationship between wind speed and gas exchanges over the ocean. J. Geophys. Res. Oceans,
        !  97, pp. 7373-7382. https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/92JC00188.  Modifications were
        !  made to ensure the robustness of the Schmidt number relationship from -2-40 deg. C and were communicated to
        !  by Wanninkhof to John Dunne via e-mail on April 20, 2007.  Note that these are technically for seawater at
        ! 35 ppt.  ISSUE: Should we change this option to W92_extended?
        call get_param(param_file, "generic_COBALT", "a1_co2", cobalt%a1_co2, "CO2 Schmidt # regression coefficient", &
                       units="dimensionless", default= 2068.9)
        call get_param(param_file, "generic_COBALT", "a2_co2", cobalt%a2_co2, "CO2 Schmidt # regression coefficient", &
                       units="deg. C-1", default= -118.63)
        call get_param(param_file, "generic_COBALT", "a3_co2", cobalt%a3_co2, "CO2 Schmidt # regression coefficient", &
                       units="deg. C-2", default=  2.9311)
        call get_param(param_file, "generic_COBALT", "a4_co2", cobalt%a4_co2, "CO2 Schmidt # regression coefficient", &
                       units="deg. C-3", default= -0.027)
        call get_param(param_file, "generic_COBALT", "a5_co2", cobalt%a5_co2, "CO2 Schmidt # regression coefficient", &
                       units="deg. C-4", default=  0.0)     ! Not used for W92
        !  Compute the Schmidt number of O2 in seawater using the W92_extended relationships as above
        call get_param(param_file, "generic_COBALT", "a1_o2", cobalt%a1_o2, "O2 Schmidt # regression coefficient", &
                       units="dimensionless", default= 1929.7)
        call get_param(param_file, "generic_COBALT", "a2_o2", cobalt%a2_o2, "O2 Schmidt # regression coefficient", &
                       units="deg. C=1", default= -117.46)
        call get_param(param_file, "generic_COBALT", "a3_o2", cobalt%a3_o2, "O2 Schmidt # regression coefficient", &
                       units="deg. C-2", default= 3.116)
        call get_param(param_file, "generic_COBALT", "a4_o2", cobalt%a4_o2, "O2 Schmidt # regression coefficient", &
                       units="deg. C-3", default= -0.0306)
        call get_param(param_file, "generic_COBALT", "a5_o2", cobalt%a5_o2, "O2 Schmidt # regression coefficient", &
                       units="deg. C-4", default= 0.0)       ! Not used for W92
    else if ((trim(as_param_cobalt) == "W14") .or. (trim(as_param_cobalt) == "gfdl_cmip6")) then
        !  Compute the Schmidt number of CO2 in seawater using the formulation presented in Wanninkhof et al., 2014.
        !  Relationship between wind speed and gas exchange over the ocean revisited.  Limnol. Oceanogr: Methods. 12,
        !  pp. 361-362 https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.4319/lom.2014.12.351.  Note that these are
        !  technically for seawater at 35 ppt.  Alternatives values for freshwater are available in W14 but would need
        !  to be implemented 
        call get_param(param_file, "generic_COBALT", "a1_co2", cobalt%a1_co2, "CO2 Schmidt # regression coefficient", &
                       units="dimensionless", default= 2116.8)
        call get_param(param_file, "generic_COBALT", "a2_co2", cobalt%a2_co2, "CO2 Schmidt # regression coefficient", &
                       units="deg. C-1", default= -136.25)
        call get_param(param_file, "generic_COBALT", "a3_co2", cobalt%a3_co2, "CO2 Schmidt # regression coefficient", &
                       units="deg. C-2", default= 4.7353)
        call get_param(param_file, "generic_COBALT", "a4_co2", cobalt%a4_co2, "CO2 Schmidt # regression coefficient", &
                       units="deg. C-3", default= -0.092307)
        call get_param(param_file, "generic_COBALT", "a5_co2", cobalt%a5_co2, "CO2 Schmidt # regression coefficient", &
                       units="deg. C-4", default= 0.0007555)
        !  Compute the Schmidt number of O2 in seawater using the W14 relationships as above 
        call get_param(param_file, "generic_COBALT", "a1_o2", cobalt%a1_o2, "O2 Schmidt # regression coefficient", &
                       units="dimensionless", default= 1920.4)
        call get_param(param_file, "generic_COBALT", "a2_o2", cobalt%a2_o2, "O2 Schmidt # regression coefficient", &
                       units="deg. C-1", default= -135.6)
        call get_param(param_file, "generic_COBALT", "a3_o2", cobalt%a3_o2, "O2 Schmidt # regression coefficient", &
                       units="deg. C-2", default= 5.2122)
        call get_param(param_file, "generic_COBALT", "a4_o2", cobalt%a4_o2, "O2 Schmidt # regression coefficient", &
                       units="deg. C-3", default= -0.10939)
        call get_param(param_file, "generic_COBALT", "a5_o2", cobalt%a5_o2, "O2 Schmidt # regression coefficient", &
                       units="deg. C-4", default= 0.00093777)
    else
        call mpp_error(FATAL,"generic_cobalt: unable to set Schmidt number coefficients for as_param "//trim(as_param_cobalt))
    endif
    !-----------------------------------------------------------------------
    ! Stoichiometry
    !-----------------------------------------------------------------------
    !
    ! Values taken from OCMIP-II Biotic protocols after Anderson (1995) for an
    ! organic material of C106H172O38N16(H3PO4) which gives an average oxidation state
    ! for carbon of (3*16+2*38-172)/106 = -0.4528.  These calculations ignore H3PO4.
    !
    ! Nitrate Production:
    !   16*H+ + 16*NO3- + 106*CO2 + 78*H2O <-> C106H172O38N16 + 150*O2
    !   Effect is to increase alkalinity by 16 NO3 (1 mole) equivalents.
    !
    ! Ammonia Production (and reverse for remineralization):
    !   16*NH4+ + 106*CO2 + 62*H2O <-> C106H172O38N16 + 118*O2 + 16*H+
    !   Effect is to decrease (increase for remineralization) alkalinity by 16 NH4 equivalents.
    !
    ! Primary production via nitrogen fixation:
    !   8*N2 + 106*CO2 + 86*H2O <-> C106H172O38N16 + 130*O2
    !   No effect on alkalinity.
    !
    ! Nitrification:
    !   NH4+ + 2*O2 <-> NO3- + H2O + 2*H+
    !   Effect is to decrease alkalinity by 2 NH4 equivalents.
    !
    ! Denitrification:
    !   C106H172O38N16 + 472/5*NO3- + 552/5*H+ <-> 106*CO2 + 16*NH4+ + 236/5*N2 + 546/5*H2O
    !   Effect is to increase alkalinity by 552/472 = 1.169 NO3 equivalents.
    !
    ! Anammox:
    !   5NH4+ + 3NO3- --> 4N2 + 9H2O + 2H+
    !   Effect is to decrease alkalinity by 0.4 mole equivalents per mole of NH4 removed
    !
    call get_param(param_file, "generic_COBALT", "n_2_n_denit", cobalt%n_2_n_denit, &
                   "moles NO3 used per mole org. N remineralized via denitrification", &
                   units="mol N mol N-1",default= 472.0/(5.0*16.0))
    call get_param(param_file, "generic_COBALT", "no3_2_nh4_amx", cobalt%no3_2_nh4_amx, &
                   "moles NO3 used per mole NH4+ oxidized to N2 via anammox",units="mol N mol N-1", &
                   default = 3.0/5.0)
    call get_param(param_file, "generic_COBALT", "o2_2_nfix", cobalt%o2_2_nfix, &
                   "moles O2 created per mole of N fixed", units="mol O2 mol N-1", default= 130.0/16.0)
    call get_param(param_file, "generic_COBALT", "o2_2_nh4", cobalt%o2_2_nh4, &
                   "moles O2 created (consumed) per mole NH4-based prim. prod. (aerobic remineralization to NH4+)", &
                   units="mol O2 mol N-1", default=  118.0 / 16.0)
    call get_param(param_file, "generic_COBALT", "o2_2_nitrif", cobalt%o2_2_nitrif, &
                   "moles O2 consumed per mole of NH4+ nitrified to NO3-" , units="mol O2 mol N-1", default=2.0)
    call get_param(param_file, "generic_COBALT", "o2_2_no3", cobalt%o2_2_no3, &
                   "moles O2 created per mole of NO3-based prim. prod. ", units="mol O2 mol N-1", default=  150.0/16.0)
    call get_param(param_file, "generic_COBALT", "alk_2_n_denit", cobalt%alk_2_n_denit, &
                   "moles alkalinity created per mole NO3- consumed during denitrification", & 
                   units="mol alk mol N-1 ", default= 552.0/472.0)
    call get_param(param_file, "generic_COBALT", "alk_2_nh4_amx", cobalt%alk_2_nh4_amx, &
                   "moles alkalinity removed per mole NH4+ consumed via anammox", units="mol alk mol N-1", &
                   default= 2.0/5.0)
    !
    !-----------------------------------------------------------------------
    ! Nutrient Limitation Parameters (phytoplankton)
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "k_fed_Di", phyto(DIAZO)%k_fed,            "k_fed_Di",       units="mol Fed kg-1", default=4.0e-9)  ! mol Fed kg-1
    call get_param(param_file, "generic_COBALT", "k_fed_Lg", phyto(LARGE)%k_fed,            "k_fed_Lg",       units="mol Fed kg-1", default=2.0e-9)  ! mol Fed kg-1
    call get_param(param_file, "generic_COBALT", "k_fed_Md", phyto(MEDIUM)%k_fed,           "k_fed_Md",       units="mol Fed kg-1", default=8.0e-10) ! mol Fed kg-1
    call get_param(param_file, "generic_COBALT", "k_fed_Sm", phyto(SMALL)%k_fed,            "k_fed_Sm",       units="mol Fed kg-1", default=4.0e-10) ! mol Fed kg-1
    call get_param(param_file, "generic_COBALT", "k_nh4_Lg", phyto(LARGE)%k_nh4,            "k_nh4_Lg",       units="mol NH4 kg-1", default=5.0e-8)  ! mol NH4 kg-1
    call get_param(param_file, "generic_COBALT", "k_nh4_Md", phyto(MEDIUM)%k_nh4,           "k_nh4_Md",       units="mol NH4 kg-1", default=2.0e-8)  ! mol NH4 kg-1
    call get_param(param_file, "generic_COBALT", "k_nh4_Sm", phyto(SMALL)%k_nh4,            "k_nh4_Sm",       units="mol NH4 kg-1", default=1.0e-8)  ! mol NH4 kg-1
    call get_param(param_file, "generic_COBALT", "k_nh4_Di", phyto(DIAZO)%k_nh4,            "k_nh4_Di",       units="mol NH4 kg-1", default=1.0e-7)  ! mol NH4 kg-1
    call get_param(param_file, "generic_COBALT", "k_no3_Lg", phyto(LARGE)%k_no3,            "k_no3_Lg",       units="mol NO3 kg-1", default=2.5e-6)  ! mol NO3 kg-1
    call get_param(param_file, "generic_COBALT", "k_no3_Md", phyto(MEDIUM)%k_no3,           "k_no3_Md",       units="mol NO3 kg-1", default=1.0e-6)  ! mol NO3 kg-1
    call get_param(param_file, "generic_COBALT", "k_no3_Sm", phyto(SMALL)%k_no3,            "k_no3_Sm",       units="mol NO3 kg-1", default=5.0e-7)  ! mol NO3 kg-1
    call get_param(param_file, "generic_COBALT", "k_no3_Di", phyto(DIAZO)%k_no3,            "k_no3_Di",       units="mol NO3 kg-1", default=5.0e-6)  ! mol NO3 kg-1
    call get_param(param_file, "generic_COBALT", "k_po4_Di", phyto(DIAZO)%k_po4,            "k_po4_Di",       units="mol PO4 kg-1", default=1.0e-7)  ! mol PO4 kg-1
    call get_param(param_file, "generic_COBALT", "k_po4_Lg", phyto(LARGE)%k_po4,            "k_po4_Lg",       units="mol PO4 kg-1", default=5.0e-8)  ! mol PO4 kg-1
    call get_param(param_file, "generic_COBALT", "k_po4_Md", phyto(MEDIUM)%k_po4,           "k_po4_Md",       units="mol PO4 kg-1", default=2.0e-8)  ! mol PO4 kg-1
    call get_param(param_file, "generic_COBALT", "k_po4_Sm", phyto(SMALL)%k_po4,            "k_po4_Sm",       units="mol PO4 kg-1", default=1.0e-8)  ! mol PO4 kg-1
    call get_param(param_file, "generic_COBALT", "k_sio4_Lg",phyto(LARGE)%k_sio4,           "k_sio4_Lg",      units="mol SiO4 kg-1", default=2.0e-6) ! mol SiO4 kg-1
    call get_param(param_file, "generic_COBALT", "k_sio4_Md",phyto(MEDIUM)%k_sio4,          "k_sio4_Md",      units="mol SiO4 kg-1", default=1.0e-6) ! mol SiO4 kg-1
    call get_param(param_file, "generic_COBALT", "k_fe_2_n_Di", phyto(DIAZO)%k_fe_2_n,      "k_fe_2_n_Di",    units="mol Fe kg-1", &
                   default= 12.0e-6, scale=c2n)! mol Fe mol N-1
    call get_param(param_file, "generic_COBALT", "k_fe_2_n_Lg", phyto(LARGE)%k_fe_2_n,      "k_fe_2_n_Lg",    units="mol Fe kg-1", &
                   default= 10.0e-6, scale=c2n)! mol Fe mol N-1
    call get_param(param_file, "generic_COBALT", "k_fe_2_n_Md", phyto(MEDIUM)%k_fe_2_n,     "k_fe_2_n_Md",    units="mol Fe kg-1", &
                   default= 4.0e-6, scale=c2n)! mol Fe mol N-1
    call get_param(param_file, "generic_COBALT", "k_fe_2_n_Sm",phyto(SMALL)%k_fe_2_n,       "k_fe_2_n_Sm",    units="mol Fe kg-1", &
                   default= 2.0e-6, scale=c2n)! mol Fe mol N-1
    call get_param(param_file, "generic_COBALT", "fe_2_n_max_Sm",phyto(SMALL)%fe_2_n_max,   "fe_2_n_max_Sm",  units="mol Fe kg-1", &
                   default= 50.0e-6, scale=c2n)! mol Fe mol N-1
    call get_param(param_file, "generic_COBALT", "fe_2_n_max_Md", phyto(MEDIUM)%fe_2_n_max, "fe_2_n_max_Md",  units="mol Fe kg-1", &
                   default= 250.0e-6, scale=c2n)! mol Fe mol N-1
    call get_param(param_file, "generic_COBALT", "fe_2_n_max_Lg", phyto(LARGE)%fe_2_n_max,  "fe_2_n_max_Lg",  units="mol Fe kg-1", &
                   default= 500.0e-6, scale=c2n)! mol Fe mol N-1
    call get_param(param_file, "generic_COBALT", "fe_2_n_max_Di", phyto(DIAZO)%fe_2_n_max,  "fe_2_n_max_Di",  units="mol Fe kg-1", &
                   default= 500.0e-6, scale=c2n)! mol Fe mol N-1
    call get_param(param_file, "generic_COBALT", "fe_2_n_upt_fac", cobalt%fe_2_n_upt_fac,   "fe_2_n_upt_fac", units="mol Fe kg-1", default= 60.0e-6) ! mol Fe mol N-1
    !
    !-----------------------------------------------------------------------
    ! Phytoplankton light limitation/growth rate
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "alpha_Di_hl", phyto(DIAZO)%alpha_hl,  "alpha_Di_hl", units="g C g chl-1 s-1", &
                   default= 0.4e-5, scale = micromolQpersec2W )! g C g Chl-1 sec-1 (W m-2)-1
    call get_param(param_file, "generic_COBALT", "alpha_Lg_hl", phyto(LARGE)%alpha_hl,  "alpha_Lg_hl", units="g C g chl-1 s-1", &
                   default= 0.4e-5, scale = micromolQpersec2W )! g C g Chl-1 sec-1 (W m-2)-1
    call get_param(param_file, "generic_COBALT", "alpha_Md_hl", phyto(MEDIUM)%alpha_hl, "alpha_Md_hl", units="g C g chl-1 s-1", &
                   default= 0.8e-5, scale = micromolQpersec2W )! g C g Chl-1 sec-1 (W m-2)-1
    call get_param(param_file, "generic_COBALT", "alpha_Sm_hl", phyto(SMALL)%alpha_hl,  "alpha_Sm_hl", units="g C g chl-1 s-1", &
                   default= 1.6e-5, scale = micromolQpersec2W )! g C g Chl-1 sec-1 (W m-2)-1
    call get_param(param_file, "generic_COBALT", "alpha_Di_ll", phyto(DIAZO)%alpha_ll,  "alpha_Di_ll", units="g C g chl-1 s-1", &
                   default= 0.8e-5, scale = micromolQpersec2W )! g C g Chl-1 sec-1 (W m-2)-1
    call get_param(param_file, "generic_COBALT", "alpha_Lg_ll", phyto(LARGE)%alpha_ll,  "alpha_Lg_ll", units="g C g chl-1 s-1", &
                   default= 0.8e-5, scale = micromolQpersec2W )! g C g Chl-1 sec-1 (W m-2)-1
    call get_param(param_file, "generic_COBALT", "alpha_Md_ll", phyto(MEDIUM)%alpha_ll, "alpha_Md_ll", units="g C g chl-1 s-1", & 
                   default= 1.6e-5, scale = micromolQpersec2W )! g C g Chl-1 sec-1 (W m-2)-1
    call get_param(param_file, "generic_COBALT", "alpha_Sm_ll", phyto(SMALL)%alpha_ll,  "alpha_Sm_ll", units="g C g chl-1 s-1", &
                   default= 3.2e-5, scale = micromolQpersec2W )! g C g Chl-1 sec-1 (W m-2)-1

    call get_param(param_file, "generic_COBALT", "kappa_eppley",        cobalt%kappa_eppley,            "kappa_eppley",        units="deg C-1",         default= 0.063)                   ! deg C-1
    call get_param(param_file, "generic_COBALT", "P_C_max_Di_hl", phyto(DIAZO)%P_C_max_hl, "P_C_max_Di_hl", units="day-1", &
                   default= 0.6, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "P_C_max_Lg_hl", phyto(LARGE)%P_C_max_hl, "P_C_max_Lg_hl", units="day-1", &
                   default= 1.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "P_C_max_Md_hl", phyto(MEDIUM)%P_C_max_hl,"P_C_max_Md_hl", units="day-1", &
                   default= 1.1, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "P_C_max_Sm_hl", phyto(SMALL)%P_C_max_hl, "P_C_max_Sm_hl", units="day-1", &
                   default= 1.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "P_C_max_Di_ll", phyto(DIAZO)%P_C_max_ll, "P_C_max_Di_ll", units="day-1", &
                   default= 0.3, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "P_C_max_Lg_ll", phyto(LARGE)%P_C_max_ll, "P_C_max_Lg_ll", units="day-1", &
                   default= 0.5, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "P_C_max_Md_ll", phyto(MEDIUM)%P_C_max_ll,"P_C_max_Md_ll", units="day-1", &
                   default= 0.55, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "P_C_max_Sm_ll", phyto(SMALL)%P_C_max_ll, "P_C_max_Sm_ll", units="day-1", &
                   default= 0.5, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "numlightadapt",       cobalt%numlightadapt,           "numlightadapt",       units="",                default= 10)               ! dimensionless
    call get_param(param_file, "generic_COBALT", "thetamax_Di",         phyto(DIAZO)%thetamax,          "thetamax_Di",         units="g chl g C-1",     default= 0.035)                  ! g Chl g C-1
    call get_param(param_file, "generic_COBALT", "thetamax_Lg",         phyto(LARGE)%thetamax,          "thetamax_Lg",         units="g chl g C-1",     default= 0.07)                  ! g Chl g C-1
    call get_param(param_file, "generic_COBALT", "thetamax_Md",         phyto(MEDIUM)%thetamax,         "thetamax_Md",         units="g chl g C-1",     default= 0.045)                 ! g Chl g C-1
    call get_param(param_file, "generic_COBALT", "thetamax_Sm",         phyto(SMALL)%thetamax,          "thetamax_Sm",         units="g chl g C-1",     default= 0.035)                  ! g Chl g C-1
    call get_param(param_file, "generic_COBALT", "bresp_frac_mixed_Di", phyto(DIAZO)%bresp_frac_mixed,  "bresp_frac_mixed_Di", units="",                default= 0.02)   ! none
    call get_param(param_file, "generic_COBALT", "bresp_frac_mixed_Lg", phyto(LARGE)%bresp_frac_mixed,  "bresp_frac_mixed_Lg", units="",                default= 0.02)   ! none
    call get_param(param_file, "generic_COBALT", "bresp_frac_mixed_Md", phyto(MEDIUM)%bresp_frac_mixed, "bresp_frac_mixed_Md", units="",                default= 0.02)  ! none
    call get_param(param_file, "generic_COBALT", "bresp_frac_mixed_Sm", phyto(SMALL)%bresp_frac_mixed,  "bresp_frac_mixed_Sm", units="",                default= 0.02)   ! none
    call get_param(param_file, "generic_COBALT", "bresp_frac_strat_Di", phyto(DIAZO)%bresp_frac_strat,  "bresp_frac_strat_Di", units="",                default= 0.01)   ! none
    call get_param(param_file, "generic_COBALT", "bresp_frac_strat_Lg", phyto(LARGE)%bresp_frac_strat,  "bresp_frac_strat_Lg", units="",                default= 0.01)   ! none
    call get_param(param_file, "generic_COBALT", "bresp_frac_strat_Md", phyto(MEDIUM)%bresp_frac_strat, "bresp_frac_strat_Md", units="",                default= 0.01)  ! none
    call get_param(param_file, "generic_COBALT", "bresp_frac_strat_Sm", phyto(SMALL)%bresp_frac_strat,  "bresp_frac_strat_Sm", units="",                default= 0.01)   ! none
    call get_param(param_file, "generic_COBALT", "sink_max_Di",         phyto(DIAZO)%sink_max,          "sink_max_Di",         units="m day-1", &
                   default= 1.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "sink_max_Lg",         phyto(LARGE)%sink_max,          "sink_max_Lg",         units="m day-1", &
                   default= 5.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "sink_max_Md",         phyto(MEDIUM)%sink_max,         "sink_max_Md",         units="m day-1", &
                   default= 1.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "sink_max_Sm",         phyto(SMALL)%sink_max,          "sink_max_Sm",         units="m day-1", &
                   default= 0.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "thetamin",            cobalt%thetamin,                "thetamin",            units="g chl g C-1", default= 0.002) ! g Chl g C-1
    call get_param(param_file, "generic_COBALT", "zeta",                cobalt%zeta,                    "zeta",                units="",            default= 0.05)  ! dimensionless
    call get_param(param_file, "generic_COBALT", "par_adj",             cobalt%par_adj,                 "par_adj",             units="",            default= 0.83)  ! dimensionless
    call get_param(param_file, "generic_COBALT", "gamma_irr_aclm",      cobalt%gamma_irr_aclm,          "gamma_irr_aclm",      units="day-1", &
                   default= 1.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "gamma_mu_mem",        cobalt%gamma_mu_mem,            "gamma_mu_mem",        units="day-1", &
                   default= 1.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "ml_aclm_efold",       cobalt%ml_aclm_efold,           "ml_aclm_efold",       units="",            default= 2.5)   ! dimensionless
    call get_param(param_file, "generic_COBALT", "zmld_ref",            cobalt%zmld_ref,                "zmld_ref",            units="m",           default= 10.0)  ! m
    call get_param(param_file, "generic_COBALT", "densdiff_mld",        cobalt%densdiff_mld,            "densdiff_mld",        units="kg m-3",      default= 0.03)  ! kg m-3
    call get_param(param_file, "generic_COBALT", "irrad_day_thresh",    cobalt%irrad_day_thresh,        "irrad_day_thresh",    units="watts m-2",   default= 1.0 )  ! watts m-2
    call get_param(param_file, "generic_COBALT", "do_case2_mod",        cobalt%do_case2_mod, &
                   "When ture, modify the opacity of case 2 (coastal) waters"//&
                   "which are identified based on a temperature and depth threshold", default=.false. )
    if (cobalt%do_case2_mod) then
      call get_param(param_file, "generic_COBALT", "case2_depth",    cobalt%case2_depth, &
                     "Depth threshold to identify Case 2 water when using a modified opacity.", units="m",  default=30.0 )                  ! m
      call get_param(param_file, "generic_COBALT", "case2_salt",     cobalt%case2_salt,  &
                     "Salinity threshold to identify Case 2 water when using a modified opacity.", units="PSU", default=30.0 )                    ! PSU
      call get_param(param_file, "generic_COBALT", "case2_opac_add", cobalt%case2_opac_add, &     ! m-1
                     "Additional opactity added in Case 2 waters when using a modified opacity.", units="m-1", default=0.05 )
    else
      cobalt%case2_depth = 0.0                                                           ! m
      cobalt%case2_salt = 0.0                                                            ! PSU
      cobalt%case2_opac_add = 0.0                                                        ! m-1
    endif
    call get_param(param_file, "generic_COBALT", "min_daylength",       cobalt%min_daylength,           "min_daylength",       units="hours",       default= 6.0 )                 ! hours
    call get_param(param_file, "generic_COBALT", "refuge_conc",         cobalt%refuge_conc,             "refuge_conc",         units="mol kg-1",    default= 1.0e-10)                  ! moles N kg-1
    !
    !-----------------------------------------------------------------------
    ! Nitrogen fixation inhibition parameters
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "o2_inhib_Di_pow", cobalt%o2_inhib_Di_pow, "o2_inhib_Di_pow", units="mol O2-1 m3",   default=4.0)                 ! mol O2-1 m3
    call get_param(param_file, "generic_COBALT", "o2_inhib_Di_sat", cobalt%o2_inhib_Di_sat, "o2_inhib_Di_sat", units="mol kg-1",      default=3.0e-4)              ! mol O2 kg-1
    !
    !-----------------------------------------------------------------------
    ! Other stoichiometry
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "p_2_n_static",     cobalt%p_2_n_static,          "p_2_n_static",    default=.false. )
    call get_param(param_file, "generic_COBALT", "c_2_n",            cobalt%c_2_n,                 "c_2_n",           units="", default= 106.0 / 16.0)
    call get_param(param_file, "generic_COBALT", "p_2_n_static_Di",  phyto(DIAZO)%p_2_n_static,    "p_2_n_static_Di", units="mol P mol N-1", default= 1.0/40.0 )         ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_static_Lg",  phyto(LARGE)%p_2_n_static,    "p_2_n_static_Lg", units="mol P mol N-1", default= 1.0/14.0 )         ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_static_Md",  phyto(MEDIUM)%p_2_n_static,   "p_2_n_static_Md", units="mol P mol N-1", default= 1.0/20.0 )        ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_static_Sm",  phyto(SMALL)%p_2_n_static,    "p_2_n_static_Sm", units="mol P mol N-1", default= 1.0/24.0 )         ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_min_Di",     phyto(DIAZO)%p_2_n_min,       "p_2_n_min_Di",    units="mol P mol N-1", default= 1.0/40.0 )               ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_slope_Di",   phyto(DIAZO)%p_2_n_slope,     "p_2_n_slope_Di",  units="mol P mol N-1 mol P-1 kg", default= 0.0*1.0e6)          ! mol P mol N-1 mol P-1 kg
    call get_param(param_file, "generic_COBALT", "p_2_n_max_Di",     phyto(DIAZO)%p_2_n_max,       "p_2_n_max_Di",    units="mol P mol N-1", default= 1.0/40.0 )               ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_min_Sm",     phyto(SMALL)%p_2_n_min,       "p_2_n_min_Sm",    units="mol P mol N-1", default= 1.0/31.0 )               ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_slope_Sm",   phyto(SMALL)%p_2_n_slope,     "p_2_n_slope_Sm",  units="mol P mol N-1 mol P-1 kg", default= 0.048*1.0e6)        ! mol P mol N-1 mol P-1 kg
    call get_param(param_file, "generic_COBALT", "p_2_n_max_Sm",     phyto(SMALL)%p_2_n_max,       "p_2_n_max_Sm",    units="mol P mol N-1", default= 1.0/20.0 )               ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_min_Md",     phyto(MEDIUM)%p_2_n_min,      "p_2_n_min_Md",    units="mol P mol N-1", default= 1.0/31.0 )              ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_slope_Md",   phyto(MEDIUM)%p_2_n_slope,    "p_2_n_slope_Md",  units="mol P mol N-1 mol P-1 kg", default= 0.048*1.0e6)       ! mol P mol N-1 mol P-1 kg
    call get_param(param_file, "generic_COBALT", "p_2_n_max_Md",     phyto(MEDIUM)%p_2_n_max,      "p_2_n_max_Md",    units="mol P mol N-1", default= 1.0/16.0 )              ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_min_Lg",     phyto(LARGE)%p_2_n_min,       "p_2_n_min_Lg",    units="mol P mol N-1", default= 1.0/31.0 )               ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "p_2_n_slope_Lg",   phyto(LARGE)%p_2_n_slope,     "p_2_n_slope_Lg",  units="mol P mol N-1 mol P-1 kg", default= 0.048*1.0e6)        ! mol P mol N-1 mol P-1 kg
    call get_param(param_file, "generic_COBALT", "p_2_n_max_Lg",     phyto(LARGE)%p_2_n_max,       "p_2_n_max_Lg",    units="mol P mol N-1", default= 1.0/14.0 )               ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "si_2_n_static_Lg", phyto(LARGE)%si_2_n_static,   "si_2_n_static_Lg",units="mol Si mol N-1", default= 2.0)            ! mol Si mol N-1
    call get_param(param_file, "generic_COBALT", "si_2_n_static_Md", phyto(MEDIUM)%si_2_n_static,  "si_2_n_static_Md",units="mol Si mol N-1", default= 2.0)           ! mol Si mol N-1
    call get_param(param_file, "generic_COBALT", "si_2_n_max_Lg",    phyto(LARGE)%si_2_n_max,      "si_2_n_max_Lg",   units="mol Si mol N-1", default= 3.0)                  ! mol Si mol N-1
    call get_param(param_file, "generic_COBALT", "si_2_n_max_Lg",    phyto(MEDIUM)%si_2_n_max,     "si_2_n_max_Lg",   units="mol Si mol N-1", default= 1.0)                 ! mol Si mol N-1
    !
    !-----------------------------------------------------------------------
    ! Zooplankton Stoichiometry - presently static
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "q_p_2_n_smz",zoo(1)%q_p_2_n,"q_p_2_n_smz", units="mol P mol N-1", default= 1.0/20.0)          ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "q_p_2_n_mdz",zoo(2)%q_p_2_n,"q_p_2_n_mdz", units="mol P mol N-1", default= 1.0/18.0)          ! mol P mol N-1
    call get_param(param_file, "generic_COBALT", "q_p_2_n_lgz",zoo(3)%q_p_2_n,"q_p_2_n_lgz", units="mol P mol N-1", default= 1.0/16.0)          ! mol P mol N-1
    !
    !-----------------------------------------------------------------------
    ! Bacteria Stoichiometry - presently static
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "q_p_2_n_bact",bact(1)%q_p_2_n, "q_p_2_n_bact", units="mol P mol N-1", default=1.0/16.0)        ! mol P mol N-1
    !
    !
    !-----------------------------------------------------------------------
    ! Phytoplankton aggregation
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "agg_Sm",           phyto(SMALL)%agg,  "agg_Sm", units="day-1(mol N kg)-1", &
                   default=0.05, scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "agg_Di",           phyto(DIAZO)%agg,  "agg_Di", units="day-1(mol N kg)-1", & 
                   default=0.0 , scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "agg_Lg",           phyto(LARGE)%agg,  "agg_Lg", units="day-1(mol N kg)-1", &
                   default=0.25, scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "agg_Md",           phyto(MEDIUM)%agg, "agg_Md", units="day-1(mol N kg)-1", &
                   default=0.10, scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "frac_mu_stress_Sm",phyto(SMALL)%frac_mu_stress,  "frac_mu_stress_Sm",units="", default=0.25)  ! none
    call get_param(param_file, "generic_COBALT", "frac_mu_stress_Di",phyto(DIAZO)%frac_mu_stress,  "frac_mu_stress_Di",units="", default=0.25)  ! none
    call get_param(param_file, "generic_COBALT", "frac_mu_stress_Lg",phyto(LARGE)%frac_mu_stress,  "frac_mu_stress_Lg",units="", default=0.25)  ! none
    call get_param(param_file, "generic_COBALT", "frac_mu_stress_Md",phyto(MEDIUM)%frac_mu_stress, "frac_mu_stress_Md",units="", default=0.25)  ! none
    !
    !-----------------------------------------------------------------------
    ! Phytoplankton and bacterial losses to viruses
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "vir_Sm",    phyto(SMALL)%vir,  "vir_Sm",   units="day-1 (mole N kg)-1", &
                   default=0.25, scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "vir_Di",    phyto(DIAZO)%vir,  "vir_Di",   units="day-1 (mole N kg)-1", &
                   default=0.05, scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "vir_Lg",    phyto(LARGE)%vir,  "vir_Lg",   units="day-1 (mole N kg)-1", &
                   default=0.05, scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "vir_Md",    phyto(MEDIUM)%vir, "vir_Md",   units="day-1 (mole N kg)-1", &
                   default=0.125, scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "vir_Bact",  bact(1)%vir,       "vir_Bact", units="day-1 (mole N kg)-1", &
                   default=0.25, scale = micromol2mol / sperd)  ! s-1 (mole N kg)-1
    call get_param(param_file, "generic_COBALT", "ktemp_vir", cobalt%vir_ktemp,  "ktemp_vir", units="C-1", default= 0.063)           ! C-1
    !
    !-----------------------------------------------------------------------
    ! Phytoplankton losses to mortality
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "mort_Sm",phyto(SMALL)%mort,  "mort_Sm",units="day-1", &
                   default= 0.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "mort_Di",phyto(DIAZO)%mort,  "mort_Di",units="day-1", &
                   default= 0.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "mort_Lg",phyto(LARGE)%mort,  "mort_Lg",units="day-1", &
                   default= 0.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "mort_Md",phyto(MEDIUM)%mort, "mort_Md",units="day-1", &
                   default= 0.0, scale = I_sperd ) ! s-1
    !
    !-----------------------------------------------------------------------
    ! Phytoplankton losses to exudation
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "exu_Sm",phyto(SMALL)%exu, "exu_Sm",units="", default=0.13)       ! dimensionless (fraction of NPP)
    call get_param(param_file, "generic_COBALT", "exu_Di",phyto(DIAZO)%exu, "exu_Di",units="", default=0.13)       ! dimensionless (fraction of NPP)
    call get_param(param_file, "generic_COBALT", "exu_Lg",phyto(LARGE)%exu, "exu_Lg",units="", default=0.13)       ! dimensionless (fraction of NPP)
    call get_param(param_file, "generic_COBALT", "exu_Md",phyto(MEDIUM)%exu,"exu_Md",units="", default=0.13)       ! dimensionless (fraction of NPP)
    !
    !-----------------------------------------------------------------------
    ! Zooplankton ingestion parameterization and temperature dependence
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "imax_smz", zoo(1)%imax, "imax_smz", units="day-1", & 
                   default= 0.8*1.42, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "imax_mdz", zoo(2)%imax, "imax_mdz", units="day-1", & 
                   default= 0.57, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "imax_lgz", zoo(3)%imax, "imax_lgz", units="day-1", & 
                   default= 0.23, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "ki_smz",           zoo(1)%ki,                "ki_smz",           units="mol N kg-1", default=1.25e-6)                        ! moles N kg-1
    call get_param(param_file, "generic_COBALT", "ki_mdz",           zoo(2)%ki,                "ki_mdz",           units="mol N kg-1", default=1.25e-6)                        ! moles N kg-1
    call get_param(param_file, "generic_COBALT", "ki_lgz",           zoo(3)%ki,                "ki_lgz",           units="mol N kg-1", default=1.25e-6)                        ! moles N kg-1
    call get_param(param_file, "generic_COBALT", "ktemp_smz",        zoo(1)%ktemp,             "ktemp_smz",        units="C-1", default=0.063)                   ! C-1
    call get_param(param_file, "generic_COBALT", "ktemp_mdz",        zoo(2)%ktemp,             "ktemp_mdz",        units="C-1", default=0.063)                   ! C-1
    call get_param(param_file, "generic_COBALT", "ktemp_lgz",        zoo(3)%ktemp,             "ktemp_lgz",        units="C-1", default=0.063)                   ! C-1
    !
    !-----------------------------------------------------------------------
    ! Bacterial growth and uptake parameters
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "mu_max_bact",   bact(1)%mu_max,     "mu_max_bact",   units="day-1", & 
                   default= 1.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "k_ldon_bact",   bact(1)%k_ldon,     "k_ldon_bact",   units="mol ldon kg-1", default= 5.0e-7)            ! mol ldon kg-1
    call get_param(param_file, "generic_COBALT", "ktemp_bact",    bact(1)%ktemp,      "ktemp_bact",    units="C-1", default= 0.063)                ! C-1
    call get_param(param_file, "generic_COBALT", "gge_max_bact",  bact(1)%gge_max,    "gge_max_bact",  units="", default= 0.4)                ! dimensionless
    call get_param(param_file, "generic_COBALT", "bresp_bact",    bact(1)%bresp,      "bresp_bact",    units="day-1", & 
                   default= 0.0075, scale = I_sperd ) ! s-1
    !
    !-----------------------------------------------------------------------
    ! Zooplankton switching and prey preference parameters
    !-----------------------------------------------------------------------
    !
    ! parameters controlling the extent of biomass-based switching between
    ! multiple prey options
    call get_param(param_file, "generic_COBALT", "nswitch_smz",zoo(1)%nswitch, "nswitch_smz", units="unitless", default= 2.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "nswitch_mdz",zoo(2)%nswitch, "nswitch_mdz", units="unitless", default= 2.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "nswitch_lgz",zoo(3)%nswitch, "nswitch_lgz", units="unitless", default= 2.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "mswitch_smz",zoo(1)%mswitch, "mswitch_smz", units="unitless", default= 2.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "mswitch_mdz",zoo(2)%mswitch, "mswitch_mdz", units="unitless", default= 2.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "mswitch_lgz",zoo(3)%mswitch, "mswitch_lgz", units="unitless", default= 2.0)          ! dimensionless
    ! innate prey availability for small zooplankton
    call get_param(param_file, "generic_COBALT", "smz_ipa_smp", zoo(1)%ipa_smp,  "smz_ipa_smp",  units="unitless", default=1.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "smz_ipa_mdp", zoo(1)%ipa_mdp,  "smz_ipa_mdp",  units="unitless", default=0.4)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "smz_ipa_lgp", zoo(1)%ipa_lgp,  "smz_ipa_lgp",  units="unitless", default=0.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "smz_ipa_diaz",zoo(1)%ipa_diaz, "smz_ipa_diaz", units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "smz_ipa_smz", zoo(1)%ipa_smz,  "smz_ipa_smz",  units="unitless", default=0.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "smz_ipa_mdz", zoo(1)%ipa_mdz,  "smz_ipa_mdz",  units="unitless", default=0.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "smz_ipa_lgz", zoo(1)%ipa_lgz,  "smz_ipa_lgz",  units="unitless", default=0.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "smz_ipa_bact",zoo(1)%ipa_bact, "smz_ipa_bact", units="unitless", default=0.5)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "smz_ipa_det", zoo(1)%ipa_det,  "smz_ipa_det",  units="unitless", default=0.0)          ! dimensionless
    ! innate prey availability for medium zooplankton
    call get_param(param_file, "generic_COBALT", "mdz_ipa_smp", zoo(2)%ipa_smp,  "mdz_ipa_smp",  units="unitless", default=0.4)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "mdz_ipa_mdp", zoo(2)%ipa_mdp,  "mdz_ipa_mdp",  units="unitless", default=1.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "mdz_ipa_lgp", zoo(2)%ipa_lgp,  "mdz_ipa_lgp",  units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "mdz_ipa_diaz",zoo(2)%ipa_diaz, "mdz_ipa_diaz", units="unitless", default=0.75)        ! dimensionless
    call get_param(param_file, "generic_COBALT", "mdz_ipa_smz", zoo(2)%ipa_smz,  "mdz_ipa_smz",  units="unitless", default=1.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "mdz_ipa_mdz", zoo(2)%ipa_mdz,  "mdz_ipa_mdz",  units="unitless", default=0.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "mdz_ipa_lgz", zoo(2)%ipa_lgz,  "mdz_ipa_lgz",  units="unitless", default=0.0)          ! dimensionless
    call get_param(param_file, "generic_COBALT", "mdz_ipa_bact",zoo(2)%ipa_bact, "mdz_ipa_bact", units="unitless", default=0.0)        ! dimensionless
    call get_param(param_file, "generic_COBALT", "mdz_ipa_det", zoo(2)%ipa_det,  "mdz_ipa_det",  units="unitless", default=0.0)          ! dimensionless
    ! innate prey availability large predatory zooplankton/krill
    call get_param(param_file, "generic_COBALT", "lgz_ipa_smp", zoo(3)%ipa_smp,  "lgz_ipa_smp",  units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "lgz_ipa_mdp", zoo(3)%ipa_mdp,  "lgz_ipa_mdp",  units="unitless", default=0.4)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "lgz_ipa_lgp", zoo(3)%ipa_lgp,  "lgz_ipa_lgp",  units="unitless", default=1.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "lgz_ipa_diaz",zoo(3)%ipa_diaz, "lgz_ipa_diaz", units="unitless", default=0.4)       ! dimensionless
    call get_param(param_file, "generic_COBALT", "lgz_ipa_smz", zoo(3)%ipa_smz,  "lgz_ipa_smz",  units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "lgz_ipa_mdz", zoo(3)%ipa_mdz,  "lgz_ipa_mdz",  units="unitless", default=1.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "lgz_ipa_lgz", zoo(3)%ipa_lgz,  "lgz_ipa_lgz",  units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "lgz_ipa_bact",zoo(3)%ipa_bact, "lgz_ipa_bact", units="unitless", default=0.0)       ! dimensionless
    call get_param(param_file, "generic_COBALT", "lgz_ipa_det", zoo(3)%ipa_det,  "lgz_ipa_det",  units="unitless", default=0.0)         ! dimensionless
    !
    !----------------------------------------------------------------------
    ! Zooplankton bioenergetics
    !----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "gge_max_smz",zoo(1)%gge_max, "gge_max_smz",  units="unitless", default=0.4)              ! dimensionless
    call get_param(param_file, "generic_COBALT", "gge_max_mdz",zoo(2)%gge_max, "gge_max_mdz",  units="unitless", default=0.4)              ! dimensionless
    call get_param(param_file, "generic_COBALT", "gge_max_lgz",zoo(3)%gge_max, "gge_max_lgz",  units="unitless", default=0.4)              ! dimensionless
    call get_param(param_file, "generic_COBALT", "bresp_smz",  zoo(1)%bresp,   "bresp_smz",    units="day-1", & 
                   default= 0.8*0.020, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "bresp_mdz",  zoo(2)%bresp,   "bresp_mdz",    units="day-1", & 
                   default= 0.008, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "bresp_lgz",  zoo(3)%bresp,   "bresp_lgz",    units="day-1", & 
                   default= 0.0032, scale = I_sperd ) ! s-1
    !
    !----------------------------------------------------------------------
    ! Partitioning of zooplankton ingestion to other compartments
    !----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "phi_det_smz",   zoo(1)%phi_det,    "phi_det_smz",   units="unitless", default= 0.00)            ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_det_mdz",   zoo(2)%phi_det,    "phi_det_mdz",   units="unitless", default= 0.15)            ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_det_lgz",   zoo(3)%phi_det,    "phi_det_lgz",   units="unitless", default= 0.30)            ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_ldon_smz",  zoo(1)%phi_ldon,   "phi_ldon_smz",  units="unitless", default= 0.625*0.30)      ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_ldon_mdz",  zoo(2)%phi_ldon,   "phi_ldon_mdz",  units="unitless", default= 0.625*0.15)      ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_ldon_lgz",  zoo(3)%phi_ldon,   "phi_ldon_lgz",  units="unitless", default= 0.625*0.0)       ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_ldop_smz",  zoo(1)%phi_ldop,   "phi_ldop_smz",  units="unitless", default= 0.575*0.30)     ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_ldop_mdz",  zoo(2)%phi_ldop,   "phi_ldop_mdz",  units="unitless", default= 0.575*0.15)     ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_ldop_lgz",  zoo(3)%phi_ldop,   "phi_ldop_lgz",  units="unitless", default= 0.575*0.0)      ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_srdon_smz", zoo(1)%phi_srdon,  "phi_srdon_smz", units="unitless", default= 0.075*0.30)    ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_srdon_mdz", zoo(2)%phi_srdon,  "phi_srdon_mdz", units="unitless", default= 0.075*0.15)    ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_srdon_lgz", zoo(3)%phi_srdon,  "phi_srdon_lgz", units="unitless", default= 0.075*0.0)     ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_srdop_smz", zoo(1)%phi_srdop,  "phi_srdop_smz", units="unitless", default= 0.125*0.30)   ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_srdop_mdz", zoo(2)%phi_srdop,  "phi_srdop_mdz", units="unitless", default= 0.125*0.15)   ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_srdop_lgz", zoo(3)%phi_srdop,  "phi_srdop_lgz", units="unitless", default= 0.125*0.0)    ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_sldon_smz", zoo(1)%phi_sldon,  "phi_sldon_smz", units="unitless", default= 0.3*0.30)    ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_sldon_mdz", zoo(2)%phi_sldon,  "phi_sldon_mdz", units="unitless", default= 0.3*0.15)    ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_sldon_lgz", zoo(3)%phi_sldon,  "phi_sldon_lgz", units="unitless", default= 0.3*0.0)     ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_sldop_smz", zoo(1)%phi_sldop,  "phi_sldop_smz", units="unitless", default= 0.3*0.30)    ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_sldop_mdz", zoo(2)%phi_sldop,  "phi_sldop_mdz", units="unitless", default= 0.3*0.15)    ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_sldop_lgz", zoo(3)%phi_sldop,  "phi_sldop_lgz", units="unitless", default= 0.3*0.0)     ! dimensionless
    !
    !----------------------------------------------------------------------
    ! Partitioning of viral losses to various dissolved pools
    !----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "phi_ldon_vir",  cobalt%lysis_phi_ldon,  "phi_ldon_vir",  units="unitless", default=0.625)    ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_srdon_vir", cobalt%lysis_phi_srdon, "phi_srdon_vir", units="unitless", default=0.075)  ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_sldon_vir", cobalt%lysis_phi_sldon, "phi_sldon_vir", units="unitless", default=0.3)  ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_ldop_vir",  cobalt%lysis_phi_ldop,  "phi_ldop_vir",  units="unitless", default=0.575)   ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_srdop_vir", cobalt%lysis_phi_srdop, "phi_srdop_vir", units="unitless", default=0.125) ! dimensionless
    call get_param(param_file, "generic_COBALT", "phi_sldop_vir", cobalt%lysis_phi_sldop, "phi_sldop_vir", units="unitless", default=0.3) ! dimensionless
    !
    !----------------------------------------------------------------------
    ! Parameters for unresolved higher predators
    !----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "imax_hp",     cobalt%imax_hp,     "imax_hp",      units="day-1", &
                   default= 0.09, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "ki_hp",       cobalt%ki_hp,       "ki_hp",        units="mol N kg-1", default=1.25e-6)           ! mol N kg-1
    call get_param(param_file, "generic_COBALT", "coef_hp",     cobalt%coef_hp,     "coef_hp",      units="unitless", default=2.0)            ! dimensionless
    call get_param(param_file, "generic_COBALT", "ktemp_hp",    cobalt%ktemp_hp,    "ktemp_hp",     units="C-1", default=0.063)         ! C-1
    call get_param(param_file, "generic_COBALT", "nswitch_hp",  cobalt%nswitch_hp,  "nswitch_hp",   units="unitless", default=2.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "mswitch_hp",  cobalt%mswitch_hp,  "mswitch_hp",   units="unitless", default=2.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_smp",  cobalt%hp_ipa_smp,  "hp_ipa_smp",   units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_mdp",  cobalt%hp_ipa_mdp,  "hp_ipa_mdp",   units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_lgp",  cobalt%hp_ipa_lgp,  "hp_ipa_lgp",   units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_diaz", cobalt%hp_ipa_diaz, "hp_ipa_diaz",  units="unitless", default=0.0)        ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_smz",  cobalt%hp_ipa_smz,  "hp_ipa_smz",   units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_mdz",  cobalt%hp_ipa_mdz,  "hp_ipa_mdz",   units="unitless", default=1.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_lgz",  cobalt%hp_ipa_lgz,  "hp_ipa_lgz",   units="unitless", default=1.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_bact", cobalt%hp_ipa_bact, "hp_ipa_bact",  units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_ipa_det",  cobalt%hp_ipa_det,  "hp_ipa_det",   units="unitless", default=0.0)         ! dimensionless
    call get_param(param_file, "generic_COBALT", "hp_phi_det",  cobalt%hp_phi_det,  "hp_phi_det",   units="unitless", default=0.35)        ! dimensionless
    ! max iron from sediment entered as micromol Fe m-2 day-1, converted to moles Fe m-2 sec-1 for model calculations
    call get_param(param_file, "generic_COBALT", "ffe_sed_max", cobalt%ffe_sed_max, &
                   "maximum iron release from the sediment", units="micromoles Fe m-2 day-1", &
                   default= 170.0, scale = I_sperd*(1/micromol2mol) )
    call get_param(param_file, "generic_COBALT", "ffe_geotherm_ratio", cobalt%ffe_geotherm_ratio, &
                   "iron release per unit of geothermal heat",units="mol Fe m-2 s-1 (W m-2)-1", default= 2.0e-12)
    call get_param(param_file, "generic_COBALT", "jfe_iceberg_ratio",  cobalt%jfe_iceberg_ratio, &
                   "iron release per kg of ice melt", units="mol Fe kg-1 ice melt", default= 1.0e-7)
    call get_param(param_file, "generic_COBALT", "jno3_iceberg_ratio", cobalt%jno3_iceberg_ratio, &
                   "nitrate release per kg of ice melt",units="mol N kg-1 ice melt", default= 2.0e-6)
    call get_param(param_file, "generic_COBALT", "jpo4_iceberg_ratio", cobalt%jpo4_iceberg_ratio, &
                   "phosphate release per kg of ice melt",units="mol P kg-1 ice melt", default= 1.1e-7)
    ! fe_coast is effectively the fraction of the equivalent benthic flux that you would like to release from the
    ! vertical land face.  This can be useful in regions of steep topography where benthic sources are not resolved
    call get_param(param_file, "generic_COBALT", "fe_coast", cobalt%fe_coast, "coastal iron flux parameter", &          
                   units="none", default= 0.0 )

    ! Radiocarbon
    call get_param(param_file, "generic_COBALT", "half_life_14c", cobalt%half_life_14c, "half_life_14c", units="s", default= 5730.0 )                  ! s
    call get_param(param_file, "generic_COBALT", "lambda_14c",    cobalt%lambda_14c,    "lambda_14c",    units="-s", & 
                   default= log(2.0) / (cobalt%half_life_14c), scale = I_spery ) 
    !
    !---------------------------------------------------------------------------
    ! Ballast, remineralization and iron scavenging parameters (Section 4 of code)
    !---------------------------------------------------------------------------
    !
    ! Parameters controlling the production of calcium carbonate and aragonite detritus are tuned to match global flux
    ! estimates.  Values are entered as moles of calcium carbonate detritus created per mole of organic C detritus
    ! produced within plankton foodweb pathways associated with shell forming organisms.  Values are then converted to
    ! per mole of N detritus.  Note that the rates are scaled by the saturation state (see Section 4 of the code).
    call get_param(param_file, "generic_COBALT", "ca_2_n_arag", cobalt%ca_2_n_arag, &
                   "ratio of aragonite detritus prod. to org. C detritus prod. by aragonite shell formers", &
                   units="mol cadet_arag mol org. C-1", default= 0.055, scale = c2n)
    call get_param(param_file, "generic_COBALT", "ca_2_n_calc", cobalt%ca_2_n_calc, &
                   "ratio of calcite detritus prod. to org. C detritus prod. by calcite shell formers", &
                   units="mol cadet_calc mol org. C-1",default= 0.050, scale = c2n)
    call get_param(param_file, "generic_COBALT", "caco3_sat_max", cobalt%caco3_sat_max, &
                  "cap for positive scaling of caco3 detritus prod with saturation state", units="none", default= 10.0)

    ! Organic matter remineralization: Oxygen and temperature dependence follows Laufkotter et al. (2017).
    call get_param(param_file, "generic_COBALT", "k_o2", cobalt%k_o2, "O2 half-saturation for remineralization", &
                   units="mol O2 kg-1", default= 8.0e-6)
    call get_param(param_file, "generic_COBALT", "k_no3_denit", cobalt%k_no3_denit, &
           "nitrate half-saturation for denitrification", units="mol NO3 kg-1", default= 1.0e-6)
    call get_param(param_file, "generic_COBALT", "o2_min", cobalt%o2_min, "Minimum O2 for aerobic remineralization", &
                   units="mol O2 kg-1", default= 0.8e-6)
    call get_param(param_file, "generic_COBALT", "kappa_remin", cobalt%kappa_remin, &
                   "Temperature dependence of remineralization", units="deg C-1", default=0.063)
    call get_param(param_file, "generic_COBALT", "remin_ramp_scale", cobalt%remin_ramp_scale, &
                   "depth scale from the surface over which remineralization ramps up", units="m", default= 50.0)
    ! gamma_ndet is set to produce a Martin-curve like remineralization length scale at temperatures ~10 deg. C
    call get_param(param_file, "generic_COBALT", "gamma_ndet", cobalt%gamma_ndet, &
                   "Remineralization rate for unprotected organic matter", units="s-1", default=cobalt%wsink/350.0)

    ! mineral ballasting after Klaas and Archer (2002) and Dunne et al. (2007) (see p. 3) 
    ! conversion is 0.070 g C (g Ca)-1 to moles N (mole Ca)-1; Similar conversions below, but lith remains per gram
    call get_param(param_file, "generic_COBALT", "rpcaco3", cobalt%rpcaco3, "Organic matter protection from CaCO3", &
                   units="mol N mol Ca-1", default= 0.070/12.0*16.0/106.0*100.0)
    call get_param(param_file, "generic_COBALT", "rplith", cobalt%rplith, &
                   "Organic matter protection from lithogenic minerals",units="mol N g lith-1", &
                   default= 0.065/12.0*16.0/106.0)
    call get_param(param_file, "generic_COBALT", "rpsio2", cobalt%rpsio2, "Organic matter protection from silica", &
                   units="mol N mol Si-1", default= 0.026/12.0*16.0/106.0*60.0)
    ! From TOPAZ: Calibrated to Data from Betzer et al. (1984) yielding ~50% attenuation between 900-2200m 
    call get_param(param_file, "generic_COBALT", "gamma_cadet_arag", cobalt%gamma_cadet_arag, &
                   "Dissolution rate for aragonite detritus at 0 saturation", units="s-1", default=cobalt%wsink/760.0)
    ! From TOPAZ: Calibrated to get an approximate 67% transfer efficiency between 1000-3800 at Ocean Station P with
    ! (1-omega_cadet) scaling.
    call get_param(param_file, "generic_COBALT", "gamma_cadet_calc", cobalt%gamma_cadet_calc, &
                   "Dissolution rate for calcite detritus at 0 saturation", units="s-1", default=cobalt%wsink/1343.0)
    ! Uses temperature-dependence of decomposing diatoms following Kamatani (1982)
    call get_param(param_file, "generic_COBALT", "kappa_sidet", cobalt%kappa_sidet, &
                   "Temperature dependence of silica dissolution", units="deg C -1", default= 0.063)
    ! large remineralization length-scale at cold temperatures, but approaches Gnanadesikan et al. (1999) values of
    ! ~2000m at warm temperatures.  This was partially tuned to match observed silica profiles.
    call get_param(param_file, "generic_COBALT", "gamma_sidet", cobalt%gamma_sidet, &
                   "Dissolution rate of silica detritus at 0 deg. C", units="s-1", default= cobalt%wsink/1.0e4)

    ! phi_lith and k_lith parameters largely tuned to maintain hot-spots of lithogenic detritus near source regions
    ! where this was needed to explain export patterns
    call get_param(param_file, "generic_COBALT", "phi_lith" , cobalt%phi_lith, &
                   "scaling factor for creating lithogenic detritus via filter feeding" , units="dimensionless", &
                   default= 0.002)
    call get_param(param_file, "generic_COBALT", "k_lith", cobalt%k_lith, &
                   "background formation rate of lithogenic detritus", units="year-1", default= 0.5, scale = I_spery)

    ! Iron scavenging parameters
    ! Background and don-proportional ligand concentrations were coarsely calibrated to Volker and Tagliabue (2015)
    call get_param(param_file, "generic_COBALT", "felig_bkg", cobalt%felig_bkg, &
                   "background organic ligand concentration", units="mol Fe kg-1", default= 0.5e-9)
    call get_param(param_file, "generic_COBALT", "felig_2_don", cobalt%felig_2_don, &
                   "proportionality for enhancing ligands with non-refractory don", units="mol lig (mol N)-1", &
                   default= 0.5e-3)
    ! Scavenging coefficients -  alpha_fescav allows for a linear scavenging rate for free iron (feprime) used in
    ! COBALTv1, beta_fescav scales the reaction between feprime and ndet and is the default for COBALTv2 and beyond
    ! In either case, values are calibrated to create reasonable levels of iron, iron-limitation and HNLC regions
    ! (e.g., see Tagliabue et al., 2016). 
    call get_param(param_file, "generic_COBALT", "alpha_fescav", cobalt%alpha_fescav, & 
                   "linear iron scavenging rate constant", units="year-1", default= 0.0, scale = I_spery)
    call get_param(param_file, "generic_COBALT", "beta_fescav", cobalt%beta_fescav, &
                   "iron scavenging rate constant for free iron - detritus interaction", &
                   units="year-1 (mol ndet kg-1)-1", default= 2.5e9, scale = I_spery)
    ! Value coarsely tuned to ferrocline
    call get_param(param_file, "generic_COBALT", "remin_eff_fedet", cobalt%remin_eff_fedet, &
                   "iron remineralization efficiency relative to organic matter", units="unitless", default= 0.25)
    ! mimics the impact of oxygen free radicals on iron binding; tuned to observed hydrogen peroxide profiles
    ! (Yuan and Shiller, 2001).
    call get_param(param_file, "generic_COBALT", "io_fescav", cobalt%io_fescav, &
                   "scaling for weakened iron-ligand binding in high light", units="W m-2", default= 10.0)
    ! Multiplies beta_fescav when feprime is above solubility limit defined by Liu and Millero (2002).  Tuned to
    ! control the offshore propagation of coastal iron signals.
    call get_param(param_file, "generic_COBALT", "fast_fescav_fac", cobalt%fast_fescav_fac, &
                   "multiplier for enhanced scavenging when free iron is above saturation",units="none",default= 2.0)
    ! Ligand binding strength set to produce strong binding and reasonable subsurface iron at low light, and allow most
    ! of the iron to be free at high light (following Fan and Dunne).
    ! kfe_eq_lig = [FeL]/([L]*[Fe']), so large values imply that most is [FeL]
    call get_param(param_file, "generic_COBALT", "kfe_eq_lig_ll", cobalt%kfe_eq_lig_ll, &
                   "low light ligand binding strength", units="(mol lig-1 kg)-1", default= 1.0e12)
    call get_param(param_file, "generic_COBALT", "kfe_eq_lig_hl", cobalt%kfe_eq_lig_hl, & 
                   "high light ligand binding strength", units="(mol lig-1 kg)-1", default= 1.0e9)
    !
    !-----------------------------------------------------------------------
    ! Sediments
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "bottom_thickness", cobalt%bottom_thickness, &
           "effective bottom layer thickness for calculating rates of benthic processes", units="m", default= 1.0)
    call get_param(param_file, "generic_COBALT", "z_sed", cobalt%z_sed, "effective sediment layer thickness", &
           units="m", default= 0.1)
    call get_param(param_file, "generic_COBALT", "z_burial", cobalt%z_burial, &
           "depth scale for ramping up particulate organic burial", units="m", default= 10.0)
    call get_param(param_file, "generic_COBALT", "z_denit", cobalt%z_denit, &
           "depth scale for ramping up benthic denitrification", units="m", default= 10.0)
    call get_param(param_file, "generic_COBALT", "scale_burial", cobalt%scale_burial, &
           "scaling factor for particulate organic burial", units="none", default= 0.0)
    call get_param(param_file, "generic_COBALT", "cased_steady",       cobalt%cased_steady,       "cased_steady",       default=.false. )
    call get_param(param_file, "generic_COBALT", "phi_surfresp_cased", cobalt%phi_surfresp_cased, "phi_surfresp_cased", units="unitless", default=0.14307)
    call get_param(param_file, "generic_COBALT", "phi_deepresp_cased", cobalt%phi_deepresp_cased, "phi_deepresp_cased", units="unitless", default=4.1228)
    call get_param(param_file, "generic_COBALT", "alpha_cased",        cobalt%alpha_cased,        "alpha_cased",        units="unitless", default=2.7488)
    call get_param(param_file, "generic_COBALT", "beta_cased",         cobalt%beta_cased,         "beta_cased",         units="unitless", default=-2.2185)
    call get_param(param_file, "generic_COBALT", "gamma_cased",        cobalt%gamma_cased,        "gamma_cased", units="year-1", &
                   default=0.03607, scale = I_spery ) 
    call get_param(param_file, "generic_COBALT", "Co_cased",           cobalt%Co_cased,           "Co_cased",           units="mol m-3", default=8.1e3) ! moles m-3
    !
    !-----------------------------------------------------------------------
    ! Dissolved Organic Material
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "gamma_srdon",  cobalt%gamma_srdon, "gamma_srdon", units="1/(default*spery)", default= 10.0)
      cobalt%gamma_srdon = 1.0 / (cobalt%gamma_srdon  * spery)          ! s-1
    call get_param(param_file, "generic_COBALT", "gamma_srdop",  cobalt%gamma_srdop, "gamma_srdop", units="1/(default*spery)", default= 4.0 )
      cobalt%gamma_srdop = 1.0 / (cobalt%gamma_srdop  * spery)           ! s-1
    call get_param(param_file, "generic_COBALT", "gamma_sldon",  cobalt%gamma_sldon, "gamma_sldon", units="1/(default*sperd)", default= 90.0)
      cobalt%gamma_sldon = 1.0 /(cobalt%gamma_sldon  * sperd)           ! s-1
    call get_param(param_file, "generic_COBALT", "gamma_sldop",  cobalt%gamma_sldop, "gamma_sldop", units="1/(default*sperd)", default= 90.0)
      cobalt%gamma_sldop = 1.0/ (cobalt%gamma_sldop * sperd)           ! s-1
    ! 2016/08/24 jgj add parameter for background dissolved organic material
    ! For the oceanic carbon budget, a constant 42 uM of dissolved organic
    ! carbon is added to represent the refractory component.
    ! For the oceanic nitrogen budget, a constant 2 uM of dissolved organic
    ! nitrogen is added to represent the refractory component.
    ! 2016/09/22 jgj changed background DOC to 4.0e-5 per agreement with CAS, JPD
    !
    call get_param(param_file, "generic_COBALT", "doc_background",  cobalt%doc_background, "doc_background", units="uM", default=4.0e-5)    ! uM

    !---------------------------------------------------------------------
    !
    !-----------------------------------------------------------------------
    ! Nitrification / Anammox
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "gamma_nitrif",  cobalt%gamma_nitrif, "gamma_nitrif", units="gamma_nitrif /(default*sperd)", default= 30.0)     ! s-1
      cobalt%gamma_nitrif = gamma_nitrif / (cobalt%gamma_nitrif * sperd)           ! s-1
    call get_param(param_file, "generic_COBALT", "knh4_nitrif",   cobalt%k_nh3_nitrif, "knh4_nitrif",  units="mol kg-1", default=k_nh3_nitrif )                     ! moles kg-1
    call get_param(param_file, "generic_COBALT", "irr_inhibit",   cobalt%irr_inhibit,  "irr_inhibit",  units="W m-2", default=irr_inhibit)                         ! W m-2
    !call get_param(param_file, "generic_COBALT", "gamma_nh4amx",  cobalt%gamma_nh4amx, "gamma_nh4amx", units="s-1", default=0.07 / sperd)                      ! s-1
    call get_param(param_file, "generic_COBALT", "gamma_nh4amx",  cobalt%gamma_nh4amx, "gamma_nh4amx", units="day-1", &
                   default= 0.0, scale = I_sperd ) ! s-1
    call get_param(param_file, "generic_COBALT", "o2_min_amx",    cobalt%o2_min_amx,   "o2_min_amx",   units="mol O2 kg-1", default=4.0e-6 )                                ! mol O2 kg-1
    call get_param(param_file, "generic_COBALT", "k_no3_amx", cobalt%k_no3_amx, &
           "nitrate half-saturation for anammox", units="mol NO3 kg-1", default= 1.0e-6)
    call get_param(param_file, "generic_COBALT", "k_o2_nit",         cobalt%k_o2_nit,          "k_o2_nit",         units="mol O2 kg-1   ", default= k_o2_nit)                    ! mol O2 kg-1
    call get_param(param_file, "generic_COBALT", "o2_min_nit",       cobalt%o2_min_nit,        "o2_min_nit",       units="mol O2 kg-1   ", default= o2_min_nit )                 ! mol O2 kg-1
    !
    !-----------------------------------------------------------------------
    ! Miscellaneous
    !-----------------------------------------------------------------------
    !
    call get_param(param_file, "generic_COBALT", "tracer_debug",  cobalt%tracer_debug, "tracer_debug", default=.false.)

    call g_tracer_end_param_list(package_name)
    !===========
    !Block Ends: g_tracer_add_param
    !===========
    if (is_root_pe()) write(stdoutunit,*) '!------------------------END--------------------------------------------'
    if (is_root_pe()) write(stdoutunit,*) '! ', trim(package_name), ' parameter check'
    if (is_root_pe()) write(stdoutunit,*) '!------------------------END--------------------------------------------'
  end subroutine user_add_params

  subroutine user_add_tracers(tracer_list)
    type(g_tracer_type), pointer :: tracer_list
    character(len=fm_string_len), parameter :: sub_name = 'user_add_tracers'
    real :: as_coeff_cobalt
    type(param_file_type) :: param_file  !< structure indicating parameter file to parse
    !
    ! This include declares and sets the variable "version". (use of include statement copied from MOM6)
# include "version_variable.h"

    if ((trim(as_param_cobalt) == 'W92') .or. (trim(as_param_cobalt) == 'gfdl_cmip6')) then
      ! Air-sea gas exchange coefficient presented in OCMIP2 protocol.
      ! Value is 0.337 cm/hr in units of m/s.
      as_coeff_cobalt=9.36e-7
    else
      ! Value is 0.251 cm/hr in units of m/s
      as_coeff_cobalt=6.972e-7
    endif

    !
    !Add here only the parameters that are required at the time of registeration
    !(to make flux exchanging Ocean tracers known for all PE's)
    !
    call g_tracer_start_param_list(package_name)
    
    ! add MOM6-style interfaces for a parameter file
    call get_COBALT_param_file(param_file)
    call log_version(param_file, "COBALT", version, "", log_to_all=.true., debugging=.true.)
    !Specify and initialize all parameters used by this package
    call user_add_params(param_file)
    
    call get_param(param_file, "generic_COBALT", "htotal_in", cobalt%htotal_in, "htotal_in", units="", default=1.0e-08)
    !
    ! Sinking velocity of detritus: a value of 20 m d-1 is consistent with a characteristic sinking
    ! velocity of 100 m d-1 of marine aggregates and a disaggregation rate constant
    ! of 5 d-1 in the surface ocean (Clegg and Whitfield, 1992; Dunne, 1999).  Alternatively, 100 m d-1
    ! is more in line with the deep water synthesis of Berelson (2002; Particel settling rates increase
    ! with depth in the ocean, DSR-II, 49, 237-252).
    !
    call get_param(param_file, "generic_COBALT", "wsink",  cobalt%wsink, "wsink", units="m day-1", &
                   default= 100.0, scale = I_sperd ) ! s-1

    call get_param(param_file, "generic_COBALT", "ice_restart_file"   , cobalt%ice_restart_file   ,  "ice_restart_file", default="ice_cobalt.res.nc")
    call get_param(param_file, "generic_COBALT", "ocean_restart_file" , cobalt%ocean_restart_file ,  "ocean_restart_file", default="ocean_cobalt.res.nc")
    call get_param(param_file, "generic_COBALT", "IC_file"            , cobalt%IC_file            ,  "IC_file"           , default="")
    !
    call g_tracer_end_param_list(package_name)

    ! Set Restart files
    call g_tracer_set_files(ice_restart_file    = cobalt%ice_restart_file,&
         ocean_restart_file  = cobalt%ocean_restart_file )

    !All tracer fields shall be registered for diag output.

    !=====================================================
    !Specify all prognostic tracers of this modules.
    !=====================================================
    !User adds one call for each prognostic tracer below!
    !User should specify if fluxes must be extracted from boundary
    !by passing one or more of the following methods as .true.
    !and provide the corresponding parameters array
    !methods: flux_gas,flux_runoff,flux_wetdep,flux_drydep
    !
    !Pass an init_value arg if the tracers should be initialized to a nonzero value everywhere
    !otherwise they will be initialized to zero.
    !
    !===========================================================
    !Prognostic Tracers
    !===========================================================
    !
    !
    !       ALK (Total carbonate alkalinity)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'alk',         &
         longname   = 'Alkalinity',  &
         units      = 'mol/kg',      &
         prog       = .true.,        &
         flux_runoff= .true.,        &
         flux_param = (/ 1.0e-03 /), &
         flux_bottom= .true.         )
    !
    !       Aragonite (Sinking detrital/particulate CaCO3)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'cadet_arag',     &
         longname   = 'Detrital CaCO3', &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         sink_rate  = cobalt%wsink,     &
         btm_reservoir = .true.         )
    !
    !       Calcite
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'cadet_calc',     &
         longname   = 'Detrital CaCO3', &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         sink_rate  = cobalt%wsink,     &
         btm_reservoir = .true.         )
    !
    !       DIC (Dissolved inorganic carbon)
    !
    call g_tracer_add(tracer_list,package_name,                       &
         name       = 'dic',                                           &
         longname   = 'Dissolved Inorganic Carbon',                    &
         units      = 'mol/kg',                                        &
         prog       = .true.,                                          &
         flux_gas   = .true.,                                          &
         flux_gas_name  = 'co2_flux',                                  &
         flux_gas_type  = 'air_sea_gas_flux_generic',                  &
         flux_gas_molwt = WTMCO2,                                      &
         flux_gas_param = (/ as_coeff_cobalt, 9.7561e-06 /),           &
         ! Uncomment for "no mass change" check
         ! flux_gas_param = (/ 0.0, 0.0 /),                            &
         flux_gas_restart_file  = 'ocean_cobalt_airsea_flux.res.nc',   &
         flux_runoff= .true.,                                          &
         flux_param = (/12.011e-03  /),                                &
         flux_bottom= .true.,                                          &
         init_value = 0.001                                            )
    !
    !       Dissolved Fe (assumed to be all available to phytoplankton)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'fed',            &
         longname   = 'Dissolved Iron', &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         flux_runoff= .true.,           &
         flux_wetdep= .true.,           &
         flux_drydep= .true.,           &
         flux_param = (/ 55.847e-03 /), &
         flux_bottom= .true.            )
    !
    !    Fedet (Sinking detrital/particulate iron)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'fedet',         &
         longname   = 'Detrital Iron', &
         units      = 'mol/kg',        &
         prog       = .true.,          &
         sink_rate  = cobalt%wsink,     &
         btm_reservoir = .true.        )
    !
    !       Diazotroph Fe (Iron in N2-fixing phytoplankton for variable Fe:N ratios)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'fedi',            &
         longname   = 'Diazotroph Iron', &
         units      = 'mol/kg',          &
         prog       = .true.,            &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       Large Fe (Iron in large phytoplankton to allow for variable Fe:N ratios)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'felg',       &
         longname   = 'Large Phytoplankton Iron', &
         units      = 'mol/kg',     &
         prog       = .true.,       &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       Medium Fe (Iron in medium phytoplankton to allow for variable Fe:N ratios)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'femd',       &
         longname   = 'Medium Phytoplankton Iron', &
         units      = 'mol/kg',     &
         prog       = .true.,        &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       Small Fe
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'fesm',       &
         longname   = 'Small Phytoplankton Iron', &
         units      = 'mol/kg',     &
         prog       = .true.,        &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       Diazotroph P (allow for flexible P:N)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'pdi',            &
         longname   = 'Diazotroph Phosphorus', &
         units      = 'mol/kg',          &
         prog       = .true.,            &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       Large Phyto. P. (allow for flexible P:N)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'plg',       &
         longname   = 'Large Phytoplankton Phosphorus', &
         units      = 'mol/kg',     &
         prog       = .true.,       &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       Medium Phyto. P (allow for flexible P:N)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'pmd',       &
         longname   = 'Medium Phytoplankton Phosphorus', &
         units      = 'mol/kg',     &
         prog       = .true.,        &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       Small Phyto. P (allow for flexible P:N)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'psm',       &
         longname   = 'Small Phytoplankton Phosphorus', &
         units      = 'mol/kg',     &
         prog       = .true.,        &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       LDON (Labile dissolved organic nitrogen)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'ldon',           &
         flux_runoff= .true.,         &
         longname   = 'labile DON',     &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         flux_param = (/ 1.0e-3 /)       )
    !
    !       LDOP (Labile dissolved organic phosphorous)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'ldop',           &
         flux_runoff= .true.,         &
         longname   = 'labile DOP',     &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         flux_param = (/ 1.0e-3 /)       )
    !
    !       LITH (Lithogenic aluminosilicate particles)
    !
    call g_tracer_add(tracer_list,package_name,     &
         name       = 'lith',                       &
         longname   = 'Lithogenic Aluminosilicate', &
         units      = 'g/kg',                       &
         prog       = .true.,                       &
!         const_init_value = 0.0 ,                   &
         flux_runoff= .true.,                       &
         flux_wetdep= .true.,                       &
         flux_drydep= .true.,                       &
         flux_param = (/ 1.0e-03 /)                 )
    !
    !     LITHdet (Detrital Lithogenic aluminosilicate particles)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'lithdet',   &
         longname   = 'lithdet',   &
         units      = 'g/kg',      &
         prog       = .true.,      &
         sink_rate  = cobalt%wsink, &
         btm_reservoir = .true.    )
    !
    !       NBact: Bacteria nitrogen
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'nbact',          &
         longname   = 'bacterial',      &
         units      = 'mol/kg',         &
         prog       = .true.            )
    !
    !    Ndet (Sinking detrital/particulate Nitrogen)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'ndet',      &
         longname   = 'ndet',      &
         flux_runoff= .true.,      &
         units      = 'mol/kg',    &
         prog       = .true.,      &
         sink_rate  = cobalt%wsink,&
         btm_reservoir = .true.,   &
         flux_param = (/ 1.0e-3 /) )
    !
    !    NDi (assumed to be facultative N2-fixers, with a variable N:P ratio
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'ndi',                 &
         longname   = 'Diazotroph Nitrogen', &
         units      = 'mol/kg',              &
         prog       = .true., &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !    NLg (assumed to be a dynamic combination of diatoms and other
    !         eukaryotes all effectively greater than 20 um in diameter,
    !         and having a fixed C:N ratio)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'nlg',            &
         longname   = 'Large Phytoplankton Nitrogen', &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !    NMd (assumed to be a dynamic combination of diatoms and other
    !         eukaryotes all effectively greater than 5 um in diameter,
    !         and having a fixed C:N ratio)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'nmd',            &
         longname   = 'Medium Phytoplankton Nitrogen', &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       NSm (Nitrogen in picoplankton and nanoplankton
    !            ~less than 5 um in diameter and having a fixed C:N:P ratio)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'nsm',            &
         longname   = 'Small Phytoplankton Nitrogen', &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         move_vertical = .true.,        &
         btm_reservoir = .true.          )
    !
    !       NH4
    !
    if (do_nh3_atm_ocean_exchange) then
       if (mpp_root_pe().eq.mpp_pe()) then
          write(*,*) 'setting up nh3 tracer (ocean)'
       end if
       call g_tracer_add(tracer_list,package_name,&
            name       = 'nh4',             &
            longname   = 'Ammonia',         &
            units      = 'mol/kg',          &
            prog       = .true.,            &
            flux_runoff= .true.,            &
            flux_wetdep= .true.,            &
            flux_drydep= .true.,            &
            flux_gas   = .true.,            &
!            implementation='duce',   &
            implementation='johnson',   &
            flux_gas_name  = 'nh3_flux',    &
            flux_gas_type  = 'air_sea_gas_flux_generic', &
            flux_gas_molwt = WTMN,                       &
            flux_gas_param = (/ 17.,vb_nh3 /), &
            flux_gas_restart_file  = 'ocean_cobalt_airsea_flux.res.nc',    &
            flux_param = (/ WTMN*1.e-3/),    &
            flux_bottom= .true.,            &
            init_value = 0.001              )
    else
       if (mpp_root_pe().eq.mpp_pe()) then
          write(*,*) 'setting up nh3 tracer (ocean - no exchange)'
       end if

       call g_tracer_add(tracer_list,package_name,&
         name       = 'nh4',             &
         longname   = 'Ammonia',         &
         units      = 'mol/kg',          &
         prog       = .true.,            &
         flux_runoff= .true.,            &
         flux_wetdep= .true.,            &
         flux_drydep= .true.,            &
         flux_param = (/ 14.0067e-03 /), &
         flux_bottom= .true.             )
    end if
    !
    !       NO3
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'no3',             &
         longname   = 'Nitrate',         &
         units      = 'mol/kg',          &
         prog       = .true.,            &
         flux_runoff= .true.,            &
         flux_wetdep= .true.,            &
         flux_drydep= .true.,            &
         flux_param = (/ 14.0067e-03 /), &
         flux_bottom= .true.             )
    !
    !       O2
    !
    call g_tracer_add(tracer_list,package_name,                        &
         name       = 'o2',                                            &
         longname   = 'Oxygen',                                        &
         units      = 'mol/kg',                                        &
         prog       = .true.,                                          &
         flux_runoff= .true.,                                          &
         flux_param = (/ 1.0e-3 /),                                     &
         flux_gas   = .true.,                                          &
         flux_gas_name  = 'o2_flux',                                   &
         flux_gas_type  = 'air_sea_gas_flux_generic',                  &
         flux_gas_molwt = WTMO2,                                       &
         flux_gas_param = (/ as_coeff_cobalt, 9.7561e-06 /),           &
         flux_gas_restart_file  = 'ocean_cobalt_airsea_flux.res.nc',   &
         flux_bottom= .true.             )
    !
    !    Pdet (Sinking detrital/particulate Phosphorus)
    !
    call g_tracer_add(tracer_list,package_name,         &
         name       = 'pdet',                           &
         longname   = 'Detrital Phosphorus',            &
         flux_runoff= .true.,                           &
         units      = 'mol/kg',                         &
         prog       = .true.,                           &
         sink_rate  = cobalt%wsink,                     &
         btm_reservoir = .true.,                        &
         flux_param = (/ 1.0e-3 /) )
    !
    !       PO4
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'po4',       &
         longname   = 'Phosphate', &
         flux_runoff= .true.,      &
         units      = 'mol/kg',    &
         prog       = .true.,      &
         flux_wetdep= .true.,      &
         flux_drydep= .true.,      &
         flux_bottom= .true.,      &
         flux_param = (/ 1.0e-3 /) )
    !
    !       SRDON (Semi-Refractory dissolved organic nitrogen)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'srdon',           &
         longname   = 'Semi-Refractory DON', &
         flux_runoff= .true.,           &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         flux_param = (/ 1.0e-3 /)  )
    !
    !       SRDOP (Semi-Refractory dissolved organic phosphorus)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'srdop',           &
         longname   = 'Semi-Refractory DOP', &
         flux_runoff= .true.,           &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         flux_param = (/ 1.0e-3 /)      )
    !
    !       SLDON (Semilabile dissolved organic nitrogen)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'sldon',           &
         longname   = 'Semilabile DON', &
         flux_runoff= .true.,           &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         flux_param = (/ 1.0e-3 /)      )
    !
    !       SLDOP (Semilabile dissolved organic phosphorus)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'sldop',           &
         longname   = 'Semilabile DOP', &
         flux_runoff= .true.,           &
         units      = 'mol/kg',         &
         prog       = .true.,           &
         flux_param = (/ 1.0e-3 /)      )
    !
    !       Sidet (Sinking detrital/particulate Silicon)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'sidet',            &
         longname   = 'Detrital Silicon', &
         units      = 'mol/kg',           &
         prog       = .true.,             &
         sink_rate  = cobalt%wsink,        &
         btm_reservoir = .true.    )
    !
    !    SiLg (Silicon in large phytoplankton for variable Si:N ratios
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'silg',          &
         longname   = 'Large Phytoplankton Silicon', &
         units      = 'mol/kg',        &
         prog       = .true.,          &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !    SiMd (Silicon in medium phytoplankton for variable Si:N ratios
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'simd',          &
         longname   = 'Medium Phytoplankton Silicon', &
         units      = 'mol/kg',        &
         prog       = .true.,          &
         move_vertical = .true.,         &
         btm_reservoir = .true.          )
    !
    !       SiO4
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'sio4',       &
         longname   = 'Silicate',   &
         units      = 'mol/kg',     &
         prog       = .true.,       &
         flux_runoff= .true.,       &
         flux_param = (/ 1.0e-3 /), &
         flux_bottom= .true.        )

    !
    !     Small zooplankton N
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'nsmz',     &
         longname   = 'Small Zooplankton Nitrogen', &
         units      = 'mol/kg',   &
         prog       = .true.     )

    !
    !     Medium-sized zooplankton N
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'nmdz',     &
         longname   = 'Medium-sized zooplankton Nitrogen', &
         units      = 'mol/kg',   &
         prog       = .true.      )

    !
    !     Large zooplankton N (Pred zoo + krill)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'nlgz',     &
         longname   = 'large Zooplankton Nitrogen', &
         units      = 'mol/kg',   &
         prog       = .true.      )

      if (do_14c) then                                        !<<RADIOCARBON
      !       D14IC (Dissolved inorganic radiocarbon)
      !
      call g_tracer_add(tracer_list,package_name,       &
         name       = 'di14c',                          &
         longname   = 'Dissolved Inorganic Radiocarbon',&
         units      = 'mol/kg',                         &
         prog       = .true.,                           &
         flux_gas       = .true.,                       &
         flux_gas_name  = 'c14o2_flux',                 &
         flux_gas_type  = 'air_sea_gas_flux',           &
         flux_gas_molwt = WTMCO2,                       &
         flux_gas_param = (/ as_coeff_cobalt, 9.7561e-06 /),   &
         flux_gas_restart_file  = 'ocean_cobalt_airsea_flux.res.nc', &
         flux_runoff= .true.,                           &
         flux_param     = (/14.e-03  /),                &
         flux_bottom    = .true.,                       &
         init_value     = 0.001)
      !
      !       DO14C (Dissolved organic radiocarbon)
      !
      call g_tracer_add(tracer_list,package_name, &
         name       = 'do14c',                    &
         longname   = 'DO14C',                    &
         units      = 'mol/kg',                   &
         prog       = .true.)
      endif                                                   !RADIOCARBON>>

    !===========================================================
    !Diagnostic Tracers
    !===========================================================
    !
    !    Cased (CaCO3 in sediments)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'cased',          &
         longname   = 'Sediment CaCO3', &
         units      = 'mol m-3',       &
         prog       = .false.           )
    !
    !       Chl (Chlorophyll)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'chl',         &
         longname   = 'Chlorophyll', &
         units      = 'ug kg-1',     &
         prog       = .false.,       &
         init_value = 0.08           )
    !
    !       CO3_ion (Carbonate ion)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'co3_ion',       &
         longname   = 'Carbonate ion', &
         units      = 'mol/kg',        &
         prog       = .false.          )
    !
    !      cadet_arag_btf (Aragonite flux to sediments)
    !
    call g_tracer_add(tracer_list,package_name,  &
         name       = 'cadet_arag_btf',          &
         longname   = 'aragonite flux to Sediments', &
         units      = 'mol m-2 s-1',             &
         prog       = .false.                    )
    !
    !      cadet_calc_btf (Calcite flux to sediments)
    !
    call g_tracer_add(tracer_list,package_name,  &
         name       = 'cadet_calc_btf',          &
         longname   = 'calcite flux to Sediments', &
         units      = 'mol m-2 s-1',             &
         prog       = .false.                    )
    !
    !      lithdet_btf (Lithogenic flux to sediments)
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'lithdet_btf',            &
         longname   = 'Lith flux to Sediments', &
         units      = 'g m-2 s-1',              &
         prog       = .false.                   )
    !
    !      ndet_btf (N flux to sediments)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'ndet_btf',            &
         longname   = 'N flux to Sediments', &
         units      = 'mol m-2 s-1',         &
         prog       = .false.                )
    !
    !      pdet_btf (P flux to sediments)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'pdet_btf',            &
         longname   = 'P flux to Sediments', &
         units      = 'mol m-2 s-1',         &
         prog       = .false.                )
    !
    !      sidet_btf (SiO2 flux to sediments)
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'sidet_btf',              &
         longname   = 'SiO2 flux to Sediments', &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !      fedet_btf (Fe flux to sediments)
    !      (only used in "no mass change" check)
    call g_tracer_add(tracer_list,package_name, &
         name       = 'fedet_btf',              &
         longname   = 'Fe flux to Sediments',   &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for nsm_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'nsm_btf',                &
         longname   = 'nsm flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for nmd_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'nmd_btf',                &
         longname   = 'nmd flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for nlg_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'nlg_btf',                &
         longname   = 'nlg flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for ndi_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'ndi_btf',                &
         longname   = 'ndi flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
     !
    !  add bottom flux for fesm_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'fesm_btf',                &
         longname   = 'fesm flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for femd_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'femd_btf',                &
         longname   = 'femd flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for felg_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'felg_btf',                &
         longname   = 'felg flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for fedi_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'fedi_btf',                &
         longname   = 'fedi flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for psm_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'psm_btf',                &
         longname   = 'psm flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for pmd_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'pmd_btf',                &
         longname   = 'pmd flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for plg_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'plg_btf',                &
         longname   = 'plg flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for pdi_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'pdi_btf',                &
         longname   = 'pdi flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for simd_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'simd_btf',                &
         longname   = 'simd flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !  add bottom flux for silg_btf
    !
    call g_tracer_add(tracer_list,package_name, &
         name       = 'silg_btf',                &
         longname   = 'silg flux to Sediments',  &
         units      = 'mol m-2 s-1',            &
         prog       = .false.                   )
    !
    !       htotal (H+ ion concentration)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'htotal',               &
         longname   = 'H+ ion concentration', &
         units      = 'mol/kg',               &
         prog       = .false.,                &
         init_value = cobalt%htotal_in         )
    !
    !       Irr_aclm (photoacclimation irradiance)
    !
    call g_tracer_add(tracer_list,package_name,&
         name       = 'irr_aclm',           &
         longname   = 'photoacclimation irradiance', &
         units      = 'Watts/m^2',         &
         prog       = .false.              )

     call g_tracer_add(tracer_list,package_name,&
         name       = 'irr_aclm_sfc',       &
         longname   = 'Surface photoacclimation irradiance', &
         units      = 'Watts/m^2',         &
         prog       = .false.              )

      call g_tracer_add(tracer_list,package_name,&
         name       = 'irr_aclm_z',       &
         longname   = 'depth-resolved photoacclim irrad', &
         units      = 'Watts/m^2',         &
         prog       = .false.              )

    call g_tracer_add(tracer_list,package_name,&
         name       = 'mu_mem_ndi',           &
         longname   = 'Growth memory', &
         units      = 'sec-1',         &
         prog       = .false.              )

    call g_tracer_add(tracer_list,package_name,&
         name       = 'mu_mem_nlg',           &
         longname   = 'Growth memory', &
         units      = 'sec-1',         &
         prog       = .false.              )

    call g_tracer_add(tracer_list,package_name,&
         name       = 'mu_mem_nmd',           &
         longname   = 'Growth memory', &
         units      = 'sec-1',         &
         prog       = .false.              )

    call g_tracer_add(tracer_list,package_name,&
         name       = 'mu_mem_nsm',           &
         longname   = 'Growth memory', &
         units      = 'sec-1',         &
         prog       = .false.              )

    if (do_nh3_atm_ocean_exchange .or. scheme_nitrif.eq.2 .or. scheme_nitrif.eq.3) then
       call g_tracer_add(tracer_list,package_name,&
            name       = 'nh3',         &
            longname   = 'NH3', &
            units      = 'mol/kg',     &
            prog       = .false.,       &
            init_value = 1.e-10           )
    end if

  end subroutine user_add_tracers


  ! <SUBROUTINE NAME="generic_COBALT_update_from_coupler">
  !  <OVERVIEW>
  !   Modify the values obtained from the coupler if necessary.
  !  </OVERVIEW>
  !  <DESCRIPTION>
  !   Some tracer fields need to be modified after values are obtained from the coupler.
  !   This subroutine is the place for specific tracer manipulations.
  !  </DESCRIPTION>
  !  <TEMPLATE>
  !   call generic_COBALT_update_from_coupler(tracer_list)
  !  </TEMPLATE>
  !  <IN NAME="tracer_list" TYPE="type(g_tracer_type), pointer">
  !   Pointer to the head of generic tracer list.
  !  </IN>
  ! </SUBROUTINE>
  subroutine generic_COBALT_update_from_coupler(tracer_list)
    type(g_tracer_type), pointer :: tracer_list

    character(len=fm_string_len), parameter :: sub_name = 'generic_COBALT_update_from_copler'

    real, dimension(:,:)  ,pointer    :: stf_alk,dry_no3,wet_no3

    !
    ! NO3 has deposition, river flux, and negative deposition contribution to alkalinity
    !
    call g_tracer_get_pointer(tracer_list,'no3','drydep',dry_no3)
    call g_tracer_get_pointer(tracer_list,'no3','wetdep',wet_no3)

    call g_tracer_get_pointer(tracer_list,'alk','stf',stf_alk)

    stf_alk = stf_alk - dry_no3 - wet_no3 ! update 'tracer%stf' thru pointer

    return
  end subroutine generic_COBALT_update_from_coupler

  ! <SUBROUTINE NAME="generic_COBALT_update_from_bottom">
  !
  !  <OVERVIEW>
  !   Set values of bottom fluxes and reservoirs
  !  </OVERVIEW>
  !
  !  <DESCRIPTION>
  !
  !   This routine calculates bottom fluxes for tracers with bottom reservoirs.
  !   It is called near the end of the time step, meaning that the fluxes
  !   calculated pertain to the next time step.
  !
  !  </DESCRIPTION>
  !
  !  <TEMPLATE>
  !   call generic_COBALT_update_from_bottom(tracer_list,dt, tau, model_time)
  !  </TEMPLATE>
  !
  !  <IN NAME="tracer_list" TYPE="type(g_tracer_type), pointer">
  !  </IN>
  !  <IN NAME="dt" TYPE="real">
  !   Time step increment
  !  </IN>
  !  <IN NAME="tau" TYPE="integer">
  !   Time step index to be used for %field
  !  </IN>
  !
  ! </SUBROUTINE>
  subroutine generic_COBALT_update_from_bottom(tracer_list, dt, tau, model_time)
    type(g_tracer_type), pointer :: tracer_list
    real,               intent(in) :: dt
    integer,            intent(in) :: tau
    type(time_type),    intent(in) :: model_time
    integer :: isc,iec, jsc,jec,isd,ied,jsd,jed,nk,ntau
    logical :: used
    real, dimension(:,:,:),pointer :: grid_tmask
    real, dimension(:,:,:),pointer :: temp_field

    call g_tracer_get_common(isc,iec,jsc,jec,isd,ied,jsd,jed,nk,ntau,grid_tmask=grid_tmask)

    !
    ! The bottom reservoirs of aragonite and calcite are immediately redistributed to the
    ! water column as a bottom flux (btf) where they impact the alkalinity and DIC
    !
    call g_tracer_get_values(tracer_list,'cadet_arag','btm_reservoir',cobalt%fcadet_arag_btm,isd,jsd)
    cobalt%fcadet_arag_btm = cobalt%fcadet_arag_btm/dt
    call g_tracer_get_pointer(tracer_list,'cadet_arag_btf','field',temp_field)
    temp_field(:,:,1) = cobalt%fcadet_arag_btm(:,:)
    call g_tracer_set_values(tracer_list,'cadet_arag','btm_reservoir',0.0)
    used = g_send_data(cobalt%id_fcadet_arag_btm,cobalt%fcadet_arag_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'cadet_calc','btm_reservoir',cobalt%fcadet_calc_btm,isd,jsd)
    cobalt%fcadet_calc_btm = cobalt%fcadet_calc_btm/dt
    call g_tracer_get_pointer(tracer_list,'cadet_calc_btf','field',temp_field)
    temp_field(:,:,1) = cobalt%fcadet_calc_btm(:,:)
    call g_tracer_set_values(tracer_list,'cadet_calc','btm_reservoir',0.0)
    used = g_send_data(cobalt%id_fcadet_calc_btm, cobalt%fcadet_calc_btm, &
    model_time, rmask = grid_tmask(:,:,1), &
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
    !
    ! Iron is buried, but can re-enter the water column in association with
    ! organic matter degradation (see ffe_sed in update_from_source)
    !
    call g_tracer_get_values(tracer_list,'fedet','btm_reservoir',cobalt%ffedet_btm,isd,jsd)
    cobalt%ffedet_btm = cobalt%ffedet_btm/dt
    ! uncomment for "no mass change check"
    !call g_tracer_get_pointer(tracer_list,'fedet_btf','field',temp_field)
    !temp_field(:,:,1) = cobalt%ffedet_btm(:,:)
    call g_tracer_set_values(tracer_list,'fedet','btm_reservoir',0.0)
    used = g_send_data(cobalt%id_ffedet_btm, cobalt%ffedet_btm, &
    model_time, rmask = grid_tmask(:,:,1), &
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
    !
    ! Lithogenic material is buried
    !
    call g_tracer_get_values(tracer_list,'lithdet','btm_reservoir',cobalt%flithdet_btm,isd,jsd)
    cobalt%flithdet_btm = cobalt%flithdet_btm /dt
    call g_tracer_get_pointer(tracer_list,'lithdet_btf','field',temp_field)
    temp_field(:,:,1) = cobalt%flithdet_btm(:,:)
    call g_tracer_set_values(tracer_list,'lithdet','btm_reservoir',0.0)
    used = g_send_data(cobalt%id_flithdet_btm, cobalt%flithdet_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
    !
    ! N, P, and Si detritus that hits the bottom is re-entered as a bottom source of
    ! nh4, po4, and SiO4 respectively
    !
    call g_tracer_get_values(tracer_list,'ndet','btm_reservoir',cobalt%fndet_btm,isd,jsd)
    cobalt%fndet_btm = cobalt%fndet_btm/dt
    call g_tracer_get_pointer(tracer_list,'ndet_btf','field',temp_field)
    temp_field(:,:,1) = cobalt%fndet_btm(:,:)
    call g_tracer_set_values(tracer_list,'ndet','btm_reservoir',0.0)
    used = g_send_data(cobalt%id_fndet_btm,cobalt%fndet_btm,          &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'pdet','btm_reservoir',cobalt%fpdet_btm,isd,jsd)
    cobalt%fpdet_btm = cobalt%fpdet_btm/dt
    call g_tracer_get_pointer(tracer_list,'pdet_btf','field',temp_field)
    temp_field(:,:,1) = cobalt%fpdet_btm(:,:)
    call g_tracer_set_values(tracer_list,'pdet','btm_reservoir',0.0)
    used = g_send_data(cobalt%id_fpdet_btm,cobalt%fpdet_btm,          &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'sidet','btm_reservoir',cobalt%fsidet_btm,isd,jsd)
    cobalt%fsidet_btm = cobalt%fsidet_btm/dt
    call g_tracer_get_pointer(tracer_list,'sidet_btf','field',temp_field)
    temp_field(:,:,1) = cobalt%fsidet_btm(:,:)
    call g_tracer_set_values(tracer_list,'sidet','btm_reservoir',0.0)
    used = g_send_data(cobalt%id_fsidet_btm,    cobalt%fsidet_btm,          &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    !
    ! Handling sinking phytoplankton: Nitrogen, don't need P because n2p is static
    !
    call g_tracer_get_values(tracer_list,'ndi','btm_reservoir',phyto(DIAZO)%fn_btm,isd,jsd)
    phyto(DIAZO)%fn_btm = phyto(DIAZO)%fn_btm/dt
    call g_tracer_get_pointer(tracer_list,'ndi_btf','field',temp_field)
    temp_field(:,:,1) = phyto(DIAZO)%fn_btm(:,:)
    call g_tracer_set_values(tracer_list,'ndi','btm_reservoir',0.0)
    used = g_send_data(phyto(DIAZO)%id_fn_btm,phyto(DIAZO)%fn_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'nlg','btm_reservoir',phyto(LARGE)%fn_btm,isd,jsd)
    phyto(LARGE)%fn_btm = phyto(LARGE)%fn_btm/dt
    call g_tracer_get_pointer(tracer_list,'nlg_btf','field',temp_field)
    temp_field(:,:,1) = phyto(LARGE)%fn_btm(:,:)
    call g_tracer_set_values(tracer_list,'nlg','btm_reservoir',0.0)
    used = g_send_data(phyto(LARGE)%id_fn_btm,phyto(LARGE)%fn_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'nmd','btm_reservoir',phyto(MEDIUM)%fn_btm,isd,jsd)
    phyto(MEDIUM)%fn_btm = phyto(MEDIUM)%fn_btm/dt
    call g_tracer_get_pointer(tracer_list,'nmd_btf','field',temp_field)
    temp_field(:,:,1) = phyto(MEDIUM)%fn_btm(:,:)
    call g_tracer_set_values(tracer_list,'nmd','btm_reservoir',0.0)
    used = g_send_data(phyto(MEDIUM)%id_fn_btm,phyto(MEDIUM)%fn_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'nsm','btm_reservoir',phyto(SMALL)%fn_btm,isd,jsd)
    phyto(SMALL)%fn_btm = phyto(SMALL)%fn_btm/dt
    call g_tracer_get_pointer(tracer_list,'nsm_btf','field',temp_field)
    temp_field(:,:,1) = phyto(SMALL)%fn_btm(:,:)
    call g_tracer_set_values(tracer_list,'nsm','btm_reservoir',0.0)
    used = g_send_data(phyto(SMALL)%id_fn_btm,phyto(SMALL)%fn_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
    !
    ! Sinking phytoplankton: Iron
    !
    !> Iron flux to the sediment is removed, and flux from the sediment is
    !! handled separately later using a relationship based on Dale et al., 2015. 
    call g_tracer_get_values(tracer_list,'fedi','btm_reservoir',phyto(DIAZO)%ffe_btm,isd,jsd)
    phyto(DIAZO)%ffe_btm = phyto(DIAZO)%ffe_btm/dt
    call g_tracer_set_values(tracer_list,'fedi','btm_reservoir',0.0)
    used = g_send_data(phyto(DIAZO)%id_ffe_btm,phyto(DIAZO)%ffe_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'felg','btm_reservoir',phyto(LARGE)%ffe_btm,isd,jsd)
    phyto(LARGE)%ffe_btm = phyto(LARGE)%ffe_btm/dt
    call g_tracer_set_values(tracer_list,'felg','btm_reservoir',0.0)
    used = g_send_data(phyto(LARGE)%id_ffe_btm,phyto(LARGE)%ffe_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'femd','btm_reservoir',phyto(MEDIUM)%ffe_btm,isd,jsd)
    phyto(MEDIUM)%ffe_btm = phyto(MEDIUM)%ffe_btm/dt
    call g_tracer_set_values(tracer_list,'femd','btm_reservoir',0.0)
    used = g_send_data(phyto(MEDIUM)%id_ffe_btm,phyto(MEDIUM)%ffe_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'fesm','btm_reservoir',phyto(SMALL)%ffe_btm,isd,jsd)
    phyto(SMALL)%ffe_btm = phyto(SMALL)%ffe_btm/dt
    call g_tracer_set_values(tracer_list,'fesm','btm_reservoir',0.0)
    used = g_send_data(phyto(SMALL)%id_ffe_btm,phyto(SMALL)%ffe_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
    !
    ! Sinking phytoplankton: Phosphorus
    !
    call g_tracer_get_values(tracer_list,'pdi','btm_reservoir',phyto(DIAZO)%fp_btm,isd,jsd)
    phyto(DIAZO)%fp_btm = phyto(DIAZO)%fp_btm/dt
    call g_tracer_get_pointer(tracer_list,'pdi_btf','field',temp_field)
    temp_field(:,:,1) = phyto(DIAZO)%fp_btm(:,:)
    call g_tracer_set_values(tracer_list,'pdi','btm_reservoir',0.0)
    used = g_send_data(phyto(DIAZO)%id_fp_btm,phyto(DIAZO)%fp_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'plg','btm_reservoir',phyto(LARGE)%fp_btm,isd,jsd)
    phyto(LARGE)%fp_btm = phyto(LARGE)%fp_btm/dt
    call g_tracer_get_pointer(tracer_list,'plg_btf','field',temp_field)
    temp_field(:,:,1) = phyto(LARGE)%fp_btm(:,:)
    call g_tracer_set_values(tracer_list,'plg','btm_reservoir',0.0)
    used = g_send_data(phyto(LARGE)%id_fp_btm,phyto(LARGE)%fp_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'pmd','btm_reservoir',phyto(MEDIUM)%fp_btm,isd,jsd)
    phyto(MEDIUM)%fp_btm = phyto(MEDIUM)%fp_btm/dt
    call g_tracer_get_pointer(tracer_list,'pmd_btf','field',temp_field)
    temp_field(:,:,1) = phyto(MEDIUM)%fp_btm(:,:)
    call g_tracer_set_values(tracer_list,'pmd','btm_reservoir',0.0)
    used = g_send_data(phyto(MEDIUM)%id_fp_btm,phyto(MEDIUM)%fp_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'psm','btm_reservoir',phyto(SMALL)%fp_btm,isd,jsd)
    phyto(SMALL)%fp_btm = phyto(SMALL)%fp_btm/dt
    call g_tracer_get_pointer(tracer_list,'psm_btf','field',temp_field)
    temp_field(:,:,1) = phyto(SMALL)%fp_btm(:,:)
    call g_tracer_set_values(tracer_list,'psm','btm_reservoir',0.0)
    used = g_send_data(phyto(SMALL)%id_fp_btm,phyto(SMALL)%fp_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
    !
    ! Handle phytoplankton sinking: silicate
    !
    call g_tracer_get_values(tracer_list,'silg','btm_reservoir',phyto(LARGE)%fsi_btm,isd,jsd)
    phyto(LARGE)%fsi_btm = phyto(LARGE)%fsi_btm/dt
    call g_tracer_get_pointer(tracer_list,'silg_btf','field',temp_field)
    temp_field(:,:,1) = phyto(LARGE)%fsi_btm(:,:)
    call g_tracer_set_values(tracer_list,'silg','btm_reservoir',0.0)
    used = g_send_data(phyto(LARGE)%id_fsi_btm,phyto(LARGE)%fsi_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

    call g_tracer_get_values(tracer_list,'simd','btm_reservoir',phyto(MEDIUM)%fsi_btm,isd,jsd)
    phyto(MEDIUM)%fsi_btm = phyto(MEDIUM)%fsi_btm/dt
    call g_tracer_get_pointer(tracer_list,'simd_btf','field',temp_field)
    temp_field(:,:,1) = phyto(MEDIUM)%fsi_btm(:,:)
    call g_tracer_set_values(tracer_list,'simd','btm_reservoir',0.0)
    used = g_send_data(phyto(MEDIUM)%id_fsi_btm,phyto(MEDIUM)%fsi_btm, &
    model_time, rmask = grid_tmask(:,:,1),&
    is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

  end subroutine generic_COBALT_update_from_bottom

  ! <SUBROUTINE NAME="generic_COBALT_update_from_source">
  !  <OVERVIEW>
  !   Update tracer concentration fields due to the source/sink contributions.
  !  </OVERVIEW>
  !  <DESCRIPTION>
  !   This is the subroutine to contain most of the biogeochemistry for calculating the
  !   interaction of tracers with each other and with outside forcings.
  !  </DESCRIPTION>
  !  <TEMPLATE>
  !   call generic_COBALT_update_from_source(tracer_list,Temp,Salt,dzt,hblt_depth,&
  !                                         ilb,jlb,tau,dt, grid_dat,sw_pen,opacity)
  !  </TEMPLATE>
  !  <IN NAME="tracer_list" TYPE="type(g_tracer_type), pointer">
  !   Pointer to the head of generic tracer list.
  !  </IN>
  !  <IN NAME="ilb,jlb" TYPE="integer">
  !   Lower bounds of x and y extents of input arrays on data domain
  !  </IN>
  !  <IN NAME="Temp" TYPE="real, dimension(ilb:,jlb:,:)">
  !   Ocean temperature
  !  </IN>
  !  <IN NAME="Salt" TYPE="real, dimension(ilb:,jlb:,:)">
  !   Ocean salinity
  !  </IN>
  !  <IN NAME="dzt" TYPE="real, dimension(ilb:,jlb:,:)">
  !   Ocean layer thickness (meters)
  !  </IN>
  !  <IN NAME="opacity" TYPE="real, dimension(ilb:,jlb:,:)">
  !   Ocean opacity
  !  </IN>
  !  <IN NAME="sw_pen" TYPE="real, dimension(ilb:,jlb:)">
  !   Shortwave peneteration
  !  </IN>
  !  <IN NAME="hblt_depth" TYPE="real, dimension(ilb:,jlb:)">
  !
  !  </IN>
  !  <IN NAME="grid_dat" TYPE="real, dimension(ilb:,jlb:)">
  !   Grid area
  !  </IN>
  !  <IN NAME="tau" TYPE="integer">
  !   Time step index of %field
  !  </IN>
  !  <IN NAME="dt" TYPE="real">
  !   Time step increment
  !  </IN>
  ! </SUBROUTINE>
  !subroutine generic_COBALT_update_from_source(tracer_list,Temp,Salt,rho_dzt,dzt,hblt_depth,&
  !     ilb,jlb,tau,dt,grid_dat,model_time,nbands,max_wavelength_band,sw_pen_band,opacity_band,internal_heat,frunoff)
  ! If you'd like to pass the thermodynamic variables for a mld calculation
  subroutine generic_COBALT_update_from_source(tracer_list,Temp,Salt,rho_dzt,dzt,hblt_depth,&
       ilb,jlb,tau,dt,grid_dat,model_time,nbands,max_wavelength_band,sw_pen_band,opacity_band,internal_heat,frunoff,geolat,eqn_of_state)
  !subroutine generic_COBALT_update_from_source(tracer_list,Temp,Salt,rho_dzt,dzt,hblt_depth,&
  !     ilb,jlb,tau,dt,grid_dat,model_time,nbands,max_wavelength_band,sw_pen_band,opacity_band,internal_heat,frunoff)

    type(g_tracer_type),            pointer    :: tracer_list
    real, dimension(ilb:,jlb:,:),   intent(in) :: Temp,Salt,rho_dzt,dzt
    real, dimension(ilb:,jlb:),     intent(in) :: hblt_depth
    integer,                        intent(in) :: ilb,jlb,tau
    real,                           intent(in) :: dt
    real, dimension(ilb:,jlb:),     intent(in) :: grid_dat
    type(time_type),                intent(in) :: model_time

    integer,                        intent(in) :: nbands
    real, dimension(:),             intent(in) :: max_wavelength_band
    real, dimension(:,ilb:,jlb:),   intent(in) :: sw_pen_band
    real, dimension(:,ilb:,jlb:,:), intent(in) :: opacity_band
    ! internal_heat is optional because it will be a NULL pointer if
    ! geothermal heating is disabled.
    ! Later it will be tested if it is present (not NULL).
    real, dimension(ilb:,jlb:),     intent(in), optional :: internal_heat
    real, dimension(ilb:,jlb:),     intent(in) :: frunoff
    real, dimension(ilb:,jlb:),     intent(in) :: geolat
    type(EOS_type),                 intent(in) :: eqn_of_state !< Equation of state structure

    character(len=fm_string_len), parameter :: sub_name = 'generic_COBALT_update_from_source'
    integer :: isc,iec, jsc,jec,isd,ied,jsd,jed,nk,ntau, i, j, k , m, n, k_100, k_200, kmld_ref
    real, dimension(:,:,:) ,pointer :: grid_tmask
    integer, dimension(:,:),pointer :: mask_coast,grid_kmt
    !
    !------------------------------------------------------------------------
    ! Local Variables
    !------------------------------------------------------------------------
    !
    logical :: used, first
    integer :: nb
    real :: r_dt
    real :: feprime_temp
    real :: P_C_m, k_po4_adjust
    real :: TK, PRESS, PKSPA, PKSPC
    real :: tmp_hblt, tmp_irrad, tmp_irrad_ML,tmp_opacity,tmp_mu_ML
    real :: frac_sfc_irrad_aclm, irrad_aclm_thresh
    real :: tmp_irrad_aclm, tmp_zaclm
    real :: drho_dzt
    integer, dimension(:,:), Allocatable :: k_bot, kblt
    real, dimension(:), Allocatable   :: tmp_irr_band
    real, dimension(:,:), Allocatable :: rho_dzt_100,rho_dzt_200,rho_dzt_bot,sfc_irrad
    real,dimension(1:NUM_ZOO,1:NUM_PREY) :: ipa_matrix,pa_matrix,ingest_matrix
    real,dimension(1:NUM_PREY) :: hp_ipa_vec,hp_pa_vec,hp_ingest_vec
    real,dimension(1:NUM_PREY) :: prey_vec,prey_p2n_vec,prey_fe2n_vec,prey_si2n_vec
    real,dimension(1:NUM_ZOO)  :: tot_prey
    real :: a_theta, diff_theta2, diff_theta2_tol
    real :: tot_prey_hp, sw_fac_denom, assim_eff
    real :: bact_uptake_ratio, vmax_bact, growth_ratio, food1, food2
    real :: fpoc_btm, log10_fpoc_btm
    real :: fe_salt
    real :: sal,tt,tkb,ts,ts2,ts3,ts4,ts5
    real :: rho_mld_ref,rho_k,dK,dKm1,afac,deltaRhoAtK,deltaRhoAtKm1,deltaRhoFlag
    real :: alpha_temp, alpha_step
    real :: P_C_max_temp, P_C_max_step, bresp_temp
    real :: theta_temp, theta_step, irrlim_temp, P_C_m_temp
    real :: mu_temp, mu_opt
    integer :: yearday
    real :: rev_angle, dec_angle, temp_arg

    logical ::  phos_nh3_override

    real, dimension(:,:,:), Allocatable :: ztop, zmid, zbot
    real, dimension(:,:,:), Allocatable :: pre_totn, net_srcn, post_totn
    real, dimension(:,:,:), Allocatable :: pre_totp, net_srcp, post_totp
    real, dimension(:,:,:), Allocatable :: pre_totsi, post_totsi
    real, dimension(:,:,:), Allocatable :: pre_totfe, net_srcfe, post_totfe
    real, dimension(:,:,:), Allocatable :: pre_totc, net_srcc, post_totc
    real, dimension(:,:),   Allocatable :: pka_nh3,phos_nh3_exchange

    real :: tr,ltr
    real :: imbal
    integer :: stdoutunit, imbal_flag, outunit
    type(g_tracer_type), pointer :: g_tracer,g_tracer_next
    real :: KD_SMOOTH = 1.0E-05

    if(do_vertfill_pre) then
      g_tracer => tracer_list
      do
       if(g_tracer_is_prog(g_tracer)) then
         call g_tracer_vertfill(g_tracer, dzt, KD_SMOOTH*dt, tau=1)
       endif
       !traverse the linked list till hit NULL
       call g_tracer_get_next(g_tracer, g_tracer_next)
       if(.NOT. associated(g_tracer_next)) exit
       g_tracer=>g_tracer_next
      enddo
    endif

    r_dt = 1.0 / dt

    call g_tracer_get_common(isc,iec,jsc,jec,isd,ied,jsd,jed,nk,ntau,&
         grid_tmask=grid_tmask,grid_mask_coast=mask_coast,grid_kmt=grid_kmt)

    call mpp_clock_begin(id_clock_carbon_calculations)
    !Get necessary fields
    call g_tracer_get_values(tracer_list,'htotal','field', cobalt%f_htotal,isd,jsd)
    call g_tracer_get_values(tracer_list,'po4'   ,'field', cobalt%f_po4,isd,jsd)
    call g_tracer_get_values(tracer_list,'sio4'  ,'field', cobalt%f_sio4,isd,jsd)
    call g_tracer_get_values(tracer_list,'alk'   ,'field', cobalt%f_alk,isd,jsd)
    call g_tracer_get_values(tracer_list,'dic'   ,'field', cobalt%f_dic  ,isd,jsd)
    if (do_nh3_diag) then
       allocate(pka_nh3(isd:ied,jsd:jed))
       pka_nh3 = 0.
       call g_tracer_get_values(tracer_list,'nh4'   ,'field', cobalt%f_nh4  ,isd,jsd)
    end if
    allocate(phos_nh3_exchange(isd:ied,jsd:jed))

    !
    ! Calculate some thickness/vertical reference points for later calculations
    !
    allocate(ztop(isc:iec,jsc:jec,1:nk))
    allocate(zmid(isc:iec,jsc:jec,1:nk))
    allocate(zbot(isc:iec,jsc:jec,1:nk))
    do j = jsc, jec ; do i = isc, iec   !{
       cobalt%zt(i,j,1) = dzt(i,j,1)
       ztop(i,j,1) = 0.0
       zmid(i,j,1) = 0.5*dzt(i,j,1)
       zbot(i,j,1) = dzt(i,j,1)
    enddo; enddo !} i,j

    do k = 2, nk ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%zt(i,j,k) = cobalt%zt(i,j,k-1) + dzt(i,j,k)
       ztop(i,j,k) = zbot(i,j,k-1)
       zmid(i,j,k) = ztop(i,j,k) + 0.5*dzt(i,j,k)
       zbot(i,j,k) = ztop(i,j,k) + dzt(i,j,k)
    enddo; enddo ; enddo !} i,j,k

    !---------------------------------------------------------------------
    !Calculate co3_ion
    !Also calculate co2 fluxes csurf and alpha for the next round of exchange
    !---------------------------------------------------------------------


    k=1
    do j = jsc, jec ; do i = isc, iec  !{
       cobalt%htotallo(i,j) = cobalt%htotal_scale_lo * cobalt%f_htotal(i,j,k)
       cobalt%htotalhi(i,j) = cobalt%htotal_scale_hi * cobalt%f_htotal(i,j,k)
    enddo; enddo ; !} i, j


    call FMS_co2calc(CO2_dope_vec,grid_tmask(:,:,k),&
         Temp(:,:,k), Salt(:,:,k),                    &
         cobalt%f_dic(:,:,k),                          &
         cobalt%f_po4(:,:,k),                          &
         cobalt%f_sio4(:,:,k),                         &
         cobalt%f_alk(:,:,k),                          &
         cobalt%htotallo, cobalt%htotalhi,&
                                !InOut
         cobalt%f_htotal(:,:,k),                       &
                                !Optional In
         zt=cobalt%zt(:,:,k),                          &
                                !OUT
         co2star=cobalt%co2_csurf(:,:), alpha=cobalt%co2_alpha(:,:), &
         pCO2surf=cobalt%pco2_csurf(:,:), &
         co3_ion=cobalt%f_co3_ion(:,:,k), &
         omega_arag=cobalt%omega_arag(:,:,k), &
         omega_calc=cobalt%omega_calc(:,:,k))

    do k = 2, nk
       do j = jsc, jec ; do i = isc, iec  !{
          cobalt%htotallo(i,j) = cobalt%htotal_scale_lo * cobalt%f_htotal(i,j,k)
          cobalt%htotalhi(i,j) = cobalt%htotal_scale_hi * cobalt%f_htotal(i,j,k)
       enddo; enddo ; !} i, j

       call FMS_co2calc(CO2_dope_vec,grid_tmask(:,:,k),&
            Temp(:,:,k), Salt(:,:,k),                    &
            cobalt%f_dic(:,:,k),                          &
            cobalt%f_po4(:,:,k),                          &
            cobalt%f_sio4(:,:,k),                         &
            cobalt%f_alk(:,:,k),                          &
            cobalt%htotallo, cobalt%htotalhi,&
                                !InOut
            cobalt%f_htotal(:,:,k),                       &
                                !Optional In
            zt=cobalt%zt(:,:,k),                          &
                                !OUT
            co3_ion=cobalt%f_co3_ion(:,:,k), &
            omega_arag=cobalt%omega_arag(:,:,k), &
            omega_calc=cobalt%omega_calc(:,:,k))
    enddo

    call g_tracer_set_values(tracer_list,'htotal','field',cobalt%f_htotal  ,isd,jsd)
    call g_tracer_set_values(tracer_list,'co3_ion','field',cobalt%f_co3_ion  ,isd,jsd)
    call g_tracer_set_values(tracer_list,'dic','alpha',cobalt%co2_alpha    ,isd,jsd)
    call g_tracer_set_values(tracer_list,'dic','csurf',cobalt%co2_csurf    ,isd,jsd)

    call mpp_clock_end(id_clock_carbon_calculations)

    if (do_nh3_atm_ocean_exchange) then
       !to override pH used for ocean nh3 exchange
       call data_override('OCN', 'phos_nh3_exchange', phos_nh3_exchange(isc:iec,jsc:jec), model_time,override=phos_nh3_override)

       do j = jsc, jec ; do i = isc, iec
          pka_nh3(i,j)   = calc_pka_nh3(temp(i,j,1),salt(i,j,1))*grid_tmask(i,j,1)
          tr             = 298.15/(temp(i,j,1)+273.15)-1.
          ltr            = -tr+log(298.15/(temp(i,j,1)+273.15))
          !f1p
          !henry's constant is from Mark Jacobson's book "Fundamental of Atmospheric Modeling".
          !I used this expression to be consistent with the cloud chemistry module
          !the units are in mol/kg/atm. However it's probably in mol/kg(pure water)/atm
          !the density of pure water is ~997 kg/m3 (25C). alpha will then be scaled by the density of seawater, which for some reason I don't quite understand is always set to 1035.
          !to be consistent, I am scaling the Jacobson's number by 997/1035. This decreases the solubility of NH3 by less than 4%. For references, Sander's estimate is alpha(298.15)=0.59*101.63=59.96 mol/kg(pure water)/atm, about 4% greater than Jacobson's.
          cobalt%nh3_alpha(i,j)   =  5.76e1*exp(13.79*tr-5.39*ltr)*997.
          !apply salinity correction
          cobalt%nh3_alpha(i,j)   =  cobalt%nh3_alpha(i,j)/saltout_correction(101325./(1.e-3*rdgas*wtmair*(temp(i,j,1)+273.15)*cobalt%nh3_alpha(i,j)),vb_nh3,salt(i,j,1))* 1./cobalt%Rho_0 !mol/kg/atm
          if (phos_nh3_override) then
             cobalt%nh3_csurf(i,j)   =  cobalt%f_nh4(i,j,1)/(1.+10**(pka_nh3(i,j)-max(min(phos_nh3_exchange(i,j),11.),3.))) !in mol/kg
          else
             cobalt%nh3_csurf(i,j)   =  cobalt%f_nh4(i,j,1)/(1.+10**(pka_nh3(i,j)+log10(min(max(cobalt%f_htotal(i,j,1),1e-11),1e-3)))) !in mol/kg
          end if
          cobalt%pnh3_csurf(i,j)  =  cobalt%nh3_csurf(i,j)/cobalt%nh3_alpha(i,j)*1.e6 !in uatm
       enddo; enddo ; !

       call g_tracer_set_values(tracer_list,'nh4','alpha',cobalt%nh3_alpha    ,isd,jsd)
       call g_tracer_set_values(tracer_list,'nh4','csurf',cobalt%nh3_csurf    ,isd,jsd)
    end if

      if (do_14c) then                                        !<<RADIOCARBON

      ! Normally, the alpha would be multiplied by the atmospheric 14C/12C ratio. However,
      ! here that is set to 1, so that alpha_14C = alpha_12C. This needs to be changed!

   call g_tracer_get_values(tracer_list,'di14c' ,'field', cobalt%f_di14c,isd,jsd,positive=.true.)

    ! This is not used until later, but get it now
    call g_tracer_get_values(tracer_list,'do14c' ,'field', cobalt%f_do14c,isd,jsd,positive=.true.)

       do j = jsc, jec ; do i = isc, iec  !{
       cobalt%c14o2_csurf(i,j) =  cobalt%co2_csurf(i,j) *                &
         cobalt%f_di14c(i,j,1) / (cobalt%f_dic(i,j,1) + epsln)
       cobalt%c14o2_alpha(i,j) =  cobalt%co2_alpha(i,j)
       enddo; enddo ; !} i, j

    call g_tracer_set_values(tracer_list,'di14c','alpha',cobalt%c14o2_alpha      ,isd,jsd)
    call g_tracer_set_values(tracer_list,'di14c','csurf',cobalt%c14o2_csurf      ,isd,jsd)

      endif                                                   !RADIOCARBON>>

    !---------------------------------------------------------------------
    ! Get positive tracer concentrations
    !---------------------------------------------------------------------

    call mpp_clock_begin(id_clock_phyto_growth)

    call g_tracer_get_values(tracer_list,'cadet_arag','field',cobalt%f_cadet_arag ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'cadet_calc','field',cobalt%f_cadet_calc ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'fed'    ,'field',cobalt%f_fed      ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'fedet'  ,'field',cobalt%f_fedet    ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'ldon'   ,'field',cobalt%f_ldon     ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'ldop'   ,'field',cobalt%f_ldop     ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'lith'   ,'field',cobalt%f_lith     ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'lithdet','field',cobalt%f_lithdet  ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'ndet'   ,'field',cobalt%f_ndet     ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'nh4'    ,'field',cobalt%f_nh4      ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'no3'    ,'field',cobalt%f_no3      ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'o2'     ,'field',cobalt%f_o2       ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'pdet'   ,'field',cobalt%f_pdet     ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'po4'    ,'field',cobalt%f_po4      ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'srdon'   ,'field',cobalt%f_srdon   ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'srdop'   ,'field',cobalt%f_srdop   ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'sldon'   ,'field',cobalt%f_sldon   ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'sldop'   ,'field',cobalt%f_sldop   ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'sidet'  ,'field',cobalt%f_sidet    ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'sio4'   ,'field',cobalt%f_sio4     ,isd,jsd,positive=.true.)
!
    ! phytoplankton fields
    !
    call g_tracer_get_values(tracer_list,'fedi'   ,'field',phyto(DIAZO)%f_fe(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'felg'   ,'field',phyto(LARGE)%f_fe(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'femd'   ,'field',phyto(MEDIUM)%f_fe(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'fesm'   ,'field',phyto(SMALL)%f_fe(:,:,:),isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'pdi'   ,'field',phyto(DIAZO)%f_p(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'plg'   ,'field',phyto(LARGE)%f_p(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'pmd'   ,'field',phyto(MEDIUM)%f_p(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'psm'   ,'field',phyto(SMALL)%f_p(:,:,:),isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'ndi'    ,'field',phyto(DIAZO)%f_n(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'nlg'    ,'field',phyto(LARGE)%f_n(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'nmd'    ,'field',phyto(MEDIUM)%f_n(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'nsm'    ,'field',phyto(SMALL)%f_n(:,:,:),isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'silg'   ,'field',cobalt%f_silg     ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'simd'   ,'field',cobalt%f_simd     ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'mu_mem_ndi' ,'field',phyto(DIAZO)%f_mu_mem,isd,jsd)
    call g_tracer_get_values(tracer_list,'mu_mem_nlg' ,'field',phyto(LARGE)%f_mu_mem,isd,jsd)
    call g_tracer_get_values(tracer_list,'mu_mem_nmd' ,'field',phyto(MEDIUM)%f_mu_mem,isd,jsd)
    call g_tracer_get_values(tracer_list,'mu_mem_nsm' ,'field',phyto(SMALL)%f_mu_mem,isd,jsd)
    !
    ! zooplankton fields
    !
    call g_tracer_get_values(tracer_list,'nsmz'    ,'field',zoo(1)%f_n(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'nmdz'    ,'field',zoo(2)%f_n(:,:,:) ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'nlgz'    ,'field',zoo(3)%f_n(:,:,:) ,isd,jsd,positive=.true.)
    !
    ! bacteria
    !
    call g_tracer_get_values(tracer_list,'nbact'   ,'field',bact(1)%f_n(:,:,:) ,isd,jsd,positive=.true.)
    !
    ! diagnostic tracers that are passed between time steps (except chlorophyll)
    !
    call g_tracer_get_values(tracer_list,'cased'  ,'field',cobalt%f_cased    ,isd,jsd)
    call g_tracer_get_values(tracer_list,'co3_ion','field',cobalt%f_co3_ion  ,isd,jsd,positive=.true.)
    call g_tracer_get_values(tracer_list,'cadet_arag_btf','field',cobalt%f_cadet_arag_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'cadet_calc_btf','field',cobalt%f_cadet_calc_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'lithdet_btf','field',cobalt%f_lithdet_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'ndet_btf','field',cobalt%f_ndet_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'pdet_btf','field',cobalt%f_pdet_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'sidet_btf','field',cobalt%f_sidet_btf,isd,jsd)
    ! add phytoplankton because they now sink
    call g_tracer_get_values(tracer_list,'ndi_btf','field',cobalt%f_ndi_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'nlg_btf','field',cobalt%f_nlg_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'nmd_btf','field',cobalt%f_nmd_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'nsm_btf','field',cobalt%f_nsm_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'fedi_btf','field',cobalt%f_fedi_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'felg_btf','field',cobalt%f_felg_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'femd_btf','field',cobalt%f_femd_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'fesm_btf','field',cobalt%f_fesm_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'pdi_btf','field',cobalt%f_pdi_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'plg_btf','field',cobalt%f_plg_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'pmd_btf','field',cobalt%f_pmd_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'psm_btf','field',cobalt%f_psm_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'silg_btf','field',cobalt%f_silg_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'simd_btf','field',cobalt%f_simd_btf,isd,jsd)
    ! uncomment for "no mass change" test
    !call g_tracer_get_values(tracer_list,'fedet_btf','field',cobalt%f_fedet_btf,isd,jsd)
    call g_tracer_get_values(tracer_list,'irr_aclm','field',cobalt%f_irr_aclm ,isd,jsd)
    call g_tracer_get_values(tracer_list,'irr_aclm_z','field',cobalt%f_irr_aclm_z ,isd,jsd)
    call g_tracer_get_values(tracer_list,'irr_aclm_sfc','field',cobalt%f_irr_aclm_sfc ,isd,jsd)

    ! zero out cumulative COBALT-wide production diagnostics
    do k = 1, nk  ; do j = jsc, jec ; do i = isc, iec
       cobalt%jprod_fed(i,j,k) = 0.0
       cobalt%jprod_fedet(i,j,k) = 0.0
       cobalt%jprod_ndet(i,j,k) = 0.0
       cobalt%jprod_pdet(i,j,k) = 0.0
       cobalt%jprod_sldon(i,j,k) = 0.0
       cobalt%jprod_ldon(i,j,k) = 0.0
       cobalt%jprod_srdon(i,j,k) = 0.0
       cobalt%jprod_sldop(i,j,k) = 0.0
       cobalt%jprod_ldop(i,j,k) = 0.0
       cobalt%jprod_srdop(i,j,k) = 0.0
       cobalt%jprod_sidet(i,j,k) = 0.0
       cobalt%jprod_sio4(i,j,k) = 0.0
       cobalt%jprod_po4(i,j,k) = 0.0
       cobalt%jprod_nh4(i,j,k) = 0.0
       cobalt%jno3denit_wc(i,j,k) = 0.0
       cobalt%jremin_ndet(i,j,k) = 0.0
       cobalt%jo2resp_wc(i,j,k) = 0.0
    enddo;  enddo ;  enddo !} i,j,k
!
!-----------------------------------------------------------------------------------
! 1: Phytoplankton growth and nutrient uptake calculations
!-----------------------------------------------------------------------------------
!
    !
    !-----------------------------------------------------------------------------------
    ! 1.1: Nutrient Limitation Calculations
    !-----------------------------------------------------------------------------------
    !
    ! Calculate iron cell quota
    !
    do k = 1, nk  ; do j = jsc, jec ; do i = isc, iec
       do n = 1,NUM_PHYTO    !{
          phyto(n)%q_fe_2_n(i,j,k) = max(0.0, phyto(n)%f_fe(i,j,k)/ &
                 max(epsln,phyto(n)%f_n(i,j,k)))
          !phyto(n)%q_p_2_n(i,j,k) = phyto(n)%p_2_n_static
          phyto(n)%q_p_2_n(i,j,k) = max(0.0, phyto(n)%f_p(i,j,k)/ &
                 max(epsln,phyto(n)%f_n(i,j,k)))
          phyto(n)%uptake_p_2_n(i,j,k) = min(phyto(n)%p_2_n_min + phyto(n)%p_2_n_slope*cobalt%f_po4(i,j,k), &
                                             phyto(n)%p_2_n_max)
       enddo  !} n
       !
       ! N limitation with NH4 inhibition after Frost and Franzen (1992)
       ! (Note nitrate does not limit diazotroph growth but uptake limitation is used
       ! to determine nitrogen fixation versus facultative no3/nh4 uptake, see Sec. 1.3)
       do n= 1, NUM_PHYTO   !{
          if (scheme_no3_nh4_lim .eq. 1) then
             phyto(n)%no3lim(i,j,k) = cobalt%f_no3(i,j,k) / &
                  ( (phyto(n)%k_no3+cobalt%f_no3(i,j,k)) * (1.0 + cobalt%f_nh4(i,j,k)/phyto(n)%k_nh4) )
             phyto(n)%nh4lim(i,j,k) = cobalt%f_nh4(i,j,k) / (phyto(n)%k_nh4 + cobalt%f_nh4(i,j,k))
          elseif (scheme_no3_nh4_lim .eq. 2) then
             phyto(n)%no3lim(i,j,k) = cobalt%f_no3(i,j,k) / &
                  ( phyto(n)%k_no3+cobalt%f_no3(i,j,k)+phyto(n)%k_no3/phyto(n)%k_nh4*cobalt%f_nh4(i,j,k) )
             phyto(n)%nh4lim(i,j,k) = cobalt%f_nh4(i,j,k) / &
                  ( phyto(n)%k_nh4+cobalt%f_nh4(i,j,k)+phyto(n)%k_nh4/phyto(n)%k_no3*cobalt%f_no3(i,j,k) )
          end if
       enddo !} n
       !
       ! O2 inhibition term for diazotrophs
       !
       n = DIAZO
       phyto(n)%o2lim(i,j,k) = (1.0 - cobalt%f_o2(i,j,k)**cobalt%o2_inhib_Di_pow / &
         (cobalt%f_o2(i,j,k)**cobalt%o2_inhib_Di_pow+cobalt%o2_inhib_Di_sat**cobalt%o2_inhib_Di_pow))
       !
       ! SiO4, PO4 and Fe uptake limitation with Michaelis-Mentin
       !
       phyto(LARGE)%silim(i,j,k) = cobalt%f_sio4(i,j,k) / (phyto(LARGE)%k_sio4 + cobalt%f_sio4(i,j,k))
       phyto(MEDIUM)%silim(i,j,k) = cobalt%f_sio4(i,j,k) / (phyto(MEDIUM)%k_sio4 + cobalt%f_sio4(i,j,k))
       do n= 1, NUM_PHYTO   !{
          k_po4_adjust = phyto(n)%uptake_p_2_n(i,j,k)/phyto(n)%p_2_n_max
          phyto(n)%po4lim(i,j,k) = cobalt%f_po4(i,j,k) / (phyto(n)%k_po4*k_po4_adjust + cobalt%f_po4(i,j,k))
          phyto(n)%felim(i,j,k)  = cobalt%f_fed(i,j,k) / (phyto(n)%k_fed + cobalt%f_fed(i,j,k))
          phyto(n)%def_fe(i,j,k) = phyto(n)%q_fe_2_n(i,j,k)**2.0 / (phyto(n)%k_fe_2_n**2.0 +  &
               phyto(n)%q_fe_2_n(i,j,k)**2.0)
       enddo !} n
    enddo;  enddo ;  enddo !} i,j,k
    !
    ! Calculate nutrient limitation based on the most limiting nutrient (liebig_lim)
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       n=DIAZO
       phyto(n)%liebig_lim(i,j,k) = phyto(n)%o2lim(i,j,k)* &
          min(phyto(n)%po4lim(i,j,k), max(phyto(n)%def_fe(i,j,k),phyto(n)%felim(i,j,k)))
       do n= 2, NUM_PHYTO   !{
          phyto(n)%liebig_lim(i,j,k) = min(phyto(n)%no3lim(i,j,k)+phyto(n)%nh4lim(i,j,k),&
             phyto(n)%po4lim(i,j,k), max(phyto(n)%def_fe(i,j,k),phyto(n)%felim(i,j,k)))
       enddo !} n
    enddo;  enddo ;  enddo !} i,j,k
    !
    !-----------------------------------------------------------------------
    ! 1.2: Light Limitation/Growth Calculations
    !-----------------------------------------------------------------------
    !

    !  
    ! Calculate the mixed layer for phytoplankton photoacclimation
    ! The default criteria is de Boyer Montegut et al. (2004), where the
    ! mixed layer is calculated relative to zmld_ref = 10m and is based on when
    ! a potential density difference of 0.03 kg m-3.  This definition
    ! captures mixing on time scale of 1 to a few days.
    ! de Boyer-Montegut reference:  https://doi.org/10.1029/2004JC002378
    !
    do j = jsc, jec ; do i = isc, iec   !{

      if (grid_tmask(i,j,1).ne.0.0) then

        ! Find the k index closest to 10m  
        deltaRhoFlag = 0.0
        do k = 1,nk
          if (zmid(i,j,k) .lt. cobalt%zmld_ref) then
            kmld_ref = k
          elseif ((zmid(i,j,k).gt.cobalt%zmld_ref).and.(deltaRhoFlag.eq.0.0)) then
            if (k.eq.1) then
              kmld_ref = k
            elseif ((zmid(i,j,k)-cobalt%zmld_ref).lt.(cobalt%zmld_ref-zmid(i,j,k-1))) then
              kmld_ref = k
            endif
            deltaRhoFlag = 1.0
          endif
        enddo

        ! calculate the density at the reference depth
        call calculate_density(Temp(i,j,kmld_ref),Salt(i,j,kmld_ref),101325.0,rho_mld_ref,eqn_of_state)

        ! Calculate effective mixed layer depth for photoacclimation (mld_aclm)
        ! (parts of this code were drawn from the MOM6 mld calculation)
        dK = 0.0
        deltaRhoAtK = 0.0
        deltaRhoAtKm1 = 0.0
        deltaRhoFlag = 0.0
        cobalt%mld_aclm(i,j) = 0.0
        do k = 1,nk !{
          deltaRhoAtKm1 = deltaRhoAtK
          dKm1 = dK
          dK = cobalt%mld_aclm(i,j) + 0.5*dzt(i,j,k)
          ! Issue: Could MOM6 pass a potential density rather than recalculating
          call calculate_density(Temp(i,j,k),Salt(i,j,k),101325.0,rho_k,eqn_of_state)
          cobalt%rho_test(i,j,k) = rho_k
          deltaRhoAtK = rho_k - rho_mld_ref
          if (deltaRhoAtK.lt.cobalt%densdiff_mld) then
            cobalt%mld_aclm(i,j) = cobalt%mld_aclm(i,j) + dzt(i,j,k)
          elseif ((deltaRhoAtK.gt.cobalt%densdiff_mld).and.(deltaRhoFlag.eq.0.0)) then
            afac = (cobalt%densdiff_mld - deltaRhoAtKm1)/(deltaRhoAtK - deltaRhoAtKm1)
            cobalt%mld_aclm(i,j) = afac*dK + (1.0-afac)*dKm1
            deltaRhoFlag = 1.0
          endif
        enddo  !} k

        cobalt%mld_aclm(i,j) = cobalt%mld_aclm(i,j)*grid_tmask(i,j,1)
      else
        cobalt%mld_aclm(i,j) = 0.0
      endif

    enddo; enddo !} j,i

    !
    ! Calculate the underwater light field for the BGC calculations.  By default, COBALT
    ! receives visible and near-infrared inputs.  Near infrared is currently considered
    ! anything with wavelengths longer than ~710 nanometers.  Note that both shortwave and
    ! near infrared fall into the "shortwave" part of the radiation spectrum.  Following
    ! Morel and Antoine (1994) and Sweeney et al. (2005), about 57% of the incoming irradiance
    ! is assumed to lie within the visible range in the standard MOM6 radiation scheme
    ! with interactive chlorophyll.    
    ! 
    ! In COBALTv1/v2, all of the visible wavelengths were included as photosynthically
    ! active radiation (PAR).  This approach, however, included wavelengths shorter than  
    ! the ~350-400 nanometer lower bound applied for PAR.  The "par_adj" parameter
    ! allows for a downward adjustment.  Its default value of 0.83 gives a PAR of 47%
    ! of the total shortwave flux, consistent with Baker and Frouin (1987).
    !
    ! The instantaneous and acclimation irradiances are then calculated at all depths.  The
    ! former is eventually used to calculate instaneous phytoplankton growth, while the latter
    ! is used to calculate the chlorophyll to carbon ratio.  The acclimation irradiance
    ! is effectively an average of the instantaneous irradiance over daylight hours across 
    ! an acclimation timescale (typically 24 hours).  The daylength for this calculation is 
    ! taken from CBM model described in Forsythe et al. (1995). The acclimation irradiance 
    ! in the surface mixed layer is generally assumed to be the average in the mixed layer,
    ! except when the mixed layer extends beyond "ml_aclm_efold" e-folding scales for the
    ! irradiance.  In these deep mixed layer cases the average light down to "ml_aclm_efold" 
    ! is used for the acclimation irradiance with the mixed layer.  Full details of these 
    ! calculations and their implications are presented and discussed in Stock et al. (submitted).
    !
    ! References:
    ! Morel and Antoine: https://doi.org/10.1175/1520-0485(1994)024<1652:HRWTUO>2.0.CO;2
    ! Sweeney et al.: https://doi.org/10.1175/JPO2740.1
    ! Baker and Frouin:  https://doi.org/10.4319/lo.1987.32.6.1370
    ! Forsythe et al.: https://www.sciencedirect.com/science/article/pii/030438009400034F
    ! Stock et al. (submitted) (link to be added as soon as available)
    !
    allocate(tmp_irr_band(nbands))        ! irradiance in wavelength bands
    allocate(sfc_irrad(isc:iec,jsc:jec))  ! surface photosythetically available irradiance
    allocate(kblt(isc:iec,jsc:jec))       ! tracks of max k index in mixed layer
    frac_sfc_irrad_aclm = 1.0/(2.71828**cobalt%ml_aclm_efold) ! controls acclimation in deep mixed layers
    do j = jsc, jec ; do i = isc, iec   !{

       ! Calculate photosynthetically available radiation at air-sea interface (sfc_irrad)
       sfc_irrad(i,j) = 0.0
       do nb=1,nbands !{
          if (max_wavelength_band(nb) .lt. 710.0) then !{
             tmp_irr_band(nb) = cobalt%par_adj*sw_pen_band(nb,i,j)
             sfc_irrad(i,j) = sfc_irrad(i,j) + cobalt%par_adj*sw_pen_band(nb,i,j)
          else
             tmp_irr_band(nb) = 0.0
          endif !}
       enddo !}

       ! calculate the day length (cobalt%daylength(i,j) based on the CBM daylength model
       ! rev_angle = revolution angle (eq. (1) of Forsythe et al.)
       ! dec_angle = sun's declination angle (eq. (2) of Forsythe et al.)
       ! daylength (in hours)
       yearday = day_of_year(model_time)
       rev_angle = 0.2163108 + 2.0*atan(0.9671396*tan(0.00860*(real(yearday,8) - 186.0)))
       dec_angle = asin(0.39795*cos(rev_angle))
       temp_arg = (sin(12.0*3.14/180.0)+sin(geolat(i,j)*3.14/180.0)*sin(dec_angle)) / &
                      (cos(geolat(i,j)*3.14/180.0)*cos(dec_angle))
       ! bound to be -1 (complete darkness) or 1 (complete day)
       temp_arg = max(min(temp_arg,1.0),-1.0)
       cobalt%daylength(i,j) = 24.0 - 24.0/3.14*acos(temp_arg)

       ! Calculate the acclimation irradiance at the surface.  This basic equation relaxes
       ! the irradiance toward the current value with a an inverse time scale set by gamma:
       !  
       ! I_aclm(t+1) = I_aclm(t) + (I*(24/daylength)-I_aclm(t))*gamma*dt
       !
       ! multiplication by 24/daylength adjusts the irradiance averaged over 24 hours to
       ! upward to approximate the irradiance during daylight hours. For photoacclimation 
       ! the default relaxation timescale is set to 1 day (gamma = 1/86400 sec), and
       ! "dt" is the time step.
       cobalt%f_irr_aclm_sfc(i,j,1) = (cobalt%f_irr_aclm_sfc(i,j,1) + &
         (sfc_irrad(i,j)*24.0/max(cobalt%daylength(i,j),cobalt%min_daylength)-cobalt%f_irr_aclm_sfc(i,j,1)) * &
         min(1.0,cobalt%gamma_irr_aclm * dt)) * grid_tmask(i,j,1)

       !
       ! Calculate the subsurface and irradiance fields
       !
       kblt(i,j) = 0         ! saves the max k index within the mixed layer 
       tmp_irrad_ML = 0.0    ! integrates the irradiance in the mixed layer
       tmp_hblt = 0.0        ! tracks depth of the top of the current layer for mld calcs
       tmp_irrad_aclm = 0.0  ! integrates the irradiance in the surface photoacclimation layer
       tmp_zaclm = 0.0       ! tracks depth of top of the curent layer photoacclimation layer calcs
       ! Define the irradiance threshold for a "deep" mixed layer for photoacclimation
       irrad_aclm_thresh = frac_sfc_irrad_aclm*cobalt%f_irr_aclm_sfc(i,j,1)
       do k = 1, nk !{
          tmp_irrad = 0.0
          ! Sum up the irradiance in all bands at the k level
          do nb=1,nbands !{

             ! Issue: This code currently includes an option to increase opacity in shallow/fresh
             ! water.  This should be moved to a namelist (and eventually replaced with a more 
             ! robust coastal optics model with full feedbacks to the physics)
             if ((zmid(i,j,nk).le.cobalt%case2_depth).or.(Salt(i,j,k).le.cobalt%case2_salt)) then
               tmp_opacity = opacity_band(nb,i,j,k) + cobalt%case2_opac_add
             else
               tmp_opacity = opacity_band(nb,i,j,k)
             endif

             ! Calculate the irradiance at the mid-point of the grid cell
             tmp_irrad = tmp_irrad + max(0.0,tmp_irr_band(nb) * exp(-tmp_opacity * dzt(i,j,k) * 0.5))
             ! Calculate the irradiance at the bottom of the grid cell in preparation for the next k 
             tmp_irr_band(nb) = tmp_irr_band(nb) * exp(-tmp_opacity * dzt(i,j,k))
          enddo !}

          cobalt%irr_inst(i,j,k) = tmp_irrad * grid_tmask(i,j,k)
          cobalt%irr_aclm_inst(i,j,k) = tmp_irrad*24.0/max(cobalt%daylength(i,j),cobalt%min_daylength)* &
                                        grid_tmask(i,j,k)
          ! Issue: evaluate what it would take to remove this variable 
          cobalt%irr_mix(i,j,k) = tmp_irrad * grid_tmask(i,j,k)

          ! This acclimation irradiance variables saves the full depth structure (even in the mixed layer)
          ! We need it to determine when a mixed layer is "deep" for purposes of photoacclimation
          cobalt%f_irr_aclm_z(i,j,k) = (cobalt%f_irr_aclm_z(i,j,k) + &
                  (cobalt%irr_inst(i,j,k)*24.0/max(cobalt%daylength(i,j),cobalt%min_daylength) - &
                  cobalt%f_irr_aclm_z(i,j,k)) * min(1.0,cobalt%gamma_irr_aclm * dt)) * grid_tmask(i,j,k)

          ! calculate the integrated light in the mixed layer and in the integrated light and depth of
          ! the acclimation layer.  These are equal for shallow mixed layers but can depart for deeper layers
          if ( (k == 1) .or. (tmp_hblt .lt. cobalt%mld_aclm(i,j)) ) then
             kblt(i,j) = kblt(i,j)+1
             tmp_irrad_ML = tmp_irrad_ML + cobalt%irr_inst(i,j,k) * dzt(i,j,k)
             tmp_hblt = tmp_hblt + dzt(i,j,k)

             if (cobalt%f_irr_aclm_z(i,j,k) .ge. irrad_aclm_thresh) then
                tmp_irrad_aclm = tmp_irrad_aclm + cobalt%irr_inst(i,j,k) * dzt(i,j,k)
                tmp_zaclm = tmp_zaclm + dzt(i,j,k)
             endif
          endif

       enddo !} k-loop

       ! Phytoplankton in the surface mixed layer (1:kblt) aclimate to the mean daytime
       ! light level over the photoacclimation layer: (tmp_irrad_aclm/tmp_zaclm)*24/daylength
       cobalt%irr_aclm_inst(i,j,1:kblt(i,j)) = tmp_irrad_aclm/max(1.0e-6,tmp_zaclm)*24.0/ &
                                               max(cobalt%daylength(i,j),cobalt%min_daylength)
       ! Issue: what would it take to remove irr_mix? 
       cobalt%irr_mix(i,j,1:kblt(i,j)) = tmp_irrad_ML / max(1.0e-6,tmp_hblt)
    enddo;  enddo !} i,j

    deallocate(tmp_irr_band)
    !
    ! Calculate the final photoacclimation irradiance using the standard relaxation
    ! scheme (I_aclm(t+1) = I_aclm(t) + (I*(24/daylength)-I_aclm(t))*gamma*dt).
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
         cobalt%f_irr_aclm(i,j,k) = (cobalt%f_irr_aclm(i,j,k) + (cobalt%irr_aclm_inst(i,j,k) - &
           cobalt%f_irr_aclm(i,j,k)) * min(1.0,cobalt%gamma_irr_aclm * dt)) * grid_tmask(i,j,k)
    enddo; enddo ; enddo !} i,j,k


    ! This needs to be moved! 
    !nh3
    if (do_nh3_diag) then
    cobalt%f_nh3(:,:,:) = 0.
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%f_nh3(i,j,k) = cobalt%f_nh4(i,j,k)/(1.+10**(calc_pka_nh3(temp(i,j,k),salt(i,j,k))+log10(min(max(cobalt%f_htotal(i,j,1),1e-10),1e-5)))) * grid_tmask(i,j,k)
    enddo;  enddo ; enddo !} i,j,k
    end if


    !
    ! Calculate the phytoplankton growth rate calculation based on Geider et al. (1997).
    ! This section also allows for low- and high-light adapted "ecotypes" (e.g., Moore
    ! and Chisholm, 1999).  As described in Stock et al. (submitted), low-light adapted
    ! ecotypes are characterized by a steep initial slope of the photosynthesis-irradiance
    ! curve (i.e., high values of Geider's "alpha") and a low maximum photosynthetic rate
    ! (low P_C_max, and high-light adapted cells have the opposite.  The best suited
    ! ecotype is the one that achieves maximal growth at the acclimation irradiance.
    !
    ! references:
    ! Moore and Chisholm: https://doi.org/10.4319/lo.1999.44.3.0628
    ! Stock et al. (submitted) (link to be added as soon as available)
    !   
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%f_chl(i,j,k) = 0.0

      ! calculate temperature dependence for all phytoplankton
       cobalt%expkT(i,j,k) = exp(cobalt%kappa_eppley * Temp(i,j,k))

       do n = 1, NUM_PHYTO   !{

          !
          ! The basal respiration is a fraction of the maximum photosynthetic rate
          ! is higher within the mixed layer.  The variable bresp_temp will be multiplied
          ! by the P_C_max for each ecotype to determine which is most competitive. 
          !
          if (k.le.kblt(i,j)) then
             bresp_temp = phyto(n)%bresp_frac_mixed*cobalt%expkT(i,j,k)
          else
             bresp_temp = phyto(n)%bresp_frac_strat*cobalt%expkT(i,j,k)
          endif
          ! adjust basal respiration to maintain small refuge
          bresp_temp = bresp_temp*phyto(n)%f_n(i,j,k)/(cobalt%refuge_conc+phyto(n)%f_n(i,j,k))

          ! Loop through the ecotypes to find the most competitive.  This is essentially a Geider growth
          ! rate calculation for each ecotype using the acclimation irradiance.  
          mu_opt = -999.0 ! arbitrarily low value
          do m = 1,cobalt%numlightadapt
            ! since we test the low and high, divide by m-1 so first step is the low and last is the high
            alpha_step = (phyto(n)%alpha_ll - phyto(n)%alpha_hl)/(real(cobalt%numlightadapt,8)-1.0)
            alpha_temp = phyto(n)%alpha_hl + (real(m,8)-1.0)*alpha_step
            P_C_max_step = (phyto(n)%P_C_max_hl - phyto(n)%P_C_max_ll)/(real(cobalt%numlightadapt,8)-1.0)
            P_C_max_temp = phyto(n)%P_C_max_hl - (real(m,8)-1.0)*P_C_max_step
            P_C_m_temp = max(phyto(n)%liebig_lim(i,j,k)*P_C_max_temp*cobalt%expkT(i,j,k),epsln)
            theta_temp = max(phyto(n)%thetamax/(1.0 + phyto(n)%thetamax*alpha_temp*cobalt%f_irr_aclm(i,j,k)*0.5/P_C_m_temp), &
                             cobalt%thetamin)
            irrlim_temp = 1.0-exp(-alpha_temp*cobalt%f_irr_aclm(i,j,k)*theta_temp/P_C_m_temp)
            mu_temp = P_C_m_temp/(1.0 + cobalt%zeta)*irrlim_temp - bresp_temp*P_C_max_temp
            ! test to see if the latest ecotype is better than the current optimum.  If so, replace the
            ! current best values.
            if (mu_temp.ge.mu_opt) then
              mu_opt = mu_temp
              phyto(n)%irrlim(i,j,k) = 1.0-exp(-alpha_temp*cobalt%irr_inst(i,j,k)*theta_temp/P_C_m_temp)
              phyto(n)%theta(i,j,k) = theta_temp
              phyto(n)%bresp(i,j,k) =  bresp_temp*P_C_max_temp
              phyto(n)%mu(i,j,k) = P_C_m_temp/(1.0 + cobalt%zeta)*phyto(n)%irrlim(i,j,k) - phyto(n)%bresp(i,j,k)
              phyto(n)%P_C_max(i,j,k) = P_C_max_temp*cobalt%expkT(i,j,k)
              phyto(n)%alpha(i,j,k) = alpha_temp
            endif
          enddo

          ! Calculate the chlorophyll.  Coversions give mg Chl (1000 kg)-1 ~ mg Chl m-3 
          phyto(n)%chl(i,j,k) = cobalt%c_2_n*12.0e6*phyto(n)%theta(i,j,k)*phyto(n)%f_n(i,j,k)
          cobalt%f_chl(i,j,k) = cobalt%f_chl(i,j,k)+phyto(n)%chl(i,j,k)

          ! calculate net production by phytoplankton group
          phyto(n)%jprod_n(i,j,k) = phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k)
          ! diagnositic for average growth in the mixed layer (averaged below)
          phyto(n)%mu_mix(i,j,k) = phyto(n)%mu(i,j,k)

       enddo !} n

    enddo;  enddo ; enddo !} i,j,k

    !
    ! Calculate the time averaged growth rate (generally over 24 hours)
    ! This is used later for phytoplankton stress calculations that can 
    ! control sinking and aggregation.  First loop provides average growth
    ! in the mixed layer.  The second averages over all depths.
    !
    do j = jsc, jec ; do i = isc, iec ; do n = 1,NUM_PHYTO !{
       tmp_mu_ML = 0.0 ; tmp_hblt = 0.0
       do k = 1, nk !{
          if ((k == 1) .or. (tmp_hblt .lt. cobalt%mld_aclm(i,j))) then !{
             tmp_mu_ML = tmp_mu_ML + phyto(n)%mu_mix(i,j,k) * dzt(i,j,k)
             tmp_hblt = tmp_hblt + dzt(i,j,k)
          endif !}
       enddo !} k-loop
       phyto(n)%mu_mix(i,j,1:kblt(i,j)) = tmp_mu_ML / max(epsln,tmp_hblt)
    enddo;  enddo; enddo !} i,j,n

    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec; do n = 1,NUM_PHYTO !{
       phyto(n)%f_mu_mem(i,j,k) = phyto(n)%f_mu_mem(i,j,k) + (phyto(n)%mu_mix(i,j,k) - &
             phyto(n)%f_mu_mem(i,j,k))*min(1.0,cobalt%gamma_mu_mem*dt)*grid_tmask(i,j,k)
    enddo; enddo ; enddo; enddo !} i,j,k,n

    !-----------------------------------------------------------------------
    ! 1.3: Nutrient uptake calculations
    !-----------------------------------------------------------------------
    !
    ! Uptake of nitrate and ammonia
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       n = DIAZO
       phyto(n)%juptake_n2(i,j,k) =  max(0.0,(1.0 - phyto(n)%no3lim(i,j,k) - phyto(n)%nh4lim(i,j,k))* &
          phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k))
       phyto(n)%juptake_nh4(i,j,k) = max(0.0,phyto(n)%nh4lim(i,j,k)* phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k))
       phyto(n)%juptake_no3(i,j,k) = max(0.0,phyto(n)%no3lim(i,j,k)* phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k))

       ! If growth is negative, results in net respiration and production of nh4, aerobic loss in all cases
       ! jo2resp_wc is a cumulative variable that tracks the total oxygen consumption in the water column
       cobalt%jprod_nh4(i,j,k) = cobalt%jprod_nh4(i,j,k) - min(0.0,phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k))
       cobalt%jo2resp_wc(i,j,k) = cobalt%jo2resp_wc(i,j,k) - min(0.0,phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k))*cobalt%o2_2_nh4
       do n = 2, NUM_PHYTO !{
          ! Nitrate versus ammonia uptake proportional to their relative limitations
          phyto(n)%juptake_no3(i,j,k) = max( 0.0, phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k)*   &
             phyto(n)%no3lim(i,j,k)/(phyto(n)%no3lim(i,j,k)+phyto(n)%nh4lim(i,j,k)+epsln) )
          phyto(n)%juptake_nh4(i,j,k) = max( 0.0, phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k)*   &
             phyto(n)%nh4lim(i,j,k)/(phyto(n)%no3lim(i,j,k)+phyto(n)%nh4lim(i,j,k)+epsln) )
          ! If growth is negative, results in net respiration and production of nh4, aerobic loss in all cases
          cobalt%jprod_nh4(i,j,k) = cobalt%jprod_nh4(i,j,k) - min(0.0,phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k))
          cobalt%jo2resp_wc(i,j,k) = cobalt%jo2resp_wc(i,j,k) - min(0.0,phyto(n)%mu(i,j,k)*phyto(n)%f_n(i,j,k))*cobalt%o2_2_nh4
       enddo !} n
    enddo;  enddo ; enddo !} i,j,k
    !
    ! Phosphorous uptake
    !
    do k = 1, nk  ;    do j = jsc, jec ;      do i = isc, iec   !{
       n=DIAZO
       phyto(n)%juptake_po4(i,j,k) = (phyto(n)%juptake_n2(i,j,k)+phyto(n)%juptake_nh4(i,j,k) + &
          phyto(n)%juptake_no3(i,j,k))*phyto(n)%uptake_p_2_n(i,j,k)
       ! If growth is negative, results in net release of po4
       cobalt%jprod_po4(i,j,k) = cobalt%jprod_po4(i,j,k) - &
          min(0.0,phyto(n)%mu(i,j,k)*phyto(n)%f_p(i,j,k))
       do n = 2, NUM_PHYTO
          phyto(n)%juptake_po4(i,j,k) = (phyto(n)%juptake_nh4(i,j,k)+phyto(n)%juptake_no3(i,j,k))* &
            phyto(n)%uptake_p_2_n(i,j,k)
          ! If growth is negative, results in net release of po4
          cobalt%jprod_po4(i,j,k) = cobalt%jprod_po4(i,j,k) - &
            min(0.0,phyto(n)%mu(i,j,k)*phyto(n)%f_p(i,j,k))
       enddo !} n
    enddo; enddo ; enddo !} i,j,k
    !
    ! Iron uptake
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       do n = 1, NUM_PHYTO  !{
          if (phyto(n)%q_fe_2_n(i,j,k).lt.phyto(n)%fe_2_n_max) then
             ! Scaling fe uptake with the maximum iron-limited photosynthesis allows for luxury iron uptake
             ! when other nutrients are limiting but iron is not
             phyto(n)%juptake_fe(i,j,k) = phyto(n)%P_C_max(i,j,k)*cobalt%expkT(i,j,k)*phyto(n)%f_n(i,j,k)* &
                phyto(n)%felim(i,j,k)*cobalt%fe_2_n_upt_fac
             phyto(n)%jexuloss_fe(i,j,k) = 0.0
          else
             ! if you've exceeded the maximum quota, stop uptake and exude extra
             phyto(n)%juptake_fe(i,j,k) = 0.0
             phyto(n)%jexuloss_fe(i,j,k) = cobalt%expkT(i,j,k)*phyto(n)%bresp(i,j,k)*phyto(n)%f_fe(i,j,k)
          endif
       enddo   !} n
    enddo; enddo ; enddo !} i,j,k
    !
    ! Silicate uptake
    !
    do k = 1, nk  ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%nlg_diatoms(i,j,k)=phyto(LARGE)%f_n(i,j,k)*phyto(LARGE)%silim(i,j,k)
       cobalt%nmd_diatoms(i,j,k)=phyto(MEDIUM)%f_n(i,j,k)*phyto(MEDIUM)%silim(i,j,k)
       cobalt%q_si_2_n_lg_diatoms(i,j,k)= cobalt%f_silg(i,j,k)/ &
             (cobalt%nlg_diatoms(i,j,k) + epsln)
       cobalt%q_si_2_n_md_diatoms(i,j,k)= cobalt%f_simd(i,j,k)/ &
             (cobalt%nmd_diatoms(i,j,k) + epsln)
       phyto(LARGE)%juptake_sio4(i,j,k) = &
             max(phyto(LARGE)%juptake_no3(i,j,k)+phyto(LARGE)%juptake_nh4(i,j,k),0.0)*phyto(LARGE)%silim(i,j,k)* &
             phyto(LARGE)%silim(i,j,k)*phyto(LARGE)%si_2_n_max
       phyto(MEDIUM)%juptake_sio4(i,j,k) = &
             max(phyto(MEDIUM)%juptake_no3(i,j,k)+phyto(MEDIUM)%juptake_nh4(i,j,k),0.0)*phyto(MEDIUM)%silim(i,j,k)* &
             phyto(MEDIUM)%silim(i,j,k)*phyto(MEDIUM)%si_2_n_max

       ! Note that this is si_2_n in large phytoplankton pool, not in diatoms themselves (q_si_2_n_lg_diatoms)
       phyto(LARGE)%q_si_2_n(i,j,k) = cobalt%f_silg(i,j,k)/(phyto(LARGE)%f_n(i,j,k)+epsln)
       phyto(MEDIUM)%q_si_2_n(i,j,k) = cobalt%f_simd(i,j,k)/(phyto(MEDIUM)%f_n(i,j,k)+epsln)
    enddo; enddo ; enddo !} i,j,k

    call mpp_clock_end(id_clock_phyto_growth)
!
!---------------------------------------------------------------------------
! 2: Free-living bacterial transformatioins and growth/uptake calculations
!---------------------------------------------------------------------------
!
    call mpp_clock_begin(id_clock_bacteria_growth)

    ! Anammox converts NH4+ to N2 using NO3- in low O2 environments.
    ! This was not included in ESM4.1 and gamma_nh4amx is currently 0.0
    ! by default. 
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{

       if (cobalt%f_o2(i,j,k) .lt. cobalt%o2_min_amx) then !{
         ! Uptake of NH4+ and NO3- through the anammox process
         cobalt%juptake_nh4amx(i,j,k) = cobalt%gamma_nh4amx * &
            cobalt%f_no3(i,j,k) / (cobalt%k_no3_amx + cobalt%f_no3(i,j,k)) * &
            cobalt%f_nh4(i,j,k)
         cobalt%juptake_no3amx(i,j,k) = cobalt%juptake_nh4amx(i,j,k)*&
            cobalt%no3_2_nh4_amx
         ! N lost to N2 via anammox
         cobalt%jnamx(i,j,k) = cobalt%juptake_nh4amx(i,j,k) + cobalt%juptake_no3amx(i,j,k)
       else
         cobalt%juptake_nh4amx(i,j,k) = 0.0
         cobalt%juptake_no3amx(i,j,k) = 0.0
         cobalt%jnamx(i,j,k) = 0.0
       endif !}

    enddo; enddo; enddo  !} i,j,k

    !
    !  Calculate nitrification rates.  There are three possible schemes to use.  Schemes 2 and 3 are
    !  described in Paulot et al., 2020.  Ocean Ammonia Outgassing: Modulation by CO2 and Anthropogenic
    !  Nitrogen Deposition.  JAMES. https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1029/2019MS002026
    !  They rely on the calculated partitioning of reduced nitrogen species between ammonium (NH4) and
    !  ammonia (NH3).  Scheme 1 is from COBALTv1.  Note that the acclimation irradiance, which reflects
    !  the irradiance during daylight hours, has been used to impose nitrification photoinhibition.
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%juptake_nh4nitrif(i,j,k) = 0.0
       if (scheme_nitrif .eq. 2 .or. scheme_nitrif .eq. 3) then
          if (cobalt%f_o2(i,j,k) .gt. cobalt%o2_min_nit) then  !{
             cobalt%juptake_nh4nitrif(i,j,k) = cobalt%gamma_nitrif * &
                  cobalt%f_nh3(i,j,k)/(cobalt%f_nh3(i,j,k)+cobalt%k_nh3_nitrif) *  &
                  (1.-cobalt%f_irr_aclm(i,j,k)/(cobalt%irr_inhibit+cobalt%f_irr_aclm(i,j,k))) * &
                  cobalt%f_o2(i,j,k)/(cobalt%k_o2_nit+cobalt%f_o2(i,j,k)) * cobalt%f_nh4(i,j,k)**2

             if (scheme_nitrif .eq. 3) then
                cobalt%juptake_nh4nitrif(i,j,k) = cobalt%juptake_nh4nitrif(i,j,k)*cobalt%expkT(i,j,k)
             end if
          end if
       elseif (scheme_nitrif .eq. 1) then
          if (cobalt%f_o2(i,j,k) .gt. cobalt%o2_min) then  !{
             cobalt%juptake_nh4nitrif(i,j,k) = cobalt%gamma_nitrif * cobalt%expkT(i,j,k) * cobalt%f_nh4(i,j,k) * &
                  phyto(SMALL)%nh4lim(i,j,k) * (1.0 - cobalt%f_irr_aclm(i,j,k) / &
                  (cobalt%irr_inhibit + cobalt%f_irr_aclm(i,j,k))) * cobalt%f_o2(i,j,k) / &
                  ( cobalt%k_o2 + cobalt%f_o2(i,j,k) )
          end if
       end if
       ! Add the oxygen used for nitrification to the total water column respiration
       cobalt%jo2resp_wc(i,j,k) = cobalt%jo2resp_wc(i,j,k)+cobalt%juptake_nh4nitrif(i,j,k)*cobalt%o2_2_nitrif
       ! Calculate the production of NO3 from nitrification
       cobalt%jprod_no3nitrif(i,j,k) = cobalt%juptake_nh4nitrif(i,j,k)
    enddo; enddo; enddo  !} i,j,k

    !
    ! Calculate production by free-living heterotrophic bacteria, which consume labile dissolved organic material
    !

    ! back calculate an effective maximum ldon uptake rate (at 0 deg. C) for bacteria, i.e.:
    ! mu_max = gge_max*vmax - bresp; so (vmax = mu_max+bresp)/gge_max
    vmax_bact = (1.0/bact(1)%gge_max)*(bact(1)%mu_max + bact(1)%bresp)
    do k = 1, nk  ; do j = jsc, jec ; do i = isc, iec   !{
       !
       ! Calculate the growth rate of heterotrophic bacteria (bact%mu)
       !
       bact(1)%temp_lim(i,j,k) = exp(bact(1)%ktemp*Temp(i,j,k))
       bact(1)%ldonlim(i,j,k) = cobalt%f_ldon(i,j,k)/(bact(1)%k_ldon + cobalt%f_ldon(i,j,k))
       ! Note that the minimum value of o2lim, o2_min/(k_o2+o2_min), sets the rate for
       ! anaerobic remineralization.
       bact(1)%o2lim(i,j,k) = max(cobalt%f_o2(i,j,k),cobalt%o2_min)/  &
                              (cobalt%k_o2 + max(cobalt%f_o2(i,j,k),cobalt%o2_min))
       bact(1)%juptake_ldon(i,j,k) = vmax_bact*bact(1)%temp_lim(i,j,k)*bact(1)%ldonlim(i,j,k)* &
               bact(1)%o2lim(i,j,k)*bact(1)%f_n(i,j,k)
       bact_uptake_ratio = ( cobalt%f_ldop(i,j,k)/max(cobalt%f_ldon(i,j,k),epsln) )
       bact(1)%juptake_ldop(i,j,k) = bact(1)%juptake_ldon(i,j,k)*bact_uptake_ratio
       ! calculate bacteria production if N-limited, adjust down if P-limited
       bact(1)%jprod_n(i,j,k) = bact(1)%gge_max*bact(1)%juptake_ldon(i,j,k) - &
          bact(1)%f_n(i,j,k)/(cobalt%refuge_conc + bact(1)%f_n(i,j,k)) * &
          bact(1)%temp_lim(i,j,k)*bact(1)%bresp*bact(1)%f_n(i,j,k)
       bact(1)%jprod_n(i,j,k) = min(bact(1)%jprod_n(i,j,k), &
                                    bact(1)%juptake_ldop(i,j,k)/bact(1)%q_p_2_n)
       ! remineralization of organic N to nh4 = difference between uptake and production
       ! bact(1)%jprod_n < 0 results in dissolved organic matter production addressed later
       bact(1)%jprod_nh4(i,j,k) = bact(1)%juptake_ldon(i,j,k) - max(bact(1)%jprod_n(i,j,k),0.0)
       cobalt%jprod_nh4(i,j,k) = cobalt%jprod_nh4(i,j,k) + bact(1)%jprod_nh4(i,j,k)

       if (cobalt%f_o2(i,j,k) .gt. cobalt%o2_min) then  !{
          ! aerobic remineralization, nh4 production, o2 respired
          cobalt%jo2resp_wc(i,j,k) = cobalt%jo2resp_wc(i,j,k) + bact(1)%jprod_nh4(i,j,k)*cobalt%o2_2_nh4
       else
          ! low o2 leads to water column denitrification. nh4 is created, but no o2 is used
          cobalt%jno3denit_wc(i,j,k) = cobalt%jno3denit_wc(i,j,k) + &
                                       bact(1)%jprod_nh4(i,j,k)*cobalt%n_2_n_denit
       endif  !}

       ! produce phosphate
       bact(1)%jprod_po4(i,j,k) = bact(1)%juptake_ldop(i,j,k) - max(bact(1)%jprod_n(i,j,k)*bact(1)%q_p_2_n,0.0)
       cobalt%jprod_po4(i,j,k) = cobalt%jprod_po4(i,j,k) + bact(1)%jprod_po4(i,j,k)
    enddo; enddo ; enddo !} i,j,k
!
    call mpp_clock_end(id_clock_bacteria_growth)
!
!-----------------------------------------------------------------------
! 3: Plankton foodweb dynamics
!-----------------------------------------------------------------------
!
    !
    ! 3.1 Plankton foodweb dynamics: consumption by zooplankton and higher predators
    !
    ! Zooplankton feeding is parameterized with observed allometric (i.e., size-dependent) feeding rates and predator-
    ! prey linkages (i.e., Hansen, B.W. et al., 1994; Hansen, P.J., et al. 1997).  Feeding relationships are based on
    ! simple saturating (Holling Type 2) relationships when there is a single prey type.  A density dependent switching
    ! response, however, is included when multiple prey types are present (Stock et al., 2008).  A small "refuge"
    ! concentration has also been included as an extra safeguard against negative values and as a reflection of the 
    ! paradigm that Baas Becking's hypothesis that "Everything is everywhere, but the environment selects".  Predation
    ! by higher predators (e.g., planktivorous fish) is modeled in a manner analogous to zooplankton, but assuming that
    ! the biomass of these unresolved predators scales in proportion to the available prey.
    !
    ! References: 
    ! Hansen, B.W., Bjornsen, P.K., Hansen, P.J., 1994. The size ratio between planktonic predators and their prey.
    !    Limnol. & Oceanogr. 39, 395–402. https://aslopubs.onlinelibrary.wiley.com/doi/10.4319/lo.1994.39.2.0395
    ! Hansen, P.J., Bjornsen, P.K., Hansen, B.W., 1997. Zooplankton grazing and growth: scaling within the 
    !    2–2000-micron body size range. Limnol. & Oceanogr. 42, 687–704.
    !    https://aslopubs.onlinelibrary.wiley.com/doi/10.4319/lo.1997.42.4.0687 
    ! Stock, C.A., Powell, T.M., and Levin, S.A., 2008. Bottom-up and top-down forcing in a simple size-structured
    !    plankton dynamics model.  Journal of Marine Systems. 74 (1-2), 134-152. 
    !    https://doi.org/10.1016/j.jmarsys.2007.12.004 

    call mpp_clock_begin(id_clock_zooplankton_calculations)

    !
    ! Set-up local matrices for calculating zooplankton ingestion of multiple prey types.  The rows are consumers
    ! (i.e., NUM_ZOO zooplankton) and the columns are food sources (i.e., NUM_PREY potential food sources)
    !
    ! ipa_matrix = the innate prey availability matrix (dimensionless, between 0-1)
    ! pa_matrix = prey availability matrix after accounting for switching (dimensionless, between 0-1)
    ! tot_prey = total prey available to predator m (moles kg-1)
    ! ingest_matrix = NUM_ZOO x NUM_PREY matrix of ingestion (moles kg-1 sec-1)
    !
    ! Note: The definition of predator-prey matrices is intended to allow for efficient experimentation with
    ! predator-prey interconnections.  Initial attempts to include a sweep over all elements of the predator-prey
    ! matrix, however, proved to be computationally costly.  Thus, while matrix structures are included, the
    ! standard COBALTv3 interactions are hard-coded.  This makes the code faster, but adding new consumer-
    ! resource linkages requires new code rather than just changing innate prey availability parameters.
    ! A computationally efficient and flexible scheme will be pursued in future work.
    !
    ! Note: The ipa_matrix must be ordered phytoplankton, bacteria, zooplankton, then detritus.  The order
    ! of the phytoplankton and zooplankton is dictated by the phytoplankton and zooplankton type definitions in
    ! cobalt_types.F90.  In the case of phytoplankton, the code expects diazotrophs to be first.  For legacy
    ! reasons this has also led the phytoplankton to be ordered from large to small and the zooplankton from
    ! small to large.
    !
    do m = 1,NUM_ZOO !{
       ipa_matrix(m,1) = zoo(m)%ipa_diaz
       ipa_matrix(m,2) = zoo(m)%ipa_lgp
       ipa_matrix(m,3) = zoo(m)%ipa_mdp
       ipa_matrix(m,4) = zoo(m)%ipa_smp
       ipa_matrix(m,5) = zoo(m)%ipa_bact
       ipa_matrix(m,6) = zoo(m)%ipa_smz
       ipa_matrix(m,7) = zoo(m)%ipa_mdz
       ipa_matrix(m,8) = zoo(m)%ipa_lgz
       ipa_matrix(m,9) = zoo(m)%ipa_det
       tot_prey(m) = 0.0
       do n = 1,NUM_PREY !{
           ingest_matrix(m,n) = 0.0
       enddo !} n
    enddo !} m

    !
    ! Set-up local matrices for calculating higher predator ingestion of multiple prey types.
    ! Note: Order must be the same as zooplankton
    !

    hp_ipa_vec(1) = cobalt%hp_ipa_diaz
    hp_ipa_vec(2) = cobalt%hp_ipa_lgp
    hp_ipa_vec(3) = cobalt%hp_ipa_mdp
    hp_ipa_vec(4) = cobalt%hp_ipa_smp
    hp_ipa_vec(5) = cobalt%hp_ipa_bact
    hp_ipa_vec(6) = cobalt%hp_ipa_smz
    hp_ipa_vec(7) = cobalt%hp_ipa_mdz
    hp_ipa_vec(8) = cobalt%hp_ipa_lgz
    hp_ipa_vec(9) = cobalt%hp_ipa_det
    tot_prey_hp = 0.0
    do n = 1,NUM_PREY  !{
       hp_ingest_vec(n) = 0.0
    enddo !} n

    !
    ! Set all static stoichiometric ratios outside k,j,i loop
    !

    prey_p2n_vec(5) = bact(1)%q_p_2_n
    prey_p2n_vec(6) = zoo(1)%q_p_2_n
    prey_p2n_vec(7) = zoo(2)%q_p_2_n
    prey_p2n_vec(8) = zoo(3)%q_p_2_n

    prey_fe2n_vec(5) = 0.0
    prey_fe2n_vec(6) = 0.0
    prey_fe2n_vec(7) = 0.0
    prey_fe2n_vec(8) = 0.0

    prey_si2n_vec(1) = 0.0
    prey_si2n_vec(4) = 0.0
    prey_si2n_vec(5) = 0.0
    prey_si2n_vec(6) = 0.0
    prey_si2n_vec(7) = 0.0
    prey_si2n_vec(8) = 0.0

    !
    ! Main loop for calculating predation by zooplankton and higher predators
    !

    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec; !{

       !
       ! 3.1.1: Calculate zooplankton ingestion fluxes
       !

       ! Calculate the temperature and oxygen limitations for zooplankton feeding and growth
       ! Since zooplankton ingestion uses oxygen, there is no zooplankton feeding when f_o2 is less than o2_min.
       do m = 1,3  !{
          zoo(m)%temp_lim(i,j,k) = exp(zoo(m)%ktemp*Temp(i,j,k))
          zoo(m)%o2lim(i,j,k) = max((cobalt%f_o2(i,j,k) - cobalt%o2_min),0.0)/ &
                                (cobalt%k_o2 + max(cobalt%f_o2(i,j,k)-cobalt%o2_min,0.0))
       enddo  !}  m
       cobalt%hp_temp_lim(i,j,k) = exp(cobalt%ktemp_hp*Temp(i,j,k))
       cobalt%hp_o2lim(i,j,k) = max((cobalt%f_o2(i,j,k) - cobalt%o2_min),0.0)/ &
                                (cobalt%k_o2 + max(cobalt%f_o2(i,j,k)-cobalt%o2_min,0.0))

       ! Prey vectors for ingestion and loss calculations
       ! Note: ordering must match that used for the prey availability matrices above 
       prey_vec(1) = max(phyto(DIAZO)%f_n(i,j,k) - cobalt%refuge_conc,0.0)
       prey_vec(2) = max(phyto(LARGE)%f_n(i,j,k) - cobalt%refuge_conc,0.0)
       prey_vec(3) = max(phyto(MEDIUM)%f_n(i,j,k) - cobalt%refuge_conc,0.0)
       prey_vec(4) = max(phyto(SMALL)%f_n(i,j,k) - cobalt%refuge_conc,0.0)
       prey_vec(5) = max(bact(1)%f_n(i,j,k) - cobalt%refuge_conc,0.0)
       prey_vec(6) = max(zoo(1)%f_n(i,j,k) - cobalt%refuge_conc,0.0)
       prey_vec(7) = max(zoo(2)%f_n(i,j,k) - cobalt%refuge_conc,0.0)
       prey_vec(8) = max(zoo(3)%f_n(i,j,k) - cobalt%refuge_conc,0.0)
       prey_vec(9) = max(cobalt%f_ndet(i,j,k) - cobalt%refuge_conc,0.0)

       ! Set dynamic prey stoichiometric ratios inside k,j,i loop
       prey_p2n_vec(1) = phyto(DIAZO)%q_p_2_n(i,j,k)
       prey_p2n_vec(2) = phyto(LARGE)%q_p_2_n(i,j,k)
       prey_p2n_vec(3) = phyto(MEDIUM)%q_p_2_n(i,j,k)
       prey_p2n_vec(4) = phyto(SMALL)%q_p_2_n(i,j,k)
       prey_p2n_vec(9)  = cobalt%f_pdet(i,j,k)/(cobalt%f_ndet(i,j,k)+epsln)
       prey_fe2n_vec(1) = phyto(DIAZO)%q_fe_2_n(i,j,k)
       prey_fe2n_vec(2) = phyto(LARGE)%q_fe_2_n(i,j,k)
       prey_fe2n_vec(3) = phyto(MEDIUM)%q_fe_2_n(i,j,k)
       prey_fe2n_vec(4) = phyto(SMALL)%q_fe_2_n(i,j,k)
       prey_fe2n_vec(9) = cobalt%f_fedet(i,j,k)/(cobalt%f_ndet(i,j,k)+epsln)
       prey_si2n_vec(2) = phyto(LARGE)%q_si_2_n(i,j,k)
       prey_si2n_vec(3) = phyto(MEDIUM)%q_si_2_n(i,j,k)
       prey_si2n_vec(9) = cobalt%f_sidet(i,j,k)/(cobalt%f_ndet(i,j,k)+epsln)

       !
       ! Calculate zooplankton ingestion
       !
       ! Small zooplankton (m = 1) consuming medium phytoplankton (3), small phytoplankton (4) and bacteria (5).
       ! Density-dependent switching occurs between phytoplankton and bacterial prey.
       !
       m = 1
       ! alternative prey items for the switching calculation
       food1 = ipa_matrix(m,3)*prey_vec(3)+ipa_matrix(m,4)*prey_vec(4)
       food2 = ipa_matrix(m,5)*prey_vec(5)
       ! calculate realized prey availability from innate availability and relative abundance of alternative prey
       sw_fac_denom = food1**zoo(m)%nswitch+food2**zoo(m)%nswitch
       pa_matrix(m,3) = ipa_matrix(m,3)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,4) = ipa_matrix(m,4)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,5) = ipa_matrix(m,5)*(food2**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       ! calculate the total prey from the realized prey availability
       tot_prey(m) = pa_matrix(m,3)*prey_vec(3) + pa_matrix(m,4)*prey_vec(4) + &
                     pa_matrix(m,5)*prey_vec(5)
       ! calculate the rate at which small zooplankton ingests each prey type
       ingest_matrix(m,3) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)*zoo(m)%imax* &
                 pa_matrix(m,3)*prey_vec(3)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,4) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)*zoo(m)%imax* &
                 pa_matrix(m,4)*prey_vec(4)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,5) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)*zoo(m)%imax* &
                 pa_matrix(m,5)*prey_vec(5)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ! calculate the total ingestion of each element by small zooplankton 
       zoo(m)%jingest_n(i,j,k) = ingest_matrix(m,3) + ingest_matrix(m,4) + ingest_matrix(m,5)
       zoo(m)%jingest_p(i,j,k) = ingest_matrix(m,3)*prey_p2n_vec(3) + &
                                 ingest_matrix(m,4)*prey_p2n_vec(4) + &
                                 ingest_matrix(m,5)*prey_p2n_vec(5)
       zoo(m)%jingest_fe(i,j,k) = ingest_matrix(m,3)*prey_fe2n_vec(3) + &
                                  ingest_matrix(m,4)*prey_fe2n_vec(4)
       zoo(m)%jingest_sio2(i,j,k) = ingest_matrix(m,3)*prey_si2n_vec(3)

       ! Medium zooplankton (m = 2) consuming diazotrophs (1), large phytoplankton (2), medium phytoplankton (3),
       ! small phytoplankton (4), and small zooplankton (6).  Switching occurs between herbivory (1-4) and carnivory.
       !
       ! Note: The default availability of large phytoplankton to medium zooplankton is 0.  The optimal setting for
       ! this parameter, however, is still being actively investigated.
       !
       m = 2
       ! alternative prey items for the switching calculation (herbivory versus carnivory)
       food1 = ipa_matrix(m,1)*prey_vec(1)+ipa_matrix(m,2)*prey_vec(2)+ &
               ipa_matrix(m,3)*prey_vec(3)+ipa_matrix(m,4)*prey_vec(4)
       food2 = ipa_matrix(m,6)*prey_vec(6)
       ! calculate realized prey availability from innate availability and relative abundance of alternative prey
       sw_fac_denom = food1**zoo(m)%nswitch+food2**zoo(m)%nswitch
       pa_matrix(m,1) = ipa_matrix(m,1)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,2) = ipa_matrix(m,2)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,3) = ipa_matrix(m,3)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,4) = ipa_matrix(m,4)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,6) = ipa_matrix(m,6)*(food2**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       ! calculate the total prey from the realized prey availability
       tot_prey(m) = pa_matrix(m,1)*prey_vec(1) + pa_matrix(m,2)*prey_vec(2) + &
                     pa_matrix(m,3)*prey_vec(3) + pa_matrix(m,4)*prey_vec(4) + &
                     pa_matrix(m,6)*prey_vec(6)
       ! calculate the rate at which medium zooplankton ingests each prey type
       ingest_matrix(m,1) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)*zoo(m)%imax* &
                     pa_matrix(m,1)*prey_vec(1)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,2) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)*zoo(m)%imax* &
                     pa_matrix(m,2)*prey_vec(2)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,3) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)*zoo(m)%imax* &
                     pa_matrix(m,3)*prey_vec(3)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,4) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)*zoo(m)%imax* &
                     pa_matrix(m,4)*prey_vec(4)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,6) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)*zoo(m)%imax* &
                     pa_matrix(m,6)*prey_vec(6)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ! calculate the total ingestion of each element by medium zooplankton
       zoo(m)%jingest_n(i,j,k) = ingest_matrix(m,1) + ingest_matrix(m,2) + &
                                 ingest_matrix(m,3) + ingest_matrix(m,4) + &
                                 ingest_matrix(m,6)
       zoo(m)%jingest_p(i,j,k) = ingest_matrix(m,1)*prey_p2n_vec(1) + &
                                 ingest_matrix(m,2)*prey_p2n_vec(2) + &
                                 ingest_matrix(m,3)*prey_p2n_vec(3) + &
                                 ingest_matrix(m,4)*prey_p2n_vec(4) + &
                                 ingest_matrix(m,6)*prey_p2n_vec(6)
       zoo(m)%jingest_fe(i,j,k) = ingest_matrix(m,1)*prey_fe2n_vec(1) + &
                                  ingest_matrix(m,2)*prey_fe2n_vec(2) + &
                                  ingest_matrix(m,3)*prey_fe2n_vec(3) + &
                                  ingest_matrix(m,4)*prey_fe2n_vec(4)
       zoo(m)%jingest_sio2(i,j,k) = ingest_matrix(m,2)*prey_si2n_vec(2) + &
                                    ingest_matrix(m,3)*prey_si2n_vec(3)

       ! Large zooplankton (m = 3) consuming diazotrophs (1), large phytoplankton (2), medium pytoplankton (3),
       ! and medium zooplankton (7).  Switching occurs between herbibory (1-3) and carnivory (7).
       !
       m = 3
       ! alternative prey items for the switching calculation (herbivory versus carnivory)
       food1 = ipa_matrix(m,1)*prey_vec(1)+ipa_matrix(m,2)*prey_vec(2)+ &
               ipa_matrix(m,3)*prey_vec(3)
       food2 = ipa_matrix(m,7)*prey_vec(7)
       ! calculate realized prey availability from innate availability and relative abundance of alternative prey
       sw_fac_denom = food1**zoo(m)%nswitch+food2**zoo(m)%nswitch
       pa_matrix(m,1) = ipa_matrix(m,1)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,2) = ipa_matrix(m,2)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,3) = ipa_matrix(m,3)*(food1**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       pa_matrix(m,7) = ipa_matrix(m,7)*(food2**zoo(m)%nswitch / &
               (sw_fac_denom+epsln) )**(1.0/zoo(m)%mswitch)
       ! calculate the total prey from the realized prey availability
       tot_prey(m) = pa_matrix(m,1)*prey_vec(1) + pa_matrix(m,2)*prey_vec(2) + &
                     pa_matrix(m,3)*prey_vec(3) + pa_matrix(m,7)*prey_vec(7)
       ! calculate the rate at which large zooplankton ingests each prey type
       ingest_matrix(m,1) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)* &
                     zoo(m)%imax*pa_matrix(m,1)*prey_vec(1)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,2) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)* &
                     zoo(m)%imax*pa_matrix(m,2)*prey_vec(2)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,3) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)* &
                     zoo(m)%imax*pa_matrix(m,3)*prey_vec(3)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ingest_matrix(m,7) = zoo(m)%temp_lim(i,j,k)*zoo(m)%o2lim(i,j,k)* &
                     zoo(m)%imax*pa_matrix(m,7)*prey_vec(7)*zoo(m)%f_n(i,j,k)/(zoo(m)%ki+tot_prey(m))
       ! calculate the total ingestion of each element by large zooplankton
       zoo(m)%jingest_n(i,j,k) = ingest_matrix(m,1) + ingest_matrix(m,2) + &
                                 ingest_matrix(m,3) + ingest_matrix(m,7)
       zoo(m)%jingest_p(i,j,k) = ingest_matrix(m,1)*prey_p2n_vec(1) + &
                                 ingest_matrix(m,2)*prey_p2n_vec(2) + &
                                 ingest_matrix(m,3)*prey_p2n_vec(3) + &
                                 ingest_matrix(m,7)*prey_p2n_vec(7)
       zoo(m)%jingest_fe(i,j,k) = ingest_matrix(m,1)*prey_fe2n_vec(1) + &
                                  ingest_matrix(m,2)*prey_fe2n_vec(2) + &
                                  ingest_matrix(m,3)*prey_fe2n_vec(3)
       zoo(m)%jingest_sio2(i,j,k) = ingest_matrix(m,2)*prey_si2n_vec(2) + &
                                    ingest_matrix(m,3)*prey_si2n_vec(3)

       ! calculate the total filter feeding by medium and large zooplankton.  This rate is ultimately used to
       ! scale the conversion of lithogenic dust into lithogenic detritus. 
       cobalt%total_filter_feeding(i,j,k) = ingest_matrix(2,1) + ingest_matrix(2,2) + &
          ingest_matrix(2,3) + ingest_matrix(2,4) +  ingest_matrix(3,1) + ingest_matrix(3,2) + &
          ingest_matrix(3,3) + ingest_matrix(3,4)
       !
       ! calculate losses of each prey type to zooplankton, starting with phytoplankton
       !
       do n = 1,NUM_PHYTO
          phyto(n)%jzloss_n(i,j,k) = 0.0
       enddo

       do m = 1,NUM_ZOO !{
          phyto(DIAZO)%jzloss_n(i,j,k) = phyto(DIAZO)%jzloss_n(i,j,k) + ingest_matrix(m,DIAZO)
          phyto(LARGE)%jzloss_n(i,j,k) = phyto(LARGE)%jzloss_n(i,j,k) + ingest_matrix(m,LARGE)
          phyto(MEDIUM)%jzloss_n(i,j,k) = phyto(MEDIUM)%jzloss_n(i,j,k) + ingest_matrix(m,MEDIUM)
          phyto(SMALL)%jzloss_n(i,j,k) = phyto(SMALL)%jzloss_n(i,j,k) + ingest_matrix(m,SMALL)
       enddo !} m

       do n = 1,NUM_PHYTO !{
          phyto(n)%jzloss_p(i,j,k) = phyto(n)%jzloss_n(i,j,k)*prey_p2n_vec(n)
          phyto(n)%jzloss_fe(i,j,k) = phyto(n)%jzloss_n(i,j,k)*prey_fe2n_vec(n)
          phyto(n)%jzloss_sio2(i,j,k) = phyto(n)%jzloss_n(i,j,k)*prey_si2n_vec(n)
       enddo !} n
       !
       ! calculate losses of bacteria to zooplankton
       !
       bact(1)%jzloss_n(i,j,k) = 0.0
       do m = 1,NUM_ZOO !{
          bact(1)%jzloss_n(i,j,k) = bact(1)%jzloss_n(i,j,k) + ingest_matrix(m,5)
       enddo !} m
       bact(1)%jzloss_p(i,j,k) = bact(1)%jzloss_n(i,j,k)*prey_p2n_vec(5)
       !
       ! losses of zooplankton to zooplankton
       ! Note: n = loop through zooplankton as prey; m = loop through zooplankton as predators
       !
       do n = 1,NUM_ZOO !{
          zoo(n)%jzloss_n(i,j,k) = 0.0
          do m = 1,NUM_ZOO !{
             zoo(n)%jzloss_n(i,j,k) = zoo(n)%jzloss_n(i,j,k) + ingest_matrix(m,NUM_PHYTO+1+n)
          enddo !} m
          zoo(n)%jzloss_p(i,j,k) = zoo(n)%jzloss_n(i,j,k)*prey_p2n_vec(NUM_PHYTO+1+n)
       enddo !} n

       !
       ! 3.1.2 Calculate ingestion by higher predators
       !

       ! The higher-predator ingestion calculations mirror those used for zooplankton.  Switching occurs between
       ! medium and large zooplankton assuming that forage fish have unique adaptations for these two size classes
       !
       food1 = hp_ipa_vec(7)*prey_vec(7)
       food2 = hp_ipa_vec(8)*prey_vec(8)
       ! calculate realized prey availability from innate availability and relative abundance of alternative prey
       sw_fac_denom = food1**cobalt%nswitch_hp+food2**cobalt%nswitch_hp
       hp_pa_vec(7) = hp_ipa_vec(7)*(food1**cobalt%nswitch_hp / &
               (sw_fac_denom+epsln) )**(1.0/cobalt%mswitch_hp)
       hp_pa_vec(8) = hp_ipa_vec(8)*(food2**cobalt%nswitch_hp / &
               (sw_fac_denom+epsln) )**(1.0/cobalt%mswitch_hp)
       ! calculate the total prey from the realized prey availability
       tot_prey_hp = hp_pa_vec(7)*prey_vec(7) + hp_pa_vec(8)*prey_vec(8)
       ! calculate the rate at which large zooplankton ingests each prey type.  The default assumption for higher
       ! predators is that the biomass of higher predators scales in proportion to the available prey.  That is,
       ! it is implicitly assumed that fish biomass is proportional to tot_prey_hp.  For example, the ingestion of 
       ! medium zooplankton (mz) by hp is:
       !
       ! hp_ingest_vec(7) = Imax(T,O2)*(available mz biomass)/(ki_hp + tot_prey_hp) * HP; where HP ~ tot_prey_hp
       ! 
       ! Note that this results in a density-dependent (i.e., quadratic) mortality consistent with fish aggregating
       ! over regions of abundant prey.  This response can be modulated with coef_hp, but care would be needed
       ! to ensure imax_hp has proper units if this coefficient were changed.
       hp_ingest_vec(7) = cobalt%hp_temp_lim(i,j,k)*cobalt%hp_o2lim(i,j,k)*cobalt%imax_hp* &
                          hp_pa_vec(7)*prey_vec(7)*tot_prey_hp**(cobalt%coef_hp-1.0)/ &
                            (cobalt%ki_hp+tot_prey_hp)
       hp_ingest_vec(8) = cobalt%hp_temp_lim(i,j,k)*cobalt%hp_o2lim(i,j,k)*cobalt%imax_hp* &
                          hp_pa_vec(8)*prey_vec(8)*tot_prey_hp**(cobalt%coef_hp-1.0)/ &
                            (cobalt%ki_hp+tot_prey_hp)
       cobalt%hp_jingest_n(i,j,k) = hp_ingest_vec(7) + hp_ingest_vec(8)
       cobalt%hp_jingest_p(i,j,k) = hp_ingest_vec(7)*prey_p2n_vec(7) + &
                                    hp_ingest_vec(8)*prey_p2n_vec(8)
       !
       ! Calculate losses of zooplankton to higher predators
       !

       do n = 1,NUM_ZOO !{
         zoo(n)%jhploss_n(i,j,k) = hp_ingest_vec(NUM_PHYTO+1+n)
         zoo(n)%jhploss_p(i,j,k) = zoo(n)%jhploss_n(i,j,k)*prey_p2n_vec(NUM_PHYTO+1+n)
       enddo !} n

    enddo; enddo; enddo  !} i,j,k
    call mpp_clock_end(id_clock_zooplankton_calculations)

    !
    ! 3.2: Plankton foodweb dynamics: mortality and loss terms other than zooplankton and higher predator consumption
    !

    call mpp_clock_begin(id_clock_other_losses)

    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec; !{

       ! 3.2.1 Calculate losses of phytoplankton to aggregation and mortality and the rate of direct sinking.
       !
       ! These losses depend on whether phytoplankton are growing well or "stressed". Stress is quantified as a
       ! factor between 0-1, "phyto(n)%stress_fac", determined by the ratio of growth rate achieved over 24 hours
       ! (i.e., mu_mem) relative to the maximum photosynthetic rate, i.e.:
       !
       ! growth_ratio = min( max(mu_mem,0)/(frac_mu_stress*P_C_max(T)), 1.0)
       !
       ! Note that growth_ratio = 1 when mu_mem >= frac_mu_stress*P_C_max(T) and approaches 0 as mu_mem -> 0.  The
       ! stress factor is then calculated as:
       !
       ! stress_fac = (1-growth_ratio)**2
       !
       ! stress_fac thus equals 0 when mu_mem >= frac_mu_stress*P_C_max(T) and ramps up non-linearly to 1 as mu_mem->0.
       ! Since stress_fac multiplies the loss term, stress_fac=0 shuts the loss off when the cell is "happy", while
       ! stress_fac=1 allows the loss to achieve its full value when the cell is severely stressed.  This 
       ! parameterization is consistent with observed sinking, aggregation, and stress-driven mortality responses
       ! (e.g., Waite et al., 1992; Smayda et al., 1971).
       !
       ! Aggregation is modeled as a density-dependent (quadratic) loss that effects large cells most, does not
       ! depend on temperature, and results in sinking detritus (e.g., Jackson et al., 1992).  Phytoplankton sinking as 
       ! non-aggregates is simulated directly using the "move_vertical" option in generic_tracers, with larger and more
       ! stressed cells sinking more quickly.  Phytoplankton mortality (cell death) is not used in the default settings
       ! but it is set as a linear loss rate that generates dissolved organic material.  Note that this differs from
       ! phytoplankton basal respiration, which is also linear but results in inorganic nutrients and carbon via 
       ! respiration.
       !
       ! REFERENCES
       ! Waite, A., Bienfeng, P.K., Harrison, P.J., 1992. Spring bloom sedimentation in a subarctic ecosystem.
       !    Marine Biology, 114 131-138.  https://doi.org/10.1007/BF00350862
       ! Smayda, T.J., Normal and accelerated sinking of phytoplankton in the sea. Marine Geology, 11(2), 105-122.
       !    https://doi.org/10.1016/0025-3227(71)90070-3
       ! Jackson, G.A., 1990. A model of the formation of marine algal flocs by physical coagulation processes.
       !    Deep Sea Res A, 37(8), 1197-1211. https://doi.org/10.1016/0198-0149(90)90038-W

       do n = 1,NUM_PHYTO !{
            ! calculate the stress factor
            growth_ratio = min(max(phyto(n)%f_mu_mem(i,j,k),0.0)/ &
                           (phyto(n)%frac_mu_stress*phyto(n)%P_C_max(i,j,k)*cobalt%expkT(i,j,k)),1.0)
            phyto(n)%stress_fac(i,j,k) = (1.0-growth_ratio)**2
            ! calculate aggregation losses
            phyto(n)%jaggloss_n(i,j,k) = phyto(n)%stress_fac(i,j,k)*phyto(n)%agg*phyto(n)%f_n(i,j,k)**2.0
            phyto(n)%jaggloss_p(i,j,k) = phyto(n)%jaggloss_n(i,j,k)*phyto(n)%q_p_2_n(i,j,k)
            phyto(n)%jaggloss_fe(i,j,k) = phyto(n)%jaggloss_n(i,j,k)*phyto(n)%q_fe_2_n(i,j,k)
            phyto(n)%jaggloss_sio2(i,j,k) = phyto(n)%jaggloss_n(i,j,k)*phyto(n)%q_si_2_n(i,j,k)
            ! calculate phytoplankton mortality (cell death) (not used in default settings)
            phyto(n)%jmortloss_n(i,j,k) = cobalt%expkT(i,j,k)*phyto(n)%stress_fac(i,j,k)* &
                   phyto(n)%mort*phyto(n)%f_n(i,j,k)* &
                   phyto(n)%f_n(i,j,k)/(cobalt%refuge_conc + phyto(n)%f_n(i,j,k))
            phyto(n)%jmortloss_p(i,j,k) = phyto(n)%jmortloss_n(i,j,k)*phyto(n)%q_p_2_n(i,j,k)
            phyto(n)%jmortloss_fe(i,j,k) = phyto(n)%jmortloss_n(i,j,k)*phyto(n)%q_fe_2_n(i,j,k)
            phyto(n)%jmortloss_sio2(i,j,k) = phyto(n)%jmortloss_n(i,j,k)*phyto(n)%q_si_2_n(i,j,k)
            ! calculate the vertical sinking
            phyto(n)%vmove(i,j,k) = phyto(n)%sink_max*phyto(n)%stress_fac(i,j,k)
       enddo !} n

       ! 3.2.2 Calculate phytoplankton and bacterial losses to viruses
       !
       ! Viral losses are modeled as a density-dependent (quadratic) loss term that impacts bacteria and phytoplankton
       ! regardless of their stress.  Viral losses are more effective loss mechanisms for small phytoplankton (Murray
       ! and Jackson, 1992) and produce dissolved organic material.
       !
       ! Reference: Murray and Jackson (1992). Viral dynamics: a model of the effects of size, shape motion and
       ! abundance of single-celled planktonic organisms and other particles, Mar. Ecol. Prog. Ser., 89, 103-116.
       ! http://www.jstor.org/stable/24831780.

       do n = 1,NUM_PHYTO !{
          phyto(n)%jvirloss_n(i,j,k) = bact(1)%temp_lim(i,j,k)*phyto(n)%vir*phyto(n)%f_n(i,j,k)**2.0
          phyto(n)%jvirloss_p(i,j,k) = phyto(n)%jvirloss_n(i,j,k)*phyto(n)%q_p_2_n(i,j,k)
          phyto(n)%jvirloss_fe(i,j,k) = phyto(n)%jvirloss_n(i,j,k)*phyto(n)%q_fe_2_n(i,j,k)
          phyto(n)%jvirloss_sio2(i,j,k) = phyto(n)%jvirloss_n(i,j,k)*phyto(n)%q_si_2_n(i,j,k)
       enddo !} n

       bact(1)%jvirloss_n(i,j,k) = bact(1)%temp_lim(i,j,k)*bact(1)%vir*bact(1)%f_n(i,j,k)**2.0
       bact(1)%jvirloss_p(i,j,k) = bact(1)%jvirloss_n(i,j,k)*bact(1)%q_p_2_n

       ! 3.2.3 Calculate losses to exudation
       !
       ! Phytoplankton are assumed to lose a constant fraction of nitrogen they fix to dissolved organic nutrients 
       ! (phyto(n)%exu = 0.13 (Baines and Pace, 1991).  The model assumes losses of phosphate and iron occur in
       ! proportion the the loss in N, but Si is assumed to be in the cell structure.
       !
       ! Reference: Baines, S.B., Pace, M.L., 1991.  The production of dissolved organic matter by phytoplankton and
       ! its importance to bacteria: Patterns across marine and freshwater systems. Limnol. and Oceanogr., 36(6),
       ! 1078-1090. https://doi.org/10.4319/lo.1991.36.6.1078 

       n = DIAZO
       phyto(n)%jexuloss_n(i,j,k) = phyto(n)%exu*max(phyto(n)%juptake_no3(i,j,k)+ &
                                    phyto(n)%juptake_nh4(i,j,k)+phyto(n)%juptake_n2(i,j,k),0.0)
       phyto(n)%jexuloss_p(i,j,k) = phyto(n)%exu*max(phyto(n)%juptake_po4(i,j,k),0.0)
       phyto(n)%jexuloss_fe(i,j,k) = phyto(n)%jexuloss_fe(i,j,k) + phyto(n)%exu*max(phyto(n)%juptake_fe(i,j,k),0.0)
       do n = 2,NUM_PHYTO !{
          phyto(n)%jexuloss_n(i,j,k) = phyto(n)%exu*max(phyto(n)%juptake_no3(i,j,k)+phyto(n)%juptake_nh4(i,j,k),0.0)
          phyto(n)%jexuloss_p(i,j,k) = phyto(n)%exu*max(phyto(n)%juptake_po4(i,j,k),0.0)
          phyto(n)%jexuloss_fe(i,j,k) = phyto(n)%jexuloss_fe(i,j,k) + phyto(n)%exu*max(phyto(n)%juptake_fe(i,j,k),0.0)
       enddo

    enddo; enddo; enddo  !} i,j,k

    ! Assume that individually sinking phytoplankton, which sink at slow rates relative to aggregates and fecal
    ! pellets, collect in a nepholoid layer and are available for resuspension if they are exposed to mixing. This
    ! is accomplished by setting the vertical sinking rate in the bottom layer to 0, and is assumed to occur when 
    ! the depth is less than twice the depth of active mixing.  Cells are otherwise assumed to sink into the
    ! benthos and be remineralized along with sinking detritus.

    do j = jsc, jec ; do i = isc, iec   !{
       do n = 1,NUM_PHYTO
         if (cobalt%zt(i,j,nk).le.(2.0*hblt_depth(i,j))) then
           phyto(n)%vmove(i,j,nk) = 0.0
         endif
       enddo
    enddo; enddo !} i,j

    ! set the direct sinking rates for phytoplankton
    call g_tracer_set_values(tracer_list,'ndi','vmove',phyto(DIAZO)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'nsm','vmove',phyto(SMALL)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'nmd','vmove',phyto(MEDIUM)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'nlg','vmove',phyto(LARGE)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'pdi','vmove',phyto(DIAZO)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'psm','vmove',phyto(SMALL)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'pmd','vmove',phyto(MEDIUM)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'plg','vmove',phyto(LARGE)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'fedi','vmove',phyto(DIAZO)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'fesm','vmove',phyto(SMALL)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'femd','vmove',phyto(MEDIUM)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'felg','vmove',phyto(LARGE)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'simd','vmove',phyto(MEDIUM)%vmove,isd,jsd)
    call g_tracer_set_values(tracer_list,'silg','vmove',phyto(LARGE)%vmove,isd,jsd)

    call mpp_clock_end(id_clock_other_losses)

    !
    ! 3.3: Plankton foodweb dynamics: production of different ecosystem constituents resulting from ingestion and other
    !      loss processes. Products include detritus, dissolved organic matter, new zooplankton and inorganic nutrients
    !

    call mpp_clock_begin(id_clock_production_loop)
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{

       ! 3.3.1: Production of detritus and dissolved organic matter
       ! 
       ! The production of detritus and dissolved organic material is controlled by phi_det, phi_ldon, phi_sldon and
       ! phi_srdon (and corresponding values for P).  These are generally specified as fractions of the ingested   
       ! material, or fractions of the loss term. 
       !
       ! Note: For zooplankton ingestion, the "assimilation efficiency" is determined by 1.0 - the egested fraction.
       ! This is assumed to be 0.7 by default. Thus, for zoo phi_det + phi_ldon + phi_sldon + phi_srdon = 0.3.  If 
       ! this sum increases, you have effectively decreased the assimilation efficiency and vice-versa.

       do m = 1,NUM_ZOO
           ! calculate detritus and dissolved organic production for each zooplankton group
           zoo(m)%jprod_ndet(i,j,k) = zoo(m)%phi_det*zoo(m)%jingest_n(i,j,k)
           zoo(m)%jprod_pdet(i,j,k) = zoo(m)%phi_det*zoo(m)%jingest_p(i,j,k)
           zoo(m)%jprod_sldon(i,j,k) = zoo(m)%phi_sldon*zoo(m)%jingest_n(i,j,k)
           zoo(m)%jprod_ldon(i,j,k) = zoo(m)%phi_ldon*zoo(m)%jingest_n(i,j,k)
           zoo(m)%jprod_srdon(i,j,k) = zoo(m)%phi_srdon*zoo(m)%jingest_n(i,j,k)
           zoo(m)%jprod_sldop(i,j,k) = zoo(m)%phi_sldop*zoo(m)%jingest_p(i,j,k)
           zoo(m)%jprod_ldop(i,j,k) = zoo(m)%phi_ldop*zoo(m)%jingest_p(i,j,k)
           zoo(m)%jprod_srdop(i,j,k) = zoo(m)%phi_srdop*zoo(m)%jingest_p(i,j,k)
           zoo(m)%jprod_fedet(i,j,k) = zoo(m)%phi_det*zoo(m)%jingest_fe(i,j,k)
           zoo(m)%jprod_sidet(i,j,k) = zoo(m)%phi_det*zoo(m)%jingest_sio2(i,j,k)

           ! augment cumulative production variables for detritus and dissolved organics
           cobalt%jprod_ndet(i,j,k) = cobalt%jprod_ndet(i,j,k) + zoo(m)%jprod_ndet(i,j,k)
           cobalt%jprod_pdet(i,j,k) = cobalt%jprod_pdet(i,j,k) + zoo(m)%jprod_pdet(i,j,k)
           cobalt%jprod_sldon(i,j,k) = cobalt%jprod_sldon(i,j,k) + zoo(m)%jprod_sldon(i,j,k)
           cobalt%jprod_ldon(i,j,k) = cobalt%jprod_ldon(i,j,k) + zoo(m)%jprod_ldon(i,j,k)
           cobalt%jprod_srdon(i,j,k) = cobalt%jprod_srdon(i,j,k) + zoo(m)%jprod_srdon(i,j,k)
           cobalt%jprod_sldop(i,j,k) = cobalt%jprod_sldop(i,j,k) + zoo(m)%jprod_sldop(i,j,k)
           cobalt%jprod_ldop(i,j,k) = cobalt%jprod_ldop(i,j,k) + zoo(m)%jprod_ldop(i,j,k)
           cobalt%jprod_srdop(i,j,k) = cobalt%jprod_srdop(i,j,k) + zoo(m)%jprod_srdop(i,j,k)
           cobalt%jprod_fedet(i,j,k) = cobalt%jprod_fedet(i,j,k) + zoo(m)%jprod_fedet(i,j,k)
           cobalt%jprod_sidet(i,j,k) = cobalt%jprod_sidet(i,j,k) + zoo(m)%jprod_sidet(i,j,k)
       enddo !} m

       ! Production of detritus and dissolved organic material from higher predator egestion
       ! (just added to cumulative total. It is easy to calculate from phi_det and hp_jingest)
       cobalt%jprod_ndet(i,j,k) = cobalt%jprod_ndet(i,j,k) + cobalt%hp_phi_det*cobalt%hp_jingest_n(i,j,k)
       cobalt%jprod_pdet(i,j,k) = cobalt%jprod_pdet(i,j,k) + cobalt%hp_phi_det*cobalt%hp_jingest_p(i,j,k)
       cobalt%jprod_fedet(i,j,k) = cobalt%jprod_fedet(i,j,k) + cobalt%hp_phi_det*cobalt%hp_jingest_fe(i,j,k)
       cobalt%jprod_sidet(i,j,k) = cobalt%jprod_sidet(i,j,k) + cobalt%hp_phi_det*cobalt%hp_jingest_sio2(i,j,k)

       ! Detritus produced via phytoplankton aggregation
       do m = 1,NUM_PHYTO
           cobalt%jprod_ndet(i,j,k) = cobalt%jprod_ndet(i,j,k) + phyto(m)%jaggloss_n(i,j,k)
           cobalt%jprod_pdet(i,j,k) = cobalt%jprod_pdet(i,j,k) + phyto(m)%jaggloss_p(i,j,k)
           cobalt%jprod_fedet(i,j,k) = cobalt%jprod_fedet(i,j,k) + phyto(m)%jaggloss_fe(i,j,k)
           cobalt%jprod_sidet(i,j,k) = cobalt%jprod_sidet(i,j,k) + phyto(m)%jaggloss_sio2(i,j,k)
       enddo !} m

       ! Dissolved organic and inorganic production from viral lysis, exudation, and phytoplankton mortality
       ! All of the exuded organic material is assumed to be labile. The partitioning of losses to viruses and
       ! phytoplankton mortality is determined by lysis_phi_ldon, lysis_phi_sldon and lysis_phi_srdon.
       do m = 1,NUM_PHYTO
           cobalt%jprod_ldon(i,j,k) = cobalt%jprod_ldon(i,j,k) + cobalt%lysis_phi_ldon* &
                   (phyto(m)%jvirloss_n(i,j,k) + phyto(m)%jmortloss_n(i,j,k)) + phyto(m)%jexuloss_n(i,j,k)
           cobalt%jprod_sldon(i,j,k) = cobalt%jprod_sldon(i,j,k) + cobalt%lysis_phi_sldon* &
                   (phyto(m)%jvirloss_n(i,j,k) + phyto(m)%jmortloss_n(i,j,k))
           cobalt%jprod_srdon(i,j,k) = cobalt%jprod_srdon(i,j,k) + cobalt%lysis_phi_srdon* &
                   (phyto(m)%jvirloss_n(i,j,k) + phyto(m)%jmortloss_n(i,j,k))
           cobalt%jprod_ldop(i,j,k) = cobalt%jprod_ldop(i,j,k) + cobalt%lysis_phi_ldop* &
                   (phyto(m)%jvirloss_p(i,j,k) + phyto(m)%jmortloss_p(i,j,k)) + phyto(m)%jexuloss_p(i,j,k)
           cobalt%jprod_sldop(i,j,k) = cobalt%jprod_sldop(i,j,k) + cobalt%lysis_phi_sldop* &
                   (phyto(m)%jvirloss_p(i,j,k) + phyto(m)%jmortloss_p(i,j,k))
           cobalt%jprod_srdop(i,j,k) = cobalt%jprod_srdop(i,j,k) + cobalt%lysis_phi_srdop* &
                   (phyto(m)%jvirloss_p(i,j,k) + phyto(m)%jmortloss_p(i,j,k))
           cobalt%jprod_fed(i,j,k) = cobalt%jprod_fed(i,j,k)   + phyto(m)%jvirloss_fe(i,j,k) + &
                                       phyto(m)%jmortloss_fe(i,j,k) + phyto(m)%jexuloss_fe(i,j,k)
           cobalt%jprod_sio4(i,j,k) = cobalt%jprod_sio4(i,j,k) + phyto(m)%jvirloss_sio2(i,j,k) + &
                                      phyto(m)%jmortloss_sio2(i,j,k)
       enddo !} m

       ! Sources of dissolved organic material from viral lysis due to bacteria
       cobalt%jprod_ldon(i,j,k) = cobalt%jprod_ldon(i,j,k) + cobalt%lysis_phi_ldon*bact(1)%jvirloss_n(i,j,k)
       cobalt%jprod_sldon(i,j,k) = cobalt%jprod_sldon(i,j,k) + cobalt%lysis_phi_sldon*bact(1)%jvirloss_n(i,j,k)
       cobalt%jprod_srdon(i,j,k) = cobalt%jprod_srdon(i,j,k) + cobalt%lysis_phi_srdon*bact(1)%jvirloss_n(i,j,k)
       cobalt%jprod_ldop(i,j,k) = cobalt%jprod_ldop(i,j,k) + cobalt%lysis_phi_ldop*bact(1)%jvirloss_p(i,j,k)
       cobalt%jprod_sldop(i,j,k) = cobalt%jprod_sldop(i,j,k) + cobalt%lysis_phi_sldop*bact(1)%jvirloss_p(i,j,k)
       cobalt%jprod_srdop(i,j,k) = cobalt%jprod_srdop(i,j,k) + cobalt%lysis_phi_srdop*bact(1)%jvirloss_p(i,j,k)

       ! When bacterial production is negative, send it to dissolved organic material assuming the partitioning
       ! between different labilities is analogous to viral lysis.
       cobalt%jprod_ldon(i,j,k) = cobalt%jprod_ldon(i,j,k) - cobalt%lysis_phi_ldon*min(bact(1)%jprod_n(i,j,k),0.0)
       cobalt%jprod_sldon(i,j,k) = cobalt%jprod_sldon(i,j,k) - cobalt%lysis_phi_sldon*min(bact(1)%jprod_n(i,j,k),0.0)
       cobalt%jprod_srdon(i,j,k) = cobalt%jprod_srdon(i,j,k) - cobalt%lysis_phi_srdon*min(bact(1)%jprod_n(i,j,k),0.0)
       cobalt%jprod_ldop(i,j,k) = cobalt%jprod_ldop(i,j,k) - cobalt%lysis_phi_ldop* &
                                  min(bact(1)%jprod_n(i,j,k)*bact(1)%q_p_2_n,0.0)
       cobalt%jprod_sldop(i,j,k) = cobalt%jprod_sldop(i,j,k) - cobalt%lysis_phi_sldop* &
                                  min(bact(1)%jprod_n(i,j,k)*bact(1)%q_p_2_n,0.0)
       cobalt%jprod_srdop(i,j,k) = cobalt%jprod_srdop(i,j,k) - cobalt%lysis_phi_srdop* &
                                  min(bact(1)%jprod_n(i,j,k)*bact(1)%q_p_2_n,0.0)


       ! 3.3.2: Zooplankton production and excretion calculations
       !
       ! Zooplankton growth and respiration/excretion depends on two fundamental metabolic parameters:
       !
       ! 1. The assimilation efficiency is the fraction of ingested food that is assimilated for either anabolic
       !    (i.e., growth) or catabolic (i.e., respiration) reactions.  It is equal to 1 - the egested fraction
       !    and has a default value of 0.7.
       ! 2. The gross growth efficiency is the fraction of ingested food that contributes to growth (anabolic)
       !    metabolism.
       !
       ! Zooplankton production is determined by multiplying the ingestion rate by the maximum growth efficiency
       ! (i.e., gge_max) and then subtracting off the basal respiration rate.  By default, gge_max = 0.4 (Straile
       ! et al., 1997, Hansen et al., 1997).  Thus, when ingestion >> basal respiration, gge -> 0.4, the fraction of
       ! ingestion respired -> 0.7-0.4 = 0.3, and the fraction egested as either detritus or dissolved organic matter
       ! = 0.3.  When ingestion = basal respiration, gge -> 0, the fraction of ingestion respired -> 0.7 and the
       ! fraction egested remains 0.3.  When production is negative, respire all assimilated material and route negative
       ! production to detritus.  Nutrients are excreted in balance with respiration.
       !
       ! References:
       ! Hansen, P.J., Bjornsen, P.K., Hansen, B.W., 1997. Zooplankton grazing and growth: scaling within the 
       !   2–2000-micron body size range. Limnol. & Oceanogr. 42, 687–704.
       !   https://aslopubs.onlinelibrary.wiley.com/doi/10.4319/lo.1997.42.4.0687
       ! Straile, D., 1997. Gross growth efficiencies of protozoan and metazoan zooplankton and their dependence on
       !   food concentration, predator-prey weight ratio, and taxonomic group. Limnol. and Oceanogr. 42, 1375-1385.
       !    https://doi.org/10.4319/lo.1997.42.6.137

       do m = 1,NUM_ZOO
          ! calculate the assimilation efficiency
          assim_eff = 1.0-zoo(m)%phi_det-zoo(m)%phi_ldon-zoo(m)%phi_sldon-zoo(m)%phi_srdon

          ! calculate production assuming N is limiting
          zoo(m)%jprod_n(i,j,k) = zoo(m)%gge_max*zoo(m)%jingest_n(i,j,k) - &
                         zoo(m)%f_n(i,j,k)/(cobalt%refuge_conc + zoo(m)%f_n(i,j,k))* &
                         zoo(m)%temp_lim(i,j,k)*zoo(m)%bresp*zoo(m)%f_n(i,j,k)
          ! Adjust downward if there is insufficient phosphorus to support N-based zooplankton growth.  This assumes
          ! that the zooplankter can used its full allotment of assimilated P
          zoo(m)%jprod_n(i,j,k) = min(zoo(m)%jprod_n(i,j,k), &
                                      assim_eff*zoo(m)%jingest_p(i,j,k)/zoo(m)%q_p_2_n)

          ! Ingested material that does not go to zooplankton production or egestion (i.e., detrital production or
          ! production of dissolved organic material) is excreted as nh4 or po4 as part of the respiration process.
          if (zoo(m)%jprod_n(i,j,k) .gt. 0.0) then
             zoo(m)%jprod_nh4(i,j,k) =  zoo(m)%jingest_n(i,j,k) - zoo(m)%jprod_ndet(i,j,k) -  &
                                        zoo(m)%jprod_n(i,j,k) - zoo(m)%jprod_ldon(i,j,k) - &
                                        zoo(m)%jprod_sldon(i,j,k) - zoo(m)%jprod_srdon(i,j,k)
             zoo(m)%jprod_po4(i,j,k) =  zoo(m)%jingest_p(i,j,k) - zoo(m)%jprod_pdet(i,j,k) - &
                                        zoo(m)%jprod_n(i,j,k)*zoo(m)%q_p_2_n - zoo(m)%jprod_ldop(i,j,k) -  &
                                        zoo(m)%jprod_sldop(i,j,k) - zoo(m)%jprod_srdop(i,j,k)
          ! If production is negative, respire all assimilated material and route negative production to large detritus
          else
             zoo(m)%jprod_nh4(i,j,k) =  zoo(m)%jingest_n(i,j,k) - zoo(m)%jprod_ndet(i,j,k) - &
                                        zoo(m)%jprod_ldon(i,j,k) - zoo(m)%jprod_sldon(i,j,k) - &
                                        zoo(m)%jprod_srdon(i,j,k)
             zoo(m)%jprod_po4(i,j,k) =  zoo(m)%jingest_p(i,j,k) - zoo(m)%jprod_pdet(i,j,k) - &
                                        zoo(m)%jprod_ldop(i,j,k) - zoo(m)%jprod_sldop(i,j,k) - &
                                        zoo(m)%jprod_srdop(i,j,k)
             zoo(m)%jprod_ndet(i,j,k) = zoo(m)%jprod_ndet(i,j,k) - zoo(m)%jprod_n(i,j,k)
             zoo(m)%jprod_pdet(i,j,k) = zoo(m)%jprod_pdet(i,j,k) - zoo(m)%jprod_n(i,j,k)*zoo(m)%q_p_2_n
             cobalt%jprod_ndet(i,j,k) = cobalt%jprod_ndet(i,j,k) - zoo(m)%jprod_n(i,j,k)
             cobalt%jprod_pdet(i,j,k) = cobalt%jprod_pdet(i,j,k) - zoo(m)%jprod_n(i,j,k)*zoo(m)%q_p_2_n
          endif

          ! Add respiration-associated excretion to the cumulative production of inorganic nutrients
          cobalt%jprod_nh4(i,j,k) = cobalt%jprod_nh4(i,j,k) + zoo(m)%jprod_nh4(i,j,k)
          cobalt%jprod_po4(i,j,k) = cobalt%jprod_po4(i,j,k) + zoo(m)%jprod_po4(i,j,k)
          ! Zooplankton respiration uses oxygen 
          cobalt%jo2resp_wc(i,j,k) = cobalt%jo2resp_wc(i,j,k) + zoo(m)%jprod_nh4(i,j,k)*cobalt%o2_2_nh4

          ! Any ingested iron that is not allocated to detritus is routed back to the dissolved pool
          zoo(m)%jprod_fed(i,j,k) = (1.0 - zoo(m)%phi_det)*zoo(m)%jingest_fe(i,j,k)
          cobalt%jprod_fed(i,j,k) = cobalt%jprod_fed(i,j,k) + zoo(m)%jprod_fed(i,j,k)

          ! Ingested opal not allocated to detritus undergoes rapid dissolution to dissolved silica
          zoo(m)%jprod_sio4(i,j,k) = (1.0 - zoo(m)%phi_det)*zoo(m)%jingest_sio2(i,j,k)
          cobalt%jprod_sio4(i,j,k) = cobalt%jprod_sio4(i,j,k) + zoo(m)%jprod_sio4(i,j,k)
       enddo !} m

       ! Food ingested by higher predators that is not egested to detritus is excreted
       cobalt%jprod_fed(i,j,k) = cobalt%jprod_fed(i,j,k) + (1.0-cobalt%hp_phi_det)*cobalt%hp_jingest_fe(i,j,k)
       cobalt%jprod_sio4(i,j,k) = cobalt%jprod_sio4(i,j,k) + (1.0-cobalt%hp_phi_det)*cobalt%hp_jingest_sio2(i,j,k)
       cobalt%jprod_nh4(i,j,k) = cobalt%jprod_nh4(i,j,k) + (1.0-cobalt%hp_phi_det)*cobalt%hp_jingest_n(i,j,k)
       cobalt%jprod_po4(i,j,k) = cobalt%jprod_po4(i,j,k) + (1.0-cobalt%hp_phi_det)*cobalt%hp_jingest_p(i,j,k)
       cobalt%jo2resp_wc(i,j,k) = cobalt%jo2resp_wc(i,j,k) + (1.0-cobalt%hp_phi_det)*cobalt%hp_jingest_n(i,j,k)* &
                                  cobalt%o2_2_nh4

    enddo; enddo ; enddo !} i,j,k
    call mpp_clock_end(id_clock_production_loop)

    call mpp_clock_begin(id_clock_ballast_loops)
!
!------------------------------------------------------------------------------------
! 4: Mineral ballasting/dissolution and detrital remineralization  
!------------------------------------------------------------------------------------
    !
    ! 4.1: Determine the aragonite and calcite saturation states and the production of calcite and aragonite detritus
    !
    ! Calculate the aragonite and calcite saturation states
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
      cobalt%co3_sol_arag(i,j,k) = cobalt%f_co3_ion(i,j,k) / max(cobalt%omega_arag(i,j,k),epsln)
      cobalt%co3_sol_calc(i,j,k) = cobalt%f_co3_ion(i,j,k) / max(cobalt%omega_calc(i,j,k),epsln)
    enddo; enddo ; enddo !} i,j,k

    ! Calculate the rate of aragonite and calcite detritus production
    ! The production of calcite and aragonite detritus is assumed to be proportional to the saturation state with
    ! respect to calcite and aragonite and rates associated with the production of detritus from organisms that form
    ! calcite or aragonite shells.  The overall scalings are controlled by the parameters ca_2_n_arag and ca_2_n_calc.
    ! The saturation state dependence is capped with the parameter caco3_sat_max.   
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
        ! Pteropods are assumed to be the primary aragonite shell formers.  Pteropods fall into the medium and large
        ! zooplankton groups within COBALT.  Production of aragonite detritus is thus linked to the consumption of
        ! medium and large zooplankton by zooplankton and higher predators, and the proportion of the material consumed
        ! that ends up as detritus (i.e., phi_det).
        cobalt%jprod_cadet_arag(i,j,k) = (zoo(2)%jzloss_n(i,j,k)*zoo(3)%phi_det + &
                       (zoo(2)%jhploss_n(i,j,k) + zoo(3)%jhploss_n(i,j,k))*cobalt%hp_phi_det)* &
                       cobalt%ca_2_n_arag*min(cobalt%caco3_sat_max, max(0.0,cobalt%omega_arag(i,j,k) - 1.0)) + epsln
        ! Forams and coccolithophores are assumed to be the primary calcite shell formers.  Forams fall into the small
        ! zooplankton group and coccolithophores fall into the small and medium phytoplankton groups. Production of
        ! calcite detritus is thus linked to a) the consumption of these groups by zooplankton and the proportion of the
        ! material consumed that ends up as detritus, and b) the aggregation of small and medium phytoplankton groups. 
        ! The fractional detritus production by the primary zooplankton predator for each group was used for a).
        cobalt%jprod_cadet_calc(i,j,k) = (zoo(1)%jzloss_n(i,j,k)*zoo(2)%phi_det + &
                       phyto(SMALL)%jzloss_n(i,j,k)*zoo(1)%phi_det + phyto(MEDIUM)%jzloss_n(i,j,k)*zoo(2)%phi_det + &
                       phyto(SMALL)%jaggloss_n(i,j,k) + phyto(MEDIUM)%jaggloss_n(i,j,k))*cobalt%ca_2_n_calc* &
                       min(cobalt%caco3_sat_max, max(0.0, cobalt%omega_calc(i,j,k) - 1.0)) + epsln
    enddo; enddo ; enddo !} i,j,k

    !
    ! 4.2: Lithogenic detritus production
    !
    ! Lithogenic minerals (f_lith) are assumed to be incorporated into detritus by filter feeding copepods.  The 
    ! creation rate is assumed to be proportional to the total filter feeding (moles N kg-1 sec-1) divided by the total
    ! phytoplankton biomass being fed upon (moles N kg-1), plus a background rate (k_lith).  The proportionality
    ! constant is phi_lith.  Large phytoplankton and diazotrophs are assumed to be solely consumed by filter feeding
    ! copepods.  The proportion of medium and small phytoplankton subject to filter feeding is assumed proportional to
    ! the relative prey availability of small and medium phytoplankton to copepods versus small zooplankton.
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%jprod_lithdet(i,j,k)=( cobalt%total_filter_feeding(i,j,k)/ & 
                                   ( phyto(LARGE)%f_n(i,j,k) + phyto(DIAZO)%f_n(i,j,k) + &
                                     0.8*phyto(MEDIUM)%f_n(i,j,k) + 0.3*phyto(SMALL)%f_n(i,j,k) + epsln) * &
                                    cobalt%phi_lith + cobalt%k_lith ) * cobalt%f_lith(i,j,k)
    enddo; enddo ; enddo !} i,j,k

    !
    ! 4.3: Dissolution of aragonite, calcite and silica detritus
    !
    ! Calcite and aragonite detritus are assumed to dissolve with at a rate proportional to subsaturation, with
    ! the maximum dissolution rates set gamma_cadet_arag and gamma_cadet_calc, respectively.  The dissolution of
    ! silica detritus is assumed to be temperature dependent.  Relevant references for all parameters can be found in
    ! the COBALTv2 documentation paper: https://doi.org/10.1029/2019MS002043.
    !
    ! Note: Dissolution of aragonite and calcite detritus has been observed under supersaturating conditions. This
    ! process will be added in a future COBALT update. 
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       cobalt%jdiss_cadet_arag(i,j,k) = cobalt%gamma_cadet_arag * &
         max(0.0, 1.0 - cobalt%omega_arag(i,j,k)) * cobalt%f_cadet_arag(i,j,k)
       cobalt%jdiss_cadet_calc(i,j,k) = cobalt%gamma_cadet_calc * &
         max(0.0, 1.0 - cobalt%omega_calc(i,j,k)) * cobalt%f_cadet_calc(i,j,k)
       cobalt%jdiss_sidet(i,j,k) = cobalt%gamma_sidet * exp(cobalt%kappa_sidet * &
          Temp(i,j,k)) * cobalt%f_sidet(i,j,k)
       cobalt%jprod_sio4(i,j,k) = cobalt%jprod_sio4(i,j,k) + cobalt%jdiss_sidet(i,j,k)

       ! Allow for the dissolution of silica associated with free sinking phytoplankton.  The rate of dissolution is
       ! assumed to approach the detrital value as the phytoplankton stress approaches 1.  This is handled by reducing
       ! silica uptake, raising the possibility of negative silica uptake (i.e., net silica loss) when stressed.
       phyto(MEDIUM)%juptake_sio4(i,j,k) = phyto(MEDIUM)%juptake_sio4(i,j,k) - &
          phyto(MEDIUM)%stress_fac(i,j,k)*cobalt%gamma_sidet*exp(cobalt%kappa_sidet*Temp(i,j,k))* &
          cobalt%f_simd(i,j,k)
       phyto(LARGE)%juptake_sio4(i,j,k) = phyto(LARGE)%juptake_sio4(i,j,k) - &
          phyto(LARGE)%stress_fac(i,j,k)*cobalt%gamma_sidet*exp(cobalt%kappa_sidet*Temp(i,j,k))* &
          cobalt%f_silg(i,j,k)
    enddo; enddo ; enddo !} i,j,k

    !
    ! 4.4: Remineralization of nitrogen, phosphorous and iron detritus
    !
    ! Remineralization is handled following Laufkotter et al. (2017), which combines a "mineral protection/ballasting" 
    ! scheme (Armstrong et al., 2001; and Klaas and Archer 2002) with temperature and oxygen dependence calibrated to
    ! a global database of sediment trap profiles.  As described in Laufkotter, remineralization under aerobic
    ! conditions was ramped up over a depth scale of 50m. This prevents excessive recycling in warm surface waters and 
    ! is attributed to the colonization of the particles as they traverse the euphotic zone  
    ! 
    ! In the mineral protection scheme, only the portion of organic material left unprotected by biogenic or lithogenic
    ! minerals is available to be remineralized.  The "unprotected" organic fraction is calculated as:
    !
    ! max( 0.0,f_ndet - rpcaco3*(cadet_arag+cadet_calc) - rplith*lithdet - rpsio2*sidet)
    !
    ! Where rpcaco3, rplith and rpsio2 are protection factors associated with each mineral (Dunne et al., 2005).
    !
    ! As was the case for free-living bacteria, the remineralization rate for sinking detritus under anaerobic
    ! conditions is scaled by o2_min/(k_o2+o2_min).  All anaerobic remineralization is assumed to occur via
    ! denitrification, so a scaling
    !
    ! References:
    ! Laufkotter et al., 2017: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017GB005643
    ! Armstrong, 2002: https://doi.org/10.1016/S0967-0645(01)00101-1
    ! Klaas and Archer, 2002: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2001GB001765
    ! Dunne et al., 2005: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2004GB002390    
    !
    do k=1,nk ; do j=jsc,jec ; do i=isc,iec  !{
       cobalt%expkreminT(i,j,k) = exp(cobalt%kappa_remin * Temp(i,j,k))
       ! Calculate remineralization under aerobic remineralization
       if (cobalt%f_o2(i,j,k) .gt. cobalt%o2_min) then  !{
          cobalt%jremin_ndet(i,j,k) = cobalt%gamma_ndet * cobalt%expkreminT(i,j,k) * &
               zbot(i,j,k)/(zbot(i,j,k) + cobalt%remin_ramp_scale) * cobalt%f_o2(i,j,k) / &
               ( cobalt%k_o2 + cobalt%f_o2(i,j,k) )*max( 0.0, cobalt%f_ndet(i,j,k) - &
               cobalt%rpcaco3*(cobalt%f_cadet_arag(i,j,k) + cobalt%f_cadet_calc(i,j,k)) - &
               cobalt%rplith*cobalt%f_lithdet(i,j,k) - cobalt%rpsio2*cobalt%f_sidet(i,j,k) )
          ! Augment total nh4 production and o2 consumption
          cobalt%jprod_nh4(i,j,k) = cobalt%jprod_nh4(i,j,k) + cobalt%jremin_ndet(i,j,k)
          cobalt%jo2resp_wc(i,j,k) = cobalt%jo2resp_wc(i,j,k) + cobalt%jremin_ndet(i,j,k)*cobalt%o2_2_nh4

       ! Calculate remineralization under anaerobic conditions
       else !}{
          cobalt%jremin_ndet(i,j,k) = cobalt%gamma_ndet * cobalt%o2_min / &
               (cobalt%k_o2 + cobalt%o2_min)* &
               cobalt%f_no3(i,j,k) / (cobalt%k_no3_denit + cobalt%f_no3(i,j,k))* &
               max(0.0, cobalt%f_ndet(i,j,k) - &
               cobalt%rpcaco3*(cobalt%f_cadet_arag(i,j,k) + cobalt%f_cadet_calc(i,j,k)) - &
               cobalt%rplith*cobalt%f_lithdet(i,j,k) - cobalt%rpsio2*cobalt%f_sidet(i,j,k) )
          ! Augment total nh4 production and no3 consumption
          cobalt%jno3denit_wc(i,j,k) = cobalt%jno3denit_wc(i,j,k) + cobalt%jremin_ndet(i,j,k) * cobalt%n_2_n_denit
          cobalt%jprod_nh4(i,j,k) = cobalt%jprod_nh4(i,j,k) + cobalt%jremin_ndet(i,j,k)
       endif !}

       ! P is assumed to be remineralized in direct proportion to N, resulting in PO4 release
       cobalt%jremin_pdet(i,j,k) = cobalt%jremin_ndet(i,j,k)/(cobalt%f_ndet(i,j,k) + epsln)*cobalt%f_pdet(i,j,k)
       cobalt%jprod_po4(i,j,k) = cobalt%jprod_po4(i,j,k) + cobalt%jremin_pdet(i,j,k)

       ! Fe is assumed to be remineralized in proportion to N, but the proportionality is dictated by a
       ! remineralization efficiency (remin_eff_fedet) which has been coarsely tuned to the ferrocline depth.
       ! In addition, it was noted in COBALTv2 (see Stock et al., 2020) that the proportionality between organic matter
       ! and iron remineralization can lead to iron minima in low oxygen zones where organic remineralization is low.
       ! Since low O2 is actually conducive to solubilizing iron, O2 inhibition of iron remineralization was removed.
       cobalt%jremin_fedet(i,j,k) = cobalt%jremin_ndet(i,j,k)* &
         (cobalt%k_o2 + max(cobalt%f_o2(i,j,k),cobalt%o2_min))/max(cobalt%f_o2(i,j,k),cobalt%o2_min) / &
         (cobalt%f_ndet(i,j,k) + epsln) * cobalt%remin_eff_fedet*cobalt%f_fedet(i,j,k)
       cobalt%jprod_fed(i,j,k) = cobalt%jprod_fed(i,j,k) + cobalt%jremin_fedet(i,j,k)
    enddo; enddo; enddo  !} i,j,k

    ! 
    ! 4.5: Iron scavenging onto detritus
    !  
    ! COBALT uses a single ligand complexation model for iron scavenging onto detritus (e.g., Archer and Johnson, 2000).
    ! The binding strength of the ligand, however, is modulated between weak high-light (kfe_eq_hl) and strong low-
    ! light limits (kfelig_ll) to mimic the weakening effect that oxygen free radicals have on iron binding in well-lit
    ! waters (Fan, 2008).  The weakest binding is at light levels greater than io_fescav = 10 watts m-2.  Values decline 
    ! to the strongest low-light limit at 0.01 watts m-2.
    !
    ! The ligand concentration includes a background concentration (felig_bkg) and an additional amount proportional
    ! to dissolved organic matter (felig_2_don).  When the free iron (feprime) exceeds solubility limits defined as
    ! a function of temperature and salinity according to Liu and Millero (2002), scavenging is increased by the factor
    ! fast_fescav_fac to mimic rapid precipitation. For coarse resolution global simulations, fast_fescav_fac was set
    ! set to 10.0.  This high value helped erode coastal iron signals that likely propagated too far into the open
    ! due to under-resolved shelves.  The current default is 2.0, which was able to better maintain iron limitation
    ! patterns in higher-resolution simulations. 
    ! 
    ! The scavenging formulation includes both a linear option (~alpha_fescav*feprime) and an option that depends on
    ! the interaction between free iron and detritus (~beta_fescav*feprime*f_ndet).  The latter was used in COBALTv1,
    ! while the former was used in COBALTv2 and remains the default in COBALTv3.
    !
    ! References:
    ! Archer and Johnson (2000): https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2004GB002390 
    ! Fan et al. (2008): https://www.sciencedirect.com/science/article/pii/S030442030800008X
    ! Liu and Millero (2002): https://www.sciencedirect.com/science/article/pii/S030442030800008X
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       ! Calculate the equilibrium ligand binding strength and a function of light
       cobalt%kfe_eq_lig(i,j,k) = min(cobalt%kfe_eq_lig_ll, 10.0**( log10(cobalt%kfe_eq_lig_hl) + &
          max(0.0,log10(cobalt%io_fescav/max(epsln,cobalt%irr_inst(i,j,k)))) ) )

       ! Calculate the ligand concentration
       cobalt%ligand(i,j,k) = cobalt%felig_bkg + cobalt%felig_2_don*(cobalt%f_sldon(i,j,k) + &
            cobalt%f_srdon(i,j,k) + cobalt%f_ldon(i,j,k))

       ! Solve for the free iron from the following system of three equations:
       !         (1) kfe_eq_lig = [FeL] / ([feprime] * [L])
       !         (2) [Fed] = [Feprime] + [FeL]
       !         (3) [Ltotal] = [L] + [FeL]
       !
       ! The free iron [feprime], the ligand bound iron [FeL], and the ligand without iron [L] are unknown.  The
       ! equilibrium ligand binding strength (kfe_eq_lig), the total iron [Fed] and the total ligand [Ltotal] are
       ! known.  If one i) uses eq. (2) to solve for [FeL] in terms of [Fed] and [Feprime]; ii) substitutes this
       ! relationship into eq. (3) to find an expression for [L] in terms of [Ltotal], [Fed] and [feprime]; iii)
       ! substitutes both the expressions for [FeL] and [L] into eq. (1); then iv) group the terms, it will yield
       ! a quadratic function for [feprime] that can be solved with the quadratic formula:
       ! (-b +- sqrt(b^2 - 4ac))/(2a) where:
       !
       ! a = kfe_eq_lig
       ! b = 1.0 + kfe_eq_lig * ([Ltotal] - [fed])
       ! c = -[Fed]
       !
       ! This solution is simplified by the fact that sqrt(b^2 - 4ac) must be larger than b, so the only positive
       ! root is (-b + sqrt(b^2 - 4ac))/(2a).
       feprime_temp = 1.0 + cobalt%kfe_eq_lig(i,j,k) * (cobalt%ligand(i,j,k) - cobalt%f_fed(i,j,k))
       cobalt%feprime(i,j,k) = (-feprime_temp + (feprime_temp * feprime_temp + 4.0 * cobalt%kfe_eq_lig(i,j,k) * &
            cobalt%f_fed(i,j,k))**(0.5)) / (2.0 * max(epsln,cobalt%kfe_eq_lig(i,j,k)))

       ! Calculate the iron solubility following Liu and Millero (2002).  The quantity "fe_salt" is the ionic strength
       ! These values were derived for Fe(III) at a pH of 8 over a range of salinities and temperatures. 
       fe_salt = 19.922*Salt(i,j,k)/(1000.0 - 1.005*Salt(i,j,k))
       cobalt%fe_sol(i,j,k) = 10**(-10.53 + 322.5/(Temp(i,j,k)+273.15) - 2.524*sqrt(fe_salt) + &
                              2.921*fe_salt)

       ! Calculate the iron adsorption to detrital particles
       if (cobalt%feprime(i,j,k).lt.cobalt%fe_sol(i,j,k)) then
         cobalt%jfe_ads(i,j,k) = cobalt%alpha_fescav*cobalt%feprime(i,j,k) + &
                                 cobalt%beta_fescav*cobalt%feprime(i,j,k)*cobalt%f_ndet(i,j,k)
       else
         cobalt%jfe_ads(i,j,k) = cobalt%fast_fescav_fac*(cobalt%alpha_fescav*cobalt%feprime(i,j,k) + &
                                 cobalt%beta_fescav*cobalt%feprime(i,j,k)*cobalt%f_ndet(i,j,k))
       endif
       ! Add a limiter so you don't scavenge more than half the available iron in a single time step.
       cobalt%jfe_ads(i,j,k) = min(cobalt%jfe_ads(i,j,k),cobalt%f_fed(i,j,k)/(2.0*dt))

    enddo; enddo; enddo  !} i,j,k

!
!-------------------------------------------------------------------------------------------------
! 5: Sediment, coastal and ice dynamics
!-------------------------------------------------------------------------------------------------
!

    ! Nutrient inputs associated with icebergs/frozen runoff.  This is currently entered as a surface flux.  The
    ! parameters "jfe_iceberg_ratio", "jno3_iceberg_ratio" and "jpo4_iceberg_ratio" are the ratios of nutrient input
    ! per kg of runoff.  For iron, values can be set within the broad ranges discussed in Laufkotter et al. (2018).
    ! These inputs are currently entered at the ocean surface, but they have defined within a 3D array to allow
    ! eventual consideration of depth-dependent inputs.
    do j = jsc, jec ; do i = isc, iec !{
       ! CAS: Is this check relevant for MOM6?
       if (grid_kmt(i,j) .gt. 0) then !{
          cobalt%jfe_iceberg(i,j,1) = cobalt%jfe_iceberg_ratio*max(frunoff(i,j),0.0)/rho_dzt(i,j,1)
          cobalt%jno3_iceberg(i,j,1) = cobalt%jno3_iceberg_ratio*max(frunoff(i,j),0.0)/rho_dzt(i,j,1)
          cobalt%jpo4_iceberg(i,j,1) = cobalt%jpo4_iceberg_ratio*max(frunoff(i,j),0.0)/rho_dzt(i,j,1)
       endif !}
    enddo; enddo  !} i,j
    ! CAS: Is the necessary?
    do k = 2, nk ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%jfe_iceberg(i,j,k) = 0.0
       cobalt%jno3_iceberg(i,j,k) = 0.0
       cobalt%jpo4_iceberg(i,j,k) = 0.0 
    enddo; enddo; enddo  !} i,j,k

    ! Calculate the bottom conditions and the fluxes to the bottom for diagnostics and benthic flux calculations.
    ! MOM4/5 used the bottom grid cell, but MOM6 often has a number of vanishingly thin layers overlying the bottom.
    ! Grid scale noise in these layers can occur, particularly for quantitities with large bottom fluxes.  COBALT thus
    ! uses conditions over a specified bottom layer thickness (cobalt%bottom_thickness, default = 1m) for bottom calcs.

    ! Local variables used to determine the layers falling within the bottom thickness
    allocate(rho_dzt_bot(isc:iec,jsc:jec))
    allocate(k_bot(isc:iec,jsc:jec))

    do j = jsc, jec; do i = isc, iec  !{
       if (grid_kmt(i,j) .gt. 0) then !{

          ! Add the phytoplankton fluxes to the detritus fluxes to get total flux to benthos
          cobalt%fntot_btm(i,j) = cobalt%f_ndet_btf(i,j,1) + cobalt%f_ndi_btf(i,j,1) + &
            cobalt%f_nsm_btf(i,j,1) + cobalt%f_nmd_btf(i,j,1) + cobalt%f_nlg_btf(i,j,1)
          cobalt%fptot_btm(i,j) = cobalt%f_pdet_btf(i,j,1) + cobalt%f_pdi_btf(i,j,1) + &
            cobalt%f_psm_btf(i,j,1) + cobalt%f_pmd_btf(i,j,1) + cobalt%f_plg_btf(i,j,1)
          cobalt%ffetot_btm(i,j) = cobalt%f_fedet_btf(i,j,1) + cobalt%f_fedi_btf(i,j,1) + &
            cobalt%f_fesm_btf(i,j,1) + cobalt%f_femd_btf(i,j,1) + cobalt%f_felg_btf(i,j,1)
          cobalt%fsitot_btm(i,j) = cobalt%f_sidet_btf(i,j,1) + cobalt%f_silg_btf(i,j,1) + &
            cobalt%f_simd_btf(i,j,1)

          ! Calculate the values of tracers influencing the sedimentary transformations
          ! and fluxes over a layer defined by "bottom_thickess".
          rho_dzt_bot(i,j) = 0.0
          cobalt%btm_o2(i,j) = 0.0
          cobalt%btm_no3(i,j) = 0.0
          cobalt%btm_co3_sol_calc(i,j) = 0.0
          cobalt%btm_co3_ion(i,j) = 0.0
          cobalt%btm_omega_calc(i,j) = 0.0
          k_bot(i,j) = 0
          ! Note that grid_kmt is always the total number of layers in MOM6
          do k = grid_kmt(i,j),1,-1   !{
            ! Check if the top of layer k is within the bottom thickness.  If so, include its properties in the bottom
            ! layer averages.  Overshoots will be subtracted off later.
            if (rho_dzt_bot(i,j).lt.(cobalt%Rho_0*cobalt%bottom_thickness)) then
              k_bot(i,j) = k
              rho_dzt_bot(i,j) = rho_dzt_bot(i,j) + rho_dzt(i,j,k)
              cobalt%btm_o2(i,j) = cobalt%btm_o2(i,j) + cobalt%f_o2(i,j,k)*rho_dzt(i,j,k) 
              cobalt%btm_no3(i,j) = cobalt%btm_no3(i,j) + cobalt%f_no3(i,j,k)*rho_dzt(i,j,k) 
              cobalt%btm_co3_sol_calc(i,j) = cobalt%btm_co3_sol_calc(i,j) + cobalt%co3_sol_calc(i,j,k)*rho_dzt(i,j,k) 
              cobalt%btm_co3_ion(i,j) = cobalt%btm_co3_ion(i,j) + cobalt%f_co3_ion(i,j,k)*rho_dzt(i,j,k) 
            endif
          enddo
          ! Subtract off overshoot
          drho_dzt = rho_dzt_bot(i,j) - cobalt%Rho_0*cobalt%bottom_thickness
          cobalt%btm_o2(i,j)=cobalt%btm_o2(i,j)-cobalt%f_o2(i,j,k_bot(i,j))*drho_dzt
          cobalt%btm_no3(i,j)=cobalt%btm_no3(i,j)-cobalt%f_no3(i,j,k_bot(i,j))*drho_dzt
          cobalt%btm_co3_sol_calc(i,j)=cobalt%btm_co3_sol_calc(i,j)-cobalt%co3_sol_calc(i,j,k_bot(i,j))*drho_dzt
          cobalt%btm_co3_ion(i,j)=cobalt%btm_co3_ion(i,j)-cobalt%f_co3_ion(i,j,k_bot(i,j))*drho_dzt
          ! convert back to moles kg-1
          cobalt%btm_o2(i,j)=cobalt%btm_o2(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
          cobalt%btm_no3(i,j)=cobalt%btm_no3(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
          cobalt%btm_co3_sol_calc(i,j)=cobalt%btm_co3_sol_calc(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
          cobalt%btm_co3_ion(i,j)=cobalt%btm_co3_ion(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
          ! calculate the saturation state with respect to calcite for subsequent calculations
          cobalt%btm_omega_calc(i,j)=cobalt%btm_co3_ion(i,j)/cobalt%btm_co3_sol_calc(i,j)

          ! Calculate the processing of organic matter in the sediment.  The fate of organic matter is partitioned
          ! between burial (i.e., removal from the system), aerobic remineralization, remineralization via 
          ! denitrification, and remineralization via sulfate reduction.  Note that the latter pathway is effectively
          ! a "catch all" for any other anaerobic pathway and the sulfate cycle is not explicitly modeled.
          k = grid_kmt(i,j)
          if (cobalt%fntot_btm(i,j) .gt. 0.0) then !{

             ! The Burial flux estimates are based on Dunne et al., 2007. A synthesis of global particle export from
             ! the surface ocean and cycling through the ocean interior and on the seafloor.  Global Biogeochemical
             ! Cycles. Vol. 21, GB4006, doi:10.1029/2006GB002907.  See Figure 2, eq. (3).  The default units of this
             ! relationship are mmoles C m-2 day-1, and the local variable "fpoc_btm" is used to create a bottom flux
             ! in these units.
             !
             ! As described in Dunne et al., (2007) this relationship was generally developed for deeper ocean areas
             ! and its validity in shallow areas is unclear.  Past experiments suggest that it may overestimate burial
             ! in shallow areas, resulting in large nutrient losses that are inconsistent with observations.  The 
             ! parameter "z_burial" thus provides a depth scale (an effective "half-saturation") for ramping up burial
             ! from 0 to its full value.
             !
             ! Since burial is highly uncertain and often used in global earth system simulations to balance inputs and
             ! outputs, a dimensionless scaling factor (cobalt%scale_burial) has also been included.
             fpoc_btm = cobalt%fntot_btm(i,j)*cobalt%c_2_n*sperd*1000.0
             ! Should we use ztop?
             cobalt%frac_burial(i,j) = 0.013 + 0.53*fpoc_btm**2.0/((7.0+fpoc_btm)**2.0) * &
                  cobalt%zt(i,j,k) / (cobalt%z_burial + cobalt%zt(i,j,k))
             cobalt%frac_burial(i,j) = cobalt%scale_burial*cobalt%frac_burial(i,j)
             cobalt%fn_burial(i,j) = cobalt%frac_burial(i,j)*cobalt%fntot_btm(i,j)
             cobalt%fp_burial(i,j) = cobalt%frac_burial(i,j)*cobalt%fptot_btm(i,j)

             ! Denitrification follows Middelburg et al., 1996. Denitrification in marine sediments: a modeling study
             ! Global Biogeochemical Cycles 10(4).  pp. 661-673.  https://doi.org/10.1029/96GB02562. COBALT uses the  
             ! carbon flux-based relationship based on Middelburg's first extraction of his metamodel (the first
             ! equation in Section 3.4 of the paper).  This relationship requires a flux to the benthos in micromoles C
             ! cm-2 day-1.  This means that fpoc_btm defined for the burial calculation above must be multiplied by:
             ! 
             ! 1e3 micromoles/millimole*1e-4 cm2/m2 = 0.1
             ! 
             ! to get the proper units.  The Middelburg relationship yields a rate at which arriving particulate organic
             ! carbon is denitrified in micromoles C cm-2 day-1.  This is converted to a rate at which arriving
             ! particulate organic nitrogen denitrified in moles N m-2 sec-1 by dividing by:
             ! 
             ! c_2_n*sperd*1e6 micromoles/mole*1e-4 cm2/m2 = c_2_n*sperd*100
             !
             ! The nitrate demand associated with this denitrification (fno3denit_sed) is obtained by multiplying the
             ! resulting value by the moles of NO3 required to denitrify each mole of organic N (n_2_n_denit).
             !
             ! A number of limiters are applied to support global application.  First, the C flux used in the  
             ! Middelburg relationship is capped at 43.0 micromoles C cm-2 day-1 to avoid anomalous extrapolation.
             ! Second, denitrification is slowed when bottom nitrate is low by a) scaling rates with a nitrate
             ! half-saturation constant with (k_no3_denit), b) preventing the exhaustion of bottom nitrate over
             ! single time step, and c) limiting the total amount of organic carbon denitrified to that arriving at
             ! the sediment minus that which was buried. Finally, to prevent excessive denitrification in very shallow
             ! areas, a depth scale (z_denit) was included to ramp up rates to full Middelburg values only in deeper
             ! waters.
             log10_fpoc_btm = log10(min(43.0,0.1*fpoc_btm))
             cobalt%fno3denit_sed(i,j) = min(cobalt%btm_no3(i,j)*cobalt%bottom_thickness*cobalt%Rho_0*r_dt,  &
                  min((cobalt%fntot_btm(i,j)-cobalt%fn_burial(i,j))*cobalt%n_2_n_denit, &
                  10.0**(-0.9543+0.7662*log10_fpoc_btm - 0.235*log10_fpoc_btm**2.0)/(cobalt%c_2_n*sperd*100.0)* &
                  cobalt%n_2_n_denit*cobalt%btm_no3(i,j)/(cobalt%k_no3_denit + cobalt%btm_no3(i,j)))) * &
                  cobalt%zt(i,j,k) / (cobalt%z_denit + cobalt%zt(i,j,k))

             ! Calculate the organic matter remineralized via sediment aerobic processes (fnoxic_sed).  This generally
             ! equals the total flux minus burial and denitrification.  However, if there is insufficient bottom
             ! oxygen to support this, some is assumed to be remineralized via sulfate reduction.
             if (cobalt%btm_o2(i,j) .gt. cobalt%o2_min) then  !{
                cobalt%fnoxic_sed(i,j) = max(0.0, min(cobalt%btm_o2(i,j)*cobalt%bottom_thickness* &
                                         cobalt%Rho_0*r_dt*(1.0/cobalt%o2_2_nh4), &
                                         cobalt%fntot_btm(i,j) - cobalt%fn_burial(i,j) - &
                                         cobalt%fno3denit_sed(i,j)/cobalt%n_2_n_denit))
             else
                cobalt%fnoxic_sed(i,j) = 0.0
             endif !}
             ! Any remaining organic matter is remineralized via sulfate reduction
             cobalt%fnfeso4red_sed(i,j) = max(0.0, cobalt%fntot_btm(i,j)-cobalt%fnoxic_sed(i,j)- &
                                          cobalt%fn_burial(i,j)-cobalt%fno3denit_sed(i,j)/cobalt%n_2_n_denit)
          else
             cobalt%fnfeso4red_sed(i,j) = 0.0
             cobalt%fno3denit_sed(i,j) = 0.0
             cobalt%fnoxic_sed(i,j) = 0.0
          endif !}

          !
          ! Iron flux from the sediment
          !

          ! Iron from sediment (Dale, 2015).  The maximum release from the sediment is set by ffe_sed_max.  The
          ! hyperbolic tangent requires the flux of carbon to the sediments (as mmoles m-2 day-1) in the numerator
          ! and the bottom water oxygen concentration (in microMolar units) in the denominator. Note that ffe_sed_max
          ! was converted to moles Fe m-2 sec-1 during parameter input, so ffe_sed has is in moles Fe m-2 sec-1
          cobalt%ffe_sed(i,j) = cobalt%ffe_sed_max * tanh( (cobalt%fntot_btm(i,j)*cobalt%c_2_n*sperd*1.0e3)/ &
                                max(cobalt%btm_o2(i,j)*1.0e6,epsln) )

          ! Additional coastal iron (Optional, default fe_coast = 0)
          !
          ! Coarse resolution models and/or intermediate resolution models in areas with exceptionally steep bathymetry
          ! can under-represent coastal iron because they don't resolve shallow regions. An option to add iron through
          ! the vertical face of the land mass has thus been included.  The flux is posed as a fraction (fe_coast) of
          ! the sediment Fe flux (moles Fe m-2 sec-1) that would have result from the sinking organic matter flux and
          ! O2 level of the adjacent waters.  This then spread across the layer mass (rho_dzt(i,j,k)) to give an input
          ! in moles Fe kg-1 sec-1. Conceptually, this can be though of as a net iron flux resulting from the fraction
          ! of the sinking flux that would have been intercepted at shallower depths were the model resolution finer.
          ! default value of fe_coast is 0 (i.e., only the explicitly resolved benthic flux is included). Values of
          ! fe_coast ~ 0.01-0.1 should produce reasonably elevated coastal iron values near steep bathymetry.
          !
          ! Old Expression:
          ! cobalt%jfe_coast(i,j,1) = cobalt%fe_coast * mask_coast(i,j) * grid_tmask(i,j,1) / &
          !     sqrt(grid_dat(i,j))
          !
          do j = jsc, jec ; do i = isc, iec ; do k = 1, nk !{
             if (cobalt%fe_coast == 0.0) then
               cobalt%jfe_coast(i,j,k) = 0.0
             else
               cobalt%jfe_coast(i,j,k) = cobalt%fe_coast*mask_coast(i,j)*grid_tmask(i,j,k)*cobalt%ffe_sed_max* &
                 tanh( ( (cobalt%f_ndet(i,j,k)*cobalt%wsink+phyto(SMALL)%f_n(i,j,k)*phyto(SMALL)%vmove(i,j,k)+ &
                 phyto(MEDIUM)%f_n(i,j,k)*phyto(MEDIUM)%vmove(i,j,k)+ & 
                 phyto(LARGE)%f_n(i,j,k)*phyto(LARGE)%vmove(i,j,k)+
                 phyto(DIAZO)%f_n(i,j,k)*phyto(DIAZO)%vmove(i,j,k))*cobalt%c_2_n*sperd*1.0e3 )/ &
                 /max(cobalt%btm_o2(i,j)*1.0e6,epsln) )/rho_dzt(i,j,k)
             endif
          enddo; enddo; enddo  !} i,j

          ! Have ffe_geotherm default to zero if the internal_heat variable
          ! needed to calculate it is not available (if geothermal heating is disabled).
          if(present(internal_heat)) then
              cobalt%ffe_geotherm(i,j) = cobalt%ffe_geotherm_ratio*internal_heat(i,j)*4184.0/dt
          else
              cobalt%ffe_geotherm(i,j) = 0.0
          endif

          !
          ! Calcium carbonate flux and burial, based on Dunne et al., 2012
          !
          ! phi_surfresp_cased = 0.14307   ! const for enhanced diss., surf sed respiration (dimensionless)
          ! phi_deepresp_cased = 4.1228    ! const for enhanced diss., deep sed respiration (dimensionless)
          ! alpha_cased = 2.7488 ! exponent controlling non-linearity of deep dissolution
          ! beta_cased = -2.2185 ! exponent controlling non-linearity of effective thickness
          ! gamma_cased = 0.03607/spery   ! dissolution rate constant
          ! Co_cased = 8.1e3        ! moles CaCo3 m-3 for pure calcite sediment with porosity = 0.7
          !
          ! if cased_steady is true, burial is calculated from Dunne's eq. (2) assuming dcased/dt = 0.
          ! This ensures that all the calcite bottom flux is partitioned between burial and redissolution.
          ! The steady state cased value of cased is calculated to reflect the changing bottom conditions.
          ! This influences the the partitioning of burial and redissolution over time, but there are
          ! no alkalinity changes/drifts associated with the long-term evolution of cased
          !
          ! If cased_steady is false, calcite is partitioned between dissolution, burial and evolving
          ! cased as described in Dunne et al. (2012).  The multi-century scale evolution of cased
          ! impacts alkalinity, but care must to ensure that cased starts in equilibrium with the
          ! mean ocean state to avoid unrealistic drifts.

          ! Enhanced dissolution by fast respiration near the sediment surface, proportional
          ! to organic flux, moles Ca m-2 s-1, limited to a max 1/2 the instantaneous calcite flux
          cobalt%fcased_redis_surfresp(i,j)=min(0.5*cobalt%f_cadet_calc_btf(i,j,1), &
            cobalt%phi_surfresp_cased*cobalt%fntot_btm(i,j)*cobalt%c_2_n)

          ! Ca-specific dissolution coeficient, depends on calcite saturation state and is enhanced by
          ! respiration deep in the sediment (s-1), non-linearity controlled by alpha_cased
          cobalt%cased_redis_coef(i,j) = cobalt%gamma_cased*max(0.0,1.0-cobalt%btm_omega_calc(i,j)+ &
            cobalt%phi_deepresp_cased*cobalt%fntot_btm(i,j)*cobalt%c_2_n*spery)**cobalt%alpha_cased

          ! Effective thickness term that enhances burial of calcite when total sediment accumulation is high
          ! dimensionless value between 0 and 1
          cobalt%cased_redis_delz(i,j) = max(1.0, &
            cobalt%f_lithdet_btf(i,j,1)*spery+cobalt%f_cadet_calc_btf(i,j,1)*100.0*spery)**cobalt%beta_cased

          ! calculate the sediment redissolution rate (moles Ca m-2 sec-1). This calculation is subject to
          ! three limiters: a) a maximum of 1/2 of the total cased over one time step; b) a maximum of 0.01
          ! moles Ca per day; and c) a minimum of 0.0
          cobalt%fcased_redis(i,j) = max(0.0, min(0.01/sperd, min(0.5*cobalt%f_cased(i,j,1)*r_dt,  &
            cobalt%fcased_redis_surfresp(i,j)+cobalt%cased_redis_coef(i,j)*cobalt%cased_redis_delz(i,j)*cobalt%f_cased(i,j,1))) )

          !
          ! Old expression
          !
          !cobalt%fcased_redis(i,j) = max(0.0, min(0.01/sperd,min(0.5 * cobalt%f_cased(i,j,1) * r_dt, min(0.5 *       &
          !   cobalt%f_cadet_calc_btf(i,j,1), 0.14307 * cobalt%f_ndet_btf(i,j,1) * cobalt%c_2_n) +        &
          !   0.03607 / spery * max(0.0, 1.0 - cobalt%omega_calc(i,j,k) +   &
          !   4.1228 * cobalt%f_ndet_btf(i,j,1) * cobalt%c_2_n * spery)**(2.7488) *                        &
          !   max(1.0, cobalt%f_lithdet_btf(i,j,1) * spery + cobalt%f_cadet_calc_btf(i,j,1) * 100.0 *  &
          !   spery)**(-2.2185) * cobalt%f_cased(i,j,1))))*grid_tmask(i,j,k)

          if (cobalt%cased_steady) then
            cobalt%fcased_burial(i,j) = cobalt%f_cadet_calc_btf(i,j,1) - cobalt%fcased_redis(i,j)
            cobalt%f_cased(i,j,1) = cobalt%fcased_burial(i,j)*cobalt%Co_cased/cobalt%f_cadet_calc_btf(i,j,1)
          else
            cobalt%fcased_burial(i,j) = max(0.0, cobalt%f_cadet_calc_btf(i,j,1) * cobalt%f_cased(i,j,1) / &
              cobalt%Co_cased)
            cobalt%f_cased(i,j,1) = cobalt%f_cased(i,j,1) + (cobalt%f_cadet_calc_btf(i,j,1) -            &
              cobalt%fcased_redis(i,j) - cobalt%fcased_burial(i,j)) / cobalt%z_sed * dt *                &
              grid_tmask(i,j,k)
          endif

          !
          ! Bottom flux boundaries passed to the vertical mixing routine
          ! (negative values are fluxes into the ocean)
          !
          cobalt%b_alk(i,j) = - 2.0*(cobalt%fcased_redis(i,j)+cobalt%f_cadet_arag_btf(i,j,1)) -    &
             cobalt%fnoxic_sed(i,j) - cobalt%fno3denit_sed(i,j)*cobalt%alk_2_n_denit
          cobalt%b_dic(i,j) =  - cobalt%fcased_redis(i,j) - cobalt%f_cadet_arag_btf(i,j,1) -       &
             (cobalt%fntot_btm(i,j) - cobalt%fn_burial(i,j)) * cobalt%c_2_n
          ! uncomment for "no mass change" test (next 2 lines)
          !cobalt%b_dic(i,j) =  - cobalt%f_cadet_calc_btf(i,j,1)  - cobalt%f_cadet_arag_btf(i,j,1) -            &
          !   (cobalt%fntot_btm(i,j) - cobalt%fn_burial(i,j)) * cobalt%c_2_n
          cobalt%b_fed(i,j) = - cobalt%ffe_sed(i,j) - cobalt%ffe_geotherm(i,j)
          ! uncomment for "no mass change" test (next line)
          !cobalt%b_fed(i,j) = - cobalt%ffetot_btm(i,j)
          cobalt%b_nh4(i,j) = - cobalt%fntot_btm(i,j) + cobalt%fn_burial(i,j)
          cobalt%b_no3(i,j) = cobalt%fno3denit_sed(i,j)
          ! uncomment if you want to include sulfate reduction
          !cobalt%b_o2(i,j)  = cobalt%o2_2_nh4 * (cobalt%fnoxic_sed(i,j) + cobalt%fnfeso4red_sed(i,j))
          cobalt%b_o2(i,j)  = cobalt%o2_2_nh4 * cobalt%fnoxic_sed(i,j)
          cobalt%b_po4(i,j) = - cobalt%fptot_btm(i,j) + cobalt%fp_burial(i,j)
          cobalt%b_sio4(i,j)= - cobalt%fsitot_btm(i,j)

       endif !}
    enddo; enddo  !} i, j

    do k = 2, nk ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%f_cased(i,j,k) = 0.0
    enddo; enddo ; enddo  !} i,j,k

    call mpp_clock_end(id_clock_ballast_loops)

    call g_tracer_set_values(tracer_list,'alk',  'btf', cobalt%b_alk ,isd,jsd)
    call g_tracer_set_values(tracer_list,'dic',  'btf', cobalt%b_dic ,isd,jsd)
    call g_tracer_set_values(tracer_list,'fed',  'btf', cobalt%b_fed ,isd,jsd)
    call g_tracer_set_values(tracer_list,'nh4',  'btf', cobalt%b_nh4 ,isd,jsd)
    call g_tracer_set_values(tracer_list,'no3',  'btf', cobalt%b_no3 ,isd,jsd)
    call g_tracer_set_values(tracer_list,'o2',   'btf', cobalt%b_o2  ,isd,jsd)
    call g_tracer_set_values(tracer_list,'po4',  'btf', cobalt%b_po4 ,isd,jsd)
    call g_tracer_set_values(tracer_list,'sio4', 'btf', cobalt%b_sio4,isd,jsd)
!
    call mpp_clock_begin(id_clock_source_sink_loop1)
!
!-----------------------------------------------------------------------
! 8: Source/sink calculations
!-----------------------------------------------------------------------
!
    !
    !-------------------------------------------------------------------
    ! 8.1: Update the prognostics tracer fields via their pointers.
    !-------------------------------------------------------------------
    !
    call g_tracer_get_pointer(tracer_list,'alk'    ,'field',cobalt%p_alk    )
    call g_tracer_get_pointer(tracer_list,'cadet_arag','field',cobalt%p_cadet_arag)
    call g_tracer_get_pointer(tracer_list,'cadet_calc','field',cobalt%p_cadet_calc)
    call g_tracer_get_pointer(tracer_list,'dic'    ,'field',cobalt%p_dic    )
    call g_tracer_get_pointer(tracer_list,'fed'    ,'field',cobalt%p_fed    )
    call g_tracer_get_pointer(tracer_list,'fedi'   ,'field',cobalt%p_fedi   )
    call g_tracer_get_pointer(tracer_list,'felg'   ,'field',cobalt%p_felg   )
    call g_tracer_get_pointer(tracer_list,'femd'   ,'field',cobalt%p_femd   )
    call g_tracer_get_pointer(tracer_list,'fesm'   ,'field',cobalt%p_fesm )
    call g_tracer_get_pointer(tracer_list,'fedet'  ,'field',cobalt%p_fedet  )
    call g_tracer_get_pointer(tracer_list,'ldon'   ,'field',cobalt%p_ldon   )
    call g_tracer_get_pointer(tracer_list,'ldop'   ,'field',cobalt%p_ldop   )
    call g_tracer_get_pointer(tracer_list,'lith'   ,'field',cobalt%p_lith   )
    call g_tracer_get_pointer(tracer_list,'lithdet','field',cobalt%p_lithdet)
    call g_tracer_get_pointer(tracer_list,'nbact'  ,'field',cobalt%p_nbact  )
    call g_tracer_get_pointer(tracer_list,'ndet'   ,'field',cobalt%p_ndet   )
    call g_tracer_get_pointer(tracer_list,'ndi'    ,'field',cobalt%p_ndi    )
    call g_tracer_get_pointer(tracer_list,'nlg'    ,'field',cobalt%p_nlg    )
    call g_tracer_get_pointer(tracer_list,'nmd'    ,'field',cobalt%p_nmd    )
    call g_tracer_get_pointer(tracer_list,'nsm' ,'field',cobalt%p_nsm )
    call g_tracer_get_pointer(tracer_list,'nh4'    ,'field',cobalt%p_nh4    )
    call g_tracer_get_pointer(tracer_list,'no3'    ,'field',cobalt%p_no3    )
    call g_tracer_get_pointer(tracer_list,'o2'     ,'field',cobalt%p_o2     )
    call g_tracer_get_pointer(tracer_list,'pdi'    ,'field',cobalt%p_pdi    )
    call g_tracer_get_pointer(tracer_list,'plg'    ,'field',cobalt%p_plg    )
    call g_tracer_get_pointer(tracer_list,'pmd'    ,'field',cobalt%p_pmd    )
    call g_tracer_get_pointer(tracer_list,'psm'    ,'field',cobalt%p_psm    )
    call g_tracer_get_pointer(tracer_list,'pdet'   ,'field',cobalt%p_pdet   )
    call g_tracer_get_pointer(tracer_list,'po4'    ,'field',cobalt%p_po4    )
    call g_tracer_get_pointer(tracer_list,'srdon'   ,'field',cobalt%p_srdon   )
    call g_tracer_get_pointer(tracer_list,'srdop'   ,'field',cobalt%p_srdop   )
    call g_tracer_get_pointer(tracer_list,'sldon'   ,'field',cobalt%p_sldon   )
    call g_tracer_get_pointer(tracer_list,'sldop'   ,'field',cobalt%p_sldop   )
    call g_tracer_get_pointer(tracer_list,'sidet'  ,'field',cobalt%p_sidet  )
    call g_tracer_get_pointer(tracer_list,'silg'   ,'field',cobalt%p_silg   )
    call g_tracer_get_pointer(tracer_list,'simd'   ,'field',cobalt%p_simd   )
    call g_tracer_get_pointer(tracer_list,'sio4'   ,'field',cobalt%p_sio4   )
    call g_tracer_get_pointer(tracer_list,'nsmz'   ,'field',cobalt%p_nsmz   )
    call g_tracer_get_pointer(tracer_list,'nmdz'   ,'field',cobalt%p_nmdz   )
    call g_tracer_get_pointer(tracer_list,'nlgz'   ,'field',cobalt%p_nlgz   )

    if (do_14c) then
       call g_tracer_get_pointer(tracer_list,'di14c','field',cobalt%p_di14c)
       call g_tracer_get_pointer(tracer_list,'do14c','field',cobalt%p_do14c)
    endif

    ! CAS calculate total N and P before source/sink
    ! calculate internal sources (those not applied as air-sea or benthos
    ! exchanges) to close the balance
    allocate(pre_totn(isc:iec,jsc:jec,1:nk))
    allocate(pre_totc(isc:iec,jsc:jec,1:nk))
    allocate(net_srcn(isc:iec,jsc:jec,1:nk))
    allocate(net_srcp(isc:iec,jsc:jec,1:nk))
    allocate(net_srcc(isc:iec,jsc:jec,1:nk))
    allocate(pre_totp(isc:iec,jsc:jec,1:nk))
    allocate(pre_totfe(isc:iec,jsc:jec,1:nk))
    allocate(net_srcfe(isc:iec,jsc:jec,1:nk))
    allocate(pre_totsi(isc:iec,jsc:jec,1:nk))
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
         pre_totn(i,j,k) = (cobalt%p_no3(i,j,k,tau) + cobalt%p_nh4(i,j,k,tau) + &
                    cobalt%p_ndi(i,j,k,tau) + cobalt%p_nlg(i,j,k,tau) + &
                    cobalt%p_nmd(i,j,k,tau) + &
                    cobalt%p_nsm(i,j,k,tau) + cobalt%p_nbact(i,j,k,tau) + &
                    cobalt%p_ldon(i,j,k,tau) + cobalt%p_sldon(i,j,k,tau) + &
                    cobalt%p_srdon(i,j,k,tau) +  cobalt%p_ndet(i,j,k,tau) + &
                    cobalt%p_nsmz(i,j,k,tau) + cobalt%p_nmdz(i,j,k,tau) + &
                    cobalt%p_nlgz(i,j,k,tau))*grid_tmask(i,j,k)
         net_srcn(i,j,k) = (phyto(DIAZO)%juptake_n2(i,j,k) - cobalt%jno3denit_wc(i,j,k) - &
                    cobalt%jnamx(i,j,k) + cobalt%jno3_iceberg(i,j,k))*dt*grid_tmask(i,j,k)
         net_srcc(i,j,k) = 0.0
         pre_totc(i,j,k) = (cobalt%p_dic(i,j,k,tau) + &
                    cobalt%p_cadet_arag(i,j,k,tau) + cobalt%p_cadet_calc(i,j,k,tau) + &
                    cobalt%c_2_n*(cobalt%p_ndi(i,j,k,tau) + cobalt%p_nlg(i,j,k,tau) + &
                    cobalt%p_nmd(i,j,k,tau) + &
                    cobalt%p_nsm(i,j,k,tau) + cobalt%p_nbact(i,j,k,tau) + &
                    cobalt%p_ldon(i,j,k,tau) + cobalt%p_sldon(i,j,k,tau) + &
                    cobalt%p_srdon(i,j,k,tau) +  cobalt%p_ndet(i,j,k,tau) + &
                    cobalt%p_nsmz(i,j,k,tau) + cobalt%p_nmdz(i,j,k,tau) + &
                    cobalt%p_nlgz(i,j,k,tau)))*grid_tmask(i,j,k)
         pre_totp(i,j,k) = (cobalt%p_po4(i,j,k,tau) + cobalt%p_pdi(i,j,k,tau) + &
                    cobalt%p_plg(i,j,k,tau) + cobalt%p_pmd(i,j,k,tau) + cobalt%p_psm(i,j,k,tau) + &
                    cobalt%p_ldop(i,j,k,tau) + cobalt%p_sldop(i,j,k,tau) + &
                    cobalt%p_srdop(i,j,k,tau) +  cobalt%p_pdet(i,j,k,tau) + &
                    cobalt%p_nsmz(i,j,k,tau)*zoo(1)%q_p_2_n + &
                    cobalt%p_nmdz(i,j,k,tau)*zoo(2)%q_p_2_n + &
                    cobalt%p_nlgz(i,j,k,tau)*zoo(3)%q_p_2_n + &
                    bact(1)%q_p_2_n*cobalt%p_nbact(i,j,k,tau))*grid_tmask(i,j,k)
         net_srcp(i,j,k) = cobalt%jpo4_iceberg(i,j,k)*dt*grid_tmask(i,j,k)
         pre_totfe(i,j,k) = (cobalt%p_fed(i,j,k,tau) + cobalt%p_fedi(i,j,k,tau) + &
                    cobalt%p_felg(i,j,k,tau) + cobalt%p_femd(i,j,k,tau) + cobalt%p_fesm(i,j,k,tau) + &
                    cobalt%p_fedet(i,j,k,tau))*grid_tmask(i,j,k)
         net_srcfe(i,j,k) = (cobalt%jfe_coast(i,j,k)+cobalt%jfe_iceberg(i,j,k))*dt*grid_tmask(i,j,k)
         pre_totsi(i,j,k) = (cobalt%p_sio4(i,j,k,tau) + cobalt%p_silg(i,j,k,tau) + &
                    cobalt%p_simd(i,j,k,tau) + cobalt%p_sidet(i,j,k,tau))*grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k

    if (cobalt%id_no3_in_source .gt. 0)                &
         used = g_send_data(cobalt%id_no3_in_source,         cobalt%f_no3,          &
         model_time, rmask = grid_tmask,&
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

    call mpp_clock_end(id_clock_source_sink_loop1)
    !
    !-----------------------------------------------------------------------
    ! 8.2: Source sink calculations
    !-----------------------------------------------------------------------
    !
    !     Phytoplankton Nitrogen and Phosphorus
    !
    call mpp_clock_begin(id_clock_source_sink_loop2)
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! Diazotrophic Phytoplankton Nitrogen
       !
       cobalt%jndi(i,j,k) = phyto(DIAZO)%mu(i,j,k)*phyto(DIAZO)%f_n(i,j,k) - &
                            phyto(DIAZO)%jzloss_n(i,j,k) -       &
                            phyto(DIAZO)%jhploss_n(i,j,k) - phyto(DIAZO)%jaggloss_n(i,j,k) -       &
                            phyto(DIAZO)%jvirloss_n(i,j,k) - phyto(DIAZO)%jexuloss_n(i,j,k) -      &
                            phyto(DIAZO)%jmortloss_n(i,j,k)
       cobalt%p_ndi(i,j,k,tau) = cobalt%p_ndi(i,j,k,tau) + cobalt%jndi(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Large Phytoplankton Nitrogen
       !
       cobalt%jnlg(i,j,k) = phyto(LARGE)%mu(i,j,k)*phyto(LARGE)%f_n(i,j,k) -    &
                            phyto(LARGE)%jzloss_n(i,j,k) - phyto(LARGE)%jhploss_n(i,j,k) -         &
                            phyto(LARGE)%jaggloss_n(i,j,k) - phyto(LARGE)%jvirloss_n(i,j,k) -      &
                            phyto(LARGE)%jexuloss_n(i,j,k) - phyto(LARGE)%jmortloss_n(i,j,k)
       cobalt%p_nlg(i,j,k,tau) = cobalt%p_nlg(i,j,k,tau) + cobalt%jnlg(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Medium Phytoplankton Nitrogen
       !
       cobalt%jnmd(i,j,k) = phyto(MEDIUM)%mu(i,j,k)*phyto(MEDIUM)%f_n(i,j,k) -    &
                            phyto(MEDIUM)%jzloss_n(i,j,k) - phyto(MEDIUM)%jhploss_n(i,j,k) -         &
                            phyto(MEDIUM)%jaggloss_n(i,j,k) - phyto(MEDIUM)%jvirloss_n(i,j,k) -      &
                            phyto(MEDIUM)%jexuloss_n(i,j,k) - phyto(MEDIUM)%jmortloss_n(i,j,k)
       cobalt%p_nmd(i,j,k,tau) = cobalt%p_nmd(i,j,k,tau) + cobalt%jnmd(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Small Phytoplankton Nitrogen
       !
       cobalt%jnsm(i,j,k) = phyto(SMALL)%mu(i,j,k)*phyto(SMALL)%f_n(i,j,k) -    &
                            phyto(SMALL)%jzloss_n(i,j,k) - phyto(SMALL)%jhploss_n(i,j,k) -         &
                            phyto(SMALL)%jaggloss_n(i,j,k) - phyto(SMALL)%jvirloss_n(i,j,k) -      &
                            phyto(SMALL)%jexuloss_n(i,j,k) - phyto(SMALL)%jmortloss_n(i,j,k)
       cobalt%p_nsm(i,j,k,tau) = cobalt%p_nsm(i,j,k,tau) + cobalt%jnsm(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Diazotrophic Phytoplankton Phosphorus
       !
       cobalt%jpdi(i,j,k) = phyto(DIAZO)%juptake_po4(i,j,k) + &
                            min(phyto(DIAZO)%mu(i,j,k),0.0)*phyto(DIAZO)%f_p(i,j,k) - &
                            phyto(DIAZO)%jzloss_p(i,j,k) -  &
                            phyto(DIAZO)%jhploss_p(i,j,k) - phyto(DIAZO)%jaggloss_p(i,j,k) -       &
                            phyto(DIAZO)%jvirloss_p(i,j,k) - phyto(DIAZO)%jexuloss_p(i,j,k) -      &
                            phyto(DIAZO)%jmortloss_p(i,j,k)
       cobalt%p_pdi(i,j,k,tau) = cobalt%p_pdi(i,j,k,tau) + cobalt%jpdi(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Large Phytoplankton Phosphorus
       !
       cobalt%jplg(i,j,k) = phyto(LARGE)%juptake_po4(i,j,k) + &
                            min(phyto(LARGE)%mu(i,j,k),0.0)*phyto(LARGE)%f_p(i,j,k) - &
                            phyto(LARGE)%jzloss_p(i,j,k) - phyto(LARGE)%jhploss_p(i,j,k) -         &
                            phyto(LARGE)%jaggloss_p(i,j,k) - phyto(LARGE)%jvirloss_p(i,j,k) -      &
                            phyto(LARGE)%jexuloss_p(i,j,k) - phyto(LARGE)%jmortloss_p(i,j,k)
       cobalt%p_plg(i,j,k,tau) = cobalt%p_plg(i,j,k,tau) + cobalt%jplg(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Medium Phytoplankton Phosphorus
       !
       cobalt%jpmd(i,j,k) = phyto(MEDIUM)%juptake_po4(i,j,k) + &
                            min(phyto(MEDIUM)%mu(i,j,k),0.0)*phyto(MEDIUM)%f_p(i,j,k) - &
                            phyto(MEDIUM)%jzloss_p(i,j,k) - phyto(MEDIUM)%jhploss_p(i,j,k) -         &
                            phyto(MEDIUM)%jaggloss_p(i,j,k) - phyto(MEDIUM)%jvirloss_p(i,j,k) -      &
                            phyto(MEDIUM)%jexuloss_p(i,j,k) - phyto(MEDIUM)%jmortloss_p(i,j,k)
       cobalt%p_pmd(i,j,k,tau) = cobalt%p_pmd(i,j,k,tau) + cobalt%jpmd(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Small Phytoplankton Phosphorus
       !
       cobalt%jpsm(i,j,k) = phyto(SMALL)%juptake_po4(i,j,k) + &
                            min(phyto(SMALL)%mu(i,j,k),0.0)*phyto(SMALL)%f_p(i,j,k) - &
                            phyto(SMALL)%jzloss_p(i,j,k) - phyto(SMALL)%jhploss_p(i,j,k) -         &
                            phyto(SMALL)%jaggloss_p(i,j,k) - phyto(SMALL)%jvirloss_p(i,j,k) -      &
                            phyto(SMALL)%jexuloss_p(i,j,k) - phyto(SMALL)%jmortloss_p(i,j,k)
       cobalt%p_psm(i,j,k,tau) = cobalt%p_psm(i,j,k,tau) + cobalt%jpsm(i,j,k)*dt*grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k
!
    call mpp_clock_end(id_clock_source_sink_loop2)
    !
    !     Phytoplankton Silicon and Iron
    !
    call mpp_clock_begin(id_clock_source_sink_loop3)
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! Large Phytoplankton Silicon
       !
       cobalt%jsilg(i,j,k) = phyto(LARGE)%juptake_sio4(i,j,k) - &
                             phyto(LARGE)%jzloss_sio2(i,j,k) - phyto(LARGE)%jhploss_sio2(i,j,k) - &
                             phyto(LARGE)%jaggloss_sio2(i,j,k) - phyto(LARGE)%jvirloss_sio2(i,j,k) - &
                             phyto(LARGE)%jmortloss_sio2(i,j,k)
       cobalt%p_silg(i,j,k,tau) = cobalt%p_silg(i,j,k,tau) + cobalt%jsilg(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Medium Phytoplankton Silicon
       !
       cobalt%jsimd(i,j,k) = phyto(MEDIUM)%juptake_sio4(i,j,k) - &
                             phyto(MEDIUM)%jzloss_sio2(i,j,k) - phyto(MEDIUM)%jhploss_sio2(i,j,k) - &
                             phyto(MEDIUM)%jaggloss_sio2(i,j,k) - phyto(MEDIUM)%jvirloss_sio2(i,j,k) - &
                             phyto(MEDIUM)%jmortloss_sio2(i,j,k)
       cobalt%p_simd(i,j,k,tau) = cobalt%p_simd(i,j,k,tau) + cobalt%jsimd(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Diazotrophic Phytoplankton Iron
       !
       cobalt%jfedi(i,j,k) = phyto(DIAZO)%juptake_fe(i,j,k) - &
                             phyto(DIAZO)%jzloss_fe(i,j,k) - &
                             phyto(DIAZO)%jhploss_fe(i,j,k) - phyto(DIAZO)%jaggloss_fe(i,j,k) - &
                             phyto(DIAZO)%jvirloss_fe(i,j,k) - phyto(DIAZO)%jexuloss_fe(i,j,k) - &
                             phyto(DIAZO)%jmortloss_fe(i,j,k)
       cobalt%p_fedi(i,j,k,tau) = cobalt%p_fedi(i,j,k,tau) + cobalt%jfedi(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Large Phytoplankton Iron
       !
       cobalt%jfelg(i,j,k) = phyto(LARGE)%juptake_fe(i,j,k) - &
                             phyto(LARGE)%jzloss_fe(i,j,k) - &
                             phyto(LARGE)%jhploss_fe(i,j,k) - phyto(LARGE)%jaggloss_fe(i,j,k) - &
                             phyto(LARGE)%jvirloss_fe(i,j,k) - phyto(LARGE)%jexuloss_fe(i,j,k) - &
                             phyto(LARGE)%jmortloss_fe(i,j,k)
       cobalt%p_felg(i,j,k,tau) = cobalt%p_felg(i,j,k,tau) + cobalt%jfelg(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Medium Phytoplankton Iron
       !
       cobalt%jfemd(i,j,k) = phyto(MEDIUM)%juptake_fe(i,j,k) - &
                             phyto(MEDIUM)%jzloss_fe(i,j,k) - &
                             phyto(MEDIUM)%jhploss_fe(i,j,k) - phyto(MEDIUM)%jaggloss_fe(i,j,k) - &
                             phyto(MEDIUM)%jvirloss_fe(i,j,k) - phyto(MEDIUM)%jexuloss_fe(i,j,k) - &
                             phyto(MEDIUM)%jmortloss_fe(i,j,k)
       cobalt%p_femd(i,j,k,tau) = cobalt%p_femd(i,j,k,tau) + cobalt%jfemd(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Small Phytoplankton Iron
       !
       cobalt%jfesm(i,j,k) = phyto(SMALL)%juptake_fe(i,j,k) - &
                                phyto(SMALL)%jzloss_fe(i,j,k) - &
                                phyto(SMALL)%jhploss_fe(i,j,k) - phyto(SMALL)%jaggloss_fe(i,j,k) - &
                                phyto(SMALL)%jvirloss_fe(i,j,k) - phyto(SMALL)%jexuloss_fe(i,j,k) - &
                                phyto(SMALL)%jmortloss_fe(i,j,k)
       cobalt%p_fesm(i,j,k,tau) = cobalt%p_fesm(i,j,k,tau) + cobalt%jfesm(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Bacteria
       !
       cobalt%jnbact(i,j,k) = bact(1)%jprod_n(i,j,k) - bact(1)%jzloss_n(i,j,k) - &
                              bact(1)%jvirloss_n(i,j,k) - bact(1)%jhploss_n(i,j,k)
       cobalt%p_nbact(i,j,k,tau) = cobalt%p_nbact(i,j,k,tau) + cobalt%jnbact(i,j,k)*dt*grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k

    call mpp_clock_end(id_clock_source_sink_loop3)
    !
    !    Zooplankton
    !
    call mpp_clock_begin(id_clock_source_sink_loop4)
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! Small zooplankton
       !
       cobalt%jnsmz(i,j,k) = zoo(1)%jprod_n(i,j,k) - zoo(1)%jzloss_n(i,j,k) - &
                             zoo(1)%jhploss_n(i,j,k)
       cobalt%p_nsmz(i,j,k,tau) = cobalt%p_nsmz(i,j,k,tau) + cobalt%jnsmz(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Medium zooplankton
       !
       cobalt%jnmdz(i,j,k) = zoo(2)%jprod_n(i,j,k) - zoo(2)%jzloss_n(i,j,k) - &
                             zoo(2)%jhploss_n(i,j,k)
       cobalt%p_nmdz(i,j,k,tau) = cobalt%p_nmdz(i,j,k,tau) + cobalt%jnmdz(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Large zooplankton
       !
       cobalt%jnlgz(i,j,k) = zoo(3)%jprod_n(i,j,k) - zoo(3)%jzloss_n(i,j,k) - &
                             zoo(3)%jhploss_n(i,j,k)
       cobalt%p_nlgz(i,j,k,tau) = cobalt%p_nlgz(i,j,k,tau) + cobalt%jnlgz(i,j,k)*dt*grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k
!
    call mpp_clock_end(id_clock_source_sink_loop4)
    !
    !     NO3
    !
    call mpp_clock_begin(id_clock_source_sink_loop5)
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       cobalt%jno3(i,j,k) =  cobalt%jprod_no3nitrif(i,j,k) - phyto(DIAZO)%juptake_no3(i,j,k) - &
                             phyto(LARGE)%juptake_no3(i,j,k) - phyto(MEDIUM)%juptake_no3(i,j,k) - &
                             phyto(SMALL)%juptake_no3(i,j,k) - &
                             cobalt%jno3denit_wc(i,j,k) - cobalt%juptake_no3amx(i,j,k)
       cobalt%p_no3(i,j,k,tau) = cobalt%p_no3(i,j,k,tau) + &
               (cobalt%jno3(i,j,k)+cobalt%jno3_iceberg(i,j,k))*dt*grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k
    !
    !     Other nutrients
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! NH4
       !
       cobalt%jnh4(i,j,k) = cobalt%jprod_nh4(i,j,k) - phyto(DIAZO)%juptake_nh4(i,j,k) - &
                            phyto(LARGE)%juptake_nh4(i,j,k) - phyto(MEDIUM)%juptake_nh4(i,j,k) - &
                            phyto(SMALL)%juptake_nh4(i,j,k) - &
                            cobalt%juptake_nh4nitrif(i,j,k) - cobalt%juptake_nh4amx(i,j,k)
       cobalt%p_nh4(i,j,k,tau) = cobalt%p_nh4(i,j,k,tau) + cobalt%jnh4(i,j,k) * dt * grid_tmask(i,j,k)
       !
       ! PO4
       !
       cobalt%jpo4(i,j,k) = cobalt%jprod_po4(i,j,k) - phyto(DIAZO)%juptake_po4(i,j,k) - &
                            phyto(LARGE)%juptake_po4(i,j,k) - phyto(MEDIUM)%juptake_po4(i,j,k) - &
                            phyto(SMALL)%juptake_po4(i,j,k)
       cobalt%p_po4(i,j,k,tau) = cobalt%p_po4(i,j,k,tau) + &
              (cobalt%jpo4(i,j,k)+cobalt%jpo4_iceberg(i,j,k)) * dt * grid_tmask(i,j,k)
       !
       ! SiO4
       !
       cobalt%jsio4(i,j,k) = cobalt%jprod_sio4(i,j,k) - phyto(LARGE)%juptake_sio4(i,j,k) - &
                             phyto(MEDIUM)%juptake_sio4(i,j,k)
       cobalt%p_sio4(i,j,k,tau) = cobalt%p_sio4(i,j,k,tau) + cobalt%jsio4(i,j,k) * dt * grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k

    ! 2016/06/13 JGJ: keep original Fed calculation
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! Fed
       !
       cobalt%jfed(i,j,k) = cobalt%jprod_fed(i,j,k) + cobalt%jfe_coast(i,j,k) + &
                            cobalt%jfe_iceberg(i,j,k) - phyto(DIAZO)%juptake_fe(i,j,k) - &
                            phyto(LARGE)%juptake_fe(i,j,k) - phyto(MEDIUM)%juptake_fe(i,j,k) - &
                            phyto(SMALL)%juptake_fe(i,j,k) - cobalt%jfe_ads(i,j,k)
       cobalt%p_fed(i,j,k,tau) = cobalt%p_fed(i,j,k,tau) + cobalt%jfed(i,j,k) * dt * grid_tmask(i,j,k)
    enddo; enddo; enddo  !} i,j,k

    call mpp_clock_end(id_clock_source_sink_loop5)
    !
    !-----------------------------------------------------------------------
    !     Detrital Components
    !-----------------------------------------------------------------------
    !
    call mpp_clock_begin(id_clock_source_sink_loop6)
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! Cadet_arag
       !
       cobalt%jcadet_arag(i,j,k) = cobalt%jprod_cadet_arag(i,j,k) - cobalt%jdiss_cadet_arag(i,j,k)
       cobalt%p_cadet_arag(i,j,k,tau) = cobalt%p_cadet_arag(i,j,k,tau) + cobalt%jcadet_arag(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Cadet_calc
       !
       cobalt%jcadet_calc(i,j,k) = cobalt%jprod_cadet_calc(i,j,k) - cobalt%jdiss_cadet_calc(i,j,k)
       cobalt%p_cadet_calc(i,j,k,tau) = cobalt%p_cadet_calc(i,j,k,tau) + cobalt%jcadet_calc(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Lithdet
       !
       cobalt%jlithdet(i,j,k) = cobalt%jprod_lithdet(i,j,k)
       cobalt%p_lithdet(i,j,k,tau) = cobalt%p_lithdet(i,j,k,tau) + cobalt%jlithdet(i,j,k) * dt *  &
                                     grid_tmask(i,j,k)
       !
       ! Ndet
       !
       cobalt%jndet(i,j,k) = cobalt%jprod_ndet(i,j,k) - cobalt%jremin_ndet(i,j,k) - &
                             cobalt%det_jzloss_n(i,j,k) - cobalt%det_jhploss_n(i,j,k)
       cobalt%p_ndet(i,j,k,tau) = cobalt%p_ndet(i,j,k,tau) + cobalt%jndet(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Pdet
       !
       cobalt%jpdet(i,j,k) = cobalt%jprod_pdet(i,j,k) - cobalt%jremin_pdet(i,j,k) - &
                             cobalt%det_jzloss_p(i,j,k) - cobalt%det_jhploss_p(i,j,k)
       cobalt%p_pdet(i,j,k,tau) = cobalt%p_pdet(i,j,k,tau) + cobalt%jpdet(i,j,k)*dt*grid_tmask(i,j,k)
       !
       ! Sidet
       !
       cobalt%jsidet(i,j,k) = cobalt%jprod_sidet(i,j,k) - &
                              cobalt%jdiss_sidet(i,j,k) - cobalt%det_jzloss_si(i,j,k) - &
                              cobalt%det_jhploss_si(i,j,k)
       cobalt%p_sidet(i,j,k,tau) = cobalt%p_sidet(i,j,k,tau) + cobalt%jsidet(i,j,k)*dt*grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k

    ! 2016/06/13 JGJ: keep original jfedet calculation
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! Fedet
       !
       cobalt%jprod_fedet(i,j,k) = cobalt%jprod_fedet(i,j,k) + cobalt%jfe_ads(i,j,k)
       cobalt%jfedet(i,j,k) = cobalt%jprod_fedet(i,j,k) - &
                              cobalt%jremin_fedet(i,j,k) - cobalt%det_jzloss_fe(i,j,k) - &
                              cobalt%det_jhploss_fe(i,j,k)
       cobalt%p_fedet(i,j,k,tau) = cobalt%p_fedet(i,j,k,tau) + cobalt%jfedet(i,j,k)*dt*grid_tmask(i,j,k)
    enddo; enddo; enddo  !} i,j,k
    !
    !     Dissolved Organic Matter
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! Labile Dissolved Organic Nitrogen
       !
       cobalt%jldon(i,j,k) = cobalt%jprod_ldon(i,j,k) + &
                             cobalt%gamma_sldon*cobalt%expkT(i,j,k)*cobalt%f_sldon(i,j,k) + &
                             cobalt%gamma_srdon*cobalt%f_srdon(i,j,k) - bact(1)%juptake_ldon(i,j,k)
       cobalt%p_ldon(i,j,k,tau) = cobalt%p_ldon(i,j,k,tau) +  cobalt%jldon(i,j,k)*dt*               &
            grid_tmask(i,j,k)
       !
       ! Labile Dissolved Organic Phosphorous
       !
       cobalt%jldop(i,j,k) = cobalt%jprod_ldop(i,j,k) + &
                             cobalt%gamma_sldop*cobalt%expkT(i,j,k)*cobalt%f_sldop(i,j,k) + &
                             cobalt%gamma_srdop*cobalt%f_srdop(i,j,k) - bact(1)%juptake_ldop(i,j,k)
       cobalt%p_ldop(i,j,k,tau) = cobalt%p_ldop(i,j,k,tau) +  cobalt%jldop(i,j,k)*dt*               &
                             grid_tmask(i,j,k)
       !
       ! Semilabile Dissolved Organic Nitrogen
       !
       cobalt%jsldon(i,j,k) = cobalt%jprod_sldon(i,j,k) - &
                              cobalt%gamma_sldon*cobalt%expkT(i,j,k)*cobalt%f_sldon(i,j,k)
       cobalt%p_sldon(i,j,k,tau) = cobalt%p_sldon(i,j,k,tau) +  cobalt%jsldon(i,j,k) * dt *               &
            grid_tmask(i,j,k)
       !
       ! Semilabile dissolved organic phosphorous
       !
       cobalt%jsldop(i,j,k) = cobalt%jprod_sldop(i,j,k) - &
                              cobalt%gamma_sldop*cobalt%expkT(i,j,k)*cobalt%f_sldop(i,j,k)
       cobalt%p_sldop(i,j,k,tau) = cobalt%p_sldop(i,j,k,tau) + cobalt%jsldop(i,j,k) * dt *                &
                                  grid_tmask(i,j,k)
       !
       ! Refractory Dissolved Organic Nitrogen
       !
       cobalt%jsrdon(i,j,k) = cobalt%jprod_srdon(i,j,k) -  cobalt%gamma_srdon * cobalt%f_srdon(i,j,k)
       cobalt%p_srdon(i,j,k,tau) = cobalt%p_srdon(i,j,k,tau) +  cobalt%jsrdon(i,j,k) * dt *               &
            grid_tmask(i,j,k)
       !
       ! Refractory dissolved organic phosphorous
       !
       cobalt%jsrdop(i,j,k) = cobalt%jprod_srdop(i,j,k) - cobalt%gamma_srdop * cobalt%f_srdop(i,j,k)
       cobalt%p_srdop(i,j,k,tau) = cobalt%p_srdop(i,j,k,tau) + cobalt%jsrdop(i,j,k) * dt *                &
                                  grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k
    !
    !     O2
    !
    do k = 1, nk ; do j =jsc, jec ; do i = isc, iec  !{
       cobalt%jo2(i,j,k) = (cobalt%o2_2_no3 * (phyto(DIAZO)%juptake_no3(i,j,k) +   &
            phyto(LARGE)%juptake_no3(i,j,k) + phyto(MEDIUM)%juptake_no3(i,j,k) + &
            phyto(SMALL)%juptake_no3(i,j,k)) + cobalt%o2_2_nh4 *       &
            (phyto(DIAZO)%juptake_nh4(i,j,k) + phyto(LARGE)%juptake_nh4(i,j,k) +      &
            phyto(MEDIUM)%juptake_nh4(i,j,k) + phyto(SMALL)%juptake_nh4(i,j,k) + &
            phyto(DIAZO)%juptake_n2(i,j,k))) * grid_tmask(i,j,k)
       cobalt%jo2(i,j,k) = cobalt%jo2(i,j,k) - cobalt%jo2resp_wc(i,j,k)
       cobalt%p_o2(i,j,k,tau) = cobalt%p_o2(i,j,k,tau) + cobalt%jo2(i,j,k) * dt * grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k
    !
    !     The Carbon system
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       !
       ! Alkalinity
       ! CAS: remove o2 removal via nitrification from the total o2 respired
       !      to isolate the change in alkalinity due to aerobic organic
       !      matter remineralization
       !
       cobalt%jalk(i,j,k) = 2.0 * (cobalt%jdiss_cadet_arag(i,j,k) +        &
          cobalt%jdiss_cadet_calc(i,j,k) - cobalt%jprod_cadet_arag(i,j,k) - &
          cobalt%jprod_cadet_calc(i,j,k)) + phyto(DIAZO)%juptake_no3(i,j,k) + &
          phyto(LARGE)%juptake_no3(i,j,k) + phyto(MEDIUM)%juptake_no3(i,j,k) + &
          phyto(SMALL)%juptake_no3(i,j,k) + &
          (cobalt%jo2resp_wc(i,j,k)-cobalt%juptake_nh4nitrif(i,j,k)*cobalt%o2_2_nitrif)/cobalt%o2_2_nh4 + &
          cobalt%alk_2_n_denit*cobalt%jno3denit_wc(i,j,k) - &
          cobalt%alk_2_nh4_amx*cobalt%juptake_nh4amx(i,j,k) - &
          phyto(DIAZO)%juptake_nh4(i,j,k) - phyto(LARGE)%juptake_nh4(i,j,k) - &
          phyto(MEDIUM)%juptake_nh4(i,j,k) - &
          phyto(SMALL)%juptake_nh4(i,j,k) - 2.0 * cobalt%juptake_nh4nitrif(i,j,k)

       cobalt%p_alk(i,j,k,tau) = cobalt%p_alk(i,j,k,tau) + cobalt%jalk(i,j,k) * dt * grid_tmask(i,j,k)
       !
       ! Dissolved Inorganic Carbon
       !

       cobalt%jdic(i,j,k) =(cobalt%c_2_n * (cobalt%jprod_nh4(i,j,k) - &
          phyto(DIAZO)%juptake_no3(i,j,k) - phyto(LARGE)%juptake_no3(i,j,k) - &
          phyto(MEDIUM)%juptake_no3(i,j,k) - phyto(SMALL)%juptake_no3(i,j,k) - &
          phyto(DIAZO)%juptake_nh4(i,j,k) - phyto(LARGE)%juptake_nh4(i,j,k) - &
          phyto(MEDIUM)%juptake_nh4(i,j,k) - phyto(SMALL)%juptake_nh4(i,j,k) - &
          phyto(DIAZO)%juptake_n2(i,j,k)) + &
          cobalt%jdiss_cadet_arag(i,j,k) + cobalt%jdiss_cadet_calc(i,j,k) - &
          cobalt%jprod_cadet_arag(i,j,k) - cobalt%jprod_cadet_calc(i,j,k))

       cobalt%p_dic(i,j,k,tau) = cobalt%p_dic(i,j,k,tau) + cobalt%jdic(i,j,k) * dt * grid_tmask(i,j,k)
    enddo; enddo ; enddo !} i,j,k
!

    if (do_14c) then                                        !<<RADIOCARBON

         do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{

          cobalt%c14_2_n(i,j,k) = cobalt%c_2_n *                             &
            cobalt%f_di14c(i,j,k) / (epsln + cobalt%f_dic(i,j,k))

         enddo; enddo ; enddo !} i,j,k

      ! Sinking particulate 14C is generated in the local ratio of 14C/12C
      ! to sinking 12C, which itself is strictly tied to P through a fixed
      ! C:P. Therefore, jpop can be used to calculate fpo14c.

      do j = jsc, jec ;      do i = isc, iec   !{
        cobalt%fpo14c(i,j,1) =  (cobalt%jprod_ndet(i,j,1) - (cobalt%jremin_ndet(i,j,1) +          &
                             cobalt%det_jzloss_n(i,j,1) + cobalt%det_jhploss_n(i,j,1))) *         &
                             cobalt%c14_2_n(i,j,1) * rho_dzt(i,j,1)
        cobalt%j14c_reminp(i,j,1) = (-1) * cobalt%fpo14c(i,j,1) / rho_dzt(i,j,1)
      enddo; enddo !} i,j

      do k = 2, nk ; do j = jsc, jec ; do i = isc, iec   !{
        cobalt%fpo14c(i,j,k) = max(0.,cobalt%fpo14c(i,j,k-1) +          &
                               (cobalt%jprod_ndet(i,j,k) * cobalt%c14_2_n(i,j,k) - (cobalt%jremin_ndet(i,j,k) + &
                               cobalt%det_jzloss_n(i,j,k) + cobalt%det_jhploss_n(i,j,k)) *                      &
                               cobalt%fpo14c(i,j,k-1) / max(epsln,cobalt%f_ndet(i,j,k-1) * cobalt%Rho_0 *       &
                               cobalt%wsink)) * rho_dzt(i,j,k))

         cobalt%j14c_reminp(i,j,k) = (cobalt%fpo14c(i,j,k-1) - cobalt%fpo14c(i,j,k)) / rho_dzt(i,j,k)
      enddo; enddo ; enddo !} i,j,k

     ! Decay the radiocarbon in both DIC and DOC

      do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{

        cobalt%j14c_decay_dic(i,j,k) = cobalt%f_di14c(i,j,k) *               &
          cobalt%lambda_14c

        cobalt%j14c_decay_doc(i,j,k) = cobalt%f_do14c(i,j,k) *               &
          cobalt%lambda_14c

      enddo; enddo ; enddo !} i,j,k

      do j = jsc, jec ; do i = isc, iec  !{
         k = grid_kmt(i,j)
         if (k .gt. 0) then !{
           cobalt%b_di14c(i,j) = - cobalt%fpo14c(i,j,k)- cobalt%fcased_redis(i,j) - cobalt%f_cadet_arag_btf(i,j,1)
         endif
      enddo; enddo  !} i, j

     call g_tracer_set_values(tracer_list,'di14c','btf',cobalt%b_di14c,isd,jsd)
!
! Include only 14C in the semirefractory component of DOC
!
     do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
       cobalt%jdo14c(i,j,k) = cobalt%jprod_srdon(i,j,k) * cobalt%c14_2_n(i,j,k) - &
           cobalt%gamma_srdon * cobalt%f_do14c(i,j,k)

       cobalt%p_do14c(i,j,k,tau) = cobalt%p_do14c(i,j,k,tau) +               &
         (cobalt%jdo14c(i,j,k) - cobalt%j14c_decay_doc(i,j,k)) * dt          &
         * grid_tmask(i,j,k)
!
! Use the DIC budget except remove the srdon component and sinking detritus components which are treated separately
!
       cobalt%jdi14c(i,j,k) =(cobalt%c14_2_n(i,j,k) * (cobalt%jno3(i,j,k) + &
          cobalt%jnh4(i,j,k) + cobalt%jno3denit_wc(i,j,k) - phyto(DIAZO)%juptake_n2(i,j,k)) + &
          cobalt%jsrdon(i,j,k)) + cobalt%jdiss_cadet_arag(i,j,k) + cobalt%jdiss_cadet_calc(i,j,k) - &
          cobalt%jprod_cadet_arag(i,j,k) - cobalt%jprod_cadet_calc(i,j,k) -&
          cobalt%jdo14c(i,j,k) + cobalt%j14c_reminp(i,j,k)

       cobalt%p_di14c(i,j,k,tau) = cobalt%p_di14c(i,j,k,tau) +               &
         (cobalt%jdi14c(i,j,k) - cobalt%j14c_decay_dic(i,j,k)) * dt          &
         * grid_tmask(i,j,k)
     enddo; enddo ; enddo !} i,j,k
    endif                                                   !RADIOCARBON>>
    !
    !-----------------------------------------------------------------------
    !     Lithogenic aluminosilicate particulates
    !-----------------------------------------------------------------------
    !
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
       cobalt%p_lith(i,j,k,tau) = cobalt%p_lith(i,j,k,tau) - cobalt%jlithdet(i,j,k) * dt *        &
            grid_tmask(i,j,k)
    enddo; enddo ; enddo  !} i,j,k
    call mpp_clock_end(id_clock_source_sink_loop6)
    call mpp_clock_begin(id_clock_cobalt_calc_diagnostics)
    !
    !Set the diagnostics tracer fields.
    !
    call g_tracer_set_values(tracer_list,'cased',  'field',cobalt%f_cased    ,isd,jsd)
    call g_tracer_set_values(tracer_list,'chl',    'field',cobalt%f_chl      ,isd,jsd)
    if (do_nh3_diag) call g_tracer_set_values(tracer_list,'nh3',    'field',cobalt%f_nh3      ,isd,jsd)
    call g_tracer_set_values(tracer_list,'co3_ion','field',cobalt%f_co3_ion  ,isd,jsd)
    call g_tracer_set_values(tracer_list,'irr_aclm' ,'field',cobalt%f_irr_aclm ,isd,jsd)
    call g_tracer_set_values(tracer_list,'irr_aclm_z' ,'field',cobalt%f_irr_aclm_z ,isd,jsd)
    call g_tracer_set_values(tracer_list,'irr_aclm_sfc' ,'field',cobalt%f_irr_aclm_sfc ,isd,jsd)
    call g_tracer_set_values(tracer_list,'mu_mem_ndi' ,'field',phyto(DIAZO)%f_mu_mem ,isd,jsd)
    call g_tracer_set_values(tracer_list,'mu_mem_nlg' ,'field',phyto(LARGE)%f_mu_mem ,isd,jsd)
    call g_tracer_set_values(tracer_list,'mu_mem_nmd' ,'field',phyto(MEDIUM)%f_mu_mem ,isd,jsd)
    call g_tracer_set_values(tracer_list,'mu_mem_nsm' ,'field',phyto(SMALL)%f_mu_mem ,isd,jsd)

    ! CAS calculate totals after source/sinks have been applied
    imbal_flag = 0;
    stdoutunit = stdout();
    allocate(post_totn(isc:iec,jsc:jec,1:nk))
    allocate(post_totc(isc:iec,jsc:jec,1:nk))
    allocate(post_totp(isc:iec,jsc:jec,1:nk))
    allocate(post_totsi(isc:iec,jsc:jec,1:nk))
    allocate(post_totfe(isc:iec,jsc:jec,1:nk))
    do k = 1, nk ; do j = jsc, jec ; do i = isc, iec  !{
         post_totn(i,j,k) = (cobalt%p_no3(i,j,k,tau) + cobalt%p_nh4(i,j,k,tau) + &
                    cobalt%p_ndi(i,j,k,tau) + cobalt%p_nlg(i,j,k,tau) + cobalt%p_nmd(i,j,k,tau) + &
                    cobalt%p_nsm(i,j,k,tau) + cobalt%p_nbact(i,j,k,tau) + &
                    cobalt%p_ldon(i,j,k,tau) + cobalt%p_sldon(i,j,k,tau) + &
                    cobalt%p_srdon(i,j,k,tau) +  cobalt%p_ndet(i,j,k,tau) + &
                    cobalt%p_nsmz(i,j,k,tau) + cobalt%p_nmdz(i,j,k,tau) + &
                    cobalt%p_nlgz(i,j,k,tau))*grid_tmask(i,j,k)
         imbal = (post_totn(i,j,k) - pre_totn(i,j,k) - net_srcn(i,j,k))*86400.0/dt*1.03e6
         if (abs(imbal).gt.imbalance_tolerance) then
           call mpp_error(FATAL,&
           '==>biological source/sink imbalance (generic_COBALT_update_from_source): Nitrogen')
         endif

         post_totc(i,j,k) = (cobalt%p_dic(i,j,k,tau) + &
                    cobalt%p_cadet_arag(i,j,k,tau) + cobalt%p_cadet_calc(i,j,k,tau) + &
                    cobalt%c_2_n*(cobalt%p_ndi(i,j,k,tau) + cobalt%p_nlg(i,j,k,tau) + &
                    cobalt%p_nmd(i,j,k,tau) + cobalt%p_nsm(i,j,k,tau) + cobalt%p_nbact(i,j,k,tau) + &
                    cobalt%p_ldon(i,j,k,tau) + cobalt%p_sldon(i,j,k,tau) + &
                    cobalt%p_srdon(i,j,k,tau) +  cobalt%p_ndet(i,j,k,tau) + &
                    cobalt%p_nsmz(i,j,k,tau) + cobalt%p_nmdz(i,j,k,tau) + &
                    cobalt%p_nlgz(i,j,k,tau)))*grid_tmask(i,j,k)
        imbal = (post_totc(i,j,k) - pre_totc(i,j,k) - net_srcc(i,j,k))*86400.0/dt*1.03e6
         if (abs(imbal).gt.imbalance_tolerance) then
           call mpp_error(FATAL,&
           '==>biological source/sink imbalance (generic_COBALT_update_from_source): Carbon')
         endif

         post_totp(i,j,k) = (cobalt%p_po4(i,j,k,tau) + cobalt%p_pdi(i,j,k,tau) + &
                    cobalt%p_plg(i,j,k,tau) + cobalt%p_pmd(i,j,k,tau) + cobalt%p_psm(i,j,k,tau) + &
                    cobalt%p_ldop(i,j,k,tau) + cobalt%p_sldop(i,j,k,tau) + &
                    cobalt%p_srdop(i,j,k,tau) +  cobalt%p_pdet(i,j,k,tau) + &
                    cobalt%p_nsmz(i,j,k,tau)*zoo(1)%q_p_2_n + &
                    cobalt%p_nmdz(i,j,k,tau)*zoo(2)%q_p_2_n + &
                    cobalt%p_nlgz(i,j,k,tau)*zoo(3)%q_p_2_n + &
                    bact(1)%q_p_2_n*cobalt%p_nbact(i,j,k,tau))*grid_tmask(i,j,k)
         imbal = (post_totp(i,j,k) - pre_totp(i,j,k) - net_srcp(i,j,k))*86400.0/dt*1.03e6
         if (abs(imbal).gt.imbalance_tolerance) then
           call mpp_error(FATAL,&
           '==>biological source/sink imbalance (generic_COBALT_update_from_source): Phosphorus')
         endif

         post_totfe(i,j,k) = (cobalt%p_fed(i,j,k,tau) + cobalt%p_fedi(i,j,k,tau) + &
                    cobalt%p_felg(i,j,k,tau) + cobalt%p_femd(i,j,k,tau) + cobalt%p_fesm(i,j,k,tau) + &
                    cobalt%p_fedet(i,j,k,tau))*grid_tmask(i,j,k)
         imbal = (post_totfe(i,j,k) - pre_totfe(i,j,k) - net_srcfe(i,j,k))*86400.0/dt*1.03e6
         if (abs(imbal).gt.imbalance_tolerance) then
           call mpp_error(FATAL,&
           '==>biological source/sink imbalance (generic_COBALT_update_from_source): Iron')
         endif

         post_totsi(i,j,k) = (cobalt%p_sio4(i,j,k,tau) + cobalt%p_silg(i,j,k,tau) + &
                    cobalt%p_simd(i,j,k,tau) + cobalt%p_sidet(i,j,k,tau))*grid_tmask(i,j,k)
         imbal = (post_totsi(i,j,k) - pre_totsi(i,j,k))*86400.0/dt*1.03e6
         if (abs(imbal).gt.imbalance_tolerance) then
           call mpp_error(FATAL,&
           '==>biological source/sink imbalance (generic_COBALT_update_from_source): Silica')
         endif
    enddo; enddo ; enddo  !} i,j,k


    !
    !
    !-----------------------------------------------------------------------
    !       Save variables for diagnostics
    !-----------------------------------------------------------------------
    !

    do j = jsc, jec ; do i = isc, iec  !{
      if (grid_kmt(i,j) .gt. 0) then !{
        cobalt%o2min(i,j)=cobalt%p_o2(i,j,1,tau)
        cobalt%z_o2min(i,j)=cobalt%zt(i,j,1)
        cobalt%z_sat_arag(i,j)=missing_value1
        cobalt%z_sat_calc(i,j)=missing_value1
        cobalt%mask_z_sat_arag(i,j) = .FALSE.
        cobalt%mask_z_sat_calc(i,j) = .FALSE.
        if (cobalt%omega_arag(i,j,1) .le. 1.0) cobalt%z_sat_arag(i,j)=0.0
        if (cobalt%omega_calc(i,j,1) .le. 1.0) cobalt%z_sat_calc(i,j)=0.0
      endif !}
    enddo ; enddo  !} i,j,k
    do j = jsc, jec ; do i = isc, iec  !{
    first = .true.
      do k = 2, nk
         if (k .le. grid_kmt(i,j) .and. first) then !{
           if (cobalt%p_o2(i,j,k,tau) .lt. cobalt%p_o2(i,j,k-1,tau)) then
             cobalt%o2min(i,j)=cobalt%p_o2(i,j,k,tau)
             cobalt%z_o2min(i,j)=cobalt%zt(i,j,k)
           else
             first = .false.
           endif !}
         endif !}
      enddo;
    enddo ; enddo  !} i,j

    do k = 2, nk ; do j = jsc, jec ; do i = isc, iec  !{
      if (k .le. grid_kmt(i,j)) then !{
        if (cobalt%omega_arag(i,j,k) .le. 1.0 .and. cobalt%z_sat_arag(i,j) .lt. 0.0) then
          cobalt%z_sat_arag(i,j)=cobalt%zt(i,j,k)
          cobalt%mask_z_sat_arag(i,j) = .TRUE.
        endif
        if (cobalt%omega_calc(i,j,k) .le. 1.0 .and. cobalt%z_sat_calc(i,j) .lt. 0.0) then
          cobalt%z_sat_calc(i,j)=cobalt%zt(i,j,k)
          cobalt%mask_z_sat_calc(i,j) = .TRUE.
        endif
      endif !}
    enddo; enddo ; enddo  !} i,j,k

    ! Calculate the oxygen saturation using the relationships of Garcia and Gordon, 1992. Oxygen solubility in
    ! seawater: Better fitting equations.  Limnol. Oceanogr., 37(6), pp. 1307-1312.
    ! https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.4319/lo.1992.37.6.1307
    do k = 1, nk  ; do j = jsc, jec ; do i = isc, iec
       sal = min(42.0,max(0.0,Salt(i,j,k)))
       tt = 298.15 - min(40.0,max(0.0,Temp(i,j,k)))
       tkb = 273.15 + min(40.0,max(0.0,Temp(i,j,k)))
       ts = log(tt / tkb)
       ts2 = ts  * ts
       ts3 = ts2 * ts
       ts4 = ts3 * ts
       ts5 = ts4 * ts

       !The atmospheric code needs solubilities in units of mol/m3/atm
       cobalt%o2sat(i,j,k) = (1000.0/22391.6) * grid_tmask(i,j,1) *  & !convert from ml/l to mol m-3
            exp( cobalt%a_0 + cobalt%a_1*ts + cobalt%a_2*ts2 + cobalt%a_3*ts3 + cobalt%a_4*ts4 + cobalt%a_5*ts5 + &
            (cobalt%b_0 + cobalt%b_1*ts + cobalt%b_2*ts2 + cobalt%b_3*ts3 + cobalt%c_0*sal)*sal)

    enddo; enddo ; enddo  !} i,j,k

    !
    !---------------------------------------------------------------------
    ! Calculate total carbon  = Dissolved Inorganic Carbon + Phytoplankton Carbon
    !   + Dissolved Organic Carbon (including refractory) + Heterotrophic Biomass
    !   + Detrital Orgainc and Inorganic Carbon
    ! For the oceanic carbon budget, a constant 42 uM of dissolved organic
    ! carbon is added to represent the refractory component.
    ! For the oceanic nitrogen budget, a constant 2 uM of dissolved organic
    ! nitrogen is added to represent the refractory component.
    !---------------------------------------------------------------------
    !
    cobalt%tot_layer_int_c(:,:,:) = (cobalt%p_dic(:,:,:,tau) + cobalt%doc_background + cobalt%p_cadet_arag(:,:,:,tau) +&
         cobalt%p_cadet_calc(:,:,:,tau) + cobalt%c_2_n * (cobalt%p_ndi(:,:,:,tau) + cobalt%p_nlg(:,:,:,tau) +      &
         cobalt%p_nmd(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau) + cobalt%p_nbact(:,:,:,tau) + &
         cobalt%p_ldon(:,:,:,tau) + cobalt%p_sldon(:,:,:,tau) + cobalt%p_srdon(:,:,:,tau) +  &
         cobalt%p_ndet(:,:,:,tau) + cobalt%p_nsmz(:,:,:,tau) + cobalt%p_nmdz(:,:,:,tau) + &
         cobalt%p_nlgz(:,:,:,tau))) * rho_dzt(:,:,:)

    cobalt%tot_layer_int_fe(:,:,:) = (cobalt%p_fed(:,:,:,tau) + cobalt%p_fedi(:,:,:,tau) + &
         cobalt%p_felg(:,:,:,tau) + cobalt%p_femd(:,:,:,tau) + cobalt%p_fesm(:,:,:,tau) + &
         cobalt%p_fedet(:,:,:,tau)) * rho_dzt(:,:,:)

    cobalt%tot_layer_int_n(:,:,:) = (cobalt%p_no3(:,:,:,tau) + &
         cobalt%p_nh4(:,:,:,tau) + cobalt%p_ndi(:,:,:,tau) + cobalt%p_nlg(:,:,:,tau) + &
         cobalt%p_nsm(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) + cobalt%p_nbact(:,:,:,tau) + &
         cobalt%p_ldon(:,:,:,tau) + cobalt%p_sldon(:,:,:,tau) + cobalt%p_srdon(:,:,:,tau) +  cobalt%p_ndet(:,:,:,tau) + &
         cobalt%p_nsmz(:,:,:,tau) + cobalt%p_nmdz(:,:,:,tau) + cobalt%p_nlgz(:,:,:,tau)) * &
         rho_dzt(:,:,:)

    cobalt%tot_layer_int_p(:,:,:) = (cobalt%p_po4(:,:,:,tau) + &
         cobalt%p_pdi(:,:,:,tau) + cobalt%p_plg(:,:,:,tau) + &
         cobalt%p_pmd(:,:,:,tau) + cobalt%p_psm(:,:,:,tau) + &
         cobalt%p_ldop(:,:,:,tau) + cobalt%p_sldop(:,:,:,tau) + &
         cobalt%p_srdop(:,:,:,tau) + cobalt%p_pdet(:,:,:,tau) + &
         bact(1)%q_p_2_n*cobalt%p_nbact(:,:,:,tau) + zoo(1)%q_p_2_n*cobalt%p_nsmz(:,:,:,tau) +  &
         zoo(2)%q_p_2_n*cobalt%p_nmdz(:,:,:,tau) + zoo(3)%q_p_2_n*cobalt%p_nlgz(:,:,:,tau))  &
         * rho_dzt(:,:,:)

    cobalt%tot_layer_int_si(:,:,:) = (cobalt%p_sio4(:,:,:,tau) + cobalt%p_silg(:,:,:,tau) +   &
         cobalt%p_simd(:,:,:,tau) + cobalt%p_sidet(:,:,:,tau)) * rho_dzt(:,:,:)

    cobalt%tot_layer_int_o2(:,:,:) = cobalt%p_o2(:,:,:,tau)*rho_dzt(:,:,:)

    cobalt%tot_layer_int_alk(:,:,:) = cobalt%p_alk(:,:,:,tau)*rho_dzt(:,:,:)

    cobalt%tot_layer_int_dic(:,:,:) = cobalt%p_dic(:,:,:,tau)*rho_dzt(:,:,:)

    ! CAS: background excluded in accordance with CMIP6 request
    cobalt%tot_layer_int_doc(:,:,:) = cobalt%c_2_n * (cobalt%p_ldon(:,:,:,tau) + cobalt%p_sldon(:,:,:,tau) +  &
         cobalt%p_srdon(:,:,:,tau)) * rho_dzt(:,:,:)

   cobalt%tot_layer_int_poc(:,:,:) = (cobalt%p_ndi(:,:,:,tau) + cobalt%p_nlg(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau) + &
         cobalt%p_nbact(:,:,:,tau) + cobalt%p_ndet(:,:,:,tau) + cobalt%p_nsmz(:,:,:,tau) + cobalt%p_nmdz(:,:,:,tau) + &
         cobalt%p_nlgz(:,:,:,tau))*cobalt%c_2_n*rho_dzt(:,:,:)


    !
    !---------------------------------------------------------------------
    ! calculate water column vertical integrals for diagnostics
    !---------------------------------------------------------------------
    !
    do j = jsc, jec ; do i = isc, iec !{
       cobalt%wc_vert_int_c(i,j) = 0.0
       cobalt%wc_vert_int_dic(i,j) = 0.0
       cobalt%wc_vert_int_doc(i,j) = 0.0
       cobalt%wc_vert_int_poc(i,j) = 0.0
       cobalt%wc_vert_int_n(i,j) = 0.0
       cobalt%wc_vert_int_p(i,j) = 0.0
       cobalt%wc_vert_int_fe(i,j) = 0.0
       cobalt%wc_vert_int_si(i,j) = 0.0
       cobalt%wc_vert_int_o2(i,j) = 0.0
       cobalt%wc_vert_int_alk(i,j) = 0.0
       cobalt%wc_vert_int_npp(i,j) = 0.0
       cobalt%wc_vert_int_jdiss_sidet(i,j) = 0.0
       cobalt%wc_vert_int_jdiss_cadet(i,j) = 0.0
       cobalt%wc_vert_int_jo2resp(i,j) = 0.0
       cobalt%wc_vert_int_jprod_cadet(i,j) = 0.0
       cobalt%wc_vert_int_jno3denit(i,j) = 0.0
       cobalt%wc_vert_int_jprod_no3nitrif(i,j) = 0.0
       cobalt%wc_vert_int_juptake_nh4(i,j) = 0.0
       cobalt%wc_vert_int_jprod_nh4(i,j) = 0.0
       cobalt%wc_vert_int_juptake_no3(i,j) = 0.0
       cobalt%wc_vert_int_nfix(i,j) = 0.0
       cobalt%wc_vert_int_jfe_iceberg(i,j) = 0.0
       cobalt%wc_vert_int_jno3_iceberg(i,j) = 0.0
       cobalt%wc_vert_int_jpo4_iceberg(i,j) = 0.0
       cobalt%wc_vert_int_jnamx(i,j) = 0.0
    enddo; enddo !} i,j
    do j = jsc, jec ; do i = isc, iec ; do k = 1, nk  !{
          cobalt%wc_vert_int_c(i,j) = cobalt%wc_vert_int_c(i,j) + cobalt%tot_layer_int_c(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_dic(i,j) = cobalt%wc_vert_int_dic(i,j) + cobalt%tot_layer_int_dic(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_doc(i,j) = cobalt%wc_vert_int_doc(i,j) + cobalt%tot_layer_int_doc(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_poc(i,j) = cobalt%wc_vert_int_poc(i,j) + cobalt%tot_layer_int_poc(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_n(i,j) = cobalt%wc_vert_int_n(i,j) + cobalt%tot_layer_int_n(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_p(i,j) = cobalt%wc_vert_int_p(i,j) + cobalt%tot_layer_int_p(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_fe(i,j) = cobalt%wc_vert_int_fe(i,j) + cobalt%tot_layer_int_fe(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_si(i,j) = cobalt%wc_vert_int_si(i,j) + cobalt%tot_layer_int_si(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_o2(i,j) = cobalt%wc_vert_int_o2(i,j) + cobalt%tot_layer_int_o2(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_alk(i,j) = cobalt%wc_vert_int_alk(i,j) + cobalt%tot_layer_int_alk(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_npp(i,j) = cobalt%wc_vert_int_npp(i,j) + (phyto(SMALL)%jprod_n(i,j,k) +  &
              phyto(MEDIUM)%jprod_n(i,j,k) + phyto(LARGE)%jprod_n(i,j,k) + phyto(DIAZO)%jprod_n(i,j,k))* &
              rho_dzt(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_jdiss_sidet(i,j) = cobalt%wc_vert_int_jdiss_sidet(i,j) +                 &
             cobalt%jdiss_sidet(i,j,k) * rho_dzt(i,j,k) * grid_tmask(i,j,k)
          cobalt%wc_vert_int_jdiss_cadet(i,j) = cobalt%wc_vert_int_jdiss_cadet(i,j) +                 &
             (cobalt%jdiss_cadet_calc(i,j,k)+cobalt%jdiss_cadet_arag(i,j,k))*rho_dzt(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_jo2resp(i,j) = cobalt%wc_vert_int_jo2resp(i,j) +                         &
             cobalt%jo2resp_wc(i,j,k) * rho_dzt(i,j,k) * grid_tmask(i,j,k)
          cobalt%wc_vert_int_jprod_cadet(i,j) = cobalt%wc_vert_int_jprod_cadet(i,j) +                 &
             (cobalt%jprod_cadet_calc(i,j,k)+cobalt%jprod_cadet_arag(i,j,k))*rho_dzt(i,j,k)*grid_tmask(i,j,k)
          cobalt%wc_vert_int_jno3denit(i,j) = cobalt%wc_vert_int_jno3denit(i,j) +                     &
             cobalt%jno3denit_wc(i,j,k) * rho_dzt(i,j,k) * grid_tmask(i,j,k)
          cobalt%wc_vert_int_jprod_no3nitrif(i,j) = cobalt%wc_vert_int_jprod_no3nitrif(i,j) +                         &
             cobalt%jprod_no3nitrif(i,j,k) * rho_dzt(i,j,k) * grid_tmask(i,j,k)
          cobalt%wc_vert_int_juptake_nh4(i,j) = cobalt%wc_vert_int_juptake_nh4(i,j) +                     &
             (phyto(SMALL)%juptake_nh4(i,j,k)+phyto(MEDIUM)%juptake_nh4(i,j,k)+ &
              phyto(LARGE)%juptake_nh4(i,j,k)+phyto(DIAZO)%juptake_nh4(i,j,k) )* &
             rho_dzt(i,j,k) * grid_tmask(i,j,k)

          cobalt%wc_vert_int_jprod_nh4(i,j) = cobalt%wc_vert_int_jprod_nh4(i,j) +                     &
             cobalt%jprod_nh4(i,j,k)*rho_dzt(i,j,k) * grid_tmask(i,j,k)

          cobalt%wc_vert_int_juptake_no3(i,j) = cobalt%wc_vert_int_juptake_no3(i,j) +                     &
             (phyto(DIAZO)%juptake_no3(i,j,k)+phyto(LARGE)%juptake_no3(i,j,k)+ &
              phyto(MEDIUM)%juptake_no3(i,j,k)+phyto(SMALL)%juptake_no3(i,j,k))* &
             rho_dzt(i,j,k) * grid_tmask(i,j,k)
          cobalt%wc_vert_int_nfix(i,j) = cobalt%wc_vert_int_nfix(i,j) + phyto(DIAZO)%juptake_n2(i,j,k) *&
             rho_dzt(i,j,k) * grid_tmask(i,j,k)
          cobalt%wc_vert_int_jnamx(i,j)=cobalt%wc_vert_int_jnamx(i,j)+cobalt%jnamx(i,j,k)* &
             rho_dzt(i,j,k) * grid_tmask(i,j,k)

          cobalt%wc_vert_int_jfe_iceberg(i,j) = cobalt%wc_vert_int_jfe_iceberg(i,j) + cobalt%jfe_iceberg(i,j,k) *&
             rho_dzt(i,j,k) * grid_tmask(i,j,k)
          cobalt%wc_vert_int_jno3_iceberg(i,j) = cobalt%wc_vert_int_jno3_iceberg(i,j) + cobalt%jno3_iceberg(i,j,k) *&
             rho_dzt(i,j,k) * grid_tmask(i,j,k)
          cobalt%wc_vert_int_jpo4_iceberg(i,j) = cobalt%wc_vert_int_jpo4_iceberg(i,j) + cobalt%jpo4_iceberg(i,j,k) *&
             rho_dzt(i,j,k) * grid_tmask(i,j,k)

    enddo; enddo; enddo  !} i,j,k
    !
    !---------------------------------------------------------------------
    ! Add external bottom fluxes to specific rates
    !---------------------------------------------------------------------
    !
    do j = jsc, jec ; do i = isc, iec ; do k = 1, nk  !{
       ! CAS added calcite and aragonite redisolution terms
       cobalt%jdiss_cadet_calc_plus_btm(i,j,k)  = cobalt%jdiss_cadet_calc(i,j,k)
       cobalt%jdiss_cadet_arag_plus_btm(i,j,k)  = cobalt%jdiss_cadet_arag(i,j,k)
       ! CAS added a jprod_nh4_plus_btm for remoc CMIP variable
       cobalt%jprod_nh4_plus_btm(i,j,k) = cobalt%jprod_nh4(i,j,k)
       cobalt%jalk_plus_btm(i,j,k)  = cobalt%jalk(i,j,k)
       cobalt%jdic_plus_btm(i,j,k)  = cobalt%jdic(i,j,k)
       cobalt%jfed_plus_btm(i,j,k)  = cobalt%jfed(i,j,k)
       cobalt%jnh4_plus_btm(i,j,k)  = cobalt%jnh4(i,j,k)
       cobalt%jno3_plus_btm(i,j,k)  = cobalt%jno3(i,j,k)
       cobalt%jo2_plus_btm(i,j,k)   = cobalt%jo2(i,j,k)
       cobalt%jpo4_plus_btm(i,j,k)  = cobalt%jpo4(i,j,k)
       cobalt%jsio4_plus_btm(i,j,k) = cobalt%jsio4(i,j,k)
       cobalt%jdin_plus_btm(i,j,k)  = cobalt%jno3(i,j,k) + cobalt%jnh4(i,j,k)
    enddo; enddo; enddo  !} i,j,k

    do j = jsc, jec ; do i = isc, iec  !{
      k = grid_kmt(i,j)
      rho_dzt_bot(i,j) = 0.0
      if (k .gt. 0) then !{
        do k = grid_kmt(i,j),1,-1   !{
          if (rho_dzt_bot(i,j).lt.cobalt%Rho_0*cobalt%bottom_thickness) then
            rho_dzt_bot(i,j) = rho_dzt_bot(i,j) + rho_dzt(i,j,k)
            k_bot(i,j) = k
          endif
        enddo
      endif
    enddo; enddo

    do j = jsc, jec ; do i = isc, iec  !{
      k = grid_kmt(i,j)
      if (k .gt. 0) then !{
        do k = grid_kmt(i,j),k_bot(i,j),-1   !{
          cobalt%jdiss_cadet_calc_plus_btm(i,j,k) = cobalt%jdiss_cadet_calc(i,j,k) +  &
            cobalt%fcased_redis(i,j) / rho_dzt_bot(i,j)
          cobalt%jdiss_cadet_arag_plus_btm(i,j,k)  = cobalt%jdiss_cadet_arag(i,j,k) +  &
            cobalt%f_cadet_arag_btf(i,j,1) / rho_dzt_bot(i,j)
          cobalt%jprod_nh4_plus_btm(i,j,k) = cobalt%jprod_nh4(i,j,k) + &
            (cobalt%f_ndet_btf(i,j,1) - cobalt%fn_burial(i,j)) / rho_dzt_bot(i,j)
          cobalt%jalk_plus_btm(i,j,k) = cobalt%jalk(i,j,k) +                       &
            (2.0 * (cobalt%fcased_redis(i,j) + cobalt%f_cadet_arag_btf(i,j,1)) +    &
            cobalt%f_ndet_btf(i,j,1) + cobalt%alk_2_n_denit * cobalt%fno3denit_sed(i,j)) / &
            rho_dzt_bot(i,j)
          cobalt%jdic_plus_btm(i,j,k) = cobalt%jdic(i,j,k) +                       &
             (cobalt%fcased_redis(i,j) + cobalt%f_cadet_arag_btf(i,j,1) +            &
             ((cobalt%f_ndet_btf(i,j,1) - cobalt%fn_burial(i,j)) * cobalt%c_2_n)) / &
             rho_dzt_bot(i,j)
          cobalt%jfed_plus_btm(i,j,k) = cobalt%jfed(i,j,k) + &
             (cobalt%ffe_sed(i,j)+cobalt%ffe_geotherm(i,j)) / rho_dzt_bot(i,j)
          cobalt%jnh4_plus_btm(i,j,k) = cobalt%jnh4(i,j,k) + &
             (cobalt%f_ndet_btf(i,j,1) - cobalt%fn_burial(i,j)) / rho_dzt_bot(i,j)
          cobalt%jno3_plus_btm(i,j,k) = cobalt%jno3(i,j,k) - &
             cobalt%fno3denit_sed(i,j) / rho_dzt_bot(i,j)
          cobalt%jo2_plus_btm(i,j,k) = cobalt%jo2(i,j,k) + &
             (cobalt%o2_2_nh4 * (cobalt%fnoxic_sed(i,j) + cobalt%fnfeso4red_sed(i,j))) / &
             rho_dzt_bot(i,j)
          cobalt%jpo4_plus_btm(i,j,k) = cobalt%jpo4(i,j,k) + &
             (cobalt%f_pdet_btf(i,j,1) - cobalt%fp_burial(i,j)) / rho_dzt_bot(i,j)
          cobalt%jsio4_plus_btm(i,j,k) = cobalt%jsio4(i,j,k) + &
             cobalt%f_sidet_btf(i,j,1) / rho_dzt_bot(i,j)
          cobalt%jdin_plus_btm(i,j,k)  = cobalt%jno3_plus_btm(i,j,k) + &
             cobalt%jnh4_plus_btm(i,j,k)
        enddo
      endif
    enddo; enddo

!****************************************************************************************************

    allocate(rho_dzt_100(isc:iec,jsc:jec))
    !
    !---------------------------------------------------------------------
    ! calculate upper 100 m vertical integrals
    !---------------------------------------------------------------------
    !
    do j = jsc, jec ; do i = isc, iec !{
       rho_dzt_100(i,j) = rho_dzt(i,j,1)
       cobalt%f_alk_int_100(i,j) = cobalt%p_alk(i,j,1,tau) * rho_dzt(i,j,1)
       cobalt%f_dic_int_100(i,j) = cobalt%p_dic(i,j,1,tau) * rho_dzt(i,j,1)
       cobalt%f_din_int_100(i,j) = (cobalt%p_no3(i,j,1,tau) + cobalt%p_nh4(i,j,1,tau)) * rho_dzt(i,j,1)
       cobalt%f_fed_int_100(i,j) = cobalt%p_fed(i,j,1,tau) * rho_dzt(i,j,1)
       cobalt%f_po4_int_100(i,j) = cobalt%p_po4(i,j,1,tau) * rho_dzt(i,j,1)
       cobalt%f_sio4_int_100(i,j) = cobalt%p_sio4(i,j,1,tau) * rho_dzt(i,j,1)
       cobalt%jalk_100(i,j) = cobalt%jalk(i,j,1) * rho_dzt(i,j,1)
       cobalt%jdic_100(i,j) = cobalt%jdic(i,j,1) * rho_dzt(i,j,1)
       cobalt%jdin_100(i,j) = (cobalt%jno3(i,j,1) + cobalt%jnh4(i,j,1)) * rho_dzt(i,j,1)
       cobalt%jfed_100(i,j) = cobalt%jfed(i,j,1) * rho_dzt(i,j,1)
       cobalt%jpo4_100(i,j) = cobalt%jpo4(i,j,1) * rho_dzt(i,j,1)
       cobalt%jsio4_100(i,j) = cobalt%jsio4(i,j,1) * rho_dzt(i,j,1)
       cobalt%jprod_ptot_100(i,j) = cobalt%jprod_po4(i,j,1) * rho_dzt(i,j,1)
       do n = 1, NUM_PHYTO  !{
          phyto(n)%jprod_n_100(i,j) = phyto(n)%jprod_n(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%jprod_n_new_100(i,j) = phyto(n)%juptake_no3(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%jzloss_n_100(i,j) = phyto(n)%jzloss_n(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%jexuloss_n_100(i,j) = phyto(n)%jexuloss_n(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%f_n_100(i,j) = phyto(n)%f_n(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%juptake_fe_100(i,j) = phyto(n)%juptake_fe(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%juptake_po4_100(i,j) = phyto(n)%juptake_po4(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%jvirloss_n_100(i,j) = phyto(n)%jvirloss_n(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%jmortloss_n_100(i,j) = phyto(n)%jmortloss_n(i,j,1) * rho_dzt(i,j,1)
          phyto(n)%jaggloss_n_100(i,j) = phyto(n)%jaggloss_n(i,j,1) * rho_dzt(i,j,1)
       enddo   !} n
       phyto(DIAZO)%jprod_n_n2_100(i,j) = phyto(DIAZO)%juptake_n2(i,j,1) * rho_dzt(i,j,1)
       cobalt%jprod_diat_100(i,j) = (phyto(LARGE)%jprod_n(i,j,1)*phyto(LARGE)%silim(i,j,1) + &
                             phyto(MEDIUM)%jprod_n(i,j,1)*phyto(MEDIUM)%silim(i,j,1)) *rho_dzt(i,j,1)
       phyto(LARGE)%juptake_sio4_100(i,j) = phyto(LARGE)%juptake_sio4(i,j,1) * rho_dzt(i,j,1)
       do n = 1, NUM_ZOO  !{
          zoo(n)%jprod_n_100(i,j) = zoo(n)%jprod_n(i,j,1) * rho_dzt(i,j,1)
          zoo(n)%jingest_n_100(i,j) = zoo(n)%jingest_n(i,j,1) * rho_dzt(i,j,1)
          zoo(n)%jremin_n_100(i,j) = zoo(n)%jprod_nh4(i,j,1) * rho_dzt(i,j,1)
          zoo(n)%f_n_100(i,j) = zoo(n)%f_n(i,j,1) * rho_dzt(i,j,1)
       enddo   !} n

       do n = 1,2  !{
          zoo(n)%jzloss_n_100(i,j) = zoo(n)%jzloss_n(i,j,1) * rho_dzt(i,j,1)
          zoo(n)%jprod_don_100(i,j) = (zoo(n)%jprod_ldon(i,j,1) + zoo(n)%jprod_sldon(i,j,1) + &
             zoo(n)%jprod_srdon(i,j,1))  * rho_dzt(i,j,1)
       enddo   !} n

       do n = 2,3  !{
          zoo(n)%jhploss_n_100(i,j) = zoo(n)%jhploss_n(i,j,1) * rho_dzt(i,j,1)
          zoo(n)%jprod_ndet_100(i,j) = zoo(n)%jprod_ndet(i,j,1) * rho_dzt(i,j,1)
       enddo   !} n

       cobalt%hp_jingest_n_100(i,j) = cobalt%hp_jingest_n(i,j,1)*rho_dzt(i,j,1)
       cobalt%hp_jremin_n_100(i,j) =  cobalt%hp_jingest_n(i,j,1)*rho_dzt(i,j,1)*(1.0-cobalt%hp_phi_det)
       cobalt%hp_jprod_ndet_100(i,j) =  cobalt%hp_jingest_n(i,j,1)*rho_dzt(i,j,1)*cobalt%hp_phi_det

       bact(1)%jprod_n_100(i,j) = bact(1)%jprod_n(i,j,1) * rho_dzt(i,j,1)
       bact(1)%jzloss_n_100(i,j) = bact(1)%jzloss_n(i,j,1) * rho_dzt(i,j,1)
       bact(1)%jvirloss_n_100(i,j) = bact(1)%jvirloss_n(i,j,1) * rho_dzt(i,j,1)
       bact(1)%jremin_n_100(i,j) = bact(1)%jprod_nh4(i,j,1) * rho_dzt(i,j,1)
       bact(1)%juptake_ldon_100(i,j) = bact(1)%juptake_ldon(i,j,1) * rho_dzt(i,j,1)
       bact(1)%f_n_100(i,j) = bact(1)%f_n(i,j,1) * rho_dzt(i,j,1)

       cobalt%jprod_lithdet_100(i,j) = cobalt%jprod_lithdet(i,j,1) * rho_dzt(i,j,1)
       cobalt%jprod_sidet_100(i,j) = cobalt%jprod_sidet(i,j,1) * rho_dzt(i,j,1)
       cobalt%jprod_cadet_calc_100(i,j) = cobalt%jprod_cadet_calc(i,j,1) * rho_dzt(i,j,1)
       cobalt%jprod_cadet_arag_100(i,j) = cobalt%jprod_cadet_arag(i,j,1) * rho_dzt(i,j,1)
       cobalt%jremin_ndet_100(i,j) = cobalt%jremin_ndet(i,j,1) * rho_dzt(i,j,1)

       cobalt%f_ndet_100(i,j) = cobalt%f_ndet(i,j,1)*rho_dzt(i,j,1)
       cobalt%f_don_100(i,j) = (cobalt%f_ldon(i,j,1)+cobalt%f_sldon(i,j,1)+cobalt%f_srdon(i,j,1))* &
           rho_dzt(i,j,1)
       cobalt%f_silg_100(i,j) = cobalt%f_silg(i,j,1)*rho_dzt(i,j,1)
       cobalt%f_simd_100(i,j) = cobalt%f_simd(i,j,1)*rho_dzt(i,j,1)

       cobalt%fndet_100(i,j) = cobalt%f_ndet(i,j,1) * cobalt%Rho_0 * cobalt%wsink
       cobalt%fpdet_100(i,j) = cobalt%f_pdet(i,j,1) * cobalt%Rho_0 * cobalt%wsink
       cobalt%ffedet_100(i,j) = cobalt%f_fedet(i,j,1) * cobalt%Rho_0 * cobalt%wsink
       cobalt%flithdet_100(i,j) = cobalt%f_lithdet(i,j,1) * cobalt%Rho_0 * cobalt%wsink
       cobalt%fsidet_100(i,j) = cobalt%f_sidet(i,j,1) * cobalt%Rho_0 * cobalt%wsink
       cobalt%fcadet_arag_100(i,j) = cobalt%f_cadet_arag(i,j,1) * cobalt%Rho_0 * cobalt%wsink
       cobalt%fcadet_calc_100(i,j) = cobalt%f_cadet_calc(i,j,1) * cobalt%Rho_0 * cobalt%wsink
       cobalt%fntot_100(i,j) = (cobalt%f_ndet(i,j,1)*cobalt%wsink + &
         phyto(SMALL)%f_n(i,j,1)*phyto(SMALL)%vmove(i,j,1) + &
         phyto(MEDIUM)%f_n(i,j,1)*phyto(MEDIUM)%vmove(i,j,1) + &
         phyto(LARGE)%f_n(i,j,1)*phyto(LARGE)%vmove(i,j,1) + &
         phyto(DIAZO)%f_n(i,j,1)*phyto(DIAZO)%vmove(i,j,1))*cobalt%Rho_0
       cobalt%fptot_100(i,j) = (cobalt%f_pdet(i,j,1)*cobalt%wsink + &
         phyto(SMALL)%f_p(i,j,1)*phyto(SMALL)%vmove(i,j,1) + &
         phyto(MEDIUM)%f_p(i,j,1)*phyto(MEDIUM)%vmove(i,j,1) + &
         phyto(LARGE)%f_p(i,j,1)*phyto(LARGE)%vmove(i,j,1) + &
         phyto(DIAZO)%f_p(i,j,1)*phyto(DIAZO)%vmove(i,j,1))*cobalt%Rho_0
       cobalt%ffetot_100(i,j) = (cobalt%f_fedet(i,j,1)*cobalt%wsink + &
         phyto(SMALL)%f_fe(i,j,1)*phyto(SMALL)%vmove(i,j,1) + &
         phyto(MEDIUM)%f_fe(i,j,1)*phyto(MEDIUM)%vmove(i,j,1) + &
         phyto(LARGE)%f_fe(i,j,1)*phyto(LARGE)%vmove(i,j,1) + &
         phyto(DIAZO)%f_fe(i,j,1)*phyto(DIAZO)%vmove(i,j,1))*cobalt%Rho_0
       cobalt%fsitot_100(i,j) = (cobalt%f_sidet(i,j,1)*cobalt%wsink + &
         cobalt%f_simd(i,j,1)*phyto(MEDIUM)%vmove(i,j,1) + &
         cobalt%f_silg(i,j,1)*phyto(LARGE)%vmove(i,j,1))*cobalt%Rho_0
    enddo; enddo !} i,j

    do j = jsc, jec ; do i = isc, iec ; !{
       k_100 = 1
       do k = 2, grid_kmt(i,j)  !{
          if (rho_dzt_100(i,j) .lt. cobalt%Rho_0 * 100.0) then
             k_100 = k
             rho_dzt_100(i,j) = rho_dzt_100(i,j) + rho_dzt(i,j,k)
             cobalt%f_alk_int_100(i,j) = cobalt%f_alk_int_100(i,j) + cobalt%p_alk(i,j,k,tau) * rho_dzt(i,j,k)
             cobalt%f_dic_int_100(i,j) = cobalt%f_dic_int_100(i,j) + cobalt%p_dic(i,j,k,tau) * rho_dzt(i,j,k)
             cobalt%f_din_int_100(i,j) = cobalt%f_din_int_100(i,j) + (cobalt%p_no3(i,j,k,tau) +        &
                cobalt%p_nh4(i,j,k,tau)) * rho_dzt(i,j,k)
             cobalt%f_fed_int_100(i,j) = cobalt%f_fed_int_100(i,j) + cobalt%p_fed(i,j,k,tau) * rho_dzt(i,j,k)
             cobalt%f_po4_int_100(i,j) = cobalt%f_po4_int_100(i,j) + cobalt%p_po4(i,j,k,tau) * rho_dzt(i,j,k)
             cobalt%f_sio4_int_100(i,j) = cobalt%f_sio4_int_100(i,j) + cobalt%p_sio4(i,j,k,tau) *  rho_dzt(i,j,k)
             cobalt%jalk_100(i,j) = cobalt%jalk_100(i,j) + cobalt%jalk(i,j,k) * rho_dzt(i,j,k)
             cobalt%jdic_100(i,j) = cobalt%jdic_100(i,j) + cobalt%jdic(i,j,k) * rho_dzt(i,j,k)
             cobalt%jdin_100(i,j) = cobalt%jdin_100(i,j) + (cobalt%jno3(i,j,k) + cobalt%jnh4(i,j,k)) * rho_dzt(i,j,k)
             cobalt%jfed_100(i,j) = cobalt%jfed_100(i,j) + cobalt%jfed(i,j,k) * rho_dzt(i,j,k)
             cobalt%jpo4_100(i,j) = cobalt%jpo4_100(i,j) + cobalt%jpo4(i,j,k) * rho_dzt(i,j,k)
             cobalt%jsio4_100(i,j) = cobalt%jsio4_100(i,j) + cobalt%jsio4(i,j,k) * rho_dzt(i,j,k)
             cobalt%jprod_ptot_100(i,j) = cobalt%jprod_ptot_100(i,j) + cobalt%jprod_po4(i,j,k) * rho_dzt(i,j,k)

             do n = 1, NUM_PHYTO !{
                phyto(n)%jprod_n_100(i,j) = phyto(n)%jprod_n_100(i,j) + phyto(n)%jprod_n(i,j,k)* &
                   rho_dzt(i,j,k)
                phyto(n)%jprod_n_new_100(i,j) = phyto(n)%jprod_n_new_100(i,j) + phyto(n)%juptake_no3(i,j,k)* &
                   rho_dzt(i,j,k)
                phyto(n)%jzloss_n_100(i,j) = phyto(n)%jzloss_n_100(i,j) + phyto(n)%jzloss_n(i,j,k)* &
                   rho_dzt(i,j,k)
                phyto(n)%jexuloss_n_100(i,j) = phyto(n)%jexuloss_n_100(i,j) + phyto(n)%jexuloss_n(i,j,k)* &
                   rho_dzt(i,j,k)
                phyto(n)%jvirloss_n_100(i,j) = phyto(n)%jvirloss_n_100(i,j) + phyto(n)%jvirloss_n(i,j,k)* &
                   rho_dzt(i,j,k)
                phyto(n)%jmortloss_n_100(i,j) = phyto(n)%jmortloss_n_100(i,j) + phyto(n)%jmortloss_n(i,j,k)* &
                   rho_dzt(i,j,k)
                phyto(n)%jaggloss_n_100(i,j) = phyto(n)%jaggloss_n_100(i,j) + phyto(n)%jaggloss_n(i,j,k)* &
                   rho_dzt(i,j,k)
                phyto(n)%f_n_100(i,j) = phyto(n)%f_n_100(i,j) + phyto(n)%f_n(i,j,k)*rho_dzt(i,j,k)
                phyto(n)%juptake_fe_100(i,j) = phyto(n)%juptake_fe_100(i,j) + phyto(n)%juptake_fe(i,j,k)*rho_dzt(i,j,k)
                phyto(n)%juptake_po4_100(i,j) = phyto(n)%juptake_po4_100(i,j) + phyto(n)%juptake_po4(i,j,k)*rho_dzt(i,j,k)
             enddo !} n
             phyto(DIAZO)%jprod_n_n2_100(i,j) = phyto(DIAZO)%jprod_n_n2_100(i,j) + &
                 phyto(DIAZO)%juptake_n2(i,j,k)*rho_dzt(i,j,k)
             cobalt%jprod_diat_100(i,j) = cobalt%jprod_diat_100(i,j) + &
               (phyto(LARGE)%jprod_n(i,j,k)*phyto(LARGE)%silim(i,j,k) + &
                phyto(MEDIUM)%jprod_n(i,j,k)*phyto(MEDIUM)%silim(i,j,k))*rho_dzt(i,j,k)
             phyto(LARGE)%juptake_sio4_100(i,j) = phyto(LARGE)%juptake_sio4_100(i,j) + &
                 phyto(LARGE)%juptake_sio4(i,j,k)*rho_dzt(i,j,k)
             phyto(MEDIUM)%juptake_sio4_100(i,j) = phyto(MEDIUM)%juptake_sio4_100(i,j) + &
                 phyto(MEDIUM)%juptake_sio4(i,j,k)*rho_dzt(i,j,k)

             do n = 1, NUM_ZOO !{
                zoo(n)%jprod_n_100(i,j) = zoo(n)%jprod_n_100(i,j) + zoo(n)%jprod_n(i,j,k)* &
                   rho_dzt(i,j,k)
                zoo(n)%jingest_n_100(i,j) = zoo(n)%jingest_n_100(i,j) + zoo(n)%jingest_n(i,j,k)* &
                   rho_dzt(i,j,k)
                zoo(n)%jremin_n_100(i,j) = zoo(n)%jremin_n_100(i,j) + zoo(n)%jprod_nh4(i,j,k)* &
                   rho_dzt(i,j,k)
                zoo(n)%f_n_100(i,j) = zoo(n)%f_n_100(i,j) + zoo(n)%f_n(i,j,k)*rho_dzt(i,j,k)
             enddo !} n

             do n = 1,2 !{
                zoo(n)%jzloss_n_100(i,j) = zoo(n)%jzloss_n_100(i,j) + zoo(n)%jzloss_n(i,j,k)* &
                   rho_dzt(i,j,k)
                zoo(n)%jprod_don_100(i,j) = zoo(n)%jprod_don_100(i,j) + (zoo(n)%jprod_ldon(i,j,k) + &
                   zoo(n)%jprod_sldon(i,j,k) + zoo(n)%jprod_srdon(i,j,k))*rho_dzt(i,j,k)
             enddo !} n

             do n = 2,3 !{
                zoo(n)%jhploss_n_100(i,j) = zoo(n)%jhploss_n_100(i,j) + zoo(n)%jhploss_n(i,j,k)* &
                   rho_dzt(i,j,k)
                zoo(n)%jprod_ndet_100(i,j) = zoo(n)%jprod_ndet_100(i,j) + zoo(n)%jprod_ndet(i,j,k)* &
                   rho_dzt(i,j,k)
             enddo !} n

             cobalt%hp_jingest_n_100(i,j) = cobalt%hp_jingest_n_100(i,j) + cobalt%hp_jingest_n(i,j,k)* &
                 rho_dzt(i,j,k)
             cobalt%hp_jremin_n_100(i,j) = cobalt%hp_jremin_n_100(i,j) + cobalt%hp_jingest_n(i,j,k)* &
                 (1.0-cobalt%hp_phi_det)*rho_dzt(i,j,k)
             cobalt%hp_jprod_ndet_100(i,j) = cobalt%hp_jprod_ndet_100(i,j) + cobalt%hp_jingest_n(i,j,k)* &
                 cobalt%hp_phi_det*rho_dzt(i,j,k)

             bact(1)%jprod_n_100(i,j) = bact(1)%jprod_n_100(i,j) + bact(1)%jprod_n(i,j,k) * rho_dzt(i,j,k)
             bact(1)%jzloss_n_100(i,j) = bact(1)%jzloss_n_100(i,j) + bact(1)%jzloss_n(i,j,k) * rho_dzt(i,j,k)
             bact(1)%jvirloss_n_100(i,j) = bact(1)%jvirloss_n_100(i,j) + bact(1)%jvirloss_n(i,j,k) * rho_dzt(i,j,k)
             bact(1)%jremin_n_100(i,j) = bact(1)%jremin_n_100(i,j) + bact(1)%jprod_nh4(i,j,k) * rho_dzt(i,j,k)
             bact(1)%juptake_ldon_100(i,j) = bact(1)%juptake_ldon_100(i,j) + bact(1)%juptake_ldon(i,j,k) * rho_dzt(i,j,k)
             bact(1)%f_n_100(i,j) = bact(1)%f_n_100(i,j) + bact(1)%f_n(i,j,k)*rho_dzt(i,j,k)

             cobalt%jprod_lithdet_100(i,j) = cobalt%jprod_lithdet_100(i,j) + cobalt%jprod_lithdet(i,j,k) * rho_dzt(i,j,k)
             cobalt%jprod_sidet_100(i,j) = cobalt%jprod_sidet_100(i,j) + cobalt%jprod_sidet(i,j,k) * rho_dzt(i,j,k)
             cobalt%jprod_cadet_calc_100(i,j) = cobalt%jprod_cadet_calc_100(i,j) + cobalt%jprod_cadet_calc(i,j,k) * rho_dzt(i,j,k)
             cobalt%jprod_cadet_arag_100(i,j) = cobalt%jprod_cadet_arag_100(i,j) + cobalt%jprod_cadet_arag(i,j,k) * rho_dzt(i,j,k)
             cobalt%jremin_ndet_100(i,j) = cobalt%jremin_ndet_100(i,j) + cobalt%jremin_ndet(i,j,k) * rho_dzt(i,j,k)
             cobalt%f_ndet_100(i,j) = cobalt%f_ndet_100(i,j) + cobalt%f_ndet(i,j,k)*rho_dzt(i,j,k)
             cobalt%f_don_100(i,j) = cobalt%f_don_100(i,j) + (cobalt%f_ldon(i,j,k) + cobalt%f_sldon(i,j,k) + &
                cobalt%f_srdon(i,j,k))*rho_dzt(i,j,k)
             cobalt%f_silg_100(i,j) = cobalt%f_silg_100(i,j) + cobalt%f_silg(i,j,k)*rho_dzt(i,j,k)

             cobalt%fndet_100(i,j) = cobalt%f_ndet(i,j,k) * cobalt%Rho_0 * cobalt%wsink
             cobalt%fpdet_100(i,j) = cobalt%f_pdet(i,j,k) * cobalt%Rho_0 * cobalt%wsink
             cobalt%ffedet_100(i,j) = cobalt%f_fedet(i,j,k) * cobalt%Rho_0 * cobalt%wsink
             cobalt%flithdet_100(i,j) = cobalt%f_lithdet(i,j,k) * cobalt%Rho_0 * cobalt%wsink
             cobalt%fsidet_100(i,j) = cobalt%f_sidet(i,j,k) * cobalt%Rho_0 * cobalt%wsink
             cobalt%fcadet_arag_100(i,j) = cobalt%f_cadet_arag(i,j,k) * cobalt%Rho_0 * cobalt%wsink
             cobalt%fcadet_calc_100(i,j) = cobalt%f_cadet_calc(i,j,k) * cobalt%Rho_0 * cobalt%wsink

             cobalt%fntot_100(i,j) = (cobalt%f_ndet(i,j,k)*cobalt%wsink + &
               phyto(SMALL)%f_n(i,j,k)*phyto(SMALL)%vmove(i,j,k) + &
               phyto(MEDIUM)%f_n(i,j,k)*phyto(MEDIUM)%vmove(i,j,k) + &
               phyto(LARGE)%f_n(i,j,k)*phyto(LARGE)%vmove(i,j,k) + &
               phyto(DIAZO)%f_n(i,j,k)*phyto(DIAZO)%vmove(i,j,k))*cobalt%Rho_0
             cobalt%fptot_100(i,j) = (cobalt%f_pdet(i,j,k)*cobalt%wsink + &
               phyto(SMALL)%f_p(i,j,k)*phyto(SMALL)%vmove(i,j,k) + &
               phyto(MEDIUM)%f_p(i,j,k)*phyto(MEDIUM)%vmove(i,j,k) + &
               phyto(LARGE)%f_p(i,j,k)*phyto(LARGE)%vmove(i,j,k) + &
               phyto(DIAZO)%f_p(i,j,k)*phyto(DIAZO)%vmove(i,j,k))*cobalt%Rho_0
             cobalt%ffetot_100(i,j) = (cobalt%f_fedet(i,j,k)*cobalt%wsink + &
               phyto(SMALL)%f_fe(i,j,k)*phyto(SMALL)%vmove(i,j,k) + &
               phyto(MEDIUM)%f_fe(i,j,k)*phyto(MEDIUM)%vmove(i,j,k) + &
               phyto(LARGE)%f_fe(i,j,k)*phyto(LARGE)%vmove(i,j,k) + &
               phyto(DIAZO)%f_fe(i,j,k)*phyto(DIAZO)%vmove(i,j,k))*cobalt%Rho_0
             cobalt%fsitot_100(i,j) = (cobalt%f_sidet(i,j,k)*cobalt%wsink + &
               cobalt%f_simd(i,j,k)*phyto(MEDIUM)%vmove(i,j,k) + &
               cobalt%f_silg(i,j,k)*phyto(LARGE)%vmove(i,j,k))*cobalt%Rho_0

          endif
       enddo  !} k

       if (k_100 .gt. 1 .and. k_100 .lt. grid_kmt(i,j)) then
          drho_dzt = cobalt%Rho_0 * 100.0 - rho_dzt_100(i,j)
          cobalt%f_alk_int_100(i,j) = cobalt%f_alk_int_100(i,j) + cobalt%p_alk(i,j,k_100,tau) * drho_dzt
          cobalt%f_dic_int_100(i,j) = cobalt%f_dic_int_100(i,j) + cobalt%p_dic(i,j,k_100,tau) * drho_dzt
          cobalt%f_din_int_100(i,j) = cobalt%f_din_int_100(i,j) + (cobalt%p_no3(i,j,k_100,tau) +       &
             cobalt%p_nh4(i,j,k_100,tau)) * drho_dzt
          cobalt%f_fed_int_100(i,j) = cobalt%f_fed_int_100(i,j) + cobalt%p_fed(i,j,k_100,tau) * drho_dzt
          cobalt%f_po4_int_100(i,j) = cobalt%f_po4_int_100(i,j) + cobalt%p_po4(i,j,k_100,tau) * drho_dzt
          cobalt%f_sio4_int_100(i,j) = cobalt%f_sio4_int_100(i,j) + cobalt%p_sio4(i,j,k_100,tau) * drho_dzt
          cobalt%jalk_100(i,j) = cobalt%jalk_100(i,j) + cobalt%jalk(i,j,k_100) * drho_dzt
          cobalt%jdic_100(i,j) = cobalt%jdic_100(i,j) + cobalt%jdic(i,j,k_100) * drho_dzt
          cobalt%jdin_100(i,j) = cobalt%jdin_100(i,j) + (cobalt%jno3(i,j,k_100) +  cobalt%jnh4(i,j,k_100)) * drho_dzt
          cobalt%jfed_100(i,j) = cobalt%jfed_100(i,j) + cobalt%jfed(i,j,k_100) * drho_dzt
          cobalt%jpo4_100(i,j) = cobalt%jpo4_100(i,j) + cobalt%jpo4(i,j,k_100) * drho_dzt
          cobalt%jsio4_100(i,j) = cobalt%jsio4_100(i,j) + cobalt%jsio4(i,j,k_100) * drho_dzt

          do n = 1, NUM_PHYTO !{
              phyto(n)%jprod_n_100(i,j) = phyto(n)%jprod_n_100(i,j) + phyto(n)%jprod_n(i,j,k_100)* &
                 drho_dzt
              phyto(n)%jprod_n_new_100(i,j) = phyto(n)%jprod_n_new_100(i,j) + phyto(n)%juptake_no3(i,j,k_100)* &
                 drho_dzt
              phyto(n)%jzloss_n_100(i,j) = phyto(n)%jzloss_n_100(i,j) + phyto(n)%jzloss_n(i,j,k_100)* &
                 drho_dzt
              phyto(n)%jexuloss_n_100(i,j) = phyto(n)%jexuloss_n_100(i,j) + phyto(n)%jexuloss_n(i,j,k_100)* &
                 drho_dzt
              phyto(n)%jvirloss_n_100(i,j) = phyto(n)%jvirloss_n_100(i,j) + phyto(n)%jvirloss_n(i,j,k_100)* &
                 drho_dzt
              phyto(n)%jmortloss_n_100(i,j) = phyto(n)%jmortloss_n_100(i,j) + phyto(n)%jmortloss_n(i,j,k_100)* &
                 drho_dzt
              phyto(n)%jaggloss_n_100(i,j) = phyto(n)%jaggloss_n_100(i,j) + phyto(n)%jaggloss_n(i,j,k_100)* &
                 drho_dzt
              phyto(n)%f_n_100(i,j) = phyto(n)%f_n_100(i,j) + phyto(n)%f_n(i,j,k_100)*drho_dzt
              phyto(n)%juptake_fe_100(i,j) = phyto(n)%juptake_fe_100(i,j) + phyto(n)%juptake_fe(i,j,k_100)*drho_dzt
              phyto(n)%juptake_po4_100(i,j) = phyto(n)%juptake_po4_100(i,j) + phyto(n)%juptake_po4(i,j,k_100)*drho_dzt
           enddo !} n
           phyto(DIAZO)%jprod_n_n2_100(i,j) = phyto(DIAZO)%jprod_n_n2_100(i,j) + &
               phyto(DIAZO)%juptake_n2(i,j,k_100)*drho_dzt
           cobalt%jprod_diat_100(i,j) = cobalt%jprod_diat_100(i,j) + &
                (phyto(LARGE)%jprod_n(i,j,k_100)*phyto(LARGE)%silim(i,j,k_100) + &
                 phyto(MEDIUM)%jprod_n(i,j,k_100)*phyto(MEDIUM)%silim(i,j,k_100))*drho_dzt
           phyto(LARGE)%juptake_sio4_100(i,j) = phyto(LARGE)%juptake_sio4_100(i,j) + &
               phyto(LARGE)%juptake_sio4(i,j,k_100)*drho_dzt
           phyto(MEDIUM)%juptake_sio4_100(i,j) = phyto(MEDIUM)%juptake_sio4_100(i,j) + &
               phyto(MEDIUM)%juptake_sio4(i,j,k_100)*drho_dzt

           do n = 1, NUM_ZOO !{
               zoo(n)%jprod_n_100(i,j) = zoo(n)%jprod_n_100(i,j) + zoo(n)%jprod_n(i,j,k_100)* &
                 drho_dzt
               zoo(n)%jingest_n_100(i,j) = zoo(n)%jingest_n_100(i,j) + zoo(n)%jingest_n(i,j,k_100)* &
                 drho_dzt
               zoo(n)%jremin_n_100(i,j) = zoo(n)%jremin_n_100(i,j) + zoo(n)%jprod_nh4(i,j,k_100)* &
                 drho_dzt
               zoo(n)%f_n_100(i,j) = zoo(n)%f_n_100(i,j) + zoo(n)%f_n(i,j,k_100)*drho_dzt
           enddo !} n

           do n = 1,2 !{
               zoo(n)%jzloss_n_100(i,j) = zoo(n)%jzloss_n_100(i,j) + zoo(n)%jzloss_n(i,j,k_100)* &
                 drho_dzt
               zoo(n)%jprod_don_100(i,j) = zoo(n)%jprod_don_100(i,j) + (zoo(n)%jprod_ldon(i,j,k_100) + &
                 zoo(n)%jprod_sldon(i,j,k_100) + zoo(n)%jprod_srdon(i,j,k_100))*drho_dzt
           enddo !} n

           do n = 2,3 !{
               zoo(n)%jhploss_n_100(i,j) = zoo(n)%jhploss_n_100(i,j) + zoo(n)%jhploss_n(i,j,k_100)* &
                 drho_dzt
               zoo(n)%jprod_ndet_100(i,j) = zoo(n)%jprod_ndet_100(i,j) + zoo(n)%jprod_ndet(i,j,k_100)* &
                 drho_dzt
           enddo !} n

           cobalt%hp_jingest_n_100(i,j) = cobalt%hp_jingest_n_100(i,j) + cobalt%hp_jingest_n(i,j,k_100)* &
               drho_dzt
           cobalt%hp_jremin_n_100(i,j) = cobalt%hp_jremin_n_100(i,j) + cobalt%hp_jingest_n(i,j,k_100)* &
               (1.0-cobalt%hp_phi_det)*drho_dzt
           cobalt%hp_jprod_ndet_100(i,j) = cobalt%hp_jprod_ndet_100(i,j) + cobalt%hp_jingest_n(i,j,k_100)* &
               cobalt%hp_phi_det*drho_dzt

           bact(1)%jprod_n_100(i,j) = bact(1)%jprod_n_100(i,j) + bact(1)%jprod_n(i,j,k_100)* &
                drho_dzt
           bact(1)%jzloss_n_100(i,j) = bact(1)%jzloss_n_100(i,j) + bact(1)%jzloss_n(i,j,k_100)* &
                drho_dzt
           bact(1)%jvirloss_n_100(i,j) = bact(1)%jvirloss_n_100(i,j) + bact(1)%jvirloss_n(i,j,k_100)* &
                drho_dzt
           bact(1)%jremin_n_100(i,j) = bact(1)%jremin_n_100(i,j) + bact(1)%jprod_nh4(i,j,k_100)* &
                drho_dzt
           bact(1)%juptake_ldon_100(i,j) = bact(1)%juptake_ldon_100(i,j) + bact(1)%juptake_ldon(i,j,k_100)* &
                drho_dzt
           bact(1)%f_n_100(i,j) = bact(1)%f_n_100(i,j) + bact(1)%f_n(i,j,k_100)*drho_dzt

           cobalt%jprod_lithdet_100(i,j) = cobalt%jprod_lithdet_100(i,j) + cobalt%jprod_lithdet(i,j,k_100)* &
                drho_dzt
           cobalt%jprod_sidet_100(i,j) = cobalt%jprod_sidet_100(i,j) + cobalt%jprod_sidet(i,j,k_100)* &
                drho_dzt
           cobalt%jprod_cadet_calc_100(i,j) = cobalt%jprod_cadet_calc_100(i,j) + cobalt%jprod_cadet_calc(i,j,k_100)* &
                drho_dzt
           cobalt%jprod_cadet_arag_100(i,j) = cobalt%jprod_cadet_arag_100(i,j) + cobalt%jprod_cadet_arag(i,j,k_100)* &
                drho_dzt
           cobalt%jremin_ndet_100(i,j) = cobalt%jremin_ndet_100(i,j) + cobalt%jremin_ndet(i,j,k_100)* &
                drho_dzt

           cobalt%f_ndet_100(i,j) = cobalt%f_ndet_100(i,j) + cobalt%f_ndet(i,j,k_100)*drho_dzt
           cobalt%f_don_100(i,j) = cobalt%f_don_100(i,j) + (cobalt%f_ldon(i,j,k_100) + cobalt%f_sldon(i,j,k_100) + &
              cobalt%f_srdon(i,j,k_100))*drho_dzt
           cobalt%f_silg_100(i,j) = cobalt%f_silg_100(i,j) + cobalt%f_silg(i,j,k_100)*drho_dzt
           cobalt%f_simd_100(i,j) = cobalt%f_simd_100(i,j) + cobalt%f_simd(i,j,k_100)*drho_dzt

           cobalt%fndet_100(i,j) = cobalt%f_ndet(i,j,k_100) * cobalt%Rho_0 * cobalt%wsink
           cobalt%fpdet_100(i,j) = cobalt%f_pdet(i,j,k_100) * cobalt%Rho_0 * cobalt%wsink
           cobalt%ffedet_100(i,j) = cobalt%f_fedet(i,j,k_100) * cobalt%Rho_0 * cobalt%wsink
           cobalt%flithdet_100(i,j) = cobalt%f_lithdet(i,j,k_100) * cobalt%Rho_0 * cobalt%wsink
           cobalt%fsidet_100(i,j) = cobalt%f_sidet(i,j,k_100) * cobalt%Rho_0 * cobalt%wsink
           cobalt%fcadet_arag_100(i,j) = cobalt%f_cadet_arag(i,j,k_100) * cobalt%Rho_0 * cobalt%wsink
           cobalt%fcadet_calc_100(i,j) = cobalt%f_cadet_calc(i,j,k_100) * cobalt%Rho_0 * cobalt%wsink

           cobalt%fntot_100(i,j) = (cobalt%f_ndet(i,j,k_100)*cobalt%wsink + &
               phyto(SMALL)%f_n(i,j,k_100)*phyto(SMALL)%vmove(i,j,k_100) + &
               phyto(MEDIUM)%f_n(i,j,k_100)*phyto(MEDIUM)%vmove(i,j,k_100) + &
               phyto(LARGE)%f_n(i,j,k_100)*phyto(LARGE)%vmove(i,j,k_100) + &
               phyto(DIAZO)%f_n(i,j,k_100)*phyto(DIAZO)%vmove(i,j,k_100))*cobalt%Rho_0
           cobalt%fptot_100(i,j) = (cobalt%f_pdet(i,j,k_100)*cobalt%wsink + &
               phyto(SMALL)%f_p(i,j,k_100)*phyto(SMALL)%vmove(i,j,k_100) + &
               phyto(MEDIUM)%f_p(i,j,k_100)*phyto(MEDIUM)%vmove(i,j,k_100) + &
               phyto(LARGE)%f_p(i,j,k_100)*phyto(LARGE)%vmove(i,j,k_100) + &
               phyto(DIAZO)%f_p(i,j,k_100)*phyto(DIAZO)%vmove(i,j,k_100))*cobalt%Rho_0
           cobalt%ffetot_100(i,j) = (cobalt%f_fedet(i,j,k_100)*cobalt%wsink + &
               phyto(SMALL)%f_fe(i,j,k_100)*phyto(SMALL)%vmove(i,j,k_100) + &
               phyto(MEDIUM)%f_fe(i,j,k_100)*phyto(MEDIUM)%vmove(i,j,k_100) + &
               phyto(LARGE)%f_fe(i,j,k_100)*phyto(LARGE)%vmove(i,j,k_100) + &
               phyto(DIAZO)%f_fe(i,j,k_100)*phyto(DIAZO)%vmove(i,j,k_100))*cobalt%Rho_0
           cobalt%fsitot_100(i,j) = (cobalt%f_sidet(i,j,k_100)*cobalt%wsink + &
               cobalt%f_simd(i,j,k_100)*phyto(MEDIUM)%vmove(i,j,k_100) + &
               cobalt%f_silg(i,j,k_100)*phyto(LARGE)%vmove(i,j,k_100))*cobalt%Rho_0
       endif

       cobalt%jprod_allphytos_100(i,j) = phyto(SMALL)%jprod_n_100(i,j) + phyto(MEDIUM)%jprod_n_100(i,j) + &
                              phyto(LARGE)%jprod_n_100(i,j) + phyto(DIAZO)%jprod_n_100(i,j)
    enddo ; enddo  !} i,j

    !
    ! Calculate biomass-weighted nutrient and light limitation terms for CMIP6
    ! Note: these need to be done after other 100m integrals because the biomass
    ! weighting requires the pre-calculated 100m biomass
    !
    do j = jsc, jec ; do i = isc, iec !{
       rho_dzt_100(i,j) = rho_dzt(i,j,1)
       do n = 1,NUM_PHYTO
          phyto(n)%nlim_bw_100(i,j) = (phyto(n)%no3lim(i,j,1)+phyto(n)%nh4lim(i,j,1))* &
                phyto(n)%f_n(i,j,1)*rho_dzt(i,j,1)/(phyto(n)%f_n_100(i,j)+epsln)
          phyto(n)%plim_bw_100(i,j) = phyto(n)%po4lim(i,j,1)* &
                phyto(n)%f_n(i,j,1)*rho_dzt(i,j,1)/(phyto(n)%f_n_100(i,j)+epsln)
          phyto(n)%def_fe_bw_100(i,j) = phyto(n)%def_fe(i,j,1)* &
                phyto(n)%f_n(i,j,1)*rho_dzt(i,j,1)/(phyto(n)%f_n_100(i,j)+epsln)
          phyto(n)%irrlim_bw_100(i,j) = phyto(n)%irrlim(i,j,1)* &
                phyto(n)%f_n(i,j,1)*rho_dzt(i,j,1)/(phyto(n)%f_n_100(i,j)+epsln)
       enddo   !} n
    enddo; enddo  !} i, j

    do j = jsc, jec ; do i = isc, iec ; !{
       k_100 = 1
       do k = 2, grid_kmt(i,j)  !{
          if (rho_dzt_100(i,j) .lt. cobalt%Rho_0 * 100.0) then
             k_100 = k
             rho_dzt_100(i,j) = rho_dzt_100(i,j) + rho_dzt(i,j,k)
             do n = 1,NUM_PHYTO
                phyto(n)%nlim_bw_100(i,j) = phyto(n)%nlim_bw_100(i,j) + &
                   (phyto(n)%no3lim(i,j,k)+phyto(n)%nh4lim(i,j,k))* &
                   phyto(n)%f_n(i,j,k)*rho_dzt(i,j,k)/(phyto(n)%f_n_100(i,j)+epsln)
                phyto(n)%plim_bw_100(i,j) = phyto(n)%plim_bw_100(i,j) + phyto(n)%po4lim(i,j,k)* &
                   phyto(n)%f_n(i,j,k)*rho_dzt(i,j,k)/(phyto(n)%f_n_100(i,j)+epsln)
                phyto(n)%def_fe_bw_100(i,j) = phyto(n)%def_fe_bw_100(i,j) + phyto(n)%def_fe(i,j,k)* &
                   phyto(n)%f_n(i,j,k)*rho_dzt(i,j,k)/(phyto(n)%f_n_100(i,j)+epsln)
                phyto(n)%irrlim_bw_100(i,j) = phyto(n)%irrlim_bw_100(i,j) + phyto(n)%irrlim(i,j,k)* &
                   phyto(n)%f_n(i,j,k)*rho_dzt(i,j,k)/(phyto(n)%f_n_100(i,j)+epsln)
             enddo
          endif
       enddo  !} k

       if (k_100 .gt. 1 .and. k_100 .lt. grid_kmt(i,j)) then
          drho_dzt = cobalt%Rho_0 * 100.0 - rho_dzt_100(i,j)
          do n = 1,NUM_PHYTO
             phyto(n)%nlim_bw_100(i,j) = phyto(n)%nlim_bw_100(i,j) + &
                (phyto(n)%no3lim(i,j,k_100)+phyto(n)%nh4lim(i,j,k_100))* &
                phyto(n)%f_n(i,j,k_100)*drho_dzt/(phyto(n)%f_n_100(i,j)+epsln)
             phyto(n)%plim_bw_100(i,j) = phyto(n)%plim_bw_100(i,j) + phyto(n)%po4lim(i,j,k_100)* &
                phyto(n)%f_n(i,j,k_100)*drho_dzt/(phyto(n)%f_n_100(i,j)+epsln)
             phyto(n)%def_fe_bw_100(i,j) = phyto(n)%def_fe_bw_100(i,j) + phyto(n)%def_fe(i,j,k_100)* &
                phyto(n)%f_n(i,j,k_100)*drho_dzt/(phyto(n)%f_n_100(i,j)+epsln)
             phyto(n)%irrlim_bw_100(i,j) = phyto(n)%irrlim_bw_100(i,j) + phyto(n)%irrlim(i,j,k_100)* &
                phyto(n)%f_n(i,j,k_100)*drho_dzt/(phyto(n)%f_n_100(i,j)+epsln)
          enddo
        endif
    enddo; enddo  !} i, j
    deallocate(rho_dzt_100)

    do j = jsc, jec ; do i = isc, iec ; !{
      if (grid_kmt(i,j) .gt. 0) then !{
    !     cobalt%btm_temp_old(i,j) = Temp(i,j,grid_kmt(i,j))
    !     cobalt%btm_o2_old(i,j) = cobalt%f_o2(i,j,grid_kmt(i,j))
    !     cobalt%btm_htotal_old(i,j) = cobalt%f_htotal(i,j,grid_kmt(i,j))
    !     cobalt%btm_co3_sol_arag_old(i,j) = cobalt%co3_sol_arag(i,j,grid_kmt(i,j))
    !     cobalt%btm_co3_sol_calc_old(i,j) = cobalt%co3_sol_calc(i,j,grid_kmt(i,j))
    !     cobalt%btm_co3_ion_old(i,j) = cobalt%f_co3_ion(i,j,grid_kmt(i,j))
         cobalt%cased_2d(i,j) = cobalt%f_cased(i,j,1)
      endif
    enddo; enddo  !} i, j

    ! Calculate the bottom layer over a thickness defined by cobalt%bottom_thickness
    ! rather than the bottom-most layer as in MOM4/5.  This avoids numerical issues
    ! generated in "vanishing" layers that overlie the benthos in most regions.
    do j = jsc, jec ; do i = isc, iec  !{
       rho_dzt_bot(i,j) = 0.0
       cobalt%btm_temp(i,j) = 0.0
       cobalt%btm_o2(i,j) = 0.0
       cobalt%btm_dic(i,j) = 0.0
       cobalt%btm_alk(i,j) = 0.0
       cobalt%btm_htotal(i,j) = 0.0
       cobalt%btm_co3_sol_arag(i,j) = 0.0
       cobalt%btm_co3_sol_calc(i,j) = 0.0
       cobalt%btm_co3_ion(i,j) = 0.0
       cobalt%btm_omega_calc(i,j) = 0.0
       cobalt%btm_omega_arag(i,j) = 0.0
       k_bot(i,j) = 0
       k = grid_kmt(i,j)
       if (k .gt. 0) then !{
         cobalt%grid_kmt_diag(i,j) = float(k)
         cobalt%rho_dzt_kmt_diag(i,j) = rho_dzt(i,j,k)
         do k = grid_kmt(i,j),1,-1   !{
           if (rho_dzt_bot(i,j).lt.cobalt%Rho_0*cobalt%bottom_thickness) then
             k_bot(i,j) = k
             rho_dzt_bot(i,j) = rho_dzt_bot(i,j) + rho_dzt(i,j,k)
             cobalt%k_bot_diag(i,j) = grid_kmt(i,j)-float(k)+1.0
             cobalt%btm_o2(i,j) = cobalt%btm_o2(i,j) + &
               cobalt%f_o2(i,j,k)*rho_dzt(i,j,k)
             cobalt%btm_alk(i,j) = cobalt%btm_alk(i,j) + &
               cobalt%f_alk(i,j,k)*rho_dzt(i,j,k)
             cobalt%btm_dic(i,j) = cobalt%btm_dic(i,j) + &
               cobalt%f_dic(i,j,k)*rho_dzt(i,j,k)
             cobalt%btm_temp(i,j) = cobalt%btm_temp(i,j) + &
               Temp(i,j,k)*rho_dzt(i,j,k)
             cobalt%btm_htotal(i,j) = cobalt%btm_htotal(i,j) + &
               cobalt%f_htotal(i,j,k)*rho_dzt(i,j,k)
             cobalt%btm_co3_sol_arag(i,j) = cobalt%btm_co3_sol_arag(i,j) + &
               cobalt%co3_sol_arag(i,j,k)*rho_dzt(i,j,k)
             cobalt%btm_co3_sol_calc(i,j) = cobalt%btm_co3_sol_calc(i,j) + &
               cobalt%co3_sol_calc(i,j,k)*rho_dzt(i,j,k)
             cobalt%btm_co3_ion(i,j) = cobalt%btm_co3_ion(i,j) + &
               cobalt%f_co3_ion(i,j,k)*rho_dzt(i,j,k)
           endif
         enddo
         ! diagnostic to assess how far up into the water column info is being drawn from
         cobalt%rho_dzt_bot_diag(i,j) = rho_dzt_bot(i,j)
         ! calculate overshoot and subtract off
         drho_dzt = rho_dzt_bot(i,j) - cobalt%Rho_0*cobalt%bottom_thickness
         cobalt%btm_temp(i,j)=cobalt%btm_temp(i,j)-Temp(i,j,k_bot(i,j))*drho_dzt
         cobalt%btm_o2(i,j)=cobalt%btm_o2(i,j)-cobalt%f_o2(i,j,k_bot(i,j))*drho_dzt
         cobalt%btm_alk(i,j)=cobalt%btm_alk(i,j)-cobalt%f_alk(i,j,k_bot(i,j))*drho_dzt
         cobalt%btm_dic(i,j)=cobalt%btm_dic(i,j)-cobalt%f_dic(i,j,k_bot(i,j))*drho_dzt
         cobalt%btm_htotal(i,j)=cobalt%btm_htotal(i,j)-cobalt%f_htotal(i,j,k_bot(i,j))*drho_dzt
         cobalt%btm_co3_sol_arag(i,j)=cobalt%btm_co3_sol_arag(i,j)-cobalt%co3_sol_arag(i,j,k_bot(i,j))*drho_dzt
         cobalt%btm_co3_sol_calc(i,j)=cobalt%btm_co3_sol_calc(i,j)-cobalt%co3_sol_calc(i,j,k_bot(i,j))*drho_dzt
         cobalt%btm_co3_ion(i,j)=cobalt%btm_co3_ion(i,j)-cobalt%f_co3_ion(i,j,k_bot(i,j))*drho_dzt
         ! convert back to moles kg-1
         cobalt%btm_temp(i,j)=cobalt%btm_temp(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
         cobalt%btm_o2(i,j)=cobalt%btm_o2(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
         cobalt%btm_alk(i,j)=cobalt%btm_alk(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
         cobalt%btm_dic(i,j)=cobalt%btm_dic(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
         cobalt%btm_htotal(i,j)=cobalt%btm_htotal(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
         cobalt%btm_co3_sol_arag(i,j)=cobalt%btm_co3_sol_arag(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
         cobalt%btm_co3_sol_calc(i,j)=cobalt%btm_co3_sol_calc(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
         cobalt%btm_co3_ion(i,j)=cobalt%btm_co3_ion(i,j)/(cobalt%bottom_thickness*cobalt%Rho_0)
         ! calculate bottom saturation states
         cobalt%btm_omega_calc(i,j) = cobalt%btm_co3_ion(i,j)/cobalt%btm_co3_sol_calc(i,j)
         cobalt%btm_omega_arag(i,j) = cobalt%btm_co3_ion(i,j)/cobalt%btm_co3_sol_arag(i,j)
       endif
    enddo; enddo

    !
    !---------------------------------------------------------------------
    ! calculate upper 200m vertical integrals for mesozooplankton
    ! quantities for comparison with COPEPOD database
    !---------------------------------------------------------------------
    !
    allocate(rho_dzt_200(isc:iec,jsc:jec))
    do j = jsc, jec ; do i = isc, iec !{
       rho_dzt_200(i,j) = rho_dzt(i,j,1)
       cobalt%jprod_mesozoo_200(i,j) = (zoo(2)%jprod_n(i,j,1) + zoo(3)%jprod_n(i,j,1))*rho_dzt(i,j,1)
       cobalt%f_mesozoo_200(i,j) = (zoo(2)%f_n(i,j,1)+zoo(3)%f_n(i,j,1))*rho_dzt(i,j,1)
       cobalt%jprod_allphytos_200(i,j) = (phyto(1)%jprod_n(i,j,1) + phyto(2)%jprod_n(i,j,1) + &
             phyto(3)%jprod_n(i,j,1) + phyto(4)%jprod_n(i,j,1))*rho_dzt(i,j,1);
    enddo; enddo !} i,j

    do j = jsc, jec ; do i = isc, iec ; !{
       k_200 = 1
       do k = 2, grid_kmt(i,j)  !{
          if (rho_dzt_200(i,j) .lt. cobalt%Rho_0 * 200.0) then
             k_200 = k
             rho_dzt_200(i,j) = rho_dzt_200(i,j) + rho_dzt(i,j,k)
             cobalt%jprod_mesozoo_200(i,j) = cobalt%jprod_mesozoo_200(i,j) + &
                (zoo(2)%jprod_n(i,j,k) + zoo(3)%jprod_n(i,j,k))*rho_dzt(i,j,k)
             cobalt%f_mesozoo_200(i,j) = cobalt%f_mesozoo_200(i,j) + &
                (zoo(2)%f_n(i,j,k)+zoo(3)%f_n(i,j,k))*rho_dzt(i,j,k)
             cobalt%jprod_allphytos_200(i,j) = cobalt%jprod_allphytos_200(i,j) + &
                 (phyto(1)%jprod_n(i,j,k) + phyto(2)%jprod_n(i,j,k) + &
                 phyto(3)%jprod_n(i,j,k) + phyto(4)%jprod_n(i,j,k))*rho_dzt(i,j,k);
          endif
       enddo  !} k

       if (k_200 .gt. 1 .and. k_200 .lt. grid_kmt(i,j)) then
          drho_dzt = cobalt%Rho_0 * 200.0 - rho_dzt_200(i,j)
          cobalt%jprod_mesozoo_200(i,j) = cobalt%jprod_mesozoo_200(i,j) + &
              (zoo(2)%jprod_n(i,j,k_200) + zoo(3)%jprod_n(i,j,k_200))*drho_dzt
          cobalt%f_mesozoo_200(i,j) = cobalt%f_mesozoo_200(i,j) + &
              (zoo(2)%f_n(i,j,k_200)+zoo(3)%f_n(i,j,k_200))*drho_dzt
          cobalt%jprod_allphytos_200(i,j) = cobalt%jprod_allphytos_200(i,j) + &
               (phyto(1)%jprod_n(i,j,k_200) + phyto(2)%jprod_n(i,j,k_200) + &
               phyto(3)%jprod_n(i,j,k_200) + phyto(4)%jprod_n(i,j,k_200))*drho_dzt
       endif
    enddo ; enddo  !} i,j

    call g_tracer_get_values(tracer_list,'alk','runoff_tracer_flux',cobalt%runoff_flux_alk,isd,jsd)
    call g_tracer_get_values(tracer_list,'dic','runoff_tracer_flux',cobalt%runoff_flux_dic,isd,jsd)
    if (do_14c) then  !{
      call g_tracer_get_values(tracer_list,'di14c','runoff_tracer_flux',cobalt%runoff_flux_di14c,isd,jsd)
    endif  !}
    call g_tracer_get_values(tracer_list,'fed','runoff_tracer_flux',cobalt%runoff_flux_fed,isd,jsd)
    call g_tracer_get_values(tracer_list,'fed','drydep',cobalt%dry_fed,isd,jsd)
    call g_tracer_get_values(tracer_list,'fed','wetdep',cobalt%wet_fed,isd,jsd)
    call g_tracer_get_values(tracer_list,'lith','runoff_tracer_flux',cobalt%runoff_flux_lith,isd,jsd)
    call g_tracer_get_values(tracer_list,'lith','drydep',cobalt%dry_lith,isd,jsd)
    call g_tracer_get_values(tracer_list,'lith','wetdep',cobalt%wet_lith,isd,jsd)
    call g_tracer_get_values(tracer_list,'no3','runoff_tracer_flux',cobalt%runoff_flux_no3,isd,jsd)
    call g_tracer_get_values(tracer_list,'no3','drydep',cobalt%dry_no3,isd,jsd)
    call g_tracer_get_values(tracer_list,'no3','wetdep',cobalt%wet_no3,isd,jsd)
    call g_tracer_get_values(tracer_list,'nh4','drydep',cobalt%dry_nh4,isd,jsd)
    call g_tracer_get_values(tracer_list,'nh4','wetdep',cobalt%wet_nh4,isd,jsd)
    call g_tracer_get_values(tracer_list,'po4','drydep',cobalt%dry_po4,isd,jsd)
    call g_tracer_get_values(tracer_list,'po4','wetdep',cobalt%wet_po4,isd,jsd)
    call g_tracer_get_values(tracer_list,'ldon','runoff_tracer_flux',cobalt%runoff_flux_ldon,isd,jsd)
    call g_tracer_get_values(tracer_list,'sldon','runoff_tracer_flux',cobalt%runoff_flux_sldon,isd,jsd)
    call g_tracer_get_values(tracer_list,'srdon','runoff_tracer_flux',cobalt%runoff_flux_srdon,isd,jsd)
    call g_tracer_get_values(tracer_list,'ndet','runoff_tracer_flux',cobalt%runoff_flux_ndet,isd,jsd)
    call g_tracer_get_values(tracer_list,'pdet','runoff_tracer_flux',cobalt%runoff_flux_pdet,isd,jsd)
    call g_tracer_get_values(tracer_list,'po4','runoff_tracer_flux',cobalt%runoff_flux_po4,isd,jsd)
    call g_tracer_get_values(tracer_list,'ldop','runoff_tracer_flux',cobalt%runoff_flux_ldop,isd,jsd)
    call g_tracer_get_values(tracer_list,'sldop','runoff_tracer_flux',cobalt%runoff_flux_sldop,isd,jsd)
    call g_tracer_get_values(tracer_list,'srdop','runoff_tracer_flux',cobalt%runoff_flux_srdop,isd,jsd)
! JGJ: Added for CMIP6
    call g_tracer_get_values(tracer_list,'dic','stf_gas',cobalt%stf_gas_dic,isd,jsd)
    call g_tracer_get_values(tracer_list,'o2','stf_gas',cobalt%stf_gas_o2,isd,jsd)
    call g_tracer_get_values(tracer_list,'dic','deltap',cobalt%deltap_dic,isd,jsd)
    call g_tracer_get_values(tracer_list,'o2','deltap',cobalt%deltap_o2,isd,jsd)


!---------------------------------------------------------------------
! Add vertical integrals for diagnostics
!---------------------------------------------------------------------
!

    call mpp_clock_end(id_clock_cobalt_calc_diagnostics)
    call mpp_clock_begin(id_clock_cobalt_send_diagnostics)

    if (cobalt%id_pka_nh3 .gt. 0) then
       used = g_send_data(cobalt%id_pka_nh3,  pka_nh3,   &
            model_time, rmask = grid_tmask(:,:,1),&
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
    end if
    if (allocated(pka_nh3)) deallocate(pka_nh3)
    deallocate(phos_nh3_exchange)
!
!---------------------------------------------------------------------
!
! Send phytoplankton diagnostic data

    call cobalt_send_diagnostics(model_time,grid_tmask,Temp,rho_dzt,dzt, &
            isc,iec,jsc,jec,nk,tau,phyto,zoo,bact,cobalt) 

!==============================================================================================================

    call mpp_clock_end(id_clock_cobalt_send_diagnostics)

  end subroutine generic_COBALT_update_from_source


  ! <SUBROUTINE NAME="generic_COBALT_set_boundary_values">
  !  <OVERVIEW>
  !   Calculate and set coupler values at the surface / bottom
  !  </OVERVIEW>
  !  <DESCRIPTION>
  !
  !  </DESCRIPTION>
  !  <TEMPLATE>
  !   call generic_COBALT_set_boundary_values(tracer_list,SST,SSS,rho,ilb,jlb,tau,dzt)
  !  </TEMPLATE>
  !  <IN NAME="tracer_list" TYPE="type(g_tracer_type), pointer">
  !   Pointer to the head of generic tracer list.
  !  </IN>
  !  <IN NAME="ilb,jlb" TYPE="integer">
  !   Lower bounds of x and y extents of input arrays on data domain
  !  </IN>
  !  <IN NAME="SST" TYPE="real, dimension(ilb:,jlb:)">
  !   Sea Surface Temperature
  !  </IN>
  !  <IN NAME="SSS" TYPE="real, dimension(ilb:,jlb:)">
  !   Sea Surface Salinity
  !  </IN>
  !  <IN NAME="rho" TYPE="real, dimension(ilb:,jlb:,:,:)">
  !   Ocean density
  !  </IN>
  !  <IN NAME="tau" TYPE="integer">
  !   Time step index of %field
  !  </IN>
  ! </SUBROUTINE>

  !User must provide the calculations for these boundary values.
  subroutine generic_COBALT_set_boundary_values(tracer_list,SST,SSS,rho,ilb,jlb,tau,dzt,model_time)
    type(g_tracer_type),          pointer    :: tracer_list
    real, dimension(ilb:,jlb:),   intent(in)   :: SST, SSS
    real, dimension(ilb:,jlb:,:,:), intent(in) :: rho
    integer,                        intent(in) :: ilb,jlb,tau
    real, dimension(ilb:,jlb:,:), optional, intent(in) :: dzt
    type(time_type),                intent(in) :: model_time
    integer :: isc,iec, jsc,jec,isd,ied,jsd,jed,nk,ntau , i, j
    real    :: sal,ST,o2_saturation
    real    :: tt,tk,ts,ts2,ts3,ts4,ts5
    real, dimension(:,:,:)  ,pointer  :: grid_tmask
    real, dimension(:,:,:,:), pointer :: o2_field,dic_field,po4_field,sio4_field,alk_field,di14c_field,nh4_field
    real, dimension(:,:,:), ALLOCATABLE :: htotal_field,co3_ion_field
    real, dimension(:,:), ALLOCATABLE :: co2_alpha,co2_csurf,co2_sc_no,o2_alpha,o2_csurf,o2_sc_no,nh3_alpha,nh3_csurf,nh3_sc_no,phos_nh3_exchange
    real, dimension(:,:), ALLOCATABLE :: c14o2_alpha,c14o2_csurf
    real :: pka_nh3,tr,ltr

    logical :: phos_nh3_override

    character(len=fm_string_len), parameter :: sub_name = 'generic_COBALT_set_boundary_values'

    !
    !
    !Get the necessary properties
    !
    call g_tracer_get_common(isc,iec,jsc,jec,isd,ied,jsd,jed,nk,ntau,grid_tmask=grid_tmask)

    call g_tracer_get_pointer(tracer_list,'o2' ,'field',  o2_field)

    allocate(co2_alpha(isd:ied, jsd:jed)); co2_alpha=0.0
    allocate(co2_csurf(isd:ied, jsd:jed)); co2_csurf=0.0
    allocate(co2_sc_no(isd:ied, jsd:jed)); co2_sc_no=0.0
    allocate(nh3_alpha(isd:ied, jsd:jed)); nh3_alpha=0.0
    allocate(nh3_csurf(isd:ied, jsd:jed)); nh3_csurf=0.0
    allocate(nh3_sc_no(isd:ied, jsd:jed)); nh3_sc_no=0.0
    !for nh3 ph emission override
    allocate(phos_nh3_exchange(isd:ied, jsd:jed)); phos_nh3_exchange=0.0
    allocate(c14o2_alpha(isd:ied, jsd:jed)); c14o2_alpha=0.0
    allocate(c14o2_csurf(isd:ied, jsd:jed)); c14o2_csurf=0.0
    allocate(o2_alpha(isd:ied, jsd:jed)); o2_alpha=0.0
    allocate(o2_csurf(isd:ied, jsd:jed)); o2_csurf=0.0
    allocate(o2_sc_no(isd:ied, jsd:jed)); o2_sc_no=0.0
    allocate(htotal_field(isd:ied,jsd:jed,nk),co3_ion_field(isd:ied,jsd:jed,nk))
    htotal_field=0.0 ; co3_ion_field=0.0

    !nnz: Since the generic_COBALT_update_from_source() subroutine is called by this time
    !     the following if block is not really necessary (since this calculation is already done in source).
    !     It is only neccessary if source routine is commented out for debugging.
    !Note: In order for this to work we should NOT zero out the coupler values for generic tracers
    !      This zeroing is done for non-generic TOPAZ by calling zero_ocean_sfc.
    !      Since the coupler values here are non-cumulative there is no need to zero them out anyway.

    if (cobalt%init .OR. cobalt%force_update_fluxes) then
       !Get necessary fields
       call g_tracer_get_pointer(tracer_list,'dic'   ,'field', dic_field)
       call g_tracer_get_pointer(tracer_list,'po4'   ,'field', po4_field)
       call g_tracer_get_pointer(tracer_list,'sio4'  ,'field', sio4_field)
       call g_tracer_get_pointer(tracer_list,'alk'   ,'field', alk_field)
       call g_tracer_get_pointer(tracer_list,'nh4'   ,'field', nh4_field)

       call g_tracer_get_values(tracer_list,'htotal' ,'field', htotal_field,isd,jsd)
       call g_tracer_get_values(tracer_list,'co3_ion','field',co3_ion_field,isd,jsd)

       do j = jsc, jec ; do i = isc, iec  !{
          cobalt%htotallo(i,j) = cobalt%htotal_scale_lo * htotal_field(i,j,1)
          cobalt%htotalhi(i,j) = cobalt%htotal_scale_hi * htotal_field(i,j,1)
       enddo; enddo ; !} i, j

       if(.not. present(dzt)) then
          ! 2017/08/11 jgj is cobalt type defined/passed here ?
          !        do j = jsc, jec ; do i = isc, iec  !{
          !         cobalt%zt(i,j,1) = dzt(i,j,1)
          !        enddo; enddo ; !} i, j
          call mpp_error(FATAL,"mocsy method of co2_calc needs dzt to be passed to the FMS_co2calc subroutine.")
       endif

       call FMS_co2calc(CO2_dope_vec,grid_tmask(:,:,1), &
            SST(:,:), SSS(:,:),                            &
            dic_field(:,:,1,tau),                          &
            po4_field(:,:,1,tau),                          &
            sio4_field(:,:,1,tau),                         &
            alk_field(:,:,1,tau),                          &
            cobalt%htotallo, cobalt%htotalhi,              &
                                !InOut
            htotal_field(:,:,1),                           &
                                !Optional In
            !! jgj 2017/08/11
            !!zt=cobalt%zt(:,:,1),                           &
            zt=dzt(:,:,1),                                 &
                                !OUT
            co2star=co2_csurf(:,:), alpha=co2_alpha(:,:),  &
            pCO2surf=cobalt%pco2_csurf(:,:), &
            co3_ion=co3_ion_field(:,:,1), &
            omega_arag=cobalt%omega_arag(:,:,1), &
            omega_calc=cobalt%omega_calc(:,:,1))

       !Set fields !nnz: if These are pointers do I need to do this?
       call g_tracer_set_values(tracer_list,'htotal' ,'field',htotal_field ,isd,jsd)
       call g_tracer_set_values(tracer_list,'co3_ion','field',co3_ion_field,isd,jsd)

       call g_tracer_set_values(tracer_list,'dic','alpha',co2_alpha    ,isd,jsd)
       call g_tracer_set_values(tracer_list,'dic','csurf',co2_csurf    ,isd,jsd)


      if (do_14c) then                                        !<<RADIOCARBON

        ! Normally, the alpha would be multiplied by the atmospheric 14C/12C ratio. However,
        ! here that is set to 1, so that alpha_14C = alpha_12C. This needs to be changed!

       call g_tracer_get_pointer(tracer_list,'di14c'   ,'field', di14c_field)

        do j = jsc, jec ; do i = isc, iec  !{
          c14o2_csurf(i,j) =  co2_csurf(i,j) *                &
            di14c_field(i,j,1,tau) / (dic_field(i,j,1,tau) + epsln)
          c14o2_alpha(i,j) =  co2_alpha(i,j)
        enddo; enddo ; !} i, j

        call g_tracer_set_values(tracer_list,'di14c','alpha',c14o2_alpha      ,isd,jsd)
        call g_tracer_set_values(tracer_list,'di14c','csurf',c14o2_csurf      ,isd,jsd)

      endif

       if (do_nh3_atm_ocean_exchange) then
          !          write(*,*) 'min htot ',minval(htotal_field(:,:,1))

          call data_override('OCN', 'phos_nh3_exchange', phos_nh3_exchange(isc:iec,jsc:jec), model_time,override=phos_nh3_override)

          do j = jsc, jec ; do i = isc, iec  !{
             !nh3
             pka_nh3        = calc_pka_nh3(SST(i,j),SSS(i,j))
             tr             = 298.15/(SST(i,j)+273.15)-1.
             ltr            = -tr+log(298.15/(SST(i,j)+273.15))
             !mol/kg/atm from Jacobson 2005 (fundamental of atmospheric modeling)
             !997/1035 is to convert pure water to salt water
             nh3_alpha(i,j) = 5.76e1*exp(13.79*tr-5.39*ltr)*997. !in mol/kg(water)/atm -> mol/m3/atm
             nh3_alpha(i,j) = nh3_alpha(i,j)/saltout_correction(101325./(1e-3*rdgas*wtmair*(SST(i,j)+273.15)*nh3_alpha(i,j)),vb_nh3,SSS(i,j)) * 1./cobalt%Rho_0 !mol/kg/atm
             if (phos_nh3_override) then
                nh3_csurf(i,j) = nh4_field(i,j,1,tau)/(1.+10**(pka_nh3-(max(min(11.,phos_nh3_exchange(i,j)),3.))))
             else
                nh3_csurf(i,j) = nh4_field(i,j,1,tau)/(1.+10**(pka_nh3+log10(min(max(1e-11,htotal_field(i,j,1)),1e-3))))
             end if
             cobalt%pnh3_csurf(i,j)  =  cobalt%nh3_csurf(i,j)/nh3_alpha(i,j)*1.e6 !in uatm
          enddo;enddo

          call g_tracer_set_values(tracer_list,'nh4','alpha',nh3_alpha    ,isd,jsd)
          call g_tracer_set_values(tracer_list,'nh4','csurf',nh3_csurf    ,isd,jsd)

       end if
       !!nnz: If source is called uncomment the following
       cobalt%init = .false. !nnz: This is necessary since the above two calls appear in source subroutine too.
    endif

    call g_tracer_get_values(tracer_list,'dic','alpha', co2_alpha ,isd,jsd)
    call g_tracer_get_values(tracer_list,'dic','csurf', co2_csurf ,isd,jsd)

    call g_tracer_get_values(tracer_list,'o2','alpha', o2_alpha ,isd,jsd)
    call g_tracer_get_values(tracer_list,'o2','csurf', o2_csurf ,isd,jsd)


    do j=jsc,jec ; do i=isc,iec
       !This calculation needs an input of SST and SSS
       sal = SSS(i,j) ; ST = SST(i,j)

       !nnz:
       !Note: In the following calculations in order to get results for co2 and o2
       !      identical with cobalt code in MOM cobalt%Rho_0 must be replaced with rho(i,j,1,tau)
       !      This is achieved by uncommenting the following if desired.
       !! cobalt%Rho_0 = rho(i,j,1,tau)
       !      But since %Rho_0 plays the role of a unit conversion factor in this module
       !      it may be safer to keep it as a constant (1035.0) rather than the actual variable
       !      surface density rho(i,j,1,tau)

       !---------------------------------------------------------------------
       !     CO2
       !---------------------------------------------------------------------

       !---------------------------------------------------------------------
       !  Compute the Schmidt number of CO2 in seawater using the
       !  formulation presented by Wanninkhof (1992, J. Geophys. Res., 97,
       !  7373-7382).
       !  2018/01/17 jgj  update Schmidt number for CO2 to use
       !  Wanninkhof, Limnol. Oceanogr: Methods, 12, 2014, 351-362
       !---------------------------------------------------------------------
       if (trim(as_param_cobalt) == 'W92') then
         co2_sc_no(i,j) = cobalt%a1_co2 + ST*(cobalt%a2_co2 + ST*(cobalt%a3_co2 + ST*cobalt%a4_co2)) * &
                          grid_tmask(i,j,1)
       else if ((trim(as_param_cobalt) == 'W14') .or. (trim(as_param_cobalt) == 'gfdl_cmip6')) then
         co2_sc_no(i,j) = cobalt%a1_co2 + ST*(cobalt%a2_co2 + ST*(cobalt%a3_co2 + &
                          ST*(cobalt%a4_co2 + ST*cobalt%a5_co2))) * grid_tmask(i,j,1)
       endif
!       sc_no_term = sqrt(660.0 / (sc_co2 + epsln))
!
!       co2_alpha(i,j) = co2_alpha(i,j)* sc_no_term * cobalt%Rho_0 !nnz: MOM has rho(i,j,1,tau)
!       co2_csurf(i,j) = co2_csurf(i,j)* sc_no_term * cobalt%Rho_0 !nnz: MOM has rho(i,j,1,tau)
!
! in 'ocmip2_new' atmos_ocean_fluxes.F90 coupler formulation, the schmidt number is carried in explicitly
!
       co2_alpha(i,j) = co2_alpha(i,j) * cobalt%Rho_0 !nnz: MOM has rho(i,j,1,tau)
       co2_csurf(i,j) = co2_csurf(i,j) * cobalt%Rho_0 !nnz: MOM has rho(i,j,1,tau)

       !---------------------------------------------------------------------
       !     O2
       !---------------------------------------------------------------------
       !  Compute the oxygen saturation concentration at 1 atm total
       !  pressure in mol/kg given the temperature (t, in deg C) and
       !  the salinity (s, in permil)
       !
       !  From Garcia and Gordon (1992), Limnology and Oceonography.
       !  The formula used is from page 1310, eq (8).
       !
       !  *** Note: the "a3*ts^2" term (in the paper) is incorrect. ***
       !  *** It shouldn't be there.                                ***
       !
       !  o2_saturation is defined between T(freezing) <= T <= 40 deg C and
       !                                   0 permil <= S <= 42 permil
       ! 2015/05/15  jgj ESM2.6 has values of 60+ in Red Sea - impose
       ! bounds on salinity to keep it in 0-42 range
       !
       ! check value: T = 10 deg C, S = 35 permil,
       !              o2_saturation = 0.282015 mol m-3
       !---------------------------------------------------------------------
       !
       ! jgj 2015/05/14 impose temperature and salinity bounds for o2sat
       sal = min(42.0,max(0.0,sal))
       tt = 298.15 - min(40.0,max(0.0,ST))
       tk = 273.15 + min(40.0,max(0.0,ST))
       ts = log(tt / tk)
       ts2 = ts  * ts
       ts3 = ts2 * ts
       ts4 = ts3 * ts
       ts5 = ts4 * ts

       !The atmospheric code needs solubilities in units of mol/m3/atm
       o2_saturation = (1000.0/22391.6) * grid_tmask(i,j,1) *  & !convert from ml/l to mol m-3
            exp( cobalt%a_0 + cobalt%a_1*ts + cobalt%a_2*ts2 + cobalt%a_3*ts3 + cobalt%a_4*ts4 + cobalt%a_5*ts5 + &
            (cobalt%b_0 + cobalt%b_1*ts + cobalt%b_2*ts2 + cobalt%b_3*ts3 + cobalt%c_0*sal)*sal)

       !---------------------------------------------------------------------
       !  Compute the Schmidt number of O2 in seawater using the
       !  formulation proposed by Keeling et al. (1998, Global Biogeochem.
       !  Cycles, 12, 141-163).
       !  2018/01/17 jgj  update Schmidt number for O2 to use
       !  Wanninkhof, Limnol. Oceanogr: Methods, 12, 2014, 351-362
       !---------------------------------------------------------------------
       !
       ! In 'ocmip2_generic' atmos_ocean_fluxes.F90 coupler formulation,
       ! the schmidt number is carried in explicitly
       !
       if (trim(as_param_cobalt) == 'W92') then
         o2_sc_no(i,j)  = cobalt%a1_o2 + ST * (cobalt%a2_o2 + ST * (cobalt%a3_o2 + ST * cobalt%a4_o2 )) * &
                          grid_tmask(i,j,1)
       else if ((trim(as_param_cobalt) == 'W14') .or. (trim(as_param_cobalt) == 'gfdl_cmip6')) then
         o2_sc_no(i,j) = cobalt%a1_o2 + ST*(cobalt%a2_o2 + ST*(cobalt%a3_o2 + &
                         ST*(cobalt%a4_o2 + ST*cobalt%a5_o2))) * grid_tmask(i,j,1)
       endif
       !
       !      renormalize the alpha value for atm o2
       !      data table override for o2_flux_pcair_atm is now set to 0.21
       !
       o2_alpha(i,j) = (o2_saturation / 0.21)
       o2_csurf(i,j) = o2_field(i,j,1,tau) * cobalt%Rho_0 !nnz: MOM has rho(i,j,1,tau)

    enddo; enddo

    !
    ! Set %csurf, %alpha and %sc_no for these tracers. This will mark them
    ! for sending fluxes to coupler
    !
    call g_tracer_set_values(tracer_list,'dic','alpha',co2_alpha,isd,jsd)
    call g_tracer_set_values(tracer_list,'dic','csurf',co2_csurf,isd,jsd)
    call g_tracer_set_values(tracer_list,'dic','sc_no',co2_sc_no,isd,jsd)

    call g_tracer_set_values(tracer_list,'o2', 'alpha',o2_alpha, isd,jsd)
    call g_tracer_set_values(tracer_list,'o2', 'csurf',o2_csurf, isd,jsd)
    call g_tracer_set_values(tracer_list,'o2', 'sc_no',o2_sc_no, isd,jsd)

    if (do_nh3_atm_ocean_exchange) then
       call g_tracer_get_values(tracer_list,'nh4','alpha', nh3_alpha ,isd,jsd)
       call g_tracer_get_values(tracer_list,'nh4','csurf', nh3_csurf ,isd,jsd)

       do j=jsc,jec ; do i=isc,iec
       !nh3
       !f1p
          nh3_sc_no(i,j) = schmidt_w(sst(i,j),sss(i,j),vb_nh3)*grid_tmask(i,j,1)
          nh3_csurf(i,j) = nh3_csurf(i,j)*cobalt%Rho_0
          nh3_alpha(i,j) = nh3_alpha(i,j)*cobalt%Rho_0
       end do;end do

       call g_tracer_set_values(tracer_list,'nh4', 'alpha',nh3_alpha, isd,jsd)
       call g_tracer_set_values(tracer_list,'nh4', 'csurf',nh3_csurf, isd,jsd)
       call g_tracer_set_values(tracer_list,'nh4', 'sc_no',nh3_sc_no, isd,jsd)

    end if

    if (do_14c) then                                      !<<RADIOCARBON

       call g_tracer_get_values(tracer_list,'di14c','alpha', c14o2_alpha ,isd,jsd)
       call g_tracer_get_values(tracer_list,'di14c','csurf', c14o2_csurf ,isd,jsd)

       do j=jsc,jec ; do i=isc,iec
         !---------------------------------------------------------------------
         !     14CO2 - schmidt number is calculated same as before, as is alpha.
         !   Need to multiply alpha by frac_14catm to get the real alpha.
         !   NOTE: FOR NOW, D14C fixed at 0 permil!! Need to fix this later.
         !---------------------------------------------------------------------

          sal = SSS(i,j) ; ST = SST(i,j)
          co2_sc_no(i,j) = cobalt%a1_co2 + ST * (cobalt%a2_co2 + ST * (cobalt%a3_co2 + ST * cobalt%a4_co2)) * &
               grid_tmask(i,j,1)

          c14o2_alpha(i,j) = c14o2_alpha(i,j) * cobalt%Rho_0
          c14o2_csurf(i,j) = c14o2_csurf(i,j) * cobalt%Rho_0

       enddo; enddo

       call g_tracer_set_values(tracer_list,'di14c','alpha',c14o2_alpha,isd,jsd)
       call g_tracer_set_values(tracer_list,'di14c','csurf',c14o2_csurf,isd,jsd)
       call g_tracer_set_values(tracer_list,'di14c','sc_no',co2_sc_no,isd,jsd)

    endif                                                  !RADIOCARBON>>

    deallocate(co2_alpha,co2_csurf,&
         co2_sc_no,o2_alpha,          &
         c14o2_alpha,c14o2_csurf,     &
         o2_csurf,o2_sc_no, nh3_alpha,nh3_csurf,&
         nh3_sc_no,phos_nh3_exchange)

  end subroutine generic_COBALT_set_boundary_values


  ! <SUBROUTINE NAME="generic_COBALT_end">
  !  <OVERVIEW>
  !   End the module.
  !  </OVERVIEW>
  !  <DESCRIPTION>
  !   Deallocate all work arrays
  !  </DESCRIPTION>
  !  <TEMPLATE>
  !   call generic_COBALT_end
  !  </TEMPLATE>
  ! </SUBROUTINE>


  subroutine generic_COBALT_end
    character(len=fm_string_len), parameter :: sub_name = 'generic_COBALT_end'
    call user_deallocate_arrays
  end subroutine generic_COBALT_end

  !
  !   This is an internal sub, not a public interface.
  !   Allocate all the work arrays to be used in this module.
  !
  subroutine user_allocate_arrays
    integer :: isc,iec,jsc,jec,isd,ied,jsd,jed,nk,ntau,n

    call g_tracer_get_common(isc,iec,jsc,jec,isd,ied,jsd,jed,nk,ntau)

    !Allocate all the private arrays.

    !Used in FMS_co2calc
    CO2_dope_vec%isc = isc ; CO2_dope_vec%iec = iec
    CO2_dope_vec%jsc = jsc ; CO2_dope_vec%jec = jec
    CO2_dope_vec%isd = isd ; CO2_dope_vec%ied = ied
    CO2_dope_vec%jsd = jsd ; CO2_dope_vec%jed = jed

    allocate(cobalt%htotallo(isd:ied,jsd:jed))
    allocate(cobalt%htotalhi(isd:ied,jsd:jed))

    !
    ! allocate and initialize array elements of all phytoplankton groups
    ! CAS: add fluxes for additional explicit phytoplankton loss terms

    do n = 1, NUM_PHYTO
       allocate(phyto(n)%P_C_max(isd:ied,jsd:jed,nk))      ; phyto(n)%P_C_max        = 0.0
       allocate(phyto(n)%alpha(isd:ied,jsd:jed,nk))        ; phyto(n)%alpha          = 0.0
       allocate(phyto(n)%bresp(isd:ied,jsd:jed,nk))        ; phyto(n)%bresp          = 0.0
       allocate(phyto(n)%def_fe(isd:ied,jsd:jed,nk))       ; phyto(n)%def_fe         = 0.0
       allocate(phyto(n)%def_p(isd:ied,jsd:jed,nk))        ; phyto(n)%def_p          = 0.0
       allocate(phyto(n)%f_fe(isd:ied,jsd:jed,nk))         ; phyto(n)%f_fe           = 0.0
       allocate(phyto(n)%f_n(isd:ied,jsd:jed,nk))          ; phyto(n)%f_n            = 0.0
       allocate(phyto(n)%f_p(isd:ied,jsd:jed,nk))          ; phyto(n)%f_p            = 0.0
       allocate(phyto(n)%felim(isd:ied,jsd:jed,nk))        ; phyto(n)%felim          = 0.0
       allocate(phyto(n)%irrlim(isd:ied,jsd:jed,nk))       ; phyto(n)%irrlim         = 0.0
       allocate(phyto(n)%jzloss_fe(isd:ied,jsd:jed,nk))    ; phyto(n)%jzloss_fe      = 0.0
       allocate(phyto(n)%jzloss_n(isd:ied,jsd:jed,nk))     ; phyto(n)%jzloss_n       = 0.0
       allocate(phyto(n)%jzloss_p(isd:ied,jsd:jed,nk))     ; phyto(n)%jzloss_p       = 0.0
       allocate(phyto(n)%jzloss_sio2(isd:ied,jsd:jed,nk))  ; phyto(n)%jzloss_sio2    = 0.0
       allocate(phyto(n)%jaggloss_fe(isd:ied,jsd:jed,nk))  ; phyto(n)%jaggloss_fe    = 0.0
       allocate(phyto(n)%jaggloss_n(isd:ied,jsd:jed,nk))   ; phyto(n)%jaggloss_n     = 0.0
       allocate(phyto(n)%jaggloss_p(isd:ied,jsd:jed,nk))   ; phyto(n)%jaggloss_p     = 0.0
       allocate(phyto(n)%jaggloss_sio2(isd:ied,jsd:jed,nk)); phyto(n)%jaggloss_sio2  = 0.0
       allocate(phyto(n)%jvirloss_fe(isd:ied,jsd:jed,nk))  ; phyto(n)%jvirloss_fe    = 0.0
       allocate(phyto(n)%jvirloss_n(isd:ied,jsd:jed,nk))   ; phyto(n)%jvirloss_n     = 0.0
       allocate(phyto(n)%jvirloss_p(isd:ied,jsd:jed,nk))   ; phyto(n)%jvirloss_p     = 0.0
       allocate(phyto(n)%jvirloss_sio2(isd:ied,jsd:jed,nk)); phyto(n)%jvirloss_sio2  = 0.0
       allocate(phyto(n)%jmortloss_fe(isd:ied,jsd:jed,nk))  ; phyto(n)%jmortloss_fe    = 0.0
       allocate(phyto(n)%jmortloss_n(isd:ied,jsd:jed,nk))   ; phyto(n)%jmortloss_n     = 0.0
       allocate(phyto(n)%jmortloss_p(isd:ied,jsd:jed,nk))   ; phyto(n)%jmortloss_p     = 0.0
       allocate(phyto(n)%jmortloss_sio2(isd:ied,jsd:jed,nk)); phyto(n)%jmortloss_sio2  = 0.0
       allocate(phyto(n)%jexuloss_fe(isd:ied,jsd:jed,nk))  ; phyto(n)%jexuloss_fe    = 0.0
       allocate(phyto(n)%jexuloss_n(isd:ied,jsd:jed,nk))   ; phyto(n)%jexuloss_n     = 0.0
       allocate(phyto(n)%jexuloss_p(isd:ied,jsd:jed,nk))   ; phyto(n)%jexuloss_p     = 0.0
       allocate(phyto(n)%jhploss_fe(isd:ied,jsd:jed,nk))   ; phyto(n)%jhploss_fe     = 0.0
       allocate(phyto(n)%jhploss_n(isd:ied,jsd:jed,nk))    ; phyto(n)%jhploss_n      = 0.0
       allocate(phyto(n)%jhploss_p(isd:ied,jsd:jed,nk))    ; phyto(n)%jhploss_p      = 0.0
       allocate(phyto(n)%jhploss_sio2(isd:ied,jsd:jed,nk)) ; phyto(n)%jhploss_sio2   = 0.0
       allocate(phyto(n)%juptake_fe(isd:ied,jsd:jed,nk))   ; phyto(n)%juptake_fe     = 0.0
       allocate(phyto(n)%juptake_nh4(isd:ied,jsd:jed,nk))  ; phyto(n)%juptake_nh4    = 0.0
       allocate(phyto(n)%juptake_no3(isd:ied,jsd:jed,nk))  ; phyto(n)%juptake_no3    = 0.0
       allocate(phyto(n)%juptake_po4(isd:ied,jsd:jed,nk))  ; phyto(n)%juptake_po4    = 0.0
       allocate(phyto(n)%uptake_p_2_n(isd:ied,jsd:jed,nk)) ; phyto(n)%uptake_p_2_n   = 0.0
       allocate(phyto(n)%jprod_n(isd:ied,jsd:jed,nk))      ; phyto(n)%jprod_n        = 0.0
       allocate(phyto(n)%liebig_lim(isd:ied,jsd:jed,nk))   ; phyto(n)%liebig_lim     = 0.0
       allocate(phyto(n)%mu(isd:ied,jsd:jed,nk))           ; phyto(n)%mu             = 0.0
       allocate(phyto(n)%po4lim(isd:ied,jsd:jed,nk))       ; phyto(n)%po4lim         = 0.0
       allocate(phyto(n)%q_fe_2_n(isd:ied,jsd:jed,nk))     ; phyto(n)%q_fe_2_n       = 0.0
       allocate(phyto(n)%q_p_2_n(isd:ied,jsd:jed,nk))      ; phyto(n)%q_p_2_n        = 0.0
       allocate(phyto(n)%q_si_2_n(isd:ied,jsd:jed,nk))     ; phyto(n)%q_si_2_n       = 0.0
       allocate(phyto(n)%theta(isd:ied,jsd:jed,nk))        ; phyto(n)%theta          = 0.0
       allocate(phyto(n)%chl(isd:ied,jsd:jed,nk))          ; phyto(n)%chl            = 0.0
       allocate(phyto(n)%f_mu_mem(isd:ied,jsd:jed,nk))     ; phyto(n)%f_mu_mem       = 0.0
       allocate(phyto(n)%mu_mix(isd:ied,jsd:jed,nk))       ; phyto(n)%mu_mix         = 0.0
       allocate(phyto(n)%stress_fac(isd:ied,jsd:jed,nk))   ; phyto(n)%stress_fac     = 0.0
       allocate(phyto(n)%nh4lim(isd:ied,jsd:jed,nk))       ; phyto(n)%nh4lim         = 0.0
       allocate(phyto(n)%no3lim(isd:ied,jsd:jed,nk))       ; phyto(n)%no3lim         = 0.0
       allocate(phyto(n)%vmove(isd:ied,jsd:jed,nk))        ; phyto(n)%vmove          = 0.0
    enddo
    !
    ! allocate and initialize array elements of only one phytoplankton group
    !
    allocate(phyto(DIAZO)%juptake_n2(isd:ied,jsd:jed,nk))   ; phyto(DIAZO)%juptake_n2    = 0.0
    allocate(phyto(DIAZO)%o2lim(isd:ied,jsd:jed,nk))        ; phyto(DIAZO)%o2lim         = 0.0
    allocate(phyto(LARGE)%juptake_sio4(isd:ied,jsd:jed,nk)) ; phyto(LARGE)%juptake_sio4  = 0.0
    allocate(phyto(LARGE)%silim(isd:ied,jsd:jed,nk))        ; phyto(LARGE)%silim         = 0.0
    allocate(phyto(MEDIUM)%juptake_sio4(isd:ied,jsd:jed,nk)); phyto(MEDIUM)%juptake_sio4 = 0.0
    allocate(phyto(MEDIUM)%silim(isd:ied,jsd:jed,nk))       ; phyto(MEDIUM)%silim        = 0.0
    !
    ! allocate and initialize arrays for bacteria
    !
    allocate(bact(1)%f_n(isd:ied,jsd:jed,nk))              ; bact(1)%f_n             = 0.0
    allocate(bact(1)%jzloss_n(isd:ied,jsd:jed,nk))         ; bact(1)%jzloss_n        = 0.0
    allocate(bact(1)%jzloss_p(isd:ied,jsd:jed,nk))         ; bact(1)%jzloss_p        = 0.0
    allocate(bact(1)%jvirloss_n(isd:ied,jsd:jed,nk))       ; bact(1)%jvirloss_n      = 0.0
    allocate(bact(1)%jvirloss_p(isd:ied,jsd:jed,nk))       ; bact(1)%jvirloss_p      = 0.0
    allocate(bact(1)%jhploss_n(isd:ied,jsd:jed,nk))        ; bact(1)%jhploss_n       = 0.0
    allocate(bact(1)%jhploss_p(isd:ied,jsd:jed,nk))        ; bact(1)%jhploss_p       = 0.0
    allocate(bact(1)%juptake_ldon(isd:ied,jsd:jed,nk))     ; bact(1)%juptake_ldon    = 0.0
    allocate(bact(1)%juptake_ldop(isd:ied,jsd:jed,nk))     ; bact(1)%juptake_ldop    = 0.0
    allocate(bact(1)%jprod_nh4(isd:ied,jsd:jed,nk))        ; bact(1)%jprod_nh4       = 0.0
    allocate(bact(1)%jprod_po4(isd:ied,jsd:jed,nk))        ; bact(1)%jprod_po4       = 0.0
    allocate(bact(1)%jprod_n(isd:ied,jsd:jed,nk))          ; bact(1)%jprod_n         = 0.0
    allocate(bact(1)%o2lim(isd:ied,jsd:jed,nk))            ; bact(1)%o2lim           = 0.0
    allocate(bact(1)%ldonlim(isd:ied,jsd:jed,nk))          ; bact(1)%ldonlim         = 0.0
    allocate(bact(1)%temp_lim(isd:ied,jsd:jed,nk))         ; bact(1)%temp_lim        = 0.0
    !
    ! CAS: allocate and initialize array elements for all zooplankton groups
    !
    do n = 1, NUM_ZOO
       allocate(zoo(n)%f_n(isd:ied,jsd:jed,nk))           ; zoo(n)%f_n            = 0.0
       allocate(zoo(n)%jzloss_n(isd:ied,jsd:jed,nk))      ; zoo(n)%jzloss_n       = 0.0
       allocate(zoo(n)%jzloss_p(isd:ied,jsd:jed,nk))      ; zoo(n)%jzloss_p       = 0.0
       allocate(zoo(n)%jhploss_n(isd:ied,jsd:jed,nk))     ; zoo(n)%jhploss_n      = 0.0
       allocate(zoo(n)%jhploss_p(isd:ied,jsd:jed,nk))     ; zoo(n)%jhploss_p      = 0.0
       allocate(zoo(n)%jingest_n(isd:ied,jsd:jed,nk))     ; zoo(n)%jingest_n      = 0.0
       allocate(zoo(n)%jingest_p(isd:ied,jsd:jed,nk))     ; zoo(n)%jingest_p      = 0.0
       allocate(zoo(n)%jingest_sio2(isd:ied,jsd:jed,nk))  ; zoo(n)%jingest_sio2   = 0.0
       allocate(zoo(n)%jingest_fe(isd:ied,jsd:jed,nk))    ; zoo(n)%jingest_fe     = 0.0
       allocate(zoo(n)%jprod_fed(isd:ied,jsd:jed,nk))     ; zoo(n)%jprod_fed      = 0.0
       allocate(zoo(n)%jprod_fedet(isd:ied,jsd:jed,nk))   ; zoo(n)%jprod_fedet    = 0.0
       allocate(zoo(n)%jprod_ndet(isd:ied,jsd:jed,nk))    ; zoo(n)%jprod_ndet     = 0.0
       allocate(zoo(n)%jprod_pdet(isd:ied,jsd:jed,nk))    ; zoo(n)%jprod_pdet     = 0.0
       allocate(zoo(n)%jprod_ldon(isd:ied,jsd:jed,nk))    ; zoo(n)%jprod_ldon     = 0.0
       allocate(zoo(n)%jprod_ldop(isd:ied,jsd:jed,nk))    ; zoo(n)%jprod_ldop     = 0.0
       allocate(zoo(n)%jprod_srdon(isd:ied,jsd:jed,nk))    ; zoo(n)%jprod_srdon   = 0.0
       allocate(zoo(n)%jprod_srdop(isd:ied,jsd:jed,nk))    ; zoo(n)%jprod_srdop   = 0.0
       allocate(zoo(n)%jprod_sldon(isd:ied,jsd:jed,nk))    ; zoo(n)%jprod_sldon   = 0.0
       allocate(zoo(n)%jprod_sldop(isd:ied,jsd:jed,nk))    ; zoo(n)%jprod_sldop   = 0.0
       allocate(zoo(n)%jprod_sidet(isd:ied,jsd:jed,nk))   ; zoo(n)%jprod_sidet    = 0.0
       allocate(zoo(n)%jprod_sio4(isd:ied,jsd:jed,nk))   ; zoo(n)%jprod_sio4      = 0.0
       allocate(zoo(n)%jprod_po4(isd:ied,jsd:jed,nk))     ; zoo(n)%jprod_po4      = 0.0
       allocate(zoo(n)%jprod_nh4(isd:ied,jsd:jed,nk))     ; zoo(n)%jprod_nh4      = 0.0
       allocate(zoo(n)%jprod_n(isd:ied,jsd:jed,nk))      ; zoo(n)%jprod_n         = 0.0
       allocate(zoo(n)%o2lim(isd:ied,jsd:jed,nk))        ; zoo(n)%o2lim           = 0.0
       allocate(zoo(n)%temp_lim(isd:ied,jsd:jed,nk))     ; zoo(n)%temp_lim        = 0.0
    enddo

    ! higher predator ingestion
    allocate(cobalt%hp_jingest_n(isd:ied,jsd:jed,nk))     ; cobalt%hp_jingest_n      = 0.0
    allocate(cobalt%hp_jingest_p(isd:ied,jsd:jed,nk))     ; cobalt%hp_jingest_p      = 0.0
    allocate(cobalt%hp_jingest_sio2(isd:ied,jsd:jed,nk))  ; cobalt%hp_jingest_sio2   = 0.0
    allocate(cobalt%hp_jingest_fe(isd:ied,jsd:jed,nk))    ; cobalt%hp_jingest_fe     = 0.0

    allocate(cobalt%f_alk(isd:ied, jsd:jed, 1:nk))        ; cobalt%f_alk=0.0
    allocate(cobalt%f_cadet_arag(isd:ied, jsd:jed, 1:nk)) ; cobalt%f_cadet_arag=0.0
    allocate(cobalt%f_cadet_calc(isd:ied, jsd:jed, 1:nk)) ; cobalt%f_cadet_calc=0.0
    allocate(cobalt%f_dic(isd:ied, jsd:jed, 1:nk))        ; cobalt%f_dic=0.0
    allocate(cobalt%f_fed(isd:ied, jsd:jed, 1:nk))        ; cobalt%f_fed=0.0
    allocate(cobalt%f_fedet(isd:ied, jsd:jed, 1:nk))      ; cobalt%f_fedet=0.0
    allocate(cobalt%f_ldon(isd:ied, jsd:jed, 1:nk))       ; cobalt%f_ldon=0.0
    allocate(cobalt%f_ldop(isd:ied, jsd:jed, 1:nk))       ; cobalt%f_ldop=0.0
    allocate(cobalt%f_lith(isd:ied, jsd:jed, 1:nk))       ; cobalt%f_lith=0.0
    allocate(cobalt%f_lithdet(isd:ied, jsd:jed, 1:nk))    ; cobalt%f_lithdet=0.0
    allocate(cobalt%f_ndet(isd:ied, jsd:jed, 1:nk))       ; cobalt%f_ndet=0.0
    allocate(cobalt%f_nh4(isd:ied, jsd:jed, 1:nk))        ; cobalt%f_nh4=0.0
    allocate(cobalt%f_no3(isd:ied, jsd:jed, 1:nk))        ; cobalt%f_no3=0.0
    allocate(cobalt%f_o2(isd:ied, jsd:jed, 1:nk))         ; cobalt%f_o2=0.0
    allocate(cobalt%f_pdet(isd:ied, jsd:jed, 1:nk))       ; cobalt%f_pdet=0.0
    allocate(cobalt%f_po4(isd:ied, jsd:jed, 1:nk))        ; cobalt%f_po4=0.0
    allocate(cobalt%f_srdon(isd:ied, jsd:jed, 1:nk))      ; cobalt%f_srdon=0.0
    allocate(cobalt%f_srdop(isd:ied, jsd:jed, 1:nk))      ; cobalt%f_srdop=0.0
    allocate(cobalt%f_sldon(isd:ied, jsd:jed, 1:nk))      ; cobalt%f_sldon=0.0
    allocate(cobalt%f_sldop(isd:ied, jsd:jed, 1:nk))      ; cobalt%f_sldop=0.0
    allocate(cobalt%f_sidet(isd:ied, jsd:jed, 1:nk))      ; cobalt%f_sidet=0.0
    allocate(cobalt%f_silg(isd:ied, jsd:jed, 1:nk))       ; cobalt%f_silg=0.0
    allocate(cobalt%f_simd(isd:ied, jsd:jed, 1:nk))       ; cobalt%f_simd=0.0
    allocate(cobalt%f_sio4(isd:ied, jsd:jed, 1:nk))       ; cobalt%f_sio4=0.0
    allocate(cobalt%co3_sol_arag(isd:ied, jsd:jed, 1:nk)) ; cobalt%co3_sol_arag=0.0
    allocate(cobalt%co3_sol_calc(isd:ied, jsd:jed, 1:nk)) ; cobalt%co3_sol_calc=0.0
    allocate(cobalt%rho_test(isd:ied, jsd:jed, 1:nk)) ; cobalt%rho_test=0.0
    allocate(cobalt%f_chl(isd:ied, jsd:jed, 1:nk))        ; cobalt%f_chl=0.0
    if (do_nh3_diag) allocate(cobalt%f_nh3(isd:ied, jsd:jed, 1:nk))        ; cobalt%f_nh3=0.0
    allocate(cobalt%f_co3_ion(isd:ied, jsd:jed, 1:nk))    ; cobalt%f_co3_ion=0.0
    allocate(cobalt%f_htotal(isd:ied, jsd:jed, 1:nk))     ; cobalt%f_htotal=0.0
    allocate(cobalt%f_irr_aclm(isd:ied, jsd:jed, 1:nk))    ; cobalt%f_irr_aclm=0.0
    allocate(cobalt%f_irr_aclm_z(isd:ied, jsd:jed, 1:nk))  ; cobalt%f_irr_aclm_z=0.0
    allocate(cobalt%f_irr_aclm_sfc(isd:ied, jsd:jed, 1:nk)) ; cobalt%f_irr_aclm_sfc=0.0
    allocate(cobalt%f_cased(isd:ied, jsd:jed, 1:nk))      ; cobalt%f_cased=0.0
    allocate(cobalt%f_cadet_arag_btf(isd:ied, jsd:jed, 1:nk)); cobalt%f_cadet_arag_btf=0.0
    allocate(cobalt%f_cadet_calc_btf(isd:ied, jsd:jed, 1:nk)); cobalt%f_cadet_calc_btf=0.0
    allocate(cobalt%f_fedet_btf(isd:ied, jsd:jed, 1:nk))  ; cobalt%f_fedet_btf=0.0
    allocate(cobalt%f_lithdet_btf(isd:ied, jsd:jed, 1:nk)); cobalt%f_lithdet_btf=0.0
    allocate(cobalt%f_ndet_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_ndet_btf=0.0
    allocate(cobalt%f_pdet_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_pdet_btf=0.0
    allocate(cobalt%f_sidet_btf(isd:ied, jsd:jed, 1:nk))  ; cobalt%f_sidet_btf=0.0
    allocate(cobalt%f_ndi_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_ndi_btf=0.0
    allocate(cobalt%f_nsm_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_nsm_btf=0.0
    allocate(cobalt%f_nmd_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_nmd_btf=0.0
    allocate(cobalt%f_nlg_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_nlg_btf=0.0
    allocate(cobalt%f_pdi_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_pdi_btf=0.0
    allocate(cobalt%f_psm_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_psm_btf=0.0
    allocate(cobalt%f_pmd_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_pmd_btf=0.0
    allocate(cobalt%f_plg_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_plg_btf=0.0
    allocate(cobalt%f_fedi_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_fedi_btf=0.0
    allocate(cobalt%f_fesm_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_fesm_btf=0.0
    allocate(cobalt%f_femd_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_femd_btf=0.0
    allocate(cobalt%f_felg_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_felg_btf=0.0
    allocate(cobalt%f_simd_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_simd_btf=0.0
    allocate(cobalt%f_silg_btf(isd:ied, jsd:jed, 1:nk))   ; cobalt%f_silg_btf=0.0

    allocate(cobalt%jnbact(isd:ied, jsd:jed, 1:nk))       ; cobalt%jnbact=0.0
    allocate(cobalt%jndi(isd:ied, jsd:jed, 1:nk))         ; cobalt%jndi=0.0
    allocate(cobalt%jnsm(isd:ied, jsd:jed, 1:nk))         ; cobalt%jnsm=0.0
    allocate(cobalt%jnmd(isd:ied, jsd:jed, 1:nk))         ; cobalt%jnmd=0.0
    allocate(cobalt%jnlg(isd:ied, jsd:jed, 1:nk))         ; cobalt%jnlg=0.0
    allocate(cobalt%jpdi(isd:ied, jsd:jed, 1:nk))         ; cobalt%jpdi=0.0
    allocate(cobalt%jpsm(isd:ied, jsd:jed, 1:nk))         ; cobalt%jpsm=0.0
    allocate(cobalt%jpmd(isd:ied, jsd:jed, 1:nk))         ; cobalt%jpmd=0.0
    allocate(cobalt%jplg(isd:ied, jsd:jed, 1:nk))         ; cobalt%jplg=0.0
    allocate(cobalt%jnsmz(isd:ied, jsd:jed, 1:nk))        ; cobalt%jnsmz=0.0
    allocate(cobalt%jnmdz(isd:ied, jsd:jed, 1:nk))        ; cobalt%jnmdz=0.0
    allocate(cobalt%jnlgz(isd:ied, jsd:jed, 1:nk))        ; cobalt%jnlgz=0.0
    allocate(cobalt%jalk(isd:ied, jsd:jed, 1:nk))         ; cobalt%jalk=0.0
    allocate(cobalt%jalk_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jalk_plus_btm=0.0
    allocate(cobalt%jcadet_arag(isd:ied, jsd:jed, 1:nk))  ; cobalt%jcadet_arag=0.0
    allocate(cobalt%jcadet_calc(isd:ied, jsd:jed, 1:nk))  ; cobalt%jcadet_calc=0.0
    allocate(cobalt%jdic(isd:ied, jsd:jed, 1:nk))         ; cobalt%jdic=0.0
    allocate(cobalt%jdic_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jdic_plus_btm=0.0
    allocate(cobalt%jdin_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jdin_plus_btm=0.0
    allocate(cobalt%jfed(isd:ied, jsd:jed, 1:nk))         ; cobalt%jfed=0.0
    allocate(cobalt%jfed_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jfed_plus_btm=0.0
    allocate(cobalt%jfedi(isd:ied, jsd:jed, 1:nk))        ; cobalt%jfedi=0.0
    allocate(cobalt%jfelg(isd:ied, jsd:jed, 1:nk))        ; cobalt%jfelg=0.0
    allocate(cobalt%jfemd(isd:ied, jsd:jed, 1:nk))        ; cobalt%jfemd=0.0
    allocate(cobalt%jfesm(isd:ied, jsd:jed, 1:nk))        ; cobalt%jfesm=0.0
    allocate(cobalt%jfedet(isd:ied, jsd:jed, 1:nk))       ; cobalt%jfedet=0.0
    allocate(cobalt%jldon(isd:ied, jsd:jed, 1:nk))        ; cobalt%jldon=0.0
    allocate(cobalt%jldop(isd:ied, jsd:jed, 1:nk))        ; cobalt%jldop=0.0
    allocate(cobalt%jlith(isd:ied, jsd:jed, 1:nk))        ; cobalt%jlith=0.0
    allocate(cobalt%jlithdet(isd:ied, jsd:jed, 1:nk))     ; cobalt%jlithdet=0.0
    allocate(cobalt%jndet(isd:ied, jsd:jed, 1:nk))        ; cobalt%jndet=0.0
    allocate(cobalt%jnh4(isd:ied, jsd:jed, 1:nk))         ; cobalt%jnh4=0.0
    allocate(cobalt%jnh4_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jnh4_plus_btm=0.0
    allocate(cobalt%jno3(isd:ied, jsd:jed, 1:nk))         ; cobalt%jno3=0.0
    allocate(cobalt%jno3_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jno3_plus_btm=0.0
    allocate(cobalt%jo2(isd:ied, jsd:jed, 1:nk))          ; cobalt%jo2=0.0
    allocate(cobalt%jo2_plus_btm(isd:ied, jsd:jed, 1:nk)) ; cobalt%jo2_plus_btm=0.0
    allocate(cobalt%jpdet(isd:ied, jsd:jed, 1:nk))        ; cobalt%jpdet=0.0
    allocate(cobalt%jpo4(isd:ied, jsd:jed, 1:nk))         ; cobalt%jpo4=0.0
    allocate(cobalt%jpo4_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jpo4_plus_btm=0.0
    allocate(cobalt%jsrdon(isd:ied, jsd:jed, 1:nk))       ; cobalt%jsrdon=0.0
    allocate(cobalt%jsrdop(isd:ied, jsd:jed, 1:nk))       ; cobalt%jsrdop=0.0
    allocate(cobalt%jsldon(isd:ied, jsd:jed, 1:nk))       ; cobalt%jsldon=0.0
    allocate(cobalt%jsldop(isd:ied, jsd:jed, 1:nk))       ; cobalt%jsldop=0.0
    allocate(cobalt%jsidet(isd:ied, jsd:jed, 1:nk))       ; cobalt%jsidet=0.0
    allocate(cobalt%jsilg(isd:ied, jsd:jed, 1:nk))        ; cobalt%jsilg=0.0
    allocate(cobalt%jsimd(isd:ied, jsd:jed, 1:nk))        ; cobalt%jsimd=0.0
    allocate(cobalt%jsio4(isd:ied, jsd:jed, 1:nk))        ; cobalt%jsio4=0.0
    allocate(cobalt%jsio4_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jsio4_plus_btm=0.0
    allocate(cobalt%jprod_fed(isd:ied, jsd:jed, 1:nk))    ; cobalt%jprod_fed=0.0
    allocate(cobalt%jprod_fedet(isd:ied, jsd:jed, 1:nk))  ; cobalt%jprod_fedet=0.0
    allocate(cobalt%jprod_ndet(isd:ied, jsd:jed, 1:nk))   ; cobalt%jprod_ndet=0.0
    allocate(cobalt%jprod_pdet(isd:ied, jsd:jed, 1:nk))   ; cobalt%jprod_pdet=0.0
    allocate(cobalt%jprod_ldon(isd:ied, jsd:jed, 1:nk))   ; cobalt%jprod_ldon=0.0
    allocate(cobalt%jprod_ldop(isd:ied, jsd:jed, 1:nk))   ; cobalt%jprod_ldop=0.0
    allocate(cobalt%jprod_sldon(isd:ied, jsd:jed, 1:nk))  ; cobalt%jprod_sldon=0.0
    allocate(cobalt%jprod_sldop(isd:ied, jsd:jed, 1:nk))  ; cobalt%jprod_sldop=0.0
    allocate(cobalt%jprod_srdon(isd:ied, jsd:jed, 1:nk))  ; cobalt%jprod_srdon=0.0
    allocate(cobalt%jprod_srdop(isd:ied, jsd:jed, 1:nk))  ; cobalt%jprod_srdop=0.0
    allocate(cobalt%jprod_sidet(isd:ied, jsd:jed, 1:nk))  ; cobalt%jprod_sidet=0.0
    allocate(cobalt%jprod_sio4(isd:ied, jsd:jed, 1:nk))   ; cobalt%jprod_sio4=0.0
    allocate(cobalt%jprod_lithdet(isd:ied, jsd:jed, 1:nk)); cobalt%jprod_lithdet=0.0
    allocate(cobalt%jprod_cadet_arag(isd:ied, jsd:jed, 1:nk)); cobalt%jprod_cadet_arag=0.0
    allocate(cobalt%jprod_cadet_calc(isd:ied, jsd:jed, 1:nk)); cobalt%jprod_cadet_calc=0.0
    allocate(cobalt%jprod_nh4(isd:ied, jsd:jed, 1:nk))    ; cobalt%jprod_nh4=0.0
    allocate(cobalt%jprod_nh4_plus_btm(isd:ied, jsd:jed, 1:nk))    ; cobalt%jprod_nh4_plus_btm=0.0
    allocate(cobalt%jprod_po4(isd:ied, jsd:jed, 1:nk))    ; cobalt%jprod_po4=0.0
    allocate(cobalt%det_jzloss_n(isd:ied, jsd:jed, 1:nk)) ; cobalt%det_jzloss_n=0.0
    allocate(cobalt%det_jzloss_p(isd:ied, jsd:jed, 1:nk)) ; cobalt%det_jzloss_p=0.0
    allocate(cobalt%det_jzloss_fe(isd:ied, jsd:jed, 1:nk)); cobalt%det_jzloss_fe=0.0
    allocate(cobalt%det_jzloss_si(isd:ied, jsd:jed, 1:nk)); cobalt%det_jzloss_si=0.0
    allocate(cobalt%det_jhploss_n(isd:ied, jsd:jed, 1:nk)); cobalt%det_jhploss_n=0.0
    allocate(cobalt%det_jhploss_p(isd:ied, jsd:jed, 1:nk)); cobalt%det_jhploss_p=0.0
    allocate(cobalt%det_jhploss_fe(isd:ied, jsd:jed, 1:nk)); cobalt%det_jhploss_fe=0.0
    allocate(cobalt%det_jhploss_si(isd:ied, jsd:jed, 1:nk)); cobalt%det_jhploss_si=0.0
    allocate(cobalt%jdiss_cadet_arag(isd:ied, jsd:jed, 1:nk)); cobalt%jdiss_cadet_arag=0.0
    allocate(cobalt%jdiss_cadet_arag_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jdiss_cadet_arag_plus_btm=0.0
    allocate(cobalt%jdiss_cadet_calc(isd:ied, jsd:jed, 1:nk)); cobalt%jdiss_cadet_calc=0.0
    allocate(cobalt%jdiss_cadet_calc_plus_btm(isd:ied, jsd:jed, 1:nk)); cobalt%jdiss_cadet_calc_plus_btm=0.0
    allocate(cobalt%jdiss_sidet(isd:ied, jsd:jed, 1:nk))  ; cobalt%jdiss_sidet=0.0
    allocate(cobalt%jremin_ndet(isd:ied, jsd:jed, 1:nk))  ; cobalt%jremin_ndet=0.0
    allocate(cobalt%jremin_pdet(isd:ied, jsd:jed, 1:nk))  ; cobalt%jremin_pdet=0.0
    allocate(cobalt%jremin_fedet(isd:ied, jsd:jed, 1:nk)) ; cobalt%jremin_fedet=0.0
    allocate(cobalt%jfe_ads(isd:ied, jsd:jed, 1:nk))      ; cobalt%jfe_ads=0.0
    allocate(cobalt%jfe_coast(isd:ied, jsd:jed, 1:nk))    ; cobalt%jfe_coast=0.0
    allocate(cobalt%jfe_iceberg(isd:ied, jsd:jed, 1:nk))  ; cobalt%jfe_iceberg=0.0
    allocate(cobalt%jno3_iceberg(isd:ied, jsd:jed, 1:nk)) ; cobalt%jno3_iceberg=0.0
    allocate(cobalt%jpo4_iceberg(isd:ied, jsd:jed, 1:nk)) ; cobalt%jpo4_iceberg=0.0
    allocate(cobalt%kfe_eq_lig(isd:ied, jsd:jed, 1:nk))   ; cobalt%kfe_eq_lig=0.0
    allocate(cobalt%feprime(isd:ied, jsd:jed, 1:nk))      ; cobalt%feprime=0.0
    allocate(cobalt%ligand(isd:ied, jsd:jed, 1:nk))       ; cobalt%ligand=0.0
    allocate(cobalt%fe_sol(isd:ied, jsd:jed, 1:nk))       ; cobalt%fe_sol=0.0
    allocate(cobalt%expkT(isd:ied, jsd:jed, 1:nk))        ; cobalt%expkT=0.0
    allocate(cobalt%expkreminT(isd:ied, jsd:jed, 1:nk))   ; cobalt%expkreminT=0.0
    allocate(cobalt%hp_o2lim(isd:ied, jsd:jed, 1:nk))     ; cobalt%hp_o2lim=0.0
    allocate(cobalt%hp_temp_lim(isd:ied, jsd:jed, 1:nk))  ; cobalt%hp_temp_lim=0.0
    allocate(cobalt%irr_inst(isd:ied, jsd:jed, 1:nk))     ; cobalt%irr_inst=0.0
    allocate(cobalt%irr_mix(isd:ied, jsd:jed, 1:nk))      ; cobalt%irr_mix=0.0
    allocate(cobalt%irr_aclm_inst(isd:ied, jsd:jed, 1:nk))   ; cobalt%irr_aclm_inst=0.0
    allocate(cobalt%jno3denit_wc(isd:ied, jsd:jed, 1:nk)) ; cobalt%jno3denit_wc=0.0
    allocate(cobalt%juptake_no3amx(isd:ied, jsd:jed, 1:nk))   ; cobalt%juptake_no3amx=0.0
    allocate(cobalt%juptake_nh4amx(isd:ied, jsd:jed, 1:nk))   ; cobalt%juptake_nh4amx=0.0
    allocate(cobalt%jo2resp_wc(isd:ied, jsd:jed, 1:nk))   ; cobalt%jo2resp_wc=0.0
    allocate(cobalt%jprod_no3nitrif(isd:ied, jsd:jed, 1:nk)) ; cobalt%jprod_no3nitrif=0.0
    allocate(cobalt%juptake_nh4nitrif(isd:ied, jsd:jed, 1:nk)) ; cobalt%juptake_nh4nitrif=0.0
    allocate(cobalt%jnamx(isd:ied, jsd:jed, 1:nk)) ; cobalt%jnamx=0.0
    allocate(cobalt%omega_arag(isd:ied, jsd:jed, 1:nk))   ; cobalt%omega_arag=0.0
    allocate(cobalt%omega_calc(isd:ied, jsd:jed, 1:nk))   ; cobalt%omega_calc=0.0
    allocate(cobalt%tot_layer_int_c(isd:ied, jsd:jed,1:nk))  ; cobalt%tot_layer_int_c=0.0
    allocate(cobalt%tot_layer_int_fe(isd:ied, jsd:jed,1:nk)) ; cobalt%tot_layer_int_fe=0.0
    allocate(cobalt%tot_layer_int_n(isd:ied, jsd:jed, 1:nk)) ; cobalt%tot_layer_int_n=0.0
    allocate(cobalt%tot_layer_int_p(isd:ied, jsd:jed, 1:nk)) ; cobalt%tot_layer_int_p=0.0
    allocate(cobalt%tot_layer_int_si(isd:ied, jsd:jed, 1:nk)); cobalt%tot_layer_int_si=0.0
    allocate(cobalt%tot_layer_int_o2(isd:ied, jsd:jed, 1:nk)); cobalt%tot_layer_int_o2=0.0
    allocate(cobalt%tot_layer_int_alk(isd:ied, jsd:jed, 1:nk)); cobalt%tot_layer_int_alk=0.0
    allocate(cobalt%total_filter_feeding(isd:ied,jsd:jed,1:nk)); cobalt%total_filter_feeding=0.0
    allocate(cobalt%nlg_diatoms(isd:ied, jsd:jed, 1:nk)); cobalt%nlg_diatoms=0.0
    allocate(cobalt%nmd_diatoms(isd:ied, jsd:jed, 1:nk)); cobalt%nmd_diatoms=0.0
    allocate(cobalt%q_si_2_n_lg_diatoms(isd:ied, jsd:jed, 1:nk)); cobalt%q_si_2_n_lg_diatoms=0.0
    allocate(cobalt%q_si_2_n_md_diatoms(isd:ied, jsd:jed, 1:nk)); cobalt%q_si_2_n_md_diatoms=0.0
    allocate(cobalt%zt(isd:ied, jsd:jed, 1:nk))           ; cobalt%zt=0.0
    allocate(cobalt%b_alk(isd:ied, jsd:jed))              ; cobalt%b_alk=0.0
    allocate(cobalt%b_dic(isd:ied, jsd:jed))              ; cobalt%b_dic=0.0
    allocate(cobalt%b_fed(isd:ied, jsd:jed))              ; cobalt%b_fed=0.0
    allocate(cobalt%b_nh4(isd:ied, jsd:jed))              ; cobalt%b_nh4=0.0
    allocate(cobalt%b_no3(isd:ied, jsd:jed))              ; cobalt%b_no3=0.0
    allocate(cobalt%b_o2(isd:ied, jsd:jed))               ; cobalt%b_o2=0.0
    allocate(cobalt%b_po4(isd:ied, jsd:jed))              ; cobalt%b_po4=0.0
    allocate(cobalt%b_sio4(isd:ied, jsd:jed))             ; cobalt%b_sio4=0.0
    allocate(cobalt%pco2_csurf(isd:ied, jsd:jed))         ; cobalt%pco2_csurf=0.0
    allocate(cobalt%co2_csurf(isd:ied, jsd:jed))          ; cobalt%co2_csurf=0.0
    allocate(cobalt%co2_alpha(isd:ied, jsd:jed))          ; cobalt%co2_alpha=0.0
    allocate(cobalt%nh3_csurf(isd:ied, jsd:jed))          ; cobalt%nh3_csurf=0.0
    allocate(cobalt%nh3_alpha(isd:ied, jsd:jed))          ; cobalt%nh3_alpha=0.0
    allocate(cobalt%pnh3_csurf(isd:ied, jsd:jed))         ; cobalt%pnh3_csurf=0.0
    allocate(cobalt%fcadet_arag_btm(isd:ied, jsd:jed))    ; cobalt%fcadet_arag_btm=0.0
    allocate(cobalt%fcadet_calc_btm(isd:ied, jsd:jed))    ; cobalt%fcadet_calc_btm=0.0
    allocate(cobalt%ffedet_btm(isd:ied, jsd:jed))         ; cobalt%ffedet_btm=0.0
    allocate(cobalt%flithdet_btm(isd:ied, jsd:jed))       ; cobalt%flithdet_btm=0.0
    allocate(cobalt%fpdet_btm(isd:ied, jsd:jed))          ; cobalt%fpdet_btm=0.0
    allocate(cobalt%fndet_btm(isd:ied, jsd:jed))          ; cobalt%fndet_btm=0.0
    allocate(cobalt%fsidet_btm(isd:ied, jsd:jed))         ; cobalt%fsidet_btm=0.0
    allocate(cobalt%ffetot_btm(isd:ied, jsd:jed))         ; cobalt%ffetot_btm=0.0
    allocate(cobalt%fptot_btm(isd:ied, jsd:jed))          ; cobalt%fptot_btm=0.0
    allocate(cobalt%fntot_btm(isd:ied, jsd:jed))          ; cobalt%fntot_btm=0.0
    allocate(cobalt%fsitot_btm(isd:ied, jsd:jed))         ; cobalt%fsitot_btm=0.0
    allocate(cobalt%fcased_burial(isd:ied, jsd:jed))      ; cobalt%fcased_burial=0.0
    allocate(cobalt%fcased_redis(isd:ied, jsd:jed))       ; cobalt%fcased_redis=0.0
    allocate(cobalt%fcased_redis_surfresp(isd:ied, jsd:jed)) ; cobalt%fcased_redis_surfresp=0.0
    allocate(cobalt%cased_redis_coef(isd:ied, jsd:jed))   ; cobalt%cased_redis_coef=0.0
    allocate(cobalt%cased_redis_delz(isd:ied, jsd:jed))   ; cobalt%cased_redis_delz=0.0
    allocate(cobalt%ffe_sed(isd:ied, jsd:jed))            ; cobalt%ffe_sed=0.0
    allocate(cobalt%ffe_geotherm(isd:ied, jsd:jed))       ; cobalt%ffe_geotherm=0.0
    allocate(cobalt%ffe_iceberg(isd:ied, jsd:jed))        ; cobalt%ffe_iceberg=0.0
    allocate(cobalt%fnfeso4red_sed(isd:ied, jsd:jed))     ; cobalt%fnfeso4red_sed=0.0
    allocate(cobalt%fno3denit_sed(isd:ied, jsd:jed))      ; cobalt%fno3denit_sed=0.0
    allocate(cobalt%fnoxic_sed(isd:ied, jsd:jed))         ; cobalt%fnoxic_sed=0.0
    allocate(cobalt%frac_burial(isd:ied, jsd:jed))        ; cobalt%frac_burial=0.0
    allocate(cobalt%fn_burial(isd:ied, jsd:jed))          ; cobalt%fn_burial=0.0
    allocate(cobalt%fp_burial(isd:ied, jsd:jed))          ; cobalt%fp_burial=0.0
!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem
    allocate(cobalt%dissoc(isd:ied, jsd:jed, 1:nk))        ; cobalt%dissoc=0.0
    allocate(cobalt%o2sat(isd:ied, jsd:jed, 1:nk))         ; cobalt%o2sat=0.0
    allocate(cobalt%remoc(isd:ied, jsd:jed, 1:nk))         ; cobalt%remoc=0.0
    allocate(cobalt%tot_layer_int_doc(isd:ied, jsd:jed, 1:nk)); cobalt%tot_layer_int_doc=0.0
    allocate(cobalt%tot_layer_int_poc(isd:ied, jsd:jed, 1:nk)); cobalt%tot_layer_int_poc=0.0
    allocate(cobalt%tot_layer_int_dic(isd:ied, jsd:jed, 1:nk)); cobalt%tot_layer_int_dic=0.0
    allocate(cobalt%f_alk_int_100(isd:ied, jsd:jed))       ; cobalt%f_alk_int_100=0.0
    allocate(cobalt%f_dic_int_100(isd:ied, jsd:jed))       ; cobalt%f_dic_int_100=0.0
    allocate(cobalt%f_din_int_100(isd:ied, jsd:jed))       ; cobalt%f_din_int_100=0.0
    allocate(cobalt%f_fed_int_100(isd:ied, jsd:jed))       ; cobalt%f_fed_int_100=0.0
    allocate(cobalt%f_po4_int_100(isd:ied, jsd:jed))       ; cobalt%f_po4_int_100=0.0
    allocate(cobalt%f_sio4_int_100(isd:ied, jsd:jed))      ; cobalt%f_sio4_int_100=0.0
    allocate(cobalt%jalk_100(isd:ied, jsd:jed))            ; cobalt%jalk_100=0.0
    allocate(cobalt%jdic_100(isd:ied, jsd:jed))            ; cobalt%jdic_100=0.0
    allocate(cobalt%jdin_100(isd:ied, jsd:jed))            ; cobalt%jdin_100=0.0
    allocate(cobalt%jfed_100(isd:ied, jsd:jed))            ; cobalt%jfed_100=0.0
    allocate(cobalt%jpo4_100(isd:ied, jsd:jed))            ; cobalt%jpo4_100=0.0
    allocate(cobalt%jsio4_100(isd:ied, jsd:jed))           ; cobalt%jsio4_100=0.0
    allocate(cobalt%jprod_ptot_100(isd:ied, jsd:jed))      ; cobalt%jprod_ptot_100=0.0
    allocate(cobalt%wc_vert_int_c(isd:ied, jsd:jed))       ; cobalt%wc_vert_int_c=0.0
    allocate(cobalt%wc_vert_int_dic(isd:ied, jsd:jed))        ; cobalt%wc_vert_int_dic=0.0
    allocate(cobalt%wc_vert_int_doc(isd:ied, jsd:jed))        ; cobalt%wc_vert_int_doc=0.0
    allocate(cobalt%wc_vert_int_poc(isd:ied, jsd:jed))        ; cobalt%wc_vert_int_poc=0.0
    allocate(cobalt%wc_vert_int_n(isd:ied, jsd:jed))          ; cobalt%wc_vert_int_n=0.0
    allocate(cobalt%wc_vert_int_p(isd:ied, jsd:jed))          ; cobalt%wc_vert_int_p=0.0
    allocate(cobalt%wc_vert_int_fe(isd:ied, jsd:jed))         ; cobalt%wc_vert_int_fe=0.0
    allocate(cobalt%wc_vert_int_si(isd:ied, jsd:jed))         ; cobalt%wc_vert_int_si=0.0
    allocate(cobalt%wc_vert_int_o2(isd:ied, jsd:jed))         ; cobalt%wc_vert_int_o2=0.0
    allocate(cobalt%wc_vert_int_alk(isd:ied, jsd:jed))        ; cobalt%wc_vert_int_alk=0.0
    allocate(cobalt%wc_vert_int_npp(isd:ied, jsd:jed))     ; cobalt%wc_vert_int_npp=0.0
    allocate(cobalt%wc_vert_int_jdiss_sidet(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_jdiss_sidet=0.0
    allocate(cobalt%wc_vert_int_jdiss_cadet(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_jdiss_cadet=0.0
    allocate(cobalt%wc_vert_int_jo2resp(isd:ied, jsd:jed))      ; cobalt%wc_vert_int_jo2resp=0.0
    allocate(cobalt%wc_vert_int_jprod_cadet(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_jprod_cadet=0.0
    allocate(cobalt%wc_vert_int_jno3denit(isd:ied, jsd:jed))    ; cobalt%wc_vert_int_jno3denit=0.0
    allocate(cobalt%wc_vert_int_jprod_no3nitrif(isd:ied, jsd:jed)) ; cobalt%wc_vert_int_jprod_no3nitrif=0.0
    allocate(cobalt%wc_vert_int_jnamx(isd:ied, jsd:jed)) ; cobalt%wc_vert_int_jnamx=0.0
    allocate(cobalt%wc_vert_int_juptake_nh4(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_juptake_nh4=0.0
    allocate(cobalt%wc_vert_int_jprod_nh4(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_jprod_nh4=0.0
    allocate(cobalt%wc_vert_int_juptake_no3(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_juptake_no3=0.0
    allocate(cobalt%wc_vert_int_nfix(isd:ied, jsd:jed))         ; cobalt%wc_vert_int_nfix=0.0
    allocate(cobalt%wc_vert_int_jfe_iceberg(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_jfe_iceberg=0.0
    allocate(cobalt%wc_vert_int_jno3_iceberg(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_jno3_iceberg=0.0
    allocate(cobalt%wc_vert_int_jpo4_iceberg(isd:ied, jsd:jed))  ; cobalt%wc_vert_int_jpo4_iceberg=0.0
!==============================================================================================================
    !
    ! allocate 100m integrated quantities
    !
    do n = 1, NUM_PHYTO
       allocate(phyto(n)%jprod_n_100(isd:ied,jsd:jed))      ; phyto(n)%jprod_n_100      = 0.0
       allocate(phyto(n)%jprod_n_new_100(isd:ied,jsd:jed))  ; phyto(n)%jprod_n_new_100  = 0.0
       allocate(phyto(n)%jzloss_n_100(isd:ied,jsd:jed))     ; phyto(n)%jzloss_n_100  = 0.0
       allocate(phyto(n)%jexuloss_n_100(isd:ied,jsd:jed))   ; phyto(n)%jexuloss_n_100  = 0.0
       allocate(phyto(n)%jvirloss_n_100(isd:ied,jsd:jed))   ; phyto(n)%jvirloss_n_100  = 0.0
       allocate(phyto(n)%jmortloss_n_100(isd:ied,jsd:jed))  ; phyto(n)%jmortloss_n_100  = 0.0
       allocate(phyto(n)%jaggloss_n_100(isd:ied,jsd:jed))   ; phyto(n)%jaggloss_n_100  = 0.0
       allocate(phyto(n)%f_n_100(isd:ied,jsd:jed))          ; phyto(n)%f_n_100  = 0.0
       allocate(phyto(n)%juptake_fe_100(isd:ied,jsd:jed))   ; phyto(n)%juptake_fe_100  = 0.0
       allocate(phyto(n)%juptake_po4_100(isd:ied,jsd:jed))  ; phyto(n)%juptake_po4_100  = 0.0
       ! Biomass-weighted limitation terms (for cmip)
       allocate(phyto(n)%nlim_bw_100(isd:ied,jsd:jed)) ; phyto(n)%nlim_bw_100 = 0.0
       allocate(phyto(n)%plim_bw_100(isd:ied,jsd:jed)) ; phyto(n)%plim_bw_100 = 0.0
       allocate(phyto(n)%irrlim_bw_100(isd:ied,jsd:jed)) ; phyto(n)%irrlim_bw_100 = 0.0
       allocate(phyto(n)%def_fe_bw_100(isd:ied,jsd:jed)) ; phyto(n)%def_fe_bw_100 = 0.0
       ! sinking fluxes
       allocate(phyto(n)%fn_btm(isd:ied,jsd:jed)) ; phyto(n)%fn_btm = 0.0
       allocate(phyto(n)%fp_btm(isd:ied,jsd:jed)) ; phyto(n)%fp_btm = 0.0
       allocate(phyto(n)%fsi_btm(isd:ied,jsd:jed)) ; phyto(n)%fsi_btm = 0.0
       allocate(phyto(n)%ffe_btm(isd:ied,jsd:jed)) ; phyto(n)%ffe_btm = 0.0
    enddo
    allocate(phyto(DIAZO)%jprod_n_n2_100(isd:ied,jsd:jed)); phyto(DIAZO)%jprod_n_n2_100 = 0.0
    allocate(cobalt%jprod_allphytos_100(isd:ied,jsd:jed))   ; cobalt%jprod_allphytos_100 = 0.0
    allocate(cobalt%jprod_allphytos_200(isd:ied,jsd:jed))   ; cobalt%jprod_allphytos_200 = 0.0
    allocate(cobalt%jprod_diat_100(isd:ied,jsd:jed))   ; cobalt%jprod_diat_100 = 0.0
    allocate(phyto(MEDIUM)%juptake_sio4_100(isd:ied,jsd:jed)) ; phyto(MEDIUM)%juptake_sio4_100 = 0.0
    allocate(phyto(LARGE)%juptake_sio4_100(isd:ied,jsd:jed)) ; phyto(LARGE)%juptake_sio4_100 = 0.0

    do n = 1, NUM_ZOO
       allocate(zoo(n)%jprod_n_100(isd:ied,jsd:jed))      ; zoo(n)%jprod_n_100      = 0.0
       allocate(zoo(n)%jingest_n_100(isd:ied,jsd:jed))    ; zoo(n)%jingest_n_100    = 0.0
       allocate(zoo(n)%jremin_n_100(isd:ied,jsd:jed))     ; zoo(n)%jremin_n_100     = 0.0
       allocate(zoo(n)%f_n_100(isd:ied,jsd:jed))          ; zoo(n)%f_n_100          = 0.0
    enddo

   do n = 1,2
       allocate(zoo(n)%jzloss_n_100(isd:ied,jsd:jed))     ; zoo(n)%jzloss_n_100     = 0.0
       allocate(zoo(n)%jprod_don_100(isd:ied,jsd:jed))    ; zoo(n)%jprod_don_100    = 0.0
   enddo

   do n = 2,3
       allocate(zoo(n)%jhploss_n_100(isd:ied,jsd:jed))    ; zoo(n)%jhploss_n_100     = 0.0
       allocate(zoo(n)%jprod_ndet_100(isd:ied,jsd:jed))   ; zoo(n)%jprod_ndet_100    = 0.0
   enddo

   allocate(cobalt%hp_jingest_n_100(isd:ied,jsd:jed))    ; cobalt%hp_jingest_n_100    = 0.0
   allocate(cobalt%hp_jremin_n_100(isd:ied,jsd:jed))     ; cobalt%hp_jremin_n_100     = 0.0
   allocate(cobalt%hp_jprod_ndet_100(isd:ied,jsd:jed))   ; cobalt%hp_jprod_ndet_100   = 0.0

   allocate(bact(1)%jprod_n_100(isd:ied,jsd:jed))   ; bact(1)%jprod_n_100 = 0.0
   allocate(bact(1)%jzloss_n_100(isd:ied,jsd:jed))  ; bact(1)%jzloss_n_100 = 0.0
   allocate(bact(1)%jvirloss_n_100(isd:ied,jsd:jed)); bact(1)%jvirloss_n_100 = 0.0
   allocate(bact(1)%jremin_n_100(isd:ied,jsd:jed))  ; bact(1)%jremin_n_100 = 0.0
   allocate(bact(1)%juptake_ldon_100(isd:ied,jsd:jed)) ; bact(1)%juptake_ldon_100 = 0.0
   allocate(bact(1)%f_n_100(isd:ied,jsd:jed))       ; bact(1)%f_n_100 = 0.0

   allocate(cobalt%jprod_lithdet_100(isd:ied,jsd:jed))      ; cobalt%jprod_lithdet_100 = 0.0
   allocate(cobalt%jprod_sidet_100(isd:ied,jsd:jed))        ; cobalt%jprod_sidet_100 = 0.0
   allocate(cobalt%jprod_cadet_calc_100(isd:ied,jsd:jed))   ; cobalt%jprod_cadet_calc_100 = 0.0
   allocate(cobalt%jprod_cadet_arag_100(isd:ied,jsd:jed))   ; cobalt%jprod_cadet_arag_100 = 0.0
   allocate(cobalt%jremin_ndet_100(isd:ied,jsd:jed))        ; cobalt%jremin_ndet_100 = 0.0
   allocate(cobalt%jprod_mesozoo_200(isd:ied,jsd:jed))      ; cobalt%jprod_mesozoo_200 = 0.0
   allocate(cobalt%daylength(isd:ied,jsd:jed))              ; cobalt%daylength = 0.0

   allocate(cobalt%f_ndet_100(isd:ied,jsd:jed))             ; cobalt%f_ndet_100 = 0.0
   allocate(cobalt%f_don_100(isd:ied,jsd:jed))              ; cobalt%f_don_100  = 0.0
   allocate(cobalt%f_silg_100(isd:ied,jsd:jed))             ; cobalt%f_silg_100 = 0.0
   allocate(cobalt%f_simd_100(isd:ied,jsd:jed))             ; cobalt%f_simd_100 = 0.0
   allocate(cobalt%f_mesozoo_200(isd:ied,jsd:jed))          ; cobalt%f_mesozoo_200 = 0.0

   allocate(cobalt%fndet_100(isd:ied,jsd:jed))             ; cobalt%fndet_100 = 0.0
   allocate(cobalt%fpdet_100(isd:ied,jsd:jed))             ; cobalt%fpdet_100 = 0.0
   allocate(cobalt%fsidet_100(isd:ied,jsd:jed))            ; cobalt%fsidet_100 = 0.0
   allocate(cobalt%flithdet_100(isd:ied,jsd:jed))          ; cobalt%flithdet_100 = 0.0
   allocate(cobalt%fcadet_calc_100(isd:ied,jsd:jed))       ; cobalt%fcadet_calc_100 = 0.0
   allocate(cobalt%fcadet_arag_100(isd:ied,jsd:jed))       ; cobalt%fcadet_arag_100 = 0.0
   allocate(cobalt%ffedet_100(isd:ied,jsd:jed))            ; cobalt%ffedet_100 = 0.0
   allocate(cobalt%fntot_100(isd:ied,jsd:jed))             ; cobalt%fntot_100 = 0.0
   allocate(cobalt%fptot_100(isd:ied,jsd:jed))             ; cobalt%fptot_100 = 0.0
   allocate(cobalt%fsitot_100(isd:ied,jsd:jed))            ; cobalt%fsitot_100 = 0.0
   allocate(cobalt%ffetot_100(isd:ied,jsd:jed))            ; cobalt%ffetot_100 = 0.0

   allocate(cobalt%btm_temp(isd:ied,jsd:jed))              ; cobalt%btm_temp = 0.0
   allocate(cobalt%btm_o2(isd:ied,jsd:jed))                ; cobalt%btm_o2 = 0.0
   allocate(cobalt%btm_no3(isd:ied,jsd:jed))               ; cobalt%btm_no3 = 0.0
   allocate(cobalt%btm_alk(isd:ied,jsd:jed))               ; cobalt%btm_alk = 0.0
   allocate(cobalt%btm_dic(isd:ied,jsd:jed))               ; cobalt%btm_dic = 0.0
   allocate(cobalt%btm_htotal(isd:ied,jsd:jed))            ; cobalt%btm_htotal = 0.0
   allocate(cobalt%btm_co3_ion(isd:ied,jsd:jed))           ; cobalt%btm_co3_ion = 0.0
   allocate(cobalt%btm_co3_sol_arag(isd:ied,jsd:jed))      ; cobalt%btm_co3_sol_arag = 0.0
   allocate(cobalt%btm_co3_sol_calc(isd:ied,jsd:jed))      ; cobalt%btm_co3_sol_calc = 0.0
   allocate(cobalt%btm_omega_arag(isd:ied,jsd:jed))        ; cobalt%btm_omega_arag = 0.0
   allocate(cobalt%btm_omega_calc(isd:ied,jsd:jed))        ; cobalt%btm_omega_calc = 0.0
   allocate(cobalt%cased_2d(isd:ied,jsd:jed))              ; cobalt%cased_2d = 0.0
   allocate(cobalt%grid_kmt_diag(isd:ied,jsd:jed))         ; cobalt%grid_kmt_diag = 0.0
   allocate(cobalt%k_bot_diag(isd:ied,jsd:jed))            ; cobalt%k_bot_diag = 0.0
   allocate(cobalt%rho_dzt_bot_diag(isd:ied,jsd:jed))      ; cobalt%rho_dzt_bot_diag = 0.0
   allocate(cobalt%rho_dzt_kmt_diag(isd:ied,jsd:jed))      ; cobalt%rho_dzt_kmt_diag = 0.0

   allocate(cobalt%btm_temp_old(isd:ied,jsd:jed))          ; cobalt%btm_temp_old = 0.0
   allocate(cobalt%btm_o2_old(isd:ied,jsd:jed))            ; cobalt%btm_o2_old = 0.0
   allocate(cobalt%btm_htotal_old(isd:ied,jsd:jed))        ; cobalt%btm_htotal_old = 0.0
   allocate(cobalt%btm_co3_ion_old(isd:ied,jsd:jed))       ; cobalt%btm_co3_ion_old = 0.0
   allocate(cobalt%btm_co3_sol_arag_old(isd:ied,jsd:jed))  ; cobalt%btm_co3_sol_arag_old = 0.0
   allocate(cobalt%btm_co3_sol_calc_old(isd:ied,jsd:jed))  ; cobalt%btm_co3_sol_calc_old = 0.0

   allocate(cobalt%o2min(isd:ied, jsd:jed))                ; cobalt%o2min=0.0
   allocate(cobalt%z_o2min(isd:ied, jsd:jed))              ; cobalt%z_o2min=0.0
   allocate(cobalt%z_sat_arag(isd:ied, jsd:jed))           ; cobalt%z_sat_arag=0.0
   allocate(cobalt%z_sat_calc(isd:ied, jsd:jed))           ; cobalt%z_sat_calc=0.0
   allocate(cobalt%mask_z_sat_arag(isd:ied, jsd:jed))      ; cobalt%mask_z_sat_arag = .FALSE.
   allocate(cobalt%mask_z_sat_calc(isd:ied, jsd:jed))      ; cobalt%mask_z_sat_calc = .FALSE.
   if (do_14c) then                                        !<<RADIOCARBON
      allocate(cobalt%c14_2_n(isd:ied, jsd:jed, 1:nk));        cobalt%c14_2_n=0.0
      allocate(cobalt%f_di14c(isd:ied, jsd:jed, 1:nk));        cobalt%f_di14c=0.0
      allocate(cobalt%f_do14c(isd:ied, jsd:jed, 1:nk));        cobalt%f_do14c=0.0
      allocate(cobalt%fpo14c(isd:ied, jsd:jed, 1:nk));         cobalt%fpo14c=0.0
      allocate(cobalt%j14c_decay_dic(isd:ied, jsd:jed, 1:nk)); cobalt%j14c_decay_dic=0.0
      allocate(cobalt%j14c_decay_doc(isd:ied, jsd:jed, 1:nk)); cobalt%j14c_decay_doc=0.0
      allocate(cobalt%j14c_reminp(isd:ied, jsd:jed, 1:nk));    cobalt%j14c_reminp=0.0
      allocate(cobalt%jdi14c(isd:ied, jsd:jed, 1:nk));         cobalt%jdi14c=0.0
      allocate(cobalt%jdo14c(isd:ied, jsd:jed, 1:nk));         cobalt%jdo14c=0.0
      allocate(cobalt%c14o2_csurf  (isd:ied, jsd:jed));        cobalt%c14o2_csurf=0.0
      allocate(cobalt%c14o2_alpha  (isd:ied, jsd:jed));        cobalt%c14o2_alpha=0.0
      allocate(cobalt%b_di14c      (isd:ied, jsd:jed));        cobalt%b_di14c=0.0
   endif                                                   !RADIOCARBON>>
      allocate(cobalt%runoff_flux_alk(isd:ied, jsd:jed));      cobalt%runoff_flux_alk=0.0
      allocate(cobalt%runoff_flux_dic(isd:ied, jsd:jed));      cobalt%runoff_flux_dic=0.0
      allocate(cobalt%runoff_flux_di14c(isd:ied, jsd:jed));    cobalt%runoff_flux_di14c=0.0
      allocate(cobalt%runoff_flux_lith(isd:ied, jsd:jed));     cobalt%runoff_flux_lith=0.0
      allocate(cobalt%runoff_flux_fed(isd:ied, jsd:jed));      cobalt%runoff_flux_fed=0.0
      allocate(cobalt%runoff_flux_no3(isd:ied, jsd:jed));      cobalt%runoff_flux_no3=0.0
      allocate(cobalt%runoff_flux_ldon(isd:ied, jsd:jed));     cobalt%runoff_flux_ldon=0.0
      allocate(cobalt%runoff_flux_sldon(isd:ied, jsd:jed));    cobalt%runoff_flux_sldon=0.0
      allocate(cobalt%runoff_flux_srdon(isd:ied, jsd:jed));    cobalt%runoff_flux_srdon=0.0
      allocate(cobalt%runoff_flux_ndet(isd:ied, jsd:jed));     cobalt%runoff_flux_ndet=0.0
      allocate(cobalt%runoff_flux_pdet(isd:ied, jsd:jed));     cobalt%runoff_flux_pdet=0.0
      allocate(cobalt%runoff_flux_po4(isd:ied, jsd:jed));      cobalt%runoff_flux_po4=0.0
      allocate(cobalt%runoff_flux_ldop(isd:ied, jsd:jed));     cobalt%runoff_flux_ldop=0.0
      allocate(cobalt%runoff_flux_sldop(isd:ied, jsd:jed));    cobalt%runoff_flux_sldop=0.0
      allocate(cobalt%runoff_flux_srdop(isd:ied, jsd:jed));    cobalt%runoff_flux_srdop=0.0
      allocate(cobalt%dry_fed(isd:ied, jsd:jed));              cobalt%dry_fed=0.0
      allocate(cobalt%wet_fed(isd:ied, jsd:jed));              cobalt%wet_fed=0.0
      allocate(cobalt%dry_lith(isd:ied, jsd:jed));             cobalt%dry_lith=0.0
      allocate(cobalt%wet_lith(isd:ied, jsd:jed));             cobalt%wet_lith=0.0
      allocate(cobalt%dry_no3(isd:ied, jsd:jed));              cobalt%dry_no3=0.0
      allocate(cobalt%wet_no3(isd:ied, jsd:jed));              cobalt%wet_no3=0.0
      allocate(cobalt%dry_nh4(isd:ied, jsd:jed));              cobalt%dry_nh4=0.0
      allocate(cobalt%wet_nh4(isd:ied, jsd:jed));              cobalt%wet_nh4=0.0
      allocate(cobalt%dry_po4(isd:ied, jsd:jed));              cobalt%dry_po4=0.0
      allocate(cobalt%wet_po4(isd:ied, jsd:jed));              cobalt%wet_po4=0.0
      allocate(cobalt%stf_gas_dic(isd:ied, jsd:jed));          cobalt%stf_gas_dic=0.0
      allocate(cobalt%stf_gas_o2(isd:ied, jsd:jed));           cobalt%stf_gas_o2=0.0
      allocate(cobalt%deltap_dic(isd:ied, jsd:jed));           cobalt%deltap_dic=0.0
      allocate(cobalt%deltap_o2(isd:ied, jsd:jed));            cobalt%deltap_o2=0.0
      allocate(cobalt%mld_aclm(isd:ied, jsd:jed));             cobalt%mld_aclm=0.0


  end subroutine user_allocate_arrays

  !
  !   This is an internal sub, not a public interface.
  !   Deallocate all the work arrays allocated by user_allocate_arrays.
  !
  subroutine user_deallocate_arrays
    integer n

    deallocate(cobalt%htotalhi,cobalt%htotallo)

    do n = 1, NUM_PHYTO
       deallocate(phyto(n)%P_C_max)
       deallocate(phyto(n)%alpha)
       deallocate(phyto(n)%bresp)
       deallocate(phyto(n)%def_fe)
       deallocate(phyto(n)%def_p)
       deallocate(phyto(n)%f_fe)
       deallocate(phyto(n)%f_n)
       deallocate(phyto(n)%f_p)
       deallocate(phyto(n)%felim)
       deallocate(phyto(n)%irrlim)
       deallocate(phyto(n)%jzloss_fe)
       deallocate(phyto(n)%jzloss_n)
       deallocate(phyto(n)%jzloss_p)
       deallocate(phyto(n)%jzloss_sio2)
       deallocate(phyto(n)%jaggloss_n)
       deallocate(phyto(n)%jaggloss_p)
       deallocate(phyto(n)%jaggloss_fe)
       deallocate(phyto(n)%jaggloss_sio2)
       deallocate(phyto(n)%jvirloss_n)
       deallocate(phyto(n)%jvirloss_p)
       deallocate(phyto(n)%jvirloss_fe)
       deallocate(phyto(n)%jvirloss_sio2)
       deallocate(phyto(n)%jmortloss_n)
       deallocate(phyto(n)%jmortloss_p)
       deallocate(phyto(n)%jmortloss_fe)
       deallocate(phyto(n)%jmortloss_sio2)
       deallocate(phyto(n)%jexuloss_n)
       deallocate(phyto(n)%jexuloss_p)
       deallocate(phyto(n)%jexuloss_fe)
       deallocate(phyto(n)%jhploss_fe)
       deallocate(phyto(n)%jhploss_n)
       deallocate(phyto(n)%jhploss_p)
       deallocate(phyto(n)%juptake_fe)
       deallocate(phyto(n)%juptake_nh4)
       deallocate(phyto(n)%juptake_no3)
       deallocate(phyto(n)%juptake_po4)
       deallocate(phyto(n)%uptake_p_2_n)
       deallocate(phyto(n)%jprod_n)
       deallocate(phyto(n)%liebig_lim)
       deallocate(phyto(n)%mu)
       deallocate(phyto(n)%po4lim)
       deallocate(phyto(n)%q_fe_2_n)
       deallocate(phyto(n)%q_p_2_n)
       deallocate(phyto(n)%q_si_2_n)
       deallocate(phyto(n)%theta)
       deallocate(phyto(n)%chl)
       deallocate(phyto(n)%f_mu_mem)
       deallocate(phyto(n)%mu_mix)
       deallocate(phyto(n)%stress_fac)
       deallocate(phyto(n)%juptake_fe_100)
       deallocate(phyto(n)%juptake_po4_100)
       deallocate(phyto(n)%nh4lim)
       deallocate(phyto(n)%no3lim)
       deallocate(phyto(n)%vmove)
       deallocate(phyto(n)%nlim_bw_100)
       deallocate(phyto(n)%plim_bw_100)
       deallocate(phyto(n)%irrlim_bw_100)
       deallocate(phyto(n)%def_fe_bw_100)
    enddo
    deallocate(phyto(DIAZO)%juptake_n2)
    deallocate(phyto(DIAZO)%o2lim)
    deallocate(phyto(LARGE)%juptake_sio4)
    deallocate(phyto(MEDIUM)%juptake_sio4)
    deallocate(phyto(LARGE)%juptake_sio4_100)
    deallocate(phyto(MEDIUM)%juptake_sio4_100)
    deallocate(phyto(LARGE)%silim)
    deallocate(phyto(MEDIUM)%silim)

    ! bacteria
    deallocate(bact(1)%f_n)
    deallocate(bact(1)%jzloss_n)
    deallocate(bact(1)%jzloss_p)
    deallocate(bact(1)%jvirloss_n)
    deallocate(bact(1)%jvirloss_p)
    deallocate(bact(1)%jhploss_n)
    deallocate(bact(1)%jhploss_p)
    deallocate(bact(1)%juptake_ldon)
    deallocate(bact(1)%juptake_ldop)
    deallocate(bact(1)%jprod_nh4)
    deallocate(bact(1)%jprod_po4)
    deallocate(bact(1)%jprod_n)
    deallocate(bact(1)%o2lim)
    deallocate(bact(1)%ldonlim)
    deallocate(bact(1)%temp_lim)

    ! zooplankton
    do n = 1, NUM_ZOO
       deallocate(zoo(n)%f_n)
       deallocate(zoo(n)%jzloss_n)
       deallocate(zoo(n)%jzloss_p)
       deallocate(zoo(n)%jhploss_n)
       deallocate(zoo(n)%jhploss_p)
       deallocate(zoo(n)%jingest_n)
       deallocate(zoo(n)%jingest_p)
       deallocate(zoo(n)%jingest_sio2)
       deallocate(zoo(n)%jingest_fe)
       deallocate(zoo(n)%jprod_ndet)
       deallocate(zoo(n)%jprod_pdet)
       deallocate(zoo(n)%jprod_ldon)
       deallocate(zoo(n)%jprod_ldop)
       deallocate(zoo(n)%jprod_srdon)
       deallocate(zoo(n)%jprod_srdop)
       deallocate(zoo(n)%jprod_sldon)
       deallocate(zoo(n)%jprod_sldop)
       deallocate(zoo(n)%jprod_fedet)
       deallocate(zoo(n)%jprod_fed)
       deallocate(zoo(n)%jprod_sidet)
       deallocate(zoo(n)%jprod_sio4)
       deallocate(zoo(n)%jprod_po4)
       deallocate(zoo(n)%jprod_nh4)
       deallocate(zoo(n)%jprod_n)
       deallocate(zoo(n)%o2lim)
       deallocate(zoo(n)%temp_lim)
    enddo

    deallocate(cobalt%f_alk)
    deallocate(cobalt%f_cadet_arag)
    deallocate(cobalt%f_cadet_calc)
    deallocate(cobalt%f_dic)
    deallocate(cobalt%f_fed)
    deallocate(cobalt%f_fedet)
    deallocate(cobalt%f_ldon)
    deallocate(cobalt%f_ldop)
    deallocate(cobalt%f_lith)
    deallocate(cobalt%f_lithdet)
    deallocate(cobalt%f_ndet)
    deallocate(cobalt%f_nh4)
    deallocate(cobalt%f_no3)
    deallocate(cobalt%f_o2)
    deallocate(cobalt%f_pdet)
    deallocate(cobalt%f_po4)
    deallocate(cobalt%f_srdon)
    deallocate(cobalt%f_srdop)
    deallocate(cobalt%f_sldon)
    deallocate(cobalt%f_sldop)
    deallocate(cobalt%f_sidet)
    deallocate(cobalt%f_silg)
    deallocate(cobalt%f_simd)
    deallocate(cobalt%f_sio4)
    deallocate(cobalt%co3_sol_arag)
    deallocate(cobalt%co3_sol_calc)
    deallocate(cobalt%rho_test)
    deallocate(cobalt%f_chl)
    if (allocated(cobalt%f_nh3)) deallocate(cobalt%f_nh3)
    deallocate(cobalt%f_co3_ion)
    deallocate(cobalt%f_htotal)
    deallocate(cobalt%f_irr_aclm)
    deallocate(cobalt%f_irr_aclm_z)
    deallocate(cobalt%f_irr_aclm_sfc)
    deallocate(cobalt%f_cased)
    deallocate(cobalt%f_cadet_arag_btf)
    deallocate(cobalt%f_cadet_calc_btf)
    deallocate(cobalt%f_fedet_btf)
    deallocate(cobalt%f_lithdet_btf)
    deallocate(cobalt%f_ndet_btf)
    deallocate(cobalt%f_pdet_btf)
    deallocate(cobalt%f_sidet_btf)
    deallocate(cobalt%f_ndi_btf)
    deallocate(cobalt%f_nsm_btf)
    deallocate(cobalt%f_nmd_btf)
    deallocate(cobalt%f_nlg_btf)
    deallocate(cobalt%f_pdi_btf)
    deallocate(cobalt%f_psm_btf)
    deallocate(cobalt%f_pmd_btf)
    deallocate(cobalt%f_plg_btf)
    deallocate(cobalt%f_fedi_btf)
    deallocate(cobalt%f_fesm_btf)
    deallocate(cobalt%f_femd_btf)
    deallocate(cobalt%f_felg_btf)
    deallocate(cobalt%f_simd_btf)
    deallocate(cobalt%f_silg_btf)
    deallocate(cobalt%jnbact)
    deallocate(cobalt%jndi)
    deallocate(cobalt%jnsm)
    deallocate(cobalt%jnmd)
    deallocate(cobalt%jnlg)
    deallocate(cobalt%jpdi)
    deallocate(cobalt%jpsm)
    deallocate(cobalt%jpmd)
    deallocate(cobalt%jplg)
    deallocate(cobalt%jnsmz)
    deallocate(cobalt%jnmdz)
    deallocate(cobalt%jnlgz)
    deallocate(cobalt%jalk)
    deallocate(cobalt%jalk_plus_btm)
    deallocate(cobalt%jcadet_arag)
    deallocate(cobalt%jcadet_calc)
    deallocate(cobalt%jdic)
    deallocate(cobalt%jdic_plus_btm)
    deallocate(cobalt%jdin_plus_btm)
    deallocate(cobalt%jfed)
    deallocate(cobalt%jfed_plus_btm)
    deallocate(cobalt%jfedi)
    deallocate(cobalt%jfelg)
    deallocate(cobalt%jfemd)
    deallocate(cobalt%jfesm)
    deallocate(cobalt%jfedet)
    deallocate(cobalt%jldon)
    deallocate(cobalt%jldop)
    deallocate(cobalt%jlith)
    deallocate(cobalt%jlithdet)
    deallocate(cobalt%jndet)
    deallocate(cobalt%jnh4)
    deallocate(cobalt%jnh4_plus_btm)
    deallocate(cobalt%jno3)
    deallocate(cobalt%jno3_plus_btm)
    deallocate(cobalt%jo2)
    deallocate(cobalt%jo2_plus_btm)
    deallocate(cobalt%jpdet)
    deallocate(cobalt%jpo4)
    deallocate(cobalt%jpo4_plus_btm)
    deallocate(cobalt%jsrdon)
    deallocate(cobalt%jsrdop)
    deallocate(cobalt%jsldon)
    deallocate(cobalt%jsldop)
    deallocate(cobalt%jsidet)
    deallocate(cobalt%jsilg)
    deallocate(cobalt%jsimd)
    deallocate(cobalt%jsio4)
    deallocate(cobalt%jsio4_plus_btm)
    deallocate(cobalt%jprod_ndet)
    deallocate(cobalt%jprod_pdet)
    deallocate(cobalt%jprod_ldon)
    deallocate(cobalt%jprod_ldop)
    deallocate(cobalt%jprod_sldop)
    deallocate(cobalt%jprod_sldon)
    deallocate(cobalt%jprod_srdon)
    deallocate(cobalt%jprod_srdop)
    deallocate(cobalt%jprod_fed)
    deallocate(cobalt%jprod_fedet)
    deallocate(cobalt%jprod_sidet)
    deallocate(cobalt%jprod_sio4)
    deallocate(cobalt%jprod_lithdet)
    deallocate(cobalt%jprod_cadet_arag)
    deallocate(cobalt%jprod_cadet_calc)
    deallocate(cobalt%jprod_nh4)
    deallocate(cobalt%jprod_nh4_plus_btm)
    deallocate(cobalt%jprod_po4)
    deallocate(cobalt%det_jzloss_n)
    deallocate(cobalt%det_jzloss_p)
    deallocate(cobalt%det_jzloss_fe)
    deallocate(cobalt%det_jzloss_si)
    deallocate(cobalt%det_jhploss_n)
    deallocate(cobalt%det_jhploss_p)
    deallocate(cobalt%det_jhploss_fe)
    deallocate(cobalt%det_jhploss_si)
    deallocate(cobalt%jdiss_cadet_arag)
    deallocate(cobalt%jdiss_cadet_arag_plus_btm)
    deallocate(cobalt%jdiss_cadet_calc)
    deallocate(cobalt%jdiss_cadet_calc_plus_btm)
    deallocate(cobalt%jdiss_sidet)
    deallocate(cobalt%jremin_ndet)
    deallocate(cobalt%jremin_pdet)
    deallocate(cobalt%jremin_fedet)
    deallocate(cobalt%jfe_ads)
    deallocate(cobalt%jfe_coast)
    deallocate(cobalt%jfe_iceberg)
    deallocate(cobalt%jno3_iceberg)
    deallocate(cobalt%jpo4_iceberg)
    deallocate(cobalt%kfe_eq_lig)
    deallocate(cobalt%feprime)
    deallocate(cobalt%ligand)
    deallocate(cobalt%fe_sol)
    deallocate(cobalt%expkT)
    deallocate(cobalt%expkreminT)
    deallocate(cobalt%hp_o2lim)
    deallocate(cobalt%hp_temp_lim)
    deallocate(cobalt%hp_jingest_n)
    deallocate(cobalt%hp_jingest_p)
    deallocate(cobalt%hp_jingest_sio2)
    deallocate(cobalt%hp_jingest_fe)
    deallocate(cobalt%irr_inst)
    deallocate(cobalt%irr_mix)
    deallocate(cobalt%irr_aclm_inst)
    deallocate(cobalt%jno3denit_wc)
    deallocate(cobalt%juptake_no3amx)
    deallocate(cobalt%juptake_nh4amx)
    deallocate(cobalt%jo2resp_wc)
    deallocate(cobalt%jprod_no3nitrif)
    deallocate(cobalt%juptake_nh4nitrif)
    deallocate(cobalt%jnamx)
    deallocate(cobalt%omega_arag)
    deallocate(cobalt%omega_calc)
    deallocate(cobalt%tot_layer_int_c)
    deallocate(cobalt%tot_layer_int_fe)
    deallocate(cobalt%tot_layer_int_n)
    deallocate(cobalt%tot_layer_int_p)
    deallocate(cobalt%tot_layer_int_si)
    deallocate(cobalt%tot_layer_int_alk)
    deallocate(cobalt%tot_layer_int_o2)
    deallocate(cobalt%total_filter_feeding)
    deallocate(cobalt%nlg_diatoms)
    deallocate(cobalt%nmd_diatoms)
    deallocate(cobalt%q_si_2_n_lg_diatoms)
    deallocate(cobalt%q_si_2_n_md_diatoms)
    deallocate(cobalt%zt)
!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem
    deallocate(cobalt%dissoc)
    deallocate(cobalt%o2sat)
    deallocate(cobalt%remoc)
    deallocate(cobalt%tot_layer_int_doc)
    deallocate(cobalt%tot_layer_int_poc)
    deallocate(cobalt%tot_layer_int_dic)

!==============================================================================================================

    deallocate(cobalt%b_alk)
    deallocate(cobalt%b_dic)
    deallocate(cobalt%b_fed)
    deallocate(cobalt%b_nh4)
    deallocate(cobalt%b_no3)
    deallocate(cobalt%b_o2)
    deallocate(cobalt%b_po4)
    deallocate(cobalt%b_sio4)
    deallocate(cobalt%pco2_csurf)
    deallocate(cobalt%co2_csurf)
    deallocate(cobalt%co2_alpha)
    deallocate(cobalt%nh3_csurf)
    deallocate(cobalt%pnh3_csurf)
    deallocate(cobalt%nh3_alpha)
    deallocate(cobalt%fcadet_arag_btm)
    deallocate(cobalt%fcadet_calc_btm)
    deallocate(cobalt%ffedet_btm)
    deallocate(cobalt%flithdet_btm)
    deallocate(cobalt%fpdet_btm)
    deallocate(cobalt%fndet_btm)
    deallocate(cobalt%fsidet_btm)
    deallocate(cobalt%ffetot_btm)
    deallocate(cobalt%fptot_btm)
    deallocate(cobalt%fntot_btm)
    deallocate(cobalt%fsitot_btm)
    deallocate(cobalt%fcased_burial)
    deallocate(cobalt%fcased_redis)
    deallocate(cobalt%fcased_redis_surfresp)
    deallocate(cobalt%cased_redis_coef)
    deallocate(cobalt%cased_redis_delz)
    deallocate(cobalt%ffe_sed)
    deallocate(cobalt%ffe_geotherm)
    deallocate(cobalt%ffe_iceberg)
    deallocate(cobalt%fnfeso4red_sed)
    deallocate(cobalt%fno3denit_sed)
    deallocate(cobalt%fnoxic_sed)
    deallocate(cobalt%frac_burial)
    deallocate(cobalt%fn_burial)
    deallocate(cobalt%fp_burial)
    deallocate(cobalt%jprod_allphytos_100)
    deallocate(cobalt%jprod_allphytos_200)
    deallocate(cobalt%jprod_diat_100)
    deallocate(cobalt%hp_jingest_n_100)
    deallocate(cobalt%hp_jremin_n_100)
    deallocate(cobalt%hp_jprod_ndet_100)
    deallocate(cobalt%jprod_lithdet_100)
    deallocate(cobalt%jprod_sidet_100)
    deallocate(cobalt%jprod_cadet_arag_100)
    deallocate(cobalt%jprod_cadet_calc_100)
    deallocate(cobalt%jprod_mesozoo_200)
    deallocate(cobalt%daylength)
    deallocate(cobalt%jremin_ndet_100)
    deallocate(cobalt%f_ndet_100)
    deallocate(cobalt%f_don_100)
    deallocate(cobalt%f_silg_100)
    deallocate(cobalt%f_simd_100)
    deallocate(cobalt%f_mesozoo_200)
    deallocate(cobalt%fndet_100)
    deallocate(cobalt%fpdet_100)
    deallocate(cobalt%fsidet_100)
    deallocate(cobalt%fcadet_calc_100)
    deallocate(cobalt%fcadet_arag_100)
    deallocate(cobalt%ffedet_100)
    deallocate(cobalt%flithdet_100)
    deallocate(cobalt%btm_temp)
    deallocate(cobalt%fntot_100)
    deallocate(cobalt%fptot_100)
    deallocate(cobalt%ffetot_100)
    deallocate(cobalt%fsitot_100)
    deallocate(cobalt%btm_o2)
    deallocate(cobalt%btm_no3)
    deallocate(cobalt%btm_alk)
    deallocate(cobalt%btm_dic)
    deallocate(cobalt%btm_htotal)
    deallocate(cobalt%btm_co3_ion)
    deallocate(cobalt%btm_co3_sol_arag)
    deallocate(cobalt%btm_co3_sol_calc)
    deallocate(cobalt%btm_omega_arag)
    deallocate(cobalt%btm_omega_calc)
    deallocate(cobalt%grid_kmt_diag)
    deallocate(cobalt%k_bot_diag)
    deallocate(cobalt%rho_dzt_bot_diag)
    deallocate(cobalt%rho_dzt_kmt_diag)
    deallocate(cobalt%cased_2d)
    deallocate(cobalt%btm_temp_old)
    deallocate(cobalt%btm_o2_old)
    deallocate(cobalt%btm_htotal_old)
    deallocate(cobalt%btm_co3_ion_old)
    deallocate(cobalt%btm_co3_sol_arag_old)
    deallocate(cobalt%btm_co3_sol_calc_old)
    deallocate(cobalt%o2min)
    deallocate(cobalt%z_o2min)
    deallocate(cobalt%z_sat_arag)
    deallocate(cobalt%z_sat_calc)
    deallocate(cobalt%mask_z_sat_arag)
    deallocate(cobalt%mask_z_sat_calc)
!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem
    deallocate(cobalt%f_alk_int_100)
    deallocate(cobalt%f_dic_int_100)
    deallocate(cobalt%f_din_int_100)
    deallocate(cobalt%f_fed_int_100)
    deallocate(cobalt%f_po4_int_100)
    deallocate(cobalt%f_sio4_int_100)
    deallocate(cobalt%jalk_100)
    deallocate(cobalt%jdic_100)
    deallocate(cobalt%jdin_100)
    deallocate(cobalt%jfed_100)
    deallocate(cobalt%jpo4_100)
    deallocate(cobalt%jsio4_100)
    deallocate(cobalt%jprod_ptot_100)
    deallocate(cobalt%wc_vert_int_c)
    deallocate(cobalt%wc_vert_int_dic)
    deallocate(cobalt%wc_vert_int_doc)
    deallocate(cobalt%wc_vert_int_poc)
    deallocate(cobalt%wc_vert_int_n)
    deallocate(cobalt%wc_vert_int_p)
    deallocate(cobalt%wc_vert_int_fe)
    deallocate(cobalt%wc_vert_int_si)
    deallocate(cobalt%wc_vert_int_o2)
    deallocate(cobalt%wc_vert_int_alk)
    deallocate(cobalt%wc_vert_int_npp)
    deallocate(cobalt%wc_vert_int_jdiss_sidet)
    deallocate(cobalt%wc_vert_int_jdiss_cadet)
    deallocate(cobalt%wc_vert_int_jo2resp)
    deallocate(cobalt%wc_vert_int_jprod_cadet)
    deallocate(cobalt%wc_vert_int_jno3denit)
    deallocate(cobalt%wc_vert_int_jprod_no3nitrif)
    deallocate(cobalt%wc_vert_int_jnamx)
    deallocate(cobalt%wc_vert_int_juptake_nh4)
    deallocate(cobalt%wc_vert_int_jprod_nh4)
    deallocate(cobalt%wc_vert_int_juptake_no3)
    deallocate(cobalt%wc_vert_int_nfix)
    deallocate(cobalt%wc_vert_int_jfe_iceberg)
    deallocate(cobalt%wc_vert_int_jno3_iceberg)
    deallocate(cobalt%wc_vert_int_jpo4_iceberg)
!==============================================================================================================

    do n = 1, NUM_PHYTO
       deallocate(phyto(n)%jprod_n_100)
       deallocate(phyto(n)%jprod_n_new_100)
       deallocate(phyto(n)%jzloss_n_100)
       deallocate(phyto(n)%jexuloss_n_100)
       deallocate(phyto(n)%jvirloss_n_100)
       deallocate(phyto(n)%jmortloss_n_100)
       deallocate(phyto(n)%jaggloss_n_100)
       deallocate(phyto(n)%f_n_100)
       deallocate(phyto(n)%fn_btm)
       deallocate(phyto(n)%fp_btm)
       deallocate(phyto(n)%fsi_btm)
       deallocate(phyto(n)%ffe_btm)
    enddo
    deallocate(phyto(DIAZO)%jprod_n_n2_100)

    do n = 1, NUM_ZOO
       deallocate(zoo(n)%jprod_n_100)
       deallocate(zoo(n)%jingest_n_100)
       deallocate(zoo(n)%jremin_n_100)
       deallocate(zoo(n)%f_n_100)
    enddo

    do n = 1,2
       deallocate(zoo(n)%jzloss_n_100)
       deallocate(zoo(n)%jprod_don_100)
    enddo

    do n = 2,3
       deallocate(zoo(n)%jhploss_n_100)
       deallocate(zoo(n)%jprod_ndet_100)
    enddo

    deallocate(bact(1)%jprod_n_100)
    deallocate(bact(1)%jzloss_n_100)
    deallocate(bact(1)%jvirloss_n_100)
    deallocate(bact(1)%jremin_n_100)
    deallocate(bact(1)%juptake_ldon_100)
    deallocate(bact(1)%f_n_100)

    if (do_14c) then                                        !<<RADIOCARBON
      deallocate(cobalt%c14_2_n)
      deallocate(cobalt%f_di14c)
      deallocate(cobalt%f_do14c)
      deallocate(cobalt%fpo14c)
      deallocate(cobalt%j14c_decay_dic)
      deallocate(cobalt%j14c_decay_doc)
      deallocate(cobalt%j14c_reminp)
      deallocate(cobalt%jdi14c)
      deallocate(cobalt%jdo14c)
      deallocate(cobalt%c14o2_alpha)
      deallocate(cobalt%c14o2_csurf)
      deallocate(cobalt%b_di14c )
    endif                                                   !RADIOCARBON>>
      deallocate(cobalt%runoff_flux_alk)
      deallocate(cobalt%runoff_flux_dic)
      deallocate(cobalt%runoff_flux_di14c)
      deallocate(cobalt%runoff_flux_lith)
      deallocate(cobalt%runoff_flux_fed)
      deallocate(cobalt%runoff_flux_no3)
      deallocate(cobalt%runoff_flux_ldon)
      deallocate(cobalt%runoff_flux_sldon)
      deallocate(cobalt%runoff_flux_srdon)
      deallocate(cobalt%runoff_flux_ndet)
      deallocate(cobalt%runoff_flux_pdet)
      deallocate(cobalt%runoff_flux_po4)
      deallocate(cobalt%runoff_flux_ldop)
      deallocate(cobalt%runoff_flux_sldop)
      deallocate(cobalt%runoff_flux_srdop)
      deallocate(cobalt%dry_fed)
      deallocate(cobalt%wet_fed)
      deallocate(cobalt%dry_lith)
      deallocate(cobalt%wet_lith)
      deallocate(cobalt%dry_no3)
      deallocate(cobalt%wet_no3)
      deallocate(cobalt%dry_nh4)
      deallocate(cobalt%wet_nh4)
      deallocate(cobalt%dry_po4)
      deallocate(cobalt%wet_po4)
      deallocate(cobalt%stf_gas_dic)
      deallocate(cobalt%stf_gas_o2)
      deallocate(cobalt%deltap_dic)
      deallocate(cobalt%deltap_o2)
      deallocate(cobalt%mld_aclm)

  end subroutine user_deallocate_arrays


!f1p
 function calc_pka_nh3(tc,salt) result(pka)
    !temperature, salinity
    real, intent(in) :: tc,salt
    real :: pka,tk
!Bell 2007
!    pka = 10.0423-0.0315536*tc+0.003071*salt

!Clegg 1995
    real, parameter :: a1=0.0500616
    real, parameter :: a2=-9.412696
    real, parameter :: a3=-2.029559e-7
    real, parameter :: a4=-0.0142372
    real, parameter :: a5=1.46041e-5
    real, parameter :: a6=3.730005
    real, parameter :: a7=7.14045e-5
    real, parameter :: a8=-0.0229021
    real, parameter :: a9=-5.521278e-7
    real, parameter :: a10=1.95413e-4

    tk=tc+273.15;
    pka     = 9.244605-2729.33*(1/298.15-1./tk)  &
            + (a1+a2/tk+a3*tk**2.)*salt**0.5       &
            + (a4+a5*tk+a6/tk)*salt              &
            + (a7+a8/tk)*salt**2.                 &
            + (a9+a10/tk)*salt**3.;

  end function calc_pka_nh3

!salting out correction for solubility (Johnson 2010, Ocean Science)
  function saltout_correction(kh,vb,salt) result(C)
    real, intent(in) :: Kh,vb,salt
    real*8 :: log_kh
    real :: theta,C
    log_kh = log(kh)
    theta = (7.3353282561828962e-04 + (3.3961477466551352e-05*log_kh) + (-2.4088830102075734e-06*(log_kh)**2) + (1.5711393120941302e-07*(log_kh)**3))*log(vb)
    C = 10**(theta*salt)
  end function saltout_correction

  !schmidt number in water
  function schmidt_w(t,s,vb,rho) result(sc)
    !schmidt number of the gas in the water
    real, intent(in) :: t,s,vb
    real, intent(in), optional :: rho
    real             :: sc

    sc=2.*v_sw(t,s,rho)/(d_hm(t,s,vb)+d_wc(t,s,vb))
  end function schmidt_w

  function v_sw(t,s,rho) result(v)
    real, intent(in) :: t,s
    real, intent(in), optional :: rho
    real             :: n,p,v
    n=n_sw(t,s)*1e-3
    if (present(rho)) then
       p=rho
    else
       p=p_sw(t,s)
    end if
    v = 1e4*n/p
  end function  v_sw

  function p_sw(t,s) result(p)
    !density of sea water
    !millero and poisson (1981)
    real, intent(in) :: t,s
    real             :: p, a, b, c
    a = 0.824493-(4.0899e-3*t)+(7.6438e-5*(t**2))-(8.2467e-7*(t**3))+(5.3875e-9*(t**4))
    b = -5.72466e-3+(1.0277e-4*t)-(1.6546e-6*(t**2))
    c = 4.8314e-4
    ! density of pure water
    p = 999.842594+(6.793952e-2*t)-(9.09529e-3*(t**2))+(1.001685e-4*(t**3))-(1.120083e-6*(t**4))+(6.536332e-9*(t**5))
    !salinity correction
    p = (p+(a*s)+(b*(s**(1.5)))+(c*s))
  end function p_sw

  function d_wc(t,s,vb) result(d)
    real, intent(in) :: t,s,vb
    real             :: d
    real, parameter  :: phi = 2.6
    !wilkie and chang 1955
    d = ((t+273.15)*7.4e-8*(phi*18.01)**0.5)/((n_sw(t,s))*(vb**0.6))
  end function d_wc

  function d_hm(t,s,vb) result(d)
    real, intent(in) :: t,s,vb
    real             :: d, epsilonstar
    ! hayduk 1982
    epsilonstar = (9.58/vb)-1.12
    d=1.25e-8*(vb**(-0.19)-0.292)*((t+273.15)**(1.52))*((n_sw(t,s))**epsilonstar)
  end function d_hm

  function n_sw(t,s) result(n)
    !dynamic viscosity
    !laliberte 2007
    real :: n
    real, intent(in) :: t,s !temperature (c) and salinity
    !salt in the order nacl,kcl,cacl2,mgcl2,mgso4
    real, parameter :: mass_fraction(5) = (/ 0.798,0.022,0.033,0.047,0.1 /)
    real, parameter :: v1(5) = (/ 16.22 , 6.4883, 32.028, 24.032, 72.269/)
    real, parameter :: v2(5) = (/ 1.3229 , 1.3175, 0.78792, 2.2694, 2.2238/)
    real, parameter :: v3(5) = (/ 1.4849 , -0.7785, -1.1495,  3.7108, 6.6037/)
    real, parameter :: v4(5) = (/ 0.0074691 , 0.09272, 0.0026995,  0.021853, 0.0079004/)
    real, parameter :: v5(5) = (/ 30.78 , -1.3, 780860., -1.1236, 3340.1/)
    real, parameter :: v6(5) = (/ 2.0583 , 2.0811, 5.8442,0.14474, 6.1304/)

    real :: n_0,ln_n_m,w_i_ln_n_i_tot,ni,w_i_tot,w_i
    integer :: i
    w_i_tot=0
    w_i_ln_n_i_tot=0
    do i=1,5
       w_i = mass_fraction(i)*s/1000
       w_i_tot = w_i+w_i_tot
    enddo
    do i=1,5
       w_i = mass_fraction(i)*s/1000
       ni = (exp(((v1(i)*w_i_tot**v2(i))+v3(i))/((v4(i)*t) + 1)))/((v5(i)*(w_i_tot**v6(i)))+1)
       w_i_ln_n_i_tot = w_i_ln_n_i_tot + (w_i*log(ni))
    enddo
    n_0 = (t+246)/(137.37+(5.2842*t)+(0.05594*(t**2)))
    ln_n_m = (1-w_i_tot)*log(n_0)+w_i_ln_n_i_tot
    n = exp(ln_n_m)
  end function n_sw



end module generic_COBALT
