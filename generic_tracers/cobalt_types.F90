!> COBALT_glbl module consists of core parameters and vars 
!! to be used by generic COBALT related modules
!<----------------------------------------------------------------
module cobalt_types
  use field_manager_mod, only: fm_string_len, fm_path_name_len      
  implicit none; private

  !
  character(len=fm_string_len), parameter, public :: mod_name       = 'generic_COBALT'
  character(len=fm_string_len), parameter, public :: package_name   = 'generic_cobalt'

  !
  public phytoplankton
  public zooplankton
  public bacteria
  public generic_COBALT_type
  public vardesc

  !The following variables for using this module
  ! are overwritten by generic_tracer_nml namelist
  logical, public :: do_nh3_diag                             !< logic for setting NH3 diagnostic tracer field 

! Namelist Options

  character(len=10), public ::  co2_calc = 'mocsy'           !< carbonate formalation options. Default is 'mocsy'
  logical, public :: do_14c             = .false.            !< If true, then simulate radiocarbon 
  logical, public :: do_nh3_atm_ocean_exchange = .false.     ! If true, then do NH3 air-sea exchange 
  !
  logical, public :: do_vertfill_pre = .false.             !< Returns tracer arrays with sensible values
  logical, public :: debug           = .false.             !< not use   
  real, public    :: o2_min_nit= 0.01e-6                   !< Oxygen threshold for nitrification (mol O2 kg-1)
  real, public    :: k_o2_nit  = 3.9e-6                    !< Oxygen half saturation constant for nitrification
  real, public    :: irr_inhibit = 10.                     !< Irradiance inhibition term for nitrification (W m-2)
  real, public    :: gamma_nitrif= 3.5e6                   !< Rate constant for nitrification (month-1)
  real, public    :: k_nh3_nitrif= 3.1e-9                  !< NH3 half-saturation for nitrification (mol NH3 kg-1)  
  real, public    :: imbalance_tolerance=1.0e-10           !< the tolerance for non-conservation in C,N,P,Sc,Fe

  integer, public :: scheme_no3_nh4_lim = 2 !< Nitrate and ammonia limitation scheme options
                                            !! 1-Frost and Franzen (1992)
                                            !! 2-O'Neill

  integer, public :: scheme_nitrif = 3      !< nitrification scheme options:
                                            !! 1-default COBALT
                                            !! 2-update with no temperature dependence
                                            !! 3-update with temperature dependence

  ! parameters      
  integer, parameter, public :: NUM_PHYTO = 4 !< total number of phytoplankton groups
  integer, parameter, public :: NUM_ZOO = 3   !< total number of zooplankton groups
  integer, parameter, public :: NUM_BACT = 1  !< total number of bacteria groups
  integer, parameter, public :: NUM_PREY = 9  !< total numbers of prey groups
  integer, parameter, public :: DIAZO      = 1 !< ID for diazotrophs
  integer, parameter, public :: LARGE      = 2 !< ID for large phytoplankton
  integer, parameter, public :: MEDIUM     = 3 !< ID for medium phytoplankton
  integer, parameter, public :: SMALL      = 4 !< ID for small phytoplankton 

  real, parameter, public :: sperd = 24.0 * 3600.0    !< number of seconds in a day (sec)
  real, parameter, public :: I_sperd = 1.0/sperd      !< inverse of number of seconds in a day (sec)
  real, parameter, public :: spery = 365.25 * sperd   !< number of seconds in a year (sec)
  real, parameter, public :: I_spery = 1.0/spery      !< inverse of number of seconds in a year (sec)
  real, parameter, public :: epsln=1.0e-30            !< small, but non-zero value for numerical stability
  real, parameter, public :: missing_value1=-1.0e+10  !< large negative value to represent missing value in diags
  real, parameter, public :: vb_nh3 = 25.             !< Liquid molar volume at boiling point for NH3 (cm3 mol−1)
  real, parameter, public :: micromol2mol = 1.0e6     !< convert micromoles to moles (used in concentration parameters)
  real, parameter, public :: c2n = 106.0/16.0         !< convert nutrient ratios relative to carbon to ratios relative to nitrogen
  real, parameter, public :: micromolQpersec2W = 2.77e18/6.022e17 !< convert photosynthetically available radiation (micromole quanta
                                                      !! per-second) to watts. 

  !> An auxiliary type for storing varible names
  type vardesc
     character(len=fm_string_len) :: name     !< The variable name in a NetCDF file.
     character(len=fm_string_len) :: longname !< The long name of that variable.
     character(len=1)  :: hor_grid !< The hor. grid:  u, v, h, q, or 1.
     character(len=1)  :: z_grid   !< The vert. grid:  L, i, or 1.
     character(len=1)  :: t_grid   !< The time description: s, a, m, or 1.
     character(len=fm_string_len) :: units  !< The dimensions of the variable.
     character(len=1)  :: mem_size !< The size in memory: d or f.
  end type vardesc

  !> phytoplankton data type
  type phytoplankton
     real ::  alpha_hl          !< Chlorophyll a-specific initial slope of the photosynthesis-irradiance curve high level (g C g Chl-1 sec-1 (W m-2)-1)
     real ::  alpha_ll          !< Chlorophyll a-specific initial slope of the photosynthesis-irradiance curve low level (g C g Chl-1 sec-1 (W m-2)-1)
     real ::  fe_2_n_max        !< Maximum iron to nitrogen ratio (mol Fe mol N-1)
     real ::  p_2_n_static      !< Fixed P:N ratio in phytoplankton (mol P mol N-1)
     real ::  p_2_n_min         !< Minimum P:N ratio (mol P mol N-1)
     real ::  p_2_n_slope       !< P:N slope (mol P mol N-1 mol P-1 kg)
     real ::  p_2_n_max         !< Maximum P:N ratio (mol P mol N-1)
     real ::  k_fe_2_n          !< Half-saturation iron to nitrogen for phytoplankton growth (mol Fe mol N-1)
     real ::  k_fed             !< Half-saturation constant for iron uptake (mol Fed kg-1)
     real ::  k_nh4             !< Half-saturation constant for ammonium limitation (mol NH4 kg-1)
     real ::  k_no3             !< Half-saturation constant for nitrate limitation (mol NO3 kg-1)
     real ::  k_po4             !< Half-saturation constant for phosphate limitation (mol PO4 kg-1)
     real ::  k_sio4            !< Half-saturation constant for silicate limitation (mol SiO4 kg-1)
     real ::  P_C_max_hl        !< Light-saturated carbon-specific photosynthesis rate high level (s-1)
     real ::  P_C_max_ll        !< Light-saturated carbon-specific photosynthesis rate low level  (s-1)
     real ::  si_2_n_max        !< Maximum silica to nitrogen ratio (mol Si mol N-1)
     real ::  si_2_n_static     !< Fixed SI:N ratio in phytoplankton (mol Si mol N-1)
     real ::  thetamax          !< Maximum chlorophyll to carbon ratio (g Chl g C-1)
     real ::  bresp_frac_mixed  !< basal respiration rate in mixed layer (dimensionless)
     real ::  bresp_frac_strat  !< basal respiration rate outside mixed layer (dimensionless)
     real ::  sink_max          !< sinking rate (m sec-1)
     real ::  agg               !< aggregation loss coefficient (s-1 (mole N kg)-1)
     real ::  frac_mu_stress    !< fraction pf stress-driven loss (dimensionless)
     real ::  vir               !< Viral lysis loss coefficient (s-1 (mole N kg)-1)
     real ::  mort              !< mortality loss coefficient (s-1)
     real ::  exu               !< Maximum ingestion rate (dimensionless (fraction of NPP))
     real, ALLOCATABLE, dimension(:,:)  ::  jprod_n_100      !<
     real, ALLOCATABLE, dimension(:,:)  ::  jprod_n_new_100  !<
     real, ALLOCATABLE, dimension(:,:)  ::  jprod_n_n2_100   !<
     real, ALLOCATABLE, dimension(:,:)  ::  jzloss_n_100     !<
     real, ALLOCATABLE, dimension(:,:)  ::  jaggloss_n_100   !<
     real, ALLOCATABLE, dimension(:,:)  ::  jvirloss_n_100   !<
     real, ALLOCATABLE, dimension(:,:)  ::  jmortloss_n_100  !<
     real, ALLOCATABLE, dimension(:,:)  ::  jexuloss_n_100   !<
     real, ALLOCATABLE, dimension(:,:)  ::  f_n_100          !<
     real, ALLOCATABLE, dimension(:,:)  ::  juptake_fe_100   !<
     real, ALLOCATABLE, dimension(:,:)  ::  juptake_po4_100  !<
     real, ALLOCATABLE, dimension(:,:)  ::  juptake_sio4_100 !<
     real, ALLOCATABLE, dimension(:,:)  ::  nlim_bw_100      !<
     real, ALLOCATABLE, dimension(:,:)  ::  plim_bw_100      !<
     real, ALLOCATABLE, dimension(:,:)  ::  def_fe_bw_100    !<
     real, ALLOCATABLE, dimension(:,:)  ::  irrlim_bw_100    !<
     real, ALLOCATABLE, dimension(:,:)  ::  fn_btm           !<
     real, ALLOCATABLE, dimension(:,:)  ::  ffe_btm          !<
     real, ALLOCATABLE, dimension(:,:)  ::  fp_btm           !<
     real, ALLOCATABLE, dimension(:,:)  ::  fsi_btm          !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  P_C_max        !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  alpha          !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  bresp          !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  def_fe         !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  def_p          !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  f_fe           !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  f_n            !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  f_p            !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  felim          !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  irrlim         !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jzloss_fe      !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jzloss_n       !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jzloss_p       !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jzloss_sio2    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jaggloss_fe    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jaggloss_n     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jaggloss_p     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jaggloss_sio2  !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  stress_fac     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jvirloss_fe    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jvirloss_n     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jvirloss_p     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jvirloss_sio2  !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jmortloss_fe   !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jmortloss_n    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jmortloss_p    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jmortloss_sio2 !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jexuloss_fe    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jexuloss_n     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jexuloss_p     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jhploss_fe     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jhploss_n      !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jhploss_p      !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jhploss_sio2   !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  juptake_n2     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  juptake_fe     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  juptake_nh4    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  juptake_no3    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  juptake_po4    !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  juptake_sio4   !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  uptake_p_2_n   !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  jprod_n        !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  liebig_lim     !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  mu             !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  f_mu_mem       !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  mu_mix         !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  nh4lim         !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  no3lim         !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  po4lim         !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  o2lim          !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  q_fe_2_n       !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  q_p_2_n        !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  silim          !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  q_si_2_n       !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  theta          !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  chl            !<
     real, ALLOCATABLE, dimension(:,:,:)  ::  vmove          !<
     integer ::  id_P_C_max      = -1
     integer ::  id_alpha        = -1
     integer ::  id_bresp        = -1
     integer ::  id_def_fe       = -1
     integer ::  id_def_p        = -1
     integer ::  id_felim        = -1
     integer ::  id_irrlim       = -1
     integer ::  id_jzloss_fe    = -1
     integer ::  id_jzloss_n     = -1
     integer ::  id_jzloss_p     = -1
     integer ::  id_jzloss_sio2  = -1
     integer ::  id_jaggloss_fe  = -1
     integer ::  id_jaggloss_n   = -1
     integer ::  id_jaggloss_p   = -1
     integer ::  id_jaggloss_sio2= -1
     integer ::  id_stress_fac   = -1
     integer ::  id_jvirloss_fe  = -1
     integer ::  id_jvirloss_n   = -1
     integer ::  id_jvirloss_p   = -1
     integer ::  id_jvirloss_sio2= -1
     integer ::  id_jmortloss_fe  = -1
     integer ::  id_jmortloss_n   = -1
     integer ::  id_jmortloss_p   = -1
     integer ::  id_jmortloss_sio2= -1
     integer ::  id_jexuloss_n   = -1
     integer ::  id_jexuloss_p   = -1
     integer ::  id_jexuloss_fe  = -1
     integer ::  id_jhploss_fe   = -1
     integer ::  id_jhploss_n    = -1
     integer ::  id_jhploss_p    = -1
     integer ::  id_jhploss_sio2 = -1
     integer ::  id_juptake_n2   = -1
     integer ::  id_juptake_fe   = -1
     integer ::  id_juptake_nh4  = -1
     integer ::  id_juptake_no3  = -1
     integer ::  id_juptake_po4  = -1
     integer ::  id_juptake_sio4 = -1
     integer ::  id_jprod_n      = -1
     integer ::  id_liebig_lim   = -1
     integer ::  id_mu           = -1
     integer ::  id_f_mu_mem     = -1
     integer ::  id_mu_mix       = -1
     integer ::  id_nh4lim       = -1
     integer ::  id_no3lim       = -1
     integer ::  id_po4lim       = -1
     integer ::  id_o2lim        = -1
     integer ::  id_q_fe_2_n     = -1
     integer ::  id_q_p_2_n      = -1
     integer ::  id_silim        = -1
     integer ::  id_q_si_2_n     = -1
     integer ::  id_theta        = -1
     integer ::  id_chl          = -1
     integer ::  id_vmove        = -1
     integer ::  id_jprod_n_100  = -1
     integer ::  id_jprod_n_new_100  = -1
     integer ::  id_jprod_n_n2_100 = -1
     integer ::  id_jzloss_n_100     = -1
     integer ::  id_jaggloss_n_100   = -1
     integer ::  id_jvirloss_n_100   = -1
     integer ::  id_jmortloss_n_100  = -1
     integer ::  id_jexuloss_n_100   = -1
     integer ::  id_f_n_100          = -1
     integer ::  id_sfc_f_n          = -1
     integer ::  id_sfc_chl          = -1
     integer ::  id_sfc_def_fe       = -1
     integer ::  id_sfc_felim        = -1
     integer ::  id_sfc_q_fe_2_n     = -1
     integer ::  id_sfc_q_p_2_n      = -1
     integer ::  id_sfc_nh4lim       = -1
     integer ::  id_sfc_no3lim       = -1
     integer ::  id_sfc_po4lim       = -1
     integer ::  id_sfc_irrlim       = -1
     integer ::  id_sfc_theta        = -1
     integer ::  id_sfc_mu           = -1
     integer ::  id_fn_btm           = -1
     integer ::  id_fp_btm           = -1
     integer ::  id_ffe_btm          = -1
     integer ::  id_fsi_btm          = -1
  end type phytoplankton

  !> zooplankton data type
  type zooplankton
    real imax              !< maximum ingestion rate (sec-1)
    real ki                !< half-sat for ingestion (moles N m-3)
    real gge_max           !< max gross growth efficiciency (approached as i >> bresp, dimensionless)
    real nswitch           !< switching parameter (dimensionless)
    real mswitch           !< switching parameter (dimensionless)
    real bresp             !< basal respiration rate (sec-1)
    real ktemp             !< temperature dependence of zooplankton rates (C-1)
    real upswim_chl_thresh !< threshold for swimming the the mixed layer (chl/chl_surf < thresh)
    real upswim_I_thresh   !< Irradiance threshold for upward swimming (watts m-2)
    real swim_max          !< maximum upward swimming speed (m s-2)
    real phi_det           !< fraction of ingested N to detritus
    real phi_ldon          !< fraction of ingested N/P to labile don
    real phi_sldon         !< fraction of ingested N/P to semi-labile don
    real phi_srdon         !< fraction of ingested N/P to semi-refractory don
    real phi_ldop          !< fraction of ingested N/P to labile dop
    real phi_sldop         !< fraction of ingested N/P to semi-labile dop
    real phi_srdop         !< fraction of ingested N/P to semi-refractory dop
    real q_p_2_n           !< p:n ratio of zooplankton
    real ipa_smp           !< innate prey availability of low-light adapt. small phytos
    real ipa_mdp           !< innate prey availability of medium phytoplankton
    real ipa_lgp           !< innate prey availability of large phytoplankton
    real ipa_diaz          !< innate prey availability of diazotrophs
    real ipa_smz           !< innate prey availability of small zooplankton
    real ipa_mdz           !< innate prey availability of large zooplankton
    real ipa_lgz           !< innate prey availability of x-large zooplankton
    real ipa_det           !< innate prey availability of detritus
    real ipa_bact          !< innate prey availability for bacteria
    real, ALLOCATABLE, dimension(:,:)  ::   jprod_n_100     !< zooplankton nitrogen prod. integral in upper 100m 
    real, ALLOCATABLE, dimension(:,:)  ::   jingest_n_100   !< zooplankton nitrogen ingestion integral in upper 100m  
    real, ALLOCATABLE, dimension(:,:)  ::   jzloss_n_100    !< zooplankton nitrogen loss to zooplankton integral in upper 100m  
    real, ALLOCATABLE, dimension(:,:)  ::   jhploss_n_100   !< zooplankton nitrogen loss to higher preds. integral in upper 100m 
    real, ALLOCATABLE, dimension(:,:)  ::   jprod_ndet_100  !< zooplankton nitrogen detritus prod. integral in upper 100m 
    real, ALLOCATABLE, dimension(:,:)  ::   jprod_don_100   !< zooplankton dissolved org. nitrogen prod. integral in upper 100m
    real, ALLOCATABLE, dimension(:,:)  ::   jremin_n_100    !< zooplankton nitrogen remineralization integral in upper 100m 
    real, ALLOCATABLE, dimension(:,:)  ::   f_n_100         !< zooplankton nitrogen biomass in upper 100m
    real, ALLOCATABLE, dimension(:,:,:) ::  f_n          !< zooplankton biomass
    real, ALLOCATABLE, dimension(:,:,:) ::  jzloss_n     !< Losses of n due to consumption by other zooplankton groups
    real, ALLOCATABLE, dimension(:,:,:) ::  jzloss_p     !< Losses of p due to consumption by other zooplankton groups
    real, ALLOCATABLE, dimension(:,:,:) ::  jhploss_n    !< Losses of n due to consumption by unresolved higher preds
    real, ALLOCATABLE, dimension(:,:,:) ::  jhploss_p    !< Losses of p due to consumption by unresolved higher preds
    real, ALLOCATABLE, dimension(:,:,:) ::  jingest_n    !< Total ingestion of n
    real, ALLOCATABLE, dimension(:,:,:) ::  jingest_p    !< Total ingestion of p
    real, ALLOCATABLE, dimension(:,:,:) ::  jingest_sio2 !< Total ingestion of silicate
    real, ALLOCATABLE, dimension(:,:,:) ::  jingest_fe   !< Total ingestion of iron
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_ndet   !< production of nitrogen detritus by zooplankton group
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_pdet   !< production of phosphorous detritus by zooplankton group
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_ldon   !< production of labile dissolved organic N by zooplankton group
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_ldop   !< production of labile dissolved organic P by zooplankton group
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_srdon  !< production of semi-refractory dissolved organic N by zooplankton group
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_srdop  !< production of semi-refractory dissolved organic P by zooplankton group
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_sldon  !< production of semi-labile dissolved organic N by zooplankton group
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_sldop  !< production of semi-labile dissolved organic P by zooplankton group
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_fed    !< production of dissolved iron
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_fedet  !< production of iron detritus
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_sidet  !< production of silica detritus
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_sio4   !< production of silicate via rapid dissolution at surface
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_po4    !< phosphate production by zooplankton
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_nh4    !< ammonia production by zooplankton
    real, ALLOCATABLE, dimension(:,:,:) ::  jprod_n      !< zooplankton production
    real, ALLOCATABLE, dimension(:,:,:) ::  o2lim        !< oxygen limitation of zooplankton activity
    real, ALLOCATABLE, dimension(:,:,:) ::  temp_lim     !< Temperature limitation
    real, ALLOCATABLE, dimension(:,:,:) ::  vmove        !< Vertical movement
    integer ::  id_jzloss_n       = -1 !< ID associated with diagnostics for losses of n due to consumption by other zooplankton groups
    integer ::  id_jzloss_p       = -1 !< ID associated with diagnostics for losses of p due to consumption by other zooplankton groups
    integer ::  id_jhploss_n      = -1 !< ID associated with diagnostics for losses of n due to consumption by unresolved higher preds 
    integer ::  id_jhploss_p      = -1 !< ID associated with diagnostics for losses of p due to consumption by unresolved higher preds
    integer ::  id_jingest_n      = -1 !< ID associated with diagnostics for total ingestion of n
    integer ::  id_jingest_p      = -1 !< ID associated with diagnostics for total ingestion of p
    integer ::  id_jingest_sio2   = -1 !< ID associated with diagnostics for total ingestion of silicate
    integer ::  id_jingest_fe     = -1 !< ID associated with diagnostics for total ingestion of iron
    integer ::  id_jprod_ndet     = -1 !< ID associated with diagnostics for production of nitrogen detritus by zooplankton group
    integer ::  id_jprod_pdet     = -1 !< ID associated with diagnostics for production of phosphorous detritus by zooplankton group
    integer ::  id_jprod_ldon     = -1 !< ID associated with diagnostics for production of labile dissolved organic N by zooplankton group
    integer ::  id_jprod_ldop     = -1 !< ID associated with diagnostics for production of labile dissolved organic P by zooplankton group
    integer ::  id_jprod_srdon    = -1 !< ID associated with diagnostics for production of semi-refractory dissolved organic N by zooplankton group
    integer ::  id_jprod_srdop    = -1 !< ID associated with diagnostics for production of semi-refractory dissolved organic P by zooplankton group
    integer ::  id_jprod_sldon    = -1 !< ID associated with diagnostics for production of semi-labile dissolved organic N by zooplankton group
    integer ::  id_jprod_sldop    = -1 !< ID associated with diagnostics for production of semi-labile dissolved organic P by zooplankton group
    integer ::  id_jprod_fed      = -1 !< ID associated with diagnostics for production of dissolved iron
    integer ::  id_jprod_fedet    = -1 !< ID associated with diagnostics for production of iron detritus
    integer ::  id_jprod_sidet    = -1 !< ID associated with diagnostics for production of silica detritus
    integer ::  id_jprod_sio4     = -1 !< ID associated with diagnostics for production of silicate via rapid dissolution at surface
    integer ::  id_jprod_po4      = -1 !< ID associated with diagnostics for phosphate production by zooplankton
    integer ::  id_jprod_nh4      = -1 !< ID associated with diagnostics for ammonia production by zooplankton
    integer ::  id_jprod_n        = -1 !< ID associated with diagnostics for zooplankton production
    integer ::  id_o2lim          = -1 !< ID associated with diagnostics for oxygen limitation of zooplankton activity
    integer ::  id_temp_lim       = -1 !< ID associated with diagnostics for temperature limitation
    integer ::  id_vmove          = -1 !< ID associated with diagnostics for vertical movement
    integer ::  id_jprod_n_100    = -1 !< ID associated with diagnostics for zooplankton nitrogen prod. integral in upper 100m
    integer ::  id_jingest_n_100  = -1 !< ID associated with diagnostics for zooplankton nitrogen ingestion integral in upper 100m
    integer ::  id_jzloss_n_100   = -1 !< ID associated with diagnostics for zooplankton nitrogen loss to zooplankton integral in upper 100m
    integer ::  id_jhploss_n_100  = -1 !< ID associated with diagnostics for zooplankton nitrogen loss to higher preds. integral in upper 100m
    integer ::  id_jprod_ndet_100 = -1 !< ID associated with diagnostics for zooplankton nitrogen detritus prod. integral in upper 100m
    integer ::  id_jprod_don_100  = -1 !< ID associated with diagnostics for zooplankton dissolved org. nitrogen prod. integral in upper 100m
    integer ::  id_jremin_n_100   = -1 !< ID associated with diagnostics for zooplankton nitrogen remineralization integral in upper 100m
    integer ::  id_f_n_100        = -1 !< ID associated with diagnostics for zooplankton nitrogen biomass in upper 100m
  end type zooplankton

  !> bacteria data type
  type bacteria
    real ::  mu_max           !< maximum bacterial growth rate (sec-1)
    real ::  k_ldon           !< half-sat for nitrogen-limited growth (mmoles N m-3)
    real ::  gge_max          !< max gross growth efficiciency (dimensionless)
    real ::  amx_ge           !< growth efficiency due to anammox reaction (dimensionless)
    real ::  nitrif_ge        !< growth efficiency of nitrifying bacteria (dimensionless)
    real ::  bresp            !< basal respiration rate (sec-1)
    real ::  ktemp            !< temperature dependence of bacterial rates (C-1)
    real ::  vir              !< virus-driven loss rate for bacteria (sec-1 mmole N m-3)
    real ::  q_p_2_n          !< p:n ratio for bacteria
    real, ALLOCATABLE, dimension(:,:)  ::       jprod_n_100      !< Bacteria nitrogen prod. integral in upper 100m 
    real, ALLOCATABLE, dimension(:,:)  ::       jzloss_n_100     !< Bacteria nitrogen loss to zooplankton integral in upper 100m
    real, ALLOCATABLE, dimension(:,:)  ::       jvirloss_n_100   !< Bacteria nitrogen loss to viruses integral in upper 100m
    real, ALLOCATABLE, dimension(:,:)  ::       jremin_n_100     !< Bacteria nitrogen remineralization integral in upper 100m
    real, ALLOCATABLE, dimension(:,:)  ::       juptake_ldon_100 !< Bacterial uptake of labile dissolved org. nitrogen in upper 100m
    real, ALLOCATABLE, dimension(:,:)  ::       f_n_100          !< Bacterial nitrogen biomass in upper 100m
    real, ALLOCATABLE, dimension(:,:,:) ::      f_n              !< bacteria biomass
    real, ALLOCATABLE, dimension(:,:,:) ::      jzloss_n         !< Losses of n due to consumption by zooplankton
    real, ALLOCATABLE, dimension(:,:,:) ::      jzloss_p         !< Losses of p due to consumption by zooplankton
    real, ALLOCATABLE, dimension(:,:,:) ::      jhploss_n        !< Losses of n due to consumption by unresolved higher preds
    real, ALLOCATABLE, dimension(:,:,:) ::      jhploss_p        !< Losses of p due to consumption by unresolved higher preds
    real, ALLOCATABLE, dimension(:,:,:) ::      jvirloss_n       !< nitrogen losses via viruses
    real, ALLOCATABLE, dimension(:,:,:) ::      jvirloss_p       !< phosphorous losses via viruses
    real, ALLOCATABLE, dimension(:,:,:) ::      juptake_ldon     !< Total uptake of ldon
    real, ALLOCATABLE, dimension(:,:,:) ::      juptake_ldop     !< Total uptake of sldon
    real, ALLOCATABLE, dimension(:,:,:) ::      juptake_po4      !< phosphate uptake with anammox/nitrification
    real, ALLOCATABLE, dimension(:,:,:) ::      jprod_nh4        !< production of ammonia bacteria
    real, ALLOCATABLE, dimension(:,:,:) ::      jprod_po4        !< production of phosphate by bacteria
    real, ALLOCATABLE, dimension(:,:,:) ::      jprod_n          !< total free-living bacterial production
    real, ALLOCATABLE, dimension(:,:,:) ::      jprod_n_het      !< heterotrophic bacteria production
    real, ALLOCATABLE, dimension(:,:,:) ::      jprod_n_amx      !< anammox bacteria production
    real, ALLOCATABLE, dimension(:,:,:) ::      jprod_n_nitrif   !< nitrifying bacteria production
    real, ALLOCATABLE, dimension(:,:,:) ::      mu_h             !< growth rate of heterotrophic bacteria
    real, ALLOCATABLE, dimension(:,:,:) ::      mu_cstar         !< biomass turnover due to chemosynthesis
    real, ALLOCATABLE, dimension(:,:,:) ::      bhet             !< heterotrophic bacteria biomass
    real, ALLOCATABLE, dimension(:,:,:) ::      ldonlim          !< limitation due to organic substrate
    real, ALLOCATABLE, dimension(:,:,:) ::      o2lim            !< limitation due to oxygen
    real, ALLOCATABLE, dimension(:,:,:) ::      temp_lim         !< Temperature limitation
    integer ::  id_jzloss_n         = -1  !< ID associated with diagnostics for losses of n due to consumption by zooplankton
    integer ::  id_jzloss_p         = -1  !< ID associated with diagnostics for losses of p due to consumption by zooplankton
    integer ::  id_jhploss_n        = -1  !< ID associated with diagnostics for losses of n due to consumption by unresolved higher preds
    integer ::  id_jhploss_p        = -1  !< ID associated with diagnostics for losses of p due to consumption by unresolved higher preds
    integer ::  id_jvirloss_n       = -1  !< ID associated with diagnostics for nitrogen losses via viruses
    integer ::  id_jvirloss_p       = -1  !< ID associated with diagnostics for phosphorous losses via viruses
    integer ::  id_juptake_ldon     = -1  !< ID associated with diagnostics for total uptake of ldon
    integer ::  id_juptake_ldop     = -1  !< ID associated with diagnostics for total uptake of sldon
    integer ::  id_juptake_po4      = -1  !< ID associated with diagnostics for phosphate uptake with anammox/nitrification
    integer ::  id_jprod_nh4        = -1  !< ID associated with diagnostics for production of ammonia bacteria
    integer ::  id_jprod_po4        = -1  !< ID associated with diagnostics for production of phosphate by bacteria
    integer ::  id_jprod_n          = -1  !< ID associated with diagnostics for total free-living bacterial production
    integer ::  id_jprod_n_het      = -1  !< ID associated with diagnostics for heterotrophic bacteria production
    integer ::  id_jprod_n_amx      = -1  !< ID associated with diagnostics for anammox bacteria production
    integer ::  id_jprod_n_nitrif   = -1  !< ID associated with diagnostics for nitrifying bacteria production
    integer ::  id_mu_h             = -1  !< ID associated with diagnostics for growth rate of heterotrophic bacteria
    integer ::  id_mu_cstar         = -1  !< ID associated with diagnostics for biomass turnover due to chemosynthesis
    integer ::  id_bhet             = -1  !< ID associated with diagnostics for heterotrophic bacteria biomass
    integer ::  id_temp_lim         = -1  !< ID associated with diagnostics for temperature limitation
    integer ::  id_o2lim            = -1  !< ID associated with diagnostics for limitation due to oxygen
    integer ::  id_ldonlim          = -1  !< ID associated with diagnostics for limitation due to organic substrate
    integer ::  id_jprod_n_100      = -1  !< ID associated with diagnostics for bacteria nitrogen prod. integral in upper 100m
    integer ::  id_jzloss_n_100     = -1  !< ID associated with diagnostics for bacteria nitrogen loss to zooplankton integral in upper 100m
    integer ::  id_jvirloss_n_100   = -1  !< ID associated with diagnostics for bacteria nitrogen loss to viruses integral in upper 100m
    integer ::  id_jremin_n_100     = -1  !< ID associated with diagnostics for bacteria nitrogen remineralization integral in upper 100m
    integer ::  id_juptake_ldon_100 = -1  !< ID associated with diagnostics for bacterial uptake of labile dissolved org. nitrogen in upper 100m   
    integer ::  id_f_n_100          = -1  !< ID associated with diagnostics for bacterial nitrogen biomass in upper 100m
  end type bacteria

  !> data type for other variables used in generic_cobalt module 
  type generic_COBALT_type

     logical  ::       &
          init,             &                  ! If tracers should be initializated
          do_case2_mod,     &                  ! Flag to include augmented opacity in shallow/river-influenced systems
          force_update_fluxes,&                ! If OCMIP2 tracers fluxes should be updated every coupling timesteps
                                               !    when update_from_source is not called every coupling timesteps
                                               !    as is the case with MOM6  THERMO_SPANS_COUPLING option
          p_2_n_static,     &                  ! If P:N is fixed in phytoplankton
          cased_steady,     &                  ! steady state approximation for cased
          tracer_debug

     real  ::          &
          atm_co2_flux,     &
          c_2_n,            &
          ca_2_n_arag,      &
          ca_2_n_calc,      &
          caco3_sat_max,    &
          doc_background,   &
          fe_2_n_upt_fac,   &
          fe_2_n_sed,       &
          ffe_sed_max,      &
          ffe_geotherm_ratio,&
          jfe_iceberg_ratio,&
          jno3_iceberg_ratio,&
          jpo4_iceberg_ratio,&
          fe_coast,         &
          felig_2_don,      &
          felig_bkg ,       &
          gamma_cadet_arag, &
          gamma_cadet_calc, &
          par_adj,          &
          gamma_irr_aclm,   &
          ml_aclm_efold,    &
          zmld_ref,         &
          densdiff_mld,     &
          irrad_day_thresh, &
          case2_depth,      & ! depth threshold for case 2 (coastal) waters
          case2_salt,       & ! salt threshold for case 2 (coastal) waters
          case2_opac_add,   & ! added opacity for case 2 (coastal) waters
          min_daylength,    &
          gamma_irr_mem_dp, &
          gamma_mu_mem,     &
          irr_mem_dpthresh1, &
          irr_mem_dpthresh2, &
          dpause_max,       &
          gamma_ndet,       &
          gamma_nitrif,     &
          k_nh3_nitrif,     &
          gamma_sidet,      &
          gamma_srdon,      &
          gamma_srdop,      &
          gamma_sldon,      &
          gamma_sldop,      &
          gamma_nh4amx,     &
          kappa_sidet,      &
          irr_inhibit,      &
          k_n_inhib_di,     &
          k_o2,             &
          k_o2_nit,         &
          kappa_eppley,     &
          kappa_remin,      &
          remin_ramp_scale, &
          kfe_eq_lig_hl,    &
          kfe_eq_lig_ll,    &
          alpha_fescav,     &
          beta_fescav,      &
          io_fescav,        &
          remin_eff_fedet,  &
          half_life_14c,    &
          lambda_14c,       &
          k_lith,           &
          phi_lith,         &
          alk_2_n_denit,    &
          n_2_n_denit,      &
          k_no3_denit,      &
          no3_2_nh4_amx,     &
          alk_2_nh4_amx,    &
          z_burial,         &
          phi_surfresp_cased, &
          phi_deepresp_cased, &
          alpha_cased,      &
          beta_cased,       &
          gamma_cased,      &
          Co_cased,         &
          o2_min,           &
          o2_min_amx,       &
          o2_min_nit,       &
          o2_2_nfix,        &
          o2_2_nh4,         &
          o2_2_no3,         &
          o2_2_nitrif,      &
          o2_inhib_di_pow,  &
          o2_inhib_di_sat,  &
          rpcaco3,          &
          rplith,           &
          rpsio2,           &
          thetamin,         &
          vir_ktemp,        &
          lysis_phi_ldon,   &
          lysis_phi_srdon,  &
          lysis_phi_sldon,  &
          lysis_phi_ldop,   &
          lysis_phi_srdop,  &
          lysis_phi_sldop,  &
          wsink,            &
          bottom_thickness, &
          z_sed,            &
          zeta,             &
          refuge_conc,      &
          imax_hp,          & ! unresolved higher pred. max ingestion rate
          ki_hp,            & ! unresolved higher pred. half-sat
          ktemp_hp,         & ! temperature dependence for higher predators
          coef_hp,          & ! scaling between unresolved preds and available prey
          nswitch_hp,	    & ! higher predator switching behavior
          mswitch_hp,       & ! higher predator switching behavior
          hp_ipa_smp,       & ! innate prey availability of small phytos to hp
          hp_ipa_mdp,       & ! innate prey availability of medium phytos to hp
          hp_ipa_lgp,       & ! "  "  "  "  "  "  "  "  "   large phytos to hp
          hp_ipa_diaz,      & ! "  "  "  "  "  "  "  "  "   diazotrophs to hp
          hp_ipa_bact,      & ! "  "  "  "  "  "  "  "  "   bacteria to hp
          hp_ipa_smz,       & ! "  "  "  "  "  "  "  "  "   small zooplankton to hp
          hp_ipa_mdz,       & ! "  "  "  "  "  "  "  "  "   medium zooplankton to hp
          hp_ipa_lgz,       & ! "  "  "  "  "  "  "  "  "   large zooplankton to hp
          hp_ipa_det,       & ! "  "  "  "  "  "  "  "  "   detritus to hp
          hp_phi_det          ! fraction of ingested N to detritus

     real, dimension(3)                    :: total_atm_co2

     real    :: htotal_scale_lo, htotal_scale_hi, htotal_in
     real    :: Rho_0, a_0, a_1, a_2, a_3, a_4, a_5, b_0, b_1, b_2, b_3, c_0
     real    :: a1_co2, a2_co2, a3_co2, a4_co2, a5_co2
     real    :: a1_o2, a2_o2, a3_o2, a4_o2, a5_o2

     logical, dimension(:,:), ALLOCATABLE ::  &
          mask_z_sat_arag,&
          mask_z_sat_calc

     real, dimension(:,:,:), ALLOCATABLE ::  &
          f_alk,&				! Other prognostic variables
          f_cadet_arag,&
          f_cadet_calc,&
          f_dic,&
          f_fed,&
          f_fedet,&
          f_ldon,&
          f_ldop,&
          f_lith,&
          f_lithdet,&
          f_ndet,&
          f_nh4,&
          f_no3,&
          f_o2,&
          f_pdet,&
          f_po4,&
          f_srdon,&
          f_srdop,&
          f_sldon,&
          f_sldop,&
          f_sidet,&
          f_simd,&
          f_silg,&
          f_sio4,&
          co3_sol_arag,&
          co3_sol_calc,&
          rho_test,&
          f_chl,&
          f_nh3,&
          f_co3_ion,&
          f_htotal,&
          f_irr_aclm,&
          f_irr_aclm_z,&
          f_irr_aclm_sfc, &
          f_irr_mem_dp,&
          f_cased,&
          f_cadet_arag_btf,&
          f_cadet_calc_btf,&
          f_fedet_btf, &
          f_lithdet_btf, &
          f_ndet_btf,&
          f_pdet_btf,&
          f_sidet_btf,&
          f_nsm_btf,&
          f_nmd_btf,&
          f_nlg_btf,&
          f_ndi_btf,&
          f_simd_btf,&
          f_silg_btf,&
          f_fesm_btf,&
          f_femd_btf,&
          f_felg_btf,&
          f_fedi_btf,&
          f_psm_btf,&
          f_pmd_btf,&
          f_plg_btf,&
          f_pdi_btf,&
          jnbact,&
          jndi,&
          jnsm,&
          jnmd,&
          jnlg,&
          jnsmz,&
          jnmdz,&
          jnlgz,&
          jalk,&
          jalk_plus_btm,&
          jcadet_arag,&
          jcadet_calc,&
          jdic,&
          jdic_plus_btm,&
          jdin_plus_btm,&
          jfed,&
          jfed_plus_btm,&
          jfedi,&
          jfesm,&
          jfemd,&
          jfelg,&
          jpdi,&
          jpsm,&
          jpmd,&
          jplg,&
          jfedet,&
          jldon,&
          jldop,&
          jlith,&
          jlithdet,&
          jndet,&
          jnh4,&
          jnh4_plus_btm,&
          jno3,&
          jno3_plus_btm,&
          jo2,&
          jo2_plus_btm,&
          jpdet,&
          jpo4,&
          jpo4_plus_btm,&
          jsrdon,&
          jsrdop,&
          jsldon,&
          jsldop,&
          jsidet,&
          jsimd,&
          jsilg,&
          jsio4,&
          jsio4_plus_btm,&
          jprod_ndet,&
          jprod_pdet,&
          jprod_ldon,&
          jprod_ldop,&
          jprod_sldon,&
          jprod_sldop,&
          jprod_srdon,&
          jprod_srdop,&
          jprod_fed,&
          jprod_fedet,&
          jprod_sidet,&
          jprod_sio4, &
          jprod_lithdet,&
          jprod_cadet_arag,&
          jprod_cadet_calc,&
          jprod_nh4,&
          jprod_nh4_plus_btm,&
          jprod_po4,&
          det_jzloss_n,&
          det_jzloss_p,&
          det_jzloss_fe,&
          det_jzloss_si,&
          det_jhploss_n,&
          det_jhploss_p,&
          det_jhploss_fe,&
          det_jhploss_si,&
          jdiss_cadet_arag,&
          jdiss_cadet_arag_plus_btm,&
          jdiss_cadet_calc,&
          jdiss_cadet_calc_plus_btm,&
          jdiss_sidet,&
          jremin_ndet,&
          jremin_pdet,&
          jremin_fedet,&
          jfe_ads,&
          jfe_coast,&
          jfe_iceberg,&
          jno3_iceberg,&
          jpo4_iceberg,&
          kfe_eq_lig,&
          feprime,&
          ligand,&
          fe_sol,&
          expkT,&
          expkreminT,&
          hp_temp_lim,&
          hp_o2lim,&
          hp_jingest_n,&
          hp_jingest_p,&
          hp_jingest_fe,&
          hp_jingest_sio2,&
          irr_inst,&
          irr_mix,&
          irr_aclm_inst, &
          chl2sfcchl, &
          jno3denit_wc,&
          juptake_no3amx,&
          juptake_nh4amx,&
          jprod_n2amx,&
          juptake_nh4nitrif,&
          jprod_no3nitrif,&
          jo2resp_wc,&
          omega_arag,&
          omega_calc,&
          omegaa,&
          omegac,&
          fntot, &
          fptot, &
          ffetot, &
          fsitot, &
          tot_layer_int_c,&
          tot_layer_int_fe,&
          tot_layer_int_n,&
          tot_layer_int_p,&
          tot_layer_int_si,&
          tot_layer_int_o2,&
          tot_layer_int_alk,&
          total_filter_feeding,&
          nmd_diatoms,&
          nlg_diatoms,&
          q_si_2_n_md_diatoms,&
          q_si_2_n_lg_diatoms,&
          zt, &
          c14_2_n,&
          f_di14c,&
          f_do14c,&
          fpo14c,&
          j14c_decay_dic,&
          j14c_decay_doc,&
          j14c_reminp,&
          jdi14c,&
          jdo14c, &
          dissoc, &
          o2sat, &
          remoc, &
          tot_layer_int_doc,&
          tot_layer_int_poc,&
          tot_layer_int_dic

!==============================================================================================================

     real, dimension(:,:), ALLOCATABLE :: &
          b_alk,b_dic,b_fed,b_nh4,b_no3,b_o2,b_po4,b_sio4,b_di14c,&	! bottom flux terms
          co2_csurf,pco2_csurf,co2_alpha,c14o2_csurf,c14o2_alpha,&
          nh3_csurf,nh3_alpha,pnh3_csurf,&
          fcadet_arag_btm,&
          fcadet_calc_btm,&
          ffedet_btm,&
          flithdet_btm,&
          fpdet_btm,&
          fndet_btm,&
          fsidet_btm,&
          fntot_btm,&
          fptot_btm,&
          ffetot_btm,&
          fsitot_btm,&
          fcased_burial,&
          fcased_redis,&
          fcased_redis_surfresp,&
          cased_redis_coef,&
          cased_redis_delz,&
          ffe_sed,&
          ffe_geotherm,&
          ffe_iceberg,&
          fnfeso4red_sed,&
          fno3denit_sed,&
          fnoxic_sed,&
          frac_burial,&
          fn_burial,&
          fp_burial,&
          jprod_allphytos_100,&
          jprod_allphytos_200,&
          jprod_diat_100,&
          mld_aclm,&
          htotallo, htotalhi,&
          hp_jingest_n_100,&
          hp_jremin_n_100,&
          hp_jprod_ndet_100,&
          jprod_lithdet_100,&
          jprod_sidet_100,&
          jprod_cadet_calc_100,&
          jprod_cadet_arag_100,&
          jprod_mesozoo_200, &
          jremin_ndet_100, &
          f_ndet_100, &
          f_don_100, &
          f_simd_100, &
          f_silg_100, &
          f_mesozoo_200, &
          fndet_100, &
          fpdet_100, &
          fsidet_100, &
          fcadet_calc_100, &
          fcadet_arag_100, &
          ffedet_100, &
          flithdet_100, &
          fntot_100, &
          fptot_100, &
          fsitot_100, &
          ffetot_100, &
          btm_temp,     &
          btm_temp_old, &
          btm_o2_old,   &
          btm_o2,       &
          btm_no3,      &
          btm_alk,       &
          btm_dic,       &
          grid_kmt_diag, &
          k_bot_diag,   &
          rho_dzt_kmt_diag, &
          rho_dzt_bot_diag, &
          btm_htotal,   &
          btm_htotal_old, &
          btm_co3_ion,  &
          btm_co3_ion_old, &
          btm_co3_sol_arag, &
          btm_co3_sol_arag_old, &
          btm_co3_sol_calc, &
          btm_co3_sol_calc_old, &
          btm_omega_calc, &
          btm_omega_arag, &
          cased_2d,     &
          o2min, &
          z_o2min, &
          z_sat_arag,&
          z_sat_calc,&
          dp_fac,&
          daylength,&
!==============================================================================================================
! JGJ 2016/08/08 CMIP6 Ocnbgc
          f_alk_int_100, &
          f_dic_int_100, &
          f_din_int_100, &
          f_fed_int_100, &
          f_po4_int_100, &
          f_sio4_int_100, &
          jalk_100, &
          jdic_100, &
          jdin_100, &
          jfed_100, &
          jpo4_100, &
          jsio4_100, &
          jprod_ptot_100, &
          wc_vert_int_c,&
          wc_vert_int_dic,&
          wc_vert_int_doc,&
          wc_vert_int_poc,&
          wc_vert_int_n,&
          wc_vert_int_p,&
          wc_vert_int_fe,&
          wc_vert_int_si,&
          wc_vert_int_o2,&
          wc_vert_int_alk,&
          wc_vert_int_chemoautopp,&
          wc_vert_int_npp, &
          wc_vert_int_jdiss_sidet,&
          wc_vert_int_jdiss_cadet,&
          wc_vert_int_jo2resp,&
          wc_vert_int_jprod_cadet,&
          wc_vert_int_jno3denit,&
          wc_vert_int_jprod_no3nitrif,&
          wc_vert_int_juptake_nh4,&
          wc_vert_int_jprod_nh4,&
          wc_vert_int_juptake_no3,&
          wc_vert_int_nfix,&
          wc_vert_int_jprod_n2amx,&
          wc_vert_int_jfe_iceberg,&
          wc_vert_int_jno3_iceberg,&
          wc_vert_int_jpo4_iceberg
!==============================================================================================================

     real, dimension(:,:,:,:), pointer :: &
          p_alk,&
          p_cadet_arag,&
          p_cadet_calc,&
          p_dic,&
          p_di14c,&
          p_do14c,&
          p_fed,&
          p_fedet,&
          p_fedi,&
          p_felg,&
          p_femd,&
          p_fesm,&
          p_pdi,&
          p_plg,&
          p_pmd,&
          p_psm,&
          p_ldon,&
          p_ldop,&
          p_lith,&
          p_lithdet,&
          p_nbact,&
          p_ndet,&
          p_ndi,&
          p_nlg,&
          p_nmd,&
          p_nsm,&
          p_nh4,&
          p_no3,&
          p_o2,&
          p_pdet,&
          p_po4,&
          p_srdon,&
          p_srdop,&
          p_sldon,&
          p_sldop,&
          p_sidet,&
          p_silg,&
          p_simd,&
          p_sio4,&
          p_nsmz,&
          p_nmdz,&
          p_nlgz

      real, dimension (:,:), allocatable :: &
          runoff_flux_alk,&
          runoff_flux_dic,&
          runoff_flux_di14c,&
          runoff_flux_lith,&
          runoff_flux_fed,&
          runoff_flux_no3,&
          runoff_flux_ldon,&
          runoff_flux_sldon,&
          runoff_flux_srdon,&
          runoff_flux_ndet,&
          runoff_flux_pdet,&
          runoff_flux_po4,&
          runoff_flux_ldop,&
          runoff_flux_sldop,&
          runoff_flux_srdop,&
          dry_fed, wet_fed,&
          dry_lith, wet_lith,&
          dry_no3, wet_no3,&
          dry_nh4, wet_nh4,&
          dry_po4, wet_po4, &
          stf_gas_dic,&
          stf_gas_o2,&
          deltap_dic,&
          deltap_o2

     integer :: numlightadapt
     character(len=fm_string_len)          :: file
     character(len=fm_string_len) :: ice_restart_file
     character(len=fm_string_len) :: ocean_restart_file,IC_file

     integer               ::          &
          id_co3_sol_arag  = -1,       &
          id_co3_sol_calc  = -1,       &
          id_rho_test      = -1,       &
          id_dep_dry_fed   = -1,       &
          id_dep_dry_nh4   = -1,       &
          id_dep_dry_no3   = -1,       &
          id_dep_dry_po4   = -1,       &
          id_dep_wet_fed   = -1,       &
          id_dep_wet_nh4   = -1,       &
          id_dep_wet_no3   = -1,       &
          id_dep_wet_po4   = -1,       &
          id_dep_wet_lith  = -1,       &
          id_dep_dry_lith  = -1,       &
          id_omega_arag    = -1,       &
          id_omega_calc    = -1,       &
          id_irr_aclm      = -1,       &
          id_irr_aclm_z    = -1,       &
          id_jfed          = -1,       &
          id_jfedc          = -1,      & !liao
          id_jprod_ndet    = -1,       &
          id_jprod_pdet    = -1,       &
          id_jprod_sldon   = -1,       &
          id_jprod_ldon    = -1,       &
          id_jprod_srdon   = -1,       &
          id_jprod_sldop   = -1,       &
          id_jprod_ldop    = -1,       &
          id_jprod_srdop   = -1,       &
          id_jprod_fed     = -1,       &
          id_jprod_fedet   = -1,       &
          id_jprod_sidet   = -1,       &
          id_jprod_sio4    = -1,       &
          id_jprod_lithdet = -1,       &
          id_jprod_cadet_arag = -1,    &
          id_jprod_cadet_calc = -1,    &
          id_jprod_po4     = -1,       &
          id_jprod_nh4     = -1,       &
          id_jprod_nh4_plus_btm = -1,  &
          id_det_jzloss_n  = -1,       &
          id_det_jhploss_n = -1,       &
          id_jdiss_sidet   = -1,       &
          id_jdiss_cadet_arag = -1,    &
          id_jdiss_cadet_arag_plus_btm = -1, &
          id_jdiss_cadet_calc = -1,    &
          id_jdiss_cadet_calc_plus_btm = -1, &
          id_jremin_ndet   = -1,       &
          id_jremin_pdet   = -1,       &
          id_jremin_fedet  = -1,       &
          id_jfe_ads       = -1,       &
          id_jfe_coast     = -1,       &
          id_jfe_iceberg   = -1,       &
          id_jno3_iceberg  = -1,       &
          id_jpo4_iceberg  = -1,       &
          id_kfe_eq_lig    = -1,       &
          id_feprime       = -1,       &
          id_ligand        = -1,       &
          id_fe_sol        = -1,       &
          id_expkT         = -1,       &
          id_expkreminT    = -1,       &
          id_hp_temp_lim   = -1,       &
          id_hp_o2lim      = -1,       &
          id_irr_inst      = -1,       &
          id_irr_mix       = -1,       &
          id_irr_aclm_inst = -1,       &
          id_chl2sfcchl    = -1,       &
          id_jalk          = -1,       &
          id_jalkc          = -1,       &  !liao
          id_jalk_plus_btm = -1,       &
          id_jdic          = -1,       &
          id_jdicc          = -1,       &  !liao
          id_jno3c          = -1,       &  !liao
          id_jpo4c          = -1,       &  !liao
          id_jsio4c         = -1,       &  !liao
          id_jdic_plus_btm = -1,       &
          id_jnh4          = -1,       &
          id_jndet         = -1,       &
          id_jnh4_plus_btm = -1,       &
          id_jno3denit_wc  = -1,       &
          id_juptake_no3amx = -1,      &
          id_juptake_nh4amx = -1,      &
          id_jprod_n2amx = -1,         &
          id_juptake_nh4nitrif = -1,   &
          id_jprod_no3nitrif = -1,     &
          id_jo2resp_wc    = -1,       &
          id_co2_csurf     = -1,       &
          id_pco2_csurf    = -1,       &
          id_co2_alpha     = -1,       &
          id_nh3_csurf     = -1,       &
          id_nh3_alpha     = -1,       &
          id_fcadet_arag   = -1,       &
          id_fcadet_calc   = -1,       &
          id_ffedet        = -1,       &
          id_fndet         = -1,       &
          id_fpdet         = -1,       &
          id_fsidet        = -1,       &
          id_fntot         = -1,       &
          id_fptot         = -1,       &
          id_fsitot        = -1,       &
          id_ffetot        = -1,       &
          id_flithdet      = -1,       &
          id_fcadet_arag_btm = -1,     &
          id_fcadet_calc_btm = -1,     &
          id_ffedet_btm    = -1,       &
          id_flithdet_btm  = -1,       &
          id_fndet_btm     = -1,       &
          id_fpdet_btm     = -1,       &
          id_fsidet_btm    = -1,       &
          id_fntot_btm     = -1,       &
          id_fptot_btm     = -1,       &
          id_ffetot_btm    = -1,       &
          id_fsitot_btm    = -1,       &
          id_fcased_burial = -1,       &
          id_fcased_redis  = -1,       &
          id_fcased_redis_surfresp  = -1, &
          id_cased_redis_coef  = -1,   &
          id_cased_redis_delz  = -1,   &
          id_ffe_sed       = -1,       &
          id_ffe_geotherm  = -1,       &
          id_ffe_iceberg = -1,         &
          id_fnfeso4red_sed= -1,       &
          id_fno3denit_sed = -1,       &
          id_fnoxic_sed    = -1,       &
          id_frac_burial   = -1,       &
          id_fn_burial  = -1,       &
          id_fp_burial  = -1,       &
          id_nphyto_tot    = -1,       &
          id_no3_in_source = -1,       &
          id_pco2surf      = -1,       &
          id_pnh3surf      = -1,       &
          id_sfc_alk       = -1,       &
          id_sfc_cadet_arag= -1,       &
          id_sfc_cadet_calc= -1,       &
          id_sfc_dic       = -1,       &
          id_sfc_fed       = -1,       &
          id_sfc_ldon      = -1,       &
          id_sfc_sldon     = -1,       &
          id_sfc_srdon     = -1,       &
          id_sfc_no3       = -1,       &
          id_sfc_nh4       = -1,       &
          id_sfc_po4       = -1,       &
          id_sfc_sio4      = -1,       &
          id_sfc_htotal    = -1,       &
          id_sfc_o2        = -1,       &
          id_sfc_chl       = -1,       &
          id_sfc_irr       = -1,       &
          id_sfc_irr_aclm   = -1,       &
          id_sfc_irr_mem_dp = -1,      &
          id_sfc_temp      = -1,       &
          id_btm_temp      = -1,       &
          id_btm_temp_old  = -1,       &
          id_btm_o2_old    = -1,       &
          id_btm_o2        = -1,       &
          id_btm_no3       = -1,       &
          id_btm_alk       = -1,       &
          id_btm_dic       = -1,       &
          id_grid_kmt_diag = -1,       &
          id_k_bot_diag    = -1,       &
          id_rho_dzt_kmt_diag = -1,    &
          id_rho_dzt_bot_diag = -1,    &
          id_btm_htotal    = -1,       &
          id_btm_htotal_old    = -1,   &
          id_btm_co3_sol_arag = -1,    &
          id_btm_co3_sol_arag_old = -1,&
          id_btm_co3_sol_calc = -1,    &
          id_btm_co3_sol_calc_old = -1,&
          id_btm_co3_ion      = -1,    &
          id_btm_co3_ion_old  = -1,    &
          id_btm_omega_calc   = -1,    &
          id_btm_omega_arag   = -1,    &
          id_cased_2d      = -1,       &
          id_sfc_co3_ion   = -1,       &
          id_sfc_co3_sol_arag = -1,    &
          id_sfc_co3_sol_calc = -1,    &
          id_runoff_flux_alk = -1,     &
          id_runoff_flux_dic = -1,     &
          id_runoff_flux_di14c = -1,     &
          id_runoff_flux_fed = -1,     &
          id_runoff_flux_lith = -1,    &
          id_runoff_flux_no3 = -1,     &
          id_runoff_flux_ldon = -1,    &
          id_runoff_flux_sldon = -1,   &
          id_runoff_flux_srdon = -1,   &
          id_runoff_flux_ndet = -1,    &
          id_runoff_flux_pdet = -1,    &
          id_runoff_flux_po4 = -1,     &
          id_runoff_flux_ldop = -1,    &
          id_runoff_flux_sldop = -1,   &
          id_runoff_flux_srdop = -1,   &
          id_tot_layer_int_c = -1,     &
          id_tot_layer_int_fe = -1,    &
          id_tot_layer_int_n = -1,     &
          id_tot_layer_int_p = -1,     &
          id_tot_layer_int_si = -1,    &
          id_tot_layer_int_o2 = -1,    &
          id_tot_layer_int_alk = -1,   &
          id_wc_vert_int_c = -1,       &
          id_wc_vert_int_dic = -1,     &
          id_wc_vert_int_doc = -1,     &
          id_wc_vert_int_poc = -1,     &
          id_wc_vert_int_n = -1,       &
          id_wc_vert_int_p = -1,       &
          id_wc_vert_int_fe = -1,      &
          id_wc_vert_int_si = -1,      &
          id_wc_vert_int_o2 = -1,      &
          id_wc_vert_int_alk = -1,     &
          id_wc_vert_int_chemoautopp = -1, &
          id_wc_vert_int_npp = -1, &
          id_wc_vert_int_jdiss_sidet = -1, &
          id_wc_vert_int_jdiss_cadet = -1, &
          id_wc_vert_int_jo2resp = -1,     &
          id_wc_vert_int_jprod_cadet = -1, &
          id_wc_vert_int_jno3denit = -1,   &
          id_wc_vert_int_jprod_no3nitrif = -1, &
          id_wc_vert_int_juptake_nh4 = -1, &
          id_wc_vert_int_jprod_nh4 = -1, &
          id_wc_vert_int_juptake_no3 = -1, &
          id_wc_vert_int_nfix = -1,        &
          id_wc_vert_int_jfe_iceberg = -1, &
          id_wc_vert_int_jno3_iceberg = -1, &
          id_wc_vert_int_jpo4_iceberg = -1, &
          id_wc_vert_int_jprod_n2amx = -1, &
          id_total_filter_feeding = -1,&
          id_nlg_diatoms = -1,         &
          id_nmd_diatoms = -1,         &
          id_jprod_allphytos_100 = -1, &
          id_jprod_allphytos_200 = -1, &
          id_jprod_diat_100 = -1,      &
          id_mld_aclm          = -1,      &
          id_q_si_2_n_lg_diatoms = -1, &
          id_q_si_2_n_md_diatoms = -1, &
          id_hp_jingest_n_100 = -1,    &
          id_hp_jremin_n_100 = -1,     &
          id_hp_jprod_ndet_100 = -1,   &
          id_jprod_lithdet_100 = -1,   &
          id_jprod_sidet_100 = -1,     &
          id_jprod_cadet_calc_100 = -1, &
          id_jprod_cadet_arag_100 = -1, &
          id_jprod_mesozoo_200 = -1,   &
          id_dp_fac            = -1,   &
          id_daylength         = -1,   &
          id_jremin_ndet_100 = -1,     &
          id_f_ndet_100 = -1,          &
          id_f_don_100 = -1,           &
          id_f_silg_100 = -1,          &
          id_f_simd_100 = -1,          &
          id_f_mesozoo_200 = -1,       &
          id_fndet_100 = -1,           &
          id_fpdet_100 = -1,           &
          id_ffedet_100 = -1,          &
          id_fcadet_calc_100 = -1,     &
          id_fcadet_arag_100 = -1,     &
          id_flithdet_100 = -1,        &
          id_fsidet_100 = -1,          &
          id_fntot_100 = -1,           &
          id_fptot_100 = -1,           &
          id_ffetot_100 = -1,          &
          id_fsitot_100 = -1,          &
          id_o2min         = -1,       &
          id_z_o2min       = -1,       &
          id_z_sat_arag    = -1,       & ! Depth of Aragonite saturation
          id_z_sat_calc    = -1,       & ! Depth of Calcite saturation
          id_b_di14c       = -1,       & ! Bottom flux of DI14C
          id_c14_2_n       = -1,       & ! DI14C to PO4 uptake ratio
          id_c14o2_csurf   = -1,       & ! Surface water 14CO2*
          id_c14o2_alpha   = -1,       & ! Surface water 14CO2* solubility
          id_fpo14c        = -1,       & ! PO14C sinking flux
          id_j14c_decay_dic= -1,       & ! Radioactive decay of DI14C
          id_j14c_decay_doc= -1,       & ! Radioactive decay of DO14C
          id_j14c_reminp   = -1,       & ! 14C particle remineralization layer integral
          id_jdi14c        = -1,       & ! DI14C source layer integral
          id_jdo14c        = -1,       & ! Semilabile DO14C source layer integral
          id_f_alk_int_100  = -1, &
          id_f_dic_int_100  = -1, &
          id_f_din_int_100  = -1, &
          id_f_fed_int_100  = -1, &
          id_f_po4_int_100  = -1, &
          id_f_sio4_int_100 = -1, &
          id_jo2_plus_btm   = -1, &
          id_jo2            = -1, & !liao
          id_jo2c           = -1, & !liao
          id_jalk_100       = -1, &
          id_jdic_100       = -1, &
          id_jdin_100       = -1, &
          id_jfed_100       = -1, &
          id_jpo4_100       = -1, &
          id_jsio4_100      = -1, &
!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem
          id_thetao         = -1, &        ! for testing
          id_dissic         = -1, &
          id_dissicnat      = -1, &
          id_dissicabio     = -1, &
          id_dissi14cabio   = -1, &
          id_dissoc         = -1, &
          id_phyc           = -1, &
          id_zooc           = -1, &
          id_bacc           = -1, &
          id_detoc          = -1, &
          id_calc           = -1, &
          id_arag           = -1, &
          id_phydiat        = -1, &
          id_phydiaz        = -1, &
          id_phypico        = -1, &
          id_phymisc        = -1, &
          id_zmicro         = -1, &
          id_zmeso          = -1, &
          id_talk           = -1, &
          id_talknat        = -1, &
          id_ph             = -1, &
          id_phnat          = -1, &
          id_phabio         = -1, &
          id_o2_cmip        = -1, &
          id_o2sat          = -1, &
          id_no3_cmip       = -1, &
          id_pka_nh3        = -1, &
          id_nh4_cmip       = -1, &
          id_po4_cmip       = -1, &
          id_dfe            = -1, &
          id_si             = -1, &
          id_chl_cmip       = -1, &
          id_chldiat        = -1, &
          id_chldiaz        = -1, &
          id_chlpico        = -1, &
          id_chlmisc        = -1, &
          id_poc            = -1, &
          id_pon            = -1, &
          id_pop            = -1, &
          id_bfe            = -1, &
          id_bsi            = -1, &
          id_phyn           = -1, &
          id_phyp           = -1, &
          id_phyfe          = -1, &
          id_physi          = -1, &
          id_co3            = -1, &
          id_co3nat         = -1, &
          id_co3abio        = -1, &
          id_co3satcalc     = -1, &
          id_co3satarag     = -1, &
          id_pp             = -1, &
          id_pnitrate       = -1, &
          id_pphosphate     = -1, &
          id_pbfe           = -1, &
          id_pbsi           = -1, &
          id_parag          = -1, &
          id_pcalc          = -1, &
          id_expc           = -1, &
          id_expn           = -1, &
          id_expp           = -1, &
          id_expfe          = -1, &
          id_expsi          = -1, &
          id_expcalc        = -1, &
          id_exparag        = -1, &
          id_remoc          = -1, &
          id_dcalc          = -1, &
          id_darag          = -1, &
          id_ppdiat         = -1, &
          id_ppdiaz         = -1, &
          id_pppico         = -1, &
          id_ppmisc        = -1, &
          id_bddtdic        = -1, &
          id_bddtdin        = -1, &
          id_bddtdip        = -1, &
          id_bddtdife       = -1, &
          id_bddtdisi       = -1, &
          id_bddtalk        = -1, &
          id_fescav         = -1, &
          id_fediss         = -1, &
          id_graz           = -1, &
          id_dissicos           = -1, &
          id_dissicnatos        = -1, &
          id_dissicabioos       = -1, &
          id_dissi14cabioos     = -1, &
          id_dissocos           = -1, &
          id_phycos             = -1, &
          id_zoocos             = -1, &
          id_baccos             = -1, &
          id_detocos            = -1, &
          id_calcos             = -1, &
          id_aragos             = -1, &
          id_phydiatos          = -1, &
          id_phydiazos          = -1, &
          id_phypicoos          = -1, &
          id_phymiscos          = -1, &
          id_zmicroos           = -1, &
          id_zmesoos            = -1, &
          id_talkos             = -1, &
          id_talknatos          = -1, &
          id_phos               = -1, &
          id_phnatos            = -1, &
          id_phabioos           = -1, &
          id_o2os               = -1, &
          id_o2satos            = -1, &
          id_no3os              = -1, &
          id_nh4os              = -1, &
          id_po4os              = -1, &
          id_dfeos              = -1, &
          id_sios               = -1, &
          id_chlos              = -1, &
          id_chldiatos          = -1, &
          id_chldiazos          = -1, &
          id_chlpicoos          = -1, &
          id_chlmiscos          = -1, &
          id_ponos              = -1, &
          id_popos              = -1, &
          id_bfeos              = -1, &
          id_bsios              = -1, &
          id_phynos             = -1, &
          id_phypos             = -1, &
          id_phyfeos            = -1, &
          id_physios            = -1, &
          id_co3os              = -1, &
          id_co3natos           = -1, &
          id_co3abioos          = -1, &
          id_co3satcalcos       = -1, &
          id_co3sataragos       = -1, &
          id_limndiat           = -1, &
          id_limndiaz           = -1, &
          id_limnpico           = -1, &
          id_limnmisc           = -1, &
          id_limirrdiat         = -1, &
          id_limirrdiaz         = -1, &
          id_limirrpico         = -1, &
          id_limirrmisc         = -1, &
          id_limfediat          = -1, &
          id_limfediaz          = -1, &
          id_limfepico          = -1, &
          id_limfemisc          = -1, &
          id_limpdiat           = -1, &
          id_limpdiaz           = -1, &
          id_limppico           = -1, &
          id_limpmisc           = -1, &
          id_intpp              = -1, &
          id_intppnitrate       = -1, &
          id_intppdiat          = -1, &
          id_intppdiaz          = -1, &
          id_intpppico          = -1, &
          id_intppmisc          = -1, &
          id_intpbn             = -1, &
          id_intpbp             = -1, &
          id_intpbfe            = -1, &
          id_intpbsi            = -1, &
          id_intpcalcite        = -1, &
          id_intparag           = -1, &
          id_epc100             = -1, &
          id_epn100             = -1, &
          id_epp100             = -1, &
          id_epfe100            = -1, &
          id_epsi100            = -1, &
          id_epcalc100          = -1, &
          id_eparag100          = -1, &
          id_intdic             = -1, &
          id_intdoc             = -1, &
          id_intpoc             = -1, &
          id_spco2              = -1, &
          id_spco2nat           = -1, &
          id_spco2abio          = -1, &
          id_dpco2              = -1, &
          id_dpco2nat           = -1, &
          id_dpco2abio          = -1, &
          id_dpo2               = -1, &
          id_fgco2              = -1, &
          id_fgco2nat           = -1, &
          id_fgco2abio          = -1, &
          id_fg14co2abio        = -1, &
          id_fgo2               = -1, &
          id_icfriver           = -1, &
          id_fric               = -1, &
          id_ocfriver           = -1, &
          id_froc               = -1, &
          id_intpn2             = -1, &
          id_fsn                = -1, &
          id_frn                = -1, &
          id_fsfe               = -1, &
          id_frfe               = -1, &
          id_zo2min             = -1, &
          id_zsatcalc           = -1, &
          id_zsatarag           = -1, &
          id_fddtdic            = -1, &
          id_fddtdin            = -1, &
          id_fddtdip            = -1, &
          id_fddtdife           = -1, &
          id_fddtdisi           = -1, &
          id_fddtalk            = -1, &
          id_fbddtdic           = -1, &
          id_fbddtdin           = -1, &
          id_fbddtdip           = -1, &
          id_fbddtdife          = -1, &
          id_fbddtdisi          = -1, &
          id_fbddtalk           = -1

!==============================================================================================================
  end type generic_COBALT_type

end module cobalt_types
