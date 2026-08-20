!> The COBALT_send_diag module contains a subroutine
!! to handle sending diagnostics.
!<----------------------------------------------------------------
module COBALT_reg_diag

  use cobalt_types
  use time_manager_mod,  only: time_type
  use g_tracer_utils, only : g_diag_type
  use g_tracer_utils, only : register_diag_field=>g_register_diag_field

  implicit none; private
  public cobalt_reg_diagnostics

  contains

  !> subroutine that handles registration for diagnostic variables
  subroutine cobalt_reg_diagnostics(diag_list,axes,init_time,phyto,zoo,bact,cobalt)
    !
    type(g_diag_type), pointer :: diag_list
    integer,                                   intent(in) :: axes(3)
    type(time_type),                           intent(in) :: init_time
    type(phytoplankton), dimension(NUM_PHYTO), intent(inout) :: phyto
    type(zooplankton), dimension(NUM_ZOO),     intent(inout) :: zoo
    type(bacteria), dimension(NUM_BACT),       intent(inout) :: bact
    type(generic_COBALT_type),                 intent(inout) :: cobalt

    ! local
    integer :: axesTi(3)

    axesTi(:)=0

    ! Register the diagnostics for the various phytoplankton
    !
    ! Register Limitation Diagnostics
    !
    phyto(DIAZ)%id_P_C_max = register_diag_field(package_name, "P_C_max_Di", axes(1:3), &
         init_time, "Diaz. Maximum Growth Rate", "sec-1", missing_value = missing_value1)

    phyto(LGP)%id_P_C_max = register_diag_field(package_name, "P_C_max_Lg", axes(1:3), &
         init_time, "Large Phyto. Maximum Growth Rate", "sec-1", missing_value = missing_value1)

    phyto(MDP)%id_P_C_max = register_diag_field(package_name, "P_C_max_Md", axes(1:3), &
         init_time, "Medium Phyto. Maximum Growth Rate", "sec-1", missing_value = missing_value1)

    phyto(SMP)%id_P_C_max = register_diag_field(package_name, "P_C_max_Sm", axes(1:3), &
         init_time, "Small Phyto. Maximum Growth Rate", "sec-1", missing_value = missing_value1)

    phyto(DIAZ)%id_alpha = register_diag_field(package_name, "alpha_Di", axes(1:3), &
         init_time, "Diaz. Photo. vs Irrad. slope", "gC gChl-1 sec-1 (W m-2)-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_alpha = register_diag_field(package_name, "alpha_Lg", axes(1:3), &
         init_time, "Large Phyto. Photo. vs Irrad. slope", "gC gChl-1 sec-1 (W m-2)-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_alpha = register_diag_field(package_name, "alpha_Md", axes(1:3), &
         init_time, "Medium Phyto. Photo. vs Irrad. slope", "gC gChl-1 sec-1 (W m-2)-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_alpha = register_diag_field(package_name, "alpha_Sm", axes(1:3), &
         init_time, "Small Phyto. Photo. vs Irrad. slope", "gC gChl-1 sec-1 (W m-2)-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_bresp = register_diag_field(package_name, "bresp_Di", axes(1:3), &
         init_time, "Diaz. Basal Respiration Rate", "sec-1", missing_value = missing_value1)

    phyto(LGP)%id_bresp = register_diag_field(package_name, "bresp_Lg", axes(1:3), &
         init_time, "Large Phyto. Basal Respiration Rate", "sec-1", missing_value = missing_value1)

    phyto(MDP)%id_bresp = register_diag_field(package_name, "bresp_Md", axes(1:3), &
         init_time, "Medium Phyto. Basal Respiration Rate", "sec-1", missing_value = missing_value1)

    phyto(SMP)%id_bresp = register_diag_field(package_name, "bresp_Sm", axes(1:3), &
         init_time, "Small Phyto. Basal Respiration Rate", "sec-1", missing_value = missing_value1)

    phyto(DIAZ)%id_def_fe = register_diag_field(package_name, "def_fe_Di", axes(1:3), &
         init_time, "Diaz. Phyto. Fe Deficiency", "dimensionless", missing_value = missing_value1)

    phyto(LGP)%id_def_fe = register_diag_field(package_name, "def_fe_Lg", axes(1:3), &
         init_time, "Large Phyto. Fe Deficiency", "dimensionless", missing_value = missing_value1)

    phyto(MDP)%id_def_fe = register_diag_field(package_name, "def_fe_Md", axes(1:3), &
         init_time, "Medium Phyto. Fe Deficiency", "dimensionless", missing_value = missing_value1)

    phyto(SMP)%id_def_fe = register_diag_field(package_name, "def_fe_Sm", axes(1:3), &
         init_time, "Small Phyto. Fe Deficiency", "dimensionless", missing_value = missing_value1)

    phyto(DIAZ)%id_felim = register_diag_field(package_name, "felim_Di", axes(1:3), &
         init_time, "Diaz. Phyto. Fed uptake Limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(LGP)%id_felim = register_diag_field(package_name, "felim_Lg", axes(1:3), &
         init_time, "Large Phyto. Fed uptake Limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(MDP)%id_felim = register_diag_field(package_name, "felim_Md", axes(1:3), &
         init_time, "Medium Phyto. Fed uptake Limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_felim = register_diag_field(package_name, "felim_Sm", axes(1:3), &
         init_time, "Small Phyto. Fed uptake Limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_irrlim = register_diag_field(package_name, "irrlim_Di", axes(1:3), &
         init_time, "Diaz. Phyto. Light Limitation", "dimensionless", missing_value = missing_value1)

    phyto(LGP)%id_irrlim = register_diag_field(package_name, "irrlim_Lg", axes(1:3), &
         init_time, "Large Phyto. Light Limitation", "dimensionless", missing_value = missing_value1)

    phyto(MDP)%id_irrlim = register_diag_field(package_name, "irrlim_Md", axes(1:3), &
         init_time, "Medium Phyto. Light Limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_irrlim = register_diag_field(package_name, "irrlim_Sm", axes(1:3), &
         init_time, "Small Phyto. Light Limitation", "dimensionless", missing_value = missing_value1)

    phyto(DIAZ)%id_theta = register_diag_field(package_name, "theta_Di", axes(1:3), &
         init_time, "Diaz. Phyto. Chl:C", "g Chl (g C)-1", missing_value = missing_value1)

    phyto(LGP)%id_theta = register_diag_field(package_name, "theta_Lg", axes(1:3), &
         init_time, "Large Phyto. Chl:C", "g Chl (g C)-1", missing_value = missing_value1)

    phyto(MDP)%id_theta = register_diag_field(package_name, "theta_Md", axes(1:3), &
         init_time, "Medium Phyto. Chl:C", "g Chl (g C)-1", missing_value = missing_value1)

    phyto(SMP)%id_theta = register_diag_field(package_name, "theta_Sm", axes(1:3), &
         init_time, "Small Phyto. Chl:C", "g Chl (g C)-1", missing_value = missing_value1)

    phyto(DIAZ)%id_chl = register_diag_field(package_name, "chl_Di", axes(1:3), &
         init_time, "Diaz. Phyto. Chlorophyll", "ug Kg-1", missing_value = missing_value1)

    phyto(LGP)%id_chl = register_diag_field(package_name, "chl_Lg", axes(1:3), &
         init_time, "Large Phyto. Chlorophyll", "ug Kg-1", missing_value = missing_value1)

    phyto(MDP)%id_chl = register_diag_field(package_name, "chl_Md", axes(1:3), &
         init_time, "Medium Phyto. Chlorophyll", "ug Kg-1", missing_value = missing_value1)

    phyto(SMP)%id_chl = register_diag_field(package_name, "chl_Sm", axes(1:3), &
         init_time, "Small Phyto. Chlorophyll", "ug Kg-1", missing_value = missing_value1)

    phyto(DIAZ)%id_mu = register_diag_field(package_name, "mu_Di", axes(1:3), &
         init_time, "Diaz. Phyto. Overall Growth Rate", "s-1", missing_value = missing_value1)

    phyto(LGP)%id_mu = register_diag_field(package_name, "mu_Lg", axes(1:3), &
         init_time, "Large Phyto. Overall Growth Rate", "s-1", missing_value = missing_value1)

    phyto(MDP)%id_mu = register_diag_field(package_name, "mu_Md", axes(1:3), &
         init_time, "Medium Phyto. Overall Growth Rate", "s-1", missing_value = missing_value1)

    phyto(SMP)%id_mu = register_diag_field(package_name, "mu_Sm", axes(1:3), &
         init_time, "Small Phyto. Growth Rate", "s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_f_mu_mem = register_diag_field(package_name, "mu_mem_Di", axes(1:3), &
         init_time, "Diaz. Phyto. Growth Memory", "s-1", missing_value = missing_value1)

    phyto(LGP)%id_f_mu_mem = register_diag_field(package_name, "mu_mem_Lg", axes(1:3), &
         init_time, "Large Phyto. Growth memory", "s-1", missing_value = missing_value1)

    phyto(MDP)%id_f_mu_mem = register_diag_field(package_name, "mu_mem_Md", axes(1:3), &
         init_time, "Medium Phyto. Growth memory", "s-1", missing_value = missing_value1)

    phyto(SMP)%id_f_mu_mem = register_diag_field(package_name, "mu_mem_Sm", axes(1:3), &
         init_time, "Small Phyto. Growth Memory", "s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_f_pcmlim_aclm = register_diag_field(package_name, "pcmlim_aclm_Di", axes(1:3), &
         init_time, "Diaz. Phyto. acclimation nut*temp lim", "none", missing_value = missing_value1)

    phyto(LGP)%id_f_pcmlim_aclm = register_diag_field(package_name, "pcmlim_aclm_Lg", axes(1:3), &
         init_time, "Large Phyto. acclimation nut*temp lim", "none", missing_value = missing_value1)

    phyto(MDP)%id_f_pcmlim_aclm = register_diag_field(package_name, "pcmlim_aclm_Md", &
         axes(1:3), &
         init_time, "Medium Phyto. acclimation nut*temp lim", "none", missing_value = missing_value1)

    phyto(SMP)%id_f_pcmlim_aclm = register_diag_field(package_name, "pcmlim_aclm_Sm", axes(1:3), &
         init_time, "Small Phyto. acclimation nut*temp lim", "none", missing_value = missing_value1)

    phyto(DIAZ)%id_pcmlim_aclm_inst = register_diag_field(package_name, "pcmlim_aclm_inst_Di", &
         axes(1:3), &
         init_time, "Diaz. Phyto. instantaneous nut*temp lim", "none", &
         missing_value = missing_value1)

    phyto(LGP)%id_pcmlim_aclm_inst = register_diag_field(package_name, "pcmlim_aclm_inst_Lg", &
         axes(1:3), &
         init_time, "Large Phyto. instantaneous nut*temp lim", "none", &
         missing_value = missing_value1)

    phyto(MDP)%id_pcmlim_aclm_inst = register_diag_field(package_name, "pcmlim_aclm_inst_Md", &
         axes(1:3), &
         init_time, "Medium Phyto. instantaneous nut*temp lim", "none", &
         missing_value = missing_value1)

    phyto(SMP)%id_pcmlim_aclm_inst = register_diag_field(package_name, "pcmlim_aclm_inst_Sm", &
         axes(1:3), &
         init_time, "Small Phyto. instantaneous nut*temp lim", "none", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_vmove = register_diag_field(package_name, "vmove_Di", axes(1:3), &
         init_time, "Diaz. Phyto. movement", "m s-1", missing_value = missing_value1)

    phyto(LGP)%id_vmove = register_diag_field(package_name, "vmove_Lg", axes(1:3), &
         init_time, "Large Phyto. movement", "m s-1", missing_value = missing_value1)

    phyto(MDP)%id_vmove = register_diag_field(package_name, "vmove_Md", axes(1:3), &
         init_time, "Medium Phyto. movement", "m s-1", missing_value = missing_value1)

    phyto(SMP)%id_vmove = register_diag_field(package_name, "vmove_Sm", axes(1:3), &
         init_time, "Small Phyto. movement", "m s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_mu_mix = register_diag_field(package_name, "mu_mix_Di", axes(1:3), &
         init_time, "Diaz. Phyto. ML ave", "s-1", missing_value = missing_value1)

    phyto(LGP)%id_mu_mix = register_diag_field(package_name, "mu_mix_Lg", axes(1:3), &
         init_time, "Large Phyto. ML ave", "s-1", missing_value = missing_value1)

    phyto(MDP)%id_mu_mix = register_diag_field(package_name, "mu_mix_Md", axes(1:3), &
         init_time, "Medium Phyto. ML ave", "s-1", missing_value = missing_value1)

    phyto(SMP)%id_mu_mix = register_diag_field(package_name, "mu_mix_Sm", axes(1:3), &
         init_time, "Small Phyto. ML ave", "s-1", missing_value = missing_value1)

    phyto(LGP)%id_nh4lim = register_diag_field(package_name, "nh4lim_Lg", axes(1:3), &
         init_time, "Ammonia Limitation of Large Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(MDP)%id_nh4lim = register_diag_field(package_name, "nh4lim_Md", axes(1:3), &
         init_time, "Ammonia Limitation of Medium Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_nh4lim = register_diag_field(package_name, "nh4lim_Sm", axes(1:3), &
         init_time, "Ammonia Limitation of Small Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_nh4lim = register_diag_field(package_name, "nh4lim_Di", axes(1:3), &
         init_time, "Ammonia Limitation of Diazo", "dimensionless", missing_value = missing_value1)

    phyto(LGP)%id_no3lim = register_diag_field(package_name, "no3lim_Lg", axes(1:3), &
         init_time, "Nitrate Limitation of Large Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(MDP)%id_no3lim = register_diag_field(package_name, "no3lim_Md", axes(1:3), &
         init_time, "Nitrate Limitation of Medium Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_no3lim = register_diag_field(package_name, "no3lim_Sm", axes(1:3), &
         init_time, "Nitrate Limitation of Small Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_no3lim = register_diag_field(package_name, "no3lim_Di", axes(1:3), &
         init_time, "Ammonia Limitation of Diazo", "dimensionless", missing_value = missing_value1)

    phyto(DIAZ)%id_po4lim = register_diag_field(package_name, "po4lim_Di", axes(1:3), &
         init_time, "Phosphate Limitation of Diaz. Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(LGP)%id_po4lim = register_diag_field(package_name, "po4lim_Lg", axes(1:3), &
         init_time, "Phosphate Limitation of Large Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(MDP)%id_po4lim = register_diag_field(package_name, "po4lim_Md", axes(1:3), &
         init_time, "Phosphate Limitation of Medium Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_po4lim = register_diag_field(package_name, "po4lim_Sm", axes(1:3), &
         init_time, "Phosphate Limitation of Small Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_liebig_lim = register_diag_field(package_name, "liebig_Sm", axes(1:3), &
         init_time, "Overall (Liebig) nutrient lim., Small Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(MDP)%id_liebig_lim = register_diag_field(package_name, "liebig_Md", axes(1:3), &
         init_time, "Overall (Liebig) nutrient lim., Medium Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(LGP)%id_liebig_lim = register_diag_field(package_name, "liebig_Lg", axes(1:3), &
         init_time, "Overall (Liebig) nutrient lim., Large Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_liebig_lim = register_diag_field(package_name, "liebig_Di", axes(1:3), &
         init_time, "Overall (Liebig) nutrient lim., Diazotrophs", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_o2lim = register_diag_field(package_name, "o2lim_Di", axes(1:3), &
         init_time, "Oxygen Limitation of Diaz. Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_q_fe_2_n = register_diag_field(package_name, "q_fe_2_n_Di", axes(1:3), &
         init_time, "Fe:N ratio of Diaz. Phyto", "mol Fe/mol N", missing_value = missing_value1)

    phyto(LGP)%id_q_fe_2_n = register_diag_field(package_name, "q_fe_2_n_Lg", axes(1:3), &
         init_time, "Fe:N ratio of Large Phyto", "mol Fe/mol N", missing_value = missing_value1)

    phyto(MDP)%id_q_fe_2_n = register_diag_field(package_name, "q_fe_2_n_Md", axes(1:3), &
         init_time, "Fe:N ratio of Medium Phyto", "mol Fe/mol N", missing_value = missing_value1)

    phyto(SMP)%id_q_fe_2_n = register_diag_field(package_name, "q_fe_2_n_Sm", axes(1:3), &
         init_time, "Fe:N ratio of Small Phyto", "mol Fe/mol N", missing_value = missing_value1)

    phyto(DIAZ)%id_q_p_2_n = register_diag_field(package_name, "q_p_2_n_Di", axes(1:3), &
         init_time, "P:N ratio of Diaz. Phyto", "mol P/mol N", missing_value = missing_value1)

    phyto(LGP)%id_q_p_2_n = register_diag_field(package_name, "q_p_2_n_Lg", axes(1:3), &
         init_time, "P:N ratio of Large Phyto", "mol P/mol N", missing_value = missing_value1)

    phyto(MDP)%id_q_p_2_n = register_diag_field(package_name, "q_p_2_n_Md", axes(1:3), &
         init_time, "P:N ratio of Medium Phyto", "mol P/mol N", missing_value = missing_value1)

    phyto(SMP)%id_q_p_2_n = register_diag_field(package_name, "q_p_2_n_Sm", axes(1:3), &
         init_time, "P:N ratio of Small Phyto", "mol P/mol N", missing_value = missing_value1)

    phyto(LGP)%id_silim = register_diag_field(package_name, "silim_Lg", axes(1:3), &
         init_time, "SiO4 Limitation of Large Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(MDP)%id_silim = register_diag_field(package_name, "silim_Md", axes(1:3), &
         init_time, "SiO4 Limitation of Medium Phyto", "dimensionless", &
         missing_value = missing_value1)

    phyto(LGP)%id_q_si_2_n = register_diag_field(package_name, "q_si_2_n_Lg", axes(1:3), &
         init_time, "Si:N ratio of Large Phyto", "mol Si/mol N", missing_value = missing_value1)

    phyto(MDP)%id_q_si_2_n = register_diag_field(package_name, "q_si_2_n_Md", axes(1:3), &
         init_time, "Si:N ratio of Medium Phyto", "mol Si/mol N", missing_value = missing_value1)

    !
    ! Register diagnostics for phytoplankton loss terms: zooplankton
    ! CAS: loss diagnostics simplified to just N

    phyto(DIAZ)%id_jzloss_n = register_diag_field(package_name, "jzloss_n_Di", axes(1:3), &
         init_time, "Diazotroph nitrogen loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jzloss_n = register_diag_field(package_name, "jzloss_n_Lg", axes(1:3), &
         init_time, "Large phyto nitrogen loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jzloss_n = register_diag_field(package_name, "jzloss_n_Md", axes(1:3), &
         init_time, "Medium phyto nitrogen loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jzloss_n = register_diag_field(package_name, "jzloss_n_Sm", axes(1:3), &
         init_time, "Small phyto nitrogen loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jzloss_p = register_diag_field(package_name, "jzloss_p_Di", axes(1:3), &
         init_time, "Diazotroph phosphorus loss to zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jzloss_p = register_diag_field(package_name, "jzloss_p_Lg", axes(1:3), &
         init_time, "Large phyto phosphorus loss to zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jzloss_p = register_diag_field(package_name, "jzloss_p_Md", axes(1:3), &
         init_time, "Medium phyto phosphorus loss to zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jzloss_p = register_diag_field(package_name, "jzloss_p_Sm", axes(1:3), &
         init_time, "Small phyto phosphorus loss to zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jzloss_fe = register_diag_field(package_name, "jzloss_fe_Di", axes(1:3), &
         init_time, "Diazotroph iron loss to zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jzloss_fe = register_diag_field(package_name, "jzloss_fe_Lg", axes(1:3), &
         init_time, "Large phyto iron loss to zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jzloss_fe = register_diag_field(package_name, "jzloss_fe_Md", axes(1:3), &
         init_time, "Medium phyto iron loss to zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jzloss_fe = register_diag_field(package_name, "jzloss_fe_Sm", axes(1:3), &
         init_time, "Small phyto iron loss to zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jzloss_sio2 = register_diag_field(package_name, "jzloss_sio2_Di", axes(1:3), &
         init_time, "Diazotroph silica loss to zooplankton", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jzloss_sio2 = register_diag_field(package_name, "jzloss_sio2_Lg", axes(1:3), &
         init_time, "Large phyto silica loss to zooplankton", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jzloss_sio2 = register_diag_field(package_name, "jzloss_sio2_Md", axes(1:3), &
         init_time, "Medium phyto silica loss to zooplankton", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jzloss_sio2 = register_diag_field(package_name, "jzloss_sio2_Sm", axes(1:3), &
         init_time, "Small phyto silica loss to zooplankton", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    !
    !  Register diagnostics for phytoplankton loss terms: aggregation
    !

    phyto(DIAZ)%id_jaggloss_n = register_diag_field(package_name, "jaggloss_n_Di", axes(1:3), &
         init_time, "Diazotroph nitrogen loss to aggregation", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jaggloss_n = register_diag_field(package_name, "jaggloss_n_Lg", axes(1:3), &
         init_time, "Large phyto nitrogen loss to aggregation", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jaggloss_n = register_diag_field(package_name, "jaggloss_n_Md", axes(1:3), &
         init_time, "Medium phyto nitrogen loss to aggregation", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jaggloss_n = register_diag_field(package_name, "jaggloss_n_Sm", axes(1:3), &
         init_time, "Small phyto nitrogen loss to aggregation", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jaggloss_p = register_diag_field(package_name, "jaggloss_p_Di", axes(1:3), &
         init_time, "Diazotroph phosphorus loss to aggregation", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jaggloss_p = register_diag_field(package_name, "jaggloss_p_Lg", axes(1:3), &
         init_time, "Large phyto phosphorus loss to aggregation", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jaggloss_p = register_diag_field(package_name, "jaggloss_p_Md", axes(1:3), &
         init_time, "Medium phyto phosphorus loss to aggregation", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jaggloss_p = register_diag_field(package_name, "jaggloss_p_Sm", axes(1:3), &
         init_time, "Small phyto phosphorus loss to aggregation", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jaggloss_fe = register_diag_field(package_name, "jaggloss_fe_Di", axes(1:3), &
         init_time, "Diazotroph iron loss to aggregation", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jaggloss_fe = register_diag_field(package_name, "jaggloss_fe_Lg", axes(1:3), &
         init_time, "Large phyto iron loss to aggregation", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jaggloss_fe = register_diag_field(package_name, "jaggloss_fe_Md", axes(1:3), &
         init_time, "Medium phyto iron loss to aggregation", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jaggloss_fe = register_diag_field(package_name, "jaggloss_fe_Sm", axes(1:3), &
         init_time, "Small phyto iron loss to aggregation", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jaggloss_sio2 = register_diag_field(package_name, "jaggloss_sio2_Di", &
         axes(1:3), &
         init_time, "Diazotroph silica loss to aggregation", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jaggloss_sio2 = register_diag_field(package_name, "jaggloss_sio2_Lg", &
         axes(1:3), &
         init_time, "Large phyto silica loss to aggregation", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jaggloss_sio2 = register_diag_field(package_name, "jaggloss_sio2_Md", &
         axes(1:3), &
         init_time, "Medium phyto silica loss to aggregation", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jaggloss_sio2 = register_diag_field(package_name, "jaggloss_sio2_Sm", &
         axes(1:3), &
         init_time, "Small phyto silica loss to aggregation", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_stress_fac = register_diag_field(package_name, "stress_fac_Di", axes(1:3), &
         init_time, "Diazotroph stress factor", "dimensionless", missing_value = missing_value1)

    phyto(LGP)%id_stress_fac = register_diag_field(package_name, "stress_fac_Lg", axes(1:3), &
         init_time, "Large phyto stress factor", "dimensionless", missing_value = missing_value1)

    phyto(MDP)%id_stress_fac = register_diag_field(package_name, "stress_fac_Md", axes(1:3), &
         init_time, "Medium phyto stress factor", "dimensionless", missing_value = missing_value1)

    phyto(SMP)%id_stress_fac = register_diag_field(package_name, "stress_fac_Sm", axes(1:3), &
         init_time, "Small phyto stress factor", "dimensionless", missing_value = missing_value1)

    !
    !  Register diagnostics for phytoplankton loss terms: viruses
    !
    phyto(DIAZ)%id_jvirloss_n = register_diag_field(package_name, "jvirloss_n_Di", axes(1:3), &
         init_time, "Diazotroph nitrogen loss to viruses", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jvirloss_n = register_diag_field(package_name, "jvirloss_n_Lg", axes(1:3), &
         init_time, "Large phyto nitrogen loss to viruses", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jvirloss_n = register_diag_field(package_name, "jvirloss_n_Md", axes(1:3), &
         init_time, "Medium phyto nitrogen loss to viruses", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jvirloss_n = register_diag_field(package_name, "jvirloss_n_Sm", axes(1:3), &
         init_time, "Small phyto nitrogen loss to viruses", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jvirloss_p = register_diag_field(package_name, "jvirloss_p_Di", axes(1:3), &
         init_time, "Diazotroph phosphorus loss to viruses", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jvirloss_p = register_diag_field(package_name, "jvirloss_p_Lg", axes(1:3), &
         init_time, "Large phyto phosphorus loss to viruses", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jvirloss_p = register_diag_field(package_name, "jvirloss_p_Md", axes(1:3), &
         init_time, "Medium phyto phosphorus loss to viruses", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jvirloss_p = register_diag_field(package_name, "jvirloss_p_Sm", axes(1:3), &
         init_time, "Small phyto phosphorus loss to viruses", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jvirloss_fe = register_diag_field(package_name, "jvirloss_fe_Di", axes(1:3), &
         init_time, "Diazotroph iron loss to viruses", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jvirloss_fe = register_diag_field(package_name, "jvirloss_fe_Lg", axes(1:3), &
         init_time, "Large phyto iron loss to viruses", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jvirloss_fe = register_diag_field(package_name, "jvirloss_fe_Md", axes(1:3), &
         init_time, "Medium phyto iron loss to viruses", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jvirloss_fe = register_diag_field(package_name, "jvirloss_fe_Sm", axes(1:3), &
         init_time, "Small phyto iron loss to viruses", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jvirloss_sio2 = register_diag_field(package_name, "jvirloss_sio2_Di", &
         axes(1:3), &
         init_time, "Diazotroph silica loss to viruses", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jvirloss_sio2 = register_diag_field(package_name, "jvirloss_sio2_Lg", &
         axes(1:3), &
         init_time, "Large phyto silica loss to viruses", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jvirloss_sio2 = register_diag_field(package_name, "jvirloss_sio2_Md", &
         axes(1:3), &
         init_time, "Medium phyto silica loss to viruses", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jvirloss_sio2 = register_diag_field(package_name, "jvirloss_sio2_Sm", &
         axes(1:3), &
         init_time, "Small phyto silica loss to viruses", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    !
    !  Register diagnostics for phytoplankton loss terms: mortality
    !

    phyto(DIAZ)%id_jmortloss_n = register_diag_field(package_name, "jmortloss_n_Di", axes(1:3), &
         init_time, "Diazotroph nitrogen loss to mortality", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jmortloss_n = register_diag_field(package_name, "jmortloss_n_Lg", axes(1:3), &
         init_time, "Large phyto nitrogen loss to mortality", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jmortloss_n = register_diag_field(package_name, "jmortloss_n_Md", axes(1:3), &
         init_time, "Medium phyto nitrogen loss to mortality", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jmortloss_n = register_diag_field(package_name, "jmortloss_n_Sm", axes(1:3), &
         init_time, "Small phyto nitrogen loss to mortality", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jmortloss_p = register_diag_field(package_name, "jmortloss_p_Di", axes(1:3), &
         init_time, "Diazotroph phosphorus loss to mortality", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jmortloss_p = register_diag_field(package_name, "jmortloss_p_Lg", axes(1:3), &
         init_time, "Large phyto phosphorus loss to mortality", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jmortloss_p = register_diag_field(package_name, "jmortloss_p_Md", axes(1:3), &
         init_time, "Medium phyto phosphorus loss to mortality", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jmortloss_p = register_diag_field(package_name, "jmortloss_p_Sm", axes(1:3), &
         init_time, "Small phyto phosphorus loss to mortality", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jmortloss_fe = register_diag_field(package_name, "jmortloss_fe_Di", axes(1:3), &
         init_time, "Diazotroph iron loss to mortality", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jmortloss_fe = register_diag_field(package_name, "jmortloss_fe_Lg", axes(1:3), &
         init_time, "Large phyto iron loss to mortality", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jmortloss_fe = register_diag_field(package_name, "jmortloss_fe_Md", &
         axes(1:3), &
         init_time, "Medium phyto iron loss to mortality", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jmortloss_fe = register_diag_field(package_name, "jmortloss_fe_Sm", axes(1:3), &
         init_time, "Small phyto iron loss to mortality", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)
    !
    ! Register diagnostics for phytoplankton silica dissolution from respiration and mortality
    !
    phyto(DIAZ)%id_jdissloss_si = register_diag_field(package_name, "jdissloss_si_Di", axes(1:3), &
         init_time, "Diazotroph silica dissolution from respiration and mortality", &
         "mol Si kg-1 s-1", missing_value = missing_value1)

    phyto(LGP)%id_jdissloss_si = register_diag_field(package_name, "jdissloss_si_Lg", axes(1:3), &
         init_time, "Large phyto silica dissolution from respiration and mortality", &
         "mol Si kg-1 s-1", missing_value = missing_value1)

    phyto(MDP)%id_jdissloss_si = register_diag_field(package_name, "jdissloss_si_Md", &
         axes(1:3), &
         init_time, "Medium phyto silica dissolution from respiration and mortality", &
         "mol Si kg-1 s-1", missing_value = missing_value1)

    phyto(SMP)%id_jdissloss_si = register_diag_field(package_name, "jdissloss_si_Sm", axes(1:3), &
         init_time, "Small phyto silica dissolution from respiration and mortality", &
         "mol Si kg-1 s-1", missing_value = missing_value1)
    !
    ! Register diagnostics for phytoplankton exudation
    !
    phyto(DIAZ)%id_jexuloss_n = register_diag_field(package_name, "jexuloss_n_Di", axes(1:3), &
         init_time, "Diazotroph nitrogen loss via exudation", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jexuloss_n = register_diag_field(package_name, "jexuloss_n_Lg", axes(1:3), &
         init_time, "Large phyto nitrogen loss via exudation", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jexuloss_n = register_diag_field(package_name, "jexuloss_n_Md", axes(1:3), &
         init_time, "Medium phyto nitrogen loss via exudation", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jexuloss_n = register_diag_field(package_name, "jexuloss_n_Sm", axes(1:3), &
         init_time, "Small phyto nitrogen loss via exudation", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jexuloss_p = register_diag_field(package_name, "jexuloss_p_Di", axes(1:3), &
         init_time, "Diazotroph phosphorus loss via exudation", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jexuloss_p = register_diag_field(package_name, "jexuloss_p_Lg", axes(1:3), &
         init_time, "Large phyto phosphorus loss via exudation", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jexuloss_p = register_diag_field(package_name, "jexuloss_p_Md", axes(1:3), &
         init_time, "Medium phyto phosphorus loss via exudation", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jexuloss_p = register_diag_field(package_name, "jexuloss_p_Sm", axes(1:3), &
         init_time, "Small phyto phosphorus loss via exudation", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jexuloss_fe = register_diag_field(package_name, "jexuloss_fe_Di", axes(1:3), &
         init_time, "Diazotroph iron loss via exudation", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jexuloss_fe = register_diag_field(package_name, "jexuloss_fe_Lg", axes(1:3), &
         init_time, "Large phyto iron loss via exudation", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jexuloss_fe = register_diag_field(package_name, "jexuloss_fe_Md", axes(1:3), &
         init_time, "Medium phyto iron loss via exudation", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jexuloss_fe = register_diag_field(package_name, "jexuloss_fe_Sm", axes(1:3), &
         init_time, "Small phyto iron loss via exudation", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Phytoplankton losses to higher predators (0 by default)
    !
    phyto(DIAZ)%id_jhploss_n = register_diag_field(package_name, "jhploss_n_Di", axes(1:3), &
         init_time, "Diazotroph nitrogen loss to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jhploss_n = register_diag_field(package_name, "jhploss_n_Lg", axes(1:3), &
         init_time, "Large phyto nitrogen loss to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jhploss_n = register_diag_field(package_name, "jhploss_n_Md", axes(1:3), &
         init_time, "Medium phyto nitrogen loss to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jhploss_n = register_diag_field(package_name, "jhploss_n_Sm", axes(1:3), &
         init_time, "Small phyto nitrogen loss to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jhploss_p = register_diag_field(package_name, "jhploss_p_Di", axes(1:3), &
         init_time, "Diazotroph phosphorus loss to higher predators", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jhploss_p = register_diag_field(package_name, "jhploss_p_Lg", axes(1:3), &
         init_time, "Large phyto phosphorus loss to higher predators", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jhploss_p = register_diag_field(package_name, "jhploss_p_Md", axes(1:3), &
         init_time, "Medium phyto phosphorus loss to higher predators", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jhploss_p = register_diag_field(package_name, "jhploss_p_Sm", axes(1:3), &
         init_time, "Small phyto phosphorus loss to higher predators", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jhploss_fe = register_diag_field(package_name, "jhploss_fe_Di", axes(1:3), &
         init_time, "Diazotroph iron loss to higher predators", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jhploss_fe = register_diag_field(package_name, "jhploss_fe_Lg", axes(1:3), &
         init_time, "Large phyto iron loss to higher predators", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jhploss_fe = register_diag_field(package_name, "jhploss_fe_Md", axes(1:3), &
         init_time, "Medium phyto iron loss to higher predators", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jhploss_fe = register_diag_field(package_name, "jhploss_fe_Sm", axes(1:3), &
         init_time, "Small phyto iron loss to higher predators", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jhploss_sio2 = register_diag_field(package_name, "jhploss_sio2_Di", axes(1:3), &
         init_time, "Diazotroph silica loss to higher predators", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jhploss_sio2 = register_diag_field(package_name, "jhploss_sio2_Lg", axes(1:3), &
         init_time, "Large phyto silica loss to higher predators", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jhploss_sio2 = register_diag_field(package_name, "jploss_sio2_Md", axes(1:3), &
         init_time, "Medium phyto silica loss to higher predators", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jhploss_sio2 = register_diag_field(package_name, "jhploss_sio2_Sm", axes(1:3), &
         init_time, "Small phyto silica loss to higher predators", "mol Si kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register dynamic silicate diagnostics
    !
    cobalt%id_nlg_diatoms = register_diag_field(package_name, "nlg_diatoms", axes(1:3), &
         init_time, "large phytoplankton nitrogen from diatoms", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_nmd_diatoms = register_diag_field(package_name, "nmd_diatoms", axes(1:3), &
         init_time, "medium phytoplankton nitrogen from diatoms", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_nlg_diatoms = register_diag_field(package_name, "nlg_misc", axes(1:3), &
         init_time, "large phytoplankton nitrogen from misc non-diatoms", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_nmd_diatoms = register_diag_field(package_name, "nmd_misc", axes(1:3), &
         init_time, "medium phytoplankton nitrogen from misc. non-diatoms", "mol kg-1", &
         missing_value = missing_value1)
    !
    ! Register Phytoplankton Production Diagnostics
    !

    phyto(DIAZ)%id_juptake_n2 = register_diag_field(package_name, "juptake_n2_Di", axes(1:3), &
         init_time, "Nitrogen fixation", "mol N kg-1 s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_juptake_fe = register_diag_field(package_name, "juptake_fe_Di", axes(1:3), &
         init_time, "Diaz. phyto. Fed uptake", "mol Fe kg-1 s-1", missing_value = missing_value1)

    phyto(LGP)%id_juptake_fe = register_diag_field(package_name, "juptake_fe_Lg", axes(1:3), &
         init_time, "Large phyto. Fed uptake", "mol Fe kg-1 s-1", missing_value = missing_value1)

    phyto(MDP)%id_juptake_fe = register_diag_field(package_name, "juptake_fe_Md", axes(1:3), &
         init_time, "Medium phyto. Fed uptake", "mol Fe kg-1 s-1", missing_value = missing_value1)

    phyto(SMP)%id_juptake_fe = register_diag_field(package_name, "juptake_fe_Sm", axes(1:3), &
         init_time, "Small phyto. Fed uptake", "mol Fe kg-1 s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_juptake_nh4 = register_diag_field(package_name, "juptake_nh4_Di", axes(1:3), &
         init_time, "Diaz. phyto. NH4 uptake", "mol NH4 kg-1 s-1", missing_value = missing_value1)

    phyto(LGP)%id_juptake_nh4 = register_diag_field(package_name, "juptake_nh4_Lg", axes(1:3), &
         init_time, "Large phyto. NH4 uptake", "mol NH4 kg-1 s-1", missing_value = missing_value1)

    phyto(MDP)%id_juptake_nh4 = register_diag_field(package_name, "juptake_nh4_Md", axes(1:3), &
         init_time, "Medium phyto. NH4 uptake", "mol NH4 kg-1 s-1", missing_value = missing_value1)

    phyto(SMP)%id_juptake_nh4 = register_diag_field(package_name, "juptake_nh4_Sm", axes(1:3), &
         init_time, "Small phyto. NH4 uptake", "mol NH4 kg-1 s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_juptake_no3 = register_diag_field(package_name, "juptake_no3_Di", axes(1:3), &
         init_time, "Diaz. phyto. NO3 uptake", "mol NO3 kg-1 s-1", missing_value = missing_value1)

    phyto(LGP)%id_juptake_no3 = register_diag_field(package_name, "juptake_no3_Lg", axes(1:3), &
         init_time, "Large phyto. NO3 uptake", "mol NO3 kg-1 s-1", missing_value = missing_value1)

    phyto(MDP)%id_juptake_no3 = register_diag_field(package_name, "juptake_no3_Md", axes(1:3), &
         init_time, "Medium phyto. NO3 uptake", "mol NO3 kg-1 s-1", missing_value = missing_value1)

    phyto(SMP)%id_juptake_no3 = register_diag_field(package_name, "juptake_no3_Sm", axes(1:3), &
         init_time, "Small phyto. NO3 uptake", "mol NO3 kg-1 s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_juptake_po4 = register_diag_field(package_name, "juptake_po4_Di", axes(1:3), &
         init_time, "Diaz. phyto. PO4 uptake", "mol PO4 kg-1 s-1", missing_value = missing_value1)

    phyto(LGP)%id_juptake_po4 = register_diag_field(package_name, "juptake_po4_Lg", axes(1:3), &
         init_time, "Large phyto. PO4 uptake", "mol PO4 kg-1 s-1", missing_value = missing_value1)

    phyto(MDP)%id_juptake_po4 = register_diag_field(package_name, "juptake_po4_Md", axes(1:3), &
         init_time, "Medium phyto. PO4 uptake", "mol PO4 kg-1 s-1", missing_value = missing_value1)

    phyto(SMP)%id_juptake_po4 = register_diag_field(package_name, "juptake_po4_Sm", axes(1:3), &
         init_time, "Small phyto. PO4 uptake", "mol PO4 kg-1 s-1", missing_value = missing_value1)

    phyto(LGP)%id_juptake_sio4 = register_diag_field(package_name, "juptake_sio4_Lg", axes(1:3), &
         init_time, "Large phyto. SiO4 uptake", "mol kg-1 s-1", missing_value = missing_value1)

    phyto(MDP)%id_juptake_sio4 = register_diag_field(package_name, "juptake_sio4_Md", &
         axes(1:3), &
         init_time, "Medium phyto. SiO4 uptake", "mol kg-1 s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_jprod_n = register_diag_field(package_name, "jprod_ndi", axes(1:3), &
         init_time, "Diazotroph Nitrogen production", "mol kg-1 s-1", missing_value = missing_value1)

    phyto(SMP)%id_jprod_n = register_diag_field(package_name, "jprod_nsmp", axes(1:3), &
         init_time, "Small phyto. Nitrogen production", "mol kg-1 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jprod_n = register_diag_field(package_name, "jprod_nmdp", axes(1:3), &
         init_time, "Medium phyto. Nitrogen production", "mol kg-1 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jprod_n = register_diag_field(package_name, "jprod_nlgp", axes(1:3), &
         init_time, "Large phyto. Nitrogen production", "mol kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register zooplankton diagnostics, starting with losses of zooplankton to ingestion by zooplankton
    !

    zoo(SMZ)%id_jzloss_n = register_diag_field(package_name, "jzloss_n_Smz", axes(1:3), &
         init_time, "Small zooplankton nitrogen loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jzloss_n = register_diag_field(package_name, "jzloss_n_Mdz", axes(1:3), &
         init_time, "Medium-sized zooplankton nitrogen loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jzloss_n = register_diag_field(package_name, "jzloss_n_Lgz", axes(1:3), &
         init_time, "Large zooplankton nitrogen loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jzloss_p = register_diag_field(package_name, "jzloss_p_Smz", axes(1:3), &
         init_time, "Small zooplankton phosphorus loss to zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jzloss_p = register_diag_field(package_name, "jzloss_p_Mdz", axes(1:3), &
         init_time, "Medium-sized zooplankton phosphorus loss to zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jzloss_p = register_diag_field(package_name, "jzloss_p_Lgz", axes(1:3), &
         init_time, "Large zooplankton phosphorus loss to zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register diagnostics for zooplankton loss terms: higher predators
    !

    zoo(SMZ)%id_jhploss_n = register_diag_field(package_name, "jhploss_n_Smz", axes(1:3), &
         init_time, "Small zooplankton nitrogen loss to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jhploss_n = register_diag_field(package_name, "jhploss_n_Mdz", axes(1:3), &
         init_time, "Medium-sized zooplankton nitrogen loss to higher predators", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jhploss_n = register_diag_field(package_name, "jhploss_n_Lgz", axes(1:3), &
         init_time, "Large zooplankton nitrogen loss to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jhploss_p = register_diag_field(package_name, "jhploss_p_Smz", axes(1:3), &
         init_time, "Small zooplankton phosphorus loss to higher predators", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jhploss_p = register_diag_field(package_name, "jhploss_p_Mdz", axes(1:3), &
         init_time, "Medium-sized zooplankton phosphorus loss to higher predators", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jhploss_p = register_diag_field(package_name, "jhploss_p_Lgz", axes(1:3), &
         init_time, "Large zooplankton phosphorus loss to higher predators", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register zooplankton ingestion rates
    !

    zoo(SMZ)%id_jingest_n = register_diag_field(package_name, "jingest_n_Smz", axes(1:3), &
         init_time, "Ingestion of nitrogen by small zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jingest_n = register_diag_field(package_name, "jingest_n_Mdz", axes(1:3), &
         init_time, "Ingestion of nitrogen by medium-sized zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jingest_n = register_diag_field(package_name, "jingest_n_Lgz", axes(1:3), &
         init_time, "Ingestion of nitrogen by large zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jingest_p = register_diag_field(package_name, "jingest_p_Smz", axes(1:3), &
         init_time, "Ingestion of phosphorous by small zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jingest_p = register_diag_field(package_name, "jingest_p_Mdz", axes(1:3), &
         init_time, "Ingestion of phosphorous by medium-sized zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jingest_p = register_diag_field(package_name, "jingest_p_Lgz", axes(1:3), &
         init_time, "Ingestion of phosphorous by large zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jingest_sio2 = register_diag_field(package_name, "jingest_sio2_Smz", axes(1:3), &
         init_time, "Ingestion of sio2 by small zooplankton", "mol SiO2 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jingest_sio2 = register_diag_field(package_name, "jingest_sio2_Mdz", axes(1:3), &
         init_time, "Ingestion of sio2 by medium-sized zooplankton", "mol SiO2 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jingest_sio2 = register_diag_field(package_name, "jingest_sio2_Lgz", axes(1:3), &
         init_time, "Ingestion of sio2 by large zooplankton", "mol SiO2 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jingest_fe = register_diag_field(package_name, "jingest_fe_Smz", axes(1:3), &
         init_time, "Ingestion of Fe by small zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jingest_fe = register_diag_field(package_name, "jingest_fe_Mdz", axes(1:3), &
         init_time, "Ingestion of Fe by medium-sized zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jingest_fe = register_diag_field(package_name, "jingest_fe_Lgz", axes(1:3), &
         init_time, "Ingestion of Fe by large zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register detrital production terms for zooplankton
    !

    zoo(SMZ)%id_jprod_ndet = register_diag_field(package_name, "jprod_ndet_Smz", axes(1:3), &
         init_time, "Production of nitrogen detritus by small zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_ndet = register_diag_field(package_name, "jprod_ndet_Mdz", axes(1:3), &
         init_time, "Production of nitrogen detritus by medium zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_ndet = register_diag_field(package_name, "jprod_ndet_Lgz", axes(1:3), &
         init_time, "Production of nitrogen detritus by large zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jprod_pdet = register_diag_field(package_name, "jprod_pdet_Smz", axes(1:3), &
         init_time, "Production of phosphorous detritus by small zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_pdet = register_diag_field(package_name, "jprod_pdet_Mdz", axes(1:3), &
         init_time, "Production of phosphorous detritus by medium zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_pdet = register_diag_field(package_name, "jprod_pdet_Lgz", axes(1:3), &
         init_time, "Production of phosphorous detritus by large zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jprod_sidet = register_diag_field(package_name, "jprod_sidet_Smz", axes(1:3), &
         init_time, "Production of opal detritus by small zooplankton", "mol SiO2 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_sidet = register_diag_field(package_name, "jprod_sidet_Mdz", axes(1:3), &
         init_time, "Production of opal detritus by medium zooplankton", "mol SiO2 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_sidet = register_diag_field(package_name, "jprod_sidet_Lgz", axes(1:3), &
         init_time, "Production of opal detritus by large zooplankton", "mol SiO2 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jprod_sio4 = register_diag_field(package_name, "jprod_sio4_Smz", axes(1:3), &
         init_time, "Production of sio4 through grazing/dissolution", "mol SiO4 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_sio4 = register_diag_field(package_name, "jprod_sio4_Mdz", axes(1:3), &
         init_time, "Production of sio4 through grazing/dissolution", "mol SiO4 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_sio4 = register_diag_field(package_name, "jprod_sio4_Lgz", axes(1:3), &
         init_time, "Production of sio4 through grazing/dissolution", "mol SiO4 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jprod_fedet = register_diag_field(package_name, "jprod_fedet_Smz", axes(1:3), &
         init_time, "Production of iron detritus by small zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_fedet = register_diag_field(package_name, "jprod_fedet_Mdz", axes(1:3), &
         init_time, "Production of iron detritus by medium zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_fedet = register_diag_field(package_name, "jprod_fedet_Lgz", axes(1:3), &
         init_time, "Production of iron detritus by large zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Detrus losses to zooplankton and higher predators.  These are 0 by default but could be made non-zero with the
    ! introduction of detrivory.  These are N diagnostics for this, but may want to add P, Si, Fe etc.
    !
    cobalt%id_det_jzloss_n = register_diag_field(package_name, "det_jzloss_n", axes(1:3), &
         init_time, "Loss of nitrogen detritus to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_det_jhploss_n = register_diag_field(package_name, "det_jhploss_n", axes(1:3), &
         init_time, "Loss of nitrogen detritus to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register dissolved organic/inorganic production terms for zooplankton
    !
    ! Labile dissolved organic nitrogen
    zoo(SMZ)%id_jprod_ldon = register_diag_field(package_name, "jprod_ldon_Smz", axes(1:3), &
         init_time, "Production of labile dissolved organic nitrogen by small zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jprod_ldon = register_diag_field(package_name, "jprod_ldon_Mdz", axes(1:3), &
         init_time, "Production of labile dissolved organic nitrogen by medium zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jprod_ldon = register_diag_field(package_name, "jprod_ldon_Lgz", axes(1:3), &
         init_time, "Production of labile dissolved organic nitrogen by large zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    ! Labile dissolved organic phosphorous
    zoo(SMZ)%id_jprod_ldop = register_diag_field(package_name, "jprod_ldop_Smz", axes(1:3), &
         init_time, "Production of labile dissolved organic phosphorous by small zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jprod_ldop = register_diag_field(package_name, "jprod_ldop_Mdz", axes(1:3), &
         init_time, "Production of labile dissolved organic phosphorous by medium zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jprod_ldop = register_diag_field(package_name, "jprod_ldop_Lgz", axes(1:3), &
         init_time, "Production of labile dissolved organic phosphorous by large zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    ! Refractory dissolved organic nitrogen
    zoo(SMZ)%id_jprod_srdon = register_diag_field(package_name, "jprod_srdon_Smz", axes(1:3), &
         init_time, &
         "Production of semi-refractory dissolved organic nitrogen by small zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jprod_srdon = register_diag_field(package_name, "jprod_srdon_Mdz", axes(1:3), &
         init_time, &
         "Production of semi-refractory dissolved organic nitrogen by medium zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jprod_srdon = register_diag_field(package_name, "jprod_srdon_Lgz", axes(1:3), &
         init_time, &
         "Production of semi-refractory dissolved organic nitrogen by large zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    ! Labile dissolved organic phosphorous
    zoo(SMZ)%id_jprod_srdop = register_diag_field(package_name, "jprod_srdop_Smz", axes(1:3), &
         init_time, &
         "Production of semi-refractory dissolved organic phosphorous by small zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jprod_srdop = register_diag_field(package_name, "jprod_srdop_Mdz", axes(1:3), &
         init_time, &
         "Production of semi-refractory dissolved organic phosphorous by medium zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jprod_srdop = register_diag_field(package_name, "jprod_srdop_Lgz", axes(1:3), &
         init_time, &
         "Production of semi-refractory dissolved organic phosphorous by large zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    ! semi-labile dissolved organic nitrogen
    zoo(SMZ)%id_jprod_sldon = register_diag_field(package_name, "jprod_sldon_Smz", axes(1:3), &
         init_time, "Production of semi-labile dissolved organic nitrogen by small zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jprod_sldon = register_diag_field(package_name, "jprod_sldon_Mdz", axes(1:3), &
         init_time, "Production of semi-labile dissolved organic nitrogen by medium zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jprod_sldon = register_diag_field(package_name, "jprod_sldon_Lgz", axes(1:3), &
         init_time, "Production of semi-labile dissolved organic nitrogen by large zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    ! semi-labile dissolved organic phosphorous
    zoo(SMZ)%id_jprod_sldop = register_diag_field(package_name, "jprod_sldop_Smz", axes(1:3), &
         init_time, &
         "Production of semi-labile dissolved organic phosphorous by small zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jprod_sldop = register_diag_field(package_name, "jprod_sldop_Mdz", axes(1:3), &
         init_time, &
         "Production of semi-labile dissolved organic phosphorous by medium zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jprod_sldop = register_diag_field(package_name, "jprod_sldop_Lgz", axes(1:3), &
         init_time, &
         "Production of semi-labile dissolved organic phosphorous by large zooplankton", &
         "mol P kg-1 s-1", missing_value = missing_value1)

    ! dissolved iron
    zoo(SMZ)%id_jprod_fed = register_diag_field(package_name, "jprod_fed_Smz", axes(1:3), &
         init_time, "Production of dissolved iron by small zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_fed = register_diag_field(package_name, "jprod_fed_Mdz", axes(1:3), &
         init_time, "Production of dissolved iron by medium-sized zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_fed = register_diag_field(package_name, "jprod_fed_Lgz", axes(1:3), &
         init_time, "Production of dissolved iron by large zooplankton", "mol Fe kg-1 s-1", &
         missing_value = missing_value1)

    ! phosphate
    zoo(SMZ)%id_jprod_po4 = register_diag_field(package_name, "jprod_po4_Smz", axes(1:3), &
         init_time, "Production of phosphate by small zooplankton", "mol PO4 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_po4 = register_diag_field(package_name, "jprod_po4_Mdz", axes(1:3), &
         init_time, "Production of phosphate by medium-sized zooplankton", "mol PO4 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_po4 = register_diag_field(package_name, "jprod_po4_Lgz", axes(1:3), &
         init_time, "Production of phosphate by large zooplankton", "mol PO4 kg-1 s-1", &
         missing_value = missing_value1)

    ! ammonia
    zoo(SMZ)%id_jprod_nh4 = register_diag_field(package_name, "jprod_nh4_Smz", axes(1:3), &
         init_time, "Production of ammonia by small zooplankton", "mol NH4 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_nh4 = register_diag_field(package_name, "jprod_nh4_Mdz", axes(1:3), &
         init_time, "Production of ammonia by medium-sized zooplankton", "mol NH4 kg-1 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_nh4 = register_diag_field(package_name, "jprod_nh4_Lgz", axes(1:3), &
         init_time, "Production of ammonia by large zooplankton", "mol NH4 kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register zooplankton production terms
    !

    zoo(SMZ)%id_jprod_n = register_diag_field(package_name, "jprod_nsmz", axes(1:3), &
         init_time, "Production of new biomass (nitrogen) by small zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_n = register_diag_field(package_name, "jprod_nmdz", axes(1:3), &
         init_time, "Production of new biomass (nitrogen) by medium-sized zooplankton", &
         "mol N kg-1 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jprod_n = register_diag_field(package_name, "jprod_nlgz", axes(1:3), &
         init_time, "Production of new biomass (nitrogen) by large zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_o2lim = register_diag_field(package_name, "o2lim_Smz", axes(1:3), &
         init_time, "Oxygen limitation of small zooplankton", "dimensionless", &
         missing_value = missing_value1)

    zoo(MDZ)%id_o2lim = register_diag_field(package_name, "o2lim_Mdz", axes(1:3), &
         init_time, "Oxygen limitation of medium-sized zooplankton", "dimensionless", &
         missing_value = missing_value1)

    zoo(LGZ)%id_o2lim = register_diag_field(package_name, "o2lim_Lgz", axes(1:3), &
         init_time, "Oxygen limitation of large zooplankton", "dimensionless", &
         missing_value = missing_value1)

    zoo(SMZ)%id_temp_lim = register_diag_field(package_name, "temp_lim_Smz", axes(1:3), &
         init_time, "Temperature limitation of small zooplankton", "dimensionless", &
         missing_value = missing_value1)

    zoo(MDZ)%id_temp_lim = register_diag_field(package_name, "temp_lim_Mdz", axes(1:3), &
         init_time, "Temperature limitation of medium-sized zooplankton", "dimensionless", &
         missing_value = missing_value1)

    zoo(LGZ)%id_temp_lim = register_diag_field(package_name, "temp_lim_Lgz", axes(1:3), &
         init_time, "Temperature limitation of large zooplankton", "dimensionless", &
         missing_value = missing_value1)

    !
    ! Register bacterial diagnostics, starting with losses of bacteria to ingestion by zooplankton
    ! CAS: limit loss terms to N

    bact(1)%id_jzloss_n = register_diag_field(package_name, "jzloss_n_Bact", axes(1:3), &
         init_time, "Bacterial nitrogen loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    bact(1)%id_jzloss_p = register_diag_field(package_name, "jzloss_p_Bact", axes(1:3), &
         init_time, "Bacterial phosphorus loss to zooplankton", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register diagnostics for bacteria loss terms: viruses
    !

    bact(1)%id_jvirloss_n = register_diag_field(package_name, "jvirloss_n_Bact", axes(1:3), &
         init_time, "Bacterial nitrogen loss to viruses", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    bact(1)%id_jvirloss_p = register_diag_field(package_name, "jvirloss_p_Bact", axes(1:3), &
         init_time, "Bacterial phosphorus loss to viruses", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register diagnostics for bacteria loss terms: higher predators (0 by default)
    !

    bact(1)%id_jhploss_n = register_diag_field(package_name, "jhploss_n_Bact", axes(1:3), &
         init_time, "Bacterial nitrogen loss to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    bact(1)%id_jhploss_p = register_diag_field(package_name, "jhploss_p_Bact", axes(1:3), &
         init_time, "Bacterial phosphorus loss to higher predators", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register bacterial uptake terms
    !

    bact(1)%id_juptake_ldon = register_diag_field(package_name, "juptake_ldon", axes(1:3), &
         init_time, "Bacterial uptake of labile dissolved organic nitrogen", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    bact(1)%id_juptake_ldop = register_diag_field(package_name, "juptake_ldop", axes(1:3), &
         init_time, "Bacterial uptake of labile dissolved organic phosphorous", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register dissolved inorganic production terms for bacteria
    !
    ! phosphate
    bact(1)%id_jprod_po4 = register_diag_field(package_name, "jprod_po4_Bact", axes(1:3), &
         init_time, "Production of phosphate by bacteria", "mol PO4 kg-1 s-1", &
         missing_value = missing_value1)

    ! ammonia
    bact(1)%id_jprod_nh4 = register_diag_field(package_name, "jprod_nh4_Bact", axes(1:3), &
         init_time, "Production of ammonia by bacteria", "mol NH4 kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Register bacterial production terms
    !

    bact(1)%id_jprod_n = register_diag_field(package_name, "jprod_nbact", axes(1:3), &
         init_time, "Production of new biomass (nitrogen) by bacteria", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    bact(1)%id_o2lim = register_diag_field(package_name, "o2lim_Bact", axes(1:3), &
         init_time, "Oxygen limitation of bacteria", "dimensionless", missing_value = missing_value1)

    bact(1)%id_ldonlim = register_diag_field(package_name, "ldonlim_Bact", axes(1:3), &
         init_time, "ldon limitation of bacteria", "dimensionless", missing_value = missing_value1)

    bact(1)%id_temp_lim = register_diag_field(package_name, "temp_lim_Bact", axes(1:3), &
         init_time, "Temperature limitation of bacteria", "dimensionless", &
         missing_value = missing_value1)

    bact(1)%id_no3lim = register_diag_field(package_name, "no3lim_Bact", axes(1:3), &
         init_time, "Nitrate limitation of bacteria", "dimensionless", &
         missing_value = missing_value1)

    !
    ! Register general COBALT diagnostics
    !

    cobalt%id_co3_sol_arag = register_diag_field(package_name, "co3_sol_arag", axes(1:3), &
         init_time, "Carbonate Ion Solubility for Aragonite", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_co3_sol_calc = register_diag_field(package_name, "co3_sol_calc", axes(1:3), &
         init_time, "Carbonate Ion Solubility for Calcite", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_omega_arag = register_diag_field(package_name, "omega_arag", axes(1:3), &
         init_time, "Carbonate Ion Saturation State for Aragonite", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_omega_calc = register_diag_field(package_name, "omega_calc", axes(1:3), &
         init_time, "Carbonate Ion Saturation State for Calcite", "mol kg-1", &
         missing_value = missing_value1)

    !
    ! A few overall production diagnostics
    !

    cobalt%id_jprod_cadet_arag = register_diag_field(package_name, "jprod_cadet_arag", axes(1:3), &
         init_time, "Aragonite CaCO3 production", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jprod_cadet_calc = register_diag_field(package_name, "jprod_cadet_calc", axes(1:3), &
         init_time, "Calcite CaCO3 production", "mol kg-1 s-1", missing_value = missing_value1)

     ! << Add neritic CaCO3 burial effect on dissolved inorganic carbon (DIC)
    cobalt%id_jdic_caco3_nerbur = register_diag_field(package_name, "jdic_caco3_nerbur", &
         axes(1:3), &
         init_time, "Impact of neritic CaCO3 burial on DIC", "mol kg-1 s-1", &
         missing_value = missing_value1)
    ! >>

    cobalt%id_jprod_lithdet = register_diag_field(package_name, "jprod_lithdet", axes(1:3), &
         init_time, "Lithogenic detritus production", "g kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jprod_sidet = register_diag_field(package_name, "jprod_sidet", axes(1:3), &
         init_time, "opal detritus production", "mol SiO2 kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jprod_sio4 = register_diag_field(package_name, "jprod_sio4", axes(1:3), &
         init_time, "sio4 production", "mol SiO2 kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jprod_fedet = register_diag_field(package_name, "jprod_fedet", axes(1:3), &
         init_time, "Detrital Fedet production", "mol Fe kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jprod_ndet = register_diag_field(package_name, "jprod_ndet", axes(1:3), &
         init_time, "Detrital PON production", "mol N kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jprod_ndet_fast = register_diag_field(package_name, "jprod_ndet_fast", axes(1:3), &
         init_time, "Fast-sinking detrital PON production", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_pdet = register_diag_field(package_name, "jprod_pdet", axes(1:3), &
         init_time, "Detrital phosphorus production", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_pdet_fast = register_diag_field(package_name, "jprod_pdet_fast", axes(1:3), &
         init_time, "Fast-sinking detrital phosphorus production", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_ldon = register_diag_field(package_name, "jprod_ldon", axes(1:3), &
         init_time, "labile dissolved organic nitrogen production", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_ldop = register_diag_field(package_name, "jprod_ldop", axes(1:3), &
         init_time, "labile dissolved organic phosphorous production", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_srdon = register_diag_field(package_name, "jprod_srdon", axes(1:3), &
         init_time, "refractory dissolved organic nitrogen production", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_srdop = register_diag_field(package_name, "jprod_srdop", axes(1:3), &
         init_time, "refractory dissolved organic phosphorous production", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_sldon = register_diag_field(package_name, "jprod_sldon", axes(1:3), &
         init_time, "semi-labile dissolved organic nitrogen production", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_sldop = register_diag_field(package_name, "jprod_sldop", axes(1:3), &
         init_time, "semi-labile dissolved organic phosphorous production", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_po4 = register_diag_field(package_name, "jprod_po4", axes(1:3), &
         init_time, "phosphate production", "mol PO4 kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jprod_nh4 = register_diag_field(package_name, "jprod_nh4", axes(1:3), &
         init_time, "NH4 production", "mol NH4 kg-1 s-1", missing_value = missing_value1)

! CAS added a "plus_btm" version of jprod_nh4 to use for the remoc CMIP variable
    cobalt%id_jprod_nh4_plus_btm = register_diag_field(package_name, "jprod_nh4_plus_btm", &
         axes(1:3), &
         init_time, "NH4 production plus bottom fluxes", "mol NH4 kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! loss diagnostics: detrital loss terms
    !

    cobalt%id_det_jzloss_n = register_diag_field(package_name, "det_jzloss_n", axes(1:3), &
         init_time, "nitrogen detritus loss to zooplankton", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_det_jhploss_n = register_diag_field(package_name, "det_jhploss_n", axes(1:3), &
         init_time, "nitrogen detritus loss to higher predators", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Loss diagnostics: dissolution and remineralization
    !

    cobalt%id_jdiss_sidet = register_diag_field(package_name, "jdiss_sidet", axes(1:3), &
         init_time, "SiO2 detritus dissolution", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jdiss_cadet_arag = register_diag_field(package_name, "jdiss_cadet_arag", axes(1:3), &
         init_time, "CaCO3 detritus dissolution", "mol CaCO3 kg-1 s-1", &
         missing_value = missing_value1)

    ! CAS added diagnostic including bottom fluxes for cmip5
    cobalt%id_jdiss_cadet_arag_plus_btm = register_diag_field(package_name, &
         "jdiss_cadet_arag_plus_btm", axes(1:3), &
         init_time, "CaCO3 detritus dissolution plus bottom dissolution", "mol CaCO3 kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jdiss_cadet_calc = register_diag_field(package_name, "jdiss_cadet_calc", axes(1:3), &
         init_time, "CaCO3 detritus dissolution", "mol CaCO3 kg-1 s-1", &
         missing_value = missing_value1)

    ! CAS added diagnostic including bottom fluxes for cmip5
    cobalt%id_jdiss_cadet_calc_plus_btm = register_diag_field(package_name, &
         "jdiss_cadet_calc_plus_btm", axes(1:3), &
         init_time, "CaCO3 detritus dissolution plus bottom dissolution", "mol CaCO3 kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jremin_ndet = register_diag_field(package_name, "jremin_ndet", axes(1:3), &
         init_time, "Nitrogen detritus remineralization", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jremin_ndet_fast = register_diag_field(package_name, "jremin_ndet_fast", axes(1:3), &
         init_time, "Fast-sinking nitrogen detritus remineralization", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jremin_pdet = register_diag_field(package_name, "jremin_pdet", axes(1:3), &
         init_time, "Phosphorous detritus remineralization", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jremin_pdet_fast = register_diag_field(package_name, "jremin_pdet_fast", axes(1:3), &
         init_time, "Fast-sinking phosphorous detritus remineralization", "mol P kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jremin_fedet = register_diag_field(package_name, "jremin_fedet", axes(1:3), &
         init_time, "Iron detritus remineralization", "mol kg-1 s-1", missing_value = missing_value1)

    !
    ! iron cycling diagnostics
    !

    cobalt%id_jprod_fed = register_diag_field(package_name, "jprod_fed", axes(1:3), &
         init_time, "dissolved iron production", "mol Fe kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jfed = register_diag_field(package_name, "jfed", axes(1:3), &
         init_time, "Dissolved Iron Change", "mol Fe kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jfe_ads = register_diag_field(package_name, "jfe_ads", axes(1:3), &
         init_time, "Iron adsorption", "mol Fe kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jfe_coast = register_diag_field(package_name, "jfe_coast", axes(1:3), &
         init_time, "Coastal iron efflux", "mol Fe kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jfe_iceberg = register_diag_field(package_name, "jfe_iceberg", axes(1:3), &
         init_time, "iceberg iron efflux", "mol Fe kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jno3_iceberg = register_diag_field(package_name, "jno3_iceberg", axes(1:3), &
         init_time, "iceberg nitrate efflux", "mol N kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jpo4_iceberg = register_diag_field(package_name, "jpo4_iceberg", axes(1:3), &
         init_time, "iceberg phosphate efflux", "mol P kg-1 s-1", missing_value = missing_value1)

    cobalt%id_kfe_eq_lig = register_diag_field(package_name, "kfe_eq_lig", axes(1:3), &
         init_time, "Effective ligand binding strength", "kg mol-1", missing_value = missing_value1)

    cobalt%id_feprime = register_diag_field(package_name, "feprime", axes(1:3), &
         init_time, "Free iron concentration", "mol kg-1", missing_value = missing_value1)

    cobalt%id_ligand = register_diag_field(package_name, "ligand", axes(1:3), &
         init_time, "ligand concentration", "mol kg-1", missing_value = missing_value1)

    cobalt%id_fe_sol = register_diag_field(package_name, "fe_sol", axes(1:3), &
         init_time, "iron solubility", "mol kg-1", missing_value = missing_value1)

    !
    ! Temperature limitation diagnostics
    !

    cobalt%id_expkT = register_diag_field(package_name, "expkT", axes(1:3), &
         init_time, "Eppley temperature limitation factor", "dimensionless", &
         missing_value = missing_value1)

    cobalt%id_expkreminT = register_diag_field(package_name, "expkreminT", axes(1:3), &
         init_time, "Detritus remineralization temperature limitation factor", "dimensionless", &
         missing_value = missing_value1)

    cobalt%id_hp_o2lim = register_diag_field(package_name, "hp_o2lim", axes(1:3), &
         init_time, "Oxygen limitation of higher predators", "dimensionless", &
         missing_value = missing_value1)

    cobalt%id_hp_temp_lim = register_diag_field(package_name, "hp_temp_lim", axes(1:3), &
         init_time, "Temperature limitation of higher predators", "dimensionless", &
         missing_value = missing_value1)

    !
    ! Some additional light field diagnostics
    !

    cobalt%id_irr_inst = register_diag_field(package_name, "irr_inst", axes(1:3), &
         init_time, "Instantaneous Light", "W m-2", missing_value = missing_value1)

    cobalt%id_irr_mix = register_diag_field(package_name, "irr_mix", axes(1:3), &
         init_time, "Instantaneous light, avg over mixing layer", "W m-2", &
         missing_value = missing_value1)

    cobalt%id_irr_aclm_inst = register_diag_field(package_name, "irr_aclm_inst", axes(1:3), &
         init_time, "Instantaneous light, avg over photoadapt layer", "W m-2", &
         missing_value = missing_value1)

    !vardesc_temp = vardesc("irr_aclm_z","Acclimation irradiance with vertical structure preserved in mixed layer",'h','L','s','W m-2','f')
    !cobalt%id_irr_aclm_z = register_diag_field(package_name, vardesc_temp%name, axes(1:3),&
    !     init_time, vardesc_temp%longname,vardesc_temp%units, missing_value = missing_value1)
    !
    !vardesc_temp = vardesc("irr_aclm","Acclimation irradiance",'h','L','s','W m-2','f')
    !cobalt%id_irr_aclm = register_diag_field(package_name, vardesc_temp%name, axes(1:3),&
    !     init_time, vardesc_temp%longname,vardesc_temp%units, missing_value = missing_value1)

    !
    ! Nitrification/Denitrification/Anammox diagnostics
    !

    cobalt%id_jprod_no3nitrif = register_diag_field(package_name, "jprod_no3nitrif", axes(1:3), &
         init_time, "Nitrification", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_juptake_nh4nitrif = register_diag_field(package_name, "juptake_nh4nitrif", &
         axes(1:3), &
         init_time, "NH4 uptake via Nitrification", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jno3denit_wc = register_diag_field(package_name, "jno3denit_wc", axes(1:3), &
         init_time, "Water column Denitrification", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jnamx = register_diag_field(package_name, "jnamx", axes(1:3), &
         init_time, "Fixed N loss via Anammox", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_juptake_nh4amx = register_diag_field(package_name, "juptake_nh4amx", axes(1:3), &
         init_time, "NH4 uptake via Anammox", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_juptake_no3amx = register_diag_field(package_name, "juptake_no3amx", axes(1:3), &
         init_time, "NO3 uptake via Anammox", "mol kg-1 s-1", missing_value = missing_value1)

    !
    ! Track total aerobic respiration in the water column
    !

    cobalt%id_jo2resp_wc = register_diag_field(package_name, "jo2resp_wc", axes(1:3), &
         init_time, "Water column aerobic respiration", "mol kg-1 s-1", &
         missing_value = missing_value1)

    !
    ! Some useful totals (change to moles kg-1 for MOM6?)
    !

    cobalt%id_nphyto_tot = register_diag_field(package_name, "nphyto_tot", axes(1:3), &
         init_time, "Total N: Di+Lg+Md+Sm", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_tot_layer_int_c = register_diag_field(package_name, "tot_layer_int_c", axes(1:3), &
         init_time, "Total Carbon (DIC+OC+IC) boxwise", "mol m-2", missing_value = missing_value1)

    cobalt%id_tot_layer_int_fe = register_diag_field(package_name, "tot_layer_int_fe", axes(1:3), &
         init_time, "Total Iron (Fed_OFe) boxwise", "mol m-2", missing_value = missing_value1)

    cobalt%id_tot_layer_int_n = register_diag_field(package_name, "tot_layer_int_n", axes(1:3), &
         init_time, "Total Nitrogen (NO3+NH4+ON) boxwise", "mol m-2", missing_value = missing_value1)

    cobalt%id_tot_layer_int_p = register_diag_field(package_name, "tot_layer_int_p", axes(1:3), &
         init_time, "Total Phosphorus (PO4+OP) boxwise", "mol m-2", missing_value = missing_value1)

    cobalt%id_tot_layer_int_si = register_diag_field(package_name, "tot_layer_int_si", axes(1:3), &
         init_time, "Total Silicon (SiO4+SiO2) boxwise", "mol m-2", missing_value = missing_value1)

    cobalt%id_tot_layer_int_o2 = register_diag_field(package_name, "tot_layer_int_o2", axes(1:3), &
         init_time, "Total oxygen boxwise", "mol m-2", missing_value = missing_value1)

    cobalt%id_tot_layer_int_alk = register_diag_field(package_name, "tot_layer_int_alk", &
         axes(1:3), &
         init_time, "Total alkalinity boxwise", "mol m-2", missing_value = missing_value1)

    cobalt%id_total_filter_feeding = register_diag_field(package_name, "total_filter_feeding", &
         axes(1:3), &
         init_time, "Total filter feeding by large organisms", "mol N kg-1 s-1", &
         missing_value = missing_value1)

    !
    !  Save river, depositon and bulk elemental fluxes
    !

    cobalt%id_dep_dry_fed = register_diag_field(package_name, "dep_dry_fed", axes(1:2), &
         init_time, "Dry Deposition of Iron to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_dry_lith = register_diag_field(package_name, "dep_dry_lith", axes(1:2), &
         init_time, "Dry Deposition of Lithogenic Material", "g m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_dry_nh4 = register_diag_field(package_name, "dep_dry_nh4", axes(1:2), &
         init_time, "Dry Deposition of Ammonia to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_dry_no3 = register_diag_field(package_name, "dep_dry_no3", axes(1:2), &
         init_time, "Dry Deposition of Nitrate to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_dry_po4 = register_diag_field(package_name, "dep_dry_po4", axes(1:2), &
         init_time, "Dry Deposition of Phosphate to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_wet_fed = register_diag_field(package_name, "dep_wet_fed", axes(1:2), &
         init_time, "Wet Deposition of Iron to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_wet_lith = register_diag_field(package_name, "dep_wet_lith", axes(1:2), &
         init_time, "Wet Deposition of Lithogenic Material", "g m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_wet_nh4 = register_diag_field(package_name, "dep_wet_nh4", axes(1:2), &
         init_time, "Wet Deposition of Ammonia to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_wet_no3 = register_diag_field(package_name, "dep_wet_no3", axes(1:2), &
         init_time, "Wet Deposition of Nitrate to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_dep_wet_po4 = register_diag_field(package_name, "dep_wet_po4", axes(1:2), &
         init_time, "Wet Deposition of Phosphate to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_pka_nh3     = register_diag_field(package_name, "pka_nh3", axes(1:2), &
         init_time, "pKa of NH3", "", missing_value = missing_value1)

    cobalt%id_runoff_flux_alk = register_diag_field(package_name, "runoff_flux_alk", axes(1:2), &
         init_time, "Alkalinity runoff flux to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_runoff_flux_dic = register_diag_field(package_name, "runoff_flux_dic", axes(1:2), &
         init_time, "Dissolved Inorganic Carbon runoff flux to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_runoff_flux_di14c = register_diag_field(package_name, "runoff_flux_di14c", &
         axes(1:2), &
         init_time, "Dissolved Inorganic Carbon 14 runoff flux to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_runoff_flux_fed = register_diag_field(package_name, "runoff_flux_fed", axes(1:2), &
         init_time, "Iron runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_lith = register_diag_field(package_name, "runoff_flux_lith", axes(1:2), &
         init_time, "Lithogenic runoff flux to the ocean", "g m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_runoff_flux_no3 = register_diag_field(package_name, "runoff_flux_no3", axes(1:2), &
         init_time, "Nitrate runoff flux to the ocean", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_runoff_flux_ldon = register_diag_field(package_name, "runoff_flux_ldon", axes(1:2), &
         init_time, "LDON runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_sldon = register_diag_field(package_name, "runoff_flux_sldon", &
         axes(1:2), &
         init_time, "SLDON runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_srdon = register_diag_field(package_name, "runoff_flux_srdon", &
         axes(1:2), &
         init_time, "SRDON runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_ndet = register_diag_field(package_name, "runoff_flux_ndet", axes(1:2), &
         init_time, "NDET runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_pdet = register_diag_field(package_name, "runoff_flux_pdet", axes(1:2), &
         init_time, "PDET runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_po4 = register_diag_field(package_name, "runoff_flux_po4", axes(1:2), &
         init_time, "PO4 runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_ldop = register_diag_field(package_name, "runoff_flux_ldop", axes(1:2), &
         init_time, "LDOP runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_sldop = register_diag_field(package_name, "runoff_flux_sldop", &
         axes(1:2), &
         init_time, "SLDOP runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_runoff_flux_srdop = register_diag_field(package_name, "runoff_flux_srdop", &
         axes(1:2), &
         init_time, "SRDOP runoff flux to the ocean", "mol m-2 s-1", missing_value = missing_value1)

    !
    ! 3D sinking information
    !
    ! These were historically saved at the tracer points.  The sinking algorithm, however, uses an upwind scheme that
    ! applies the fluxes at the grid interfaces.  We have thus added an option to save the fluxes at the interfaces
    !
    ! Niki: The register_diag_field interface needs to be extended to take the MOM6 axes_grp as argument
    !      instead of this integer array axes_grp%handle
    !      Currently the actual MOM6 diag axes is chosen to be T or Tl based on the size of the axes argument, 2 or 3.
    !      The actual values of these axes argument are not used, only their size is checked to determine the diag axes!
    !      This is not correct since axesTi and axesTl are both of size 3, likewise there are many axes of size 2.
    !      To accomodate axesTi with the least amount of code modification we can set and check for an input array of size 1.

    cobalt%id_fcadet_arag_tp = register_diag_field(package_name, "fcadet_arag", axesTi(1:3), &
         init_time, "CaCO3 sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fcadet_calc_tp = register_diag_field(package_name, "fcadet_calc", axesTi(1:3), &
         init_time, "CaCO3 sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_ffedet_tp = register_diag_field(package_name, "ffedet", axesTi(1:3), &
         init_time, "fedet sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_flithdet_tp = register_diag_field(package_name, "flithdet", axesTi(1:3), &
         init_time, "lithdet sinking flux (@tracer points)", "g m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fndet_tp = register_diag_field(package_name, "fndet", axesTi(1:3), &
         init_time, "Nitrogen detritus sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fndet_fast_tp = register_diag_field(package_name, "fndet_fast", axesTi(1:3), &
         init_time, "Fast-sinking nitrogen detritus flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fpdet_tp = register_diag_field(package_name, "fpdet", axesTi(1:3), &
         init_time, "Phosphorus detritus sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fpdet_fast_tp = register_diag_field(package_name, "fpdet_fast", axesTi(1:3), &
         init_time, "Fast-sinking phosphorus detritus flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fsidet_tp = register_diag_field(package_name, "fsidet", axesTi(1:3), &
         init_time, "sidet sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_ffetot_tp = register_diag_field(package_name, "ffetot", axesTi(1:3), &
         init_time, "total Fe sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fntot_tp = register_diag_field(package_name, "fntot", axesTi(1:3), &
         init_time, "total N sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fptot_tp = register_diag_field(package_name, "fptot", axesTi(1:3), &
         init_time, "total P sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fsitot_tp = register_diag_field(package_name, "fsitot", axesTi(1:3), &
         init_time, "total Si sinking flux (@tracer points)", "mol m-2 s-1", &
         missing_value = missing_value1)

    !
    ! Option to save at interfaces: sinking is parameterized using an upwind scheme applied at the interfaces, so
    ! these should yield flux metrics that are more consistent with the numerics.
    !
    cobalt%id_fcadet_arag_i = register_diag_field(package_name, "fcadet_arag_i", axesTi(1:1), &
         init_time, "CaCO3 sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fcadet_calc_i = register_diag_field(package_name, "fcadet_calc_i", axesTi(1:1), &
         init_time, "CaCO3 sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_ffedet_i = register_diag_field(package_name, "ffedet_i", axesTi(1:1), &
         init_time, "fedet sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_flithdet_i = register_diag_field(package_name, "flithdet_i", axesTi(1:1), &
         init_time, "lithdet sinking flux (@interfaces)", "g m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fndet_i = register_diag_field(package_name, "fndet_i", axesTi(1:1), &
         init_time, "Nitrogen detritus sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fndet_fast_i = register_diag_field(package_name, "fndet_fast_i", axesTi(1:1), &
         init_time, "Fast-sinking nitrogen detritus flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fpdet_i = register_diag_field(package_name, "fpdet_i", axesTi(1:1), &
         init_time, "Phosphorus detritus sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fpdet_fast_i = register_diag_field(package_name, "fpdet_fast_i", axesTi(1:1), &
         init_time, "Fast-sinking phosphorus detritus flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fsidet_i = register_diag_field(package_name, "fsidet_i", axesTi(1:1), &
         init_time, "sidet sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_ffetot_i = register_diag_field(package_name, "ffetot_i", axesTi(1:1), &
         init_time, "total Fe sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fntot_i = register_diag_field(package_name, "fntot_i", axesTi(1:1), &
         init_time, "total N sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fptot_i = register_diag_field(package_name, "fptot_i", axesTi(1:1), &
         init_time, "total P sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fsitot_i = register_diag_field(package_name, "fsitot_i", axesTi(1:1), &
         init_time, "total Si sinking flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1)

    !
    ! 2D sinking, bottom source/sink and burial diagnostics
    !

    ! CAS: 3/9/2021: Changed variable long names to better differentiate calcite from aragonite
    cobalt%id_fcadet_arag_btm = register_diag_field(package_name, "fcadet_arag_btm", axes(1:2), &
         init_time, "Aragonite sinking flux at bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fcadet_calc_btm = register_diag_field(package_name, "fcadet_calc_btm", axes(1:2), &
         init_time, "Calcite sinking flux at bottom", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_fcased_burial = register_diag_field(package_name, "fcased_burial", axes(1:2), &
         init_time, "Calcite permanent burial flux", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_fcased_redis = register_diag_field(package_name, "fcased_redis", axes(1:2), &
         init_time, "Calcite redissolution from sediments", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fcased_redis_surfresp = register_diag_field(package_name, "fcased_redis_surfresp", &
         axes(1:2), &
         init_time, "Calcite redissolution rom sediments, surfresp", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_cased_redis_coef = register_diag_field(package_name, "cased_redis_coef", axes(1:2), &
         init_time, "Calcite redissolution from sediments, deepresp coefficient, ", "s-1", &
         missing_value = missing_value1)

    cobalt%id_cased_redis_delz = register_diag_field(package_name, "cased_redis_delz", axes(1:2), &
         init_time, "Calcite redissolution from sediments, effective depth", "none (0-1)", &
         missing_value = missing_value1)

    cobalt%id_ffedet_btm = register_diag_field(package_name, "ffedet_btm", axes(1:2), &
         init_time, "Iron detritus sinking flux burial", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_ffe_btm = register_diag_field(package_name, "ffedi_btm", axes(1:2), &
         init_time, "diazo Fe sinking flux to bottom", "mol m-2 s-1", missing_value = missing_value1)

    phyto(LGP)%id_ffe_btm = register_diag_field(package_name, "ffelg_btm", axes(1:2), &
         init_time, "large phyto Fe sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_ffe_btm = register_diag_field(package_name, "ffemd_btm", axes(1:2), &
         init_time, "medium phyto Fe sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_ffe_btm = register_diag_field(package_name, "ffesm_btm", axes(1:2), &
         init_time, "small phyto Fe sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_ffetot_btm = register_diag_field(package_name, "ffetot_btm", axes(1:2), &
         init_time, "Total Fe sinking flux to bottom", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_ffe_sed = register_diag_field(package_name, "ffe_sed", axes(1:2), &
         init_time, "Sediment iron efflux", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_ffe_geotherm = register_diag_field(package_name, "ffe_geotherm", axes(1:2), &
         init_time, "Geothermal iron efflux", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_flithdet_btm = register_diag_field(package_name, "flithdet_btm", axes(1:2), &
         init_time, "Lithogenic detrital sinking flux burial", "g m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fndet_btm = register_diag_field(package_name, "fndet_btm", axes(1:2), &
         init_time, "Nitrogen detritus sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fndet_fast_btm = register_diag_field(package_name, "fndet_fast_btm", axes(1:2), &
         init_time, "Fast-sinking nitrogen detritus flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_fn_btm = register_diag_field(package_name, "fndi_btm", axes(1:2), &
         init_time, "diazo N sinking flux to bottom", "mol m-2 s-1", missing_value = missing_value1)

    phyto(LGP)%id_fn_btm = register_diag_field(package_name, "fnlg_btm", axes(1:2), &
         init_time, "large phyto N sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_fn_btm = register_diag_field(package_name, "fnmd_btm", axes(1:2), &
         init_time, "medium phyto N sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_fn_btm = register_diag_field(package_name, "fnsm_btm", axes(1:2), &
         init_time, "small phyto N sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fntot_btm = register_diag_field(package_name, "fntot_btm", axes(1:2), &
         init_time, "Total N sinking flux to bottom", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_fnso4red_sed = register_diag_field(package_name, "fnso4red_sed", axes(1:2), &
         init_time, "Sediment Ndet remineralized by SO4 reduction without HS- oxidation", &
         "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_fno3denit_sed = register_diag_field(package_name, "fno3denit_sed", axes(1:2), &
         init_time, "Sediment denitrification flux", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_fnoxic_sed = register_diag_field(package_name, "fnoxic_sed", axes(1:2), &
         init_time, "Sediment oxic Ndet remineralization flux", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fpdet_btm = register_diag_field(package_name, "fpdet_btm", axes(1:2), &
         init_time, "Phosphorus detritus sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fpdet_fast_btm = register_diag_field(package_name, "fpdet_fast_btm", axes(1:2), &
         init_time, "Fast-sinking phosphorus detritus flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_fp_btm = register_diag_field(package_name, "fpdi_btm", axes(1:2), &
         init_time, "diazo P sinking flux to bottom", "mol m-2 s-1", missing_value = missing_value1)

    phyto(LGP)%id_fp_btm = register_diag_field(package_name, "fplg_btm", axes(1:2), &
         init_time, "large phyto P sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_fp_btm = register_diag_field(package_name, "fpmd_btm", axes(1:2), &
         init_time, "medium phyto P sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_fp_btm = register_diag_field(package_name, "fpsm_btm", axes(1:2), &
         init_time, "small phyto P sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fptot_btm = register_diag_field(package_name, "fptot_btm", axes(1:2), &
         init_time, "Total P sinking flux to bottom", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_fsidet_btm = register_diag_field(package_name, "fsidet_btm", axes(1:2), &
         init_time, "sidet sinking flux to bottom", "mol m-2 s-1", missing_value = missing_value1)

    phyto(LGP)%id_fsi_btm = register_diag_field(package_name, "fsilg_btm", axes(1:2), &
         init_time, "large phyto Si sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_fsi_btm = register_diag_field(package_name, "fsimd_btm", axes(1:2), &
         init_time, "medium phyto Si sinking flux to bottom", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fsitot_btm = register_diag_field(package_name, "fsitot_btm", axes(1:2), &
         init_time, "Total Si sinking flux to bottom", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_frac_burial = register_diag_field(package_name, "frac_burial", axes(1:2), &
         init_time, "fraction of organic matter buried", "dimensionless", &
         missing_value = missing_value1)

    cobalt%id_fn_burial = register_diag_field(package_name, "fn_burial", axes(1:2), &
         init_time, "ndet burial flux", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_fp_burial = register_diag_field(package_name, "fp_burial", axes(1:2), &
         init_time, "pdet burial flux", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_b_alk = register_diag_field(package_name, "b_alk", axes(1:2), &
         init_time, "Benthic alk flux into ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_b_dic = register_diag_field(package_name, "b_dic", axes(1:2), &
         init_time, "Benthic dic flux into ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_b_fed = register_diag_field(package_name, "b_fed", axes(1:2), &
         init_time, "Benthic fed flux into ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_b_nh4 = register_diag_field(package_name, "b_nh4", axes(1:2), &
         init_time, "Benthic nh4 flux into ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_b_no3 = register_diag_field(package_name, "b_no3", axes(1:2), &
         init_time, "Benthic no3 flux into ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_b_o2 = register_diag_field(package_name, "b_o2", axes(1:2), &
         init_time, "Benthic o2 flux into ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_b_po4 = register_diag_field(package_name, "b_po4", axes(1:2), &
         init_time, "Benthic po4 flux into ocean", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_b_sio4 = register_diag_field(package_name, "b_sio4", axes(1:2), &
         init_time, "Benthic sio4 flux into ocean", "mol m-2 s-1", missing_value = missing_value1)

    !
    ! Surface Diagnostics
    !

    cobalt%id_pco2surf = register_diag_field(package_name, "pco2surf", axes(1:2), &
         init_time, "Oceanic pCO2", "uatm", missing_value = missing_value1)

    cobalt%id_co2_alpha = register_diag_field(package_name, "co2_alpha", axes(1:2), &
         init_time, "Solubility of CO2 for air", "mol/kg/atm", missing_value = missing_value1)

    cobalt%id_co2_csurf = register_diag_field(package_name, "co2_csurf", axes(1:2), &
         init_time, "H2CO3+CO2(aq) (H2CO3*) in water", "mol kg-1", missing_value = missing_value1)

    cobalt%id_pnh3surf = register_diag_field(package_name, "pnh3surf", axes(1:2), &
         init_time, "Oceanic pNH3", "uatm", missing_value = missing_value1)

    cobalt%id_nh3_alpha = register_diag_field(package_name, "nh3_alpha", axes(1:2), &
         init_time, "Solubility of NH3 for air", "mol/kg/atm", missing_value = missing_value1)

    cobalt%id_nh3_csurf = register_diag_field(package_name, "nh3_csurf", axes(1:2), &
         init_time, "concentration of nh3 in water", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_alk = register_diag_field(package_name, "sfc_alk", axes(1:2), &
         init_time, "Surface Alkalinity", "eq kg-1", missing_value = missing_value1)

    cobalt%id_sfc_cadet_arag = register_diag_field(package_name, "sfc_cadet_arag", axes(1:2), &
         init_time, "Surface Detrital Aragonite", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_cadet_calc = register_diag_field(package_name, "sfc_cadet_calc", axes(1:2), &
         init_time, "Surface Detrital Calcite", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_dic = register_diag_field(package_name, "sfc_dic", axes(1:2), &
         init_time, "Surface Dissolved Inorganic Carbon", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_fed = register_diag_field(package_name, "sfc_fed", axes(1:2), &
         init_time, "Surface Dissolved Iron", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_ldon = register_diag_field(package_name, "sfc_ldon", axes(1:2), &
         init_time, "Surface Labile Dissolved Organic Nitrogen", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_sfc_sldon = register_diag_field(package_name, "sfc_sldon", axes(1:2), &
         init_time, "Surface semi-labile Dissolved Organic Nitrogen", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_sfc_srdon = register_diag_field(package_name, "sfc_srdon", axes(1:2), &
         init_time, "Surface semi-refractory Dissolved Organic Nitrogen", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_sfc_no3 = register_diag_field(package_name, "sfc_no3", axes(1:2), &
         init_time, "Surface NO3", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_nh4 = register_diag_field(package_name, "sfc_nh4", axes(1:2), &
         init_time, "Surface NH4", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_po4 = register_diag_field(package_name, "sfc_po4", axes(1:2), &
         init_time, "Surface PO4", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_sio4 = register_diag_field(package_name, "sfc_sio4", axes(1:2), &
         init_time, "Surface SiO4", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_htotal = register_diag_field(package_name, "sfc_htotal", axes(1:2), &
         init_time, "Surface Htotal", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_o2 = register_diag_field(package_name, "sfc_o2", axes(1:2), &
         init_time, "Surface Oxygen", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_chl = register_diag_field(package_name, "sfc_chl", axes(1:2), &
         init_time, "Surface Chl", "ug kg-1", missing_value = missing_value1)

    cobalt%id_sfc_irr = register_diag_field(package_name, "sfc_irr", axes(1:2), &
         init_time, "Surface Irradiance", "W m-2", missing_value = missing_value1)

    cobalt%id_sfc_irr_aclm = register_diag_field(package_name, "sfc_irr_aclm", axes(1:2), &
         init_time, "Surface day irrad. over photacclim. time scale", "W m-2", &
         missing_value = missing_value1)

    cobalt%id_sfc_temp = register_diag_field(package_name, "sfc_temp", axes(1:2), &
         init_time, "Surface Temperature", "deg C", missing_value = missing_value1)

    cobalt%id_btm_temp = register_diag_field(package_name, "btm_temp", axes(1:2), &
         init_time, "Bottom Temperature", "deg C", missing_value = missing_value1)

    cobalt%id_btm_o2 = register_diag_field(package_name, "btm_o2", axes(1:2), &
         init_time, "Bottom Oxygen", "mol kg-1", missing_value = missing_value1)

    cobalt%id_btm_no3 = register_diag_field(package_name, "btm_no3", axes(1:2), &
         init_time, "Bottom NO3", "mol kg-1", missing_value = missing_value1)

    cobalt%id_btm_alk = register_diag_field(package_name, "btm_alk", axes(1:2), &
         init_time, "Bottom Alkalinity", "eq kg-1", missing_value = missing_value1)

    cobalt%id_btm_dic = register_diag_field(package_name, "btm_dic", axes(1:2), &
         init_time, "Bottom Dissolved Inorganic Carbon", "mol kg-1", missing_value = missing_value1)

    cobalt%id_btm_htotal = register_diag_field(package_name, "btm_htotal", axes(1:2), &
         init_time, "Bottom Htotal", "mol kg-1", missing_value = missing_value1)

    cobalt%id_btm_co3_ion = register_diag_field(package_name, "btm_co3_ion", axes(1:2), &
         init_time, "Bottom Carbonate Ion", "mol kg-1", missing_value = missing_value1)

    cobalt%id_btm_co3_sol_arag = register_diag_field(package_name, "btm_co3_sol_arag", axes(1:2), &
         init_time, "Bottom Aragonite Solubility", "mol kg-1", missing_value = missing_value1)

    cobalt%id_btm_co3_sol_calc = register_diag_field(package_name, "btm_co3_sol_calc", axes(1:2), &
         init_time, "Bottom Calcite Solubility", "mol kg-1", missing_value = missing_value1)

    cobalt%id_btm_omega_arag = register_diag_field(package_name, "btm_omega_arag", axes(1:2), &
         init_time, "Bottom saturation state for aragonite", "none", missing_value = missing_value1)

    cobalt%id_btm_omega_calc = register_diag_field(package_name, "btm_omega_calc", axes(1:2), &
         init_time, "Bottom saturation state for calcite", "none", missing_value = missing_value1)

 ! Diagnostics to assess averaging over the bottom mixed layer.
    cobalt%id_grid_kmt_diag = register_diag_field(package_name, "grid_kmt_diag", axes(1:2), &
         init_time, "The k-index of the bottom grid cell", "none", missing_value = missing_value1)

    cobalt%id_rho_dzt_kmt_diag = register_diag_field(package_name, "rho_dzt_kmt_diag", axes(1:2), &
         init_time, "The thickness of the bottom grid cell", "kg m-2", &
         missing_value = missing_value1)

    cobalt%id_k_bot_diag = register_diag_field(package_name, "k_bot_diag", axes(1:2), &
         init_time, "The k-index of shallowest grid cell included in the bottom boundary", "none", &
         missing_value = missing_value1)

    cobalt%id_rho_dzt_bot_diag = register_diag_field(package_name, "rho_dzt_bot_diag", axes(1:2), &
         init_time, "The thickness that contributes to the bottom boundary layer calculation", &
         "kg m-2", missing_value = missing_value1)

    cobalt%id_cased_2d = register_diag_field(package_name, "cased_2d", axes(1:2), &
         init_time, "calcium carbonate in sediment", "mol m-3", missing_value = missing_value1)

    cobalt%id_sfc_co3_ion = register_diag_field(package_name, "sfc_co3_ion", axes(1:2), &
         init_time, "Surface Carbonate Ion", "mol kg-1", missing_value = missing_value1)

    cobalt%id_sfc_co3_sol_arag = register_diag_field(package_name, "sfc_co3_sol_arag", axes(1:2), &
         init_time, "Surface Carbonate Ion Solubility for Aragonite", "mol kg-1", &
         missing_value = missing_value1)

    cobalt%id_sfc_co3_sol_calc = register_diag_field(package_name, "sfc_co3_sol_calc", axes(1:2), &
         init_time, "Surface Carbonate Ion Solubility for Calcite ", "mol kg-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_sfc_f_n = register_diag_field(package_name, "sfc_nsmp", axes(1:2), &
         init_time, "Surface small phyto. nitrogen", "mol kg-1", missing_value = missing_value1)

    phyto(MDP)%id_sfc_f_n = register_diag_field(package_name, "sfc_nmdp", axes(1:2), &
         init_time, "Surface medium phyto. nitrogen", "mol kg-1", missing_value = missing_value1)

    phyto(LGP)%id_sfc_f_n = register_diag_field(package_name, "sfc_nlgp", axes(1:2), &
         init_time, "Surface large phyto. nitrogen", "mol kg-1", missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_f_n = register_diag_field(package_name, "sfc_ndi", axes(1:2), &
         init_time, "Surface diazotroph nitrogen", "mol kg-1", missing_value = missing_value1)

    phyto(SMP)%id_sfc_chl = register_diag_field(package_name, "sfc_chl_smp", axes(1:2), &
         init_time, "Surface small phyto. chlorophyll", "ug kg-1", missing_value = missing_value1)

    phyto(MDP)%id_sfc_chl = register_diag_field(package_name, "sfc_chl_mdp", axes(1:2), &
         init_time, "Surface medium phyto. chlorophyll", "ug kg-1", missing_value = missing_value1)

    phyto(LGP)%id_sfc_chl = register_diag_field(package_name, "sfc_chl_lgp", axes(1:2), &
         init_time, "Surface large phyto. chlorophyll", "ug kg-1", missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_chl = register_diag_field(package_name, "sfc_chl_di", axes(1:2), &
         init_time, "Surface diazotroph chlorophyll", "mol kg-1", missing_value = missing_value1)

    phyto(SMP)%id_sfc_def_fe = register_diag_field(package_name, "sfc_def_fe_smp", axes(1:2), &
         init_time, "Surface small phyto. iron deficiency", "dimensionsless", &
         missing_value = missing_value1)

    phyto(MDP)%id_sfc_def_fe = register_diag_field(package_name, "sfc_def_fe_mdp", axes(1:2), &
         init_time, "Surface medium phyto. iron deficiency", "dimensionsless", &
         missing_value = missing_value1)

    phyto(LGP)%id_sfc_def_fe = register_diag_field(package_name, "sfc_def_fe_lgp", axes(1:2), &
         init_time, "Surface large phyto. iron deficiency", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_def_fe = register_diag_field(package_name, "sfc_def_fe_di", axes(1:2), &
         init_time, "Surface diazotroph iron deficiency", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_sfc_felim = register_diag_field(package_name, "sfc_felim_smp", axes(1:2), &
         init_time, "Surface small phyto. iron uptake limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(MDP)%id_sfc_felim = register_diag_field(package_name, "sfc_felim_mdp", axes(1:2), &
         init_time, "Surface medium phyto. iron uptake limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(LGP)%id_sfc_felim = register_diag_field(package_name, "sfc_felim_lgp", axes(1:2), &
         init_time, "Surface large phyto. iron uptake limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_felim = register_diag_field(package_name, "sfc_felim_di", axes(1:2), &
         init_time, "Surface diazotroph iron uptake limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_q_fe_2_n = register_diag_field(package_name, "sfc_q_fe_2_n_di", axes(1:2), &
         init_time, "Surface diazotroph iron:nitrogen", "moles Fe (moles N)-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_sfc_q_fe_2_n = register_diag_field(package_name, "sfc_q_fe_2_n_smp", &
         axes(1:2), &
         init_time, "Surface small phyto. iron:nitrogen", "moles Fe (moles N)-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_sfc_q_fe_2_n = register_diag_field(package_name, "sfc_q_fe_2_n_mdp", &
         axes(1:2), &
         init_time, "Surface medium phyto. iron:nitrogen", "moles Fe (moles N)-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_sfc_q_fe_2_n = register_diag_field(package_name, "sfc_q_fe_2_n_lgp", &
         axes(1:2), &
         init_time, "Surface large phyto. iron:nitrogen", "moles Fe (moles N)-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_q_p_2_n = register_diag_field(package_name, "sfc_q_p_2_n_di", axes(1:2), &
         init_time, "Surface diazotroph P:N", "moles P (moles N)-1", missing_value = missing_value1)

    phyto(SMP)%id_sfc_q_p_2_n = register_diag_field(package_name, "sfc_q_p_2_n_smp", axes(1:2), &
         init_time, "Surface small phyto. P:N", "moles P (moles N)-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_sfc_q_p_2_n = register_diag_field(package_name, "sfc_q_p_2_n_mdp", axes(1:2), &
         init_time, "Surface medium phyto. P:N", "moles P (moles N)-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_sfc_q_p_2_n = register_diag_field(package_name, "sfc_q_p_2_n_lgp", axes(1:2), &
         init_time, "Surface large phyto. P:N", "moles P (moles N)-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_sfc_irrlim = register_diag_field(package_name, "sfc_irrlim_smp", axes(1:2), &
         init_time, "Surface small phyto. light limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(MDP)%id_sfc_irrlim = register_diag_field(package_name, "sfc_irrlim_mdp", axes(1:2), &
         init_time, "Surface medium phyto. light limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(LGP)%id_sfc_irrlim = register_diag_field(package_name, "sfc_irrlim_lgp", axes(1:2), &
         init_time, "Surface large phyto. light limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_irrlim = register_diag_field(package_name, "sfc_irrlim_di", axes(1:2), &
         init_time, "Surface diazotroph light limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_sfc_theta = register_diag_field(package_name, "sfc_theta_smp", axes(1:2), &
         init_time, "Surface small phyto. Chl:C", "g Chl (g C)-1", missing_value = missing_value1)

    phyto(MDP)%id_sfc_theta = register_diag_field(package_name, "sfc_theta_mdp", axes(1:2), &
         init_time, "Surface medium phyto. Chl:C", "g Chl (g C)-1", missing_value = missing_value1)

    phyto(LGP)%id_sfc_theta = register_diag_field(package_name, "sfc_theta_lgp", axes(1:2), &
         init_time, "Surface large phyto. Chl:C", "g Chl (g C)-1", missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_theta = register_diag_field(package_name, "sfc_theta_di", axes(1:2), &
         init_time, "Surface diazotroph Chl:C", "g Chl (g C)-1", missing_value = missing_value1)

    phyto(SMP)%id_sfc_pcmlim_aclm = register_diag_field(package_name, "sfc_pcmlim_aclm_smp", &
         axes(1:2), &
         init_time, "Surface small phyto. Temp*Nut Lim", "none", missing_value = missing_value1)

    phyto(MDP)%id_sfc_pcmlim_aclm = register_diag_field(package_name, "sfc_pcmlim_aclm_mdp", &
         axes(1:2), &
         init_time, "Surface medium phyto. Temp*Nut Lim", "none", missing_value = missing_value1)

    phyto(LGP)%id_sfc_pcmlim_aclm = register_diag_field(package_name, "sfc_pcmlim_aclm_lgp", &
         axes(1:2), &
         init_time, "Surface large phyto. Temp*Nut Lim", "none", missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_pcmlim_aclm = register_diag_field(package_name, "sfc_pcmlim_aclm_di", &
         axes(1:2), &
         init_time, "Surface diazotroph Temp*Nut Lim", "none", missing_value = missing_value1)

    phyto(SMP)%id_sfc_mu = register_diag_field(package_name, "sfc_mu_smp", axes(1:2), &
         init_time, "Surface small phyto. Chl:C", "sec-1", missing_value = missing_value1)

    phyto(MDP)%id_sfc_mu = register_diag_field(package_name, "sfc_mu_mdp", axes(1:2), &
         init_time, "Surface medium phyto. Chl:C", "sec-1", missing_value = missing_value1)

    phyto(LGP)%id_sfc_mu = register_diag_field(package_name, "sfc_mu_lgp", axes(1:2), &
         init_time, "Surface large phyto. Chl:C", "sec-1", missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_mu = register_diag_field(package_name, "sfc_mu_di", axes(1:2), &
         init_time, "Surface diazotroph growth rate", "sec-1", missing_value = missing_value1)

    phyto(SMP)%id_sfc_no3lim = register_diag_field(package_name, "sfc_no3lim_smp", axes(1:2), &
         init_time, "Surface small phyto. nitrate limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(MDP)%id_sfc_no3lim = register_diag_field(package_name, "sfc_no3lim_mdp", axes(1:2), &
         init_time, "Surface medium phyto. nitrate limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(LGP)%id_sfc_no3lim = register_diag_field(package_name, "sfc_no3lim_lgp", axes(1:2), &
         init_time, "Surface large phyto. nitrate limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_no3lim = register_diag_field(package_name, "sfc_no3lim_di", axes(1:2), &
         init_time, "Surface diazotroph nitrate limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_sfc_nh4lim = register_diag_field(package_name, "sfc_nh4lim_smp", axes(1:2), &
         init_time, "Surface small phyto. ammonia limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(MDP)%id_sfc_nh4lim = register_diag_field(package_name, "sfc_nh4lim_mdp", axes(1:2), &
         init_time, "Surface medium phyto. ammonia limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(LGP)%id_sfc_nh4lim = register_diag_field(package_name, "sfc_nh4lim_lgp", axes(1:2), &
         init_time, "Surface large phyto. ammonia limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_nh4lim = register_diag_field(package_name, "sfc_nh4lim_di", axes(1:2), &
         init_time, "Surface diazotroph ammonia limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(SMP)%id_sfc_po4lim = register_diag_field(package_name, "sfc_po4lim_smp", axes(1:2), &
         init_time, "Surface small phyto. phosphate limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(MDP)%id_sfc_po4lim = register_diag_field(package_name, "sfc_po4lim_mdp", axes(1:2), &
         init_time, "Surface medium phyto. phosphate limitation", "dimensionsless", &
         missing_value = missing_value1)

    phyto(LGP)%id_sfc_po4lim = register_diag_field(package_name, "sfc_po4lim_lgp", axes(1:2), &
         init_time, "Surface large phyto. phosphate limitation", "dimensionless", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_sfc_po4lim = register_diag_field(package_name, "sfc_po4lim_di", axes(1:2), &
         init_time, "Surface diazotroph phosphate limitation", "dimensionless", &
         missing_value = missing_value1)

    !
    ! 100m, 200m integrated fluxes
    !

    cobalt%id_jprod_allphytos_100 = register_diag_field(package_name, "jprod_allphytos_100", &
         axes(1:2), &
         init_time, "Total Nitrogen prim. prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_allphytos_200 = register_diag_field(package_name, "jprod_allphytos_200", &
         axes(1:2), &
         init_time, "Total Nitrogen prim. prod. integral in upper 200m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_mld_aclm = register_diag_field(package_name, "mld_aclm", axes(1:2), &
         init_time, "mixed layer for photacclimation", "m", missing_value = missing_value1)

! CAS: Added diagnostic for diatom NPP in top 100m for CMIP
    cobalt%id_jprod_diat_100 = register_diag_field(package_name, "jprod_diat_100", axes(1:2), &
         init_time, "Diatom prim. prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jprod_n_100 = register_diag_field(package_name, "jprod_ndi_100", axes(1:2), &
         init_time, "Diazotroph nitrogen prim. prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jprod_n_100 = register_diag_field(package_name, "jprod_nsmp_100", axes(1:2), &
         init_time, "Small phyto. nitrogen  prim. prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jprod_n_100 = register_diag_field(package_name, "jprod_nmdp_100", axes(1:2), &
         init_time, "Medium phyto. nitrogen  prim. prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jprod_n_100 = register_diag_field(package_name, "jprod_nlgp_100", axes(1:2), &
         init_time, "Large phyto. nitrogen  prim. prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jprod_n_new_100 = register_diag_field(package_name, "jprod_ndi_new_100", &
         axes(1:2), &
         init_time, "Diazotroph new (NO3-based) prim. prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(SMP)%id_jprod_n_new_100 = register_diag_field(package_name, "jprod_nsmp_new_100", &
         axes(1:2), &
         init_time, "Small phyto. new (NO3-based) prim. prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(MDP)%id_jprod_n_new_100 = register_diag_field(package_name, "jprod_nmdp_new_100", &
         axes(1:2), &
         init_time, "Medium phyto. new (NO3-based) prim. prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(LGP)%id_jprod_n_new_100 = register_diag_field(package_name, "jprod_nlgp_new_100", &
         axes(1:2), &
         init_time, "Large phyto. new (NO3-based) prim. prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_jprod_n_n2_100 = register_diag_field(package_name, "jprod_ndi_n2_100", &
         axes(1:2), &
         init_time, "Diazotroph nitrogen fixation in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jzloss_n_100 = register_diag_field(package_name, "jzloss_ndi_100", axes(1:2), &
         init_time, "Diazotroph nitrogen loss to zooplankton integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(SMP)%id_jzloss_n_100 = register_diag_field(package_name, "jzloss_nsmp_100", axes(1:2), &
         init_time, "Small phyto. nitrogen loss to zooplankton integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(MDP)%id_jzloss_n_100 = register_diag_field(package_name, "jzloss_nmdp_100", &
         axes(1:2), &
         init_time, "Medium phyto. nitrogen loss to zooplankton integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(LGP)%id_jzloss_n_100 = register_diag_field(package_name, "jzloss_nlgp_100", axes(1:2), &
         init_time, "Large phyto. nitrogen loss to zooplankton integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_jaggloss_n_100 = register_diag_field(package_name, "jaggloss_ndi_100", &
         axes(1:2), &
         init_time, "Diazotroph phyto. nitrogen aggregation loss integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(SMP)%id_jaggloss_n_100 = register_diag_field(package_name, "jaggloss_nsmp_100", &
         axes(1:2), &
         init_time, "Small phyto. nitrogen aggregation loss integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(MDP)%id_jaggloss_n_100 = register_diag_field(package_name, "jaggloss_nmdp_100", &
         axes(1:2), &
         init_time, "Medium phyto. nitrogen aggregation loss integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(LGP)%id_jaggloss_n_100 = register_diag_field(package_name, "jaggloss_nlgp_100", &
         axes(1:2), &
         init_time, "Large phyto. nitrogen aggregation loss integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    phyto(DIAZ)%id_jvirloss_n_100 = register_diag_field(package_name, "jvirloss_ndi_100", &
         axes(1:2), &
         init_time, "Diazotroph nitrogen virus loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jvirloss_n_100 = register_diag_field(package_name, "jvirloss_nsmp_100", &
         axes(1:2), &
         init_time, "Small phyto. nitrogen virus loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jvirloss_n_100 = register_diag_field(package_name, "jvirloss_nmdp_100", &
         axes(1:2), &
         init_time, "Medium phyto. nitrogen virus loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jvirloss_n_100 = register_diag_field(package_name, "jvirloss_nlgp_100", &
         axes(1:2), &
         init_time, "Large phyto. nitrogen virus loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jmortloss_n_100 = register_diag_field(package_name, "jmortloss_ndi_100", &
         axes(1:2), &
         init_time, "Diazotroph nitrogen mortality loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jmortloss_n_100 = register_diag_field(package_name, "jmortloss_nsmp_100", &
         axes(1:2), &
         init_time, "Small phyto. nitrogen mortality loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jmortloss_n_100 = register_diag_field(package_name, "jmortloss_nmdp_100", &
         axes(1:2), &
         init_time, "Medium phyto. nitrogen mortality loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jmortloss_n_100 = register_diag_field(package_name, "jmortloss_nlgp_100", &
         axes(1:2), &
         init_time, "Large phyto. nitrogen mortality loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_jexuloss_n_100 = register_diag_field(package_name, "jexuloss_ndi_100", &
         axes(1:2), &
         init_time, "Diazotroph nitrogen exudation loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(SMP)%id_jexuloss_n_100 = register_diag_field(package_name, "jexuloss_nsmp_100", &
         axes(1:2), &
         init_time, "Small phyto. nitrogen exudation loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(MDP)%id_jexuloss_n_100 = register_diag_field(package_name, "jexuloss_nmdp_100", &
         axes(1:2), &
         init_time, "Medium phyto. nitrogen exudation loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    phyto(LGP)%id_jexuloss_n_100 = register_diag_field(package_name, "jexuloss_nlgp_100", &
         axes(1:2), &
         init_time, "Large phyto. nitrogen exudation loss integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jprod_n_100 = register_diag_field(package_name, "jprod_nsmz_100", axes(1:2), &
         init_time, "Small zooplankton nitrogen prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jprod_n_100 = register_diag_field(package_name, "jprod_nmdz_100", axes(1:2), &
         init_time, "Medium zooplankton nitrogen prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jprod_n_100 = register_diag_field(package_name, "jprod_nlgz_100", axes(1:2), &
         init_time, "Large zooplankton nitrogen prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jingest_n_100 = register_diag_field(package_name, "jingest_n_nsmz_100", axes(1:2), &
         init_time, "Small zooplankton nitrogen ingestion integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    zoo(MDZ)%id_jingest_n_100 = register_diag_field(package_name, "jingest_n_nmdz_100", axes(1:2), &
         init_time, "Medium zooplankton nitrogen ingestion integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    zoo(LGZ)%id_jingest_n_100 = register_diag_field(package_name, "jingest_n_nlgz_100", axes(1:2), &
         init_time, "Large zooplankton nitrogen ingestion integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    zoo(SMZ)%id_jzloss_n_100 = register_diag_field(package_name, "jzloss_nsmz_100", axes(1:2), &
         init_time, "Small zooplankton nitrogen loss to zooplankton integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jzloss_n_100 = register_diag_field(package_name, "jzloss_nmdz_100", axes(1:2), &
         init_time, "Medium zooplankton nitrogen loss to zooplankton integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jhploss_n_100 = register_diag_field(package_name, "jhploss_nmdz_100", axes(1:2), &
         init_time, "Medium zooplankton nitrogen loss to higher preds. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jhploss_n_100 = register_diag_field(package_name, "jhploss_nlgz_100", axes(1:2), &
         init_time, "Large zooplankton nitrogen loss to higher preds. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jprod_ndet_100 = register_diag_field(package_name, "jprod_ndet_nmdz_100", axes(1:2), &
         init_time, "Medium zooplankton nitrogen detritus prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jprod_ndet_100 = register_diag_field(package_name, "jprod_ndet_nlgz_100", axes(1:2), &
         init_time, "Large zooplankton nitrogen detritus prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(SMZ)%id_jprod_don_100 = register_diag_field(package_name, "jprod_don_nsmz_100", axes(1:2), &
         init_time, "Small zooplankton dissolved org. nitrogen prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jprod_don_100 = register_diag_field(package_name, "jprod_don_nmdz_100", axes(1:2), &
         init_time, "Medium zooplankton dissolved org. nitrogen prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(SMZ)%id_jremin_n_100 = register_diag_field(package_name, "jremin_n_nsmz_100", axes(1:2), &
         init_time, "Small zooplankton nitrogen remineralization integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(MDZ)%id_jremin_n_100 = register_diag_field(package_name, "jremin_n_nmdz_100", axes(1:2), &
         init_time, "Medium zooplankton nitrogen remineralization integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    zoo(LGZ)%id_jremin_n_100 = register_diag_field(package_name, "jremin_n_nlgz_100", axes(1:2), &
         init_time, "Large zooplankton nitrogen remineralization integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_hp_jremin_n_100 = register_diag_field(package_name, "jremin_n_hp_100", axes(1:2), &
         init_time, "Higher predator nitrogen remineralization integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_hp_jingest_n_100 = register_diag_field(package_name, "jingest_n_hp_100", axes(1:2), &
         init_time, "Higher predator ingestion of nitrogen integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_hp_jprod_ndet_100 = register_diag_field(package_name, "jprod_ndet_hp_100", &
         axes(1:2), &
         init_time, "Higher predator nitrogen detritus prod. integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    bact(1)%id_jprod_n_100 = register_diag_field(package_name, "jprod_nbact_100", axes(1:2), &
         init_time, "Bacteria nitrogen prod. integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    bact(1)%id_jzloss_n_100 = register_diag_field(package_name, "jzloss_nbact_100", axes(1:2), &
         init_time, "Bacteria nitrogen loss to zooplankton integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    bact(1)%id_jvirloss_n_100 = register_diag_field(package_name, "jvirloss_nbact_100", axes(1:2), &
         init_time, "Bacteria nitrogen loss to viruses integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    bact(1)%id_jremin_n_100 = register_diag_field(package_name, "jremin_n_nbact_100", axes(1:2), &
         init_time, "Bacteria nitrogen remineralization integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    bact(1)%id_juptake_ldon_100 = register_diag_field(package_name, "juptake_ldon_nbact_100", &
         axes(1:2), &
         init_time, "Bacterial uptake of labile dissolved org. nitrogen in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_jprod_lithdet_100 = register_diag_field(package_name, "jprod_lithdet_100", &
         axes(1:2), &
         init_time, "Lithogenic detritus production integral in upper 100m", "g m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_sidet_100 = register_diag_field(package_name, "jprod_sidet_100", axes(1:2), &
         init_time, "Silica detritus production integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_cadet_calc_100 = register_diag_field(package_name, "jprod_cadet_calc_100", &
         axes(1:2), &
         init_time, "Calcite detritus production integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jprod_cadet_arag_100 = register_diag_field(package_name, "jprod_cadet_arag_100", &
         axes(1:2), &
         init_time, "Aragonite detritus production integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    ! << Add neritic CaCO3 burial
    cobalt%id_jdic_caco3_nerbur_150 = register_diag_field(package_name, "jdic_caco3_nerbur_150", &
         axes(1:2), &
         init_time, "Neritic CaCO3 burial integral in upper 150m", "mol m-2 s-1", &
         missing_value = missing_value1)
    ! >>

    cobalt%id_jremin_ndet_100 = register_diag_field(package_name, "jremin_ndet_100", axes(1:2), &
         init_time, "Remineralization of nitrogen detritus integral in upper 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jremin_ndet_fast_100 = register_diag_field(package_name, "jremin_ndet_fast_100", &
         axes(1:2), &
         init_time, "Remineralization of fast-sinking nitrogen detritus integral in upper 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_jprod_mesozoo_200 = register_diag_field(package_name, "jprod_mesozoo_200", &
         axes(1:2), &
         init_time, "Mesozooplankton Production, 200m integration", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jalk_100 = register_diag_field(package_name, "jalk_100", axes(1:2), &
         init_time, "integrated alkalinity tendency in the top 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jdic_100 = register_diag_field(package_name, "jdic_100", axes(1:2), &
         init_time, "integrated dissolved organic carbon tendency in the top 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jdin_100 = register_diag_field(package_name, "jdin_100", axes(1:2), &
         init_time, "integrated dissolved inorganic nitrogen tendency in the top 100m", &
         "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_jfed_100 = register_diag_field(package_name, "jfed_100", axes(1:2), &
         init_time, "integrated dissolved iron tendency in the top 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jpo4_100 = register_diag_field(package_name, "jpo4_100", axes(1:2), &
         init_time, "integrated phosphate tendency in the top 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_jsio4_100 = register_diag_field(package_name, "jsio4_100", axes(1:2), &
         init_time, "integrated silicate tendency in the top 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_daylength = register_diag_field(package_name, "daylength", axes(1:2), &
         init_time, "daylength", "hours", missing_value = missing_value1)

    !
    ! Water column integrated fluxes
    !

    cobalt%id_wc_vert_int_npp = register_diag_field(package_name, "wc_vert_int_npp", axes(1:2), &
         init_time, "Water column net primary production vertical integral", "mol N m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jdiss_sidet = register_diag_field(package_name, &
         "wc_vert_int_jdiss_sidet", axes(1:2), &
         init_time, "Water column silica dissolution vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jdiss_cadet = register_diag_field(package_name, &
         "wc_vert_int_jdiss_cadet", axes(1:2), &
         init_time, "Water column calcium carbonate dissolution vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jo2resp = register_diag_field(package_name, "wc_vert_int_jo2resp", &
         axes(1:2), &
         init_time, "Water column oxygen respired vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jprod_cadet = register_diag_field(package_name, &
         "wc_vert_int_jprod_cadet", axes(1:2), &
         init_time, "Water column calcium carbonate production vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jno3denit = register_diag_field(package_name, "wc_vert_int_jno3denit", &
         axes(1:2), &
         init_time, "Water column denitrification vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jprod_no3nitrif = register_diag_field(package_name, &
         "wc_vert_int_jprod_no3nitrif", axes(1:2), &
         init_time, "Water column nitrification vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jnamx = register_diag_field(package_name, "wc_vert_int_jnamx", &
         axes(1:2), &
         init_time, "Water column N loss from anammox vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_juptake_nh4 = register_diag_field(package_name, &
         "wc_vert_int_juptake_nh4", axes(1:2), &
         init_time, " Water column ammonia based NPP vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jprod_nh4 = register_diag_field(package_name, "wc_vert_int_jprod_nh4", &
         axes(1:2), &
         init_time, " Water column ammonia production vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_juptake_no3 = register_diag_field(package_name, &
         "wc_vert_int_juptake_no3", axes(1:2), &
         init_time, "Water column nitrate based NPP, vertical integral", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_nfix = register_diag_field(package_name, "wc_vert_int_nfix", axes(1:2), &
         init_time, "Nitrogen fixation vertical integral", "mol N m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jfe_iceberg = register_diag_field(package_name, &
         "wc_vert_int_jfe_iceberg", axes(1:2), &
         init_time, "iceberg dissolved iron, vertical integral", "mol Fe m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jno3_iceberg = register_diag_field(package_name, &
         "wc_vert_int_jno3_iceberg", axes(1:2), &
         init_time, "iceberg nitrate, vertical integral", "mol N m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_jpo4_iceberg = register_diag_field(package_name, &
         "wc_vert_int_jpo4_iceberg", axes(1:2), &
         init_time, "iceberg phosphate, vertical integral", "mol P m-2 s-1", &
         missing_value = missing_value1)

    !
    ! 100m integrated biomass
    !

    cobalt%id_f_alk_int_100 = register_diag_field(package_name, "f_alk_int_100", axes(1:2), &
         init_time, "integrated alkalinity in the top 100m", "mol equiv. m-2", &
         missing_value = missing_value1)

    cobalt%id_f_dic_int_100 = register_diag_field(package_name, "f_dic_int_100", axes(1:2), &
         init_time, "integrated DIC in the top 100m", "mol m-2", missing_value = missing_value1)

    cobalt%id_f_din_int_100 = register_diag_field(package_name, "f_din_int_100", axes(1:2), &
         init_time, "integrated DIN in the top 100m", "mol m-2", missing_value = missing_value1)

    cobalt%id_f_fed_int_100 = register_diag_field(package_name, "f_fed_int_100", axes(1:2), &
         init_time, "integrated dissolved iron in the top 100m", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_f_po4_int_100 = register_diag_field(package_name, "f_po4_int_100", axes(1:2), &
         init_time, "integrated phosphate in the top 100m", "mol m-2", &
         missing_value = missing_value1)

     cobalt%id_f_sio4_int_100 = register_diag_field(package_name, "f_sio4_int_100", axes(1:2), &
         init_time, "integrated silicate in the top 100m", "mol m-2", missing_value = missing_value1)

    phyto(SMP)%id_f_n_100 = register_diag_field(package_name, "nsmp_100", axes(1:2), &
         init_time, "Small phytoplankton nitrogen biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    phyto(MDP)%id_f_n_100 = register_diag_field(package_name, "nmdp_100", axes(1:2), &
         init_time, "Medium phytoplankton nitrogen biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    phyto(LGP)%id_f_n_100 = register_diag_field(package_name, "nlgp_100", axes(1:2), &
         init_time, "Large phytoplankton nitrogen biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    phyto(DIAZ)%id_f_n_100 = register_diag_field(package_name, "ndi_100", axes(1:2), &
         init_time, "Diazotroph nitrogen biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    zoo(SMZ)%id_f_n_100 = register_diag_field(package_name, "nsmz_100", axes(1:2), &
         init_time, "Small zooplankton nitrogen biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    zoo(MDZ)%id_f_n_100 = register_diag_field(package_name, "nmdz_100", axes(1:2), &
         init_time, "Medium zooplankton nitrogen biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    zoo(LGZ)%id_f_n_100 = register_diag_field(package_name, "nlgz_100", axes(1:2), &
         init_time, "Large zooplankton nitrogen biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    bact(1)%id_f_n_100 = register_diag_field(package_name, "nbact_100", axes(1:2), &
         init_time, "Bacterial nitrogen biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_f_simd_100 = register_diag_field(package_name, "simdp_100", axes(1:2), &
         init_time, "Medium phytoplankton silicon biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_f_silg_100 = register_diag_field(package_name, "silgp_100", axes(1:2), &
         init_time, "Large phytoplankton silicon biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_f_ndet_100 = register_diag_field(package_name, "ndet_100", axes(1:2), &
         init_time, "Nitrogen detritus biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_f_ndet_fast_100 = register_diag_field(package_name, "ndet_fast_100", axes(1:2), &
         init_time, "Fast-sinking nitrogen detritus biomass in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_f_don_100 = register_diag_field(package_name, "don_100", axes(1:2), &
         init_time, "Dissolved organic nitrogen (sr+sl+l) in upper 100m", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_f_mesozoo_200 = register_diag_field(package_name, "mesozoo_200", axes(1:2), &
         init_time, "Mesozooplankton biomass as nitrogen, 200m integral", "mol N m-2", &
         missing_value = missing_value1)

    !
    ! Water column integrated tracers
    !

    cobalt%id_wc_vert_int_c = register_diag_field(package_name, "wc_vert_int_c", axes(1:2), &
         init_time, "Total carbon (DIC+OC+IC) vertical integral", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_dic = register_diag_field(package_name, "wc_vert_int_dic", axes(1:2), &
         init_time, "Dissolved inorganic carbon vertical integral", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_doc = register_diag_field(package_name, "wc_vert_int_doc", axes(1:2), &
         init_time, "Total dissolved organic carbon  vertical integral", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_poc = register_diag_field(package_name, "wc_vert_int_poc", axes(1:2), &
         init_time, "Total particulate organic carbon vertical integral", "mol m-2", &
         missing_value = missing_value1)

    cobalt%id_wc_vert_int_n = register_diag_field(package_name, "wc_vert_int_n", axes(1:2), &
         init_time, "Total nitrogen vertical integral", "mol m-2", missing_value = missing_value1)

    cobalt%id_wc_vert_int_p = register_diag_field(package_name, "wc_vert_int_p", axes(1:2), &
         init_time, "Total phosphorus vertical integral", "mol m-2", missing_value = missing_value1)

    cobalt%id_wc_vert_int_fe = register_diag_field(package_name, "wc_vert_int_fe", axes(1:2), &
         init_time, "Total iron vertical integral", "mol m-2", missing_value = missing_value1)

    cobalt%id_wc_vert_int_si = register_diag_field(package_name, "wc_vert_int_si", axes(1:2), &
         init_time, "Total silicon vertical integral", "mol m-2", missing_value = missing_value1)

    cobalt%id_wc_vert_int_o2 = register_diag_field(package_name, "wc_vert_int_o2", axes(1:2), &
         init_time, "Total oxygen vertical integral", "mol m-2", missing_value = missing_value1)

    cobalt%id_wc_vert_int_alk = register_diag_field(package_name, "wc_vert_int_alk", axes(1:2), &
         init_time, "Total alkalinity vertical integral", "mol m-2", missing_value = missing_value1)

    !
    ! sinking flux = 100m
    !

    cobalt%id_fndet_100 = register_diag_field(package_name, "fndet_100", axes(1:2), &
         init_time, "Nitrogen detritus sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fndet_fast_100 = register_diag_field(package_name, "fndet_fast_100", axes(1:2), &
         init_time, "Fast-sinking nitrogen detritus flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fpdet_100 = register_diag_field(package_name, "fpdet_100", axes(1:2), &
         init_time, "Phosphorous detritus sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fpdet_fast_100 = register_diag_field(package_name, "fpdet_fast_100", axes(1:2), &
         init_time, "Fast-sinking phosphorous detritus flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_ffedet_100 = register_diag_field(package_name, "ffedet_100", axes(1:2), &
         init_time, "Iron detritus sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fsidet_100 = register_diag_field(package_name, "fsidet_100", axes(1:2), &
         init_time, "Silicon detritus sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fcadet_calc_100 = register_diag_field(package_name, "fcadet_calc_100", axes(1:2), &
         init_time, "Calcite detritus sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fcadet_arag_100 = register_diag_field(package_name, "fcadet_arag_100", axes(1:2), &
         init_time, "Aragonite detritus sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_flithdet_100 = register_diag_field(package_name, "flithdet_100", axes(1:2), &
         init_time, "Lithogenic detritus sinking flux @ 100m", "g m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fntot_100 = register_diag_field(package_name, "fntot_100", axes(1:2), &
         init_time, "total nitrogen sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_fptot_100 = register_diag_field(package_name, "fptot_100", axes(1:2), &
         init_time, "total phosphorous sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    cobalt%id_ffetot_100 = register_diag_field(package_name, "ffetot_100", axes(1:2), &
         init_time, "total iron sinking flux @ 100m", "mol m-2 s-1", missing_value = missing_value1)

    cobalt%id_fsitot_100 = register_diag_field(package_name, "fsitot_100", axes(1:2), &
         init_time, "total silicon sinking flux @ 100m", "mol m-2 s-1", &
         missing_value = missing_value1)

    ! Oxygen minima (value and location
    !

    cobalt%id_o2min = register_diag_field(package_name, "o2min", axes(1:2), &
         init_time, "Minimum Oxygen", "mol kg-1", missing_value = missing_value1)


      if (do_14c) then                                        !<<RADIOCARBON
    cobalt%id_b_di14c = register_diag_field(package_name, "b_di14c", axes(1:2), &
         init_time, "Bottom flux of DI14C into sediment", "mol m-2 s-1", &
         missing_value = missing_value1)
    cobalt%id_c14_2_n = register_diag_field(package_name, "c14_2_n", axes(1:3), &
         init_time, "Ratio of DI14C to N-nutrients", "mol kg-1", missing_value = missing_value1)
    cobalt%id_c14o2_alpha = register_diag_field(package_name, "c14o2_alpha", axes(1:2), &
         init_time, "Saturation surface 14CO2* per uatm", "mol kg-1 atm-1", &
         missing_value = missing_value1)
    cobalt%id_c14o2_csurf = register_diag_field(package_name, "c14o2_csurf", axes(1:2), &
         init_time, "14CO2* concentration at surface", "mol kg-1", missing_value = missing_value1)
    cobalt%id_fpo14c = register_diag_field(package_name, "fpo14c", axes(1:3), &
         init_time, "PO14C sinking flux at layer bottom", "mol m-2 s-1", &
         missing_value = missing_value1)
    cobalt%id_j14c_decay_dic = register_diag_field(package_name, "j14c_decay_dic", axes(1:3), &
         init_time, "DI14C radioactive decay", "mol kg-1 s-1", missing_value = missing_value1)
    cobalt%id_j14c_decay_doc = register_diag_field(package_name, "j14c_decay_doc", axes(1:3), &
         init_time, "DO14C radioactive decay", "mol kg-1 s-1", missing_value = missing_value1)
    cobalt%id_j14c_reminp = register_diag_field(package_name, "j14c_reminp", axes(1:3), &
         init_time, "Sinking PO14C remineralization", "mol kg-1 s-1", missing_value = missing_value1)
    cobalt%id_jdi14c = register_diag_field(package_name, "jdi14c", axes(1:3), &
         init_time, "DI14C source", "mol kg-1 s-1", missing_value = missing_value1)
    cobalt%id_jdo14c = register_diag_field(package_name, "jdo14c", axes(1:3), &
         init_time, "DO14C source", "mol kg-1 s-1", missing_value = missing_value1)
      endif                                                   !RADIOCARBON>>


    !
    ! Additional diagnostics added for debugging jgj 2015/10/26
    !

    cobalt%id_jalk = register_diag_field(package_name, "jalk", axes(1:3), &
         init_time, "Alkalinity source", "mole eq kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jalkh = register_diag_field(package_name, "jalkh", axes(1:3), &
         init_time, "Alkalinity content source (jalk * thickness)", "mole eq kg-1 m s-1", &
         missing_value = missing_value1)

    cobalt%id_jalk_plus_btm = register_diag_field(package_name, "jalk_plus_btm", axes(1:3), &
         init_time, "Alkalinity source plus btm", "mole eq kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jdic = register_diag_field(package_name, "jdic", axes(1:3), &
         init_time, "Dissolved Inorganic Carbon source", "mol kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jdich = register_diag_field(package_name, "jdich", axes(1:3), &
         init_time, "Dissolved Inorganic Carbon content source (jdic * thickness)", &
         "mol kg-1 m s-1", missing_value = missing_value1)

    cobalt%id_jno3 = register_diag_field(package_name, "jno3", axes(1:3), &
         init_time, "no3 source", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jno3h = register_diag_field(package_name, "jno3h", axes(1:3), &
         init_time, "no3 content source (jno3 * thickness)", "mol kg-1 m s-1", &
         missing_value = missing_value1)

    cobalt%id_jpo4 = register_diag_field(package_name, "jpo4", axes(1:3), &
         init_time, "po4 source", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jpo4h = register_diag_field(package_name, "jpo4h", axes(1:3), &
         init_time, "po4 content source (jpo4 * thickness)", "mol kg-1 m s-1", &
         missing_value = missing_value1)

    cobalt%id_jsio4 = register_diag_field(package_name, "jsio4", axes(1:3), &
         init_time, "sio4 source", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jsio4h = register_diag_field(package_name, "jsio4h", axes(1:3), &
         init_time, "sio4 content source (jsio4 * thickness)", "mol kg-1 m s-1", &
         missing_value = missing_value1)

    cobalt%id_jdic_plus_btm = register_diag_field(package_name, "jdic_plus_btm", axes(1:3), &
         init_time, "Dissolved Inorganic Carbon source plus btm", "mol kg-1 s-1", &
         missing_value = missing_value1)

    cobalt%id_jnh4 = register_diag_field(package_name, "jnh4", axes(1:3), &
         init_time, "NH4 source", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jnh4h = register_diag_field(package_name, "jnh4h", axes(1:3), &
         init_time, "NH4 content source (jnh4 * thickness)", "mol kg-1 m s-1", &
         missing_value = missing_value1)

    cobalt%id_jndet = register_diag_field(package_name, "jndet", axes(1:3), &
         init_time, "NDET source", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jndeth = register_diag_field(package_name, "jndeth", axes(1:3), &
         init_time, "NDET content source (jndet * thickness)", "mol kg-1 m s-1", &
         missing_value = missing_value1)

    cobalt%id_jnh4_plus_btm = register_diag_field(package_name, "jnh4_plus_btm", axes(1:3), &
         init_time, "NH4 source plus btm", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jo2_plus_btm = register_diag_field(package_name, "jo2_plus_btm", axes(1:3), &
         init_time, "O2 source plus btm", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jo2 = register_diag_field(package_name, "jo2", axes(1:3), &
         init_time, "O2 source", "mol kg-1 s-1", missing_value = missing_value1)

    cobalt%id_jo2h = register_diag_field(package_name, "jo2h", axes(1:3), &
         init_time, "O2 content source (jo2 * thickness)", "mol kg-1 m s-1", &
         missing_value = missing_value1)


!==============================================================================================================
! 2016/07/05 jgj register and send temperature as a test

    cobalt%id_thetao = register_diag_field(package_name, "temp", axes(1:3), &
         init_time, "Potential Temperature", "Celsius", missing_value = missing_value1, &
         cmor_field_name="thetao", cmor_units="C", &
         cmor_standard_name="sea_water_potential_temperature", &
         cmor_long_name ="Sea Water Potential Temperature")

!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem Oyr/Omon/day: Marine Biogeochemical Fields

    cobalt%id_dissic = register_diag_field(package_name, "dissic_raw", axes(1:3), &
         init_time, "Dissolved Inorganic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissic", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_carbon_in_sea_water", &
         cmor_long_name="Dissolved Inorganic Carbon Concentration")

    cobalt%id_dissicnat = register_diag_field(package_name, "dissicnat_raw", axes(1:3), &
         init_time, "Natural Dissolved Inorganic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissicnat", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_carbon_natural_analogue_in_sea_water", &
         cmor_long_name="Natural Dissolved Inorganic Carbon Concentration")

    cobalt%id_dissicabio = register_diag_field(package_name, "dissicabio_raw", axes(1:3), &
         init_time, "Abiotic Dissolved Inorganic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissicabio", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_carbon_abiotic_analogue_in_sea_water", &
         cmor_long_name="Abiotic Dissolved Inorganic Carbon Concentration")

    cobalt%id_dissi14cabio = register_diag_field(package_name, "dissi14cabio_raw", axes(1:3), &
         init_time, "Abiotic Dissolved Inorganic 14Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissi14cabio", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_carbon14_abiotic_analogue_in_sea_water", &
         cmor_long_name="Abiotic Dissolved Inorganic 14Carbon Concentration")

    cobalt%id_dissoc = register_diag_field(package_name, "dissoc_raw", axes(1:3), &
         init_time, "Dissolved Organic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissoc", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_organic_carbon_in_sea_water", &
         cmor_long_name="Dissolved Organic Carbon Concentration")

    cobalt%id_phyc = register_diag_field(package_name, "phyc_raw", axes(1:3), &
         init_time, "Phytoplankton Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="phyc", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Phytoplankton Carbon Concentration")

    cobalt%id_zooc = register_diag_field(package_name, "zooc_raw", axes(1:3), &
         init_time, "Zooplankton Carbon Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="zooc", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_zooplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Zooplankton Carbon Concentration")

    cobalt%id_bacc = register_diag_field(package_name, "bacc_raw", axes(1:3), &
         init_time, "Bacterial Carbon Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="bacc", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_bacteria_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Bacterial Carbon Concentration")

    cobalt%id_detoc = register_diag_field(package_name, "detoc_raw", axes(1:3), &
         init_time, "Detrital Organic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="detoc", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_organic_detritus_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Detrital Organic Carbon Concentration")

    cobalt%id_calc = register_diag_field(package_name, "calc_raw", axes(1:3), &
         init_time, "Calcite Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="calc", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_calcite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Calcite Concentration")

    ! Note that COBALT only tracks aragonite detritus
    cobalt%id_arag = register_diag_field(package_name, "arag_raw", axes(1:3), &
         init_time, "Aragonite Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="arag", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_aragonite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Aragonite Concentration")

    cobalt%id_phydiat = register_diag_field(package_name, "phydiat_raw", axes(1:3), &
         init_time, "Mole Concentration of Diatoms expressed as Carbon in Sea Water", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="phydiat", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_diatoms_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Mole Concentration of Diatoms expressed as Carbon in Sea Water")

    cobalt%id_phydiaz = register_diag_field(package_name, "phydiaz_raw", axes(1:3), &
         init_time, "Mole Concentration of Diazotrophs expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phydiaz", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_diazotrophs_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Mole Concentration of Diazotrophs expressed as Carbon in Sea Water")

    cobalt%id_phypico = register_diag_field(package_name, "phypico_raw", axes(1:3), &
         init_time, "Mole Concentration of Picophytoplankton expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phypico", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_picophytoplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Mole Concentration of Picophytoplankton expressed as Carbon in Sea Water")

    cobalt%id_phymisc = register_diag_field(package_name, "phymisc_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Miscellaneous Phytoplankton expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phymisc", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_miscellaneous_phytoplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Mole Concentration of Miscellaneous Phytoplankton expressed as Carbon in Sea Water")

    cobalt%id_zmicro = register_diag_field(package_name, "zmicro_raw", axes(1:3), &
         init_time, "Mole Concentration of Microzooplankton expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="zmicro", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_microzooplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Mole Concentration of Microzooplankton expressed as Carbon in Sea Water")

    cobalt%id_zmeso = register_diag_field(package_name, "zmeso_raw", axes(1:3), &
         init_time, "Mole Concentration of Mesozooplankton expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="zmeso", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_mesozooplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Mole Concentration of Mesozooplankton expressed as Carbon in Sea Water")

    cobalt%id_talk = register_diag_field(package_name, "talk_raw", axes(1:3), &
         init_time, "Total Alkalinity", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="talk", cmor_units="mol m-3", &
         cmor_standard_name="sea_water_alkalinity_expressed_as_mole_equivalent", &
         cmor_long_name="Total Alkalinity")

    cobalt%id_talknat = register_diag_field(package_name, "talknat_raw", axes(1:3), &
         init_time, "Natural Total Alkalinity", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="talknat", cmor_units="mol m-3", &
         cmor_standard_name="sea_water_alkalinity_natural_analogue_expressed_as_mole_equivalent", &
         cmor_long_name="Natural Total Alkalinity")

    cobalt%id_ph = register_diag_field(package_name, "ph_raw", axes(1:3), &
         init_time, "pH", "1", missing_value = missing_value1, cmor_field_name="ph", &
         cmor_units="1", cmor_standard_name="sea_water_ph_reported_on_total_scale", &
         cmor_long_name="pH")

    cobalt%id_phnat = register_diag_field(package_name, "phnat_raw", axes(1:3), &
         init_time, "Natural pH", "1", missing_value = missing_value1, cmor_field_name="phnat", &
         cmor_units="1", &
         cmor_standard_name="sea_water_ph_natural_analogue_reported_on_total_scale", &
         cmor_long_name="Natural pH")

    cobalt%id_phabio = register_diag_field(package_name, "phabio_raw", axes(1:3), &
         init_time, "Abiotic pH", "1", missing_value = missing_value1, cmor_field_name="phabio", &
         cmor_units="1", &
         cmor_standard_name="sea_water_ph_abiotic_analogue_reported_on_total_scale", &
         cmor_long_name="Abiotic pH")

    ! Same name in model and CMOR, but different units - use "_cmip" to differentiate
    cobalt%id_o2_cmip = register_diag_field(package_name, "o2_raw", axes(1:3), &
         init_time, "Dissolved Oxygen Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="o2_cmip", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_molecular_oxygen_in_sea_water", &
         cmor_long_name="Dissolved Oxygen Concentration")

    cobalt%id_o2sat = register_diag_field(package_name, "o2sat_raw", axes(1:3), &
         init_time, "Dissolved Oxygen Concentration at Saturation", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="o2sat", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_molecular_oxygen_in_sea_water_at_saturation", &
         cmor_long_name="Dissolved Oxygen Concentration at Saturation")

    ! Same name in model and CMOR, but different units - use "_cmip" to differentiate
    cobalt%id_no3_cmip = register_diag_field(package_name, "no3_raw", axes(1:3), &
         init_time, "Dissolved Nitrate Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="no3_cmip", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_nitrate_in_sea_water", &
         cmor_long_name="Dissolved Nitrate Concentration")

!! same name in model and CMOR, but different units - use for now
    cobalt%id_nh4_cmip = register_diag_field(package_name, "nh4_raw", axes(1:3), &
         init_time, "Dissolved Ammonium Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="nh4_cmip", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_ammonium_in_sea_water", &
         cmor_long_name="Dissolved Ammonium Concentration")

!! same name in model and CMOR, but different units - use _cmip for now
    cobalt%id_po4_cmip = register_diag_field(package_name, "po4_raw", axes(1:3), &
         init_time, "Total Dissolved Inorganic Phosphorus Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="po4_cmip", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_phosphorus_in_sea_water", &
         cmor_long_name="Total Dissolved Inorganic Phosphorus Concentration")

    cobalt%id_dfe = register_diag_field(package_name, "dfe_raw", axes(1:3), &
         init_time, "Dissolved Iron Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="dfe", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_iron_in_sea_water", &
         cmor_long_name="Dissolved Iron Concentration")

    cobalt%id_si = register_diag_field(package_name, "si_raw", axes(1:3), &
         init_time, "Total Dissolved Inorganic Silicon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="si", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_silicon_in_sea_water", &
         cmor_long_name="Total Dissolved Inorganic Silicon Concentration")

!! same name in model and CMOR, but different units - use _cmip for now
    cobalt%id_chl_cmip = register_diag_field(package_name, "chl_raw", axes(1:3), &
         init_time, &
         "Mass Concentration of Total Phytoplankton expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chl_cmip", &
         cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_phytoplankton_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Mass Concentration of Total Phytoplankton expressed as Chlorophyll in Sea Water")

    cobalt%id_chldiat = register_diag_field(package_name, "chldiat_raw", axes(1:3), &
         init_time, "Mass Concentration of Diatoms expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chldiat", cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_diatoms_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Mass Concentration of Diatoms expressed as Chlorophyll in Sea Water")

    cobalt%id_chldiaz = register_diag_field(package_name, "chldiaz_raw", axes(1:3), &
         init_time, "Mass Concentration of Diazotrophs expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chldiaz", cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_diazotrophs_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Mass Concentration of Diazotrophs expressed as Chlorophyll in Sea Water")

    cobalt%id_chlpico = register_diag_field(package_name, "chlpico_raw", axes(1:3), &
         init_time, &
         "Mass Concentration of Picophytoplankton expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chlpico", cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_picophytoplankton_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Mass Concentration of Picophytoplankton expressed as Chlorophyll in Sea Water")

    cobalt%id_chlmisc = register_diag_field(package_name, "chlmisc_raw", axes(1:3), &
         init_time, &
         "Mass Concentration of Other Phytoplankton expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chlmisc", cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_miscellaneous_phytoplankton_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Mass Concentration of Other Phytoplankton expressed as Chlorophyll in Sea Water")

! 2017/11/27 not in data request
    cobalt%id_poc = register_diag_field(package_name, "poc_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Particulate Organic Matter expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="poc", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Mole Concentration of Particulate Organic Matter expressed as Carbon in Sea Water")

    cobalt%id_pon = register_diag_field(package_name, "pon_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Particulate Organic Matter expressed as Nitrogen in Sea Water", &
         "mol N m-3", missing_value = missing_value1, cmor_field_name="pon", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_nitrogen_in_sea_water", &
         cmor_long_name="Mole Concentration of Particulate Organic Matter expressed as Nitrogen in Sea Water")

    cobalt%id_pop = register_diag_field(package_name, "pop_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Particulate Organic Matter expressed as Phosphorus in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="pop", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_phosphorus_in_sea_water", &
         cmor_long_name="Mole Concentration of Particulate Organic Matter expressed as Phosphorus in Sea Water")

    cobalt%id_bfe = register_diag_field(package_name, "bfe_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Particulate Organic Matter expressed as Iron in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="bfe", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_iron_in_sea_water", &
         cmor_long_name="Mole Concentration of Particulate Organic Matter expressed as Iron in Sea Water")

! CHECK3:
! 2017/12/28 jgj Updated standard name in data request to include _organic
    cobalt%id_bsi = register_diag_field(package_name, "bsi_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Particulate Organic Matter expressed as Silicon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="bsi", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_silicon_in_sea_water", &
         cmor_long_name="Mole Concentration of Particulate Organic Matter expressed as Silicon in Sea Water")

    cobalt%id_phyn = register_diag_field(package_name, "phyn_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Total Phytoplankton expressed as Nitrogen in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phyn", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_nitrogen_in_sea_water", &
         cmor_long_name="Mole Concentration of Total Phytoplankton expressed as Nitrogen in Sea Water")

    cobalt%id_phyp = register_diag_field(package_name, "phyp_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Total Phytoplankton expressed as Phosphorus in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phyp", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_phosphorus_in_sea_water", &
         cmor_long_name="Mole Concentration of Total Phytoplankton expressed as Phosphorus in Sea Water")

    cobalt%id_phyfe = register_diag_field(package_name, "phyfe_raw", axes(1:3), &
         init_time, "Mole Concentration of Total Phytoplankton expressed as Iron in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phyfe", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_iron_in_sea_water", &
         cmor_long_name="Mole Concentration of Total Phytoplankton expressed as Iron in Sea Water")

    cobalt%id_physi = register_diag_field(package_name, "physi_raw", axes(1:3), &
         init_time, "Mole Concentration of Total Phytoplankton expressed as Silicon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="physi", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_silicon_in_sea_water", &
         cmor_long_name="Mole Concentration of Total Phytoplankton expressed as Silicon in Sea Water")

    cobalt%id_co3 = register_diag_field(package_name, "co3_raw", axes(1:3), &
         init_time, "Carbonate Ion Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="co3", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Carbonate Ion Concentration")

    cobalt%id_co3nat = register_diag_field(package_name, "co3nat_raw", axes(1:3), &
         init_time, "Natural Carbonate Ion Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="co3nat", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_natural_analogue_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Natural Carbonate Ion Concentration")

    cobalt%id_co3abio = register_diag_field(package_name, "co3abio_raw", axes(1:3), &
         init_time, "Abiotic Carbonate Ion Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="co3abio", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_abiotic_analogue_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Abiotic Carbonate Ion Concentration")

! 2018/01/17 jgj Updated standard name in data request to match equivalent surface variable
    cobalt%id_co3satcalc = register_diag_field(package_name, "co3satcalc_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Carbonate Ion in Equilibrium with Pure Calcite in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="co3satcalc", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_expressed_as_carbon_at_equilibrium_with_pure_calcite_in_sea_water", &
         cmor_long_name="Mole Concentration of Carbonate Ion in Equilibrium with Pure Calcite in Sea Water")

! 2018/01/17 jgj Updated standard name in data request to match equivalent surface variable
    cobalt%id_co3satarag = register_diag_field(package_name, "co3satarag_raw", axes(1:3), &
         init_time, &
         "Mole Concentration of Carbonate Ion in Equilibrium with Pure Aragonite in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="co3satarag", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_expressed_as_carbon_at_equilibrium_with_pure_aragonite_in_sea_water", &
         cmor_long_name="Mole Concentration of Carbonate Ion in Equilibrium with Pure Aragonite in Sea Water")

!------------------------------------------------------------------------------------------------------------------
! 3-D rates
! CHECK3: all GFDL and CMOR units

    cobalt%id_pp = register_diag_field(package_name, "pp_raw", axes(1:3), &
         init_time, "Primary Carbon Production by Total Phytoplankton", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="pp", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production", &
         cmor_long_name="Primary Carbon Production by Total Phytoplankton")

    cobalt%id_pnitrate = register_diag_field(package_name, "pnitrate_raw", axes(1:3), &
         init_time, "Primary Carbon Production by Phytoplankton due to Nitrate Uptake Alone", &
         "mol m-3 s-1", missing_value = missing_value1, cmor_field_name="pnitrate", &
         cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_nitrate_utilization", &
         cmor_long_name="Primary Carbon Production by Phytoplankton due to Nitrate Uptake Alone")

! Not requested
!    vardesc_temp = vardesc("pphosphate_raw","Primary Carbon Production by Phytoplankton due to Phosphorus",'h','L','s','mol m-3 s-1','f')
!    cobalt%id_pphosphate = register_diag_field(package_name, vardesc_temp%name, axes(1:3), &
!         init_time, vardesc_temp%longname,vardesc_temp%units, missing_value = missing_value1, &
!         cmor_field_name="pphosphate", cmor_units="mol m-3 s-1",                          &
!         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_phosphorus", &
!         cmor_long_name="Primary Carbon Production by Phytoplankton due to Phosphorus")

    cobalt%id_pbfe = register_diag_field(package_name, "pbfe_raw", axes(1:3), &
         init_time, "Biogenic Iron Production", "mol m-3 s-1", missing_value = missing_value1, &
         cmor_field_name="pbfe", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_iron_in_sea_water_due_to_biological_production", &
         cmor_long_name="Biogenic Iron Production")

    cobalt%id_pbsi = register_diag_field(package_name, "pbsi_raw", axes(1:3), &
         init_time, "Biogenic Silicon Production", "mol m-3 s-1", missing_value = missing_value1, &
         cmor_field_name="pbsi", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_silicon_in_sea_water_due_to_biological_production", &
         cmor_long_name="Biogenic Silicon Production")

    ! Note: COBALT only models the production of calcite detritus, so values will be smaller than estimates of total
    ! calcite production by approximately a factor of 1 over the calcite-specific export ratio.
    cobalt%id_pcalc = register_diag_field(package_name, "pcalc_raw", axes(1:3), &
         init_time, "Calcite Production", "mol m-3 s-1", missing_value = missing_value1, &
         cmor_field_name="pcalc", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_calcite_expressed_as_carbon_in_sea_water_due_to_biological_production", &
         cmor_long_name="Calcite Production")

    ! Note: COBALT only models the production of aragonite detritus, so values will be smaller than estimates of total
    ! aragonite production by approximately a factor of 1 over the aragonite-specific export ratio.
    cobalt%id_parag = register_diag_field(package_name, "parag_raw", axes(1:3), &
         init_time, "Aragonite Production", "mol m-3 s-1", missing_value = missing_value1, &
         cmor_field_name="parag", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_aragonite_expressed_as_carbon_in_sea_water_due_to_biological_production", &
         cmor_long_name="Aragonite Production")

    ! As with the native COBALT diagnostics, these fluxes were historically saved at the tracer points.  We have added
    ! an option to save them at the interfaces to be more consistent with the upwind numerics.
    cobalt%id_expc_tp = register_diag_field(package_name, "expc_raw", axes(1:3), &
         init_time, "Sinking Particulate Organic Carbon Flux", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expc", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_matter_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Particulate Organic Carbon Flux")

    cobalt%id_expcob = register_diag_field(package_name, "expcob_raw", axes(1:2), &
         init_time, "Sinking Flux of Particulate Organic Carbon Reaching the Ocean Bottom", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="expcob", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_matter_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Flux of Particulate Organic Carbon Reaching the Ocean Bottom")

    cobalt%id_expn_tp = register_diag_field(package_name, "expn_raw", axes(1:3), &
         init_time, "Sinking Particulate Organic Nitrogen Flux", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expn", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_nitrogen_in_sea_water", &
         cmor_long_name="Sinking Particulate Organic Nitrogen Flux")

    cobalt%id_expnob = register_diag_field(package_name, "expnob_raw", axes(1:2), &
         init_time, "Sinking Flux of Particulate Organic Nitrogen Reaching the Ocean Bottom", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="expnob", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_nitrogen_in_sea_water", &
         cmor_long_name="Sinking Flux of Particulate Organic Nitrogen Reaching the Ocean Bottom")

    cobalt%id_expp_tp = register_diag_field(package_name, "expp_raw", axes(1:3), &
         init_time, "Sinking Particulate Organic Phosphorus Flux", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expp", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_phosphorus_in_sea_water", &
         cmor_long_name="Sinking Particulate Organic Phosphorus Flux")

    cobalt%id_exppob = register_diag_field(package_name, "exppob_raw", axes(1:2), &
         init_time, "Sinking Flux of Particulate Organic Phosphorus Reaching the Ocean Bottom", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="exppob", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_phosphorus_in_sea_water", &
         cmor_long_name="Sinking Flux of Particulate Organic Phosphorus Reaching the Ocean Bottom")

    cobalt%id_expfe_tp = register_diag_field(package_name, "expfe_raw", axes(1:3), &
         init_time, "Sinking Particulate Iron Flux", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expfe", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_iron_in_sea_water", &
         cmor_long_name="Sinking Particulate Iron Flux")

    cobalt%id_expfeob = register_diag_field(package_name, "expfeob_raw", axes(1:2), &
         init_time, "Sinking Flux of Particulate Iron Reaching the Ocean Bottom", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expfeob", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_iron_in_sea_water", &
         cmor_long_name="Sinking Flux of Particulate Iron Reaching the Ocean Bottom")

    cobalt%id_expsi_tp = register_diag_field(package_name, "expsi_raw", axes(1:3), &
         init_time, "Sinking Particulate Silicon Flux", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expsi", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_silicon_in_sea_water", &
         cmor_long_name="Sinking Particulate Silicon Flux")

    cobalt%id_expsiob = register_diag_field(package_name, "expsiob_raw", axes(1:2), &
         init_time, "Sinking Flux of Particulate Silicon Reaching the Ocean Bottom", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="expsiob", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_silicon_in_sea_water", &
         cmor_long_name="Sinking Flux of Particulate Silicon Reaching the Ocean Bottom")

    cobalt%id_expcalc_tp = register_diag_field(package_name, "expcalc_raw", axes(1:3), &
         init_time, "Sinking Calcite Flux", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="expcalc", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_calcite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Calcite Flux")

    cobalt%id_expcalcob = register_diag_field(package_name, "expcalcob_raw", axes(1:2), &
         init_time, "Sinking Flux of Calcite Reaching the Ocean Bottom", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expcalcob", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_calcite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Flux of Calcite Reaching the Ocean Bottom")

    cobalt%id_exparag_tp = register_diag_field(package_name, "exparag_raw", axes(1:3), &
         init_time, "Sinking Aragonite Flux", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="exparag", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_aragonite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Aragonite Flux")

    cobalt%id_exparagob = register_diag_field(package_name, "exparagob_raw", axes(1:2), &
         init_time, "Sinking Flux of Aragonite Reaching the Ocean Bottom", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="exparagob", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_aragonite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Flux of Aragonite Reaching the Ocean Bottom")

    !
    ! Option to save at interfaces
    !
    cobalt%id_expc_i = register_diag_field(package_name, "expc_raw_i", axes(1:1), &
         init_time, "Sinking Particulate Organic Carbon Flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expc_i", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_matter_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Particulate Organic Carbon Flux")

    cobalt%id_expn_i = register_diag_field(package_name, "expn_raw_i", axes(1:1), &
         init_time, "Sinking Particulate Organic Nitrogen Flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expn_i", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_nitrogen_in_sea_water", &
         cmor_long_name="Sinking Particulate Organic Nitrogen Flux")

    cobalt%id_expp_i = register_diag_field(package_name, "expp_raw_i", axes(1:1), &
         init_time, "Sinking Particulate Organic Phosphorus Flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expp_i", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_phosphorus_in_sea_water", &
         cmor_long_name="Sinking Particulate Organic Phosphorus Flux")

    cobalt%id_expfe_i = register_diag_field(package_name, "expfe_raw_i", axes(1:1), &
         init_time, "Sinking Particulate Iron Flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expfe_i", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_iron_in_sea_water", &
         cmor_long_name="Sinking Particulate Iron Flux")

    cobalt%id_expsi_i = register_diag_field(package_name, "expsi_raw_i", axes(1:1), &
         init_time, "Sinking Particulate Silicon Flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expsi_i", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_silicon_in_sea_water", &
         cmor_long_name="Sinking Particulate Silicon Flux")

    cobalt%id_expcalc_i = register_diag_field(package_name, "expcalc_raw_i", axes(1:1), &
         init_time, "Sinking Calcite Flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="expcalc_i", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_calcite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Calcite Flux")

    cobalt%id_exparag_i = register_diag_field(package_name, "exparag_raw_i", axes(1:1), &
         init_time, "Sinking Aragonite Flux (@interfaces)", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="exparag_i", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_aragonite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Sinking Aragonite Flux")

    cobalt%id_remoc = register_diag_field(package_name, "remoc_raw", axes(1:3), &
         init_time, "Remineralization of Organic Carbon", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="remoc", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic matter_expressed_as_carbon_in_sea_water_due_to_remineralization", &
         cmor_long_name="Remineralization of Organic Carbon")

    cobalt%id_dcalc = register_diag_field(package_name, "dcalc_raw", axes(1:3), &
         init_time, "Calcite Dissolution", "mol m-3 s-1", missing_value = missing_value1, &
         cmor_field_name="dcalc", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_calcite_expressed_as_carbon_in_sea_water_due_to_dissolution", &
         cmor_long_name="Calcite Dissolution")

    cobalt%id_darag = register_diag_field(package_name, "darag_raw", axes(1:3), &
         init_time, "Aragonite Dissolution", "mol m-3 s-1", missing_value = missing_value1, &
         cmor_field_name="darag", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_aragonite_expressed_as_carbon_in_sea_water_due_to_dissolution", &
         cmor_long_name="Aragonite Dissolution")

! CHECK3:
    cobalt%id_ppdiat = register_diag_field(package_name, "ppdiat_raw", axes(1:3), &
         init_time, "Net Primary Organic Carbon Production by Diatoms", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="ppdiat", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production_by_diatoms", &
         cmor_long_name="Net Primary Organic Carbon Production by Diatoms")

! CHECK3:
    cobalt%id_ppdiaz = register_diag_field(package_name, "ppdiaz_raw", axes(1:3), &
         init_time, "Net Primary Mole Productivity of Carbon by Diazotrophs", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="ppdiaz", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production_by_diazotrophs", &
         cmor_long_name="Net Primary Mole Productivity of Carbon by Diazotrophs")

! CHECK3:
    cobalt%id_pppico = register_diag_field(package_name, "pppico_raw", axes(1:3), &
         init_time, "Net Primary Mole Productivity of Carbon by Picophytoplankton", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="pppico", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production_by_picophytoplankton", &
         cmor_long_name="Net Primary Mole Productivity of Carbon by Picophytoplankton")

! CHECK3:
    cobalt%id_ppmisc = register_diag_field(package_name, "ppmisc_raw", axes(1:3), &
         init_time, "Net Primary Organic Carbon Production by Other Phytoplankton", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="ppmisc", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_net_primary_production_by_miscellaneous_phytoplankton", &
         cmor_long_name="Net Primary Organic Carbon Production by Other Phytoplankton")

    cobalt%id_bddtdic = register_diag_field(package_name, "bddtdic_raw", axes(1:3), &
         init_time, "Rate of Change of Dissolved Inorganic Carbon due to Biological Activity", &
         "mol m-3 s-1", missing_value = missing_value1, cmor_field_name="bddtdic", &
         cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_dissolved_inorganic_carbon_in_sea_water_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Inorganic Carbon due to Biological Activity")

    cobalt%id_bddtdin = register_diag_field(package_name, "bddtdin_raw", axes(1:3), &
         init_time, "Rate of Change of Nitrogen Nutrient due to Biological Activity", &
         "mol m-3 s-1", missing_value = missing_value1, cmor_field_name="bddtdin", &
         cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_dissolved_inorganic_nitrogen_in_sea_water_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Nitrogen Nutrient due to Biological Activity")

    cobalt%id_bddtdip = register_diag_field(package_name, "bddtdip_raw", axes(1:3), &
         init_time, "Rate of Change of Dissolved Phosphorus due to Biological Activity", &
         "mol m-3 s-1", missing_value = missing_value1, cmor_field_name="bddtdip", &
         cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_dissolved_inorganic_phosphorus_in_sea_water_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Phosphorus due to Biological Activity")

    cobalt%id_bddtdife = register_diag_field(package_name, "bddtdife_raw", axes(1:3), &
         init_time, "Rate of Change of Dissolved Inorganic Iron due to Biological Activity", &
         "mol m-3 s-1", missing_value = missing_value1, cmor_field_name="bddtdife", &
         cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_dissolved_inorganic_iron_in_sea_water_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Inorganic Iron due to Biological Activity")

    cobalt%id_bddtdisi = register_diag_field(package_name, "bddtdisi_raw", axes(1:3), &
         init_time, "Rate of Change of Dissolved Inorganic Silicon due to Biological Activity", &
         "mol m-3 s-1", missing_value = missing_value1, cmor_field_name="bddtdisi", &
         cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_dissolved_inorganic_silicon_in_sea_water_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Inorganic Silicon due to Biological Activity")

    cobalt%id_bddtalk = register_diag_field(package_name, "bddtalk_raw", axes(1:3), &
         init_time, "Rate of Change of Alkalinity due to Biological Activity", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="bddtalk", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_sea_water_alkalinity_expressed_as_mole_equivalent_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Alkalinity due to Biological Activity")

    cobalt%id_fescav = register_diag_field(package_name, "fescav_raw", axes(1:3), &
         init_time, "Nonbiogenic Iron Scavenging", "mol m-3 s-1", missing_value = missing_value1, &
         cmor_field_name="fescav", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_dissolved_iron_in_sea_water_due_to_scavenging_by_inorganic_particles", &
         cmor_long_name="Nonbiogenic Iron Scavenging")

    cobalt%id_fediss = register_diag_field(package_name, "fediss_raw", axes(1:3), &
         init_time, "Particle Source of Dissolved Iron", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="fediss", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_dissolved_iron_in_sea_water_due_to_dissolution_from_inorganic_particles", &
         cmor_long_name="Particle Source of Dissolved Iron")

    cobalt%id_graz = register_diag_field(package_name, "graz_raw", axes(1:3), &
         init_time, "Total Grazing of Phytoplankton by Zooplankton", "mol m-3 s-1", &
         missing_value = missing_value1, cmor_field_name="graz", cmor_units="mol m-3 s-1", &
         cmor_standard_name="tendency_of_mole_concentration_of_particulate_organic_matter_expressed_as_carbon_in_sea_water_due_to_grazing_of_phytoplankton", &
         cmor_long_name="Total Grazing of Phytoplankton by Zooplankton")

!------------------------------------------------------------------------------------------------------------------
! 3-D Limitation terms
! 2018/07/18 limitation terms are now 2-D

    cobalt%id_limndiat = register_diag_field(package_name, "limndiat_raw", axes(1:2), &
         init_time, "Nitrogen Limitation of Diatoms", "1", missing_value = missing_value1, &
         cmor_field_name="limndiat", cmor_units="1", &
         cmor_standard_name="nitrogen_growth_limitation_of_diatoms", &
         cmor_long_name="Nitrogen Limitation of Diatoms")

    cobalt%id_limndiaz = register_diag_field(package_name, "limndiaz_raw", axes(1:2), &
         init_time, "Nitrogen Limitation of Diazotrophs", "1", missing_value = missing_value1, &
         cmor_field_name="limndiaz", cmor_units="1", &
         cmor_standard_name="nitrogen_growth_limitation_of_diazotrophs", &
         cmor_long_name="Nitrogen Limitation of Diazotrophs")

    cobalt%id_limnpico = register_diag_field(package_name, "limnpico_raw", axes(1:2), &
         init_time, "Nitrogen Limitation of Picophytoplankton", "1", &
         missing_value = missing_value1, cmor_field_name="limnpico", cmor_units="1", &
         cmor_standard_name="nitrogen_growth_limitation_of_picophytoplankton", &
         cmor_long_name="Nitrogen Limitation of Picophytoplankton")

    cobalt%id_limnmisc = register_diag_field(package_name, "limnmisc_raw", axes(1:2), &
         init_time, "Nitrogen Limitation of Other Phytoplankton", "1", &
         missing_value = missing_value1, cmor_field_name="limnmisc", cmor_units="1", &
         cmor_standard_name="nitrogen_growth_limitation_of_miscellaneous_phytoplankton", &
         cmor_long_name="Nitrogen Limitation of Other Phytoplankton")

    cobalt%id_limirrdiat = register_diag_field(package_name, "limirrdiat_raw", axes(1:2), &
         init_time, "Irradiance Limitation of Diatoms", "1", missing_value = missing_value1, &
         cmor_field_name="limirrdiat", cmor_units="1", &
         cmor_standard_name="growth_limitation_of_diatoms_due_to_solar_irradiance", &
         cmor_long_name="Irradiance Limitation of Diatoms")

    cobalt%id_limirrdiaz = register_diag_field(package_name, "limirrdiaz_raw", axes(1:2), &
         init_time, "Irradiance Limitation of Diazotrophs", "1", missing_value = missing_value1, &
         cmor_field_name="limirrdiaz", cmor_units="1", &
         cmor_standard_name="growth_limitation_of_diazotrophs_due_to_solar_irradiance", &
         cmor_long_name="Irradiance Limitation of Diazotrophs")

    cobalt%id_limirrpico = register_diag_field(package_name, "limirrpico_raw", axes(1:2), &
         init_time, "Irradiance Limitation of Picophytoplankton", "1", &
         missing_value = missing_value1, cmor_field_name="limirrpico", cmor_units="1", &
         cmor_standard_name="growth_limitation_of_picophytoplankton_due_to_solar_irradiance", &
         cmor_long_name="Irradiance Limitation of Picophytoplankton")

    cobalt%id_limirrmisc = register_diag_field(package_name, "limirrmisc_raw", axes(1:2), &
         init_time, "Irradiance Limitation of Other Phytoplankton", "1", &
         missing_value = missing_value1, cmor_field_name="limirrmisc", cmor_units="1", &
         cmor_standard_name="growth_limitation_of_miscellaneous_phytoplankton_due_to_solar_irradiance", &
         cmor_long_name="Irradiance Limitation of Other Phytoplankton")

    cobalt%id_limfediat = register_diag_field(package_name, "limfediat_raw", axes(1:2), &
         init_time, "Iron Limitation of Diatoms", "1", missing_value = missing_value1, &
         cmor_field_name="limfediat", cmor_units="1", &
         cmor_standard_name="iron_growth_limitation_of_diatoms", &
         cmor_long_name="Iron Limitation of Diatoms")

    cobalt%id_limfediaz = register_diag_field(package_name, "limfediaz_raw", axes(1:2), &
         init_time, "Iron Limitation of Diazotrophs", "1", missing_value = missing_value1, &
         cmor_field_name="limfediaz", cmor_units="1", &
         cmor_standard_name="iron_growth_limitation_of_diazotrophs", &
         cmor_long_name="Iron Limitation of Diazotrophs")

    cobalt%id_limfepico = register_diag_field(package_name, "limfepico_raw", axes(1:2), &
         init_time, "Iron Limitation of Picophytoplankton", "1", missing_value = missing_value1, &
         cmor_field_name="limfepico", cmor_units="1", &
         cmor_standard_name="iron_growth_limitation_of_picophytoplankton", &
         cmor_long_name="Iron Limitation of Picophytoplankton")

    cobalt%id_limfemisc = register_diag_field(package_name, "limfemisc_raw", axes(1:2), &
         init_time, "Iron Limitation of Other Phytoplankton", "1", missing_value = missing_value1, &
         cmor_field_name="limfemisc", cmor_units="1", &
         cmor_standard_name="iron_growth_limitation_of_miscellaneous_phytoplankton", &
         cmor_long_name="Iron Limitation of Other Phytoplankton")

!-- added by JGJ - not requested by CMIP6/OMIP
    cobalt%id_limpdiat = register_diag_field(package_name, "limpdiat_raw", axes(1:2), &
         init_time, "Phosphorus Limitation of Diatoms", "1", missing_value = missing_value1, &
         cmor_field_name="limpdiat", cmor_units="1", &
         cmor_standard_name="phosphorus_growth_limitation_of_diatoms", &
         cmor_long_name="Phosphorus Limitation of Diatoms")

    cobalt%id_limpdiaz = register_diag_field(package_name, "limpdiaz_raw", axes(1:2), &
         init_time, "Phosphorus Limitation of Diazotrophs", "1", missing_value = missing_value1, &
         cmor_field_name="limpdiaz", cmor_units="1", &
         cmor_standard_name="phosphorus_growth_limitation_of_diazotrophs", &
         cmor_long_name="Phosphorus Limitation of Diazotrophs")

    cobalt%id_limppico = register_diag_field(package_name, "limppico_raw", axes(1:2), &
         init_time, "Phosphorus Limitation of Picophytoplankton", "1", &
         missing_value = missing_value1, cmor_field_name="limppico", cmor_units="1", &
         cmor_standard_name="phosphorus_growth_limitation_of_picophytoplankton", &
         cmor_long_name="Phosphorus Limitation of Picophytoplankton")

    cobalt%id_limpmisc = register_diag_field(package_name, "limpmisc_raw", axes(1:2), &
         init_time, "Phosphorus Limitation of Other Phytoplankton", "1", &
         missing_value = missing_value1, cmor_field_name="limpmisc", cmor_units="1", &
         cmor_standard_name="phosphorus_growth_limitation_of_miscellaneous_phytoplankton", &
         cmor_long_name="Phosphorus Limitation of Other Phytoplankton")

!------------------------------------------------------------------------------------------------------------------
! 2-D fields
! sfc tracers

    cobalt%id_dissicos = register_diag_field(package_name, "dissicos_raw", axes(1:2), &
         init_time, "Surface Dissolved Inorganic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissicos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_carbon_in_sea_water", &
         cmor_long_name="Surface Dissolved Inorganic Carbon Concentration")

    cobalt%id_dissicnatos = register_diag_field(package_name, "dissicnatos_raw", axes(1:2), &
         init_time, "Surface Natural Dissolved Inorganic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissicnatos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_carbon_natural_analogue_in_sea_water", &
         cmor_long_name="Surface Natural Dissolved Inorganic Carbon Concentration")

    cobalt%id_dissicabioos = register_diag_field(package_name, "dissicabioos_raw", axes(1:2), &
         init_time, "Surface Abiotic Dissolved Inorganic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissicabioos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_carbon_abiotic_analogue_in_sea_water", &
         cmor_long_name="Surface Abiotic Dissolved Inorganic Carbon Concentration")

    cobalt%id_dissi14cabioos = register_diag_field(package_name, "dissi14cabioos_raw", axes(1:2), &
         init_time, "Surface Abiotic Dissolved Inorganic 14Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissi14cabioos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_carbon14_abiotic_analogue_in_sea_water", &
         cmor_long_name="Surface Abiotic Dissolved Inorganic 14Carbon Concentration")

    cobalt%id_dissocos = register_diag_field(package_name, "dissocos_raw", axes(1:2), &
         init_time, "Surface Dissolved Organic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dissocos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_organic_carbon_in_sea_water", &
         cmor_long_name="Surface Dissolved Organic Carbon Concentration")

! also in Oday (updated to match Omon long_name, standard_name)
    cobalt%id_phycos = register_diag_field(package_name, "phycos_raw", axes(1:2), &
         init_time, "Surface Phytoplankton Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="phycos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Phytoplankton Carbon Concentration")

    cobalt%id_zoocos = register_diag_field(package_name, "zoocos_raw", axes(1:2), &
         init_time, "Surface Zooplankton Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="zoocos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_zooplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Zooplankton Carbon Concentration")

    cobalt%id_baccos = register_diag_field(package_name, "baccos_raw", axes(1:2), &
         init_time, "Surface Bacterial Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="baccos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_bacteria_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Bacterial Carbon Concentration")

    cobalt%id_detocos = register_diag_field(package_name, "detocos_raw", axes(1:2), &
         init_time, "Surface Detrital Organic Carbon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="detocos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_organic_detritus_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Detrital Organic Carbon Concentration")

    cobalt%id_calcos = register_diag_field(package_name, "calcos_raw", axes(1:2), &
         init_time, "Surface Calcite Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="calcos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_calcite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Calcite Concentration")

    ! Note that COBALT onlt tracks aragonite detritus
    cobalt%id_aragos = register_diag_field(package_name, "aragos_raw", axes(1:2), &
         init_time, "Surface Aragonite Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="aragos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_aragonite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Aragonite Concentration")

    cobalt%id_phydiatos = register_diag_field(package_name, "phydiatos_raw", axes(1:2), &
         init_time, "Surface Mole Concentration of Diatoms expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phydiatos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_diatoms_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Diatoms expressed as Carbon in Sea Water")

    cobalt%id_phydiazos = register_diag_field(package_name, "phydiazos_raw", axes(1:2), &
         init_time, "Surface Mole Concentration of Diazotrophs expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phydiazos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_diazotrophs_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Diazotrophs expressed as Carbon in Sea Water")

    cobalt%id_phypicoos = register_diag_field(package_name, "phypicoos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Picophytoplankton expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phypicoos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_picophytoplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Picophytoplankton expressed as Carbon in Sea Water")

    cobalt%id_phymiscos = register_diag_field(package_name, "phymiscos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Miscellaneous Phytoplankton expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phymiscos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_miscellaneous_phytoplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Miscellaneous Phytoplankton expressed as Carbon in Sea Water")

    cobalt%id_zmicroos = register_diag_field(package_name, "zmicroos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Microzooplankton expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="zmicroos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_microzooplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Microzooplankton expressed as Carbon in Sea Water")

    cobalt%id_zmesoos = register_diag_field(package_name, "zmesoos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Mesozooplankton expressed as Carbon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="zmesoos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_mesozooplankton_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Mesozooplankton expressed as Carbon in Sea Water")

    cobalt%id_talkos = register_diag_field(package_name, "talkos_raw", axes(1:2), &
         init_time, "Surface Total Alkalinity", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="talkos", cmor_units="mol m-3", &
         cmor_standard_name="sea_water_alkalinity_expressed_as_mole_equivalent", &
         cmor_long_name="Surface Total Alkalinity")

    cobalt%id_talknatos = register_diag_field(package_name, "talknatos_raw", axes(1:2), &
         init_time, "Surface Natural Total Alkalinity", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="talknatos", cmor_units="mol m-3", &
         cmor_standard_name="sea_water_alkalinity_natural_analogue_expressed_as_mole_equivalent", &
         cmor_long_name="Surface Natural Total Alkalinity")

    cobalt%id_phos = register_diag_field(package_name, "phos_raw", axes(1:2), &
         init_time, "Surface pH", "1", missing_value = missing_value1, cmor_field_name="phos", &
         cmor_units="1", cmor_standard_name="sea_water_ph_reported_on_total_scale", &
         cmor_long_name="Surface pH")

    cobalt%id_phnatos = register_diag_field(package_name, "phnatos_raw", axes(1:2), &
         init_time, "Surface Natural pH", "1", missing_value = missing_value1, &
         cmor_field_name="phnatos", cmor_units="1", &
         cmor_standard_name="sea_water_ph_natural_analogue_reported_on_total_scale", &
         cmor_long_name="Surface Natural pH")

    cobalt%id_phabioos = register_diag_field(package_name, "phabioos_raw", axes(1:2), &
         init_time, "Surface Abiotic pH", "1", missing_value = missing_value1, &
         cmor_field_name="phabioos", cmor_units="1", &
         cmor_standard_name="sea_water_ph_abiotic_analogue_reported_on_total_scale", &
         cmor_long_name="Surface Abiotic pH")

    cobalt%id_o2os = register_diag_field(package_name, "o2os_raw", axes(1:2), &
         init_time, "Surface Dissolved Oxygen Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="o2os", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_molecular_oxygen_in_sea_water", &
         cmor_long_name="Surface Dissolved Oxygen Concentration")

    cobalt%id_o2satos = register_diag_field(package_name, "o2satos_raw", axes(1:2), &
         init_time, "Surface Dissolved Oxygen Concentration at Saturation", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="o2satos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_molecular_oxygen_in_sea_water_at_saturation", &
         cmor_long_name="Surface Dissolved Oxygen Concentration at Saturation")

    cobalt%id_no3os = register_diag_field(package_name, "no3os_raw", axes(1:2), &
         init_time, "Surface Dissolved Nitrate Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="no3os", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_nitrate_in_sea_water", &
         cmor_long_name="Surface Dissolved Nitrate Concentration")

    cobalt%id_nh4os = register_diag_field(package_name, "nh4os_raw", axes(1:2), &
         init_time, "Surface Dissolved Ammonium Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="nh4os", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_ammonium_in_sea_water", &
         cmor_long_name="Surface Dissolved Ammonium Concentration")

! CHECK3:
! 2018/01/12 standard name is  "surface_mole_concentration_of_dissolved_inorganic_phosphorous_in_sea_water" in data request
! 2018/01/17 removed surface_ from CF standard name in data request
    cobalt%id_po4os = register_diag_field(package_name, "po4os_raw", axes(1:2), &
         init_time, "Surface Total Dissolved Inorganic Phosphorus Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="po4os", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_phosphorus_in_sea_water", &
         cmor_long_name="Surface Total Dissolved Inorganic Phosphorus Concentration")

    cobalt%id_dfeos = register_diag_field(package_name, "dfeos_raw", axes(1:2), &
         init_time, "Surface Dissolved Iron Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="dfeos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_iron_in_sea_water", &
         cmor_long_name="Surface Dissolved Iron Concentration")

    cobalt%id_sios = register_diag_field(package_name, "sios_raw", axes(1:2), &
         init_time, "Surface Total Dissolved Inorganic Silicon Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="sios", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_inorganic_silicon_in_sea_water", &
         cmor_long_name="Surface Total Dissolved Inorganic Silicon Concentration")

    cobalt%id_chlos = register_diag_field(package_name, "chlos_raw", axes(1:2), &
         init_time, &
         "Surface Mass Concentration of Total Phytoplankton expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chlos", cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_phytoplankton_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Surface Mass Concentration of Total Phytoplankton expressed as Chlorophyll in Sea Water")

    cobalt%id_chldiatos = register_diag_field(package_name, "chldiatos_raw", axes(1:2), &
         init_time, "Surface Mass Concentration of Diatoms expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chldiatos", &
         cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_diatoms_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Surface Mass Concentration of Diatoms expressed as Chlorophyll in Sea Water")

    cobalt%id_chldiazos = register_diag_field(package_name, "chldiazos_raw", axes(1:2), &
         init_time, &
         "Surface Mass Concentration of Diazotrophs expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chldiazos", &
         cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_diazotrophs_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Surface Mass Concentration of Diazotrophs expressed as Chlorophyll in Sea Water")

    cobalt%id_chlpicoos = register_diag_field(package_name, "chlpicoos_raw", axes(1:2), &
         init_time, &
         "Surface Mass Concentration of Picophytoplankton expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chlpicoos", &
         cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_picophytoplankton_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Surface Mass Concentration of Picophytoplankton expressed as Chlorophyll in Sea Water")

    cobalt%id_chlmiscos = register_diag_field(package_name, "chlmiscos_raw", axes(1:2), &
         init_time, &
         "Surface Mass Concentration of Other Phytoplankton expressed as Chlorophyll in Sea Water", &
         "kg m-3", missing_value = missing_value1, cmor_field_name="chlmiscos", &
         cmor_units="kg m-3", &
         cmor_standard_name="mass_concentration_of_miscellaneous_phytoplankton_expressed_as_chlorophyll_in_sea_water", &
         cmor_long_name="Surface Mass Concentration of Other Phytoplankton expressed as Chlorophyll in Sea Water")

    cobalt%id_ponos = register_diag_field(package_name, "ponos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Particulate Organic Matter expressed as Nitrogen in Sea Water", &
         "mol N m-3", missing_value = missing_value1, cmor_field_name="ponos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_nitrogen_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Particulate Organic Matter expressed as Nitrogen in Sea Water")

    cobalt%id_popos = register_diag_field(package_name, "popos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Particulate Organic Matter expressed as Phosphorus in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="popos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_phosphorus_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Particulate Organic Matter expressed as Phosphorus in Sea Water")

    cobalt%id_bfeos = register_diag_field(package_name, "bfeos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Particulate Organic Matter expressed as Iron in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="bfeos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_iron_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Particulate Organic Matter expressed as Iron in Sea Water")

    cobalt%id_bsios = register_diag_field(package_name, "bsios_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Particulate Organic Matter expressed as Silicon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="bsios", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_particulate_organic_matter_expressed_as_silicon_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Particulate Organic Matter expressed as Silicon in Sea Water")

    cobalt%id_phynos = register_diag_field(package_name, "phynos_raw", axes(1:2), &
         init_time, "Surface Mole Concentration of Phytoplankton Nitrogen in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phynos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_nitrogen_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Phytoplankton Nitrogen in Sea Water")

    cobalt%id_phypos = register_diag_field(package_name, "phypos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Total Phytoplankton expressed as Phosphorus in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phypos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_phosphorus_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Total Phytoplankton expressed as Phosphorus in Sea Water")

! 2017/12/28 jgj Updated long name in data request: Surface Mole Concentration of Total Phytoplankton expressed as Iron in Sea Water
    cobalt%id_phyfeos = register_diag_field(package_name, "phyfeos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Total Phytoplankton expressed as Iron in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="phyfeos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_iron_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Total Phytoplankton expressed as Iron in Sea Water")

    cobalt%id_physios = register_diag_field(package_name, "physios_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Total Phytoplankton expressed as Silicon in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="physios", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_phytoplankton_expressed_as_silicon_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Total Phytoplankton expressed as Silicon in Sea Water")

    cobalt%id_co3os = register_diag_field(package_name, "co3os_raw", axes(1:2), &
         init_time, "Surface Carbonate Ion Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="co3os", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Carbonate Ion Concentration")

    cobalt%id_co3natos = register_diag_field(package_name, "co3natos_raw", axes(1:2), &
         init_time, "Surface Natural Carbonate Ion Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="co3natos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_ion_natural_analogue_in_sea_water", &
         cmor_long_name="Surface Natural Carbonate Ion Concentration")

    cobalt%id_co3abioos = register_diag_field(package_name, "co3abioos_raw", axes(1:2), &
         init_time, "Surface Abiotic Carbonate Ion Concentration", "mol m-3", &
         missing_value = missing_value1, cmor_field_name="co3abioos", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_abiotic_analogue_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Surface Abiotic Carbonate Ion Concentration")

    cobalt%id_co3satcalcos = register_diag_field(package_name, "co3satcalcos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Carbonate Ion in Equilibrium with Pure Calcite in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="co3satcalcos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_expressed_as_carbon_at_equilibrium_with_pure_calcite_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Carbonate Ion in Equilibrium with Pure Calcite in Sea Water")

    cobalt%id_co3sataragos = register_diag_field(package_name, "co3sataragos_raw", axes(1:2), &
         init_time, &
         "Surface Mole Concentration of Carbonate Ion in Equilibrium with Pure Aragonite in Sea Water", &
         "mol m-3", missing_value = missing_value1, cmor_field_name="co3sataragos", &
         cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_carbonate_expressed_as_carbon_at_equilibrium_with_pure_aragonite_in_sea_water", &
         cmor_long_name="Surface Mole Concentration of Carbonate Ion in Equilibrium with Pure Aragonite in Sea Water")

!------------------------------------------------------------------------------------------------------------------
! 2-D fields (from Omon)

    cobalt%id_intpp = register_diag_field(package_name, "intpp_raw", axes(1:2), &
         init_time, "Primary Organic Carbon Production by All Types of Phytoplankton", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="intpp", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_phytoplankton", &
         cmor_long_name="Primary Organic Carbon Production by All Types of Phytoplankton")

    cobalt%id_intppnitrate = register_diag_field(package_name, "intppnitrate_raw", axes(1:2), &
         init_time, &
         "Primary Organic Carbon Production by Phytoplankton Based on Nitrate Uptake Alone", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="intppnitrate", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="net_primary_mole_productivity_of_biomass_expressed_as_carbon_due_to_nitrate_utilization", &
         cmor_long_name="Primary Organic Carbon Production by Phytoplankton Based on Nitrate Uptake Alone")

    cobalt%id_intppdiat = register_diag_field(package_name, "intppdiat_raw", axes(1:2), &
         init_time, "Net Primary Organic Carbon Production by Diatoms", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="intppdiat", cmor_units="mol m-2 s-1", &
         cmor_standard_name="net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_diatoms", &
         cmor_long_name="Net Primary Organic Carbon Production by Diatoms")

    cobalt%id_intppdiaz = register_diag_field(package_name, "intppdiaz_raw", axes(1:2), &
         init_time, "Net Primary Mole Productivity of Carbon by Diazotrophs", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="intppdiaz", cmor_units="mol m-2 s-1", &
         cmor_standard_name="net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_diazotrophs", &
         cmor_long_name="Net Primary Mole Productivity of Carbon by Diazotrophs")

    cobalt%id_intpppico = register_diag_field(package_name, "intpppico_raw", axes(1:2), &
         init_time, "Net Primary Mole Productivity of Carbon by Picophytoplankton", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="intpppico", cmor_units="mol m-2 s-1", &
         cmor_standard_name="net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_picophytoplankton", &
         cmor_long_name="Net Primary Mole Productivity of Carbon by Picophytoplankton")

    cobalt%id_intppmisc = register_diag_field(package_name, "intppmisc_raw", axes(1:2), &
         init_time, "Net Primary Organic Carbon Production by Other Phytoplankton", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="intppmisc", cmor_units="mol m-2 s-1", &
         cmor_standard_name="net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_miscellaneous_phytoplankton", &
         cmor_long_name="Net Primary Organic Carbon Production by Other Phytoplankton")

    cobalt%id_intppnano = register_diag_field(package_name, "intppnano_raw", axes(1:2), &
         init_time, "Net Primary Organic Carbon Production by Nanophytoplankton", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="intppnano", cmor_units="mol m-2 s-1", &
         cmor_standard_name="net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_nanophytoplankton", &
         cmor_long_name="Net Primary Organic Carbon Production by Nanophytoplankton")

    cobalt%id_intppmicro = register_diag_field(package_name, "intppmicro_raw", axes(1:2), &
         init_time, "Net Primary Organic Carbon Production by Microphytoplankton", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="intppmicro", cmor_units="mol m-2 s-1", &
         cmor_standard_name="net_primary_mole_productivity_of_biomass_expressed_as_carbon_by_microphytoplankton", &
         cmor_long_name="Net Primary Organic Carbon Production by Microphytoplankton")

    cobalt%id_intpbn = register_diag_field(package_name, "intpbn_raw", axes(1:2), &
         init_time, "Nitrogen Production", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="intpbn", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_nitrogen_due_to_biological_production", &
         cmor_long_name="Nitrogen Production")

    cobalt%id_intpbp = register_diag_field(package_name, "intpbp_raw", axes(1:2), &
         init_time, "Phosphorus Production", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="intpbp", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_phosphorus_due_to_biological_production", &
         cmor_long_name="Phosphorus Production")

    cobalt%id_intpbfe = register_diag_field(package_name, "intpbfe_raw", axes(1:2), &
         init_time, "Iron Production", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="intpbfe", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_iron_due_to_biological_production", &
         cmor_long_name="Iron Production")

    cobalt%id_intpbsi = register_diag_field(package_name, "intpbsi_raw", axes(1:2), &
         init_time, "Silicon Production", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="intpbsi", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_silicon_due_to_biological_production", &
         cmor_long_name="Silicon Production")

    cobalt%id_intpcalcite = register_diag_field(package_name, "intpcalcite_raw", axes(1:2), &
         init_time, "Calcite Production", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="intpcalcite", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_calcite_expressed_as_carbon_due_to_biological_production", &
         cmor_long_name="Calcite Production")

    cobalt%id_intparag = register_diag_field(package_name, "intparag_raw", axes(1:2), &
         init_time, "Aragonite Production", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="intparag", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_aragonite_expressed_as_carbon_due_to_biological_production", &
         cmor_long_name="Aragonite Production")

! CHECK3: these should be AT 100m  - check how to output to conform to CMOR/data request
    cobalt%id_epc100 = register_diag_field(package_name, "epc100_raw", axes(1:2), &
         init_time, "Downward Flux of Particulate Organic Carbon", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="epc100", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_matter_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Downward Flux of Particulate Organic Carbon")

    cobalt%id_epn100 = register_diag_field(package_name, "epn100_raw", axes(1:2), &
         init_time, "Downward Flux of Particulate Nitrogen", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="epn100", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_nitrogen_in_sea_water", &
         cmor_long_name="Downward Flux of Particulate Nitrogen")

    cobalt%id_epp100 = register_diag_field(package_name, "epp100_raw", axes(1:2), &
         init_time, "Downward Flux of Particulate Phosphorus", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="epp100", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_organic_phosphorus_in_sea_water", &
         cmor_long_name="Downward Flux of Particulate Phosphorus")

    cobalt%id_epfe100 = register_diag_field(package_name, "epfe100_raw", axes(1:2), &
         init_time, "Downward Flux of Particulate Iron", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="epfe100", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_iron_in_sea_water", &
         cmor_long_name="Downward Flux of Particulate Iron")

    cobalt%id_epsi100 = register_diag_field(package_name, "epsi100_raw", axes(1:2), &
         init_time, "Downward Flux of Particulate Silicon", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="epsi100", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_particulate_silicon_in_sea_water", &
         cmor_long_name="Downward Flux of Particulate Silicon")

    cobalt%id_epcalc100 = register_diag_field(package_name, "epcalc100_raw", axes(1:2), &
         init_time, "Downward Flux of Calcite", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="epcalc100", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_calcite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Downward Flux of Calcite")

    cobalt%id_eparag100 = register_diag_field(package_name, "eparag100_raw", axes(1:2), &
         init_time, "Downward Flux of Aragonite", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="eparag100", cmor_units="mol m-2 s-1", &
         cmor_standard_name="sinking_mole_flux_of_aragonite_expressed_as_carbon_in_sea_water", &
         cmor_long_name="Downward Flux of Aragonite")

! vertically integrated
    cobalt%id_intdic = register_diag_field(package_name, "intdic_raw", axes(1:2), &
         init_time, "Dissolved Inorganic Carbon Content", "kg m-2", &
         missing_value = missing_value1, cmor_field_name="intdic", cmor_units="kg m-2", &
         cmor_standard_name="ocean_mass_content_of_dissolved_inorganic_carbon", &
         cmor_long_name="Dissolved Inorganic Carbon Content")

    cobalt%id_intdoc = register_diag_field(package_name, "intdoc_raw", axes(1:2), &
         init_time, "Dissolved Organic Carbon Content", "kg m-2", missing_value = missing_value1, &
         cmor_field_name="intdoc", cmor_units="kg m-2", &
         cmor_standard_name="ocean_mass_content_of_dissolved_organic_carbon", &
         cmor_long_name="Dissolved Organic Carbon Content")

    cobalt%id_intpoc = register_diag_field(package_name, "intpoc_raw", axes(1:2), &
         init_time, "Particulate Organic Carbon Content", "kg m-2", &
         missing_value = missing_value1, cmor_field_name="intpoc", cmor_units="kg m-2", &
         cmor_standard_name="ocean_mass_content_of_particulate_organic_matter_expressed_as_carbon", &
         cmor_long_name="Particulate Organic Carbon Content")

    cobalt%id_spco2 = register_diag_field(package_name, "spco2_raw", axes(1:2), &
         init_time, "Surface Aqueous Partial Pressure of CO2", "Pa", &
         missing_value = missing_value1, cmor_field_name="spco2", cmor_units="Pa", &
         cmor_standard_name="surface_partial_pressure_of_carbon_dioxide_in_sea_water", &
         cmor_long_name="Surface Aqueous Partial Pressure of CO2")

    cobalt%id_spco2nat = register_diag_field(package_name, "spco2nat_raw", axes(1:2), &
         init_time, "Natural Surface Aqueous Partial Pressure of CO2", "Pa", &
         missing_value = missing_value1, cmor_field_name="spco2nat", cmor_units="Pa", &
         cmor_standard_name="surface_partial_pressure_of_carbon_dioxide_natural_analogue_in_sea_water", &
         cmor_long_name="Natural Surface Aqueous Partial Pressure of CO2")

    cobalt%id_spco2abio = register_diag_field(package_name, "spco2abio_raw", axes(1:2), &
         init_time, "Abiotic Surface Aqueous Partial Pressure of CO2", "Pa", &
         missing_value = missing_value1, cmor_field_name="spco2abio", cmor_units="Pa", &
         cmor_standard_name="surface_partial_pressure_of_carbon_dioxide_abiotic_analogue_in_sea_water", &
         cmor_long_name="Abiotic Surface Aqueous Partial Pressure of CO2")

    cobalt%id_dpco2 = register_diag_field(package_name, "dpco2_raw", axes(1:2), &
         init_time, "Delta PCO2", "Pa", missing_value = missing_value1, cmor_field_name="dpco2", &
         cmor_units="Pa", &
         cmor_standard_name="surface_carbon_dioxide_partial_pressure_difference_between_sea_water_and_air", &
         cmor_long_name="Delta PCO2")

    cobalt%id_dpco2nat = register_diag_field(package_name, "dpco2nat_raw", axes(1:2), &
         init_time, "Natural Delta PCO2", "Pa", missing_value = missing_value1, &
         cmor_field_name="dpco2nat", cmor_units="Pa", &
         cmor_standard_name="surface_carbon_dioxide_natural_analogue_partial_pressure_difference_between_sea_water_and_air", &
         cmor_long_name="Natural Delta PCO2")

    cobalt%id_dpco2abio = register_diag_field(package_name, "dpco2abio_raw", axes(1:2), &
         init_time, "Abiotic Delta PCO2", "Pa", missing_value = missing_value1, &
         cmor_field_name="dpco2abio", cmor_units="Pa", &
         cmor_standard_name="surface_carbon_dioxide_abiotic_analogue_partial_pressure_difference_between_sea_water_and_air", &
         cmor_long_name="Abiotic Delta PCO2")

    cobalt%id_dpo2 = register_diag_field(package_name, "dpo2_raw", axes(1:2), &
         init_time, "Delta PO2", "Pa", missing_value = missing_value1, cmor_field_name="dpo2", &
         cmor_units="Pa", &
         cmor_standard_name="surface_molecular_oxygen_partial_pressure_difference_between_sea_water_and_air", &
         cmor_long_name="Delta PO2")

    cobalt%id_fgco2 = register_diag_field(package_name, "fgco2_raw", axes(1:2), &
         init_time, "Surface Downward Flux of Total CO2", "kg m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fgco2", cmor_units="kg m-2 s-1", &
         cmor_standard_name="surface_downward_mass_flux_of_carbon_dioxide_expressed_as_carbon", &
         cmor_long_name="Surface Downward Flux of Total CO2")

    cobalt%id_fgco2nat = register_diag_field(package_name, "fgco2nat_raw", axes(1:2), &
         init_time, "Surface Downward Flux of Natural CO2", "kg m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fgco2nat", cmor_units="kg m-2 s-1", &
         cmor_standard_name="surface_downward_mass_flux_of_carbon_dioxide_natural_analogue_expressed_as_carbon", &
         cmor_long_name="Surface Downward Flux of Natural CO2")

    cobalt%id_fgco2abio = register_diag_field(package_name, "fgco2abio_raw", axes(1:2), &
         init_time, "Surface Downward Flux of Abiotic CO2", "kg m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fgco2abio", cmor_units="kg m-2 s-1", &
         cmor_standard_name="surface_downward_mass_flux_of_carbon_dioxide_abiotic_analogue_expressed_as_carbon", &
         cmor_long_name="Surface Downward Flux of Abiotic CO2")

    cobalt%id_fg14co2abio = register_diag_field(package_name, "fg14co2abio_raw", axes(1:2), &
         init_time, "Surface Downward Flux of Abiotic 14CO2", "kg m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fg14co2abio", cmor_units="kg m-2 s-1", &
         cmor_standard_name="surface_downward_mass_flux_of_carbon14_dioxide_abiotic_analogue_expressed_as_carbon", &
         cmor_long_name="Surface Downward Flux of Abiotic 14CO2")

    cobalt%id_fgo2 = register_diag_field(package_name, "fgo2_raw", axes(1:2), &
         init_time, "Surface Downward Flux of O2", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="fgo2", cmor_units="mol m-2 s-1", &
         cmor_standard_name="surface_downward_mole_flux_of_molecular_oxygen", &
         cmor_long_name="Surface Downward Flux of O2")

    cobalt%id_icfriver = register_diag_field(package_name, "icfriver_raw", axes(1:2), &
         init_time, "Flux of Inorganic Carbon Into Ocean Surface by Runoff", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="icfriver", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_inorganic_carbon_due_to_runoff_and_sediment_dissolution", &
         cmor_long_name="Flux of Inorganic Carbon Into Ocean Surface by Runoff")

    cobalt%id_fric = register_diag_field(package_name, "fric_raw", axes(1:2), &
         init_time, "Downward Inorganic Carbon Flux at Ocean Bottom", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fric", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_inorganic_carbon_due_to_sedimentation", &
         cmor_long_name="Downward Inorganic Carbon Flux at Ocean Bottom")

    cobalt%id_ocfriver = register_diag_field(package_name, "ocfriver_raw", axes(1:2), &
         init_time, "Flux of Organic Carbon Into Ocean Surface by Runoff", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="ocfriver", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_organic_carbon_due_to_runoff_and_sediment_dissolution", &
         cmor_long_name="Flux of Organic Carbon Into Ocean Surface by Runoff")

    cobalt%id_froc = register_diag_field(package_name, "froc_raw", axes(1:2), &
         init_time, "Downward Organic Carbon Flux at Ocean Bottom", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="froc", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_organic_carbon_due_to_sedimentation", &
         cmor_long_name="Downward Organic Carbon Flux at Ocean Bottom")

    cobalt%id_intpn2 = register_diag_field(package_name, "intpn2_raw", axes(1:2), &
         init_time, "Nitrogen Fixation Rate in Ocean", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="intpn2", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_elemental_nitrogen_due_to_fixation", &
         cmor_long_name="Nitrogen Fixation Rate in Ocean")

    cobalt%id_fsn = register_diag_field(package_name, "fsn_raw", axes(1:2), &
         init_time, "Surface Downward Net Flux of Nitrogen", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fsn", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_elemental_nitrogen_due_to_deposition_and_fixation_and_runoff", &
         cmor_long_name="Surface Downward Net Flux of Nitrogen")

    cobalt%id_frn = register_diag_field(package_name, "frn_raw", axes(1:2), &
         init_time, "Nitrogen Loss to Sediments and through Denitrification", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="frn", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_elemental_nitrogen_due_to_denitrification_and_sedimentation", &
         cmor_long_name="Nitrogen Loss to Sediments and through Denitrification")

    cobalt%id_fsfe = register_diag_field(package_name, "fsfe_raw", axes(1:2), &
         init_time, "Surface Downward Net Flux of Iron", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fsfe", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_iron_due_to_deposition_and_runoff_and_sediment_dissolution", &
         cmor_long_name="Surface Downward Net Flux of Iron")

    cobalt%id_frfe = register_diag_field(package_name, "frfe_raw", axes(1:2), &
         init_time, "Iron Loss to Sediments", "mol m-2 s-1", missing_value = missing_value1, &
         cmor_field_name="frfe", cmor_units="mol m-2 s-1", &
         cmor_standard_name="minus_tendency_of_ocean_mole_content_of_iron_due_to_sedimentation", &
         cmor_long_name="Iron Loss to Sediments")

    cobalt%id_o2min = register_diag_field(package_name, "o2min_raw", axes(1:2), &
         init_time, "Oxygen Minimum Concentration", "mol m-3", missing_value = missing_value1, &
         cmor_field_name="o2min", cmor_units="mol m-3", &
         cmor_standard_name="mole_concentration_of_dissolved_molecular_oxygen_in_sea_water_at_shallowest_local_minimum_in_vertical_profile", &
         cmor_long_name="Oxygen Minimum Concentration")

    cobalt%id_zo2min = register_diag_field(package_name, "zo2min_raw", axes(1:2), &
         init_time, "Depth of Oxygen Minimum Concentration", "m", missing_value = missing_value1, &
         cmor_field_name="zo2min", cmor_units="m", &
         cmor_standard_name="depth_at_shallowest_local_minimum_in_vertical_profile_of_mole_concentration_of_dissolved_molecular_oxygen_in_sea_water", &
         cmor_long_name="Depth of Oxygen Minimum Concentration")

    cobalt%id_zsatcalc = register_diag_field(package_name, "zsatcalc_raw", axes(1:2), &
         init_time, "Calcite Saturation Depth", "m", missing_value = missing_value1, &
         cmor_field_name="zsatcalc", cmor_units="m", &
         cmor_standard_name="minimum_depth_of_calcite_undersaturation_in_sea_water", &
         cmor_long_name="Calcite Saturation Depth")

    cobalt%id_zsatarag = register_diag_field(package_name, "zsatarag_raw", axes(1:2), &
         init_time, "Aragonite Saturation Depth", "m", missing_value = missing_value1, &
         cmor_field_name="zsatarag", cmor_units="m", &
         cmor_standard_name="minimum_depth_of_aragonite_undersaturation_in_sea_water", &
         cmor_long_name="Aragonite Saturation Depth")

    cobalt%id_fddtdic = register_diag_field(package_name, "fddtdic_raw", axes(1:2), &
         init_time, "Rate of Change of Net Dissolved Inorganic Carbon", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fddtdic", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_carbon", &
         cmor_long_name="Rate of Change of Net Dissolved Inorganic Carbon")

    cobalt%id_fddtdin = register_diag_field(package_name, "fddtdin_raw", axes(1:2), &
         init_time, "Rate of Change of Net Dissolved Inorganic Nitrogen", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fddtdin", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_nitrogen", &
         cmor_long_name="Rate of Change of Net Dissolved Inorganic Nitrogen")

    cobalt%id_fddtdip = register_diag_field(package_name, "fddtdip_raw", axes(1:2), &
         init_time, "Rate of Change of Net Dissolved Inorganic Phosphorus", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fddtdip", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_phosphorus", &
         cmor_long_name="Rate of Change of Net Dissolved Inorganic Phosphorus")

    cobalt%id_fddtdife = register_diag_field(package_name, "fddtdife_raw", axes(1:2), &
         init_time, "Rate of Change of Net Dissolved Inorganic Iron", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fddtdife", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_iron", &
         cmor_long_name="Rate of Change of Net Dissolved Inorganic Iron")

    cobalt%id_fddtdisi = register_diag_field(package_name, "fddtdisi_raw", axes(1:2), &
         init_time, "Rate of Change of Net Dissolved Inorganic Silicon", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fddtdisi", cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_silicon", &
         cmor_long_name="Rate of Change of Net Dissolved Inorganic Silicon")

    cobalt%id_fddtalk = register_diag_field(package_name, "fddtalk_raw", axes(1:2), &
         init_time, "Rate of Change of Total Alkalinity", "mol m-2 s-1", &
         missing_value = missing_value1, cmor_field_name="fddtalk", cmor_units="mol m-2 s-1", &
         cmor_standard_name="integral_wrt_depth_of_tendency_of_sea_water_alkalinity_expressed_as_mole_equivalent", &
         cmor_long_name="Rate of Change of Total Alkalinity")

    cobalt%id_fbddtdic = register_diag_field(package_name, "fbddtdic_raw", axes(1:2), &
         init_time, "Rate of Change of Dissolved Inorganic Carbon due to Biological Activity", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="fbddtdic", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_carbon_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Inorganic Carbon due to Biological Activity")

    cobalt%id_fbddtdin = register_diag_field(package_name, "fbddtdin_raw", axes(1:2), &
         init_time, "Rate of Change of Dissolved Inorganic Nitrogen due to Biological Activity", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="fbddtdin", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_nitrogen_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Inorganic Nitrogen due to Biological Activity")

    cobalt%id_fbddtdip = register_diag_field(package_name, "fbddtdip_raw", axes(1:2), &
         init_time, "Rate of Change of Dissolved Inorganic Phosphorus due to Biological Activity", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="fbddtdip", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_phosphorus_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Inorganic Phosphorus due to Biological Activity")

    cobalt%id_fbddtdife = register_diag_field(package_name, "fbddtdife_raw", axes(1:2), &
         init_time, "Rate of Change of Dissolved Inorganic Iron due to Biological Activity", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="fbddtdife", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_iron_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Inorganic Iron due to Biological Activity")

    cobalt%id_fbddtdisi = register_diag_field(package_name, "fbddtdisi_raw", axes(1:2), &
         init_time, "Rate of Change of Dissolved Inorganic Silicon due to Biological Activity", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="fbddtdisi", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="tendency_of_ocean_mole_content_of_dissolved_inorganic_silicon_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Dissolved Inorganic Silicon due to Biological Activity")

    cobalt%id_fbddtalk = register_diag_field(package_name, "fbddtalk_raw", axes(1:2), &
         init_time, "Rate of Change of Biological Alkalinity due to Biological Activity", &
         "mol m-2 s-1", missing_value = missing_value1, cmor_field_name="fbddtalk", &
         cmor_units="mol m-2 s-1", &
         cmor_standard_name="integral_wrt_depth_of_tendency_of_sea_water_alkalinity_expressed_as_mole_equivalent_due_to_biological_processes", &
         cmor_long_name="Rate of Change of Biological Alkalinity due to Biological Activity")

    !DMS diagnostics

     cobalt%id_weight_dmsp_strat = register_diag_field(package_name, "weight_dmsp_strat", &
         axes(1:2), &
         init_time, "Weight dmsp stratified model", "unitless", missing_value = missing_value1)

     cobalt%id_dmsp_zeu = register_diag_field(package_name, "dmsp_zeu", axes(1:2), &
         init_time, "Euphotic layer depth for DMSP", "m", missing_value = missing_value1)

     cobalt%id_dmspos_mix     = register_diag_field(package_name, "dmspos_mix", axes(1:2), &
         init_time, "Surface concentration of DMSP using Mixed Model", "mol m-3", &
         missing_value = missing_value1)

     cobalt%id_dmspos_strat   = register_diag_field(package_name, "dmspos_strat", axes(1:2), &
         init_time, "Surface concentration of DMSP using Stratified Model", "mol m-3", &
         missing_value = missing_value1)

     cobalt%id_dmspos   = register_diag_field(package_name, "dmspos", axes(1:2), &
         init_time, "Surface concentration of DMSP", "mol m-3", missing_value = missing_value1)

     cobalt%id_irr_aclm_sfc_dayint = register_diag_field(package_name, "irr_aclm_sfc_dayint", &
         axes(1:2), &
         init_time, "Surface 24h int. irrad. over photacclim. time scale", "W m-2", &
         missing_value = missing_value1)

     cobalt%id_irr_sfc_dms = register_diag_field(package_name, "irr_sfc_dms", axes(1:2), &
         init_time, &
         "Surface 24h int. irrad. over photacclim. time scale in units for DMS calculation", &
         "mol photons m-2 d-1", missing_value = missing_value1)

     cobalt%id_dmsos_mix     = register_diag_field(package_name, "dmsos_mix", axes(1:2), &
         init_time, "Surface concentration of DMS using Mixed Model", "mol m-3", &
         missing_value = missing_value1)

     cobalt%id_dmsos_strat     = register_diag_field(package_name, "dmsos_strat", axes(1:2), &
         init_time, "Surface concentration of DMS using Stratified Model", "mol m-3", &
         missing_value = missing_value1)

     cobalt%id_dmsos     = register_diag_field(package_name, "dmsos", axes(1:2), &
         init_time, "Surface concentration of DMS", "mol m-3", missing_value = missing_value1)

     cobalt%id_chl_dmsp  = register_diag_field(package_name, "chl_dmsp_sfc", axes(1:2), &
         init_time, "Chl used for DMSp calculation", "mg/m3", missing_value = missing_value1)


!==============================================================================================================

  end subroutine cobalt_reg_diagnostics
end module COBALT_reg_diag
