!> COBALT_utils module consists of utility subroutines
!! to be used by generic COBALT module.
!<----------------------------------------------------------------
module COBALT_diag

  use cobalt_glbl      

  use time_manager_mod,  only: time_type

  use g_tracer_utils, only : g_send_data

  implicit none; private
  public cobalt_send_diagnostics

  contains

    !> subroutine that handles send_diag calls for COBALT phyto, zoo, and bact      
    subroutine cobalt_send_diagnostics(model_time,grid_tmask,Temp,rho_dzt,dzt,&
                                 isc,iec,jsc,jec,nk,tau,phyto,zoo,bact,cobalt)         
      type(time_type),                           intent(in) :: model_time
      real, dimension(:,:,:),                    pointer :: grid_tmask
      real, dimension(isc:,jsc:,:),                    intent(in) :: Temp
      real, dimension(isc:,jsc:,:),                    intent(in) :: rho_dzt
      real, dimension(isc:,jsc:,:),                    intent(in) :: dzt
      integer,                                   intent(in) :: isc
      integer,                                   intent(in) :: iec
      integer,                                   intent(in) :: jsc
      integer,                                   intent(in) :: jec
      integer,                                   intent(in) :: nk 
      integer,                                   intent(in) :: tau
      type(phytoplankton), dimension(NUM_PHYTO), intent(in) :: phyto
      type(zooplankton), dimension(NUM_ZOO),     intent(in) :: zoo
      type(bacteria), dimension(NUM_BACT),       intent(in) :: bact
      type(generic_COBALT_type),                 intent(inout) :: cobalt
      !> local variables
      integer :: n
      logical :: used  
            
!
!---------------------------------------------------------------------
!
! Send phytoplankton diagnostic data

      do n= 1, NUM_PHYTO
            used = g_send_data(phyto(n)%id_P_C_max,   phyto(n)%P_C_max,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_alpha,     phyto(n)%alpha,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_bresp,     phyto(n)%bresp,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_def_fe,     phyto(n)%def_fe,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_felim,      phyto(n)%felim,            &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_irrlim,     phyto(n)%irrlim,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jzloss_n, phyto(n)%jzloss_n*rho_dzt,      &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jaggloss_n, phyto(n)%jaggloss_n*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jvirloss_n, phyto(n)%jvirloss_n*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jmortloss_n, phyto(n)%jmortloss_n*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jexuloss_n, phyto(n)%jexuloss_n*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_juptake_fe, phyto(n)%juptake_fe*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_juptake_nh4, phyto(n)%juptake_nh4*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_juptake_no3, phyto(n)%juptake_no3*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_juptake_po4, phyto(n)%juptake_po4*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
 
            used = g_send_data(phyto(n)%id_jprod_n, phyto(n)%jprod_n*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_liebig_lim,phyto(n)%liebig_lim,          &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_mu,        phyto(n)%mu,                  &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_nh4lim,     phyto(n)%nh4lim,             &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_no3lim,     phyto(n)%no3lim,             &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_po4lim,     phyto(n)%po4lim,             &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_o2lim,     phyto(n)%o2lim,             &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_q_fe_2_n,   phyto(n)%q_fe_2_n,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_q_p_2_n,   phyto(n)%q_p_2_n,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_silim, phyto(n)%silim,       &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_q_si_2_n, phyto(n)%q_si_2_n,       &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_theta,      phyto(n)%theta,              &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_chl,      phyto(n)%chl,              &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_f_mu_mem,      phyto(n)%f_mu_mem,              &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_mu_mix,      phyto(n)%mu_mix,            &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_stress_fac, phyto(n)%stress_fac,         &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_vmove,    phyto(n)%vmove,              &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
      enddo

      do n=2,3
            used = g_send_data(phyto(n)%id_juptake_sio4, phyto(n)%juptake_sio4*rho_dzt,   &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
      enddo 

      used = g_send_data(phyto(DIAZO)%id_juptake_n2, phyto(DIAZO)%juptake_n2*rho_dzt,   &
      model_time, rmask = grid_tmask,&
      is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

    !--------------------------------------------------------------------------------------
    ! Send bacterial diagnostic data
    !

       used = g_send_data(bact(1)%id_jzloss_n, bact(1)%jzloss_n*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_jvirloss_n, bact(1)%jvirloss_n*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_juptake_ldon, bact(1)%juptake_ldon*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_juptake_ldop, bact(1)%juptake_ldop*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_juptake_po4, bact(1)%juptake_po4*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_jprod_nh4, bact(1)%jprod_nh4*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_jprod_po4, bact(1)%jprod_po4*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_jprod_n, bact(1)%jprod_n*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_jprod_n_het, bact(1)%jprod_n_het*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_jprod_n_nitrif, bact(1)%jprod_n_nitrif*rho_dzt,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_jprod_n_amx, bact(1)%jprod_n_amx*rho_dzt,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_mu_h, bact(1)%mu_h,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_mu_cstar, bact(1)%mu_cstar,     &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_bhet, bact(1)%bhet,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_o2lim, bact(1)%o2lim,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_ldonlim, bact(1)%ldonlim,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(bact(1)%id_temp_lim, bact(1)%temp_lim,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

    !--------------------------------------------------------------------------------------
    ! Send zooplankton diagnostic data
    !

       do n= 1, NUM_ZOO
            used = g_send_data(zoo(n)%id_jzloss_n, zoo(n)%jzloss_n*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jhploss_n, zoo(n)%jhploss_n*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jingest_n, zoo(n)%jingest_n*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jingest_p, zoo(n)%jingest_p*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jingest_sio2, zoo(n)%jingest_sio2*rho_dzt,      &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jingest_fe, zoo(n)%jingest_fe*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_ndet, zoo(n)%jprod_ndet*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_pdet, zoo(n)%jprod_pdet*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_ldon, zoo(n)%jprod_ldon*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_ldop, zoo(n)%jprod_ldop*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_sldon, zoo(n)%jprod_sldon*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_sldop, zoo(n)%jprod_sldop*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_srdon, zoo(n)%jprod_srdon*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_srdop, zoo(n)%jprod_srdop*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_fed,  zoo(n)%jprod_fed*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_fedet, zoo(n)%jprod_fedet*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_sidet, zoo(n)%jprod_sidet*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_sio4, zoo(n)%jprod_sio4*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_po4,  zoo(n)%jprod_po4*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_nh4,  zoo(n)%jprod_nh4*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_n,   zoo(n)%jprod_n*rho_dzt,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_o2lim, zoo(n)%o2lim,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_temp_lim, zoo(n)%temp_lim,           &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_vmove,    zoo(n)%vmove,              &
            model_time, rmask = grid_tmask,&
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       enddo

    !
    ! Production diagnostics
    !
       used = g_send_data(cobalt%id_jprod_cadet_arag, cobalt%jprod_cadet_arag * rho_dzt, &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_cadet_calc, cobalt%jprod_cadet_calc * rho_dzt, &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_ndet, cobalt%jprod_ndet*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
    !    used = g_send_data(cobalt%id_jprod_pdet, cobalt%jprod_pdet*rho_dzt,           &
    !    model_time, rmask = grid_tmask,&
    !    is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
    !    used = g_send_data(cobalt%id_jprod_srdon, cobalt%jprod_srdon*rho_dzt,           &
    !    model_time, rmask = grid_tmask,&
    !    is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
    !    used = g_send_data(cobalt%id_jprod_sldon, cobalt%jprod_sldon*rho_dzt,           &
    !    model_time, rmask = grid_tmask,&
    !    is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
    !    used = g_send_data(cobalt%id_jprod_ldon, cobalt%jprod_ldon*rho_dzt,           &
    !    model_time, rmask = grid_tmask,&
    !    is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
    !    used = g_send_data(cobalt%id_jprod_srdop, cobalt%jprod_srdop*rho_dzt,           &
    !    model_time, rmask = grid_tmask,&
    !    is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
    !    used = g_send_data(cobalt%id_jprod_sldop, cobalt%jprod_sldop*rho_dzt,           &
    !    model_time, rmask = grid_tmask,&
    !    is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
    !    used = g_send_data(cobalt%id_jprod_ldop, cobalt%jprod_ldop*rho_dzt,           &
    !    model_time, rmask = grid_tmask,&
    !    is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_nh4, cobalt%jprod_nh4*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_nh4_plus_btm, cobalt%jprod_nh4_plus_btm*rho_dzt, &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_po4, cobalt%jprod_po4*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_net_phyto_resp, cobalt%net_phyto_resp*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_fed, cobalt%jprod_fed*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_fedet,  cobalt%jprod_fedet*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_sidet, cobalt%jprod_sidet*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_sio4, cobalt%jprod_sio4*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jprod_lithdet, cobalt%jprod_lithdet*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jdiss_cadet_arag, cobalt%jdiss_cadet_arag*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jdiss_cadet_calc, cobalt%jdiss_cadet_calc*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jdiss_sidet, cobalt%jdiss_sidet*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jremin_ndet, cobalt%jremin_ndet*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jremin_pdet, cobalt%jremin_pdet*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jremin_fedet, cobalt%jremin_fedet*rho_dzt,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jfed,       cobalt%jfed*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!liao
       used = g_send_data(cobalt%id_jfedc,       cobalt%jfed,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!liao
       used = g_send_data(cobalt%id_jfe_ads,       cobalt%jfe_ads*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jfe_coast, cobalt%jfe_coast*rho_dzt,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jfe_iceberg, cobalt%jfe_iceberg*rho_dzt,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jno3_iceberg, cobalt%jno3_iceberg*rho_dzt,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_jpo4_iceberg, cobalt%jpo4_iceberg*rho_dzt,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_kfe_eq_lig, log10(cobalt%kfe_eq_lig+epsln),         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_feprime, cobalt%feprime,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_ligand, cobalt%ligand,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fe_sol, cobalt%fe_sol,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_expkT,       cobalt%expkT,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_expkreminT,       cobalt%expkreminT,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_hp_o2lim, cobalt%hp_o2lim,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_hp_temp_lim, cobalt%hp_temp_lim,         &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_irr_inst,      cobalt%irr_inst,              &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_irr_mix,       cobalt%irr_mix,               &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_irr_aclm_inst,  cobalt%irr_aclm_inst,        &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_irr_aclm,       cobalt%f_irr_aclm,               &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_irr_aclm_z,       cobalt%f_irr_aclm_z,               &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_chl2sfcchl,     cobalt%chl2sfcchl,    &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_jno3denit_wc,  cobalt%jno3denit_wc*rho_dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_juptake_nh4amx, cobalt%juptake_nh4amx*rho_dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_juptake_no3amx, cobalt%juptake_no3amx*rho_dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_jo2resp_wc,  cobalt%jo2resp_wc*rho_dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_jprod_no3nitrif, cobalt%jprod_no3nitrif*rho_dzt,       &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_juptake_nh4nitrif, cobalt%juptake_nh4nitrif*rho_dzt,       &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_jprod_n2amx, cobalt%jprod_n2amx*rho_dzt,       &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_tot_layer_int_c, cobalt%tot_layer_int_c,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_tot_layer_int_fe,cobalt%tot_layer_int_fe,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_tot_layer_int_n,cobalt%tot_layer_int_n,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_tot_layer_int_p,cobalt%tot_layer_int_p,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_tot_layer_int_si,cobalt%tot_layer_int_si,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_tot_layer_int_o2,cobalt%tot_layer_int_o2,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_tot_layer_int_alk,cobalt%tot_layer_int_alk,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_total_filter_feeding,cobalt%total_filter_feeding,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_nlg_diatoms,cobalt%nlg_diatoms,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_nmd_diatoms,cobalt%nmd_diatoms,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_q_si_2_n_lg_diatoms,cobalt%q_si_2_n_lg_diatoms,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_q_si_2_n_md_diatoms,cobalt%q_si_2_n_md_diatoms,&
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
        used = g_send_data(cobalt%id_co2_csurf,      cobalt%co2_csurf,              &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_pco2_csurf,      cobalt%pco2_csurf,              &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_co2_alpha,      cobalt%co2_alpha,              &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_nh3_csurf,      cobalt%nh3_csurf,              &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_nh3_alpha,      cobalt%nh3_alpha,              &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
   !     used = g_send_data(cobalt%id_fcadet_arag_btm,   cobalt%fcadet_arag_btm,      &
   !     model_time, rmask = grid_tmask(:,:,1),&
   !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
   !     used = g_send_data(cobalt%id_fcadet_calc_btm,   cobalt%fcadet_calc_btm,      &
   !     model_time, rmask = grid_tmask(:,:,1),&
   !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
   !     used = g_send_data(cobalt%id_ffedet_btm,   cobalt%ffedet_btm,             &
   !     model_time, rmask = grid_tmask(:,:,1),&
   !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
   !     used = g_send_data(cobalt%id_fndet_btm,    cobalt%fndet_btm,              &
   !     model_time, rmask = grid_tmask(:,:,1),&
   !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
   !     used = g_send_data(cobalt%id_fpdet_btm,    cobalt%fpdet_btm,              &
   !     model_time, rmask = grid_tmask(:,:,1),&
   !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
   !     used = g_send_data(cobalt%id_fsidet_btm,   cobalt%fsidet_btm,             &
   !     model_time, rmask = grid_tmask(:,:,1),&
   !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_ffetot_btm,   cobalt%ffetot_btm,             &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fntot_btm,    cobalt%fntot_btm,              &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fptot_btm,    cobalt%fptot_btm,              &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fsitot_btm,   cobalt%fsitot_btm,             &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_flithdet_btm,   cobalt%flithdet_btm,             &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fcased_burial, cobalt%fcased_burial,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fcased_redis,  cobalt%fcased_redis,          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fcased_redis_surfresp,cobalt%fcased_redis_surfresp, &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_cased_redis_coef,  cobalt%cased_redis_coef, &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_cased_redis_delz,  cobalt%cased_redis_delz, &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_ffe_sed,       cobalt%ffe_sed,               &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_ffe_geotherm,  cobalt%ffe_geotherm,          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_ffe_iceberg,  cobalt%ffe_iceberg,          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fnfeso4red_sed,cobalt%fnfeso4red_sed,        &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fno3denit_sed, cobalt%fno3denit_sed,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fnoxic_sed,    cobalt%fnoxic_sed,            &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_frac_burial,    cobalt%frac_burial,          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fn_burial,    cobalt%fn_burial,        &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_fp_burial,    cobalt%fp_burial,            &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_co3_sol_arag,  cobalt%co3_sol_arag,             &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_co3_sol_calc,  cobalt%co3_sol_calc,             &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_omega_arag,  cobalt%omega_arag,                 &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_omega_calc,  cobalt%omega_calc,                 &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_rho_test,  cobalt%rho_test,                 &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fcadet_arag, cobalt%p_cadet_arag(:,:,:,tau) * cobalt%Rho_0 * &
       cobalt%wsink * grid_tmask(:,:,:), &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fcadet_calc, cobalt%p_cadet_calc(:,:,:,tau) * cobalt%Rho_0 * &
       cobalt%wsink*grid_tmask(:,:,:), &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_ffedet,        cobalt%p_fedet(:,:,:,tau) * cobalt%Rho_0 * &
       cobalt%wsink * grid_tmask(:,:,:),&
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_flithdet,      cobalt%p_lithdet(:,:,:,tau) * cobalt%Rho_0 * &
       cobalt%wsink * grid_tmask(:,:,:),&
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fndet,         cobalt%p_ndet(:,:,:,tau) * cobalt%Rho_0 * &
       cobalt%wsink * grid_tmask(:,:,:),&
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fpdet,         cobalt%p_pdet(:,:,:,tau) * cobalt%Rho_0 * &
       cobalt%wsink * grid_tmask(:,:,:),&
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fsidet,        cobalt%p_sidet(:,:,:,tau)  * cobalt%Rho_0 * &
       cobalt%wsink  *grid_tmask(:,:,:),&
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_ffetot,(cobalt%p_fedet(:,:,:,tau)*cobalt%wsink + &
       cobalt%p_fesm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
       cobalt%p_femd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
       cobalt%p_felg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
       cobalt%p_fedi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))*cobalt%Rho_0*grid_tmask(:,:,:), &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fntot,(cobalt%p_ndet(:,:,:,tau)*cobalt%wsink + &
       cobalt%p_nsm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
       cobalt%p_nmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
       cobalt%p_nlg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
       cobalt%p_ndi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))*cobalt%Rho_0*grid_tmask(:,:,:), &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fptot,(cobalt%p_pdet(:,:,:,tau)*cobalt%wsink + &
       cobalt%p_psm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
       cobalt%p_pmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
       cobalt%p_plg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
       cobalt%p_pdi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))*cobalt%Rho_0*grid_tmask(:,:,:), &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_fsitot,(cobalt%p_sidet(:,:,:,tau)*cobalt%wsink + &
       cobalt%p_simd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
       cobalt%p_silg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:))*cobalt%Rho_0*grid_tmask(:,:,:), &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       used = g_send_data(cobalt%id_nphyto_tot,   (cobalt%p_ndi(:,:,:,tau) +  &
       cobalt%p_nlg(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau)), &
       model_time, rmask = grid_tmask(:,:,:),&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!
! Radiocarbon fields
!
       if (do_14c) then
         used = g_send_data(cobalt%id_b_di14c,        cobalt%b_di14c,                &
         model_time, rmask = grid_tmask(:,:,1),                                  &
         is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
         used = g_send_data(cobalt%id_c14_2_n,        cobalt%c14_2_n,                &
         model_time, rmask = grid_tmask,                                         &
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
         used = g_send_data(cobalt%id_c14o2_csurf,    cobalt%c14o2_csurf,            &
         model_time, rmask = grid_tmask(:,:,1),                                  &
         is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
         used = g_send_data(cobalt%id_c14o2_alpha,    cobalt%c14o2_alpha,            &
         model_time, rmask = grid_tmask(:,:,1),                                  &
         is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
         used = g_send_data(cobalt%id_fpo14c,         cobalt%fpo14c,                 &
         model_time, rmask = grid_tmask,                                         &
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
         used = g_send_data(cobalt%id_j14c_decay_dic, cobalt%j14c_decay_dic*rho_dzt, &
         model_time, rmask = grid_tmask,                                         &
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
         used = g_send_data(cobalt%id_j14c_decay_doc, cobalt%j14c_decay_doc*rho_dzt, &
         model_time, rmask = grid_tmask,&
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
         used = g_send_data(cobalt%id_j14c_reminp,    cobalt%j14c_reminp*rho_dzt,    &
         model_time, rmask = grid_tmask,                                         &
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
         used = g_send_data(cobalt%id_jdi14c,         cobalt%jdi14c*rho_dzt,         &
         model_time, rmask = grid_tmask,&
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
         used = g_send_data(cobalt%id_jdo14c,         cobalt%jdo14c*rho_dzt,         &
         model_time, rmask = grid_tmask,                                         &
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
       endif
!
! 2D COBALT fields
!
       used = g_send_data(cobalt%id_pco2surf,      cobalt%pco2_csurf,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_pnh3surf,      cobalt%pnh3_csurf,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_alk,       cobalt%p_alk(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_cadet_arag,cobalt%p_cadet_arag(:,:,1,tau),  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_cadet_calc,cobalt%p_cadet_calc(:,:,1,tau),  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_dic,       cobalt%p_dic(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_fed,       cobalt%p_fed(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_ldon,       cobalt%p_ldon(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_sldon,       cobalt%p_sldon(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_srdon,       cobalt%p_srdon(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_no3,       cobalt%p_no3(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_nh4,       cobalt%p_nh4(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_po4,       cobalt%p_po4(:,:,1,tau),         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_sio4,      cobalt%p_sio4(:,:,1,tau),        &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_htotal,    cobalt%f_htotal(:,:,1),          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_o2,       cobalt%p_o2(:,:,1,tau),           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_chl,       cobalt%f_chl(:,:,1),             &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_irr,      cobalt%irr_inst(:,:,1),        &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_irr_aclm,  cobalt%f_irr_aclm_sfc(:,:,1),       &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_irr_mem_dp,  cobalt%f_irr_mem_dp(:,:,1),       &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_mld_aclm,  cobalt%mld_aclm,       &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_temp,      Temp(:,:,1),                    &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_temp,      cobalt%btm_temp,                &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_temp_old,      cobalt%btm_temp_old,        &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_o2,      cobalt%btm_o2,                &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_o2_old,  cobalt%btm_o2_old,            &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_no3,      cobalt%btm_no3,                &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_alk,      cobalt%btm_alk,                &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_dic,      cobalt%btm_dic,                &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_grid_kmt_diag, cobalt%grid_kmt_diag,                &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_k_bot_diag, cobalt%k_bot_diag,                &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_rho_dzt_bot_diag, cobalt%rho_dzt_bot_diag,    &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_rho_dzt_kmt_diag, cobalt%rho_dzt_kmt_diag,    &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_htotal,      cobalt%btm_htotal,        &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_htotal_old,  cobalt%btm_htotal_old,    &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_cased_2d,      cobalt%cased_2d,                &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_co3_ion, cobalt%f_co3_ion(:,:,1),            &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_co3_sol_arag, cobalt%co3_sol_arag(:,:,1),  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_sfc_co3_sol_calc, cobalt%co3_sol_calc(:,:,1),  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_co3_ion, cobalt%btm_co3_ion,            &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_co3_ion_old, cobalt%btm_co3_ion_old,    &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_co3_sol_arag, cobalt%btm_co3_sol_arag,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_co3_sol_arag_old, cobalt%btm_co3_sol_arag_old,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_co3_sol_calc, cobalt%btm_co3_sol_calc,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_co3_sol_calc_old, cobalt%btm_co3_sol_calc_old,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_omega_calc, cobalt%btm_omega_calc,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_btm_omega_arag, cobalt%btm_omega_arag,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

       do n= 1, NUM_PHYTO
          used = g_send_data(phyto(n)%id_sfc_f_n, phyto(n)%f_n(:,:,1),            &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_chl, phyto(n)%chl(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_def_fe, phyto(n)%def_fe(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_felim, phyto(n)%felim(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_irrlim, phyto(n)%irrlim(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_theta, phyto(n)%theta(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_mu, phyto(n)%mu(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_po4lim, phyto(n)%po4lim(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_q_fe_2_n, phyto(n)%q_fe_2_n(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_q_p_2_n, phyto(n)%q_p_2_n(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_nh4lim, phyto(n)%nh4lim(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_sfc_no3lim, phyto(n)%no3lim(:,:,1),      &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
     !     used = g_send_data(phyto(n)%id_fn_btm, phyto(n)%fn_btm,      &
     !     model_time, rmask = grid_tmask(:,:,1),&
     !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
     !     used = g_send_data(phyto(n)%id_fp_btm, phyto(n)%fp_btm,      &
     !     model_time, rmask = grid_tmask(:,:,1),&
     !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
     !     used = g_send_data(phyto(n)%id_fsi_btm, phyto(n)%fsi_btm,      &
     !     model_time, rmask = grid_tmask(:,:,1),&
     !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
     !     used = g_send_data(phyto(n)%id_ffe_btm, phyto(n)%ffe_btm,      &
     !     model_time, rmask = grid_tmask(:,:,1),&
     !     is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       enddo

!
! Save river, depositon and bulk elemental fluxes
!
       used = g_send_data(cobalt%id_dep_dry_fed, cobalt%dry_fed,                          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_dry_lith, cobalt%dry_lith,                        &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_dry_nh4, cobalt%dry_nh4,                          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_dry_no3, cobalt%dry_no3,                          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_dry_po4, cobalt%dry_po4,                          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_wet_fed, cobalt%wet_fed,                          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_wet_lith, cobalt%wet_lith,                        &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_wet_nh4, cobalt%wet_nh4,                          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_wet_no3, cobalt%wet_no3,                          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_dep_wet_po4, cobalt%wet_po4,                          &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_alk, cobalt%runoff_flux_alk,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_dic, cobalt%runoff_flux_dic,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_di14c, cobalt%runoff_flux_di14c,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_fed, cobalt%runoff_flux_fed,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_lith, cobalt%runoff_flux_lith,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_no3, cobalt%runoff_flux_no3,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_ldon, cobalt%runoff_flux_ldon,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_sldon, cobalt%runoff_flux_sldon,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_srdon, cobalt%runoff_flux_srdon,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_ndet, cobalt%runoff_flux_ndet,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_pdet, cobalt%runoff_flux_pdet,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_po4, cobalt%runoff_flux_po4,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_ldop, cobalt%runoff_flux_ldop,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_sldop, cobalt%runoff_flux_sldop,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_runoff_flux_srdop, cobalt%runoff_flux_srdop,           &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
!
! Save 100m integral fluxes
!
       used = g_send_data(cobalt%id_jprod_allphytos_100, cobalt%jprod_allphytos_100,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_jprod_allphytos_200, cobalt%jprod_allphytos_200,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_jprod_diat_100, cobalt%jprod_diat_100,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       do n= 1, NUM_PHYTO  !{
          used = g_send_data(phyto(n)%id_jprod_n_100, phyto(n)%jprod_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_jprod_n_new_100, phyto(n)%jprod_n_new_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_jzloss_n_100, phyto(n)%jzloss_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_jexuloss_n_100, phyto(n)%jexuloss_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_jvirloss_n_100, phyto(n)%jvirloss_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_jmortloss_n_100, phyto(n)%jmortloss_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_jaggloss_n_100, phyto(n)%jaggloss_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(phyto(n)%id_f_n_100, phyto(n)%f_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       enddo !} n
       used = g_send_data(phyto(DIAZO)%id_jprod_n_n2_100, phyto(DIAZO)%jprod_n_n2_100,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       do n= 1, NUM_ZOO  !{
          used = g_send_data(zoo(n)%id_jprod_n_100, zoo(n)%jprod_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(zoo(n)%id_jingest_n_100, zoo(n)%jingest_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(zoo(n)%id_jremin_n_100, zoo(n)%jremin_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(zoo(n)%id_f_n_100, zoo(n)%f_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       enddo !} n

       do n= 1,2  !{
          used = g_send_data(zoo(n)%id_jzloss_n_100, zoo(n)%jzloss_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(zoo(n)%id_jprod_don_100, zoo(n)%jprod_don_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       enddo !} n

       do n= 2,3  !{
          used = g_send_data(zoo(n)%id_jhploss_n_100, zoo(n)%jhploss_n_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(zoo(n)%id_jprod_ndet_100, zoo(n)%jprod_ndet_100,         &
          model_time, rmask = grid_tmask(:,:,1),&
          is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       enddo !} n

        used = g_send_data(cobalt%id_hp_jingest_n_100, cobalt%hp_jingest_n_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_hp_jremin_n_100, cobalt%hp_jremin_n_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_hp_jprod_ndet_100, cobalt%hp_jprod_ndet_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(bact(1)%id_jprod_n_100, bact(1)%jprod_n_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(bact(1)%id_jzloss_n_100, bact(1)%jzloss_n_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(bact(1)%id_jvirloss_n_100, bact(1)%jvirloss_n_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(bact(1)%id_jremin_n_100, bact(1)%jremin_n_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(bact(1)%id_juptake_ldon_100, bact(1)%juptake_ldon_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(bact(1)%id_f_n_100, bact(1)%f_n_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_jprod_lithdet_100, cobalt%jprod_lithdet_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_jprod_sidet_100, cobalt%jprod_sidet_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_jprod_cadet_calc_100, cobalt%jprod_cadet_calc_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_jprod_cadet_arag_100, cobalt%jprod_cadet_arag_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_jremin_ndet_100, cobalt%jremin_ndet_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_jprod_mesozoo_200, cobalt%jprod_mesozoo_200,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_dp_fac, cobalt%dp_fac,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_daylength, cobalt%daylength,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_f_ndet_100, cobalt%f_ndet_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_f_don_100, cobalt%f_don_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_f_silg_100, cobalt%f_silg_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_f_simd_100, cobalt%f_simd_100,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_f_mesozoo_200, cobalt%f_mesozoo_200,         &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_fndet_100,     cobalt%fndet_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_fpdet_100,     cobalt%fpdet_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_fsidet_100,     cobalt%fsidet_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_flithdet_100,     cobalt%flithdet_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_fcadet_calc_100,     cobalt%fcadet_calc_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_fcadet_arag_100,     cobalt%fcadet_arag_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_ffedet_100,     cobalt%ffedet_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_fntot_100,     cobalt%fntot_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_fptot_100,     cobalt%fptot_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_fsitot_100,     cobalt%fsitot_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
        used = g_send_data(cobalt%id_ffetot_100,     cobalt%ffetot_100,                &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
!
!---------------------------------------------------------------------
! Save water column vertical integrals
!---------------------------------------------------------------------
!
       used = g_send_data(cobalt%id_wc_vert_int_c,    cobalt%wc_vert_int_c,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_dic,    cobalt%wc_vert_int_dic,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_doc,    cobalt%wc_vert_int_doc,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_poc,    cobalt%wc_vert_int_poc,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_n,    cobalt%wc_vert_int_n,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_p,    cobalt%wc_vert_int_p,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_fe,    cobalt%wc_vert_int_fe,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_si,    cobalt%wc_vert_int_si,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_o2,    cobalt%wc_vert_int_o2,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_alk,    cobalt%wc_vert_int_alk,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_chemoautopp, cobalt%wc_vert_int_chemoautopp, &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_net_phyto_resp, cobalt%wc_vert_int_net_phyto_resp, &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_npp,    cobalt%wc_vert_int_npp,         &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jdiss_sidet,    cobalt%wc_vert_int_jdiss_sidet,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jdiss_cadet,    cobalt%wc_vert_int_jdiss_cadet,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jo2resp,    cobalt%wc_vert_int_jo2resp,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jprod_cadet,    cobalt%wc_vert_int_jprod_cadet,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jno3denit,    cobalt%wc_vert_int_jno3denit,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jprod_no3nitrif,    cobalt%wc_vert_int_jprod_no3nitrif,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jprod_n2amx,    cobalt%wc_vert_int_jprod_n2amx,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_juptake_nh4,    cobalt%wc_vert_int_juptake_nh4,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jprod_nh4,    cobalt%wc_vert_int_jprod_nh4,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_juptake_no3,    cobalt%wc_vert_int_juptake_no3,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_nfix,    cobalt%wc_vert_int_nfix,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jfe_iceberg,    cobalt%wc_vert_int_jfe_iceberg,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jno3_iceberg,    cobalt%wc_vert_int_jno3_iceberg,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_wc_vert_int_jpo4_iceberg,    cobalt%wc_vert_int_jpo4_iceberg,  &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
!
!---------------------------------------------------------------------
! Save CaCO3 saturation and O2 minimum depths
!---------------------------------------------------------------------
!
       used = g_send_data(cobalt%id_o2min,         cobalt%o2min,                    &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_z_o2min,    cobalt%z_o2min,                     &
       model_time, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_z_sat_arag,    cobalt%z_sat_arag,               &
       model_time, mask = cobalt%mask_z_sat_arag, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
       used = g_send_data(cobalt%id_z_sat_calc,    cobalt%z_sat_calc,               &
       model_time, mask = cobalt%mask_z_sat_calc, rmask = grid_tmask(:,:,1),&
       is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!
!---------------------------------------------------------------------
! Send additional diagnostics  jgj 2015/10/26
!---------------------------------------------------------------------
!
       used = g_send_data(cobalt%id_jalk, cobalt%jalk*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!liao
       used = g_send_data(cobalt%id_jalkc, cobalt%jalk,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!liao
       used = g_send_data(cobalt%id_jalk_plus_btm, cobalt%jalk_plus_btm*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jdic, cobalt%jdic*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!liao
       used = g_send_data(cobalt%id_jdicc, cobalt%jdic,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jno3c, cobalt%jno3,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jpo4c, cobalt%jpo4,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jsio4c, cobalt%jsio4,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!liao

       used = g_send_data(cobalt%id_jdic_plus_btm, cobalt%jdic_plus_btm*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jnh4, cobalt%jnh4*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jndet, cobalt%jndet*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jnh4_plus_btm, cobalt%jnh4_plus_btm*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jo2_plus_btm, cobalt%jo2_plus_btm*rho_dzt,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!liao
       used = g_send_data(cobalt%id_jo2, cobalt%jo2*rho_dzt,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_jo2c, cobalt%jo2,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
!liao
!==============================================================================================================
!  2016/07/05 jgj  send temperature as a test

       used = g_send_data(cobalt%id_thetao,  Temp,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem Oyr and Omon: 3-D Marine Biogeochemical Tracer Fields
!
       used = g_send_data(cobalt%id_dissic,  cobalt%p_dic(:,:,:,tau) * cobalt%Rho_0,           &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! PENDING:
!        used = g_send_data(cobalt%id_dissicnat,                                                    &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

!        used = g_send_data(cobalt%id_dissicabio,                                                    &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

!        used = g_send_data(cobalt%id_dissi14cabio,                                                    &
!         model_time, rmask = grid_tmask,&
!         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       ! CAS updated to not include background values in accordance with CMIP results
       cobalt%dissoc(:,:,:) = cobalt%c_2_n * (cobalt%p_ldon(:,:,:,tau) + cobalt%p_sldon(:,:,:,tau) + &
                                           cobalt%p_srdon(:,:,:,tau) )

       used = g_send_data(cobalt%id_dissoc,  cobalt%dissoc * cobalt%Rho_0,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_phyc,  (cobalt%p_nlg(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau) +  &
       cobalt%p_ndi(:,:,:,tau)) * cobalt%c_2_n * cobalt%Rho_0, &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_zooc,  (cobalt%p_nlgz(:,:,:,tau) + cobalt%p_nsmz(:,:,:,tau) +  &
       cobalt%p_nmdz(:,:,:,tau)) * cobalt%c_2_n * cobalt%Rho_0, &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_bacc,  cobalt%p_nbact(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_detoc,  cobalt%p_ndet(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_calc,  cobalt%p_cadet_calc(:,:,:,tau) * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_arag,  cobalt%p_cadet_arag(:,:,:,tau) * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_phydiat,  (cobalt%nlg_diatoms + cobalt%nmd_diatoms) * &
              cobalt%c_2_n * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_phydiaz,  cobalt%p_ndi(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_phypico,  cobalt%p_nsm(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_phymisc,  (cobalt%p_nlg(:,:,:,tau)-cobalt%nlg_diatoms + &
              cobalt%p_nmd(:,:,:,tau)-cobalt%nmd_diatoms) * cobalt%c_2_n * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_zmicro,  cobalt%p_nsmz(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_zmeso,  (cobalt%p_nlgz(:,:,:,tau)+cobalt%p_nmdz(:,:,:,tau)) * cobalt%c_2_n * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

      used = g_send_data(cobalt%id_talk,  cobalt%p_alk(:,:,:,tau) * cobalt%Rho_0,       &
      model_time, rmask = grid_tmask,&
      is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! PENDING:
!        used = g_send_data(cobalt%id_talknat,
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CHECK3: this is using ntau=1
       used = g_send_data(cobalt%id_ph,  log10(cobalt%f_htotal+epsln) * (-1.0),       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! PENDING:
!        used = g_send_data(cobalt%id_phnat,  log10(cobalt%f_htotal+epsln) * -1.0,       &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! PENDING:
!        used = g_send_data(cobalt%id_phabio,  log10(cobalt%f_htotal+epsln) * -1.0,       &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_o2_cmip,  cobalt%p_o2(:,:,:,tau) * cobalt%Rho_0,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_o2sat,  cobalt%o2sat, &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_no3_cmip,  cobalt%p_no3(:,:,:,tau) * cobalt%Rho_0,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_nh4_cmip,  cobalt%p_nh4(:,:,:,tau) * cobalt%Rho_0,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_po4_cmip,  cobalt%p_po4(:,:,:,tau) * cobalt%Rho_0,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_dfe,  cobalt%p_fed(:,:,:,tau) * cobalt%Rho_0,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_si,  cobalt%p_sio4(:,:,:,tau) * cobalt%Rho_0,       &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_chl_cmip,  cobalt%f_chl * cobalt%Rho_0 / 1e9,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_chldiat,  (phyto(LARGE)%theta * cobalt%nlg_diatoms + &
              phyto(MEDIUM)%theta * cobalt%nmd_diatoms) * cobalt%c_2_n * cobalt%Rho_0 * 12e-3,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_chldiaz,  phyto(DIAZO)%theta * cobalt%p_ndi(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0 * 12e-3,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_chlpico,  phyto(SMALL)%theta * cobalt%p_nsm(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0 * 12e-3,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_chlmisc, (phyto(LARGE)%theta * (cobalt%p_nlg(:,:,:,tau)-cobalt%nlg_diatoms) + phyto(MEDIUM)%theta * (cobalt%p_nmd(:,:,:,tau)-cobalt%nmd_diatoms)) *  &
       cobalt%c_2_n * cobalt%Rho_0 * 12e-3,   &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_poc,  (cobalt%p_ndi(:,:,:,tau) + cobalt%p_nlg(:,:,:,tau) + &
       cobalt%p_nmd(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau) + cobalt%p_nbact(:,:,:,tau) +  cobalt%p_ndet(:,:,:,tau) + &
       cobalt%p_nsmz(:,:,:,tau) + cobalt%p_nmdz(:,:,:,tau) + cobalt%p_nlgz(:,:,:,tau)) * cobalt%Rho_0 * cobalt%c_2_n,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_pon,  (cobalt%p_ndi(:,:,:,tau) + cobalt%p_nlg(:,:,:,tau) + &
       cobalt%p_nmd(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau) + cobalt%p_nbact(:,:,:,tau) +  cobalt%p_ndet(:,:,:,tau) + &
       cobalt%p_nsmz(:,:,:,tau) + cobalt%p_nmdz(:,:,:,tau) + cobalt%p_nlgz(:,:,:,tau)) * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: added bacteria and more general accomodation of static but different p_2_n ratios
        used = g_send_data(cobalt%id_pop,  (cobalt%p_pdi(:,:,:,tau) + &
        cobalt%p_plg(:,:,:,tau) + cobalt%p_pmd(:,:,:,tau) +  cobalt%p_psm(:,:,:,tau) + &
        cobalt%p_pdet(:,:,:,tau) + zoo(1)%q_p_2_n * cobalt%p_nsmz(:,:,:,tau) + zoo(2)%q_p_2_n * cobalt%p_nmdz(:,:,:,tau) + &
        zoo(3)%q_p_2_n * cobalt%p_nlgz(:,:,:,tau) + bact(1)%q_p_2_n * cobalt%p_nbact(:,:,:,tau)) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: old code, delete once satisfied with new code above
!    if (cobalt%id_pop .gt. 0)            &
!        used = g_send_data(cobalt%id_pop,  ((phyto(DIAZO)%p_2_n_static * cobalt%p_ndi(:,:,:,tau)) + cobalt%p_nlg(:,:,:,tau) + &
!        cobalt%p_nsm(:,:,:,tau) + cobalt%p_nbact(:,:,:,tau) +  cobalt%p_pdet(:,:,:,tau) + &
!        cobalt%p_nsmz(:,:,:,tau) + cobalt%p_nmdz(:,:,:,tau) + cobalt%p_nlgz(:,:,:,tau)) * cobalt%Rho_0,  &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_bfe,  (cobalt%p_fedi(:,:,:,tau) + cobalt%p_felg(:,:,:,tau) + &
              cobalt%p_femd(:,:,:,tau) + cobalt%p_fesm(:,:,:,tau) + &
       cobalt%p_fedet(:,:,:,tau))  * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_bsi,  (cobalt%p_silg(:,:,:,tau) + cobalt%p_simd(:,:,:,tau) + &
              cobalt%p_sidet(:,:,:,tau))  * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_phyn,  (cobalt%p_nlg(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) +  &
       cobalt%p_nsm(:,:,:,tau) + cobalt%p_ndi(:,:,:,tau)) * cobalt%Rho_0, &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: added p_2_n ratios for other phyto groups
       used = g_send_data(cobalt%id_phyp,  (cobalt%p_pdi(:,:,:,tau) + &
       cobalt%p_plg(:,:,:,tau) + cobalt%p_pmd(:,:,:,tau) + cobalt%p_psm(:,:,:,tau) )* &
       cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: added p_2_n ratios for other phyto groups
!        used = g_send_data(cobalt%id_phyp,  (phyto(DIAZO)%p_2_n_static * cobalt%p_ndi(:,:,:,tau) + &
!        phyto(LARGE)%p_2_n_static * cobalt%p_nlg(:,:,:,tau) + phyto(MEDIUM)%p_2_n_static * cobalt%p_nmd(:,:,:,tau) + &
!        phyto(SMALL)%p_2_n_static * cobalt%p_nsm(:,:,:,tau) )* &
!        cobalt%Rho_0,  &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_phyfe,  (cobalt%p_fedi(:,:,:,tau) + cobalt%p_felg(:,:,:,tau) +  &
       cobalt%p_femd(:,:,:,tau) + cobalt%p_fesm(:,:,:,tau)) * cobalt%Rho_0, &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_physi,  (cobalt%p_silg(:,:,:,tau) + cobalt%p_simd(:,:,:,tau)) * &
              cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_co3,  cobalt%f_co3_ion * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! PENDING:
!        used = g_send_data(cobalt%id_co3nat,  cobalt%f_co3_ion * cobalt%Rho_0,  &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! PENDING:
!        used = g_send_data(cobalt%id_co3abio,  cobalt%f_co3_ion * cobalt%Rho_0,  &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_co3satcalc,  cobalt%co3_sol_calc * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

       used = g_send_data(cobalt%id_co3satarag,  cobalt%co3_sol_arag * cobalt%Rho_0,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem Oyr: Marine Biogeochemical 3-D Fields: Rates of Production and Removal
!
! CHECK3: using dzt for layer thickness
! Maybe just use cobalt%Rho_0 instead of rho_dzt / dzt in production terms
!
! also in Omon
       used = g_send_data(cobalt%id_pp,  (phyto(DIAZO)%jprod_n +  phyto(LARGE)%jprod_n +  &
       phyto(MEDIUM)%jprod_n + phyto(SMALL)%jprod_n) * rho_dzt * cobalt%c_2_n / dzt,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! also in Omon
       used = g_send_data(cobalt%id_pnitrate,  (phyto(DIAZO)%juptake_no3 +  phyto(LARGE)%juptake_no3 +  &
       phyto(MEDIUM)%juptake_no3 + phyto(SMALL)%juptake_no3) * rho_dzt * cobalt%c_2_n / dzt,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! Not requested
!        used = g_send_data(cobalt%id_pphosphate,  (phyto(DIAZO)%juptake_po4 +  phyto(LARGE)%juptake_po4 +  &
!        phyto(SMALL)%juptake_po4) * rho_dzt * cobalt%c_2_n / dzt,  &
!        model_time, rmask = grid_tmask,&
!        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! also in Omon
       used = g_send_data(cobalt%id_pbfe,  (phyto(DIAZO)%juptake_fe +  phyto(LARGE)%juptake_fe +  &
       phyto(MEDIUM)%juptake_fe + phyto(SMALL)%juptake_fe) * rho_dzt / dzt,  &
       model_time, rmask = grid_tmask,&
       is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! also in Omon
        used = g_send_data(cobalt%id_pbsi,  (phyto(LARGE)%juptake_sio4 + phyto(MEDIUM)%juptake_sio4) * &
               rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_pcalc,  cobalt%jprod_cadet_calc * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_parag,  cobalt%jprod_cadet_arag * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! also in Omon
        used = g_send_data(cobalt%id_expc, (cobalt%p_ndet(:,:,:,tau)*cobalt%wsink +  &
         cobalt%p_nsm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
         cobalt%p_nmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
         cobalt%p_nlg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
         cobalt%p_ndi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))* &
         cobalt%c_2_n*cobalt%Rho_0*grid_tmask(:,:,:), &
         model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

         used = g_send_data(cobalt%id_expn, &
         (cobalt%p_ndet(:,:,:,tau)*cobalt%wsink +  &
         cobalt%p_nsm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
         cobalt%p_nmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
         cobalt%p_nlg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
         cobalt%p_ndi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))* &
         cobalt%Rho_0*grid_tmask(:,:,:), &
         model_time, rmask = grid_tmask,&
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_expp, &
         (cobalt%p_pdet(:,:,:,tau)*cobalt%wsink +  &
         cobalt%p_psm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
         cobalt%p_pmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
         cobalt%p_plg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
         cobalt%p_pdi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))* &
         cobalt%Rho_0*grid_tmask(:,:,:), &
         model_time, rmask = grid_tmask,&
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_expfe, &
         (cobalt%p_fedet(:,:,:,tau)*cobalt%wsink +  &
         cobalt%p_fesm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
         cobalt%p_femd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
         cobalt%p_felg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
         cobalt%p_fedi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))* &
         cobalt%Rho_0*grid_tmask(:,:,:), &
         model_time, rmask = grid_tmask,&
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_expsi, &
         (cobalt%p_sidet(:,:,:,tau)*cobalt%wsink +  &
         cobalt%p_simd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
         cobalt%p_silg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:))* &
         cobalt%Rho_0*grid_tmask(:,:,:), &
         model_time, rmask = grid_tmask,&
         is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_expcalc,  cobalt%p_cadet_calc(:,:,:,tau) * cobalt%Rho_0 * cobalt%wsink,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_exparag,  cobalt%p_cadet_arag(:,:,:,tau) * cobalt%Rho_0 * cobalt%wsink,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: added jprod_nh4_plus_btm to calculate
        used = g_send_data(cobalt%id_remoc,  cobalt%jprod_nh4_plus_btm*cobalt%c_2_n*cobalt%Rho_0,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: added redissolution from sediment
        used = g_send_data(cobalt%id_dcalc,  cobalt%jdiss_cadet_calc_plus_btm*cobalt%Rho_0, &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: added redissolution from sediment
        used = g_send_data(cobalt%id_darag,  cobalt%jdiss_cadet_arag_plus_btm*cobalt%Rho_0, &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: fixed unit conversion on production from all groups by adding *rho_dzt,
        used = g_send_data(cobalt%id_ppdiat,  (phyto(LARGE)%jprod_n * phyto(LARGE)%silim + &
               phyto(MEDIUM)%jprod_n * phyto(MEDIUM)%silim) * rho_dzt * cobalt%c_2_n / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_ppdiaz,  phyto(DIAZO)%jprod_n * rho_dzt * cobalt%c_2_n / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_pppico,  phyto(SMALL)%jprod_n * rho_dzt * cobalt%c_2_n / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_ppmisc, ((phyto(LARGE)%jprod_n * (1.0 - phyto(LARGE)%silim)) + &
               (phyto(MEDIUM)%jprod_n * (1.0 - phyto(MEDIUM)%silim))) * rho_dzt * cobalt%c_2_n / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CHECK3 _z regridding
! CAS fixed conversion for all bddt terms
        used = g_send_data(cobalt%id_bddtdic,  cobalt%jdic_plus_btm * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_bddtdin,  cobalt%jdin_plus_btm * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_bddtdip,  cobalt%jpo4_plus_btm * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_bddtdife,  cobalt%jfed_plus_btm * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_bddtdisi,  cobalt%jsio4_plus_btm * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

        used = g_send_data(cobalt%id_bddtalk,  cobalt%jalk_plus_btm * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: fixed conversion
        used = g_send_data(cobalt%id_fescav,  cobalt%jfe_ads * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! CAS: fixed conversion
        used = g_send_data(cobalt%id_fediss,  cobalt%jremin_fedet * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

! also in Omon
! CAS: fixed conversion
        used = g_send_data(cobalt%id_graz,  (phyto(DIAZO)%jzloss_n +  phyto(LARGE)%jzloss_n +  &
        phyto(SMALL)%jzloss_n) * cobalt%c_2_n  * rho_dzt / dzt,  &
        model_time, rmask = grid_tmask,&
        is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem Omon: Marine Biogeochemical 2-D Surface Fields
!  Identical to Oyr 3-D Tracer fields but for surface only

        used = g_send_data(cobalt%id_dissicos,  cobalt%p_dic(:,:,1,tau) * cobalt%Rho_0,           &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_dissicnatos,                                                    &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_dissicabioos,                                                    &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_dissi14cabioos,                                                    &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_dissocos,  cobalt%dissoc(:,:,1) * cobalt%Rho_0,       &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_phycos,  (cobalt%p_nlg(:,:,1,tau) + cobalt%p_nmd(:,:,1,tau) +  &
        cobalt%p_nsm(:,:,1,tau) + cobalt%p_ndi(:,:,1,tau)) * cobalt%c_2_n * cobalt%Rho_0, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_zoocos,  (cobalt%p_nlgz(:,:,1,tau) + cobalt%p_nsmz(:,:,1,tau) +  &
        cobalt%p_nmdz(:,:,1,tau)) * cobalt%c_2_n * cobalt%Rho_0, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_baccos,  cobalt%p_nbact(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_detocos,  cobalt%p_ndet(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_calcos,  cobalt%p_cadet_calc(:,:,1,tau) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_aragos,  cobalt%p_cadet_arag(:,:,1,tau) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_phydiatos,  (cobalt%nlg_diatoms(:,:,1) + cobalt%nmd_diatoms(:,:,1)) * &
               cobalt%c_2_n * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_phydiazos,  cobalt%p_ndi(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_phypicoos,  cobalt%p_nsm(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_phymiscos,  ((cobalt%p_nlg(:,:,1,tau)-cobalt%nlg_diatoms(:,:,1)) + &
               (cobalt%p_nmd(:,:,1,tau)-cobalt%nmd_diatoms(:,:,1))) * cobalt%c_2_n * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_zmicroos,  cobalt%p_nsmz(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_zmesoos,  (cobalt%p_nlgz(:,:,1,tau)+cobalt%p_nmdz(:,:,1,tau)) * cobalt%c_2_n * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_talkos,  cobalt%p_alk(:,:,1,tau) * cobalt%Rho_0,       &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_talknatos,
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3: this is using ntau=1
        used = g_send_data(cobalt%id_phos,  log10(cobalt%f_htotal(:,:,1)+epsln) * (-1.0),       &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_phnatos,  log10(cobalt%f_htotal(:,:,1)+epsln) * -1.0,       &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_phabioos,  log10(cobalt%f_htotal(:,:,1)+epsln) * -1.0,       &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_o2os,  cobalt%p_o2(:,:,1,tau) * cobalt%Rho_0,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_o2satos,  cobalt%o2sat(:,:,1), &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_no3os,  cobalt%p_no3(:,:,1,tau) * cobalt%Rho_0,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_nh4os,  cobalt%p_nh4(:,:,1,tau) * cobalt%Rho_0,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_po4os,  cobalt%p_po4(:,:,1,tau) * cobalt%Rho_0,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_dfeos,  cobalt%p_fed(:,:,1,tau) * cobalt%Rho_0,       &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_sios,  cobalt%p_sio4(:,:,1,tau) * cobalt%Rho_0,       &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_chlos,  cobalt%f_chl(:,:,1) * cobalt%Rho_0 / 1e9,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_chldiatos,  (phyto(LARGE)%theta(:,:,1) * cobalt%nlg_diatoms(:,:,1) + &
        phyto(MEDIUM)%theta(:,:,1) * cobalt%nmd_diatoms(:,:,1)) * cobalt%c_2_n * cobalt%Rho_0 * 12e3,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_chldiazos,  phyto(DIAZO)%theta(:,:,1) * cobalt%p_ndi(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0 * 12e3,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_chlpicoos,  phyto(SMALL)%theta(:,:,1) * cobalt%p_nsm(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0 * 12e3,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_chlmiscos,  (phyto(LARGE)%theta(:,:,1) * (cobalt%p_nlg(:,:,1,tau)-cobalt%nlg_diatoms(:,:,1)) + &
        phyto(MEDIUM)%theta(:,:,1) * (cobalt%p_nmd(:,:,1,tau)-cobalt%nmd_diatoms(:,:,1))) * &
        cobalt%c_2_n * cobalt%Rho_0 * 12e3,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_ponos,  (cobalt%p_ndi(:,:,1,tau) + cobalt%p_nlg(:,:,1,tau) + &
        cobalt%p_nmd(:,:,1,tau) + cobalt%p_nsm(:,:,1,tau) + cobalt%p_nbact(:,:,1,tau) +  cobalt%p_ndet(:,:,1,tau) + &
        cobalt%p_nsmz(:,:,1,tau) + cobalt%p_nmdz(:,:,1,tau) + cobalt%p_nlgz(:,:,1,tau)) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_popos,  (cobalt%p_pdi(:,:,1,tau) + &
        cobalt%p_plg(:,:,1,tau) + cobalt%p_pmd(:,:,1,tau) + cobalt%p_psm(:,:,1,tau) + &
        bact(1)%q_p_2_n*cobalt%p_nbact(:,:,1,tau) +  cobalt%p_pdet(:,:,1,tau) + &
        zoo(1)%q_p_2_n*cobalt%p_nsmz(:,:,1,tau) + zoo(2)%q_p_2_n*cobalt%p_nmdz(:,:,1,tau) + &
        zoo(3)%q_p_2_n*cobalt%p_nlgz(:,:,1,tau)) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_popos,  ((phyto(DIAZO)%p_2_n_static * cobalt%p_ndi(:,:,1,tau)) + &
!        phyto(LARGE)%p_2_n_static*cobalt%p_nlg(:,:,1,tau) + &
!        phyto(MEDIUM)%p_2_n_static*cobalt%p_nmd(:,:,1,tau) + &
!        phyto(SMALL)%p_2_n_static*cobalt%p_nsm(:,:,1,tau) + &
!        bact(1)%q_p_2_n*cobalt%p_nbact(:,:,1,tau) +  cobalt%p_pdet(:,:,1,tau) + &
!        zoo(1)%q_p_2_n*cobalt%p_nsmz(:,:,1,tau) + zoo(2)%q_p_2_n*cobalt%p_nmdz(:,:,1,tau) + &
!        zoo(3)%q_p_2_n*cobalt%p_nlgz(:,:,1,tau)) * cobalt%Rho_0,  &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_bfeos,  (cobalt%p_fedi(:,:,1,tau) + cobalt%p_felg(:,:,1,tau) + &
        cobalt%p_femd(:,:,1,tau) + cobalt%p_fesm(:,:,1,tau) + &
        cobalt%p_fedet(:,:,1,tau))  * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_bsios,  (cobalt%p_silg(:,:,1,tau) + cobalt%p_simd(:,:,1,tau) + &
        cobalt%p_sidet(:,:,1,tau))  * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_phynos,  (cobalt%p_nlg(:,:,1,tau) + cobalt%p_nmd(:,:,1,tau) +  &
        cobalt%p_nsm(:,:,1,tau) + cobalt%p_ndi(:,:,1,tau)) * cobalt%Rho_0, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_phypos,  (cobalt%p_pdi(:,:,1,tau) + &
        cobalt%p_plg(:,:,1,tau) + cobalt%p_pmd(:,:,1,tau) + cobalt%p_psm(:,:,1,tau)) * &
        cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_phypos,  (phyto(DIAZO)%p_2_n_static*cobalt%p_ndi(:,:,1,tau) + &
!        phyto(LARGE)%p_2_n_static*cobalt%p_nlg(:,:,1,tau) + &
!        phyto(MEDIUM)%p_2_n_static*cobalt%p_nmd(:,:,1,tau) + &
!        phyto(SMALL)%p_2_n_static*cobalt%p_nsm(:,:,1,tau))*cobalt%Rho_0,  &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_phyfeos,  (cobalt%p_fedi(:,:,1,tau) + cobalt%p_felg(:,:,1,tau) +  &
        cobalt%p_femd(:,:,1,tau) + cobalt%p_fesm(:,:,1,tau)) * cobalt%Rho_0, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_physios, (cobalt%p_silg(:,:,1,tau) + cobalt%p_simd(:,:,1,tau)) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_co3os,  cobalt%f_co3_ion(:,:,1) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

         used = g_send_data(cobalt%id_co3natos,  cobalt%f_co3_ion(:,:,1) * cobalt%Rho_0,  &
         model_time, rmask = grid_tmask(:,:,1),&
         is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

         used = g_send_data(cobalt%id_co3abioos,  cobalt%f_co3_ion(:,:,1) * cobalt%Rho_0,  &
         model_time, rmask = grid_tmask(:,:,1),&
         is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_co3satcalcos,  cobalt%co3_sol_calc(:,:,1) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_co3sataragos,  cobalt%co3_sol_arag(:,:,1) * cobalt%Rho_0,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)


!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem Omon: 3-D Marine Biogeochemical 3-D Fields
! Tracers and rates above
! Limitation terms below
! 2018/07/18 changed to 2d terms
!
        used = g_send_data(cobalt%id_limndiat,  phyto(LARGE)%nlim_bw_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS, JGJ: not outputting/serving limndiaz because diazotrophs are not N limited
!        used = g_send_data(cobalt%id_limndiaz,  phyto(DIAZO)%nlim_bw_100, &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limnpico,  phyto(SMALL)%nlim_bw_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limnmisc,  phyto(LARGE)%nlim_bw_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limirrdiat,  phyto(LARGE)%irrlim_bw_100,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limirrdiaz,  phyto(DIAZO)%irrlim_bw_100,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limirrpico,  phyto(SMALL)%irrlim_bw_100,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limirrmisc,  phyto(LARGE)%irrlim_bw_100,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limfediat,  phyto(LARGE)%def_fe_bw_100,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limfediaz,  phyto(DIAZO)%def_fe_bw_100,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limfepico,  phyto(SMALL)%def_fe_bw_100,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limfemisc,  phyto(LARGE)%def_fe_bw_100,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limpdiat,  phyto(LARGE)%plim_bw_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limpdiaz,  phyto(DIAZO)%plim_bw_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limppico,  phyto(SMALL)%plim_bw_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_limpmisc,  phyto(LARGE)%plim_bw_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem Omon: Marine Biogeochemical 2-D Fields

        used = g_send_data(cobalt%id_intpp,  cobalt%jprod_allphytos_100 * cobalt%c_2_n, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intppnitrate,  (phyto(DIAZO)%jprod_n_new_100 +  phyto(LARGE)%jprod_n_new_100 +  &
        phyto(SMALL)%jprod_n_new_100) * cobalt%c_2_n,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intppdiat,  cobalt%jprod_diat_100 * cobalt%c_2_n,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intppdiaz,  phyto(DIAZO)%jprod_n_100 * cobalt%c_2_n,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intpppico,  phyto(SMALL)%jprod_n_100 * cobalt%c_2_n,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: can now use jprod_diat_100 to back out misc production
        used = g_send_data(cobalt%id_intppmisc,  (phyto(LARGE)%jprod_n_100 + phyto(MEDIUM)%jprod_n_100 - &
        cobalt%jprod_diat_100)  * cobalt%c_2_n,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: I think this is fine
        used = g_send_data(cobalt%id_intpbn,  cobalt%jprod_allphytos_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: I've added juptake_po4_100 in a manner analagous to iron below
        used = g_send_data(cobalt%id_intpbp, (phyto(DIAZO)%juptake_po4_100 +  phyto(LARGE)%juptake_po4_100 +  &
        phyto(MEDIUM)%juptake_po4_100 + phyto(SMALL)%juptake_po4_100),  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intpbfe,  (phyto(DIAZO)%juptake_fe_100 +  phyto(LARGE)%juptake_fe_100 +  &
        phyto(MEDIUM)%juptake_fe_100 + phyto(SMALL)%juptake_fe_100),  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intpbsi,  phyto(LARGE)%juptake_sio4_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intpcalcite,  cobalt%jprod_cadet_calc_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intparag,  cobalt%jprod_cadet_arag_100, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3: should be AT 100m
        used = g_send_data(cobalt%id_epc100,  cobalt%fndet_100 * cobalt%c_2_n,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3: should be AT 100m
        used = g_send_data(cobalt%id_epn100,  cobalt%fndet_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3: should be AT 100m
        used = g_send_data(cobalt%id_epp100,  cobalt%fpdet_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3: should be AT 100m
        used = g_send_data(cobalt%id_epfe100,  cobalt%ffedet_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3: should be AT 100m
        used = g_send_data(cobalt%id_epsi100,  cobalt%fsidet_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3: should be AT 100m
        used = g_send_data(cobalt%id_epcalc100,  cobalt%fcadet_calc_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3: should be AT 100m
        used = g_send_data(cobalt%id_eparag100,  cobalt%fcadet_arag_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intdic,  cobalt%wc_vert_int_dic*12e-3,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intdoc,  cobalt%wc_vert_int_doc*12e-3,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intpoc,  cobalt%wc_vert_int_poc*12e-3,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_spco2,  cobalt%pco2_csurf * 0.1013,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_spco2nat,  cobalt%pco2_csurf * 0.1013,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_spco2abio,  cobalt%pco2_csurf * 0.1013,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK3:
        used = g_send_data(cobalt%id_dpco2,  cobalt%deltap_dic * 0.1013,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_dpco2nat,  cobalt%dic_deltap * 0.1013,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_dpco2abio,  cobalt%dic_deltap * 0.1013,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_dpo2,  cobalt%deltap_o2 * 0.1013,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_fgco2,  cobalt%stf_gas_dic * 12e-3,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! PENDING:
!        used = g_send_data(cobalt%id_fgco2nat,
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_fgco2abio,
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_fg14co2abio,
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_fgo2,  cobalt%stf_gas_o2,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: Updated on 3/9/2021 to include sediment dissolution based on variable long name, also added
!      aragonite flux to the sediment (which is instantaneously redissolved)
        used = g_send_data(cobalt%id_icfriver,  cobalt%runoff_flux_dic + cobalt%fcased_redis + &
        cobalt%fcadet_arag_btm,model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: Updated on 3/9/2021 to exclude calcite dissolution but include the aragonite flux to
!      the bottom
        used = g_send_data(cobalt%id_fric,  cobalt%fcadet_calc_btm + cobalt%fcadet_arag_btm,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: variable includes release from sediment, but there is no organic carbon release from
!      sediment in COBALT
        used = g_send_data(cobalt%id_ocfriver, cobalt%c_2_n* &
        (cobalt%runoff_flux_ldon+cobalt%runoff_flux_sldon+cobalt%runoff_flux_srdon),&
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: Updated on 3/9/2021 to reflect that the total loss of organic carbon at sediments is
!      equal to the total flux, not just the burial
        used = g_send_data(cobalt%id_froc,cobalt%c_2_n*cobalt%fndet_btm, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_intpn2,  cobalt%wc_vert_int_nfix,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: Updated on 3/9/2021 to include nh4 deposition and riverine fluxes of organic nitrogen
        used = g_send_data(cobalt%id_fsn,  cobalt%runoff_flux_no3 + cobalt%dry_no3 + cobalt%wet_no3 + &
        cobalt%dry_nh4 + cobalt%wet_nh4 + cobalt%runoff_flux_ldon + cobalt%runoff_flux_sldon + &
        cobalt%runoff_flux_srdon + cobalt%wc_vert_int_nfix,  &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! JYL: Updated on 3/21/2021 to include anammox
        used = g_send_data(cobalt%id_frn,  cobalt%fno3denit_sed + cobalt%wc_vert_int_jno3denit + &
        cobalt%wc_vert_int_jprod_n2amx + cobalt%fn_burial, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CAS: Updated on 3/9/2021 to include ffe_iceberg
        used = g_send_data(cobalt%id_fsfe,  cobalt%runoff_flux_fed + cobalt%dry_fed + cobalt%wet_fed + &
        cobalt%ffe_sed+cobalt%ffe_geotherm+cobalt%ffe_iceberg, &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_frfe,  cobalt%ffedet_btm,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! revise calculation if providing to CMIP6
! 2016/08/15 - we will not be providing o2min, zo2min
!        used = g_send_data(cobalt%id_o2min,  cobalt%o2min,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_zo2min,  cobalt%zo2min,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! 2016/08/15 - we will not be providing zsatcalc, zsatarag
        used = g_send_data(cobalt%id_zsatcalc,  cobalt%z_sat_calc,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_zsatarag,  cobalt%z_sat_arag,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! 2016/08/15 - we will not be providing these fields
! CHECK: rate was computed offline for TOPAZ by saving a reference history file, dividing by secs_per_month and differencing monthly averages
! can we compute rates in the code this time?
!        used = g_send_data(cobalt%id_fddtdic,  cobalt%f_dic_int_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK: rate was computed offline for TOPAZ by saving a reference history file, dividing by secs_per_month and differencing monthly averages
! can we compute rates in the code this time?
!        used = g_send_data(cobalt%id_fddtdin,  cobalt%f_din_int_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK: rate was computed offline for TOPAZ by saving a reference history file, dividing by secs_per_month and differencing monthly averages
! can we compute rates in the code this time?
!        used = g_send_data(cobalt%id_fddtdip,  cobalt%f_po4_int_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK: rate was computed offline for TOPAZ by saving a reference history file, dividing by secs_per_month and differencing monthly averages
! can we compute rates in the code this time?
!        used = g_send_data(cobalt%id_fddtdife,  cobalt%f_fed_int_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK: rate was computed offline for TOPAZ by saving a reference history file, dividing by secs_per_month and differencing monthly averages
! can we compute rates in the code this time?
!        used = g_send_data(cobalt%id_fddtdisi,  cobalt%f_sio4_int_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

! CHECK: rate was computed offline for TOPAZ by saving a reference history file, dividing by secs_per_month and differencing monthly averages
! can we compute rates in the code this time?
!        used = g_send_data(cobalt%id_fddtalk,  cobalt%f_alk_int_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_fbddtdic,  cobalt%jdic_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_fbddtdin,  cobalt%jdin_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_fbddtdip,  cobalt%jpo4_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_fbddtdife,  cobalt%jfed_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_fbddtdisi,  cobalt%jsio4_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!        used = g_send_data(cobalt%id_fbddtalk,  cobalt%jalk_100,   &
!        model_time, rmask = grid_tmask(:,:,1),&
!        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!==============================================================================================================
! JGJ 2016/08/08 CMIP6 OcnBgchem day: Marine Biogeochemical daily fields
! chlos and phycos - in Omon and Oday
!==============================================================================================================
! 2016/08/15 JGJ: 100m integrals w/o CMOR conversion

        used = g_send_data(cobalt%id_f_dic_int_100,  cobalt%f_dic_int_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_f_din_int_100,  cobalt%f_din_int_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_f_po4_int_100,  cobalt%f_po4_int_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_f_fed_int_100,  cobalt%f_fed_int_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_f_sio4_int_100,  cobalt%f_sio4_int_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_f_alk_int_100,  cobalt%f_alk_int_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_jdic_100,  cobalt%jdic_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_jdin_100,  cobalt%jdin_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_jpo4_100,  cobalt%jpo4_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_jfed_100,  cobalt%jfed_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_jsio4_100,  cobalt%jsio4_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        used = g_send_data(cobalt%id_jalk_100,  cobalt%jalk_100,   &
        model_time, rmask = grid_tmask(:,:,1),&
        is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!==============================================================================================================

    end subroutine cobalt_send_diagnostics     
end module COBALT_diag   
