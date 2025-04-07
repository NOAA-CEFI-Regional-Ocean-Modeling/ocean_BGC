! The cobalt_send_diag module contains the subroutine that handles the sending of COBALT diagnostics.
!
! Flux and limitation diagnostics associated with the biological source and sink terms are saved at the end of the
! "ocean_cobalt_update_from_source" routine.  They thus reflect the biological source and sink terms that were based on
! and applied to the tracer concentrations at the beginning of the time step.  This is prior to the application of
! vertical mixing and sinking for that time step.  This is specified by setting "post_vertdiff" to false or omitting it
! from the cobalt_send_diag call.
!
! Prognostic tracer diagnostics are saved after vertical diffusion and mixing is applied (post_vertdiff = true).  This
! ensures that prognostic tracers take the values at the end of the time step.
! 
! Sinking and mixing diagnostics are also saved after the update to align with the implicit mixing and sinking 
! formulation.
!
module COBALT_send_diag

  use cobalt_types 

  use time_manager_mod,  only: time_type

  use g_tracer_utils, only : g_send_data,g_tracer_get_pointer,g_tracer_set_values
  use g_tracer_utils, only : g_tracer_type
  use FMS_co2calc_mod, only : FMS_co2calc, CO2_dope_vector

  implicit none; private
  public cobalt_send_diagnostics

  type(CO2_dope_vector) :: CO2_dope_vec

  contains

    !> subroutine that handles send_diag calls for COBALT phyto, zoo, and bact      
    subroutine cobalt_send_diagnostics(tracer_list, model_time,grid_tmask,Temp,Salt,rho_dzt,dzt,&
                                 isc,iec,jsc,jec,isd,ied,jsd,jed,nk,grid_kmt,tau,phyto,zoo,bact,cobalt,&
                                 post_vertdiff)         
      type(g_tracer_type),                       pointer :: tracer_list
      type(time_type),                           intent(in) :: model_time
      real, dimension(:,:,:),                    pointer :: grid_tmask
      real, dimension(isc:,jsc:,:),              intent(in) :: Temp, Salt
      real, dimension(isc:,jsc:,:),              intent(in) :: rho_dzt
      real, dimension(isc:,jsc:,:),              intent(in) :: dzt
      integer,                                   intent(in) :: isc
      integer,                                   intent(in) :: iec
      integer,                                   intent(in) :: jsc
      integer,                                   intent(in) :: jec
      integer,                                   intent(in) :: isd
      integer,                                   intent(in) :: ied
      integer,                                   intent(in) :: jsd
      integer,                                   intent(in) :: jed
      integer,                                   intent(in) :: nk
      integer, dimension(isc:,jsc:),             intent(in) :: grid_kmt
      integer,                                   intent(in) :: tau
      type(phytoplankton), dimension(NUM_PHYTO), intent(inout) :: phyto
      type(zooplankton), dimension(NUM_ZOO),     intent(inout) :: zoo
      type(bacteria), dimension(NUM_BACT),       intent(inout) :: bact
      type(generic_COBALT_type),                 intent(inout) :: cobalt
      logical,                                   intent(in), optional :: post_vertdiff
      !> local variables
      integer :: n,i,j,k
      logical :: used  
      logical :: is_post_vertdiff
      real :: drho_dzt
      integer, dimension(:,:), Allocatable :: k_bot
      real, dimension(:,:), Allocatable :: rho_dzt_100,rho_dzt_200,rho_dzt_bot
      integer :: k_100,k_200

      ! Set default value
      is_post_vertdiff = .false.

      ! Check if post_vertdiff is present
      if (present(post_vertdiff)) then
        is_post_vertdiff = post_vertdiff
      endif
     
      ! Determine whether the cobalt_send_diagnostics is saving in update_from_source or after vertdiff 
      ! (default to .false. if post_vertdiff is not present)
      select case (is_post_vertdiff)
        case (.true.)     ! Saving prognostic tracers after update from vertical diffusion and sinking

          !call g_tracer_get_common(isc,iec,jsc,jec,isd,ied,jsd,jed,nk,ntau,&
          !     grid_tmask=grid_tmask,grid_mask_coast=mask_coast,grid_kmt=grid_kmt)

          ! Get prognostics tracer fields via their pointers 
          call g_tracer_get_pointer(tracer_list,'alk'    ,'field',cobalt%p_alk    )
          call g_tracer_get_pointer(tracer_list,'cadet_arag','field',cobalt%p_cadet_arag)
          call g_tracer_get_pointer(tracer_list,'cadet_calc','field',cobalt%p_cadet_calc)
          call g_tracer_get_pointer(tracer_list,'dic'    ,'field',cobalt%p_dic    )
          call g_tracer_get_pointer(tracer_list,'fed'    ,'field',cobalt%p_fed    )
          call g_tracer_get_pointer(tracer_list,'fedi'   ,'field',cobalt%p_fedi   )
          call g_tracer_get_pointer(tracer_list,'felg'   ,'field',cobalt%p_felg   )
          call g_tracer_get_pointer(tracer_list,'femd'   ,'field',cobalt%p_femd   )
          call g_tracer_get_pointer(tracer_list,'fesm'   ,'field',cobalt%p_fesm   )
          call g_tracer_get_pointer(tracer_list,'fedet'  ,'field',cobalt%p_fedet  )
          call g_tracer_get_pointer(tracer_list,'ldon'   ,'field',cobalt%p_ldon   )
          call g_tracer_get_pointer(tracer_list,'ldop'   ,'field',cobalt%p_ldop   )
          call g_tracer_get_pointer(tracer_list,'nbact'  ,'field',cobalt%p_nbact  )
          call g_tracer_get_pointer(tracer_list,'ndet'   ,'field',cobalt%p_ndet   )
          call g_tracer_get_pointer(tracer_list,'ndi'    ,'field',cobalt%p_ndi    )
          call g_tracer_get_pointer(tracer_list,'nlg'    ,'field',cobalt%p_nlg    )
          call g_tracer_get_pointer(tracer_list,'nmd'    ,'field',cobalt%p_nmd    )
          call g_tracer_get_pointer(tracer_list,'nsm'    ,'field',cobalt%p_nsm    ) 
          call g_tracer_get_pointer(tracer_list,'nh4'    ,'field',cobalt%p_nh4    )
          call g_tracer_get_pointer(tracer_list,'no3'    ,'field',cobalt%p_no3    )
          call g_tracer_get_pointer(tracer_list,'o2'     ,'field',cobalt%p_o2     )
          call g_tracer_get_pointer(tracer_list,'pdi'    ,'field',cobalt%p_pdi    )
          call g_tracer_get_pointer(tracer_list,'plg'    ,'field',cobalt%p_plg    )
          call g_tracer_get_pointer(tracer_list,'pmd'    ,'field',cobalt%p_pmd    )
          call g_tracer_get_pointer(tracer_list,'psm'    ,'field',cobalt%p_psm    )
          call g_tracer_get_pointer(tracer_list,'pdet'   ,'field',cobalt%p_pdet   )
          call g_tracer_get_pointer(tracer_list,'po4'    ,'field',cobalt%p_po4    )
          call g_tracer_get_pointer(tracer_list,'srdon'  ,'field',cobalt%p_srdon )
          call g_tracer_get_pointer(tracer_list,'srdop'  ,'field',cobalt%p_srdop )
          call g_tracer_get_pointer(tracer_list,'sldon'  ,'field',cobalt%p_sldon )
          call g_tracer_get_pointer(tracer_list,'sldop'  ,'field',cobalt%p_sldop )
          call g_tracer_get_pointer(tracer_list,'sidet'  ,'field',cobalt%p_sidet  )
          call g_tracer_get_pointer(tracer_list,'silg'   ,'field',cobalt%p_silg   )
          call g_tracer_get_pointer(tracer_list,'simd'   ,'field',cobalt%p_simd   )
          call g_tracer_get_pointer(tracer_list,'sio4'   ,'field',cobalt%p_sio4   )
          call g_tracer_get_pointer(tracer_list,'nsmz'   ,'field',cobalt%p_nsmz   )
          call g_tracer_get_pointer(tracer_list,'nmdz'   ,'field',cobalt%p_nmdz   )
          call g_tracer_get_pointer(tracer_list,'nlgz'   ,'field',cobalt%p_nlgz   )
          call g_tracer_get_pointer(tracer_list,'lith'   ,'field',cobalt%p_lith   )
          call g_tracer_get_pointer(tracer_list,'lithdet','field',cobalt%p_lithdet)

          ! Flag to recalculate the carbon-system parameters at the end of the time step to align with the prognostic
          ! tracer values at the end of the time step.  If this is set to .False., variables are saved in
          ! update_from_source and apply to conditions prior to mixing and sinking.
          ! To do: Move nh3 exchange calculations here as well?
          if (cobalt%recalculate_carbon) then
            k=1
            do j = jsc, jec ; do i = isc, iec  !{
              cobalt%htotallo(i,j) = cobalt%htotal_scale_lo * cobalt%f_htotal(i,j,k)
              cobalt%htotalhi(i,j) = cobalt%htotal_scale_hi * cobalt%f_htotal(i,j,k)
            enddo; enddo ; !} i, j

            ! Use pointers
            call FMS_co2calc(CO2_dope_vec,grid_tmask(:,:,k),&
              Temp(:,:,k), Salt(:,:,k), &
              cobalt%p_dic(:,:,k,tau), &
              cobalt%p_po4(:,:,k,tau), &
              cobalt%p_sio4(:,:,k,tau), &
              cobalt%p_alk(:,:,k,tau), &
              cobalt%htotallo, cobalt%htotalhi,&
                                !InOut
              cobalt%f_htotal(:,:,k), &
                                !Optional In
              zt=cobalt%zt(:,:,k), &
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
                Temp(:,:,k), Salt(:,:,k), &
                cobalt%p_dic(:,:,k,tau), &
                cobalt%p_po4(:,:,k,tau), &
                cobalt%p_sio4(:,:,k,tau), &
                cobalt%p_alk(:,:,k,tau), &
                cobalt%htotallo, cobalt%htotalhi,&
                                !InOut
                cobalt%f_htotal(:,:,k), &
                                !Optional In
                zt=cobalt%zt(:,:,k), &
                                !OUT
                co3_ion=cobalt%f_co3_ion(:,:,k), &
                omega_arag=cobalt%omega_arag(:,:,k), &
                omega_calc=cobalt%omega_calc(:,:,k))
            enddo
            call g_tracer_set_values(tracer_list,'htotal','field',cobalt%f_htotal,isd,jsd)
            call g_tracer_set_values(tracer_list,'co3_ion','field',cobalt%f_co3_ion,isd,jsd)
            call g_tracer_set_values(tracer_list,'dic','alpha',cobalt%co2_alpha,isd,jsd)
            call g_tracer_set_values(tracer_list,'dic','csurf',cobalt%co2_csurf,isd,jsd)

            do k = 1, nk ; do j = jsc, jec ; do i = isc, iec   !{
              cobalt%co3_sol_arag(i,j,k) = cobalt%f_co3_ion(i,j,k) / max(cobalt%omega_arag(i,j,k),epsln)
              cobalt%co3_sol_calc(i,j,k) = cobalt%f_co3_ion(i,j,k) / max(cobalt%omega_calc(i,j,k),epsln)
              ! Update diatom and misc phytoplankton groups for diagnostics
              cobalt%nlg_diatoms(i,j,k)=phyto(LARGE)%f_n(i,j,k)*phyto(LARGE)%silim(i,j,k)
              cobalt%nmd_diatoms(i,j,k)=phyto(MEDIUM)%f_n(i,j,k)*phyto(MEDIUM)%silim(i,j,k)
              cobalt%nlg_misc(i,j,k)=phyto(LARGE)%f_n(i,j,k) - phyto(LARGE)%f_n(i,j,k)*phyto(LARGE)%silim(i,j,k)
              cobalt%nmd_misc(i,j,k)=phyto(MEDIUM)%f_n(i,j,k) - phyto(MEDIUM)%f_n(i,j,k)*phyto(MEDIUM)%silim(i,j,k)
            enddo; enddo ; enddo !} i,j,k

          endif !} recalculate carbon system properties

          used = g_send_data(cobalt%id_co3_sol_arag, cobalt%co3_sol_arag, &
              model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_co3_sol_calc, cobalt%co3_sol_calc, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_omega_arag, cobalt%omega_arag, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_omega_calc, cobalt%omega_calc, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_thetao, Temp, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! Derived tracers
          used = g_send_data(cobalt%id_nphyto_tot, (cobalt%p_ndi(:,:,:,tau) +  &
            cobalt%p_nlg(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau)), &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_nlg_diatoms,cobalt%nlg_diatoms,&
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_nmd_diatoms,cobalt%nmd_diatoms,&
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_nlg_misc,cobalt%nlg_misc,&
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_nmd_misc,cobalt%nmd_misc,&
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! These diagnostics have not generally been used or tested and can generally be derived from 3D diagnostics
          ! Check in CMIP7 requests.  If so, confirm functionality
          used = g_send_data(cobalt%id_o2min, cobalt%o2min, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_zo2min, cobalt%zo2min, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_zsatarag, cobalt%zsatarag, &
            model_time, mask = cobalt%mask_zsatarag, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_zsatcalc, cobalt%zsatcalc, &
            model_time, mask = cobalt%mask_zsatcalc, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

          ! Surface tracers (could remove if instead extracted from 3D files in the diagnostic table?)
          used = g_send_data(phyto(DIAZO)%id_sfc_f_n, cobalt%p_ndi(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(phyto(LARGE)%id_sfc_f_n, cobalt%p_nlg(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(phyto(MEDIUM)%id_sfc_f_n, cobalt%p_nmd(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(phyto(SMALL)%id_sfc_f_n, cobalt%p_nsm(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_alk, cobalt%p_alk(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_cadet_arag,cobalt%p_cadet_arag(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_cadet_calc,cobalt%p_cadet_calc(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_dic, cobalt%p_dic(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_fed, cobalt%p_fed(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_ldon, cobalt%p_ldon(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_sldon, cobalt%p_sldon(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_srdon, cobalt%p_srdon(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_no3, cobalt%p_no3(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_nh4, cobalt%p_nh4(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_po4, cobalt%p_po4(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_sio4, cobalt%p_sio4(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_o2, cobalt%p_o2(:,:,1,tau), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_temp, Temp(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(phyto(DIAZO)%id_sfc_chl, cobalt%p_ndi(:,:,1,tau)*c2n*phyto(DIAZO)%theta(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(phyto(LARGE)%id_sfc_chl, cobalt%p_nlg(:,:,1,tau)*c2n*phyto(LARGE)%theta(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(phyto(MEDIUM)%id_sfc_chl, cobalt%p_nmd(:,:,1,tau)*c2n*phyto(MEDIUM)%theta(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(phyto(SMALL)%id_sfc_chl, cobalt%p_nsm(:,:,1,tau)*c2n*phyto(SMALL)%theta(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_chl, (cobalt%p_ndi(:,:,1,tau)*c2n*phyto(DIAZO)%theta(:,:,1) + &
            cobalt%p_nlg(:,:,1,tau)*c2n*phyto(LARGE)%theta(:,:,1) + &
            cobalt%p_nmd(:,:,1,tau)*c2n*phyto(MEDIUM)%theta(:,:,1) + &
            cobalt%p_nsm(:,:,1,tau)*c2n*phyto(SMALL)%theta(:,:,1)), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_pco2surf, cobalt%pco2_csurf, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_pnh3surf, cobalt%pnh3_csurf, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_co2_csurf, cobalt%co2_csurf, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_co2_alpha, cobalt%co2_alpha, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_nh3_csurf, cobalt%nh3_csurf, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_nh3_alpha, cobalt%nh3_alpha,              &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_htotal, cobalt%f_htotal(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_co3_ion, cobalt%f_co3_ion(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_co3_sol_arag, cobalt%co3_sol_arag(:,:,1),  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_co3_sol_calc, cobalt%co3_sol_calc(:,:,1),  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

          ! Calculate bottom layer values over a thickness defined by cobalt%bottom_thickness 
          ! rather than the bottom-most layer as in MOM4/5.  This avoids numerical issues
          ! generated in "vanishing" layers that overlie the benthos in most regions.
          allocate(rho_dzt_bot(isc:iec,jsc:jec))
          allocate(k_bot(isc:iec,jsc:jec))
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
                  cobalt%btm_o2(i,j) = cobalt%btm_o2(i,j) + cobalt%p_o2(i,j,k,tau)*rho_dzt(i,j,k)
                  cobalt%btm_alk(i,j) = cobalt%btm_alk(i,j) + cobalt%p_alk(i,j,k,tau)*rho_dzt(i,j,k)
                  cobalt%btm_dic(i,j) = cobalt%btm_dic(i,j) + cobalt%p_dic(i,j,k,tau)*rho_dzt(i,j,k) 
                  cobalt%btm_temp(i,j) = cobalt%btm_temp(i,j) + Temp(i,j,k)*rho_dzt(i,j,k)
                  cobalt%btm_htotal(i,j) = cobalt%btm_htotal(i,j) + cobalt%f_htotal(i,j,k)*rho_dzt(i,j,k)
                  cobalt%btm_co3_sol_arag(i,j) = cobalt%btm_co3_sol_arag(i,j) + & 
                    cobalt%co3_sol_arag(i,j,k)*rho_dzt(i,j,k)
                  cobalt%btm_co3_sol_calc(i,j) = cobalt%btm_co3_sol_calc(i,j) + &
                    cobalt%co3_sol_calc(i,j,k)*rho_dzt(i,j,k)
                  cobalt%btm_co3_ion(i,j) = cobalt%btm_co3_ion(i,j) + cobalt%f_co3_ion(i,j,k)*rho_dzt(i,j,k)
                endif
              enddo
              ! diagnostic to assess how far up into the water column info is being drawn from
              cobalt%rho_dzt_bot_diag(i,j) = rho_dzt_bot(i,j)
              ! calculate overshoot and subtract off
              drho_dzt = rho_dzt_bot(i,j) - cobalt%Rho_0*cobalt%bottom_thickness
              cobalt%btm_temp(i,j)=cobalt%btm_temp(i,j)-Temp(i,j,k_bot(i,j))*drho_dzt
              cobalt%btm_o2(i,j)=cobalt%btm_o2(i,j)-cobalt%p_o2(i,j,k_bot(i,j),tau)*drho_dzt
              cobalt%btm_alk(i,j)=cobalt%btm_alk(i,j)-cobalt%p_alk(i,j,k_bot(i,j),tau)*drho_dzt
              cobalt%btm_dic(i,j)=cobalt%btm_dic(i,j)-cobalt%p_dic(i,j,k_bot(i,j),tau)*drho_dzt
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
          deallocate(rho_dzt_bot)
          deallocate(k_bot)

          ! CALCULATE BOTTOM PROGNOSTIC TRACERS
          used = g_send_data(cobalt%id_btm_temp, cobalt%btm_temp, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_o2, cobalt%btm_o2, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_no3, cobalt%btm_no3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_alk, cobalt%btm_alk, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_dic, cobalt%btm_dic, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          ! Diagnostics associated with the bottom calculation
          used = g_send_data(cobalt%id_grid_kmt_diag, cobalt%grid_kmt_diag, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_k_bot_diag, cobalt%k_bot_diag, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_rho_dzt_bot_diag, cobalt%rho_dzt_bot_diag, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_rho_dzt_kmt_diag, cobalt%rho_dzt_kmt_diag, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          ! Bottom diagnosic tracers
          used = g_send_data(cobalt%id_btm_htotal, cobalt%btm_htotal, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_co3_ion, cobalt%btm_co3_ion,            &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_co3_sol_arag, cobalt%btm_co3_sol_arag,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_co3_sol_calc, cobalt%btm_co3_sol_calc,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_omega_calc, cobalt%btm_omega_calc, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_btm_omega_arag, cobalt%btm_omega_arag,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)

          ! Sinking fluxes (todo: option to move to interfaces)
          used = g_send_data(cobalt%id_fcadet_arag, cobalt%p_cadet_arag(:,:,:,tau) * cobalt%Rho_0 * &
            cobalt%wsink * grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fcadet_calc, cobalt%p_cadet_calc(:,:,:,tau) * cobalt%Rho_0 * &
            cobalt%wsink*grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_ffedet, cobalt%p_fedet(:,:,:,tau) * cobalt%Rho_0 * &
            cobalt%wsink * grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_flithdet,      cobalt%p_lithdet(:,:,:,tau) * cobalt%Rho_0 * &
            cobalt%wsink * grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fndet, cobalt%p_ndet(:,:,:,tau) * cobalt%Rho_0 * &
            cobalt%wsink * grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fpdet, cobalt%p_pdet(:,:,:,tau) * cobalt%Rho_0 * &
            cobalt%wsink * grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fsidet, cobalt%p_sidet(:,:,:,tau) * cobalt%Rho_0 * &
            cobalt%wsink  * grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_ffetot, (cobalt%p_fedet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_fesm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
            cobalt%p_femd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
            cobalt%p_felg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
            cobalt%p_fedi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))*cobalt%Rho_0*grid_tmask(:,:,:), &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fntot, (cobalt%p_ndet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_nsm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
            cobalt%p_nmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
            cobalt%p_nlg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
            cobalt%p_ndi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))*cobalt%Rho_0*grid_tmask(:,:,:), &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fptot, (cobalt%p_pdet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_psm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + &
            cobalt%p_pmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
            cobalt%p_plg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + &
            cobalt%p_pdi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:))*cobalt%Rho_0*grid_tmask(:,:,:), &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fsitot, (cobalt%p_sidet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_simd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
            cobalt%p_silg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:))*cobalt%Rho_0*grid_tmask(:,:,:), &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_nphyto_tot, (cobalt%p_ndi(:,:,:,tau) +  &
            cobalt%p_nlg(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau)), &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)


          ! The carbon layer integral (organic + inorganic).  Note that this can be calculated with or without a constant
          ! background level of recalcitrant dissolved organic carbon by setting cobalt%doc_background.  This is included
          ! at a level of 40 micromoles kg-1 by default.
          cobalt%tot_layer_int_c(:,:,:) = (cobalt%p_dic(:,:,:,tau) + cobalt%doc_background + cobalt%p_cadet_arag(:,:,:,tau) +&
            cobalt%p_cadet_calc(:,:,:,tau) + cobalt%c_2_n * (cobalt%p_ndi(:,:,:,tau) + cobalt%p_nlg(:,:,:,tau) + &
            cobalt%p_nmd(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau) + cobalt%p_nbact(:,:,:,tau) + cobalt%p_ldon(:,:,:,tau) + &
            cobalt%p_sldon(:,:,:,tau) + cobalt%p_srdon(:,:,:,tau) + cobalt%p_ndet(:,:,:,tau) + cobalt%p_nsmz(:,:,:,tau) + &
            cobalt%p_nmdz(:,:,:,tau) + cobalt%p_nlgz(:,:,:,tau))) * rho_dzt(:,:,:)

          ! dissolved organic component also includes an optional background doc
          cobalt%tot_layer_int_doc(:,:,:) = (cobalt%c_2_n * (cobalt%p_ldon(:,:,:,tau) + cobalt%p_sldon(:,:,:,tau) + &
            cobalt%p_srdon(:,:,:,tau)) + cobalt%doc_background) * rho_dzt(:,:,:)

          cobalt%tot_layer_int_poc(:,:,:) = (cobalt%p_ndi(:,:,:,tau) + cobalt%p_nlg(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) + &
            cobalt%p_nsm(:,:,:,tau) + cobalt%p_nbact(:,:,:,tau) + cobalt%p_ndet(:,:,:,tau) + cobalt%p_nsmz(:,:,:,tau) + &
            cobalt%p_nmdz(:,:,:,tau) + cobalt%p_nlgz(:,:,:,tau))*cobalt%c_2_n*rho_dzt(:,:,:)

          cobalt%tot_layer_int_dic(:,:,:) = cobalt%p_dic(:,:,:,tau)*rho_dzt(:,:,:)

          cobalt%tot_layer_int_fe(:,:,:) = (cobalt%p_fed(:,:,:,tau) + cobalt%p_fedi(:,:,:,tau) + cobalt%p_felg(:,:,:,tau) + &
            cobalt%p_femd(:,:,:,tau) + cobalt%p_fesm(:,:,:,tau) + cobalt%p_fedet(:,:,:,tau)) * rho_dzt(:,:,:)

          cobalt%tot_layer_int_n(:,:,:) = (cobalt%p_no3(:,:,:,tau) + cobalt%p_nh4(:,:,:,tau) + cobalt%p_ndi(:,:,:,tau) + &
            cobalt%p_nlg(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) + cobalt%p_nsm(:,:,:,tau) + cobalt%p_nbact(:,:,:,tau) + &
            cobalt%p_ldon(:,:,:,tau) + cobalt%p_sldon(:,:,:,tau) + cobalt%p_srdon(:,:,:,tau) +  cobalt%p_ndet(:,:,:,tau) + &
            cobalt%p_nsmz(:,:,:,tau) + cobalt%p_nmdz(:,:,:,tau) + cobalt%p_nlgz(:,:,:,tau)) * rho_dzt(:,:,:)

          cobalt%tot_layer_int_p(:,:,:) = (cobalt%p_po4(:,:,:,tau) + cobalt%p_pdi(:,:,:,tau) + cobalt%p_plg(:,:,:,tau) + &
            cobalt%p_pmd(:,:,:,tau) + cobalt%p_psm(:,:,:,tau) + cobalt%p_ldop(:,:,:,tau) + cobalt%p_sldop(:,:,:,tau) + &
            cobalt%p_srdop(:,:,:,tau) + cobalt%p_pdet(:,:,:,tau) + bact(1)%q_p_2_n*cobalt%p_nbact(:,:,:,tau) + &
            zoo(1)%q_p_2_n*cobalt%p_nsmz(:,:,:,tau) + zoo(2)%q_p_2_n*cobalt%p_nmdz(:,:,:,tau) + &
            zoo(3)%q_p_2_n*cobalt%p_nlgz(:,:,:,tau))*rho_dzt(:,:,:)

          cobalt%tot_layer_int_si(:,:,:) = (cobalt%p_sio4(:,:,:,tau) + cobalt%p_silg(:,:,:,tau) + &
            cobalt%p_simd(:,:,:,tau) + cobalt%p_sidet(:,:,:,tau)) * rho_dzt(:,:,:)

          cobalt%tot_layer_int_o2(:,:,:) = cobalt%p_o2(:,:,:,tau)*rho_dzt(:,:,:)

          cobalt%tot_layer_int_alk(:,:,:) = cobalt%p_alk(:,:,:,tau)*rho_dzt(:,:,:)

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
          enddo; enddo !} i,j

          do j = jsc, jec ; do i = isc, iec ; do k = 1, nk  !{
            ! Tracers ("tot_layer_int" variables already multiplied by "rho_dzt", so just sum over k to get moles m-2)
            cobalt%wc_vert_int_c(i,j) = cobalt%wc_vert_int_c(i,j) + &
              cobalt%tot_layer_int_c(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_dic(i,j) = cobalt%wc_vert_int_dic(i,j) + &
              cobalt%tot_layer_int_dic(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_doc(i,j) = cobalt%wc_vert_int_doc(i,j) + &
              cobalt%tot_layer_int_doc(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_poc(i,j) = cobalt%wc_vert_int_poc(i,j) + &
              cobalt%tot_layer_int_poc(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_n(i,j) = cobalt%wc_vert_int_n(i,j) + &
              cobalt%tot_layer_int_n(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_p(i,j) = cobalt%wc_vert_int_p(i,j) + &
              cobalt%tot_layer_int_p(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_fe(i,j) = cobalt%wc_vert_int_fe(i,j) + & 
              cobalt%tot_layer_int_fe(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_si(i,j) = cobalt%wc_vert_int_si(i,j) + &
              cobalt%tot_layer_int_si(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_o2(i,j) = cobalt%wc_vert_int_o2(i,j) + &
              cobalt%tot_layer_int_o2(i,j,k)*grid_tmask(i,j,k)
            cobalt%wc_vert_int_alk(i,j) = cobalt%wc_vert_int_alk(i,j) + & 
              cobalt%tot_layer_int_alk(i,j,k)*grid_tmask(i,j,k)
          enddo; enddo; enddo  !} i,j,k

          ! send layer integral diag
          used = g_send_data(cobalt%id_tot_layer_int_c, cobalt%tot_layer_int_c,&
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_tot_layer_int_fe,cobalt%tot_layer_int_fe,&
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_tot_layer_int_n,cobalt%tot_layer_int_n,&
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_tot_layer_int_p,cobalt%tot_layer_int_p,&
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_tot_layer_int_si,cobalt%tot_layer_int_si,&
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_tot_layer_int_o2,cobalt%tot_layer_int_o2,&
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_tot_layer_int_alk,cobalt%tot_layer_int_alk,&
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

          ! send water column integral diag
          used = g_send_data(cobalt%id_wc_vert_int_c, cobalt%wc_vert_int_c, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_dic,    cobalt%wc_vert_int_dic, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_doc,    cobalt%wc_vert_int_doc, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_poc,    cobalt%wc_vert_int_poc, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_n, cobalt%wc_vert_int_n, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_p, cobalt%wc_vert_int_p, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_fe, cobalt%wc_vert_int_fe, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_si, cobalt%wc_vert_int_si, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_o2, cobalt%wc_vert_int_o2, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_alk, cobalt%wc_vert_int_alk, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

          ! make generic?  _intz with user-specified integration depth
          allocate(rho_dzt_100(isc:iec,jsc:jec))
          ! Calculate 100m integrals of prognostic variables and and 100m fluxes
          do j = jsc, jec ; do i = isc, iec !{
            rho_dzt_100(i,j) = rho_dzt(i,j,1)
            ! prognostic variables
            cobalt%f_alk_int_100(i,j) = cobalt%p_alk(i,j,1,tau) * rho_dzt(i,j,1)
            cobalt%f_dic_int_100(i,j) = cobalt%p_dic(i,j,1,tau) * rho_dzt(i,j,1)
            cobalt%f_din_int_100(i,j) = (cobalt%p_no3(i,j,1,tau) + cobalt%p_nh4(i,j,1,tau)) * rho_dzt(i,j,1)
            cobalt%f_fed_int_100(i,j) = cobalt%p_fed(i,j,1,tau) * rho_dzt(i,j,1)
            cobalt%f_po4_int_100(i,j) = cobalt%p_po4(i,j,1,tau) * rho_dzt(i,j,1)
            cobalt%f_sio4_int_100(i,j) = cobalt%p_sio4(i,j,1,tau) * rho_dzt(i,j,1)
            phyto(DIAZO)%f_n_100(i,j) = cobalt%p_ndi(i,j,1,tau) * rho_dzt(i,j,1)
            phyto(LARGE)%f_n_100(i,j) = cobalt%p_nlg(i,j,1,tau) * rho_dzt(i,j,1)
            phyto(MEDIUM)%f_n_100(i,j) = cobalt%p_nmd(i,j,1,tau) * rho_dzt(i,j,1)
            phyto(SMALL)%f_n_100(i,j) = cobalt%p_nsm(i,j,1,tau) * rho_dzt(i,j,1)
            zoo(1)%f_n_100(i,j) = cobalt%p_nsmz(i,j,1,tau) * rho_dzt(i,j,1)
            zoo(2)%f_n_100(i,j) = cobalt%p_nmdz(i,j,1,tau) * rho_dzt(i,j,1)
            zoo(3)%f_n_100(i,j) = cobalt%p_nlgz(i,j,1,tau) * rho_dzt(i,j,1)
            bact(1)%f_n_100(i,j) = cobalt%p_nbact(i,j,1,tau) * rho_dzt(i,j,1)
            cobalt%f_ndet_100(i,j) = cobalt%p_ndet(i,j,1,tau) * rho_dzt(i,j,1)
            cobalt%f_don_100(i,j) = (cobalt%p_ldon(i,j,1,tau)+cobalt%p_sldon(i,j,1,tau)+cobalt%p_srdon(i,j,1,tau))* &
              rho_dzt(i,j,1)
            cobalt%f_silg_100(i,j) = cobalt%p_silg(i,j,1,tau)*rho_dzt(i,j,1)
            cobalt%f_simd_100(i,j) = cobalt%p_simd(i,j,1,tau)*rho_dzt(i,j,1)
            ! sinking fluxes (should we just handle these with by remapping the appropriate 3D variable onto 100m?)
            ! need to add fast sinking detritus
            cobalt%fndet_100(i,j) = cobalt%p_ndet(i,j,1,tau) * cobalt%Rho_0 * cobalt%wsink
            cobalt%fpdet_100(i,j) = cobalt%p_pdet(i,j,1,tau) * cobalt%Rho_0 * cobalt%wsink
            cobalt%ffedet_100(i,j) = cobalt%p_fedet(i,j,1,tau) * cobalt%Rho_0 * cobalt%wsink
            cobalt%flithdet_100(i,j) = cobalt%p_lithdet(i,j,1,tau) * cobalt%Rho_0 * cobalt%wsink
            cobalt%fsidet_100(i,j) = cobalt%p_sidet(i,j,1,tau) * cobalt%Rho_0 * cobalt%wsink
            cobalt%fcadet_arag_100(i,j) = cobalt%p_cadet_arag(i,j,1,tau) * cobalt%Rho_0 * cobalt%wsink
            cobalt%fcadet_calc_100(i,j) = cobalt%p_cadet_calc(i,j,1,tau) * cobalt%Rho_0 * cobalt%wsink
            cobalt%fntot_100(i,j) = (cobalt%p_ndet(i,j,1,tau)*cobalt%wsink + &
              cobalt%p_nsm(i,j,1,tau)*phyto(SMALL)%vmove(i,j,1) + &
              cobalt%p_nmd(i,j,1,tau)*phyto(MEDIUM)%vmove(i,j,1) + &
              cobalt%p_nlg(i,j,1,tau)*phyto(LARGE)%vmove(i,j,1) + &
              cobalt%p_ndi(i,j,1,tau)*phyto(DIAZO)%vmove(i,j,1))*cobalt%Rho_0
            cobalt%fptot_100(i,j) = (cobalt%p_pdet(i,j,1,tau)*cobalt%wsink + &
              cobalt%p_psm(i,j,1,tau)*phyto(SMALL)%vmove(i,j,1) + &
              cobalt%p_pmd(i,j,1,tau)*phyto(MEDIUM)%vmove(i,j,1) + &
              cobalt%p_plg(i,j,1,tau)*phyto(LARGE)%vmove(i,j,1) + &
              cobalt%p_pdi(i,j,1,tau)*phyto(DIAZO)%vmove(i,j,1))*cobalt%Rho_0
            cobalt%ffetot_100(i,j) = (cobalt%p_fedet(i,j,1,tau)*cobalt%wsink + &
              cobalt%p_fesm(i,j,1,tau)*phyto(SMALL)%vmove(i,j,1) + &
              cobalt%p_femd(i,j,1,tau)*phyto(MEDIUM)%vmove(i,j,1) + &
              cobalt%p_felg(i,j,1,tau)*phyto(LARGE)%vmove(i,j,1) + &
              cobalt%p_fedi(i,j,1,tau)*phyto(DIAZO)%vmove(i,j,1))*cobalt%Rho_0
            cobalt%fsitot_100(i,j) = (cobalt%p_sidet(i,j,1,tau)*cobalt%wsink + &
              cobalt%p_simd(i,j,1,tau)*phyto(MEDIUM)%vmove(i,j,1) + &
              cobalt%p_silg(i,j,1,tau)*phyto(LARGE)%vmove(i,j,1))*cobalt%Rho_0
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
                phyto(DIAZO)%f_n_100(i,j) = phyto(DIAZO)%f_n_100(i,j) + cobalt%p_ndi(i,j,k,tau) * rho_dzt(i,j,k)
                phyto(LARGE)%f_n_100(i,j) = phyto(LARGE)%f_n_100(i,j) + cobalt%p_nlg(i,j,k,tau) * rho_dzt(i,j,k)
                phyto(MEDIUM)%f_n_100(i,j) = phyto(MEDIUM)%f_n_100(i,j) + cobalt%p_nmd(i,j,k,tau) * rho_dzt(i,j,k)
                phyto(SMALL)%f_n_100(i,j) = phyto(SMALL)%f_n_100(i,j) + cobalt%p_nsm(i,j,k,tau) * rho_dzt(i,j,k)
                zoo(1)%f_n_100(i,j) = zoo(1)%f_n_100(i,j) + cobalt%p_nsmz(i,j,k,tau) * rho_dzt(i,j,k)
                zoo(2)%f_n_100(i,j) = zoo(2)%f_n_100(i,j) + cobalt%p_nmdz(i,j,k,tau) * rho_dzt(i,j,k)
                zoo(3)%f_n_100(i,j) = zoo(3)%f_n_100(i,j) + cobalt%p_nlgz(i,j,k,tau) * rho_dzt(i,j,k)
                bact(1)%f_n_100(i,j) = bact(1)%f_n_100(i,j) + cobalt%p_nbact(i,j,k,tau) * rho_dzt(i,j,k)
                cobalt%f_ndet_100(i,j) = cobalt%f_ndet_100(i,j) + cobalt%p_ndet(i,j,k,tau)*rho_dzt(i,j,k)
                cobalt%f_don_100(i,j) = cobalt%f_don_100(i,j) + (cobalt%p_ldon(i,j,k,tau)+cobalt%p_sldon(i,j,k,tau) + &
                  cobalt%p_srdon(i,j,k,tau))*rho_dzt(i,j,k)
                cobalt%f_silg_100(i,j) = cobalt%f_silg_100(i,j) + cobalt%p_silg(i,j,k,tau)*rho_dzt(i,j,k)
                cobalt%f_simd_100(i,j) = cobalt%f_simd_100(i,j) + cobalt%p_simd(i,j,k,tau)*rho_dzt(i,j,k)  ! Missed
                ! sinking fluxes (should we just handle these with by remapping the appropriate 3D variable onto 100m?)
                cobalt%fndet_100(i,j) = cobalt%p_ndet(i,j,k,tau) * cobalt%Rho_0 * cobalt%wsink
                cobalt%fpdet_100(i,j) = cobalt%p_pdet(i,j,k,tau) * cobalt%Rho_0 * cobalt%wsink
                cobalt%ffedet_100(i,j) = cobalt%p_fedet(i,j,k,tau) * cobalt%Rho_0 * cobalt%wsink
                cobalt%flithdet_100(i,j) = cobalt%p_lithdet(i,j,k,tau) * cobalt%Rho_0 * cobalt%wsink
                cobalt%fsidet_100(i,j) = cobalt%p_sidet(i,j,k,tau) * cobalt%Rho_0 * cobalt%wsink
                cobalt%fcadet_arag_100(i,j) = cobalt%p_cadet_arag(i,j,k,tau) * cobalt%Rho_0 * cobalt%wsink
                cobalt%fcadet_calc_100(i,j) = cobalt%p_cadet_calc(i,j,k,tau) * cobalt%Rho_0 * cobalt%wsink
                cobalt%fntot_100(i,j) = (cobalt%p_ndet(i,j,k,tau)*cobalt%wsink + &
                  cobalt%p_nsm(i,j,k,tau)*phyto(SMALL)%vmove(i,j,k) + &
                  cobalt%p_nmd(i,j,k,tau)*phyto(MEDIUM)%vmove(i,j,k) + &
                  cobalt%p_nlg(i,j,k,tau)*phyto(LARGE)%vmove(i,j,k) + &
                  cobalt%p_ndi(i,j,k,tau)*phyto(DIAZO)%vmove(i,j,k))*cobalt%Rho_0
                cobalt%fptot_100(i,j) = (cobalt%p_pdet(i,j,k,tau)*cobalt%wsink + &
                  cobalt%p_psm(i,j,k,tau)*phyto(SMALL)%vmove(i,j,k) + &
                  cobalt%p_pmd(i,j,k,tau)*phyto(MEDIUM)%vmove(i,j,k) + &
                  cobalt%p_plg(i,j,k,tau)*phyto(LARGE)%vmove(i,j,k) + &
                  cobalt%p_pdi(i,j,k,tau)*phyto(DIAZO)%vmove(i,j,k))*cobalt%Rho_0
                cobalt%ffetot_100(i,j) = (cobalt%p_fedet(i,j,k,tau)*cobalt%wsink + &
                  cobalt%p_fesm(i,j,k,tau)*phyto(SMALL)%vmove(i,j,k) + &
                  cobalt%p_femd(i,j,k,tau)*phyto(MEDIUM)%vmove(i,j,k) + &
                  cobalt%p_felg(i,j,k,tau)*phyto(LARGE)%vmove(i,j,k) + &
                  cobalt%p_fedi(i,j,k,tau)*phyto(DIAZO)%vmove(i,j,k))*cobalt%Rho_0
                cobalt%fsitot_100(i,j) = (cobalt%p_sidet(i,j,k,tau)*cobalt%wsink + &
                  cobalt%p_simd(i,j,k,tau)*phyto(MEDIUM)%vmove(i,j,k) + &
                  cobalt%p_silg(i,j,k,tau)*phyto(LARGE)%vmove(i,j,k))*cobalt%Rho_0
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
              phyto(DIAZO)%f_n_100(i,j) = phyto(DIAZO)%f_n_100(i,j) + cobalt%p_ndi(i,j,k_100,tau) * drho_dzt
              phyto(LARGE)%f_n_100(i,j) = phyto(LARGE)%f_n_100(i,j) + cobalt%p_nlg(i,j,k_100,tau) * drho_dzt
              phyto(MEDIUM)%f_n_100(i,j) = phyto(MEDIUM)%f_n_100(i,j) + cobalt%p_nmd(i,j,k_100,tau) * drho_dzt
              phyto(SMALL)%f_n_100(i,j) = phyto(SMALL)%f_n_100(i,j) + cobalt%p_nsm(i,j,k_100,tau) * drho_dzt
              zoo(1)%f_n_100(i,j) = zoo(1)%f_n_100(i,j) + cobalt%p_nsmz(i,j,k_100,tau) * drho_dzt
              zoo(2)%f_n_100(i,j) = zoo(2)%f_n_100(i,j) + cobalt%p_nmdz(i,j,k_100,tau) * drho_dzt
              zoo(3)%f_n_100(i,j) = zoo(3)%f_n_100(i,j) + cobalt%p_nlgz(i,j,k_100,tau) * drho_dzt
              bact(1)%f_n_100(i,j) = bact(1)%f_n_100(i,j) + cobalt%p_nbact(i,j,k_100,tau) * drho_dzt
              cobalt%f_ndet_100(i,j) = cobalt%f_ndet_100(i,j) + cobalt%p_ndet(i,j,k_100,tau)*drho_dzt
              cobalt%f_don_100(i,j) = cobalt%f_don_100(i,j) + (cobalt%p_ldon(i,j,k_100,tau)+cobalt%p_sldon(i,j,k_100,tau) + &
                cobalt%p_srdon(i,j,k_100,tau))*drho_dzt
              cobalt%f_silg_100(i,j) = cobalt%f_silg_100(i,j) + cobalt%p_silg(i,j,k_100,tau)*drho_dzt
              cobalt%f_simd_100(i,j) = cobalt%f_simd_100(i,j) + cobalt%p_simd(i,j,k_100,tau)*drho_dzt
              ! sinking fluxes (should we just handle these with by remapping the appropriate 3D variable onto 100m?)
              ! need to add fast sinking detritus
              cobalt%fndet_100(i,j) = cobalt%p_ndet(i,j,k_100,tau) * cobalt%Rho_0 * cobalt%wsink
              cobalt%fpdet_100(i,j) = cobalt%p_pdet(i,j,k_100,tau) * cobalt%Rho_0 * cobalt%wsink
              cobalt%ffedet_100(i,j) = cobalt%p_fedet(i,j,k_100,tau) * cobalt%Rho_0 * cobalt%wsink
              cobalt%flithdet_100(i,j) = cobalt%p_lithdet(i,j,k_100,tau) * cobalt%Rho_0 * cobalt%wsink
              cobalt%fsidet_100(i,j) = cobalt%p_sidet(i,j,k_100,tau) * cobalt%Rho_0 * cobalt%wsink
              cobalt%fcadet_arag_100(i,j) = cobalt%p_cadet_arag(i,j,k_100,tau) * cobalt%Rho_0 * cobalt%wsink
              cobalt%fcadet_calc_100(i,j) = cobalt%p_cadet_calc(i,j,k_100,tau) * cobalt%Rho_0 * cobalt%wsink
              cobalt%fntot_100(i,j) = (cobalt%p_ndet(i,j,k_100,tau)*cobalt%wsink + &
                cobalt%p_nsm(i,j,k_100,tau)*phyto(SMALL)%vmove(i,j,k_100) + &
                cobalt%p_nmd(i,j,k_100,tau)*phyto(MEDIUM)%vmove(i,j,k_100) + &
                cobalt%p_nlg(i,j,k_100,tau)*phyto(LARGE)%vmove(i,j,k_100) + &
                cobalt%p_ndi(i,j,k_100,tau)*phyto(DIAZO)%vmove(i,j,k_100))*cobalt%Rho_0
              cobalt%fptot_100(i,j) = (cobalt%p_pdet(i,j,k_100,tau)*cobalt%wsink + &
                cobalt%p_psm(i,j,k_100,tau)*phyto(SMALL)%vmove(i,j,k_100) + &
                cobalt%p_pmd(i,j,k_100,tau)*phyto(MEDIUM)%vmove(i,j,k_100) + &
                cobalt%p_plg(i,j,k_100,tau)*phyto(LARGE)%vmove(i,j,k_100) + &
                cobalt%p_pdi(i,j,k_100,tau)*phyto(DIAZO)%vmove(i,j,k_100))*cobalt%Rho_0
              cobalt%ffetot_100(i,j) = (cobalt%p_fedet(i,j,k_100,tau)*cobalt%wsink + &
                cobalt%p_fesm(i,j,k_100,tau)*phyto(SMALL)%vmove(i,j,k_100) + &
                cobalt%p_femd(i,j,k_100,tau)*phyto(MEDIUM)%vmove(i,j,k_100) + &
                cobalt%p_felg(i,j,k_100,tau)*phyto(LARGE)%vmove(i,j,k_100) + &
                cobalt%p_fedi(i,j,k_100,tau)*phyto(DIAZO)%vmove(i,j,k_100))*cobalt%Rho_0
              cobalt%fsitot_100(i,j) = (cobalt%p_sidet(i,j,k_100,tau)*cobalt%wsink + &
                cobalt%p_simd(i,j,k_100,tau)*phyto(MEDIUM)%vmove(i,j,k_100) + &
                cobalt%p_silg(i,j,k_100,tau)*phyto(LARGE)%vmove(i,j,k_100))*cobalt%Rho_0
            endif
          enddo ; enddo  !} i,j
          deallocate(rho_dzt_100)

          !
          ! Send integrated tracer diagnostics
          !

          do n = 1, NUM_PHYTO  !{
            used = g_send_data(phyto(n)%id_f_n_100, phyto(n)%f_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          enddo
          do n = 1, NUM_ZOO  !{
            used = g_send_data(zoo(n)%id_f_n_100, zoo(n)%f_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          enddo
          used = g_send_data(bact(1)%id_f_n_100, bact(1)%f_n_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_ndet_100, cobalt%f_ndet_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_don_100, cobalt%f_don_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_silg_100, cobalt%f_silg_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_simd_100, cobalt%f_simd_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_mesozoo_200, cobalt%f_mesozoo_200,         &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_dic_int_100, cobalt%f_dic_int_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_din_int_100, cobalt%f_din_int_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_po4_int_100, cobalt%f_po4_int_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_fed_int_100, cobalt%f_fed_int_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_sio4_int_100, cobalt%f_sio4_int_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_f_alk_int_100, cobalt%f_alk_int_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !
          ! 100m flux diagnostics (handle through diagnostic table?)
          !
          used = g_send_data(cobalt%id_fndet_100, cobalt%fndet_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fpdet_100, cobalt%fpdet_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fsidet_100, cobalt%fsidet_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_flithdet_100, cobalt%flithdet_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fcadet_calc_100, cobalt%fcadet_calc_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fcadet_arag_100, cobalt%fcadet_arag_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_ffedet_100, cobalt%ffedet_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fntot_100, cobalt%fntot_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fptot_100, cobalt%fptot_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fsitot_100, cobalt%fsitot_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_ffetot_100, cobalt%ffetot_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

          allocate(rho_dzt_200(isc:iec,jsc:jec))
          do j = jsc, jec ; do i = isc, iec !{
            rho_dzt_200(i,j) = rho_dzt(i,j,1)
            cobalt%f_mesozoo_200(i,j) = (zoo(2)%f_n(i,j,1)+zoo(3)%f_n(i,j,1))*rho_dzt(i,j,1)
          enddo; enddo !} i,j

          do j = jsc, jec ; do i = isc, iec ; !{
            k_200 = 1
            do k = 2, grid_kmt(i,j)  !{
              if (rho_dzt_200(i,j) .lt. cobalt%Rho_0 * 200.0) then
                k_200 = k
                rho_dzt_200(i,j) = rho_dzt_200(i,j) + rho_dzt(i,j,k)
                cobalt%f_mesozoo_200(i,j) = cobalt%f_mesozoo_200(i,j) + &
                  (zoo(2)%f_n(i,j,k)+zoo(3)%f_n(i,j,k))*rho_dzt(i,j,k)
              endif
            enddo  !} k

            if (k_200 .gt. 1 .and. k_200 .lt. grid_kmt(i,j)) then
              drho_dzt = cobalt%Rho_0 * 200.0 - rho_dzt_200(i,j)
              cobalt%f_mesozoo_200(i,j) = cobalt%f_mesozoo_200(i,j) + &
                (zoo(2)%f_n(i,j,k_200)+zoo(3)%f_n(i,j,k_200))*drho_dzt
            endif
          enddo ; enddo  !} i,j
          deallocate(rho_dzt_200)

          !
          ! 3D CMIP Variables derived from prognostic tracer and updated diagnostic tracer variables
          !
          ! Carbon pools
          used = g_send_data(cobalt%id_dissic, cobalt%p_dic(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! Does not include background organic carbon values in accordance with CMIP request
          cobalt%dissoc(:,:,:) = cobalt%c_2_n * (cobalt%p_ldon(:,:,:,tau) + cobalt%p_sldon(:,:,:,tau) + &
            cobalt%p_srdon(:,:,:,tau) )
          used = g_send_data(cobalt%id_dissoc,  cobalt%dissoc * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_phyc, (cobalt%p_nlg(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) + &
            cobalt%p_nsm(:,:,:,tau) + cobalt%p_ndi(:,:,:,tau)) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_zooc, (cobalt%p_nlgz(:,:,:,tau) + cobalt%p_nmdz(:,:,:,tau) + &
            cobalt%p_nsmz(:,:,:,tau)) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bacc,  cobalt%p_nbact(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! need to add fast sinking detritus once implemented
          used = g_send_data(cobalt%id_detoc,  cobalt%p_ndet(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! Includes on calcite and aragonite detritus, not comparable to total calcite/aragonite st surface
          used = g_send_data(cobalt%id_calc,  cobalt%p_cadet_calc(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_arag,  cobalt%p_cadet_arag(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_phydiat, (cobalt%nlg_diatoms+cobalt%nmd_diatoms)*cobalt%c_2_n*cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_phydiaz,  cobalt%p_ndi(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_phypico,  cobalt%p_nsm(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_phymisc,  (cobalt%nlg_misc + cobalt%nmd_misc) * cobalt%c_2_n*cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_zmicro,  cobalt%p_nsmz(:,:,:,tau) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_zmeso,  (cobalt%p_nlgz(:,:,:,tau)+cobalt%p_nmdz(:,:,:,tau)) * &
            cobalt%c_2_n * cobalt%Rho_0, model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! alkalinity, pH, Carbonate system and O2
          used = g_send_data(cobalt%id_talk, cobalt%p_alk(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_ph, log10(cobalt%f_htotal+epsln) * (-1.0), &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_o2_cmip, cobalt%p_o2(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_o2sat, cobalt%o2sat, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_co3, cobalt%f_co3_ion*cobalt%Rho_0, &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_co3satcalc, cobalt%co3_sol_calc * cobalt%Rho_0, &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_co3satarag, cobalt%co3_sol_arag * cobalt%Rho_0, &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! Nutrients
          used = g_send_data(cobalt%id_no3_cmip, cobalt%p_no3(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_nh4_cmip, cobalt%p_nh4(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_po4_cmip, cobalt%p_po4(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_dfe, cobalt%p_fed(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_si, cobalt%p_sio4(:,:,:,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! Chlorophyll: CMIP asks for in kg Chl m-3
          used = g_send_data(cobalt%id_chl_cmip, cobalt%f_chl * cobalt%Rho_0 / 1.0e9, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_chldiat, (phyto(LARGE)%theta * cobalt%nlg_diatoms + &
            phyto(MEDIUM)%theta * cobalt%nmd_diatoms) * cobalt%c_2_n * cobalt%Rho_0 * 12.0e-3, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_chldiaz,  phyto(DIAZO)%theta * cobalt%p_ndi(:,:,:,tau) * &
            cobalt%c_2_n * cobalt%Rho_0 * 12.0e-3, &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_chlpico,  phyto(SMALL)%theta * cobalt%p_nsm(:,:,:,tau) * &
            cobalt%c_2_n * cobalt%Rho_0 * 12.0e-3, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_chlmisc, (phyto(LARGE)%theta * cobalt%nlg_misc + &
            phyto(MEDIUM)%theta * cobalt%nmd_misc) * cobalt%c_2_n*cobalt%Rho_0*12.0e-3, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! Aggregate Particulate C, N, P, Fe and Si pools
          used = g_send_data(cobalt%id_poc, (cobalt%p_ndi(:,:,:,tau)+cobalt%p_nlg(:,:,:,tau)+ &
            cobalt%p_nmd(:,:,:,tau)+cobalt%p_nsm(:,:,:,tau)+cobalt%p_nbact(:,:,:,tau)+cobalt%p_ndet(:,:,:,tau) + &
            cobalt%p_nsmz(:,:,:,tau)+cobalt%p_nmdz(:,:,:,tau)+cobalt%p_nlgz(:,:,:,tau))*cobalt%Rho_0*cobalt%c_2_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_pon, (cobalt%p_ndi(:,:,:,tau)+cobalt%p_nlg(:,:,:,tau)+ &
            cobalt%p_nmd(:,:,:,tau)+cobalt%p_nsm(:,:,:,tau)+cobalt%p_nbact(:,:,:,tau)+cobalt%p_ndet(:,:,:,tau)+ &
            cobalt%p_nsmz(:,:,:,tau)+cobalt%p_nmdz(:,:,:,tau)+cobalt%p_nlgz(:,:,:,tau))*cobalt%Rho_0, &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_pop, (cobalt%p_pdi(:,:,:,tau)+cobalt%p_plg(:,:,:,tau)+ &
            cobalt%p_pmd(:,:,:,tau)+cobalt%p_psm(:,:,:,tau)+bact(1)%q_p_2_n*cobalt%p_nbact(:,:,:,tau)+ &
            cobalt%p_pdet(:,:,:,tau)+zoo(1)%q_p_2_n*cobalt%p_nsmz(:,:,:,tau)+zoo(2)%q_p_2_n*cobalt%p_nmdz(:,:,:,tau)+ &
            zoo(3)%q_p_2_n*cobalt%p_nlgz(:,:,:,tau))*cobalt%Rho_0,  &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bfe, (cobalt%p_fedi(:,:,:,tau) + cobalt%p_felg(:,:,:,tau) + &
            cobalt%p_femd(:,:,:,tau) + cobalt%p_fesm(:,:,:,tau) + cobalt%p_fedet(:,:,:,tau))*cobalt%Rho_0, &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bsi, (cobalt%p_silg(:,:,:,tau) + cobalt%p_simd(:,:,:,tau) + &
            cobalt%p_sidet(:,:,:,tau))*cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! Phytoplankton partitioned by nutrients
          used = g_send_data(cobalt%id_phyn, (cobalt%p_nlg(:,:,:,tau) + cobalt%p_nmd(:,:,:,tau) +  &
            cobalt%p_nsm(:,:,:,tau) + cobalt%p_ndi(:,:,:,tau)) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_phyp, (cobalt%p_pdi(:,:,:,tau) + cobalt%p_plg(:,:,:,tau) + &
            cobalt%p_pmd(:,:,:,tau) + cobalt%p_psm(:,:,:,tau))*cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_phyfe,  (cobalt%p_fedi(:,:,:,tau) + cobalt%p_felg(:,:,:,tau) +  &
            cobalt%p_femd(:,:,:,tau) + cobalt%p_fesm(:,:,:,tau)) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_physi, (cobalt%p_silg(:,:,:,tau) + cobalt%p_simd(:,:,:,tau))*cobalt%Rho_0, & 
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! PENDING: Does CMIP want this?  Do we have them?
          !used = g_send_data(cobalt%id_dissicnat, (define quantity),  &
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_dissicabio, (define quantity), &      
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_dissi14cabio, (define quantity), &
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_talknat, (define quantity), &
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_phnat, (define quantity), &
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_phabio, (define quantity), &
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_co3nat, (define quantity),  &
          !  model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_co3abio, (define quantity),  &
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_co3nat, (define quantity),  &
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !used = g_send_data(cobalt%id_co3abio, (define quantity),  &
          !  model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

          !
          ! CMIP 3D Sinking Variables
          !
          used = g_send_data(cobalt%id_expc, (cobalt%p_ndet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_nsm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + cobalt%p_nmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
            cobalt%p_nlg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + cobalt%p_ndi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:)) * &
            cobalt%c_2_n*cobalt%Rho_0*grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_expn, (cobalt%p_ndet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_nsm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + cobalt%p_nmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
            cobalt%p_nlg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + cobalt%p_ndi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:)) * &
            cobalt%Rho_0*grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_expp, (cobalt%p_pdet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_psm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + cobalt%p_pmd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + & 
            cobalt%p_plg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + cobalt%p_pdi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:)) * & 
            cobalt%Rho_0*grid_tmask(:,:,:), model_time, rmask = grid_tmask, & 
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_expfe, (cobalt%p_fedet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_fesm(:,:,:,tau)*phyto(SMALL)%vmove(:,:,:) + cobalt%p_femd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:) + &
            cobalt%p_felg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:) + cobalt%p_fedi(:,:,:,tau)*phyto(DIAZO)%vmove(:,:,:)) * &
            cobalt%Rho_0*grid_tmask(:,:,:), model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_expsi, (cobalt%p_sidet(:,:,:,tau)*cobalt%wsink + &
            cobalt%p_simd(:,:,:,tau)*phyto(MEDIUM)%vmove(:,:,:)+cobalt%p_silg(:,:,:,tau)*phyto(LARGE)%vmove(:,:,:)) * &
            cobalt%Rho_0*grid_tmask(:,:,:),model_time, rmask = grid_tmask, &
            is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_expcalc, cobalt%p_cadet_calc(:,:,:,tau)*cobalt%Rho_0*cobalt%wsink, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_exparag, cobalt%p_cadet_arag(:,:,:,tau)*cobalt%Rho_0*cobalt%wsink, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !
          ! Surface CMIP variables (extract directly at specified depth from 3D fields?)
          !
          used = g_send_data(cobalt%id_dissicos, cobalt%p_dic(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dissocos, cobalt%dissoc(:,:,1) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phycos, (cobalt%p_nlg(:,:,1,tau) + cobalt%p_nmd(:,:,1,tau) +  &
            cobalt%p_nsm(:,:,1,tau) + cobalt%p_ndi(:,:,1,tau)) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_zoocos, (cobalt%p_nlgz(:,:,1,tau) + cobalt%p_nsmz(:,:,1,tau) +  &
            cobalt%p_nmdz(:,:,1,tau)) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_baccos, cobalt%p_nbact(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_detocos, cobalt%p_ndet(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0,  &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_calcos, cobalt%p_cadet_calc(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_aragos, cobalt%p_cadet_arag(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phydiatos, (cobalt%nlg_diatoms(:,:,1) + cobalt%nmd_diatoms(:,:,1)) * &
            cobalt%c_2_n * cobalt%Rho_0, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phydiazos, cobalt%p_ndi(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phypicoos, cobalt%p_nsm(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phymiscos, (cobalt%nlg_misc(:,:,1) + cobalt%nmd_misc(:,:,1)) * &
            cobalt%c_2_n * cobalt%Rho_0, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_zmicroos, cobalt%p_nsmz(:,:,1,tau) * cobalt%c_2_n * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_zmesoos, (cobalt%p_nlgz(:,:,1,tau)+cobalt%p_nmdz(:,:,1,tau)) * &
            cobalt%c_2_n * cobalt%Rho_0, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_talkos,  cobalt%p_alk(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phos,  log10(cobalt%f_htotal(:,:,1)+epsln) * (-1.0), &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_o2os, cobalt%p_o2(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_o2satos,  cobalt%o2sat(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_no3os,  cobalt%p_no3(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_nh4os,  cobalt%p_nh4(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_po4os,  cobalt%p_po4(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dfeos,  cobalt%p_fed(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sios,  cobalt%p_sio4(:,:,1,tau) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_chlos,  cobalt%f_chl(:,:,1) * cobalt%Rho_0 / 1.0e9, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_chldiatos,  (phyto(LARGE)%theta(:,:,1) * cobalt%nlg_diatoms(:,:,1) + &
            phyto(MEDIUM)%theta(:,:,1) * cobalt%nmd_diatoms(:,:,1)) * cobalt%c_2_n * cobalt%Rho_0 * 12.0e-3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_chldiazos,  phyto(DIAZO)%theta(:,:,1) * cobalt%p_ndi(:,:,1,tau) * &
            cobalt%c_2_n * cobalt%Rho_0 * 12.0e-3, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_chlpicoos,  phyto(SMALL)%theta(:,:,1) * cobalt%p_nsm(:,:,1,tau) * &
            cobalt%c_2_n * cobalt%Rho_0 * 12.0e-3, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_chlmiscos,  (phyto(LARGE)%theta(:,:,1) * cobalt%nlg_misc(:,:,1) + &
            phyto(MEDIUM)%theta(:,:,1) * cobalt%nmd_misc(:,:,1)) * cobalt%c_2_n * cobalt%Rho_0 * 12.0e-3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_ponos, (cobalt%p_ndi(:,:,1,tau)+cobalt%p_nlg(:,:,1,tau)+ &
            cobalt%p_nmd(:,:,1,tau)+cobalt%p_nsm(:,:,1,tau)+cobalt%p_nbact(:,:,1,tau)+cobalt%p_ndet(:,:,1,tau)+ &
            cobalt%p_nsmz(:,:,1,tau)+cobalt%p_nmdz(:,:,1,tau)+cobalt%p_nlgz(:,:,1,tau))*cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_popos, (cobalt%p_pdi(:,:,1,tau)+cobalt%p_plg(:,:,1,tau)+ & 
            cobalt%p_pmd(:,:,1,tau)+cobalt%p_psm(:,:,1,tau)+bact(1)%q_p_2_n*cobalt%p_nbact(:,:,1,tau)+ &
            cobalt%p_pdet(:,:,1,tau)+zoo(1)%q_p_2_n*cobalt%p_nsmz(:,:,1,tau)+ &
            zoo(2)%q_p_2_n*cobalt%p_nmdz(:,:,1,tau)+zoo(3)%q_p_2_n*cobalt%p_nlgz(:,:,1,tau))*cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_bfeos, (cobalt%p_fedi(:,:,1,tau) + cobalt%p_felg(:,:,1,tau) + &
            cobalt%p_femd(:,:,1,tau) + cobalt%p_fesm(:,:,1,tau) + cobalt%p_fedet(:,:,1,tau))*cobalt%Rho_0, & 
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_bsios,  (cobalt%p_silg(:,:,1,tau) + cobalt%p_simd(:,:,1,tau) + &
            cobalt%p_sidet(:,:,1,tau)) * cobalt%Rho_0, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phynos,  (cobalt%p_nlg(:,:,1,tau) + cobalt%p_nmd(:,:,1,tau) +  &
            cobalt%p_nsm(:,:,1,tau) + cobalt%p_ndi(:,:,1,tau)) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phypos, (cobalt%p_pdi(:,:,1,tau) + cobalt%p_plg(:,:,1,tau) + &
            cobalt%p_pmd(:,:,1,tau) + cobalt%p_psm(:,:,1,tau))*cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_phyfeos,  (cobalt%p_fedi(:,:,1,tau) + cobalt%p_felg(:,:,1,tau) + &
            cobalt%p_femd(:,:,1,tau) + cobalt%p_fesm(:,:,1,tau)) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_physios, (cobalt%p_silg(:,:,1,tau) + cobalt%p_simd(:,:,1,tau))*cobalt%Rho_0, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_co3os,  cobalt%f_co3_ion(:,:,1) * cobalt%Rho_0,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_co3satcalcos,  cobalt%co3_sol_calc(:,:,1) * cobalt%Rho_0,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_co3sataragos,  cobalt%co3_sol_arag(:,:,1) * cobalt%Rho_0,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! PENDING: Does CMIP want this?  Do we have them?
          !used = g_send_data(cobalt%id_dissicnatos, (define quantity), &
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_dissicabioos,  (define quantity), &
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_dissi14cabioos, (define quantity), &
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_talknatos, (define quantity), &
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_phnatos,  log10(cobalt%f_htotal(:,:,1)+epsln) * -1.0, &
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_phabioos,  log10(cobalt%f_htotal(:,:,1)+epsln) * -1.0, &
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! used = g_send_data(cobalt%id_co3natos,  cobalt%f_co3_ion(:,:,1) * cobalt%Rho_0,  &
          !   model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_co3abioos,  cobalt%f_co3_ion(:,:,1) * cobalt%Rho_0,  &
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

          ! Air-Sea flux variables - here or with fluxes?
          used = g_send_data(cobalt%id_spco2,  cobalt%pco2_csurf * 0.1013, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dpco2,  cobalt%deltap_dic * 0.1013, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dpo2, cobalt%deltap_o2 * 0.1013, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fgco2,  cobalt%stf_gas_dic * 12.0e-3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fgo2, cobalt%stf_gas_o2, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_spco2nat,  cobalt%pco2_csurf * 0.1013, &
          !  model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_spco2abio,  cobalt%pco2_csurf * 0.1013,   &
          !  model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_dpco2nat,  cobalt%dic_deltap * 0.1013,   &
          !  model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_dpco2abio,  cobalt%dic_deltap * 0.1013,   &
          !  model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_fgco2nat,
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_fgco2abio,
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !used = g_send_data(cobalt%id_fg14co2abio,
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

          !
          ! 100m sinking flux (should derive 3D field and MOM6 interpolation?)
          !
          used = g_send_data(cobalt%id_epc100, cobalt%fntot_100 * cobalt%c_2_n,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_epn100, cobalt%fntot_100, &
            model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_epp100, cobalt%fptot_100, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_epfe100, cobalt%ffetot_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_epsi100, cobalt%fsitot_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_epcalc100,  cobalt%fcadet_calc_100,   &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_eparag100, cobalt%fcadet_arag_100,   &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !
          ! Vertical integrals (kg m-2)
          !
          used = g_send_data(cobalt%id_intdic, cobalt%wc_vert_int_dic*12.0e-3,   &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intdoc, cobalt%wc_vert_int_doc*12.0e-3,   &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intpoc, cobalt%wc_vert_int_poc*12.0e-3,   &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

        case (.false.)
        ! Logic for "update_from_source" case

          !
          ! Note: All 3D rates have been changed from layer integrals to moles kg sec-1 to be more conistent with MOM6
          ! diagnostic approaches.  Layer integrals were used in MOM4/5 to facilitate the integration of fluxes over
          ! layers in a z/z* coordinate.  Fluxes are rarely saved in native coordinates for MOM6, however, and they
          ! vary with time, making he layer integrals difficult to interpret and interpolate.  Saving in rates in
          ! native units avoids this issue.
          !

          !
          ! Send phytoplankton diagnostic data
          !
          do n= 1, NUM_PHYTO
            used = g_send_data(phyto(n)%id_P_C_max, phyto(n)%P_C_max, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_alpha, phyto(n)%alpha, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_bresp, phyto(n)%bresp, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_def_fe, phyto(n)%def_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_felim, phyto(n)%felim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_irrlim, phyto(n)%irrlim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            ! N loss and mortality
            used = g_send_data(phyto(n)%id_jzloss_n, phyto(n)%jzloss_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jaggloss_n, phyto(n)%jaggloss_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jvirloss_n, phyto(n)%jvirloss_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jmortloss_n, phyto(n)%jmortloss_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jexuloss_n, phyto(n)%jexuloss_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jhploss_n, phyto(n)%jhploss_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            ! P loss and mortality
            used = g_send_data(phyto(n)%id_jzloss_p, phyto(n)%jzloss_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jaggloss_p, phyto(n)%jaggloss_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jvirloss_p, phyto(n)%jvirloss_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jmortloss_p, phyto(n)%jmortloss_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jexuloss_p, phyto(n)%jexuloss_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jhploss_p, phyto(n)%jhploss_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            ! Fe loss and mortality
            used = g_send_data(phyto(n)%id_jzloss_fe, phyto(n)%jzloss_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jaggloss_fe, phyto(n)%jaggloss_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jvirloss_fe, phyto(n)%jvirloss_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jmortloss_fe, phyto(n)%jmortloss_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jexuloss_fe, phyto(n)%jexuloss_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jhploss_fe, phyto(n)%jhploss_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            ! Si loss and mortality
            used = g_send_data(phyto(n)%id_jzloss_sio2, phyto(n)%jzloss_sio2, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jaggloss_sio2, phyto(n)%jaggloss_sio2, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jvirloss_sio2, phyto(n)%jvirloss_sio2, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jmortloss_sio2, phyto(n)%jmortloss_sio2, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jhploss_sio2, phyto(n)%jhploss_sio2, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            ! Uptake
            used = g_send_data(phyto(n)%id_juptake_fe, phyto(n)%juptake_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_juptake_nh4, phyto(n)%juptake_nh4, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_juptake_no3, phyto(n)%juptake_no3, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_juptake_po4, phyto(n)%juptake_po4,   &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_jprod_n, phyto(n)%jprod_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_liebig_lim,phyto(n)%liebig_lim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_mu, phyto(n)%mu, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_nh4lim, phyto(n)%nh4lim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_no3lim, phyto(n)%no3lim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_po4lim, phyto(n)%po4lim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_o2lim, phyto(n)%o2lim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_q_fe_2_n, phyto(n)%q_fe_2_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_q_p_2_n, phyto(n)%q_p_2_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_silim, phyto(n)%silim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_q_si_2_n, phyto(n)%q_si_2_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_theta, phyto(n)%theta, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_chl, phyto(n)%chl, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_f_mu_mem, phyto(n)%f_mu_mem, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_mu_mix, phyto(n)%mu_mix, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(phyto(n)%id_stress_fac, phyto(n)%stress_fac, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            ! Applied at lower interface
            used = g_send_data(phyto(n)%id_vmove, phyto(n)%vmove, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          enddo

          do n=2,3
            used = g_send_data(phyto(n)%id_juptake_sio4, phyto(n)%juptake_sio4, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          enddo 

          used = g_send_data(phyto(DIAZO)%id_juptake_n2, phyto(DIAZO)%juptake_n2,   &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

          !
          ! Send bacterial uptake, production and limitation diagnostic data
          !
          used = g_send_data(bact(1)%id_jzloss_n, bact(1)%jzloss_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_jvirloss_n, bact(1)%jvirloss_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_jzloss_p, bact(1)%jzloss_p, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_jvirloss_p, bact(1)%jvirloss_p, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_juptake_ldon, bact(1)%juptake_ldon, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_juptake_ldop, bact(1)%juptake_ldop, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_jprod_nh4, bact(1)%jprod_nh4, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_jprod_po4, bact(1)%jprod_po4, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_jprod_n, bact(1)%jprod_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_o2lim, bact(1)%o2lim, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_ldonlim, bact(1)%ldonlim, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(bact(1)%id_temp_lim, bact(1)%temp_lim, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

          !
          ! Send zooplankton ingestion, production and limitation diagnostic data
          !
          do n= 1, NUM_ZOO
            used = g_send_data(zoo(n)%id_jzloss_n, zoo(n)%jzloss_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jhploss_n, zoo(n)%jhploss_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jzloss_p, zoo(n)%jzloss_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jhploss_p, zoo(n)%jhploss_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jingest_n, zoo(n)%jingest_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jingest_p, zoo(n)%jingest_p, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jingest_sio2, zoo(n)%jingest_sio2, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jingest_fe, zoo(n)%jingest_fe, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_ndet, zoo(n)%jprod_ndet, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_pdet, zoo(n)%jprod_pdet, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_ldon, zoo(n)%jprod_ldon, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_ldop, zoo(n)%jprod_ldop, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_sldon, zoo(n)%jprod_sldon, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_sldop, zoo(n)%jprod_sldop, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_srdon, zoo(n)%jprod_srdon, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_srdop, zoo(n)%jprod_srdop, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_fed,  zoo(n)%jprod_fed, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_fedet, zoo(n)%jprod_fedet, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_sidet, zoo(n)%jprod_sidet, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_sio4, zoo(n)%jprod_sio4, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_po4,  zoo(n)%jprod_po4, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_nh4,  zoo(n)%jprod_nh4, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_jprod_n, zoo(n)%jprod_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_o2lim, zoo(n)%o2lim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(zoo(n)%id_temp_lim, zoo(n)%temp_lim, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          enddo

          !
          ! General COBALT Production diagnostics (not specific to phytoplankton, zooplankton or bacteria)
          !
          used = g_send_data(cobalt%id_jprod_cadet_arag, cobalt%jprod_cadet_arag, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_cadet_calc, cobalt%jprod_cadet_calc, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_ndet, cobalt%jprod_ndet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_pdet, cobalt%jprod_pdet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_srdon, cobalt%jprod_srdon, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_sldon, cobalt%jprod_sldon, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_ldon, cobalt%jprod_ldon, &
            model_time, rmask = grid_tmask,is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_srdop, cobalt%jprod_srdop, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_sldop, cobalt%jprod_sldop, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_ldop, cobalt%jprod_ldop, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_nh4, cobalt%jprod_nh4, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_nh4_plus_btm, cobalt%jprod_nh4_plus_btm, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_po4, cobalt%jprod_po4, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_fed, cobalt%jprod_fed, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_fedet,  cobalt%jprod_fedet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_sidet, cobalt%jprod_sidet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_sio4, cobalt%jprod_sio4, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_lithdet, cobalt%jprod_lithdet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jdiss_cadet_arag, cobalt%jdiss_cadet_arag, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jdiss_cadet_calc, cobalt%jdiss_cadet_calc, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jdiss_sidet, cobalt%jdiss_sidet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jremin_ndet, cobalt%jremin_ndet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jremin_pdet, cobalt%jremin_pdet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jremin_fedet, cobalt%jremin_fedet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_det_jzloss_n, cobalt%det_jzloss_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_det_jhploss_n, cobalt%det_jhploss_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jfed, cobalt%jfed, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jfe_ads, cobalt%jfe_ads, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          ! Additional iron scavenging diagnostics aligned with adsorption to help understand rates
          used = g_send_data(cobalt%id_kfe_eq_lig, log10(cobalt%kfe_eq_lig+epsln), &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_feprime, cobalt%feprime, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_ligand, cobalt%ligand, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fe_sol, cobalt%fe_sol, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jfe_coast, cobalt%jfe_coast, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jfe_iceberg, cobalt%jfe_iceberg, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jno3_iceberg, cobalt%jno3_iceberg, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jpo4_iceberg, cobalt%jpo4_iceberg, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jno3denit_wc,  cobalt%jno3denit_wc, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_juptake_nh4amx, cobalt%juptake_nh4amx, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_juptake_no3amx, cobalt%juptake_no3amx, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jo2resp_wc,  cobalt%jo2resp_wc, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jprod_no3nitrif, cobalt%jprod_no3nitrif,       &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_juptake_nh4nitrif, cobalt%juptake_nh4nitrif,       &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jnamx, cobalt%jnamx, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !
          ! Other general COBALT limitation and forcing terms
          !
          used = g_send_data(cobalt%id_expkT, cobalt%expkT, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_expkreminT, cobalt%expkreminT, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_hp_o2lim, cobalt%hp_o2lim, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_hp_temp_lim, cobalt%hp_temp_lim,  &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_irr_inst, cobalt%irr_inst, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_irr_mix, cobalt%irr_mix, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_irr_aclm_inst, cobalt%irr_aclm_inst, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_irr_aclm, cobalt%f_irr_aclm, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_irr_aclm_z, cobalt%f_irr_aclm_z, &
            model_time, rmask = grid_tmask(:,:,:), is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_total_filter_feeding,cobalt%total_filter_feeding, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !
          ! Benthic fluxes and processes applied during the time step
          !
          used = g_send_data(cobalt%id_b_alk, -cobalt%b_alk, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_b_dic, -cobalt%b_dic, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_b_fed, -cobalt%b_fed, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_b_nh4, -cobalt%b_nh4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_b_no3, -cobalt%b_no3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_b_o2, -cobalt%b_o2, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_b_po4, -cobalt%b_po4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_b_sio4, -cobalt%b_sio4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fcased_burial, cobalt%fcased_burial, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fcased_redis, cobalt%fcased_redis, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fcased_redis_surfresp,cobalt%fcased_redis_surfresp, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_cased_redis_coef, cobalt%cased_redis_coef, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_cased_redis_delz,  cobalt%cased_redis_delz, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_ffe_sed, cobalt%ffe_sed, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_ffe_geotherm,  cobalt%ffe_geotherm, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_ffe_iceberg,  cobalt%ffe_iceberg, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fnso4red_sed,cobalt%fnso4red_sed, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fno3denit_sed, cobalt%fno3denit_sed, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fnoxic_sed, cobalt%fnoxic_sed, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_frac_burial, cobalt%frac_burial, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fn_burial, cobalt%fn_burial, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_fp_burial, cobalt%fp_burial, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_cased_2d, cobalt%cased_2d, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          !
          ! Radiocarbon fields
          !
          if (do_14c) then
            used = g_send_data(cobalt%id_b_di14c, cobalt%b_di14c, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
            used = g_send_data(cobalt%id_c14_2_n, cobalt%c14_2_n, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(cobalt%id_c14o2_csurf, cobalt%c14o2_csurf, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
            used = g_send_data(cobalt%id_c14o2_alpha, cobalt%c14o2_alpha, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
            used = g_send_data(cobalt%id_fpo14c, cobalt%fpo14c, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(cobalt%id_j14c_decay_dic, cobalt%j14c_decay_dic, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(cobalt%id_j14c_decay_doc, cobalt%j14c_decay_doc, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(cobalt%id_j14c_reminp, cobalt%j14c_reminp, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(cobalt%id_jdi14c, cobalt%jdi14c, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
            used = g_send_data(cobalt%id_jdo14c, cobalt%jdo14c, &
              model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          endif

          !
          ! Surface light and acclimation diagnostics (handle in diag table?)
          !
          used = g_send_data(cobalt%id_sfc_irr, cobalt%irr_inst(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_sfc_irr_aclm, cobalt%f_irr_aclm_sfc(:,:,1), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_mld_aclm, cobalt%mld_aclm, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc,ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_daylength, cobalt%daylength, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)


          !
          ! Surface Phytplankton Limitations and Rates (handle in diag tables?)
          ! 
          do n= 1, NUM_PHYTO
            used = g_send_data(phyto(n)%id_sfc_def_fe, phyto(n)%def_fe(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_felim, phyto(n)%felim(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_irrlim, phyto(n)%irrlim(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_theta, phyto(n)%theta(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_mu, phyto(n)%mu(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_po4lim, phyto(n)%po4lim(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_q_fe_2_n, phyto(n)%q_fe_2_n(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_q_p_2_n, phyto(n)%q_p_2_n(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_nh4lim, phyto(n)%nh4lim(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_sfc_no3lim, phyto(n)%no3lim(:,:,1), &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          enddo

          !
          ! Save river, depositon and bulk elemental fluxes
          !
          used = g_send_data(cobalt%id_dep_dry_fed, cobalt%dry_fed, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_dry_lith, cobalt%dry_lith, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_dry_nh4, cobalt%dry_nh4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_dry_no3, cobalt%dry_no3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_dry_po4, cobalt%dry_po4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_wet_fed, cobalt%wet_fed, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_wet_lith, cobalt%wet_lith, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_wet_nh4, cobalt%wet_nh4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_wet_no3, cobalt%wet_no3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_dep_wet_po4, cobalt%wet_po4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_alk, cobalt%runoff_flux_alk, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_dic, cobalt%runoff_flux_dic, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_di14c, cobalt%runoff_flux_di14c, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_fed, cobalt%runoff_flux_fed, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_lith, cobalt%runoff_flux_lith, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_no3, cobalt%runoff_flux_no3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_ldon, cobalt%runoff_flux_ldon, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_sldon, cobalt%runoff_flux_sldon, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_srdon, cobalt%runoff_flux_srdon, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_ndet, cobalt%runoff_flux_ndet, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_pdet, cobalt%runoff_flux_pdet, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_po4, cobalt%runoff_flux_po4, &
            model_time, rmask = grid_tmask(:,:,1),is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_ldop, cobalt%runoff_flux_ldop, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_sldop, cobalt%runoff_flux_sldop, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_runoff_flux_srdop, cobalt%runoff_flux_srdop, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

          !
          ! Save 100m integral fluxes (move calculation here for consistency with post_vertdiff?)
          !
          ! Phytoplankton 100m flux integrals
          used = g_send_data(cobalt%id_jprod_allphytos_100, cobalt%jprod_allphytos_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jprod_allphytos_200, cobalt%jprod_allphytos_200, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jprod_diat_100, cobalt%jprod_diat_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          do n= 1, NUM_PHYTO  !{
            used = g_send_data(phyto(n)%id_jprod_n_100, phyto(n)%jprod_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_jprod_n_new_100, phyto(n)%jprod_n_new_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_jzloss_n_100, phyto(n)%jzloss_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_jexuloss_n_100, phyto(n)%jexuloss_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_jvirloss_n_100, phyto(n)%jvirloss_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_jmortloss_n_100, phyto(n)%jmortloss_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(phyto(n)%id_jaggloss_n_100, phyto(n)%jaggloss_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          enddo !} n
          used = g_send_data(phyto(DIAZO)%id_jprod_n_n2_100, phyto(DIAZO)%jprod_n_n2_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !
          ! Zooplankton 100m flux integrals - generalized to define production terms for all zooplankton regardless of
          ! default settings.  This may create zero arrays in some cases, but supports setting changes
          !
          do n= 1, NUM_ZOO  !{
            used = g_send_data(zoo(n)%id_jprod_n_100, zoo(n)%jprod_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(zoo(n)%id_jingest_n_100, zoo(n)%jingest_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(zoo(n)%id_jremin_n_100, zoo(n)%jremin_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(zoo(n)%id_jzloss_n_100, zoo(n)%jzloss_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(zoo(n)%id_jprod_don_100, zoo(n)%jprod_don_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(zoo(n)%id_jhploss_n_100, zoo(n)%jhploss_n_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
            used = g_send_data(zoo(n)%id_jprod_ndet_100, zoo(n)%jprod_ndet_100, &
              model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          enddo !} n
          used = g_send_data(cobalt%id_jprod_mesozoo_200, cobalt%jprod_mesozoo_200, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! Higher predator 100m flux integrals
          used = g_send_data(cobalt%id_hp_jingest_n_100, cobalt%hp_jingest_n_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_hp_jremin_n_100, cobalt%hp_jremin_n_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_hp_jprod_ndet_100, cobalt%hp_jprod_ndet_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !
          ! Bacteria 100m flux integrals
          !
          used = g_send_data(bact(1)%id_jprod_n_100, bact(1)%jprod_n_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(bact(1)%id_jzloss_n_100, bact(1)%jzloss_n_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(bact(1)%id_jvirloss_n_100, bact(1)%jvirloss_n_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(bact(1)%id_jremin_n_100, bact(1)%jremin_n_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(bact(1)%id_juptake_ldon_100, bact(1)%juptake_ldon_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !
          ! Detritus production 100m flux integrals
          !
          used = g_send_data(cobalt%id_jprod_lithdet_100, cobalt%jprod_lithdet_100, &
            model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jprod_sidet_100, cobalt%jprod_sidet_100, &
            model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jprod_cadet_calc_100, cobalt%jprod_cadet_calc_100, &
            model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jprod_cadet_arag_100, cobalt%jprod_cadet_arag_100, &
            model_time, rmask = grid_tmask(:,:,1),  is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jremin_ndet_100, cobalt%jremin_ndet_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !
          ! Water column flux integrals
          !
          used = g_send_data(cobalt%id_wc_vert_int_npp, cobalt%wc_vert_int_npp, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jdiss_sidet, cobalt%wc_vert_int_jdiss_sidet, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jdiss_cadet, cobalt%wc_vert_int_jdiss_cadet, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jo2resp, cobalt%wc_vert_int_jo2resp, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jprod_cadet, cobalt%wc_vert_int_jprod_cadet, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jno3denit, cobalt%wc_vert_int_jno3denit, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jprod_no3nitrif, cobalt%wc_vert_int_jprod_no3nitrif, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jnamx, cobalt%wc_vert_int_jnamx, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_juptake_nh4, cobalt%wc_vert_int_juptake_nh4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jprod_nh4, cobalt%wc_vert_int_jprod_nh4, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_juptake_no3, cobalt%wc_vert_int_juptake_no3, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_nfix, cobalt%wc_vert_int_nfix, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jfe_iceberg, cobalt%wc_vert_int_jfe_iceberg, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jno3_iceberg, cobalt%wc_vert_int_jno3_iceberg, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_wc_vert_int_jpo4_iceberg, cobalt%wc_vert_int_jpo4_iceberg, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          !
          ! Additional 3D flux diagnostics
          !
          used = g_send_data(cobalt%id_jalk, cobalt%jalk, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jalk_plus_btm, cobalt%jalk_plus_btm, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jdiss_cadet_arag_plus_btm, cobalt%jdiss_cadet_arag_plus_btm, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jdiss_cadet_calc_plus_btm, cobalt%jdiss_cadet_calc_plus_btm, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk) 
          used = g_send_data(cobalt%id_jdic, cobalt%jdic, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jno3, cobalt%jno3, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jpo4, cobalt%jpo4, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jsio4, cobalt%jsio4, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jdic_plus_btm, cobalt%jdic_plus_btm, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jnh4, cobalt%jnh4, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jndet, cobalt%jndet, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jnh4_plus_btm, cobalt%jnh4_plus_btm, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jo2_plus_btm, cobalt%jo2_plus_btm, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_jo2, cobalt%jo2, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)

          !
          ! CMIP marine biogeochemical fluxes and other fields
          !
          ! Note: *rho_dzt/dzt in unescessarily complex, now just multiply by cobalt%Rho_0
          used = g_send_data(cobalt%id_pp,  (phyto(DIAZO)%jprod_n +  phyto(LARGE)%jprod_n + &
            phyto(MEDIUM)%jprod_n + phyto(SMALL)%jprod_n) * cobalt%Rho_0 * cobalt%c_2_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_pnitrate,  (phyto(DIAZO)%juptake_no3 +  phyto(LARGE)%juptake_no3 + &
            phyto(MEDIUM)%juptake_no3 + phyto(SMALL)%juptake_no3) * cobalt%Rho_0 * cobalt%c_2_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_pphosphate,  (phyto(DIAZO)%juptake_po4 +  phyto(LARGE)%juptake_po4 + &
            phyto(MEDIUM)%juptake_po4 + phyto(SMALL)%juptake_po4) *  cobalt%Rho_0 * cobalt%c_2_n, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_pbfe,  (phyto(DIAZO)%juptake_fe +  phyto(LARGE)%juptake_fe + &
            phyto(MEDIUM)%juptake_fe + phyto(SMALL)%juptake_fe) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_pbsi, (phyto(LARGE)%juptake_sio4 + phyto(MEDIUM)%juptake_sio4) * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_pcalc, cobalt%jprod_cadet_calc * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_parag,  cobalt%jprod_cadet_arag * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_remoc, cobalt%jprod_nh4_plus_btm*cobalt%c_2_n*cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_dcalc,  cobalt%jdiss_cadet_calc_plus_btm*cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_darag,  cobalt%jdiss_cadet_arag_plus_btm*cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_ppdiat, (phyto(LARGE)%jprod_n * phyto(LARGE)%silim + &
            phyto(MEDIUM)%jprod_n * phyto(MEDIUM)%silim) * cobalt%Rho_0 * cobalt%c_2_n,  &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_ppdiaz,  phyto(DIAZO)%jprod_n * cobalt%Rho_0 * cobalt%c_2_n,  &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_pppico,  phyto(SMALL)%jprod_n *  cobalt%Rho_0 * cobalt%c_2_n,  &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_ppmisc, ((phyto(LARGE)%jprod_n * (1.0 - phyto(LARGE)%silim)) + &
            (phyto(MEDIUM)%jprod_n * (1.0 - phyto(MEDIUM)%silim))) * cobalt%Rho_0 * cobalt%c_2_n,  &
            model_time, rmask = grid_tmask,  is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bddtdic, cobalt%jdic_plus_btm * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bddtdin, cobalt%jdin_plus_btm * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bddtdip, cobalt%jpo4_plus_btm * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bddtdife, cobalt%jfed_plus_btm * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bddtdisi, cobalt%jsio4_plus_btm * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_bddtalk, cobalt%jalk_plus_btm * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fescav, cobalt%jfe_ads * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_fediss, cobalt%jremin_fedet * cobalt%Rho_0, &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          used = g_send_data(cobalt%id_graz, (phyto(DIAZO)%jzloss_n + phyto(LARGE)%jzloss_n + &
            phyto(MEDIUM)%jzloss_n + phyto(SMALL)%jzloss_n) * cobalt%c_2_n  * cobalt%Rho_0,  &
            model_time, rmask = grid_tmask, is_in=isc, js_in=jsc, ks_in=1,ie_in=iec, je_in=jec, ke_in=nk)
          !
          ! CMIP 100m biomass-weighted limitation terms 
          ! (recommend using surface to avoid aliasing the limitation with information from below the nutricline)
          ! (***needs to update these for 4P formulation***) 
          !
          used = g_send_data(cobalt%id_limndiat, phyto(LARGE)%nlim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! Not outputting/serving limndiaz because diazotrophs are not N limited
          ! used = g_send_data(cobalt%id_limndiaz, phyto(DIAZO)%nlim_bw_100, &
          !  model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limnpico, phyto(SMALL)%nlim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limnmisc, phyto(LARGE)%nlim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limirrdiat, phyto(LARGE)%irrlim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limirrdiaz, phyto(DIAZO)%irrlim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limirrpico, phyto(SMALL)%irrlim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limirrmisc, phyto(LARGE)%irrlim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limfediat, phyto(LARGE)%def_fe_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limfediaz, phyto(DIAZO)%def_fe_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limfepico, phyto(SMALL)%def_fe_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limfemisc, phyto(LARGE)%def_fe_bw_100,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limpdiat, phyto(LARGE)%plim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limpdiaz, phyto(DIAZO)%plim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limppico, phyto(SMALL)%plim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_limpmisc, phyto(LARGE)%plim_bw_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

          ! should this be 100m or full water column?
          used = g_send_data(cobalt%id_intpp,  cobalt%jprod_allphytos_100 * cobalt%c_2_n, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intppnitrate,  (phyto(DIAZO)%jprod_n_new_100 +  phyto(LARGE)%jprod_n_new_100 + &
            phyto(MEDIUM)%jprod_n_new_100 + phyto(SMALL)%jprod_n_new_100) * cobalt%c_2_n, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intppdiat,  cobalt%jprod_diat_100 * cobalt%c_2_n,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intppdiaz,  phyto(DIAZO)%jprod_n_100 * cobalt%c_2_n, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intpppico,  phyto(SMALL)%jprod_n_100 * cobalt%c_2_n, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intppmisc, (phyto(LARGE)%jprod_n_100 + phyto(MEDIUM)%jprod_n_100 - &
            cobalt%jprod_diat_100) *cobalt%c_2_n, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intpbn,  cobalt%jprod_allphytos_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intpbp, (phyto(DIAZO)%juptake_po4_100 +  phyto(LARGE)%juptake_po4_100 +  &
            phyto(MEDIUM)%juptake_po4_100 + phyto(SMALL)%juptake_po4_100), model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intpbfe,  (phyto(DIAZO)%juptake_fe_100 +  phyto(LARGE)%juptake_fe_100 +  &
            phyto(MEDIUM)%juptake_fe_100 + phyto(SMALL)%juptake_fe_100), model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intpbsi,  phyto(LARGE)%juptake_sio4_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intpcalcite,  cobalt%jprod_cadet_calc_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intparag,  cobalt%jprod_cadet_arag_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! CAS: Updated on 3/9/2021 to include sediment dissolution based on variable long name, also added
          !      aragonite flux to the sediment (which is instantaneously redissolved)
          used = g_send_data(cobalt%id_icfriver,  cobalt%runoff_flux_dic + cobalt%fcased_redis + &
            cobalt%fcadet_arag_btm,model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! CAS: Updated on 3/9/2021 to exclude calcite dissolution but include the aragonite flux to
          !      the bottom
          used = g_send_data(cobalt%id_fric,  cobalt%fcadet_calc_btm + cobalt%fcadet_arag_btm,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! CAS: variable includes release from sediment, but there is no organic carbon release from
          !      sediment in COBALT
          used = g_send_data(cobalt%id_ocfriver, cobalt%c_2_n* &
            (cobalt%runoff_flux_ldon+cobalt%runoff_flux_sldon+cobalt%runoff_flux_srdon), &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! CAS: Updated on 3/9/2021 to reflect that the total loss of organic carbon at sediments is
          !      equal to the total flux, not just the burial
          used = g_send_data(cobalt%id_froc,cobalt%c_2_n*cobalt%fndet_btm, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_intpn2,  cobalt%wc_vert_int_nfix,  &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! CAS: Updated on 3/9/2021 to include nh4 deposition and riverine fluxes of organic nitrogen
          used = g_send_data(cobalt%id_fsn,  cobalt%runoff_flux_no3 + cobalt%dry_no3 + cobalt%wet_no3 + &
            cobalt%dry_nh4 + cobalt%wet_nh4 + cobalt%runoff_flux_ldon + cobalt%runoff_flux_sldon + &
            cobalt%runoff_flux_srdon + cobalt%wc_vert_int_nfix, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! JYL: Updated on 3/21/2021 to include anammox
          used = g_send_data(cobalt%id_frn,  cobalt%fno3denit_sed + cobalt%wc_vert_int_jno3denit + &
            cobalt%wc_vert_int_jnamx + cobalt%fn_burial, model_time, rmask = grid_tmask(:,:,1), &
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          ! CAS: Updated on 3/9/2021 to include ffe_iceberg
          used = g_send_data(cobalt%id_fsfe,  cobalt%runoff_flux_fed + cobalt%dry_fed + cobalt%wet_fed + &
            cobalt%ffe_sed+cobalt%ffe_geotherm+cobalt%ffe_iceberg, model_time, rmask = grid_tmask(:,:,1), & 
            is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_frfe,  cobalt%ffedet_btm, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

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

          used = g_send_data(cobalt%id_jdic_100, cobalt%jdic_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jdin_100, cobalt%jdin_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jpo4_100, cobalt%jpo4_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jfed_100, cobalt%jfed_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jsio4_100, cobalt%jsio4_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)
          used = g_send_data(cobalt%id_jalk_100, cobalt%jalk_100, &
            model_time, rmask = grid_tmask(:,:,1), is_in=isc, js_in=jsc, ie_in=iec, je_in=jec)

!==============================================================================================================

      end select

    end subroutine cobalt_send_diagnostics   

end module COBALT_send_diag   
