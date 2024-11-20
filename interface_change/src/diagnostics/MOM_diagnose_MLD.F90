!> Provides functions for some diabatic processes such as fraxil, brine rejection,
!! tendency due to surface flux divergence.
module MOM_diagnose_mld

! This file is part of MOM6. See LICENSE.md for the license.

use MOM_diag_mediator, only : post_data
use MOM_diag_mediator, only : diag_ctrl
use MOM_EOS,           only : calculate_density, calculate_TFreeze
use MOM_error_handler, only : MOM_error, FATAL, WARNING
use MOM_grid,          only : ocean_grid_type
use MOM_interface_heights, only : thickness_to_dz
use MOM_unit_scaling,  only : unit_scale_type
use MOM_variables,     only : thermo_var_ptrs
use MOM_verticalGrid,  only : verticalGrid_type

implicit none ; private

#include <MOM_memory.h>

public diagnoseMLDbyDensityDifference

! A note on unit descriptions in comments: MOM6 uses units that can be rescaled for dimensional
! consistency testing. These are noted in comments with units like Z, H, L, and T, along with
! their mks counterparts with notation like "a velocity [Z T-1 ~> m s-1]".  If the units
! vary with the Boussinesq approximation, the Boussinesq variant is given first.

contains
!> Diagnose a mixed layer depth (MLD) determined by a given density difference with the surface.
!> This routine is appropriate in MOM_diabatic_aux due to its position within the time stepping.
subroutine diagnoseMLDbyDensityDifference(id_MLD, h, tv, densityDiff, G, GV, US, diagPtr, &
                                          ref_h_mld, id_ref_z, id_ref_rho, id_N2subML, id_MLDsq, dz_subML, MLD_out)
  type(ocean_grid_type),   intent(in) :: G           !< Grid type
  type(verticalGrid_type), intent(in) :: GV          !< ocean vertical grid structure
  type(unit_scale_type),   intent(in) :: US          !< A dimensional unit scaling type
  integer,                 intent(in) :: id_MLD      !< Handle (ID) of MLD diagnostic
  real, dimension(SZI_(G),SZJ_(G),SZK_(GV)), &
                           intent(in) :: h           !< Layer thickness [H ~> m or kg m-2]
  type(thermo_var_ptrs),   intent(in) :: tv          !< Structure containing pointers to any
                                                     !! available thermodynamic fields.
  real,                    intent(in) :: densityDiff !< Density difference to determine MLD [R ~> kg m-3]
  type(diag_ctrl),         pointer    :: diagPtr     !< Diagnostics structure
  real,                    intent(in) :: ref_h_mld   !< Depth of the calculated "surface" densisty [Z ~> m]
  integer,                 intent(in) :: id_ref_z    !< Handle (ID) of reference depth diagnostic
  integer,                 intent(in) :: id_ref_rho  !< Handle (ID) of reference density diagnostic
  real, dimension(SZI_(G),SZJ_(G)), &
              optional, intent(inout) :: MLD_out     !< Send MLD to other routines [Z ~> m]
  integer,       optional, intent(in) :: id_N2subML  !< Optional handle (ID) of subML stratification
  integer,       optional, intent(in) :: id_MLDsq    !< Optional handle (ID) of squared MLD
  real,          optional, intent(in) :: dz_subML    !< The distance over which to calculate N2subML
                                                     !! or 50 m if missing [Z ~> m]

  ! Local variables
  real, dimension(SZI_(G)) :: deltaRhoAtKm1, deltaRhoAtK ! Density differences [R ~> kg m-3].
  real, dimension(SZI_(G)) :: pRef_MLD, pRef_N2 ! Reference pressures [R L2 T-2 ~> Pa].
  real, dimension(SZI_(G)) :: hRef_MLD          ! Reference depth  [Z ~> m].
  real, dimension(SZI_(G)) :: H_subML, dH_N2    ! Summed thicknesses used in N2 calculation [H ~> m or kg m-2]
  real, dimension(SZI_(G)) :: dZ_N2             ! Summed vertical distance used in N2 calculation [Z ~> m]
  real, dimension(SZI_(G)) :: T_subML, T_deeper ! Temperatures used in the N2 calculation [C ~> degC].
  real, dimension(SZI_(G)) :: S_subML, S_deeper ! Salinities used in the N2 calculation [S ~> ppt].
  real, dimension(SZI_(G)) :: rho_subML, rho_deeper ! Densities used in the N2 calculation [R ~> kg m-3].
  real, dimension(SZI_(G),SZK_(GV)) :: dZ_2d   ! Layer thicknesses in depth units [Z ~> m]
  real, dimension(SZI_(G)) :: dZ, dZm1         ! Layer thicknesses associated with interfaces [Z ~> m]
  real, dimension(SZI_(G)) :: rhoSurf          ! Density used in finding the mixed layer depth [R ~> kg m-3].
  real, dimension(SZI_(G), SZJ_(G)) :: z_ref_diag ! The actual depth of the reference density [Z ~> m].
  real, dimension(SZI_(G), SZJ_(G)) :: MLD     ! Diagnosed mixed layer depth [Z ~> m].
  real, dimension(SZI_(G), SZJ_(G)) :: subMLN2 ! Diagnosed stratification below ML [T-2 ~> s-2].
  real, dimension(SZI_(G), SZJ_(G)) :: MLD2    ! Diagnosed MLD^2 [Z2 ~> m2].
  logical, dimension(SZI_(G)) :: N2_region_set ! If true, all necessary values for calculating N2
                                               ! have been stored already.
  real :: gE_Rho0          ! The gravitational acceleration, sometimes divided by the Boussinesq
                           ! reference density [H T-2 R-1 ~> m4 s-2 kg-1 or m s-2].
  real :: dZ_sub_ML        ! Depth below ML over which to diagnose stratification [Z ~> m]
  real :: aFac             ! A nondimensional factor [nondim]
  real :: ddRho            ! A density difference [R ~> kg m-3]
  real :: dddpth           ! A depth difference [Z ~> m]
  real :: rhoSurf_k, rhoSurf_km1  ! Desisty in the layers below and above the target reference depth [R ~> kg m-3].
  real, dimension(SZI_(G), SZJ_(G)) :: rhoSurf_2d ! The density that is considered the "surface" when calculating
                                                  ! the MLD. It can be saved as a diagnostic [R ~> kg m-3].
  !integer, dimension(2) :: EOSdom ! The i-computational domain for the equation of state
  integer :: i, j, is, ie, js, je, k, nz, id_N2, id_SQ

  id_SQ = -1 ; if (PRESENT(id_MLDsq)) id_SQ = id_MLDsq

  id_N2 = -1
  if (present(id_N2subML)) then
    if (present(dz_subML)) then
      id_N2 = id_N2subML
      dZ_sub_ML = dz_subML
    else
      call MOM_error(FATAL, "When the diagnostic of the subML stratification is "//&
                "requested by providing id_N2_subML to diagnoseMLDbyDensityDifference, "//&
                "the distance over which to calculate that distance must also be provided.")
    endif
  endif

  !gE_rho0 = (US%L_to_Z**2 * GV%g_Earth) / GV%H_to_RZ
  gE_rho0 = US%L_to_Z**2*GV%g_Earth / GV%Rho0

  is = G%isc ; ie = G%iec ; js = G%jsc ; je = G%jec ; nz = GV%ke

  hRef_MLD(:) = ref_h_mld
  !pRef_MLD(:) = GV%H_to_RZ*GV%g_Earth*ref_h_mld
  pRef_MLD(:) = GV%Rho0*GV%g_Earth*ref_h_mld
  z_ref_diag(:,:) = 0.

  !EOSdom(:) = EOS_domain(G%HI)
  do j=js,je
    ! Find the vertical distances across layers.
    call thickness_to_dz(h, tv, dZ_2d, j, G, GV)

    if (pRef_MLD(is) /= 0.0) then
      rhoSurf(:) = 0.0
      do i=is,ie 
        dZ(i) = 0.5 * dZ_2d(i,1) ! Depth of center of surface layer
        if (dZ(i) >= hRef_MLD(i)) then
          call calculate_density(tv%T(i,j,1), tv%S(i,j,1), pRef_MLD(i), rhoSurf_k, tv%eqn_of_state)
          rhoSurf(i) = rhoSurf_k
        endif
      enddo
      do k=2,nz
        do i=is,ie
          dZm1(i) = dZ(i) ! Depth of center of layer K-1
          dZ(i) = dZ(i) + 0.5 * ( dZ_2d(i,k) + dZ_2d(i,k-1) ) ! Depth of center of layer K
          dddpth = dZ(i) - dZm1(i)
          if ((rhoSurf(i) == 0.) .and. &
              (dZm1(i) < hRef_MLD(i)) .and. (dZ(i) >= hRef_MLD(i))) then
            aFac = ( hRef_MLD(i) - dZm1(i) ) / dddpth
            z_ref_diag(i,j) = (dZ(i) * aFac + dZm1(i) * (1. - aFac))
            call calculate_density(tv%T(i,j,k)  , tv%S(i,j,k)  , pRef_MLD(i), rhoSurf_k, tv%eqn_of_state)
            call calculate_density(tv%T(i,j,k-1), tv%S(i,j,k-1), pRef_MLD(i), rhoSurf_km1,tv%eqn_of_state)
            rhoSurf(i) = (rhoSurf_k * aFac + rhoSurf_km1 * (1. - aFac))
            H_subML(i) = h(i,j,k)
          elseif ((rhoSurf(i) == 0.) .and. (k >= nz)) then
            call calculate_density(tv%T(i,j,1), tv%S(i,j,1), pRef_MLD(i), rhoSurf_k, tv%eqn_of_state)
            rhoSurf(i) = rhoSurf_k
          endif
        enddo
      enddo
      do i=is,ie
        dZ(i) = 0.5 * dZ_2d(i,1) ! reset dZ to surface depth
        rhoSurf_2d(i,j) = rhoSurf(i)
        deltaRhoAtK(i) = 0.
        MLD(i,j) = 0.
        if (id_N2>0) then
          subMLN2(i,j) = 0.0
          dH_N2(i) = 0.0 ; dZ_N2(i) = 0.0
          T_subML(i) = 0.0  ; S_subML(i) = 0.0 ; T_deeper(i) = 0.0 ; S_deeper(i) = 0.0
          N2_region_set(i) = (G%mask2dT(i,j)<0.5) ! Only need to work on ocean points.
        endif
      enddo
    elseif (pRef_MLD(is) == 0.0) then
      rhoSurf(:) = 0.0
      do i=is,ie ; dZ(i) = 0.5 * dZ_2d(i,1) ; enddo ! Depth of center of surface layer
      call calculate_density(tv%T(:,j,1), tv%S(:,j,1), pRef_MLD, rhoSurf, is, ie-is+1, tv%eqn_of_state) !, EOSdom)
      do i=is,ie
        rhoSurf_2d(i,j) = rhoSurf(i)
        deltaRhoAtK(i) = 0.
        MLD(i,j) = 0.
        if (id_N2>0) then
          subMLN2(i,j) = 0.0
          H_subML(i) = h(i,j,1) ; dH_N2(i) = 0.0 ; dZ_N2(i) = 0.0
          T_subML(i) = 0.0  ; S_subML(i) = 0.0 ; T_deeper(i) = 0.0 ; S_deeper(i) = 0.0
          N2_region_set(i) = (G%mask2dT(i,j)<0.5) ! Only need to work on ocean points.
        endif
      enddo
    endif

    do k=2,nz
      do i=is,ie
        dZm1(i) = dZ(i) ! Depth of center of layer K-1
        dZ(i) = dZ(i) + 0.5 * ( dZ_2d(i,k) + dZ_2d(i,k-1) ) ! Depth of center of layer K
      enddo

      ! Prepare to calculate stratification, N2, immediately below the mixed layer by finding
      ! the cells that extend over at least dz_subML.
      if (id_N2>0) then
         do i=is,ie
          if (MLD(i,j) == 0.0) then  ! Still in the mixed layer.
            H_subML(i) = H_subML(i) + h(i,j,k)
          elseif (.not.N2_region_set(i)) then ! This block is below the mixed layer, but N2 has not been found yet.
            if (dZ_N2(i) == 0.0) then ! Record the temperature, salinity, pressure, immediately below the ML
              T_subML(i) = tv%T(i,j,k) ; S_subML(i) = tv%S(i,j,k)
              H_subML(i) = H_subML(i) + 0.5 * h(i,j,k) ! Start midway through this layer.
              dH_N2(i) = 0.5 * h(i,j,k)
              dZ_N2(i) = 0.5 * dz_2d(i,k)
            elseif (dZ_N2(i) + dZ_2d(i,k) < dZ_sub_ML) then
              dH_N2(i) = dH_N2(i) + h(i,j,k)
              dZ_N2(i) = dZ_N2(i) + dz_2d(i,k)
            else  ! This layer includes the base of the region where N2 is calculated.
              T_deeper(i) = tv%T(i,j,k) ; S_deeper(i) = tv%S(i,j,k)
              dH_N2(i) = dH_N2(i) + 0.5 * h(i,j,k)
              dZ_N2(i) = dZ_N2(i) + 0.5 * dz_2d(i,k)
              N2_region_set(i) = .true.
            endif
          endif
        enddo ! i-loop
      endif ! id_N2>0

      ! Mixed-layer depth, using sigma-0 (surface reference pressure)
      do i=is,ie ; deltaRhoAtKm1(i) = deltaRhoAtK(i) ; enddo ! Store value from previous iteration of K
      call calculate_density(tv%T(:,j,k), tv%S(:,j,k), pRef_MLD, deltaRhoAtK, is, ie-is+1, tv%eqn_of_state) !, EOSdom)
      do i = is, ie
        deltaRhoAtK(i) = deltaRhoAtK(i) - rhoSurf(i) ! Density difference between layer K and surface
        ddRho = deltaRhoAtK(i) - deltaRhoAtKm1(i)
        if ((MLD(i,j) == 0.) .and. (ddRho > 0.) .and. &
            (deltaRhoAtKm1(i) < densityDiff) .and. (deltaRhoAtK(i) >= densityDiff)) then
          aFac = ( densityDiff - deltaRhoAtKm1(i) ) / ddRho
          MLD(i,j) = (dZ(i) * aFac + dZm1(i) * (1. - aFac))
        endif
        if (id_SQ > 0) MLD2(i,j) = MLD(i,j)**2
      enddo ! i-loop
    enddo ! k-loop
    do i=is,ie
      if ((MLD(i,j) == 0.) .and. (deltaRhoAtK(i) < densityDiff)) MLD(i,j) = dZ(i) ! Mixing goes to the bottom
    enddo

    if (id_N2>0) then  ! Now actually calculate stratification, N2, below the mixed layer.
      !do i=is,ie ; pRef_N2(i) = (GV%g_Earth * GV%H_to_RZ) * (H_subML(i) + 0.5*dH_N2(i)) ; enddo
      do i=is,ie ; pRef_N2(i) = (GV%g_Earth * GV%Rho0) * (H_subML(i) + 0.5*dH_N2(i)) ; enddo
      ! if ((.not.N2_region_set(i)) .and. (dZ_N2(i) > 0.5*dZ_sub_ML)) then
      !    ! Use whatever stratification we can, measured over whatever distance is available?
      !    T_deeper(i) = tv%T(i,j,nz) ; S_deeper(i) = tv%S(i,j,nz)
      !    N2_region_set(i) = .true.
      ! endif
      call calculate_density(T_subML, S_subML, pRef_N2, rho_subML, is, ie-is+1, tv%eqn_of_state) !, EOSdom)
      call calculate_density(T_deeper, S_deeper, pRef_N2, rho_deeper, is, ie-is+1, tv%eqn_of_state) !, EOSdom)
      do i=is,ie ; if ((G%mask2dT(i,j) > 0.0) .and. N2_region_set(i)) then
        subMLN2(i,j) =  gE_rho0 * (rho_deeper(i) - rho_subML(i)) / dH_N2(i)
      endif ; enddo
    endif
  enddo ! j-loop

  if (id_MLD > 0) call post_data(id_MLD, MLD, diagPtr)
  if (id_N2 > 0)  call post_data(id_N2, subMLN2, diagPtr)
  if (id_SQ > 0)  call post_data(id_SQ, MLD2, diagPtr)

  if ((id_ref_z > 0) .and. (pRef_MLD(is)/=0.)) call post_data(id_ref_z, z_ref_diag , diagPtr)
  if (id_ref_rho > 0) call post_data(id_ref_rho, rhoSurf_2d , diagPtr)

  if (present(MLD_out)) MLD_out(:,:) = MLD(:,:)

end subroutine diagnoseMLDbyDensityDifference

!> \namespace mom_diagnose_mld
!!
!!    This module contains subroutines that apply various diabatic processes.  Usually these
!!  subroutines are called from the MOM_diabatic module.  All of these routines use appropriate
!!  limiters or logic to work properly with arbitrary layer thicknesses (including massless layers)
!!  and an arbitrarily large timestep.
!!
!!    The subroutine diagnoseMLDbyDensityDifference diagnoses a mixed layer depth based on a
!!  density difference criterion, and may also estimate the stratification of the water below
!!  this diagnosed mixed layer.
!!
!!    The subroutine diagnoseMLDbyEnergy diagnoses a mixed layer depth based on a mixing-energy
!!  criterion, as described by Reichl et al., 2022, JGR: Oceans, doi:10.1029/2021JC018140.
!!

end module MOM_diagnose_mld
