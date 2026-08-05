module FMS_co2calc_mod  !{
!
!This module is renamed from FMS_ocmip2_co2calc.F90
!It is called FMS_co2calc_mod because now we only support mocsy 
!
!Modifications:
!1. mask check should surround all the calculations since MOM restart masks htotal on land 
!   and htotal cannot be zero in this calculation (division by htotal) 
!2. if (mask(i,j) .eq. 0.0) --> if (mask(i,j) .gt. 0.0) for floating point comparison efficiency
! 
!<CONTACT EMAIL="Richard.Slater@noaa.gov"> Richard D. Slater
!</CONTACT>
!
!<REVIEWER EMAIL="John.Dunne@noaa.gov"> John P. Dunne
!</REVIEWER>
!
!<REVIEWER EMAIL="Niki.Zadeh@noaa.gov"> Niki T. Zadeh
!</REVIEWER>
!
!<OVERVIEW>
! Carbonate chemistry calculation (pCO2, CO2*, CO3--, and saturation states)
!</OVERVIEW>
!
!<DESCRIPTION>
! Calculate the partial pressure of CO2 (pCO2) in thermodynamic
! equilibrium with the current alkalinity (Alk) and total dissolved
! inorganic carbon (DIC) at a particular temperature and salinity,
! via mocsy's vars() routine. mocsy also returns an initial-guess-free
! solve for the total hydrogen ion concentration (htotal); the
! htotallo/htotalhi bracket arguments below are optional and retained
! only for interface compatibility with existing callers, which
! currently always pass them but whose values are not used to seed an
! iterative solver.
!</DESCRIPTION>
!

!
!------------------------------------------------------------------
!
!       Global definitions
!
!------------------------------------------------------------------
!

use fms_mod,     only: check_nml_error
use mpp_mod,     only: input_nml_file, mpp_error, stdout, stdlog, WARNING, FATAL
use mocsy_vars,  only: vars

implicit none

private

public  :: read_mocsy_namelist
public  :: FMS_co2calc, CO2_dope_vector

character(len=128) :: version = '$Id$'
character(len=128) :: tagname = '$Name$'

! Mocsy namelist options
character(len=3) :: boron_formulation = 'l10'
character(len=3) :: dissociation_constants = 'm10'
character(len=2) :: hf_equilibrium_constant = 'dg'
real :: epsln = 1.e-10
real :: minimum_temperature = -2.
real :: maximum_salinity = 200.
real :: max_species_value = 4.  ! max_species_value based on pure salt (NaCl) at 200 psu
                                !       200 (g/kg) / 58.4428 (g/mol) = 3.4 mol/kg 
logical :: sal_floor_based_on_alk = .true.
logical :: print_oor_warnings = .false.
logical :: apply_epsln_floor = .true.
logical :: apply_species_ceiling = .true.
logical :: apply_salinity_ceiling = .true.
logical :: apply_temperature_floor = .true.

namelist /mocsy_nml/ boron_formulation, dissociation_constants, &
                     hf_equilibrium_constant, epsln,   &
                     print_oor_warnings, &
                     maximum_salinity, max_species_value, &
                     minimum_temperature, sal_floor_based_on_alk, &
                     apply_epsln_floor, apply_species_ceiling, &
                     apply_salinity_ceiling, apply_temperature_floor

type CO2_dope_vector
  integer  :: isc, iec, jsc, jec
  integer  :: isd, ied, jsd, jed
end type CO2_dope_vector
!
!-----------------------------------------------------------------------
!
!       Subroutine and function definitions
!
!-----------------------------------------------------------------------
!

contains

subroutine read_mocsy_namelist()

    integer :: ioun, ierr, io_status, stdoutunit, stdlogunit
    stdoutunit=stdout();stdlogunit=stdlog()

    read (input_nml_file, nml=mocsy_nml, iostat=io_status)
    ierr = check_nml_error(io_status,'mocsy_nml')

    write (stdoutunit,'(/)')
    write (stdoutunit, mocsy_nml)
    write (stdlogunit, mocsy_nml)

end subroutine read_mocsy_namelist


!#######################################################################
! <SUBROUTINE NAME="FMS_co2calc">
!
! <DESCRIPTION>
!       Calculate co2* from total alkalinity and total CO2 at
! temperature (t) and salinity (s), via mocsy's vars() routine.
!
! INPUT
!
!       dope_vec   = an array of indices corresponding to the compute
!                    and data domain boundaries.
!
!       mask       = land mask array (0.0 = land)
!
!       dic_in     = total inorganic carbon (mol/kg)
!                    where 1 T = 1 metric ton = 1000 kg
!
!       ta_in      = total alkalinity (eq/kg)
!
!       pt_in      = inorganic phosphate (mol/kg)
!
!       sit_in     = inorganic silicate (mol/kg)
!
!       htotal     = H+ concentration (mol/kg)
!
!  INPUT (optional)
!
!       zt         = dzt; depth of the layer being solved (m). Mocsy is
!                    called with optgas='Pinsitu', so co2star/pCO2surf/
!                    co3_ion/omega_* below are the IN-SITU values at this
!                    depth, not fixed-surface-pressure values. They read
!                    as "surface" only because every current caller in
!                    this repo passes the top model layer's zt.
!
!       htotallo   = lower limit of htotal range (not used since mocsy's
!                    own pH solver doesn't need a bisection bracket;
!                    retained only for interface compatibility)
!
!       htotalhi   = upper limit of htotal range (see htotallo)
!
! OUTPUT
!       co2star    = CO2*water, or H2CO3 concentration (mol/kg)
!       alpha      = Solubility of CO2 for air (mol/kg/atm)
!       pco2surf   = oceanic pCO2, in-situ at depth zt (uatm)
!       co3_ion    = Carbonate ion, or CO3-- concentration (mol/kg)
!       omega_arag = aragonite saturation state (dimensionless; avail. only w/ mocsy)
!       omega_calc = calcite saturation state (dimensionless; avail. only w/ mocsy)
!
! FILES and PROGRAMS NEEDED: mocsy_vars::vars()
!
! IMPORTANT: co2star and alpha need to be multiplied by rho before being
! passed to the atmosphere.
!
! </DESCRIPTION>

subroutine FMS_co2calc(dope_vec, mask,                      &
                          t_in, s_in, dic_in, pt_in, sit_in, ta_in, htotallo, &
                          htotalhi, htotal, zt, co2star, alpha, pCO2surf, &
                          co3_ion, omega_arag, omega_calc)  !{

implicit none

! Mocsy parameters
real, dimension(1) :: ph, pco2, fco2, co2, hco3, co3,  &
                      OmegaA, OmegaC, BetaD, rhoSW, p, depth, tempis
real, dimension(1) :: temp, sal, alk, dic, sil, phos, Patm, lat
character(10)      :: optCON, optGas, optT, optP, optB, optKf, optK1K2

!
!       arguments
!
type(CO2_dope_vector), intent(in)           :: dope_vec
real, dimension(dope_vec%isd:dope_vec%ied,dope_vec%jsd:dope_vec%jed), &
      intent(in)::             mask, &
                               t_in, &
                               s_in, &
                               dic_in, &
                               pt_in, &
                               sit_in, &
                               ta_in
real, dimension(dope_vec%isd:dope_vec%ied,dope_vec%jsd:dope_vec%jed), &
      intent(inout)         :: htotal
real, dimension(dope_vec%isd:dope_vec%ied,dope_vec%jsd:dope_vec%jed), &
      intent(in), optional  :: zt, &
                               htotallo, &
                               htotalhi
real, dimension(dope_vec%isd:dope_vec%ied,dope_vec%jsd:dope_vec%jed), &
      intent(out), optional :: alpha, &
                               pCO2surf, &
                               co2star, &
                               co3_ion, &
                               omega_arag, &
                               omega_calc
!
!       local variables
!
integer :: isc, iec, jsc, jec
integer :: i,j
real :: salinity

    if (.not. present(zt)) then
        call mpp_error(FATAL,"Depth must be specified when invoking Mocsy.")
    end if

! Set the loop indices.
  isc = dope_vec%isc ; iec = dope_vec%iec
  jsc = dope_vec%jsc ; jec = dope_vec%jec

!
!       Initialize the module
!
  do j = jsc, jec  !{
    do i = isc, iec  !{
      if (mask(i,j) .gt. 0.0) then  !{

        ! Initialize Mocsy input arrays
        Patm  = 0. 
        depth = 0. 
        lat   = 0. 
        temp  = 0. 
        sal   = 0. 
        alk   = 0. 
        dic   = 0. 
        sil   = 0. 
        phos  = 0. 

        ! Initialize salinity array
        salinity = 0.0
        salinity = s_in(i,j)

        ! Floor for low salinity waters based on alkalinity
        ! Molecular weight of sodium bicarbonate = 84.
        if (sal_floor_based_on_alk) then
          salinity = max(s_in(i,j),ta_in(i,j)*84.)   ! yields concentration in g/kg (per mil)
        endif

        ! Assign Mocsy inputs
        Patm(1)  = 1.      ! atm
        depth(1) = zt(i,j) ! m
        lat(1)   = 30.     ! degrees
        temp(1)  = t_in(i,j)   ! degC
        sal(1)   = salinity    ! psu
        alk(1)   = ta_in(i,j)  ! mol/kg
        dic(1)   = dic_in(i,j) ! mol/kg
        sil(1)   = sit_in(i,j) ! mol/kg
        phos(1)  = pt_in(i,j)  ! mol/kg

        if (apply_temperature_floor) then
          temp(1)  = max(temp(1),minimum_temperature) ! degC
        endif

        if (apply_epsln_floor) then
          sal(1)   = max(sal(1),epsln)  ! psu
          alk(1)   = max(alk(1),epsln)  ! mol/kg
          dic(1)   = max(dic(1),epsln)  ! mol/kg
          sil(1)   = max(sil(1),epsln)  ! mol/kg
          phos(1)  = max(phos(1),epsln) ! mol/kg
        endif

        if (apply_species_ceiling) then
          alk(1)   = min(alk(1),max_species_value)  ! mol/kg
          dic(1)   = min(dic(1),max_species_value)  ! mol/kg
          sil(1)   = min(sil(1),max_species_value)  ! mol/kg
          phos(1)  = min(phos(1),max_species_value) ! mol/kg
        endif

        if (apply_salinity_ceiling) then
          sal(1)   = min(sal(1),maximum_salinity)  ! psu
        endif

        call vars(ph, pco2, fco2, co2, hco3, co3, OmegaA, OmegaC, BetaD, rhoSW, p, tempis, &
                 temp, sal, alk, dic, sil, phos, Patm, depth, lat, 1,                     &
                 optCON='mol/kg', optT='Tpot   ', optP='m ', optb=boron_formulation,      &
                 optK1K2=dissociation_constants, optkf=hf_equilibrium_constant,           &
                 optgas='Pinsitu',verbose=print_oor_warnings)

        htotal(i,j) = 10.**(-1.*ph(1))

        if (present(co2star))   co2star(i,j)   = co2(1)
        if (present(co3_ion))   co3_ion(i,j)   = co3(1)
        if (present(alpha))     alpha(i,j)     = (co2(1)/(pco2(1)*1.e-6))
        if (present(pCO2surf))  pCO2surf(i,j)  = pco2(1)
        if (present(omega_arag)) omega_arag(i,j) = OmegaA(1)
        if (present(omega_calc)) omega_calc(i,j) = OmegaC(1)

      else  !}{mask(i,j)=0.0

        if (present(co3_ion)) then
          co3_ion(i,j) = 0.0
        endif
        if (present(co2star)) then
          co2star(i,j) = 0.0
        endif
        if (present(alpha)) then  !{
          alpha(i,j) = 0.0
        endif  !}
        if (present(pco2surf)) then  !{
          pCO2surf(i,j) = 0.0
        endif  !}

      endif  !}mask

    enddo  !} i
  enddo  !} j


return

end subroutine  FMS_co2calc  !}
! </SUBROUTINE> NAME="FMS_co2calc"


end module  FMS_co2calc_mod  !}
