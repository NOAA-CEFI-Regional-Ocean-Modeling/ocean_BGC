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
! Surface fCO2 calculation
!</OVERVIEW>
!
!<DESCRIPTION>
! Calculate the fugacity of CO2 at the surface in thermodynamic
! equilibrium with the current alkalinity (Alk) and total dissolved
! inorganic carbon (DIC) at a particular temperature and salinity
! using an initial guess for the total hydrogen
! ion concentration (htotal)
!</DESCRIPTION>
!

!
!------------------------------------------------------------------
!
!       Global definitions
!
!------------------------------------------------------------------
!

use fm_util_mod, only: fm_util_start_namelist, fm_util_end_namelist
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
! temperature (t) and salinity (s).
! It is assumed that init_ocmip2_co2calc has already been called with
! the T and S to calculate the various coefficients.
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
!       htotallo   = lower limit of htotal range
!
!       htotalhi   = upper limit of htotal range
!
!       htotal     = H+ concentration (mol/kg)
!
!  INPUT (optional)
!
!       zt         = dzt
!
! OUTPUT
!       co2star    = CO2*water, or H2CO3 concentration (mol/kg)
!       alpha      = Solubility of CO2 for air (mol/kg/atm)
!       pco2surf   = oceanic pCO2 (ppmv)
!       co3_ion    = Carbonate ion, or CO3-- concentration (mol/kg)
!       omega_arag = aragonite saturation state (mol/kg ; avail. only w/ mocsy)
!       omega_calc = aragonite saturation state (mol/kg ; avail. only w/ mocsy)
!
! FILES and PROGRAMS NEEDED: drtsafe, ta_iter_1
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

!
!       local parameters
!

real, parameter :: permeg = 1.e-6
real, parameter :: xacc = 1.0e-10

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
                               ta_in, &
                               htotallo, &
                               htotalhi
real, dimension(dope_vec%isd:dope_vec%ied,dope_vec%jsd:dope_vec%jed), &
      intent(inout)         :: htotal
real, dimension(dope_vec%isd:dope_vec%ied,dope_vec%jsd:dope_vec%jed), &
      intent(in), optional  :: zt
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
real :: alpha_internal
real :: bt
real :: co2star_internal
real :: dlogtk
real :: ft
real :: htotal2
real :: invtk
real :: is
real :: is2
real :: k1
real :: k2
real :: k1p
real :: k2p
real :: k3p
real :: kb
real :: kf
real :: ks
real :: ksi
real :: kw
real :: log100
real :: s2
real :: scl
real :: sqrtis
real :: sqrts
real :: st
real :: tk
real :: tk100
real :: tk1002
real :: logf_of_s
real :: salinity

!character(len=10) :: co2_calc_method

!  co2_calc_method = trim(co2_calc)
!  if (co2_calc == 'mocsy') then
    if (.not. present(zt)) then 
        call mpp_error(FATAL,"Depth must be specified when invoking Mocsy.") 
    end if
!  end if   
!else
!  call mpp_error(FATAL,"co2_calc must be mocsy.")
!end if




! Set the loop indices.
  isc = dope_vec%isc ; iec = dope_vec%iec
  jsc = dope_vec%jsc ; jec = dope_vec%jec

!
!       Initialize the module
!
  log100 = log(100.0)

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

!#######################################################################
! <FUNCTION NAME="drtsafe">
!
! <DESCRIPTION>
!       File taken from Numerical Recipes. Modified  R. M. Key 4/94
! </DESCRIPTION>

function drtsafe(k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf, &
               bt, dic, ft, pt, sit, st, ta, x1_in, x2_in, x, xacc)  !{

implicit none

!
!       arguments
!

real    :: k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf
real    :: bt, dic, ft, pt, sit, st, ta
real    :: drtsafe
real    :: x1_in, x2_in, x, xacc

!
!       local parameters
!

integer, parameter      :: maxit = 100

!
!       local variables
!

integer :: j
real    :: fl, df, fh, swap, xl, xh, dxold, dx, f, temp
real    :: x1
real    :: x2
logical :: bracketed

drtsafe=x
call ta_iter_1(k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf, &
               bt, dic, ft, pt, sit, st, ta, drtsafe, f, df)
dx=f/df
if (abs(dx) .lt. xacc) then
!     write (6,*) 'Exiting drtsafe at C on iteration  ', j, ', ph = ', -log10(drtsafe)
  return
endif

bracketed = .false.
x1 = x
x2 = x

do j = 1, 10

  x1 = x1 * 0.1
  x2 = x2 * 10.0
  call ta_iter_1(k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf, &
                 bt, dic, ft, pt, sit, st, ta, x1, fl, temp)
  call ta_iter_1(k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf, &
                 bt, dic, ft, pt, sit, st, ta, x2, fh, temp)
  if (fl*fh .lt. 0.0) then
    bracketed = .true.
    exit
  endif

enddo

if (.not. bracketed) then
  call mpp_error(WARNING, 'drtsafe: root not bracketed')
endif

if(fl .lt. 0.0) then
  xl=x1
  xh=x2
else
  xh=x1
  xl=x2
  swap=fl
  fl=fh
  fh=swap
end if
drtsafe=0.5*(x1+x2)
dxold=abs(x2-x1)
dx=dxold
call ta_iter_1(k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf, &
               bt, dic, ft, pt, sit, st, ta, drtsafe, f, df)
do j=1,maxit  !{
  if (((drtsafe-xh)*df-f)*((drtsafe-xl)*df-f) .ge. 0.0 .or.     &
      abs(2.0*f) .gt. abs(dxold*df)) then
    dxold=dx
    dx=0.5*(xh-xl)
    drtsafe=xl+dx
    if (xl .eq. drtsafe) then
!     write (6,*) 'Exiting drtsafe at A on iteration  ', j, ', ph = ', -log10(drtsafe)
      return
    endif
  else
    dxold=dx
    dx=f/df
    temp=drtsafe
    drtsafe=drtsafe-dx
    if (temp .eq. drtsafe) then
!     write (6,*) 'Exiting drtsafe at B on iteration  ', j, ', ph = ', -log10(drtsafe)
      return
    endif
  end if
  if (abs(dx) .lt. xacc) then
!     write (6,*) 'Exiting drtsafe at C on iteration  ', j, ', ph = ', -log10(drtsafe)
    return
  endif
  call ta_iter_1(k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf, &
                 bt, dic, ft, pt, sit, st, ta, drtsafe, f, df)
  if(f .lt. 0.0) then
    xl=drtsafe
    fl=f
  else
    xh=drtsafe
    fh=f
  end if
enddo  !} j

! should have an error condition here for not converging?

return

end  function  drtsafe  !}
! </FUNCTION> NAME="drtsafe"

!#######################################################################
! <SUBROUTINE NAME="ta_iter_1">
!
! <DESCRIPTION>
! This routine expresses TA as a function of DIC, htotal and constants.
! It also calculates the derivative of this function with respect to 
! htotal. It is used in the iterative solution for htotal. In the call
! "x" is the input value for htotal, "fn" is the calculated value for TA
! and "df" is the value for dTA/dhtotal
! </DESCRIPTION>

subroutine ta_iter_1(k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf, &
                     bt, dic, ft, pt, sit, st, ta, x, fn, df)  !{

implicit none

!
!       arguments
!

real    :: k1, k2, kb, k1p, k2p, k3p, ksi, kw, ks, kf
real    :: bt, dic, ft, pt, sit, st, ta, x, fn, df

!
!       local variables
!

real    :: x2, x3, k12, k12p, k123p, c, a, am1, am2, da, b, bm1, bm2, db
real    :: xpkbm1,xpkfm1,xpksim1,xpkscm1

x2 = x*x
x3 = x2*x
k12 = k1*k2
k12p = k1p*k2p
k123p = k12p*k3p
c = 1.0/(1.0 + st/ks)

a = x3 + k1p*x2 + k12p*x + k123p
am1 = 1.0/a
am2 = am1*am1
da = 3.0*x2 + 2.0*k1p*x + k12p

b = x2 + k1*x + k12
bm1 = 1.0/b
bm2 = bm1*bm1
db = 2.0*x + k1

xpkbm1  = 1.0/(x+kb)
xpkfm1  = 1.0/(x+kf)
xpksim1 = 1.0/(x+ksi)
xpkscm1 = 1.0/(x+ks*c)

!
!     fn = hco3+co3+borate+oh+hpo4+2*po4+silicate+hfree+hso4+hf+h3po4-ta
!OR
!fn = -ta  + kw/x - c x 
!   + dic (k1 x + 2 k12)/(k12 + k1 x + x^2)
!   + pt  (2 k123p + k12p x - x^3)/(k123p + k12p x + k1p x^2 + x^3)
!   + bt/(1. + x/kb) + sit/(1. + x/ksi) 
!   - st/(1. + (c ks)/x) - ft/(1. + kf/x)

fn = - ta + kw/x - c*x               &
     + dic*(k1*x+2.0*k12)*bm1        &
     + pt*(-x3+k12p*x+2.0*k123p)*am1 &
     + bt *kb *xpkbm1                &
     + sit*ksi*xpksim1               &
     - st *x  *xpkscm1               &
     - ft *x  *xpkfm1              

!
!     df = dfn/dx
!
df = - kw/x2 - c                                               &
     + dic*(bm1*k1 - bm2*db*(k1*x+2.0*k12))                    &
     + pt*(am1*(-3.*x2+k12p) - am2*da*(-x3+k12p*x+2.0*k123p))  &
     - bt*  kb  *xpkbm1 *xpkbm1                                &
     - sit* ksi *xpksim1*xpksim1                               &
     - st*  ks*c*xpkscm1*xpkscm1                               &
     - ft*  kf  *xpkfm1 *xpkfm1                                
     

return

end subroutine  ta_iter_1  !}
! </SUBROUTINE> NAME="ta_iter_1"

end module  FMS_co2calc_mod  !}
