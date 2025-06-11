module ocean_atm_chemical_fluxes_utils

  !This module provides a set of functions to calculate chemical properties required for atm/ocean gas exchange (e.g., solubility)
  !These properties can be accessed via get_XXX_ocean_atm_flux_property, where XXX can be NH3 or DMS
  !author: Fabien.Paulot@noaa.gov  
  
  use constants_mod,     only : WTMN, WTMAIR,rdgas

  implicit none ; private

  real, parameter, public   :: vb_nh3 = 25., vb_dms = 77.
  
  public get_nh3_ocean_atm_flux_property, get_dms_ocean_atm_flux_property
  public calc_pka_nh3, schmidt_w_nh3, schmidt_w_dms
  
contains

  subroutine get_dms_ocean_atm_flux_property(tc,salt,f_dms,pH,rho_0, &
       dms_alpha,dms_csurf,pdms_csurf)

    real, intent(in)  :: tc,salt,f_dms,pH,rho_0
    real, intent(out) :: dms_alpha, dms_csurf
    real, intent(out), optional :: pdms_csurf

    dms_alpha = 0.537023e3*exp(3500.*(1./(tc+273.15)-1./298.15)) !M/atm
    dms_alpha = dms_alpha/saltout_correction(101325./(1.e-3*rdgas*wtmair*tc*dms_alpha),vb_dms,salt)*1./rho_0 !mol/m3/atm
!    dms_sco_no = schmidt_dms(sstc)
    dms_csurf  = f_dms    
    
  end subroutine get_dms_ocean_atm_flux_property

  subroutine get_nh3_ocean_atm_flux_property(tc,salt,f_nh4,pH,rho_0,nh3_alpha,nh3_csurf,pnh3_csurf,pKa_nh3)

    real, intent(in)  :: tc,salt,f_nh4,pH,rho_0
    real, intent(out) :: nh3_alpha, nh3_csurf
    real, intent(out), optional :: pnh3_csurf, pKa_nh3

    real :: tr, ltr, pKa_nh3_local

    !henry's constant is from Mark Jacobson's book "Fundamental of Atmospheric Modeling".
    !I used this expression to be consistent with the cloud chemistry module
    !the units are in mol/kg/atm. However it's probably in mol/kg(pure water)/atm
    !the density of pure water is ~997 kg/m3 (25C). alpha will then be scaled by the density of seawater, which for some reason I don't quite understand is always set to 1035.
    !to be consistent, I am scaling the Jacobson's number by 997/1035. This decreases the solubility of NH3 by less than 4%. For references, Sander's estimate is alpha(298.15)=0.59*101.63=59.96 mol/kg(pure water)/atm, about 4% greater than Jacobson's.

    pKa_nh3_local  = calc_pKa_nh3(tc,salt)
    tr             = 298.15/(tc+273.15)-1.
    ltr            = -tr+log(298.15/(tc+273.15))

    nh3_alpha      = 5.76e1*exp(13.79*tr-5.39*ltr)*997.

    nh3_alpha      = nh3_alpha/saltout_correction(101325./(1.e-3*rdgas*wtmair*(tc+273.15)*nh3_alpha),vb_nh3,salt)* 1./rho_0 !mol/kg/atm

    nh3_csurf      = f_nh4/(1.+10**(pKa_nh3_local-pH))

    if (present(pnh3_csurf)) pnh3_csurf = nh3_csurf/nh3_alpha*1.e6 !in uatm
    if (present(pKa_nh3))    pKa_nh3    = pKa_nh3_local

  end subroutine get_nh3_ocean_atm_flux_property

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

      tk      = tc+273.15
      pka     = 9.244605-2729.33*(1/298.15-1./tk)  &
              + (a1+a2/tk+a3*tk**2.)*salt**0.5     &
              + (a4+a5*tk+a6/tk)*salt              &
              + (a7+a8/tk)*salt**2.                &
              + (a9+a10/tk)*salt**3.

  end function calc_pka_nh3

  !salting out correction for solubility (Johnson 2010, Ocean Science)
  function saltout_correction(kh,vb,salt) result(C)
      real, intent(in) :: Kh,vb,salt
      real*8 :: T,log_kh,theta2
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

    function schmidt_w_dms(sstc) result(S)
      !Wanninkhof (2014)
      real             :: S
      real, intent(in) :: sstc
      S = 2855.7 + (-177.63 + (6.0438 + (-0.11645 + 0.00094743 * sstc ) * sstc ) * sstc ) * sstc
    end function schmidt_w_dms

    function schmidt_w_nh3(tc,salt) result(S)
      real, intent(in) :: tc,salt
      real :: S
      S = schmidt_w(tc,salt,vb_nh3)      
    end function schmidt_w_nh3  

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

  end module ocean_atm_chemical_fluxes_utils
