
module generic_coupler_fluxes
  use time_manager_mod,       only : time_type
  use diag_manager_mod,       only : diag_axis_init, register_diag_field, send_data
  use atmos_ocean_fluxes_mod, only : aof_set_coupler_flux
  use coupler_types_mod,      only : coupler_2d_bc_type,coupler_1d_bc_type
  use coupler_types_mod,      only : ind_flux, ind_deltap, ind_kw
  use coupler_types_mod,      only : ind_alpha,ind_csurf,ind_sc_no
  use coupler_types_mod,      only : coupler_type_spawn,coupler_type_set_diags
  use data_override_mod,      only : data_override
  use constants_mod,          only : WTMN, WTMAIR,rdgas

  implicit none ; private

  integer :: id_nh3_sc_no=-1, id_nh3_csurf=-1,id_nh3_alpha=-1, id_nh4_csurf = -1
  integer :: id_phos=-1, id_sos=-1, id_pka_nh3 = -1

  integer :: ind_nh3_flux = 0 

  public generic_coupler_fluxes_init
  public generic_coupler_fluxes_set_nh3_alpha
  
contains

  subroutine generic_coupler_fluxes_init(ocean_time, axes, axt, is,ie,js,je, ocean_iob_fields, gas_fields_ocn)
    type(time_type),          intent(in) :: ocean_time
    integer, dimension(1),    intent(in) :: axes
    integer, dimension(2),    intent(in) :: axt
    integer,                  intent(in) :: is, ie, js, je
    type(coupler_2d_bc_type), intent(inout) :: ocean_iob_fields
    type(coupler_1d_bc_type),  optional, intent(in) :: gas_fields_ocn !< If present, this type describes the

    !set ocean/atm fluxes for nh3
    ind_nh3_flux = aof_set_coupler_flux('nh3_flux',      &
         flux_type         = 'air_sea_gas_flux_generic', &  
         implementation    = 'johnson',                  &
         mol_wt            = WTMN,                       &
         param             = (/ 17., 25. /),             &
         ice_restart_file  = 'ice_airsea_flux.res.nc',   &
         ocean_restart_file= 'ocean_airsea_flux_res.nc'  &
         )

    if (present(gas_fields_ocn)) then
       call coupler_type_spawn(gas_fields_ocn, ocean_iob_fields, (/is,is,ie,ie/), &
            (/js,js,je,je/), suffix = '_ocn', as_needed=.true.)
       call coupler_type_set_diags(ocean_iob_fields, "ocean_sfc", axes(1:2), ocean_time)
    end if

    !Register diagnostics
    id_nh3_sc_no = register_diag_field('ocean_model', 'nh3_sc_no',axt, ocean_time, 'NH3 Schmidt Number',  &
                                       'unitless', missing_value=1.e10)
    id_nh3_csurf = register_diag_field('ocean_model', 'nh3_csurf',axt, ocean_time, 'NH3 surface concentration',  &
                                       'mol/m^3', missing_value=1.e10)
    id_nh4_csurf = register_diag_field('ocean_model', 'nh4_csurf',axt, ocean_time, 'NH4 surface concentration',  &
                                       'mol/m^3', missing_value=1.e10)
    id_phos = register_diag_field('ocean_model', 'phos',axt, ocean_time, 'ocean surface pH', &
                                  'unitless', missing_value=1.e10)
    id_sos = register_diag_field('ocean_model', 'sos',axt, ocean_time, 'ocean surface salt', &
                                 'psu', missing_value=1.e10)
    id_pka_nh3 = register_diag_field('ocean_model', 'pka_nh3',axt, ocean_time, 'pka of NH3',&
                                     'unitless', missing_value=1.e10)
    id_nh3_alpha = register_diag_field('ocean_model', 'nh3_alpha',axt, ocean_time, 'NH3 solubilty',  &
                                       'mol/m3/atm', missing_value=1.e10)

  end subroutine generic_coupler_fluxes_init


  subroutine generic_coupler_fluxes_set_nh3_alpha(ocean_iob_fields, is, ie, js, je, ocean_sst, ocean_mask, ocean_time)
    type(coupler_2d_bc_type), intent(inout) :: ocean_iob_fields
    integer,                  intent(in)    :: is, ie, js, je
    real, dimension(is:,js:), intent(in)    :: ocean_sst
    logical, dimension(is:,js:), intent(in) ::  ocean_mask
    type(time_type),          intent(in)    :: ocean_time

    logical :: sent
    real            :: ltr,tr
    integer         :: i,j
    real, parameter :: vb_nh3 = 25
    real            :: sstc
    real, dimension(size(ocean_mask,1),size(ocean_mask,2)) :: nh4_surf,ph_surf,nh3_alpha,nh3_csurf,nh3_sc_no,pka_nh3,salt_surf

    nh4_surf(:,:) = 0; ph_surf(:,:)=8.0;salt_surf(:,:)=33.0         
    call data_override('OCN', 'nh4_surf', nh4_surf, ocean_time)
    call data_override('OCN', 'ph_surf',  ph_surf, ocean_time)
    call data_override('OCN', 'salt_surf',  salt_surf, ocean_time)  

    do i = 1,size(ocean_sst,1); do j=1,size(ocean_sst,2)
       !Note that COBALT uses sst in C. For consistency. To avoid mistakes, I am converting sst to C
       sstc = ocean_sst(i,j)-273.15
       pka_nh3(i,j)   = calc_pka_nh3(sstc,salt_surf(i,j))
       tr             = 298.15/(sstc+273.15)-1.
       ltr            = -tr+log(298.15/(sstc+273.15))       

       !calculate solubility/schmidt number for NH3
       nh3_alpha(i,j) = 5.76e1*exp(13.79*tr-5.39*ltr)*997.
       nh3_alpha(i,j) = nh3_alpha(i,j)/saltout_correction(101325./(1.e-3*rdgas*wtmair*(sstc+273.15)*nh3_alpha(i,j)),vb_nh3,salt_surf(i,j)) !mol/m3/atm  

       nh3_csurf(i,j) = nh4_surf(i,j)/(1.+10**(pka_nh3(i,j)-max(min(ph_surf(i,j),11.),3.))) !in mol/m3
       nh3_sc_no(i,j) = schmidt_w(sstc,salt_surf(i,j),vb_nh3)
    enddo;enddo

    ocean_iob_fields%bc(ind_nh3_flux)%field(ind_sc_no)%values(is:ie,js:je) = nh3_sc_no
    ocean_iob_fields%bc(ind_nh3_flux)%field(ind_csurf)%values(is:ie,js:je) = nh3_csurf
    ocean_iob_fields%bc(ind_nh3_flux)%field(ind_alpha)%values(is:ie,js:je) = nh3_alpha

    if (id_nh3_sc_no>0) sent = send_data(id_nh3_sc_no, nh3_sc_no, ocean_time, mask=ocean_mask)
    if (id_nh3_csurf>0) sent = send_data(id_nh3_csurf, nh3_csurf, ocean_time, mask=ocean_mask)
    if (id_nh4_csurf>0) sent = send_data(id_nh4_csurf, nh4_surf, ocean_time, mask=ocean_mask)            
    if (id_sos>0) sent = send_data(id_sos, salt_surf, ocean_time, mask=ocean_mask)
    if (id_phos>0) sent = send_data(id_phos, ph_surf, ocean_time, mask=ocean_mask)                        
    if (id_nh3_alpha>0) sent = send_data(id_nh3_alpha, nh3_alpha, ocean_time, mask=ocean_mask)
    if (id_pka_nh3>0)   sent = send_data(id_pka_nh3,   pka_nh3, ocean_time, mask=ocean_mask)                        
  end subroutine generic_coupler_fluxes_set_nh3_alpha

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

end module generic_coupler_fluxes
