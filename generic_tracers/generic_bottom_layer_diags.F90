! The generic_bottom_layer_diags module simplifies the averaging of 3D data
! over a layer near the bottom topography. The main subroutine is generic_bld_average,
! which does the actual averaging of the 3D data into a 2D bottom field.
! The other subroutines allocate and update information about the vertical coordinates.
module generic_bottom_layer_diags
  use MOM_error_handler, only : MOM_error, FATAL
  implicit none
  private

  type, public :: generic_bld
    real, pointer :: rho_dzt(:, :, :) => null()  !< density * thickness
    real, allocatable :: rho_dzt_bot(:, :)       !< Accumulated mass in bottom layer
    integer, pointer :: kmt(:, :) => null()      !< Index of the bottom layer
    integer, allocatable :: k_bot(:, :)          !< Shallowest layer within bottom_thickness
    real :: Rho_0, bottom_thickness
    integer :: isc, iec, jsc, jec
  end type generic_bld

  public generic_bld_alloc, generic_bld_dealloc, generic_bld_update, generic_bld_average

  contains

    subroutine generic_bld_alloc(bld, isc, iec, jsc, jec, Rho_0, bottom_thickness)
      type(generic_bld), intent(inout) :: bld
      integer, intent(in) :: isc, iec, jsc, jec
      real, intent(in) :: Rho_0, bottom_thickness
      allocate(bld%k_bot(isc:iec, jsc:jec))
      allocate(bld%rho_dzt_bot(isc:iec, jsc:jec))
      bld%isc = isc
      bld%iec = iec
      bld%jsc = jsc
      bld%jec = jec
      bld%Rho_0 = Rho_0
      bld%bottom_thickness = bottom_thickness
    end subroutine generic_bld_alloc

    subroutine generic_bld_dealloc(bld)
      type(generic_bld), intent(inout) :: bld
      nullify(bld%rho_dzt)
      nullify(bld%kmt)
      deallocate(bld%k_bot)
      deallocate(bld%rho_dzt_bot)
    end subroutine generic_bld_dealloc

    !> Update layer information needed for bottom averaging.
    !! Should be called at beginning of time step.
    subroutine generic_bld_update(bld, rho_dzt, grid_kmt)
      type(generic_bld), intent(inout) :: bld
      real, target :: rho_dzt(:, :, :)
      integer, target :: grid_kmt(:, :)
      integer :: i, j, k

      bld%rho_dzt => rho_dzt
      bld%kmt => grid_kmt

      do j = bld%jsc, bld%jec; do i = bld%isc, bld%iec
        bld%k_bot(i,j) = 0
        bld%rho_dzt_bot(i,j) = 0.0
        if (bld%kmt(i,j) .gt. 0) then
          do k = bld%kmt(i,j),1,-1
            ! Check if the top of layer k is within the bottom thickness.
            ! If so, include its properties in the bottom
            ! layer averages. Overshoots will be subtracted off later.
            if (bld%rho_dzt_bot(i,j).lt.(bld%Rho_0*bld%bottom_thickness)) then
              bld%k_bot(i,j) = k
              bld%rho_dzt_bot(i,j) = bld%rho_dzt_bot(i,j) + bld%rho_dzt(i,j,k)
            else
              exit
            endif
          enddo
        endif
      end do; end do
    end subroutine generic_bld_update

    !> Average a 3D field over the bottom layer defined by the bottom thickness.
    subroutine generic_bld_average(bld, field, field_btm)
      type(generic_bld), intent(in) :: bld
      real, intent(in) :: field(:, :, :)   !< 3D field to be averaged
      real, intent(out) :: field_btm(:, :) !< 2D field of bottom layer average
      integer :: i, j, k

      ! COBALT always calls generic_bld_update beforehand,
      ! but check anyways in case another module is using this routine.
      if (.not. associated(bld%rho_dzt) .or. .not. associated(bld%kmt)) then
        call MOM_error(FATAL, "generic_bld_update must be called before generic_bld_average.")
      endif

      do j = bld%jsc, bld%jec; do i = bld%isc, bld%iec
        field_btm(i,j) = 0.0
        if (bld%kmt(i,j) .gt. 0) then
          do k = bld%kmt(i,j), bld%k_bot(i,j), -1
            field_btm(i,j) = field_btm(i,j) + field(i,j,k) * bld%rho_dzt(i,j,k)
          enddo
          ! Remove overshoot from the last layer added to the average.
          field_btm(i,j) = field_btm(i,j) - field(i,j,bld%k_bot(i,j))*(bld%rho_dzt_bot(i,j) - bld%Rho_0*bld%bottom_thickness)
          field_btm(i,j) = field_btm(i,j) / (bld%bottom_thickness*bld%Rho_0)
        endif
      end do; end do
    end subroutine generic_bld_average
end module generic_bottom_layer_diags