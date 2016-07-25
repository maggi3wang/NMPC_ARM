!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File precisionModule.f90
!
! Defines ip = integer precision
!         rp = real precision
! for use in integer(ip), real(rp) declarations.
!
! 03 Apr 2008: Original version.
! 23 Dec 2008: Easier to set ip and rp explicitly.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module precisionModule

  implicit none
  private
  public  :: ip, rp

! intrinsic           :: selected_int_kind, selected_real_kind

! integer,  parameter :: ip = 4 ! selected_int_kind(8)   ! integer*4
  integer,  parameter :: ip = 8 ! selected_int_kind(15)  ! integer*8
  integer,  parameter :: rp = 8 ! selected_real_kind(15) ! real*8

end module precisionModule
