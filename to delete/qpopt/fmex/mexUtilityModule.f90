!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File: mexUtilityModule.f90
!
! Wrapper for mexUtility
!
! 19 Dec 2008
! Philip Gill and Michael Saunders
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module mexUtility
  use precision,   only : ip, rp

  implicit none
  private
  public               :: int2dbl

contains

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine int2dbl( n, intvec, dblevec )

    integer(ip), intent(in)  :: n, intvec(n)
    real(rp),    intent(out) :: dblevec(n)

    ! ==================================================================
    ! Copy integer vector to double precision vector.
    !
    ! First version by  Anders Forsgren 21 June 1989
    ! ==================================================================
    integer(ip)             :: i
    !---------------------------------------------------------------------

    do i = n, 1, -1
        dblevec(i) = intvec(i)
    end do

  end subroutine int2dbl

end module mexUtility
