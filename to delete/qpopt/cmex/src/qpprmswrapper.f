*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     qpprmswrapper   qpopenappend   qpclose
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpprmswrapper
     $   ( name, inform )

      implicit
     $     none
      character*(*)
     $     name
      integer
     $     inform
*     ==================================================================
*     Read options for qpopt from the file named name. inform .eq 0 if
*     successful.
*
*     09 Jan 2000: First version of qpprmswrapper
*     ==================================================================
      integer
     $     ioptns, iostat
*     ------------------------------------------------------------------
      parameter ( ioptns = 55 )
*     ------------------------------------------------------------------
      open( ioptns, iostat=iostat, file=name, status='old' )
      if ( 0 .ne. iostat ) then
         inform = 2 + iostat
      else
         call qpprms( ioptns, inform )
         close( iostat )
      end if

*     end of qpprmswrapper
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpopenappend
     $   ( iunit, name, inform )

      implicit
     $     none
      integer
     $     iunit
      character*(*)
     $     name
      integer
     $     inform

*     ==================================================================
*     Open file named name to FORTRAN unit iunit. inform .eq. 0 if
*     successful.  Opening for appending is not in the FORTRAN 77
*     standard, but it is understood by f2c.
*
*     09 Jan 2000: First version of qpopenappend
*     ==================================================================
      open( iunit, iostat=inform, file=name, access='append' )

*     end of qpopenappend
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpclose
     $   ( iunit )

      integer
     $     iunit

*     ==================================================================
*     Close unit iunit.
*
*     09 Jan 2000: First version of qpclose
*     ==================================================================
      close( iunit )

*     end of qpclose
      end
