*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     lpprmswrapper   lpopenAppend   lpclose
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpprmswrapper
     $   ( name, inform )

      implicit
     $     none
      character*(*)
     $     name
      integer
     $     inform
*     ==================================================================
*     Read options for lpopt from the file named name. inform .eq 0 if
*     successful.
*
*     09 Jan 2000: First version of lpprmswrapper
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
         call lpprms( ioptns, inform )
         close( iostat )
      end if

*     end of lpprmswrapper
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpopenappend
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
*     sucessful. Although opening for appending is not in the FORTRAN 77
*     standard, it is understood by f2c.
*
*     09 Jan 2000: First version of lpopenappend
*     ==================================================================
      open( iunit, iostat=inform, file=name, access='append' )

*     end of lpopenappend
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpclose
     $   ( iunit )

      integer
     $     iunit

*     ==================================================================
*     Close unit iunit.
*
*     09 Jan 2000: First version of lpclose
*     ==================================================================
      close( iunit )

*     end of lpclose
      end
