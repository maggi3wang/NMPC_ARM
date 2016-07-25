*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     File qpmain.f
*
*     Sample program for QPOPT Version 1.0-10   June 1995.
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      program            qpmain
      implicit           double precision (a-h,o-z)

*     Set the declared array dimensions.
*     ldH    = the declared row dimension of H.
*     ldA    = the declared row dimension of A.
*     maxn   = maximum no. of variables allowed for.
*     maxbnd = maximum no. of variables + linear constraints.
*     leniw  = the length of the integer work array.
*     lenw   = the length of the double precision work array.

      parameter        ( ldH    =   8,   ldA  =   7,
     $                   maxn   =   8,
     $                   leniw  =  20,   lenw = 500,
     $                   maxbnd = maxn + ldA )

      integer            istate(maxbnd)
      integer            iw(leniw)
      double precision   H(ldH,maxn)
      double precision   bl(maxbnd), bu(maxbnd), clamda(maxbnd)
      double precision   cvec(maxn)
      double precision   A(ldA,maxn), Ax(ldA), x(maxn)
      double precision   w(lenw)
      external           qpHess, qpHes1

      double precision   bigbnd
      character*20       lFile
      logical            byname, byunit

      parameter        ( point1 = 0.1d+0, zero = 0.0d+0, one = 1.0d+0 )

*     ------------------------------------------------------------------
*     Assign file numbers and open files by various means.
*     (Some systems don't need explicit open statements.)
*     iOptns = unit number for the Options file.
*     iPrint = unit number for the Print file.
*     iSumm  = unit number for the Summary file.
*     ------------------------------------------------------------------
      iOptns = 4
      iPrint = 10
      iSumm  = 6
      byname = .true.
      byunit = .false.

      if ( byname ) then
         lFile = 'qpoptmain.opt'
         open( iOptns, file=lFile, status='OLD',     err=800 )

         lFile = 'qpoptmain.out'
         open( iPrint, file=lFile, status='UNKNOWN', err=800 )

      else if ( byunit ) then
         lUnit = iOptns
         open( lUnit, status='OLD',     err=900 )

         lUnit = iPrint
         open( lUnit, status='UNKNOWN', err=900 )
      end if

*     ==================================================================
*     Set the actual problem dimensions.
*     n      = the number of variables.
*     nclin  = the number of general linear constraints (may be 0).
*     bigbnd = the Infinite Bound size.
*     ==================================================================
      n      = 8
      nclin  = 7
      bigbnd = 1.0d+21

*     ------------------------------------------------------------------
*     Define H, A, bl, bu, cvec and the initial x.
*     This example is due to Bunch and Kaufman,
*     `A computational method for the indefinite quadratic programming
*     problem', Linear Algebra and its Applications, 34, 341-370 (1980).
*     ------------------------------------------------------------------
      do 200, j = 1, n
         do 120, i = 1, nclin
            A(i,j) = zero
  120    continue

         do 150, i = 1, j-1
            H(i,j) = abs(i - j)
  150    continue

         H(j,j)  = 1.69d+0
         bl(j)   = - j - point1*dble(j - 1)
         bu(j)   =   j
         cvec(j) =   dble(8 - j)
         x(j)    = - dble(j)
  200 continue

      do 220, i = 1, nclin
         A(i,i)   = - one
         A(i,i+1) =   one
         bl(n+i)  = - one - 0.05d+0*dble(i - 1)
         bu(n+i)  =   bigbnd
  220 continue

*     ------------------------------------------------------------------
*     Set a few options in-line.
*     The Print file   will be on unit iPrint.
*     The Summary file will be on the default unit 6
*     (typically the screen).
*     ------------------------------------------------------------------
      call qpprmi( 'Print file          =', iPrint )
      call qpprmr( 'Infinite Bound size =', bigbnd )

*     Read the Options file.

      call qpprms( iOptns, inform )
      if (inform .ne. 0) then
         write(iPrint, 3000) inform
         stop
      end if

*     ------------------------------------------------------------------
*     Solve the QP problem.
*     ------------------------------------------------------------------
      call qpopt ( n, nclin, ldA, ldH,
     $             A, bl, bu, cvec, H, qpHess,
     $             istate, x,
     $             inform, iter, obj, Ax, clamda,
     $             iw, leniw, w, lenw )

*     Test for an error condition.

      if (inform .gt. 1) go to 999

*     ==================================================================
*     Re-solve the problem with the Hessian defined by a subroutine.
*     ==================================================================

*     Set some new options in-line,
*     but stop listing them on the Print file.

      call qpprm ( 'Nolist'                            )
      call qpprm ( 'Problem Type  QP2'                 )
      call qpprm ( 'Feasibility Tolerance  =  1.0e-10' )
      call qpprmr( 'Optimality tolerance   ', 1.0d-5   )
      call qpprmi( 'Print level            ',      10  )

*     ------------------------------------------------------------------
*     Define a new starting point.
*     ------------------------------------------------------------------
      x(1) =  -1.0
      x(2) =  12.0
      x(3) =  -3.0
      x(4) =  14.0
      x(5) =  -5.0
      x(6) =  16.0
      x(7) =  -7.0
      x(8) =  18.0

      call qpopt ( n, nclin, ldA, ldH,
     $             A, bl, bu, cvec, H, qpHes1,
     $             istate, x,
     $             inform, iter, obj, Ax, clamda,
     $             iw, leniw, w, lenw )

      if (inform .gt. 1) go to 999
      stop

*     ------------------------------------------------------------------
*     Error conditions.
*     ------------------------------------------------------------------
  800 write(iSumm , 4000) 'Error while opening file', lFile
      stop

  900 write(iSumm , 4010) 'Error while opening unit', lUnit
      stop

  999 write(iPrint, 3010) inform
      stop


 3000 format(/ '  QPPRMS terminated with  inform =', i3)
 3010 format(/ '  QPOPT  terminated with  inform =', i3)
 4000 format(/  a, 2x, a  )
 4010 format(/  a, 2x, i6 )

*     end of the example program for QPOPT
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpHes1( n, ldH, jthcol, H, x, Hx, iw, leniw, w, lenw )

      implicit           double precision (a-h,o-z)

      integer            iw(leniw)
      double precision   H(ldH,*), Hx(n), x(n)
      double precision   w(lenw)

*     ==================================================================
*     qpHes1    computes the vector  Hx = (H)*x  for some matrix H
*     that defines the Hessian of the required QP problem.
*
*     In this version of  qpHess  the Hessian matrix is implicit.
*     The array  H  is not accessed.  There is no special coding
*     for the case jthcol .gt. 0.
*     ==================================================================
      do 200, i = 1, n
         sum    = 1.69d+0*x(i)
         do 100, j = 1, n
            sum = sum + dble( abs(i-j) )*x(j)
  100    continue
         Hx(i)  = sum
  200 continue

*     end of qpHes1
      end
