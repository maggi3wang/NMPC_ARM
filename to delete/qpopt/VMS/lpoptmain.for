*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     File lpmain.f
*
*     Sample program for LPOPT Version 1.00-7  April 1993.
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      program            lpmain
      implicit           double precision(a-h,o-z)

*     Set the declared array dimensions.
*     ldA    = the declared row dimension of A.
*     maxn   = maximum no. of variables allowed for.
*     maxbnd = maximum no. of variables + linear constraints.
*     liwork = the length of the integer work array.
*     lwork  = the length of the double precision work array.

      parameter         (ldA    =   5, maxn   =   3,
     $                   liwork =  20, lwork  = 500,
     $                   maxbnd = maxn + ldA )

      integer            istate(maxbnd)
      integer            iwork(liwork)
      double precision   bl(maxbnd), bu(maxbnd), clamda(maxbnd)
      double precision   cvec(maxn)
      double precision   A(ldA,maxn), Ax(ldA), x(maxn)
      double precision   work(lwork)

      double precision   bigbnd
      character*20       lFile
      logical            byname, byunit

*     ------------------------------------------------------------------
*     This sample linear program (LP) is a portfolio investment problem
*     (see Chapter 7, pp 258--262 of ``Numerical Linear Algebra and
*     Optimization'', by Gill, Murray and Wright, Addison Wesley, 1990).
*     The problem involves the rearrangement of a portfolio of three
*     stocks, Glitter, Risky and Trusty, so that the net worth of the
*     investor is maximized.
*     The problem is characterized by the following data:
*                                Glitter     Risky        Trusty
*     1990 Holdings                 75       1000           25
*     1990 Price/share($)           20          2          100
*     2099 Price/share($)           18          3          102
*     2099 Dividend                  5          0            2
*
*     ------------------------------------------------------------------
*     The variables x(1), x(2) and x(3) represent the change in each of
*     the three stocks.
*
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
         lFile = 'lpoptmain.opt'
         open( iOptns, file=lFile, status='OLD',     err=800 )

         lFile = 'lpoptmain.out'
         open( iPrint, file=lFile, status='UNKNOWN', err=800 )

      else if ( byunit ) then
         lUnit = iOptns
         open( lUnit, status='OLD',     err=900 )

         lUnit = iPrint
         open( lUnit, status='UNKNOWN', err=900 )
      end if

*     =============================================================
*     Set the actual problem dimensions.
*     n      = the number of variables.
*     nclin  = the number of general linear constraints (may be 0).
*     =============================================================
      n      = 3
      nclin  = 5

*     Objective function:  maximize  5x(1) + 2x(3), or equivalently,
*     minimize -5x(1) - 2x(3).

      cvec(1) = - 5.0d+0
      cvec(2) =   0.0d+0
      cvec(3) = - 3.0d+0

*     ------------------------------------------------------------------
*     Define the value used to indicate an ``infinite'' bound.
*     ------------------------------------------------------------------
      bigbnd =  1.0d+21

*     -------------------------------------------------------------
*     a      = the general constraint matrix.
*     bl     = the lower bounds on  x  and  A*x.
*     bu     = the upper bounds on  x  and  A*x.
*     x      = the initial estimate of the solution.
*     -------------------------------------------------------------
*     A nonnegative amount of stock must be present after rearrangement.
*     For Glitter:  x(1) +   75 ge 0.
      bl(1) = -75.0d+0
      bu(1) =  bigbnd

*     For Risky:    x(2) + 1000 ge 0.
      bl(2) = -1000.0d+0
      bu(2) =  bigbnd

*     For Trusty:   x(3) +   25 ge 0.
      bl(3) = -25.0d+0
      bu(3) =  bigbnd

*     The current value of the portfolio must be the same after
*     rearrangement, i.e.,
*     20(75+x(1)) + 2(1000+x(2)) + 100(25+x(3)) = 6000,   or
*     20x(1) + 2x(2) + 100x(3) = 0.

      A(1,1)  =     20.0d+0
      A(1,2)  =      2.0d+0
      A(1,3)  =    100.0d+0
      bl(n+1) =     0.0d+0
      bu(n+1) =     0.0d+0

*     The value of the portfolio must increase by at least 5 per cent
*     at the end of the year, i.e.,
*     18(75+x(1)) + 3(1000+x(2)) + 102(25+x(3)) ge 6300,   or
*     18x(1) + 3x(2) + 102x(3) ge -600.

      A(2,1)  =    18.0d+0
      A(2,2)  =     3.0d+0
      A(2,3)  =   102.0d+0
      bl(n+2) = - 600.0d+0
      bu(n+2) =  bigbnd

*     ------------------------------------------------------------------
*     There are three ``balanced portfolio'' constraints.  The value of
*     a stock must constitute at least a quarter of the total final
*     value of the portfolio.  After rearrangement, the value of the
*     portfolio after is  20(75+x(1)) + 2(1000+x(2)) + 100(25+x(3)).
*     ------------------------------------------------------------------
*     If Glitter is to constitute at least a quarter of the final
*     portfolio, then   15x(1) - 0.5x(2) - 25x(3) ge 0.

      A(3,1)  =    15.0d+0
      A(3,2)  = -   0.5d+0
      A(3,3)  = -  25.0d+0
      bl(n+3) =     0.0d+0
      bu(n+3) =  bigbnd

*     If Risky is to constitute at least a quarter of the final
*     portfolio, then   -5x(1) + 1.5x(2) - 25x(3) ge -500.

      A(4,1)  = -   5.0d+0
      A(4,2)  =     1.5d+0
      A(4,3)  = -  25.0d+0
      bl(n+4) = - 500.0d+0
      bu(n+4) =  bigbnd

*     If Trusty is to constitute at least a quarter of the final
*     portfolio, then   -5x(1) - 0.5x(2) + 75x(3) ge -1000.

      A(5,1)  = -   5.0d+0
      A(5,2)  = -   0.5d+0
      A(5,3)  =    75.0d+0
      bl(n+5) = -1000.0d+0
      bu(n+5) =  bigbnd

*     Set the initial estimate of the solution.
*     This portfolio is infeasible.

      x(1) =   10.0d+0
      x(2) =   20.0d+0
      x(3) =  100.0d+0

*     ------------------------------------------------------------------
*     Set a few options via lpprm.
*     The Print file   will be on unit iPrint.
*     The Summary file will be on the default unit 6
*     (typically the screen).
*     ------------------------------------------------------------------
      call lpprmi( 'Print file          =', iPrint )
      call lpprmr( 'Infinite Bound size =', bigbnd )

*     Read the Options file.

      call lpprms( iOptns, inform )
      if (inform .ne. 0) then
         write(iPrint, 3000) inform
         stop
      end if

*     ------------------------------------------------------------------
*     Solve the problem.
*     ------------------------------------------------------------------
      call lpopt ( n, nclin, ldA,
     $             a, bl, bu,
     $             x, Ax, inform,
     $             iter, istate,
     $             clamda, obj, cvec,
     $             iwork, liwork, work, lwork )

*     Test for an error condition.

      if (inform .gt. 1) go to 999

*     ==============================================================
*     Solve the problem again with some new options set inline.
*     ==============================================================

*     Set some new options inline.
*     Suppress listing them on the Print file.

      call lpprmi( 'Optimality phase iterations', 10     )
      call lpprmr( 'Optimality tolerance       ', 1.0d-5 )
      call lpprm ( 'Print level                 = 10 '   )

      x(1) =  0.0d+0
      x(2) =  0.0d+0
      x(3) =  0.0d+0

*     ------------------------------------------------------------------
*     Solve the problem again.
*     ------------------------------------------------------------------
      call lpopt ( n, nclin, ldA,
     $             A, bl, bu,
     $             x, Ax, inform,
     $             iter, istate,
     $             clamda, obj, cvec,
     $             iwork, liwork, work, lwork )

*     Test for an error condition.

      if (inform .gt. 1) go to 999
      stop

*     Error conditions.

  800 if (iSumm  .gt. 0)
     $   write(iSumm , 4000) 'Error while opening file', lFile
      stop

  900 if (iSumm  .gt. 0)
     $   write(iSumm , 4010) 'Error while opening unit', lUnit
      stop

  999 if (iPrint .gt. 0) write(iPrint, 3010) inform
      stop

 3000 format(/ '  lpprms terminated with  inform =', I3)
 3010 format(/ '  lpopt  terminated with  inform =', I3)
 4000 format(/  a, 2x, a  )
 4010 format(/  a, 2x, i6 )

*     end of the example program for lpopt
      end
