*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*                                                                      *
*    Q P O P T         Version 1.0-10(2)       November 4, 2001        *
*                                                                      *
*    Philip E. Gill    Walter  Murray          Michael A. Saunders     *
*    UC San Diego      Stanford University     Stanford University     *
*                                                                      *
*----------------------------------------------------------------------*
*     (C) 1992--1997  Regents of the University of California          *
*                     and the Trustees of Stanford University          *
*                                                                      *
*     This software is NOT in the public domain. Its use is governed   *
*     by a license agreement with either Stanford University or the    *
*     University of California.  It is a breach of copyright to make   *
*     copies except as authorized by the license agreement.            *
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     File  qpoptsubs.f
*
*     qpopt    qpcolr   qpcore   qpcrsh   qpdflt   qpgetd   qpHess
*     qploc    qpprm    qpprmi   qpprmr   qpprms   qpprnt
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpopt ( n, nclin, ldA, ldH,
     $                   A, bl, bu, cvec, H,
     $                   qpHess, istate, x,
     $                   inform, iter, obj, Ax, clamda,
     $                   iw, leniw, w, lenw )

      implicit           double precision (a-h,o-z)
      integer            leniw, lenw
      integer            istate(n+nclin)
      integer            iw(leniw)
      double precision   A(ldA,*), Ax(*), bl(n+nclin), bu(n+nclin)
      double precision   clamda(n+nclin), cvec(*)
      double precision   H(ldH,*), x(n)
      double precision   w(lenw)
      external           qpHess, qpprnt

*     ==================================================================
*     QPOPT  solves problems of the form
*
*              minimize               f(x)
*                 x
*                                    (  x )
*              subject to    bl  .le.(    ).ge.  bu,
*                                    ( Ax )
*
*     where  '  denotes the transpose of a column vector,  x  denotes
*     the n-vector of parameters and  F(x) is one of the following...
*
*     FP            =              none    (find a feasible point)
*     LP            =    c'x
*     QP1           =          1/2 x'Hx     H n x n symmetric
*     QP2 (default) =    c'x + 1/2 x'Hx     H n x n symmetric
*     QP3           =          1/2 x'G'Gx   G m x n upper trapezoidal
*     QP4           =    c'x + 1/2 x'G'Gx   G m x n upper trapezoidal
*
*     Both  H and G  are stored in the two-dimensional array  H  of
*     row dimension  ldH.  H  can be entered explicitly as the matrix
*     H,  or implicitly via a user-supplied version of the
*     subroutine qpHess.  If  ldH = 0,  H is not touched.
*
*     The vector  c  is entered in the one-dimensional array  cvec.
*
*     nclin  is the number of general linear constraints (rows of  A).
*     (nclin may be zero.)
*
*     The first  n  components of  bl  and   bu  are lower and upper
*     bounds on the variables.  The next  nclin  components are
*     lower and upper bounds on the general linear constraints.
*
*     The matrix  A  of coefficients in the general linear constraints
*     is entered as the two-dimensional array  A  (of dimension
*     ldA by n).  If nclin = 0, a is not referenced.
*
*     The vector  x  must contain an initial estimate of the solution,
*     and will contain the computed solution on output.
*
*     For more information, see:
*     User's guide  for QPOPT (Version 1.0), by
*     P. E. Gill, W. Murray and M. A. Saunders.
*
*     Version 1.0-6   Jun 30, 1991. (Nag Mk 16 version).
*     Version 1.0-7   Mar 21, 1993. Summary file added.
*     Version 1.0-8   Apr 10, 1994. Sum of infeas. added as an option.
*     Version 1.0-9   Jul 15, 1994. Debug output eliminated.
*     Version 1.0-10  Sep 15, 1995. New document.
*
*     This version of  QPOPT  dated 04 November 2001.
*     (C) 1992--2001  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/

      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      common    /sol3cm/ lennam, ldT, ncolT, ldQ
      common    /sol4cm/ epspt3, epspt5, epspt8, epspt9
      common    /sol5cm/ Asize, dTmax, dTmin

      parameter         (lenlc = 20)
      common    /sol1lc/ loclc(lenlc)
      save      /sol1lc/
      logical            header, prnt
      common    /sol4lc/ alfa  , trulam, isdel , jdel  , jadd  ,
     $                   header, prnt

      common    /sol1qp/ lqptyp, mHess

*     +Include lcparm-Sep-95++++++++++++++++++++++++++++++++++++++++++++
      parameter         (mxparm = 30)
      integer            iprmlc(mxparm), ipsvlc
      double precision   rprmlc(mxparm), rpsvlc

      common    /lcpar1/ ipsvlc(mxparm),
     $                   itmax1, itmax2, kchk  , kcycle, lcrash, lprob ,
     $                   maxact, mxfree, maxnZ , mm    , minsum, msglc ,
     $                   nn    , nnclin, nprob , ipadlc(15)

      common    /lcpar2/ rpsvlc(mxparm),
     $                   bigbnd, bigdx , bndlow, bndupp, tolact, tolfea,
     $                   tolOpt, tolrnk, rpadlc(22)

      equivalence       (iprmlc(1), itmax1), (rprmlc(1), bigbnd)

      save      /lcpar1/, /lcpar2/
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      equivalence       (msglc , msglvl)

*     local variables.

      logical            cold  , cset  , done  , found , halted, hot
      logical            named , rowerr, Rset  , unitQ , vertex, warm
      character*6        msg
      character*2        prbtyp
      character*4        start
      character*16       names(1)
      parameter         (zero   =0.0d+0, point3 =3.3d-1, half   =0.5d+0)
      parameter         (point8 =0.8d+0, point9 =0.9d+0, one    =1.0d+0)
      parameter         (hundrd =1.0d+2                                )

      character*40       title
      data               title
     $                 / 'QPOPT  ---  Version 1.0-10(2)  Nov  2001' /

*     Set the machine-dependent constants.

      call mchpar()

      epsmch = wmach( 3)
      rteps  = wmach( 4)

      epspt3 = epsmch**point3
      epspt5 = rteps
      epspt8 = epsmch**point8
      epspt9 = epsmch**point9

      named  = .false.

      names(1) = ' '

      iter   = 0
      header = .true.
      prnt   = .true.

*     Set the default values of the parameters.

      call qpdflt( n, nclin, title )

C-->  condmx = max( one/epspt5, hundrd )
C-->  condmx = max( one/epspt3, hundrd )
      condmx = max( one/epspt5, hundrd )

      lqptyp = lprob
      mHess  = mm
      nctotl = n   + nclin

*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*     Set all parameters determined by the problem type.
*        key  lqptyp        objective
*        ---  ------        ---------
*        FP      1              none    (find a feasible point)
*        LP      2    c'x
*        QP1     3          1/2 x'Ax     A n x n symmetric
*        QP2     4    c'x + 1/2 x'Ax     A n x n symmetric
*        QP3     5          1/2 x'A'Ax   A m x n upper trapezoidal
*        QP4     6    c'x + 1/2 x'A'Ax   A m x n upper trapezoidal
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if      (lqptyp .eq. 1 ) then
         prbtyp = 'FP'
         cset   = .false.

      else if (lqptyp .eq. 2 ) then
         prbtyp = 'LP'
         cset   = .true.

      else if (lqptyp .ge. 3  .and.  lqptyp .le. 6) then
         prbtyp = 'QP'
         cset   = .true.
         if (lqptyp .eq. 3  .or.  lqptyp .eq. 5) cset = .false.
      else
         prbtyp = 'illegal'
         msg    = 'noprob'
         go to 800
      end if

*     Assign the dimensions of arrays in the parameter list of qpcore.
*     Economies of storage are possible if the minimum number of active
*     constraints and the minimum number of fixed variables are known in
*     advance.  The expert user should alter minact and minfxd
*     accordingly.
*     If a linear program is being solved and the matrix of general
*     constraints has fewer rows than columns, i.e.,  nclin .lt. n,  a
*     non-zero value is known for minfxd.  In this case, vertex must
*     be set  .true..

      vertex =  prbtyp .ne. 'QP'  .and.  nclin  .lt. n

      minfxd = n      - mxfree
      minact = mxfree - maxnZ

      ldT    = max( maxnZ, maxact )
      ncolT  = mxfree
      ldR    = ldT
      if (nclin .eq. 0) then
         ldQ = 1
      else
         ldQ = max( 1, mxfree )
      endif

      numinf = 0
      ncnln  = 0
      lennam = 1

*     ==================================================================
*     Cold start:  Only  x  is provided.
*     Warm start:  Initial working set is specified in  istate.
*     Hot  start:  The work arrays  iw  and  w  are assumed to have been
*                  initialized during a previous run.
*                  The first four components of  iw  contain details
*                  on the dimension of the initial working set.
*     ==================================================================
      if      (lcrash .eq. 0) then
         start = 'cold'
      else if (lcrash .eq. 1) then
         start = 'warm'
      else if (lcrash .eq. 2) then
         start = 'hot '
      end if

      cold   = lcrash .eq. 0
      warm   = lcrash .eq. 1
      hot    = lcrash .eq. 2

*     Allocate remaining work arrays.

      litotl = 3
      lwtotl = 0
      call qploc ( cset, n, nclin, litotl, lwtotl )

      lkactv = loclc( 1)
      lkx    = loclc( 2)

      lfeatu = loclc( 3)
      lAnorm = loclc( 4)
      lHx    = loclc( 6)
      ld     = loclc( 7)
      lgq    = loclc( 8)
      lcq    = loclc( 9)
      lrlam  = loclc(10)
      lR     = loclc(11)
      lT     = loclc(12)
      lQ     = loclc(13)
      lwtinf = loclc(14)
      lwrk   = loclc(15)

*     Check input parameters and storage limits.

      call cminit( nerror, msglvl, start,
     $             leniw, lenw, litotl, lwtotl,
     $             n, nclin, ncnln,
     $             istate, named, names,
     $             bigbnd, bl, bu, clamda, x )

      if (nerror .gt. 0) then
         iw(1) = litotl
         iw(2) = lwtotl
         msg   = 'errors'
         go to 800
      end if

*     ------------------------------------------------------------------
*     Define the initial feasibility tolerances in clamda.
*     ------------------------------------------------------------------
      if (tolfea .gt. zero)
     $   call dload ( n+nclin, tolfea, w(lfeatu), 1 )

      call cmdgen( 'Initialize anti-cycling variables', msglvl,
     $             n, nclin, nmoved, iter, numinf,
     $             istate, bl, bu, clamda, w(lfeatu), x )

      if (cold  .or.  warm) then
*        ---------------------------------------------------------------
*        Cold or warm start.  Just about everything must be initialized.
*        The only exception is istate during a warm start.
*        ---------------------------------------------------------------
         iAnrmj = lAnorm
         do 200, j = 1, nclin
            w(iAnrmj) = dnrm2 ( n, A(j,1), ldA )
            iAnrmj    = iAnrmj + 1
  200    continue
         if (nclin .gt. 0)
     $      call dcond ( nclin, w(lAnorm), 1, Asize, Amin )

         call dcond ( nctotl, w(lfeatu), 1, feamax, feamin )
         call dcopy ( nctotl, w(lfeatu), 1, w(lwtinf), 1 )
         call dscal ( nctotl, (one/feamin), w(lwtinf), 1 )

*        ---------------------------------------------------------------
*        Define the initial working set.
*               nfree ,  nactiv,  kactiv, kx,
*               istate (if start  = 'cold')
*               nartif (if vertex = 'true')
*        ---------------------------------------------------------------
         call cmcrsh( start , vertex,
     $                nclin , nctotl, nactiv, nartif,
     $                nfree , n     , ldA,
     $                istate, iw(lkactv), iw(lkx),
     $                bigbnd, tolact,
     $                A, Ax, bl, bu, clamda, x, w(lgq), w(lwrk) )

*        ---------------------------------------------------------------
*        Compute the TQ factorization of the working-set matrix.
*        ---------------------------------------------------------------
         unitQ = .true.
         nZ    = nfree
         if (nactiv .gt. 0) then
            iT     = nactiv + 1
            nact1  = nactiv
            nactiv = 0
            ngq    = 0

            call rzadds( unitQ, vertex,
     $                   1, nact1, iT, nactiv, nartif, nZ, nfree,
     $                   nrejtd, ngq, n, ldQ, ldA, ldT,
     $                   istate, iw(lkactv), iw(lkx),
     $                   condmx,
     $                   A, w(lT), w(lgq), w(lQ),
     $                   w(lwrk), w(ld), w(lrlam) )
         end if

      else if (hot) then
*        ---------------------------------------------------------------
*        Arrays  iw  and  w  have been defined in a previous run.
*        The first three elements of  iw  are  unitQ,  nfree and nactiv.
*        ---------------------------------------------------------------
         unitQ  = iw(1) .eq. 1
         nfree  = iw(2)
         nactiv = iw(3)

         nZ     = nfree - nactiv
      end if

      if (cset) then

*        Install the transformed linear term in cq.

         call dcopy ( n, cvec, 1, w(lcq), 1 )
         call cmqmul( 6, n, nZ, nfree, ldQ, unitQ,
     $                iw(lkx), w(lcq), w(lQ), w(lwrk) )
      end if

      Rset   = .false.
      if (prbtyp .eq. 'LP') then
         itmax  = max(itmax1, itmax2)
         itnlim = itmax
      else
         itmax  = itmax1
         itnlim = itmax1 + itmax2
      end if

      jinf   =  0

C+    When minimizing the sum of infeasibilities,
C+    nZr    =  nZ  implies steepest-descent in the two-norm.
C+    nZr    =  0   implies steepest-descent in the infinity norm.
      nZr    =  0

*     ==================================================================
*     repeat               (until working set residuals are acceptable)
*        ---------------------------------------------------------------
*        Move x onto the constraints in the working set.
*        ---------------------------------------------------------------
  300    call cmsetx( rowerr, unitQ, nclin,
     $                nactiv, nfree, nZ,
     $                n, ldQ, ldA,
     $                ldT, istate, iw(lkactv),
     $                iw(lkx),jmax, errmax,
     $                xnorm, A, Ax,
     $                bl, bu, clamda,
     $                w(lT), x, w(lQ),
     $                w(ld), w(lwrk) )

         if (rowerr) then
            msg    = 'rowerr'
            numinf = 1
            go to 800
         end if

         call lpcore( prbtyp, msg,
     $                cset, named, names,
     $                Rset, unitQ,
     $                iter, itmax, jinf, nviol,
     $                n, nclin, ldA,
     $                nactiv, nfree, nZr, nZ,
     $                istate, iw(lkactv), iw(lkx),
     $                qpprnt,
     $                obj, numinf, xnorm,
     $                A, Ax, bl, bu,
     $                cvec, clamda, w(lfeatu),
     $                x, iw, w )

         if (prbtyp .eq. 'QP'  .and.  msg .eq. 'feasbl') then
            if (msglvl .gt. 0) then
               if (iPrint .gt. 0) write(iPrint, 1000) iter
               if (iSumm  .gt. 0) write(iSumm , 1000) iter
            end if
            itmax  = min( iter + itmax2, itnlim )

*           ------------------------------------------------------------
*           Compute the first QP objective and transformed gradient.
*           ------------------------------------------------------------
            if (cset) then
               obj = ddot( n, cvec, 1, x, 1 )
            else
               obj = zero
            end if

            jthcol = 0
            call qpHess( n, ldH, jthcol, H, x, w(lHx), iw,leniw,w,lenw )
            obj    = obj + half*ddot( n, w(lHx), 1, x, 1 )
            call dcopy ( n, w(lHx), 1, w(lgq), 1 )
            call cmqmul( 6, n, nZ, nfree, ldQ, unitQ,
     $                   iw(lkx), w(lgq), w(lQ), w(lwrk) )
            if (cset)
     $         call daxpy ( n, one, w(lcq), 1, w(lgq), 1 )

*           ------------------------------------------------------------
*           Find the Cholesky factor R of an initial reduced Hessian.
*           The magnitudes of the diagonals of  R  are nonincreasing.
*           ------------------------------------------------------------
            if (cset) then
               ngq = 2
            else
               ngq = 1
            end if
            Hsize  = one
            call qpcrsh( unitQ, qpHess,
     $                   maxnZ, n, ngq, nZr, nZ, nfree,
     $                   ldQ, ldH, ldR,
     $                   iw(lkx), Hsize, tolrnk,
     $                   w(lgq), H, w(lR), w(lQ),
     $                   w(lwrk), w(lrlam),
     $                   iw, leniw, w, lenw )

            call qpcore( prbtyp, msg,
     $                   cset, named, names,
     $                   unitQ,
     $                   iter, itmax, nviol,
     $                   n, nclin, ldA, ldH,
     $                   nactiv, nfree, nZr, nZ,
     $                   istate, iw(lkactv), iw(lkx),
     $                   qpHess, qpprnt,
     $                   obj, xnorm, Hsize,
     $                   A, Ax, bl, bu,
     $                   cvec, clamda, w(lfeatu),
     $                   H, x,
     $                   iw, leniw, w, lenw )
         end if

         found  = msg .eq. 'optiml'  .or.
     $            msg .eq. 'feasbl'  .or.
     $            msg .eq. 'deadpt'  .or.
     $            msg .eq. 'weak  '  .or.
     $            msg .eq. 'unbndd'  .or.
     $            msg .eq. 'infeas'
         halted = msg .eq. 'itnlim'  .or.
     $            msg .eq. 'Rz2big'

         if (found) then
            call cmdgen( 'Optimal', msglvl,
     $                   n, nclin, nmoved, iter, numinf,
     $                   istate, bl, bu, clamda, w(lfeatu), x )
         end if

         done   = found  .and.  nviol   .eq. 0
     $                   .and.  nmoved  .eq. 0

*     until      done  .or.  halted
      if (.not. (done  .or.  halted)) go to 300
*     ==================================================================
*     Set   clamda.  Print the full solution.
*     Clean up.  Save values for a subsequent hot start.
*     ------------------------------------------------------------------
      call cmwrap( .true., nfree, ldA,
     $             n, nclin, nctotl,
     $             nactiv, istate, iw(lkactv), iw(lkx),
     $             A, bl, bu, x, clamda, w(lfeatu),
     $             w(lwrk), w(lrlam), x )
      call cmprnt( msglvl, n, nclin, nctotl, bigbnd,
     $             named, names, istate,
     $             bl, bu, clamda, w(lfeatu), w(lwrk) )

      iw(1) = 0
      if (unitQ) iw(1) = 1
      iw(2) = nfree
      iw(3) = nactiv

*     ==================================================================
*     Print messages if required.
*     Recover the optional parameters set by the user.
*     ==================================================================
  800 prnt = msglvl .gt. 0  .and.  iPrint .gt. 0
      if (     msg .eq. 'feasbl') then
         inform = 0
         if (prnt) write(iPrint, 2001)
      else if (msg .eq. 'optiml') then
         inform = 0
         if (prnt) write(iPrint, 2002) prbtyp
      else if (msg .eq. 'deadpt'  .or.  msg .eq. 'weak  ') then
         inform = 1
         if (prnt) then
            if (prbtyp .eq. 'QP') then
               write(iPrint, 2010)
               if (nZ .gt. nZr) write(iPrint, 2015) nZ-nZr
            else
               write(iPrint, 2011)
            end if
         end if
      else if (msg .eq. 'unbndd') then
         inform = 2
         if (prnt) write(iPrint, 2020) prbtyp
      else if (msg .eq. 'infeas') then
         inform = 3
         if (prnt) write(iPrint, 2030)
      else if (msg .eq. 'rowerr') then
         inform = 3
         if (prnt) write(iPrint, 2035)
      else if (msg .eq. 'itnlim') then
         inform = 4
         if (prnt) write(iPrint, 2040)
      else if (msg .eq. 'Rz2big') then
         inform = 5
         if (prnt) write(iPrint, 2050) maxnZ
      else if (msg .eq. 'errors') then
         inform = 6
         if (prnt) write(iPrint, 2060) nerror
      else if (msg .eq. 'noprob') then
         inform = 7
         if (prnt) write(iPrint, 2070)
      end if

      if (prnt) then
         if (inform .lt.   5) then
            if      (numinf .eq. 0) then
               if (prbtyp .ne. 'FP') write(iPrint, 3000) prbtyp, obj
            else if (inform .eq. 3) then
               if (msg .eq. 'infeas') then
                  if (minsum .eq. 0) then
                     write(iPrint, 3010) obj
                  else
                     write(iPrint, 3011) obj
                  end if
               else if (msg .eq. 'rowerr') then
                  write(iPrint, 3015) errmax
               end if
            else
               write(iPrint, 3020) obj
            end if
         end if
      end if

      if (inform .lt. 6) then
         if (msglvl .gt. 0) then
            if (iPrint .gt. 0) write(iPrint, 2000) prbtyp, iter, inform
            if (iSumm  .gt. 0) write(iSumm , 2000) prbtyp, iter, inform
         end if
      end if

      call icopy ( mxparm, ipsvlc, 1, iprmlc, 1 )
      call dcopy ( mxparm, rpsvlc, 1, rprmlc, 1 )

      return

 1000 format(  ' Itn', i6, ' -- Feasible point found.' )
 2000 format(/ ' Exit from ', a2, ' problem after ', i4, ' iterations.',
     $         '  Inform =', i3 )
 2001 format(/ ' Exit QPOPT - Feasible point found.     ')
 2002 format(/ ' Exit QPOPT - Optimal ', a2, ' solution.')
 2010 format(/ ' Exit QPOPT - Iterations terminated at a dead-point',
     $         ' (check the optimality conditions).     ')
 2011 format(/ ' Exit QPOPT - Optimal solution is not unique.' )
 2015 format(  '            - Artificial constraints in working set = ',
     $         i4 )
 2020 format(/ ' Exit QPOPT - ', a2,         ' solution is unbounded.' )
 2030 format(/ ' Exit QPOPT - No feasible point for the linear',
     $         ' constraints.')
 2035 format(/ ' Exit QPOPT - Cannot satisfy the constraints to the',
     $         ' accuracy requested.')
 2040 format(/ ' Exit QPOPT - Too many iterations.')
 2050 format(/ ' Exit QPOPT - Reduced Hessian exceeds assigned',
     $         ' dimension.   maxnZ = ', i4
     $       / ' Problem abandoned.' )
 2060 format(/ ' Exit QPOPT - ', i10, ' errors found in the input',
     $         ' parameters.  Problem abandoned.'         )
 2070 format(  ' Exit QPOPT - Problem type not recognized.'
     $       / ' Problem abandoned.'         )

 3000 format(/ ' Final ', a2, ' objective value =', g16.7 )
 3010 format(/ ' Sum of infeasibilities =',         g16.7 )
 3011 format(/ ' Minimum sum of infeasibilities =', g16.7 )
 3015 format(/ ' Maximum row error =',              g16.7 )
 3020 format(/ ' Final sum of infeasibilities =',   g16.7 )

*     end of qpopt
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpcolr( singlr, posdef, renewr, unitQ,
     $                   n, nZr, nfree, ldQ, ldH, ldR,
     $                   kx, Hsize, Dzz, tolrnk,
     $                   qpHess, H, R, Q,
     $                   Hz, wrk, iw, leniw, w, lenw )

      implicit           double precision (a-h,o-z)
      logical            singlr, posdef, renewr, unitQ
      integer            kx(n)
      integer            iw(leniw)

      double precision   H(ldH,*), R(ldR,*), Hz(n), Q(ldQ,*)
      double precision   wrk(n)
      double precision   w(lenw)
      external           qpHess

*     ==================================================================
*     QPCOLR  is used to compute the last column of the (nZr x nZr)
*     triangular factor Rz such that
*                    Hz  =  (Rz)'D(Rz),
*     where Hz is the reduced Hessian Z'HZ, and D is a diagonal
*     matrix.  If  Hz  is positive definite, Rz is the Cholesky factor
*     of  Hz  and  D  is the identity matrix;  otherwise, D(nZr) is
*     negative or small and the last diagonal of Rz is one.
*
*     The element D(nZr) is stored in Dzz.  Dzz is equal to one if
*     posdef is true.
*
*     Original f66 version written by PEG,   March-1982.
*     This version of  qpcolr  dated 16-Jan-1995.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      parameter         (zero = 0.0d+0, one = 1.0d+0, ten = 10.0d+0)

      if (nZr .eq. 0) then
         posdef = .true.
         renewr = .false.
         singlr = .false.
         Dzz    =  one
      else
         if (renewr) then
*           ------------------------------------------------------------
*           Compute the first nZr-1 elements of the last column of Rz
*           and Dzznew, the square of the last diagonal element.
*           ------------------------------------------------------------
            call dload ( n, zero, wrk, 1 )
            if (unitQ) then

*              Only bounds are in the working set.  The nZr-th column of
*              Z is just a column of the identity matrix.

               jthcol      = kx(nZr)
               wrk(jthcol) = one
            else

*              Expand the new column of  Z  into an n-vector.

               do 110, k = 1, nfree
                  j      = kx(k)
                  wrk(j) = Q(k,nZr)
  110          continue
               jthcol = 0
            end if

*           Compute the nZr-th column of Z'HZ.

            call qpHess( n, ldH, jthcol, H, wrk, Hz, iw, leniw, w,lenw )
            call cmqmul( 4, n, nZr, nfree, ldQ, unitQ,
     $                   kx, Hz, Q, wrk )
            call dcopy ( nZr, Hz, 1, R(1,nZr), 1 )

            nZr1   = nZr - 1
            zthz   = R(nZr,nZr)
            Dzznew = zthz
            if (nZr1 .gt. 0) then
               call dtrsv ( 'U', 'T', 'N', nZr1, R, ldR, R(1,nZr), 1 )
               rznorm  = dnrm2 ( nZr1, R(1,nZr), 1 )
               Dzznew  = zthz - rznorm*rznorm
            end if

            R(nZr,nZr) = one
            Dzz        = Dzznew

*           Update the estimate of the norm of the Hessian.

            Hsize  = max( Hsize, abs( zthz ) )

         end if

         Dzznew = Dzz*R(nZr,nZr)**2

*        ---------------------------------------------------------------
*        Attempt to compute Rzz, the square root of  Dzznew.  The last
*        diagonal of Rz.  The variables posdef and singlr are set here.
*        They are used to indicate if the new Z'HZ is positive definite
*        or singular.  If the required diagonal modification is large
*        the last row and column of Rz are marked for recomputation next
*        iteration.
*        ---------------------------------------------------------------
*        Rdsmin is the square of the smallest allowable diagonal element
*        for a positive-definite Cholesky factor.  Note that the test
*        for positive definiteness is unavoidably scale dependent.

         if (nZr .eq. 1) then
            rdsmin =  tolrnk*Hsize
         else
            call dcond ( nZr, R, ldR+1, dRzmax, dRzmin )
            rdsmin = (tolrnk*dRzmax)*dRzmax
         end if

         posdef =  Dzznew .gt. rdsmin

         if (posdef) then
            Dzz    = one
            Rzz    = sqrt( Dzznew )
            renewr = .false.
            singlr = .false.
         else
            Dzz    = Dzznew
            Rzz    = one
            singlr = Dzznew .ge. - rdsmin
            renewr = Dzznew .lt. - ten*Hsize
         end if

         R(nZr,nZr) = Rzz
      end if

*     end of qpcolr
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpcore( prbtyp, msg,
     $                   cset, named, names,
     $                   unitQ,
     $                   iter, itmax, nviol,
     $                   n, nclin, ldA, ldH,
     $                   nactiv, nfree , nZr, nZ,
     $                   istate, kactiv, kx,
     $                   qpHess, qpprnt,
     $                   objqp, xnorm, Hsize,
     $                   A, Ax, bl, bu,
     $                   cvec, featol, featlu,
     $                   H, x,
     $                   iw, leniw, w, lenw )

      implicit           double precision (a-h,o-z)
      character*2        prbtyp
      character*6        msg
      character*16       names(*)
      logical            cset, named, unitQ

      integer            istate(n+nclin), kactiv(n), kx(n)
      integer            iw(leniw)

      double precision   A(ldA,*), Ax(*), bl(n+nclin), bu(n+nclin),
     $                   cvec(*), featol(n+nclin), featlu(n+nclin),
     $                   H(ldH,*), x(n)
      double precision   w(lenw)
      external           qpHess, qpprnt

*     ==================================================================
*     qpcore  is a subroutine for general quadratic programming.
*     On entry, it is assumed that an initial working set of
*     linear constraints and bounds is available.
*     The arrays  istate, kactiv and kx  will have been set accordingly
*     and the arrays  T  and  Q  will contain the TQ factorization of
*     the matrix whose rows are the gradients of the active linear
*     constraints with the columns corresponding to the active bounds
*     removed.  The TQ factorization of the resulting (nactiv by nfree)
*     matrix is  A(free)*Q = (0 T),  where Q is (nfree by nfree) and T
*     is upper-triangular.
*
*     Over a cycle of iterations, the feasibility tolerance featol
*     increases slightly (from tolx0 to tolx1 in steps of tolinc).
*     this ensures that all steps taken will be positive.
*
*     After idegen consecutive iterations, variables within featol of
*     their bounds are set exactly on their bounds and iterative
*     refinement is used to satisfy the constraints in the working set.
*     featol is then reduced to tolx0 for the next cycle of iterations.
*
*     Values of istate(j) for the linear constraints.......
*
*     Istate(j)
*     ---------
*          0    constraint j is not in the working set.
*          1    constraint j is in the working set at its lower bound.
*          2    constraint j is in the working set at its upper bound.
*          3    constraint j is in the working set as an equality.
*
*     Constraint j may be violated by as much as featol(j).
*
*     This version of  qpcore  dated  16-Jan-95.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/

      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      common    /sol3cm/ lennam, ldT , ncolT , ldQ
      common    /sol4cm/ epspt3, epspt5, epspt8, epspt9
      common    /sol5cm/ Asize , dTmax , dTmin

      integer            loclc
      parameter         (lenlc = 20)
      common    /sol1lc/ loclc(lenlc)
      save      /sol1lc/
      common    /sol3lc/ tolx0 , tolinc, idegen, kdegen, ndegen,
     $                   itnfix, nfix(2)
      logical            header, prnt
      common    /sol4lc/ alfa  , trulam, isdel , jdel  , jadd  ,
     $                   header, prnt

*     +Include lcparm-Sep-95++++++++++++++++++++++++++++++++++++++++++++
      parameter         (mxparm = 30)
      integer            iprmlc(mxparm), ipsvlc
      double precision   rprmlc(mxparm), rpsvlc

      common    /lcpar1/ ipsvlc(mxparm),
     $                   itmax1, itmax2, kchk  , kcycle, lcrash, lprob ,
     $                   maxact, mxfree, maxnZ , mm    , minsum, msglc ,
     $                   nn    , nnclin, nprob , ipadlc(15)

      common    /lcpar2/ rpsvlc(mxparm),
     $                   bigbnd, bigdx , bndlow, bndupp, tolact, tolfea,
     $                   tolOpt, tolrnk, rpadlc(22)

      equivalence       (iprmlc(1), itmax1), (rprmlc(1), bigbnd)

      save      /lcpar1/, /lcpar2/
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      equivalence       (msglc , msglvl)

      logical            deadpt, delreg, firstv
      logical            giveup, hitcon, hitlow
      logical            minmzr, move  , onbnd , overfl, posdef
      logical            renewr, Rset
      logical            singlr, statpt
      logical            unbndd, uncon , unitgZ
      character*6        empty
      parameter         (empty  ='      ')
      parameter         (zero   =0.0d+0, half   =0.5d+0, one   =1.0d+0)
      parameter         (mrefn  =1                                    )

*     Specify the machine-dependent parameters.

      flmax  = wmach(7)

      if (cset) then
         ngq = 2
      else
         ngq = 1
      end if

      ldR    = ldT
      iT     = 1

      lAnorm = loclc( 4)
      lAd    = loclc( 5)
      lHx    = loclc( 6)
      ld     = loclc( 7)
      lgq    = loclc( 8)
      lcq    = loclc( 9)
      lrlam  = loclc(10)

      lR     = loclc(11)
      lT     = loclc(12)
      lQ     = loclc(13)
      lwtinf = loclc(14)
      lwrk   = loclc(15)

*     Initialize.

      irefn  =   0
      jinf   =   0
      nctotl =   n + nclin
      nviol  =   0
      numinf =   0
      suminf =   zero
      condmx =   flmax

      delreg = .false.
      firstv = .false.
      overfl = .false.
      posdef = .true.
      renewr = .false.
      Rset   = .true.
      singlr = .false.
      uncon  = .false.
      unitgZ = .false.

      notOpt = 0
      Dzz    = one

      msg    = empty

**    ======================Start of main loop==========================
*+    do while (msg .eq. empty)
  100 if       (msg .eq. empty) then

         if (nZ  .gt. 0 ) then
            gZnorm = dnrm2 ( nZ, w(lgq), 1 )
         else
            gZnorm = zero
         end if

         if (nZr .eq. nZ) then
            gZrnrm = gZnorm
         else
            gZrnrm = zero
            if (nZr .gt. 0) gZrnrm = dnrm2 ( nZr, w(lgq), 1 )
         end if

         gfnorm = gZnorm
         if (nfree .gt. 0  .and.  nactiv .gt. 0)
     $      gfnorm = dnrm2 ( nfree, w(lgq), 1 )

         objsiz = one  + abs( objqp )
         wssize = zero
         if (nactiv .gt. 0) wssize = dTmax
         dinky  = epspt8*max( wssize, objsiz, gfnorm )
         tolLM  = tolOpt*max( wssize, objsiz, gfnorm )

         if ( uncon ) then
            unitgZ = gZrnrm .le. dinky
         end if

*        If the reduced gradient (Zr)'g is small and Hz is positive
*        definite,  x is a minimizer on the working set.
*        A maximum number of unconstrained steps is imposed to
*        allow for  dinky  being too large because of bad scaling.

         statpt = gZrnrm .le. dinky
         giveup = irefn  .gt. mrefn

         minmzr = statpt  .and.  posdef
         deadpt = statpt  .and.  singlr

*        ---------------------------------------------------------------
*        Print the details of this iteration.
*        ---------------------------------------------------------------
*        Define small quantities that reflect the size of x, R and
*        the constraints in the working set.

         if (prnt) then
            if (nZr .gt. 0) then
               call dcond ( nZr, w(lR), ldR+1, dRzmax, dRzmin )
               condRz = ddiv  ( dRzmax, dRzmin, overfl )
            else
               condRz = one
            end if

            if (nactiv .gt. 0) then
               condT  = ddiv  ( dTmax, dTmin, overfl )
            else
               condT  = one
            end if

            call qpprnt( prbtyp, header, Rset,
     $                   msglvl, iter,
     $                   isdel, jdel, jadd,
     $                   n, nclin, nactiv,
     $                   nfree, nZ, nZr,
     $                   ldR, ldT, istate,
     $                   alfa, condRz, condT,
     $                   Dzz, gZrnrm,
     $                   numinf, suminf, notOpt, objqp, trulam,
     $                   Ax, w(lR), w(lT), x,
     $                   w(lwrk) )
         end if

         if (minmzr  .or.  giveup) then
*           ============================================================
*           The point  x  is a constrained stationary point.
*           Compute Lagrange multipliers.
*           ============================================================
*           Define what we mean by ``non-optimal'' multipliers.

            notOpt = 0
            jdel   = 0
            zerolm = tolLM
            smllst = tolLM
            biggst = tolLM + one
            tinyst = tolLM

            call cmmul1( prbtyp, msglvl,
     $                   n     , ldA   , ldT   ,
     $                   nactiv, nfree , nZ    ,
     $                   istate, kactiv, kx    ,
     $                   zerolm, notOpt, numinf,
     $                   trusml, smllst, jsmlst, ksmlst,
     $                           tinyst, jtiny , jinf  ,
     $                   trubig, biggst, jbigst, kbigst,
     $                   A     , w(lAnorm), w(lgq),
     $                   w(lrlam), w(lT), w(lwtinf) )

            if (nZr .lt. nZ) then
               call cmmul2( msglvl, n, nZr, nZ,
     $                      zerolm, notOpt, numinf,
     $                      trusml, smllst, jsmlst,
     $                      tinyst, jtiny , w(lgq) )
            end if

            if (notOpt .eq. 0  .and.  posdef) then
               msg    = 'optiml'
               go to 100
            end if

*           ------------------------------------------------------------
*           Delete one of three types of constraint
*           (1) regular           jsmlst > 0   istate(jsmlst) = 1, 2
*           (2) temporary bound   jsmlst > 0,  istate(jsmlst) = 4
*           (3) artificial        jsmlst < 0
*           ------------------------------------------------------------
            trulam = trusml
            jdel   = jsmlst
            delreg = .false.

            if (nZr+1 .gt. maxnZ  .and.  jdel .ne. 0) then
               msg    = 'Rz2big'
               go to 100
            end if

            if (jdel .gt. 0) then

*              Regular constraint or temporary bound.
*              delreg  says that a regular constraint was deleted.
*              jdsave, issave are only defined if  delreg  is true.

               kdel         = ksmlst
               isdel        = istate(jdel)
               istate(jdel) = 0
               delreg       = isdel .ne. 4
               if (delreg) then
                  jdsave = jdel
                  issave = isdel
               end if
            end if

*           Update the factorizations.

            call rzdel ( unitQ, iT,
     $                   n, nactiv, nfree, ngq, nZ, nZr,
     $                   ldA, ldQ, ldT,
     $                   jdel, kdel, kactiv, kx,
     $                   A, w(lT), w(lgq), w(lQ),
     $                   w(lwrk), w(ld), w(lrlam) )

            renewr = .true.
            call qpcolr( singlr, posdef, renewr, unitQ,
     $                   n, nZr, nfree, ldQ, ldH, ldR,
     $                   kx, Hsize, Dzz, tolrnk,
     $                   qpHess, H, w(lR), w(lQ),
     $                   w(lwrk), w(ld), iw, leniw, w, lenw )
            irefn  =  0
            prnt   = .false.
            uncon  = .false.
         else
*           ============================================================
*           Compute a search direction.
*           ============================================================
            if (iter .ge. itmax) then
               msg = 'itnlim'
               go to 100
            end if

            prnt  = .true.
            iter  = iter  + 1

            call qpgetd( delreg, posdef, statpt, unitgZ, unitQ,
     $                   n, nclin, nfree,
     $                   ldA, ldQ, ldR, nZr,
     $                   issave, jdsave,
     $                   kx, dnorm, gzdz,
     $                   A, w(lAd), w(ld),
     $                   w(lgq), w(lR), w(lQ), w(lwrk) )

*           ------------------------------------------------------------
*           Find the constraint we bump into along  d.
*           Update  x  and  Ax  if the step  alfa  is nonZero.
*           ------------------------------------------------------------
*           cmchzr initializes  alfhit  to bigalf. If it is still
*           that value on exit,  it is regarded as infinite.

            bigalf = ddiv  ( bigdx, dnorm, overfl )

            call cmchzr( firstv, n, nclin,
     $                   istate, bigalf, bigbnd, dnorm,
     $                   hitlow, move, onbnd, unbndd,
     $                   alfhit, alfap, jadd,
     $                   w(lAnorm), w(lAd), Ax,
     $                   bl, bu, featol, featlu, w(ld), x )

*           ------------------------------------------------------------
*           If Hz is positive definite,  alfa = 1.0  will be the step
*           to the minimizer of the quadratic on the current working
*           set.  If the unit step does not violate the nearest
*           constraint by more than featol,  the constraint is not
*           added to the working set.
*           ------------------------------------------------------------
            uncon  = alfap .gt. one  .and.  posdef
            hitcon = .not. uncon

            if (hitcon) then
               alfa  = alfhit
               irefn = 0
            else
               irefn  = irefn + 1
               jadd   = 0
               alfa   = one
            end if

            if (hitcon  .and.  unbndd) then
               msg = 'unbndd'
               go to 100
            end if

*           Predict the change in the QP objective function.

            if (posdef) then
                objchg = alfa*gzdz*(one - half*alfa)
            else
                objchg = alfa*gzdz + half*alfa**2*Dzz
            end if

*           Check for a dead point or unbounded solution.

            if (objchg .ge. - epspt9*objsiz  .and.  deadpt) then
               msg = 'deadpt'
               go to 100
            end if

            if (objchg .ge.   epspt9*objsiz) then
               msg = 'resetx'
               go to 100
            end if

            call daxpy ( n, alfa, w(ld), 1, x, 1 )
            if (nclin .gt. 0)
     $         call daxpy ( nclin, alfa, w(lAd), 1, Ax, 1 )
            xnorm  = dnrm2 ( n, x, 1 )

            if (hitcon) then
*              ---------------------------------------------------------
*              Add a constraint to the working set.
*              Update the TQ factors of the working set.
*              Use  d  as temporary work space.
*              ---------------------------------------------------------
               if (bl(jadd) .eq. bu(jadd)) then
                  istate(jadd) = 3
               else if (hitlow) then
                  istate(jadd) = 1
               else
                  istate(jadd) = 2
               end if

               if (jadd .gt. n) then
                  iadd = jadd - n
               else
                  if (hitlow) then
                     x(jadd) = bl(jadd)
                  else
                     x(jadd) = bu(jadd)
                  end if

                  do 510, ifix = 1, nfree
                     if (kx(ifix) .eq. jadd) go to 520
  510             continue
  520          end if

               call rzadd ( unitQ, Rset,
     $                      inform, ifix, iadd, jadd, it,
     $                      nactiv, nZ, nfree, nZr, ngq,
     $                      n, ldA, ldQ, ldR, ldT,
     $                      kx, condmx, Dzz,
     $                      A, w(lR), w(lT), w(lgq), w(lQ),
     $                      w(lwrk), w(lrlam), w(ld) )
               nZr    = nZr - 1
               nZ     = nZ  - 1

               if (jadd .le. n) then

*                 A simple bound has been added.

                  nfree  = nfree  - 1
               else

*                 A general constraint has been added.

                  nactiv = nactiv + 1
                  kactiv(nactiv) = iadd
               end if

*              ---------------------------------------------------------
*              Check if  Hz  has become positive definite.
*              Recompute the last column of Rz if unacceptable
*              growth has occurred.
*              --------------------------------------------------------
               if (.not. posdef) then
                  call qpcolr( singlr, posdef, renewr, unitQ,
     $                         n, nZr, nfree, ldQ, ldH, ldR,
     $                         kx, Hsize, Dzz, tolrnk,
     $                         qpHess, H, w(lR), w(lQ),
     $                         w(lwrk), w(ld), iw, leniw, w, lenw )
               end if
            end if

*           Increment featol.

            call daxpy ( nctotl, tolinc, featlu, 1, featol, 1 )

            if (mod( iter, kchk ) .eq. 0) then
*              ---------------------------------------------------------
*              Check the feasibility of constraints with non-negative
*              istate  values.  If violations have occurred,  force
*              iterative refinement and a switch to phase 1.
*              ---------------------------------------------------------
               call cmfeas( n, nclin, istate,
     $                      bigbnd, nviol, jmax, errmax,
     $                      Ax, bl, bu, featol, x )

               if (nviol .gt. 0) then
                  if (msglvl .gt. 0) then
                     if (iPrint .gt. 0) write(iPrint, 2100) errmax, jmax
                     if (iSumm  .gt. 0) write(iSumm , 2100) errmax, jmax
                  end if
               end if
            end if

            if (mod( iter, idegen ) .eq. 0) then

*              Every  idegen  iterations, reset  featol  and
*              move  x  on to the working set if it is close.

               call cmdgen( 'End of cycle', msglvl,
     $                      n, nclin, nmoved, iter, numinf,
     $                      istate, bl, bu, featol, featlu, x )
               nviol = nviol + nmoved
            end if

            if (nviol .gt. 0) then
               msg    = 'resetx'
               go to 100
            end if

*           ------------------------------------------------------------
*           Compute the QP objective and transformed gradient.
*           ------------------------------------------------------------
            if (cset) then
               objqp = ddot( n, cvec, 1, x, 1 )
            else
               objqp = zero
            end if

            jthcol = 0
            call qpHess( n, ldH, jthcol, H, x, w(lHx), iw,leniw,w,lenw )
            objqp  = objqp + half*ddot( n, w(lHx), 1, x, 1 )
            call dcopy ( n, w(lHx), 1, w(lgq), 1 )
            call cmqmul( 6, n, nZ, nfree, ldQ, unitQ,
     $                   kx, w(lgq), w(lQ), w(lwrk) )
            if (cset)
     $         call daxpy ( n, one, w(lcq), 1, w(lgq), 1 )
         end if
         go to 100
*+    end while
      end if
*     ======================end of main loop============================
*
      return

 2100 format(  ' XXX  Iterative refinement.  The maximum violation is ',
     $           1p, e14.2, ' in constraint', i5 )

*     end of qpcore
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpcrsh( unitQ, qpHess,
     $                   maxnZ, n, ngq, nZr, nZ, nfree,
     $                   ldQ, ldH, ldR,
     $                   kx, Hsize, tolrnk,
     $                   gq, H, R, Q,
     $                   Hz, wrk,
     $                   iw, leniw, w, lenw )

      implicit           double precision (a-h,o-z)
      logical            unitQ
      integer            kx(n)
      integer            iw(leniw)
      double precision   gq(n,*), H(ldH,*), R(ldR,*),
     $                   Q(ldQ,*), Hz(n)
      double precision   wrk(n)
      double precision   w(lenw)
      external           qpHess

*     ==================================================================
*     qpcrsh  computes the Cholesky factor Rz of the reduced Hessian
*     Z'HZ,  given the (nfree x nZ) matrix Z.  If the reduced Hessian is
*     indefinite, the Cholesky factor of the (nZr x nZr) matrix  Hz  is
*     returned, where Hz is formed from  H  and  nZr  columns of Z.
*     Column interchanges are used in an attempt to maximize nZr.
*     These are applied to  Z  and the rows of the matrix  gq.
*
*     This version of qpcrsh dated 16-Jan-1995.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      parameter         (zero = 0.0d+0, one = 1.0d+0)

      nZr  = 0
      if (nZ .eq. 0) return

      mnZ  = min( nZ, maxnZ )

*     ------------------------------------------------------------------
*     Compute  Z'HZ  and store the upper-triangular symmetric part in
*     the first  mnZ  columns of R.
*     ------------------------------------------------------------------
      do 200, k = 1, mnZ
         call dload ( n, zero, wrk, 1 )
         if ( unitQ ) then

*           Only bounds are in the working set.  The k-th column of Z
*           is just a column of the identity matrix.

            jthcol = kx(k)
            wrk(jthcol) = one
         else

*           Expand the column of Z into an n-vector.

            do 120, i = 1, nfree
               j      = kx(i)
               wrk(j) = Q(i,k)
  120       continue
            jthcol = 0
         end if

*        Set  R(*,k)  =  top of  H*(column of Z).

         call qpHess( n, ldH, jthcol, H, wrk, Hz, iw, leniw, w, lenw )
         call cmqmul( 4, n, nZ, nfree, ldQ, unitQ, kx, Hz, Q, wrk )
         call dcopy ( mnZ, Hz, 1, R(1,k), 1 )

*        Update an estimate of the size of the reduced Hessian.

         Hsize  = max( Hsize, abs( R(k,k) ) )
  200 continue

*     ------------------------------------------------------------------
*     Form the Cholesky factorization R'R = Z'HZ as far as possible,
*     using symmetric interchanges.
*     ------------------------------------------------------------------
      dmin   = tolrnk*Hsize

      do 400, j = 1, mnZ

*        Find the maximum diagonal of the Schur complement.

         kmax = j - 1 + idamax( mnZ-j+1, R(j,j), ldR+1 )
         dmax = R(kmax,kmax)

*        See if the diagonal is big enough.

         if (dmax .le. dmin) return

*        Perform a symmetric interchange if necessary.

         if (kmax .ne. j) then
            if (unitQ) then
               k        = kx(kmax)
               kx(kmax) = kx(j)
               kx(j)    = k
            else
               call dswap ( nfree, Q(1,kmax) , 1, Q(1,j) , 1 )
            end if

            if (ngq .gt. 0)
     $      call dswap ( ngq       , gq(kmax,1)  , n  , gq(j,1)  , n   )
            call dswap ( kmax-j    , R(j+1 ,kmax), 1  , R(j,j+1 ), ldR )
            call dswap ( j         , R(1   ,j   ), 1  , R(1,kmax), 1   )
            call dswap ( mnZ-kmax+1, R(kmax,kmax), ldR, R(j,kmax), ldR )
         end if

*        Set the diagonal element of R.

         d      = sqrt( dmax )
         R(j,j) = d
         nZr    = nZr   + 1

         if (j .lt. mnZ) then

*           Set the super-diagonal elements of this row of R and update
*           the elements of the Schur complement.

            call dscal ( mnZ-j,      (one/d), R(j  ,j+1), ldR )
            call dsyr  (  'U', mnZ-j, (-one), R(j  ,j+1), ldR,
     $                                        R(j+1,j+1), ldR )
         end if
  400 continue

*     end of qpcrsh
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpdflt( n, nclin, title )

      implicit           double precision (a-h,o-z)

      character*(*)      title

*     ==================================================================
*     qpdflt loads the default values of parameters not set by the user.
*
*     30 Dec 1986: first version.
*     04 Nov 2001: Current version of  qpdflt.
*     ==================================================================
      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/

      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      common    /sol4cm/ epspt3, epspt5, epspt8, epspt9

      logical            newopt, listop
      common    /sol2lc/ newopt, listop, ncalls
      save      /sol2lc/
      common    /sol3lc/ tolx0 , tolinc, idegen, kdegen, ndegen,
     $                   itnfix, nfix(2)

*     +Include lcparm-Sep-95++++++++++++++++++++++++++++++++++++++++++++
      parameter         (mxparm = 30)
      integer            iprmlc(mxparm), ipsvlc
      double precision   rprmlc(mxparm), rpsvlc

      common    /lcpar1/ ipsvlc(mxparm),
     $                   itmax1, itmax2, kchk  , kcycle, lcrash, lprob ,
     $                   maxact, mxfree, maxnZ , mm    , minsum, msglc ,
     $                   nn    , nnclin, nprob , ipadlc(15)

      common    /lcpar2/ rpsvlc(mxparm),
     $                   bigbnd, bigdx , bndlow, bndupp, tolact, tolfea,
     $                   tolOpt, tolrnk, rpadlc(22)

      equivalence       (iprmlc(1), itmax1), (rprmlc(1), bigbnd)

      save      /lcpar1/, /lcpar2/
*     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      equivalence       (msglc , msglvl)

      character*3        NoYes(0:1)
      character*4        icrsh(0:2)
      character*7        qptype(1:10)
      parameter        ( zero   =  0.0d+0, hundrd = 100.0d+0 )
      parameter        ( rdummy = -11111.0d+0, idummy = -11111 )
      parameter        ( gigant = 1.0d+20*.99999d+0        )
      parameter        ( wrktol = 1.0d-2                   )
      data                NoYes(0),  NoYes(1)
     $                 /     ' No',     'Yes'/
      data               icrsh(0), icrsh(1), icrsh(2)
     $                 /'Cold'   ,'Warm'   ,'Hot '   /
      data               qptype(1), qptype(2)
     $                 / '     FP', '     LP'/
      data               qptype(3), qptype(4), qptype(5), qptype(6)
     $                 / '    QP1', '    QP2', '    QP3', '    QP4'/
      data               qptype(7), qptype(8), qptype(9), qptype(10)
     $                 / '       ', '       ', '       ', 'illegal'/

      epsmch = wmach( 3)

*     Make a dummy call to lpnkey to ensure that the defaults are set.

      call lpnkey( )
      newopt = .true.

*     Save the optional parameters set by the user.  The values in
*     rprmlc and iprmlc may be changed to their default values.

      call icopy ( mxparm, iprmlc, 1, ipsvlc, 1 )
      call dcopy ( mxparm, rprmlc, 1, rpsvlc, 1 )

      if (          iPrint .lt. 0     )   call mcout ( iPrint, iSumry )
      if (          iSumm  .lt. 0     )   call mcout ( iPrntr, iSumm  )
      if (          iSumm  .eq. iPrint)   iPrint  = 0
      if (          kchk   .le. 0      )  kchk    = 50
      if (          kcycle .le. 0      )  kcycle  = 5
      if (          kcycle .gt. 9999999)  kcycle  = 9999999
                                          kdegen  = kcycle
      if (          lprob  .lt. 0      )  lprob   = 4
      if (          lcrash .lt. 0
     $    .or.      lcrash .gt. 2      )  lcrash  = 0
      if (          itmax1 .lt. 0      )  itmax1  = max(50, 5*(n+nclin))
      if (          itmax2 .lt. 0      )  itmax2  = max(50, 5*(n+nclin))

      if (          maxact .lt. 0
     $    .or.      maxact .gt. n
     $    .or.      maxact .gt. nclin  )  maxact  = max(1, min(n,nclin))

      if (          mm     .lt. 0
     $    .or.      mm     .gt. n      )  mm      = n
      if (          lprob  .le. 2      )  mm      = 0

      if (          maxnZ  .lt. 0
     $    .and.     mm     .ge. 0      )  maxnZ   = mm
      if (          maxnZ  .eq. 0      )  maxnZ   = 1
      if (          maxnZ  .lt. 1
     $    .or.      maxnZ  .gt. n      )  maxnZ   = n
      if (          mxfree .lt. 1
     $    .or.      mxfree .gt. n      )  mxfree  = n
      if (          mxfree .lt. maxnZ  )  mxfree  = maxnZ
      if (          minsum .lt. 0      )  minsum  = 0
      if (          nclin  .lt. n
     $    .and.     lprob  .le. 2      ) then
                                          mxfree  = nclin  + 1
                                          maxnZ   = mxfree
                                         end if
      if (          msglvl .eq. idummy )  msglvl  = 10
      if (          tolact .lt. zero   )  tolact  = wrktol
      if (          tolfea .eq. rdummy
     $    .or.     (tolfea .ge. zero
     $    .and.     tolfea .lt. epsmch))  tolfea  = epspt5
      if (          tolOpt .eq. rdummy
     $    .or.     (tolOpt .ge. zero
     $    .and.     tolOpt .lt. epsmch))  tolOpt  = epspt5
      if (          tolrnk .le. zero   )  tolrnk  = epsmch*hundrd
      if (          bigbnd .le. zero   )  bigbnd  = gigant
      if (          bigdx  .le. zero   )  bigdx   = max(gigant, bigbnd)

      if (msglvl .gt. 0) then
*        ----------------
*        Print the title.
*        ----------------
         lenT = len( title )
         if (lenT .gt. 0) then
            nspace = (81 - lenT)/2 + 1
            if (iPrint .gt. 0) then
               write(iPrint, '(///// (80a1) )')
     $               (' ', j=1, nspace), (title(j:j), j=1,lenT)
               write(iPrint, '(80a1 //)')
     $               (' ', j=1, nspace), ('='       , j=1,lenT)
            end if

            if (iSumm .gt. 0) then
               write(iSumm , '(///// (80a1) )')
     $               (' ', j=1, nspace), (title(j:j), j=1,lenT)
               write(iSumm , '(80a1 //)')
     $               (' ', j=1, nspace), ('='       , j=1,lenT)
            end if
         end if

         if (iPrint .gt. 0) then
            write(iPrint, 2000)
            write(iPrint, 2100) qptype(lprob),
     $                          nclin , icrsh(lcrash), NoYes(minsum),
     $                          n     , bigbnd,        tolfea,
     $                          mm    , bigdx ,        tolOpt,
     $                          kchk  , kdegen,        tolact,
     $                          maxnZ , maxact,        tolrnk,
     $                          mxfree
            write(iPrint, 2200) msglvl, iPrint,        itmax1,
     $                          epsmch, iSumm ,        itmax2
         end if
      end if

      return

 2000 format(
     $//' Parameters'
     $/ ' ----------' )
 2100 format(
     $/ ' Problem type...........', 3x, a7
     $/ ' Linear constraints.....',     i10,   2x,
     $  1x, a4, ' start.............',  12x,
     $  ' Min. Sum of Infeas.....', 7x, a3
     $/ ' Variables..............',     i10,   2x,
     $  ' Infinite bound size....', 1p, e10.2, 2x,
     $  ' Feasibility tolerance..', 1p, e10.2
     $/ ' Hessian rows...........',     i10,   2x,
     $  ' Infinite step size.....', 1p, e10.2, 2x,
     $  ' Optimality tolerance...',     e10.2
     $/ ' Check frequency........',     i10,   2x,
     $  ' Expand frequency.......',     i10,   2x,
     $  ' Crash tolerance........',     e10.2
     $/ ' Max degrees of freedom.',     i10,   2x,
     $  ' Max active constraints.',     i10,   2x,
     $  ' Rank tolerance.........',     e10.2
     $/ ' Max free variables.....',     i10 )
 2200 format(
     $/ ' Print level............',     i10,   2x,
     $  ' Print file.............',     i10,   2x,
     $  ' Feasibility phase itns.',     i10
     $/ ' Unit round-off.........', 1p, e10.2, 2x,
     $  ' Summary file...........',     i10,   2x,
     $  ' Optimality  phase itns.',     i10 )

*     end of qpdflt
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpgetd( delreg, posdef, statpt, unitgZ, unitQ,
     $                   n, nclin, nfree,
     $                   ldA, ldQ, ldR, nZr,
     $                   issave, jdsave,
     $                   kx, dnorm, gzdz,
     $                   A, Ad, d,
     $                   gq, R, Q, v )

      implicit           double precision (a-h,o-z)
      logical            delreg, posdef, statpt, unitgZ, unitQ
      integer            kx(n)
      double precision   A(ldA,*), Ad(*), d(n),
     $                   gq(n), R(ldR,*), Q(ldQ,*)
      double precision   v(n)

*     ==================================================================
*     qpgetd  computes the following quantities for  qpcore.
*     (1) The search direction d (and its 2-norm).  The vector d is
*         defined as  Z*(dz), where  (dz)  is defined as follows.
*         If Hz is positive definite, (dz) is the solution of the
*         (nZr x nZr)  triangular system  (Rz)'(Rz)*(dz) = - (gz).
*         Otherwise  (dz) is the solution of the triangular system
*         (Rz)(dz) =  gamma ez,  where ez is the nZr-th unit vector and
*         gamma = -sgn(gq(nZr)).
*     (2) The vector Ad,  where A is the matrix of linear constraints.
*
*     Original version written 31-December-1986.
*     This version of  qpgetd  dated 21-Dec-1990.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      logical            dellow, revers
      parameter         (zero = 0.0d+0, one  = 1.0d+0)

      if (posdef) then
         if (unitgZ) then
            if (nZr .gt. 1)
     $         call dload ( nZr-1, zero, v, 1 )
            v(nZr) = - gq(nZr)/R(nZr,nZr)
         else
            call dcopy ( nZr, gq  , 1, v, 1 )
            call dscal ( nZr, (-one), v, 1 )
            call dtrsv ( 'U', 'T', 'N', nZr, R, ldR, v, 1 )
         end if
      else
         if (nZr .gt. 1)
     $      call dload ( nZr-1, zero, v, 1 )
         if (gq(nZr) .gt. zero) then
            v(nZr) = - one
         else
            v(nZr) =   one
         end if
      end if

*     Solve  (Rz)*(dz) =  v.

      call dcopy ( nZr, v, 1, d, 1 )
      call dtrsv ( 'U', 'n', 'n', nZr, R, ldR, d, 1 )

*     Compute  d = Zr*(dz)  and its norm.  Find  gz'dz

      dnorm  = dnrm2 ( nZr, d, 1 )
      gzdz   = ddot  ( nZr, d, 1, gq, 1 )

      call cmqmul( 1, n, nZr, nfree, ldQ, unitQ, kx, d, Q, v )

*     Compute  Ad.

      if (nclin .gt. 0)
     $   call dgemv ( 'No transpose', nclin, n, one, A, ldA,
     $                d, 1, zero, Ad, 1 )

      if (delreg  .and.  (gzdz .gt. zero  .or.  statpt)) then
*        ---------------------------------------------------------------
*        The reduced-gradient norm is small enough that we need to worry
*        about the sign of d.  Make  d  point away from the last deleted
*        constraint.
*        ---------------------------------------------------------------
*        Jdsave  is the index of the last deleted regular constraint.

         if (jdsave .le. n) then
            atd =  d(jdsave)
         else
            atd = Ad(jdsave-n)
         end if

         dellow = issave .eq. 1
         if (dellow) then
            revers = atd .lt. zero
         else
            revers = atd .gt. zero
         end if

         if (revers) then
            call dscal ( n, (-one), d, 1 )
            if (nclin .gt. 0)
     $         call dscal ( nclin, (-one), Ad, 1 )
            gzdz = - gzdz
         end if
      end if

*     end of qpgetd
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpHess( n, ldH, jthcol, H, x, Hx, iw, leniw, w, lenw )

      implicit           double precision (a-h,o-z)
      integer            iw(leniw)
      double precision   H(ldH,*), Hx(n), x(n)
      double precision   w(lenw)

*     ==================================================================
*     qpHess  is used to compute the product Hx, where H is the QP
*     Hessian matrix stored in H and x is an n-vector.
*
*     If lqptyp is 3 ('QP1') or 4 ('QP2'), the upper-half of the
*     two-dimensional array  H  contains the elements of the
*     symmetric Hessian matrix H.
*     The value of  mHess  defines the dimension of the mHess x mHess
*     leading principal minor of H.
*     The value of mHess is input as the option 'Hessian Rows' with
*     default value n.  The sub-diagonal elements of H are not
*     referenced.
*
*     If lqptyp is 5 ('QP3') or 6 ('QP4'), the Hessian is of the form
*     H = R'R, where  R  is an mHess x n upper-trapezoidal matrix.  The
*     factor R is stored in the first mHess rows of the upper half of
*     the two-dimensional array H.  The sub-diagonal elements of  R  are
*     not referenced.
*
*     Original F66 Version written    March-1982.
*     This version of qpHess dated 16-Jan-1995.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================

      common    /sol1qp/ lqptyp, mHess
      parameter        ( zero = 0.0d+0, one = 1.0d+0 )

      if (lqptyp .eq. 3  .or.  lqptyp .eq. 4) then

*        Problem type QP1 and QP2.

         if (jthcol .gt. 0) then
*           ------------------------------------------------------------
*           Special case -- extract one column of H.
*           ------------------------------------------------------------
            if (jthcol .gt. mHess) then
               call dload ( mHess, zero, Hx, 1 )
            else
               call dcopy ( jthcol, H(1,jthcol), 1, Hx, 1 )
               num    = mHess  - jthcol
               jp1    = jthcol + 1
               if (num .gt. 0)
     $            call dcopy ( num, H(jthcol,jp1), ldH, Hx(jp1), 1)
            end if
         else
*           ------------------------------------------------------------
*           Normal case.
*           ------------------------------------------------------------
            call dsymv ( 'upper-triangular', mHess, one, H, ldH, x, 1,
     $                   zero, Hx, 1 )
         end if

         if (n .gt. mHess) call dload ( n-mHess, zero, Hx(mHess+1), 1 )

      else if (lqptyp .eq. 5  .or.  lqptyp .eq. 6) then

*        Problem type qp3 and qp4.

         if (mHess .eq. 0) then
            call dload ( n, zero, Hx, 1 )

         else if (jthcol .gt. 0) then
*           ------------------------------------------------------------
*           Special case -- extract one column of H.
*           ------------------------------------------------------------
            lenrx = min( jthcol, mHess )
            call dcopy ( lenrx, H(1,jthcol), 1, Hx, 1 )
         else
*           ------------------------------------------------------------
*           Normal case.
*           ------------------------------------------------------------
            call dcopy ( mHess, x, 1, Hx, 1 )
            call dtrmv ( 'U', 'N', 'N', mHess, H, ldH, Hx, 1 )
            if (n .gt. mHess)
     $         call dgemv ( 'N', mHess, n-mHess, one, H(1,mHess+1), ldH,
     $                      x(mHess+1), 1, one, Hx, 1 )
            lenrx = mHess
         end if

         if (n .gt. lenrx)
     $      call dgemv ( 'T', lenrx, n-lenrx, one, H(1,lenrx+1), ldH,
     $                   Hx, 1, zero, Hx(lenrx+1), 1 )
         call dtrmv ( 'U', 'T', 'N', lenrx, H, ldH, Hx, 1 )

      end if

*     end of qpHess
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qploc ( cset, n, nclin, litotl, lwtotl )

      implicit           double precision (a-h,o-z)
      logical            cset

*     ==================================================================
*     qploc   allocates the addresses of the work arrays for qpcore.
*
*     Note that the arrays ( gq, cq ) lie in contiguous areas of
*     workspace.
*
*     Original version written  2-January-1987.
*     This version of qploc dated  18-Nov-1990.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      common    /sol3cm/ lennam, ldT, ncolT, ldQ

      parameter        ( lenlc = 20 )
      common    /sol1lc/ loclc(lenlc)
      save      /sol1lc/

*     ------------------------------------------------------------------
*     Refer to the first free space in the work arrays.
*     ------------------------------------------------------------------
      miniw     = litotl + 1
      minw      = lwtotl + 1

*     ------------------------------------------------------------------
*     Integer workspace.
*     ------------------------------------------------------------------
      lkactv    = miniw
      lkx       = lkactv + n
      miniw     = lkx    + n

*     ------------------------------------------------------------------
*     Real workspace.
*     Assign array lengths that depend upon the problem dimensions.
*     ------------------------------------------------------------------
      lenRT     = ldT *ncolT
      if (nclin .eq. 0) then
         lenQ  = 0
      else
         lenQ  = ldQ*ldQ
      end if

      if (cset) then
         lencq  = n
      else
         lencq  = 0
      end if

*     ------------------------------------------------------------------
*     We start with arrays that can be preloaded by smart users.
*     ------------------------------------------------------------------
      lfeatu    = minw
      minw      = lfeatu + nclin + n

*     Next comes stuff used by  lpcore  and  qpcore.

      lAnorm    = minw
      lAd       = lAnorm + nclin
      lHx       = lAd    + nclin
      ld        = lHx    + n
      lgq       = ld     + n
      lcq       = lgq    + n
      lrlam     = lcq    + lencq
      lR        = lrlam  + n
      lT        = lR
      lQ        = lT     + lenRT
      lwtinf    = lQ     + lenQ
      lwrk      = lwtinf + n  + nclin
      minw      = lwrk   + n  + nclin

*     Load the addresses in loclc.

      loclc( 1) = lkactv
      loclc( 2) = lkx

      loclc( 3) = lfeatu
      loclc( 4) = lAnorm
      loclc( 5) = lAd
      loclc( 6) = lHx
      loclc( 7) = ld
      loclc( 8) = lgq
      loclc( 9) = lcq
      loclc(10) = lrlam
      loclc(11) = lR
      loclc(12) = lT
      loclc(13) = lQ
      loclc(14) = lwtinf
      loclc(15) = lwrk

      litotl    = miniw - 1
      lwtotl    = minw  - 1

*     end of qploc
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpprm ( string )
      character*(*)      string

*     ==================================================================
*     qpprm   loads the option supplied in  string  into the relevant
*     element of  iprmlc  or  rprmlc.
*     ==================================================================
      call lpprm ( string )

      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpprmi( string, ivalue )

      implicit           double precision (a-h,o-z)
      character*(*)      string
      integer            ivalue

*     ==================================================================
*     qpprmi decodes the option contained in  string // ivalue.
*
*     14 Sep 1995: first version.
*     ==================================================================
      character*16       key
      character*72       buff72

      write(key, '(i16)') ivalue
      lenbuf = len(string)
      buff72 = string
      buff72(lenbuf+1:lenbuf+16) = key
      call lpprm ( buff72 )

*     end of qpprmi
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpprmr( string, rvalue )

      implicit           double precision (a-h,o-z)
      character*(*)      string
      double precision   rvalue

*     ==================================================================
*     qpprmr decodes the option contained in  string // rvalue.
*
*     14 Sep 1995: first version.
*     ==================================================================
      character*16       key
      character*72       buff72

      write(key, '(1p, e16.8)') rvalue
      lenbuf = len(string)
      buff72 = string
      buff72(lenbuf+1:lenbuf+16) = key
      call lpprm ( buff72 )

*     end of qpprmr
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpprms( ioptns, inform )
      integer            ioptns, inform

*     ==================================================================
*     qpprms  just calls lpprms, which reads the options file from unit
*     ioptns  and loads the options into the relevant elements of
*     iprmlc  and  rprmlc.
*     ==================================================================

      call lpprms( ioptns, inform )

*     end of qpprms
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine qpprnt( prbtyp, header, Rset,
     $                   msglvl, iter,
     $                   isdel, jdel, jadd,
     $                   n, nclin, nactiv,
     $                   nfree, nZ, nZr,
     $                   ldR, ldT, istate,
     $                   alfa, condRz, condT,
     $                   Dzz, gZrnrm,
     $                   numinf, suminf, notOpt, objqp, trusml,
     $                   Ax, R, T, x, work )

      implicit           double precision (a-h,o-z)
      character*2        prbtyp
      logical            header, Rset
      integer            istate(*)
      double precision   Ax(*), R(ldR,*), T(ldT,*), x(n)
      double precision   work(n)

*     ==================================================================
*     qpprnt prints various levels of output for qpcore.
*
*           msg        cumulative result
*           ---        -----------------
*
*       .le.  0        no output.
*
*       .eq.  1        nothing now (but full output later).
*
*       .eq.  5        one terse line of output.
*
*       .ge. 10        same as 5 (but full output later).
*
*       .ge. 20        constraint status,  x  and  Ax.
*
*       .ge. 30        diagonals of  T  and  R.
*
*
*     Original version of qpprnt written by PEG, 31-October-1984.
*     This version of  qpprnt  dated  23-Dec-92.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      logical            newSet, prtHdr
      parameter         (mLine1 = 40    , mLine2 = 5     )
      parameter         (zero   = 0.0d+0, one    = 1.0d+0)

      character*2        ladd, ldel
      character*9        lcondR, lDzz
      character*15       lmchar
      character*2        lstate(0:5)

      data               lstate(0), lstate(1), lstate(2)
     $                  /'  '     , 'L '     , 'U '     /
      data               lstate(3), lstate(4), lstate(5)
     $                  /'E '     , 'F '     , 'A '     /

      if (msglvl .ge. 15  .and.  iPrint .gt. 0)
     $   write(iPrint, 1000) prbtyp, iter

      if (msglvl .ge. 5) then
*        ---------------------------------------------------------------
*        Some printing required.  Set up information for the terse line.
*        ---------------------------------------------------------------
         Itn = mod( iter, 1000  )
         ndf = mod( nZr , 10000 )

         if (jdel .ne. 0) then
            if (notOpt .gt. 0) then
               write( lmchar, '( i5, 1p,e10.2 )' ) notOpt, trusml
            else
               write( lmchar, '( 5x, 1p,e10.2 )' )         trusml
            end if

            if      (jdel .gt. 0) then
               kdel =   isdel

            else if (jdel .lt. 0) then
               jdel = nZ - nZr + 1
               kdel = 5
            end if
         else
            jdel = 0
            kdel = 0
            lmchar = '               '
         end if

         lDzz   = '         '
         lcondR = '         '
         if (Rset  .and.  nZr .gt. 0) then
            write( lcondR, '( 1p,e9.1 )' ) condRz
            if (Dzz .ne. one)
     $         write( lDzz, '( 1p,e9.1 )' ) Dzz
         end if

         if (jadd .gt. 0) then
            kadd = istate(jadd)
         else
            kadd = 0
         end if

         ldel   = lstate(kdel)
         ladd   = lstate(kadd)

         if (numinf .gt. 0) then
            obj = suminf
         else
            obj = objqp
         end if

*        ---------------------------------------------------------------
*        If necessary, print a header.
*        Print a single line of information.
*        ---------------------------------------------------------------
         if (iPrint .gt. 0) then
*           ------------------------------
*           Terse line for the Print file.
*           ------------------------------
            newSet = lines1 .ge. mLine1
            prtHdr = msglvl .ge. 15  .or.  header
     $                               .or.  newSet

            if (prtHdr) then
               if (prbtyp .eq. 'QP') then
                  write(iPrint, 1300)
               else
                  write(iPrint, 1200)
               end if
               lines1 = 0
            end if

            write(iPrint, 1700) Itn, jdel, ldel, jadd, ladd,
     $                          alfa, numinf, obj,
     $                          gZrnrm, ndf, nZ-nZr,
     $                          n-nfree, nactiv, lmchar, condT,
     $                          lcondR, lDzz
            lines1 = lines1 + 1
         end if

         if (iSumm .gt. 0) then
*           --------------------------------
*           Terse line for the Summary file.
*           --------------------------------
            newSet = lines2 .ge. mLine2
            prtHdr =                      header
     $                              .or.  newSet
            if (prtHdr) then
               write(iSumm , 1100)
               lines2 = 0
            end if
            write(iSumm , 1700) Itn, jdel, ldel, jadd, ladd,
     $                          alfa, numinf, obj,
     $                          gZrnrm, ndf, nZ-nZr
            lines2 = lines2 + 1
         end if

         if (msglvl .ge. 20  .and.  iPrint .gt. 0) then
            write(iPrint, 2000) prbtyp
            write(iPrint, 2100) (x(j) , istate(j)  ,  j=1,n)
            if (nclin .gt. 0)
     $      write(iPrint, 2200) (Ax(k), istate(n+k), k=1,nclin )

            if (msglvl .ge. 30) then
*              ---------------------------------------------------------
*              Print the diagonals of  T  and  R.
*              ---------------------------------------------------------
               if (nactiv .gt. 0) then
                  call dcopy ( nactiv, T(1,nZ+1), ldT+1, work, 1 )
                  write(iPrint, 3000) prbtyp, (work(j), j=1,nactiv)
               end if
               if (Rset  .and.  nZr .gt. 0)
     $            write(iPrint, 3100) prbtyp, (R(j,j) , j=1,nZr )
            end if
            write(iPrint, 5000)
         end if
      end if

      header = .false.
      jdel  = 0
      jadd  = 0
      alfa  = zero

      return

 1000 format(///' ', a2, ' iteration', i5
     $         /' =================' )
 1100 format(// ' Itn Jdel  Jadd     Step Ninf  Sinf/Objective',
     $          ' Norm gZ   Zr  Art' )
 1200 format(// ' Itn Jdel  Jadd     Step Ninf  Sinf/Objective',
     $          ' Norm gZ   Zr  Art  Bnd  Lin NOpt    Min Lm  Cond T' )
 1300 format(// ' Itn Jdel  Jadd     Step Ninf  Sinf/Objective',
     $          ' Norm gZ   Zr  Art  Bnd  Lin NOpt    Min Lm  Cond T',
     $          '  Cond Rz     Rzz' )
 1700 format(    i4, i5, a1, i5, a1, 1p, e8.1, i5, e16.8,
     $           e8.1, 2i5, 2i5, a15, e8.0, 2a9 )
 2000 format(/ ' Values and status of the ', a2, ' constraints'
     $       / ' ---------------------------------------' )
 2100 format(/ ' Variables...'                 /   (1x,5(1p,e15.6, i5)))
 2200 format(/ ' General linear constraints...'/   (1x,5(1p,e15.6, i5)))
 3000 format(/ ' Diagonals of ' , a2,' working set factor T'
     $       /(1p, 5e15.6))
 3100 format(/ ' Diagonals of ' , a2, ' triangle Rz        '
     $       /(1p, 5e15.6))
 5000 format(/// ' ---------------------------------------------------',
     $           '--------------------------------------------' )

*     end of qpprnt
      end
