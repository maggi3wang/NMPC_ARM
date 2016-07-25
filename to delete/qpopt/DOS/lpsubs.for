*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*                                                                      *
*    L P O P T         Version 1.0-10(2)       November 4, 2001        *
*                                                                      *
*    Philip E. Gill    Walter  Murray          Michael A. Saunders     *
*    UC San Diego      Stanford University     Stanford University     *
*                                                                      *
*----------------------------------------------------------------------*
*     (C) 1992--2001  Regents of the University of California          *
*                     and the Trustees of Stanford University          *
*                                                                      *
*     This software is NOT in the public domain. Its use is governed   *
*     by a license agreement with either Stanford University or the    *
*     University of California.  It is a breach of copyright to make   *
*     copies except as authorized by the license agreement.            *
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     File  lpoptsubs.f
*
*     lpopt    lpcolr   lpcore   lpdflt   lpkey    lploc    lpnkey
*     lpprm    lpprmi   lpprmr   lpprms   lpprnt
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpopt ( n, nclin, ldA,
     $                   A, bl, bu,
     $                   x, Ax, inform,
     $                   iter, istate,
     $                   clamda, obj, cvec,
     $                   iw, leniw, w, lenw  )

      implicit           double precision (a-h,o-z)
      integer            leniw, lenw
      integer            istate(n+nclin)
      integer            iw(leniw)
      double precision   A(ldA,*), bl(n+nclin), bu(n+nclin)
      double precision   clamda(n+nclin), x(n), Ax(*)
      double precision   cvec(*), w(lenw)
      external           lpprnt

*     ==================================================================
*     lpopt  solves the linear programming problem
*
*           minimize               c' x
*              x
*                                 (  x )
*           subject to    bl  .le.(    ).ge.  bu,
*                                 ( Ax )
*
*     where  A  is a constant  nclin by n  matrix.
*     The feasible region is defined by a mixture of linear equality or
*     inequality constraints on  x.
*
*     n  is the number of varibles (dimension of x).
*        (n must be positive.)
*
*     nclin  is the number of general linear constraints (rows of  A).
*        (nclin may be zero.)
*
*     The first  n  components of  bl  and   bu  are lower and upper
*     bounds on the variables.  The next  nclin  components are
*     lower and upper bounds on the general linear constraints.
*
*     The matrix  A  of coefficients in the general linear constraints
*     is entered as the two-dimensional array  A  (of dimension
*     ldA by n).  If nclin = 0,  A is not referenced.
*
*     The vector  x  must contain an initial estimate of the solution,
*     and will contain the computed solution on output.
*
*     Documentation for  LPOPT  is coming Real Soon Now.
*     Wait for the release of  users guide for LPOPT (Version 1.00),
*     by  P. E. Gill, W. Murray and M. A. Saunders,
*
*     Version 1.0-6  Jun 30, 1991.  (Nag Mk 16 version.)
*     Version 1.0-7  Mar 21, 1993.  Summary file added.
*     Version 1.0-8  Apr 10, 1994.  Sum of infeas. added as an option.
*     Version 1.0-9  Jul 15, 1994.  Debug printing removed.
*     Version 1.0-10 Sep  9, 1995.  New document.
*
*     This version of  LPOPT  dated 04 November 2001.
*     (C) 1992--2001  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/

      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      common    /sol3cm/ lennam, ldT   , ncolT , ldQ
      common    /sol4cm/ epspt3, epspt5, epspt8, epspt9
      common    /sol5cm/ Asize , dTmax , dTmin

      parameter         (lenlc = 20)
      common    /sol1lc/ loclc(lenlc)
      save      /sol1lc/
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

*     local variables.

      logical            cold  , cset  , done  , found , halted, hot
      logical            named , rowerr, Rset  , unitQ , vertex, warm
      character*6        msg
      character*2        prbtyp
      character*4        start
      character*16       names(1)
      parameter         (zero   =0.0d+0, point3 =3.3d-1, point8 =0.8d+0)
      parameter         (point9 =0.9d+0, one    =1.0d+0                )
      parameter         (hundrd =1.0d+2                                )

      character*40       title
      data               title
     $                 / 'LPOPT  ---  Version 1.0-10(2)  Nov  2001' /

*     Set the machine-dependent constants.

      call mchpar()

      epsmch = wmach( 3)
      rteps  = wmach( 4)

      epspt3 = epsmch**point3
      epspt5 = rteps
      epspt8 = epsmch**point8
      epspt9 = epsmch**point9

      named  = .false.

      inform = 0
      iter   = 0

      header = .true.
      prnt   = .true.

C-->  condmx = max( one/epspt5, hundrd )
C-->  condmx = max( one/epspt3, hundrd )
      condmx = max( one/epspt5, hundrd )

*     Set the default values of the parameters.

      call lpdflt( n     , nclin , title )

      llptyp = lprob
      nctotl = n   + nclin

*     Set all parameters determined by the problem type.

      if      (llptyp .eq. 1 ) then
         prbtyp = 'FP'
         cset   = .false.
      else if (llptyp .eq. 2 ) then
         prbtyp = 'LP'
         cset   = .true.
      else
         prbtyp = 'illegal'
         msg = 'noprob'
         go to 800
      end if

*     Assign the dimensions of arrays in the parameter list of lpcore.
*     economies of storage are possible if the minimum number of active
*     constraints and the minimum number of fixed variables are known in
*     advance.  The expert user should alter minact and minfxd
*     accordingly.
*     If a linear program is being solved and the matrix of general
*     constraints has fewer rows than columns, i.e.,  nclin .lt. n,  a
*     non-zero value is
*     known for minfxd.  Note that in this case, vertex must be
*     set  .true..

      vertex = nclin .lt. n

      minfxd = n      - mxfree
      minact = mxfree - maxnZ

      ldT    = max( maxnZ, maxact )
      ncolT  = mxfree
      if (nclin .eq. 0) then
         ldQ = 1
      else
         ldQ = max( 1, mxfree )
      end if

      ncnln  = 0
      lennam = 1

*     ==================================================================
*     Cold start:  Only  x  is provided.
*     Warm start:  Initial working set is specified in  istate.
*     Hot  start:  The work arrays  iw  and  w  are assumed to have been
*                  initialized during a previous run.
*                  The first three components of  iw  contain details
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
      call lploc ( cset, n, nclin, litotl, lwtotl )

      lkactv = loclc( 1)
      lkx    = loclc( 2)

      lfeatu = loclc( 3)
      lAnorm = loclc( 4)
      ld     = loclc( 7)
      lgq    = loclc( 8)
      lcq    = loclc( 9)
      lrlam  = loclc(10)
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
         msg = 'errors'
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
      itmax  =  itmax2
      jinf   =  0

C+    Take your pick when minimizing the sum of infeasibilities:
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
     $                xnorm , A, Ax,
     $                bl, bu, w(lfeatu),
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
     $                lpprnt,
     $                obj, numinf, xnorm,
     $                A, Ax, bl, bu,
     $                cvec, clamda, w(lfeatu),
     $                x, iw, w )

         found  = msg .eq. 'feasbl'  .or.
     $            msg .eq. 'optiml'  .or.
     $            msg .eq. 'weak  '  .or.
     $            msg .eq. 'unbndd'  .or.
     $            msg .eq. 'infeas'
         halted = msg .eq. 'itnlim'

         if (found) then
            call cmdgen( 'optimal', msglvl,
     $                   n, nclin, nmoved, iter, numinf,
     $                   istate, bl, bu, clamda, w(lfeatu), x )
         end if

         done   = found  .and.  nviol .eq. 0  .and.  nmoved .eq. 0

*     until      done  .or.  halted
      if (.not. (done  .or.  halted)) go to 300
*     ===========================================================
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
      if      (msg .eq. 'optiml') then
         inform = 0
         if (prnt) write(iPrint, 2002) prbtyp
      else if (msg .eq. 'feasbl') then
         inform = 0
         if (prnt) write(iPrint, 2001)
      else if (msg .eq. 'weak  ') then
         inform = 1
         if (prnt) write(iPrint, 2010) prbtyp
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
            if (iPrint .gt. 0) write(iPrint, 2000) prbtyp, inform, iter
            if (iSumm  .gt. 0) write(iSumm , 2000) prbtyp, inform, iter
         end if
      end if

      call icopy ( mxparm, ipsvlc, 1, iprmlc, 1 )
      call dcopy ( mxparm, rpsvlc, 1, rprmlc, 1 )

      return

 2000 format(/ ' Exit  ', a2, ' phase.   Inform = ', i2,
     $         '   iters = ', i5 )
 2001 format(/ ' Exit LPOPT - Feasible point found.     ')
 2002 format(/ ' Exit LPOPT - Optimal ', a2, ' solution.')
 2010 format(/ ' Exit LPOPT - Weak ',    a2, ' solution.')
 2020 format(/ ' Exit LPOPT - ', a2,         ' solution is unbounded.')
 2030 format(/ ' Exit LPOPT - No feasible point for the linear',
     $         ' constraints.')
 2035 format(/ ' Exit LPOPT - Cannot satisfy the constraints to the',
     $         ' accuracy requested.')
 2040 format(/ ' Exit LPOPT - Too many iterations.')
 2060 format(/ ' Exit LPOPT - ', i10, ' errors found in the input',
     $         ' parameters.  Problem abandoned.'         )
 2070 format(  ' Exit LPOPT - Problem type not recognized.',
     $         ' Problem abandoned.'         )

 3000 format(/ ' Final ', a2, ' objective value =', g16.7 )
 3010 format(/ ' Sum of infeasibilities =',         g16.7 )
 3011 format(/ ' Minimum sum of infeasibilities =', g16.7 )
 3015 format(/ ' Maximum row error =',              g16.7 )
 3020 format(/ ' Final sum of infeasibilities =',   g16.7 )

*     end of lpopt
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpcolr( nZr, ldR, R, Rzz )

      implicit           double precision (a-h,o-z)
      double precision   R(ldR,*)

*     ==================================================================
*     lpcolr  loads the last column of the  nZr x nZr  triangular factor
*     Rz  with the multiple  Rzz  of the  nZr-th unit vector.
*
*     Original version written by PEG,  23-Jul-87.
*     This version of  lpcolr  dated 17-Jul-90.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      parameter        ( zero= 0.0d+0 )

      if (nZr .eq. 0) return

      call dload ( nZr-1, zero, R(1,nZr), 1 )
      R(nZr,nZr) = Rzz

*     end of lpcolr
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpcore( prbtyp, msg,
     $                   cset, named, names,
     $                   Rset, unitQ,
     $                   iter, itmax, jinf, nviol,
     $                   n, nclin, ldA,
     $                   nactiv, nfree, nZr, nZ,
     $                   istate, kactiv, kx,
     $                   lpprnt,
     $                   obj, numinf, xnorm,
     $                   A, Ax, bl, bu,
     $                   cvec, featol, featlu,
     $                   x, iw, w)

      implicit           double precision (a-h,o-z)
      character*2        prbtyp
      character*6        msg
      character*16       names(*)
      logical            cset, named, unitQ, Rset
      integer            istate(n+nclin), kactiv(n), kx(n)
      integer            iw(*)
      double precision   A(ldA,*), Ax(*), bl(n+nclin), bu(n+nclin),
     $                   cvec(*), featol(n+nclin), featlu(n+nclin), x(n)
      double precision   w(*)
      external           lpprnt

*     ==================================================================
*     lpcore  is a subroutine for linear programming.
*     On entry, it is assumed that an initial working set of
*     linear constraints and bounds is available.  The arrays  istate,
*     kactiv  and  kx  will have been set accordingly
*     and the arrays  T  and  Q  will contain the TQ factorization of
*     the matrix whose rows are the gradients of the active linear
*     constraints with the columns corresponding to the active bounds
*     removed.  The TQ factorization of the resulting (nactiv by nfree)
*     matrix is  A(free)*Q = (0 T),  where Q is (nfree by nfree) and T
*     is upper-triangular.
*
*     kactiv holds the general constraint indices in the order in which
*     they were added.  The reverse ordering is used for T since new
*     rows are added at the front of T.
*
*     Over a cycle of iterations, the feasibility tolerance featol
*     increases slightly (from tolx0 to tolx1 in steps of tolinc).
*     this ensures that all steps taken will be positive.
*
*     After idegen consecutive iterations, variables within featol of
*     their bounds are set exactly on their bounds and iterative
*     refinement is used to satisfy the constraints in the working set.
*     Featol is then reduced to tolx0 for the next cycle of iterations.
*
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
*     This version of  lpcore  dated  04-Jan-96.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================

      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/

      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      common    /sol3cm/ lennam, ldT   , ncolT , ldQ
      common    /sol4cm/ epspt3, epspt5, epspt8, epspt9
      common    /sol5cm/ Asize , dTmax , dTmin

      integer            loclc
      parameter        ( lenlc = 20 )
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

      logical            firstv
      save               firstv
      logical            lp    , fp
      logical            hitlow
      logical            move  , onbnd
      logical            overfl, unbndd
      character*6        empty
      parameter         (empty  ='      ')
      parameter         (zero   =0.0d+0, one    =1.0d+0                )

*     Specify the machine-dependent parameters.

      flmax  = wmach(7)

      if (cset) then
         ngq = 2
      else
         ngq = 1
      end if

      lp     = prbtyp .eq. 'lp'  .or.  prbtyp .eq. 'LP'
      fp     = .not. lp

      ldR    = ldT
      iT     = 1

      lAnorm = loclc( 4)
      lAd    = loclc( 5)

      ld     = loclc( 7)
      lgq    = loclc( 8)
      lcq    = loclc( 9)
      lrlam  = loclc(10)

      lR     = loclc(11)
      lT     = loclc(12)
      lQ     = loclc(13)
      lwtinf = loclc(14)
      lwrk   = loclc(15)

      if (iter .eq. 0) then
*        -------------------------
*        First entry.  Initialize.
*        -------------------------
         jadd    =  0
         jdel    =  0
         isdel   =  0
         firstv  = .false.

         alfa    =  zero
         Dzz     =  one
      end if

      nctotl  =   n  + nclin
      nviol   =   0

      condmx  =  flmax

      call cmsinf( n, nclin, ldA,
     $             istate, bigbnd, numinf, suminf,
     $             bl, bu, A, featol,
     $             w(lgq), x, w(lwtinf) )

      if (numinf .gt. 0) then
         call cmqmul( 6, n, nZ, nfree, ldQ, unitQ,
     $                kx, w(lgq), w(lQ), w(lwrk) )
      else if (lp) then
         call dcopy ( n, w(lcq), 1, w(lgq), 1 )
      end if

      if (numinf .eq. 0  .and. lp) then
         obj = ddot  ( n, cvec, 1, x, 1 )
      else
         obj = suminf
      end if

      msg    = empty

**    ======================Start of main loop==========================
*+    do while (msg .eq. empty)
  100 if       (msg .eq. empty) then

         gznorm = zero
         if (nZ  .gt. 0 ) gznorm = dnrm2 ( nZ, w(lgq), 1 )

         if (nZr .eq. nZ) then
            gZrnrm = gznorm
         else
            gZrnrm = zero
            if (nZr .gt. 0) gZrnrm = dnrm2 ( nZr, w(lgq), 1 )
         end if

         gfnorm = gznorm
         if (nfree .gt. 0  .and.  nactiv .gt. 0)
     $      gfnorm = dnrm2 ( nfree, w(lgq), 1 )

*        ---------------------------------------------------------------
*        Print the details of this iteration.
*        ---------------------------------------------------------------
*        Define small quantities that reflect the size of x, R and
*        the constraints in the working set.

         if ( prnt ) then
            condT  = one
            if (nactiv .gt. 0)
     $         condT  = ddiv  ( dTmax, dTmin, overfl )

            call lpprnt( prbtyp, header, Rset,
     $                   msglvl, iter,
     $                   isdel, jdel, jadd,
     $                   n, nclin, nactiv,
     $                   nfree, nZ, nZr,
     $                   ldR, ldT, istate,
     $                   alfa, condRz, condT,
     $                   Dzz, gznorm,
     $                   numinf, suminf, notOpt, obj, trulam,
     $                   Ax, w(lR), w(lT), x,
     $                   w(lwrk) )
            jdel  = 0
            jadd  = 0
            alfa  = zero
         end if

         if (numinf .gt. 0) then
            dinky  = zero
            tolLM  = zero
         else
            objsiz = one  + abs( obj    )
            wssize = zero
            if (nactiv .gt. 0) wssize = dTmax
            dinky  = epspt8*max( wssize, objsiz, gfnorm )
            tolLM  = tolOpt*max( wssize, objsiz, gfnorm )
         end if

*        If the reduced gradient Z'g is small enough,
*        Lagrange multipliers will be computed.

         if (numinf .eq. 0  .and.  fp) then
            msg    = 'feasbl'
            nfixed = n - nfree
            call dload ( nactiv+nfixed, zero, w(lrlam), 1 )
            go to 100
         end if

         if (gZrnrm .le. dinky) then
*           ============================================================
*           The point  x  is a constrained stationary point.
*           Compute Lagrange multipliers.
*           ============================================================
*           Define what we mean by 'tiny' and non-optimal multipliers.

            notOpt =   0
            jdel   =   0
            zerolm = - tolLM
            smllst = - tolLM
            biggst =   tolLM + one
            tinyst =   tolLM

            call cmmul1( prbtyp, msglvl,
     $                   n, ldA, ldT,
     $                   nactiv, nfree, nZ,
     $                   istate, kactiv, kx,
     $                   zerolm, notOpt, numinf,
     $                   trusml, smllst, jsmlst, ksmlst,
     $                   tinyst, jtiny, jinf,
     $                   trubig, biggst, jbigst, kbigst,
     $                   A, w(lAnorm), w(lgq),
     $                   w(lrlam), w(lT), w(lwtinf) )

            if (nZr .lt. nZ) then
               call cmmul2( msglvl, n, nZr, nZ,
     $                      zerolm, notOpt, numinf,
     $                      trusml, smllst, jsmlst,
     $                      tinyst, jtiny, w(lgq) )
            end if

            if (abs(jsmlst) .gt. 0) then
*              ---------------------------------------------------------
*              Delete a constraint.
*              ---------------------------------------------------------
*              cmmul1  or  cmmul2  found a non-optimal multiplier.

               trulam = trusml
               jdel   = jsmlst

               if (jsmlst .gt. 0) then

*                 Regular constraint.

                  kdel   = ksmlst
                  isdel  = istate(jdel)
                  istate(jdel) = 0
               end if
            else if (minsum .gt. 0) then
               if (numinf .gt. 0  .and.  jbigst .gt. 0) then

*                 No feasible point exists for the constraints but the
*                 sum of the constraint violations can be reduced by
*                 moving off constraints with multipliers greater than 1.

                  jdel   = jbigst
                  kdel   = kbigst
                  isdel  = istate(jdel)
                  if (trubig .le. zero) is = - 1
                  if (trubig .gt. zero) is = - 2
                  istate(jdel) = is
                  trulam = trubig
                  firstv = .true.
                  numinf = numinf + 1
               end if
            end if

            if (jdel .eq. 0) then
               if (numinf .gt. 0) then
                  msg = 'infeas'
               else
                  msg = 'optiml'
               end if
               go to 100
            end if

*           Constraint  jdel  has been deleted.
*           Update the  TQ  factorization.

            call rzdel ( unitQ, iT,
     $                   n, nactiv, nfree, ngq, nZ, nZr,
     $                   ldA, ldQ, ldT,
     $                   jdel, kdel, kactiv, kx,
     $                   A, w(lT), w(lgq), w(lQ),
     $                   w(lwrk), w(ld), w(lrlam) )
            if (Rset)
     $         call lpcolr( nZr, ldR, w(lR), one )

            prnt    = .false.
         else
*           ============================================================
*           Compute a search direction.
*           ============================================================
            if (iter .ge. itmax) then
               msg    = 'itnlim'
               go to 100
            end if

            prnt  = .true.
            iter  = iter  + 1

            call dcopy ( nZr,         w(lgq), 1, w(ld), 1 )
            call dscal ( nZr, (-one), w(ld) , 1 )

            dnorm  = dnrm2 ( nZr, w(ld), 1 )

            call cmqmul( 1, n, nZr, nfree, ldQ, unitQ,
     $                   kx, w(ld), w(lQ), w(lwrk) )
            call dgemv ( 'No transpose', nclin, n, one, A, ldA,
     $                   w(ld), 1, zero, w(lAd), 1 )

*           ------------------------------------------------------------
*           Find the constraint we bump into along d.
*           Update  x  and  Ax  if the step alfa is nonzero.
*           ------------------------------------------------------------
*           alfhit is initialized to bigalf. If it remains that value
*           after the call to  cmchzr, it is regarded as infinite.

            bigalf = ddiv  ( bigdx, dnorm, overfl )

            call cmchzr( firstv, n, nclin,
     $                   istate, bigalf, bigbnd, dnorm,
     $                   hitlow, move, onbnd, unbndd,
     $                   alfhit, alfap, jadd,
     $                   w(lAnorm), w(lAd), Ax,
     $                   bl, bu, featol, featlu, w(ld), x )

            if (unbndd) then
               msg    = 'unbndd'
               go to 100
            end if

            alfa   = alfhit
            call daxpy( n, alfa, w(ld), 1, x, 1 )

            if (nclin .gt. 0)
     $         call daxpy ( nclin, alfa, w(lAd), 1, Ax, 1 )
            xnorm  = dnrm2 ( n, x, 1 )

*           ------------------------------------------------------------
*           Add a constraint to the working set.
*           Update the  TQ  factors of the working set.
*           Use  d  as temporary work space.
*           ------------------------------------------------------------
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
               if (alfa .ge. zero) then
                  if (hitlow) then
                     x(jadd) = bl(jadd)
                  else
                     x(jadd) = bu(jadd)
                  end if
               end if
               do 510, ifix = 1, nfree
                  if (kx(ifix) .eq. jadd) go to 520
  510          continue
  520       end if

            call rzadd ( unitQ, Rset,
     $                   inform, ifix, iadd, jadd, iT,
     $                   nactiv, nZ, nfree, nZr, ngq,
     $                   n, ldA, ldQ, ldR, ldT,
     $                   kx, condmx, Dzz,
     $                   A, w(lR), w(lT), w(lgq), w(lQ),
     $                   w(lwrk), w(lrlam), w(ld) )

            nZ    = nZ  - 1
            nZr   = nZr - 1

            if (jadd .le. n) then

*              A simple bound has been added.

               nfree  = nfree  - 1
            else

*              A general constraint has been added.

               nactiv = nactiv + 1
               kactiv(nactiv) = iadd
            end if

*           Increment featol.

            call daxpy ( nctotl, tolinc, featlu, 1, featol, 1 )

            if (mod( iter, kchk ) .eq. 0) then
*              ---------------------------------------------------------
*              Check the feasibility of constraints with non-negative
*              istate values.  If some violations have occurred, force
*              iterative refinement and switch to phase 1.
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

            if (numinf .ne. 0) then
               call cmsinf( n, nclin, ldA,
     $                      istate, bigbnd, numinf, suminf,
     $                      bl, bu, A, featol,
     $                      w(lgq), x, w(lwtinf) )

               if (numinf .gt. 0) then
                  call cmqmul( 6, n, nZ, nfree, ldQ, unitQ,
     $                         kx, w(lgq), w(lQ), w(lwrk) )
               else if (lp) then
                  call dcopy ( n, w(lcq), 1, w(lgq), 1 )
               end if
            end if

            if (numinf .eq. 0  .and.  lp) then
               obj = ddot  ( n, cvec, 1, x, 1 )
            else
               obj = suminf
            end if
         end if
         go to 100
*+    end while
      end if
*     ======================end of main loop============================
*
      if (msg .eq. 'optiml') then
         if (lp) then
            if (nZr .lt. nZ) then
               msg = 'weak  '
            else
               ntfixd = 0
               do 900, j = 1, n
                  if (istate(j) .eq. 4) ntfixd = ntfixd + 1
  900          continue
               if (ntfixd .gt. 0) msg = 'weak  '
            end if
            if (abs(jtiny) .gt. 0) msg = 'weak  '
         end if
      else if (msg .eq. 'unbndd'  .and. numinf .gt. 0) then
         msg = 'infeas'
      end if

      return

 2100 format(  ' XXX  Iterative refinement.  The maximum violation is ',
     $           1p, e14.2, ' in constraint', i5 )

*     end of lpcore
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpdflt( n, nclin, title )

      implicit           double precision (a-h,o-z)

      character*(*)      title

*     ==================================================================
*     lpdflt  loads the default values of parameters not set by the
*     user.
*
*     30 Dec 1986: first version.
*     04 Nov 2001: Current version of  lpdflt.
*     ==================================================================
      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/

      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      common    /sol4cm/ epspt3, epspt5, epspt8, epspt9

      logical            newOpt, listOp
      common    /sol2lc/ newOpt, listOp, ncalls
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
      character*7        lptype(1:10)
      parameter         (zero   =  0.0d+0)
      parameter         (rdummy = -11111.0d+0, idummy = -11111)
      parameter         (gigant = 1.0d+20*.99999d+0)
      parameter         (wrktol = 1.0d-2)
      data                NoYes(0),  NoYes(1)
     $                 /     ' No',     'Yes'/
      data               icrsh(0), icrsh(1), icrsh(2)
     $                 /'Cold'   ,'Warm'   ,'Hot '   /
      data               lptype(1), lptype(2)
     $                 / '     FP', '     LP'/
      data               lptype(3), lptype(4), lptype(5), lptype(6)
     $                 / 'illegal', 'illegal', 'illegal', 'illegal'/
      data               lptype(7), lptype(8), lptype(9), lptype(10)
     $                 / '       ', '       ', '       ', 'illegal'/

      epsmch = wmach( 3)

*     Make a dummy call to lpnkey to ensure that the defaults are set.

      call lpnkey()
      newOpt = .true.

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
      if (          lprob  .lt. 0      )  lprob   = 2
      if (          lcrash .lt. 0
     $    .or.      lcrash .gt. 2      )  lcrash  = 0
      if (          itmax1 .lt. 0      )  itmax1  = max(50, 5*(n+nclin))
      if (          itmax2 .lt. 0      )  itmax2  = max(50, 5*(n+nclin))
      if (          maxact .lt. 0
     $    .or.      maxact .gt. n
     $    .or.      maxact .gt. nclin  )  maxact  = max(1, min(n,nclin))
      if (          maxnZ  .lt. 0
     $    .or.      maxnZ  .gt. n      )  maxnZ   = n
      if (          mxfree .lt. 0
     $    .or.      mxfree .gt. n      )  mxfree  = n
      if (          mxfree .lt. maxnZ  )  mxfree  = maxnZ
      if (          minsum .lt. 0      )  minsum  = 0
      if (          nclin  .lt. n      )  then
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
            write(iPrint, 2100) lptype(lprob),
     $                          nclin , icrsh(lcrash), NoYes(minsum),
     $                          n     , bigbnd,        tolfea,
     $                          kchk  , bigdx ,        tolOpt,
     $                          maxnZ , kdegen,        tolact,
     $                          mxfree, maxact
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
     $/ ' Check frequency........',     i10,   2x,
     $  ' Infinite step size.....', 1p, e10.2, 2x,
     $  ' Optimality tolerance...',     e10.2
     $/ ' Max degrees of freedom.',     i10,   2x,
     $  ' Expand frequency.......',     i10,   2x,
     $  ' Crash tolerance........',     e10.2
     $/ ' Max free variables.....',     i10
     $/ ' Max active constraints.',     i10 )
 2200 format(
     $/ ' Print level............',     i10,   2x,
     $  ' Print file.............',     i10,   2x,
     $  ' Feasibility phase itns.',     i10
     $/ ' Unit round-off.........', 1p, e10.2, 2x,
     $  ' Summary file...........',     i10,   2x,
     $  ' Optimality  phase itns.',     i10 )

*     end of lpdflt
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpkey ( iPrint, iSumm , listOp, buffer, key )

      implicit           double precision (a-h,o-z)
      character*(*)      buffer
      logical            listOp

*     ==================================================================
*     lpkey   decodes the option contained in  buffer  in order to set
*     a parameter value in the relevant element of  iprmlc  or  rprmlc.
*
*
*     Input:
*        iPrint   the print   file for error messages
*        iSumm    the summary file for error messages.
*     Output:
*        key    The first keyword contained in buffer.
*
*        lpkey  calls opnumb and the subprograms
*               lookup, scannrl tokens, upcase
*        (now called oplook, opscan, optokn, opuppr)
*        supplied by Informatics General, Inc., Palo Alto, California.
*
*     This version of lpkey dated 14-Sep-95.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
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

      external           opnumb
      logical            more  , number, opnumb, sorted

      parameter         (     maxkey = 48,  maxtie = 22,   maxtok = 10,
     $                        maxtyp = 10)
      character*16       keys(maxkey), ties(maxtie), token(maxtok),
     $                   type(maxtyp)
      character*16       key, key2, key3, value

      parameter         (idummy = -11111,  rdummy = -11111.0d+0,
     $                   sorted = .true.,  zero   =  0.0d+0)

      data   keys
     $ / 'BEGIN           ', 'CENTRAL         ', 'CHEAP           ',
     $   'CHECK           ', 'COLD            ', 'CONDITION       ',
     $   'CONSTRAINTS     ', 'CRASH           ',
     $   'DEFAULTS        ', 'DERIVATIVE      ', 'DIFFERENCE      ',
     $   'END             ', 'EXPAND          ', 'FEASIBILITY     ',
     $   'FUNCTION        ', 'HESSIAN         ', 'HOT             ',
     $   'INFINITE        ', 'IPRMLC          ', 'ITERATIONS      ',
     $   'ITERS:ITERATIONS', 'ITNS :ITERATIONS', 'LINEAR          ',
     $   'LIST            ', 'LINESEARCH      ', 'LOWER           ',
     $   'MAX.    :MAXIMUM', 'MAX     :MAXIMUM', 'MAXIMUM         ',
     $   'MIN.    :MINIMIZ', 'MIN     :MINIMIZ', 'MINIMIZ         ',
     $   'NO              ', 'NOLIST          ', 'OPTIMALITY      ',
     $   'PRINT           ', 'PROBLEM         ', 'RANK            ',
     $   'RPRMLC          ', 'SAVE            ', 'START           ',
     $   'STEP            ', 'STOP            ', 'SUMMARY         ',
     $   'UPPER           ', 'VARIABLES       ', 'VERIFY          ',
     $   'WARM            '/

      data   ties
     $ / 'ACTIVE          ', 'BOUND           ', 'CONSTRAINTS     ',
     $   'DEGREES         ', 'FEASIBILITY     ', 'FILE            ',
     $   'FREE            ', 'FREQUENCY       ', 'GRADIENTS       ',
     $   'LEVEL           ', 'LINESEARCH      ', 'NO              ',
     $   'NO.      :NUMBER', 'NUMBER          ', 'OBJECTIVE       ',
     $   'PHASE           ', 'RUN             ', 'STEP            ',
     $   'SUM             ', 'TOLERANCE       ', 'TYPE            ',
     $   'YES             '/

      data   type
     $ / 'FP              ', 'LC              ', 'LINEAR       :LP',
     $   'LP              ', 'QP          :QP2', 'QP1             ',
     $   'QP2             ', 'QP3             ', 'QP4             ',
     $   'QUADRATIC   :QP2'/
*-----------------------------------------------------------------------

*     Eliminate comments and empty lines.
*     A '*' appearing anywhere in BUFFER terminates the string.

      i      = index( buffer, '*' )
      if (i .eq. 0) then
         lenbuf = len( buffer )
      else
         lenbuf = i - 1
      end if
      if (lenbuf .le. 0) then
         key = '*'
         go to 900
      end if

*     ------------------------------------------------------------------
*     Extract up to maxtok tokens from the record.
*     ntoken returns how many were actually found.
*     key, key2, key3 are the first tokens if any, otherwise blank.
*     ------------------------------------------------------------------
      ntoken = maxtok
      call optokn( buffer(1:lenbuf), ntoken, token )
      key    = token(1)
      key2   = token(2)
      key3   = token(3)

*     Certain keywords require no action.

      if (key .eq. ' '     .or.  key .eq. 'BEGIN' ) go to 900
      if (key .eq. 'LIST'  .or.  key .eq. 'NOLIST') go to 900
      if (key .eq. 'END'                          ) go to 900

*     Most keywords will have an associated integer or real value,
*     so look for it no matter what the keyword.

      i      = 1
      number = .false.

   50 if (i .lt. ntoken  .and.  .not. number) then
         i      = i + 1
         value  = token(i)
         number = opnumb( value )
         go to 50
      end if

      if (number) then
         read (value, '(bn, e16.0)') rvalue
      else
         rvalue = zero
      end if

*     Convert the keywords to their most fundamental form
*     (upper case, no abbreviations).
*     SORTED says whether the dictionaries are in alphabetic order.
*     LOCi   says where the keywords are in the dictionaries.
*     LOCi = 0 signals that the keyword wasn't there.

      call oplook( maxkey, keys, sorted, key , loc1 )
      call oplook( maxtie, ties, sorted, key2, loc2 )

*     ------------------------------------------------------------------
*     Decide what to do about each keyword.
*     The second keyword (if any) might be needed to break ties.
*     Some seemingly redundant testing of MORE is used
*     to avoid compiler limits on the number of consecutive ELSE IFs.
*     ------------------------------------------------------------------
      more   = .true.
      if (more) then
         more   = .false.
         if      (key .eq. 'CENTRAL     ') then
            cdint  = rvalue
         else if (key .eq. 'CHEAP       ') then
            ldifgz = rvalue
         else if (key .eq. 'CHECK       ') then
            kchk   = rvalue
         else if (key .eq. 'COLD        ') then
            lcrash = 0
         else if (key .eq. 'CONDITION   ') then
            Hcndbd = rvalue
         else if (key .eq. 'CONSTRAINTS ') then
            nnclin = rvalue
         else if (key .eq. 'CRASH       ') then
            tolact = rvalue
         else if (key .eq. 'DEFAULTS    ') then
            call mcout ( iPrint, iSumm )
            listOp = .true.
            do 20, i = 1, mxparm
               iprmlc(i) = idummy
               rprmlc(i) = rdummy
   20       continue
         else if (key .eq. 'DERIVATIVE  ') then
              if (key2.eq. 'LEVEL       ') lvlder = rvalue
              if (key2.eq. 'LINESEARCH  ') ndls   = 0
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'DIFFERENCE  ') then
            fdint  = rvalue
         else if (key .eq. 'EXPAND      ') then
            kcycle = rvalue
         else if (key .eq. 'FEASIBILITY ') then
              if (key2.eq. 'PHASE       ') itmax1 = rvalue
              if (key2.eq. 'TOLERANCE   ') tolfea = rvalue
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'FUNCTION    ') then
            epsrf  = rvalue
         else
            more   = .true.
         end if
      end if

      if (more) then
         more   = .false.
         if      (key .eq. 'HESSIAN     ') then
            mm     = rvalue
         else if (key .eq. 'HOT         ') then
            lcrash = 2
         else if (key .eq. 'INFINITE    ') then
              if (key2.eq. 'BOUND       ') bigbnd = rvalue * 0.99999d+0
              if (key2.eq. 'STEP        ') bigdx  = rvalue
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'IPRMLC      ') then
*           Allow things like  IPRMLC 21 = 100  to set IPRMLC(21) = 100
            ivalue = rvalue
            if (ivalue .ge. 1  .and. ivalue .le. mxparm) then
               read (key3, '(bn, i16)') iprmlc(ivalue)
            else
               if (iPrint          .gt. 0) write(iPrint, 2400) ivalue
               if (iSumm           .gt. 0) write(iSumm , 2400) ivalue
            end if
         else if (key .eq. 'ITERATIONS  ') then
            itmax2 = rvalue
         else if (key .eq. 'LINEAR      ') then
              if (key2.eq. 'CONSTRAINTS ') nnclin = rvalue
              if (key2.eq. 'FEASIBILITY ') tolfea = rvalue
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'LINESEARCH  ') then
            eta    = rvalue
         else if (key .eq. 'LOWER       ') then
            bndlow = rvalue
         else
            more   = .true.
         end if
      end if

      if (more) then
         more   = .false.
         if      (key .eq. 'MAXIMUM     ') then
              if (key2.eq. 'ACTIVE      ') maxact = rvalue
              if (key2.eq. 'DEGREES     ') maxnZ  = rvalue
              if (key2.eq. 'FREE        ') mxfree = rvalue
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'MINIMIZ     ') then
              if (key3.eq. 'YES         ') minsum = 1
              if (key3.eq. 'NO          ') minsum = 0
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'NO          ') then
            ndls   = 1
         else if (key .eq. 'OPTIMALITY  ') then
              if (key2.eq. 'PHASE       ') itmax2 = rvalue
              if (key2.eq. 'TOLERANCE   ') tolOpt = rvalue
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'PROBLEM     ') then
            if      (key2 .eq. 'NUMBER') then
               nprob  = rvalue
            else if (key2 .eq. 'TYPE  ') then

*              Recognize     Problem type = LP     etc.

               call oplook( maxtyp, type, sorted, key3, loc3 )
               if (key3 .eq. 'FP' ) lprob = 1
               if (key3 .eq. 'LP' ) lprob = 2
               if (key3 .eq. 'QP1') lprob = 3
               if (key3 .eq. 'QP2') lprob = 4
               if (key3 .eq. 'QP3') lprob = 5
               if (key3 .eq. 'QP4') lprob = 6
               if (key3 .eq. 'LC ') lprob = 7
               if (loc3 .eq.  0  ) then
                  if (iPrint .gt. 0) write(iPrint, 2330) key3
                  if (iSumm  .gt. 0) write(iSumm , 2330) key3
                  lprob = 10
               end if
            else
               if (iPrint    .gt. 0) write(iPrint, 2320) key2
               if (iSumm     .gt. 0) write(iSumm , 2320) key2
            end if
         else
            more   = .true.
         end if
      end if

      if (more) then
         more   = .false.
         if      (key .eq. 'PRINT       ') then
              if (key2.eq. 'FILE        ') iPrint = rvalue
              if (key2.eq. 'LEVEL       ') msglvl = rvalue
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'RANK        ') then
            tolrnk = rvalue
         else if (key .eq. 'RPRMLC      ') then
*           Allow things like  RPRMLC 21 = 2  to set RPRMLC(21) = 2.0
            ivalue = rvalue
            if (ivalue .ge. 1  .and. ivalue .le. mxparm) then
               read (key3, '(bn, e16.0)') rprmlc(ivalue)
            else
               if (iPrint .gt. 0) write(iPrint, 2400) ivalue
               if (iSumm  .gt. 0) write(iSumm , 2400) ivalue
            end if
         else if (key .eq. 'START       ') then
              if (key2.eq. 'OBJECTIVE   ') jvrfy1 = rvalue
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'STEP        ') then
            dxlim  = rvalue
         else if (key .eq. 'STOP        ') then
              if (key2.eq. 'OBJECTIVE   ') jvrfy2 = rvalue
              if (loc2.eq.  0            ) then
                 if (iPrint .gt. 0)        write(iPrint, 2320) key2
                 if (iSumm  .gt. 0)        write(iSumm , 2320) key2
              end if
         else if (key .eq. 'SUMMARY     ') then
            iSumm  = rvalue
         else if (key .eq. 'UPPER       ') then
            bndupp = rvalue
         else if (key .eq. 'VARIABLES   ') then
            nn     = rvalue
         else if (key .eq. 'VERIFY      ') then
              if (key2.eq. 'OBJECTIVE   ') lverfy =  1
              if (key2.eq. 'NO          ') lverfy = -1
              if (key2.eq. 'YES         ') lverfy =  3
              if (key2.eq. 'GRADIENTS   ') lverfy =  3
              if (key2.eq. 'LEVEL       ') lverfy =  rvalue
              if (loc2.eq.  0            ) lverfy =  3
         else if (key .eq. 'WARM        ') then
            lcrash = 1
         else
            if (iPrint             .gt. 0) write(iPrint, 2300) key
            if (iSumm              .gt. 0) write(iSumm , 2300) key
         end if
      end if

  900 return

 2300 format(' XXX  Keyword not recognized:         ', a)
 2320 format(' XXX  Second keyword not recognized:  ', a)
 2330 format(' XXX  Third  keyword not recognized:  ', a)
 2400 format(' XXX  The PARM subscript is out of range:', i10)

*     end of lpkey
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lploc ( cset, n, nclin, litotl, lwtotl )

      implicit           double precision (a-h,o-z)
      logical            cset

*     ==================================================================
*     lploc   allocates the addresses of the work arrays for lpcore.
*
*     Note that the arrays ( gq, cq ) lie in contiguous areas of
*     workspace.
*
*     Original version written  2-January-1987.
*     This version of  lploc  dated  18-Nov-1990.
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
      lenRT = ldT *ncolT
      if (nclin .eq. 0) then
         lenQ  = 0
      else
         lenQ  = ldQ*ldQ
      end if

      if (cset) then
         lencq = n
      else
         lencq = 0
      end if

*     ------------------------------------------------------------------
*     We start with arrays that can be preloaded by smart users.
*     ------------------------------------------------------------------
      lfeatu    = minw
      minw      = lfeatu + nclin + n

*     Next comes stuff used by  lpcore  and  qpcore.

      lAnorm    = minw
      lAd       = lAnorm + nclin
      ld        = lAd    + nclin
      lgq       = ld     + n
      lcq       = lgq    + n
      lrlam     = lcq    + lencq
      lR        = lrlam  + n
      lT        = lR
      lQ        = lT     + lenRT
      lwtinf    = lQ     + lenQ
      lwrk      = lwtinf + n     + nclin
      minw      = lwrk   + n     + nclin

*     Load the addresses in loclc.

      loclc( 1) = lkactv
      loclc( 2) = lkx

      loclc( 3) = lfeatu
      loclc( 4) = lAnorm
      loclc( 5) = lAd

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

*     end of lploc
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpnkey( )

      implicit           double precision (a-h,o-z)

*     ==================================================================
*     lpnkey  counts the number of consecutive calls of lpprm or lpprms.
*
*     Original version written  11-Sep-95,
*     This version of  lpnkey  dated  12-Sep-95.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      logical            newOpt, listOp
      common    /sol2lc/ newOpt, listOp, ncalls
      save      /sol2lc/

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
      parameter         (rdummy = -11111.0d+0, idummy = -11111)

      logical             first
      save                first
      data                first /.true./

      if ( first ) then
         nCalls = 0
         first  = .false.
         newOpt = .true.
         listOp = .true.

         call mcout ( iPrint, iSumm )
         do 10, i = 1, mxparm
            iprmlc(i) = idummy
            rprmlc(i) = rdummy
   10    continue
         first  = .false.
      end if

      if ( newOpt ) then
         nCalls = 1
      else
         nCalls = nCalls + 1
      end if

*     end of lpnkey
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpprm ( string )
      character*(*)      string

*     ==================================================================
*     lpprm   loads the option supplied in  string  into the relevant
*     element of  iprmlc  or  rprmlc.
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      logical            newOpt, listOp
      common    /sol2lc/ newOpt, listOp, ncalls
      save      /sol2lc/

      character*16        key
      character*72        buffer
*     ------------------------------------------------------------------
      buffer = string

*     If this is the first call of lpnkey, set newOpt and default values
*     of the optional parameters. The default is to list the options.
*     Increment ncalls, the number of calls of lpprms and lpprms for
*     this optimization.

      call lpnkey()

*     Call  lpkey  to decode the option and set the parameter value.
*     If required, print a heading at the start of a new run.
*     Note that the following call to lpkey may reset iPrint and iSumm.

      call lpkey ( iPrint, iSumm, listOp, buffer, key )
      if (key .eq.  'LIST'  ) listOp = .true.
      if (key .eq.  'NOLIST') listOp = .false.

      if ( listOp ) then
         if ( newOpt ) then
            if (iPrint .gt. 0) then
               write ( iPrint, '(// a / a /)' )
     $                         ' Optional Parameters',
     $                         ' -------------------'
            end if
            newOpt = .false.
         end if
         if (iPrint .gt. 0) write ( iPrint, '( 6x, a )'    ) buffer
      end if

*     end of lpprm
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpprmi( string, ivalue )

      implicit           double precision (a-h,o-z)
      character*(*)      string
      integer            ivalue

*     ==================================================================
*     lpprmi decodes the option contained in  string // ivalue.
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

*     end of lpprmi
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpprmr( string, rvalue )

      implicit           double precision (a-h,o-z)
      character*(*)      string
      double precision   rvalue

*     ==================================================================
*     lpprmr decodes the option contained in  string // rvalue.
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

*     end of lpprmr
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpprms( iOptns, inform )
      integer            iOptns, inform

*     ==================================================================
*     lpprms  reads the options file from unit  iOptns  and loads the
*     options into the relevant elements of  iprmlc  and  rprmlc.
*
*     If  iOptns .lt. 0  or  iOptns .gt. 99  then no file is read,
*     otherwise the file associated with unit  iOptns  is read.
*
*     Output:
*
*         inform = 0  if a complete  options  file was found
*                     (starting with  begin  and ending with  end);
*                  1  if  iOptns .lt. 0  or  iOptns .gt. 99;
*                  2  if  begin  was found, but end-of-file
*                     occurred before  end  was found;
*                  3  if end-of-file occurred before  begin  or
*                     endrun  were found;
*                  4  if  endrun  was found before  begin.
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      logical            newOpt, listOp
      common    /sol2lc/ newOpt, listOp, ncalls
      save      /sol2lc/
      external           lpkey
*     ------------------------------------------------------------------
*     Update ncalls, the number of calls of lpprm and lpprms since the
*     start of this problem.
*     On the very first call, the default parameters are set.

      call lpnkey()
      call opfile( iOptns, iPrint, iSumm,
     $             listOp, newOpt, inform, lpkey )

*     end of lpprms
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine lpprnt( prbtyp, header, Rset,
     $                   msglvl, iter,
     $                   isdel, jdel, jadd,
     $                   n, nclin, nactiv,
     $                   nfree, nZ, nZr,
     $                   ldR, ldT, istate,
     $                   alfa, condRz, condT,
     $                   Dzz, gZrnrm,
     $                   numinf, suminf, notOpt, objlp, trusml,
     $                   Ax, R, T, x,
     $                   work )

      implicit           double precision (a-h,o-z)
      character*2        prbtyp
      integer            istate(*)
      logical            header, Rset
      double precision   Ax(*), R(ldR,*), T(ldT,*), x(n)
      double precision   work(n)

*     ==================================================================
*     lpprnt  prints various levels of output for lpcore.
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
*       .ge. 20        constraint status,  x  and  ax.
*
*       .ge. 30        diagonals of  T  and  R.
*
*
*     Based on a version of lcprt, written by PEG, 16-February-1987.
*     This version of  lpprnt  dated  23-Dec-92.
*     (C) 1992--1997  Regents of the University of California and the
*                     Trustees of Stanford University
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/

      logical            newSet, prtHdr

      parameter         (zero   = 0.0d+0)
      parameter         (mLine1 = 40    , mLine2 = 5)

      character*2        ladd, ldel
      character*15       lmchar
      character*2        lstate(0:5)
      data               lstate(0), lstate(1), lstate(2)
     $                  /'  '     , 'L '     , 'U '     /
      data               lstate(3), lstate(4), lstate(5)
     $                  /'E '     , 'F '     , 'A '     /

      if (msglvl .ge. 15  .and.  iPrint .gt. 0)
     $     write(iPrint, 1000) prbtyp, iter

      if (msglvl .ge. 5 ) then
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
            obj = objlp
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
               write(iPrint, 1200)
               lines1 = 0
            end if

            write(iPrint, 1700) Itn, jdel, ldel, jadd, ladd,
     $                          alfa, numinf, obj,
     $                          gZrnrm, ndf, nZ-nZr,
     $                          n-nfree, nactiv, lmchar, condT
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
      jdel   = 0
      jadd   = 0
      alfa   = zero

      return

 1000 format(/// ' ', a2, ' iteration', i5
     $         / ' =================' )
 1100 format(// ' Itn Jdel  Jadd     Step Ninf  Sinf/Objective',
     $          ' Norm gZ   Zr  Art' )
 1200 format(// ' Itn Jdel  Jadd     Step Ninf  Sinf/Objective',
     $          ' Norm gZ   Zr  Art  Bnd  Lin NOpt    Min Lm  Cond T' )
 1700 format(    i4, i5, a1, i5, a1, 1p, e8.1, i5, e16.8,
     $           e8.1, 2i5,
     $           2i5, a15, e8.0 )
 2000 format(/ ' Values and status of the ', a2, ' constraints'
     $       / ' ---------------------------------------' )
 2100 format(/ ' Variables...'                 /   (1x,5(1p,e15.6, i5)))
 2200 format(/ ' General linear constraints...'/   (1x,5(1p,e15.6, i5)))
 3000 format(/ ' Diagonals of ' , a2,' working set factor T'
     $       / (1p, 5e15.6))
 3100 format(/ ' Diagonals of ' , a2, ' triangle Rz        '
     $       / (1p, 5e15.6))
 5000 format(/// ' ---------------------------------------------------',
     $           '--------------------------------------------' )

*     end of lpprnt
      end

