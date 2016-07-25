*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     file  utcomsubs.f
*
*     cmAfac   cmcrsh   cmchzr   cmdgen   cmfeas   cminit   cmmsg1
*     cmmul1   cmmul2   cmsetx   cmsinf   cmwrap
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmAfac( inform, needTQ, gset, unitQ, vertex,
     $                   nclin, nactiv, nartif, nfree, nZ,
     $                   n, ldQ, ldA, ldT,
     $                   istate, kactiv, kx,
     $                   condmx, errmax, jmax,
     $                   A, Ax, bl, bu, g, gq,
     $                   featol, T, x, Q,
     $                   w, c, s )

      implicit           double precision(a-h,o-z)

      logical            needTQ, gset, unitQ, vertex
      integer            istate(n+nclin), kactiv(n), kx(n)
      double precision   A(ldA,*), Ax(*), g(*), gq(*)
      double precision   bl(n+nclin), bu(n+nclin), featol(n+nclin)
      double precision   T(ldT,*), x(n), Q(ldQ,*)
      double precision   w(n), c(n), s(n)

*     ==================================================================
*     cmAfac  computes the point on a working set that is closest in the
*             least-squares sense to the input vector x.
*
*     If the computed point gives a row error of more than the
*     feasibility tolerance, an extra step of iterative refinement is
*     used.  If  x  is still infeasible,  an error message is printed.
*
*     w, s and c are work vectors.
*
*     Original version written by PEG,  August 1991.
*     This version of  cmAfac  dated  09-May-93.
*     ==================================================================
      logical            rowerr
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      parameter         (ntry   = 5)
      parameter         (zero   = 0.0d+0, one = 1.0d+0)

      inform = 0

      if (needTQ) then
*        ---------------------------------------------------------------
*        Compute the TQ factorization of the working-set matrix.
*        Install the transformed linear term in gq.
*        ---------------------------------------------------------------
         unitQ  = .true.
         nZ     = nfree

         if (nactiv .gt. 0) then
            iT     = nactiv + 1
            nact1  = nactiv
            nactiv = 0
            ncolgQ = 0

            call rzadds( unitQ, vertex,
     $                   1, nact1, iT, nactiv, nartif, nZ, nfree,
     $                   nrejtd, ncolgQ, n, ldQ, ldA, ldT,
     $                   istate, kactiv, kx,
     $                   condmx,
     $                   A, T, w, Q, w, c, s )
         end if

         if (gset) then
            call dcopy ( n, g, 1, gq, 1 )
            call cmqmul( 6, n, nZ, nfree, ldQ, unitQ, kx, gq, Q, w )
         end if
      end if

*     ------------------------------------------------------------------
*     Move  x  onto the simple bounds in the working set.
*     ------------------------------------------------------------------
      do 100, k = nfree+1, n
         j      = kx(k)
         is     = istate(j)
         bnd    = bl(j)
         if (is .ge. 2) bnd  = bu(j)
         if (is .ne. 4) x(j) = bnd
  100 continue

*     ------------------------------------------------------------------
*     Move  x  onto the general constraints in the working set.
*     ktry  attempts are made to get acceptable row errors.
*     ------------------------------------------------------------------
      ktry   = 1
      jmax   = 1
      errmax = zero

*     ------------------------------------------------------------------
*+    repeat
  200    if (nactiv .gt. 0) then

*           Set w = (residuals of constraints in the working set).
*           Solve for s, the smallest correction to x that gives a point
*           on the constraints in the working set.  Define  s = Y*(sY),
*           where  sY  solves the triangular system  T*(sY) = residuals.

            do 220, i = 1, nactiv
               k      = kactiv(i)
               j      = n + k
               if (istate(j) .eq. 2) then
                  bnd = bu(j)
               else
                  bnd = bl(j)
               end if
               w(nactiv-i+1) = bnd - ddot  ( n, A(k,1), ldA, x, 1 )
  220       continue

            call dtrsv ( 'U', 'N', 'N', nactiv, T(1,nZ+1), ldT,
     $                   w, 1 )
            call dload ( n, zero, s, 1 )
            call dcopy ( nactiv, w, 1, s(nZ+1), 1 )

            call cmqmul( 2, n, nZ, nfree, ldQ, unitQ, kx, s, Q, w )
            call daxpy ( n, one, s, 1, x, 1 )
         end if

*        ---------------------------------------------------------------
*        Initialize  Ax  for all the general constraints.
*        ---------------------------------------------------------------
         if (nclin .gt. 0)
     $      call dgemv ( 'N', nclin, n, one, A, ldA, x, 1, zero, Ax, 1 )

*        ---------------------------------------------------------------
*        Check the row residuals.
*        ---------------------------------------------------------------
         if (nactiv .gt. 0) then
            do 300, k = 1, nactiv
               i   = kactiv(k)
               j   = n + i
               is  = istate(j)
               if (is .eq. 1) w(k) = bl(j) - Ax(i)
               if (is .ge. 2) w(k) = bu(j) - Ax(i)
  300       continue

            jmax   = idamax( nactiv, w, 1 )
            errmax = abs( w(jmax) )
         end if

         ktry = ktry + 1
*+    until    (errmax .le. featol(jmax) .or. ktry .gt. ntry
      if (.not.(errmax .le. featol(jmax) .or. ktry .gt. ntry)) go to 200
*     ------------------------------------------------------------------
      rowerr = errmax .gt. featol(jmax)
      if ( rowerr ) then
         inform = 2
         if (iPrint .gt. 0) write(iPrint, 9000)
         if (iSumm  .gt. 0) write(iSumm , 9000)
      end if

      return

 9000 format(  ' XXX  Cannot satisfy the working-set constraints.' )

*     end of cmAfac
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmcrsh( start, vertex,
     $                   nclin, nctotl, nactiv, nartif,
     $                   nfree, n, ldA,
     $                   istate, kactiv, kx,
     $                   bigbnd, tolact,
     $                   A, Ax, bl, bu, featol, x, wx, work )

      implicit           double precision(a-h,o-z)
      character*4        start
      logical            vertex
      integer            istate(nctotl), kactiv(n), kx(n)
      double precision   A(ldA,*), Ax(*), bl(nctotl), bu(nctotl)
      double precision   featol(nctotl)
      double precision   x(n), wx(n), work(n)

*     ==================================================================
*     cmcrsh  computes the quantities  istate (optionally),  kactiv,
*     nactiv,  nz  and  nfree  associated with the working set at x.
*
*     The computation depends upon the value of the input parameter
*     start,  as follows...
*
*     Start = 'cold'  An initial working set will be selected. First,
*                     nearly-satisfied or violated bounds are added.
*                     Next,  general linear constraints are added that
*                     have small residuals.
*
*     Start = 'warm'  The quantities kactiv, nactiv and nfree are
*                     initialized from istate,  specified by the user.
*
*     If vertex is true, an artificial vertex is defined by fixing some
*     variables on their bounds.  Infeasible variables selected for the
*     artificial vertex are fixed at their nearest bound.  Otherwise,
*     the variables are unchanged.
*
*     Values of istate(j)....
*
*        - 2         - 1         0           1          2         3
*     a'x lt bl   a'x gt bu   a'x free   a'x = bl   a'x = bu   bl = bu
*
*     Original version written by  PEG, 31-October-1984.
*     This version of  cmcrsh  dated 11-May-1995.
*     ==================================================================
      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/
      parameter        ( zero = 0.0d+0, one = 1.0d+0 )

      flmax  =   wmach(7)
      biglow = - bigbnd
      bigupp =   bigbnd

*     ------------------------------------------------------------------
*     Move the variables inside their bounds.
*     ------------------------------------------------------------------
      do 10, j = 1, n
         b1    = bl(j)
         b2    = bu(j)
         tol   = featol(j)

         if (b1 .gt. biglow) then
            if (x(j) .lt. b1 - tol) x(j) = b1
         end if

         if (b2 .lt. bigupp) then
            if (x(j) .gt. b2 + tol) x(j) = b2
         end if
   10 continue

      call dcopy ( n, x, 1, wx, 1 )
      nfree  =   n
      nactiv =   0
      nartif =   0

      if      (start .eq. 'cold') then
         do 100, j = 1, nctotl
            istate(j) = 0
            if (bl(j) .eq. bu(j)) istate(j) = 3
  100    continue

      else if (start .eq. 'warm') then
         do 110, j = 1, nctotl
            if (istate(j) .gt. 3  .or.  istate(j) .lt. 0 ) istate(j) = 0
            if (bl(j) .le. biglow .and. bu(j) .ge. bigupp) istate(j) = 0
            if (bl(j) .le. biglow .and. istate(j) .eq. 1 ) istate(j) = 0
            if (bu(j) .ge. bigupp .and. istate(j) .eq. 2 ) istate(j) = 0
            if (bl(j) .ne. bu(j)  .and. istate(j) .eq. 3 ) istate(j) = 0
  110    continue
      end if

*     Define nfree and kactiv.
*     Ensure that the number of bounds and general constraints in the
*     working set does not exceed n.

      do 200, j = 1, nctotl
         if (nactiv .eq. nfree) istate(j) = 0

         if (istate(j) .gt. 0) then
            if (j .le. n) then
               nfree = nfree - 1

               if      (istate(j) .eq. 1) then
                  wx(j) = bl(j)
               else if (istate(j) .ge. 2) then
                  wx(j) = bu(j)
               end if
            else
               nactiv = nactiv + 1
               kactiv(nactiv) = j - n
            end if
         end if
  200 continue

*     ------------------------------------------------------------------
*     If a cold start is required,  attempt to add as many
*     constraints as possible to the working set.
*     ------------------------------------------------------------------
      if (start .eq. 'cold') then

*        See if any bounds are violated or nearly satisfied.
*        If so,  add these bounds to the working set and set the
*        variables exactly on their bounds.

         j = n
*+       while (j .ge. 1  .and.  nactiv .lt. nfree) do
  300    if    (j .ge. 1  .and.  nactiv .lt. nfree) then
            if (istate(j) .eq. 0) then
               b1     = bl(j)
               b2     = bu(j)
               is     = 0
               if (b1 .gt. biglow) then
                  if (wx(j) - b1 .le. (one + abs( b1 ))*tolact) is = 1
               end if
               if (b2 .lt. bigupp) then
                  if (b2 - wx(j) .le. (one + abs( b2 ))*tolact) is = 2
               end if
               if (is .gt. 0) then
                  istate(j) = is
                  if (is .eq. 1) wx(j) = b1
                  if (is .eq. 2) wx(j) = b2
                  nfree = nfree - 1
               end if
            end if
            j = j - 1
            go to 300
*+       end while
         end if

*        ---------------------------------------------------------------
*        The following loop finds the linear constraint (if any) with
*        smallest residual less than or equal to tolact  and adds it
*        to the working set.  This is repeated until the working set
*        is complete or all the remaining residuals are too large.
*        ---------------------------------------------------------------
*        First, compute the residuals for all the constraints not in the
*        working set.

         if (nclin .gt. 0  .and.  nactiv .lt. nfree) then
            do 410, i = 1, nclin
               if (istate(n+i) .le. 0)
     $         Ax(i) = ddot  (n, A(i,1), ldA, wx, 1 )
  410       continue

            is     = 1
            toobig = tolact + tolact

*+          while (is .gt. 0  .and.  nactiv .lt. nfree) do
  500       if    (is .gt. 0  .and.  nactiv .lt. nfree) then
               is     = 0
               resmin = tolact

               do 520, i = 1, nclin
                  j      = n + i
                  if (istate(j) .eq. 0) then
                     b1     = bl(j)
                     b2     = bu(j)
                     resl   = toobig
                     resu   = toobig
                     if (b1 .gt. biglow)
     $                  resl  = abs( Ax(i) - b1 ) / (one + abs( b1 ))
                     if (b2 .lt. bigupp)
     $                  resu  = abs( Ax(i) - b2 ) / (one + abs( b2 ))
                     residl   = min( resl, resu )
                     if(residl .lt. resmin) then
                        resmin = residl
                        imin   = i
                        is     = 1
                        if (resl .gt. resu) is = 2
                     end if
                  end if
  520          continue

               if (is .gt. 0) then
                  nactiv = nactiv + 1
                  kactiv(nactiv) = imin
                  j         = n + imin
                  istate(j) = is
               end if
               go to 500
*+          end while
            end if
         end if
      end if

      if (vertex  .and.  nactiv .lt. nfree) then
*        ---------------------------------------------------------------
*        Find an initial vertex by temporarily fixing some variables.
*        ---------------------------------------------------------------
*        Compute lengths of columns of selected linear constraints
*        (just the ones corresponding to variables eligible to be
*        temporarily fixed).

         do 630, j = 1, n
            if (istate(j) .eq. 0) then
               colsiz = zero
               do 620, k = 1, nclin
                  if (istate(n+k) .gt. 0)
     $            colsiz = colsiz + abs( A(k,j) )
  620          continue
               work(j) = colsiz
            end if
  630    continue

*        Find the  nartif  smallest such columns.
*        This is an expensive loop.  Later we can replace it by a
*        4-pass process (say), accepting the first col that is within
*        t  of  colmin, where  t = 0.0, 0.001, 0.01, 0.1 (say).
*        (This comment written in 1980).

*+       while (nactiv .lt. nfree) do
  640    if    (nactiv .lt. nfree) then
            colmin = flmax
            do 650, j = 1, n
               if (istate(j) .eq. 0) then
                  if (nclin .eq. 0) go to 660
                  colsiz = work(j)
                  if (colmin .gt. colsiz) then
                     colmin = colsiz
                     jmin   = j
                  end if
               end if
  650       continue
            j         = jmin

*           Fix x(j) at its current value.

  660       istate(j) = 4
            nartif    = nartif + 1
            nfree     = nfree  - 1
            go to 640
*+       end while
         end if
      end if

      jfree = 1
      jfix  = nfree + 1
      do 710, j = 1, n
         if (istate(j) .le. 0) then
            kx(jfree) = j
            jfree     = jfree + 1
         else
            kx(jfix)  = j
            jfix      = jfix  + 1
         end if
  710 continue

*     end of cmcrsh
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmchzr( firstv, n, nclin,
     $                   istate, bigalf, bigbnd, pnorm,
     $                   hitlow, move, onbnd, unbndd,
     $                   alfa, alfap, jhit,
     $                   anorm, Ap, Ax,
     $                   bl, bu, featol, featlu, p, x )

      implicit           double precision(a-h,o-z)
      logical            firstv
      logical            hitlow, move, onbnd, unbndd
      integer            istate(n+nclin)
      double precision   bl(n+nclin), bu(n+nclin)
      double precision   featol(n+nclin), featlu(n+nclin)
      double precision   anorm(*), Ap(*), Ax(*)
      double precision   p(n), x(n)

*     ==================================================================
*     cmchzr  finds a step alfa such that the point x + alfa*p reaches
*     one of the linear constraints (including bounds).
*
*     In this version of cmchzr, when x is infeasible, the number of
*     infeasibilities will never increase.  If the number stays the
*     same, the sum of infeasibilities will decrease.  If the number
*     decreases by one or more,  the sum of infeasibilities will usually
*     decrease also, but occasionally it will increase after the step
*     alfa  is taken.  (Convergence is still assured because the number
*     has decreased.)
*
*     Three possible steps are computed as follows:
*
*     alfaf = the maximum step that can be taken without violating
*              one of the constraints that are currently satisfied.
*
*     alfai = reaches a linear constraint that is currently violated.
*              Usually this will be the furthest such constraint along
*              p, subject to the angle between the constraint normal and
*              p being reasonably close to the maximum value among
*              infeasible constraints,  but if firstv = .true. it will
*              be the first one along p.  The latter case applies only
*              when the problem has been determined to be infeasible,
*              and the sum of infeasibilities are being minimized.
*              (Alfai is not defined when x is feasible.)
*
*     Alfai is needed occasionally when infeasible, to prevent
*     going unnecessarily far when alfaf is quite large.  It will
*     always come into effect when x is about to become feasible.
*     (The sum of infeasibilities will decrease initially as alfa
*     increases from zero, but may start increasing for larger steps.
*     Choosing a large alfai allows several elements of  x  to
*     become feasible at the same time.
*
*     In the end, we take  alfa = alfaf  if x is feasible, or if
*     alfai > alfap (where  alfap  is the perturbed step from pass 1).
*     Otherwise,  we take  alfa = alfai.
*
*     Input parameters
*     ----------------
*     bigalf defines what should be treated as an unbounded step.
*     bigbnd provides insurance for detecting unboundedness.
*            If alfa reaches a bound as large as bigbnd, it is
*            classed as an unbounded step.
*     featol is the array of current feasibility tolerances used by
*            cmsinf.  Typically in the range 0.5*tolx to 0.99*tolx,
*            where tolx is the featol specified by the user.
*     tolinc (in common) is used to determine stepmn (see below),
*            the minimum positive step.
*     istate is set as follows:
*            istate(j) = -2  if a'x .lt. bl - featol
*                      = -1  if a'x .gt. bu + featol
*                      =  0  if a'x is not in the working set
*                      =  1  if a'x is in the working set at bl
*                      =  2  if a'x is in the working set at bu
*                      =  3  if a'x is in the working set (an equality)
*                      =  4  if x(j) is temporarily fixed.
*            values -2 and -1 do not occur once feasible.
*     bl     the lower bounds on the variables.
*     bu     the upper bounds on ditto.
*     x      the values of       ditto.
*     p      the search direction.
*
*
*     Output Parameters
*     -----------------
*     hitlow  = true  if a lower bound restricted alfa.
*             = false otherwise.
*     move    = true  if  exact ge stepmn  (defined at end of code).
*     onbnd   = true  if  alfa = exact.  This means that the step  alfa
*                     moves x  exactly onto one of its constraints,
*                     namely  bound.
*             = false if the exact step would be too small
*                     ( exact .lt. stepmn ).
*               (with these definitions,  move = onbnd).
*     unbndd  = true  if alfa = bigalf.  Jhit may possibly be zero.
*               The parameters hitlow, move, onbnd, bound and exact
*               should not be used.
*     jhit    = the index (if any) such that constraint jhit reaches
*               a bound.
*     bound   = the bound value bl(jhit) or bu(jhit) corresponding
*               to hitlow.
*     exact   = the step that would take constraint jhit exactly onto
*               bound.
*     alfa    = an allowable, positive step.
*               if unbndd is true,  alfa = stepmx.
*               otherwise,          alfa = max( stepmn, exact ).
*
*
*     Cmchzr is based on MINOS 5.2 routine m5chzr, which implements the
*     expand procedure to deal with degeneracy. The step alfaf is
*     chosen as in the two-pass approach of Paula Harris (1973), except
*     that this version insists on returning a positive step, alfa.
*     Two features make this possible:
*
*        1. Featol increases slightly each iteration.
*
*        2. The blocking constraint, when added to the working set,
*           retains the value Ax(jhit) + alfa * Ap(jhit),
*           even if this is not exactly on the blocking bound.
*
*     For infeasible variables moving towards their bound, we require
*     the rate of change of the chosen constraint to be at least gamma
*     times as large as the biggest available.  This still gives us
*     freedom in pass 2.
*     gamma = 0.1 and 0.01 seemed to inhibit phase 1 somewhat.
*     gamma = 0.001 seems to be safe.
*
*     19 Apr 1988: first version.
*     04 Nov 2001: Current version of  cmchzr
*     ==================================================================
      common    /sol4cm/ epspt3, epspt5, epspt8, epspt9
      common    /sol3lc/ tolx0 , tolinc, idegen, kdegen, ndegen,
     $                   itnfix, nfix(2)
      logical            blockf, blocki

      parameter        ( zero  = 0.0d+0, one = 1.0d+0 )
      parameter        ( gamma = 1.0d-3               )

*     tolpiv is a tolerance to exclude negligible elements of a'p.

      biglow = - bigbnd
      bigupp =   bigbnd

      tolpiv = epspt3*epspt3*pnorm

*     ------------------------------------------------------------------
*     First pass -- find steps to perturbed constraints, so that
*     alfap will be slightly larger than the true step.
*     In degenerate cases, this strategy gives us some freedom in the
*     second pass.  The general idea follows that described by P.M.J.
*     Harris, p.21 of Mathematical Programming 5, 1 (1973), 1--28.
*     ------------------------------------------------------------------
      atpmxi = zero
      alfap  = bigalf
      jhitf  = 0

      do 200, j = 1, n+nclin
         js = istate(j)

         if (js .le. 0) then
            delta = featol(j)

            if (j .le. n) then
               atx    = x(j)
               atp    = p(j)
               atpabs = abs( atp )
               atpscd = atpabs
            else
               i      = j - n
               atx    = Ax(i)
               atp    = Ap(i)
               atpabs = abs( atp )
               atpscd = atpabs / (one  +  anorm(i))
            end if

            if ( atpscd .le. tolpiv) then
*              ---------------------------------------------------------
*              This constraint appears to be constant along p.  It is
*              not used to compute the step.  Give the residual a value
*              that can be spotted in the debug output.
*              ---------------------------------------------------------
               res = - one

            else if (atp .le. zero  .and.  js .ne. -2) then
*              ---------------------------------------------------------
*              a'x  is decreasing and the lower bound is not violated.
*              ---------------------------------------------------------
*              Test for smaller alfap.
*              If the upper bound is violated. Test for bigger atp.

               if (bl(j) .gt. biglow) then
                  res    = atx - bl(j) + delta

                  if (res .lt. alfap*atpabs) then
                     alfap = res / atpabs
                     jhitf = j
                  end if
               end if

               if (js .eq. -1) atpmxi = max ( atpmxi, atpscd )

            else if (atp .gt. zero  .and.  js .ne. -1) then
*              ---------------------------------------------------------
*              a'x  is increasing and the upper bound is not violated.
*              ---------------------------------------------------------
*              Test for smaller alfap.
*              If the lower bound is violated. Test for bigger atp.

               if (bu(j) .lt. bigupp) then
                  res = bu(j) - atx + delta

                  if (res .lt. alfap*atp) then
                     alfap = res / atp
                     jhitf = j
                  end if
               end if

               if (js .eq. -2) atpmxi = max ( atpmxi, atpscd )
            end if
         end if
  200 continue

*     ------------------------------------------------------------------
*     Second pass.
*     For feasible variables, recompute steps without perturbation.
*     amongst constraints that are closer than alfap, choose the one
*     That makes the largest angle with the search direction.
*     For infeasible variables, find the largest step subject to a'p
*     being no smaller than gamma * max(a'p).
*     ------------------------------------------------------------------
      if (firstv) then
         alfai = bigalf
      else
         alfai = zero
      end if

      atpmxf = zero
      atpmxi = gamma*atpmxi
      jhiti  = 0

      do 300, j = 1, n+nclin
         js = istate(j)

         if (js .le. 0) then

            if (j .le. n) then
               atx    = x(j)
               atp    = p(j)
               atpabs = abs( atp )
               atpscd = atpabs
            else
               i      = j - n
               atx    = Ax(i)
               atp    = Ap(i)
               atpabs = abs( atp )
               atpscd = atpabs / (one  +  anorm(i))
            end if

            if ( atpscd .le. tolpiv) then
*              ---------------------------------------------------------
*              This constraint appears to be constant along p.  It is
*              not used to compute the step.  Give the residual a value
*              that can be spotted in the debug output.
*              ---------------------------------------------------------
               res = - one

            else if (atp .le. zero  .and.  js .ne. -2) then
*              ---------------------------------------------------------
*              a'x  is decreasing.
*              ---------------------------------------------------------
*              Test for bigger a'p if the lower bound is satisfied.
*              Test for smaller alfaf.

               if (atpscd .gt. atpmxf) then

                  if (bl(j) .gt. biglow) then
                     res    = atx - bl(j)

                     if (res .le. alfap*atpabs) then
                        atpmxf = atpscd
                        jhitf  = j
                     end if
                  end if
               end if

               if (js .eq. -1)  then

*                 The upper bound is violated.
*                 Test for bigger or smaller alfai,  depending on the
*                 value of firstv.

                  if (firstv) then
                     res    = atx - bu(j)

                     if (res .le. alfai*atpabs) then
                        alfai  = res / atpabs
                        jhiti  = j
                     end if

                  else if (atpscd .ge. atpmxi) then
                     res    = atx - bu(j)

                     if (res .gt. alfai*atpabs) then
                        alfai  = res / atpabs
                        jhiti  = j
                     end if
                  end if
               end if

            else if (atp .gt. zero  .and.  js .ne.  -1)  then
*              ---------------------------------------------------------
*              a'x  is increasing and the upper bound is not violated.
*              ---------------------------------------------------------
*              Test for smaller alfap.

               if (atpscd .gt. atpmxf) then

                  if (bu(j) .lt. bigupp) then
                     res = bu(j) - atx

                     if (res .le. alfap*atp) then
                        atpmxf = atpscd
                        jhitf  = j
                     end if
                  end if
               end if

               if (js .eq. -2)  then

*                 The lower bound is violated.
*                 Test for bigger or smaller alfai,  depending on the
*                 value of firstv.

                  if (firstv) then
                     res    = bl(j) - atx

                     if (res .le. alfai*atp) then
                        alfai  = res / atp
                        jhiti  = j
                     end if
                  else if (atpscd .ge. atpmxi) then
                     res    = bl(j) - atx

                     if (res .gt. alfai*atp) then
                        alfai  = res / atp
                        jhiti  = j
                     end if
                  end if
               end if
            end if
         end if
  300 continue

*     ------------------------------------------------------------------
*     See if a feasible and/or infeasible constraint blocks.
*     ------------------------------------------------------------------
      blockf = jhitf .gt. 0
      blocki = jhiti .gt. 0
      unbndd = .not. ( blockf  .or.  blocki )

      if (unbndd) go to 900

      if (blockf) then
*        ---------------------------------------------------------------
*        A constraint is hit which is currently feasible.
*        The corresponding step alfaf is not used, so no need to get it,
*        but we know that alfaf .le. alfap, the step from pass 1.
*        ---------------------------------------------------------------
         jhit = jhitf
         if (jhit .le. n) then
            atp = p(jhit)
         else
            atp = Ap(jhit-n)
         end if
         hitlow = atp .lt. zero
      end if

*     If there is a choice between alfaf and alfai, it is probably best
*     to take alfai.  However, we can't if alfai is bigger than alfap.

      if (blocki  .and.  alfai .le. alfap) then
*        ---------------------------------------------------------------
*        An infeasible variable reaches its violated bound.
*        ---------------------------------------------------------------
         jhit = jhiti
         if (jhit .le. n) then
            atp = p(jhit)
         else
            atp = Ap(jhit-n)
         end if
         hitlow = atp .gt. zero
      end if

      if (jhit .le. n) then
         atx = x(jhit)
      else
         atx = Ax(jhit-n)
      end if

*     ------------------------------------------------------------------
*     Try to step exactly onto bound, but make sure the exact step
*     is sufficiently positive.  (Exact will be alfaf or alfai.)
*     Since featol increases by  tolinc  each iteration, we know that
*     a step as large as  stepmn  (below) will not cause any feasible
*     variables to become infeasible (where feasibility is measured
*     by the current featol).
*     ------------------------------------------------------------------
      if ( hitlow ) then
         bound = bl(jhit)
      else
         bound = bu(jhit)
      end if

      unbndd = abs( bound ) .ge. bigbnd
      if (unbndd) go to 900

      stepmn = tolinc*featlu(jhit)/ abs( atp )
      exact  = (bound - atx)      /      atp
      alfa   = max( stepmn, exact )
      onbnd  = alfa  .eq. exact
      move   = exact .ge. stepmn
      if (.not. move) ndegen = ndegen + 1

      return
*     ------------------------------------------------------------------
*     Unbounded.
*     ------------------------------------------------------------------
  900 alfa   = bigalf
      move   = .true.
      onbnd  = .false.

*     end of  cmchzr.
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmdgen( job, msglvl,
     $                   n, nclin, nmoved, iter, numinf,
     $                   istate, bl, bu, featol, featlu, x )

      implicit           double precision(a-h,o-z)

      character*1        job
      integer            istate(n+nclin)
      double precision   x(n)
      double precision   bl(n+nclin), bu(n+nclin)
      double precision   featol(n+nclin), featlu(n+nclin)

*     ==================================================================
*     cmdgen performs most of the manoeuvres associated with degeneracy.
*     the degeneracy-resolving strategy operates in the following way.
*
*     Over a cycle of iterations, the feasibility tolerance featol
*     increases slightly (from tolx0 to tolx1 in steps of tolinc).
*     this ensures that all steps taken will be positive.
*
*     After kdegen consecutive iterations, variables within
*     featol of their bounds are set exactly on their bounds and x is
*     recomputed to satisfy the general constraints in the working set.
*     Featol is then reduced to tolx0 for the next cycle of iterations.
*
*     featlu  is the array of user-supplied feasibility tolerances.
*     featol  is the array of current feasibility tolerances.
*
*     If job = 'i', cmdgen initializes the parameters in
*     common block sol3lc:
*
*     tolx0   is the minimum (scaled) feasibility tolerance.
*     tolx1   is the maximum (scaled) feasibility tolerance.
*     tolinc  is the scaled increment to the current featol.
*     idegen  is the expand frequency. It is the frequency of resetting
*             featol to (scaled) tolx0.
*     kdegen  (specified by the user) is the initial value of idegen.
*     ndegen  counts the number of degenerate steps (incremented
*             by cmchzr).
*     itnfix  is the last iteration at which a job 'e' or 'o' entry
*             caused an x to be put on a constraint.
*     nfix(j) counts the number of times a job 'o' entry has
*             caused the variables to be placed on the working set,
*             where j=1 if infeasible, j=2 if feasible.
*
*     tolx0*featlu and tolx1*featlu are both close to the feasibility
*     tolerance featlu specified by the user.  (They must both be less
*     than featlu.)
*
*
*     If job = 'e',  cmdgen has been called after a cycle of kdegen
*     iterations.  Constraints in the working set are examined to see if
*     any are off their bounds by an amount approaching featol.  Nmoved
*     returns how many.  If nmoved is positive,  x  is moved onto the
*     constraints in the working set.  It is assumed that the calling
*     routine will then continue iterations.
*
*
*     If job = 'o',  cmdgen is being called after a subproblem has been
*     judged optimal, infeasible or unbounded.  Constraint violations
*     are examined as above.
*
*     Cmdgen is based on
*
*     19-Apr-1988. Original version based on MINOS routine m5dgen.
*     09-Apr-1994. Expand frequency allowed to expand. This allows
*                  small initial values of kdegen.
*     28-Jul-1994. Current version.
*     ==================================================================
      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      common    /sol3lc/ tolx0 , tolinc, idegen, kdegen, ndegen,
     $                   itnfix, nfix(2)
      save               tolx1 , tolz

      parameter        ( zero  = 0.0d+0, point6 =0.6d+0 )

      nmoved = 0
      if (job .eq. 'i'  .or. job .eq. 'I') then
*        ---------------------------------------------------------------
*        Job = 'Initialize'.
*        Initialize at the start of each linear problem.
*        kdegen  is the expand frequency      and
*        featlu  are the user-supplied feasibility tolerances.
*        They are not changed.
*        ---------------------------------------------------------------
         epsmch = wmach( 3)

         ndegen = 0
         itnfix = 0
         nfix(1)= 0
         nfix(2)= 0
         tolx0  = 0.5d+0
         tolx1  = 0.99d+0
         tolz   = epsmch**point6

         idegen = kdegen
         if (kdegen .lt. 9999999) then
            tolinc = (tolx1 - tolx0) / idegen
         else
            tolinc = zero
         end if

         do 100, j = 1, n+nclin
            featol(j) = tolx0 * featlu(j)
  100    continue
      else
*        ---------------------------------------------------------------
*        Job = 'end of cycle' or 'optimal'.
*        Initialize local variables maxfix and tolz.
*        ---------------------------------------------------------------
         maxfix = 2

         if (job .eq. 'o'  .or. job .eq. 'O') then
*           ------------------------------------------------------------
*           Job = 'optimal'.
*           Return with nmoved = 0 if the last call was at the same
*           iteration,  or if there have already been maxfix calls with
*           the same state of feasibility.
*           ------------------------------------------------------------
            if (itnfix .eq. iter  ) return
            if (numinf .gt.   0   ) then
               j = 1
            else
               j = 2
            end if

            if (nfix(j).ge. maxfix) return
            nfix(j) = nfix(j) + 1
         end if

*        Increase the expand frequency.
*        Reset featol to its minimum value.

         idegen = idegen + 10
         if (kdegen .lt. 9999999) then
            tolinc = (tolx1 - tolx0) / idegen
            idegen = idegen + iter
         else
            tolinc = zero
         end if

         do 250, j = 1, n+nclin
            featol(j) = tolx0 * featlu(j)
  250    continue

*        Count the number of times a variable is moved a nontrivial
*        distance onto its bound.

         itnfix =   iter

         do 300, j = 1, n
            is     = istate(j)
            if (is .gt. 0  .and.  is .lt. 4) then
               if (is .eq. 1) then
                  d   = abs(x(j) -  bl(j))
               else
                  d   = abs(x(j) -  bu(j))
               end if

               if (d .gt. tolz) nmoved = nmoved + 1
            end if
  300    continue

         if (nmoved .gt. 0) then

*           Some variables were moved onto their bounds.

            if (iPrint .gt. 0) write( iPrint, 1000 ) iter, nmoved
            if (iSumm  .gt. 0) write( iSumm , 1000 ) iter, nmoved
         end if
      end if

 1000 format(' Itn', i6, ' --', i7,
     $       '  variables moved to their bounds.')

*     end of cmdgen
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmfeas( n, nclin, istate,
     $                   bigbnd, nviol, jmax, errmax,
     $                   Ax, bl, bu, featol, x )

      implicit           double precision(a-h,o-z)
      integer            istate(n+nclin)
      double precision   Ax(*), bl(n+nclin), bu(n+nclin)
      double precision   featol(n+nclin), x(n)

*     ==================================================================
*     cmfeas  checks the residuals of the constraints that are believed
*     to be feasible.  The number of constraints violated by more than
*     featol is computed, along with the maximum constraint violation.
*
*     Original version written by PEG,   April    1984.
*     This version of  cmfeas  dated  30-Jun-1988.
*     ==================================================================
      parameter        ( zero = 0.0d+0 )

      biglow = - bigbnd
      bigupp =   bigbnd

*     ==================================================================
*     Compute the number of constraints (nviol) violated by more than
*     featol and  the maximum constraint violation (errmax).
*     (The residual of a constraint in the working set is treated as if
*     it were an equality constraint fixed at that bound.)
*     ==================================================================
      nviol  = 0
      jmax   = 0
      errmax = zero

      do 200, j = 1, n+nclin
         is     = istate(j)

         if (is .ge. 0) then
            feasj  = featol(j)

            if (j .le. n) then
               con =  x(j)
            else
               con = Ax(j-n)
            end if

*           Check for constraint violations.

            if (bl(j) .gt. biglow) then
               res    = bl(j) - con
               if (res .gt.   feasj ) then
                  nviol  = nviol  + 1
                  go to 190
               end if
            end if

            if (bu(j) .lt. bigupp) then
               res    = bu(j) - con
               if (res .lt. (-feasj)) then
                  nviol  =   nviol + 1
                  res    = - res
                  go to 190
               end if
            end if

*           This constraint is satisfied,  but count a large residual
*           as a violation if the constraint is in the working set.

            res   = zero

            if      (is .eq. 1) then
               res = abs( bl(j) - con )

            else if (is .eq. 2) then
               res = abs( bu(j) - con )

            else if (is .eq. 3) then
               res = abs( bu(j) - con )
            end if

            if (res .gt. feasj ) nviol  = nviol  + 1

  190       if (res .gt. errmax) then
               jmax   = j
               errmax = res
            end if
         end if
  200 continue

*     end of cmfeas
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cminit( nerror, msglvl, start,
     $                   liwork, lwork, litotl, lwtotl,
     $                   n, nclin, ncnln,
     $                   istate, named, names,
     $                   bigbnd, bl, bu, clamda, x )

      implicit           double precision(a-h,o-z)
      character*4        start
      character*16       names(*)
      logical            named
      integer            istate(n+nclin+ncnln)
      double precision   bl(n+nclin+ncnln), bu(n+nclin+ncnln), x(n)
      double precision   clamda(n+nclin+ncnln)

*     ==================================================================
*     cminit   checks the data.
*
*     First version written by PEG,  15-Nov-1990.
*     This version of cminit dated   13-Jul-94.
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      parameter         (zero = 0.0d+0)
      logical            ok

      character*5        id(3)
      data                id(1)   ,  id(2)   ,  id(3)
     $                 / 'varbl'  , 'lncon'  , 'nlcon'   /

      nerror = 0

*     ------------------------------------------------------------------
*     Check nclin and ncnln.
*     ------------------------------------------------------------------
      if (nclin .lt. 0  .or.  ncnln .lt. 0) then
         if (nclin .lt. 0) then
            nerror = nerror + 1
            if (iPrint .gt. 0) write (iPrint, 1200) nclin
            if (iSumm  .gt. 0) write (iSumm , 1200) nclin
         end if

         if (ncnln .lt. 0) then
            nerror = nerror + 1
            if (iPrint .gt. 0) write (iPrint, 1210) ncnln
            if (iSumm  .gt. 0) write (iSumm , 1210) ncnln
         end if
         return
      end if

*     ------------------------------------------------------------------
*     Check that there is enough workspace to solve the problem.
*     ------------------------------------------------------------------
      ok     = litotl .le. liwork  .and.  lwtotl .le. lwork
      if (.not. ok)  then
         nerror = nerror + 1
         if (iPrint .gt. 0) then
            write (iPrint, 1100) liwork, lwork, litotl, lwtotl
            write (iPrint, 1110)
         end if

         if (iSumm  .gt. 0) then
            write (iSumm , 1100) liwork, lwork, litotl, lwtotl
            write (iSumm , 1110)
         end if

      else if (msglvl .gt. 0)  then
         if (iPrint .gt. 0) then
            write (iPrint, 1100) liwork, lwork, litotl, lwtotl
         end if
      end if

*     ------------------------------------------------------------------
*     Check the bounds on all variables and constraints.
*     ------------------------------------------------------------------
      do 200, j = 1, n+nclin+ncnln
         b1     = bl(j)
         b2     = bu(j)
         ok     = b1 .lt. b2  .or.
     $            b1 .eq. b2  .and.  abs(b1) .lt. bigbnd

         if (.not. ok)  then
            nerror = nerror + 1
            if (j .gt. n+nclin)  then
               k = j - n - nclin
               l = 3
            else if (j .gt. n)  then
               k = j - n
               l = 2
            else
               k = j
               l = 1
            end if

            if ( named ) then
               if (b1 .eq. b2) then
                  if (iPrint .gt. 0)
     $               write (iPrint, 1310) names(j), b1, bigbnd
                  if (iSumm  .gt. 0)
     $               write (iSumm , 1310) names(j), b1, bigbnd
               else
                  if (iPrint .gt. 0)
     $               write (iPrint, 1315) names(j), b1, b2
                  if (iSumm  .gt. 0)
     $               write (iSumm , 1315) names(j), b1, b2
               end if
            else
               if (b1 .eq. b2) then
                  if (iPrint .gt. 0)
     $               write (iPrint, 1300) id(l), k, b1, bigbnd
                  if (iSumm  .gt. 0)
     $               write (iSumm , 1300) id(l), k, b1, bigbnd
               else
                  if (iPrint .gt. 0)
     $               write (iPrint, 1305) id(l), k, b1, b2
                  if (iSumm  .gt. 0)
     $               write (iSumm , 1305) id(l), k, b1, b2
               end if
            end if
         end if
  200 continue

*     ------------------------------------------------------------------
*     Check  istate.
*     ------------------------------------------------------------------
      if (start .eq. 'warm'  .or.  start .eq. 'hot ') then
         do 420, j = 1, n+nclin+ncnln
            is     = istate(j)
            ok     = is .ge. (- 2)   .and.   is .le. 4
            if (.not. ok)  then
               nerror = nerror + 1
               if (iPrint .gt. 0) write (iPrint, 1500) j, is
               if (iSumm  .gt. 0) write (iSumm , 1500) j, is
            end if
  420    continue

         if (nerror .eq. 0) then
            do 430, i = 1, ncnln
               j      = n + nclin + i
               is     = istate(j)
               cmul   = clamda(j)

               if      (is .eq. 0) then
                  cmul = zero

               else if (is .eq. 1) then
                  if (bl(j) .le. -bigbnd) is = 0
                  if (cmul  .lt.  zero  .or.  is .eq. 0) cmul = zero

               else if (is .eq. 2) then
                  if (bu(j) .ge.  bigbnd) is = 0
                  if (cmul  .gt.  zero  .or.  is .eq. 0) cmul = zero

               else if (is .eq. 3) then
                  if (bl(j) .lt.   bu(j)) is = 0
               end if

               istate(j) = is
               clamda(j) = cmul
  430       continue
         end if
      end if

      return

 1100 format(/ ' Workspace provided is     iw(', i8,
     $         '),  w(', i8, ').' /
     $         ' To solve problem we need  iw(', i8,
     $         '),  w(', i8, ').')
 1110 format(/ ' XXX  Not enough workspace to solve problem.')
 1200 format (/' XXX  nclin  is out of range...', i10)
 1210 format (/' XXX  ncnln  is out of range...', i10)
 1300 format(/ ' XXX  The equal bounds on  ', a5, i3,
     $         '  are infinite.   Bounds =', g16.7,
     $         '  bigbnd =', g16.7)
 1305 format(/ ' XXX  The bounds on  ', a5, i3,
     $         '  are inconsistent.   bl =', g16.7, '   bu =', g16.7)
 1310 format(/ ' XXX  The equal bounds on  ', a16,
     $         '  are infinite.   Bounds =', g16.7,
     $         '  bigbnd =', g16.7)
 1315 format(/ ' XXX  The bounds on  ', a16,
     $         '  are inconsistent.   bl =', g16.7, '   bu =', g16.7)
 1500 format(/ ' XXX  Component', i5, '  of  istate  is out of',
     $         ' range...', i10)

*     end of cminit
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmmsg1( subr, msg, v, lenv )

      character*6        subr
      character*(*)      msg
      double precision   v(*)

*     ==================================================================
*     cmmsg1  prints the array v in debug format.
*
*     Original version dated 17-Jul-1987.
*     This version of  cmmsg1  dated  31-Jan-1988.
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/

      if (lenv .le. 0) then
         write (iPrint, 1000) subr, msg
      else
         write (iPrint, 1100) subr, msg, (v(i), i = 1, lenv)
      end if

      return

 1000 format(/ ' //', a6, '//  ', a )
 1100 format(/ ' //', a6, '//  ', a, ' ... ' / (1p, 5e15.5))

*     end of cmmsg1
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmmul1( prbtyp, msglvl,
     $                   n     , ldA   , ldT   ,
     $                   nactiv, nfree , nz    ,
     $                   istate, kactiv, kx    ,
     $                   zerolm, notopt, numinf,
     $                   trusml, smllst, jsmlst, ksmlst,
     $                           tinyst, jtiny , jinf  ,
     $                   trubig, biggst, jbigst, kbigst,
     $                   A     , anorms, gq    ,
     $                   rlamda, T     , wtinf )

      implicit           double precision(a-h,o-z)
      character*2        prbtyp
      integer            istate(*), kactiv(n), kx(n)
      double precision   A(ldA,*), anorms(*),
     $                   gq(n), rlamda(n), T(ldT,*), wtinf(*)

*     ==================================================================
*     cmmul1  first computes the Lagrange multiplier estimates for the
*     given working set.  It then determines the values and indices of
*     certain significant multipliers.  In this process, the multipliers
*     for inequalities at their upper bounds are adjusted so that a
*     negative multiplier for an inequality constraint indicates non-
*     optimality.  All adjusted multipliers are scaled by the 2-norm
*     of the associated constraint row.  In the following, the term
*     minimum refers to the ordering of numbers on the real line,  and
*     not to their magnitude.
*
*     jsmlst          is the index of the constraint whose multiplier is
*                     the minimum of the set of adjusted multipliers
*                     with values less than  small.
*     rlamda(ksmlst)  is the associated multiplier.
*
*     jbigst          is the index of the constraint whose multiplier is
*                     the largest of the set of adjusted multipliers with
*                     values greater than (1 + small).
*     rlamda(kbigst)  is the associated multiplier.
*
*     On exit,  elements  1  thru  nactiv  of  rlamda  contain the
*     unadjusted multipliers for the general constraints.  Elements
*     nactiv  onwards of  rlamda  contain the unadjusted multipliers
*     for the bounds.
*
*     Original version written 31-October-1984.
*     Based on a version of  lsmuls  dated 30-June-1986.
*     This version of  cmmul1  dated 14-Sep-92.
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/

      parameter         (one = 1.0d+0)

      nfixed =   n - nfree

      jtiny  =   0
      jsmlst =   0
      ksmlst =   0

      jbigst =   0
      kbigst =   0

*     ------------------------------------------------------------------
*     Compute  jsmlst  for regular constraints and temporary bounds.
*     ------------------------------------------------------------------
*     First, compute the Lagrange multipliers for the general
*     constraints in the working set, by solving  T'*lamda = Y'g.

      if (n .gt. nz)
     $   call dcopy ( n-nz, gq(nz+1), 1, rlamda, 1 )
      if (nactiv .gt. 0)
     $    call dtrsv ( 'U', 'T', 'N', nactiv, T(1,nz+1), ldT,
     $                 rlamda, 1 )

*     -----------------------------------------------------------------
*     Now set elements  nactiv, nactiv+1,... of  rlamda  equal to
*     the multipliers for the bound constraints.
*     -----------------------------------------------------------------
      do 190, l = 1, nfixed
         j     = kx(nfree+l)
         blam  = rlamda(nactiv+l)
         do 170, k = 1, nactiv
            i    = kactiv(k)
            blam = blam - A(i,j)*rlamda(nactiv-k+1)
  170    continue
         rlamda(nactiv+l) = blam
  190 continue

*     -----------------------------------------------------------------
*     Find  jsmlst  and  ksmlst.
*     -----------------------------------------------------------------
      do 330, k = 1, n-nz
         if (k .gt. nactiv) then
            j  = kx(nz+k)
         else
            j  = kactiv(nactiv-k+1) + n
         end if

         is   = istate(j)

         i    = j - n
         if (j .le. n) anormj = one
         if (j .gt. n) anormj = anorms(i)

         rlam = rlamda(k)

*        Change the sign of the estimate if the constraint is in
*        the working set at its upper bound.

         if (is .eq. 2) rlam =      - rlam
         if (is .eq. 3) rlam =   abs( rlam )
         if (is .eq. 4) rlam = - abs( rlam )

         if (is .ne. 3) then
            scdlam = rlam*anormj

            if (scdlam .lt. zerolm) then
               if (numinf .eq. 0) notopt = notopt + 1

               if (scdlam .lt. smllst) then
                  smllst = scdlam
                  trusml = rlamda(k)
                  jsmlst = j
                  ksmlst = k
               end if
            else if (scdlam .lt. tinyst) then
               tinyst = scdlam
               jtiny  = j
            end if
         end if

         scdlam = rlam/wtinf(j)
         if (scdlam .gt. biggst  .and.  j .gt. jinf) then
            biggst = scdlam
            trubig = rlamda(k)
            jbigst = j
            kbigst = k
         end if
  330 continue

*     -----------------------------------------------------------------
*     If required, print the multipliers.
*     -----------------------------------------------------------------
      if (msglvl .ge. 20) then
         if (nfixed .gt. 0)
     $      write (iPrint, 1100) prbtyp, (kx(nfree+k),
     $                         rlamda(nactiv+k), k=1,nfixed)
         if (nactiv .gt. 0)
     $      write (iPrint, 1200) prbtyp, (kactiv(k),
     $                         rlamda(nactiv-k+1), k=1,nactiv)
      end if

      return

 1100 format(/ ' Multipliers for the ', a2, ' bound  constraints   '
     $       / 4(i5, 1pe11.2))
 1200 format(/ ' Multipliers for the ', a2, ' linear constraints   '
     $       / 4(i5, 1pe11.2))

*     end of cmmul1
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmmul2( msglvl, n, nrz, nz,
     $                   zerolm, notopt, numinf,
     $                   trusml, smllst, jsmlst,
     $                   tinyst, jtiny , gq )

      implicit           double precision(a-h,o-z)
      double precision   gq(n)

*     ==================================================================
*     cmmul2  updates jsmlst and smllst when there are artificial
*     constraints.
*
*     On input,  jsmlst  is the index of the minimum of the set of
*     adjusted multipliers.
*     On output, a negative jsmlst defines the index in Q'g of the
*     artificial constraint to be deleted.
*
*     Original version written 17-Jan-1988.
*     This version of cmmul2 dated  23-Jul-1991.
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/

      do 100, j = nrz+1, nz
         rlam = - abs( gq(j) )

         if (rlam .lt. zerolm) then
            if (numinf .eq. 0) notopt = notopt + 1

            if (rlam .lt. smllst) then
               trusml =   gq(j)
               smllst =   rlam
               jsmlst = - j
            end if

         else if (rlam .lt. tinyst) then
               tinyst =   rlam
               jtiny  = - j
         end if
  100 continue

      if (msglvl .ge. 20)
     $   write (iPrint, 1000) (gq(k), k=nrz+1,nz)

      return

 1000 format(/ ' Multipliers for the artificial constraints        '
     $       / 4(5x, 1pe11.2))

*     end of cmmul2
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmsetx( rowerr, unitQ,
     $                   nclin, nactiv, nfree, nz,
     $                   n, ldQ, ldA, ldT,
     $                   istate, kactiv, kx,
     $                   jmax, errmax, xnorm,
     $                   A, Ax, bl, bu, featol,
     $                   T, x, Q, p, work )

      implicit           double precision(a-h,o-z)
      logical            rowerr, unitQ
      integer            istate(n+nclin), kactiv(n), kx(n)
      double precision   A(ldA,*), Ax(*), bl(n+nclin), bu(n+nclin)
      double precision   featol(n+nclin), p(n),
     $                   T(ldT,*), x(n), Q(ldQ,*)
      double precision   work(n)

*     ==================================================================
*     cmsetx  computes the point on a working set that is closest in the
*     least-squares sense to the input vector X.
*
*     If the computed point gives a row error of more than the
*     feasibility tolerance, an extra step of iterative refinement is
*     used.  If  X  is still infeasible,  the logical variable ROWERR
*     is set.
*
*     Original version derived from lssetx January-1987.
*     This version of  cmsetx  dated   5-Jul-1989.
*     ==================================================================
      parameter        ( zero  = 0.0d+0, one = 1.0d+0 )
      parameter        ( ntry  = 5 )

*     ------------------------------------------------------------------
*     Move  x  onto the simple bounds in the working set.
*     ------------------------------------------------------------------
      do 100, k = nfree+1, n
          j   = kx(k)
          is  = istate(j)
          bnd = bl(j)
          if (is .ge. 2) bnd  = bu(j)
          if (is .ne. 4) x(j) = bnd
  100 continue

*     ------------------------------------------------------------------
*     Move  x  onto the general constraints in the working set.
*     ntry  attempts are made to get acceptable row errors.
*     ------------------------------------------------------------------
      ktry   = 1
      jmax   = 1
      errmax = zero

*     repeat
  200    if (nactiv .gt. 0) then

*           Set work = residuals for constraints in the working set.
*           Solve for P, the smallest correction to x that gives a point
*           on the constraints in the working set.  Define  P = Y*(py),
*           where  py  solves the triangular system  T*(py) = residuals.

            do 220, i = 1, nactiv
               k   = kactiv(i)
               j   = n + k
               bnd = bl(j)
               if (istate(j) .eq. 2) bnd = bu(j)
               work(nactiv-i+1) = bnd - ddot  ( n, A(k,1), ldA, x, 1 )
  220       continue

            call dtrsv ( 'u', 'n', 'n', nactiv, T(1,nz+1), ldT,
     $                   work, 1 )
            call dload ( n, zero, p, 1 )
            call dcopy ( nactiv, work, 1, p(nz+1), 1 )

            call cmqmul( 2, n, nz, nfree, ldQ, unitQ, kx, p, Q, work )
            call daxpy ( n, one, p, 1, x, 1 )
         end if

*        ---------------------------------------------------------------
*        Compute the 2-norm of  x.
*        Initialize  Ax  for all the general constraints.
*        ---------------------------------------------------------------
         xnorm  = dnrm2 ( n, x, 1 )
         if (nclin .gt. 0)
     $      call dgemv ( 'n', nclin, n, one, A, ldA, x, 1, zero, Ax, 1 )

*        ---------------------------------------------------------------
*        Check the row residuals.
*        ---------------------------------------------------------------
         if (nactiv .gt. 0) then
            do 300, k = 1, nactiv
               i   = kactiv(k)
               j   = n + i
               is  = istate(j)
               if (is .eq. 1) work(k) = bl(j) - Ax(i)
               if (is .ge. 2) work(k) = bu(j) - Ax(i)
  300       continue

            jmax   = idamax( nactiv, work, 1 )
            errmax = abs( work(jmax) )
         end if

         ktry = ktry + 1
*     until    (errmax .le. featol(jmax) .or. ktry .gt. ntry
      if (.not.(errmax .le. featol(jmax) .or. ktry .gt. ntry)) go to 200

      rowerr = errmax .gt. featol(jmax)

*     end of cmsetx
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmsinf( n, nclin, ldA,
     $                   istate, bigbnd, numinf, suminf,
     $                   bl, bu, A, featol,
     $                   cvec, x, wtinf )

      implicit           double precision(a-h,o-z)
      integer            istate(*)
      double precision   bl(*), bu(*), A(ldA,*), featol(*)
      double precision   cvec(n), x(n), wtinf(*)

*     ==================================================================
*     cmsinf  finds the number and weighted sum of infeasibilities for
*     the bounds and linear constraints.   An appropriate gradient
*     is returned in cvec.
*
*     Positive values of  istate(j)  will not be altered.  These mean
*     the following...
*
*               1             2           3
*           a'x = bl      a'x = bu     bl = bu
*
*     Other values of  istate(j)  will be reset as follows...
*           a'x lt bl     a'x gt bu     a'x free
*              - 2           - 1           0
*
*     Original version written 31-October-1984.
*     This version of cmsinf dated  1-January-1987.
*     ==================================================================
      parameter        ( zero = 0.0d+0 )

      bigupp =   bigbnd
      biglow = - bigbnd

      numinf =   0
      suminf =   zero
      call dload ( n, (zero), cvec, 1 )

      do 200, j = 1, n+nclin
         if (istate(j) .le. 0) then
            feasj  = featol(j)
            if (j .le. n) then
               ctx = x(j)
            else
               k   = j - n
               ctx = ddot  ( n, A(k,1), ldA, x, 1 )
            end if
            istate(j) = 0

*           See if the lower bound is violated.

            if (bl(j) .gt. biglow) then
               s = bl(j) - ctx
               if (s     .gt. feasj ) then
                  istate(j) = - 2
                  weight    = - wtinf(j)
                  go to 160
               end if
            end if

*           See if the upper bound is violated.

            if (bu(j) .ge. bigupp) go to 200
            s = ctx - bu(j)
            if (s     .le. feasj ) go to 200
            istate(j) = - 1
            weight    =   wtinf(j)

*           Add the infeasibility.

  160       numinf = numinf + 1
            suminf = suminf + abs( weight ) * s
            if (j .le. n) then
               cvec(j) = weight
            else
               call daxpy ( n, weight, A(k,1), ldA, cvec, 1 )
            end if
         end if
  200 continue

*     end of cmsinf
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine cmwrap( needLM, nfree, ldA,
     $                   n, nclin, nctotl,
     $                   nactiv, istate, kactiv, kx,
     $                   A, bl, bu, c, clamda, featol, r, rlamda, x )

      implicit           double precision(a-h,o-z)

      logical            needLM
      integer            istate(nctotl), kactiv(n), kx(n)
      double precision   A(ldA,*), bl(nctotl), bu(nctotl), c(*),
     $                   clamda(nctotl),featol(nctotl), r(nctotl)
      double precision   rlamda(n), x(n)

*     ==================================================================
*     cmwrap  creates the expanded Lagrange multiplier vector clamda.
*     and resets istate for the printed solution.
*
*     This version of cmwrap is for upper-triangular T.
*
*     For npopt, qpopt and lpopt, kactiv holds the general constraint
*     indices in the order in which they were added to the working set.
*     The reverse ordering is used for T since new rows are added at
*     the front of T.
*
*     Original Fortran 77 version written  05-May-93.
*     This version of  cmwrap  dated  09-Oct-95.
*     ==================================================================
      parameter         (zero  = 0.0d+0)

      nfixed = n     - nfree
      nplin  = n     + nclin
      nZ     = nfree - nactiv

      if ( needLM ) then

*        Expand multipliers for bounds and linear constraints
*        into the  clamda  array.  This will have been done
*        when wrapping up an SQP iteration.

         call dload ( nctotl, zero, clamda, 1 )
         do 150,  k = 1, nactiv+nfixed
            if (k .le. nactiv) then
               j    = kactiv(k) + n
               rlam = rlamda(nactiv-k+1)
            else
               j    = kx(nz+k)
               rlam = rlamda(k)
            end if
            clamda(j) = rlam
  150    continue
      end if

*     Reset istate if necessary.

      do 500, j = 1, nctotl
         b1     = bl(j)
         b2     = bu(j)

         if (j .le. n) then
            rj  = x(j)
         else if (j .le. nplin) then
            i   = j - n
            rj  = ddot  ( n, A(i,1), ldA, x, 1 )
         else
            i   = j - nplin
            rj  = c(i)
         end if

         is     = istate(j)
         slk1   = rj - b1
         slk2   = b2 - rj
         tol    = featol(j)
         if (                  slk1 .lt. -tol) is = - 2
         if (                  slk2 .lt. -tol) is = - 1
         if (is .eq. 1  .and.  slk1 .gt.  tol) is =   0
         if (is .eq. 2  .and.  slk2 .gt.  tol) is =   0
         istate(j) = is
         r(j)      = rj
  500 continue

*     end of cmwrap
      end

