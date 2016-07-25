*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     File  rzsubs.f
*
*     rzadd    rzadds   rzdel
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine rzadd ( unitQ, Rset,
     $                   inform, ifix, iadd, jadd, iT,
     $                   nactiv, nZ, nfree, nZr, ngq,
     $                   n, ldA, ldQ, ldR, ldT,
     $                   kx, condmx, Dzz,
     $                   A, R, T, gqm, Q,
     $                   w, c, s )

      implicit           double precision(a-h,o-z)
      logical            unitQ, Rset
      integer            kx(n)
      double precision   A(ldA,*), R(ldR,*), T(ldT,*),
     $                   gqm(n,*), Q(ldQ,*)
      double precision   w(n), c(n), s(n)

*     ==================================================================
*     rzadd   updates the matrices  Z, Y, T, R  and  D  associated with
*     factorizations
*
*              A(free) * Q(free)  = (  0 T )
*                        Q(free)  = (  Z Y )
*                      R' *D * R  =   Hz
*
*     a) The matrices  R  and  T  are upper triangular.
*     b) The arrays  T  and  R  may be the same array.
*     c) The  nactiv x nactiv  upper-triangular matrix  T  is stored
*        with its (1,1) element in position  (iT,jT) of the
*        array  T.   The integer  jT  is always  nZ+1.  During regular
*        changes to the working set,  iT = 1;  when several constraints
*        are added simultaneously,  it  points to the first row of the
*        existing  T.
*     d) The matrix  R  is stored in the first  nZr x nZr  rows
*        and columns of the  nfree x nfree  leading principal submatrix
*        of the array  R.
*     e) If  Rset  is  false,   R  is not touched.
*
*     There are three separate cases to consider (although each case
*     shares code with another)...
*
*     (1) A free variable becomes fixed on one of its bounds when there
*         are already some general constraints in the working set.
*
*     (2) A free variable becomes fixed on one of its bounds when there
*         are only bound constraints in the working set.
*
*     (3) A general constraint (corresponding to row  iadd  of  A) is
*         added to the working set.
*
*     In cases (1) and (2), we assume that  kx(ifix) = jadd.
*     In all cases,  jadd  is the index of the constraint being added.
*
*     If there are no general constraints in the working set,  the
*     matrix  Q = (Z Y)  is the identity and will not be touched.
*
*     If  ngq .gt. 0,  the column transformations are applied to the
*     columns of the  (ngq x n)  matrix  gqm'.
*
*     Original version of  rzadd  written by PEG,  31-October-1984.
*     This version of  rzadd  dated  28-Aug-1991.
*     ==================================================================
      common    /sol1cm/ iPrint, iSumm , lines1, lines2
      save      /sol1cm/
      common    /sol4cm/ epspt3, epspt5, epspt8, epspt9
      common    /sol5cm/ Asize, dTmax, dTmin

      logical            bound , overfl
      parameter         (zero = 0.0d+0, one = 1.0d+0)

*     If the condition estimator of the updated T is greater than
*     condbd,  a warning message is printed.

      condbd = one / epspt9

      overfl = .false.
      bound  = jadd .le. n
      jT     = nZ + 1

      if (bound) then
*        ===============================================================
*        A simple bound has entered the working set.  iadd is not used.
*        ===============================================================
         nanew = nactiv

         if (unitQ) then

*           Q is not stored, but  kx  defines an ordering of the columns
*           of the identity matrix that implicitly define Q.
*           Define the sequence of pairwise interchanges P that moves
*           the newly-fixed variable to position  nfree.
*           Reorder  kx  accordingly.

            do 100, i = 1, nfree-1
               if (i .ge. ifix) then
                  w (i) = i + 1
                  kx(i) = kx(i+1)
               else
                  w(i) = i
               end if
  100       continue
         else
*           ------------------------------------------------------------
*           Q  is stored explicitly.
*           ------------------------------------------------------------
*           Set  w = the  (ifix)-th  row of  Q.
*           Move the  (nfree)-th  row of  Q  to position ifix.

            call dcopy ( nfree, Q(ifix,1), ldQ, w, 1 )
            if (ifix .lt. nfree) then
               call dcopy ( nfree, Q(nfree,1), ldQ, Q(ifix,1), ldQ )
               kx(ifix) = kx(nfree)
            end if
         end if
         kx(nfree) = jadd
      else
*        ===============================================================
*        A general constraint has entered the working set.
*        ifix is not used.
*        ===============================================================
         nanew  = nactiv + 1

*        Transform the incoming row of A by Q'.

         call dcopy ( n, A(iadd,1), ldA, w, 1 )
         call cmqmul( 8, n, nZ, nfree, ldQ, unitQ, kx, w, Q, c )

*        Check that the incoming row is not dependent upon those
*        already in the working set.

         dTnew  = dnrm2 ( nZ, w, 1 )
         if (nactiv .eq. 0) then

*           This is the only general constraint in the working set.

            cond   = ddiv  ( Asize, dTnew, overfl )
            tdTmax = dTnew
            tdTmin = dTnew
         else

*           There are already some general constraints in the working
*           set.  Update the estimate of the condition number.

            tdTmax = max( dTnew, dTmax )
            tdTmin = min( dTnew, dTmin )
            cond   = ddiv  ( tdTmax, tdTmin, overfl )
         end if

         if (cond .gt. condmx  .or.  overfl) go to 900

         if (unitQ) then

*           First general constraint added.  Set  Q = I.

            call f06qhf( 'general', nfree, nfree, zero, one, Q, ldQ )
            unitQ  = .false.
            iT     = 0
         end if
      end if

      if (bound) then
         npiv  = nfree
      else
         npiv  = nZ
      end if

      if (unitQ) then
*        ---------------------------------------------------------------
*        The orthogonal matrix  Q  is not stored explicitly.
*        Apply  P, the sequence of pairwise interchanges that moves the
*        newly-fixed variable to position  nfree.
*        ---------------------------------------------------------------
         if (ngq .gt. 0)
     $      call f06qkf( 'left', 'transpose', nfree-1, w, ngq, gqm, n )

         if (Rset) then

*           Apply the pairwise interchanges to  Rz.
*           The subdiagonal elements generated by this process are
*           stored in  s(ifix), s(2), ..., s(nZr-1).

            nsup = nZr - ifix
            call f06qnf( 'right', nZr, ifix, nZr, s, R, ldR )
         end if
      else
*        ---------------------------------------------------------------
*        The matrix  Q  is stored explicitly.
*        Define a sweep of plane rotations P such that
*                           Pw = beta*e(npiv).
*        The rotations are applied in the planes (1, 2), (2, 3), ...,
*        (npiv-1, npiv).  The rotations must be applied to Q, gqm', R
*        and T.
*        ---------------------------------------------------------------
         call f06fqf( 'varble', 'forwrds', npiv-1, w(npiv), w, 1, c, s )

         if (ngq .gt. 0)
     $      call f06qxf( 'left ', 'variable', 'forwards', npiv , ngq,
     $                   1, npiv, c, s, gqm, n )
         call f06qxf( 'right', 'variable', 'forwards', nfree, nfree,
     $                1, npiv, c, s, Q, ldQ )

         if (Rset) then

*           Apply the rotations to the triangular part of R.
*           The subdiagonal elements generated by this process are
*           stored in  s(1),  s(2), ..., s(nZr-1).

            nsup = nZr - 1
            call f06qvf( 'right', nZr, 1, nZr, c, s, R, ldR )
         end if
      end if

      if (Rset) then
*        ---------------------------------------------------------------
*        Eliminate the  nsup  subdiagonal elements of  R  stored in
*        s(nZr-nsup), ..., s(nZr-1)  with a left-hand sweep of rotations
*        in planes (nZr-nsup, nZr-nsup+1), ..., (nZr-1, nZr).
*        ---------------------------------------------------------------
         call f06qrf( 'left ', nZr, nZr-nsup, nZr, c, s, R, ldR )

         if (nsup .gt. 0  .and.  Dzz .ne. one) then
            Dzz = c(nZr-1)**2 + Dzz*s(nZr-1)**2
         end if
      end if

      if (.not. unitQ) then
         if (bound) then
*           ------------------------------------------------------------
*           Bound constraint added.   The rotations affect columns
*           nZ+1  thru  nfree  of  gqm'  and  T.
*           ------------------------------------------------------------
*           The last row and column of  Q  has been transformed to plus
*           or minus the unit vector  e(nfree).  We can reconstitute the
*           column of gqm' corresponding to the new fixed variable.

            if (w(nfree) .lt. zero) then
               if (ngq .gt. 0)
     $            call dscal ( ngq, -one, gqm(nfree,1), n )
            end if

            if (nactiv .gt. 0) then
               T(iT,jT-1) = s(jT-1)*T(iT,jT)
               T(iT,jT  ) = c(jT-1)*T(iT,jT)

               if (nactiv .gt. 1) then
                  call f06qvf( 'right', nactiv, 1, nactiv,
     $                         c(jT), s(jT), T(iT,jT), ldT )
                  call dcopy ( nactiv-1, s(jT), 1, T(iT+1,jT), ldT+1 )
               end if

               jT = jT - 1
               call dcond ( nactiv, T(iT,jT), ldT+1, tdTmax, tdTmin )
               cond = ddiv  ( tdTmax, tdTmin, overfl )
            end if
         else
*           ------------------------------------------------------------
*           General constraint added.  Install  w  at the front of  T.
*           If there is no room,  shift all the rows down one position.
*           ------------------------------------------------------------
            iT = iT - 1
            if (iT .le. 0)  then
               iT = 1
               do 210, k = 1, nactiv
                  j = jT + k - 1
                  do 200, i = k, 1, -1
                     T(i+1,j) = T(i,j)
  200             continue
  210          continue
            end if
            jT = jT - 1
            call dcopy ( nanew, w(jT), 1, T(iT,jT), ldT )
         end if
      end if

*     ==================================================================
*     Prepare to exit.  Check the magnitude of the condition estimator.
*     ==================================================================
  900 if (nanew .gt. 0) then
         if (cond .lt. condmx  .and.  .not. overfl) then

*           The factorization has been successfully updated.

            inform = 0
            dTmax  = tdTmax
            dTmin  = tdTmin
            if (cond .ge. condbd  .and.  iPrint .gt. 0)
     $         write (iPrint, 2000) jadd
         else

*           The proposed working set appears to be linearly dependent.

            inform = 1
         end if
      end if

      return

 2000 format(/ ' XXX  Serious ill-conditioning in the working set',
     $         ' after adding constraint ',  i5
     $       / ' XXX  Overflow may occur in subsequent iterations.'//)

*     end of rzadd
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine rzadds( unitQ, vertex,
     $                   k1, k2, iT, nactiv, nartif, nZ, nfree,
     $                   nrejtd, ngq, n, ldQ, ldA, ldT,
     $                   istate, kactiv, kx,
     $                   condmx,
     $                   A, T, gqm, Q, w, c, s )

      implicit           double precision(a-h,o-z)
      logical            unitQ, vertex
      integer            istate(*), kactiv(n), kx(n)
      double precision   A(ldA,*), T(ldT,*), gqm(n,*), Q(ldQ,*)
      double precision   w(n), c(n), s(n)

*     ==================================================================
*     rzadds  includes general constraints  k1  thru  k2  as new rows of
*     the  TQ  factorization:
*              A(free) * Q(free)  = (  0 T )
*                        Q(free)  = (  Z Y )
*
*     a) The  nactiv x nactiv  upper-triangular matrix  T  is stored
*        with its (1,1) element in position  (iT,jT)  of the array  T.
*
*     Original version of  rzadds  written by PEG,  October-31-1984.
*     This version of  rzadds  dated  5-Jul-1991.
*     ==================================================================
      double precision   wmach
      common    /solmch/ wmach(15)
      save      /solmch/
      common    /sol5cm/ Asize, dTmax, dTmin

      logical            overfl, Rset
      parameter        ( zero = 0.0d+0, one = 1.0d+0 )

      rtmax  = wmach(8)

      jT     = nZ + 1

*     Estimate the condition number of the constraints already
*     factorized.

      if (nactiv .eq. 0) then
         dTmax = zero
         dTmin = one
         if (unitQ) then

*           First general constraint added.  Set  Q = I.

            call f06qhf( 'general', nfree, nfree, zero, one, Q, ldQ )
            unitQ  = .false.
         end if
      else
         call dcond ( nactiv, T(iT,jT), ldT+1, dTmax, dTmin )
      end if

      do 600, k = k1, k2
         iadd = kactiv(k)
         jadd = n + iadd
         if (nactiv .lt. nfree) then
            overfl = .false.

*           Transform the incoming row of  A  by  Q'.

            call dcopy ( n, A(iadd,1), ldA, w, 1 )
            call cmqmul( 8, n, nZ, nfree, ldQ, unitQ, kx, w, Q, s )

*           Check that the incoming row is not dependent upon those
*           already in the working set.

            dTnew  = dnrm2 ( nZ, w, 1 )
            if (nactiv .eq. 0) then

*              This is the first general constraint in the working set.

               cond   = ddiv  ( Asize, dTnew, overfl )
               tdTmax = dTnew
               tdTmin = dTnew
            else

*              There are already some general constraints in the working
*              set. Update the estimate of the condition number.

               tdTmax = max( dTnew, dTmax )
               tdTmin = min( dTnew, dTmin )
               cond   = ddiv  ( tdTmax, tdTmin, overfl )
            end if

            if (cond .ge. condmx  .or.  overfl) then
*              ---------------------------------------------------------
*              This constraint appears to be dependent on those already
*              in the working set.  Skip it.
*              ---------------------------------------------------------
               istate(jadd) =   0
               kactiv(k)    = - kactiv(k)
            else
               if (nZ .gt. 1) then
*                 ------------------------------------------------------
*                 Use a single column transformation to reduce the first
*                 nZ-1  elements of  w  to zero.
*                 ------------------------------------------------------
*                 Apply the Householder reflection  I  -  w w'.
*                 The reflection is applied to  Z  and gqm so that
*                    y  =    Z  * w,   Z    =  Z    -  y w'  and
*                    y  =  gqm' * w,   gqm  =  gqm  -  w y',
*                 where  w = wrk1 (from Householder),
*                 and    y = wrk2 (workspace).
*
*                 Note that delta  has to be stored after the reflection
*                 is used.

                  delta = w(nZ)
                  call dgrfg ( nZ-1, delta, w, 1, zero, w(nZ) )
                  if (w(nZ) .gt. zero) then

                     call dgemv ( 'n', nfree, nZ, one, Q, ldQ, w, 1,
     $                            zero, s, 1 )
                     call dger  ( nfree, nZ, (-one),
     $                            s, 1, w, 1, Q, ldQ )

                     if (ngq .gt. 0) then
                        call dgemv ( 't', nZ, ngq, one, gqm, n, w, 1,
     $                               zero, s, 1 )
                        call dger  ( nZ, ngq, (-one),
     $                               w, 1, s, 1, gqm, n )
                     end if
                  end if

                  w(nZ) = delta
               end if
               iT     = iT     - 1
               jT     = jT     - 1
               nactiv = nactiv + 1
               nZ     = nZ     - 1
               call dcopy ( nactiv, w(jT), 1, T(iT,jT), ldT )
               dTmax  = tdTmax
               dTmin  = tdTmin
            end if
         end if
  600 continue

      if (nactiv .lt. k2) then

*        Some of the constraints were classed as dependent and not
*        included in the factorization.  Re-order the part of  kactiv
*        that holds the indices of the general constraints in the
*        working set.  Move accepted indices to the front and shift
*        rejected indices (with negative values) to the end.

         l      = k1 - 1
         do 700, k = k1, k2
            i         = kactiv(k)
            if (i .ge. 0) then
               l      = l + 1
               if (l .ne. k) then
                  iswap     = kactiv(l)
                  kactiv(l) = i
                  kactiv(k) = iswap
               end if
            end if
  700    continue

*        If a vertex is required,  add some temporary bounds.
*        We must accept the resulting condition number of the working
*        set.

         if (vertex) then
            Rset  = .false.
            cndmax = rtmax
            Dzz    = one
            nZadd  = nZ
            do 720, iartif = 1, nZadd
               if (unitQ) then
                  ifix = nfree
                  jadd = kx(ifix)
               else
                  rowmax = zero
                  do 710, i = 1, nfree
                     rnorm = dnrm2 ( nZ, Q(i,1), ldQ )
                     if (rowmax .lt. rnorm) then
                        rowmax = rnorm
                        ifix   = i
                     end if
  710             continue
                  jadd = kx(ifix)

                  call rzadd ( unitQ, Rset,
     $                         inform, ifix, iadd, jadd, iT,
     $                         nactiv, nZ, nfree, nZ, ngq,
     $                         n, ldA, ldQ, ldT, ldT,
     $                         kx, cndmax, Dzz,
     $                         A, T, T, gqm, Q, w, c, s )
               end if
               nfree  = nfree  - 1
               nZ     = nZ     - 1
               nartif = nartif + 1
               istate(jadd) = 4
  720       continue
         end if

         if (iT .gt. 1) then
*           ------------------------------------------------------------
*           If some dependent constraints were rejected,  move the
*           matrix T  to the top of the array  T.
*           ------------------------------------------------------------
            do 810, k = 1, nactiv
               j = nZ + k
               do 800, i = 1, k
                  T(i,j) = T(iT+i-1,j)
  800          continue
  810       continue
         end if
      end if

      nrejtd = k2 - nactiv

*     end of rzadds
      end

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine rzdel ( unitQ, iT,
     $                   n, nactiv, nfree, ngq, nZ, nZr,
     $                   ldA, ldQ, ldT,
     $                   jdel, kdel, kactiv, kx,
     $                   A, T, gqm, Q, work, c, s )

      implicit           double precision(a-h,o-z)
      logical            unitQ
      integer            kactiv(n), kx(n)
      double precision   A(ldA,*), T(ldT,*), gqm(n,*), Q(ldQ,*)
      double precision   work(n), c(n), s(n)

*     ==================================================================
*     RZDEL   updates the matrices  Z, Y  and  T  associated with the
*             factorizations
*
*              A(free) * Q(free)  = (  0 T )
*                        Q(free)  = (  Z Y )
*
*     when a regular, temporary or artificial constraint is deleted
*     from the working set.
*
*     The  nactiv x nactiv  upper-triangular matrix  T  is stored
*     with its (1,1) element in position  (iT,jT)  of the array  T.
*
*     Original version of  rzdel  written by PEG,  31-October-1984.
*     This version of  rzdel  dated 14-Sep-1992.
*     ==================================================================
      common    /sol5cm/ Asize, dTmax, dTmin
      parameter         (zero = 0.0d+0, one = 1.0d+0)

      jT = nZ + 1

      if (jdel .gt. 0) then

*        Regular constraint or temporary bound deleted.

         if (jdel .le. n) then

*           Case 1.  A simple bound has been deleted.
*           =======  Columns  nfree+1  and  ir  of gqm' must be swapped.

            ir     = nZ    + kdel
            iTdel  = nactiv + 1
            nfree  = nfree  + 1
            if (nfree .lt. ir) then
               kx(ir)    = kx(nfree)
               kx(nfree) = jdel
               call dswap ( ngq, gqm(nfree,1), n, gqm(ir,1), n )
            end if

            if (.not. unitQ) then

*              Copy the incoming column of  A(free)  into the end of T.

               do 130, k = 1, nactiv
                  i = kactiv(k)
                  T(nactiv-k+1,nfree) = A(i,jdel)
  130          continue

*              Expand  Q  by adding a unit row and column.

               if (nfree .gt. 1) then
                  call dload ( nfree-1, zero, Q(nfree,1), ldQ )
                  call dload ( nfree-1, zero, Q(1,nfree), 1    )
               end if
               Q(nfree,nfree) = one
            end if
         else

*           Case 2.  A general constraint has been deleted.
*           =======

*           Delete row  iTdel  of  T  and move up the ones below it.
*           T  becomes lower Hessenberg.

            iTdel = kdel
            do 210, k = iTdel, nactiv
               j  = jT + k - 1
               do 200, l = iTdel, k-1
                  i      = iT + l - 1
                  T(i,j) = T(i+1,j)
  200          continue
  210       continue

            do 220, i = nactiv-iTdel+1, nactiv-1
               kactiv(i) = kactiv(i+1)
  220       continue
            nactiv = nactiv - 1
         end if

         nZ    = nZ + 1

         if (nactiv .eq. 0) then
            dTmax = one
            dTmin = one
         else
*           ------------------------------------------------------------
*           Restore the nactiv x (nactiv+1) upper-Hessenberg matrix  T
*           to upper-triangular form.  The  nsup  super-diagonal
*           elements are removed by a backward sweep of rotations.
*           The rotation for the  (1,1)-th  element of  T  is generated
*           separately.
*           ------------------------------------------------------------
            nsup   = iTdel - 1

            if (nsup .gt. 0) then
               npiv   = jT + iTdel - 1
               if (nsup .gt. 1) then
                  call dcopy ( nsup-1, T(iT+1,jT+1), ldT+1, s(jT+1), 1 )
                  call f06qrf( 'right', nactiv, 1, nsup,
     $                         c(jT+1), s(jT+1), T(iT,jT+1), ldT )
               end if

               call f06baf( T(iT,jT+1), T(iT,jT), cs, sn )
               T(iT,jT) = zero
               s(jT)    = - sn
               c(jT)    =   cs
               call f06qxf( 'right', 'variable', 'backwards',
     $                      nfree, nfree, nZ, npiv, c, s, Q, ldQ )
               call f06qxf( 'left ', 'variable', 'backwards',
     $                      npiv , ngq  , nZ, npiv, c, s, gqm, n   )
            end if

            jT = jT + 1
            call dcond ( nactiv, T(iT,jT), ldT+1, dTmax, dTmin )
         end if
      end if

      nZr1 = nZr + 1

      if (nZ .gt. nZr) then
         if (jdel .gt. 0) then
            jArt =   nZr1 - 1 + idamax( nZ-nZr1+1, gqm(nZr1,1), 1 )
         else
            jArt = - jdel
         end if

         if (jArt .gt. nZr1) then

*           Swap columns  nZr1  and  jArt  of  Q  and  gqm.

            if (unitQ) then
               k        = kx(nZr1)
               kx(nZr1) = kx(jArt)
               kx(jArt) = k
            else
               call dswap ( nfree, Q(1,nZr1), 1, Q(1,jArt), 1 )
            end if

            call dswap ( ngq, gqm(nZr1,1), n, gqm(jArt,1), n )
         end if
      end if

      nZr = nZr1

*     end of rzdel
      end
