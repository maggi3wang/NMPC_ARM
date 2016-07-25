*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*     File qptest.f
*
*     Test cases for subroutine  QPOPT Version 1.0-6.  July 1994.
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      implicit           double precision(a-h,o-z)

*     Set the declared array dimensions.
*     ldA    = the declared row dimension of A.
*     ldH    = the declared row dimension of H.
*     maxn   = maximum no. of variables allowed for.
*     maxm   = maximum no. of observations allowed for.
*     maxbnd = maximum no. of variables + linear constraints.
*     leniw  = the length of the integer work array.
*     lenw   = the length of the double precision work array.

      parameter         (ldA    =  50, ldH   =  10,
     $                   maxn   = 101, maxm  =  10,
     $                   leniw  = 205, lenw  =6500,
     $                   maxbnd = maxn + ldA )

      integer            istate(maxbnd)
      integer            iwork(leniw)
      integer            iseed(3)
      double precision   Ax(ldA), A(ldA,maxn)
      double precision   bl(maxbnd), bu(maxbnd), clamda(maxbnd)
      double precision   b(maxm), cvec(maxn)
      double precision   H(ldH,maxn), x(maxn)
      double precision   work(lenw)
      double precision   zeta(maxn)

      double precision   yvar(maxn), xvar(ldA)
      double precision   bigbnd

      logical            byname
      integer            lunit
      character*20       lfile

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
      equivalence      (msgLC , msglvl)

      external           qpHess
      parameter        ( point1=0.1d+0, point3=0.3d+0, onept5=1.5d+0 )
      parameter        ( zero  =0.0d+0, one   =1.0d+0, two   =2.0d+0 )
      parameter        ( three =3.0d+0, four  =4.0d+0, five  =5.0d+0 )
      parameter        ( six   =6.0d+0, ten   =1.0d+1                )
      parameter        ( xmax  =1.0d+1                               )

*     -------------------------------------------------------------
*     Assign file numbers and problem data.
*     iPrint = the unit number for printing.
*     iOptns = the unit number for reading the options file.
*     iSumm  = the unit number for the summary output.
*     -------------------------------------------------------------
      iOptns = 4
      iPrint = 9

      byname = .true.

      if ( byname ) then

*        Unix and DOS systems.  Open the option and print files.

         lunit = iOptns
         lfile = 'qptest.opt'
         open( lunit, file=lfile, status='OLD',     err=800 )
         lunit = iPrint
         lfile = 'qptest.out'
         open( lunit, file=lfile, status='UNKNOWN', err=800 )
      else

*        VMS  systems.  Define units for the options and print files.

         lunit = iOptns
         open( lunit, status='OLD',     err=900 )
         lunit = iPrint
         open( lunit, status='UNKNOWN', err=900 )
      end if

      call qpprm('Print file on unit 9' )

   50 nspec = 0
*=======================================================================
  100 nspec = nspec + 1

*     Read problem number and options.

      call qpprms( iOptns, inform )
      if (inform .gt. 0) go to 990

      bigbnd = 1.0d+21

      if (nprob .eq. 1) then
*        ===============================================================
*        Problem 1.  Converted from a least-squares problem.  H = R'R.
*        ===============================================================
         m      = 8
         n      = 5
         nclin  = 3

         do 120, j = 1, n
            call dload ( m, zero, H(1,j), 1 )
  120    continue

         H(1,1) = one
         H(4,1) = one
         H(5,1) = one
         H(6,1) = two

         H(2,2) = one
         H(7,2) = three

         H(1,3) = one
         H(2,3) = four
         H(3,3) = four
         H(4,3) = one
         H(5,3) = one
         H(6,3) = two

         H(1,4) = three
         H(4,4) = three
         H(5,4) = three
         H(6,4) = six
         H(8,4) = zero

*        The following assignment gives a full-rank problem.
*        H(8,4) = - 1.0d-4

         H(3,5) = one
         H(4,5) = two
         H(8,5) = one

         do 130, j = 1, n
            call dload ( nclin, zero, A(1,j), 1 )
  130    continue

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two
         A(3,2) = - one
         A(3,4) = - one

         do 150, j = 1, n+nclin
            bl(j) = - two
            bu(j) =   two
  150    continue

         b(1) = one
         b(2) = two
         b(3) = three
         b(4) = four
         b(5) = five
         b(6) = one
         b(7) = two
         b(8) = three

         call dload ( n, (-two), x, 1 )
         call dgeqr ( m, n, H, ldH, zeta, inform )
         call dgeapq( 't', 's', m, n, H, ldH, zeta, 1, b, maxm,
     $                work, inform )

         call dcopy ( n, b, 1, cvec, 1 )
         call dtrmv ( 'u', 't', 'n', n, H, ldH, cvec, 1 )

         lwork  = 6*nclin + 9*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 2) then
*        ===============================================================
*        Problem 2.  Version of lssol example problem. H = R'R.
*        ===============================================================
         m      = 10
         n      = 6
         nclin  = 3

         do 210, j = 1, n
            call dload ( m, one, H(1,j), 1 )
  210    continue

*        the following gives a full rank problem.
*        H(1 ,1) = 1.0001d+0

         H(2 ,2) = two
         H(10,2) = zero

         H(3,3) = three
         H(6,3) = two
         H(9,3) = zero

         H(4,4) = four
         H(5,4) = three
         H(8,4) = zero

         H(7,5) = zero

         do 230, j = 1, n
            call dload ( nclin, one, A(1,j), 1 )
  230    continue

         A(1,6) =   two

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two

         A(3,2) = - one
         A(3,4) = - one
         A(3,6) =   five

         do 250, j = 1, n+nclin
            bl(j) = - two
            bu(j) =   two
  250    continue
         bu(1) = five

         b(1)  = one
         b(2)  = two
         b(3)  = three
         b(4)  = four
         b(5)  = five
         b(6)  = one
         b(7)  = two
         b(8)  = three
         b(9)  = four
         b(10) = five

         call dload ( n, zero, x, 1 )
         call dgeqr ( m, n, H, ldH, zeta, inform )
         call dgeapq( 't', 's', m, n, H, ldH, zeta, 1, b, maxm,
     $                work, inform )

         call dcopy ( n, b, 1, cvec, 1 )
         call dtrmv ( 'u', 't', 'n', n, H, ldH, cvec, 1 )

         lwork  = 6*nclin + 9*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 3) then
*        ===============================================================
*        Problem 3. Problem in lssol example program.   h = r'r.
*        ===============================================================
         m      = 10
         n      = 9
         nclin  = 3

         do 310, j = 1, n
            call dload ( m, one, H(1,j), 1 )
  310    continue

         H(2 ,2) =   two
         H(10,2) =   zero

         H(3,3) =    three
         H(6,3) =    two
         H(9,3) =    zero

         H(4,4) =    four
         H(5,4) =    three
         H(8,4) =    zero

         H(7,5) =    zero

         H(6,6) =    zero

         H(2 ,7) =   two
         H(3 ,7) = - one
         H(6 ,7) =   zero
         H(9 ,7) =   two
         H(10,7) =   zero

         H(2 ,8) =   zero
         H(3 ,8) = - one
         H(6 ,8) =   zero
         H(9 ,8) =   two
         H(10,8) =   two

         H(2 ,9) =   zero
         H(3 ,9) = - three
         H(6 ,9) = - one
         H(9 ,9) =   three
         H(10,9) =   two

*        The following defines a full-rank  H.
*        H(1,1) = H(1,1) - 0.0001d+0
*        H(2,2) = H(2,2) - 0.0001d+0
*        H(3,3) = H(3,3) - 0.0001d+0

         do 330, j = 1, n
            call dload ( nclin, one, A(1,j), 1 )
  330    continue

         A(1,9) =   four

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two

         A(3,2) = - one
         A(3,4) = - one

         do 350, j = 1, n
            bl(j) = - two
            bu(j) =   two
  350    continue
         bl( 3) = - bigbnd
         bl(10) =   two
         bu(10) =   bigbnd
         bl(11) = - bigbnd
         bu(11) = - two
         bl(12) = - four
         bu(12) = - two

         do 360, j = 1, m
            b(j) = one
  360    continue

         do 370, j = 1, n
            x(j) = one/ dfloat(n)
  370    continue

         call dgeqr ( m, n, H, ldH, zeta, inform )
         call dgeapq( 't', 's', m, n, H, ldH, zeta, 1, b, maxm,
     $                work, inform )

         call dcopy ( n, b, 1, cvec, 1 )
         call dtrmv ( 'u', 't', 'n', n, H, ldH, cvec, 1 )

         lwork  = 6*nclin + 9*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 4) then
*        ===============================================================
*        Problem 4.  Rockafellar's semi-definite QP.
*        ===============================================================
         n      = 5
         nclin  = 2

         A(1,1) = one
         A(2,1) = zero

         A(1,2) = two
         A(2,2) = one

         A(1,3) = two
         A(2,3) = one

         A(1,4) = six
         A(2,4) = zero

         A(1,5) = six
         A(2,5) = zero

         do 410, j = 1, n
            call dload ( n, one, H(1,j), 1 )
  410    continue

         H(2,2) =   five
         H(3,3) =   five
         H(4,4) =   two
         H(5,5) =   two

         H(3,2) =   five
         H(2,3) =   five
         H(5,4) =   two
         H(4,5) =   two

         do 450, j = 1, n
            bl(j) = - 1.0d+2
            bu(j) =   1.0d+2
  450    continue

         bl( 6) =   zero
         bu( 6) =   bigbnd
         bl( 7) =   one
         bu( 7) =   one

         call dload ( n, zero, cvec, 1 )
         cvec(1) = one

         call dload ( n, point1, x, 1 )

         lwork  = 6*nclin + 10*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 5) then
*        ===============================================================
*        Problem 5. QP with Hessian bordered by zeros.
*        ===============================================================
         m      = 5
         n      = 9
         nclin  = 3

         do 520, j = 1, m
            call dload ( j-1, one, H(1,j), 1 )
            H(j,j) = two
  520    continue

         do 530, j = 1, n
            call dload ( nclin, one, A(1,j), 1 )
  530    continue

         A(1,9) =   four

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two

         A(3,2) = - one
         A(3,4) = - one

         do 560, j = 1, n
            bl(j) = - two
            bu(j) =   two
  560    continue

         bl(10) = - two
         bu(10) =   onept5
         bl(11) = - two
         bu(11) =   onept5
         bl(12) = - two
         bu(12) =   four

         call dload ( n, (-one), cvec, 1 )
         cvec(1) = - four
         cvec(8) = - point1
         cvec(9) = - point3
         call dload ( n, zero, x, 1 )

         lwork  = 6*nclin + 10*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 6) then
*        ===============================================================
*        Problem 6. QP with positive semi-definite Hessian.
*        ===============================================================
         m      = 9
         n      = 9
         nclin  = 3

         do 620, j = 1, n
            do 610, i = 1, j
               H(i,j) = one
  610       continue
  620    continue

         do 630, i = 1, 6
            H(i,i) = two
  630    continue

         do 650, j = 1, n
            call dload ( nclin, one, A(1,j), 1 )
  650    continue

         A(1,9) =   four

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two

         A(3,2) = - one
         A(3,4) = - one

         do 660, j = 1, n
            bl(j) = - two
            bu(j) =   two
  660    continue

         bl(n+1) = - two
         bu(n+1) =   1.5d+0

         bl(n+2) = - six
         bu(n+2) =   two

         bl(n+3) = - two
         bu(n+3) =   six

         call dload ( n, one, cvec, 1 )
         cvec(1) = - five
         cvec(7) = - two
         cvec(8) = - two
         cvec(9) = - five

         do 680, j = 1, n
            x(j) = one / dfloat(j)
  680    continue

         lwork  = 6*nclin + 10*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 7  .or.  nprob .eq. 8) then
*        ===============================================================
*        Problem 7. QP with zero Hessian.
*        ===============================================================
         m      = 9
         n      = 9
         nclin  = 3

         do 720, j = 1, n
            call dload ( j, zero, H(1,j), 1 )
  720    continue

         do 750, j = 1, n
            call dload ( nclin, one, A(1,j), 1 )
  750    continue

         A(1,9) =   four

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two

         A(3,2) = - one
         A(3,4) = - one

         do 760, j = 1, n+nclin
            bl(j) = - two
            bu(j) =   two
  760    continue
         bu(n+3) =   six

         call dload ( n, (-one), cvec, 1 )

         do 780, j = 1, n
            x(j) = one / dfloat(j)
  780    continue

         if (nprob .eq. 7) then
            lwork  = 6*nclin + 10*n + 2*n**2
            liwork = 2*n + 3
         else if (nprob .eq. 8) then
            lwork  = 6*nclin +  7*n +   n**2
            liwork = 2*n + 3
         end if

      else if (nprob .eq. 9) then
*        ===============================================================
*        Problem 9.  Abel problem.
*        ===============================================================
         m      = 0
         n      = 101
         nclin  = 50

         call dload ( n, zero, x, 1 )

         en1    = n     - 1
         enc1   = nclin - 1
         rxm    = sqrt( xmax )
         xmc    = xmax*xmax*xmax
         xmsh   = xmc*rxm
*
         do 920, i = 1, n+nclin
            if (i .le. n) then
               bl(i)   = zero
               bu(i)   = bigbnd
               yvar(i) = dfloat( i - 1 )/en1*xmax
               cvec(i) = sin( yvar(i) )
            else
               imn       = i - n
               xvar(imn) = dfloat( imn - 1 )/enc1*xmax
               bl(i)  = 3.5d+0*xvar(imn)*xvar(imn)*sqrt(xvar(imn))/xmsh
               bu(i)  = bl(i)
            end if
  920    continue

         do 940, i = 1, nclin
           do 930, j = 1, n
             dif   = xvar(i) - yvar(j)
             if (dif .gt. zero) then
               A(i,j) = 0.5d+0/rxm/sqrt( dif )
             else
               A(i,j) = zero
             end if
  930      continue
  940    continue

         lwork  = 6*nclin + 7*n + n**2
         liwork = 2*n + 3

      else if (nprob .eq. 10) then
*        ===============================================================
*        Problem 10.  QP with H triangularized.
*        ===============================================================
         m      = 9
         n      = 9
         nclin  = 3

         do 1010, j = 1, n
            call dload ( j, zero, H(1,j), 1 )
            H(j,j)  = one
 1010    continue

         H(1,1) = zero
         H(4,4) = zero

         H(1,2) = one
         H(1,7) = one
         H(1,8) = one
         H(1,9) = one

         H(2,6) = one

         H(3,5) = one

         do 1040, j = 1, n
            call dload ( nclin, one, A(1,j), 1 )
 1040    continue

         A(1,9) =   four

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two

         A(3,2) = - one
         A(3,4) = - one

         do 1060, j = 1, n+nclin
            bl(j) = - two
            bu(j) =   two
 1060    continue

         bl(2)    = - three
         bl(3)    = - six
         bl(n+2)  = - six

         bu(4)    =   six
         bu(5)    =   three
         bu(n+2)  =   1.5d+0
         bu(n+3)  =   six

         call dload ( n, one, cvec, 1 )
         cvec(1) = - two
         cvec(4) = - two
         cvec(7) = - three
         cvec(8) = - three

         call dload ( n, zero, x, 1 )

         lwork  = 6*nclin + 10*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 11) then
*        ===============================================================
*        Problem 11.  Rockafellar 7-variable.
*        ===============================================================
         m      = 7
         n      = 7
         nclin  = 5

         do 1110, j = 1, n
            call dload ( j, zero, H(1,j), 1 )
 1110    continue

         H(2,2) =    70.0d+0

         H(2,3) =    70.0d+0
         H(3,3) =    70.0d+0

         H(2,4) = - 115.0d+0
         H(3,4) = - 115.0d+0
         H(4,4) =   730.5d+0

         H(5,5) =   154.0d+0

         H(5,6) =   154.0d+0
         H(6,6) =   154.0d+0

         H(5,7) = 220.0d+0
         H(6,7) = 220.0d+0
         H(7,7) = 400.0d+0

         do 1150, j = 1, n
            call dload ( nclin, one, A(1,j), 1 )
 1150    continue

         A(4,1) =   zero
         A(5,1) =   zero

         A(1,2) =   - 386.0d+0
         A(2,2) =   - 386.0d+0
         A(3,2) =   - 710.0d+0
         A(5,2) =     zero

         A(1,3) =   - 386.0d+0
         A(2,3) =   - 386.0d+0
         A(3,3) =   - 710.0d+0
         A(5,3) =     zero

         A(1,4) =   2010.5d+0
         A(2,4) =   2010.5d+0
         A(3,4) =   2775.0d+0
         A(5,4) =   zero

         A(1,5) =   154.0d+0
         A(2,5) =   154.0d+0
         A(3,5) =   220.0d+0
         A(4,5) =   zero

         A(1,6) =   154.0d+0
         A(2,6) =   154.0d+0
         A(3,6) =   220.0d+0
         A(4,6) =   zero

         A(1,7) =   220.0d+0
         A(2,7) =   220.0d+0
         A(3,7) =   400.0d+0
         A(4,7) =   zero

         do 1160, j = 1, n+nclin
            bl(j) = zero
            bu(j) = two
 1160    continue

         bl(1)    = - 3175.0d+0
         bl(n+4)  =   one
         bl(n+5)  =   one

         bu(1)    =   656.0d+0
         bu(n+1)  =   3831.0d+0
         bu(n+2)  =   3831.0d+0
         bu(n+3)  =   3831.0d+0
         bu(n+4)  =   one
         bu(n+5)  =   one

         call dload ( n, zero, cvec, 1 )
         cvec(1) = one
         call dload ( n, point1, x, 1 )

         lwork  = 6*nclin + 10*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 13) then
*        ===============================================================
*        Problem 13. Rockafellar 5-variable.
*        ===============================================================
         m      = 5
         n      = 5
         nclin  = 5

         do 1310, j = 1, n
            call dload ( j, zero, H(1,j), 1 )
 1310    continue

         H(3,3) =    one
         H(4,4) =    one
         H(4,3) =  - one
         H(3,4) =  - one

         do 1350, j = 1, 10
            bl(j) = zero
            bu(j) = one
 1350    continue

         bl(1) =  - four
         bu(1) =    four

         bu(6) =    1.0d+12
         bu(7) =    1.0d+12
         bu(8) =    1.0d+12

         bl(9) =    one
         bl(10) =   one

         do 1370, j = 1, n
            call dload ( nclin, zero, A(1,j), 1 )
 1370    continue

         A(1,1)  =   one
         A(2,1)  =   one
         A(3,1)  =   one

         A(1,2)  = - two
         A(2,2)  =   two
         A(4,2)  =   one

         A(1,3)  =   one
         A(2,3)  = - one
         A(5,3)  =   one

         A(1,4)  = - one
         A(2,4)  =   one
         A(5,4)  =   one

         A(5,5)  =   one

         call dload ( n, zero, cvec, 1 )
         cvec(1) = one

         do 1390, j = 1, n
            x(j) = zero
*           x(j) = 1.0d-1
*           x(j) = one
 1390    continue

         x(1) = 0.2
         x(3) = 0.2

         lwork  = 6*nclin + 10*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 14) then
*        ===============================================================
*        Problem 13. Rockafellar  7 variable.
*        ===============================================================
         m      = 7
         n      = 7
         nclin  = 6

         do 1410, j = 1, n
            call dload ( j, zero, H(1,j), 1 )
 1410    continue

         H(4,4) =    one
         H(5,5) =    one
         H(4,5) =  - one
         H(5,4) =  - one
         H(4,7) =    one
         H(7,4) =    one
         H(5,7) =  - one
         H(7,5) =  - one
         H(7,7) =    one

         do 1450, j = 1, 13
            bl(j) = zero
            bu(j) = 1.0d+12
 1450    continue

         bl(12) =    one
         bl(13) =    one
         bu(12) =    one
         bu(13) =    one

         bl(1)  =  - 1.0d+12

         do 1470, j = 1, n
            do 1460, i = 1, nclin
               A(i,j) = zero
 1460       continue
 1470    continue

         A(1,1)  =   one
         A(2,1)  =   one
         A(3,1)  =   one
         A(4,1)  =   one

         A(1,2)  = - two
         A(2,2)  =   two
         A(4,2)  = - two
         A(5,2)  =   one

         A(1,3)  =   100.0d+0
         A(2,3)  = - 100.0d+0
         A(4,3)  =   100.0d+0
         A(5,3)  =   one

         A(1,4)  =   one
         A(2,4)  = - one
         A(4,4)  =   one
         A(6,4)  =   one

         A(1,5)  = - one
         A(2,5)  =   one
         A(4,5)  = - one
         A(6,5)  =   one

         A(6,6)  =   one

         A(1,7)  =   one
         A(2,7)  = - one
         A(4,7)  =   one
         A(6,7)  =   one

         call dload ( n, zero, cvec, 1 )
         cvec(1) = one

         do 1490, j = 1, n
            x(j) = 0.9
*           x(j) = one
 1490    continue

         x(4) = point1
         x(6) = zero

         lwork  = 6*nclin + 10*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 15) then
*        ===============================================================
*        Problem 15.  Least-distance problem.
*        ===============================================================
         m      = 9
         n      = 9
         nclin  = 3

         do 1510, j = 1, n
            call dload ( j, zero, H(1,j), 1 )
            H(j,j)  = one
 1510    continue

         do 1540, j = 1, n
            do 1530, i = 1, nclin
               A(i,j) = one
 1530       continue
 1540    continue

         A(1,9) =   four

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two

         A(3,2) = - one
         A(3,4) = - one

         call dload ( n, (-two), bl, 1 )
         call dload ( n,   two , bu, 1 )
         bl( 3) = - bigbnd
         bl(10) =   two
         bu(10) =   bigbnd
         bl(11) = - bigbnd
         bu(11) = - two
         bl(12) = - four
         bu(12) = - two

         call dload ( m,   one , b, 1 )
         call dload ( n, (-ten), x, 1 )

         lwork  = 6*nclin + 9*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 16) then
*        ---------------------------------------------------------------
*        Problem 16. Sven's suggested test of qp4 option
*        ---------------------------------------------------------------
         m      = 8
         n      = 5
         nclin  = 3

         do 1620, j = 1, n
            do 1610, i = 1, m
               H(i,j) = zero
 1610       continue
 1620    continue

         H(1,1) = one
         H(4,1) = one
         H(5,1) = one
         H(6,1) = two

         H(2,2) = one
         H(7,2) = three

         H(1,3) = one
         H(2,3) = four
         H(3,3) = four
         H(4,3) = one
         H(5,3) = one
         H(6,3) = two

         H(1,4) = three
         H(4,4) = three
         H(5,4) = three
         H(6,4) = six
         H(8,4) = zero

*        The following assignment gives a full-rank problem.
*        H(8,4) = - 1.0d-4

         H(3,5) = one
         H(4,5) = two
         H(8,5) = one

         do 1640, j = 1, n
            do 1630, i = 1, nclin
               A(i,j) = one
 1630       continue
 1640    continue

         A(2,2) =   two
         A(2,3) =   three
         A(2,4) =   four
         A(2,5) = - two
         A(3,2) = - one
         A(3,4) = - one

         call dload ( n+nclin, (-two), bl, 1 )
         call dload ( n+nclin,   two , bu, 1 )

         b(1) = one
         b(2) = two
         b(3) = three
         b(4) = four
         b(5) = five
         b(6) = one
         b(7) = two
         b(8) = three

         call dload ( n, (-two), x, 1 )
         call dgeqr ( m, n, A, ldA, zeta, inform )
         call dgeapq( 't', 's', m, n, A, ldA, zeta, 1, b, maxm, work,
     $                inform )
         m      = n

         lwork  = 6*nclin + 9*n + 2*n**2
         liwork = 2*n + 3

      else if (nprob .eq. 17) then
*        ---------------------------------------------------------------
*        Problem 17.  Positive-definite QP.  GNQP  generates the H
*                     and constraints.
*        ---------------------------------------------------------------
         n      = 10
         nfixed =  4
         nclin  = 20
         nactiv =  6

         iprob  = 3

         iseed(1) = 1415
         iseed(2) = 1254
         iseed(3) = 7934

         condHy   = 1.0d+9
         condHz   = 1.0d+9
         condHx   = 1.0d+9
         condc1   = 1.0d+9
         condc2   = 1.0d+9
         condc3   = 1.0d+9
         condc4   = 1.0d+9
         negevh   = 0
         nulevh   = 0

         bndlo    = ten
         bndup    = ten
         spread   = one

         msglvl   = 1

         m        = n
         lwork    = 6*nclin + 10*n + 2*n**2
         liwork   = 2*n + 3

         call gnqp  ( iPrint, iSumm, iprob, inform, msglvl,
     $                ldA, ldH, n, nclin,
     $                nfixed, nactiv, negevh, nulevh,
     $                iseed, condHy, condHz, condHx,
     $                condc1, condc2, condc3, condc4,
     $                bndlo, bndup, spread,
     $                A, Ax, bl, bu, cvec, clamda, H, x,
     $                work, lwork )
         if (inform .gt. 0) stop

         msglvl   = 10
         call dload ( n, zero, x, 1 )

      end if

*     ==================================================================
*     Solve the problem.
*     ==================================================================
      call qpopt ( n, nclin, ldA, ldH,
     $             A, bl, bu, cvec, H, qpHess,
     $             istate, x,
     $             inform, iter, obj, Ax, clamda,
     $             iwork, liwork, work, lwork )

      go to 100

*     Error conditions.

  800 write(iPrint, 4000) 'Error while opening file', lfile
      stop

  900 write(iPrint, 4010) 'Error while opening unit', lunit
      stop

*     Endrun card found.

  990 if (nspec .eq. 1) write (iPrint, 9900)
      if (nspec .eq. 1) go to 50
      write (iPrint, 9920)
      stop

 4000 format(/  a, 2x, a  )
 4010 format(/  a, 2x, i6 )
 9900 format(/ ' XXX  Endrun card found before any problems solved.')
 9920 format(  ' ENDRUN')

*     end of  qptest
      end
