C     The following program causes E04NFF (QPOPT) to get stuck in a
C     near infinite loop. This occurs because each time MSG = 'resetx'
C     on exit from  E04NFZ (QPCORE), but this is not catered for in the
C     call from  E04NFF (QPOPT). The problem is as follows.
C
C     Minimize   c^Tx + (1/2)*x^THx
C
C     subject to -inf <=  x_1 <= +inf
C                -inf <=  x_2 <= +inf
C                        ...
C                -inf <= x_24 <= +inf
C
C                 x_1 - x_2 >= 0
C                 x_2 - x_3 >= 0
C                 x_3 - x_4 >= 0
C                 x_4 - x_5 >= 0
C                 x_5 - x_6 >= 0
C                 x_6 - x_7 >= 0
C
C     Starting from (0,0,...,0)^T (Feasible).
C
C     User not expecting a solution - just wants the code to exit
C     gracefully with an error condition! (Unbounded?)
C
      PROGRAM MAIN
C     .. Parameters ..
      INTEGER          NIN
      PARAMETER        (NIN=5)
      INTEGER          NVAR, NCLIN
      PARAMETER        (NVAR=24,NCLIN=6)
      INTEGER          LIWORK, LWORK
      PARAMETER        (LIWORK=10000,LWORK=100000)
      INTEGER          NCMAX
      PARAMETER        (NCMAX=50)
C     .. Local Scalars ..
      DOUBLE PRECISION OBJF
      INTEGER          I, IFAIL, ITER, J, K
      INTEGER          ISTATE(NCLIN+NVAR), IWORK(LIWORK)
      character*20     lFile
      CHARACTER*2048   STRIN
C     .. Local Arrays ..
      DOUBLE PRECISION A(NCLIN,NVAR), AX(NCMAX), BL(NCLIN+NVAR),
     *                 BU(NCLIN+NVAR), CLAMDA(NCMAX), CVEC(NVAR),
     *                 H(NVAR,NVAR), WORK(LWORK), X(NVAR)
C     .. External Subroutines ..
      EXTERNAL         qpopt, qpHess
C     .. Executable Statements ..
*

      lFile = 'tester.dat'
      open( NIN, file=lFile, status='OLD',     err=800 )

      iSumm  = 6
      iPrint = 9
      lFile  = 'tester.out'
      open( iPrint, file=lFile, status='UNKNOWN', err=800 )

      call qpprmi( 'Print   file          =', iPrint )
      call qpprmi( 'Summary file          =', iSumm  )

      READ (NIN,FMT=*) STRIN
      READ (NIN,FMT=*) (CVEC(I),I=1,NVAR)
*
      READ (NIN,FMT=*) STRIN
      READ (NIN,FMT=*) ((H(J,I),I=1,NVAR),J=1,NVAR)
*
      DO 20 I = 1, NVAR
         BL(I) = -1.0D25
         BU(I) = 1.0D25
   20 CONTINUE
      DO 40 I = NVAR + 1, NVAR + NCLIN
         BL(I) = 0.0D0
         BU(I) = 1.0D25
   40 CONTINUE
*
      DO 80 I = 1, NCLIN
         K = I + 1
         DO 60 J = 1, NVAR
            A(I,J) = 0.0D0
            IF (J.EQ.I) THEN
               A(I,J) = 1.0D0
            ELSE IF (J.EQ.K) THEN
               A(I,J) = -1.0D0
            END IF
   60    CONTINUE
   80 CONTINUE
*
      DO 100 I = 1, NVAR
         X(I) = 0.0D0
  100 CONTINUE
*
      IFAIL = -1
*
      call qpprmi( 'Print level            ',      10  )

      call qpopt ( nvar, nclin, nclin, nvar,
     $     A, bl, bu, cvec, H,
     $     qpHess, istate, x,
     $     inform, iter, objf, Ax, clamda,
     $     iwork, liwork, work, lwork )

*     CALL E04NFF(NVAR,NCLIN,A,NCLIN,BL,BU,CVEC,H,NVAR,E04NFU,ISTATE,X,
*    *            ITER,OBJF,AX,CLAMDA,IWORK,LIWORK,WORK,LWORK,IFAIL)
*
  800 STOP
*
      END
