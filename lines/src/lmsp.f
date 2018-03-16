*********************
      SUBROUTINE LMSP (DB,X,F,W,IPOLY,M,
     &                 NONE,NTWO, PJ,
     &                 A,RMSER,SW,PJM1,ERROR) 
*********************
*  Orthogonal polynomial least squares fitting to  M  points. 
*  originally from Pat Terry, RU
*
*     other subroutines called:
*     none
*
      IMPLICIT REAL*8 (A-H,O-Z)
*
      INTEGER   IPOLY        ! input degree of polynomial
      INTEGER   JMAX         ! output array index for maximum error
      INTEGER   M            ! input number of data 
      INTEGER   NONE         ! input array index of first valid point
      INTEGER   NTWO         ! input array index of last valid point
      LOGICAL   DB           ! input debug output on/off
*
      REAL*8    A(*)         ! output A(J+1) is the coefficient of X**J
      REAL*8    AMXER        ! maximum error 
      REAL*8    CORR         ! correlation coefficient
      REAL*8    COEF(11)     ! output least squares coefficients
      REAL*8    E(11)        ! local working array
      REAL*8    ERROR(*)     ! output residuals = supplied - fitted values
      REAL*8    F(*)         ! input function  (  dependent variables) 
      REAL*8    G(11)        ! local working array
      REAL*8    PJM1(*)      ! output polynomial fit at each X.
      REAL*8    PJ(*)        ! local working array
      REAL*8    RMSER        ! output rms error of fit
      REAL*8    VAR          ! variance
      REAL*8    SW           ! output sum of weights = no of points used
      REAL*8    W(*)         ! input weight of each F
      REAL*8    X(*)         ! input abscissae (independent variables) 
*
      IF (DB) PRINT *,'in LMSP'
      IF (DB) PRINT *,'IPOLY = ',IPOLY,' N one = ',NONE,' N two = ',NTWO
*
      N1 = IPOLY + 1
*     Check that input conditions are met
      IF (IPOLY .GT. 10 .OR. IPOLY .GT. NTWO-NONE-1)  GO TO 80
*
*     Clear inner product totals
      RLAM = 0.0
      B = 0.0
      D = 0.0
*     Form first orthogonal polynomial inner products
      DO 10 I = NONE,NTWO
         PJ(I) = 1.0
         D = D + F(I) * W(I)
         B = B + X(I) * W(I)
10       RLAM = RLAM + W(I)
      B = B / RLAM
      C = 0.0
      D = D / RLAM
      COEF(1) = D
      A(1) = D
*     Store lamda for later use
      SW = RLAM
      DO 20 I = NONE,NTWO
20       ERROR(I) = F(I) - D
      IF (N1 .EQ. 1)  RETURN
*
*     Initialise terms in recurrence relation for usual polynomial
*     coefficient calculations
      G(N1) = 1.0D0
      E(N1) = 0.0
*     Start main loop to generate orthogonal polynomials
      DO 60 J = 2,N1
         J1 = J - 1
         RLAM1 = RLAM
         B1 = B
         C1 = C
*        Clear inner product totals
         RLAM = 0.0
         B = 0.0
         D = 0.0D0
*        Use recurrence relation to compute next polynomial
         DO 30 I = NONE,NTWO
            P = PJ(I)
            PJ(I) = (X(I) - B1) * PJ(I) - PJM1(I) * C1
            PJM1(I) = P
            P = PJ(I) * W(I)
            D = D + ERROR(I) * P
            P = P * PJ(I)
            B = B + X(I) * P
30          RLAM = RLAM + P
*        Compute coefficients in recurrence relation
         B = B / RLAM
         C = RLAM / RLAM1
*        Compute coefficient in least squares expansion
         D = D / RLAM
         COEF(J) = D
*        Evaluate coefficients of least squares poly in usual form
         NJ = IPOLY - J1 + 1
         A(J) = 0.0D0
         G(NJ) = 0.0D0
         DO 40 JK = NJ,IPOLY
            G(JK) = G(JK) - G(JK+1) * B1 - E(JK+1) * C1
40          E(JK) = G(JK+1)
         DO 50 JK = 1,J
50          A(JK) = G(JK+IPOLY-J1) * D + A(JK)
*        Compute residuals
         DO 60 I = NONE,NTWO
60          ERROR(I) = ERROR(I) - PJ(I) * D
*
*     Clear totals for correlation computation
      SY = 0.0D0
      SZ = 0.0D0
      SY2 = 0.0D0
      SZ2 = 0.0D0
      SYZ = 0.0D0
      RMSER = 0.0D0
      AMXER = 0.0D0
*     Accumulate totals for correlation computation
      DO 70 I = NONE,NTWO
         PJM1(I) = F(I) - ERROR(I)
         SY = SY + W(I) * F(I)
         SZ = SZ + W(I) * PJM1(I)
         SY2 = SY2 + W(I) * F(I) * F(I)
         SZ2 = SZ2 + W(I) * PJM1(I) * PJM1(I)
         SYZ = SYZ + W(I) * F(I) * PJM1(I)
         P = ERROR(I) * ERROR(I)
         RMSER = RMSER + W(I) * P
*        Determine greatest residual
         IF (AMXER .GE. P) GO TO 70
            AMXER = P
            JMAX = I
70       CONTINUE
*
*     Compute variance and correlation coefficient
      VAR = RMSER / (SW - FLOAT(N1))
      RMSER = DSQRT(RMSER / SW)
      AMXER = DSQRT(AMXER)
      CORR = (SW*SYZ - SY*SZ) /
     *          DSQRT((SW*SY2 - SY*SY) * (SW*SZ2 - SZ*SZ))
*
      CONTINUE
      IF (DB) PRINT *,'A(I) = ',(A(I),I=1,IPOLY+1)
      IF (DB) PRINT *,'RMSER=',RMSER
c      IF (DB) THEN
c          DO I = 1, M
c              PRINT *,'I,X,PJM1,ERROR=',I,X(I),PJM1(I),ERROR(I)
c          END DO
c      END IF
      IF (DB) PRINT *,'leaving LMSP'
      RETURN
*
*     Error exit point
*
80    DO 90 I = 1,N1
90        A(I) = 0.0D0
      IPOLY = -1
      PRINT *,'error in LMSP'
      RETURN
*
      END
******************
