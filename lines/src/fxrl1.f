**********************
      SUBROUTINE FXRL1 (N,ISIGN,IFORM,DATA) 
**********************
*  See "Methods of exptl. physics vol12c", ed: Meeks, M.L.(1976) 
*  Fix up a real or half-size complex array for fast fourier transforming.
*     for IFORM = 0 : 
*  DATA is a real array of length N Which has just been transformed 
*  by FOUR1 or FOURG with an assumed complex length of N/2. 
*  After FOUR1 or FOURG, call FXRL1, and then DATA will be a complex 
*  array of length N/2 + 1, holding the first N/2 + 1 transform values. 
*  (The other N/2 - 1 values are the reverse order conjugate of those 
*  supplied). 
*     IFORM = -1 :
*  DATA is complex N/2 + 1 long, the transform of a real array N long.
*  First call FXRL1, then call FOUR1 or FOURG.
*  DATA now holds N real values, the transform. 
*  ISIGN = +1 or -1, and must be the same as in FOUR1 or FOURG. 
*  N must be even.
*     For example, to transform a real array: 
*  REAL DATA(200) 
*  COMPLEX CDATA(101), WORK(100)
*  EQUIVALENCE (DATA,CDATA) 
*  READ (LU,10) (DATA(I), I = 1,200)       THE REAL INPUT VALUES
*  CALL FOURG(DATA,100,+1,WORK) 
*  CALL FXRL1(DATA,200,+1,0)
*  WRITE(LU,20) (CDATA(I),1 = 1,101)       THE COMPLEX TRANSFORM
*     Norman Brenner, MIT, november 1971
*     Rewritten in hp FORTRAN IV by MJG 1981/09/17. 
* 
*     other subroutines called:
*     none
*
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8      DATA(*) 
* 
      DATA  PII/3.1415926535D0/ 
* 
      J = IABS(ISIGN) 
      IF (IABS(N-1) - N + IABS(N) - (IABS(N)/2)*2 + IABS(J-1) + 
     *    IABS(2*IFORM+1)/2 .LE. 0 .AND. IFORM .LE. 0)  GO TO 30
* 
*     error return                     
      PRINT 2000,  N,ISIGN,IFORM 
2000  FORMAT ("Error in FXRL1."/
     &        "N = ",I7," is odd or non-positive,"/ 
     &        "or ISIGN = ",I7," or IFORM ",I7," is incorrect.")
      GO TO 80 
* 
30    S = (1.0 - FLOAT(IFORM)) / SQRT(2.0)
      IP0 = 2 
      NHALF = IP0 * N / 2 
      IF (IFORM .LT. 0)  GO TO 50 
         DATA(NHALF+1) = DATA(1)
         DATA(NHALF+2) = DATA(2)
50    IMAX = NHALF / 2 + 1
         DO 70 I = 1,IMAX,IP0 
            ICONJ = NHALF + 2 - I 
            THETA = 2.0 * PII * (FLOAT(I-1)/FLOAT(2*ISIGN*N) + 0.25)
            ZR = 0.5 * (1.0 + FLOAT(2*IFORM+1) * COS(THETA))
            ZI = (FLOAT(IFORM) + 0.5) * SIN(THETA)
            DIFR = DATA(I) - DATA(ICONJ)
            DIFI = DATA(I+1) + DATA(ICONJ+1)
            TEMPR = ZR * DIFR - ZI * DIFI 
            TEMPI = ZR * DIFI + ZI * DIFR 
            DATA(I) = (DATA(I) - TEMPR) * S 
            DATA(I+1) = (DATA(I+1) - TEMPI) * S 
            IF (I .GE. ICONJ)  GO TO 80 
               DATA(ICONJ) = (DATA(ICONJ) + TEMPR) * S
70             DATA(ICONJ+1) = (DATA(ICONJ+1) - TEMPI) * S
80       RETURN 
         END
