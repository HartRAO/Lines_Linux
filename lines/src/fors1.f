**********************
      SUBROUTINE FORS1 (N, ISIGN, ISYM, DATAD)
**********************
*  See "Methods of exptl. physics vol12c",ed:Meeks, M.L.(1976) 
*  Fast fourier transform of one-dimensional, real, symmetric data. 
*  TRAN(K) = 1/SQRT(N) * SUM(DATA(J)*EXP(2*PI*I*ISIGN*(J-1)*(K-1)/N)) 
*  Summed over all J from 1 to N, for all K from 1 to N.
*  Input array DATA is real, length N/2 + 2, representing a real array
*  of length N. N is an arbitrary multiple of four, Tho if it is not
*  a power of two, replace FOUR2 by FOURT (or FOUR1 by FOURG).
*     The full array of length N has either even (ISYM = +1) or odd 
*  (ISYM = -1) symmetry, according to whether DATA(N+2-I) = + DATA(I) 
*  or - DATA(I), for I = 2,3,...N. DATA(1) = + or - DATA(1).
*  Odd symmetry implies that DATA(1) = DATA(N/2 + 1) = 0. 
*     ISIGN = +1 or -1, the direction of the transform. 
*  The first N/2 + 1 points of the transform, real and same symmetry
*  as the input, will be returned in DATA, replacing the input. 
*  DATA(N/2+2) holds no data value, but is used for working storage.
*     Running time and array storage are equivalent to the transform
*  of N/4 complex, asymmetric data. note that the complex transform of
*  even or odd symmetric data is equivalent to the cosine or sine 
*  transform, respectively, of asymmetric data. 
*     Norman Brenner, MIT, OCT, 1968. 
*     Rewritten in HP FORTRAN IV by MJG 1981/09/17. 
*
*     other subroutines called:
*     four1, fxrl1 
*
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER   N
      INTEGER   ISIGN
      INTEGER   ISYM
      REAL*8    DATAD(*) 
* 
      DATA PII /3.1415926535D0/
* 
      J = IABS(ISIGN) 
      J1 = IABS(ISYM) 
      DATAD((N/2)+1) = DATAD(N/2) 
      IF (IABS(N-1) -N +1 + IABS(N) - (IABS(N)/4)*4 + IABS(J-1) 
     *    + IABS(J1-1) .GT. 0)  GO TO 140 
* 
*  rearrange data into a conjugate-symmetric complex pair 
      NHALF = N / 2 
      ODDSM = 0.0 
      PREV = DATAD(2) * FLOAT(ISYM)
      DO 30 I = 2,NHALF,2 
         ODDSM = ODDSM + DATAD(I)
         TEMP = DATAD(I) 
         DATAD(I) = DATAD(I) - PREV 
         PREV = TEMP
         IF (ISYM .GE. 0)  GO TO 30 
            TEMP = DATAD(I-1)
            DATAD(I-1) = DATAD(I) 
            DATAD(I) = TEMP
30             CONTINUE 
      IF (ISYM .GE. 0)  GO TO 50
         ODDSM = 0.0
         DATAD(NHALF+1) = -2.0 * PREV
50       DATAD(NHALF+2) = 0.0
* 
*  Fourier transform the conjugate-symmetric complex array into a 
*  real array,both of ostensible length N/2 
      CALL FXRL1 (N/2,ISIGN,-1,DATAD)
      CALL FOUR1 (N/4,ISIGN,DATAD) 
* 
*  fix up the real array
      SQRT8 = SQRT(8.0) 
      S = SQRT(FLOAT(N)/8.0)
      THETA = 2.0 * PII / FLOAT(ISIGN*N)
      SPREV = -2.0 * SIN(THETA) 
      TEMP = 0.0
      SINTH = SIN(THETA/2.0)
      F = -4.0 * SINTH * SINTH
      NRCUR = 8 
      DATAD(NHALF+1) = DATAD(1) - ODDSM 
      DATAD(1) = DATAD(1) + ODDSM 
      DO 120 I = 1,NHALF
         ICONJ = NHALF + 2 - I
         IF (I .GT. ICONJ)  GO TO 160 
            SUM = (DATAD(ICONJ) + DATAD(I)) / SQRT8 
            DIF = (DATAD(ICONJ) - DATAD(I)) / SQRT8 
* 
*        S = 2.*SIN(ISIGN*2*PI*(I-1)/N) 
            IF (ISYM .LT. 0)  SUM = SUM / S 
            IF (ISYM .GE. 0)  DIF = -DIF / S
            DATAD(I) = SUM + DIF 
            DATAD(ICONJ) = SUM - DIF 
            IF (I .LE. NRCUR*(I/NRCUR))  GO TO 110
               S = F*TEMP + 2.0*TEMP - SPREV
               SPREV = TEMP 
               GO TO 120
110               S = 2.0 * SIN(THETA*FLOAT(I)) 
                  SPREV = 2.0 * SIN(THETA*FLOAT(I-1)) 
120               TEMP = S
      GO TO 160 
* 
*     error return 
140   PRINT 1000,  N,ISIGN,ISYM
1000  FORMAT ("Error in FORS1."/
     &        "either N = ",I7," or ISIGN = ",I7, 
     &        "or  ISYM = ",I7," is illegal.") 
* 
160   RETURN 
      END
