**********************
      SUBROUTINE FOUR1 (N,ISIGN,DATA) 
**********************
*  One-dimensional complex fast fourier transform of array DATA,
*  of length N = 2**LOG2N points. 
*  The following transform replaces DATA in storage:
*  TRAN(K) = 1/SQRT(N) * SUM(DATA(J)*EXP(2*PI*I*ISIGN*(J-1)*(K-1)/N)) 
*  Summed over all J from 1 to N, for all K from 1 to N.
*  ISIGN = +1 OR -1, the direction of the transform.
*  Running time is proportional to N*LOG2(N). 
*  Norman Brenner, MIT, October 1971. 
*  Rewritten in hp Fortran IV by MJG 1981/09/17.
*  See "Methods of Exptl. Physics Vol12C", ed: Meeks, M.L.(1976) 
* 
*     other subroutines called:
*     none
*
      IMPLICIT REAL*8 (A-H,O-Z) 
      INTEGER   ITWO(10)
      REAL*8      DATA(*) 
* 
      DATA PII/3.1415926535D0/
* 
*     error checks 
      IF (N - 1) 90,110,10
10       LOG2N = ALOG( FLOAT(N) ) / 0.693147 + 0.5
         IF (IABS(ISIGN) - 1+N - 2**LOG2N .NE. 0)  GO TO 90 
* 
*     error checks okay
      ML = 0    ! keep Gnu fortran compiler happy
      IP0 = 2 
      ITWO(1) = IP0 
      DO 30 L = 2,LOG2N 
30       ITWO(L) = ITWO(L-1) * 2
      SQRTN = SQRT(FLOAT(N))
      IMAX = N * IP0
      IREV = 1
* 
*  in binary, I-1 and IREV-1 are mirror images, as 1101 and 1011. 
*  For each I, exchange DATA(I) and DATA(IREV). To prevent
*  re-exchanging, I must not be greater than IREV.
      DO 70 I = 1,IMAX,IP0
         IF (I .GT. IREV)  GO TO 50 
         TEMPR = DATA(I)
         TEMPI = DATA(I+1)
         DATA(I) = DATA(IREV) / SQRTN 
         DATA(I+1) = DATA(IREV+1) / SQRTN 
         DATA(IREV) = TEMPR / SQRTN 
         DATA(IREV+1) = TEMPI / SQRTN 
* 
*     Compute the reversal of I by adding 1 to the high order bit of
*     the reversal of I-1 AND propagating the carry downwards.
50       DO 60 L = 1,LOG2N
            ML = LOG2N - L + 1
            IF (IREV .LE. ITWO(ML))  GO TO 70 
60             IREV = IREV - ITWO(ML) 
70       IREV = IREV + ITWO(ML) 
* 
*     Separate the data into even and odd subsequences, transform 
*     each, phase shift the odd transform and add them. 
*     This recursive formulation of the FFT is here written 
*     iteratively, from the innermost subtransform outward. 
         DO 80 L = 1,LOG2N
            ITWOL = ITWO(L) 
            DO 80 I1 = 1,ITWOL,IP0
               THETA = PII * FLOAT(ISIGN * (I1-1)) / FLOAT(ITWOL) 
               WR = COS(THETA)
               WI = SIN(THETA)
               ISTEP = 2 * ITWOL
               DO 80 IA = I1,IMAX,ISTEP 
                  IB = IA + ITWOL 
                  TEMPR = WR*DATA(IB) - DATA(IB+1)*WI 
                  TEMPI = WR*DATA(IB+1) + DATA(IB)*WI 
                  DATA(IB) = DATA(IA) - TEMPR 
                  DATA(IB+1) = DATA(IA+1) - TEMPI 
                  DATA(IA) = DATA(IA) + TEMPR 
80                DATA(IA+1) = DATA(IA+1) + TEMPI 
         GO TO 110
* 
*     error return
90    PRINT 1000,  N,ISIGN 
1000  FORMAT("Error in FOUR1"/ 
     &       "N = ",I7," is non-positive, or not a power of two,"/ 
     &       "or ISIGN = ",I7," is not +1 or -1.") 
* 
110   RETURN
      END 
