************************* 
      SUBROUTINE LINTWIDE (DATA,N1,N2,IMAX,FHEIGHT,
     &                   CENTERI,WIDTHI) 
*************************
*     find where data(i) < fheight, the specified height below the peak 
*     imax is index of peak, n1,n2 are lower and upper index limits 
*     return line center and width at the given height fraction,
*     in units of array index.
* 
*     other subroutines called:
*     none
*
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 FHEIGHT            ! input height at which width to be found
      REAL*8 CENTERI            ! output mid point at given height
      REAL*8 WIDTHI             ! output width at given height
      REAL*8 DATA(*)
* 
*     search indicies below index of peak where data < fheight
      DO K = IMAX,N1,-1 
          IF (ABS(DATA(K)) .LT. ABS(FHEIGHT)) GO TO 714 
      END DO
* 
*     linear interpolation for d(k) to d(kprev) crossing half height
  714 DK = (FHEIGHT - DATA(K+1)) / (DATA(K) - DATA(K+1))
      FWFM1 = K + 1 - DK
* 
*     find where data < fheight above index of peak 
      DO K = IMAX,N2,1
          IF (ABS(DATA(K)) .LT. ABS(FHEIGHT)) GO TO 716 
      END DO
  716 DK = (FHEIGHT - DATA(K-1)) / (DATA(K) - DATA(K-1))
      FWFM2 = K - 1 + DK
      WIDTHI = FWFM2 - FWFM1
      CENTERI = (FWFM2 + FWFM1) / 2 
      RETURN
      END 
