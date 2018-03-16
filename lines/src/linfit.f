      SUBROUTINE LINFIT (MEMFIT,SLOPE,CONSTANT,RMSDEV)
***********************
*     linear regression thru X data in array
* 
      IMPLICIT NONE
      INTEGER  M                ! local loop index
      INTEGER  MEMFIT           ! input memory to use
      INTEGER  NX               ! local number of points
      REAL*8   CONSTANT         ! output line constant
      REAL*8   RMSDEV           ! output RMS of fit
      REAL*8   SLOPE            ! output slope of line
      REAL*8   SX               ! local sum of X's
      REAL*8   SXX              ! local sum of X*X's
      REAL*8   SY               ! local sum of Y's
      REAL*8   SXY              ! local sum of X*Y's
      REAL*8   XM               ! local X value
      INCLUDE 'lines.inc'
*
*     linear regression :
      SX  = 0.0
      SXX = 0.0
      SY  = 0.0
      SXY = 0.0
*
      DO M = FIRST_CH(MEMFIT), LAST_CH(MEMFIT)
          XM   = M
          SX  = SX + XM
          SXX = SXX + XM**2
          SY  = SY + XIN(MEMFIT,M)
          SXY = SXY + XM * XIN(MEMFIT,M)
      END DO
*
*     regression coefficients :
*
      NX = LAST_CH(MEMFIT) - FIRST_CH(MEMFIT) + 1
      SLOPE = (SXY - SX*SY/NX) / (SXX - SX*SX/NX)
      CONSTANT = SY/NX - SLOPE*SX/NX
*
*     find rms noise over fitted region
*
      RMSDEV = 0.0
      DO M = FIRST_CH(MEMFIT), LAST_CH(MEMFIT)
          XM = XIN(MEMFIT,M) - (SLOPE*M + CONSTANT)
          RMSDEV = RMSDEV + XM*XM
      END DO
      RMSDEV = SQRT(RMSDEV/NX)
      RETURN
      END
