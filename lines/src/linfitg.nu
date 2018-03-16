      SUBROUTINE LINFIT (ISTART,IEND,DATA,SLOPE,CONSTANT,RMS)
************************************************************************
*     linear regression thru data in array
* 
*
      IMPLICIT NONE
      INTEGER  IEND             ! input last valid point in data
      INTEGER  ISTART           ! input first valid point in data
      INTEGER  M                ! local loop index
      INTEGER  NX               ! local number of points
      REAL*8   CONSTANT         ! output line constant
      REAL*8   DATA(*)		! input data array
      REAL*8   RMS		! output RMS of fit
      REAL*8   SLOPE            ! output slope of line
      REAL*8   SX		! local sum of X's
      REAL*8   SXX		! local sum of X*X's
      REAL*8   SY		! local sum of Y's
      REAL*8   SXY		! local sum of X*Y's
      REAL*8   XM               ! local X value
*
*     linear regression :
      SX  = 0.0
      SXX = 0.0
      SY  = 0.0
      SXY = 0.0
*
      DO M = ISTART, IEND
          XM   = M
          SX  = SX + XM
          SXX = SXX + XM**2
          SY  = SY + DATA(M)
          SXY = SXY + XM * DATA(M)
      END DO
*
*     regression coefficients :
*
      NX = IEND - ISTART + 1
      SLOPE = (SXY - SX*SY/NX) / (SXX - SX*SX/NX)
      CONSTANT = SY/NX - SLOPE*SX/NX
*
*     subtract linear drift :
C      DO I = 1, N
C          DATA(I) = DATA(I) - (SLOPE*I + CONSTANT)
C      END DO
*
*     find rms noise over fitted region
*
      RMS = 0.0
      DO M = ISTART, IEND
          XM = DATA(M) - (SLOPE*M + CONSTANT)
          RMS = RMS + XM*XM
      END DO
      RMS = SQRT(RMS/NX)
      RETURN
      END
