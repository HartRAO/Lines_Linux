******************************
      INTEGER FUNCTION PLSCALE (LOGAXIS,NPTS,V,VBAR,IBAR,VMIN,VMAX)
******************************
*     determine maximum and minimum of autoscaled axis
*
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      INTEGER   LOGAXIS       ! input >0 if axis is logarithmic
      INTEGER   NPTS          ! input number of valid points
      REAL      V(*)          ! input value array
      REAL      VBAR(*)       ! input error in value array
      INTEGER   IBAR          ! input 0 disregard errors 1 use errors
      REAL      VMIN          ! output axis minimum
      REAL      VMAX          ! output axis maximum
*
      REAL      VHI, VLO      ! local running maximum, minimum
      REAL      PCRANGE       ! local unadjusted range
      INTEGER   I             ! local counter
*
C     determine maxima and minima of data
      IF (IBAR .EQ. 0) THEN
          VMIN = V(1)
          VMAX = V(1)
      ELSE 
          VMIN = V(1) - ABS(VBAR(1))
          VMAX = V(1) + ABS(VBAR(1))
      END IF
*
      DO I = 2,NPTS
          IF (IBAR .EQ. 0) THEN
              VLO = V(I)
              VHI = V(I)
          ELSE 
              VLO = V(I) - ABS(VBAR(I))
              VHI = V(I) + ABS(VBAR(I))
          END IF
          VMIN = MIN (VMIN, VLO)
          VMAX = MAX (VMAX, VHI)
      END DO
*
C     convert to logarithmic values where necessary
      IF (LOGAXIS .GT. 0) THEN 
          IF (VMIN .GT. 0.0) THEN
              VMIN = ALOG10 (VMIN)
              VMAX = ALOG10 (VMAX)
          ELSE
              PRINT *,'PLSCALE: Negative data on log axis!'
              PLSCALE = 0
              GO TO 999
          END IF
      END IF
*
C     allow a 2% margin (keep inside small tick marks)
      PCRANGE = (VMAX - VMIN) / 50D0
      VMIN = VMIN - PCRANGE
      VMAX = VMAX + PCRANGE
*
      PLSCALE = 1
*
  999 RETURN
      END
*********