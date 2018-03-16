**********************
      SUBROUTINE VTOAI (MEM,VEL,I)
**********************
*     calculate spectrum array index from given velocity
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      INTEGER   I                       ! output array index of vel
      INTEGER   MEM                     ! input memory in use
      REAL*8    VEL                     ! input velocity 
*
      INCLUDE 'lines.inc'
*
      IF (CDELT1(MEM) .GT. 0D0) THEN
          I = NINT((VEL - CRVAL1(MEM))/CDELT1(MEM) + CRPIX1(MEM))
      ELSE
          PRINT *,'invalid CDELT1 = ',CDELT1(MEM)
          I = 1
      END IF
      RETURN
      END
*********
