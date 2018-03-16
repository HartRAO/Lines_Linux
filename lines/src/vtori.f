**********************
      SUBROUTINE VTORI (MEM,VEL,RI)
**********************
*     calculate spectrum array index as a real from given velocity
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      INTEGER   MEM                     ! input memory in use
      REAL*8    VEL                     ! input velocity 
      REAL*8    RI                      ! output real array index of vel
*
      INCLUDE 'lines.inc'
*
      IF (CDELT1(MEM) .GT. 0D0) THEN
          RI = (VEL - CRVAL1(MEM))/CDELT1(MEM) + CRPIX1(MEM)
      ELSE
          PRINT *,'invalid CDELT1 = ',CDELT1(MEM)
          RI = 1
      END IF
      RETURN
      END
*********
