**********************
      SUBROUTINE AITOV (MEM,I,VEL)
**********************
*     calculate velocity for given spectrum array index
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
*
      INTEGER   I                       ! input array index
      INTEGER   MEM                     ! input memory in use
      REAL*8    VEL                     ! output velocity at index
*
      INCLUDE 'lines.inc'
*
      VEL = (I - CRPIX1(MEM))*CDELT1(MEM) + CRVAL1(MEM)
      RETURN
      END
*********
