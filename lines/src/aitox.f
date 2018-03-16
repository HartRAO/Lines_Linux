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
      INTEGER   I                               ! input array index
      INTEGER   MEM                     ! input memory in use
      REAL*8    VEL                     ! output velocity at index
*
      INCLUDE 'lines.inc'
*
*     works only for regularly spaced
C      VEL = (I - CRPIX1(MEM))*CDELT1(MEM) + CRVAL1(MEM)
*
*     works for any spacing
      VEL = XIN(MEM,I)
      RETURN
      END
*********
