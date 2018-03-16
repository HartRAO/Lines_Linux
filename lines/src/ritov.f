**********************
      SUBROUTINE RITOV (MEM,RI,VEL)
**********************
*     calculate velocity for given spectrum array real index
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      INTEGER   MEM                     ! input memory in use
      REAL*8    RI                      ! input real array index
      REAL*8    VEL                     ! output velocity at index
*
      INCLUDE 'lines.inc'
*
      VEL = (RI - CRPIX1(MEM))*CDELT1(MEM) + CRVAL1(MEM)
      RETURN
      END
*********
