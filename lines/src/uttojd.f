***********************
      SUBROUTINE UTTOJD (IY,IT,DJULDA)
***********************
*     convert UT to JD
*     inputs :
*     iy = year
*     it(1)=msec it(2)=sec it(3)=mins it(4)=hours it(5)=days  UT
*
*     outputs :
*     djulda = full Julian date
*
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      INTEGER*4  IT(5), IY, NYRM1, IC, JULDA
      REAL*8     DJULDA
*
      NYRM1 = IY - 1
      IC = NYRM1 / 100
      JULDA = 1721425 + 365*NYRM1 + NYRM1/4 - IC + IC/4
      DJULDA = JULDA + (((DBLE(IT(1))/100d0 + DBLE(IT(2)))/60d0
     &         + DBLE(IT(3)))/60d0 + DBLE(IT(4)) - 12d0)/24d0
     &         + DBLE(IT(5))
      RETURN
      END
*********
