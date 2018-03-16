*******************
      SUBROUTINE JD (YY,IDAY,HH,MN,SS,MS,DJULDA)
*******************
*     convert UT to JD
*
      IMPLICIT NONE
      INTEGER IC                ! local centuries
      INTEGER IDAY              ! in day of year
      INTEGER HH                ! in UT hours
      INTEGER MN                ! in UT minutes
      INTEGER MS                ! in UT milliseconds
      INTEGER NYRM1             ! local year - 1
      INTEGER SS                ! in UT seconds
      INTEGER YY                ! in year, four digit
      REAL*8  DJULDA            ! out full julian date
*
      NYRM1 = YY - 1
      IC = NYRM1 / 100
      DJULDA = 1721425 + 365*NYRM1 + NYRM1/4 - IC + IC/4
      DJULDA = DJULDA + (((DBLE(MS)/100D0 + DBLE(SS))/60D0
     &         + DBLE(MN))/60D0 + DBLE(HH) - 12D0)/24D0
     &         + DBLE(IDAY)
      RETURN
      END
*
