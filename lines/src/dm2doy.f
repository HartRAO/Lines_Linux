***********************************************
      SUBROUTINE DM2DOY (IYEAR,MONTH,IDOM,IDOY)
***********************************************
*     convert day and month to day of year   MJG <930617.1007>
*     called by: stfromjd
*
      IMPLICIT NONE
      INTEGER  IYEAR                   ! input  year
      INTEGER  MONTH                   ! input  month
      INTEGER  IDOM                    ! input  day of month
      INTEGER  IDOY                    ! output day of year
      INTEGER  I                       ! local  loop control
      INTEGER  MDAYS(12)               ! local  days in month
*
*             month   1  2  3  4  5  6  7  8  9 10 11 12
      DATA    MDAYS /31,28,31,30,31,30,31,31,30,31,30,31/
*
      MDAYS(2) = 28
*     leap year ?
      IF ((IYEAR/4)*4 .EQ. IYEAR) MDAYS(2) = 29
*     note year 2000 IS a leap year, so this is good to 2100
*
      IDOY = 0
      IF (MONTH .GT. 1) THEN
          DO I = 1, MONTH-1
              IDOY = MDAYS(I) + IDOY
          END DO 
      END IF
      IDOY = IDOY + IDOM
      RETURN
      END
*********