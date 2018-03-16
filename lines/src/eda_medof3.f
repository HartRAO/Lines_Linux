      SUBROUTINE EDA_MEDOF3(DB, X1, X2, X3, XMED, CHANGE)
c     put the median of X1, X2, X3 in XMED 
c     and set CHANGE .TRUE. if the median isn't X2
      IMPLICIT NONE
      LOGICAL DB, CHANGE
      REAL*8  X1, X2, X3, XMED, Y1, Y2, Y3
c
      IF (DB) PRINT '(A,$)', 'in EDA_MEDOF3 '
      Y1 = X1
      Y2 = X2
      Y3 = X3
      XMED = Y2
      IF ((Y2-Y1)*(Y3-Y2) .GE. 0.0D0) GO TO 999
      CHANGE = .TRUE.
      XMED = Y1
      IF ((Y3-Y1)*(Y3-Y2) .GT. 0.0D0) GO TO 999
      XMED = Y3
 999  IF (DB) PRINT *,'exit EDA_MEDOF3'
      RETURN
      END
