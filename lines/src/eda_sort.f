      SUBROUTINE EDA_SORT(DB, Y, N, ERR)
c shell sort N values in Y() from smallest to largest
      IMPLICIT NONE
      LOGICAL DB
      INTEGER N, ERR, I, J, J1, GAP, NMG
      REAL*8  Y(N), TEMP
C
      IF (DB) PRINT '(A,$)','in EDA_SORT '
      IF (N .LT. 1) THEN
          ERR = 1
          GO TO 999
      END IF
      IF (N .EQ. 1) GO TO 999
c one element is always sorted
      GAP = N
   20     GAP = GAP/2
          NMG = N - GAP
          DO J1 = 1, NMG
              I = J1 + GAP
              J = J1
   30         IF (Y(J) .LE. Y(I)) GO TO 40
C                 swap out-of-order pair
                  TEMP = Y(I)
                  Y(I) = Y(J)
                  Y(J) = TEMP
c                 keep old pointer for next time thru
                  I = J
                  J = J - GAP
                  IF (J .GE. 1) GO TO 30
   40     CONTINUE
          END DO
          IF (GAP .GT. 1) GO TO 20
  999 IF (DB) PRINT *,'exit EDA_SORT'
      RETURN
      END
