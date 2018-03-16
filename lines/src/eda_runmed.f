      SUBROUTINE EDA_RUNMED(DB, Y, N, LEN, WORK, SAVE, NW, ERR)
c smooth Y() by running medians of length LEN
c note: use S3 for running medians of 3 instead of RUNMED
      IMPLICIT NONE
      REAL*8  EDA_MEDIAN  ! external function
      LOGICAL DB
      INTEGER N, LEN, NW, ERR, SAVEPT, SMOPT, LENP1, I, J
      REAL*8  Y(N), WORK(NW), SAVE(NW), TEMP, TWO
      DATA TWO/2.0D0/
c SAVE() acts as a window on the data
      IF (DB) PRINT '(A,$)','in EDA_RUNMED '
      IF (LEN .GT. NW) THEN
          ERR = 62
          GO TO 999
      END IF
      DO I = 1, LEN
          WORK(I) = Y(I)
          SAVE(I) = Y(I)
      END DO
      SAVEPT = 1
      SMOPT = INT((FLOAT(LEN)+TWO)/TWO)
      LENP1 = LEN + 1
      DO I = LENP1, N
          CALL EDA_SORT(DB, WORK, LEN, ERR)
          IF (ERR .NE. 0) GO TO 999
          Y(SMOPT) = EDA_MEDIAN(DB, WORK, LEN)
          TEMP = SAVE(SAVEPT)
          DO J = 1, LEN
              IF (WORK(J) .EQ. TEMP) GO TO 30
          END DO
          ERR = 63
          GO TO 999
   30     WORK(J) = Y(I)
          SAVE(SAVEPT) = Y(I)
          SAVEPT = MOD(SAVEPT, LEN)+1
          SMOPT = SMOPT + 1
      END DO
      CALL EDA_SORT(DB,WORK, LEN, ERR)
      IF (ERR .EQ. 0) Y(SMOPT) = EDA_MEDIAN(DB,WORK, LEN)
  999 IF (DB) PRINT *,'exit EDA_RUNMED'
      RETURN
      END
