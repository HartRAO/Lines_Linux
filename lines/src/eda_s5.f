      SUBROUTINE EDA_S5(DB,Y,N,WORK,SAVE,NW,ERR)
c     smooth by running medians of 5
      IMPLICIT NONE
      INTEGER N, NW, ERR
      LOGICAL CHANGE, DB
      REAL*8  Y(N), WORK(NW), SAVE(NW)
      REAL*8  YMED
c
      IF (DB) PRINT '(A,$)','in EDA_S5 '
      CHANGE = .FALSE.
      CALL EDA_RUNMED(DB,Y,N,5,WORK,SAVE,NW,ERR)
      CALL EDA_MEDOF3(DB,Y(1),Y(2),Y(3),YMED,CHANGE)
      Y(2) = YMED
      CALL EDA_MEDOF3(DB,Y(N),Y(N-1),Y(N-2),YMED,CHANGE)
      Y(N-1) = YMED
      IF (DB) PRINT *,'exit EDA_S5'
      RETURN
      END
