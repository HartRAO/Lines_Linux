      SUBROUTINE EDA_S3R(DB,Y,N)
c     compute repeated running medians of 3
      IMPLICIT NONE
      INTEGER N
      LOGICAL CHANGE, DB
      REAL*8  Y(N)
c
      IF (DB) PRINT '(A,$)','in  EDA_S3R '
   10 CHANGE = .FALSE.
          CALL EDA_S3(DB,Y,N,CHANGE)
      IF (CHANGE) GO TO 10
      CALL EDA_ENDPTS(DB,Y,N)
      IF (DB) PRINT *,'exit EDA_S3R'
      RETURN
      END
