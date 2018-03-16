      SUBROUTINE EDA_S3(DB,Y,N,CHANGE)
c     compute running median of 3 on Y()
c     set CHANGE .TRUE. if any change made
      IMPLICIT NONE
      INTEGER N, I, NM1
      LOGICAL CHANGE, DB
      REAL*8  Y(N), Y1, Y2, Y3
c
      IF (DB) PRINT '(A,$)','in EDA_S3 '
      NM1 = N-1
      Y2 = Y(1)
      Y3 = Y(2)
      DO I = 2, NM1
          Y1 = Y2
          Y2 = Y3
          Y3 = Y(I+1)
          CALL EDA_MEDOF3(DB,Y1,Y2,Y3,Y(I),CHANGE)
      END DO
      IF (DB) PRINT *,'exit EDA_S3'
      RETURN
      END
