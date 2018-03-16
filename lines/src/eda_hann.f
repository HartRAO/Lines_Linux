      SUBROUTINE EDA_HANN(DB,Y,N)
c     3-point smooth by moving averages weighted 1/4, 1/2, 1/4
      IMPLICIT NONE
      LOGICAL db
      INTEGER N, I, NM1
      REAL*8  Y(N), Y1, Y2, Y3
c
      IF (DB) PRINT '(A,$)','in EDA_HANN '
      NM1 = N-1
      Y2 = Y(1)
      Y3 = Y(2)
      DO I = 2, NM1
          Y1 = Y2
          Y2 = Y3
          Y3 = Y(I+1)
          Y(I) = (Y1 + Y2 + Y2 + Y3) / 4.0D0
      END DO
      IF (DB) PRINT *,'exit EDA_HANN'
      RETURN
      END
