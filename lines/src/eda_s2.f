      SUBROUTINE EDA_S2(DB,Y,N,ENDSAV)
c     smooth by running medians of 2
c     used to recenter results of running medians of 4
      IMPLICIT NONE
      LOGICAL DB
      INTEGER N, NM1, I
      REAL*8  Y(N), ENDSAV, TWO
      DATA TWO/2.0D0/
c     even-length medians offset the output sequence to the high end,
c     since they cannot be symmetric.
c     ENDSAV is left holding Y(N) since there is no other room for it.
c     Y(1) is unchanged
c
      IF (DB) PRINT '(A,$)','in EDA_S2 '
      NM1 = N-1
      DO I = 2, NM1
          Y(I) = (Y(I+1)+Y(I))/TWO
      END DO
      Y(N) = ENDSAV
      IF (DB) PRINT *,'exit EDA_S2'
      RETURN
      END
