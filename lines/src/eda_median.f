      REAL*8 FUNCTION EDA_MEDIAN(DB,Y,N)
c find the median of the sorted values Y(1),... Y(N)
      IMPLICIT NONE
      LOGICAL DB
      INTEGER N, MPTR, MPT2
      REAL*8  Y(N)
C
      IF (DB) PRINT '(A,$)','in EDA_MEDIAN '
      MPTR = N/2 + 1
      MPT2 = N - MPTR + 1
      EDA_MEDIAN = (Y(MPTR) + Y(MPT2))/2.0
      IF (DB) PRINT *,'exit EDA_MEDIAN'
      RETURN
      END
