      SUBROUTINE EDA_ENDPTS(DB,Y,N)
c     estimate smoohted values for both endpoints of the sequence in Y()
c     using the endpoint extrapolation rule.
c     all th evalues in Y() except the endpoints have been smoothed
      IMPLICIT NONE
      INTEGER N
      LOGICAL DB, CHANGE
      REAL*8  Y(N), Y0, YMED
c
      IF (DB) PRINT '(A,$)','in EDA_ENDPTS '
      CHANGE = .FALSE.
c     left end
      Y0 = 3.0D0*Y(2) - 2.0D0*Y(3)
      CALL EDA_MEDOF3(DB,Y0,Y(1),Y(2),YMED,CHANGE)
      Y(1) = YMED
c     right end
      Y0 = 3.0D0*Y(N-1) - 2.0D0*Y(N-2)
      CALL EDA_MEDOF3(DB,Y0,Y(N),Y(N-1),YMED,CHANGE)
      Y(N) = YMED
      IF (DB) PRINT *,'exit EDA_ENDPTS'
      RETURN
      END
