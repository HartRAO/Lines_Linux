      SUBROUTINE EDA_SPLIT(DB, Y, N, CHANGE)
c     find 2-flats in Y() and apply splitting algorithm
      IMPLICIT NONE
      LOGICAL CHANGE, DB
      INTEGER N, NM2, I1, I
      REAL*8  Y(N), Y1, W(6)
c
c     W() is a window 6 points wide which is slid along Y()
c
      IF (DB) PRINT '(A,$)', 'in EDA_SPLIT '
      NM2 = N - 2
      DO I = 1, 4
          W(I+2) = Y(I)
      END DO
c     if Y(1)=Y(2) .ne. Y(3), treat first 2 like a 2-flat with end pt rule
      W(2) = Y(3)
      I1 = 1
   20 IF (W(3) .NE. W(4)) GO TO 40
      IF ((W(3)-W(2)) * (W(5)-W(4)) .GE. 0.0D0) GO TO 40
      IF (I1 .GE. 3) GO TO 30
      Y1 = 3.0D0 * W(2) - 2.0D0 * W(1)
      CALL EDA_MEDOF3(DB, Y1, W(3), W(2), Y(I1), CHANGE)
   30 IF (I1 .GE. NM2) GO TO 40
      Y1 = 3.0D0 * W(5) - 2.0D0 * W(6)
      CALL EDA_MEDOF3(DB, Y1, W(4), W(5), Y(I1+1), CHANGE)
C     slide window
   40 DO I = 1, 5
          W(I) = W(I+1)
      END DO
      I1 = I1+1
      IF (I1 .GE. NM2) GO TO 60
      W(6) = Y(I1+3)
      GO TO 20
c     apply rule to last two points if needed
   60 W(6) = W(3)
      IF (I1 .LT. N) GO TO 20
      IF (DB) PRINT *,'exit EDA_SPLIT'
      RETURN
      END
