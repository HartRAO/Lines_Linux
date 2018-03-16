      SUBROUTINE EDA_S3RSSH(DB,Y, N, ERR)
c     smooth Y() by 3RSSH, twice
      IMPLICIT NONE
      INTEGER N, ERR
      REAL*8  Y(*)
      LOGICAL CHANGE, DB
c
      IF (DB) PRINT '(A,$)','in EDA_S3RSSH '
      CALL EDA_S3R(DB,Y,N)
      CHANGE = .FALSE.
      CALL EDA_SPLIT(DB,Y,N,CHANGE)
      IF (CHANGE) THEN
          CALL EDA_S3R(DB,Y,N)
          CHANGE = .FALSE.
          CALL EDA_SPLIT(DB,Y,N,CHANGE)
          IF(CHANGE) CALL EDA_S3R(DB,Y,N)
      END IF
      CALL EDA_HANN(DB,Y,N)
      IF (DB) PRINT *,'exit EDA_S3RSSH'
      RETURN
      END
