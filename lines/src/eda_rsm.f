      SUBROUTINE EDA_RSM (DB,Y,N,SMOOTH,ROUGH,VERSION,ERR)
c     controlling routine for median smoothing routines
      IMPLICIT NONE
      LOGICAL DB
      INTEGER N, VERSION, ERR, I
      REAL*8  Y(*), SMOOTH(*), ROUGH(*)
c     on entry,
c     Y() is a data sequence of N values
c     VERSION specifies the smoother:
c       1 = 3RSSH, twice
c       2 = 4253H, twice
c     on exit,
c     SMOOTH() and ROUGH() cotain the smooth and rough components from
c       Y() = SMOOTH() + ROUGH()
c
      IF (DB) PRINT '(A,$)','in EDA_RSM '
      IF (N .LE. 6 .OR. VERSION .LT. 1 .OR. VERSION .GT. 2) THEN
          ERR = 61
          PRINT *,'in RSM illegal N=',N,' or VERSION=',VERSION
          GO TO 999
      END IF
      DO I = 1, N
          SMOOTH(I) = Y(I)
      END DO
      IF (VERSION .EQ. 1) CALL EDA_S3RSSH(DB, SMOOTH, N, ERR)
      IF (VERSION .EQ. 2) CALL EDA_S4253H(DB, SMOOTH, N, ERR)
      IF (ERR .NE. 0) GO TO 999
      DO I = 1, N
          ROUGH(I) = Y(I) - SMOOTH(I)
      END DO
c     rerough smoothers ("twicing")
      IF (VERSION .EQ. 1) CALL EDA_S3RSSH(DB, ROUGH, N, ERR)
      IF (VERSION .EQ. 2) CALL EDA_S4253H(DB, ROUGH, N, ERR)
      IF (ERR .NE. 0) GO TO 999
      DO I = 1, N
          SMOOTH(I) = SMOOTH(I) + ROUGH(I)
          ROUGH(I) = Y(I) - SMOOTH(I)
      END DO
 999  IF (DB) PRINT *,'exit EDA_RSM'
      RETURN
      END
