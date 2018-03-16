****************
      SUBROUTINE QMEMUSED 
****************
*     find number of memories in use
*
      IMPLICIT NONE
      INTEGER I
      INCLUDE 'lines.inc'
*
      MEMUSED = 0
      DO I = 1, MAXMEM
          IF (NAXIS(I) .GT. 0) THEN
             MEMUSED = MEMUSED + 1
          END IF
      END DO
      RETURN
      END
*********