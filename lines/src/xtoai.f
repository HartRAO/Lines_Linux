**********************
      SUBROUTINE XTOAI (MEM,XVAL,IOUT)
**********************
*     calculate spectrum array index from given X value
*     assumes X values change monotonically, but not necessarily evenly
*     unlike VTOAI which assumes regular spacing
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      INTEGER   I                       ! loop index
      INTEGER   IP                      ! position check
      INTEGER   IOUT                    ! output array index of X
      INTEGER   MEM                     ! input memory in use
      INTEGER   SLOPE                   ! sign of dX/dI
      REAL*8    XVAL                    ! input X() value
*
      INCLUDE 'lines.inc'
*
      IP = 0
      SLOPE = +1
      IF (XIN(MEM,LAST_CH(MEM)) .LT. 
     &    XIN(MEM,FIRST_CH(MEM))) THEN
         SLOPE = -1
      END IF
*
      IF (DB) PRINT *,'in XTOAI: X(FIRST_CH)=',XIN(MEM,FIRST_CH(MEM)),
     & 'X(LAST_CH)=', XIN(MEM,LAST_CH(MEM)),' SLOPE=',SLOPE
      IF (DB) PRINT *,'in XTOAI: X(1)=',XIN(MEM,1),
     & 'X(NAXIS1(MEM))=', XIN(MEM,NAXIS1(MEM))
*
      IF (SLOPE .EQ. +1) THEN 
c          mod to enable VW to be widened after being reduced 2011-05-18
C          IF (XVAL .LE. XIN(MEM,FIRST_CH(MEM))) THEN
C              IOUT = FIRST_CH(MEM)
          IF (XVAL .LE. XIN(MEM,1)) THEN
              IOUT = 1
              IP = 1
              GO TO 900
          END IF
c          mod to enable VW to be widened after being reduced 2011-05-18
c          IF (XVAL .GE. XIN(MEM,LAST_CH(MEM))) THEN
c             IOUT = LAST_CH(MEM)
          IF (XVAL .GE. XIN(MEM,NAXIS1(MEM))) THEN
              IOUT = NAXIS1(MEM)
              IP = 2
              GO TO 900
          END IF
c          mod to enable VW to be widened after being reduced 2011-05-18
c          DO I = FIRST_CH(MEM), LAST_CH(MEM)-1
          DO I = 1, NAXIS1(MEM)-1
              IF (XVAL .GE. XIN(MEM,I) .AND.
     &            XVAL .LT. XIN(MEM,I+1)) THEN
                  IOUT = I
                  IP = 3
                  GO TO 900
              END IF
          END DO
      END IF
*
      IF (SLOPE .EQ. -1) THEN 
          IF (XVAL .GE. XIN(MEM,FIRST_CH(MEM))) THEN
              IOUT = FIRST_CH(MEM)
              IP = -1
              GO TO 900
          END IF
          IF (XVAL .LE. XIN(MEM,LAST_CH(MEM))) THEN
              IOUT = LAST_CH(MEM)
              IP = -2
              GO TO 900
          END IF
          DO I = FIRST_CH(MEM), LAST_CH(MEM)-1
              IF (XVAL .LT. XIN(MEM,I) .AND.
     &            XVAL .GE. XIN(MEM,I+1)) THEN
                  IOUT = I
                  IP = -3
                  GO TO 900
              END IF
          END DO
      END IF
      
 900  IF (DB) PRINT *,'in XTOAI at ',IP,' X=',XVAL,' IOUT=',IOUT
      RETURN
      END
*********
