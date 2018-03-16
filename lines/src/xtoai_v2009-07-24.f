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
      SLOPE = +1
      IF (XIN(MEM,LAST_CH(MEM)) .LT. 
     &    XIN(MEM,FIRST_CH(MEM))) THEN
         SLOPE = -1
      END IF
*
      PRINT *,'in XTOAI: X(FIRST_CH)=',XIN(MEM,FIRST_CH(MEM)),
     & 'X(LAST_CH)=', XIN(MEM,LAST_CH(MEM)),' SLOPE=',SLOPE
*
      IF (SLOPE .EQ. +1) THEN 
          IF (XVAL .LE. XIN(MEM,FIRST_CH(MEM))) THEN
              IOUT = FIRST_CH(MEM)
              IP = 1
              GO TO 900
          END IF
          IF (XVAL .GE. XIN(MEM,LAST_CH(MEM))) THEN
              IOUT = LAST_CH(MEM)
              IP = 2
              GO TO 900
          END IF
          DO I = FIRST_CH(MEM), LAST_CH(MEM)-1
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
      
 900  PRINT *,'in XTOAI at ',IP,' X=',XVAL,' IOUT=',IOUT
      RETURN
      END
*********
