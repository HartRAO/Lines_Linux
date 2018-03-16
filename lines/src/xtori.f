**********************
      SUBROUTINE XTORI (MEM,XVAL,RI)
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
      INTEGER   MEM                     ! input memory in use
      INTEGER   SLOPE                   ! sign of dX/dI
      REAL*8    RI                      ! output real array index of vel
      REAL*8    XVAL                    ! input X() value
*
      INCLUDE 'lines.inc'
*
      SLOPE = +1
      IF (XIN(MEM,FIRST_CH(MEM)+1) .LT. 
     &    XIN(MEM,FIRST_CH(MEM))) THEN
         SLOPE = -1
      END IF
*
c      PRINT *,'in XTORI: X(FIRST_CH)=',XIN(MEM,FIRST_CH(MEM)),
c     & 'X(LAST_CH)=', XIN(MEM,LAST_CH(MEM)),' SLOPE=',SLOPE
*
      RI = FIRST_CH(MEM)
      DO I = FIRST_CH(MEM), LAST_CH(MEM)-1
          IF (SLOPE .EQ. +1) THEN 
              IF (XVAL .GE. XIN(MEM,I) .AND.
     &            XVAL .LT. XIN(MEM,I+1)) THEN
                  RI = I
c                  PRINT *,'in XTORI at 1 X=',XVAL,' I=',RI
                  RETURN
              END IF
          ELSE IF (SLOPE .EQ. -1) THEN 
              IF (XVAL .LE. XIN(MEM,I) .AND.
     &            XVAL .GT. XIN(MEM,I+1)) THEN
                  RI = I
c                  PRINT *,'in XTORI at 2 X=',XVAL,' I=',RI
                  RETURN
              END IF
          END IF
      END DO
c      PRINT *,'in XTORI at 3  X=',XVAL,' I=',RI
      RETURN
      END
*********
