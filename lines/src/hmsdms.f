************************
       SUBROUTINE HMSDMS (RA, DEC)
************************
*     read RA and DEC in decimal degrees or HMS DMS, 
*     and convert to decimal degrees
*     inputs  : none
*     outputs :
*         RA, DEC     REAL*8    ra and dec in decimal degrees
*
*     other subroutines called:
*     none
*
      INTEGER   I, INPUT
      REAL*8    P(6), RA, DEC
      DATA      INPUT / 0 /
*
   20 IF (INPUT .EQ. 0) THEN
          PRINT *,'Is RA, DEC in degrees (1) or H M S, D M S (2) ?'
          READ  *,INPUT
      END IF
*
      IF (INPUT .EQ. 1) THEN
          PRINT *,'Enter RA, DEC in decimal degrees (/ exits)'
          READ  *, RA, DEC
      ELSE IF (INPUT .EQ. 2) THEN
          P(1) = -999.
          PRINT *,'Enter RA, DEC in H M S, D M S (/ exits)'
          READ *, (P(I),I = 1,6)
          IF (P(1) .NE. -999.0) THEN
*             coordinates were entered by the user so convert to degrees
              RA = ((P(1)*60 + P(2))*60 + P(3)) / 240
              DEC = ABS(P(4)) + ABS(P(5))/60 + P(6)/3600
*             check sign on dec
              IF (P(4) .LT. 0 .OR.
     &            P(4) .EQ. 0 .AND. P(5) .LT. 0) THEN
                  DEC = -DEC
              END IF
          END IF
      ELSE
          PRINT *,'illegal choice'
      END IF
      RETURN
      END
