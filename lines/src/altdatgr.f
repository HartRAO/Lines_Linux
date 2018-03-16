*************************
      SUBROUTINE ALTDATGR (ERROR)
*************************
*     return cursor positions and use to alter data to lines fitted 
*     between pairs of points
*     developed from plcursps.f
*     rev 060410 to find array index of nearest points to cursor positions
*
*     CMDP command parameters used:
*     1 = command mnemonic AG
*
*     other subroutines called:
*     pgqinf, pgncur
*
      IMPLICIT NONE
      CHARACTER ANSWER*1          ! user answer
      CHARACTER CURSOR*3          ! cursor status
      INTEGER I                   ! loop index
      INTEGER II                  ! loop index
      INTEGER J                   ! loop index
      INTEGER K                   ! loop index
      INTEGER L                   ! loop index
      INTEGER ERROR               ! output error status
      INTEGER LENGTH              ! number of characters returned in CURSOR
      INTEGER SYMBOLP             ! input value of SYMBOL
      REAL    CONST               ! linear regression constant
      REAL    SLOPE               ! linear regression slope
      REAL    XCMINUSXPII         ! cursor position - point position
      REAL    XCMINUSXP           ! cursor position - point position min 
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in ALTDATGR'
      ERROR = 0
      NPTC = 0
      SYMBOLP = SYMBOL
      SYMBOL = 14                 ! hollow square
*
*     see if display is interactive with a cursor
      CALL PGQINF('cursor', CURSOR, LENGTH)
*
      IF (CURSOR(1:LENGTH) .EQ. 'YES') THEN
          PRINT '(/A)',' Alter plotted data graphically'
          PRINT '(/A)',' How to mark points in pairs with the cursor:'
          PRINT *,' mouse left button or A = Add new point' 
          PRINT *,'     centre button or D = Delete nearest point'
          PRINT *,'      right button or X = eXit'
          PRINT *,' coordinates returned are sorted on X'
c
c          PRINT '(/2A,$)',' Mark points with cursor and ',
c     &           'get coordinates (Enter = Yes) ?'
c          READ '(A)', ANSWER
c          CALL UPCASE (ANSWER)
c
          ANSWER = 'Y'
          IF (ANSWER .NE. 'N') THEN
              IF (DB) PRINT *,'MAXPTC = ',MAXPTC,' NPTC =',NPTC,
     &                        ' SYMBOL = ',SYMBOL
              CALL PGNCUR (MAXPTC,NPTC,XCURS,YCURS,SYMBOL)
              PRINT *,'Points were marked at:'
              PRINT *,'Xcurs        Ycurs  Array index of nearest point'
              NPTC = MIN (NPTC, MAXPTC)
              DO I = 1,NPTC
*                 allow for log axes
                  IF (AXIS .EQ. 10 .OR. AXIS .EQ. 30) THEN
                      XCURS(I) = 10**XCURS(I)
                  END IF
                  IF (AXIS .EQ. 20 .OR. AXIS .EQ. 30) THEN
                      YCURS(I) = 10**YCURS(I)
                  END IF
*
*                 find nearest channel (index of point in array)
*                 using the array of plotted points XP
                  XCMINUSXP = XCURS(I) - XP(1)
                  IF (DB) PRINT *, 'NPTS=',NPTS, ' NPTC=',NPTC
                  DO II = 1, NPTS
                      XCMINUSXPII = XCURS(I) - XP(II)
                      IF (ABS(XCMINUSXPII) .LT. ABS(XCMINUSXP)) THEN
                          XCMINUSXP = XCMINUSXPII
                          IAR(I) = II
                      END IF
                      IF (DB) PRINT *,'II= ',II, 
     &                   ' XCMINUSXP=',XCMINUSXP, 
     &                   ' XCMINUSXPII=',XCMINUSXPII, ' IAR(I)=',IAR(I)
                  END DO
*
                  PRINT *, XCURS(I), YCURS(I), IAR(I)
              END DO
*
*              PRINT *,'Check that marked points are correct'
*              PRINT *,'Continue with data replacement (Y/n)?'
*              READ '(A)', ANSWER
*              CALL UPCASE (ANSWER)
*              IF (ANSWER .EQ. 'N') THEN
*                 PRINT *,'Data were NOT replaced'
*                 GO TO 100
*              END IF
*
              IF (DB) PRINT *,'Pairs of points=', NPTC/2
*             linear regression between start and end point of each pair
              DO J = 1, NPTC/2
*                 for each pair of start and end points
                  K = J*2 - 1
                  SLOPE = (YCURS(K)-YCURS(K+1))/(XCURS(K)-XCURS(K+1))
                  CONST = YCURS(K) - SLOPE * XCURS(K)
                  IF (DB) PRINT *, 'J=',J,' K=',K,' SLOPE=',SLOPE,
     &                ' CONST=',CONST
                  DO L = IAR(K), IAR(K+1)
*                     air() = array of indicies of points marked by cursor
*                     YP(L) = XP(L)*SLOPE + CONST
                      YIN(MEMPLOTTED,L) = XIN(MEMPLOTTED,L)*SLOPE+CONST
                      IF (DB) PRINT *,'J=',J,' K=',K,
     &                                ' L=IAR(K - K+1)=',L
                  END DO
              END DO
              IF (NPTC/2 .GT. 1) THEN
                  PRINT *,'data in memory',MEMPLOTTED,' were altered'
              END IF
          END IF
      END IF
      SYMBOL = SYMBOLP
      RETURN
      END
*************************
