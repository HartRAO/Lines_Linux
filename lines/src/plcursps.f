*************************
      SUBROUTINE PLCURSPS (ERROR)
*************************
*     return cursor positions
*     rev 060104 to find array index of nearest points to cursor positions
*
*     CMDP command parameters used:
*     1 = command mnemonic PGC
*
*     other subroutines called:
*     pgqinf, pgncur
*
      IMPLICIT NONE
      CHARACTER ANSWER*1          ! user answer
      CHARACTER CURSOR*3          ! cursor status
      INTEGER I                   ! loop index
      INTEGER II                  ! loop index
      INTEGER IAI                 ! array index when XCMINUSXP is smallest
      INTEGER ERROR               ! output error status
      INTEGER LENGTH              ! number of characters returned in CURSOR
      INTEGER SYMBOLP             ! input value of SYMBOL
      REAL    XCMINUSXPII         ! cursor position - point position
      REAL    XCMINUSXP           ! cursor position - point position min 
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in PLCURSPS'
      ERROR = 0
      NPTC = 0
      SYMBOLP = SYMBOL
      SYMBOL = 14                 ! hollow square
*
*     see if display is interactive with a cursor
      CALL PGQINF('cursor', CURSOR, LENGTH)
*
      IF (CURSOR(1:LENGTH) .EQ. 'YES') THEN
          PRINT '(/A)',' How to mark points with the cursor: '
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
                      IAI = II
                      IF (ABS(XCMINUSXPII) .LT. ABS(XCMINUSXP)) THEN
                          XCMINUSXP = XCMINUSXPII
c                          IAI = II
                      END IF
                      IF (DB) PRINT *,'II= ',II, 
     &                   ' XCMINUSXP=',XCMINUSXP, 
     &                   ' XCMINUSXPII=',XCMINUSXPII, ' IAI=',IAI 
                  END DO
*
                  PRINT *,'in PLCURSPS XCURS=', XCURS(I),
     &             ' YCURS=', YCURS(I),'IAI=', IAI
              END DO
          END IF
      END IF
      SYMBOL = SYMBOLP
      RETURN
      END
*************************
