***********************
      SUBROUTINE PLTEXT
***********************
*     add text to graph at points marked by user
*
      IMPLICIT NONE
      CHARACTER CH*3              ! character typed by user (click) 
      CHARACTER CURSOR*3          ! cursor status
      LOGICAL   TXTTOADD          ! true if non-blank text to be written
      INTEGER I                   ! loop index
      INTEGER ICURS               ! PGCURS return status
      INTEGER LENGTH              ! number of characters returned in CURSOR
      INTEGER LENTXT              ! length of ADDTXT label
      INTEGER NCHAR               ! external function
      INTEGER PGCURS              ! PGPLOT function
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in PLTEXT'
*     see if display is interactive with a cursor
      CALL PGQINF('cursor', CURSOR, LENGTH)
*
      IF (DB) PRINT *,'cursor=',CURSOR
      IF (CURSOR(1:LENGTH) .EQ. 'YES') THEN
*         interactive plot
          IF (DB) PRINT *,'NTXT=',NTXT 
          IF (NTXT .LT. MAXPTC) THEN
              TXTTOADD = .TRUE.
          ELSE
              TXTTOADD = .FALSE.
          END IF
*
          DO WHILE (TXTTOADD)
              PRINT *,'Text to add (Enter = none) ?'
              NTXT = NTXT + 1
              READ '(A)',BUF
              LENTXT = NCHAR(BUF)
              IF (LENTXT .EQ. 1 .AND. BUF(1:1) .EQ. ' ') THEN
                  TXTTOADD = .FALSE.
                  NTXT = NTXT - 1
              END IF     
*
              IF (TXTTOADD) THEN
*
*                 store text in large buffer
                  IF (NTXT .GT. 1) THEN
                      TXTSTART(NTXT) = TXTEND(NTXT-1)+1
                  ELSE
                      TXTSTART(NTXT) = 1
                  END IF
                  TXTEND(NTXT) = TXTSTART(NTXT)+LENTXT
                  LBUF(TXTSTART(NTXT):TXTEND(NTXT)) = BUF(1:LENTXT)
*
                  PRINT *,'Click at point to start text'
*                 cursor position is taken when user presses key
*                 or clicks mouse button
                  ICURS = PGCURS (XCURS(NTXT), YCURS(NTXT), CH)
                  IF (ICURS .NE. 1) THEN
                      PRINT *,'error reading cursor'
                  ELSE
*                     add label at cursor position                          
                      CALL PGTEXT (XCURS(NTXT), YCURS(NTXT), 
     &                    LBUF(TXTSTART(NTXT):TXTEND(NTXT)))
                  END IF
              END IF
              IF (NTXT .EQ. MAXPTC) TXTTOADD = .FALSE.
          END DO
      ELSE
*         non-interactive - assume writing to postscript plot
          DO I = 1, NTXT
              CALL PGTEXT (XCURS(I), YCURS(I), 
     &            LBUF(TXTSTART(I):TXTEND(I)))
          END DO          
      END IF
      RETURN
      END
      