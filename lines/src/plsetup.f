*********************************
       SUBROUTINE PLSETUP (ERROR)
*********************************
*      group of commands for automatic plot
*
*     CMDP command parameters used by routines called:
*     1 = command mnemonic PL
*     2/3/4 = ADD to plot on previous plot
*     2/3/4 = CAP to use set caption
*     2/3/4 = memory from which to plot the spectrum
*
*     other subroutines called:
*     plconfau, pgslct, plotdata
*
      IMPLICIT NONE
      INTEGER   ERROR           ! output error status
      INTEGER   NCHAR           ! external local function
*
      INCLUDE  'lines.inc'
*
      IF (DB) PRINT *,'in PLSETUP'


          IF (IDPLOT1 .GT. 0 .OR. IDPLOT2 .GT. 0) THEN
*             store data and captions in plot arrays
              CALL PLCONFAU (ERROR)
              IF (ERROR .GT. 0) GO TO 900
          END IF
*
*         plot to interactive device
*
          IF (IDPLOT1 .GT. 0) THEN
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT 1000, PLOTDEV
              CALL PGSLCT (IDPLOT1)
              CALL PLOTDATA (ERROR)
          ELSE
              PRINT 1001
          END IF
*
*         plot to postscript file
*
          IF (PLOTPS(1:3) .NE. 'off' .AND. PLOTPS(1:3) .NE. 'OFF') THEN
              IF (IDPLOT2 .GT. 0) THEN
*                 PLOTPS has been opened - select as the output device
                  PRINT 1000, PLOTPS(1:NCHAR(PLOTPS))
                  CALL PGSLCT (IDPLOT2)
                  CALL PLOTDATA (ERROR)
              END IF
          ELSE
              PRINT 1002
          END IF
*
*         default to no new subpanel division if turned on
*         had been reset false in PLOTDATA after call to PGSUBP
          NEW_SUBP = .FALSE.
*         default to  new page on again if turned off
          NEW_PAGE = .TRUE.
  900 RETURN
 1000 FORMAT (' plot to ',A)
 1001 FORMAT (' no screen plot device - set plotdev')
 1002 FORMAT (' no postscript output file - set plotps')
      END
*********

