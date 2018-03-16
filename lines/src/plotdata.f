*************************
      SUBROUTINE PLOTDATA (ERROR)
*************************
*     plot data on XY plot using PGPLOT
*     developed MJG/JQ 03/08/95
*     rev 1996 07  18 MJG for LINES
*
*     other subroutines called:
*     pgask, pgbbuf, pgebuf, pgenv, pgerrx, pgerry, pgiden, pgline, pgmtxt,
*     pgpt, pgqinf, pgscf, pgsch, pgsci, pgsls, pgslw, pgsubp, plscale

      IMPLICIT NONE
*
      CHARACTER STATE*10          ! PGPLOT state buffer
      INTEGER   ERROR             ! output error state
      INTEGER   I                 ! counter
      INTEGER   LENGTH            ! buffer content length
      INTEGER   PLSCALE           ! external local autoscaling routine
      REAL      XLINE(2)          ! start, end of line joining points
      REAL      XPLOT, XL, XU     ! X plotting variables
      REAL      XPMINP            ! input XPMIN
      REAL      XPMAXP            ! input XPMAX
      REAL      YLINE(2)          ! start, end of line joining points
      REAL      YPLOT, YL, YU     ! Y plotting variables
      REAL      YPMINP            ! input YPMIN
      REAL      YPMAXP            ! input YPMAX
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in PLOTDATA'
      ERROR = 0
      XPMINP = XPMIN
      XPMAXP = XPMAX
      YPMINP = YPMIN
      YPMAXP = YPMAX
*
*     must check that a valid output device has been opened
*     PGQINF is a PGPLOT subroutine
*
      CALL PGQINF ('state',STATE,LENGTH)
      IF (INDEX(STATE,'OPEN') .EQ. 0) THEN
         PRINT *,'No PGPLOT output device connected '
         GO TO 999
      END IF
*
*     Always determine possible autoscales to protect against negative log data
*     with local function PLSCALE
*
      IF (PLSCALE(MOD(AXIS,20)/10,NPTS,XP,XBAR,IXBAR,XMIN,XMAX) .NE. 1)
     &    GO TO 990
      IF (PLSCALE(AXIS/20,NPTS,YP,YBAR,IYBAR,YMIN,YMAX) .NE. 1)
     &    GO TO 990
*
*     reset state of page prompting for interactive device
      CALL PGASK (PGPROMPT)
*
*     plotting actually starts 
      IF (DB) PRINT *,'NEW_SUBP =',NEW_SUBP
      IF (NEW_SUBP) THEN
*         subdivide new page with PGPLOT subroutine to latest NXSUB, NYSUB
          IF (DB) PRINT *,'call PGSUBP'
          CALL PGSUBP (NXSUB,NYSUB)
*         new plot on the page
          NEW_PAGE = .TRUE.
      END IF
*
*     for axes, set line style = full, colour = white, PGPLOT routines
      CALL PGSLS (1)
      CALL PGSCI (1)
*
*     set character size (default = 1), PGPLOT routine
      IF (CHARSIZE .EQ. 0.0) CHARSIZE = 1.0
      CALL PGSCH (CHARSIZE)      
*
*     set the character font
      CALL PGSCF (FONT)
*
      IF (DB) PRINT *,'NEW_PAGE =',NEW_PAGE
      IF (NEW_PAGE) THEN
*         new plot on this panel
          IF (XPMIN .GE. XPMAX) THEN
C             XPMINP = XPMIN
C             XPMAXP = XPMAX
*            autoscale the plot
             XPMIN = XMIN
             XPMAX = XMAX
          END IF
          IF (YPMIN .GE. YPMAX) THEN
*            autoscale the plot
C             YPMINP = YPMIN
C             YPMAXP = YPMAX
             YPMIN = YMIN
             YPMAX = YMAX
          END IF
*         specify the range of the axes in data units and draw a box
*         this calls PGOPEN
          IF (DB) PRINT *,'call PGENV'
          IF (DB) PRINT *,'XPMIN=',XPMIN,' XPMAX=',XPMAX,
     &        'YPMIN=',YPMIN,' YPMAX=',YPMAX,' AXIS=',AXIS
          CALL PGENV (XPMIN,XPMAX,YPMIN,YPMAX,0,AXIS)
      END IF
*
*     begin plot buffer
      CALL PGBBUF  
*
*     set the character font
      CALL PGSCF (FONT)
*
* these calls to PGMTXT should not be made if ADDed plot

*     Call PGMTXT to add a caption and label X and Y axes 
*     caption, left justified
*     second and third arguments are 1.0 and 1.0 for right justification
      CALL PGMTXT ('T', 1.6, 0.0, 0.0, TOPLBL)
      IF (NEW_PAGE) THEN
*         X axis label
          CALL PGMTXT ('B', 3.2, 0.5, 0.5, XLBL)
*         Y axis label
          CALL PGMTXT ('L', 2.2, 0.5, 0.5, YLBL)
      END IF
*     optionally call pgmtxt to add the Tsys below the caption
      IF (ADDTSYS) CALL PGMTXT ('T', 0.5, 0.0, 0.0, TSYSLBL)
*
*     add username, date, time if wanted
      IF (ADDIDENT) CALL PGIDEN
*
*     set user's colour for data and line 
      CALL PGSCI (COLOUR)
      CALL PGEBUF
*
*     mark points (coords in arrays X and Y), using SYMBOL in PGPLOT PGPT
*     done this way to avoid creating two new data arrays
      CALL PGBBUF
      DO I = 1, NPTS
          XPLOT = XP(I)
          YPLOT = YP(I)
          IF (AXIS .EQ. 10 .OR. AXIS .EQ. 30) XPLOT = ALOG10(XPLOT)
          IF (AXIS .EQ. 20 .OR. AXIS .EQ. 30) YPLOT = ALOG10(YPLOT)
          CALL PGPT (1,XPLOT,YPLOT,SYMBOL)
      END DO
      CALL PGEBUF
*
*     add X error bars with PGERRX; take logs if log axis specified
*     done this way to avoid creating two new data arrays
      IF (IXBAR .EQ. 1) THEN
          CALL PGBBUF     ! begin buffer
          DO I = 1, NPTS
              IF (XBAR(I) .NE. 0.0) THEN
                  XL = XP(I) - XBAR(I)
                  XU = XP(I) + XBAR(I)
                  IF (AXIS .EQ. 10 .OR. AXIS .EQ. 30) THEN
                      XL = ALOG10(XL)
                      XU = ALOG10(XU)
                  END IF
                  YPLOT = YP(I)
                  IF (AXIS .EQ. 20 .OR. AXIS .EQ. 30) THEN
                      YPLOT = ALOG10(YPLOT)
                  END IF
                  CALL PGERRX (1,XL,XU,YPLOT,1.0)
              END IF
          END DO
          CALL PGEBUF     ! end buffer
      END IF
*
*     add Y error bars with PGERRY; take logs if log axis specified
*     done this way to avoid creating two new data arrays
      IF (IYBAR .EQ. 1) THEN
          CALL PGBBUF
          DO I = 1, NPTS
              IF (YBAR(I) .NE. 0.0) THEN
                  YL = YP(I) - YBAR(I)
                  YU = YP(I) + YBAR(I)
                  IF (AXIS .EQ. 20 .OR. AXIS .EQ. 30) THEN
                      YL = ALOG10(YL)
                      YU = ALOG10(YU)
                  END IF
                  XPLOT = XP(I)
                  IF (AXIS .EQ. 10 .OR. AXIS .EQ. 30) THEN
                      XPLOT = ALOG10(XPLOT)
                  END IF
                  CALL PGERRY (1,XPLOT,YL,YU,1.0)
              END IF
          END DO
          CALL PGEBUF
      END IF
*
      IF (LINE .GT. 0 .AND. LINE .LE. 5) THEN
*
*         set the line type for joining the data points
          CALL PGSLS (LINE)
*
*         set the line width for joining the data points
          CALL PGSLW (WIDTH)
*
*         draw a line between points in X,Y arrays using PGLINE
*         done this way to avoid creating 2 new arrays
          CALL PGBBUF
          DO I = 1, NPTS-1
              XLINE(1) = XP(I)
              XLINE(2) = XP(I+1)
              IF (AXIS .EQ. 10 .OR. AXIS .EQ. 30) THEN
*                 log X axis
                  XLINE(1) = ALOG10(XLINE(1))
                  XLINE(2) = ALOG10(XLINE(2))
              END IF
*
              YLINE(1) = YP(I)
              YLINE(2) = YP(I+1)
              IF (AXIS .EQ. 20 .OR. AXIS .EQ. 30) THEN
*                 log Y axis
                  YLINE(1) = ALOG10(YLINE(1))
                  YLINE(2) = ALOG10(YLINE(2))
              END IF
              CALL PGLINE (2,XLINE,YLINE)
          END DO
          CALL PGEBUF
      END IF
*
*     Call PGSCI to reset the default colour 
      CALL PGSCI (1)
*
*     reset the default line width
      CALL PGSLW (1)
*
*     restore the input minima and maxima
       XPMIN = XPMINP
      XPMAX = XPMAXP
      YPMIN = YPMINP
      YPMAX = YPMAXP
*
      RETURN
*
  990 PRINT *,'negative data for log axis!'
  999 ERROR = 1
      RETURN
      END
*********