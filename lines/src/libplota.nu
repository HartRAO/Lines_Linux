C LIBPLOTA
C subroutines to use PGPLOT in PLOTA environment
C developed MJG/JQ 04/06/95
C rev MJG 09/11/95 various subroutines renamed, all implicit none
C rev MJG 05/01/96 for PGPLOT V5.0.3 removing double backslashes
C                  change dataname from 40 to * characters
***********************
      SUBROUTINE PGADDLBL (DATANAME,TOPLBL)
***********************
c     put data info or comments into top labels
C
      IMPLICIT    NONE
      INTEGER     I               ! local
      INTEGER     NCHAR           ! external function
      CHARACTER   ANSWER*1        ! local user's answer
      CHARACTER   DATANAME*(*)    ! input data file info
      CHARACTER   TOPLBL(4)*80    ! input/output top labels
c
      PRINT '(A,$)',' Add data name to a top label (Enter=YES) ?'
      READ '(A)', ANSWER
      CALL UPCASE (ANSWER)
      IF (ANSWER .NE. 'N' .AND. ANSWER .NE. 'F') THEN
          DO I = 1, 4
              IF (NCHAR(TOPLBL(I)) .LE. 1) THEN
                  TOPLBL(I) = DATANAME
                  GO TO 10
              END IF
          END DO
      END IF
C
   10 CONTINUE
      PRINT '(A,$)',' List formatting options for comments (Enter=NO) ?'
      READ '(A)', ANSWER
      CALL UPCASE (ANSWER)
      IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') THEN
          PRINT *,' \u   : superscript, end with \d'
          PRINT *,' \d   : subscript, end with \u'
          PRINT *,' \b   : backspace'
          PRINT *,' \A   : Angstroms'
          PRINT *,' \x   : multiplication sign'
          PRINT *,' \\   : backslash'
          PRINT *,' \gx  : greek equivalent of x'
          PRINT *,' \fn  : switch to Normal font'
          PRINT *,' \fr  : switch to Roman  font'
          PRINT *,' \fi  : switch to Italic font'
          PRINT *,' \fs  : switch to Script font'
          PRINT *,' \mn or \mnn : graph marker number n or nn'
          PRINT *,' \(nnn) : Hershey symbol nnn'
      END IF
*
      DO I = 1, 4
          IF (NCHAR(TOPLBL(I)) .LE. 1) THEN
              PRINT *,'Optional comment for label ',I,' ?'
              READ '(A)',TOPLBL(I)
          END IF
      END DO
      RETURN
      END
**************************
      SUBROUTINE PGASKAUTO (AUTOCONFIG)
**************************
      IMPLICIT NONE
      LOGICAL AUTOCONFIG      ! output true to autoconfig, false to customize
      CHARACTER ANSWER*1      ! local user answer
*
      AUTOCONFIG = .TRUE.
      PRINT '(A,$)',' Customize the plot (Enter=NO) ?'
      READ '(A)', ANSWER
      CALL UPCASE (ANSWER)
      IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') THEN
          AUTOCONFIG = .FALSE.
      END IF
      RETURN
      END
C*************************
      SUBROUTINE PGASKAXIS (XMIN,XMAX,YMIN,YMAX,AXIS) 
C*************************
C     find axis options for pgplot
C
      IMPLICIT NONE
      REAL    XMIN            ! input X minimum
      REAL    XMAX            ! input X maximum
      REAL    YMIN            ! input Y minimum
      REAL    YMAX            ! input Y maximum
      INTEGER AXIS            ! output axis type
      CHARACTER ANSWER*3      ! read buffer
C
C     log axes permitted only for positive numbers
C
      AXIS = 0
      PRINT '(A)',
     &'  AXIS controls the plotting of axes, tick marks, etc:',
     &'  AXIS = -2 : draw no box, axes or labels;',
     &'  AXIS = -1 : draw box only;',
     &'  AXIS =  0 : draw box and label it with coordinates;',
     &'  AXIS =  1 : same as AXIS=0, but also draw the',
     &'              coordinate axes (X=0, Y=0);',
     &'  AXIS =  2 : same as AXIS=1, but also draw grid lines',
     &'            at major increments of the coordinates;'
      IF (XMIN .GT. 0.0) PRINT '(A)',
     &'  AXIS = 10 : draw box and label X-axis logarithmically;'
      IF (YMIN .GT. 0.0) PRINT '(A)',
     &'  AXIS = 20 : draw box and label Y-axis logarithmically;'
      IF (XMIN .GT. 0.0 .AND. YMIN .GT. 0.0) PRINT '(A)',
     &'  AXIS = 30 : draw box and label both axes logarithmically.'
  101 PRINT '(A,I2,A,$)',' Axis type (Enter=',AXIS,') ?' 
      READ '(A)', ANSWER
      ANSWER(3:3) = '/'
      READ  (ANSWER,*,ERR=101) AXIS
      RETURN
      END
****************************
      SUBROUTINE PGASKERRBAR (IBAR,AXCHAR)
****************************
*     plot error bars ?
*
      IMPLICIT NONE
      CHARACTER ANSWER*1      ! local user's answer
      CHARACTER AXCHAR*1      ! input axis ID
      INTEGER   IBAR          ! input/output plot err bars option
C
C     error bar plotting wanted ?
C
      IF (IBAR .EQ. 1) THEN
          PRINT '(3A,$)',' Plot ',AXCHAR,' error bars (Enter=YES) ?'
          READ '(A)', ANSWER
          CALL UPCASE (ANSWER)
          IF (ANSWER .EQ. 'N' .OR. ANSWER .EQ. 'F') IBAR = 0
      END IF
      RETURN
      END
**************************
      SUBROUTINE PGASKFONT (FONT)
**************************
C     specify wanted font
C
      IMPLICIT NONE
      INTEGER FONT
      CHARACTER ANSWER*2
C
      FONT = 1
  101 PRINT '(2A,I1,A,$)',
     &' Font: Normal=1, Roman=2, Italic=3, Script=4, ',  
     &'(Enter=',FONT,') ?'
      READ '(A)', ANSWER
      ANSWER(2:2) = '/'
      READ  (ANSWER,*,ERR=101) FONT
      IF (FONT .LT. 1 .OR. FONT .GT. 4) FONT = 1
C
      RETURN
      END
***************************
      SUBROUTINE PGASKIDENT (ADDIDENT)
***************************
      IMPLICIT NONE
      LOGICAL ADDIDENT    ! output true to add ID to plot
      CHARACTER ANSWER*1  ! local user's reply
*
      ADDIDENT = .FALSE.
      PRINT '(A,$)',' Stamp username, date, time on plot (Enter=NO) ?'
      READ '(A)', ANSWER
      CALL UPCASE (ANSWER)
      IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') ADDIDENT = .TRUE.
      RETURN
      END
C*************************
      SUBROUTINE PGASKLINE (SYMBOL, LINE, WIDTH, COLOUR) 
C*************************
C     find line options for pgplot
C
      IMPLICIT NONE
      INTEGER SYMBOL, LINE, WIDTH, COLOUR
      CHARACTER ANSWER*3
*
      SYMBOL = 1
      PRINT *,' See PGPLOT manual for table of marker symbols'
  101 PRINT '(A,I2,A,$)',' Symbol number (Enter=',SYMBOL,') ?'
      READ '(A)', ANSWER
      ANSWER (3:3) = '/'
      READ  (ANSWER,*,ERR=101) SYMBOL
*
      LINE = 1
      PRINT *,' Line type :'
      PRINT *,' None=0, Full=1, Dashed=2, Dotdash=3, Dotted=4, Fancy=5'
  201 PRINT '(A,I1,A,$)',' Wanted line type (Enter=',LINE,') ?'
      READ '(A)', ANSWER
      ANSWER (3:3) = '/'
      READ (ANSWER,*,ERR=201) LINE
      IF (LINE .LT. 0 .OR. LINE .GT. 5) LINE = 1
*
      WIDTH = 1
      PRINT *,' Line width can be set in units of 0.13 mm'
  301 PRINT '(A,I2,A,$)',' Line width (ENTER=',WIDTH,' units) ?'
      READ '(A)', ANSWER
      ANSWER (3:3) = '/'
      READ (ANSWER,*,ERR=301) WIDTH
      IF (WIDTH .LT. 1) WIDTH = 1 
*
      COLOUR = 1
      PRINT *,' Colours for data markers and line:'
      PRINT *,' Black=0, White=1, Red=2, Green=3, Blue=4, Cyan=5,', 
     &        'Magenta=6, Yellow=7'
  401 PRINT '(A,I1,A,$)',' Wanted colour (Enter=',COLOUR,') ?'
      READ '(A)', ANSWER
      ANSWER (3:3) = '/'
      READ (ANSWER,*,ERR=401) COLOUR
      IF (COLOUR .LT. 0 .OR. COLOUR .GT. 7) COLOUR = 1
*
      RETURN
      END
***************************
      SUBROUTINE PGASKPANEL (NXSUB,NYSUB)
***************************
      IMPLICIT NONE
      CHARACTER   ORDER*1     ! local define row or column order
      INTEGER     NXSUB       ! output number of subdivisions of axis
      INTEGER     NYSUB       ! output number of subdivisions of axis
      CHARACTER   ANSWER*3
C
      NXSUB = ABS(NXSUB)
  101 PRINT 1000,'X',NXSUB
 1000 FORMAT (' Number of subdivisions of ',A1,
     &        ' axis (Enter=',I3,') ?',$)
      READ '(A)', ANSWER
      ANSWER(3:3) = '/'
      READ  (ANSWER,*,ERR=101) NXSUB
*
  201 PRINT 1000,'Y',NYSUB
      READ '(A)', ANSWER
      ANSWER(3:3) = '/'
      READ  (ANSWER,*,ERR=201) NYSUB
*
      IF (NXSUB .GT. 1 .AND. NYSUB .GT. 1) THEN
          PRINT '(A,$)',' Plot in row (+) or column (-) order ?'
          READ  '(A)',ORDER
          IF (ORDER .EQ. '-') NXSUB = -NXSUB
      END IF
      RETURN
      END
***************************
      SUBROUTINE PGAUTOCONF
     &        (FORCE_NEW_PAGE,FIRST_ON_PANEL,NXSUB,NYSUB,
     &        ADDIDENT,DATANAME,TOPLBL,XLBL,YLBL,IXBAR,IYBAR,
     &        NPTS,X,XBAR,Y,YBAR,XMIN,XMAX,YMIN,YMAX,
     &        AXIS,XPMIN,XPMAX,YPMIN,YPMAX,
     &        SYMBOL,LINE,WIDTH,COLOUR,FONT,
     &        XCHAR,YCHAR)
*************************
C     automatic configuration of PGPLOT (default)
C
      IMPLICIT NONE
      CHARACTER DATANAME*(*)      ! data caption
      CHARACTER TOPLBL(4)*80      ! top labels 1 - 4 on graph
      CHARACTER XCHAR*1           ! 'X'
      CHARACTER XLBL*(*)          ! X axis label
      CHARACTER YCHAR*1           ! 'Y'
      CHARACTER YLBL*(*)           ! Y axis label
      LOGICAL   ADDIDENT          ! true to add name date time to plot
      LOGICAL   FORCE_NEW_PAGE    ! true to force a fresh new page
      LOGICAL   FIRST_ON_PANEL    ! true for first plot on panel 
      INTEGER   AXIS              ! PGPLOT axis type eg box, log, linear etc
      INTEGER   COLOUR            ! PGPLOT colour of data points and line
      INTEGER   FONT              ! PGPLOT font for labels
      INTEGER   IXBAR             ! 1 to plot error bars else 0
      INTEGER   IYBAR             ! 1 to plot error bars else 0
      INTEGER   LINE              ! PGPLOT line type joining data points
      INTEGER   NPTS              ! number of valid data points
      INTEGER   NXSUB             ! number of subdivisions of page in X
      INTEGER   NYSUB             ! number of subdivisions of page in Y
      INTEGER   SYMBOL            ! PGPLOT symbol to plot at each datum
      INTEGER   WIDTH             ! PGPLOT width of line joining data points
      REAL      X(*)              ! x values
      REAL      XBAR(*)           ! x errors
      REAL      XMAX              ! X max
      REAL      XMIN              ! X min
      REAL      XPMAX             ! X plot max
      REAL      XPMIN             ! X plot min
      REAL      Y(*)              ! y values
      REAL      YBAR(*)           ! y errors
      REAL      YMAX              ! Y max
      REAL      YMIN              ! Y min
      REAL      YPMAX             ! Y plot max
      REAL      YPMIN             ! Y plot min
C
      FORCE_NEW_PAGE = .FALSE.
      FIRST_ON_PANEL = .TRUE.
      ADDIDENT = .TRUE.
C     clear existing top labels 
      CALL CLRLBL (TOPLBL(1))
      CALL CLRLBL (TOPLBL(2))
      CALL CLRLBL (TOPLBL(3))
      CALL CLRLBL (TOPLBL(4))
      TOPLBL(1) = DATANAME
C     force axis autoscaling on plot
      XPMIN = 0
      XPMAX = -1
      YPMIN = 0
      YPMAX = -1
C     use suitable PGPLOT defaults
      AXIS = 0
      SYMBOL = 1
      LINE = 1
      WIDTH = 1
      COLOUR = 1
      FONT = 1
C
      RETURN
      END
************************
      SUBROUTINE PGAXLIM (VMIN,VMAX,AXCHAR,VPMIN,VPMAX,AUTOSCALE)
************************
*     ask user if autoscaling, return plot min and max
C
      IMPLICIT  NONE
      CHARACTER ANSWER*30     ! local user answer
      CHARACTER AXCHAR*1      ! input axis identifier, X, Y, Z
      LOGICAL   AUTOSCALE     ! output true if autoscaling
      REAL    VMIN            ! input data minimum
      REAL    VMAX            ! input data maximum
      REAL    VPMIN           ! output plot minimum
      REAL    VPMAX           ! output plot maximum
C
      AUTOSCALE = .TRUE.
      PRINT *, ' ',AXCHAR,' min =',VMIN,' ',AXCHAR,' max =',VMAX
      PRINT '(3A,$)',' Autoscale the ',AXCHAR,' axis (Enter=YES) ?'
      READ '(A)', ANSWER
      CALL UPCASE (ANSWER)
      IF (ANSWER(1:1) .EQ. 'N' .OR. ANSWER(1:1) .EQ. 'F') THEN
          AUTOSCALE = .FALSE.
  101     PRINT '(3A,$)',' Low, high ',AXCHAR,' axis limits ?'
          READ '(A)', ANSWER
	  ANSWER (30:30) = '/'
          READ  (ANSWER,*,ERR=101) VPMIN, VPMAX
      ELSE
          VPMIN = VMIN
          VPMAX = VMAX
      END IF
      RETURN
      END
**************************
      SUBROUTINE PGCURSPOS (AXIS,MAXPTC,NPTC,XCURS,YCURS)
**************************
*     return cursor position
      IMPLICIT NONE
      CHARACTER ANSWER*1          ! local user answer
      CHARACTER CURSOR*3          ! local cursor status
      REAL XCURS(*)               ! output array of marked points
      REAL YCURS(*)               ! output array of marked points
      INTEGER I                   ! local
      INTEGER AXIS                ! input plot axis type
      INTEGER LENGTH              ! number of characters returned in CURSOR
      INTEGER MAXPTC              ! input max number of points
      INTEGER NPTC                ! output number of points entered
      INTEGER SYMBOL              ! marker symbol for marked point
      DATA SYMBOL /14/            ! hollow cross
*
*     see if display is interactive with a cursor
      CALL PGQINF('cursor', CURSOR, LENGTH)
*
      IF (CURSOR(1:LENGTH) .EQ. 'YES') THEN
          PRINT *,' How to mark points with the cursor: '
          PRINT *,' mouse left button or A = Add new point' 
          PRINT *,'     centre button or D = Delete nearest point'
          PRINT *,'      right button or X = eXit'
          PRINT *,' coordinates returned are sorted on X'
          PRINT '(2A,$)',' Mark points with cursor and ',
     &           'get coordinates (Enter=NO) ?'
          READ '(A)', ANSWER
          CALL UPCASE (ANSWER)
          IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') THEN
              NPTC = 0
              CALL PGNCUR (MAXPTC,NPTC,XCURS,YCURS,SYMBOL)
              PRINT *,'Points were marked at:'
              PRINT *,'Xcurs     Ycurs'
              DO I = 1,NPTC
*                 allow for log axes
                  IF (AXIS .EQ. 10 .OR. AXIS .EQ. 30) THEN
                      XCURS(I) = 10**XCURS(I)
                  END IF
                  IF (AXIS .EQ. 20 .OR. AXIS .EQ. 30) THEN
                      YCURS(I) = 10**YCURS(I)
                  END IF
                  PRINT *,XCURS(I),YCURS(I)
              END DO
          END IF
      END IF
      RETURN
      END
*************************
      SUBROUTINE PGMAXMIN (AUTOCONFIG,AXIS,IBAR,NPTS,VMIN,VMAX,
     &                   AXCHAR,VPMIN,VPMAX)
*************************
*     find maxima and minima for linear or log axes
*
      IMPLICIT NONE
      CHARACTER AXCHAR*1      ! input axis identifier 'X' or 'Y'
      LOGICAL   AUTOCONFIG    ! input auto or user configured
      LOGICAL   AUTOSCALE     ! input true if axis is autoscaled
      INTEGER   AXIS          ! input PGPLOT axis type
      INTEGER   IBAR          ! input/output plot X err bars option
      INTEGER   LOGAX1        ! local 
      INTEGER   NPTS          ! input number of points in X,Y arrays
      REAL      PCRANGE       ! local
      REAL      VMIN          ! input  minimum value
      REAL      VMAX          ! input  maximum value
      REAL      VPMIN         ! output axis minimum
      REAL      VPMAX         ! output axis maximum
C
C     checks for log axes differ for X and Y axes
      IF (AXCHAR .EQ. 'X') THEN
          LOGAX1 = 10
      ELSE IF (AXCHAR .EQ. 'Y') THEN
          LOGAX1 = 20
      END IF
*     set up axis limits from data min and max
   10 IF (AUTOCONFIG) THEN
          VPMIN = VMIN
          VPMAX = VMAX
          AUTOSCALE = .TRUE.
      ELSE
*         ask user if autoscaling, return plot min and max
          CALL PGAXLIM (VMIN,VMAX,AXCHAR,VPMIN,VPMAX,AUTOSCALE)
      END IF
      IF (AXIS .EQ. LOGAX1 .OR. AXIS .EQ. 30) THEN 
          IF (VPMIN .GT. 0.0 .AND. VPMAX .GT. 0.0) THEN
              VPMIN = ALOG10 (VPMIN)
              VPMAX = ALOG10 (VPMAX)
          ELSE
              PRINT 1001
              GO TO 10
          END IF
      END IF
C
C     if autoscaled allow a 1% margin (keep inside small tick marks)
C
      IF (AUTOSCALE) THEN
          PCRANGE = (VPMAX - VPMIN) / 100
          VPMIN = VPMIN - PCRANGE
          VPMAX = VPMAX + PCRANGE
      END IF
C
 1001 FORMAT (' Log axis so Min, Max must be > 0')
      RETURN
      END
***************************
      SUBROUTINE PGUSERCONF
     &        (FORCE_NEW_PAGE,FIRST_ON_PANEL,NXSUB,NYSUB,
     &        ADDIDENT,DATANAME,TOPLBL,XLBL,YLBL,IXBAR,IYBAR,
     &        NPTS,X,XBAR,Y,YBAR,XMIN,XMAX,YMIN,YMAX,
     &        AXIS,XPMIN,XPMAX,YPMIN,YPMAX,
     &        SYMBOL,LINE,WIDTH,COLOUR,FONT,
     &        XCHAR,YCHAR)
***************************
C     user configuration
C
      IMPLICIT  NONE
      CHARACTER ANSWER*1          ! user's short answer
      CHARACTER DATANAME*(*)      ! data caption
      CHARACTER TOPLBL(4)*80      ! top labels 1 - 4 on graph
      CHARACTER XCHAR*1           ! 'X'
      CHARACTER XLBL*(*)          ! X axis label
      CHARACTER YCHAR*1           ! 'Y'
      CHARACTER YLBL*(*)          ! Y axis label
      LOGICAL   ADDIDENT          ! true to add name date time to plot
      LOGICAL   FORCE_NEW_PAGE    ! true to force fresh new page
      LOGICAL   FIRST_ON_PANEL    ! true for first plot on panel 
      INTEGER   AXIS              ! PGPLOT axis type eg box, log, linear etc
      INTEGER   COLOUR            ! PGPLOT colour of data points and line
      INTEGER   FONT              ! PGPLOT font for labels
      INTEGER   IXBAR             ! 1 to plot error bars else 0
      INTEGER   IYBAR             ! 1 to plot error bars else 0
      INTEGER   LINE              ! PGPLOT line type joining data points
      INTEGER   NPTS              ! number of valid data points
      INTEGER   NXSUB             ! number of subdivisions of page in X
      INTEGER   NYSUB             ! number of subdivisions of page in Y
      INTEGER   SYMBOL            ! PGPLOT symbol to plot at each datum
      INTEGER   WIDTH             ! PGPLOT width of line joining data points
      REAL      X(*)              ! x values
      REAL      XBAR(*)           ! x errors
      REAL      Y(*)              ! y values
      REAL      YBAR(*)           ! y errors
      REAL      XMAX              ! X max
      REAL      XMIN              ! X min
      REAL      YMAX              ! Y max
      REAL      YMIN              ! Y min
      REAL      XPMAX             ! X plot max
      REAL      XPMIN             ! X plot min
      REAL      YPMAX             ! Y plot max
      REAL      YPMIN             ! Y plot min
C
      PRINT '(A,$)',' Force a fresh new page (Enter=NO) ?'
      READ '(A)', ANSWER
      CALL UPCASE (ANSWER)
      FORCE_NEW_PAGE = (ANSWER .EQ. 'T' .OR. ANSWER .EQ. 'Y')
C
      IF (.NOT. FORCE_NEW_PAGE) THEN
          PRINT '(A,$)',
     &    ' Plot on previous graph (0) or new graph (1) (Enter=1) ?'
          READ '(A)', ANSWER
          FIRST_ON_PANEL = (ANSWER .NE. '0')
      ELSE
          FIRST_ON_PANEL = .TRUE.
      END IF
C
C     user sets up top labels and axis labels
C
      IF (FIRST_ON_PANEL) THEN
C         clear existing top labels 
          CALL CLRLBL (TOPLBL(1))
          CALL CLRLBL (TOPLBL(2))
          CALL CLRLBL (TOPLBL(3))
          CALL CLRLBL (TOPLBL(4))
      END IF
C
C     set up 4 top labels
      CALL PGADDLBL (DATANAME,TOPLBL)
C
C     option to add username, date, time on plot
      CALL PGASKIDENT(ADDIDENT)
C
C     user selects whether to plot error bars - MJG subroutine
C
      CALL PGASKERRBAR (IXBAR,XCHAR)
      CALL PGASKERRBAR (IYBAR,YCHAR)
C
C     find X and Y minima and maxima
C
      CALL DMAXMIN (NPTS,X,XBAR,IXBAR,XMIN,XMAX)
      CALL DMAXMIN (NPTS,Y,YBAR,IYBAR,YMIN,YMAX)
C
C     for first plot on panel user selects axis options - MJG routine
C
      IF (FIRST_ON_PANEL) THEN
          CALL PGASKAXIS (XMIN,XMAX,YMIN,YMAX,AXIS) 
      END IF
C
C     for first plot on panel set up autoscaling request
C
      IF (FIRST_ON_PANEL) THEN
          XPMIN = 0
          XPMAX = -1
          YPMIN = 0
          YPMAX = -1
      END IF
C
C     for all plots user selects line options - MJG routine
C
      CALL PGASKLINE (SYMBOL, LINE, WIDTH, COLOUR) 
C
C     for each new plot user may select font - MJG routine
C
      IF (FIRST_ON_PANEL) THEN 
          CALL PGASKFONT (FONT)
      END IF
      RETURN
      END
