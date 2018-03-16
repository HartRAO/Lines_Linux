*     include file for routines call PGPLOT via PGDOPLOT
*     derived from program PGPLOTA.
*
      INTEGER   MAXDATA
      INTEGER   MAXPTC	
      PARAMETER (MAXDATA = 1024)  ! PG size of data arrays
      PARAMETER (MAXPTC=20)       ! PG size of cursor-marked position arrays
C
      CHARACTER DEVICE*63         ! PGPLOT device in use
      CHARACTER TOPLBL*80         ! PG top label on graph
      CHARACTER XLBL*80           ! PG+ X axis label
      CHARACTER YLBL*80           ! PG+ Y axis label
      LOGICAL   ADDIDENT          ! PG true to add name date time to plot
      LOGICAL   AUTOCONFIG        ! PG true for auto, false user config
      LOGICAL   FORCE_NEW_PAGE    ! PG true to force start of new page
      LOGICAL   FIRST_ON_PANEL    ! PG true for first plot on panel 
      LOGICAL   NEW_DEVICE        ! PG true when PGPLOT needs initialisation
      INTEGER   AXIS              ! PG axis type eg box, log, linear etc
      INTEGER   COLOUR            ! PG colour of data points and line
      INTEGER   FONT              ! PG font for labels
      INTEGER   IXBAR             ! PG 1 to plot error bars else 0
      INTEGER   IYBAR             ! PG 1 to plot error bars else 0
      INTEGER   LINE              ! PG line type joining data points
      INTEGER   NPTC              ! PG number of cursor-marked points
      INTEGER   NPTS              ! PG number of valid data points
      INTEGER   NXSUB             ! PG number of subdivisions of page in X
      INTEGER   NYSUB             ! PG number of subdivisions of page in Y
      INTEGER   SYMBOL            ! PG symbol to plot at each datum
      INTEGER   WIDTH             ! PG width of line joining data points
      REAL      X(MAXDATA)        ! PG X values
      REAL      XBAR(MAXDATA)     ! PG X errors
      REAL      XCURS(MAXPTC)     ! PG X positions marked with cursor
      REAL      XMAX              ! PG X max
      REAL      XMIN              ! PG X min
      REAL      XPMAX             ! PG X plot max
      REAL      XPMIN             ! PG X plot min
      REAL      Y(MAXDATA)        ! PG y values
      REAL      YBAR(MAXDATA)     ! PG y errors
      REAL      YCURS(MAXPTC)     ! PG Y positions marked with cursor
      REAL      YMAX              ! PG Y max
      REAL      YMIN              ! PG Y min
      REAL      YPMAX             ! PG Y plot max
      REAL      YPMIN             ! PG Y plot min
*
      COMMON /LIPLOT1/ DEVICE, TOPLBL, XLBL, YLBL
      COMMON /LIPLOT2/ ADDIDENT, AUTOCONFIG, FORCE_NEW_PAGE,
     &                 FIRST_ON_PANEL, NEW_DEVICE, AXIS, COLOUR,
     &                 FONT, IXBAR, IYBAR, LINE, NPTC, NPTS, 
     &                 NXSUB, NYSUB, SYMBOL, WIDTH,
     &                 X, XBAR, XCURS, XMAX, XMIN, XPMAX, XPMIN,
     &                 Y, YBAR, YCURS, YMAX, YMIN, YPMAX, YPMIN
      SAVE /LIPLOT1/
      SAVE /LIPLOT2/
