***********************
      SUBROUTINE SMOOTH (ERROR)
***********************
*     subroutine to set up running mean smoothing
*
*     other subroutines called:
*     smooft.f  from numerical recipes
*
*     CMDP command parameters used:
*     1 = command mnemonic SM
*     2 = smoothing width 
*     3 = memory to use
*
      IMPLICIT  NONE
      CHARACTER ANSWER*1        ! user answer
      CHARACTER ANSWER2*1       ! user answer
      LOGICAL   AUTOFIT         ! true if fit is automatic
      INTEGER   COLOURP         ! COLOUR on entry, restored on exit
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! loop index
      INTEGER   IOS             ! i/o status
      INTEGER   J               ! loop index
      INTEGER   LINEP           ! LINE on entry, restored on exit
      INTEGER   MEM             ! memory in use      
      INTEGER   NCHAR           ! external function
      INTEGER   NXSUBP          ! NXSUB on entry, restored on exit
      INTEGER   NYSUBP          ! NYSUB on entry, restored on exit
      INTEGER   SMWIDTH         ! smoothing width
      INTEGER   WIDTHP          ! WIDTH on entry, restored on exit
      REAL      CHARSIZEP       ! char size on entry, restored on exit
      REAL      YPMAXP          ! YPMAX on input, restored on output
      REAL      YPMINP          ! YPMIN on input, restored on output
*
      INCLUDE   'lines.inc'
*
      IF (DB) PRINT *,'in SMOOTH'
      ERROR = 0
*     save plot entry values to be restored on exit
      CHARSIZEP = CHARSIZE
      COLOURP = COLOUR
      LINEP   = LINE
      NXSUBP = NXSUB
      NYSUBP = NYSUB
      WIDTHP = WIDTH
      YPMAXP = YPMAX
      YPMINP = YPMIN
*
*     smoothing width
*
      SMWIDTH = 0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=20) SMWIDTH
   20     CONTINUE
      END IF
      IF (DB) PRINT *,'SMWIDTH = ',SMWIDTH
* 
      IF (SMWIDTH .GT. 0) THEN
*         automatic fit using passed parameter
          AUTOFIT = .TRUE.
          IF (DB) PRINT *,'automatic smoothing'
      ELSE
*         user prompted fit
          AUTOFIT = .FALSE.
          IF (DB) PRINT *,'interactive smoothing'
      END IF
      IF (DB) PRINT *,'SMWIDTH = ',SMWIDTH
*
*     find how many memories are in use
*
      MEM = 0
      CALL QMEMUSED
      IF (MEMUSED .EQ. 0) THEN
          PRINT *,'no data'
          ERROR = 1
          GO TO 900
      END IF
*
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=30) MEM
   30     CONTINUE
      END IF
*      
      IF (MEM .EQ. 0) THEN
          IF (NAXIS(MEMSET) .EQ. 0) THEN 
              PRINT '(A,$)',' smooth data in which memory ?'
              READ '(A)',CMDP(3)
              READ (CMDP(3),*,ERR=40,IOSTAT=IOS) MEM
   40         IF (IOS .NE. 0) THEN
                  PRINT 2005, CMDP(3)
                  ERROR = IOS
                  GO TO 900
              END IF
          ELSE
              PRINT *,'mem=memset'
              MEM = MEMSET
          END IF
      END IF
*
      IF (MEM .LT. 1 .OR. MEM .GT. MAXMEM) THEN
          PRINT 2005,CMDP(3)
          ERROR = 1
          GO TO 900
      END IF
*
      IF (NAXIS(MEM) .EQ. 0) THEN
          PRINT *,'mem ',MEM,' is empty'
          ERROR = 1
          GO TO 900
      END IF
      IF (DB) PRINT *,'MEM = ',MEM
*
*
 200  IF (.NOT. AUTOFIT) THEN
*
          IF (IDPLOT1 .GT. 0) THEN
*             plot the data with the smoothed data superimposed
* 
*             first plot the data
              WRITE (CMDP(2),*) MEM
              LINE = 1
              NCMD = 2
              COLOUR = 1
              SYMBOL = 1
*             force axis autoscaling on plot
              XPMIN = 0
              XPMAX = -1
C             use input YPMAX and YPMIN instead of autoscaling
              YPMIN = YPMINP
              YPMAX = YPMAXP
*             store data and captions in plot arrays
              CALL PLCONFAU (ERROR)
              NEW_SUBP = .TRUE.
              NEW_PAGE = .TRUE.
              CHARSIZE = 2.0
              NXSUB = 1
              NYSUB = 2
              WIDTH = 1

              WRITE (TOPLBL,'(A)') OBJECT(MEM)(1:NCHAR(OBJECT(MEM)))
*
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT *,'plotting to ',PLOTDEV
              CALL PGSLCT (IDPLOT1)
*             plot the spectrum
              CALL PLOTDATA (ERROR)
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
      END IF
*
*     select the median filter
*
  300 IF (SMWIDTH .EQ. 0) THEN
          PRINT '(/A,$)',' Smoothing width (or QUIT)?' 
          READ '(A)', BUF
          CALL UPCASE (BUF)
          IF (BUF(1:4) .EQ. 'QUIT') GO TO 900
          READ (BUF,*,IOSTAT=IOS,ERR=300) SMWIDTH
      END IF
*
*     set up data arrays for smoothing
*
      NPTS = LAST_CH(MEM) - FIRST_CH(MEM) + 1
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          XSPL(I) = XIN(MEM,I)
          YSPL(I) = YIN(MEM,I)
      END DO
*
*     smooth using arrays for splines (YSPLINE in common block)
*     YSPL = input Y values
*     YSPLINE = SMOOTH output
*     YMSPLINE = ROUGH = data - SMOOTH output
*
      CALL BOXCAR (YSPL,YSPLINE,NPTS,SMWIDTH)
*
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          YMSPLINE(I) = YIN(MEM,I) - YSPLINE(I)
      END DO
*
      IF (.NOT. AUTOFIT) THEN
*
*         plot the splines fitted to the data on same graph
*
          IF (IDPLOT1 .GT. 0) THEN
              COLOUR = 2
              LINE = 2
              NEW_PAGE = .FALSE.
              NEW_SUBP = .FALSE.
*             force X axis autoscaling on plot
              XPMIN = 0
              XPMAX = -1
C             use input YPMAX and YPMIN instead of autoscaling
              YPMIN = YPMINP
              YPMAX = YPMAXP
*
              DO I = 1, NPTS
                  XP(I) = XSPL(I)
                  YP(I) = YSPLINE(I)
              END DO
*
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT *,'plotting to ',PLOTDEV
              CALL PGSLCT (IDPLOT1)
*             plot the spectrum
              CALL PLOTDATA (ERROR)
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
*
*         plot the data minus the smoothed data on a new panel
*
          IF (IDPLOT1 .GT. 0) THEN
              WRITE (TOPLBL,'(A,I4)') 'Input - data smoothed by ',
     &        SMWIDTH
              COLOUR = 3
              LINE = 1
              NEW_PAGE = .TRUE.
              NEW_SUBP = .FALSE.
*             force X axis autoscaling on plot
              XPMIN = 0
              XPMAX = -1
C             use input YPMAX and YPMIN instead of autoscaling
              YPMIN = YPMINP
              YPMAX = YPMAXP
*
              DO I = 1, NPTS
                  YP(I) = YMSPLINE(I)
              END DO
*
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT *,'plotting to ',PLOTDEV
              CALL PGSLCT (IDPLOT1)
*             plot the spectrum
              CALL PLOTDATA (ERROR)
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
*
*         check acceptability of smoothing
*
          PRINT '(A,$)',' Is this smoothing okay (Enter = yes) ?'
          READ '(A)', ANSWER
          CALL UPCASE (ANSWER)
          IF (ANSWER .EQ. 'N') THEN
              PRINT '(A,$)',' Try another smoothing (Enter = yes) ?'
              READ '(A)', ANSWER2
              CALL UPCASE (ANSWER2)
              IF (ANSWER2 .EQ. 'N') THEN
                  PRINT *,'Not smoothed'
                  GO TO 900
              ELSE
*                 replot data and enter new smoothing width
                  SMWIDTH = 0
                  GO TO 200
              END IF
          END IF
      END IF 
*
*     store the SMOOTH output back in Yin
*
      J = 1
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          YIN(MEM,I) = YSPLINE(J)
          J = J + 1
      END DO
*
      WRITE (BUF,*) 'smoothed data stored in mem ',MEM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*
  900 CONTINUE
*
*     reset plot parameters to values on entry
*
      CHARSIZE = CHARSIZEP
      COLOUR   = COLOURP
      LINE     = LINEP
      NEW_SUBP = .TRUE.
      NXSUB    = NXSUBP
      NYSUB    = NYSUBP
      WIDTH    = WIDTHP
      YPMAX    = YPMAXP
      YPMIN    = YPMINP
*
      RETURN
*
 1000 FORMAT (' plot to ',A)
 1002 FORMAT (' no postscript output file - set plotps')
 2005 FORMAT (' illegal: ',A)
      END        
*********
