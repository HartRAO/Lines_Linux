***********************
      SUBROUTINE LISPLINE (ERROR)
***********************
*     subroutine to set up orthogonal polynomial fitting
*
*     other subroutines called:
*     aitov, pgslct, plconfau, plcursps, plotdata, qmemused,
*     vtoai
*
*     CMDP command parameters used:
*     1 = command mnemonic SPL  
*     2 = number of splines to use
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
      INTEGER   NSPL            ! number of splines
      INTEGER   ISPLINE         ! no. of points to calculate spline at
      INTEGER   LINEP           ! LINE on entry, restored on exit
      INTEGER   MEM             ! memory in use      
      INTEGER   NCHAR           ! external function
      INTEGER   NXSUBP          ! NXSUB on entry, restored on exit
      INTEGER   NYSUBP          ! NYSUB on entry, restored on exit
      INTEGER   WIDTHP          ! WIDTH on entry, restored on exit
      REAL      CHARSIZEP       ! char size on entry, restored on exit
      REAL      YPMAXP          ! YPMAX on input, restored on output
      REAL      YPMINP          ! YPMIN on input, restored on output
      REAL*8    RMSERR          ! SPLINEATX output rms error of fit
      REAL*8    XEND            ! end value of X for spline fit
      REAL*8    XSTART          ! start value of X for spline fit
      REAL*8    XSTEP           ! X interval of spline fit in BELLS
*
      INCLUDE   'lines.inc'
*
      IF (DB) PRINT *,'in LISPLINE'
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
*     number of splines
*
      IF (DB) PRINT *,'in LISPLINE'
      NSPL = 0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=20) NSPL
   20     CONTINUE
      END IF
      IF (DB) PRINT *,'NSPL = ',NSPL
* 
      IF (NSPL .GT. 0) THEN
*         automatic fit using passed parameter
          AUTOFIT = .TRUE.
          IF (DB) PRINT *,'automatic spline fit'
      ELSE
*         user prompted fit
          AUTOFIT = .FALSE.
          IF (DB) PRINT *,'interactive spline fit'
      END IF
      IF (DB) PRINT *,'NSPL = ',NSPL
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
              PRINT '(A,$)',' spline fit for which memory ?'
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
*             plot the data with the splines fit superimposed
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
              WRITE (TOPLBL,'(A,I2,A)') 'spline fit to data'
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
*     select the number of splines
*
  300 IF (NSPL .EQ. 0) THEN
          PRINT '(/A,$)',' number of splines to fit (or QUIT)?' 
          READ '(A)', BUF
          CALL UPCASE (BUF)
          IF (BUF(1:4) .EQ. 'QUIT') GO TO 900
          READ (BUF,*,IOSTAT=IOS,ERR=300) NSPL
      END IF
*
*     spline fit parameters
*
      NPTS = LAST_CH(MEM) - FIRST_CH(MEM) + 1
      ISPLINE = NPTS
      XSTART = XIN(MEM,FIRST_CH(MEM))
      XEND   = XIN(MEM,LAST_CH(MEM))
      DO I = 1, LAST_CH(MEM)
          XSPL(I) = XIN(MEM,I)
          YSPL(I) = YIN(MEM,I)
      END DO
*
*     check that number of splines is within range:
*
      IF (NSPL .GT. (NPTS-1)/3) THEN
          NSPL = (NPTS - 1)/3
          PRINT *,'Too many splines for ',NPTS,' points !'
      END IF
      IF (NSPL .GT. 99) THEN
          NSPL = 99
          PRINT *,'Too many splines for BELLS routine !'
      END IF
      PRINT *,'Fitting ',NSPL,' splines'
*
*     fit the splines (YSPLINE in common block)
*
      IF (DB) PRINT *,'NPTS=',NPTS,' ISPLINE=',ISPLINE,' NSPL=',NSPL,
     &                ' XSTART=',XSTART,' XEND=',XEND
      
      CALL BELLS (DB,XSPL,YSPL,NPTS,XSTART,XEND,ISPLINE,NSPL,
     &        YSPLINE,ERROR)
*
      IF (ERROR .NE. 1) THEN
          PRINT *,'spline error = ',ERROR
          RETURN
      END IF
*     reset error to standard 0 if no problem in BELLS
      ERROR = 0
*
*     calculate splines at input X values, using parabolic fit to spline 
*     (YSPLOUT, YMSPLINE in common block)
*
*     set up array of evenly spaced X values 
*     matching those at which BELLS calculated the splines
      XSTEP = (XEND - XSTART) / (NPTS - 1)
      DO I = 1,ISPLINE
         XSPLINE(I) = XSTART + (I - 1) * XSTEP 
      END DO
*
      CALL SPLINEATX (XSPL,YSPL,NPTS,XSTART,XEND,
     &        XSPLINE,YSPLINE,ISPLINE,
     &        YSPLOUT,YMSPLINE,RMSERR)
*
      PRINT *,'rms error from spline fit =',RMSERR
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
              NPTS = 0
              DO I = FIRST_CH(MEM), LAST_CH(MEM)
                  NPTS = NPTS + 1
                  YP(NPTS) = YSPLOUT(I)
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
*         plot the data minus the splines on a new panel
*
          IF (IDPLOT1 .GT. 0) THEN
              WRITE (TOPLBL,'(A,I3,A)') 'data -',NSPL,' splines'
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
              NPTS = 0
              DO I = FIRST_CH(MEM), LAST_CH(MEM)
                  NPTS = NPTS + 1
                  YP(NPTS) = YMSPLINE(I)
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
*         check acceptability of spline fit
*
          PRINT '(A,$)',' Is this fit okay (Enter = yes) ?'
          READ '(A)', ANSWER
          CALL UPCASE (ANSWER)
          IF (ANSWER .EQ. 'N') THEN
              PRINT '(A,$)',' Try another fit (Enter = yes) ?'
              READ '(A)', ANSWER2
              CALL UPCASE (ANSWER2)
              IF (ANSWER2 .EQ. 'N') THEN
                  PRINT *,'No splines fitted'
                  GO TO 900
              ELSE
*                 replot data and enter new spline order
                  NSPL = 0
                  GO TO 200
              END IF
          END IF
      END IF 
*
*     store the data - splines back in Yin
*
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          YIN(MEM,I) = YMSPLINE(I)
      END DO
*
      WRITE (BUF,*) 'data - splines stored in mem ',MEM
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
