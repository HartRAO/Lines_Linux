************************
      SUBROUTINE SPLNFIT (XIN,YIN,NPTS,XSTART,XEND,NSPL,WRTSPLN,NFIT,
     &                    SPLNOUT,XSPLINE,YSPLINE,YSPLOUT,DY,ISPLINE)
************************
*     fit Bell splines in a least squares sense to X,Y data
*
*     XIN      array of the independent values
*     YIN      array of the dependent values
*     NPTS     number of input data points
*     NSPL     number of splines to fit
*     NFIT     number of spline fits made (for use with many-channel data)
*     ISPLINE  number of points in spline array
*     XSPLINE  array of evenly spaced spline X values
*     YSPLINE  array of evenly spaced spline Y values
*     YSPLOUT  array of Y values at input X values, derived by parabolic
*              interpolation from fitted spline
*     DY       array of residuals
*     IER      spline fit error return
*     SPL0     warning from spl.f  MW 14/11/00
*
*
*     This routine calls: BELLS
*                         SPLINEATX
*                         WRITESPL
*
*     nmax = max no. of data
      PARAMETER (NMAX = 2500)
*
      IMPLICIT  REAL*8 (A-H,O-Z)
*
      INTEGER   NPTS, NSPL, ISPLINE, NFIT, I, IER
*
      REAL*8    XIN(NMAX), YIN(NMAX),
     &          XSPLINE(NMAX), YSPLINE(NMAX),
     &          YSPLOUT(NMAX), DY(NMAX)
*
*               input & output files:
      CHARACTER INFILE*60, OUTFILE1*80, OUTFILE2*80,
*               output path for data files:
     &          SPLNOUT*80
*               answer variable:
*    &          ANSWER*1  ! Commented out...
*
*               plotting variables - commented out at present:
*    &          LABX*30, LABY*30, 
*    &          NAME(4)*40,
*    &          SYMBOL*1
*
*               option to write out splines:
      LOGICAL   WRTSPLN,
*               warning for possible rubbish in output files:
     &          SPL0
*
*               common to SPL, BELLS, SPLNFIT & EKDCFRSCT
      COMMON    /WARNING/ SPL0
*
*
*     plot the input data
*
*     PRINT *,'Plot the input data (ENTER = no) ?'
*     READ  '(A)', ANSWER
*      CALL UPCASE (ANSWER)
*
*     set up header strings
*
*  10 INAME = 2       ! put fit results in header # 2
*     CALL CLRSTRNG (NAME(2)) ! set names to blank characters
*     CALL CLRSTRNG (NAME(3))
*     CALL CLRSTRNG (NAME(4))
*     IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') THEN
*         array indicies of first and last data
*         N1 = 1
*         N2 = NPTS
*         IAXES = 0       ! plot on new axes
*         SYMBOL = ' '    ! symbol
*         LINEDATA = 0        ! no line
*         CALL DOPLOT (N1,N2,XIN,YIN,
*    &                 NAME(1),NAME(2),NAME(3),NAME(4),
*    &                 LABX,LABY,IAXES,SYMBOL,LINEDATA)
*     END IF
*     IF (ANSWER .EQ. 'Q') GO TO 900
*
*     find date range for spline fit
*
*     date range already given via XSTART and XEND in parameter file:
*  20 XSTART = XIN(1) 
*     XEND   = XIN(NPTS)
*     quick print check:
*     PRINT *,'X-start: ',XSTART,' X-end: ',XEND
*     PRINT *,' '
*     PRINT *,'Start fit at X (/ = ',XSTART,') ?'
*     READ  *, XSTART
*     PRINT *,'End fit at X (/ = ',XEND,') ?'      
*     READ  *, XEND
      DATAINTVL = (XIN(NPTS) - XIN(1)) / (NPTS - 1)
*     PRINT *,'Mean X interval of input data = ',DATAINTVL
      SMIN = (XEND - XSTART) / NMAX
      SMIN = SMIN*1.001
*     PRINT *,'Minimum X interval for spline calculation = ',SMIN
      XSTEP = SMIN
*      
*     next few steps not needed in automated version....
*     PRINT *,'Wanted X interval (/ = ',XSTEP,') ?'
*     READ *, XSTEP
*     XSTEP = MAX (XSTEP, SMIN)
*
*     automatied version: 
*     make XSTEP next integer value up from (decimal) real value given....
      XSTEP = INT(XSTEP) + 1  
      ISPLINE = (XEND - XSTART) / XSTEP + 1
*     print check:
*     PRINT *,'X-step: ',XSTEP,' I-spline: ',ISPLINE
*     XEND already given, re-calculation required for BELLS:
*     test: commenting out XENDBELLS to see whether XEND will do...
      XENDBELLS = XSTART + (ISPLINE - 1) * XSTEP
*     print check:
*     PRINT *,'XENDBELLS, XSTEP: ',XENDBELLS, XSTEP     
*
*     set up array of X values corresponding to the spline Y values 
*
      DO I = 1,ISPLINE
         XSPLINE(I) = XSTART + (I - 1) * XSTEP 
      END DO
*     print check:
*     PRINT *,'X-array set up...'
*
*     choice of number of splines - not needed for automated version:
*  30 PRINT *,'Number of splines to be fitted ?'
*     READ  *,NSPL
*
*     number of splines to be fitted must be in acceptable range:
      IF ((NSPL .GT. (NPTS-1)/3) .OR. (NSPL .GT. 99)) THEN
*         reduce number of splines to fit within range:
          IF (NSPL .GT. (NPTS-1)/3) THEN
              NSPL = (NPTS - 1)/3
              PRINT *,'Too many splines for ',NPTS,' points !'
              PRINT *,'Number of splines reduced to: ',NSPL
              PRINT *,' '
*             GO TO 30
          END IF
          IF (NSPL .GT. 99) THEN
              NSPL = 99
              PRINT *,'Too many splines for BELLS routine !'
              PRINT *,'Number of splines reduced to: ',NSPL
              PRINT *,'(to fit more splines modify BELLS)'
              PRINT *,' '
*             GO TO 30
          END IF
      END IF
*
*     fit the splines
*     print check:
*     PRINT *,'Calling BELLS...'
*
      CALL BELLS (XIN,YIN,NPTS,XSTART,XENDBELLS,ISPLINE,NSPL,
     &            YSPLINE,IER)
*     print check:
*     PRINT *,'IER from BELLS: ',IER
*
*     calculate spline at input X values, using parabolic fit to spline 
*
      CALL SPLINEATX (XIN,YIN,NPTS,XSTART,XEND,
     &                XSPLINE,YSPLINE,ISPLINE,
     &                YSPLOUT,DY,RMSERR)
*
*     PRINT *,'RMS error of spline fit = ',RMSERR
*     PRINT *,'RMS error = sqrt ((sum(Ycalc-Yin)**2)/Npoints)'
*     WRITE (NAME(INAME),1002,ERR=65,IOSTAT=IOS)
*    &       NSPL, XSTART, XEND, RMSERR
*1002 FORMAT ('N',I2,' S',G10.4,' E',G10.4,' ER',G10.4)
*  65 IF (IOS .NE. 0) PRINT *,'ERR=',IOS,'writing plot header'
c      print *,'Name2=',name(2)
c      print *,'Name3=',name(3)
c      print *,'Name4=',name(4)
*
*     PRINT *,' Plot the fitted spline on screen (ENTER = no) ?'
*     READ '(A)', ANSWER
*     CALL UPCASE (ANSWER)
*     IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') THEN
*         PRINT *, 'Spline fit plot'
*         IAXES = 1       ! plot on previous axes
*         SYMBOL = ' '    ! no symbol
*         IF (LINE .LT. 2 .OR. LINE .GT. 6) LINE = 2 ! reset line type 
*         CALL DOPLOT (1,ISPLINE,XSPLINE,YSPLINE,
*    &                 NAME(1),NAME(2),NAME(3),NAME(4),
*    &                 LABX,LABY,IAXES,SYMBOL,LINE)
*         LINE = LINE + 2 ! increment line type for next spline plot
*     ELSE IF (ANSWER .EQ. 'Q') THEN
*         GO TO 900
*     END IF
* 
*     write spline to disc file
*
*     PRINT *,'Write the spline to a disc file (ENTER = no) ?'
*     READ  '(A)', ANSWER
*     CALL UPCASE (ANSWER)
*     IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') THEN
*    
*     for automated version, check option for writing out files:
      IF (WRTSPLN) THEN
          CALL WRITESPL (INFILE,OUTFILE1,OUTFILE2,SPLNOUT,NFIT,ISPLINE,
     &                         XSPLINE,YSPLINE,XIN,YIN,YSPLOUT,DY,NPTS)
*     ELSE IF (ANSWER .EQ. 'Q') THEN
*         GO TO 900
      END IF
*
*     options for repeating
*
*     increment count for writing fit data to header
*     INAME = INAME + 1
*     IF (INAME .EQ. 5) THEN
*         INAME = 2
*         CALL CLRSTRNG (NAME(3))
*         CALL CLRSTRNG (NAME(4))
*     END IF
*
*     PRINT *,'Try a different number of splines (ENTER = no) ?'
*     READ '(A)', ANSWER
*     CALL UPCASE (ANSWER)
*     IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') GO TO 30
*     IF (ANSWER .EQ. 'Q') GO TO 900
*
*     PRINT *,'Set new X range for the fit (ENTER = no) ?'
*     READ '(A)', ANSWER
*     CALL UPCASE (ANSWER)
*     IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') GO TO 20
*     IF (ANSWER .EQ. 'Q') GO TO 900
*
*     PRINT *,'Plot the input data again (ENTER = no) ?'
*     READ '(A)', ANSWER
*     CALL UPCASE (ANSWER)
*     IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') GO TO 10
*     IF (ANSWER .EQ. 'Q') GO TO 900
*
*     PRINT *,'Start again with new data (ENTER = no) ?'
*     READ  '(A)', ANSWER
*     CALL UPCASE (ANSWER)
*     IF (ANSWER .EQ. 'Y' .OR. ANSWER .EQ. 'T') GO TO 5
*
*     else end
*
      END
