***********************
      SUBROUTINE LIPOLY (ERROR)
***********************
*     subroutine to set up orthogonal polynomial fitting
*
*     other subroutines called:
*     aitov, lmsp, pgslct, plconfau, plcursps, plotdata, poblock, qmemused,
*     vtoai
*
*     CMDP command parameters used:
*     1 = command mnemonic PO
*     2 = 'PB' if polynomial baseline blocks set in PB are to be used
*     3 = polynomial order to use
*     4 = memory to use
*
      IMPLICIT  NONE
      CHARACTER ANSWER*1        ! user answer
      CHARACTER ANSWER2*1       ! user answer
      CHARACTER ANSWER3*1       ! user answer
      LOGICAL   AUTOFIT         ! true if fit is automatic
      LOGICAL   USEPB           ! true to use PB baseline blocks
      INTEGER   COLOURP         ! COLOUR on entry, restored on exit
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! loop index
      INTEGER   IEND            ! loop end
      INTEGER   IOS             ! i/o status
      INTEGER   IPOLY           ! polynomial order
      INTEGER   ISTART          ! loop start
      INTEGER   K               ! loop index
      INTEGER   LINEP           ! LINE on entry, restored on exit
      INTEGER   MEM             ! memory in use      
      INTEGER   NCHAR           ! external function
      INTEGER   NXSUBP          ! NXSUB on entry, restored on exit
      INTEGER   NYSUBP          ! NYSUB on entry, restored on exit
      INTEGER   WIDTHP          ! WIDTH on entry, restored on exit
      REAL      CHARSIZEP       ! char size on entry, restored on exit
      REAL      YPMAXP          ! YPMAX on input, restored on output
      REAL      YPMINP          ! YPMIN on input, restored on output
      REAL*8    RMSER           ! LMSP output rms error of fit
      REAL*8    SW              ! LMSP output sum of weights = no of points used
      REAL*8    XPOLYMAX        ! parabola X max
      REAL*8    YPOLYMAX        ! parabola Y max
*
      INCLUDE   'lines.inc'
*
      IF (DB) PRINT *,'in LIPOLY'
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
*
*     interpret input parameters
*
*     use baseline blocks ?
*
      USEPB = .FALSE.
      IF (NCMD .GE. 2) THEN
          CALL UPCASE (CMDP(2))
          IF (CMDP(2) .EQ. 'PB') USEPB = .TRUE.
      END IF
*
*
*     polynomial order
*
      IPOLY = 0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=20) IPOLY
   20     CONTINUE
      END IF
      IF (DB) PRINT *,'IPOLY = ',IPOLY
*
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
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=30) MEM
   30     CONTINUE
      END IF
*      
      IF (MEM .EQ. 0) THEN
          IF (NAXIS(MEMSET) .EQ. 0) THEN 
              PRINT '(A,$)',' polynomial fit for which memory ?'
              READ '(A)',CMDP(4)
              READ (CMDP(4),*,ERR=40,IOSTAT=IOS) MEM
   40         IF (IOS .NE. 0) THEN
                  PRINT 2005, CMDP(4)
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
          PRINT 2005,CMDP(4)
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
      IF (IPOLY .GT. 0 .AND. NBASE .GT. 0) THEN
*         automatic fit
          AUTOFIT = .TRUE.
          IF (DB) PRINT *,'automatic polyfit'
      ELSE
*         user prompted fit
          AUTOFIT = .FALSE.
          IF (DB) PRINT *,'interactive polyfit'
      END IF
      IF (DB) PRINT *,'IPOLY = ',IPOLY,' NBASE = ',NBASE
*
*
*     ready to start fit procedure
*
  200 IF (.NOT. AUTOFIT) THEN
*
          IF (IDPLOT1 .GT. 0) THEN
*             plot the spectrum in MEM
*
              IF (DB) PRINT *,'plot the spectrum in mem ',MEM
              WRITE (CMDP(2),*) MEM
              NCMD = 2
*             store data and captions in plot arrays
              CALL PLCONFAU (ERROR)
*             force axis autoscaling on plot
              XPMIN = 0
              XPMAX = -1
C             use input YPMAX and YPMIN instead of autoscaling
              YPMIN = YPMINP
              YPMAX = YPMAXP
*             do two plots per page, one above the other
              NEW_PAGE = .TRUE.
              NEW_SUBP = .TRUE.
              CHARSIZE = 2.0
              COLOUR = 1
              LINE = 1
              NXSUB = 1
              NYSUB = 2
              SYMBOL = 1
              WIDTH = 1
*
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT *,'plotting to ',PLOTDEV
              CALL PGSLCT (IDPLOT1)
*             plot the spectrum
              CALL PLOTDATA (ERROR)
          ELSE
              PRINT *,'PLOTDEV = ',PLOTDEV
          END IF
*
*
*         show existing baseline blocks
*
          IF (DB) PRINT *,'USEPB = ',USEPB,' NBASE = ',NBASE
          IF (NBASE .GT. 0) THEN
              WRITE (CMDP(2),'(A)') 'SHOW'
              NCMD = 2
              CALL POBLOCK (ERROR)
              IF (.NOT. USEPB) THEN
                  PRINT '(A,$)',
     &                ' Use these baseline blocks (Enter = no) ?'
                  READ '(A)',ANSWER
                  CALL UPCASE (ANSWER)
                  IF (ANSWER .EQ. 'Y') USEPB = .TRUE.
              END IF
          END IF
*
*
*         enter new baseline blocks
*
          IF (DB) PRINT *,'USEPB = ',USEPB,' NBASE = ',NBASE
          IF (.NOT. USEPB .OR. NBASE .EQ. 0) THEN
              NPTC = 0
              IF (IDPLOT1 .GT. 0 .AND. PLOTCURS .EQ. 'YES') THEN
                  PRINT *,'Use the mouse to mark the start and end of ',
     &                    'each baseline block'
*                 get baseline blocks using cursor on graph
                  CALL PLCURSPS (ERROR)
              END IF
*
              IF (NPTC .EQ. 0) THEN
*                 no points were entered using the mouse
                  NCMD = 1
                  CALL POBLOCK (ERROR)
              END IF
*
*             check that there is room for the new pair of points
*
              IF ((NPTC/2) .GT. MAXBASE) THEN
                  WRITE (BUF,*) 'MAXBASE =',MAXBASE,
     &                ' exceeded, increase in "lines.inc"'
                  PRINT *,BUF(1:NCHAR(BUF))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
                  ERROR = 1
                  GO TO 900
              END IF
*
*             else store cursor positions in common real*8 array
              IF (DB) PRINT *,'in LIPOLY: NPTC = ',NPTC
              IF (DB) PRINT *,'store cursor positions in BASESTART,END'
              DO I = 1, NPTC, 2
                  NBASE = I/2 + 1
                  BASESTART(NBASE) = XCURS(I)
                  BASEEND(NBASE)   = XCURS(I+1)
                  IF (DB) PRINT *,'NBASE=',NBASE,
     &            ' XCURS(I)=',XCURS(I),
     &            ' BASESTART(NBASE)=', BASESTART(NBASE),
     &            ' XCURS(I+1)=',XCURS(I+1),
     &            ' BASEEND(NBASE)=',BASEEND(NBASE)
              END DO
*             list the baseline blocks
              WRITE (CMDP(2),'(A)') 'SHOW'
              NCMD = 2
              CALL POBLOCK (ERROR)
          END IF
      END IF
*
*     okay to continue ?
      IF (NBASE .EQ. 0) THEN
          WRITE (BUF,*) 'no baseline blocks set!'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          GO TO 900
      END IF
*
*
*     set up the baseline blocks
*
*     set abscissae to XIN and all weights to zero
*
      DO I = 1, NAXIS1(MEM)
          POX(I) = XIN(MEM,I)
          POY(I) = YIN(MEM,I)
          POW(I) = 0D0
      END DO
*
      DO I = 1, NBASE
*      
*         get array indicies corresponding to users velocities
*
*         this only works for regularly spaced data!!
          CALL XTOAI (MEM,BASESTART(I),ISTART)
          IF (DB) PRINT *,'after XTOAI, ISTART=',ISTART
          CALL XTOAI (MEM,BASEEND(I),IEND)
          IF (DB) PRINT *,'after XTOAI, IEND=',IEND
*
*         ensure values are within limits of data
c          PRINT *,'ISTART=',ISTART
          ISTART = MAX (FIRST_CH(MEM), ISTART)
c          PRINT *,'after MAX(), ISTART=',ISTART
          ISTART = MIN (ISTART, LAST_CH(MEM))
          IF (DB) PRINT *,'after MIN(), ISTART=',ISTART
          CALL AITOV (MEM, ISTART, BASESTART(I))
          IF (DB) PRINT *,'after AITOV, BASESTART(ISTART)=',BASESTART(I)
          IEND = MIN (IEND, LAST_CH(MEM))
C          PRINT *,'after MIN(), IEND=',IEND
          IEND = MAX (ISTART, IEND)
          IF (DB) PRINT *,'after MAX(), IEND=',IEND
          CALL AITOV (MEM, IEND, BASEEND(I))
          IF (DB) PRINT *,'after AITOV, BASEEND(I)=',BASEEND(I)
*
*         Set weight to 1 on wanted data points
          DO K = ISTART, IEND
              IF (POW(K) .EQ. 1D0) THEN
                  PRINT *,'error - background blocks overlap'
                  IF (AUTOFIT) THEN
                      ERROR = 1
                      GO TO 900
                  ELSE
                      GO TO 200
                  END IF
              ELSE
                  POW(K) = 1D0
              END IF
          END DO
      END DO
*
      IF (IPOLY .EQ. 0) THEN
*
*
*         test fit all polynomials from order 1 to 10
*
          PRINT *,'Test fitting polynomials...'
          IF (DB) THEN
              DO I = 1, NAXIS1(MEM)
                  PRINT *,'PoX, PoY, PoW = ',POX(I),POY(I),POW(I)
              END DO
          END IF
*          
          DO I = 1, 10
              CALL LMSP (DB,POX,POY,POW,I,NAXIS1(MEM),
     &                   FIRST_CH(MEM),LAST_CH(MEM), POPJ,
     &                   POA,RMSER,SW,POPJM1,POERROR)
*
*
*             store results in plot arrays for plotting and printing
*
              XP(I) = I
              YP(I) = RMSER
              IF (I .EQ. 1) THEN
                  PRINT 2010, I, YP(I), BUNIT(MEM)
              ELSE IF (I .GT. 1) THEN
                  YBAR(I) = YP(I-1) - YP(I)
                  PRINT 2011, I, YP(I), BUNIT(MEM),
     &                        YBAR(I), BUNIT(MEM)
              END IF
          END DO
*
*
          IF (IDPLOT1 .GT. 0) THEN
*             plot the rms error vs polynomial order
*
              IF (DB) PRINT *,'Plotting fit error vs order'
              WRITE (TOPLBL,'(A)') 
     &            'RMS error of fit vs polynomial order'
              WRITE (XLBL,'(A)') 'POLYNOMIAL ORDER'
              WRITE (YLBL,'(A)') 'RMS ERROR OF FIT'
              NEW_PAGE = .TRUE.
              NEW_SUBP = .FALSE.
              AXIS = 0
              COLOUR = 4
              IXBAR = 0
              IYBAR = 0
              LINE = 1
              NPTS = 10
              SYMBOL = 13
              WIDTH = 1
*             force axis autoscaling on plot
              XPMIN = 0
              XPMAX = -1
              YPMIN = 0
              YPMAX = -1
              IF (DB) THEN
                  DO I = 1, NPTS
                      PRINT *,'I=',I,' XP(I)=',XP(I),' YP(I)=',YP(I)
                  END DO
              END IF
*           
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT *,'plotting to ',PLOTDEV
              CALL PGSLCT (IDPLOT1)
              CALL PLOTDATA (ERROR)
          END IF
      END IF
*
*
*     select the polynomial order
*
  300 IF (IPOLY .EQ. 0) THEN
          PRINT '(/A,$)',' Order of polynomial to fit (or QUIT)?' 
          READ '(A)', BUF
          CALL UPCASE (BUF)
          IF (BUF(1:4) .EQ. 'QUIT') GO TO 900
          READ (BUF,*,IOSTAT=IOS,ERR=300) IPOLY
          IPOLY = MAX (IPOLY,0)
          IPOLY = MIN (IPOLY,MAXPOLY)
          PRINT *,'Fitting polynomial order ',IPOLY
      END IF
*
* 
*     Call the polynomial fitting routine 
*  
      CALL LMSP (DB,POX,POY,POW,IPOLY,NAXIS1(MEM),
     &           FIRST_CH(MEM),LAST_CH(MEM), POPJ,
     &           POA,RMSER,SW,POPJM1,POERROR)
* 
*     write out the polynomial coefficients
*
      WRITE (BUF,*) 'polynomial coefficients :'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (LBUF,2012) (I, I = 0, IPOLY)
 2012 FORMAT (11(3X,I2,9X))
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
*      
      WRITE (LBUF,2014) (POA(I), I = 1, IPOLY+1)
 2014 FORMAT (11(1PG13.6,1X))
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
*    
*     print the rms error of the fit
      WRITE (BUF,2010) IPOLY, RMSER, BUNIT(MEM)
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      IF (IPOLY .EQ. 2) THEN
*         for parabola, find max / min for fitting drift scan peaks
*         print *,'POA1,2,3=',POA(1), POA(2), POA(3)
          PRINT *,' Sum of weights = Npts =',SW
          XPOLYMAX = -POA(2) / (2.0 * POA(3))
          YPOLYMAX = POA(1) + XPOLYMAX*(POA(2) + POA(3)*XPOLYMAX)
          WRITE (BUF,*) 'Parabola Xmax = ',XPOLYMAX,' Ymax = ',YPOLYMAX
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          WRITE (BUF,*) 'For unsmoothed data RMS/SQRT(Npts) = ',
     &                  RMSER/SQRT(SW)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
*
      IF (.NOT. AUTOFIT) THEN
*
          IF (IDPLOT1 .GT. 0) THEN
*             plot the data with the polynomial fit superimposed
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
              WRITE (TOPLBL,'(A,I2,A)') 'ORDER ',IPOLY,
     &            ' POLYNOMIAL FIT'
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
*         plot the polynomial fitted to the data on same graph
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
                  YP(NPTS) = POPJM1(I)
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
*         plot the data minus the polynomial on a new panel
*
          IF (IDPLOT1 .GT. 0) THEN
              WRITE (TOPLBL,'(A)') 'DATA - POLYNOMIAL'
              COLOUR = 1
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
                  YP(NPTS) = POERROR(I)
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
*         check acceptability of polynomial fit
*
          PRINT '(A,$)',' Is this fit okay (Enter = yes) ?'
          READ '(A)', ANSWER
          CALL UPCASE (ANSWER)
          IF (ANSWER .EQ. 'N') THEN
              PRINT '(A,$)',' Try another fit (Enter = yes) ?'
              READ '(A)', ANSWER2
              CALL UPCASE (ANSWER2)
              IF (ANSWER2 .EQ. 'N') THEN
                  PRINT *,'No polynomial fitted'
                  GO TO 900
              ELSE
                  IPOLY = 0
                  IF (.NOT. USEPB) THEN
                      PRINT '(2A,$)',' Use the same background blocks ',
     &                    '(Enter = yes) ?'
                      READ '(A)', ANSWER3
                      CALL UPCASE (ANSWER3)
*                     enter new baseline blocks
                      IF (ANSWER3 .EQ. 'N') GO TO 200
                  END IF
*                 enter new polynomial order
                  GO TO 300
              END IF
          END IF
      END IF 
*
*     store the data - polynomial back in Yin
*
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          YIN(MEM,I) = POERROR(I)
      END DO
      POLYFIT(MEM) = IPOLY
*
      WRITE (BUF,*) 'data - polynomial stored in mem ',MEM
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
 2010 FORMAT (' order',i3,' fit RMS error =',f12.6,a3)
 2011 FORMAT (' order',i3,' fit RMS error =',f12.6,a3,
     &        '  error decrease =',f12.6,a3) 
      END        
*********
