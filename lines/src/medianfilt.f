***********************
      SUBROUTINE MEDIANFILT (ERROR)
***********************
*     subroutine to set up median filtering
*
*     other subroutines called:
*
*     CMDP command parameters used:
*     1 = command mnemonic MF
*     2 = version 1 = 3RSSH, twice or version 2 = 4253H, twice
*     3 = memory to use
*
      IMPLICIT  NONE
      CHARACTER ANSWER*1        ! user answer
      CHARACTER ANSWER2*1       ! user answer
      CHARACTER MF(2)*11        ! median filter names
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
      INTEGER   VERSION         ! median filter 1 or 2
      INTEGER   WIDTHP          ! WIDTH on entry, restored on exit
      REAL      CHARSIZEP       ! char size on entry, restored on exit
      REAL      YPMAXP          ! YPMAX on input, restored on output
      REAL      YPMINP          ! YPMIN on input, restored on output
*
      INCLUDE   'lines.inc'
      DATA      MF / '3RSSH,Twice', '4253H,Twice'/ 
*
      IF (DB) PRINT *,'in MEDIANFILT'
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
*     median filter version
*
      VERSION = 0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=20) VERSION
   20     CONTINUE
      END IF
      IF (DB) PRINT *,'VERSION = ',VERSION
* 
      IF (VERSION .GT. 0) THEN
*         automatic fit using passed parameter
          AUTOFIT = .TRUE.
          IF (DB) PRINT *,'automatic median filt'
      ELSE
*         user prompted fit
          AUTOFIT = .FALSE.
          IF (DB) PRINT *,'interactive median filt'
      END IF
      IF (DB) PRINT *,'NSPL = ',VERSION
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
              PRINT '(A,$)',' median filter for which memory ?'
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
*             plot the data with the median filter superimposed
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
  300 IF (VERSION .EQ. 0) THEN
          PRINT *,'Median filter 1 = ',MF(1),' 2 =',MF(2)
          PRINT '(/A,$)',' Median filter 1 or 2 (or QUIT)?' 
          READ '(A)', BUF
          CALL UPCASE (BUF)
          IF (BUF(1:4) .EQ. 'QUIT') GO TO 900
          READ (BUF,*,IOSTAT=IOS,ERR=300) VERSION
      END IF
*
*     set up data arrays for median filter
*
      NPTS = LAST_CH(MEM) - FIRST_CH(MEM) + 1
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          XSPL(I) = XIN(MEM,I)
          YSPL(I) = YIN(MEM,I)
      END DO
*
*     median filter using arrays for splines (YSPLINE in common block)
*     YSPL = input Y values
*     YSPLINE = median filtered SMOOTH output
*     YMSPLINE = median filtered ROUGH output
*
      CALL EDA_RSM (DB,YSPL,NPTS,YSPLINE,YMSPLINE,VERSION,ERROR)
*
      IF (ERROR .NE. 0) THEN
          PRINT *,'median filter error = ',ERROR
          RETURN
      END IF
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
*         plot the data minus the splines on a new panel
*
          IF (IDPLOT1 .GT. 0) THEN
              WRITE (TOPLBL,'(2A)') 'Rough from median filter ',
     &        MF(VERSION)
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
                  PRINT *,'Not filtered'
                  GO TO 900
              ELSE
*                 replot data and enter new spline order
                  VERSION = 0
                  GO TO 200
              END IF
          END IF
      END IF 
*
*     store the median filtered SMOOTH output back in Yin
*
      J = 1
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          YIN(MEM,I) = YSPLINE(J)
          J = J + 1
      END DO
*
      WRITE (BUF,*) 'filtered data stored in mem ',MEM
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
