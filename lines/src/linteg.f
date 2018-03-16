***********************
      SUBROUTINE LINTEG (ERROR)
***********************
*     line integral calculation for spectra
*
*
*     CMDP command parameters used:
*     1 = command mnemonic INT
*     2 = start velocity for line integral window
*     3 = end velocity for line integral window
*     4 = m / s for multiple or single lines
*     5 = memory containing spectrum, default to set memory
*     
*     subroutines called:
*     aitov, getdate, lintstat, lintwide, ritov, vellim, vtoai
*
      IMPLICIT  NONE
      CHARACTER MULTIPLE*1          ! Multiple or Single lines
      LOGICAL   NEW_SUBPP           ! input NEW_SUBP value
      INTEGER   COLOURP             ! COLOUR on entry, restored on exit
      INTEGER   ERROR               ! output error status
      INTEGER   I                   ! loop index
      INTEGER   IMAX                ! array index of datamax
      INTEGER   IOS                 ! i/o status
      INTEGER   ISIGN               ! height positive or negative
      INTEGER   IVSIGN              ! sign of turbulent velocity
      INTEGER   J                   ! loop index
      INTEGER   LINEP               ! LINE on entry, restored on exit
      INTEGER   L1                  ! array index of window start vel
      INTEGER   L2                  ! array index of window end vel
      INTEGER   MEM                 ! memory in use
      INTEGER   NCHAR               ! external function
      INTEGER   NXSUBP              ! NXSUB on entry, restored on exit
      INTEGER   NYSUBP              ! NYSUB on entry, restored on exit
      INTEGER   WIDTHP              ! WIDTH on entry, restored on exit
      REAL      CHARSIZEP           ! char size on entry, restored on exit
      REAL      YPMAXP              ! YPMAX on entry, restored on exit
      REAL      YPMINP              ! YPMIN on entry, restored on exit
      REAL*8    CENTERI             ! index of line centroid from LINTSTAT
      REAL*8    CENTERV             ! velocity of line centroid
      REAL*8    CZCROSSI            ! center of zero crossing points
      REAL*8    CZCROSSV            ! center of zero crossing points
      REAL*8    C50MI               ! center of half maxima from LINTWIDE
      REAL*8    C50MV               ! center of half maximum points 
      REAL*8    DATAMAX             ! max abs data value in window
      REAL*8    DCAL                ! uncertainty in calibration
      REAL*8    DCENTERI            ! error in index of line centroid
      REAL*8    DFWHMI              ! error in width (as per gaussian)
      REAL*8    DFWHMV              ! error in width (as per gaussian)
      REAL*8    DHEIGHT             ! error in height 
      REAL*8    DJ                  ! array index of zero crossing
      REAL*8    DLTET               ! error in electron temp
      REAL*8    DVTURB              ! error in turbulent velocity
      REAL*8    DTLV                ! uncertainty in line integral
      REAL*8    FW50MI              ! index of FWHM from LINTWIDE
      REAL*8    FW50MV              ! FWHM in km/s
      REAL*8    HEIGHT              ! max data value in window
      REAL*8    LTETMP              ! LTE electron temperature
      REAL*8    PEAKEDNESS          ! peakedness from LISTATS
      REAL*8    RVTURB              ! turb vel sqd
      REAL*8    SKEWNESS            ! skewness from LISTATS
      REAL*8    STDEVI              ! std dev from LISTATS
      REAL*8    TLSUM               ! line temperature sum in window
      REAL*8    TLV                 ! line integral
      REAL*8    VMAX                ! velocity of datamax
      REAL*8    V1                  ! velocity at start of window
      REAL*8    V2                  ! velocity at end of window
      REAL*8    VL1                 ! velocity at start of window
      REAL*8    VL2                 ! velocity at end of window
      REAL*8    VTURB               ! turbulent velocity
      REAL*8    WCROSS1             ! index of lower line crossing
      REAL*8    WCROSS2             ! index of upper line crossing
      REAL*8    WEQUIVV             ! line equivalent width in km/s
      REAL*8    WMEANI              ! index of weighted mean from LISTATS
      REAL*8    WMEANV              ! velocity of weighted mean
      REAL*8    WZCROSSV            ! zero crossing width
*
      INCLUDE 'lines.inc'
*
*
      IF (DB) PRINT *,'in LINTEG'
      ERROR = 0
      V1 = 0d0
      V2 = 0d0
*     save plot entry values to be restored on exit
      CHARSIZEP = CHARSIZE
      COLOURP   = COLOUR
      LINEP     = LINE
      NEW_SUBPP = NEW_SUBP
      NXSUBP    = NXSUB
      NYSUBP    = NYSUB
      WIDTHP    = WIDTH
      YPMAXP    = YPMAX
      YPMINP    = YPMIN
*
*
*     get the memory to use
      MEM = 0
*
      CALL QMEMUSED
      IF (DB) PRINT *,' MEMUSED=',MEMUSED
      IF (MEMUSED .EQ. 0) THEN
          PRINT *,'no data to integrate'
          ERROR = 1
          RETURN
      END IF
*
      IF (NCMD .GE. 5) THEN
*         assume memory was specified as last parameter
          READ (CMDP(NCMD),*,IOSTAT=IOS,ERR=30) MEM
   30     CONTINUE
      END IF
*
      IF (MEM .EQ. 0) THEN
          IF (NAXIS(MEMSET) .EQ. 0) THEN 
              PRINT '(A,$)',' integrate data in which memory ? '
              READ '(A)',CMDP(5)
              READ (CMDP(5),*,ERR=40,IOSTAT=IOS) MEM
   40         IF (IOS .NE. 0) THEN
                  PRINT 2005, CMDP(5)
                  ERROR = 1
                  GO TO 900
              END IF
          ELSE
              PRINT *,'mem=memset'
              MEM = MEMSET
          END IF
      END IF
*
      IF (MEM .LT. 1 .OR. MEM .GT. MAXMEM) THEN
          PRINT 2005,CMDP(5)
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
*     multiple lines or single line ?
*
      MULTIPLE = ' '
      IF (NCMD .GE. 4) THEN
*         see if M or S given as fourth parameter
          READ (CMDP(4),'(A)',IOSTAT=IOS,ERR=50) BUF 
   50     IF (IOS .NE. 0) WRITE (BUF,'(A)') ' ' 
      END IF
      CALL UPCASE (BUF)
      IF (BUF(1:1) .EQ. 'M') MULTIPLE = 'M'
      IF (BUF(1:1) .EQ. 'S') MULTIPLE = 'S'
*      
      IF (MULTIPLE .NE. 'M' .AND. MULTIPLE .NE. 'S') THEN
          PRINT '(A,$)',' Are multiple lines present (ENTER = yes) ? '
          READ '(A)',BUF
          CALL UPCASE (BUF)
          IF (BUF(1:1) .NE. 'N' .AND. BUF(1:1) .NE. 'F') THEN
              MULTIPLE = 'M'
          ELSE
              MULTIPLE = 'S'
          END IF
      END IF
*
* 
*     get index/velocity limits for line integral sum 
*
      VL1 = -1111D0
      VL2 = -1111D0
      IF (NCMD .GE. 3) THEN
*         see if start and end velocities were specified as parms 2 and 3
          READ (CMDP(2),*,IOSTAT=IOS,ERR=60) VL1
   60     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(2)
              GO TO 100
          END IF
          READ (CMDP(3),*,IOSTAT=IOS,ERR=70) VL2
   70     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(2)
              GO TO 100
          END IF
      END IF
*
  100 IF (VL1 .EQ. -1111D0 .AND. VL2 .EQ. -1111D0) THEN
*         calculate and print velocities of first and last channels
          CALL AITOV (MEM,FIRST_CH(MEM),V1)
          CALL AITOV (MEM,LAST_CH(MEM),V2)
          CALL VELLIM (FIRST_CH(MEM),LAST_CH(MEM),V1,V2,1)
*
*
          IF (IDPLOT1 .GT. 0 .AND. PLOTCURS .EQ. 'YES') THEN
*             plot the spectrum in MEM
*
              IF (DB) PRINT *,'plot spectrum in mem ',MEM
              WRITE (CMDP(2),*) MEM
              NCMD = 2
*             store data and captions in plot arrays
              CALL PLCONFAU (ERROR)
*             force axis autoscaling on plot
              XPMIN = 0
              XPMAX = -1
              YPMIN = YPMINP
              YPMAX = YPMAXP
              CHARSIZE = 1.0
              COLOUR   = 1
              LINE = 1
              NEW_PAGE = .TRUE.
              NEW_SUBP = .TRUE.
              NXSUB = 1
              NYSUB = 1
              SYMBOL = 1
              WIDTH = 1
*
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT *,'plotting to ',PLOTDEV
              CALL PGSLCT (IDPLOT1)
*             plot the spectrum
              CALL PLOTDATA (ERROR)
*
              PRINT *,'For reliable line integral calculations it is'
              PRINT *,'essential to set the smallest velocity window'
              PRINT *,'that includes the line(s) of interest.'  
              PRINT *,'Use the mouse to mark the start and end of ',
     &                'velocity window'
*             get velocity window using cursor on graph
              CALL PLCURSPS (ERROR)
*
              IF (NPTC .GT. 0) THEN
*                 else store first two cursor X positions
                  VL1 = XCURS(1)
                  VL2 = XCURS(2)
                  IF (DB) PRINT *,'cursor VL1=',V1,' VL2=',V2
              END IF
          END IF
*          
          IF (IDPLOT1 .LE. 0 .OR. PLOTCURS .EQ. 'NO ' .OR.
     &        NPTC .LT. 2) THEN
  110         VL1 = V1
              VL2 = V2
              PRINT '(2A,$)',' Velocity limits for line integral ',
     &                '(km/s, / = as is) ? '
              READ '(A)',BUF 
              READ  (BUF,*,ERR=120,IOSTAT=IOS) VL1, VL2 
  120         IF (IOS .NE. 0) THEN
                  PRINT 2005, BUF
                  GO TO 110
              END IF
          END IF
      END IF
*      
*     get array indicies of start and end velocities      
      CALL XTOAI (MEM,VL1,L1)
      CALL XTOAI (MEM,VL2,L2)
      IF (L1 .LT. FIRST_CH(MEM)) THEN
*         outside limit - set equal to limit
          L1 = FIRST_CH(MEM)
      END IF
      IF (L2 .GT. LAST_CH(MEM)) THEN
*         outside limit - set equal to limit
          L2 = LAST_CH(MEM)
      END IF
*
      CALL AITOV (MEM,L1,VL1)
      CALL AITOV (MEM,L2,VL2)
* 
*
*     line integral calculation
* 
*     sum the data points within the specified window.
*     The baseline is assumed to be at zero, after a polynomial fit.
*     find the index of the data maximum
      TLSUM = 0D0 
      DATAMAX = 0D0
      DO I = L1, L2 
          TLSUM = TLSUM + YIN(MEM,I) 
          IF (ABS(YIN(MEM,I)) .GT. ABS(DATAMAX)) THEN
              DATAMAX = YIN(MEM,I) 
              IMAX = I
          END IF
      END DO
*
*     velocity of maximum
      CALL AITOV (MEM,IMAX,VMAX)
* 
*     Convert sum to line integral tlv in K km/s
      TLV = TLSUM * CDELT1(MEM)
* 
*     simple line height estimate 
      HEIGHT = YIN(MEM,IMAX) 
* 
*     see if line is positive or negative 
      IF (HEIGHT .GE. 0) THEN 
          ISIGN = +1
      ELSE
          ISIGN = -1
      END IF
*
      IF (MULTIPLE .EQ. 'S') THEN
*
*         line equivalent width estimate in velocity units
*
          WEQUIVV = TLSUM / HEIGHT * CDELT1(MEM)
*
*         do statistics on the line
*
          DO I = 1, NAXIS1(MEM)
              Y(I) = YIN(MEM,I)
          END DO
*          
          IF (DB) PRINT *,'call LINTSTAT'
          CALL LINTSTAT (DB,Y,L1,L2,ISIGN,
     &        CENTERI,WMEANI,STDEVI,SKEWNESS,PEAKEDNESS)
          CALL RITOV (MEM,CENTERI,CENTERV)
          CALL RITOV (MEM,WMEANI,WMEANV)
*
*         line full width at 1/2 * maximum
*
          IF (DB) PRINT *,'call LINTWIDE'
          CALL LINTWIDE (Y,L1,L2,IMAX,HEIGHT*0.5, 
     &                   C50MI,FW50MI)
          CALL RITOV (MEM,C50MI,C50MV)
          FW50MV = FW50MI * CDELT1(MEM)
*
*         zero crossing line width
*
          IF (DB) PRINT *,'0X width'
*         find index below centroid where line crosses zero
          DO J = NINT(CENTERI),FIRST_CH(MEM),-1
              IF ((YIN(MEM,J) / DATAMAX) .LT. 0.0) GO TO 210
          END DO
  210     DJ = YIN(MEM,J+1) / (YIN(MEM,J+1) - YIN(MEM,J))
          WCROSS1 = J + 1 - DJ
*
*         find index above centroid where line crosses zero
          DO J = NINT(CENTERI),LAST_CH(MEM),1
              IF ((YIN(MEM,J) / DATAMAX) .LT. 0.0) GO TO 220
          END DO
  220     DJ = YIN(MEM,J-1) / (YIN(MEM,J-1) - YIN(MEM,J))
          WCROSS2 = J - 1 + DJ
*          
*         zero crossing width and midpoint of zero crossing points
          WZCROSSV = (WCROSS2 - WCROSS1) * CDELT1(MEM)
          CZCROSSI = (WCROSS2 + WCROSS1) / 2
          CALL RITOV (MEM,CZCROSSI,CZCROSSV)
      END IF
*
*     rms noise in baseline estimate - use theoretical value
*
      IF (DB) PRINT *,'errors'
*     calibration uncertainty
      DCAL = DTSYS(MEM) / TSYS(MEM)
*
*     uncertainty in line height
      DHEIGHT = SQRT (RMS(MEM)**2 + (HEIGHT*DCAL)**2)
*
*     uncertainty in line integral
      DTLV = CDELT1(MEM) * RMS(MEM) / SQRT(FLOAT(L2-L1)) + 
     &       ABS(TLV * DCAL)
*
      IF (MULTIPLE .EQ. 'S') THEN
*         error in the width as for gaussian
          DFWHMI = FW50MI * DHEIGHT / ABS(HEIGHT)
          DFWHMV = DFWHMI * CDELT1(MEM)
*         error in velocity of centroid
          DCENTERI = DFWHMI / 2
      END IF
* 
*     recombination line electron temperature 
*
      IF (BUNIT(MEM) .EQ. 'K ' .AND. MULTIPLE .EQ. 'S' .AND. 
     &    TCONT(MEM) .GT. 0D0) THEN 
*         obtain LTE electron temperature and error 
          LTETMP = ((6760 * (RESTFREQ(MEM)/1D9)**1.1 * TCONT(MEM)) / 
     &               TLV)**0.87
          DLTET = LTETMP * 0.87 * SQRT ((TCER(MEM)/TCONT(MEM))**2 
     &                                + (DTLV/TLV)**2)
*
*         Check for negative root in VTURB
          IVSIGN = +1
          RVTURB = 0.541 * FW50MV**2 - 0.02475 * LTETMP
          IF (RVTURB .LT. 0.0) THEN
*            Negative root detected - line narrower than thermal width
             IVSIGN = -1
             RVTURB = -RVTURB
          END IF
          VTURB = SQRT(RVTURB) * IVSIGN
          DVTURB = SQRT((0.541 * 2.0 * DFWHMV * FW50MV)**2
     &              + (0.02475 * DLTET)**2) / (2.0 * VTURB)
      END IF
*
*
*     output to screen
*
      CALL GETDATE
      PRINT 7070, FITSDATE, OBJECT(MEM), SCAN(MEM), DATE_OBS(MEM)
      CALL VELLIM (L1,L2,VL1,VL2,1)
      PRINT 7100, RMS(MEM),BUNIT(MEM)(1:NCHAR(BUNIT(MEM)))
      PRINT 7110, DCAL*100
      PRINT 7120, ' Line peak    ', VMAX, FLOAT(IMAX)
*
      IF (MULTIPLE .EQ. 'S') THEN
          PRINT 7120, ' Line centroid', CENTERV, CENTERI
          PRINT 7130, DCENTERI*CDELT1(MEM),DCENTERI
          PRINT 7120, ' Weighted mean', WMEANV, WMEANI
          PRINT 7220, 'zero crossing width   ', WZCROSSV, CZCROSSV
          PRINT 7220, 'equivalent width      ', WEQUIVV, CENTERV
          PRINT 7220, 'full width at half max', FW50MV,C50MV
          PRINT 7230, DFWHMV
          PRINT *, 'skewness   = ',SKEWNESS
          PRINT *, 'peakedness =',PEAKEDNESS
      END IF
c
      PRINT 7300, HEIGHT, DHEIGHT, BUNIT(MEM)(1:NCHAR(BUNIT(MEM)))
      PRINT 7400, TLV, DTLV, BUNIT(MEM)(1:NCHAR(BUNIT(MEM)))
*
      IF (BUNIT(MEM) .EQ. 'K ' .AND. MULTIPLE .EQ. 'S' .AND. 
     &    TCONT(MEM) .GT. 0D0) THEN 
*
          PRINT 7500, TCONT(MEM), TCER(MEM),
     &                LTETMP, DLTET, VTURB, DVTURB
          IF (VTURB .LT. 0D0) PRINT 7510
      END IF 
*
*     write results to disk file logwrtfile
*
      IF (WRITELOG) THEN
*         write basic caption to buffer
          IF (DB) PRINT *,'fmt 8001'
          WRITE (LBUF,8001,IOSTAT=IOS)
*              12345678901234567890123456789012345678901234567890
     &        '   Fitdate, Name                ,  Scan,    DateObs,',
     &        '     MJD,        Resoln,     NomRMS, units,',
     &        '  PCCalUnc,   fromVel,     toVel,',
     &        '    LineHt,       dHt,     atVLn,',
     &        '   LineInt,      dInt,',
     &        '   VCentroid,     dVCent,   WeightMn,',
     &        ' ZeroXWidth,      atVZX,',
     &        ' EquivWidth,      atVEQ,',
     &        '   FWidthHM,      dFWHM,      atVFW,',
c     &        '   skewness,      peakedeness,',
     &        '     TCont,    dTCont,     LTETe,       dTe,',
     &        '    Vturb,    dVTurb'
          IF (IOS .NE. 0) PRINT *,'err ',IOS,' col hdr'
*         output to disk
          IF (DB) PRINT *,'write to ',LOGWRTFILE
          WRITE (LOGWRTUNIT,'(A)',IOSTAT=IOS) LBUF(1:NCHAR(LBUF))
          IF (IOS .NE. 0) CALL ER_WRITE (LOGWRTFILE,IOS)
*
*
*         write basic data to buffer
*
*         first convert embedded spaces in OBJECT to underscores
          WRITE (BUF2,'(A)') OBJECT(MEM)(1:20)
          DO I = 1, NCHAR(BUF2)
              IF (BUF2(I:I) .EQ. ' ') BUF2(I:I) = '_'
          END DO
*
          IF (DB) PRINT *,'fmt 8011'
          WRITE (LBUF,8011,IOSTAT=IOS) 
     &       FITSDATE(1:10),', ',BUF2(1:20),',', 
     &       SCAN(MEM),', ',DATE_OBS(MEM)(1:10),',',
     &       JULDATE(MEM)-2400000.5d0,',',
     &       CDELT1(MEM),',',RMS(MEM),', ',BUNIT(MEM)(1:5),',',
     &       DCAL*100,',', VL1,',', VL2,',', 
     &       HEIGHT,',', DHEIGHT,',', VMAX,',', 
     &       TLV,',', DTLV,','
          IF (IOS .NE. 0) PRINT *,'err ',IOS,' col data 1'
*
          IF (MULTIPLE .EQ. 'S') THEN
*             single line present
              IF (DB) PRINT *,'fmt 8012'
              WRITE (LBUF(185:),8012,IOSTAT=IOS)
     &        CENTERV,',', DCENTERI*CDELT1(MEM),',', WMEANV,',',
     &        WZCROSSV,',', CZCROSSV,',',
     &        WEQUIVV,',', CENTERV,',',
     &        FW50MV,',', DFWHMV,',', C50MV,','
              IF (IOS .NE. 0) PRINT *,'err ',IOS,' col data 2'
          END IF
*
          IF (BUNIT(MEM) .EQ. 'K ' .AND. MULTIPLE .EQ. 'S' .AND. 
     &        TCONT(MEM) .GT. 0D0) THEN 
*             recombination line
              IF (DB) PRINT *,'fmt 8013'
              WRITE (LBUF(305:),8013,IOSTAT=IOS)
     &        TCONT(MEM),',', TCER(MEM),',',
     &        LTETMP,',', DLTET,',', VTURB,',', DVTURB
              IF (IOS .NE. 0) PRINT *,'err ',IOS,' col hdr 3'
          END IF 
*
          WRITE (LOGWRTUNIT,'(A)',IOSTAT=IOS) LBUF(1:NCHAR(LBUF))
          IF (IOS .NE. 0) CALL ER_WRITE (LOGWRTFILE,IOS)
      END IF
*
*
  900 CONTINUE
*
*     reset plot parameters to values on entry
*
      CHARSIZE = CHARSIZEP
      COLOUR   = COLOURP
      LINE     = LINEP
      NEW_SUBP = NEW_SUBPP
      NXSUB    = NXSUBP
      NYSUB    = NYSUBP
      WIDTH    = WIDTHP
      YPMAX    = YPMAXP
      YPMIN    = YPMINP
*
      RETURN
*
 2005 FORMAT (' illegal: ',A)
 7070 FORMAT (//A10,
     &        /' Integral of ',A20,' scan ',I8,' of ',A)
 7100 FORMAT (' RMS noise in the spectrum = ',F12.6,1X,A)
 7110 FORMAT (' Calibration uncertainty = ',F7.3,' %')
 7120 FORMAT (A,' is at',F8.3,' km/s, I =',F7.3)
 7130 FORMAT (18X,'+-',F8.3,8X,'+-',F7.3)
 7220 FORMAT (' Line ',A,' =',F8.3,' km/s, centered on',F8.3,' km/s')
 7230 FORMAT (28X,'+-',F8.3)
 7300 FORMAT (' Line height   =',F12.6,' +-',F9.6,' ',A)
 7400 FORMAT (' Line integral =',F12.6,' +-',F9.6,1X,A,'*km/s')
 7500 FORMAT (/' HII region parameters from rec. line & continuum :'/
     &          6X,'Continuum Temp used =',F11.2,' +-',F8.2,1X/
     &          6X,'LTE Electron Temp   =',F11.0,' +-',F8.0,' K'/
     &          6X,'Turbulent Velocity  =',F11.2,' +-',F8.2,' km/s')
 7510 FORMAT (6X,'Negative Vturb physically unrealistic!')
 8001 FORMAT (A52,A43,A33,A33,A22,A37,A24,A24,A36,A30,A44,A22)
 8011 FORMAT (A10,A2,A20,A1,
     &        I6,A2,A10,A1,F11.3,A1,
     &        F11.6,A1,F11.6,A2,A5,A1,
     &        8(F10.3,A1))
 8012 FORMAT (10(F11.3,A1))
 8013 FORMAT (6(F10.3,A1))
*
      END 
