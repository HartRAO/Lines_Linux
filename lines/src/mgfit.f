**********************
      SUBROUTINE MGFIT (ERROR)
**********************
*     set up for Multiple Gaussian fitting using LGFIT2
*
*     This was derived from Sun Fortran version by MJG
*     This was derived from MS fortran 4.01 version by MJG
*     This was derived from HP1000 fortran version by MJG
*     Uses stripped version of von Meerwall's  program LGFIT2
*     ref : E von Meerwall (1975) Computer Physics Communications
*           vol 9, pp 117 - 128.
*
*     CMDP command parameters used:
*     1 = 'GF' gaussian fit mnemonic
*     2-4 = 'GP' if parameters from GFPARM to be used
*     2-4 = 'PL' to plot with GP
*     2-4 = memory to operate on, default to set memory
*
*     other subroutines called:
*     aitov, getdate, lgfit2, mgparm, pgslct, plcursps, plotdata, 
*     qmemused, setparm
*
      IMPLICIT  NONE
*
      LOGICAL   AUTOFIT           ! automatic / interactive fit
      LOGICAL   PL                ! true if PL specified
      INTEGER   COLOURP           ! COLOUR on entry, restored on exit
      INTEGER   ERROR             ! output error status
      INTEGER   I                 ! loop index
      INTEGER   II                ! loop index
      INTEGER   IOS               ! file i/o status
      INTEGER   ILINE             ! loop index
      INTEGER   IVSIGN            ! sign of turbulent velocity
      INTEGER   LINEP             ! LINE on entry, restored on exit
      INTEGER   MEM               ! memory in use
      INTEGER   NCHAR             ! external function
      INTEGER   NLINES            ! number of gaussians
      INTEGER   NXSUBP            ! NXSUB value on entry
      INTEGER   NYSUBP            ! NYSUB value on entry
      INTEGER   WIDTHP            ! WIDTH on entry, restored on exit
      REAL      CHARSIZEP         ! character size on entry
      REAL      YPMAXP            ! YPMAX on entry, restored on exit
      REAL      YPMINP            ! YPMIN on entry, restored on exit
      REAL*8    CFWIDTH           ! gaussian width corrected for resolution
      REAL*8    DCWIDTH           ! uncertainty in corr. gaussian width
      REAL*8    DHITE             ! uncertainty in gaussian height
      REAL*8    DLTET             ! uncertainty in LTE electron temp
      REAL*8    DPOSN             ! uncertainty in gaussian position
      REAL*8    DVTURB            ! uncertainty in turbulent velocity
      REAL*8    DWIDTH            ! uncertainty in gaussian width
      REAL*8    FGHITE            ! gaussian height
      REAL*8    FPOSN             ! gaussian position
      REAL*8    FRMSNS            ! rms noise of data - gaussians
      REAL*8    FSNRN             ! normalised signal to noise ratio
      REAL*8    FWIDTH            ! gaussian width
      REAL*8    LTETMP            ! LTE electron temperature estimate
      REAL*8    POSOFF            ! offset to put data centroid at zero
      REAL*8    RESOLV            ! assumed spectral resolution
      REAL*8    RVTURB            ! turbulent velocity squared
      REAL*8    VTURB             ! turbulent velocity
      REAL*8    XRANGE            ! range of X values
      REAL*8    XINTERVAL         ! interval between X values
      REAL*8    YMAXMIN           ! absolute largest value in data
*
      INCLUDE 'lines.inc'
*
*
      IF (DB) PRINT *,'in MGFIT'
      ERROR = 0
*
*     find how many memories are in use
      CALL QMEMUSED
      IF (MEMUSED .EQ. 0) THEN
          PRINT *,'no data to plot'
          ERROR = 1
          RETURN
      END IF
      
*     save entry values to be restored on exit
      CHARSIZEP = CHARSIZE
      COLOURP = COLOUR
      LINEP   = LINE
      NXSUBP = NXSUB
      NYSUBP = NYSUB
      WIDTHP = WIDTH
      YPMAXP = YPMAX
      YPMINP = YPMIN
*
      AUTOFIT = .FALSE.
      MEM = 0
      NLINES = 0
      PL = .FALSE.
      SCALEFACTOR = 1
      DO I = 2, NCMD
          READ (CMDP(I),*,IOSTAT=IOS,ERR=30) MEM
   30     CONTINUE
          CALL UPCASE (CMDP(I))
          IF (CMDP(I) .EQ. 'PL') THEN
              IF (IDPLOT1 .GT. 0) THEN
                  PL = .TRUE.
              ELSE
                  PRINT *,'no plot'
              END IF
          ELSE IF (CMDP(I) .EQ. 'GP') THEN
              IF (NPARIN .GT. 0) THEN
*                 automatic multiple gaussian fit using GP parameters
                  AUTOFIT = .TRUE.
                  IF (DB) PRINT *,'auto gausfit'
*
*                 store input parameters in working array for LGFIT2
                  NPAR = NPARIN
                  NLINES = NPAR / 3
                  IF (DB) PRINT *,'NPAR = ',NPAR
                  DO II = 1, NPAR
                      P(II) = PIN(II)
                  END DO
              ELSE
                  PRINT *,'GP not set!'
                  GO TO 900
              END IF
          END IF
      END DO
*
      IF (.NOT. AUTOFIT) THEN
          IF (IDPLOT1 .LE. 0) THEN
              PRINT *,'set plotdev first'
              GO TO 900
          ELSE
*             user-prompted fit
              PL = .TRUE.
              IF (DB) PRINT *,'interactive gausfit with plot'
          END IF    
      END IF
*
      IF (MEM .EQ. 0) THEN
          IF (NAXIS(MEMSET) .EQ. 0) THEN 
              PRINT '(A,$)',' fit data in which memory ?'
              READ '(A)',CMDP(3)
              READ (CMDP(3),*,ERR=40,IOSTAT=IOS) MEM
   40         IF (IOS .NE. 0) THEN
                  PRINT 2005, CMDP(3)
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
*     keep track of memory used for WRTGAUS
      MEMGF = MEM
*
*     store data in selected range in working arrays for LGFIT2
*
      NP = 0
      YMIN = YIN(MEM,FIRST_CH(MEM))     ! for plot?
      YMAX = YIN(MEM,FIRST_CH(MEM))     ! for plot?
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          NP = NP + 1                   ! number of points, for LGFIT2
          X(NP) = XIN(MEM,I)            ! offset subtracted later
          Y(NP) = YIN(MEM,I)            ! scalefactor applied later
          YMIN = MIN (YMIN, YIN(MEM,I))
          YMAX = MAX (YMAX, YIN(MEM,I))
      END DO
*
*     compute scalefactor to ensure convergence works;
*     data must be >> 1, so make largest value = 1e3
*
      YMAXMIN = MAX (ABS(YMAX),ABS(YMIN))
      SCALEFACTOR = 1.0E3 / YMAXMIN
*
*       for convergence tests only:
*
C      PRINT *,'SCALEFACTOR =',SCALEFACTOR,' NEW FACTOR ?'
C      READ *,SCALEFACTOR
*
*     for debug info only
      XRANGE = DABS (XIN(MEM,FIRST_CH(MEM)) - XIN(MEM,LAST_CH(MEM)))
      XINTERVAL = XRANGE / (NP - 1)     ! for info only
      IF (DB) PRINT *,'XRANGE = ',XRANGE
      IF (DB) PRINT *,'XINTERVAL = ',XINTERVAL
      IF (DB) PRINT *,'YMIN = ',YMIN
      IF (DB) PRINT *,'YMAX = ',YMAX
      IF (DB) PRINT *,'SCALEFACTOR = ',SCALEFACTOR
*
      IF (PL) THEN
*
*         plot the spectrum
*
          IF (DB) PRINT *,'plot spectrum in mem ',MEM
*         store data and captions in plot arrays
          WRITE (CMDP(2),*) MEM
          NCMD = 2
          CALL PLCONFAU (ERROR)
*         force X axis autoscaling on plot
          XPMIN = 0
          XPMAX = -1
          YPMIN = 0
          YPMAX = -1
          CHARSIZE = 1.0
          COLOUR = 1
          LINE = 1
          NEW_PAGE = .TRUE.
          NEW_SUBP = .TRUE.
          NXSUB = 1
          NYSUB = 1
          SYMBOL = 1
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
*         plot to interactive device
*
          CALL PGSLCT (IDPLOT1)
          CALL PLOTDATA (ERROR)
*
          IF (.NOT. AUTOFIT) THEN
*             interative fit
*             use cursor to mark half power points of gaussians
*        
              PRINT *,'Use the mouse to mark the half power points of ',
     &            'each Gaussian to fit'
              CALL PLCURSPS (ERROR)
              IF (NPTC .LT. 2) THEN
                  PRINT *,'No Gaussians were marked'
                  GO TO 900
              END IF
*
*             derive gaussian's position, height and width 
*
              NLINES = 0
              DO I = 1, NPTC, 2
                  NLINES = NLINES + 1
                  NPAR = NLINES * 3
*                 position
                  P(NPAR-2) = (XCURS(I) + XCURS(I+1))/2
*                 turn constraint off
                  A12IN(NPAR-2) = 1
*                 height
                  P(NPAR-1) = YCURS(I) + YCURS(I+1)
*                 turn constraint off
                  A12IN(NPAR-1) = 1
*                 width
                  P(NPAR) = ABS(XCURS(I+1) - XCURS(I))
*                 turn constraint off
                  A12IN(NPAR) = 1
              END DO
              PRINT *,'Unconstrained'
          END IF
      END IF
*              
*     list the Gaussian input parameters
      PRINT *,'  Position       Height     Width'
      DO I = 1, NPAR, 3
          PRINT '(F10.3,F14.6,F10.3)',P(I),P(I+1),P(I+2)
      END DO
*
*     subtract constant from Position guess and scale the height guess
*
      POSOFF = (XIN(MEM,FIRST_CH(MEM)) + XIN(MEM,LAST_CH(MEM))) / 2
      DO I = 1, NPAR, 3
          P(I) = P(I) - POSOFF
          P(I+1) = P(I+1) * SCALEFACTOR
      END DO
*
*     subtract constant from X and scale the Y data, before the fit
*
      DO I = 1, NP
*         subtract off constant to try to get drift scan fit to work
          X(I) = X(I) - POSOFF                   ! for LGFIT2
*         scale up the data to get convergence to work
          Y(I) = Y(I) * SCALEFACTOR              ! for LGFIT2
          IF (YMIN .GT. Y(NP)) YMIN = Y(NP)
          IF (YMAX .LT. Y(NP)) YMAX = Y(NP)
      END DO
*
*
*     fit multiple Gaussians iteratively to the spectrum
*
      CALL LGFIT2
*
*     Convert line parameters to correct units
*
*     frmsns  is RMS noise of data - gaussians, rescaled
      FRMSNS = SQRT (SMSQDV / (NP - NPAR)) / SCALEFACTOR
*
*
*     list results to screen for each line
*      
      ILINE = 0
      DO I = 1,NPAR,3
         ILINE = ILINE + 1
*
*        final parameters
*
*        Gaussian position with offset added again
         FPOSN  = P(I) + POSOFF
*        Gaussian height rescaled
         FGHITE = P(I+1) / SCALEFACTOR
*        Gaussian width
         FWIDTH = P(I+2)
*        normalised signal to noise ratio (VON HOERNER)
         FSNRN   = ABS (FGHITE * DSQRT(FWIDTH/XINTERVAL) / FRMSNS)
*
*        errors in parameters
*
*        From  KAPER et al. (1966) BAIN 18 465-487 see p 481, or
*           VON HOERNER  (1967) ASTROPHYS. J. 147 467-470 eqn 8.
*        NOTE that error estimates are multiplied by 2 * FRMSNS 
*        to get them to match result of single gaussian fit routine
*        obscure why, but works??
*        multiply position and width by scalefactor to get them right!!
*
         DPOSN  = HP(I) * 2 * FRMSNS * SCALEFACTOR
         DHITE  = ABS (HP(I+1)) * 2 * FRMSNS
         DWIDTH = HP(I+2) * 2 * FRMSNS * SCALEFACTOR
*
*        cfwidth is line width (km/s) maximally corrected for instrumental
*        broadening ( a delta function has apparent width = resolv)
*        as in REIFENSTEIN et al (1970) ASTRON. ASTROPHYS 4 357-377.
*        Error in corrected width as for uncorrected width
*        data are assumed to be unsmoothed rectangular-windowed spectra
         RESOLV = 1.2 * XINTERVAL
         IF (FWIDTH .GT. RESOLV) THEN
             CFWIDTH = DSQRT (FWIDTH**2 - RESOLV**2)
         ELSE
             CFWIDTH = 0.0
         END IF
         DCWIDTH = DWIDTH
*
*        Output best fit Gaussians with errors
*
         PRINT 6500,
     &        OBJECT(MEM)(1:20), SCAN(MEM),
     &        DATE_OBS(MEM)(1:NCHAR(DATE_OBS(MEM))),ILINE,
     &        FPOSN, DPOSN, FGHITE, DHITE, FWIDTH, DWIDTH,
     &        RESOLV, CFWIDTH, DCWIDTH,
     &        FSNRN
      END DO
*
      PRINT 6550, FRMSNS, RMS(MEM)
*     rescale the baseline
      PRINT *,'baseline at ',P(NPAR+1) / SCALEFACTOR
*
      IF (NLINES .EQ. 1 .AND. TCONT(MEM) .GT. 0D0) THEN
*
*        Calculate LTE Electron Temp 'LTETMP', error 'DLTET', and
*        turbulent velocity 'VTURB', error 'DVTURB' for Hna line.
*
*        LTETMP from eg BROWN, LOCKMANN, KNAPP (1978)
*                    ANN REV ASTRON ASTROPHYS 16 445-485.
*        Calculated using the UNCORRECTED line width.
*        VTURB  from EG VINER, VALLEE, HUGHES (1979)
*                    ASTROPHYS J SUPPL 39 (3) 405-428
*        Calculated using the CORRECTED line width.
*        Errors derived assuming parameters are normally distributed
*        and uncorrelated, not strictly true for HITE & WIDTH
*        See REIFENSTEIN (1970) for alternative derivation where
*        errors are added instead of taking root of sum of squares.
*
         LTETMP = ((6350.0 * (RESTFREQ(MEM)/1D9)**1.1 * TCONT(MEM)) /
     &             (FGHITE * CFWIDTH))**0.87
         DLTET  = LTETMP * 0.87 * SQRT((TCER(MEM)/TCONT(MEM))**2
     &               + (DHITE/FGHITE)**2 + (DCWIDTH/CFWIDTH)**2)
*
*        Check for negative root in VTURB
         IVSIGN = +1
         RVTURB = 0.541 * CFWIDTH**2 - 0.02475 * LTETMP
         IF (RVTURB .LT. 0.0) THEN
*           Negative root detected - line narrower than thermal width
            IVSIGN = -1
            RVTURB = -RVTURB
         END IF
         VTURB = SQRT(RVTURB) * IVSIGN
         DVTURB = SQRT((0.541 * 2.0 * DCWIDTH * CFWIDTH)**2
     *            + (0.02475 * DLTET)**2) / (2.0 * VTURB)
*
         PRINT 6520, TCONT(MEM), TCER(MEM),
     &               LTETMP, DLTET, VTURB, DVTURB
         IF (VTURB .LT. 0D0) PRINT 6530
      END IF
*
*
      IF (PL) THEN
*
*         plot the fitted gaussians superimposed on the spectrum
*
          COLOUR = 2
          LINE = 2
          SYMBOL = 1
          NEW_PAGE = .FALSE.
          NEW_SUBP = .FALSE.
          NPTS = 0
          DO I = 1, NP
              NPTS = NPTS + 1
*             use X values for plot with offset added
              XP(I) = X(I) + POSOFF
*             rescale the output fitted Gaussians
              YP(I) = GAUS(I) / SCALEFACTOR
          END DO
*         force X axis autoscaling on plot
          XPMIN = 0
          XPMAX = -1
*
*         plot to interactive device
*
          CALL PGSLCT (IDPLOT1)
          CALL PLOTDATA (ERROR)
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
*         plot the data minus the fitted gaussians
*
          PRINT *,'Press Enter key for plot of data - gaussians'
          READ  '(A)',BUF
          WRITE (TOPLBL,'(A,I2,A,F12.6)')
     &        'Data - ',NPAR/3,' Gaussians, RMS = ', FRMSNS
          COLOUR = 1
          LINE = 1
          SYMBOL = 1
          NEW_PAGE = .TRUE.
          NPTS = 0
          DO I = 1, NP
              NPTS = NPTS + 1
*             use X values for plot with offset added
              XP(I) = X(I) + POSOFF
*             rescale the residual values (data-gaussian)
              YP(I) = RES(I) / SCALEFACTOR
          END DO
*         force X axis autoscaling on plot
          XPMIN = 0
          XPMAX = -1
          YPMIN = 0
          YPMAX = -1
          IF (DB) PRINT *,'NPTS=',NPTS
*
*         plot to interactive device
*
          CALL PGSLCT (IDPLOT1)
          CALL PLOTDATA (ERROR)
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
*
*     if wanted, write results for each line to log write file
*
      IF (WRITELOG) THEN  
*         write fit data to buffers and thence to log write file
*
          IF (DB) PRINT *,'fmt 7001'
*         write source info to buffer
          WRITE (LBUF,7001,IOSTAT=IOS)
*              12345678901234567890123456789012345678901234567890
     &        '   Fitdate, Name                ,  Scan,    DateObs,',
     &        '     MJD,        Resoln,     FitRMS,     NomRMS,'
          IF (IOS .NE. 0) PRINT *,'err ',IOS,' col hdr'
*
          DO I = 1, NLINES
*             write column captions for each Gaussian in second buffer         
              IF (DB) PRINT *,'fmt 7002'
              WRITE (LBUF2,7002,IOSTAT=IOS)
     &        '  GausPos',I,',','       dP',I,',',
     &        '      Height',I,',','       dH',I,',',
     &        '    Width',I,',','       dW',I,',',
     &        ' CorWidth',I,',',' SgToNois',I,','
              IF (IOS .NE. 0) PRINT *,'err ',IOS,' gaus hdr ',I
*
*             move column captions to location in first buffer
              II = 101 + (I-1)*99
              WRITE (LBUF(II:II+99),'(A)') LBUF2(1:99)
          END DO
*
*         column headers for recombination line
          IF (NLINES .EQ. 1 .AND. TCONT(MEM) .GT. 0D0) THEN
              IF (DB) PRINT *,'fmt 7003'
              WRITE (LBUF(202:),7003,IOSTAT=IOS) 
     &        '      TCont,   TContErr,    LTETemp,   dLTETemp,',
     &        '      VTurb,     dVTurb'
              IF (IOS .NE. 0) PRINT *,'err ',IOS,' recom hdr'
          END IF
*
*         write buffer with headers to log write file
          WRITE (LOGWRTUNIT,'(A)',IOSTAT=IOS) LBUF(1:NCHAR(LBUF))  
          IF (IOS .NE. 0) CALL ER_WRITE (LOGWRTFILE,IOS)
*
*
*         write housekeeping and fit data to buffer for output
*
*         first convert embedded spaces in OBJECT to underscores
          WRITE (BUF2,'(A)') OBJECT(MEM)(1:20)
          DO I = 1, NCHAR(BUF2)
              IF (BUF2(I:I) .EQ. ' ') BUF2(I:I) = '_'
          END DO
*
*         get date and time in FITS format
          CALL GETDATE
          IF (DB) PRINT *,'fmt 7011'
          WRITE (LBUF,7011,IOSTAT=IOS)
     &        FITSDATE(1:10),', ',BUF2(1:20),',',SCAN(MEM),', ',
     &        DATE_OBS(MEM),',',JULDATE(MEM)-2400000.5D0,',',
     &        CDELT1(MEM),',',FRMSNS,',',RMS(MEM),','
          IF (IOS .NE. 0) PRINT *,'err ',IOS,' in hkeep'
*
          ILINE = 0
          DO I = 1,NPAR,3
              ILINE = ILINE + 1
*             Gaussian position with offset added again
C             to match what is written to screen (2005/12/12)
              FPOSN  = P(I) + POSOFF
C was              FPOSN  = P(I) + XIN(MEM,FIRST_CH(MEM))

*             Gaussian height rescaled
              FGHITE = P(I+1) / SCALEFACTOR
              FWIDTH = P(I+2)
              FSNRN   = ABS (FGHITE * DSQRT(FWIDTH/XINTERVAL) / FRMSNS)
C             to match what is written to screen (2005/12/12)
              DPOSN  = HP(I) * 2 * FRMSNS * SCALEFACTOR
C was              DPOSN  = HP(I) * 2 * FRMSNS
              DHITE  = ABS (HP(I+1)) * 2 * FRMSNS
C             to match what is written to screen (2005/12/12)
              DWIDTH = HP(I+2) * 2 * FRMSNS * SCALEFACTOR
C was              DWIDTH = HP(I+2) * 2 * FRMSNS
              RESOLV = 1.2 * XINTERVAL
              IF (FWIDTH .GT. RESOLV) THEN
                  CFWIDTH = DSQRT (FWIDTH**2 - RESOLV**2)
              ELSE
                  CFWIDTH = 0.0
              END IF
              DCWIDTH = DWIDTH
*
*             write results for each line to buffer
              IF (DB) PRINT *,'fmt 7012'
              WRITE (LBUF2,7012,IOSTAT=IOS)
     &            FPOSN,',', DPOSN,',', FGHITE,',', DHITE,',', 
     &            FWIDTH,',', DWIDTH,',',
     &            CFWIDTH,',', FSNRN,','
              IF (IOS .NE. 0) PRINT *,'err ',IOS,' in GAUS data'
*             move columns of results to large buffer
              II = 101 + (ILINE-1)*99
              WRITE (LBUF(II:II+99),'(A)') LBUF2(1:99)
          END DO
*
*         write results for recombination line to buffer
          IF (NLINES .EQ. 1 .AND. TCONT(MEM) .GT. 0D0) THEN
              IF (DB) PRINT *,'fmt 7013'
              WRITE (LBUF(202:), 7013,IOSTAT=IOS) 
     &        TCONT(MEM),',',TCER(MEM),',',
     &        LTETMP,',',DLTET,',',VTURB,',',DVTURB
              IF (IOS .NE. 0) PRINT *,'err ',IOS,' in RECOM data'
          END IF
*         
*         write buffer to disk file
          WRITE (LOGWRTUNIT,'(A)',IOSTAT=IOS) LBUF(1:NCHAR(LBUF))
          IF (IOS .NE. 0) CALL ER_WRITE (LOGWRTFILE,IOS)
*
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
 2005 FORMAT(' illegal: ',A)
 6500 FORMAT(/1X,A,' Scan',I8,' of ',A,' Line',I2,' Best Fit Gaussian'
     &       /3X,'Position          =',F14.6,' +-',F12.6,
     &       /3X,'Height            =',F14.6,' +-',F12.6,
     &       /3X,'Uncorrected width =',F14.6,' +-',F12.6,
     &       /3X,'Resolution        =',F14.6,
     &       /3X,'Corrected width   =',F14.6,' +-',F12.6,' (spectra)'
     &       /3X,'Signal / Noise    =',F14.6)
 6520 FORMAT(/' HII region parameters from rec. line & continuum :'/
     &        6X,'Continuum Temp used =',F11.3,' +-',F8.3,1X/
     &        6X,'LTE Electron Temp   =',F11.0,' +-',F8.0,' K'/
     &        6X,'Turbulent Velocity  =',F11.2,' +-',F8.2,' km/s')
 6530 FORMAT(6X,'Negative Vturb physically unrealistic!')
 6550 FORMAT(/3X,'RMS of residue    =',F12.6,' expected =',F12.6)
*
 7001 FORMAT (A52,A48)
 7002 FORMAT (A9,I2.2,A1, A9,I2.2,A1,
     &        A12,I2.2,A1, A9,I2.2,A1,
     &        A9,I2.2,A1, A9,I2.2,A1,
     &        A9,I2.2,A1, A9,I2.2,A1)
 7003 FORMAT (A48,A23)
 7011 FORMAT (A10,A2,A20,A1,I6,A2,
     &        A10,A1,F11.3,A1,
     &        3(F11.6,A1))
 7012 FORMAT (2(F11.6,A1),F14.6,A1,5(F11.6,A1))
 7013 FORMAT (6(F11.3,A1))
*
      END
******
