*     lines.inc = include file for lines.f and subroutines
*     if this is altered, "make clean" before running "make" again
*
*     array size parameters
*
      INTEGER*4  BUFLEN                 ! standard buffer size
      PARAMETER (BUFLEN = 160)
      INTEGER*4  LBUFLEN                ! large buffer size for 1024 pts
      PARAMETER (LBUFLEN = 16000)
      INTEGER*4  LOOPBUFSIZE            ! loop buffer size
      PARAMETER (LOOPBUFSIZE = 2000)    
      INTEGER*4  MAXBASE                ! number of polynomial baseline pairs
      PARAMETER (MAXBASE = 40)
      INTEGER*4  MAXDATA                ! size of data arrays
      PARAMETER (MAXDATA = 8192)        ! was 1028
      INTEGER*4  MAXLOOPCMDS            ! max number of cmds in do loop
      PARAMETER (MAXLOOPCMDS = 50)
      INTEGER*4  MAXMEM                 ! number of memories
      PARAMETER (MAXMEM = 13)
*     maxpar must also be changed in LGFT2 and FDEL and WRTGAUS
      INTEGER*4  MAXLINES               ! max number of gaussians to fit
      PARAMETER (MAXLINES = 50)        
      INTEGER*4  MAXCMDPARMS            ! max parameters on command line
      PARAMETER  (MAXCMDPARMS = 9)
      INTEGER*4  MAXPOLY                ! max polynomial order
      PARAMETER (MAXPOLY = 10)
      INTEGER*4  MAXPTC                 ! cursor-marked position arrays
      PARAMETER (MAXPTC = 100)
      INTEGER*4 PARMLEN                 ! max len of each command parameter
      PARAMETER (PARMLEN = 80)
      INTEGER*4  MAXPSSVALS             ! number of PSS changes
      PARAMETER (MAXPSSVALS = 200)
*
*     housekeeping in character arrays for each memory in /HKEEP1/
*
      CHARACTER BUNIT(MAXMEM)*20        ! brightness units of data (FITS)
      CHARACTER CTYPE1(MAXMEM)*30       ! coordinate type for axis 1 (FITS)
      CHARACTER CTYPE2(MAXMEM)*30       ! coordinate type for axis 2 (FITS?)
      CHARACTER DATE(MAXMEM)*20         ! date of creation of hdu (FITS)
      CHARACTER DATE_OBS(MAXMEM)*20     ! date of observation (FITS)
      CHARACTER FROMFILE(MAXMEM)*80     ! file the spectrum is from (was 64)
      CHARACTER OBJECT(MAXMEM)*66       ! name of object (FITS)
      CHARACTER COORDSYS(MAXMEM)*20     ! coordinate system for object (FITS)
      CHARACTER OBSERVER(MAXMEM)*20     ! observer's name (FITS)
      CHARACTER POSITION(MAXMEM)*8      ! codes ON, HPN etc for pointing corr
      CHARACTER TCALUNIT(MAXMEM)*20     ! calibration signal units
      CHARACTER TUNIT1(MAXMEM)*20       ! units of X axis (FITS)
      CHARACTER TELESCOP(MAXMEM)*20     ! telescope (FITS)
*
*     housekeeping for each memory in /HKEEP2/
*
      INTEGER*4 CRPIX1(MAXMEM)          ! location of ref point on axis (FITS)
      INTEGER*4 FIRST_CH(MAXMEM)        ! first valid pixel in data
      INTEGER*4 FOLDED(MAXMEM)          ! 0=unfolded, 1=folded freq. sw.sp.
      INTEGER*4 IXSPACING(MAXMEM)       ! 0=regulr 1=irreg 2=not monotonic
      INTEGER*4 LAST_CH(MAXMEM)         ! last valid pixel in data
      INTEGER*4 NAXIS(MAXMEM)           ! number of axes in data array (FITS)
      INTEGER*4 NAXIS1(MAXMEM)          ! number of values on AXIS1 (FITS)
      INTEGER*4 NUMFILE(MAXMEM)         ! number of spectrum in file
      INTEGER*4 POL(MAXMEM)             ! polarization (~FITS)
      INTEGER*4 POLYFIT(MAXMEM)         ! order of polynomial fit to baseline
      INTEGER*4 SCAN(MAXMEM)            ! scan or observation number
      INTEGER*4 SMOOTHED(MAXMEM)        ! =0 unsmoothed =1 smoothed
      INTEGER*4 TRANSFRM(MAXMEM)        ! =0 spectrum =1 transform
*
*     housekeeping for each memory in /HKEEP3/
*
      REAL*8    ADDED(MAXMEM)           ! number of spectra averaged
      REAL*8    ALTDEG(MAXMEM)          ! telescope altitude (elev) in deg
      REAL*8    ATMCORR(MAXMEM)         ! atmospheric atten. corr. applied
      REAL*8    BW(MAXMEM)              ! original bandwidth of spectrum in Hz
      REAL*8    CDELT1(MAXMEM)          ! pixel spacing on NAXIS1 (FITS)
      REAL*8    CRVAL1(MAXMEM)          ! value at CRPIX1 (FITS)
      REAL*8    DEC(MAXMEM)             ! DEC of EQUINOX in degrees (FITS)
      REAL*8    DECDOWN(MAXMEM)         ! DEC of EQUINOX for down spectrum
      REAL*8    DTSYS(MAXMEM)           ! uncertainty in TSYS in K
      REAL*8    DUR(MAXMEM)             ! total integration time of SCAN in S
      REAL*8    EQUINOX(MAXMEM)         ! equinox in years for coords (FITS)
      REAL*8    FROFFSET(MAXMEM)        ! frequency offset for fr sw sp in Hz
      REAL*8    GAINCORR(MAXMEM)        ! antenna gain correction applied
      REAL*8    GBII(MAXMEM)            ! galactic latitude
      REAL*8    GBIIDOWN(MAXMEM)        ! galactic latitude for down posn
      REAL*8    GLII(MAXMEM)            ! galactic longitude 
      REAL*8    GLIIDOWN(MAXMEM)        ! galactic longitude for down posn
      REAL*8    HA(MAXMEM)              ! hour angle of SCAN
      REAL*8    JULDATE(MAXMEM)         ! Julian date of SCAN
      REAL*8    PNTCORR(MAXMEM)         ! pointing correction applied
      REAL*8    PSS(MAXMEM)             ! point source sensitivity in Jy/K
      REAL*8    RA(MAXMEM)              ! RA of EQUINOX in degrees (FITS)
      REAL*8    RADOWN(MAXMEM)          ! RA of EQUINOX for down spectrum
      REAL*8    RESTFREQ(MAXMEM)        ! line rest frequency in Hz
      REAL*8    RMS(MAXMEM)             ! expected rms noise in data in BUNITS
      REAL*8    SPVLSR(MAXMEM)          ! Velocity on which line is centred
      REAL*8    TCAL(MAXMEM)            ! calibration signal value
      REAL*8    TCER(MAXMEM)            ! error in continuum ant. temp. in K
      REAL*8    TCONT(MAXMEM)           ! continuum ant. temp for recom line
      REAL*8    TSYS(MAXMEM)            ! system temperature of SCAN in K
*
*     data array for each memory, in /XYDATA1/
*  
      REAL*8    XIN(MAXMEM,MAXDATA)     ! for X values / velocities
      REAL*8    YIN(MAXMEM,MAXDATA)     ! for Y values / pol1 data
      REAL*8    YIN2(MAXDATA)           ! for reading pol2 data
*
*     i/o character buffers and user commands in /BUFFERS1/
*
      CHARACTER BUF*(BUFLEN)            ! i/o buffer
      CHARACTER BUF2*(BUFLEN)           ! i/o buffer 2
      CHARACTER CMD*(BUFLEN)            ! command buffer
      CHARACTER CMDP(MAXCMDPARMS)*(PARMLEN) ! command parameters from CMD
      CHARACTER FITSDATE*20             ! date/time in FITS format
      CHARACTER LBUF*(LBUFLEN)          ! large buffer
      CHARACTER LBUF2*(LBUFLEN)         ! large buffer 2
      CHARACTER LOOPBUF*(LOOPBUFSIZE)   ! buffer for do loop commands
      CHARACTER LOOPNAME*20             ! name of loop variable
*
*     read and write file unit numbers in /UNITS1/
*
      INTEGER*4  ASCRDUNIT              ! ascii spectrum file read unit
      INTEGER*4  CMDRDUNIT              ! command input file read unit
      INTEGER*4  MCARDUNIT              ! multicolumn ascii file read unit
      INTEGER*4  NCSVRDUNIT             ! nccs csv file read unit
      INTEGER*4  NEWSRDUNIT             ! news read unit
      INTEGER*4  NFRDUNIT               ! rncsv 'nextfile' read unit
      INTEGER*4  PSSRDUNIT              ! pss file read unit
*
      INTEGER*4  ASCWRTUNIT             ! ascii spectra file write unit
      INTEGER*4  GWWRTUNIT              ! multiple gaussian parm write unit
      INTEGER*4  LOGWRTUNIT             ! lines log file write unit
      INTEGER*4  MCAWRTUNIT             ! multicolumn ascii file write unit
      INTEGER*4  TMWRTUNIT              ! time series file write unit
      INTEGER*4  TMWRTUNIT2             ! time series file write unit2
      INTEGER*4  TMWRTUNIT3             ! time series file write unit3
      INTEGER*4  TMWRTUNIT4             ! time series file write unit4
      INTEGER*4  FLAWRTUNIT             ! file list auto write unit
*
*     file names in /FILES1/
*     (were *64)
      CHARACTER ASCRDFILE*80            ! ascii-format file to read from
      CHARACTER CMDRDFILE*80            ! input command read file
      CHARACTER MCARDFILE*80            ! multicolumn ascii read file
      CHARACTER HLPRDDIR*80            ! online help read directory/file
      CHARACTER NCSVRDFILE*80           ! nccs csv read file name
      CHARACTER NEWSRDFILE*80           ! news file
      CHARACTER PSSRDFILE*80            ! pss read file name
*
      CHARACTER ASCWRTFILE*80           ! ascii-format file to write to
      CHARACTER GWWRTFILE*80            ! gaussian profile write file
      CHARACTER LOGWRTFILE*14           ! lines log write file
      CHARACTER MCAWRTFILE*80           ! multicolumn ascii file to write to
      CHARACTER PREVAFILE*80            ! previously opened ascii read file
      CHARACTER TMWRTFILE*80            ! time series write file
      CHARACTER TMWRTFILE2*80           ! time series write file 2
      CHARACTER TMWRTFILE3*80           ! time series write file 3
      CHARACTER TMWRTFILE4*80           ! time series write file 4
      CHARACTER FLAWRTFILE*14           ! file list auto write file
*
*
*     times series velocity ranges in /PARMS1/
*
      REAL*8    TMVSTART(MAXBASE) ! start velocities
      REAL*8    TMVEND(MAXBASE)   ! end velocities
*
*     height at velocity values from vh, in /PARMS1/
*
      REAL*8    HATV1           ! output height at velocity of "up" peak
      REAL*8    HATV2           ! output height of FS "down" peak
*
*     pointing correction value from pc, in /PARMS1/
*     derived from 1D or 2D gaussian fit to intensities NSEWOn
      REAL*8    POINTCOR        ! pointing correction for on source intensity
      REAL*8    PCNS            ! pointing error NS
      REAL*8    PCEW            ! pointing error EW
*
*     telescope longitude and latitude for ST, HA, ALT calcs, in /PARMS1/
*
      REAL*8    TELLONG         ! telescope longitude in degrees
      REAL*8    TELLAT          ! telescope latitude in degrees
*
*     precipitable water vapour for atmosheric absorption correction at 22GHz
*
      REAL*8    PWV             ! precipitable water vapour [mm]
*
*     gain curve vs HA polynomial coeffs, in /PARMS1/
*
      REAL*8    GCC(MAXCMDPARMS)        ! gain curve polynomial coefficients
*
*     gain curve vs JD-2440000 polynomial coeffs, in /PARMS1/
*
      REAL*8    JDC(MAXCMDPARMS)        ! gain curve polynomial coefficients
*
*     program variables and parameters in /PARMS1/
*
      INTEGER*4 ASCAT                   ! at this spectrum in asc rd file
      INTEGER*4 ASCLINE                 ! at this line in ascii file
      INTEGER*4 ASCWANTED               ! wanted spectrum in asc rd file
      INTEGER*4 ICMDSTART(MAXCMDPARMS)  ! start bytes for parms
      INTEGER*4 ICMDEND(MAXCMDPARMS)    ! start bytes for parms
      INTEGER*4 LCMD                    ! number characters in cmd buffer
      INTEGER*4 LOOPCMDEND(MAXLOOPCMDS) ! index of loop command end
      INTEGER*4 LOOPCMDNUM              ! number of commands in loop
      INTEGER*4 LOOPCMDSTRT(MAXLOOPCMDS)! index of loop command start
      INTEGER*4 LOOPEND                 ! do loop end value
      INTEGER*4 LOOPINDEX               ! do loop index
      INTEGER*4 LOOPINC                 ! do loop increment
      INTEGER*4 LOOPSTART               ! do loop start value
      INTEGER*4 MEMPLOTTED              ! memory from which data was last plotted
      INTEGER*4 MEMRD                   ! memory to read into from file
      INTEGER*4 MEMSET                  ! memory in use - default
      INTEGER*4 MEMUSED                 ! no. of memories in use
      INTEGER*4 NCMD                    ! number of parameters in command
      INTEGER*4 NGCC                    ! number of gain vs HA corr coeffs 
      INTEGER*4 NJDC                    ! number of gain vs JD corr coeffs 
      INTEGER*4 NTMVEL                  ! number of velocity range blocks in use
*
      LOGICAL   DB                      ! debug on/off
      LOGICAL   DOLOOP                  ! do commands in loop
      LOGICAL   GETLOOPCMD              ! get commands for loop
      LOGICAL   TMWVNUM                 ! numeric / char velocity header
      LOGICAL   WRITELOG                ! logging to file on/off
*
*     line integral velocity range
c      INTEGER*4 NLIWINDOW         ! number of velocity windows in use
c      REAL*8    LIVSTART          ! start velocity
c      REAL*8    LIVEND            ! end velocity
*
*     polynomial fit parameters in /POLY1/
*
      REAL*8    BASESTART(MAXBASE)      ! baseline start for polyfit
      REAL*8    BASEEND(MAXBASE)        ! baseline end for polyfit
      REAL*8    POA(MAXPOLY+1)          ! polynomial coefficients
      REAL*8    POERROR(MAXDATA)        ! data minus the fitted polynomial 
      REAL*8    POPJ(MAXDATA)           ! working array
      REAL*8    POPJM1(MAXDATA)         ! fitted polynomial
      REAL*8    POW(MAXDATA)            ! weights
      REAL*8    POX(MAXDATA)            ! independent values (abscissae)
      REAL*8    POY(MAXDATA)            ! dependent values (ordinates)
*
*     spline fit arrays - also used for median filtering
*
      REAL*8    XSPL(MAXDATA)     ! input X values for BELLS
      REAL*8    YSPL(MAXDATA)     ! input Y values for BELLS
      REAL*8    XSPLINE(MAXDATA)  ! splines X values as in BELLS
      REAL*8    YSPLINE(MAXDATA)  ! splines out at uniform X from BELLS
      REAL*8    YSPLOUT(MAXDATA)  ! splines out at input X from SPLINEATX
      REAL*8    YMSPLINE(MAXDATA) ! data - fitted splines from SPLINEATX
*
      INTEGER*4 NBASE                   ! number of baseline pairs
*
*     gaussian fit arrays and parameters in /GAUS1/
*
      INTEGER*4 IFL               ! line type (unused - Lorentz/Gauss)
      INTEGER*4 MEMGF             ! memory used for Gaussian fit
      INTEGER*4 NP                ! number of points in spectrum
      INTEGER*4 NPAR              ! number of parameters = lines * 3
      INTEGER*4 NPARIN            ! number of parameters from MGPARM
      INTEGER*4 A12IN(MAXLINES*3) ! constraint switch 
      LOGICAL   FIXBASE           ! fix baseline at zero or leave free
*
      REAL*8    SCALEFACTOR       ! scale data up to ensure convergence
      REAL*8    SMSQDV            ! sum of squares of deviations
      REAL*8    GAUS(MAXDATA)     ! profile of fitted gaussians
      REAL*8    RES(MAXDATA)      ! data - gaussians
      REAL*8    X(MAXDATA)        ! data abscissae
      REAL*8    Y(MAXDATA)        ! data ordinates
      REAL*8    HP(MAXLINES*3+1)  ! output uncertainties in cent, ht, width
      REAL*8    P(MAXLINES*3+1)   ! output centroid, height, width triplets
      REAL*8    PIN(MAXLINES*3)   ! MGPARM centroid, height, width triplets
*
*     character strings in routines calling PGPLOT via PGDOPLOT in /LIPLOT1/
*
      CHARACTER CAPTION*80        ! user entered caption
      CHARACTER PLOTDEV*20        ! PGPLOT interactive device in use
      CHARACTER PLOTCURS*3        ! plotdev cursor, YES or NO
      CHARACTER PLOTPS*80         ! PG postscript file, or 'OFF'
      CHARACTER TOPLBL*80         ! PG top label on graph
      CHARACTER TSYSLBL*58        ! optional caption for Tsys
      CHARACTER XLBL*80           ! PG+ X axis label
      CHARACTER YLBL*80           ! PG+ Y axis label
*
*     variables in routines calling PGPLOT via PGDOPLOT in /LIPLOT2/
*
      INTEGER   AXIS              ! PG axis type eg box, log, linear etc
      INTEGER   COLOUR            ! PG colour of data points and line
      INTEGER   FONT              ! PG font for labels
      INTEGER   IDPLOT1           ! PG plot device id #1
      INTEGER   IDPLOT2           ! PG plot device id #2
      INTEGER   IXBAR             ! PG 1 to plot error bars else 0
      INTEGER   IYBAR             ! PG 1 to plot error bars else 0
      INTEGER   LINE              ! PG line type joining data points
      INTEGER   NPTC              ! PG number of cursor-marked points
      INTEGER   NPTS              ! PG number of valid data points
      INTEGER   NTXT              ! number of text / labels to add to plot
      INTEGER   NXSUB             ! PG number of subdivisions of page in X
      INTEGER   NYSUB             ! PG number of subdivisions of page in Y
      INTEGER   SYMBOL            ! PG symbol to plot at each datum
      INTEGER   TXTEND(MAXPTC)    ! position in string where text ends
      INTEGER   TXTSTART(MAXPTC)  ! position in string where text starts
      INTEGER   WIDTH             ! PG width of line joining data points
      INTEGER   IAR(MAXPTC)       ! array of indicies of points marked by cursor 
*
      LOGICAL   ADDIDENT          ! PG true to add usrname date time to plot
      LOGICAL   ADDTSYS           ! true to add Tsys to plot
      LOGICAL   AUTOCONFIG        ! PG true for auto, false user config
      LOGICAL   NEW_PAGE          ! PG true for new plot on panel 
      LOGICAL   NEW_SUBP          ! PG true to for new panel subdivision
      LOGICAL   PGPROMPT          ! PG true to prompt for new page
*
*     note PGPLOT uses REAL*4 not REAL*8 real variables
      REAL      CHARSIZE          ! PG character size; def=1.0=1/40 plot ht
      REAL      XBAR(MAXDATA)     ! PG X errors
      REAL      XCURS(MAXPTC)     ! PG X positions marked with cursor
      REAL      XMAX              ! PG X max
      REAL      XMIN              ! PG X min
      REAL      XP(MAXDATA)       ! PG X values
      REAL      XPMAX             ! PG X plot max
      REAL      XPMIN             ! PG X plot min
      REAL      YBAR(MAXDATA)     ! PG y errors
      REAL      YCURS(MAXPTC)     ! PG Y positions marked with cursor
      REAL      YMAX              ! PG Y max
      REAL      YMIN              ! PG Y min
      REAL      YP(MAXDATA)       ! PG Y values
      REAL      YPMAX             ! PG Y plot max
      REAL      YPMIN             ! PG Y plot min
*
*    Point Source Sensitivity values read from users file, in /PSSVALS/
*
      REAL*8    DJDPSS(MAXPSSVALS)   ! JD of changes to PSS, read from file
      REAL*8    PSSATJD(MAXPSSVALS)  ! PSS, read from file
      REAL*8    PSSLOPE(MAXPSSVALS)  ! dPSS/dT, read from file
      INTEGER*4 NPSS                 ! number PSS values read in

*
*
******common block definitions
*
*     spectrum housekeeping character arrays
*
      COMMON /HKEEP1/ 
     & BUNIT,    CTYPE1,   CTYPE2,   DATE, 
     & DATE_OBS, FROMFILE, OBJECT,   COORDSYS, OBSERVER,   
     & TCALUNIT, TUNIT1,   TELESCOP, POSITION
*
*     spectrum housekeeping variables
*
      COMMON /HKEEP2/ 
     & CRPIX1,   FIRST_CH,  FOLDED, IXSPACING,
     & LAST_CH,  NAXIS,     NAXIS1, 
     & NUMFILE,  POL,       POLYFIT, 
     & SCAN,     SMOOTHED,  TRANSFRM

      COMMON /HKEEP3/
     & ADDED,    ALTDEG,    ATMCORR,
     & BW,       CDELT1, 
     & CRVAL1,   DEC,       DECDOWN, 
     & DTSYS,    DUR,       EQUINOX,
     & FROFFSET, GAINCORR,
     & GBII,     GBIIDOWN, 
     & GLII,     GLIIDOWN,  HA, 
     & JULDATE,  PNTCORR,   PSS,       
     & RA,       RADOWN,    RESTFREQ,
     & RMS,      SPVLSR,    TCAL,      
     & TCER,     TCONT,     TSYS 
*
*     input X, Y data
*
      COMMON /XYDATA1/ XIN, YIN, YIN2
*
*     i/o buffers - character
*
      COMMON /BUFFERS1/ 
     & BUF, BUF2, CMD, CMDP, FITSDATE, LBUF, LBUF2, LOOPBUF, LOOPNAME
*
*     program parameters - character file names
*
      COMMON /FILES1/ 
     & ASCRDFILE,  CMDRDFILE,  MCARDFILE,  NCSVRDFILE,
     & NEWSRDFILE, PSSRDFILE,  HLPRDDIR,  
     & ASCWRTFILE, LOGWRTFILE, 
     & GWWRTFILE,  MCAWRTFILE,
     & PREVAFILE,
     & TMWRTFILE,  TMWRTFILE2, TMWRTFILE3, TMWRTFILE4,
     & FLAWRTFILE
*
*     read and write unit numbers
*
      COMMON /UNITS1/
     & ASCRDUNIT,  CMDRDUNIT, MCARDUNIT,   NCSVRDUNIT,  
     & NEWSRDUNIT, NFRDUNIT,  PSSRDUNIT,
     & ASCWRTUNIT, GWWRTUNIT, LOGWRTUNIT, 
     & MCAWRTUNIT,  
     & TMWRTUNIT,  TMWRTUNIT2, TMWRTUNIT3, TMWRTUNIT4,
     & FLAWRTUNIT
*
*     program parameters - variables
*     (put logicals after integers to keep compiler happier)
      COMMON /PARMS1/ 
     & TMVSTART,  TMVEND,                       ! real*8
     & HATV1,     HATV2,      POINTCOR, 
     & PCNS,      PCEW,
     & TELLONG,   TELLAT,     PWV,
     & GCC,       JDC,
     & ASCAT,     ASCLINE,    ASCWANTED,        ! integer
     & ICMDSTART, ICMDEND,      
     & LCMD,      LOOPCMDEND, LOOPCMDNUM,
     & LOOPCMDSTRT,
     & LOOPEND,   LOOPINDEX,  LOOPINC,
     & LOOPSTART, MEMPLOTTED, MEMRD,      MEMSET,
     & MEMUSED,   NCMD,       NGCC,     NJDC,
     & NTMVEL,    
     & DB,        DOLOOP,     GETLOOPCMD,       ! logical
     & TMWVNUM,   WRITELOG
*
*     polynomial parameters and arrays
*
      COMMON /POLY1/ 
     & BASESTART, BASEEND, POA,
     & POERROR,   POPJ,    POPJM1,
     & POW,       POX,     POY,
     & NBASE
*
*     spline fit arrays
*
      COMMON /SPLINE/ XSPL, YSPL, XSPLINE, YSPLINE, YSPLOUT, YMSPLINE
*
*     gaussian fit parameters and arrays
*
      COMMON /GAUS1/ 
     & IFL,    MEMGF,  NP,     NPAR,  NPARIN, A12IN,  FIXBASE,
     & SCALEFACTOR,    SMSQDV,
     & GAUS,   RES,    X,      Y,     HP,     P,      PIN    
*
*     plot character arrays 
*
      COMMON /LIPLOT1/ 
     & CAPTION, PLOTDEV, PLOTCURS, PLOTPS, TOPLBL, TSYSLBL, XLBL, YLBL
*
*     plot parameters - integer then logical then real*4
*
      COMMON /LIPLOT2/ 
     & AXIS,     COLOUR,     FONT, 
     & IDPLOT1,  IDPLOT2,    IXBAR, 
     & IYBAR,    LINE,       NPTC, 
     & NPTS,     NTXT,       NXSUB,      NYSUB, 
     & SYMBOL,   TXTEND,     TXTSTART,   
     & WIDTH,    IAR,
     & CHARSIZE,
     & ADDIDENT, ADDTSYS,    AUTOCONFIG, 
     & NEW_PAGE, NEW_SUBP,   PGPROMPT,
     & XP,       XBAR,       XCURS, 
     & XMAX,     XMIN,       XPMAX, 
     & XPMIN,    YP,         YBAR, 
     & YCURS,    YMAX,       YMIN, 
     & YPMAX,    YPMIN
*
*     point source sensitivity values
*
      COMMON/PSSVALS/
     & DJDPSS,   PSSATJD,    PSSLOPE,    NPSS
* 
      SAVE /HKEEP1/
      SAVE /HKEEP2/
      SAVE /XYDATA1/
      SAVE /BUFFERS1/
      SAVE /FILES1/
      SAVE /UNITS1/
      SAVE /PARMS1/
      SAVE /POLY1/
      SAVE /GAUS1/
      SAVE /LIPLOT1/
      SAVE /LIPLOT2/
      SAVE /PSSVALS/
