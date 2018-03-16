*************************
      SUBROUTINE CMDSELCT (ERROR, QUIT)
*************************
*     select command to execute
*
*     called by:
*     lines
*
*     subroutines called: many
*
      IMPLICIT  NONE
*
      LOGICAL   QUIT            ! output true to quit
      INTEGER   ERROR           ! returned file error code
      INTEGER   IOS             ! i/o error status
      INTEGER   LEN             ! buffer length
      INTEGER   NCHAR           ! external local function
*
      INCLUDE 'lines.inc'
*
*
      IF (DB) PRINT *,'in CMDSELCT'
      CALL UPCASE (CMDP(1))
      IF (DB) PRINT *,'CMDP(1)=',CMDP(1)(1:NCHAR(CMDP(1))),'!'
*
      IF (CMDP(1)(1:2) .EQ. 'AC') THEN
*         add constant to spectrum
          CALL MEMADD(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'AD') THEN
*         alter data 
          CALL ALTERDAT(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'AG') THEN
*         alter data graphically
          CALL ALTDATGR(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'AP') THEN
*         add two spectra taken at th esam etime with orthogonal polarization
          CALL ADPOL(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'ATM') THEN
*         correct atmospheric absorption using SLAP formula
          CALL ATMOS(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'AV') THEN
*         combine spectra into average spectrum
          CALL MEMAV(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'CLR') THEN
*         clear a memory
          CALL MEMCLR(ERROR)
*         memclr does not normally report back, so
          IF (ERROR .EQ. 0) THEN
              WRITE (BUF,*) 'cleared mem ',CMDP(2)
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          END IF
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'CMB') THEN
*         combine two bandpasses from NCCS to make the spectrum
          CALL NCOMBINE(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'CP') THEN
*         copy data from one memory to another
          CALL MEMCOPY(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'DB') THEN
          DB = .NOT. DB
          PRINT '(A,$)',' debug o'
          IF (DB) THEN
              PRINT '(A)','n'
          ELSE
              PRINT '(A)','ff'
          END IF
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'DO') THEN
*         start of do loop - get parameters for the loop
          CALL LOOPPARM
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'END') THEN
*         end of do loop commands - set up for executing loop
          GETLOOPCMD = .FALSE.
          DOLOOP = .TRUE.
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'FD') THEN
*         fix the amplitude of the "down" half of frequency-switched spectra
*         observed prior to 1990 d 338 
          CALL FIXDOWN(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'FT') THEN
*         Fourier transform the data 
          CALL LFFT4(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'FXJ') THEN
*         Fix non-monotonic jumps in X values in the data 
          CALL FXJ(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'FXM') THEN
*         Fix non-monotonic jumps in X values in the data using median filter
          CALL FXJMED(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'GC ') THEN
*         correct the gain using correction curve coefficients or manual entry
          CALL GCCORR(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'GCP') THEN
*         enter gain correction curve polynomial coefficients
          CALL GCPARM(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'GF') THEN
*         fit multiple gaussian profiles to lines
          CALL MGFIT(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'GP') THEN
*         enter gaussian fit parameters
          CALL MGPARM(ERROR)
*         this is needed to list the new parameters to the log file
          CALL UPCASE(CMDP(2))
          IF (WRITELOG .AND. 
     &        CMDP(2)(1:4) .NE. 'SHOW' .AND. 
     &        CMDP(2)(1:5) .NE. 'CLEAR') THEN
*             use the SHOW option to list the parameters
              CMDP(2) = 'SHOW'
              CALL MGPARM(ERROR)
          END IF
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'GW') THEN
*         write out data and fitted gaussian profiles
          CALL WRTGSFL(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'HE' .OR.
     &         CMDP(1)(1:1) .EQ. '?') THEN
*         list help on screen
          CALL HELPLESS(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'HM') THEN
*         smooth the spectrum
          CALL HAMM(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'INP') THEN
*         further input from commands in disc file
          CALL CMDFLOPN(ERROR) 
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'INT') THEN
*         calculate the line integral
          CALL LINTEG(ERROR) 
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'JDC') THEN
*         subtract gain as function of MJD from data 
          CALL JDCORR(ERROR) 
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'JDP') THEN
*         enter polynomial coeffs of gain vs MJD 
          CALL JDPARM(ERROR) 
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'JY') THEN
*         convert K to Jy or change Jy value
          CALL KTOJY(ERROR)
*
*
      ELSE IF (CMDP(1)(1:6) .EQ. '1720JY') THEN
*         convert K to Jy for 1720 MHz methanol or change Jy value
          CALL KTOJY1720(ERROR)
*
*
      ELSE IF (CMDP(1)(1:6) .EQ. '5000JY') THEN
*         convert K to Jy for 5000 MHz methanol or change Jy value
          CALL KTOJY5000(ERROR)
*
*
      ELSE IF (CMDP(1)(1:6) .EQ. '6668JY') THEN
*         convert K to Jy for 6668 MHz methanol or change Jy value
          CALL KTOJY6668(ERROR)
*
*
      ELSE IF (CMDP(1)(1:7) .EQ. '12178JY') THEN
*         convert K to Jy for 12178 MHz methanol or change Jy value
          CALL KTOJY12178(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'LA') THEN
*         list an ascii format file, one line per spectrum, for autoprocessing
          CALL LISTASCA(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'LD') THEN
*         list the data in ascii to screen
          CALL LISTDAT 
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'LF') THEN
*         list an ascii format file at one line per spectrum
          CALL LISTASCF(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'LH') THEN
*         list the housekeeping to the screen
          CALL LISTHKP 
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'LM') THEN
*         summarise the contents of each memory to the screen
          CALL LISTMEM(ERROR) 
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'LOG') THEN
          CALL LOGFOPEN(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'LS') THEN
*         list the contents of the directory to screen
          CALL LISTDIR
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'MC') THEN
*         multiply spectrum in memory by a constant
          CALL MEMMULT(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'MF') THEN
*         apply a median filter to data Y values
          CALL MEDIANFILT(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'MN') THEN
*         calculate mean and std dev of data in specified range
          CALL MEAN (ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'MV') THEN
*         copy data from one memory to another
          CALL MEMCOPY(ERROR)
*         clear source memory
          PRINT *,'clear mem ',CMDP(2)(1:NCHAR(CMDP(2)))
          CALL MEMCLR(ERROR)
*
*
      ELSE IF (CMDP(1)(1:7) .EQ. 'NFCLOSE') THEN
*         close 'nextfile' used by RNCSVFILE
          CLOSE (NFRDUNIT, IOSTAT=IOS, ERR=931)
  931     IF (IOS .GT. 0) THEN
              PRINT *,'error ',IOS,' closing nextfile' 
          ELSE
              PRINT *,'nextfile closed'
          END IF
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'OHJY') THEN
*         convert K to Jy or change Jy value in OH obs at 1612, 1665, 1667MHz
          CALL OHKTOJY(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'OP') THEN
*         add subtract multiply or divide data in two memories
          CALL MEMOP(ERROR)
*
*
      ELSE IF (CMDP(1)(1:5) .EQ. 'PAUSE') THEN
*         wait for user to press Enter key
          CALL PAUSE(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'PB') THEN
*         set start and end velocities for polynomial fit to baseline
          CALL POBLOCK(ERROR)
*         this is needed to list the new parameters to the log file
          CALL UPCASE(CMDP(2))
          IF (WRITELOG .AND. 
     &        CMDP(2)(1:4) .NE. 'SHOW' .AND. 
     &        CMDP(2)(1:5) .NE. 'CLEAR') THEN
*             use the SHOW option to list the parameters
              NCMD = 2
              WRITE (CMDP(2),'(A)') 'SHOW'
              CALL POBLOCK(ERROR)
          END IF
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'PC ') THEN
*         calculate pointing offsets in orthogonal directions independently
          CALL OFFSET(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'PC2') THEN
*         calculate pointing offsets using 2-D gaussian fit
          CALL GAUSFIT2DS(ERROR)
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'PCA ') THEN
*         calculate pointing offsets automatically using 1D gaussian fit
          CALL PCA(ERROR)
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'PCA2') THEN
*         calculate pointing offsets automatically using 2D gaussian fit
          CALL PCA2(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'PGC') THEN
*         use cursor to mark positions on last plot
          CALL PLCURSPS(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'PL') THEN
*         call plconfau, pgselct, plotdata      
          CALL PLSETUP(ERROR)
          IF (ERROR .GT. 0) GO TO 900
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'PO') THEN
*         fit a polynomial to the spectrum
          CALL LIPOLY(ERROR)
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'PSSF') THEN
*         read a file containing JD, PSS at JD 
          CALL PSSF(ERROR)
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'PSS ') THEN
*         set PSS to that of JD read using PSSF
          CALL PSSSET(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'PTX') THEN
*         add text / labels to a plot
*
          IF (IDPLOT1 .GT. 0) THEN
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT 1000, PLOTDEV
              CALL PGSLCT(IDPLOT1)
              NTXT = 0
              CALL PLTEXT
          ELSE
              PRINT 1001
          END IF
*
*         plot to postscript file
*
          IF (PLOTPS(1:3) .NE. 'off' .AND. PLOTPS(1:3) .NE. 'OFF') THEN
              IF (IDPLOT2 .GT. 0) THEN
*                 PLOTPS has been opened - select as the output device
                  PRINT 1000, PLOTPS(1:NCHAR(PLOTPS))
                  CALL PGSLCT(IDPLOT2)
                  CALL PLTEXT
              END IF
          ELSE
              PRINT 1002
          END IF
*
*
          ELSE IF (CMDP(1)(1:4) .EQ. 'QUIT')  THEN
          CALL PGQINF('state',BUF,LEN)
          IF (DB) PRINT *,'PGQINF state =',BUF(1:NCHAR(BUF))
          IF (INDEX(BUF,'OPEN') .NE. 0) THEN
*             shut down PGPLOT cleanly
              CALL PGCLOS
          END IF
          PRINT *,'quit'
              QUIT = .TRUE.
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'RA ') THEN
*         read spectrum from an ascii format file
          CALL ASCFILIN(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'RAV') THEN
*         read spectrum from an ascii format file
          CALL ASCFILAV(ERROR)
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'RMCA') THEN
*         read spectrum from an ascii format file
          CALL RDMCAFL(ERROR)
*
*
      ELSE IF (CMDP(1)(1:5) .EQ. 'RNCSV') THEN
*         read spectrum from an nccs csv format file
          CALL RDNCCSCSV(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'RMS') THEN
*         calculate theoretical rms noise in spectrum
              CALL ARMS(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'SET') THEN
*         list or set program parameters
              CALL SETPARM(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'SF') THEN
*         shift and fold a frequency-shifted spectrum
              CALL FOLD(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'SM') THEN
*         smooth data with a running window 
              CALL SMOOTH(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'SPL') THEN
*         fit splines thru the data
              CALL LISPLINE(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'SY') THEN
*         pass command to operating system
              CALL SYSCMD 
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'TC') THEN
*         set continuum antenna temperature and error
              CALL SETTCONT(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'TMV') THEN
*         set velocity ranges for writing a times series file
              CALL TMBLOCK(ERROR)
*         this is needed to list the new parameters to the log file
          CALL UPCASE(CMDP(2))
          IF (WRITELOG .AND. 
     &        CMDP(2)(1:4) .NE. 'SHOW' .AND. 
     &        CMDP(2)(1:5) .NE. 'CLEAR') THEN
*             use the SHOW option to list the parameters
              CMDP(2) = 'SHOW'
              NCMD = 2
              CALL TMBLOCK(ERROR)
          END IF
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'TMW') THEN
*         writing a times series file
              CALL WRTTMFL(ERROR)
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'TS  ') THEN
*         correct the on-source system temperature and error
              CALL TSYSCH(ERROR)
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'TSHA') THEN
*         correct the OHIR Tsys from end to middle of obs
              CALL TSMIDHA(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'VH') THEN
*         get line height(s) at specified velocity
              CALL VH(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'VW') THEN
*         set velocity limits
              CALL VELWIN(ERROR)
*
*
      ELSE IF (CMDP(1)(1:2) .EQ. 'WA') THEN
*         write spectrum to ascii format file
          CALL WRTASCFL(ERROR)
*
*
      ELSE IF (CMDP(1)(1:4) .EQ. 'WMCA') THEN
*         write spectrum to multi-column ascii format file
          CALL WRTMCAFL(ERROR)
*
*
      ELSE IF (CMDP(1)(1:3) .EQ. 'XED') THEN
*         correct X(I) values that are not monotonic
          CALL XEDIT(ERROR)
*
*             
      ELSE 
*
*         unidentified command - list help on screen
          PRINT *,'?? ',CMD(1:NCHAR(CMD))
*
      END IF
*
  900 RETURN
 1000 FORMAT (' plot to ',A)
 1001 FORMAT (' no screen plot device - set plotdev')
 1002 FORMAT (' no postscript output file - set plotps')
      END
*********
