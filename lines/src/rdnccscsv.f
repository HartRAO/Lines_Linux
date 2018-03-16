************************
      SUBROUTINE RDNCCSCSV (ERROR)
************************
*     read ascii spectrum file
*
*     called by:
*
*     other subroutines called:
*     nchar, er_open, er_read, linfit, memclr, memcopy, stfromjd
*     b50toj20, j2000pr
*
      IMPLICIT  NONE
*
      CHARACTER CTYPE3*30       ! local axis label of 2nd pol
      CHARACTER BINTABLE*8      ! local binary table name
      CHARACTER NEXTFILE*8      ! local nextfile name
      INTEGER   ERROR           ! output file error code
      INTEGER   I               ! local loop index
      INTEGER   IC              ! local loop character count
      INTEGER   ICOL            ! local number of columns of data
      INTEGER   IOS             ! local file status
      INTEGER   IROW            ! local number or rows of data
      INTEGER   MEM             ! local memory in use
      INTEGER   MEMFIT          ! output memory for LINFIT to use
      INTEGER   NCHAR           ! external function
      INTEGER   NFREC           ! record in nextfile
      LOGICAL   EXISTS          ! local file existence status
      LOGICAL   END_OF_SPECTRUM ! local end of spectrum encountered
      LOGICAL   LABELS_READ     ! local column headers have been read
      LOGICAL   OPEN            ! local nextfile open status
      LOGICAL   READ_DONE       ! local end of data encountered
      REAL*8    ALTRAD          ! local altitude (radians)
      REAL*8    AZRAD           ! local azimuth (radians)
      REAL*8    DECPRC          ! local dec at date of obs (deg)
      REAL*8    DEC2000         ! local dec in J2000 (deg)
      REAL*8    DTR             ! local degrees to radians
      REAL*8    RAPRC           ! local ra at date of obs (deg)
      REAL*8    RA2000          ! local ra in J2000 (deg)
      REAL*8    STFROMJD        ! external function
      REAL*8    STMID           ! siderial time at middle of obs (s)
      REAL*8    TCAL2           ! local 2nd pol cal diode temp (K)
      REAL*8    TCER2           ! local 2nd pol cal diode temp err (K)
      REAL*8    TSYS2           ! local 2nd pol system temp (K)
      REAL*8    DTSYS2          ! local 2nd pol system temp err (K)
      REAL*8    CONSTANT        ! from linfit lin reg fit constant
      REAL*8    RMSDEV          ! from linfit lin reg fit rms deviation
      REAL*8    SLOPE           ! from linfit lin reg fit slope
*      
      INCLUDE 'lines.inc'
*
      SAVE     NFREC
      DATA     NEXTFILE /'nextfile'/
      DATA DTR / 0.017453293D0/

*
      IF (DB) PRINT *,'in RDNCCSCSV'
      ERROR = 0

      IF (DB) PRINT *, 'NCMD = ',NCMD
      IF (DB) THEN
          DO I = 1, NCMD
              PRINT *, 'CMDP',I,' = ',CMDP(I)
          END DO
      END IF
*
      IF (NCMD .GE. 2) THEN
          NCSVRDFILE = CMDP(2)
      ELSE
          PRINT '(3A,$)',' NCCS CSV spectra file to read from ? '
          READ  '(A)', NCSVRDFILE
      END IF
*
*     option to read file names sequentially from file 'nextfile'
*
      IF (NCSVRDFILE(1:8) .EQ. NEXTFILE(1:8)) THEN
          INQUIRE (FILE=NEXTFILE,EXIST=EXISTS,OPENED=OPEN)
          IF (.NOT. EXISTS) THEN
              PRINT *,NEXTFILE,' not found'
              RETURN
          END IF
          IF (EXISTS .AND. .NOT. OPEN) THEN
              OPEN (NFRDUNIT,FILE=NEXTFILE,STATUS='OLD',
     &            ACCESS='SEQUENTIAL',IOSTAT=IOS,ERR=90)
   90         IF (IOS .GT. 0) THEN
*                 Error opening file - set error flag and exit
                  CALL ER_OPEN (NEXTFILE,IOS)
                  RETURN
              END IF
              PRINT *,'Opened ',NEXTFILE
              NFREC = 0
          END IF
          NFREC = NFREC + 1
          READ (NFRDUNIT,'(A)',END=940,ERR=930,IOSTAT=IOS) NCSVRDFILE
      END IF
*
      INQUIRE (FILE=NCSVRDFILE,EXIST=EXISTS)
*
      IF (.NOT. EXISTS) THEN
          CALL ER_EXIST (NCSVRDFILE)
          ERROR = 1
          RETURN
      END IF
*
*     third parameter is the destination memory
*
      MEM = MEMSET
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=181) MEM
  181     IF (IOS .NE. 0) THEN
              PRINT *, 'illegal memory',CMDP(3)(1:NCHAR(CMDP(3)))
              MEM = 0
          END IF
      END IF
      IF (MEM .EQ. 0) THEN
          PRINT '(A,$)',' store in which memory ? '
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=182) MEM
  182     IF (IOS .NE. 0 .OR. MEM .LT. 1 .OR. MEM .GT. MAXMEM .OR.
     &        MEM .EQ. MEMRD) MEM = MEMSET
      END IF    
      IF (DB) PRINT *,'mem=',MEM
*
      WRITE (LBUF,*) 'read NCCS csv file ',
     &     NCSVRDFILE(1:NCHAR(NCSVRDFILE)),
     &    ' into mem',MEM
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
*
*     clear the memory being read into completely
*     preset non-essential housekeeping in case not present
*
      WRITE (CMDP(2),*,IOSTAT=IOS,ERR=185) MEMRD
  185 IF (NCMD .LT. 2) NCMD = 2
      CALL MEMCLR (ERROR)
*
*     set up to read from the NCCS csv file
*
      ASCLINE = 0
      IROW = 0
      END_OF_SPECTRUM  = .FALSE.
      LABELS_READ      = .FALSE.
      READ_DONE        = .FALSE.
*
*     preset optional housekeeping to default values:
      IXSPACING(MEMRD) = 0             ! X values are regularly spaced
      ATMCORR(MEMRD)  = 1.0
      GAINCORR(MEMRD) = 1.0
      PNTCORR(MEMRD)  = 1.0
      COORDSYS(MEMRD) = 'UNKNOWN'
*
*     open the nccs csv file
*
      OPEN (NCSVRDUNIT,FILE=NCSVRDFILE,STATUS='OLD',
     &      ACCESS='SEQUENTIAL',IOSTAT=IOS,ERR=190)
*
*     Error opening file - set error flag and exit
  190 IF (IOS .GT. 0) THEN
          CALL ER_OPEN (NCSVRDFILE,IOS)
          RETURN
      END IF
*
*     read line by line from the file and decode

      DO WHILE (.NOT. READ_DONE)
          IF (DB .AND. ASCLINE .EQ. 0) PRINT *,'in read DO loop'
*
*         read a line from the file
*
          ASCLINE = ASCLINE + 1
          READ (NCSVRDUNIT,'(A)',END=250,ERR=920,IOSTAT=IOS) BUF
          IF (DB) PRINT *,'line ',ASCLINE,'=',BUF(1:NCHAR(BUF))
*
*         what sort of file is this?
*
          IF (ASCLINE .EQ. 1) THEN
              IF (BUF(1:6) .EQ. 'SIMPLE') THEN
                  PRINT *,'FITS file - exit!'
                  RETURN
              ELSE IF (BUF(1:5) .EQ. 'BEGIN') THEN
                  PRINT *,'ASCII spectra file - exit!'
                  RETURN
              ELSE IF (BUF(1:4) .EQ. 'DATE') THEN
                  PRINT *,'NCCS csv spectra file'
              ELSE 
                  PRINT *,'file format unknown - exit'
                  RETURN
              END IF
          END IF
* 
          IF (BUF(21:23) .EQ. 'MJD') THEN
*               at start of bintable chart data - not wanted here
                READ_DONE = .TRUE.
C                GO TO 250
          END IF          
          
*
          IF (BUF(1:1) .GT. 'A' .AND. BUF(1:1) .LE. 'Z') THEN
*
C              IF (DB) PRINT '(I5,A$)',ASCLINE, ':'
C              IF (DB) PRINT *,BUF(1:NCHAR(BUF))
*
*             read FITS-style housekeeping keyword and its value
*
              IF      (BUF(1:8) .EQ. 'DATE    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DATE(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'DATE_OBS') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DATE_OBS(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'OBJECT  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) OBJECT(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'COORDSYS') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) COORDSYS(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TELESCOP') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TELESCOP(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'LONGITUD') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) RA(MEMRD)
                  print *,'RA=',RA(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'LATITUDE') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DEC(MEMRD)
                  PRINT *,'DEC=',DEC(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'EQUINOX ') THEN
                  IF (BUF(11:11) .EQ. '''') THEN
*                     equinox was written as ascii string instead of real
                      PRINT *,'ASCII EQUINOX'
                      READ (BUF(12:19),*,IOSTAT=IOS,ERR=920) 
     &                     EQUINOX(MEMRD)
                  ELSE
*                     equinox was written correctly as a real
                      READ (BUF(11:),*,IOSTAT=IOS,ERR=920) 
     &                    EQUINOX(MEMRD)
                  END IF
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'OBSERVER') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) OBSERVER(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'SPVLSR  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) SPVLSR(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'SPBW    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) BW(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'SPFS    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) FROFFSET(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'SPTIME  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DUR(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'RESTFREQ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) RESTFREQ(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'POSITION') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) POSITION(MEMRD)
                  IF (DB) PRINT *,'POSITION=',POSITION(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCAL1   ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCAL(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCAL2   ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCAL2
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCALSIG1') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCER(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCALSIG2') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCER2
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'BINTABLE') THEN
                  READ (BUF(11:),'(A)',IOSTAT=IOS,ERR=920) BINTABLE
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TUNIT1  ' .AND. 
     &                 BINTABLE(1:7) .EQ. 'Spectra') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TUNIT1(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TUNIT2  ' .AND.
     &                 BINTABLE(1:7) .EQ. 'Spectra') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) BUNIT(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCALUNIT') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCALUNIT(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TSYS1   ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TSYS(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TSYS2   ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TSYS2
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TSYSERR1') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DTSYS(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TSYSERR2') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DTSYS2
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'PSS     ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) PSS(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'ATMCORR ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) ATMCORR(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'GAINCORR') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) GAINCORR(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'PNTCORR ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) PNTCORR(MEMRD)
                  GO TO 301
              ELSE 
                  PRINT *,'unknown keyword: ',BUF(1:NCHAR(BUF))
              END IF
  301         CONTINUE
          END IF
*
          IF (LABELS_READ .AND. .NOT. END_OF_SPECTRUM) THEN
*
*             read the spectra as X, Y1, Y2
*
              IF (ICOL .EQ. 2) THEN 
                  READ (BUF,*,IOSTAT=IOS,ERR=320,END=320) 
     &                XIN(MEMRD,IROW+1),YIN(MEMRD,IROW+1)
              ELSE IF (ICOL .EQ. 3) THEN
                  READ (BUF,*,IOSTAT=IOS,ERR=320,END=320) 
     &                XIN(MEMRD,IROW+1),YIN(MEMRD,IROW+1),YIN2(IROW+1)
              END IF
*
  320         IF (DB .AND. IOS .NE. 0) PRINT *,'data read: IOS=',IOS
              IF (IOS .EQ. 0) THEN
                  IROW = IROW + 1
              ELSE 
                  END_OF_SPECTRUM = .TRUE.
                  PRINT *,'End of spectra data reached'
              END IF
*
              IF (IROW .EQ. MAXDATA) THEN
                  END_OF_SPECTRUM = .TRUE.
                  PRINT *,'IROW = MAXDATA - stop reading data'
              END IF
          END IF
*
          IF (BUF(11:14) .EQ. 'Vlsr') THEN
*
*             current line is column headers for spectra data
*             get number of columns of data - 3 if two pols present
              ICOL = 1
              DO IC = 1, BUFLEN
                  IF (BUF(IC:IC) .EQ. ',') ICOL = ICOL + 1
              END DO
              IF (DB) PRINT *,ICOL,' columns of data'
*              
*             get axis labels in comma delimited data
*
              CALL COMMALBL (DB, BUF, 1, CTYPE1(MEMRD))
              CALL COMMALBL (DB, BUF, 2, CTYPE2(MEMRD))
              IF (ICOL .EQ. 3) CALL COMMALBL (DB, BUF, 3, CTYPE3)
              LABELS_READ = .TRUE.
          END IF
      END DO
*
*     end of file read
*
  250 CLOSE (NCSVRDUNIT, IOSTAT=IOS, ERR=252)
  252 WRITE (BUF,*) IROW,' valid values were read '
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*     bail out if no data were read
*
      IF (IROW .EQ. 0) RETURN
*
*     set up housekeeping
*
*     if DATE_OBS is missing, copy DATE to DATE_OBS instead
      IF (DATE_OBS(MEMRD)(1:4) .EQ. '    ') THEN
          DATE_OBS(MEMRD) = DATE(MEMRD)
      END IF
*
      IF (JULDATE(MEMRD) .EQ. 0D0) CALL DATE_OBS2JD (MEMRD)
*
      FROMFILE(MEMRD) = NCSVRDFILE
      IF (TCALUNIT(MEMRD)(1:1) .EQ. ' ') THEN
*         TCALUNIT not found in keywords          
          TCALUNIT(MEMRD) = BUNIT(MEMRD)
      END IF
*     store one data array per memory
      NAXIS(MEMRD) = 1          
      NAXIS1(MEMRD) = IROW
      FIRST_CH(MEMRD) = 1
      LAST_CH(MEMRD) = NAXIS1(MEMRD)
*
      PRINT *,'COORDSYS=',COORDSYS(MEMRD)
      IF (COORDSYS(MEMRD)(1:8) .EQ. 'GALACTIC') THEN
          EQUINOX(MEMRD) = 0.0
          GLII(MEMRD) = RA(MEMRD)
          GBII(MEMRD) = DEC(MEMRD)
          RA(MEMRD) = 0.0
          DEC(MEMRD) = 0.0
      END IF
      PRINT *,'EQUINOX=',EQUINOX(MEMRD)
*     see what coordinates are present and do conversions as needed
*      MEM = MEMRD
      CALL COORDCNV (MEMRD,ERROR)
*
*     Get ST in seconds at start of obs from julian date
*     add half the scan duration in seconds and convert to degrees
      STMID = (STFROMJD(MEMRD) + DUR(MEMRD)/2)/240
      IF (DB) PRINT *,'STMID=',STMID,' deg'
*
*     if coordinates are epoch B1950, first convert to J2000
      IF (EQUINOX(MEMRD) .EQ. 1950.0) THEN
          IF (DB) PRINT *,'B1950 -> J2000'
          CALL B50TOJ20 (RA(MEMRD), DEC(MEMRD), RA2000, DEC2000)
      ELSE
          RA2000 = RA(MEMRD)
          DEC2000 = DEC(MEMRD)
      END IF
      IF (DB) PRINT *,'J2000 RA, Dec=',RA2000,DEC2000
*
*     Precess coordinates from J2000 to date of observation
      IF (DB) PRINT *,'J2000 -> dateobs'
      CALL J2000PR (RA2000, DEC2000, JULDATE(MEMRD), RAPRC, DECPRC)
      IF (DB) PRINT *,'at JD',JULDATE(MEMRD),' RA, Dec =',RAPRC,DECPRC
*
*     HA = ST(at middle or end) - RA(precessed)
      HA(MEMRD) = STMID - RAPRC
      IF (HA(MEMRD) .LT. -180.0) HA(MEMRD) = HA(MEMRD) + 360.0
      IF (HA(MEMRD) .GT. +180.0) HA(MEMRD) = HA(MEMRD) - 360.0
      IF (DB) PRINT *,'STMID=',STMID,'deg, Mid HA =',HA(MEMRD),'deg'
*
*     check telescope latitude is correct
      IF (TELESCOP(MEMRD)(1:4) .NE. 'Hart' .AND.
     &    ABS(TELLAT + 25.887) .LT. 0.1) THEN
          WRITE (BUF,*) 'warning: TELLAT=',TELLAT,
     &                  ' but TELESCOP=',TELESCOP(MEM)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
*     get elevation of antenna at middle of obs
      IF (DB) PRINT *,'HA,Dec->Az,Alt'
      CALL AZALTG (TELLAT*DTR,0D0,HA(MEMRD)*DTR,DECPRC*DTR,AZRAD,ALTRAD)
*
      ALTDEG(MEMRD) = ALTRAD / DTR 
*
      IF (DB) THEN
          PRINT *, 'altdeg = ',ALTDEG(MEMRD)
      END IF
*
*     do linear regression thru X values to check linearity using fit rms
*
      MEMFIT = MEMRD
      CALL LINFIT (MEMFIT,SLOPE,CONSTANT,RMSDEV)
*
      ADDED(MEMRD)  = 1
      CRPIX1(MEMRD) = 1
      CDELT1(MEMRD) = SLOPE
      CRVAL1(MEMRD) = CONSTANT + SLOPE
*
      WRITE (BUF,*) 'data spacing check: rmsdev from fit = ',RMSDEV
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      IF (RMSDEV .GT. 1D-3) THEN
          WRITE (BUF,*) 'data are IRREGULARLY spaced'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      ELSE 
          WRITE (BUF,*) 'data are REGULARLY spaced'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
*     fix spike at channel 513 in 1025 point spectrum
*
      IF (NAXIS1(MEMRD) .EQ. 1025) THEN
          PRINT *,'fix spike in channel 513'
          YIN(MEMRD,513) = (YIN(MEMRD,512) + YIN(MEMRD,514)) / 2.0
          YIN2(513) = (YIN2(512) + YIN2(514)) / 2.0
      ELSE
          PRINT *,'despike FAILED: NAXIS1 = ', NAXIS1(MEMRD)
      END IF
*
*     transfer data to destination memory
*
      PRINT *,'for ',CTYPE2(MEMRD)(1:NCHAR(CTYPE2(MEMRD))),' :'
      WRITE (CMDP(2),*) MEMRD
      WRITE (CMDP(3),*) MEM
      NCMD = 3
      CALL MEMCOPY (ERROR)
*
*     if 2 polarizations, create data set for second pol in new mem
*
      IF (ICOL .GE. 3 .AND. MEM+1 .LE. MAXMEM) THEN
*
*         transfer 2nd column of Y data to destination memory + 1
*
          PRINT *,'for ',CTYPE3(1:NCHAR(CTYPE3)),' :'
          WRITE (CMDP(2),*) MEMRD
          WRITE (CMDP(3),*) MEM+1
          NCMD = 3
          CALL MEMCOPY (ERROR)
*
*         fix up differences in the housekeeping and data
*
          TCAL(MEM+1) = TCAL2
          TCER(MEM+1) = TCER2
          TSYS(MEM+1) = TSYS2
          DTSYS(MEM+1) = DTSYS2   ! bugfix - added 070724
          CTYPE2(MEM+1) = CTYPE3
          DO I = 1, NAXIS1(MEM+1)
              YIN(MEM+1,I) = YIN2(I)
          END DO
*
      END IF
*
      RETURN
*
c  910 CALL ER_OPEN (NCSVRDFILE, IOS)
c      ERROR = 1
c      RETURN
*
  920 CALL ER_READ (NCSVRDFILE, IOS, ASCLINE)
      ERROR = 2
      RETURN
*
*     error reading from nextfile
  930 CALL ER_READ (NEXTFILE, IOS, NFREC)
      ERROR = 3
      RETURN
*
*     reached end of 'nextfile'
  940 PRINT *,'Reached end of ',NEXTFILE,' at record ',NFREC
      CLOSE (NFRDUNIT, IOSTAT=IOS, ERR=941)
  941 ERROR = 4
      RETURN     
*
      END
*********