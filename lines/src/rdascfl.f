************************
      SUBROUTINE RDASCFL (ERROR)
************************
*     read ascii spectrum file
*
*     called by:
*     ascfilin, ascfilav
*
*     other subroutines called:
*     altcalc, nchar, coordcnv, er_open, er_read, er_close, er_eof
*
      IMPLICIT  NONE
*
      INTEGER   ERROR           ! output file error code
      INTEGER   I               ! local loop index
      INTEGER   IDIN            ! local day read in
      INTEGER   ISTART          ! local start array index for read
      INTEGER   IEND            ! local end array index for read
      INTEGER   IMIN            ! local month read in
      INTEGER   IOS             ! local file status
      INTEGER   IYRIN            ! local year read in, for 4 digit check
      INTEGER   MEM             ! local memory in use
      INTEGER   NL              ! local line number of data in file
      INTEGER   NCHAR           ! external function
      INTEGER   NPL             ! local number of data per line
      INTEGER   PCHAR           ! local no. characters in PREVAFILE
      LOGICAL   BEGIN_HEADER    ! local beginning of header encountered
      LOGICAL   END_HEADER      ! local end of header encountered
      LOGICAL   READ_DONE       ! local end of data encountered
      LOGICAL   OPENF           ! local file opened status
*      
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in RDASCFL'
      ERROR = 0
      IF (DB) PRINT *,'wanted ascii spectrum =',ASCWANTED,
     &                ' at ',ASCAT
*
      PCHAR = NCHAR(PREVAFILE)
      INQUIRE (FILE=ASCRDFILE,OPENED=OPENF)
      IF (DB .AND. OPENF) PRINT *,'file is open'
      IF (ASCRDFILE .EQ. PREVAFILE .AND. OPENF) THEN
          IF (ASCWANTED .LE. ASCAT) THEN
              IF (DB) PRINT *,'rewind file'
              REWIND (ASCRDUNIT)
              ASCLINE = 0
              ASCAT = 0
          END IF
      ELSE
          IF (PCHAR .GT. 2) THEN
              IF (DB) PRINT *,'NCHAR(PREVAFILE)=',PCHAR
              IF (DB) PRINT *,'closing ',PREVAFILE(1:PCHAR)
              CLOSE (ASCRDUNIT,IOSTAT=IOS,ERR=940)
          END IF
          PRINT '(2A,$)',' opening ',ASCRDFILE(1:NCHAR(ASCRDFILE))
          OPEN (ASCRDUNIT,FILE=ASCRDFILE,STATUS='OLD',
     &          ERR=910,IOSTAT=IOS)
          ASCLINE = 0
          ASCAT = 0
      END IF
      PREVAFILE = ASCRDFILE
*
      IF (DB) PRINT *,'line in file = ',ASCLINE,
     &        ' at spectrum: ',ASCAT,
     &        ' wanted: ',ASCWANTED
      NL = 0
      BEGIN_HEADER = .FALSE.
      END_HEADER = .FALSE.
      READ_DONE = .FALSE.
*     preset optional housekeeping to default values:
      IXSPACING(MEMRD) = 0        ! X values are regularly spaced
      ATMCORR(MEMRD)  = 1.0
      GAINCORR(MEMRD) = 1.0
      PNTCORR(MEMRD)  = 1.0
*
      DO WHILE (.NOT. READ_DONE)
*
          ASCLINE = ASCLINE + 1
          READ (ASCRDUNIT,'(A)',END=950,ERR=920,IOSTAT=IOS) BUF
C          IF (DB) PRINT *,BUF(1:NCHAR(BUF))
*
              IF (ASCLINE .EQ. 1) THEN
                  IF (BUF(1:6) .EQ. 'SIMPLE') THEN
                      PRINT *,'FITS file!'
                      RETURN
                  ELSE IF (BUF(1:5) .EQ. 'BEGIN') THEN
                      PRINT *,'ASCII spectra file'
                  ELSE 
                      PRINT *,'file format unknown'
                      RETURN
                  END IF
              END IF
*
          IF (.NOT. BEGIN_HEADER .AND. BUF(1:5) .EQ. 'BEGIN') THEN
              BEGIN_HEADER = .TRUE.
              END_HEADER = .FALSE.
              ASCAT = ASCAT + 1
              IF (DB) PRINT *,'spectrum ',ASCAT,' begin head'
              IF (ASCAT .EQ. ASCWANTED) 
     &            PRINT *,ASCRDFILE(1:NCHAR(ASCRDFILE)),
     &                    ' at spectrum',ASCAT
*
          ELSE IF (BEGIN_HEADER .AND. BUF(1:3) .EQ. 'END') THEN
              END_HEADER = .TRUE.
              IF (DB) PRINT *,'spectrum ',ASCAT,' end head'
*
          ELSE IF (BEGIN_HEADER .AND. 
     &             .NOT. END_HEADER .AND. 
     &             ASCAT .EQ. ASCWANTED) THEN
*
C             PRINT '(I5,A$)',ASCLINE, ':'
C             PRINT *,BUF(1:NCHAR(BUF))
*
*             read the housekeeping
              IF (BUF(1:8) .EQ. 'ADDED   ') THEN
                      READ (BUF(11:),*,IOSTAT=IOS,ERR=920) ADDED(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'ALTDEG  ') THEN
                      READ (BUF(11:),*,IOSTAT=IOS,ERR=920) ALTDEG(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'BUNIT   ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) BUNIT(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'BW      ') THEN
                      READ (BUF(11:),*,IOSTAT=IOS,ERR=920) BW(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'CDELT1  ') THEN
                      READ (BUF(11:),*,IOSTAT=IOS,ERR=920) CDELT1(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'CRPIX1  ') THEN
                      READ (BUF(11:),*,IOSTAT=IOS,ERR=920) CRPIX1(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'CRVAL1  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) CRVAL1(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'CTYPE1  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) CTYPE1(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'CTYPE2  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) CTYPE2(MEMRD)
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'DATE    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DATE(MEMRD)
                  IF (DATE(MEMRD)(3:3) .EQ. '/') THEN
*                     old format date, convert to Y2K FITS
                      READ (DATE(MEMRD),'(3(I2,1X))') IDIN,IMIN,IYRIN
                      IYRIN = IYRIN + 1900
                      WRITE (DATE(MEMRD),'(I4.4,A1,I2.2,A1,I2.2)')
     &                    IYRIN,'-',IMIN,'-',IDIN
                  END IF
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'DATE-OBS') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DATE_OBS(MEMRD)
                  IF (DATE_OBS(MEMRD)(3:3) .EQ. '/') THEN
*                     old format date, convert to Y2K FITS
                      READ (DATE_OBS(MEMRD),'(3(I2,1X))')IDIN,IMIN,IYRIN
                      IYRIN = IYRIN + 1900
                      WRITE (DATE_OBS(MEMRD),'(I4.4,A1,I2.2,A1,I2.2)')
     &                    IYRIN,'-',IMIN,'-',IDIN
                  END IF
                      GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'DEC     ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DEC(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'DECDOWN ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DECDOWN(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'DTSYS   ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DTSYS(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'DUR     ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) DUR(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'EQUINOX ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) EQUINOX(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'FIRST_CH') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) FIRST_CH(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'FOLDED  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) FOLDED(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'FROFFSET') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) FROFFSET(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'GBII    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) GBII(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'GBIIDOWN') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) GBIIDOWN(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'GLII    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) GLII(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'GLIIDOWN') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) GLIIDOWN(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'HA      ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) HA(MEMRD)
                  IF (HA(MEMRD) .GT. 180.0) HA(MEMRD) = HA(MEMRD)-360.0
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'JULDATE ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) JULDATE(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'LAST_CH ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) LAST_CH(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'NAXIS   ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) NAXIS(MEMRD)
                  IF (DB) PRINT *,'read header'
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'NAXIS1  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) NAXIS1(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'OBJECT  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) OBJECT(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'OBSERVER') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) OBSERVER(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'POL     ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) POL(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'POLYFIT ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) POLYFIT(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'POSITION') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) POSITION(MEMRD)
                  IF (DB) PRINT *,'POSITION=',POSITION(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'PSS     ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) PSS(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'RA      ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) RA(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'RADOWN  ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) RADOWN(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'RESTFREQ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) RESTFREQ(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'RMS     ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) RMS(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'SCAN    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) SCAN(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'SMOOTHED') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) SMOOTHED(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCAL    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCAL(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCALUNIT') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCALUNIT(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCONT   ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCONT(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TCER    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TCER(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TELESCOP') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TELESCOP(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TRANSFRM') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TRANSFRM(MEMRD)
                  GO TO 301
              ELSE IF (BUF(1:8) .EQ. 'TSYS    ') THEN
                  READ (BUF(11:),*,IOSTAT=IOS,ERR=920) TSYS(MEMRD)
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
*
          ELSE IF (BEGIN_HEADER .AND. END_HEADER .AND. 
     &        ASCAT .EQ. ASCWANTED) THEN
*                         read the spectrum
              NPL = 4
              NL = NL + 1
              ISTART = (NL-1) * NPL + 1
              IF (DB .AND. (ISTART .EQ. 1)) PRINT *,'read spectrum'
*
C             PRINT '(A,I5,A$)',' Line ',ASCLINE, ':'
C             PRINT *,BUF(1:NCHAR(BUF))
*
              IEND = ISTART + NPL - 1
              IF (IEND .GT. MAXDATA) THEN
                  PRINT *,'stop data read - more than MAXDATA = ',
     &                MAXDATA
                  READ_DONE = .TRUE.
              END IF
*
              IF (ISTART .GE. 1) THEN
                  READ (BUF,*,IOSTAT=IOS,ERR=920,END=950) 
     &                (YIN(MEMRD,I),I=ISTART,IEND)
              END IF
*
              IF (IEND .GE. NAXIS1(MEMRD)) THEN
                  READ_DONE = .TRUE.
                  IF (DB) PRINT *,'finished reading spectrum'
              END IF
*
          ELSE IF (BEGIN_HEADER .AND. END_HEADER .AND. 
     &        ASCAT .NE. ASCWANTED) THEN
              BEGIN_HEADER = .FALSE.
              END_HEADER = .FALSE.
          END IF
      END DO
*
*     save file info in housekeeping
      FROMFILE(MEMRD) = ASCRDFILE
      NUMFILE(MEMRD) = ASCWANTED
*     spectrum present unless transform explicitly present
      IF (TRANSFRM(MEMRD) .NE. 1) TRANSFRM(MEMRD) = 0
      IF (DB) PRINT *,'transform = ',TRANSFRM(MEMRD)
*
      IF (NCHAR(CTYPE2(MEMRD)) .LE. 1) THEN
*         add probable data label
          IF (BUNIT(MEMRD)(1:1) .EQ. 'K') THEN
              WRITE (CTYPE2(MEMRD),'(A)') 'ANTENNA_TEMP'
          ELSE IF (BUNIT(MEMRD)(1:2) .EQ. 'JY') THEN
              WRITE (CTYPE2(MEMRD),'(A)') 'FLUX_DENSITY'
          END IF
      END IF
*
*     see what coordinates are present and do conversions as needed
      MEM = MEMRD
      CALL COORDCNV (MEM,ERROR)
*
*     calculate the altitude/elevation of the telescope
      CALL ALTCALC (MEM,ERROR)
*
*     for evenly spaced data:
*     create XIN from CRPIX1, CDELT1, CRVAL1
      DO I = 1, NAXIS1(MEMRD)
          XIN(MEMRD,I) = CRVAL1(MEMRD) + 
     &                   (I-CRPIX1(MEMRD)) * CDELT1(MEMRD)
      END DO
*
      RETURN
*
  910 CALL ER_OPEN (ASCRDFILE, IOS)
      ERROR = 1
      RETURN
*
  920 CALL ER_READ (ASCRDFILE,IOS,ASCLINE)
      ERROR = 2
      RETURN
*
  940 CALL ER_CLOSE (PREVAFILE, IOS)
      ERROR = 4
      RETURN
*
  950 CALL ER_EOF (ASCRDFILE,ASCAT)
      ERROR = 5
      RETURN
*
      END
*********