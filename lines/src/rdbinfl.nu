****************************************
      SUBROUTINE RDBINFILE (PREVBFILE,BINFILE,BWANTED,BSPEC,ERROR)
****************************************
*     read a spectrum from a binary-format file
*
      IMPLICIT NONE
*
      CHARACTER PREVBFILE*(*)   ! input previous file name
      CHARACTER BINFILE*(*)     ! output file name
      CHARACTER CJUNK*8         ! local storage
      INTEGER   BWANTED         ! input wanted spectrum in file
      INTEGER   BSPEC           ! output number of current spectrum in file
      INTEGER   IREC            ! output record number
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file i/o status
      INTEGER   NCHAR           ! external function
*
      INCLUDE 'lines.common'
*
      IF (BINFILE .EQ. PREVBFILE) THEN
          PRINT *,'file is open'
          IF (BWANTED .LE. BSPEC) THEN
              PRINT *,'rewind file'
              REWIND (88)
              BSPEC = 0
              IREC = 0
          END IF
      ELSE
          IF (NCHAR(PREVBFILE) .GT. 1) THEN
              CLOSE (88,IOSTAT=IOS,ERR=50)
          END IF
   50     PRINT *,'opening ',BINFILE
          OPEN (88,FILE=BINFILE,STATUS='OLD',ERR=910,IOSTAT=IOS)
          BSPEC = 0
          IREC = 0
      END IF
      PREVBFILE = BINFILE
        
      DO WHILE (BSPEC .LE. BWANTED-1)
          IREC = IREC + 1
          BSPEC = (IREC-1)/2 + 1
          READ (88,ERR=920,IOSTAT=IOS,END=950) CJUNK
          IF (IREC .EQ. 1) THEN
              IF (CJUNK(1:6) .EQ. 'SIMPLE') THEN
                  PRINT *,'this looks like a FITS file!'
                  RETURN
              ELSE IF (CJUNK(1:5) .EQ. 'BEGIN') THEN
                  PRINT *,'this looks like an ASCII file!'
                  RETURN
              END IF
          END IF
      END DO
*
      IREC = IREC + 1
      BSPEC = BSPEC + 1
  100 PRINT *,'reading rec ',IREC,' = housekeeping ',BSPEC
      READ (88,ERR=920,IOSTAT=IOS,END=950)
     & OBJECT, TELESCOP, OBSERVER, DATE_OBS, DATE, BUNIT, CTYPE1,
     & NAXIS, NAXIS1, CRPIX1, POL, SCAN, FIRST_CH, LAST_CH,
     & POLYFIT, FOLDED, SMOOTHED, 
     & CRVAL1, CDELT1, RESTFREQ, RA, DEC, 
     & RADOWN, DECDOWN, EQUINOX, JULDATE, HA,
     & TSYS, DTSYS, DUR, RMS, PSS, ADDED, BW, FROFFSET
*
      IREC = IREC + 1
      PRINT *,'reading rec ',IREC,' = spectrum ',BSPEC
      READ (88,ERR=920,IOSTAT=IOS) (SPECTRUM(I), I = 1, NAXIS1)
*
      CLOSE (88,ERR=940,IOSTAT=IOS)
      ERROR = 0
      RETURN
*
  910 CALL OPEN_ERROR (BINFILE,IOS)
      ERROR = 1
      RETURN
*
  920 CALL READ_ERROR (BINFILE,IOS,IREC)
      ERROR = 2
      RETURN
*
  940 CALL CLOSE_ERROR (BINFILE,IOS)
      ERROR = 4
      RETURN
*
  950 CALL EOF_ERROR (BINFILE,BSPEC-1)
      ERROR = 5
      RETURN
*
      END
***************************
