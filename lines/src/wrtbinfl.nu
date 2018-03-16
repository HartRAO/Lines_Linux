********************************************
      SUBROUTINE WRTBINFILE (BINFILE, ERROR)
********************************************
*     write the spectrum to a binary file
*
      IMPLICIT NONE
*
      INCLUDE 'lines.common'
*
      CHARACTER BINFILE*(*)     ! output file name
      CHARACTER CJUNK*8         ! local storage
      INTEGER   IREC            ! local record number
      INTEGER   ERROR           ! local error status
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file i/o status
      INTEGER   NCHAR           ! external function
      LOGICAL   EXISTS          ! local file existence
*
      INQUIRE (FILE=BINFILE,EXIST=EXISTS)
      PRINT *,'opening ',BINFILE
      IF (EXISTS) THEN
          PRINT *,'file ',BINFILE(1:NCHAR(BINFILE)),' exists'
          OPEN (98,FILE=BINFILE,STATUS='OLD',ERR=910,IOSTAT=IOS,
     &          RECL=????,FORM='UNFORMATTED',ACCESS='DIRECT')
          IREC = 0
          DO WHILE (.TRUE.)
              IREC = IREC + 1
              READ (98,ERR=920,END=100) CJUNK
              IF (IREC .EQ. 1) THEN
                  IF (CJUNK(1:6) .EQ. 'SIMPLE') THEN
                      PRINT *,'this looks like a FITS file!'
                      ERROR = 6
                      RETURN
                  ELSE IF (CJUNK(1:5) .EQ. 'BEGIN') THEN
                      PRINT *,'this looks like an ASCII file!'
                      ERROR = 6
                      RETURN
                  END IF
              END IF
          END DO
      ELSE 
          OPEN (98,FILE=BINFILE,STATUS='NEW',ERR=910,IOSTAT=IOS
     &          RECL=????,FORM='UNFORMATTED',ACCESS='DIRECT')
          IREC = 1
      END IF    
*
  100 PRINT *,'writing rec ',IREC,' = housekeeping ',(IREC-1)/2+1
      WRITE (98,ERR=930,IOSTAT=IOS)
     & OBJECT, TELESCOP, OBSERVER, DATE_OBS, DATE, BUNIT, CTYPE1
      PRINT *,'writing rec ',IREC+1
      WRITE (98,ERR=930,IOSTAT=IOS)
     & NAXIS, NAXIS1, CRPIX1, POL, SCAN, FIRST_CH, LAST_CH,
     & POLYFIT, FOLDED, SMOOTHED, 
     & CRVAL1, CDELT1, RESTFREQ, RA, DEC, 
     & RADOWN, DECDOWN, EQUINOX, JULDATE, HA,
     & TSYS, DTSYS, DUR, RMS, PSS, ADDED, BW, FROFFSET
*
      PRINT *,'writing rec ',IREC+1,' = spectrum ',(IREC-1)/2+1
      WRITE (98,ERR=930,IOSTAT=IOS) (SPECTRUM(I), I = 1, NAXIS1)
*
      CLOSE (98,ERR=940,IOSTAT=IOS)
      ERROR = 0
      RETURN
*
  910 CALL OPEN_ERROR (BINFILE, IOS)
      ERROR = 1
      RETURN
*
  920 CALL READ_ERROR (BINFILE, IOS, IREC)
      ERROR = 2
      RETURN
*
  930 CALL WRITE_ERROR (BINFILE, IOS)
      ERROR = 3
      RETURN
*
  940 CALL CLOSE_ERROR (BINFILE, IOS)
      ERROR = 4
      RETURN
      END
***************************
