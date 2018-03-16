**********************
      SUBROUTINE PSSF (ERROR)
**********************
*     read file containing point source sensitivities 
*     as a function of Julian date
*     for a given frequency and polarization
*
*     CMDP command parameters used:
*     1 = command mnemonic pssf
*     2 = name of file with JD, PSS columns
*     3 = X column = Julian date
*     4 = Y column = PSS (Jy/K); value at start JD if gradient is used
*     5 = Y2 column = dPSS/dt (Jy/K/day) = gradient in PSS with time
*
      IMPLICIT  NONE
*
*      CHARACTER FITSDATEN*20      ! string to hold date from file
      LOGICAL   EXISTS            ! local true if file exists
      INTEGER    MAXCOL           ! local max number of columns of data
      PARAMETER (MAXCOL = 50)     ! local max number of columns of data
      INTEGER   I                 ! local loop index
*      INTEGER   IDAYN             ! day of year
*      INTEGER   DDN               ! day of month
*      INTEGER   HHN               ! UT hours
*      INTEGER   MNN               ! UT minutes
*      INTEGER   MON               ! month
*      INTEGER   MSN               ! milliseconds
*      INTEGER   SSN               ! UT seconds
*      INTEGER   YYN               ! year, four digit
*      REAL*8    DJULDAN            ! full julian date
      INTEGER   ILINE             ! local line number in the mca file
      INTEGER   IOS               ! local file error check
      INTEGER   IXCOL             ! local column number with X values
      INTEGER   IYCOL             ! local column number with Y values
      INTEGER   IY2COL            ! local column number with Y2 values
      INTEGER   NCOL              ! number of columns to read
      INTEGER   ERROR             ! output error status
      INTEGER   NCHAR             ! external function
      REAL*8    VAL(MAXCOL)       ! local all values read from buffer
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in PSSF'
      ERROR = 0
*
*     second parameter is the file name to read from
*
      IF (NCMD .GE. 2) THEN
          WRITE (PSSRDFILE,'(A)') CMDP(2)
      ELSE 
          PRINT '(A,$)',' Point Source Sens. file to read (Q quits) ? '
          READ  '(A)', PSSRDFILE
          IF ((PSSRDFILE(1:1) .EQ. 'q' .OR. 
     &         PSSRDFILE(1:1) .EQ. 'Q')
     &        .AND. NCHAR(PSSRDFILE) .EQ. 1) RETURN
      END IF
      IF (DB) PRINT *,'file=',PSSRDFILE
*
      INQUIRE (FILE=PSSRDFILE,EXIST=EXISTS)
      IF (.NOT. EXISTS) THEN
          CALL ER_EXIST(PSSRDFILE)
          RETURN
      END IF
*
*     third parameter is the number of the column with X values 
*
      IXCOL = 0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=131) IXCOL
  131     IF (IOS .NE. 0) IXCOL = 0
      END IF
      DO WHILE (IXCOL .LE. 0)
          PRINT '(A,$)',' column with Julian Dates ? '
          READ '(A)', CMDP(3)
          CALL UPCASE (CMDP(3))
          IF (CMDP(3)(1:1) .EQ. 'Q') RETURN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=132) IXCOL
  132     IF (IOS .NE. 0) PRINT 2005, CMDP(3)
      END DO
      IF (DB) PRINT *,'JD col=',IXCOL
*
*     fourth parameter is the number of the column with Y values 
*
      IYCOL = 0
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=141) IYCOL
  141     IF (IOS .NE. 0) IYCOL = 0
      END IF
      DO WHILE (IYCOL .LE. 0)
          PRINT '(A,$)',' column with PSS in Jy/K ? '
          READ '(A)', CMDP(4)
          CALL UPCASE (CMDP(4))
          IF (CMDP(4)(1:1) .EQ. 'Q') RETURN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=142) IYCOL
  142     IF (IOS .NE. 0) PRINT 2005, CMDP(4)
      END DO
      IF (DB) PRINT *,'PSS col=',IYCOL
*
*     fifth parameter is the number of the column with Y2 values 
*
      IY2COL = -1
      IF (NCMD .GE. 5) THEN
          READ (CMDP(5),*,IOSTAT=IOS,ERR=151) IY2COL
  151     IF (IOS .NE. 0) IY2COL = -1
      END IF
      IF (IY2COL .EQ. -1) THEN
          PRINT *,'If there is no column for dPSS/dT, then enter 0'
      END IF
      DO WHILE (IY2COL .LE. -1)
          PRINT '(A,$)',' column with dPSS/dT in Jy/K/day ? '
          READ '(A)', CMDP(5)
          CALL UPCASE (CMDP(5))
          IF (CMDP(5)(1:1) .EQ. 'Q') RETURN
          READ (CMDP(5),*,IOSTAT=IOS,ERR=152) IY2COL
  152     IF (IOS .NE. 0) PRINT 2005, CMDP(5)
      END DO
      IF (DB) PRINT *,'dPSS/dT col=',IY2COL
*
*     check maximum number of columns to read
*
      NCOL = MAX(IXCOL,IYCOL)
      NCOL = MAX(NCOL, IY2COL)
      IF (DB) PRINT *,'NCOL=',NCOL, 'MAXCOL=',MAXCOL
      IF (NCOL .GT. MAXCOL) THEN
          WRITE (BUF,*) 'max cols =',MAXCOL,' exceeded'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          ERROR = 1
          RETURN
      END IF
*
*     open the PSS file
*
      OPEN (PSSRDUNIT,FILE=PSSRDFILE,STATUS='OLD',
     &      ACCESS='SEQUENTIAL',IOSTAT=IOS,ERR=190)
*
*     Error opening file - set error flag and exit
  190 IF (IOS .GT. 0) THEN
          ERROR = 1
          CALL ER_OPEN (PSSRDFILE,IOS)
          RETURN
      END IF
*
*     reset all values to zero
      DO I = 1, MAXPSSVALS
          DJDPSS(I) = 0D0
          PSSATJD(I) = 0D0
          PSSLOPE(I) = 0D0
      END DO
*
*     read the file, line by line
*
      ILINE = 0    ! line number in file
      NPSS = 0        ! number of PSS values read in from file
      DO WHILE (.TRUE.)
          READ (PSSRDUNIT,'(A)',END=250,IOSTAT=IOS,ERR=200) LBUF
  200     IF (IOS .GT. 0) THEN
              CALL ER_READ (PSSRDFILE,IOS,ILINE)
          END IF
*          
*         if data are horizontal tab delimited, convert tab to space
          DO I = 1, NCHAR(LBUF)
              IF (LBUF(I:I) .EQ. CHAR(9)) LBUF(I:I) = CHAR(32)
          END DO
*
          ILINE = ILINE + 1
*
          IF (ILINE .GT. 0) THEN
*             read data from cbuffer
              READ (LBUF,*,END=250,IOSTAT=IOS,ERR=201) 
     &            (VAL(I),I=1,NCOL)
  201         IF (IOS .NE. 0) THEN
*                 error reading numeric values
                  IF (DB) PRINT *,'line ',ILINE,': ',
     &                LBUF(1:NCHAR(LBUF))
              ELSE
*                 numeric values read okay
                  IF (DB) PRINT *,'line ',ILINE,': ',(VAL(I),I=1,NCOL)
*
*                 store the data in the X,Y arrays and update the data count
                  NPSS = NPSS + 1
                  DJDPSS(NPSS) = VAL(IXCOL)
                  PSSATJD(NPSS) = VAL(IYCOL)
                  IF (IY2COL .GT. 0) THEN
*                     the gradient in PSS with time is optional
                      PSSLOPE(NPSS) = VAL(IY2COL)
                  ELSE
                      PSSLOPE(NPSS) = 0D0
                  END IF
c                 PRINT *,'line ',ILINE,': ',DJDPSS(NPSS), PSSATJD(NPSS)
              END IF
*
*             prevent memory overflow
              IF (I .EQ. MAXPSSVALS) THEN
                  WRITE (BUF,*) 'PSS array full: ',MAXPSSVALS,' points'
                  PRINT *,BUF(1:NCHAR(BUF))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
                  GO TO 250
              END IF
          END IF
      END DO
*
*     end of file read
*
  250 CLOSE (PSSRDUNIT,IOSTAT=IOS,ERR=252)
  252 WRITE (BUF,*) NPSS,' valid values were read '
*
***** get the linux box to return the UT in FITS Y2K format
*      CALL TIMENOW (FITSDATEN,YYN,MON,DDN,HHN,MNN,SSN)
*
***** get IDAY = day of year
*      CALL DM2DOY (YYN,MON,DDN,IDAYN)
*      PRINT *,'Day number =',IDAYN
*
***** get Julian date in days
*      MSN = 0
*      CALL JD (YYN,IDAYN,HHN,MNN,SSN,MSN,DJULDAN)
*      PRINT '(A,F16.7)',' Julian date now =',DJULDAN, ' MJD=',
*     &DJULDAN-2400000.5D0
*
*     append current JD as terminating date for function PSS
*      NPSS = NPSS + 1
*      DJDPSS(NPSS) = DJULDAN
*      PSSATJD(NPSS) = 0.0
*      PSSLOPE(NPSS) = 0.0
*                                         
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      IF (NPSS .GT. 0) THEN
          WRITE (BUF,*) ' JD            PSS (Jy/K)  dPSS/dT (Jy/K/day)'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          DO I = 1, NPSS
              WRITE (BUF,2500) DJDPSS(I), PSSATJD(I), PSSLOPE(I)
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          END DO
      END IF
*
      RETURN
 2005 FORMAT ('illegal :',A)
 2500 FORMAT (F13.4,F12.6,F12.6)
      END
*********
