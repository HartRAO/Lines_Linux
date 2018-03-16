*************************
      SUBROUTINE RDMCAFL (ERROR)
*************************
*  Read housekeeping and data from a disc file in multicolumn ascii format
*
*     CMDP command parameters used:
*     1 = command mnemonic RMCA
*     2 = file name to read from
*     3 = number of the column with X values (default to 1)
*     4 = number of the column with Y values (default to 2)
*     5 = line number at which the data start (optional)
*     6 = line number with a caption for the data (optional)
*     7 = line number for X and Y column labels (optional)
*     8 = memory to store data in, default to set memory (optional)
*
*     other routines called:
*     commalbl, er_exist, er_open, er_read, linfit, nchar, spacelbl
*  
*
      IMPLICIT NONE
      INTEGER    MAXCOL
      PARAMETER (MAXCOL = 100)    ! local max number of columns of data
      LOGICAL   EXISTS            ! local true if file exists
      INTEGER   ERROR             ! output error status
      INTEGER   I                 ! local loop index
      INTEGER   ICAP              ! local line number with caption
      INTEGER   ICHAR             ! local number of chars in string
      INTEGER   ILAB              ! local line number with column labels
      INTEGER   IOS               ! local file i/o error status
      INTEGER   IROW              ! local line number where data start
      INTEGER   ISLOPE            ! local sign of dX/dI
      INTEGER   ISPACE            ! local 0 = spce delim, 1 = , delimited
      INTEGER   IXCOL             ! local column number with X values
      INTEGER   IYCOL             ! local column number with Y values
      INTEGER   ILINE             ! local line number in the mca file
      INTEGER   MEM               ! local memory in use
      INTEGER   MEMFIT            ! output memory for LINFIT to use
      INTEGER   NCHAR             ! external function
      INTEGER   NCOL              ! number of columns to read
      REAL*8    CONSTANT          ! local intercept from linear regression
      REAL*8    RMSDEV            ! local rms deviation of X's from linear
      REAL*8    SLOPE             ! local slope of line fit
      REAL*8    VAL(MAXCOL)       ! local all values read from buffer
*
      INCLUDE 'lines.inc'
*
      ERROR = 0
      IF (DB) PRINT *,'in RDMCAFL'
*
*     second parameter is the file name to read from
*
      IF (NCMD .GE. 2) THEN
          WRITE (MCARDFILE,'(A)') CMDP(2)
      ELSE 
          PRINT '(A,$)',' multi-column ascii file to read (Q quits) ? '
          READ  '(A)', MCARDFILE
          IF ((MCARDFILE(1:1) .EQ. 'q' .OR. 
     &         MCARDFILE(1:1) .EQ. 'Q')
     &        .AND. NCHAR(MCARDFILE) .EQ. 1) RETURN
      END IF
      IF (DB) PRINT *,'file=',MCARDFILE
*
      INQUIRE (FILE=MCARDFILE,EXIST=EXISTS)
      IF (.NOT. EXISTS) THEN
          CALL ER_EXIST(MCARDFILE)
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
          PRINT '(A,$)',' X column ? '
          READ '(A)', CMDP(3)
          CALL UPCASE (CMDP(3))
          IF (CMDP(3)(1:1) .EQ. 'Q') RETURN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=132) IXCOL
  132     IF (IOS .NE. 0) PRINT 2005, CMDP(3)
      END DO
      IF (DB) PRINT *,'Xcol=',IXCOL
*
*     fourth parameter is the number of the column with Y values 
*
      IYCOL = 0
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=141) IYCOL
  141     IF (IOS .NE. 0) IYCOL = 0
      END IF
      DO WHILE (IYCOL .LE. 0)
          PRINT '(A,$)',' Y column ? '
          READ '(A)', CMDP(4)
          CALL UPCASE (CMDP(4))
          IF (CMDP(4)(1:1) .EQ. 'Q') RETURN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=142) IYCOL
  142     IF (IOS .NE. 0) PRINT 2005, CMDP(4)
      END DO
      IF (DB) PRINT *,'Ycol=',IYCOL
*
*     check maximum number of columns to read
*
      NCOL = MAX(IXCOL,IYCOL)
      IF (NCOL .GT. MAXCOL) THEN
          WRITE (BUF,*) 'max cols =',MAXCOL,' exceeded'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          ERROR = 1
          RETURN
      END IF
*
*     fifth parameter is the row at which the data start
*
      IROW = 0
      IF (NCMD .GE. 5) THEN
          READ (CMDP(5),*,IOSTAT=IOS,ERR=151) IROW
  151     IF (IOS .NE. 0) IROW = 0
      END IF
      DO WHILE (IROW .LE. 0)
          PRINT '(A,$)',' data start at line ? '
          READ '(A)', CMDP(5)
          CALL UPCASE (CMDP(5))
          IF (CMDP(5)(1:1) .EQ. 'Q') RETURN
          READ (CMDP(5),*,IOSTAT=IOS,ERR=152) IROW
  152     IF (IOS .NE. 0) PRINT 2005, CMDP(5)
      END DO
      IF (DB) PRINT *,'IROW=',IROW
*
*     sixth parameter is caption line - here 0 is legal
*
      ICAP = -1
      IF (NCMD .GE. 6) THEN
          READ (CMDP(6),*,IOSTAT=IOS,ERR=161) ICAP
  161     IF (IOS .NE. 0) ICAP = 0
      END IF
      DO WHILE (ICAP .LT. 0)
          PRINT '(A,$)',' caption in line (0=none) ? '
          READ '(A)', CMDP(6)
          CALL UPCASE (CMDP(6))
          IF (CMDP(6)(1:1) .EQ. 'Q') RETURN
          READ (CMDP(6),*,IOSTAT=IOS,ERR=162) ICAP
  162     IF (IOS .NE. 0) PRINT 2005, CMDP(6)
      END DO
      IF (DB) PRINT *,'ICAP=',ICAP
*
*     seventh parameter is the line with data col labels - 0 is legal
*
      ILAB = -1
      IF (NCMD .GE. 7) THEN
          READ (CMDP(7),*,IOSTAT=IOS,ERR=171) ILAB
  171     IF (IOS .NE. 0) ILAB = 0
      END IF
      DO WHILE (ILAB .LT. 0)
          PRINT '(A,$)',' column labels in line (0=none) ?'
          READ '(A)', CMDP(7)
          CALL UPCASE (CMDP(7))
          IF (CMDP(7)(1:1) .EQ. 'Q') RETURN
          READ (CMDP(7),*,IOSTAT=IOS,ERR=172) ILAB
  172     IF (IOS .NE. 0) PRINT 2005, CMDP(7)
      END DO
      IF (DB) PRINT *,'ILAB=',ILAB
*
*     eighth parameter is the destination memory
*
      MEM = MEMSET
      IF (NCMD .GE. 8) THEN
          READ (CMDP(8),*,IOSTAT=IOS,ERR=181) MEM
  181     IF (IOS .NE. 0) THEN
              PRINT *, 'illegal ',CMDP(8)(1:NCHAR(CMDP(8)))
              MEM = 0
          END IF
      END IF
      IF (MEM .EQ. 0) THEN
          PRINT '(A,$)',' store in which memory ? '
          READ '(A)',CMDP(8)
          READ (CMDP(8),*,IOSTAT=IOS,ERR=182) MEM
  182     IF (IOS .NE. 0 .OR. MEM .LT. 1 .OR. MEM .GT. MAXMEM .OR.
     &        MEM .EQ. MEMRD) MEM = MEMSET
      END IF    
      IF (DB) PRINT *,'mem=',MEM
*
*     summarise the command
*
      WRITE (LBUF,*) 'read ',MCARDFILE(1:NCHAR(MCARDFILE)),
     &    ' Xcol= ',IXCOL,' Ycol= ',IYCOL,' data from L',IROW,
     &    ' cap L',ICAP,' colhdr L',ILAB,
     &    ' to mem',MEM
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
*
*
*     clear the memory being read into completely
*
*     preset non-essential housekeeping in case not present
      WRITE (CMDP(2),*,IOSTAT=IOS,ERR=185) MEMRD
  185 IF (NCMD .LT. 2) NCMD = 2
      CALL MEMCLR (ERROR)
*
*     default the caption to the file name
      IF (ICAP .EQ. 0) WRITE (OBJECT(MEMRD),'(A)') 
     &    MCARDFILE(1:NCHAR(MCARDFILE))
*
      IF (ILAB .EQ. 0) THEN
*         default coordinate types
          WRITE (CTYPE1(MEMRD),'(A)') 'X'
          WRITE (CTYPE2(MEMRD),'(A)') 'Y'
      END IF
*
*     open the mca file
*
      OPEN (MCARDUNIT,FILE=MCARDFILE,STATUS='OLD',
     &      ACCESS='SEQUENTIAL',IOSTAT=IOS,ERR=190)
*
*     Error opening file - set error flag and exit
  190 IF (IOS .GT. 0) THEN
          CALL ER_OPEN (MCARDFILE,IOS)
          RETURN
      END IF
*
*     read the file, line by line
*
      ILINE = 0    ! initialise line number in file
      NAXIS1(MEMRD) = 0 ! make sure data count is initialised
      DO WHILE (.TRUE.)
          READ (MCARDUNIT,'(A)',END=250,IOSTAT=IOS,ERR=200) LBUF
  200     IF (IOS .GT. 0) THEN
              CALL ER_READ (MCARDFILE,IOS,ILINE)
          END IF
*          
*         if data are horizontal tab delimited, convert tab to space
          DO I = 1, NCHAR(LBUF)
              IF (LBUF(I:I) .EQ. CHAR(9)) LBUF(I:I) = CHAR(32)
          END DO
*
          ILINE = ILINE + 1
          IF (ILINE .EQ. ICAP) THEN
*             get caption
              IF (DB) PRINT *,'get caption at L',ILINE
              OBJECT(MEMRD)(1:LEN(OBJECT(MEMRD))) = LBUF(1:NCHAR(LBUF))
*             convert trailing carriage return (13) to blank (32)
              ICHAR = NCHAR(OBJECT(MEMRD))
*             change trailing carriage return (char(13)) to space (char(32))
              IF (OBJECT(MEMRD)(ICHAR:ICHAR) .EQ. CHAR(13)) THEN
                  OBJECT(MEMRD)(ICHAR:ICHAR) = CHAR(32)   
              END IF
          END IF
*
          IF (ILINE .EQ. ILAB) THEN
*             get X,Y labels
              IF (DB) PRINT *,'get column labels at L',ILINE
*             first see if column headers are comma or space delimited 
              ISPACE = 0              ! assume space delimited
              DO I = 1, NCHAR(LBUF)
                  IF (LBUF(I:I) .EQ. ',') THEN
                      ISPACE = 1      ! assume comma delimited
                  END IF
              END DO
              IF (ISPACE .EQ. 0) THEN
*                 get axis labels in space delimited data
                  IF (DB) PRINT *,'labels not comma delim'
                  CALL SPACELBL (LBUF,IXCOL,CTYPE1(MEMRD))
                  CALL SPACELBL (LBUF,IYCOL,CTYPE2(MEMRD))
              ELSE IF (ISPACE .EQ. 1) THEN
*                 get axis labels in comma delimited data
                  IF (DB) PRINT *,'labels comma delim'
                  CALL COMMALBL (DB,LBUF,IXCOL,CTYPE1(MEMRD))
                  CALL COMMALBL (DB,LBUF,IYCOL,CTYPE2(MEMRD))
              END IF
          END IF
*
          IF (ILINE .GE. IROW) THEN
*             read data from cbuffer
              READ (LBUF,*,END=250,IOSTAT=IOS,ERR=201) 
     &            (VAL(I),I=1,NCOL)
  201         IF (IOS .NE. 0) THEN
                  WRITE (LBUF2,*) ' stop read: line ',ILINE,
     &                ' non-numeric:',LBUF(1:NCHAR(LBUF))
                  PRINT *,LBUF2(1:NCHAR(LBUF2))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) 
     &                LBUF2(1:NCHAR(LBUF2))
                  GO TO 250
              END IF
              IF (DB) PRINT *,'LINE ',ILINE,':',(VAL(I),I=1,NCOL)
*
*             store the data in the X,Y arrays and update the data count
              NAXIS1(MEMRD) = NAXIS1(MEMRD) + 1
              XIN(MEMRD,NAXIS1(MEMRD)) = VAL(IXCOL)
              YIN(MEMRD,NAXIS1(MEMRD)) = VAL(IYCOL)
*
*             prevent memory overflow
              IF (NAXIS1(MEMRD) .EQ. MAXDATA) THEN
                  WRITE (BUF,*) 'mem full: ',MAXDATA,' points'
                  PRINT *,BUF(1:NCHAR(BUF))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
                  GO TO 250
              END IF
          END IF
      END DO
*
*     end of file read
*
  250 CLOSE (MCARDUNIT,IOSTAT=IOS,ERR=252)
  252 WRITE (BUF,*) NAXIS1(MEMRD),' valid values were read '
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*     bail out if no data were read
*
      IF (NAXIS1(MEMRD) .EQ. 0) RETURN
*
*     set up housekeeping
*
      CRPIX1(MEMRD) = 1
      FIRST_CH(MEMRD) = 1
      LAST_CH(MEMRD) = NAXIS1(MEMRD)
      IXSPACING(MEMRD) = 0 ! preset to regular spacing
      NAXIS(MEMRD) = 1
      NUMFILE(MEMRD) = 1
      ADDED(MEMRD) = 1
      DUR(MEMRD) = 1
      FROMFILE(MEMRD) = MCARDFILE
*
*     if XIN are regularly spaced 
C      CDELT1(MEMRD) = 
C     &   (XIN(MEMRD,LAST_CH(MEMRD)) - XIN(MEMRD,FIRST_CH(MEMRD)))
C     &   / (NAXIS1(MEMRD)-1) 
C      CRVAL1(MEMRD) = XIN(MEMRD,FIRST_CH(MEMRD))
*
*     do linear regression thru X values to check linearity using fit rms
*
      MEMFIT = MEMRD
      CALL LINFIT(MEMFIT,SLOPE,CONSTANT,RMSDEV)
*
      CDELT1(MEMRD) = SLOPE
      CRVAL1(MEMRD) = CONSTANT + SLOPE
*
      WRITE (BUF,*) 'dX/dI: avrg change per point = ',SLOPE
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,*) 'dX/dI: rms deviation from linear = ',RMSDEV
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      IF (RMSDEV .GT. 1D-3) THEN
          WRITE (BUF,*) 'X(I) are IRREGULARLY spaced'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          IXSPACING(MEMRD) = 1  ! irregular spacing
      END IF
*
      ISLOPE = +1
      IF (SLOPE .LT. 0.0) ISLOPE = -1
      DO I = FIRST_CH(MEMRD)+1, LAST_CH(MEMRD)-1
         IF ((XIN(MEMRD,I)-XIN(MEMRD,I-1))*ISLOPE .LT. 0.0) THEN
             WRITE (BUF,*) 'X(I) are not monotonic: for I= ',I-1,I,I+1,
     &           ' XIN= ',XIN(MEMRD,I-1), XIN(MEMRD,I), XIN(MEMRD,I+1)
             PRINT *,BUF(1:NCHAR(BUF))
             IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
             IXSPACING(MEMRD) = 2  ! X spacing not monotonic
         END IF
      END DO
*
*     transfer data to destination memory
*
      WRITE (CMDP(2),*) MEMRD
      WRITE (CMDP(3),*) MEM
      NCMD = 3
      CALL MEMCOPY (ERROR)
*
      RETURN
 2005 FORMAT ('illegal :',A)
      END
******************************
