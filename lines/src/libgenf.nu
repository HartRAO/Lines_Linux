C LIBGENERAL
C subroutines for general use
C developed MJG/JQ 04/06/95
C rev MJG 09/11/95 various subroutines renamed, all implicit none
****************************
      SUBROUTINE ASKPLOTPACK (PLOTPAK)
****************************
*     ask user to select plotting package
      IMPLICIT NONE
      CHARACTER PLOTPAK*1         ! output P = PGPLOT, C = CPLOT, N = None
*
      IF (PLOTPAK .EQ. 'P' .OR.
     &    PLOTPAK .EQ. 'C' .OR.
     &    PLOTPAK .EQ. 'N') RETURN
*
      PRINT *,'Plotting packages: '
      PRINT *,'PGPLOT plots interactively via X Windows, PC/Kermit etc.'
      PRINT *,'   with hardcopy via Postscript, HPGL, GIF files etc.'
      PRINT *,'CPLOT  plots interactively on HP150,',
     &        ' with hardcopy direct to Thinkjet printer'
   10 PRINT *,'Use PGPLOT (P), CPLOT (C), NONE (N) ?'
      READ '(A)', PLOTPAK
      CALL UPCASE (PLOTPAK)
      IF (PLOTPAK .NE. 'P' .AND. 
     &    PLOTPAK .NE. 'C' .AND. 
     &    PLOTPAK .NE. 'N') GO TO 10
      RETURN
      END
***********************
      SUBROUTINE CLRLBL (LABEL)
***********************
C     set character string to blanks
C     used to clear top labels and axis labels
C
      IMPLICIT  NONE
      INTEGER   I
      CHARACTER LABEL*(*)
C
      DO I = 1,LEN(LABEL)
          LABEL(I:I) = ' '
      END DO
      RETURN
      END
***************************
      SUBROUTINE COMMALABEL (CBUF,ICOL,LABEL)
***************************
*     find label in comma delimited buffer
      IMPLICIT NONE
      CHARACTER CBUF*(*)      ! input buffer
      CHARACTER LABEL*(*)     ! output label extracted from buffer
      INTEGER   I             ! local
      INTEGER   ICHARSTART    ! local
      INTEGER   ICHAREND      ! local
      INTEGER   ICOL          ! input column number for label
      INTEGER   ICOLREAD      ! local
      INTEGER   LCHAR         ! local
      INTEGER   NBUF          ! local
      INTEGER   NCHAR             ! external function
      INTEGER   NLAB          ! local
      LOGICAL   GOTLAB        ! local true if label found 
      LOGICAL   BLANK         ! local true if blank character
*                             
      NBUF = NCHAR(CBUF)      ! number of non-blank characters in CBUF
      NLAB = LEN(LABEL)       ! length of LABEL
      GOTLAB = .FALSE.        ! initialise do loop control
      ICOLREAD = 0            ! initialise data column number
      ICHARSTART = 1          ! initialise start character count of column
      ICHAREND = -1           ! initialise end character count of column
      I = 0                   ! initialise character counter for loop
      DO WHILE (.NOT. GOTLAB)
          I = I + 1                   ! increment character count
          IF (CBUF(I:I) .EQ. ',') THEN
*             found the end of a column
              ICOLREAD = ICOLREAD + 1     ! column ICOLREAD read completed
              ICHARSTART = ICHAREND + 2   ! reset col start char num
              ICHAREND = I - 1            ! char number at end of column
              IF (ICOLREAD .EQ. ICOL) GOTLAB = .TRUE.  ! terminate loop
          END IF
          IF (I .EQ. NBUF) THEN 
*             at end of buffer
              IF (ICOLREAD+1 .NE. ICOL) THEN
*                 if wanted column is last column, may not end in a comma
                  ICHARSTART = ICHAREND + 2
                  ICHAREND = NBUF
                  GOTLAB = .TRUE. ! terminate loop
              ELSE
                  PRINT *,'could not find label in column ',ICOL,' of:'
                  PRINT *,CBUF(1:NCHAR(CBUF))
                  RETURN
              END IF
          END IF
      END DO
*
      IF (GOTLAB) THEN
*         skip any spaces (char(32)) before first character
          BLANK = .TRUE.
          I = 0
          DO WHILE (BLANK)
              IF (CBUF(ICHARSTART+I:ICHARSTART+I) .EQ. CHAR(32)) THEN
                  I = I + 1
              ELSE
                  BLANK = .FALSE.
              END IF
          END DO
          LABEL(1:NLAB) = CBUF((ICHARSTART+I):ICHAREND)
      END IF
*     change trailing carriage return (char(13)) to space (char(32)
      LCHAR = NCHAR (LABEL)
      IF (LABEL(LCHAR:LCHAR) .EQ. CHAR(13)) THEN
          LABEL(LCHAR:LCHAR) = CHAR(32)
      END IF
      RETURN
      END
************************
      SUBROUTINE DMAXMIN (NPTS,V,VBAR,IBAR,VMIN,VMAX)
************************
      IMPLICIT NONE
      INTEGER     NPTS        ! input number of points  
      INTEGER     I
      INTEGER     IBAR        ! input 0 disregard errors 1 use errors
      REAL        V(*)        ! input value array
      REAL        VBAR(*)     ! input error in value array
      REAL        VMIN        ! output value minimum
      REAL        VMAX        ! output value maximum
      REAL        VLO         ! local lower data value
      REAL        VHI         ! local upper data value
C
      IF (IBAR .EQ. 0) THEN
          VMIN = V(1)
          VMAX = V(1)
      ELSE 
          VMIN = V(1) - VBAR(1)
          VMAX = V(1) + VBAR(1)
      END IF
C
      DO I = 2,NPTS
          IF (IBAR .EQ. 0) THEN
              VLO = V(I)
              VHI = V(I)
          ELSE 
              VLO = V(I) - VBAR(I)
              VHI = V(I) + VBAR(I)
          END IF
          VMIN = MIN (VMIN, VLO)
          VMAX = MAX (VMAX, VHI)
      END DO
      RETURN
      END
*************************
      SUBROUTINE LISTDATA (INFILE,NAME,N,X,Y,XBAR,YBAR,IXBAR,IYBAR)
*************************
*     list x,dx,y,dy values to screen
*
      IMPLICIT NONE
      CHARACTER INFILE*(*), NAME*(*), ANSWER*1
      INTEGER   NCHAR 
      INTEGER   I, N, IXBAR, IYBAR
      REAL      X(1), Y(1), XBAR(1), YBAR(1)
*
*     general caption
*
      PRINT '(3A)',
     &    NAME(1:NCHAR(NAME)+1),
     &    ' from file ',INFILE(1:NCHAR(INFILE))
*
*     column headings
*
      IF (IXBAR .EQ. 0 .AND. IYBAR .EQ. 0) THEN
          PRINT 1002
 1002     FORMAT (12X,'Xval,',13X,'Yval')
      ELSE IF (IXBAR .EQ. 1 .AND. IYBAR .EQ. 0) THEN
          PRINT 1003
 1003     FORMAT (12X,'Xval,',13X,'Xerr,',13X,'Yval')
      ELSE IF (IXBAR .EQ. 0 .AND. IYBAR .EQ. 1) THEN
          PRINT 1004
 1004     FORMAT (12X,'Xval,',13X,'Yval,',13X,'Yerr')
      ELSE IF (IXBAR .EQ. 1 .AND. IYBAR .EQ. 1) THEN
          PRINT 1005
 1005     FORMAT (12X,'Xval,',13X,'Xerr,',13X,'Yval,',13X,'Yerr')
      END IF
*
*     tabulate the data
*
      DO I = 1,N
          IF (IXBAR .EQ. 0 .AND. IYBAR .EQ. 0) THEN
              PRINT 1012, X(I), Y(I)
 1012         FORMAT (E16.7,',',1X,E16.7)
          ELSE IF (IXBAR .EQ. 1 .AND. IYBAR .EQ. 0) THEN
              PRINT 1013, X(I), XBAR(I), Y(I)
 1013         FORMAT (E16.7,2(',',1X,E16.7))
          ELSE IF (IXBAR .EQ. 0 .AND. IYBAR .EQ. 1) THEN
              PRINT 1013, X(I), Y(I), YBAR(I)
          ELSE IF (IXBAR .EQ. 1 .AND. IYBAR .EQ. 1) THEN
              PRINT 1015, X(I), XBAR(I), Y(I), YBAR(I)
 1015         FORMAT (E16.7,3(',',1X,E16.7))
          END IF
*
          IF ((I/20)*20 .EQ. I) THEN
              PRINT *,'Press ENTER for more, Q quits'
              READ '(A)', ANSWER
              CALL UPCASE (ANSWER)
              IF (ANSWER .EQ. 'Q') GO TO 30
          END IF
      END DO
   30 RETURN
      END
*************************
      SUBROUTINE MCASCRD (INFILE,XIN,YIN,XBAR,YBAR,IXBAR,IYBAR,
     &                     NAME,LABX,LABY,NPTS,DISC_ERROR)
*************************
*  Read housekeeping and data from a disc file in multicolumn ascii format
*
      IMPLICIT NONE
      INTEGER    NMAX
      INTEGER    MAXCOL
      PARAMETER (NMAX = 2000)     ! local max number of data points
      PARAMETER (MAXCOL = 100)    ! local max number of columns of data
      CHARACTER ANSWER*4          ! local answer buffer
      CHARACTER INFILE*(*)        ! output data file name
      CHARACTER LABX*(*)          ! output X axis label from file or user
      CHARACTER LABY*(*)          ! output Y axis label from file or user
      CHARACTER NAME*(*)          ! output data caption from file or user
      CHARACTER CBUF*1000         ! local read buffer
      LOGICAL   DISC_ERROR        ! output true if error handling file
      LOGICAL   EXISTS            ! local true if file exists
      INTEGER   I                 ! local
      INTEGER   J                 ! local
      INTEGER   ICAP              ! local
      INTEGER   IERR              ! local 
      INTEGER   IFCHAR            ! local
      INTEGER   ILAB              ! local
      INTEGER   IROW              ! local
      INTEGER   ISPACE            ! local
      INTEGER   IXBAR             ! output, 0 if no X errors, else 1
      INTEGER   IXBCOL            ! local
      INTEGER   IXCOL             ! local column number with X values
      INTEGER   IYBAR             ! output, 0 if no Y errors, else 1
      INTEGER   IYCOL             ! local column number with Y values
      INTEGER   IYBCOL            ! local
      INTEGER   LCNAME            ! local
      INTEGER   LINE              ! local
      INTEGER   NCHAR             ! external function
      INTEGER   NCOL              ! local
      INTEGER   NPTS              ! output number of data points
      REAL      XIN(*)            ! output array of X values from file
      REAL      XBAR(*)           ! output array of Y errors from file
      REAL      YIN(*)            ! output array of X values from file
      REAL      YBAR(*)           ! output array of Y errors from file
      REAL      VAL(MAXCOL)       ! local all values read from buffer
*
*     default column numbers for data and errors
      SAVE      IXCOL, IXBCOL, IYCOL, IYBCOL, ILAB, ICAP
      DATA      IXCOL /1/, IYCOL /3/, ILAB /2/, ICAP /1/
*
  100 EXISTS = .FALSE.
      DO WHILE (.NOT. EXISTS)
          PRINT '(A,$)',' Name of data file to read (q quits) ?'
          READ  '(A)', INFILE
          IFCHAR = NCHAR(INFILE)
          IF (INFILE(1:1) .EQ. 'q' .AND. IFCHAR .EQ. 1) THEN
              NPTS = 0
              DISC_ERROR = .TRUE.
              RETURN
          END IF
          INQUIRE (FILE=INFILE,EXIST=EXISTS)
          IF (.NOT. EXISTS) THEN
              PRINT *,'File not found'
          END IF
      END DO
*
  111 PRINT '(A,I2,A,$)',
     &' Data caption is in line number (Enter=',ICAP,') ?'
      READ '(A)',ANSWER
      ANSWER(4:4) = '/'
      READ  (ANSWER,*,ERR=111), ICAP
*
  121 PRINT '(A,I2,A,$)',
     &' Data column labels are in line number (Enter=',ILAB,') ?'
      READ '(A)',ANSWER
      ANSWER(4:4) = '/'
      READ  (ANSWER,*,ERR=121), ILAB
*
  131 IROW = ILAB + 1
      PRINT '(A,I2,A,$)',
     &' Data start at line number (Enter=',IROW,') ?'
      READ '(A)',ANSWER
      ANSWER(4:4) = '/'
      READ  (ANSWER,*,ERR=131), IROW

  141 PRINT '(A,I2,A,$)',
     &' Data column number with X values (Enter=',IXCOL,') ?'
      READ '(A)',ANSWER
      ANSWER(4:4) = '/'
      READ  (ANSWER,*,ERR=141), IXCOL
*
  151 IXBCOL = IXCOL + 1
      PRINT '(A,I2,A,$)',
     &' Data column number with X errors (Enter=',IXBCOL,') ?'
      READ '(A)',ANSWER
      ANSWER(4:4) = '/'
      READ  (ANSWER,*,ERR=151), IXBCOL

      IXBAR = 0                         ! no errors to plot
      IF (IXBCOL .GT. 0) IXBAR = 1      ! errors to plot optionally
*
  161 PRINT '(A,I2,A,$)',
     &' Data column number with Y values (Enter=',IYCOL,') ?'
      READ '(A)',ANSWER
      ANSWER(4:4) = '/'
      READ  (ANSWER,*,ERR=161), IYCOL
*
  171 IYBCOL = IYCOL + 1
      PRINT '(A,I2,A,$)',
     &' Data column number with Y errors (Enter=',IYBCOL,') ?'
      READ '(A)',ANSWER
      ANSWER(4:4) = '/'
      READ  (ANSWER,*,ERR=171), IYBCOL
*
      IYBAR = 0                         ! no errors to plot
      IF (IYBCOL .GT. 0) IYBAR = 1      ! errors to plot optionally
*
*     clear the caption if not used
      IF (ICAP .EQ. 0) CALL CLRLBL (NAME)
      IF (ILAB .EQ. 0) THEN
*         clear the X and Y axis labels
          CALL CLRLBL (LABX)
          CALL CLRLBL (LABY)
      END IF
*
*     maximum number of columns to read
      NCOL = MAX(IXCOL,IYCOL)
      NCOL = MAX(NCOL, IXBCOL)
      NCOL = MAX(NCOL, IYBCOL)
      IF (NCOL .GT. MAXCOL) THEN
          PRINT *,'maximum number of columns =',MAXCOL
          PRINT *,'Increase MAXCOL in MCASRD'
          DISC_ERROR = .TRUE.
          RETURN
      END IF
*
      DISC_ERROR = .FALSE.
      OPEN (10,FILE=INFILE,STATUS='OLD',ACCESS='SEQUENTIAL',
     &      IOSTAT=IERR,ERR=990)
*
      LINE = 0    ! initialise line number in file
      NPTS = 0    ! initialise number of data points read
*
      DO WHILE (.TRUE.)
          READ (10,'(A)',END=250,IOSTAT=IERR,ERR=200) CBUF
  200     IF (IERR .GT. 0) THEN
              PRINT *,'read error ',IERR,' at line ',LINE+1
          END IF
          LINE = LINE + 1
          IF (LINE .EQ. ICAP) THEN
*             get caption
              PRINT *,'Getting caption on line',line
              NAME(1:LEN(NAME)) = CBUF(1:NCHAR(CBUF))
*             convert trailing carriage return (13) to blank (32)
              LCNAME = NCHAR(NAME)
*             change trailing carriage return (char(13)) to space (char(32))
              IF (NAME(LCNAME:LCNAME) .EQ. CHAR(13)) THEN
                  NAME(LCNAME:LCNAME) = CHAR(32)   
              END IF
          ELSE IF (LINE .EQ. ILAB) THEN
*             get X,Y labels
              PRINT *,'Getting labels on line',line
*             first see if column headers are comma or space delimited 
              ISPACE = 0              ! assume space delimited
              DO I = 1, NCHAR(CBUF)
                  IF (CBUF(I:I) .EQ. ',') THEN
                      ISPACE = 1      ! assume comma delimited
                  END IF
              END DO
              IF (ISPACE .EQ. 0) THEN
*                 space delimited data
                  PRINT *,'labels are not comma delimited'
                  CALL SPACELABEL (CBUF,IXCOL,LABX)
                  CALL SPACELABEL (CBUF,IYCOL,LABY)
              ELSE IF (ISPACE .EQ. 1) THEN
*                 get axis labels in comma delimited data
                  print *,'labels are comma delimited'
                  CALL COMMALABEL (CBUF,IXCOL,LABX)
                  CALL COMMALABEL (CBUF,IYCOL,LABY)
              END IF
          ELSE IF (LINE .GE. IROW) THEN
*             read in data
              READ (CBUF,*,END=250,IOSTAT=IERR,ERR=201) 
     &            (VAL(J),J=1,NCOL)
  201         IF (IERR .NE. 0) THEN
                  PRINT *,' read terminated: line ',I,'non-numeric:'
                  PRINT *,CBUF(1:NCHAR(CBUF))
                  GO TO 250
              END IF
              NPTS = NPTS + 1
              IF (NPTS .EQ. NMAX) GO TO 250
              XIN(NPTS) = VAL(IXCOL)
              IF (IXBCOL .GT. 0) XBAR(NPTS) = VAL(IXBCOL)
              YIN(NPTS) = VAL(IYCOL)
              IF (IYBCOL .GT. 0) YBAR(NPTS) = VAL(IYBCOL)
          END IF
      END DO
*
*     end of file encountered, without errors
*
  250 PRINT *,NPTS,' valid values were read '
      IF (NPTS .GE. NMAX) THEN
          PRINT *,'Max number of points exceeded'
          PRINT *,'Increase NMAX in all parameter statements'
      END IF
      CLOSE (10)
*
*     option to change caption and axis labels
*
      PRINT *,'Data title (enter="',NAME(1:NCHAR(NAME)),'") ?'
      READ  '(A)',CBUF
      IF (NCHAR(CBUF) .GT. 1) NAME = CBUF
*
      PRINT *,'X axis label (enter="',LABX(1:NCHAR(LABX)),'") ?'
      READ  '(A)',CBUF
      IF (NCHAR(CBUF) .GT. 1) LABX = CBUF
*
      PRINT *,'Y axis label (enter="',LABY(1:NCHAR(LABY)),'") ?'
      READ  '(A)',CBUF
      IF (NCHAR(CBUF) .GT. 1) LABY = CBUF
*
      RETURN
*
*         Error opening file - set error flag and exit
  990     PRINT *,' Error ',IERR,' opening ',INFILE
          CLOSE (10,IOSTAT=IERR,ERR=991)
  991     DISC_ERROR = .TRUE.
          RETURN
      END
******************************
      INTEGER FUNCTION   NCHAR (ARRAY)
******************************
*     find number of non-blank characters in character array
*     nchar is set = 1 if array is blank
      IMPLICIT NONE
      CHARACTER ARRAY*(*)
*
*     find length of the array
      NCHAR = LEN (ARRAY)
      DO WHILE (ARRAY(NCHAR:NCHAR) .EQ. ' ' .AND. NCHAR .GT. 1)
          NCHAR = NCHAR - 1
      END DO
      RETURN
      END
***************************
      SUBROUTINE SPACELABEL (CBUF,ICOL,LABEL)
***************************
*     find label in space delimited buffer
      IMPLICIT NONE
      CHARACTER CBUF*(*)      ! input buffer
      CHARACTER LABEL*(*)     ! output label extracted from buffer
      INTEGER   I             ! local
      INTEGER   ICHARSTART    ! local
      INTEGER   ICHAREND      ! local
      INTEGER   ICOL          ! input column number for label
      INTEGER   ICOLREAD      ! local
      INTEGER   NBUF          ! local
      INTEGER   NCHAR             ! external function
      INTEGER   NLAB          ! local
      LOGICAL   GOTLAB        ! local true if label found 
      LOGICAL   PREVSPACE     ! local set true if character is a space
      LOGICAL   CHANGETYPE    ! local char changes to space/notspace
*                             
      NBUF = NCHAR(CBUF)      ! number of non-blank characters in CBUF
      NLAB = LEN(LABEL)       ! length of LABEL
      PREVSPACE = .TRUE.      ! initialise previous char = space flag
      ICOLREAD = 0            ! initialise data column number
      I = 0                   ! initialise character counter
      GOTLAB = .FALSE.        ! initialise do loop control
      DO WHILE (.NOT. GOTLAB)
          I = I + 1                   ! increment character count
          CHANGETYPE = .FALSE.        ! initialise no change space/nonspace
          IF (PREVSPACE .AND. CBUF(I:I) .NE. ' ') THEN
*             character not a space - at start of a label
              ICOLREAD = ICOLREAD + 1     ! new column lable read started
              ICHARSTART = I              ! reset col start char num
              CHANGETYPE = .TRUE.         ! space changed to not space
          END IF
          IF (.NOT. PREVSPACE .AND.
     &        (CBUF(I:I) .EQ. ' ' .OR. CBUF(I:I) .EQ. CHAR(13))) THEN
*             character is a space - at end of a label 
              ICHAREND = I - 1
              CHANGETYPE = .TRUE.         ! notspace changed to space
              IF (ICOLREAD .EQ. ICOL) GOTLAB = .TRUE.  ! terminate loop
          END IF
          IF (CHANGETYPE) PREVSPACE = .NOT. PREVSPACE
          IF (I .EQ. NBUF) THEN 
*             at end of buffer
              IF (ICOLREAD .EQ. ICOL) THEN
*                 wanted column may be last column
                  ICHAREND = NBUF
              ELSE
                  PRINT *,'could not find label for column ',ICOL,' of:'
                  PRINT *,CBUF(1:NCHAR(CBUF))
                  RETURN
              END IF
              GOTLAB = .TRUE. ! terminate loop
          END IF
      END DO
*
      IF (ICOLREAD .EQ. ICOL) THEN
          PRINT *,'got label for column', ICOL
          LABEL(1:NLAB) = CBUF(ICHARSTART:ICHAREND)
      END IF
      RETURN
      END
***********************
      SUBROUTINE UPCASE (ARRAY)
***********************
*     set characters to upper case in character array of length n
      IMPLICIT NONE
      INTEGER I, IVAL, N
      CHARACTER ARRAY*(*)
*
*     find length of the array
      N = LEN (ARRAY)
      DO I = 1,N
          IVAL = ICHAR(ARRAY(I:I))
          IF (IVAL .GE. 97 .AND. IVAL .LE. 122) IVAL = IVAL - 32
          ARRAY(I:I) = CHAR(IVAL)
      END DO
      RETURN
      END
