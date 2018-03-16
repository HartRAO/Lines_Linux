      SUBROUTINE LOGFOPEN (ERROR)
*************************
*     open the logwrite file and wind on to its end
*
*     CMDP command parameters used:
*     1 = command mnemonic LOG
*     2 = option ON or OFF
*
*     called by:
*     cmdselct, lineinit
*
*     subroutines called:
*     er_open, er_read
*
      IMPLICIT  NONE
*
      LOGICAL   EXISTS              ! file existence 
      LOGICAL   OPEN                ! file open status
      INTEGER   ERROR               ! returned file error code
      INTEGER   IOS                 ! i/o status
      INTEGER   IREC                ! record number read in
      INTEGER   NCHAR               ! external function
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in LOGFOPEN'
*
      ERROR = 0
      IF (NCMD .LT. 2) THEN
          PRINT '(A,$)','Turn logging ON or OFF ?'
          READ '(A)',CMDP(2)
      END IF
      CALL UPCASE (CMDP(2))
      IF (CMDP(2)(1:2) .EQ. 'ON') THEN
          WRITELOG = .TRUE.
      ELSE IF (CMDP(2)(1:3) .EQ. 'OFF') THEN
          WRITELOG = .FALSE.
      ELSE
          PRINT *,'illegal ',CMDP(2)(1:NCHAR(CMDP(2)))
      END IF 
*
      PRINT '(2A,$)',' logging to ',LOGWRTFILE(1:NCHAR(LOGWRTFILE))
      IF (WRITELOG) THEN
          PRINT *,' is on'
      ELSE
          PRINT *,' is off'
*         for speed, don't close file if logging turned off
c          CLOSE (LOGWRTUNIT)
*         simply wait until it is turned on again
          RETURN
      END IF
*
*     open the log write file 
*
      INQUIRE (FILE=LOGWRTFILE, EXIST=EXISTS, OPENED=OPEN)
c      IF (DB) THEN
          PRINT '(1x,A,$)',LOGWRTFILE
          IF (EXISTS) THEN
              PRINT '(A,$)',' exists and is '
              IF (OPEN) THEN
                  PRINT *,'open'
              ELSE 
                  PRINT *,'closed'
              END IF
          ELSE 
              PRINT *,' does not exist'
          END IF
c      END IF
*
      IF (EXISTS) THEN
          IF (.NOT. OPEN) THEN
              PRINT *,'open old ',LOGWRTFILE
              OPEN (LOGWRTUNIT,FILE=LOGWRTFILE,STATUS='OLD',
     &              IOSTAT=IOS)
              IF (IOS .NE. 0) CALL ER_OPEN (LOGWRTFILE,IOS)
          END IF
*         read down to the end of the file
          IREC = 0
          DO WHILE (.TRUE.)
              IREC = IREC + 1
              READ (LOGWRTUNIT,'(A)',END=661,IOSTAT=IOS) BUF
              IF (IOS .NE. 0) CALL ER_READ (LOGWRTFILE,IOS,IREC)
          END DO
*         (JQ) backspace over EOF marker so write appends correctly
  661     BACKSPACE (LOGWRTUNIT)
      ELSE
          PRINT *,'open new ',LOGWRTFILE
          OPEN (LOGWRTUNIT,FILE=LOGWRTFILE,STATUS='NEW',
     &          IOSTAT=IOS)
          IF (IOS .NE. 0) CALL ER_OPEN (LOGWRTFILE,IOS)
      END IF
*
      IF (WRITELOG) THEN
          CALL GETDATE
          WRITE (LOGWRTUNIT,*) LOGWRTFILE,'opened ',FITSDATE
      END IF
      RETURN
      END
