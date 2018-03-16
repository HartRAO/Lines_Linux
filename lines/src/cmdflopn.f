*************************
      SUBROUTINE CMDFLOPN (ERROR)
*************************
*     open input command file
*
*     called by:
*     cmdselct
*     
*     calls:
*     er_exist, er_open, nchar
*
*     CMDP command parameters used:
*     1 = command mnemonic INP
*     2 = name of command file
*
      IMPLICIT NONE
      INTEGER  ERROR            ! output error code
      INTEGER  IFCHAR           ! number of characters in name
      INTEGER  IOS              ! i/o error status
      INTEGER  NCHAR            ! external function
      LOGICAL  EXISTS           ! file existence
*      
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in CMDFLOPN'
      ERROR = 0
*
      IF (NCMD .GE. 2) THEN
          CMDRDFILE = CMDP(2)
      ELSE
          PRINT '(A,$)',' command file to read from ?'
          READ  '(A)', CMDRDFILE
      END IF
*
*
      INQUIRE (FILE=CMDRDFILE,EXIST=EXISTS)
*
      IF (.NOT. EXISTS) THEN
          IFCHAR = NCHAR(CMDRDFILE)
          CALL ER_EXIST (CMDRDFILE)
          ERROR = 1
          WRITE (CMDRDFILE,'(A)') 'user'
          GO TO 999
      END IF
*
*
      OPEN (CMDRDUNIT,FILE=CMDRDFILE,STATUS='OLD',
     &      ERR=910,IOSTAT=IOS)
*
*     try rewinding the command file if used before
      REWIND (CMDRDUNIT,IOSTAT=IOS,ERR=910)
*
  910 IF (IOS .NE. 0) THEN
          CALL ER_OPEN (CMDRDFILE,IOS)
          ERROR = IOS
          WRITE (CMDRDFILE,'(A)') 'user'
      END IF
*
  999 CONTINUE
      WRITE (BUF,*) 'Input from ',CMDRDFILE(1:NCHAR(CMDRDFILE))
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      RETURN
      END
*********
