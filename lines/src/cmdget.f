***********************
      SUBROUTINE CMDGET 
***********************
*     get command from user or file
*
*     other subroutines called:
*     cmdparse, helpless
*
      IMPLICIT  NONE
      INTEGER   ERROR               ! error return
      INTEGER   IOS                 ! file i/o status 
      INTEGER   NCHAR               ! external function
*
      INCLUDE 'lines.inc'
*
*
*     get a command from user or input file
*
      IF (DB) PRINT *,'in CMDGET'
      IF (DB) PRINT *,'loop cmd number ',LOOPCMDNUM
*
  100 IF (CMDRDFILE(1:4) .EQ. 'user') THEN
*         prompt for user's command from keyboard
          IF (.NOT. GETLOOPCMD) THEN
              PRINT '(A,$)','LINES>'
          ELSE 
              PRINT '(A,$)','LOOP>'
          END IF
*         read user's command string from keyboard
          READ '(A)', CMD
      ELSE IF (CMDRDFILE(1:4) .EQ. 'quit') THEN
*         user has typed 'q' after pause; close cmdrdfile if open
*         and restart as 'user'
          GO TO 500
      ELSE 
*         read command from input disc file
          READ (CMDRDUNIT,'(A)',ERR=500,END=500) CMD
          PRINT *,'file command=',CMD(1:NCHAR(CMD))
      END IF
*
      IF (WRITELOG) WRITE (LOGWRTUNIT,'(A)') CMD(1:NCHAR(CMD))
*
*     identify parameters in command string
*
  200 IF (GETLOOPCMD .AND. 
     &    (CMD(1:3) .NE. 'END' .AND. CMD(1:3) .NE. 'end')) THEN
*
*         service a call for help immediately
*
          IF (CMD(1:2) .EQ. 'HE' .OR. CMD(1:2) .EQ. 'he' .OR.
     &        CMD(1:1) .EQ. '?') THEN
*             parse the command
              CALL CMDPARSE 
              CALL HELPLESS (ERROR)
              GO TO 100
          END IF
*
          IF (CMD(1:4) .EQ. 'QUIT' .OR. CMD(1:4) .EQ. 'quit') THEN
*             quit the loop, not the program
              LOOPCMDNUM = 0
              GETLOOPCMD = .FALSE.
              WRITE (LOOPBUF,'(A)') ' '
              PRINT *,'quit loop'
              GO TO 100
          END IF
*
*         store following commands in the loop buffer
*
          LOOPCMDNUM = LOOPCMDNUM + 1
          IF (DB) PRINT *,'store loop cmd number ',LOOPCMDNUM
          IF (LOOPCMDNUM .GE. MAXLOOPCMDS) THEN
              PRINT *,'max number of loop commands = ',MAXLOOPCMDS
              GO TO 100
          END IF
*
          LCMD = NCHAR (CMD)
          IF (LOOPCMDNUM .EQ. 1) THEN
              LOOPCMDSTRT(LOOPCMDNUM) = 1
          ELSE
              LOOPCMDSTRT(LOOPCMDNUM) = LOOPCMDEND(LOOPCMDNUM-1) + 1
          END IF
*
          LOOPCMDEND(LOOPCMDNUM) = LOOPCMDSTRT(LOOPCMDNUM) + LCMD
*
          IF (DB) PRINT *,'CMD start = ',LOOPCMDSTRT(LOOPCMDNUM),
     &                    ' end = ',LOOPCMDEND(LOOPCMDNUM)
          IF (LOOPCMDEND(LOOPCMDNUM) .GE. LOOPBUFSIZE) THEN
              PRINT *,'Loop buffer size = ',LOOPBUFSIZE,'exceeded'
              GO TO 100
          END IF
*
          WRITE (LOOPBUF(LOOPCMDSTRT(LOOPCMDNUM):LOOPCMDEND(LOOPCMDNUM))
     &           ,'(A)'),CMD(1:LCMD)
          GO TO 100
      ELSE 
*         parse the command
          CALL CMDPARSE 
      END IF
*
*
*     return to LINES to execute the command
*
      RETURN
*
*
*     end of command file encountered
*
  500 IF (.NOT. GETLOOPCMD) THEN
*         close the command read file
          CLOSE (CMDRDUNIT,IOSTAT=IOS,ERR=510)
*         if not getting loop command, reset to user input
  510     WRITE (CMDRDFILE,'(A)') 'user'
          GO TO 100
      ELSE 
*         if in do loop, terminate the loop properly
          WRITE (CMD,'(A)') 'END'
          GO TO 200
      END IF
*
      RETURN
      END
      