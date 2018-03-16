      PROGRAM LINES
*******************
*     simple spectral line analysis program for Linux
*     M J Gaylard / HartRAO / mike@bootes.hartrao.ac.za
*
*     initialise parameters, then 
*     loop around getting user commands or processing commands in a do loop
*
*     subroutines called:
*     cmdget, cmdselct, lineinit, loopcmd, nchar
*
      IMPLICIT  NONE
      LOGICAL   QUIT            ! true to quit
      LOGICAL   STARTCMD        ! true if command line gives input file
      INTEGER   ERROR           ! returned file error code
      INTEGER   NCHAR           ! external function
*
      INCLUDE 'lines.inc'
*
      PRINT *,'LINES: spectrum and data analysis'
*
***** initialize parameters in common blocks
*
      CALL LINEINIT (ERROR)
*
*     pickup second run string (first run string is "lines")
      CALL GETARG (1, BUF)
      STARTCMD = .FALSE.
      IF (NCHAR(BUF) .GT. 1) THEN
*         2nd run string parameter is name of lines input file
          CMDP(1) = 'INP'
          CMDP(2) = BUF(1:NCHAR(BUF))
          NCMD = 2
          STARTCMD = .TRUE.
      END IF
*
***** command handling loop
*
      QUIT = .FALSE.
      DO WHILE (.NOT. QUIT)
          IF (DOLOOP) THEN
*             convert stored loop command into executable command
              CALL LOOPCMD (ERROR, QUIT)
              DOLOOP = .FALSE.
          ELSE
              IF (.NOT. STARTCMD) THEN
*                 get command from user or input file
                  CALL CMDGET
              END IF
              CALL CMDSELCT (ERROR,QUIT)
*             reset startcmd after first pass
              STARTCMD = .FALSE.
          END IF
      END DO
*
      END
*********