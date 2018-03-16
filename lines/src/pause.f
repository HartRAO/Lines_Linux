**********************
      SUBROUTINE PAUSE (ERROR)
**********************
*     enable user to pause if needed eg to view plots in do loop
*
      IMPLICIT  NONE
      CHARACTER ANSWER*1
      INTEGER   ERROR
      INTEGER   IOS
      INTEGER   ISLEEPSECONDS
      INCLUDE 'lines.inc'
*
*     interpret input parameters
*
      ISLEEPSECONDS = 0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=20) ISLEEPSECONDS
   20     CONTINUE
      END IF
*
      IF (ISLEEPSECONDS .GT. 0) THEN
          PRINT *,'sleep for ',ISLEEPSECONDS,' seconds'
          CALL SLEEP(ISLEEPSECONDS)
      END IF
      
      IF (ISLEEPSECONDS .EQ. 0) THEN       
          ERROR = 0
          PRINT '(A,/,A,$)',' press "Enter" to continue, or',
     &        ' "Q" then "Enter" to quit loop or command file ' 
          READ  '(A)',ANSWER
          IF (ANSWER .EQ. 'Q' .OR. ANSWER .EQ. 'q') then
              ERROR = 1
*             set cmdrdfile to 'quit' for cmdget to close command file
*             and reset input to user
              WRITE (CMDRDFILE,'(A)') 'quit'
          END IF
      END IF
      RETURN
      END
*********