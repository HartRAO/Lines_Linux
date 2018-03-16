**********************
      SUBROUTINE PAUSE (ERROR)
**********************
*     enable user to pause if needed eg to view plots in do loop
*
      IMPLICIT  NONE
      CHARACTER ANSWER*1
      INTEGER   ERROR
      INCLUDE 'lines.inc'
*
      ERROR = 0
      PRINT '(A,/,A,$)',' press "Enter" to continue, or',
     &        ' "Q" then "Enter" to quit loop or command file ' 
      READ  '(A)',ANSWER
      IF (ANSWER .EQ. 'Q' .OR. ANSWER .EQ. 'q') then
          ERROR = 1
*         set cmdrdfile to 'quit' for cmdget to close command file
*         and reset input to user
          WRITE (CMDRDFILE,'(A)') 'quit'
      END IF
      RETURN
      END
*********