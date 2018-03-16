*************************
      SUBROUTINE HELPLESS (ERROR)
*************************
*     list help from helpfiles using LESS
*
*     CMDP command parameters used:
*     1 = command mnemonic HE or ?
*     2 = command for which help is wanted, or web browser name
*
*     other subroutines called:
*     locase
*
      IMPLICIT  NONE
      INTEGER   ERROR           ! output file read error code
      INTEGER   NCHAR           ! external function
      CHARACTER IOP*1           ! user option

*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in HELPLESS'
      ERROR = 0
      IOP = '2'
      IF (NCMD .LE. 1) THEN
          PRINT '(A/A,$)','start a web browser for help (1)',
     &       'or get help on screen here (2)?'     
          READ '(A)', IOP
          IF (IOP .EQ. '1') THEN 
              PRINT '(A,$)','Browser to use?'
              READ '(A)', CMDP(2)
              BUF = CMDP(2)(1:NCHAR(CMDP(2))+1)
     &              //HLPRDDIR(1:NCHAR(HLPRDDIR))
     &              //'index.html &'
          ELSE IF (IOP .EQ. '2') THEN
              WRITE (CMDP(2),'(A)') 'lines'
          ELSE 
              PRINT *,'no action'
              RETURN          
          END IF
      END IF
*
      CALL LOCASE (CMDP(2))
      IF (CMDP(2)(1:1) .EQ. '?') WRITE (CMDP(2),'(A)') 'he'
      IF (DB) PRINT *,'CDMP(2) =',CMDP(2)(1:NCHAR(CMDP(2)))
*
      IF (IOP .EQ. '2') THEN
          BUF = 'less '//HLPRDDIR(1:NCHAR(HLPRDDIR))//
     &          CMDP(2)(1:NCHAR(CMDP(2)))//'.html'
      END IF
*
      IF (DB) PRINT *,BUF(1:NCHAR(BUF))         
      CALL SYSTEM (BUF(1:NCHAR(BUF)))
*
      RETURN
      END
*********