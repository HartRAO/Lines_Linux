*************************
      SUBROUTINE HELPWEB (ERROR)
*************************
*     list help from helpfiles using LESS
*
*     CMDP command parameters used:
*     1 = command mnemonic HE or ?
*     2 = web browser to start
*
*     other subroutines called:
*     locase
*
      IMPLICIT  NONE
      INTEGER   ERROR           ! output file read error code
      INTEGER   NCHAR           ! external function
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in HELPWEB'
      ERROR = 0
      IF (NCMD .LE. 1) THEN
          PRINT '(A,$)','Browser to use for help?'
          READ '(A)', CMDP(2)
          CALL LOCASE (CMDP(2))
      END IF
*
      IF (DB) PRINT *,'CDMP(2) =',CMDP(2)(1:NCHAR(CMDP(2)))
*
      BUF = CMDP(2)(1:NCHAR(CMDP(2)+1))
     &      //HLPRDDIR(1:NCHAR(HLPRDDIR))//
     &      //'index.html'
      IF (DB) PRINT *,BUF(1:NCHAR(BUF))         
*
      CALL SYSTEM (BUF(1:NCHAR(BUF)))
*
      RETURN
      END
*********