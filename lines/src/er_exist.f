************************
      SUBROUTINE ER_EXIST (FILENAME)
************************
*     report error opening disk file
*
      CHARACTER FILENAME*(*)
      INTEGER   NCHAR           ! external function
      INCLUDE 'lines.inc'
*
      WRITE (BUF,*) 'file not found:',FILENAME(1:NCHAR(FILENAME))
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      RETURN
      END
*********