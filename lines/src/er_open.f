************************
      SUBROUTINE ER_OPEN (FILENAME,IOS)
************************
*     report error opening disk file
*
      CHARACTER FILENAME*(*)
      INTEGER   IOS
      INTEGER   NCHAR           ! external function
      INCLUDE 'lines.inc'
*
      WRITE (BUF,*) 'Error ',IOS, ' opening ',FILENAME
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      RETURN
      END
*********