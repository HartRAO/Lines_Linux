*************************
      SUBROUTINE ER_CLOSE (FILENAME,IOS)
*************************
*     report error closing disk file 
*
      CHARACTER FILENAME*(*)
      INTEGER   IOS
      INTEGER   NCHAR       ! external function
      INCLUDE 'lines.inc'
*
      WRITE (BUF,*) 'Error ',IOS, ' closing ',
     &        FILENAME(1:NCHAR(FILENAME))
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      RETURN
      END
*********