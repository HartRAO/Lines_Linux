************************
      SUBROUTINE ER_READ (FILENAME,IOS,IREC)
************************
*     report error reading from a disk file
*
      CHARACTER FILENAME*(*)
      INTEGER   IOS, IREC
      INTEGER   NCHAR       ! external function
      INCLUDE 'lines.inc'
*
      WRITE (BUF,*) 'Error ',IOS,' reading rec ',IREC,' in file ',
     &        FILENAME(1:NCHAR(FILENAME))
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      RETURN
      END
*********