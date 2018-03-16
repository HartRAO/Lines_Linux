***********************
      SUBROUTINE ER_EOF (FILENAME,NSPEC)
***********************
      CHARACTER FILENAME*(*)
      INTEGER   NSPEC
      INTEGER   NCHAR       ! external function
      INCLUDE 'lines.inc'
*
      WRITE (BUF,*) 'file ends after spectrum ',NSPEC,' in ',
     &        FILENAME(1:NCHAR(FILENAME))
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      RETURN
      END
********