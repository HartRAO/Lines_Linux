*************************
      SUBROUTINE ER_WRITE (FILENAME,IOS)
*************************
      CHARACTER FILENAME*(*)
      INTEGER   IOS
      INTEGER   NCHAR       ! external function
*
      PRINT *,'Error ',IOS, ' writing to ',
     &        FILENAME(1:NCHAR(FILENAME))
      RETURN
      END
*********