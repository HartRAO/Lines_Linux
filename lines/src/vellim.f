***********************
      SUBROUTINE VELLIM (N1, N2, V1, V2, IUNIT)
***********************
*     list the velocity and array index limits for the spectrum
*
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      INTEGER   IOS             ! i/o status
      INTEGER   IUNIT           ! input unit to write to, 1 = screen
      INTEGER   N1              ! input array index of first channel
      INTEGER   N2              ! input array index of last channel
      REAL*8    V1              ! input velocity of first channel
      REAL*8    V2              ! input velocity of last channel
*
*     velocity units assumed to be km/s
*
      IOS = 0
      IF (IUNIT .EQ. 1) THEN
          PRINT 1000, N1, N2, V1, V2
      ELSE IF (IUNIT .GT. 1) THEN
          WRITE (IUNIT,1000,IOSTAT=IOS,ERR=900) N1, N2, V1, V2
      END IF
  900 IF (IOS .NE. 0) THEN
          PRINT *,'ERR ',IOS,' UNIT ',IUNIT
      END IF
      RETURN
*
 1000 FORMAT (27X,'start',9X,'end'/
     &        ' array index limits:',7X,I5,7X,I5/
     &        ' velocity limits:',F15.3,F12.3,' km/s')  


      END
*********