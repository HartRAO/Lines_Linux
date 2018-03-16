************************
      SUBROUTINE GETDATE
************************
*     get current date and time as string FITSDATE - idea from fitsio
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      INTEGER IOS               ! error status
C      INTEGER YY                ! output year, four digit
C      INTEGER MO                ! output month
C      INTEGER DD                ! output day
C      INTEGER HH                ! output hours
C      INTEGER MN                ! output minutes
C      INTEGER SS                ! output seconds
      INCLUDE 'lines.inc'
*
      CALL SYSTEM ("date +%Y-%m-%dT%H:%M:%S > fitsdate.data")
      OPEN (50,FILE='fitsdate.data',IOSTAT=IOS,ERR=130)
      READ (50,'(A)',IOSTAT=IOS,ERR=130) FITSDATE
C  100 READ (FITSDATE,'(i4,5(1x,I2))',IOSTAT=IOS,ERR=110) YY,MO,DD,HH,MN,SS
      CLOSE (50,IOSTAT=IOS,ERR=120)
  120 CALL SYSTEM ("rm fitsdate.data")
      RETURN
  130 WRITE (FITSDATE,*) 'GETDATE error'
      RETURN
      END
*********
