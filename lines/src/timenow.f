      SUBROUTINE TIMENOW (FITSDATE,YY,MO,DD,HH,MN,SS)
*     
*     get current date and time as string FITSDATE - idea from fitsio
*
      IMPLICIT NONE
      CHARACTER DATEFILE*13     ! filename for date
      CHARACTER FITSDATE*20     ! string to hold date from file
      INTEGER IOS               ! error status
      INTEGER DD                ! output day of month
      INTEGER HH                ! output UT hours
      INTEGER MN                ! output UT minutes
      INTEGER MO                ! output month
      INTEGER SS                ! output UT seconds
      INTEGER YY                ! output year, four digit
      DATA DATEFILE /'fitsdate.data'/
*     get the linux box to return the UT in FITS Y2K format
*
      WRITE (FITSDATE,*) ' '
      YY = 0
      MO = 0
      DD = 0
      HH = 0
      MN = 0
      SS = 0
*     must include argument -u to get UT
      CALL SYSTEM ("date -u +%Y-%m-%dT%H:%M:%S > fitsdate.data")
      OPEN (50,FILE=DATEFILE,IOSTAT=IOS,ERR=101)
  101 IF (IOS .NE. 0) THEN 
          PRINT *,'error',IOS,' opening ',DATEFILE
          RETURN
      END IF
      READ (50,'(A)',IOSTAT=IOS,ERR=105) FITSDATE
  105 READ (FITSDATE,'(i4,5(1x,I2))',IOSTAT=IOS,ERR=110) 
     &    YY,MO,DD,HH,MN,SS
  110 CLOSE (50,IOSTAT=IOS,ERR=120)
  120 CALL SYSTEM ("rm fitsdate.data")
      PRINT *,'Universal Time = ',FITSDATE
c      PRINT *,'Y=',YY,' MO=',MO,' DD=',DD,' HH=',HH,' MN=',MN,' SS=',SS
      RETURN
      END
