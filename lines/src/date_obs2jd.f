****************************
      SUBROUTINE DATE_OBS2JD (MEM)
****************************
* convert date in fits format string to Julian Date
*
*     other subroutines called:
*     DM2DOY, LEAPYR, JD
*
      INTEGER MEM               ! input memory in use
      INTEGER IOS
      INTEGER IDAY              ! day of year
      INTEGER DD                ! output day of month
      INTEGER HH                ! output UT hours
      INTEGER MN                ! output UT minutes
      INTEGER MO                ! output month
      INTEGER MS                ! milliseconds
      INTEGER SS                ! output UT seconds
      INTEGER YY                ! output year, four digit
      REAL*8  DJULDA            ! full julian date
*
      INCLUDE 'lines.inc'
*
*     read year, month, day, hours, mins, secs UT from DATE_OBS                                          
      READ (DATE_OBS(MEM),'(i4,5(1x,I2))',IOSTAT=IOS,ERR=110)
     &    YY,MO,DD,HH,MN,SS
*
*     get day of year
      CALL DM2DOY (YY,MO,DD,IDAY)
*
*     get julian date in days
      MS = 0
      CALL JD (YY,IDAY,HH,MN,SS,MS,DJULDA)
      JULDATE(MEM) = DJULDA
  110 RETURN
      END
*********
