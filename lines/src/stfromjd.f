      FUNCTION STFROMJD (MEM)
************************************************************************
*     Convert day, month, year + julian date to ST in seconds MJG
*     derived from STFROMUT
*     modified to use telescope longitude 99/03/16
*
*     calls:
*     dm2doy, juldate, nchar

      IMPLICIT  NONE
*
      INTEGER  IDOM                 ! local day of month
      INTEGER  IDOY                 ! local day of year
      INTEGER  IOS                  ! local error status
      INTEGER  IYR                  ! local full year
      INTEGER  MEM                  ! input memory in use
      INTEGER  MONTH                ! local month
      INTEGER  NCHAR                ! external function
      REAL*8   BASEOF               ! local ST at start of 1979
      REAL*8   DTIME                ! local UT / ST 
      REAL*8   OFFSET               ! local ST at start of day
      REAL*8   RATE                 ! local difference between UT and ST sec
      REAL*8   STFROMJD             ! output ST
*      
      INCLUDE 'lines.inc'
*
      DATA BASEOF/84806.882d0/, RATE/0.0027379093d0/
*
      IF (DB) PRINT *,'IN STFROMJD'
      IF (DB) PRINT *,'DATE_OBS(MEM) =',DATE_OBS(MEM)
*
*     date should be in new FITS Y2K format, with 4 digit year
      IYR = 0
      READ (DATE_OBS(MEM)(1:4),*,IOSTAT=IOS,ERR=901) IYR
  901 IF (IOS .NE. 0) PRINT 1010,IOS,'IYR'
*
      MONTH = 0
      READ (DATE_OBS(MEM)(6:7),*,IOSTAT=IOS,ERR=902) MONTH
  902 IF (IOS .NE. 0) PRINT 1010,IOS,'MONTH'
*
      IDOM = 0
      READ (DATE_OBS(MEM)(9:10),*,IOSTAT=IOS,ERR=903) IDOM
  903 IF (IOS .NE. 0) PRINT 1010,IOS,'IDOM'
*
      IF (DB) PRINT *,'IYR=',IYR,' MONTH=',MONTH,' IDOM=',IDOM
*
*     convert last two digits of year to full year
C      IF (IYM .LT. 50) THEN
C          IYR = IYM + 2000
C      ELSE IF (IYM .GT. 50 .AND. IYM .LT. 1950) THEN
C          IYR = IYM + 1900
C      END IF
*
*     call dm2doy to get day of year - checked 2000 leap year okay
      CALL DM2DOY (IYR,MONTH,IDOM,IDOY)
*
*     UT seconds since midnight
C      DTIME = DBLE(IH*60 + IM)*60D0 + DBLE(IS) + DBLE(MS)/100D0
      DTIME = (JULDATE(MEM) - IDINT(JULDATE(MEM)) +0.5D0)*86400D0
*
*     check telescope longitude against telescope name
      IF (TELESCOP(MEM)(1:4) .NE. 'Hart' .AND.
     &    ABS(TELLONG - 27.685D0) .LT. 0.1) THEN
          WRITE (BUF,*) 'warning: TELLONG=',TELLONG,
     &                  ' but TELESCOP=',TELESCOP(MEM)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
*     ST calculation, returns mean, not apparent ST
      OFFSET = 0D0
      IF (IDOY .NE. 0) THEN
*         check - year 2000 is not a leap year
*         tellong * 240 = longitude offset in degrees -> seconds
          OFFSET = DBLE((IYR-1979)*365 + (IYR-1977)/4 + IDOY - 258)
     &             * 236.55536d0 + BASEOF + TELLONG * 240D0
      END IF
*
      DTIME = DTIME + DTIME * RATE + OFFSET
      DTIME = DTIME - DBLE(IDINT(DTIME/86400.0d0)) * 86400.0d0
      STFROMJD = DTIME
      RETURN
 1010 FORMAT ('ERROR ',I4,' READING ',A)
      END
************
