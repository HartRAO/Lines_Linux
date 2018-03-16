******************************
      SUBROUTINE ALTCALC (MEM,ERROR)
******************************
*     calculate the altitude/elevation of the telescope
*
*     other subroutines called:
*     AZALTG, B50TOJ20, J2000PR, stfromjd
*
      IMPLICIT  NONE
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! input memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    ALTRAD          ! local altitude (radians)
      REAL*8    DECRAD          ! local declination (radians)
      REAL*8    AZRAD           ! local azimuth (radians)
      REAL*8    DECPRC          ! local dec at date of obs (deg)
      REAL*8    DEC2000         ! local dec in J2000 (deg)
      REAL*8    DTR             ! local degrees to radians
      REAL*8    HAMID           ! local ha at middle of obs (deg)
      REAL*8    HARAD           ! local ha (radians)
      REAL*8    RAPRC           ! local ra at date of obs (deg)
      REAL*8    RA2000          ! local ra in J2000 (deg)
      REAL*8    RLATRAD         ! local latitude of telescope (rad)
      REAL*8    STFROMJD        ! external function
      REAL*8    STMID           ! local siderial time at middle of obs (s)
      REAL*8    TOBS            ! local total duration of observation (s)
*
      INCLUDE 'lines.inc'
*
      DATA DTR / 0.017453293D0/
*
      IF (DB) PRINT *,'in ALTCALC, use mem ',MEM
      ERROR = 0
*
      IF (NAXIS1(MEM) .EQ. 0) THEN
          PRINT *,'no data in mem ',MEM
          ERROR = 1
          RETURN
      END IF
      IF (DB .AND. ADDED(MEM) .GT. 1) THEN
          PRINT *,'illegal - average spectrum'
          ERROR = 1
          RETURN
      END IF
*
*     if coordinates are epoch B1950, first convert to J2000
      IF (EQUINOX(MEM) .EQ. 1950.0) THEN
          IF (DB) PRINT *,'B1950 -> J2000'
          CALL B50TOJ20 (RA(MEM), DEC(MEM), RA2000, DEC2000)
      ELSE
          IF (DB) PRINT *,'data are J2000 ?'
          RA2000 = RA(MEM)
          DEC2000 = DEC(MEM)
      END IF
      IF (DB) PRINT *,EQUINOX(MEM),' RA, Dec =',RA(MEM),DEC(MEM) 
      IF (DB) PRINT *,'J2000 RA, Dec =',RA2000,DEC2000
*
*     Precess coordinates from J2000 to date of observation
      IF (DB) PRINT *,'J2000 -> dateobs'
      CALL J2000PR (RA2000, DEC2000, JULDATE(MEM), RAPRC, DECPRC)
      IF (DB) PRINT *,'JD',JULDATE(MEM),' RA, Dec =',RAPRC,DECPRC
*
*     allow for longer calibration time before 1988 day 187
*     1988 day 187 @ 7H 6M : JD 2447347.796
      IF (TELESCOP(MEM)(1:4) .EQ. 'Hart') THEN
          IF (JULDATE(MEM) .LT. 2447347.796D0) THEN
*             observation time is:
*             twice on-source integration time in secs + calib time in secs
              TOBS = DUR(MEM) + 180D0
          ELSE
              TOBS = DUR(MEM) + 60D0
          END IF
      ELSE
          TOBS = DUR(MEM)
      END IF
*
*     Get ST at start of obs from julian date and dd/mm/yy in stfromjd
*     and convert from seconds to degrees
      IF (DB) PRINT *,'JD -> ST'
      STMID = (STFROMJD(MEM) + TOBS/2)/240d0
*
*     calculate the hour angle (deg) of middle of the observation
      HAMID = STMID - RAPRC
      IF (HAMID .LT. 0.0) HAMID = HAMID + 360.0
      IF (HAMID .GT. 360.0) HAMID = HAMID - 360.0
      IF (DB) PRINT *,'STMID=',STMID,'deg, Mid HA =',HAMID,'deg'
*
*     check telescope latitude is correct
      IF (TELESCOP(MEM)(1:4) .NE. 'Hart' .AND.
     &    ABS(TELLAT + 25.887) .LT. 0.1) THEN
          WRITE (BUF,*) 'warning: TELLAT=',TELLAT,
     &                  ' but TELESCOP=',TELESCOP(MEM)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
*     convert inputs to azaltg to radians
      RLATRAD = TELLAT * DTR
      HARAD = HAMID * DTR
      DECRAD = DECPRC * DTR
*
*     get elevation of antenna at middle of obs
      IF (DB) PRINT *,'HA,Dec->Az,Alt'
      CALL AZALTG (RLATRAD,0D0,HARAD,DECRAD,AZRAD,ALTRAD)
*
      ALTDEG(MEM) = ALTRAD / DTR 
*
      IF (DB) THEN
          PRINT *, 'elevation = ',ALTDEG(MEM),'deg'
C          WRITE (BUF,*) 'elevation = ',ALTDEG(MEM),'deg'
C          PRINT *,BUF(1:NCHAR(BUF))
C          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
      RETURN
      END
*********
