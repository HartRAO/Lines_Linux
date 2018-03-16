************************
      SUBROUTINE TSMIDHA (ERROR)
************************
*     correct Tsys from end-obs zenith angle to mid-obs ZA for OHIR data
*     recorded before 1990 day 353 = JD 2448244.974
*     rescales spectrum by the new Tsys
*     also adds end hour angle to housekeeping if not present
*     also recomputes rms noise if Tsys changed
*
*     CMDP command parameters used:
*     1 = command mnemonic TSHA
*     2 = memory with spectrum to correct
*
*     other subroutines called:
*     ARMS, AZALTG, B50TOJ20, J2000PR, STFROMJD
*
      IMPLICIT  NONE
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    ALTRAD          ! local altitude (radians)
      REAL*8    AZRAD           ! local azimuth (radians)
      REAL*8    DECPRC          ! local dec at date of obs (deg)
      REAL*8    DECRAD          ! local dec (radians)  
      REAL*8    DEC2000         ! local dec in J2000 (deg)
      REAL*8    DTR             ! local degrees to radians
      REAL*8    HAEND           ! local ha at end of obs (deg)
      REAL*8    HAMID           ! local ha at middle of obs (deg)
      REAL*8    HARAD           ! local ha (radians)
      REAL*8    POLYCOEF(8)     ! local dTsys vs zenith angle at 18cm
      REAL*8    RAPRC           ! local ra at date of obs (deg)
      REAL*8    RA2000          ! local ra in J2000 (deg)
      REAL*8    RLAT            ! local latitiude of telescope (deg)
      REAL*8    RLATRAD         ! local latitiude of telescope (rad)
      REAL*8    STFROMJD        ! external function
      REAL*8    STEND           ! siderial time at end of obs (s)
      REAL*8    STMID           ! siderial time at middle of obs (s)
      REAL*8    STSTART         ! siderial time at start of obs (s)
      REAL*8    TOBS            ! total duration of observation (s)
      REAL*8    TPOLYEND        ! Tsys inc at end of obs from poly (K)
      REAL*8    TPOLYMID        ! Tsys inc at middle of obs from poly (K)
      REAL*8    TSYSMID         ! Tsys calcualted at middle of obs (K)
      REAL*8    ZAEND           ! zenith angle at end of obs (deg)
      REAL*8    ZAMID           ! zenith angle at middle of obs (deg)
*
      INCLUDE 'lines.inc'
*
      DATA DTR / 0.017453293D0/
      DATA POLYCOEF /-1.3229382e-001, 2.2694708e-001,
     &               -5.2165765e-002, 4.0770704e-003,
     &               -1.4574268e-004, 2.6915343e-006,
     &               -2.4759894e-008, 9.0049655e-011/
*
      IF (DB) PRINT *,'in TSMIDHA'
      ERROR = 0
*
*
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=250) MEM
  250     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' correct Tsys in memory ?'
          READ '(A)',CMDP(2)
          READ (CMDP(1),*,IOSTAT=IOS,ERR=260) MEM
  260     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      IF (NAXIS1(MEM) .EQ. 0) THEN
          PRINT *,'no data in mem ',MEM
          ERROR = 1
          RETURN
      END IF
*
*
      IF (JULDATE(MEM) .GT. 2448244.974D0) THEN
          PRINT *,'TSHA: Cal in middle of obs, no change'
          RETURN
      END IF
*
      IF (ADDED(MEM) .GT. 1.0) THEN
          PRINT *,'Use TSHA on individual, not average spectra'
          RETURN
      END IF
C
C     calculate the hour angle of middle of the observation
C
      PRINT *,'recorded end HA =',HA(MEM)
*
*     allow for longer calibration time before 1988 day 187
*     1988 day 187 @ 7H 6M : JD 2447347.796
      IF (JULDATE(MEM) .LT. 2447347.796D0) THEN
*         observation time is:
*         twice on-source integration time in secs + calib time in secs
          TOBS = DUR(MEM) + 180D0
      ELSE
          TOBS = DUR(MEM) + 60D0
      END IF
*
*     Get ST at start of obs from julian date and dd/mm/yy in stfromjd
      STSTART = STFROMJD(MEM)
      IF (DB) PRINT *,'STSTART=',STSTART,' S'
*
      STMID = STSTART + TOBS/2D0
      STEND = STSTART + TOBS
*     Convert STMID & STEND from seconds to degrees
      STMID = STMID/240D0           ! 360 deg / 86400 sec = 240
      STEND = STEND/240D0           ! 360 deg / 86400 sec = 240
*
*     if coordinates are epoch B1950, first convert to J2000
      IF (EQUINOX(MEM) .EQ. 1950.0) THEN
          IF (DB) PRINT *,'B1950 -> J2000'
          CALL B50TOJ20 (RA(MEM), DEC(MEM), RA2000, DEC2000)
      ELSE
          RA2000 = RA(MEM)
          DEC2000 = DEC(MEM)
      END IF
      IF (DB) PRINT *,'J2000 RA, Dec=',RA2000,DEC2000
*
*     Precess coordinates from J2000 to date of observation
      IF (DB) PRINT *,'J2000 -> dateobs'
      CALL J2000PR (RA2000, DEC2000, JULDATE(MEM), RAPRC, DECPRC)
      IF (DB) PRINT *,'JD',JULDATE(MEM),' RA, Dec =',RAPRC,DECPRC
*
*     HA = ST(at middle or end) - RA(precessed)
      HAMID = STMID - RAPRC
      IF (HAMID .LT. 0.0) HAMID = HAMID + 360.0
      IF (HAMID .GT. 360.0) HAMID = HAMID - 360.0
*      
*     calculate end HA regardless of recorded end HA
      HAEND = STEND - RAPRC
      IF (HAEND .LT. 0.0) HAEND = HAEND + 360.0
      IF (HAEND .GT. 360.0) HAEND = HAEND - 360.0
*
*     if end HA was not recorded - record calculated end HA
      IF (ABS(HA(MEM)) .EQ. 0.0) HA(MEM) = HAEND
*
      PRINT *,'computed end HA=',HAEND,' mid HA=',HAMID
*
C     correct Tsys calibration from end to middle of observation
C
*     get elevation of antenna at middle and end of observation
      IF (TELESCOP(MEM)(1:4) .EQ. 'Hart') THEN
          RLAT = 334.114
      ELSE
          PRINT *,'Latitude of telescope (deg, HART=334.114) ?'
          READ *, RLAT
      END IF
*     convert inputs to azaltg to radians
      RLATRAD = RLAT * DTR
      HARAD = HAMID * DTR
      DECRAD = DECPRC * DTR
      IF (DB) PRINT *,'HA,Dec->Az,Alt'
      CALL AZALTG (RLATRAD,0D0,HARAD,DECRAD,AZRAD,ALTRAD)
      ZAMID = 90D0 - ALTRAD / DTR 
      HARAD = HAEND * DTR
      CALL AZALTG (RLATRAD,0D0,HARAD,DECRAD,AZRAD,ALTRAD)
      ZAEND = 90D0 - ALTRAD / DTR
      PRINT *,'Mid elev=',90-ZAMID,' end elev=',90-ZAEND
*
      IF (ZAEND .GT. 20D0) THEN
*         only worth doing if zenith angle greater than 20 degrees
*
*         Calculate added TSYS at end and middle of obs
          TPOLYEND = POLYCOEF(1)
          TPOLYMID = POLYCOEF(1)
          DO I = 2,8
              TPOLYEND = TPOLYEND + POLYCOEF(I)*(ZAEND**(I-1))
              TPOLYMID = TPOLYMID + POLYCOEF(I)*(ZAMID**(I-1))
          END DO
*     
*         Calculate equivalent Tsys (VID) for middle of observation
          TSYSMID = TSYS(MEM) + TPOLYMID - TPOLYEND
*
          WRITE (BUF,*,IOSTAT=IOS) 'Old Tsys=',TSYS(MEM),
     &                             ' new Tsys=',TSYSMID
          IF (IOS .NE. 0) THEN 
              PRINT *,'IOS=',IOS,'writing Tsys'
          ELSE
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          END IF
*
*         rescale the spectrum
          DO I = FIRST_CH(MEM),LAST_CH(MEM)
               YIN(MEM,I) = YIN(MEM,I)*TSYSMID/TSYS(MEM)
          END DO
*
*         update the housekeeping
          TSYS(MEM) = TSYSMID
*
*         update RMS noise in spectrum
          WRITE (CMDP(2),'(I3)') MEM
          NCMD = 2
          CALL ARMS (ERROR)
      END IF
*
      RETURN
      END
*********