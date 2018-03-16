******************************
      SUBROUTINE ATMOS (ERROR)
******************************
*     correct atmospheric attenuation using formula from SLAP
*
*     CMDP command parameters used:
*     1 = command mnemonic ATM
*     2 = memory with data to be multiplied
*
*     other subroutines called:
*     AZALTG, B50TOJ20, J2000PR
*
      IMPLICIT  NONE
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    ABSORB          ! local absorption, relative
      REAL*8    ALTRAD          ! local altitude (radians)
      REAL*8    ATCOR221        ! atmospheric correction at 22.1GHz
      REAL*8    ATCORNEW        ! new atmospheric correction ot be applied
      REAL*8    DTR             ! local degrees to radians
      REAL*8    TAU221          ! zenith optical depth at 22.1GHz
      REAL*8    X_Z             ! zenith angle correction factor (Rohlfs)
      REAL*8    ZDEG            ! zenith angle (degrees)
      REAL*8    ZRAD            ! zenith angle (radians)
      REAL*8    SEC_Z           ! secant (zenith angle)
*
      INCLUDE 'lines.inc'
*
      DATA DTR / 0.017453293D0/
*
      IF (DB) PRINT *,'in ATMOS'
      ERROR = 0
*
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF
*
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)','Correct for atmospheric absorption in mem ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=240) MEM
  240     IF (IOS .NE. 0) THEN
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
      IF (ADDED(MEM) .GT. 1.0) THEN
          PRINT *,'Use ATM on individual, not average spectra'
          RETURN
      END IF
*
      IF (ALTDEG(MEM) .LT. 0D0 .OR. ALTDEG(MEM) .GT. 90D0) THEN
          WRITE (BUF,*) 'illegal ALTDEG=',ALTDEG(MEM)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          ERROR = 1
          RETURN
      END IF
*
      IF (DB) PRINT *,'ATMCORR(MEM) at start =',ATMCORR(MEM)
*
      ALTRAD = ALTDEG(MEM) * DTR 
      ZDEG = (90.0D0-ALTDEG(MEM))
      ZRAD = ZDEG * DTR
      SEC_Z = 1.0D0/DCOS(ZRAD)
      PRINT *, 'Elevation =',ALTDEG(MEM),' zenith angle =', ZDEG,
     &   ' sec(z) =', SEC_Z
*
*     slap formula for atmospheric absorption, at long wavelengths
      ABSORB = 1D0 / DEXP(0.0069D0*(1D0/DSIN(ALTRAD)-1D0))
      
*     zenith optical depths at 10, 22.1 and 25GHz
*     from SKA doc
*     pwv is set using SET PWV VALUE[mm]
C      tau10  = 0.0071+0.00021*PWV
      tau221 = 0.0140+0.00780*PWV
C      tau25  = 0.0120+0.00340*PWV
                  
*  zenith angle correction - Rohlfs & Wilson ch 8 The Earth's Atmosphere
      x_z = -0.0045 +1.00672*sec_z -0.002234*sec_z*sec_z
     &      -0.0006247*sec_z*sec_z*sec_z
C      tau10  = tau10*x_z
      tau221 = tau221*x_z
C      tau25  = tau25*x_z 
                                  
*     correction for atmospheric absorption */
C      atcor10  = exp(tau10)
      atcor221 = exp(tau221)
C      atcor25  = exp(tau25)                     
*
      IF (RESTFREQ(MEM) .LT. 10E9) THEN
*         use long wavelength correction from SLAP
          ATCORNEW = 1.0/ABSORB
      END IF
      IF (RESTFREQ(MEM) .GT. 22E9 .AND. 
     &    RESTFREQ(MEM) .LT. 23E9) THEN    
*         use SKA 22.1GHz formula
          ATCORNEW = ATCOR221
      END IF
C
      WRITE (BUF,*)'multiply spectrum in mem ',MEM,' by ',ATCORNEW
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
C     if previous correction, undo it at the same time as scaling by the new correction
      DO I = FIRST_CH(MEM),LAST_CH(MEM)
          YIN(MEM,I) = YIN(MEM,I) * ATCORNEW / ATMCORR(MEM)
      END DO
*
*     record the atmospheric correction in the housekeeping
      ATMCORR(MEM) = ATCORNEW
*
      RETURN
      END
*********
