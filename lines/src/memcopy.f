************************
      SUBROUTINE MEMCOPY (ERROR)
************************
*     copy housekeeping and spectrum from one memory to another
*
*     CMDP command parameters used:
*     1 = command mnemonic CP
*     2 = memory to copy data from
*     3 = memory to copy data to
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      INTEGER   FM              ! local from memory
      INTEGER   TM              ! local to memory
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   NCHAR           ! external function
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in MEMCOPY'
      ERROR = 0
      FM = 0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) FM
  210     IF (IOS .NE. 0) FM = 0
      END IF  
      IF (FM .EQ. 0) THEN 
          PRINT '(A,$)',' Copy from which mem ?'
          READ *,FM
      END IF
*
      TM = 0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=211) TM
  211     IF (IOS .NE. 0) TM = 0
      END IF  
      IF (TM .EQ. 0) THEN 
          PRINT '(A,$)',' to mem ?'
          READ *,TM
      END IF
*
      IF ((FM .LT. 1) .OR. (FM .GT. MAXMEM) .OR.
     &    (TM .LT. 1) .OR. (TM .GT. MAXMEM)) THEN
          PRINT *,'error: 1 > FM=',FM,' or TM=',TM,' > ',MAXMEM
          ERROR = 1
          RETURN
      END IF
      IF (NAXIS(FM) .LT. 1) THEN
          PRINT *,'nothing in mem ',FM
          ERROR = 1
          RETURN
      END IF
*   
      BUNIT(TM)     = BUNIT(FM)
      CTYPE1(TM)    = CTYPE1(FM)
      CTYPE2(TM)    = CTYPE2(FM)
      DATE(TM)      = DATE(FM)
      DATE_OBS(TM)  = DATE_OBS(FM)
      FROMFILE(TM)  = FROMFILE(FM)
      OBJECT(TM)    = OBJECT(FM)
      OBSERVER(TM)  = OBSERVER(FM)
      POSITION(TM)  = POSITION(FM)
      TCALUNIT(TM)  = TCALUNIT(FM)
      TUNIT1(TM)    = TUNIT1(FM)
      TELESCOP(TM)  = TELESCOP(FM)
*
      CRPIX1(TM)    = CRPIX1(FM)
      FIRST_CH(TM)  = FIRST_CH(FM)
      FOLDED(TM)    = FOLDED(FM)
      IXSPACING(TM) = IXSPACING(FM)
      LAST_CH(TM)   = LAST_CH(FM)
      NAXIS(TM)     = NAXIS(FM)
      NAXIS1(TM)    = NAXIS1(FM)
      NUMFILE(TM)   = NUMFILE(FM)
      POL(TM)       = POL(FM)
      POLYFIT(TM)   = POLYFIT(FM)
      SCAN(TM)      = SCAN(FM)
      SMOOTHED(TM)  = SMOOTHED(FM)
      TRANSFRM(TM)  = TRANSFRM(FM)
*
      ADDED(TM)     = ADDED(FM)
      ALTDEG(TM)    = ALTDEG(FM)
      ATMCORR(TM)   = ATMCORR(FM)
      BW(TM)        = BW(FM)
      CDELT1(TM)    = CDELT1(FM)
      COORDSYS(TM)  = COORDSYS(FM)
      CRVAL1(TM)    = CRVAL1(FM)
      DEC(TM)       = DEC(FM)
      DECDOWN(TM)   = DECDOWN(FM)
      DTSYS(TM)     = DTSYS(FM)
      DUR(TM)       = DUR(FM)
      EQUINOX(TM)   = EQUINOX(FM)
      FROFFSET(TM)  = FROFFSET(FM)
      GAINCORR(TM)  = GAINCORR(FM)
      GBII(TM)      = GBII(FM)
      GBIIDOWN(TM)  = GBIIDOWN(FM)
      GLII(TM)      = GLII(FM)
      GLIIDOWN(TM)  = GLIIDOWN(FM)
      HA(TM)        = HA(FM)
      JULDATE(TM)   = JULDATE(FM)
      PNTCORR(TM)   = PNTCORR(FM)
      PSS(TM)       = PSS(FM)
      RA(TM)        = RA(FM)
      RADOWN(TM)    = RADOWN(FM)
      RESTFREQ(TM)  = RESTFREQ(FM)
      RMS(TM)       = RMS(FM)
      SPVLSR(TM)    = SPVLSR(FM)
      TCAL(TM)      = TCAL(FM)
      TCER(TM)      = TCER(FM)
      TCONT(TM)     = TCONT(FM)
      TSYS(TM)      = TSYS(FM)
*
      DO I = 1, MAXDATA
          XIN(TM,I) = XIN(FM,I)
          YIN(TM,I) = YIN(FM,I)
      END DO
*
      WRITE (BUF,*) 'mem ',FM,' copied to ',TM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      RETURN
      END
      