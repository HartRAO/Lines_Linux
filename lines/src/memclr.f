***********************
      SUBROUTINE MEMCLR (ERROR)
***********************
*     clear all parameters in a memory
*
*     CMDP command parameter usage:
*     1 = command mnemonic CLR
*     2 = memory to clear, or ALL memories
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      INTEGER   TM              ! local memory to clear
      INTEGER   TM1             ! local loop start memory to clear
      INTEGER   TM2             ! local loop end memory to clear
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! file eror check
      INTEGER   ERROR           ! output error status
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in MEMCLR'
      ERROR = 0
*
*     note the following is not great code!
*     default to clearing the set memory if no parameters are given
      TM = MEMSET
      TM1 = TM
      TM2 = TM
*
      IF (NCMD .GE. 2) THEN
          CALL UPCASE (CMDP(2))
          IF (CMDP(2)(1:1) .EQ. 'A') THEN
*             clear all memories
              TM1 = 1
              TM2 = MAXMEM
          ELSE
              READ (CMDP(2),*,IOSTAT=IOS,ERR=210) TM
  210         IF ((IOS .NE. 0) .OR. 
     &            (TM .LT. 1) .OR. (TM .GT. MAXMEM)) THEN
                  PRINT *,'illegal ', CMDP(2)
                  ERROR = 1
                  RETURN
              END IF
*             clear a single memory
              TM1 = TM
              TM2 = TM
          END IF
      END IF
*
      DO TM = TM1,TM2
          IF (DB) PRINT *,'clear mem ',TM
*
*         clear memory, 
*         but preset non-essential housekeeping in case not present
*   
          WRITE (BUNIT(TM),*) ' '
          WRITE (COORDSYS(TM),*) ' '
          WRITE (CTYPE1(TM),*) ' '
          WRITE (CTYPE2(TM),*) ' '
          WRITE (DATE(TM),*) ' '
          WRITE (DATE_OBS(TM),*) ' '
          WRITE (FROMFILE(TM),*) ' '
          WRITE (OBJECT(TM),*) ' '
          WRITE (OBSERVER(TM),*) ' '
          WRITE (POSITION(TM),*) ' '
          WRITE (TCALUNIT(TM),*) ' '
          WRITE (TUNIT1(TM),*) ' '
          WRITE (TELESCOP(TM),*) ' '
*
          CRPIX1(TM)    = 0
          FIRST_CH(TM)  = 0
          FOLDED(TM)    = 0
          IXSPACING(TM) = 0
          LAST_CH(TM)   = 0
          NAXIS(TM)     = 0
          NAXIS1(TM)    = 0
          NUMFILE(TM)   = 0
          POL(TM)       = 0
          POLYFIT(TM)   = 0
          SCAN(TM)      = 0
          SMOOTHED(TM)  = 0
          TRANSFRM(TM)  = 0
*
          ADDED(TM)     = 0
          ALTDEG(TM)    = -1
          ATMCORR(TM)   = 1.0
          BW(TM)        = 0
          CDELT1(TM)    = 0
          CRVAL1(TM)    = 0
          DEC(TM)       = 0
          DECDOWN(TM)   = 0
          DTSYS(TM)     = 0
          DUR(TM)       = 0
          EQUINOX(TM)   = 0
          FROFFSET(TM)  = 0
          GAINCORR(TM)  = 1.0
          GBII(TM)      = 0
          GBIIDOWN(TM)  = 0
          GLII(TM)      = 0
          GLIIDOWN(TM)  = 0
          HA(TM)        = 0
          JULDATE(TM)   = 0
          PNTCORR(TM)   = 1.0
          PSS(TM)       = 0
          RA(TM)        = 0
          RADOWN(TM)    = 0
          RESTFREQ(TM)  = 0
          RMS(TM)       = 0
          SPVLSR(TM)    = 0
          TCAL(TM)      = 0
          TCER(TM)      = 0
          TCONT(TM)     = 0
          TSYS(TM)      = 0
*
          DO I = 1, MAXDATA
              XIN(TM,I) = 0D0
              YIN(TM,I) = 0D0
          END DO
      END DO
*
      RETURN
      END
*********      