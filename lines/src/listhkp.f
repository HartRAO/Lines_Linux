************************
      SUBROUTINE LISTHKP 
************************
*     list housekeeping to the screen
*
*     CMDP command parameters used:
*     1 = command mnemonic LH
*     2 = memory with housekeeping to list
*
*     other subroutines called:
*     dtdms, dthms
*
      IMPLICIT NONE
*
      INTEGER   IRD             ! degrees
      INTEGER   IRH             ! hours
      INTEGER   IRM             ! minutes
      INTEGER   IOS             ! file error check
      INTEGER   MEM             ! memory to list from
      INTEGER   NCHAR           ! external function
      INTEGER   NCT             ! local number of chars
      REAL*8    DECDEG          ! dec
      REAL*8    RADEG           ! ra
      REAL*8    RS              ! seconds
*
      INCLUDE 'lines.inc'
*
*     list housekeeping to screen, two items per line
*
      MEM = 0
      IF (DB) PRINT *,'MAXMEM=',MAXMEM,' MEMSET=',MEMSET
      IF (NCMD .GE. 2) THEN
          IF (DB) PRINT *,'CMDP(2)=',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=100) MEM
  100     IF (IOS .NE. 0) THEN
              MEM = 0
          END IF
      END IF
      IF (DB .AND. MEM .EQ. 0) PRINT *,'memory not specified'
      IF (MEM .LE. 0 .OR. MEM .GT. MAXMEM) MEM = MEMSET
*
      IF (NAXIS(MEM) .LT. 1) THEN
          PRINT *,'mem ',MEM,' empty'
          RETURN
      END IF
*
      PRINT *,'Housekeeping in mem ',MEM
*
*
      WRITE (BUF(1:),*)
     & 'FROMFILE= ','''',FROMFILE(MEM)(1:NCHAR(FROMFILE(MEM))),''''
      NCT = MAX(41,NCHAR(BUF)+2)
      WRITE (BUF(NCT:),*) 
     & 'RECORD ',NUMFILE(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      NCT = MIN(NCHAR(OBJECT(MEM)), BUFLEN-14)
      WRITE (BUF(1:),*) 
     & 'OBJECT  = ','''',OBJECT(MEM)(1:NCT),''''
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*) 
     & 'OBSERVER= ','''',OBSERVER(MEM)(1:NCHAR(OBSERVER(MEM))),''''
      WRITE (BUF(41:),*) 
     & 'DATE_OBS= ','''',DATE_OBS(MEM)(1:NCHAR(DATE_OBS(MEM))),''''
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)
     & 'DATE    = ','''',DATE(MEM)(1:NCHAR(DATE(MEM))),''''
      WRITE (BUF(41:),'(A,F16.6)') ' JULDATE = ',JULDATE(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'ADDED   = ',ADDED(MEM)
      WRITE (BUF(41:),*) 
     & 'TELESCOP= ','''',TELESCOP(MEM)(1:NCHAR(TELESCOP(MEM))),''''
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'SPVLSR  = ',SPVLSR(MEM),' km/s'
      WRITE (BUF(41:),*) 
     & 'POSITION= ','''',POSITION(MEM),''''
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'POL     = ',POL(MEM)
      WRITE (BUF(41:),*) 'SCAN    = ',SCAN(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'FIRST_CH= ',FIRST_CH(MEM)
      WRITE (BUF(41:),*) 'LAST_CH = ',LAST_CH(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'POLYFIT = ',POLYFIT(MEM)
      WRITE (BUF(41:),*) 'FOLDED  = ',FOLDED(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'SMOOTHED= ',SMOOTHED(MEM)
      WRITE (BUF(41:),*) 'TRANSFRM= ',TRANSFRM(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'HA      = ',HA(MEM), ' / deg'
      WRITE (BUF(41:),*) 'DUR     = ',DUR(MEM), ' / s'
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'ALTDEG  = ',ALTDEG(MEM),' / deg'
      WRITE (BUF(41:),*) 'ATMCORR = ',ATMCORR(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'PNTCORR = ',PNTCORR(MEM)
      WRITE (BUF(41:),*) 'GAINCORR= ',GAINCORR(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*) 
     & 'CTYPE1  = ','''',CTYPE1(MEM)(1:NCHAR(CTYPE1(MEM))),''''
      WRITE (BUF(41:),*) 'TUNIT1  = ','''',TUNIT1(MEM),''''
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*) 
     & 'CTYPE2  = ','''',CTYPE2(MEM)(1:NCHAR(CTYPE2(MEM))),''''
      WRITE (BUF(41:),*) 
     & 'BUNIT   = ','''',BUNIT(MEM)(1:NCHAR(BUNIT(MEM))),''''
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'NAXIS1  = ',NAXIS1(MEM)
      WRITE (BUF(41:),*) 'CRPIX1  = ',CRPIX1(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'CRVAL1  = ',CRVAL1(MEM)
      WRITE (BUF(41:),*) 'CDELT1  = ',CDELT1(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'RESTFREQ= ',RESTFREQ(MEM), ' / Hz'
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'FROFFSET= ',FROFFSET(MEM),' / Hz'
      WRITE (BUF(41:),*) 'BW      = ',BW(MEM),' / Hz'
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  
     & 'COORDSYS= ','''',COORDSYS(MEM)(1:NCHAR(COORDSYS(MEM))),''''
      WRITE (BUF(41:),*) 'EQUINOX = ',EQUINOX(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      IF (DB) PRINT *,'RA(MEM) = ',RA(MEM)
      RADEG = RA(MEM)
      CALL DTHMS (RADEG,IRH,IRM,RS)
      IF (DB) PRINT *,' RADEG=',RADEG,' H=',IRH,' M=',IRM,' S=',RS
      WRITE (BUF(1:),'(A,F8.3,A,I3,A,I2.2,A,F4.1,A)')
     & ' RA      = ',RA(MEM),' =',IRH,'h',IRM,'m',RS,'s'
      IF (DB) PRINT *,'NCHAR(BUF)=',NCHAR(BUF)
      IF (DB) PRINT '(A)',BUF(1:NCHAR(BUF))
*
      IF (DB) PRINT *,'DEC(MEM) = ',DEC(MEM)
      DECDEG = DEC(MEM)
      CALL DTDMS (DECDEG,IRD,IRM,RS)
      IF (DB) PRINT *,'DECDEG=',DECDEG,' D=',IRD,' M=',IRM,' S=',RS
      WRITE (BUF(42:),'(A,F8.3,A,I3,A,I2.2,A,F4.1,A)')
     & 'DEC     = ',DEC(MEM),' =',IRD,'d',IRM,'''',RS,'"'
      IF (DB) PRINT *,'NCHAR(BUF)=',NCHAR(BUF)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      RADEG = RADOWN(MEM)
      CALL DTHMS (RADEG,IRH,IRM,RS)
      WRITE (BUF(1:),'(A,F8.3,A,I3,A,I2.2,A,F4.1,A)')
     & ' RADOWN  = ',RADOWN(MEM),' =',IRH,'h',IRM,'m',RS,'s'
      IF (DB) PRINT *,'DECDOWN(MEM) = ',DECDOWN(MEM)
      DECDEG = DECDOWN(MEM)
      CALL DTDMS (DECDEG,IRD,IRM,RS)
      WRITE (BUF(42:),'(A,F8.3,A,I3,A,I2.2,A,F4.1,A)')
     & 'DECDOWN = ',DECDOWN(MEM),' =',IRD,'d',IRM,'''',RS,'"'
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'GLII    = ',GLII(MEM)
      WRITE (BUF(41:),*) 'GBII    = ',GBII(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'GLIIDOWN= ',GLIIDOWN(MEM)
      WRITE (BUF(41:),*) 'GBIIDOWN= ',GBIIDOWN(MEM)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      NCT = NCHAR(TCALUNIT(MEM))
      WRITE (BUF(1:),*)  'TCAL    = ',TCAL(MEM)
      WRITE (BUF(41:),*) 'TCALUNIT= ','''',
     &    TCALUNIT(MEM)(1:NCT),''''
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'TSYS    = ',TSYS(MEM), ' / ',
     &       TCALUNIT(MEM)(1:NCT)
      WRITE (BUF(41:),*) 'DTSYS   = ',DTSYS(MEM), ' / ',
     &       TCALUNIT(MEM)(1:NCT)
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),*)  'RMS     = ',RMS(MEM),' / ',
     & BUNIT(MEM)(1:NCHAR(BUNIT(MEM))) 
      WRITE (BUF(41:),*) 'PSS     = ',PSS(MEM),' / Jy/K'
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
      IF (TCONT(MEM) .GT. 0.0) THEN
          WRITE (BUF(1:),*)  'TCONT   = ',TCONT(MEM),' / K'
          WRITE (BUF(41:),*) 'TCER    = ',TCER(MEM),' / K'
          PRINT '(A)',BUF(1:NCHAR(BUF))
      END IF
*
      RETURN
      END
*********
