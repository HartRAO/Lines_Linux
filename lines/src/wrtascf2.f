*************************
      SUBROUTINE WRTASCF2 (MEM,BEGIN_COUNT,ERROR)
*************************
*     write spectrum in ascii-format file
*
*     CMDP command parameters used:
*     none
*
*     other subroutines called:
*     er_write, getdate
*
      IMPLICIT NONE
*
      CHARACTER IFMT*20         ! local write format specifier
      INTEGER   BEGIN_COUNT     ! input BEGIN keyword counter
      INTEGER   ERROR           ! output error code
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file i/o error code
      INTEGER   J               ! local print loop index
      INTEGER   JSTART          ! local print loop start index
      INTEGER   MEM             ! local memory to write from
      INTEGER   NL              ! local number of lines of data to write
      INTEGER   NCHAR           ! external function
      INTEGER   NPL             ! local number of data to write per line
*
      INCLUDE 'lines.inc'
*
      ERROR = 0
*
      IF (DB) PRINT *,'in WRTASCF2'
*
      WRITE (BUF,*) 'mem',MEM,' write to ',
     &         ASCWRTFILE(1:NCHAR(ASCWRTFILE)),
     &         ' as spectrum ',BEGIN_COUNT+1
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*     
*     write housekeeping in unblocked ascii
      BUF(1:5)   = 'BEGIN'
      WRITE (ASCWRTUNIT,'(A)') BUF(1:5)
*
      BUF(1:10)  = 'NAXIS   = '
      WRITE (BUF(11:),*) NAXIS(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10)  = 'NAXIS1  = '
      WRITE (BUF(11:),*) NAXIS1(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'BUNIT   = ','''',BUNIT(MEM)(1:NCHAR(BUNIT(MEM))),''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'OBJECT  = ','''',
     &    OBJECT(MEM)(1:NCHAR(OBJECT(MEM))),''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'POSITION= ','''',
     &    POSITION(MEM)(1:NCHAR(POSITION(MEM))),''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'TELESCOP= ','''',
     &    TELESCOP(MEM)(1:NCHAR(TELESCOP(MEM))),''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'OBSERVER= ','''',
     &    OBSERVER(MEM)(1:NCHAR(OBSERVER(MEM))),''''
              WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'DATE-OBS= ','''',
     &    DATE_OBS(MEM)(1:NCHAR(DATE_OBS(MEM))),''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
*     get the current date as a string in FITS date format
      CALL GETDATE
      WRITE (BUF(1:),'(A10,A1,A20,A1)')
     &    'DATE    = ','''',FITSDATE,''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'CTYPE1  = ','''',
     &    CTYPE1(MEM)(1:NCHAR(CTYPE1(MEM))),''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'CTYPE2  = ','''',
     &    CTYPE2(MEM)(1:NCHAR(CTYPE2(MEM))),''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'CRVAL1  = '
*     start at first point in spectrum, not FIRST_CH
      WRITE (BUF(11:),*) CRVAL1(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'CRPIX1  = '
      WRITE (BUF(11:),*) CRPIX1(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'CDELT1  = '
      WRITE (BUF(11:),*) CDELT1(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'RESTFREQ= '
      WRITE (BUF(11:),*) RESTFREQ(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'RA      = '
      WRITE (BUF(11:),*) RA(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'DEC     = '
      WRITE (BUF(11:),*) DEC(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'RADOWN  = '
      WRITE (BUF(11:),*) RADOWN(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'DECDOWN = '
      WRITE (BUF(11:),*) DECDOWN(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'EQUINOX = '
      WRITE (BUF(11:),*) EQUINOX(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'GLII    = '
      WRITE (BUF(11:),*) GLII(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'GBII    = '
      WRITE (BUF(11:),*) GBII(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'GLIIDOWN= '
      WRITE (BUF(11:),*) GLIIDOWN(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'GBIIDOWN= '
      WRITE (BUF(11:),*) GBIIDOWN(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      IF (DB) PRINT *,'juldate=',JULDATE(MEM)
      BUF(1:10) = 'JULDATE = '
      WRITE (BUF(11:),'(F16.7)') JULDATE(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'HA      = '
      WRITE (BUF(11:),*) HA(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'ALTDEG  = '
      WRITE (BUF(11:),*) ALTDEG(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'POL     = '
      WRITE (BUF(11:),*) POL(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'TCAL    = '
      WRITE (BUF(11:),*) TCAL(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      WRITE (BUF(1:),'(A10,A1,A,A1)')
     &    'TCALUNIT= ','''',TCALUNIT(MEM)(1:NCHAR(TCALUNIT(MEM))),''''
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'TSYS    = '
      WRITE (BUF(11:),*) TSYS(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'DTSYS   = '
      WRITE (BUF(11:),*) DTSYS(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'DUR     = '
      WRITE (BUF(11:),*) DUR(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'RMS     = '
      WRITE (BUF(11:),*) RMS(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'PSS     = '
      WRITE (BUF(11:),*) PSS(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'BW      = '
      WRITE (BUF(11:),*) BW(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'FROFFSET= '
      WRITE (BUF(11:),*) FROFFSET(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'SCAN    = '
      WRITE (BUF(11:),*) SCAN(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'ADDED   = '
      WRITE (BUF(11:),*) ADDED(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'FIRST_CH= '
      WRITE (BUF(11:),*) FIRST_CH(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'LAST_CH = '
      WRITE (BUF(11:),*) LAST_CH(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'POLYFIT = '
      WRITE (BUF(11:),*) POLYFIT(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'FOLDED  = '
      WRITE (BUF(11:),*) FOLDED(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'SMOOTHED= '
      WRITE (BUF(11:),*) SMOOTHED(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
      BUF(1:10) = 'TRANSFRM= '
      WRITE (BUF(11:),*) TRANSFRM(MEM)
      WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
*
*     optional housekeeping
*
      IF (TCONT(MEM) .GT. 0D0) THEN
          BUF(1:10) = 'TCONT    ='
          WRITE (BUF(11:),*) TCONT(MEM)
          WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
          BUF(1:10) = 'TCER     ='
          WRITE (BUF(11:),*) TCER(MEM)
          WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
      END IF
*
      IF (ATMCORR(MEM) .NE. 1D0) THEN
          BUF(1:10) = 'ATMCORR  ='
          WRITE (BUF(11:),*) ATMCORR(MEM)
          WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
      END IF
*
      IF (PNTCORR(MEM) .NE. 1D0) THEN
          BUF(1:10) = 'PNTCORR  ='
          WRITE (BUF(11:),*) PNTCORR(MEM)
          WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
      END IF
*
      IF (GAINCORR(MEM) .NE. 1D0) THEN
          BUF(1:10) = 'GAINCORR ='
          WRITE (BUF(11:),*) GAINCORR(MEM)
          WRITE (ASCWRTUNIT,'(A)') BUF(1:NCHAR(BUF))
      END IF
*
      BUF(1:3) = 'END'
      WRITE (ASCWRTUNIT,'(A)') BUF(1:3)
*
*     write spectrum in sets of NPL E-format numbers
      NPL = 4
      IF (NPL .EQ. 4) IFMT = '(4(E12.6,1X))'
      NL = NAXIS1(MEM) / NPL
      IF ((NL * NPL) .LT. NAXIS1(MEM)) NL = NL + 1
      DO I = 1, NL
          JSTART = (I-1)*NPL + 1
          WRITE (BUF(1:52),IFMT)
     &          (YIN(MEM,J), J = JSTART, JSTART + NPL-1)
          WRITE (ASCWRTUNIT,'(A)',IOSTAT=IOS,ERR=930) BUF(1:52)
      END DO
      RETURN
*
*
*     error writing to file
  930 CALL ER_WRITE (ASCWRTFILE,IOS)
      ERROR = 3
      RETURN
*
      END
*********
