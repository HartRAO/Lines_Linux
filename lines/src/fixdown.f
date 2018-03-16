********************************
      SUBROUTINE FIXDOWN (ERROR)
********************************
*
*     fix up right half of frequency-shifted spectra obtained prior 
*     to 1990 day 338 = JD 2448229.5
*
*     To correct frequency-shifted spectra where the spectrum is formed by
*     (up spectrum / down spectrum) - 1
*
*     CMDP command parameters used:
*     1 = command mnemonic FD
*     2 = memory with data to be multiplied
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      CHARACTER USEFDON*10      ! local start of message to user
      INTEGER   I               ! local loop index
      INTEGER   IOFFSET         ! local offset in pixels
      INTEGER   IOS             ! local error code
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    OFFSET          ! local offset in pixels
*
      INCLUDE 'lines.inc'
*
      DATA USEFDON /'Use FD on '/
*
      IF (DB) PRINT *,'in FIXDOWN'
      ERROR = 0
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)','Fix down spectrum in memory ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=240) MEM
  240     IF (IOS .NE. 0) THEN
              ERROR = IOS
              PRINT *,'ERROR ',ERROR
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
      IF (JULDATE(MEM) .GT. 2448229.5D0) THEN
          PRINT *,'FD: spectrum not affected, no change'
          RETURN
      END IF
*
      IF (ADDED(MEM) .GT. 1.0) THEN
          PRINT *,USEFDON,'individual, not average spectra'
          ERROR = 1
      END IF
*
      IF (FOLDED(MEM) .GT. 1.0) THEN
          PRINT *,USEFDON,'unfolded spectra'
          ERROR = 1
      END IF
*
      IF (FROFFSET(MEM) .EQ. 0.0) THEN
          PRINT *,USEFDON,'frequency-switched spectra'
          ERROR = 1
      END IF
*
      IF (TRANSFRM(MEM) .EQ. 1) THEN
          PRINT *,USEFDON,'spectrum, not transform'
          ERROR = 1
      END IF
*
      IF (BUNIT(MEM)(1:2) .EQ. 'Jy') THEN
          PRINT *,USEFDON,'spectra in K not Jy'
          ERROR = 1
      END IF
*
      PRINT *,'Polynomial order = ',POLYFIT(MEM)
      IF (POLYFIT(MEM) .LE. 1) THEN
          PRINT '(a)','Baseline assumed to be flat !'
      END IF
*
      IF (ERROR .NE. 0) RETURN
*
      OFFSET = NAXIS1(MEM)*FROFFSET(MEM)/BW(MEM)
      IOFFSET = NINT(OFFSET)
*
      WRITE (BUF,*)'fix down spectrum in mem ',MEM,' from pixel',
     &             IOFFSET,' to',NAXIS1(MEM)
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      DO I = IOFFSET, NAXIS1(MEM)
*         assume line is inverted in right half of spectrum
          YIN(MEM,I) = TSYS(MEM) * YIN(MEM,I) / 
     &                      (TSYS(MEM) + YIN(MEM,I))
      END DO
*
      RETURN
      END
*********