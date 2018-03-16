************************
      SUBROUTINE JDCORR (ERROR)
************************
*     multiply spectrum by a constant to correct for relative gain changes
*     fitted by a polynomial as a function of MJD
*
*     CMDP command parameters used:
*     1 = command mnemonic JDC
*     2 = relative gain of date to apply to data, 
*       or JDP to divide by relative gain vs MJD polynomial coeffs
*     3 = offset in days to add to dates in fitting the polynomial
*         eg if MJD - 11500 was used as 'x' then enter -11500 here
*         otherwise enter 0 as a place holder if parameter 4 is used
*     4 = memory with data to be corrected
*
*     other routines called:
*     nchar
*
      IMPLICIT  NONE
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    DATECORR        ! local jd - offset for polynomial eval
      REAL*8    DOFF            ! local date offset
      REAL*8    GAIN            ! local gain curve sum of terms
      REAL*8    TERM            ! local gain curve term
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in JDCORR'
      ERROR = 0
*
      MEM = MEMSET
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=205) MEM
  205     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' correct the gain for spectrum in mem ?'
          READ '(A)',CMDP(4)
          READ (CMDP(4),*,IOSTAT=IOS,ERR=240) MEM
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
*     second parameter is not a number, may be JDP to apply gain corr
      IF (NCMD .GE. 2) CALL UPCASE (CMDP(2))
*
      DOFF = 0D0
      IF (CMDP(2)(1:3) .EQ. 'JDP' .AND. NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=230) DOFF
  230     IF (IOS .NE. 0) THEN
                ERROR = IOS
                RETURN
          END IF
      END IF
      IF (DB) PRINT *,'DOFF=',DOFF
*
      GAIN = 1D0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) GAIN
  210     IF (IOS .NE. 0) THEN
*             check gain curve coeffs have been entered
              IF (CMDP(2)(1:3) .EQ. 'JDP' .AND. NJDC .GT. 0.0) THEN
*                 calculate gain value from coeffs for this JD
                  GAIN = 0D0
                  DATECORR = JULDATE(MEM) - 2400000.5D0 + DOFF
                  IF (DB) PRINT *,'DATECORR=',DATECORR
                  DO I = 1, NJDC
                      TERM = JDC(I) * DATECORR**(I-1)
                      GAIN = GAIN + TERM
                      IF (DB) PRINT *, 'I=',I,' JDC(I)=',JDC(I),
     &                        ' TERM=',TERM,' GAIN=',GAIN
                  END DO
              END IF
          END IF
      END IF
*
      IF (GAIN .EQ. 1D0 .AND. CMDP(2)(1:3) .NE. 'JDP') THEN 
          PRINT '(A,f16.6,A,$)',' Relative gain at JD = ',
     &          JULDATE(MEM),' (Enter = 1) ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) GAIN
  220     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
*     divide the data by the relative gain of date
*
      IF (DB) PRINT *,'GAIN=',GAIN
      DO I = FIRST_CH(MEM),LAST_CH(MEM)
          YIN(MEM,I) = YIN(MEM,I) / GAIN
      END DO
*
      WRITE (BUF,*) 'mem ',MEM,' divided by ',GAIN 
      PRINT *,BUF(1:NCHAR(BUF)) 
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      RETURN
      END
*********
