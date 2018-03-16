**********************
      SUBROUTINE ADPOL (ERROR)
**********************
*     add oppositely handed polarizations together to give total intensity
*
*     CMDP command parameters used:
*     1 = command mnemonic AP
*     2 = memory with data first polarization
*     3 = memory with data in orthogonal polarization
*
*     the data in the second memory is added to that in the first
*     with some validity checks
*     Tsys1 and Tsys2 are averaged
*     PSS1 and PSS2 are averaged 
*
*     other routines called:
*     nchar
*
      IMPLICIT  NONE
      INTEGER   ERROR               ! output i/o error status
      INTEGER   I                   ! local do loop parameter
      INTEGER   IEND                ! local do loop parameter
      INTEGER   IOS                 ! local i/o error status
      INTEGER   ISTART              ! local do loop parameter
      INTEGER   MEM1                ! local memory to operate on
      INTEGER   MEM2                ! local memory to operate on
      INTEGER   NCHAR               ! external function
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in ADPOL'
      ERROR = 0
      IOS = 0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=100) MEM1
          READ (CMDP(3),*,IOSTAT=IOS,ERR=100) MEM2
      END IF
  100 IF (IOS .NE. 0 .OR. NCMD .LT. 3) THEN
          PRINT'(A,$)','memory with first polarization? '
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=200) MEM1
          PRINT'(A,$)','memory with second polarization? '
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=200) MEM2
  200     IF (IOS .NE. 0) THEN
              PRINT 1000
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      IF (MEM1 .LT. 1 .OR. MEM2 .LT. 1 .OR.
     &    MEM1 .GT. MAXMEM .OR. MEM2 .GT. MAXMEM .OR.
     &    MEM1 .EQ. MEM2 .OR.
     &    NAXIS1(MEM1) .EQ. 0 .OR. NAXIS1(MEM2) .EQ. 0) THEN
          PRINT 1000
          PRINT *,'Problem with MEM1=',MEM1,' or MEM2=',MEM2
          ERROR = 1
          RETURN
      END IF
*
      ISTART = MAX (FIRST_CH(MEM1),FIRST_CH(MEM2))
      IEND   = MIN (LAST_CH(MEM1),LAST_CH(MEM2))
*
      WRITE (BUF,2000) MEM1,MEM2,ISTART,IEND
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      DO I = ISTART, IEND
          YIN(MEM1,I) = YIN(MEM1,I) + YIN(MEM2,I)
      END DO 
*
*     for consistency, average the PSS
      IF (DB) PRINT *,'PSS(',MEM1,') = ',PSS(MEM1),
     &  ' PSS(',MEM2,') = ',PSS(MEM2)
      IF (PSS(MEM1) .NE. 0.0 .AND. PSS(MEM2) .NE. 0.0) THEN
            PSS(MEM1) = (PSS(MEM1) + PSS(MEM2))/2
      END IF
      IF (DB) PRINT *,'PSS_ap = ',PSS(MEM1),' (arithmetic mean)'
*
*     for estimating noise in data, add the Tsys
*     this is used in estimating errors for the time series
      IF (DB) PRINT *,'Tsys(',MEM1,') = ',TSYS(MEM1),
     &  ' Tsys(',MEM2,') = ',TSYS(MEM2)
      IF (TSYS(MEM1) .NE. 0.0 .AND. TSYS(MEM2) .NE. 0.0) THEN
            TSYS(MEM1) = (TSYS(MEM1) + TSYS(MEM2))
      END IF
      IF (DB) PRINT *,'Tsys_ap = ',TSYS(MEM1),' (added)'
*
*     add the DTsys to average the fractional errors in Tys
*     this is used in estimating errors for time series
      IF (DB) PRINT *,'dTsys(',MEM1,') = ',DTSYS(MEM1),
     &  ' dTsys(',MEM2,') = ',dTSYS(MEM2)
      DTSYS(MEM1) = (DTSYS(MEM1) + DTSYS(MEM2))
      IF (DB) PRINT *,'dTsys_ap = ',DTSYS(MEM1), ' (added)'
*
*     add the number of spectra combined to keep rms calculation correct
      IF (DB) PRINT *,'Added(',MEM1,') = ',ADDED(MEM1),
     &  ' Added(',MEM2,') = ',ADDED(MEM2)
      ADDED(MEM1) = ADDED(MEM1) + ADDED(MEM2)
      IF (DB) PRINT *,'Added_ap = ',ADDED(MEM1), ' (added)'
*
      CALL ARMS (ERROR)
*
      RETURN
 1000 FORMAT ('illegal')
 2000 FORMAT ('Add pol in mem',I4,' to pol in mem',I4,
     &        ' from pixel',I6,' to',I6)
      END
*********

