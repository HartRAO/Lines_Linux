*********************
      SUBROUTINE ARMS (ERROR)
*********************
*     calculate theoretical rms noise in spectrum
*
*     CMDP command parameters used:
*     1 = command mnemonic RMS
*     2 = memory with spectrum to recalculate RMS
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
*
      INTEGER   ERROR               ! output error status
      INTEGER   IOS                 ! file eror check
      INTEGER   MEM                 ! input memory for rms noise calc
      INTEGER   NCHAR               ! external function
      REAL*8    NEWRMS              ! new rms noise
      REAL*8    OLDRMS              ! old rms noise
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in ARMS'
      ERROR = 0
*
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) MEM
  210     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' calculate RMS noise in memory ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) MEM
  220     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      OLDRMS = RMS(MEM)
      IF (TSYS(MEM)   .GT. 0D0 .AND.
     &    BW(MEM)     .GT. 0D0 .AND.
     &    NAXIS1(MEM) .GT. 0   .AND.
     &    DUR(MEM)    .GT. 0D0 .AND.
     &    ADDED(MEM)  .GT. 0D0) THEN
*
*         1.235 = quantization loss factor for HartRAO correlator
*
          NEWRMS = 1.235 * 2.0 * TSYS(MEM) /
     &           SQRT (BW(MEM)/NAXIS1(MEM) * DUR(MEM) * ADDED(MEM))
*
*         1.36 assumes smoothed with Hamming function
*
          IF (SMOOTHED(MEM) .EQ. 1) NEWRMS = NEWRMS / SQRT(1.36)
*
          IF (FOLDED(MEM) .EQ. 1) NEWRMS = NEWRMS / SQRT(2.)
*
          IF (PSS(MEM) .GT. 0D0) NEWRMS = NEWRMS * PSS(MEM)
*
          RMS(MEM) = NEWRMS
*
          WRITE (BUF,*,IOSTAT=IOS) 'Mem ',MEM,
     &            ' RMS noise was ',OLDRMS,
     &            ' now ',RMS(MEM),BUNIT(MEM)
          IF (IOS .NE. 0) THEN
              PRINT *,'IOS =',IOS,' in ARMS'
          ELSE
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          END IF
      ELSE 
          PRINT *,'Cannot calculate RMS noise for mem ',MEM
      END IF
      RETURN
      END
*********      