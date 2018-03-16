*********************
      SUBROUTINE FOLD (ERROR)
*********************
*     shift and fold frequency-shifted spectra
*
*     CMDP command parameters used:
*     1 = command mnemonic SF
*     2 = memory with spectrum to be folded
*
*     other subroutines called:
*     arms
*
      IMPLICIT NONE
*
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   IOFFSET         ! local offset in pixels
      INTEGER   IREF            ! local loop offset
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    OFFSET          ! local offset in pixels
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in FOLD'
      ERROR = 0
*
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) MEM
  210     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' shift and fold spectrum in memory ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) MEM
  220     IF (IOS .NE. 0) THEN
              ERROR = 1
              RETURN
          END IF
      END IF
*
      IF (FROFFSET(MEM) .EQ. 0) THEN
          PRINT *,'spectrum not frequency-shifted'
          ERROR = 1
          RETURN
      END IF
*
      IF (FOLDED(MEM) .EQ. 1) THEN
          PRINT *,'spectrum already folded'
          ERROR = 1
          RETURN
      END IF
*
      IF (TRANSFRM(MEM) .EQ. 1) THEN
          PRINT *,'transform, not spectrum'
          ERROR = 1
          RETURN
      END IF
*
      OFFSET = NAXIS1(MEM)*FROFFSET(MEM)/BW(MEM)
      IOFFSET = NINT(OFFSET)
*
      WRITE (BUF,*) 'folding mem ',MEM,': offset is ',IOFFSET,' pixels'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      DO I = 1, NAXIS1(MEM)
          IREF = IOFFSET + I
          IF (IREF .GE. 1 .AND. IREF .LT. NAXIS1(MEM)) THEN
              YIN(MEM,I) = (YIN(MEM,I)-YIN(MEM,IREF))/2.
              LAST_CH(MEM) = I
          END IF
      END DO
      FOLDED(MEM) = 1
      CALL ARMS (ERROR)
      RETURN
      END
*********