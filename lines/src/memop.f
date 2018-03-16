**********************
      SUBROUTINE MEMOP (ERROR)
**********************
*     operate on data in two memories
*
*     CMDP command parameters used:
*     1 = command mnemonic OP
*     2 = memory with data to operate on
*     3 = operator + - * /
*     4 = memory with data to operate with
*
*     other routines called:
*     nchar
*
      IMPLICIT  NONE
      CHARACTER OPERATOR*2          ! local operator
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
      IF (DB) PRINT *,'in MEMOP'
      ERROR = 0
      IOS = 0
      IF (NCMD .GE. 4) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=100) MEM1
          READ (CMDP(3),'(A1)',IOSTAT=IOS,ERR=100) OPERATOR
          READ (CMDP(4),*,IOSTAT=IOS,ERR=100) MEM2
      END IF
  100 IF (IOS .NE. 0 .OR. NCMD .LT. 4) THEN
          PRINT'(A,$)','first mem ? '
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=200) MEM1
          PRINT '(A,$)','operator (+ - * /) ? '
          READ '(A1)',OPERATOR
          PRINT'(A,$)','second mem ? '
          READ '(A)',CMDP(4)
          READ (CMDP(4),*,IOSTAT=IOS,ERR=200) MEM2
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
          ERROR = 1
          RETURN
      END IF
*
      ISTART = MAX (FIRST_CH(MEM1),FIRST_CH(MEM2))
      IEND   = MIN (LAST_CH(MEM1),LAST_CH(MEM2))
*
      IF (OPERATOR .EQ. '+') THEN
          WRITE (BUF,2000) MEM1,OPERATOR,MEM2,ISTART,IEND
          DO I = ISTART, IEND
              YIN(MEM1,I) = YIN(MEM1,I) + YIN(MEM2,I)
          END DO 
*         average the PSS if summing spectra, for consistency
          IF (PSS(MEM1) .NE. 0.0 .AND. PSS(MEM2) .NE. 0.0) THEN
                PSS(MEM1) = (PSS(MEM1) + PSS(MEM2))/2
          END IF
      ELSE IF (OPERATOR .EQ. '-') THEN
          WRITE (BUF,2000) MEM1,OPERATOR,MEM2,ISTART,IEND
          DO I = ISTART, IEND
              YIN(MEM1,I) = YIN(MEM1,I) - YIN(MEM2,I)
          END DO 
      ELSE IF (OPERATOR .EQ. '*') THEN
          WRITE (BUF,2000) MEM1,OPERATOR,MEM2,ISTART,IEND
          DO I = ISTART, IEND
              YIN(MEM1,I) = YIN(MEM1,I) * YIN(MEM2,I)
          END DO 
      ELSE IF (OPERATOR .EQ. '/') THEN
          WRITE (BUF,2000) MEM1,OPERATOR,MEM2,ISTART,IEND
          DO I = ISTART, IEND
              IF (YIN(MEM2,I) .NE. 0D0) THEN
                  YIN(MEM1,I) = YIN(MEM1,I) / YIN(MEM2,I)
              ELSE
                  YIN(MEM2,I) = 0D0
                  PRINT *,'divide by 0 at I= ',I,'; set = 0'
              END IF
          END DO
      ELSE
          PRINT 1000
          ERROR = 1
          RETURN
      END IF
*
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      RETURN
 1000 FORMAT ('illegal')
 2000 FORMAT ('mem',I4,1X,A,1X,'mem',I4,' pixels',I6,' to',I6)
      END
*********

