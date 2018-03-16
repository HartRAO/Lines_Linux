************************
      SUBROUTINE GCCORR (ERROR)
************************
*     multiply spectrum by a constant
*
*     CMDP command parameters used:
*     1 = command mnemonic GC
*     2 = multiplier to apply to data, or GCP to multiply by gain curve corr
*     3 = memory with data to be multiplied
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
      REAL*8    CONST           ! local gain correction
      REAL*8    GAIN            ! local gain curve sum of terms
      REAL*8    TERM            ! local gain curve term
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in GCCORR'
      ERROR = 0
*
      MEM = MEMSET
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' correct the gain for spectrum in mem ?'
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=240) MEM
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
      CONST = 1D0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) CONST
  210     IF (IOS .NE. 0) THEN
*             second parameter is not a number, may be GCP to apply gain corr
              CALL UPCASE (CMDP(2))
*             check gain curve coeffs have been entered
              IF (CMDP(2)(1:3) .EQ. 'GCP' .AND. NGCC .GT. 0.0) THEN
*                 calculate gain curve from coeffs for this HA
                  GAIN = 0D0
                  DO I = 1, NGCC
                      TERM = GCC(I) * HA(MEM)**(I-1)
                      GAIN = GAIN + TERM
                  END DO
*                 gain correction is reciprocal of gain curve
                  CONST = 1D0 / GAIN
              END IF
          END IF
      END IF
*
      IF (CONST .EQ. 1D0 .AND. CMDP(3)(1:3) .NE. 'GCP') THEN 
          PRINT '(A,I3,A,$)',' Gain correction (>1) at HA = ',
     &          INT(HA(MEM)),' (Enter = 1) ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) CONST
  220     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
*     multiply the data by the gain correction
*
      IF (DB) PRINT *,'CONST=',CONST,' GAINCORR=',GAINCORR(MEM)
      DO I = FIRST_CH(MEM),LAST_CH(MEM)
          YIN(MEM,I) = YIN(MEM,I) * CONST / GAINCORR(MEM)
      END DO
*
      WRITE (BUF,*) 'mem ',MEM,' multiplied by ',CONST 
      PRINT *,BUF(1:NCHAR(BUF)) 
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*     store pointing correction in housekeeping
      GAINCORR(MEM) = CONST
      IF (DB) PRINT *,'GAINCORR=',GAINCORR(MEM)
*
      RETURN
      END
*********
