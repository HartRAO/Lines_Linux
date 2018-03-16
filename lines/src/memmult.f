************************
      SUBROUTINE MEMMULT (ERROR)
************************
*     multiply spectrum by a constant
*
*     CMDP command parameters used:
*     1 = command mnemonic MC
*     2 = multiplier to apply to data, or 'pc' to multiply by pointing corr
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
      REAL*8    CONST           ! local temporary storage
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in MEMMULT'
      ERROR = 0
      CONST = 1.0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) CONST
  210     IF (IOS .NE. 0) THEN
*             second parameter is not a number, may be PC to apply pc  
              CALL UPCASE (CMDP(2))
*             mod 2001/11/27 check pointcorr not zero, ie has been set
*             bug fixed 2002/01/15 - store in PNTCORR(MEM) AFTER setting MEM!
              IF (CMDP(2)(1:2) .EQ. 'PC' .AND. POINTCOR .GT. 0.0) THEN
*                 set multiplication const equal to the pointing correction
                  CONST = POINTCOR
              ELSE
                  CONST = 1.0
              END IF
          END IF
      END IF
*
      IF (CONST .EQ. 1.0 .AND. CMDP(2)(1:2) .NE. 'PC') THEN 
          PRINT '(A,$)',' multiplier (Enter = 1) ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) CONST
  220     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      MEM = MEMSET
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' multiply spectrum in mem ?'
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
      WRITE (BUF,*) 'mem ',MEM,' multiplied by ',CONST 
      PRINT *,BUF(1:NCHAR(BUF)) 
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      DO I = FIRST_CH(MEM),LAST_CH(MEM)
          YIN(MEM,I) = YIN(MEM,I)*CONST
      END DO
*
*     if applicable, store pointing correction in housekeeping
      IF (CMDP(2)(1:2) .EQ. 'PC' .AND. POINTCOR .GT. 0.0) THEN
          PNTCORR(MEM) = POINTCOR
          IF (DB) PRINT *,'PNTCORR=',PNTCORR(MEM)
      END IF
*
      RETURN
      END
*********
