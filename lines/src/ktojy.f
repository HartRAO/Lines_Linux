**********************
      SUBROUTINE KTOJY (ERROR)
**********************
*     convert spectrum to Janskys, or change Jy value
*
*     CMDP command parameters used:
*     1 = command mnemonic JY
*     2 = new Point Source Sensitivity in Janskys per Kelvin to apply
*     3 = memory with spectrum to convert
*
*     other routines called:
*     arms, nchar
*
      IMPLICIT  NONE
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    CONST           ! local temporary storage
      REAL*8    CONST1          ! local temporary storage
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in KTOJY'
      ERROR = 0
*
      CONST = 0D0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) CONST
  210     IF (IOS .NE. 0) CONST = -1.0D0
      END IF
*
      IF (CONST .LE. 0D0) THEN 
          PRINT '(A,$)',' Janskys per Kelvin ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) CONST
  220     IF (IOS .NE. 0 .OR. CONST .LE. 0D0) THEN
              PRINT *,'illegal ',CMDP(2)(1:NCHAR(CMDP(2)))
              ERROR = IOS
              RETURN
          END IF
      END IF
      IF (DB) PRINT *,'new PSS = ',CONST
*
      MEM = MEMSET
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF
*
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' apply to spectrum in mem ?'
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=240) MEM
  240     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      IF (MEM .LT. 1 .OR. MEM .GT. MAXMEM) THEN
          PRINT *,'illegal mem=',MEM
          RETURN
      END IF
*
      IF (NAXIS1(MEM) .EQ. 0) THEN
          PRINT *,'no data in mem ',MEM
          ERROR = 1
          RETURN
      END IF
*
      IF (CONST .GT. 0D0) THEN
          IF (PSS(MEM) .EQ. 0D0) THEN
              CONST1 = CONST
          ELSE 
              CONST1 = CONST / PSS(MEM)
          END IF
          IF (DB) PRINT *,'multiply data by ',CONST1
*
          DO I = FIRST_CH(MEM),LAST_CH(MEM)
              YIN(MEM,I) = YIN(MEM,I)*CONST1
          END DO
*
*         update the housekeeping
          PSS(MEM) = CONST
          WRITE (BUNIT(MEM),'(A)') 'Jy'
          WRITE (CTYPE2(MEM),'(A)') 'FLUX_DENSITY'
      ELSE
          PRINT '(A,$)',' no change to '
      END IF
*
      WRITE (BUF,*) 'PSS = ',PSS(MEM),' Jy/K for mem ',MEM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (CMDP(2),*) MEM
      NCMD = 2
      CALL ARMS (ERROR)
*
      RETURN
      END
*********