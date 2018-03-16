***********************
      SUBROUTINE TSYSCH (ERROR)
***********************
*     correct Tsys and dTsys eg for OHIR data
*
*     CMDP command parameters used:
*     1 = command mnemonic TS
*     2 = corrected system temperature (K)
*     3 = corrected error in system temperature (K)
*     4 = memory with spectrum to correct
*
*     other subroutines called:
*     arms
*
      IMPLICIT  NONE
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    CONST1          ! local temporary storage
      REAL*8    CONST2          ! local temporary storage
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in TSYSCH'
*
      ERROR = 0
*
      MEM = MEMSET
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=250) MEM
  250     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' correct Tsys in memory ?'
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=260) MEM
  260     IF (IOS .NE. 0) THEN
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
*
      CONST1 = 0D0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) CONST1
  210     IF (IOS .NE. 0) CONST1 = 0D0
      END IF  
*
      IF (CONST1 .EQ. 0D0) THEN
          IF (TSYS(MEM) .GT. 0D0) PRINT *,'Tsys = ',TSYS(MEM),'K'
          PRINT '(A,$)',' Corrected Tsys (K) ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) CONST1
  220     IF (IOS .NE. 0 .OR. CONST1 .LE. 0D0) THEN
              PRINT *,'illegal ',CMDP(2)(1:NCHAR(CMDP(2)))
              ERROR = IOS
              RETURN
          END IF
      END IF
      IF (DB) PRINT *,'new Tsys = ',CONST1,'K'
*
*
      CONST2 = 0D0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=230) CONST2
  230     IF (IOS .NE. 0) CONST2 = 0D0
      END IF 
* 
      IF (CONST2 .EQ. 0D0) THEN
          IF (TSYS(MEM) .GT. 0D0) PRINT *,'dTsys = ',DTSYS(MEM),'K'
          PRINT '(A,$)',' error in Tsys (K) ?'
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=240) CONST2
  240     IF (IOS .NE. 0 .OR. CONST2 .LE. 0D0) THEN
              PRINT *,'illegal ',CMDP(3)(1:NCHAR(CMDP(3)))
              ERROR = IOS
              RETURN
          END IF
      END IF
      IF (DB) PRINT *,'new dTsys = ',CONST2,'K'
*
      IF (CONST1 .GT. 0D0) THEN
*         rescale the spectrum
          DO I = FIRST_CH(MEM),LAST_CH(MEM)
              YIN(MEM,I) = YIN(MEM,I)*CONST1/TSYS(MEM)
          END DO
*
*         update the housekeeping
          TSYS(MEM) = CONST1
      ELSE
          PRINT '(A,$)',' no change to '
      END IF
*      
      IF (CONST2 .GT. 0D0) THEN
*         update the housekeeping
          DTSYS(MEM) = CONST2
      END IF
*
      WRITE (BUF,*) 'Tsys = ',TSYS(MEM),'K for mem ',MEM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*     update RMS noise in spectrum
      WRITE (CMDP(2),'(I3)') MEM
      IF (NCMD .LT. 2) NCMD = 2
      CALL ARMS (ERROR)
*
      RETURN
      END
*********