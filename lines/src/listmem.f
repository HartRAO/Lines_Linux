************************
      SUBROUTINE LISTMEM (ERROR)
************************
*     summarise contents of each memory
*
*     CMDP command parameters used:
*     1 = command mnemonic LM
*     2 = first memory to list
*     3 = last memory to list
*
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      INTEGER   ERROR           ! returned file error code
      INTEGER   FM              ! first memory to list
      INTEGER   IOS             ! i/o error status
      INTEGER   MEM             ! loop index
      INTEGER   TM              ! last memory to list
      INTEGER   MEMEMPTY        ! number of empty memories present
      INTEGER   NCHAR           ! external function
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in LISTMEM'
      ERROR = 0
*
      FM = 1
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) FM
  210     IF (IOS .NE. 0) THEN
              FM = 0
              IF (DB) PRINT *,'CMDP(2)=',CMDP(2)
          END IF
      END IF  
      IF (FM .EQ. 0) THEN 
          PRINT '(A,$)',' first memory to list ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=215) FM
  215     IF (IOS .NE. 0) THEN
              IF (DB) PRINT *,'error: CMDP(2)=',CMDP(2)
              ERROR = IOS
              RETURN
          END IF
      END IF
      FM = MAX(FM,1)
      FM = MIN(FM,MAXMEM)
*
      IF (NCMD .EQ. 2) THEN
*         if only one memory specified, list it alone
          TM = FM
      ELSE
          TM = MAXMEM
      END IF
*      
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) TM
  220     IF (IOS .NE. 0) TM = 0
      END IF  
      IF (TM .EQ. 0) THEN 
          PRINT '(A,$)',' last memory to list ?'
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=225) TM
  225     IF (IOS .NE. 0) THEN
              IF (DB) PRINT *,'error: CMDP(3)=',CMDP(3)
              ERROR = IOS
              RETURN
          END IF
      END IF
      TM = MIN(TM,MAXMEM)
      TM = MAX(TM,FM)
*
      MEMEMPTY = 0
*
      WRITE (BUF,'(2A)') 
     &      'Mem Object               Date       Scan  Freq',
     &      '   BW Pol  HA S F Py Av  PSS  BU'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      DO MEM = FM, TM
          IF (NAXIS1(MEM) .GT. 0) THEN
              WRITE (BUF,1100)
     &        MEM, OBJECT(MEM)(1:20),
     &        DATE_OBS(MEM), SCAN(MEM),
     &        IDINT(RESTFREQ(MEM)/1D6), BW(MEM)/1D6, POL(MEM),
     &        IDINT(HA(MEM)), SMOOTHED(MEM),
     &        FOLDED(MEM), POLYFIT(MEM), 
     &        IDINT(ADDED(MEM)), PSS(MEM),
     &        BUNIT(MEM)
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          ELSE 
              MEMEMPTY = MEMEMPTY + 1
          END IF
      END DO
*
      IF (MEMEMPTY .GT. 0) THEN
          WRITE (BUF,*) MEMEMPTY,' of ',(TM-FM+1),' memories are empty'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
      RETURN
*
 1100 FORMAT (I3,1X,A20,A10,I6,I6,F5.2,I4,I4,I2,I2,I3,I3,F6.1,1X,A2)
      END
*********      