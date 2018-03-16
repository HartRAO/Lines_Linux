**********************
      SUBROUTINE PSSSET (ERROR)
**********************
*     convert spectrum scale from Kelvins to Janskys, or change Jy value
*
C     This program converts spectra from Kelvins to Janskys,
C     using the appropriate point source sensitivity, read in by PSSF
*
*     CMDP command parameters used:
*     1 = command mnemonic PSS
*     2 = memory with spectrum to convert
*
*     other routines called:
*     arms, nchar
*
      IMPLICIT  NONE
*
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
      IF (DB) PRINT *,'in PSSSET'
      ERROR = 0
*
      IF (NPSS .LE. 1) THEN
          PRINT *, 'No PSS values were read in using PSSF'
          ERROR = 1
          RETURN
      END IF
*
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' convert spectrum in memory ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=240) MEM
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
      IF (TRANSFRM(MEM) .EQ. 1) THEN
          PRINT *,'use PSS on spectrum, not transform'
          ERROR = 1
          RETURN
      END IF
*
      ERROR = 2     ! preset in case spectrum JD not within PSS JD range
      DO I = 1, NPSS-1
          IF (JULDATE(MEM) .GE. DJDPSS(I) .AND.
     &        JULDATE(MEM) .LT. DJDPSS(I+1)) THEN
*             new point source sensitivity
              CONST1 = PSSATJD(I) + PSSLOPE(I)*(JULDATE(MEM)-DJDPSS(I))
              ERROR = 0
          END IF
      END DO
*
      IF (ERROR .EQ. 2) THEN
          WRITE (BUF,*) '*** WARNING *** Spectrum JD = ',JULDATE(MEM),
     &    ' not in PSS JD range = ',DJDPSS(1),' to ',DJDPSS(NPSS)
          PRINT *, ' '
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*          
      IF (ERROR .EQ. 0) THEN
          IF (PSS(MEM) .GT. 0D0) THEN
*             spectra had been converted to JY
              CONST2 = CONST1 / PSS(MEM)
          ELSE
              CONST2 = CONST1
          END IF
*
          DO I = FIRST_CH(MEM),LAST_CH(MEM)
              YIN(MEM,I) = YIN(MEM,I)*CONST2
          END DO
*
*         update the housekeeping
*
          WRITE (BUF,*) 'PSS was',PSS(MEM),' now',CONST1,
     &                  ' Jy/K at JD ',JULDATE(MEM),' for mem',MEM
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))

          PSS(MEM) = CONST1
          WRITE (BUNIT(MEM),'(A)') 'Jy'
      ELSE
          PRINT *,'no change'
      END IF
*
*     update the rms noise
      WRITE (CMDP(2),'(I3)') MEM
      IF (NCMD .LT. 2) NCMD = 2
      CALL ARMS (ERROR)
*
      RETURN
      END
*********
