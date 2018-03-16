**********************
      SUBROUTINE MEMAV (ERROR)
**********************
*     add to average spectrum in memory from another memory
*
*     CMDP command parameters used:
*     1 = command mnemonic AV
*     2 = average the data from memory
*
*     other subroutines called:
*     aitov, arms, vellim, vtori
*
      IMPLICIT  NONE
*
      INTEGER   FM              ! local from memory
      INTEGER   TM              ! local to memory
      INTEGER   I               ! local loop index
      INTEGER   J               ! local velocity index
      INTEGER   IFM             ! local index of from memory
      INTEGER   ITM             ! local index of to memory
      INTEGER   ITM2            ! local index of to memory
      INTEGER   IOS             ! file eror check
      INTEGER   ERROR           ! output error status
      INTEGER   NCHAR           ! external function
      LOGICAL   VMATCH          ! local true if to and from vels match
      REAL*8    FACTOR          ! local scaling factor
c      REAL*8    VFM             ! local velocity of from memory
      REAL*8    VFIRSTFM        ! local first velocity of from memory
      REAL*8    VFIRSTTM        ! local first velocity of to memory
      REAL*8    VLASTFM         ! local last velocity of from memory
      REAL*8    VLASTTM         ! local last velocity of to memory
      REAL*8    RJ              ! local real index of velocity
      REAL*8    TEMP            ! local temporary value of intensity
      REAL*8    VTM             ! local velocity of to memory
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in MEMAV'
      ERROR = 0
      FM = 0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) FM
  210     IF (IOS .NE. 0) THEN
              FM = 0
              IF (DB) PRINT *,'CMDP(2)=',CMDP(2)
          END IF
      END IF  
      IF (FM .EQ. 0) THEN 
          PRINT '(A,$)',' from mem ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=215) FM
  215     IF (IOS .NE. 0) THEN
              IF (DB) PRINT *,'error: CMDP(2)=',CMDP(2)
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      TM = 0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) TM
  220     IF (IOS .NE. 0) TM = 0
      END IF  
      IF (TM .EQ. 0) THEN 
          PRINT '(A,$)',' add to average spectrum in mem ?'
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=225) TM
  225     IF (IOS .NE. 0) THEN
              IF (DB) PRINT *,'error: CMDP(3)=',CMDP(3)
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      IF (FM .LT. 1 .OR. FM .GT. MAXMEM .OR.
     &    TM .LT. 1 .OR. TM .GT. MAXMEM) THEN
          PRINT *,'1 > ',FM,' or ',TM,' > ',MAXMEM
          ERROR = 1
          RETURN
      END IF
*
      IF (TRANSFRM(FM) .NE. TRANSFRM(FM)) THEN
          PRINT *,'transform: from = ',TRANSFRM(FM),' to ',TRANSFRM(TM)
          ERROR = 1
          RETURN
      END IF
*
      IF (NAXIS(FM) .LT. 1) THEN
          PRINT 250, FM
          ERROR = 1
          RETURN
      END IF
  250 format ('nothing in mem',I5)
*
      IF (NAXIS(TM) .LT. 1) THEN
*         nothing in destination memory - use copy instead
          PRINT 250, TM
          CALL MEMCOPY (ERROR)
          RETURN
      END IF
*
*     otherwise, copy bit by bit from memory FM to TM 
*
*     error checks okay      
*   
*     put most recent values into some housekeeping
      DATE_OBS(TM)  = DATE_OBS(FM)
      FROMFILE(TM)  = FROMFILE(FM)
      NUMFILE(TM)   = NUMFILE(FM)
      SCAN(TM)      = SCAN(FM)
      POLYFIT(TM)   = 1
      JULDATE(TM)   = JULDATE(FM)
      ATMCORR(TM)   = ATMCORR(FM)
      GAINCORR(TM)  = GAINCORR(FM)
      PNTCORR(TM)   = PNTCORR(FM)
      if (DB) PRINT *,'juldate=',JULDATE(TM)
*
*     see if the velocities match in channel 1 of the two spectra
C      IFM = 1      
C      CALL AITOV (FM,IFM,VFM)
C      ITM = 1
C      CALL AITOV (FM,ITM,VTM)
C      VMATCH = .FALSE.
C      IF ((VFM - VTM)/VFM .LT. 0.001D0) VMATCH = .TRUE.
*
*     velocity match check change 2001/11/12
      VMATCH = .FALSE.
      IF (ABS(CDELT1(FM) - CDELT1(TM))/CDELT1(TM) .LT. 0.01D0 .AND.
     &    ABS(CRVAL1(FM) - CRVAL1(TM)) .LT.  0.01D0) VMATCH = .TRUE.
      IF (DB) PRINT *,'velocity match between spectra = ',VMATCH
*
*     scaling factor for adding in new spectrum
      FACTOR = ADDED(FM) / ADDED(TM) * DUR(FM) / DUR(TM)
      IF (DB) PRINT *,'scaling factor = ',FACTOR
*
      DO I = FIRST_CH(TM), LAST_CH(TM)
          IF (VMATCH) THEN
*             velocities of the arrays match
              TEMP = YIN(FM,I)
          ELSE
*             the following assumes the data are evenly spaced!!          
*
*             get velocity VTM of point in average spectrum
              CALL AITOV (TM,I,VTM)
*             get real array index RJ of corresponding vel in other spectrum
              CALL XTORI (FM,VTM,RJ)
*             array index below real index
              J = RJ
*             check index range is valid
              IF (J .GE. FIRST_CH(FM) .AND. J+1 .LE. LAST_CH(FM)) THEN
*                 use linear interpolation for spectrum at RJ
                  TEMP = YIN(FM,J)*(J+1-RJ) + 
     &                   YIN(FM,J+1)*(RJ-J)
              ELSE
                  TEMP = 0D0
              END IF
          END IF
C          IF (DB) PRINT '(A,E12.6,$)','TEMP=',TEMP
*
          IF (TEMP .NE. 0D0) THEN
*             combine spectrum with average
              YIN(TM,I) = (YIN(TM,I) + TEMP*FACTOR) /
     &                         (1.0 + FACTOR)
          END IF
      END DO
*
*     update the housekeeping
*
      IF (.NOT. VMATCH) THEN
*         set new first_ch and last_ch for average spectrum
          ITM = FIRST_CH(TM)
          CALL AITOV (TM,ITM,VFIRSTTM)
          ITM = LAST_CH(TM)
          CALL AITOV (TM,ITM,VLASTTM)
          IFM = FIRST_CH(FM)
          CALL AITOV (FM,IFM,VFIRSTFM)
          IFM = LAST_CH(FM)
          CALL AITOV (FM,IFM,VLASTFM)
*
          VFIRSTTM = MAX (VFIRSTTM,VFIRSTFM)
          VLASTTM  = MIN (VLASTTM, VLASTFM)
*
*         new first channel
          CALL XTOAI (TM,VFIRSTTM,ITM)
          FIRST_CH(TM) = ITM
          CALL AITOV (TM,ITM,VFIRSTTM)
*
*         new last channel
          CALL XTOAI (TM,VLASTTM,ITM2)
          LAST_CH(TM) = ITM2
          CALL AITOV (TM,ITM2,VLASTTM)
*
*         print the new index limits and velocities
          CALL VELLIM (ITM,ITM2,VFIRSTTM,VLASTTM,1)
      END IF
*
      TSYS(TM) = (TSYS(TM) + TSYS(FM)*FACTOR) / (1.0 + FACTOR)
      DTSYS(TM) = SQRT(DTSYS(TM)**2 + (DTSYS(FM)*FACTOR)**2 / 
     &           ((1.0 + FACTOR)**2))
      ADDED(TM) = ADDED(TM) * (1.0 + FACTOR)
*
      WRITE (BUF,*) ADDED(TM),' spectra averaged in mem ',TM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*     calculate rms noise in spectrum
      WRITE (CMDP(2),*) TM
      NCMD = 2
      CALL ARMS (ERROR)
*
      RETURN
      END
*********