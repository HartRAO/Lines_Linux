*************************
      SUBROUTINE NCOMBINE (ERROR)
*************************
*     combine two bandpasses from NCCS spectra 
*     for both frequency- and position-switched spectra
*
*     CMDP command parameters used:
*     1 = command mnemonic CMB
*     2 = memory with first bandpass (with signal for position-switched)
*     3 = memory with second bandpass (ref bandpass for position switched)
*     4 = memory to write resultant spectrum to 
*
*     other subroutines called:
*     arms
*
      IMPLICIT NONE
*
      LOGICAL   FIRST           ! T = first valid channel when folding
      LOGICAL   SIG_LOWER       ! T = signal vlsr in left half of sig bandpass
      LOGICAL   REF_LOWER       ! T = signal vlsr in left half of ref bandpass
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local loop index
      INTEGER   ICHAN_VLSR_SIG  ! signal channel with Vlsr of signal centred
      INTEGER   ICHAN_VLSR_REF  ! ref channel with Vlsr of signal centred
      INTEGER   ICHAR           ! local character index
      INTEGER   IOS             ! local file error check
      INTEGER   IOFFSET         ! local offset in pixels
      INTEGER   IREF            ! local loop offset
      INTEGER   MEM1            ! local memory with signal bandpass
      INTEGER   MEM2            ! local memory with reference bandpass
      INTEGER   MEM3            ! local memory to write combine spectrum to
      INTEGER   N1,N2,N3,N4     ! local do loop indecies
      INTEGER   NCHAR           ! external function
      INTEGER   NHALF           ! half the number of points in the array
      REAL*8    OFFSET          ! local offset in pixels
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in FOLD'
      ERROR = 0
*
*
*     set up memory with signal bandpass
*
      MEM1 = MEMSET             ! default
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) MEM1
  210     IF (IOS .NE. 0) MEM1 = 0
      END IF  
      IF (MEM1 .EQ. 0) THEN 
          PRINT '(A,$)',' signal bandpass is in memory ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) MEM1
  220     IF (IOS .NE. 0) THEN
              ERROR = 1
              RETURN
          END IF
      END IF
*
      IF (MEM1 .LE. 0 .OR. MEM1 .GT. MAXMEM) THEN
          PRINT *,'illegal MEM1=',MEM1
          ERROR = 1
          RETURN
      END IF
*
      IF (NAXIS(MEM1) .LT. 1) THEN
          PRINT *,'nothing in mem ',MEM1
          ERROR = 1
          RETURN
      END IF
*
      IF (FOLDED(MEM1) .EQ. 1) THEN
          PRINT *,'spectrum already folded'
          ERROR = 1
          RETURN
      END IF
*
*
*     set up memory with reference bandpass
*
      MEM2 = MEMSET + 1         ! default
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=230) MEM2
  230     IF (IOS .NE. 0) MEM2 = 0
      END IF  
      IF (MEM2 .EQ. 0) THEN 
          PRINT '(A,$)',' reference bandpass is in memory ?'
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=240) MEM2
  240     IF (IOS .NE. 0) THEN
              ERROR = 1
              RETURN
          END IF
      END IF
*
      IF (MEM2 .LE. 0 .OR. MEM2 .GT. MAXMEM) THEN
          PRINT *,'illegal MEM2=',MEM2
          ERROR = 1
          RETURN
      END IF
*
      IF (NAXIS(MEM2) .LT. 1) THEN
          PRINT *,'nothing in mem ',MEM2
          ERROR = 1
          RETURN
      END IF
*
      IF (FOLDED(MEM2) .EQ. 1) THEN
          PRINT *,'spectrum already folded'
          ERROR = 1
          RETURN
      END IF
*
*
*     set up destination memory for combined spectrum
*
      MEM3 = MEMSET         ! default
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=250) MEM3
  250     IF (IOS .NE. 0) MEM3 = 0
      END IF  
      IF (MEM3 .EQ. 0) THEN 
          PRINT '(A,$)',' destination for memory combined spectrum ?'
          READ '(A)',CMDP(4)
          READ (CMDP(4),*,IOSTAT=IOS,ERR=260) MEM3
  260     IF (IOS .NE. 0) THEN
              ERROR = 1
              RETURN
          END IF
      END IF
*
      IF (MEM3 .LE. 0 .OR. MEM3 .GT. MAXMEM) THEN
          PRINT *,'illegal MEM3=',MEM3
          ERROR = 1
          RETURN
      END IF
*
      IF (DB) PRINT *,'MEM1=',MEM1,' MEM2=',MEM2,' MEM3=',MEM3      
*
*     if the destination memory is not the signal memory,
*     then copy the signal memory to it      
*
      IF (MEM3 .NE. MEM1) THEN
          WRITE (CMDP(2),*) MEM1
          WRITE (CMDP(3),*) MEM3
          IF (NCMD .LT. 3) NCMD = 3
          CALL MEMCOPY (ERROR)
      END IF
*
*     coordinates of reference spectrum become 'down' coords of combined spectrum
*     added 2007/10/29
      RADOWN(MEM3)   = RA(MEM2)
      DECDOWN(MEM3)  = DEC(MEM2)
      GLIIDOWN(MEM3) = GLII(MEM2)
      GBIIDOWN(MEM3) = GBII(MEM2)
*
*
*     do the spectra cover the same velocity range (position-switched)
*     or not (frequency-switched)?
*
      IF (ABS(CRVAL1(MEM1)-CRVAL1(MEM2)) .LT. 0.001) THEN
          PRINT *,'Position-switched spectra'
          DO I = 1, NAXIS1(MEM3)
*             to remove gain variation across the bandpass we use:
*             yout = (ysignal/tsys_signal / yref/tsys_ref - 1) * tsys_signal
              YIN(MEM3,I) = (YIN(MEM3,I)/TSYS(MEM3) / 
     &                      (YIN(MEM2,I)/TSYS(MEM2)) - 1.0) * TSYS(MEM3)
          END DO
      ELSE 
          PRINT *,'Frequency-switched spectra'
*         is the signal in the left or right half of the spectrum?
          ICHAN_VLSR_SIG = (SPVLSR(MEM3) - CRVAL1(MEM3)) / CDELT1(MEM3)
          SIG_LOWER = (ICHAN_VLSR_SIG .LT. NAXIS1(MEM3)/2)
*
          ICHAN_VLSR_REF = (SPVLSR(MEM2) - CRVAL1(MEM2)) / CDELT1(MEM2)
          REF_LOWER = (ICHAN_VLSR_REF .LT. NAXIS1(MEM2)/2)
*
          IF (DB) PRINT *,'ICHAN_VLSR_SIG=',ICHAN_VLSR_SIG,
     &        ' ICHAN_VLSR_REF=',ICHAN_VLSR_REF, 
     &        ' SIG_LOWER=', SIG_LOWER, ' REF_LOWER=', REF_LOWER

          IF (SIG_LOWER .EQV. REF_LOWER) THEN
              PRINT *,'Signal and Ref both on same side of centre!'
              RETURN 
          END IF
*
          IF (SIG_LOWER) THEN
*             signal in left half of bandpass
              N1 = 1
              N2 = NAXIS1(MEM3)/2
              N3 = N2 + 1
              N4 = NAXIS1(MEM3)
          ELSE 
*             signal in right half of bandpass
              N1 = NAXIS1(MEM3)/2 + 1
              N2 = NAXIS1(MEM3)
              N3 = 1
              N4 = NAXIS1(MEM3)/2
          END IF
          IF (DB) PRINT *,'N1=',N1,' N2=',N2,' N3=',N3,' N4=',N4
*
          DO I = N1,N2
*             remove gain variation across the first half of bandpass:
*             yout = ((ysig/tsys_sig) / (yref/tsys_ref) - 1) * tsys_sig
              YIN(MEM3,I) = (YIN(MEM1,I)/TSYS(MEM1) / 
     &                      (YIN(MEM2,I)/TSYS(MEM2)) - 1.0) * TSYS(MEM1)
          END DO
*
          DO I = N3,N4
*             remove gain variation across second half of bandpass
*             yout = (1 - (ysig/tsys_sig) / (yref/tsys_ref)) * tsys_sig:
*             this version makes ref channel right way up (from * -Tsys)
              YIN(MEM3,I) = (1.0 - (YIN(MEM2,I)/TSYS(MEM2)) / 
     &                      (YIN(MEM1,I)/TSYS(MEM1))) * TSYS(MEM2)
          END DO
      END IF
*
*     integration time = sum of integration times
      DUR(MEM3) = DUR(MEM1) + DUR(MEM3)
*     system temperature = mean of system temperatures      
      TSYS(MEM3) = (TSYS(MEM1) + TSYS(MEM2)) / 2.0
*
*     update the 'position' to show sum of two positions
      ICHAR = NCHAR(POSITION(MEM3))
      WRITE (POSITION(MEM3)(ICHAR+1:),'(A1,A3)') '+',POSITION(MEM2)(1:3)
*
*     if either spectrum is at a half power point of the beam, do not fold
*     as it is to be used for pointing determination

      IF (POSITION(MEM1)(1:2) .EQ. 'HP' .OR.
     &    POSITION(MEM2)(1:2) .EQ. 'HP') THEN
          PRINT *,'Spectrum is at HP point of beam, so not folded'
          CALL ARMS (ERROR)
          RETURN
      END IF
*
*     shift and fold the 'down' spectrum into the 'up' spectrum
*     allowing for the offset not necessarily being half the bandwidth
*
      NHALF = NAXIS1(MEM3)/2
      FIRST = .TRUE.
      OFFSET = NAXIS1(MEM1)*FROFFSET(MEM1)/BW(MEM1)
      IF (DB) PRINT *,'OFFSET=',OFFSET, ' NHALF=', NHALF
      IOFFSET = INT(OFFSET)
*
      WRITE (BUF,*) 'folding mem ',MEM3,': offset is ',IOFFSET,' pixels'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*
      IF (SIG_LOWER) THEN
*        'up' signal on the left, 'down' signal on the right
          DO I = NHALF-IOFFSET+1, NHALF
              IREF = NHALF + I
              IF (DB .AND. I .EQ. NHALF-IOFFSET+1) 
     &            PRINT *,'sig_lower: I=',I,' IREF=',IREF
C              IF (IREF .GE. NHALF .AND. IREF .LE. NHALF+IOFFSET) THEN
              YIN(MEM3,I) = (YIN(MEM3,I)-YIN(MEM3,IREF))/2.0
              IF (FIRST) THEN
*                 set the number of the first valid channel
                  FIRST_CH(MEM3) = I
                  IF (DB) PRINT *,'FIRST_CH=',FIRST_CH(MEM3)
                  FIRST = .NOT. FIRST
              END IF
*             update the number of the last valid channel
              LAST_CH(MEM3) = I
C              END IF
          END DO
      END IF
*
      IF (.NOT. SIG_LOWER) THEN
          IF (DB) PRINT *,'.NOT. SIGLOWER'
*         allow for case where signal was 'up' on the right
          DO I = NHALF+1, NAXIS1(MEM3)
              IREF = I - IOFFSET
              IF (DB .AND. I .EQ. NHALF+1)
     &            PRINT *,'sig_upper I=',I,' IREF=',IREF
              IF (IREF .GE. 1 .AND. IREF .LE. NHALF) THEN
                  YIN(MEM3,I) = (YIN(MEM3,I) - YIN(MEM3,IREF))/2.0
                  IF (FIRST) THEN
*                     set the number of the first valid channel
                      FIRST_CH(MEM3) = I
                      IF (DB) PRINT *,'FIRST_CH=',FIRST_CH(MEM3)
                      FIRST = .NOT. FIRST
                  END IF
*                 update the number of the last valid channel
                  LAST_CH(MEM3) = I
              END IF
          END DO
      END IF
*
      IF (DB) PRINT *,'FIRST_CH=',FIRST_CH(MEM3),
     &                ' LAST_CH=',LAST_CH(MEM3)
      FOLDED(MEM3) = 1
      CALL ARMS (ERROR)
      RETURN
      END
*********
