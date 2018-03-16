***********************
      SUBROUTINE OFFSET (ERROR)
***********************
*     Calculate pointing corrections from measurements made at the
*     half power points of the telescope beam.
*     Also calculate corrected on source flux.
*     called by: pca
*
*     CMDP command parameters used:
*     1 = command mnemonic PC
*     2 = intensity hpbw north
*     3 = intensity hpbw south
*     4 = intensity hpbw east 
*     5 = intensity hpbw west
*     6 = intensity on source
*     7 = halfpower beamwidth
*     8 = memory in use
*
      IMPLICIT  NONE
      CHARACTER PCNSNAME*14     ! pcns description
      CHARACTER PCEWNAME*14     ! pcew description
      INTEGER   ERROR           ! output error status
      INTEGER   IDDEG           ! DEC degrees
      INTEGER   IDMIN           ! DEC minutes
      INTEGER   IOS             ! file eror check
      INTEGER   IRAHR           ! RA hours
      INTEGER   IRAMIN          ! RA minutes
      INTEGER   MEM             ! memory in use
      INTEGER   NCHAR           ! external function
      REAL*8    DECS            ! pointing corrected dec
      REAL*8    DEC50           ! dec 1950
      REAL*8    DEW             ! pointing offset east / west
      REAL*8    DNS             ! pointing offset north / south
      REAL*8    DOE             ! pointing offset on / east
      REAL*8    DON             ! pointing offset on / north
      REAL*8    DOS             ! pointing offset on / south
      REAL*8    DOW             ! pointing offset on / west
      REAL*8    DSEC            ! dec seconds
      REAL*8    DTR             ! degrees to radians conversion
      REAL*8    FOURLN2         ! constant 4 * ln(2)
      REAL*8    FRTHHPBW        ! freq * half halfpower beamwidth/MHzdeg
      REAL*8    GBIIS           ! pointing corrected BII
      REAL*8    GLIIS           ! pointing corrected LII
      REAL*8    HHPBW           ! half of the half power beamwidth
      REAL*8    HPBW            ! half power beamwidth
      REAL*8    LN2             ! constant ln(2)
      REAL*8    RAS             ! pointing corrected RA
      REAL*8    RASEC           ! ra seconds
      REAL*8    RA50            ! 1950 RA
      REAL*8    SE              ! intensity at hp east
      REAL*8    SN              ! intensity at hp north
      REAL*8    SP              ! intensity on source
      REAL*8    SPC             ! intensity on source corrected for pointing
      REAL*8    SS              ! intensity at hp south
      REAL*8    SW              ! intensity at hp west
      REAL*8    TRMS            ! theoretical rms noise in the data
      REAL*8    TWOLN2          ! constant 2 * ln(2)
      REAL*8    UTH             ! UT in hours derived from JULDATE
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in OFFSET'
      ERROR    = 0
      DTR      = 0.017453293
      FRTHHPBW = 385.               ! for hartebeesthoek
      FOURLN2  = 2.772589
      LN2      = 0.693147
      TWOLN2   = 1.386294
*
*
      IF (NCMD .GE. 6) THEN
          SN = 0D0
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) SN
  210     IF (IOS .NE. 0) ERROR = IOS
          SS = 0D0
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) SS
  220     IF (IOS .NE. 0) ERROR  = IOS
          SE = 0D0
          READ (CMDP(4),*,IOSTAT=IOS,ERR=230) SE
  230     IF (IOS .NE. 0) ERROR  = IOS
          SW = 0D0
          READ (CMDP(5),*,IOSTAT=IOS,ERR=240) SW
  240     IF (IOS .NE. 0) ERROR  = IOS
          SP = 0D0
          READ (CMDP(6),*,IOSTAT=IOS,ERR=250) SP
  250     IF (IOS .NE. 0) ERROR  = IOS
      END IF
*
      IF (NCMD .LT. 6 .OR. ERROR .NE. 0) THEN
          PRINT '(A,$)','Intensities at HP points N,S ?'
          READ '(A)',BUF
          READ (BUF,*,IOSTAT=IOS,ERR=260) SN,SS
  260     IF (IOS .NE. 0) THEN
              PRINT 1010,BUF
              ERROR = IOS
              RETURN
          END IF
*
          PRINT '(A,$)','Intensities at HP points E,W ?'
          READ '(A)',BUF
          READ (BUF,*,IOSTAT=IOS,ERR=270) SE,SW
  270     IF (IOS .NE. 0) THEN
              PRINT 1010,BUF
              ERROR = IOS
              RETURN
          END IF
*
          PRINT '(A,$)','Intensity on source ?'
          READ '(A)',BUF
          READ (BUF,*,IOSTAT=IOS,ERR=280) SP
  280     IF (IOS .NE. 0) THEN
              PRINT 1010,BUF
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      PRINT *,'N=',SN,' S=',SS,' E=',SE,' W=',SW,' On=',SP
*
      ERROR = 0
      HPBW = -1
      IF (NCMD .GE. 7) THEN
          READ (CMDP(7),*,IOSTAT=IOS,ERR=300) HPBW
  300     IF (IOS .NE. 0) ERROR = IOS
      END IF
      IF (DB) PRINT *,'HPBW=',HPBW
*
*
      MEM = MEMSET
      IF (NCMD .GE. 8) THEN
          READ (CMDP(8),*,IOSTAT=IOS,ERR=310) MEM
  310     IF (IOS .NE. 0) MEM = 0
      END IF
*
      IF (MEM .LT. 0 .OR. MEM .GT. MAXMEM) THEN
          PRINT '(A,$)',' memory in use ?'
          READ '(A)', BUF
          READ (BUF,*,IOSTAT=IOS,ERR=320) MEM
  320     IF (IOS .NE. 0 .OR. MEM .LT. 1 .OR. MEM .GT. MAXMEM) THEN
              MEM = MEMSET
          END IF
      END IF
      IF (DB) PRINT *,'MEM=',MEM
*
      IF (MEM .EQ. 0) PRINT *,'no memory specified'
*
      IF (MEM .GT. 0 .AND. NAXIS(MEM) .LT. 1) THEN
          PRINT *,'memory ',MEM,' is empty'
C let it continue for maually entered data          RETURN
      END IF
*
*
      IF (HPBW .LE. 0D0) THEN
          IF (MEM .GT. 0 .AND. RESTFREQ(MEM) .GT. 0D0) THEN
              HPBW = 2 * FRTHHPBW / (RESTFREQ(MEM) / 1D6) 
              IF (DB) PRINT *,'from MEM, HPBW=',HPBW
          ELSE 
              PRINT '(A,$)',' Halfpower beamwidth (deg) ?'
              READ  '(A)', BUF
              READ (BUF,*,IOSTAT=IOS,ERR=330) HPBW
  330         IF (IOS .NE. 0) THEN
                  PRINT 1010, BUF(1:NCHAR(BUF))
                  RETURN
              END IF        
          END IF
      END IF
      HHPBW = HPBW / 2
*
      IF (MEM .GT. 0 .AND. RESTFREQ(MEM) .GT. 0D0) THEN
          PRINT '(/A,F6.3,A,F6.1,A)',
     &      ' adopted halfpower beamwidth = ',HPBW,' deg at ',
     &      30D9/RESTFREQ(MEM),'cm'
      END IF
*
      IF (MEM .GT. 0 .AND. RMS(MEM) .GT. 0) THEN
          TRMS = RMS(MEM)
      ELSE
          TRMS = 1D-6
      END IF
*
      IF (DB) PRINT *,' TRMS=',TRMS
*
*
      SN = ABS(SN)
      SS = ABS(SS)
      SE = ABS(SE)
      SW = ABS(SW)
      SP = ABS(SP)
*     prevent log of zero
      SN = MAX (SN,TRMS)
      SS = MAX (SS,TRMS)
      SE = MAX (SE,TRMS)
      SW = MAX (SW,TRMS)
      SP = MAX (SP,TRMS)
*
*     calculate pointing offsets from half power points
      DNS = HHPBW * (LOG(SS) - LOG(SN)) / (FOURLN2)
      DEW = HHPBW * (LOG(SW) - LOG(SE)) / (FOURLN2)
*
*     calculate pointing offsets from on-source and one half-power point
      DON = HHPBW * (LOG(SP) - LOG(SN) - LN2) / (TWOLN2)
      DOS = HHPBW * (LOG(SS) - LOG(SP) + LN2) / (TWOLN2)
      DOE = HHPBW * (LOG(SP) - LOG(SE) - LN2) / (TWOLN2)
      DOW = HHPBW * (LOG(SW) - LOG(SP) + LN2) / (TWOLN2)
*
*
      IF (DB) PRINT *,'DNS=',DNS,' DEW=',DEW
      IF (DB) PRINT *,'DON=',DON,' DOS=',DOS
      IF (DB) PRINT *,'DOE=',DOE,' DOW=',DOW
*
*     find best pair of points to use on each axis
      PCNS = DNS
      PCNSNAME = 'NORTH + SOUTH'
      IF ((SS .LT. 0.15*SP .OR. SS .LT. 4*TRMS) .AND.
     &    (SS .LT. SN)) THEN
          PCNS = DON
          PCNSNAME = 'ON + NORTH'
      END IF
      IF ((SN .LT. 0.15*SP .OR. SN .LT. 4*TRMS) .AND.
     &    (SN .LT. SS)) THEN
          PCNS = DOS
          PCNSNAME = 'ON + SOUTH'
      END IF
*
      PCEW = DEW
      PCEWNAME = 'EAST + WEST'
      IF ((SW .LT. 0.15*SP .OR. SW .LT. 4*TRMS) .AND.
     &    (SW .LT. SE)) THEN
          PCEW = DOE
          PCEWNAME = 'ON + EAST'
      END IF
      IF ((SE .LT. 0.15*SP .OR. SE .LT. 4*TRMS) .AND.
     &    (SE .LT. SW)) THEN
          PCEW = DOW
          PCEWNAME = 'ON + WEST'
      END IF
*   
*     write input values to screen and logfile
      WRITE (BUF,*) ' '
      WRITE (BUF,'(A9,4X,A3,10X,A3,10X,A3,10X,A3,11X,A2)')
     &    ' Inputs: ','HPN','HPS','HPE','HPW','On'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      WRITE (BUF,*) ' '
      WRITE (BUF,'(7X,F12.6,1X,F12.6,1X,F12.6,1X,F12.6,1X,F12.6)')
     &    SN, SS, SE, SW, SP
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*   
*     write note on processing
      WRITE (BUF,*) ' 1-D Gaussian fits to HPN-On-HPS, HPE-On-HPW:'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*
*     write pointing calculated values to screen and logfile
      WRITE (BUF,*) ' '
      WRITE (BUF,'(A4,3X,A2,6X,A2,6X,A2,10X,A2,6X,A2,6X,A2,6X,A4)')
     &    ' PC:','NS','ON','OS','EW','OE','OW','HPBW'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,*) ' '
      WRITE (BUF,'(1X,3F8.1,4X,3F8.1,F10.0,A5)')
     &    DNS*1000,DON*1000,DOS*1000,
     &    DEW*1000,DOE*1000,DOW*1000, HPBW*1000
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*     calculate intensity on source corrected for pointing
      POINTCOR = EXP (LN2 * (PCNS**2 + PCEW**2) / HHPBW**2)
      SPC = SP * POINTCOR
*
      WRITE (BUF,'(3A,F8.1,A)') ' Use ',PCNSNAME,' = ',PCNS*1000,' mdeg'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,'(3A,F8.1,A)') ' Use ',PCEWNAME,' = ',PCEW*1000,' mdeg'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      IF (MEM .GT. 0) THEN
          PRINT '(/1X,A)', OBJECT(MEM)
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) OBJECT(MEM)
      END IF
*
      WRITE (BUF,'(A,F10.6)')' Amplitude correction =',POINTCOR
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,'(A,F15.6)')' peak intensity if corrected =',SPC
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,*) 'If beam offsets are in error:'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,'(A,F6.3,A)')'   New HA  offset = old + ',-PCEW,' deg'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,'(A,F6.3,A)')'   New Dec offset = old + ',-PCNS,' deg'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*     write to log the pointing corrections in tabular form
*
*     UT in decimal hours
      UTH = (JULDATE(MEM) - INT(JULDATE(MEM)) + 0.5) * 24
      IF (UTH .GT. 24D0) UTH = UTH - 24D0
*
      WRITE (BUF,'(A20,A10,A7,A8,A8,A8,A8,A9)')
     &  'Pointing-for-Object ','DateObs','UThrs','HAdeg','DCdeg',
     &  'PCHAdeg','PCDCdeg','Correct'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,'(A20,A10,F7.3,F8.3,F8.3,F8.3,F8.3,F9.6)')
     &  OBJECT(MEM), DATE_OBS(MEM)(1:10), UTH, HA(MEM), DEC(MEM), 
     &  -PCEW, -PCNS, POINTCOR
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*
      IF (MEM .GT. 0 .AND. NAXIS(MEM) .GT. 0) THEN
*
          WRITE (BUF,*) 'If source position is in error:'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*         add pointing offset to coords of current position, assumed on source
          RAS = RA(MEM) - PCEW / COS(DEC(MEM) * DTR)
          DECS = DEC(MEM) - PCNS
*
*         convert ra, dec to hms d'"
          CALL DTHMS (RAS,IRAHR,IRAMIN,RASEC)
          CALL DTDMS (DECS,IDDEG,IDMIN,DSEC)
*
*         convert the coordinates to galactic
          IF (IDINT(EQUINOX(MEM)) .EQ. 2000) THEN
*             convert 2000 equatorial to 1950
              CALL J20TOB50 (RAS,DECS,RA50,DEC50)
          ELSE IF (IDINT(EQUINOX(MEM)) .EQ. 1950) THEN
              RA50 = RAS
              DEC50 = DECS
          END IF
          IF (RA50 .NE. 0D0 .AND. DEC50 .NE. 0D0) THEN
*             convert 1950 equatorial to galactic
              CALL B50TOG (RA50,DEC50,GLIIS,GBIIS)
          END IF
*
          WRITE (BUF,'(2A,F6.1)') 
     &        ' pointing-corrected coordinates for ',
     &        OBJECT(MEM)(1:20), EQUINOX(MEM)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
          WRITE (BUF,*) ' '
          WRITE (BUF,'(4X,A,6X,A,7X,A,7X,A,7X,A,6X,A)')
     &        'H  M  S','D  M  S','RA','DEC','LII','BII'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
          WRITE (BUF,*) ' '
          WRITE (BUF,'(I5,I3,F5.1,I5,I3,F5.1,4X,
     &        F7.3,F9.3,A,F7.3,F9.3)')
     &        IRAHR, IRAMIN, RASEC, IDDEG, IDMIN, DSEC,
     &        RAS, DECS, '  G', GLIIS, GBIIS
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
      RETURN
*
 1010 FORMAT (' illegal ',A)
      END
