      SUBROUTINE PCA (ERROR)
********************
* automatically calculate pointing correction from spectra NSEW On
* calls: offset
*
*     CMDP command parameters used:
*     1 = command mnemonic PCA
*     2 = velocity at which to calculate pointing correction
*
      IMPLICIT  NONE
      CHARACTER FRQORPOS*2      ! local frequency or position switched spec
      CHARACTER LABEL(5)*3      ! NSEWON 
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local array index of vel from VTOAI
      INTEGER   II              ! local loop index
      INTEGER   III             ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   JJJ             ! local loop index
      INTEGER   MEM1            ! local memory with N spectrum
      INTEGER   MEMON           ! local memory with On spectrum
      INTEGER   NCHAR           ! external function
      INTEGER   NOFFSET         ! local array index offset of "down" peak
      REAL*8    H(5,7)          ! local array of heights from data
      REAL*8    ROFFSET         ! local index offset of "down" spectra
      REAL*8    SE              ! intensity at hp east
      REAL*8    SN              ! intensity at hp north
      REAL*8    SP              ! intensity on source
      REAL*8    SS              ! intensity at hp south
      REAL*8    SW              ! intensity at hp west
      REAL*8    VFORH           ! local velocity at which height is wanted
      DATA LABEL/'N  ','S  ','E  ','W  ','On '/
*
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in PCA'
      ERROR = 0
*
      VFORH = 1E10
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) VFORH
  210     IF (IOS .NE. 0) ERROR = IOS
      END IF
      IF (DB) PRINT *,'input V=',VFORH
*
      IF (VFORH .EQ. 1E10) THEN
          PRINT '(2A,$)','Velocity of peak for PC (/ exits)?'
          READ '(A)', BUF
          READ (BUF,*,IOSTAT=IOS,ERR=220) VFORH
  220     IF (IOS .NE. 0) THEN
              PRINT 1010, BUF(1:NCHAR(BUF))
              RETURN
          END IF
      END IF
      IF (VFORH .EQ. 1E10) RETURN
*
*     assume first spectrum to analyse is in memory 1
*     assume sequence is N,S,E,W,On
*     use frequency offset to check if freq or position switched spectra
      MEM1 = 1
      FRQORPOS = '??'
      IF (FROFFSET(MEM1) .GT. 0.0) THEN
          PRINT *,'freq-sw spectrum in MEM ',MEM1
          IF (FOLDED(MEM1) .EQ. 0) THEN
              FRQORPOS='FS'
          ELSE
              PRINT *,'illegal: it is folded'
              RETURN
          END IF
      END IF
      IF (FROFFSET(MEM1) .EQ. 0.0) FRQORPOS='PS'
      IF (DB) PRINT *,'FRQORPOS=',FRQORPOS
*
*     get array index of velocity
      CALL XTOAI (MEM1,VFORH,I)
*     check validity of velocity
      IF (I .LT. FIRST_CH(MEM1) .OR. I .GT. LAST_CH(MEM1)) THEN
          PRINT 1010, 'velocity'
          RETURN
      END IF
      IF (DB) PRINT *,'I=',I
*
      WRITE (BUF,*) 'automated PC:'
      PRINT *,BUF(1:14)
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:14)
*
      IF (FRQORPOS .EQ. 'FS') THEN
*         get array index offset to "down" peak
          ROFFSET = NAXIS1(MEM1)*FROFFSET(MEM1)/BW(MEM1)
          NOFFSET = NINT(ROFFSET)
          MEMON = MEM1+2
*         search 3 channels on either side of specified velocity
          DO II = 1, 7, 1
*             hhpbw north
              H(1,II) = ABS(YIN(MEM1,I-4+II))
*             hhpbw south
              H(2,II) = ABS(YIN(MEM1,I-4+II+NOFFSET))
*             hhpbw east
              H(3,II) = ABS(YIN(MEM1+1,I-4+II))
*             hhpbw west
              H(4,II) = ABS(YIN(MEM1+1,I-4+II+NOFFSET))
*             on source
              H(5,II) = ABS(YIN(MEMON,I-4+II))
          END DO
       END IF
*
      IF (FRQORPOS .EQ. 'PS') THEN
          MEMON = MEM1+4
*         search 3 channels on either side of the specified velocity
          DO II = 1, 7, 1
              H(1,II) = ABS(YIN(MEM1,I-4+II))
              H(2,II) = ABS(YIN(MEM1+1,I-4+II))
              H(3,II) = ABS(YIN(MEM1+2,I-4+II))
              H(4,II) = ABS(YIN(MEM1+3,I-4+II))
              H(5,II) = ABS(YIN(MEMON,I-4+II))
          END DO
      END IF
      IF (DB) THEN
          DO III = 1,5
              PRINT *,LABEL(III),'H(I,J)=',(H(III,JJJ),JJJ=1,7)
          END DO
      END IF
*
*     for each peak, find the maximum over the 7 channels
      SN=H(1,1)
      SS=H(2,1)
      SE=H(3,1)
      SW=H(4,1)
      SP=H(5,1)
      DO II = 2,7
          SN = MAX(SN,H(1,II))
          SS = MAX(SS,H(2,II))
          SE = MAX(SE,H(3,II))
          SW = MAX(SW,H(4,II))
          SP = MAX(SP,H(5,II))
      END DO
      IF (DB) PRINT *,'N,S,E,W,On=',SN,SS,SE,SW,SP
*
*     write parameters to CMDP for call to calculate pointing correction
      WRITE (CMDP(2),*) SN
      WRITE (CMDP(3),*) SS
      WRITE (CMDP(4),*) SE
      WRITE (CMDP(5),*) SW
      WRITE (CMDP(6),*) SP
*     let offset find half power beamwidth from the housekeeping
      WRITE (CMDP(7),*) '??'
      WRITE (CMDP(8),*) MEMON
      NCMD = 8
*
*     calculate the pointing correction
      CALL OFFSET (ERROR)
*
      RETURN
 1010 FORMAT (' illegal ',A)
      END
