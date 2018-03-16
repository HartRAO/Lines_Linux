**********************
      SUBROUTINE OHKTOJY (ERROR)
**********************
*     convert spectrum to Janskys, or change Jy value
*
C     This program converts spectra from Kelvins to Janskys,
C     using the appropriate point source sensitivity, listed below
c        (this is the automatic mode)
C     or a PSS supplied by the user (manual mode).
c     known 18 cm OH spectra sensitivity changes :
c
c PERIOD          PSS (Jy/K)  SUBREF TILTED   FLANGE ON  CORR INPUT
c                 1666  1612
c start
c (1978 d  1 : 12H)
c (JD 2443509.000)
c to              15.8  19.0      no            yes       1.0 V NON-AGC VIDEO
c 1986 d 105 (9H 58M)
c JD 2446536.138
c to              12.8  15.4      no            yes       1.7 V      "
c 1990 d 61 (12H)
c JD 2447953.000
c to              13.4  16.1      no            no        1.7 V      "
c 1990 d 215 (12H)
c JD 2448107.000
c to              12.8  15.4      no            yes       1.7 V      "
c 1990 d 229 (12H)
c JD 2448121.000
c to              10.3  12.0      yes           yes       1.7/1.5 V  "
c 1991 d  90 (12H)
c JD 2448347.000
c to              10.3  12.0      yes           yes       1.0 V AGC VIDEO
c 1992 d  86 (12H)
c JD 2448708.000
c High Level ND
c to              10.3  12.0      yes           yes       1.0 V     "
c 1994 d 183 (9H 58M)
c JD 2449535.915
c Noise Adding
c onwards         10.3  12.0      yes           yes       1.0 V     "
c (1999 d  0 : 12H)
c (JD 2451179.000)
*
*     CMDP command parameters used:
*     1 = command mnemonic OHJY
*     2 = memory with spectrum to convert
*
*     other routines called:
*     arms, nchar
*
      IMPLICIT  NONE
*
      INTEGER   I               ! local loop index
      INTEGER   IBLOCKS         ! number of PSS changes
      PARAMETER (IBLOCKS = 6)
      INTEGER   IFREQ           ! local LRF in MHz, integer
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    CONST1          ! local temporary storage
      REAL*8    CONST2          ! local temporary storage
      REAL*8    STDPSS(IBLOCKS) ! local PSS at 1666 MHz 
      REAL*8    STWPSS(IBLOCKS) ! local PSS at 1612 MHz 
      REAL*8    YCHANGE(IBLOCKS)! local JD of changes in PSS
*
      INCLUDE 'lines.inc'
*
C     Changes in point source sensitivity
C
C     Year      Day   Time (UT)   JD           1666 MHz   1612 MHz
C
C     1978        1   12H 00M     2443509.000    15.8       19.0
C     1986      105   09H 58M     2446536.138    12.8       15.4
C     1990       61   12H 00M     2447953.000    13.4       16.1
C     1990      215   12H 00M     2448107.000    12.8       15.4
C     1990      229   12H 00M     2448121.000    10.3       12.0
C     1999        0   12H 00M     2451179.000    10.3       12.0
C
      DATA YCHANGE /2443509.000D0, 2446536.138D0, 2447953.000D0,
     &              2448107.000D0, 2448121.000D0, 2451179.000D0/
C
C     STanDard (1666 MHz) Point Source Sensitivity from given date
      DATA STDPSS  /15.8, 12.8, 13.4, 12.8, 10.3, 10.3/
C
C     Sixteen TWelve (1612 MHz) Point Source Sensitivity from given date
      DATA STWPSS  /19.0, 15.4, 16.1, 15.4, 12.0, 12.0/
*
*
      IF (DB) PRINT *,'in OHKTOJY'
      ERROR = 0
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
          PRINT *,'use OHJY on spectrum, not transform'
          ERROR = 1
      END IF
*
*     find line rest freq to nearest MHz
      IFREQ = IDINT(RESTFREQ(MEM)/1D6)
*
      IF (IFREQ .EQ. 1612) THEN
          DO I = 1, IBLOCKS-1
              IF (JULDATE(MEM) .GE. YCHANGE(I) .AND.
     &            JULDATE(MEM) .LT. YCHANGE(I+1)) THEN
                  CONST1 = STWPSS(I)
              END IF
          END DO
      ELSE IF (IFREQ .EQ. 1665 .OR. IFREQ .EQ. 1667) THEN
          DO I = 1, IBLOCKS-1
              IF (JULDATE(MEM) .GE. YCHANGE(I) .AND.
     &            JULDATE(MEM) .LT. YCHANGE(I+1)) THEN
                  CONST1 = STDPSS(I)
              END IF
          END DO
      ELSE 
          PRINT *,'LRF not 1612, 1665 or 1667 MHz'
          ERROR = 1
      END IF
*
      IF (ERROR .EQ. 0) THEN
          IF (PSS(MEM) .GT. 0D0) THEN
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
     &                  ' Jy/K for mem',MEM
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