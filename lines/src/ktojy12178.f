**********************
      SUBROUTINE KTOJY12178 (ERROR)
**********************
*     convert spectrum to Janskys, or change Jy value
*
C     This program converts spectra from Kelvins to Janskys,
C     using the appropriate point source sensitivity, listed below
c        (this is the automatic mode)
*
*     CMDP command parameters used:
*     1 = command mnemonic 12178JY
*     2 = memory with spectrum to convert
*
*     other routines called:
*     arms, nchar
*
      IMPLICIT  NONE
*
      INTEGER   I               ! local loop index
      INTEGER   IBLOCKS         ! number of PSS changes
      PARAMETER (IBLOCKS = 39)
      INTEGER   IFREQ           ! local LRF in MHz, integer
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    CONST1          ! local temporary storage
      REAL*8    CONST2          ! local temporary storage
      REAL*8    PSS12178(IBLOCKS)! local PSS at 12178 MHz 
      REAL*8    YCHANGE(IBLOCKS)! local JD of changes in PSS
*
      INCLUDE 'lines.inc'
*
C     Changes in point source sensitivity, 
C     obtained by interpolation of 3.5 and 6cm continuum calibrator data
C     using axum, file calsurf
C       change takes effect at mid-day
C
c no.   Year       DayNo          JD      Rel2p5      PSS2p5
c 01  1990.00        1.00  2447893.00       1.000       24.20 nominal start
c 02  2000.00      165.00  2451709.00       1.026       23.59
c 03  2000.00      176.00  2451720.00       1.009       23.99
c 04  2000.00      186.00  2451730.00       1.014       23.86
c 05  2000.00      200.00  2451744.00       0.999       24.23
c 06  2000.00      215.00  2451759.00       0.925       26.15
c 07  2000.00      243.00  2451787.00       0.941       25.72
c 08  2000.00      256.00  2451800.00       0.950       25.48
c 09  2000.00      287.00  2451831.00       0.946       25.57
c 10  2000.00      336.00  2451880.00       0.950       25.47
c 11  2000.00      358.00  2451902.00       0.957       25.30
c 12  2001.00       14.00  2451924.00       0.980       24.70
c 13  2001.00       49.00  2451959.00       0.945       25.60
c 14  2001.00       63.00  2451973.00       0.933       25.95
c 15  2001.00       89.00  2451999.00       0.987       24.52
c 16  2001.00      111.00  2452021.00       0.966       25.05
c 17  2001.00      149.00  2452059.00       0.940       25.74
c 18  2001.00      169.00  2452079.00       0.975       24.83
c 19  2001.00      203.00  2452113.00       0.959       25.24
c 20  2001.00      216.00  2452126.00       0.898       26.96
c 21  2001.00      220.00  2452130.00       0.953       25.40
c 22  2001.00      227.00  2452137.00       0.968       25.00
c 23  2001.00      231.00  2452141.00       0.957       25.28
c 24  2001.00      237.00  2452147.00       0.915       26.44
c 25  2001.00      259.00  2452169.00       0.933       25.94
c 26  2001.00      267.00  2452177.00       0.963       25.12
c 27  2001.00      275.00  2452185.00       0.950       25.47
c 28  2001.00      279.00  2452189.00       0.982       24.64
c 29  2001.00      283.00  2452193.00       1.014       23.87
c 30  2001.00      304.00  2452214.00       1.045       23.15
c 31  2001.00      313.00  2452223.00       1.036       23.35
c 32  2002          12     2452287          1.078       22.45
C 33  2002          36     2452311          1.030       23.50
C 34  2002          48     2452323          1.026       23.59
C 35  2002          60.0   2452334.00       1.069       22.65
C 36  2002         114.0   2452389.00       1.126       21.48
C 37  2002         147.0   2452421.00       1.094       22.12
C 38  2002         205.0   2452479.00       1.047       23.12

C 35  2010           1     2455198          1.026       23.59 nominal end
C
      DATA YCHANGE 
     & /2447893D0, 2451709D0, 2451720D0, 2451730D0, 2451744D0,
     &  2451759D0, 2451787D0, 2451800D0, 2451831D0, 2451880D0,
     &  2451902D0, 2451924D0, 2451959D0, 2451973D0, 2451999D0,
     &  2452021D0, 2452059D0, 2452079D0, 2452113D0, 2452126D0,
     &  2452130D0, 2452137D0, 2452141D0, 2452147D0, 2452169D0,
     &  2452177D0, 2452185D0, 2452189D0, 2452193D0, 2452214D0,
     &  2452223D0, 2452287D0, 2452311D0, 2452323D0, 2452334D0,
     &  2452389D0, 2452421D0, 2452479D0, 2455195D0/
C
C     Point Source Sensitivity at 12178 MHz from given date
      DATA PSS12178
     & /24.20, 23.59, 23.99, 23.86, 24.23,
     &  26.15, 25.72, 25.48, 25.57, 25.47,
     &  25.30, 24.70, 25.60, 25.95, 24.52,
     &  25.05, 25.74, 24.83, 25.24, 26.96,
     &  25.40, 25.00, 25.28, 26.44, 25.94,
     &  25.12, 25.47, 24.64, 23.87, 23.15,
     &  23.35, 23.35, 22.45, 23.50, 23.59,
     &  22.65, 21.48, 22.12, 23.12/
C
*
      IF (DB) PRINT *,'in KTOJY12178'
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
          PRINT *,'use KtoJY on spectrum, not transform'
          ERROR = 1
      END IF
*
*     find line rest freq to nearest MHz
      IFREQ = IDINT(RESTFREQ(MEM)/1D6)
*
      IF (IFREQ .EQ. 12178) THEN
          DO I = 1, IBLOCKS-1
              IF (JULDATE(MEM) .GE. YCHANGE(I) .AND.
     &            JULDATE(MEM) .LT. YCHANGE(I+1)) THEN
                  CONST1 = PSS12178(I)
              END IF
          END DO
      ELSE 
          PRINT *,'LRF not 12178 MHz'
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
