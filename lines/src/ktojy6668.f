**********************
      SUBROUTINE KTOJY6668 (ERROR)
**********************
*     convert spectrum to Janskys, or change Jy value
*
C     This program converts spectra from Kelvins to Janskys,
C     using the appropriate point source sensitivity, listed below
c        (this is the automatic mode)
*
*     CMDP command parameters used:
*     1 = command mnemonic 6668JY
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
      REAL*8    PSS6668(IBLOCKS)! local PSS at 6668 MHz 
      REAL*8    YCHANGE(IBLOCKS)! local JD of changes in PSS
*
      INCLUDE 'lines.inc'
*
C     Changes in point source sensitivity, 
C     obtained by interpolation of 3.5 and 6cm continuum calibrator data
C       change takes effect at mid-day
C
C     Year      Day     JD              rel eff PSS
C
C 01    1990      1     2447893         0.999   15.72 nominal start
C 02    2000    165     2451709         1.013   15.50
C 03    2000    176     2451720         0.996   15.76
C 04    2000    186     2451730         0.999   15.72
C 05    2000    200     2451744         0.988   15.89
C 06    2000    215     2451759         0.963   16.30
C 07    2000    243     2451787         0.962   16.32
C 08    2000    256     2451800         0.963   16.30
C 09    2000    287     2451831         0.952   16.49
C 10    2000    336     2451880         0.954   16.46
C 11    2000    358     2451902         0.964   16.29
C 12    2001     14     2451924         0.977   16.07
C 13    2001     49     2451959         0.965   16.27
C 14    2001     63     2451973         0.964   16.29
C 15    2001     89     2451999         0.990   15.86
C 16    2001    111     2452021         0.989   15.87
C 17    2001    149     2452059         0.966   16.25
C 18    2001    169     2452079         0.972   16.15
C 19    2001    203     2452113         0.956   16.42
C 20    2001    216     2452126         0.932   16.85
C 21    2001    220     2452130         0.995   15.78
C 22    2001    227     2452137         0.970   16.19
C 23    2001    231     2452141         0.961   16.34
C 24    2001    237     2452147         0.941   16.68
C 25    2001    259     2452169         0.953   16.47
C 26    2001    267     2452177         0.971   16.17
C 27    2001    275     2452185         0.965   16.27
C 28    2001    279     2452189         0.989   15.87
C 29    2001    283     2452193         1.015   15.47
C 30    2001    304     2452214         1.040   15.08
C 31    2001    313     2452223         1.031   15.23
C 32    2002     12     2452287         1.055   14.88
C 33    2002     36     2452311         1.027   15.29
C 34    2002     48     2452323         1.021   15.38
C 35    2002     60.0   2452334.00      1.046   15.03
C 36    2002    114.0   2452389.00      1.078   14.58
C 37    2002    147.0   2452421.00      1.062   14.80
C 38    2002    205.0   2452479.00      1.024   15.36
C 39    2010      1     2455198         1.021   15.36 nominal end
C
      DATA YCHANGE 
     & /2447893D0, 2451709D0, 2451720D0, 2451730D0, 2451744D0,
     &  2451759D0, 2451787D0, 2451800D0, 2451831D0, 2451880D0,
     &  2451902D0, 2451924D0, 2451959D0, 2451973D0, 2451999D0,
     &  2452021D0, 2452059D0, 2452079D0, 2452113D0, 2452126D0,
     &  2452130D0, 2452137D0, 2452141D0, 2452147D0, 2452169D0,
     &  2452177D0, 2452185D0, 2452189D0, 2452193D0, 2452214D0,
     &  2452223D0, 2452287D0, 2452311D0, 2452323D0, 2452334D0,
     &  2452389D0, 2452421D0, 2452479D0, 2455198D0/
C
C     Point Source Sensitivity at 6668 MHz from given date
      DATA PSS6668
     & /15.72, 15.50, 15.76, 15.72, 15.89,
     &  16.30, 16.32, 16.30, 16.49, 16.46,
     &  16.29, 16.07, 16.27, 16.29, 15.86,
     &  15.87, 16.25, 16.15, 16.42, 16.85,
     &  15.78, 16.19, 16.34, 16.68, 16.47,
     &  16.17, 16.27, 15.87, 15.47, 15.08,
     &  15.23, 14.88, 15.29, 15.38, 15.03,
     &  14.58, 14.80, 15.36, 15.36/
C
*
      IF (DB) PRINT *,'in KTOJY6668'
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
      IF (IFREQ .EQ. 6668) THEN
          DO I = 1, IBLOCKS-1
              IF (JULDATE(MEM) .GE. YCHANGE(I) .AND.
     &            JULDATE(MEM) .LT. YCHANGE(I+1)) THEN
                  CONST1 = PSS6668(I)
              END IF
          END DO
      ELSE 
          PRINT *,'LRF not 6668 MHz'
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
