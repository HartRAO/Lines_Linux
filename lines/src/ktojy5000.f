**********************
      SUBROUTINE KTOJY5000 (ERROR)
**********************
*     convert spectrum to Janskys, or change Jy value
*
C     This program converts spectra from Kelvins to Janskys,
C     using the appropriate point source sensitivity, listed below
c        (this is the automatic mode)
*
*     CMDP command parameters used:
*     1 = command mnemonic 5000JY
*     2 = memory with spectrum to convert
*
*     other routines called:
*     arms, nchar
*
      IMPLICIT  NONE
*
      INTEGER   I               ! local loop index
      INTEGER   IBLOCKS         ! number of PSS changes
      PARAMETER (IBLOCKS = 35)
      INTEGER   IFREQ           ! local LRF in MHz, integer
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    CONST1          ! local temporary storage
      REAL*8    CONST2          ! local temporary storage
      REAL*8    PSS5000(IBLOCKS)! local PSS at 5000 MHz 
      REAL*8    YCHANGE(IBLOCKS)! local JD of changes in PSS
*
      INCLUDE 'lines.inc'
*
C     Changes in point source sensitivity, 
C     obtained from 6cm continuum calibrator data
C       change takes effect at mid-day
C
C     Year      Day     JD              rel eff PSS
C
C 01    1990      1     2447893         1.003   14.06 nominal start
C 02    2000    165     2451709         1.009   13.97
C 03    2000    176     2451720         0.992   14.21
C 04    2000    186     2451730         0.992   14.21
C 05    2000    200     2451744         0.985   14.31
C 06    2000    215     2451759         0.996   14.16
C 07    2000    243     2451787         0.982   14.36
C 08    2000    256     2451800         0.977   14.43
C 09    2000    287     2451831         0.961   14.67
C 10    2000    336     2451880         0.962   14.66
C 11    2000    358     2451902         0.974   14.48
C 12    2001     14     2451924         0.980   14.39
C 13    2001     49     2451959         0.985   14.31
C 14    2001     63     2451973         0.992   14.21
C 15    2001     89     2451999         0.997   14.14
C 16    2001    111     2452021         1.010   13.96
C 17    2001    149     2452059         0.991   14.23
C 18    2001    169     2452079         0.975   14.46
C 19    2001    203     2452113         0.958   14.72
C 20    2001    216     2452126         0.963   14.64
C 21    2001    220     2452130         0.959   14.70
C 22    2001    227     2452137         0.977   14.43
C 23    2001    231     2452141         0.969   14.55
C 24    2001    237     2452147         0.965   14.61
C 25    2001    259     2452169         0.974   14.48
C 26    2001    267     2452177         0.982   14.36
C 27    2001    275     2452185         0.980   14.39
C 28    2001    279     2452189         0.995   14.17
C 29    2001    283     2452193         1.018   13.85
C 30    2001    304     2452214         1.041   13.54
C 31    2001    313     2452223         1.032   13.66
C 32    2002     12     2452287         1.044   13.51
C 33    2002     36     2452311         1.031   13.68
C 34    2002     48     2452323         1.023   13.78 last data
C 35    2010      1     2455198                  1.00 nominal end
C
      DATA YCHANGE 
     & /2447893D0, 2451709D0, 2451720D0, 2451730D0, 2451744D0,
     &  2451759D0, 2451787D0, 2451800D0, 2451831D0, 2451880D0,
     &  2451902D0, 2451924D0, 2451959D0, 2451973D0, 2451999D0,
     &  2452021D0, 2452059D0, 2452079D0, 2452113D0, 2452126D0,
     &  2452130D0, 2452137D0, 2452141D0, 2452147D0, 2452169D0,
     &  2452177D0, 2452185D0, 2452189D0, 2452193D0, 2452214D0,
     &  2452223D0, 2452287D0, 2452311D0, 2452323D0, 2455198D0/
C
C     Point Source Sensitivity at 5000 MHz from given date
      DATA PSS5000
     & /14.06, 13.97, 14.21, 14.21, 14.31,
     &  14.16, 14.36, 14.43, 14.67, 14.66,
     &  14.48, 14.39, 14.31, 14.21, 14.14,
     &  13.96, 14.23, 14.46, 14.72, 14.64,
     &  14.70, 14.43, 14.55, 14.61, 14.48,
     &  14.36, 14.39, 14.17, 13.85, 13.54,
     &  13.66, 13.51, 13.68, 13.78,  1.00/
C
*
      IF (DB) PRINT *,'in KTOJY5000'
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
      IF (IFREQ .GT. 4500 .AND. IFREQ .LT. 5000) THEN
          DO I = 1, IBLOCKS-1
              IF (JULDATE(MEM) .GE. YCHANGE(I) .AND.
     &            JULDATE(MEM) .LT. YCHANGE(I+1)) THEN
                  CONST1 = PSS5000(I)
              END IF
          END DO
      ELSE 
          PRINT *,'LRF not 4500 - 5000 MHz'
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
