      SUBROUTINE GAUSFIT2DS (ERROR)
C
C     ********************************************************
C     *   originally called GAUFI then GAUSFIT2D by MG       *
C     *   Fits a 2-dimensional Gaussian to a measured        *
C     *   intensity distribution (at least 5 measured points *
C     *   from Karl Menten <kmenten@mpifr-bonn.mpg.de>       *
C     ********************************************************
C
*     Calculate pointing corrections from measurements made at the
*     half power points of the telescope beam using 2D gaussian fit
*     Also calculate corrected on source flux.
*     called by: pca2
*
*     CMDP command parameters used:
*     1 = command mnemonic PC2
*     2 = intensity hpbw north
*     3 = intensity hpbw south
*     4 = intensity hpbw east 
*     5 = intensity hpbw west
*     6 = intensity on source
*     7 = typical uncertainty in intensity values
*     8 = halfpower beamwidth
*     9 = memory in use
*
      IMPLICIT  NONE
      INTEGER   NMAX
      INTEGER   NCA
      PARAMETER (NMAX=20)       ! must change in GAUSJ as well
      PARAMETER (NCA = 4)       ! four parameters to be fitted
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! loop index
      INTEGER   II              ! loop index
      INTEGER   IOS             ! file eror check
      INTEGER   IP              ! loop index
      INTEGER   J               ! loop index
      INTEGER   K               ! loop index
      INTEGER   LISTA(NCA)      ! number the fit parameters
      INTEGER   MA              ! number of coefficients
      INTEGER   MEM             ! memory in use
      INTEGER   MFIT            ! number of parameters to adjust
      INTEGER   NCHAR           ! external function
      INTEGER   NDATA           ! number of data points
      REAL*8    ALAMDA
      REAL*8    ALPHA(NCA,NCA)  ! returned by MRQMIN
      REAL*8    A(NCA)          ! guesses for parameters for solution
      REAL*8    ADIFF(NCA)      ! differences between new and previous A(I)'s
      REAL*8    AOLD(NCA)       ! saved guesses for parameters for solution
      REAL*8    CHISQ           ! chisq of fit
      REAL*8    CHISQOLD        ! chisq of previous fit
      REAL*8    COVAR(NCA,NCA)  ! covariance returned by MRQMIN
      REAL*8    DCHI            ! change in chisq after iteration
      REAL*8    ERR(NCA)        ! sqrt of coavariance from iteration
      REAL*8    EWOFF           ! EW pointing offset in degrees
      REAL*8    FRTHHPBW        ! freq * half halfpower beamwidth/MHzdeg
      REAL*8    FUNCS
      REAL*8    HPBW            ! input half power beamwidth
      REAL*8    HPBWHART        ! half power beamwidth for 26-m Hart
      REAL*8    NSOFF           ! NS pointing offset in degrees
      REAL*8    POINTCORCALC    ! calculated pointing correction
      REAL*8    PNTERROR        ! uncertainty in hp values due to pnt error
      REAL*8    SIG(NMAX)       ! uncertainties at X1(I), X2(I)
      REAL*8    R               ! max difference in adiff
      REAL*8    TRMS            ! input theoretical rms noise in the data
      REAL*8    X1(NMAX)        ! positions on X1 axis
      REAL*8    X2(NMAX)        ! positions on X2 axis
      REAL*8    YPNT(NMAX)      ! intensities at X1(I), X2(I)
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in GAUSFIT2DS'
C
      FRTHHPBW = 385.               ! for hartebeesthoek
      ERROR = 0
C
      IF (NCMD .GE. 6) THEN
          YPNT(1) = 0D0         ! half-power point north
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) YPNT(1)
  210     IF (IOS .NE. 0) ERROR = IOS
          YPNT(2) = 0D0         ! half-power point south
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) YPNT(2)
  220     IF (IOS .NE. 0) ERROR  = IOS
          YPNT(3) = 0D0         ! half-power point east
          READ (CMDP(4),*,IOSTAT=IOS,ERR=230) YPNT(3)
  230     IF (IOS .NE. 0) ERROR  = IOS
          YPNT(4) = 0D0         ! half-power point west
          READ (CMDP(5),*,IOSTAT=IOS,ERR=240) YPNT(4)
  240     IF (IOS .NE. 0) ERROR  = IOS
          YPNT(5) = 0D0         ! on source
          READ (CMDP(6),*,IOSTAT=IOS,ERR=250) YPNT(5)
  250     IF (IOS .NE. 0) ERROR  = IOS
      END IF
*
      IF (NCMD .LT. 6 .OR. ERROR .NE. 0) THEN
          PRINT '(A,$)','Intensities at HP points N,S ?'
          READ '(A)',BUF
          READ (BUF,*,IOSTAT=IOS,ERR=260) YPNT(1),YPNT(2)
  260     IF (IOS .NE. 0) THEN
              PRINT 1010,BUF
 1010         FORMAT (' illegal ',A)
              ERROR = IOS
              RETURN
          END IF
*
          PRINT '(A,$)','Intensities at HP points E,W ?'
          READ '(A)',BUF
          READ (BUF,*,IOSTAT=IOS,ERR=270) YPNT(3),YPNT(4)
  270     IF (IOS .NE. 0) THEN
              PRINT 1010,BUF
              ERROR = IOS
              RETURN
          END IF
*
          PRINT '(A,$)','Intensity on source ?'
          READ '(A)',BUF
          READ (BUF,*,IOSTAT=IOS,ERR=280) YPNT(5)
  280     IF (IOS .NE. 0) THEN
              PRINT 1010,BUF
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      PRINT *,'N=',YPNT(1),' S=',YPNT(2),' E=',YPNT(3),' W=',YPNT(4),
     *        ' On=',YPNT(5)
*
      TRMS = -1
      IF (NCMD .GE. 7) THEN
          READ (CMDP(7),*,IOSTAT=IOS,ERR=290) TRMS
  290     IF (IOS .NE. 0) ERROR = IOS
      END IF
      IF (DB) PRINT *,'TRMS=',TRMS
      IF (TRMS .LT. 0D0) THEN
          PRINT '(A,$)',' Uncertainty in intensities ( > 0 )?'
          READ  '(A)', BUF
          READ (BUF,*,IOSTAT=IOS,ERR=295) TRMS
  295     IF (IOS .NE. 0) THEN
              PRINT 1010, BUF(1:NCHAR(BUF))
              RETURN
          END IF        
      END IF
      IF (TRMS .EQ. 0) TRMS = ABS(YPNT(5)/100)       ! must be > 0
      PRINT *,'Uncertainty=',TRMS
*
      HPBW = -1
      IF (NCMD .GE. 8) THEN
          READ (CMDP(8),*,IOSTAT=IOS,ERR=300) HPBW
  300     IF (IOS .NE. 0) ERROR = IOS
      END IF
      IF (DB) PRINT *,'HPBW=',HPBW
*
*
      MEM = MEMSET
      IF (NCMD .GE. 9) THEN
          READ (CMDP(9),*,IOSTAT=IOS,ERR=310) MEM
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
C let it continue for manually entered data          RETURN
      END IF
*
*
      IF (HPBW .LT. 0D0) THEN
          IF (MEM .GT. 0 .AND. RESTFREQ(MEM) .GT. 0D0) THEN
              HPBW = 2 * FRTHHPBW / (RESTFREQ(MEM) / 1D6) 
              IF (DB) PRINT *,'from freq in MEM=',MEM,' HPBW=',HPBW
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
*
      IF (MEM .GT. 0 .AND. RESTFREQ(MEM) .GT. 0D0) THEN
          PRINT '(/A,F6.3,A,F6.1,A)',
     &      ' adopted halfpower beamwidth = ',HPBW,' deg at ',
     &      30D9/RESTFREQ(MEM),'cm'
      END IF
*
*     create required arrays X1, X2, Y, SIG
*     halfpower beamwidth for 26-m telescope
      HPBWHART = 2 * FRTHHPBW / (RESTFREQ(MEM) / 1D6)
      PRINT *,'HPBWHART=',HPBWHART
*     add uncertainty due to pointing accuracy for the halfpower points 
*     near HP points slope of Gaussian is 0.6 * height / std dev
*     = 1.41 * height * pointing errror in hpbw's
*     take pointing error as 0.003 deg
      PNTERROR = 0.003 / HPBWHART * 1.41 * YPNT(5)
*     changed 2005/09/16
      X1(1) = +0.5D0    ! HPN
      X2(1) = 0D0
      SIG(1) = TRMS + PNTERROR
c
      X1(2) = -0.5D0    ! HPS
      X2(2) = 0D0
      SIG(2) = TRMS + PNTERROR
c
      X1(3) = 0D0       ! HPE
      X2(3) = +0.5D0
      SIG(3) = TRMS + PNTERROR
c
      X1(4) = 0D0       ! HPW
      X2(4) = -0.5D0
      SIG(4) = TRMS + PNTERROR
c
      X1(5) = 0D0       ! On
      X2(5) = 0D0
      YPNT(5)  = YPNT(5)
      SIG(5) = TRMS
c
      NDATA = 5         ! number of data points
      MA = 4            ! number of coefficients
      MFIT = 3          ! to keep HPBW fixed
c      MFIT = 4          ! to adjust HPBW as well
      CHISQOLD = 30.    ! initialise to check convergence
      ALAMDA = -1.      ! initialise MRQMIN on first pass
      DO I = 1, NCA
          LISTA(I) = I  ! number the parameters
      END DO
*
      DO I = 1, NDATA
          PRINT *,' X1=',X1(I),' X2=',X2(I),' Y=',YPNT(I),
     &    ' Sigma=',SIG(I)
      END DO
C       
C     set starting values - these were not good for large pointing offsets
C     replaced by call to OFFSET below
C
c      A(1) = YPNT(5) + TRMS ! guess Intensity = on-source + uncertainty
c      A(2) = 0.67 * (YPNT(1) - YPNT(2)) / YPNT(5)      ! guess offset NS (from expts)
c      A(3) = 0.67 * (YPNT(3) - YPNT(4)) / YPNT(5)      ! guess offset NS (from expts)
c      A(4) = 1.0                        ! normalised to HPBW = 1 unit
c      PRINT *, 'A(1,2,3,4)= ',A(1),A(2),A(3),A(4)    
C
C     use 1D gausfit in OFFSET to get first guesses for 2D gausfit
C
      PRINT *,'Use 1D gausfit to get first guess for 2D iterative fit'
      WRITE (CMDP(2),*) YPNT(1)
      WRITE (CMDP(3),*) YPNT(2)
      WRITE (CMDP(4),*) YPNT(3)
      WRITE (CMDP(5),*) YPNT(4)
      WRITE (CMDP(6),*) YPNT(5)
c      WRITE (CMDP(7),*) 1.0d0   ! use normalised beamwidth
      WRITE (CMDP(7),*) HPBW   ! use normalised beamwidth
      WRITE (CMDP(8),*) MEM
      NCMD = 8
      CALL OFFSET (ERROR)
      PRINT *,'back in GAUSFIT2DS, from OFFSET:'
      PRINT *,' PCNS=',PCNS,' PCEW=',PCEW,' POINTCOR=',POINTCOR
C       
C     set starting values using estimates from OFFSET
C     signs needed for PCNS and PCEW are opposite to what OFFSET gives for some reason
C
      A(1) = YPNT(5) * POINTCOR ! guess Intensity = on-source * pointing corr
      A(2) = -PCNS              ! guess offset NS (from expts)
      A(3) = -PCEW              ! guess offset NS (from expts)
      A(4) = 1.0                ! normalised to HPBW = 1 unit  20071025
c      A(4) = HPBW                ! normalised to HPBW = 1 unit 20071025
      PRINT *, 'A(1,2,3,4)= ',A(1),A(2),A(3),A(4)    
C
      IF (MFIT .EQ. 3) PRINT *,'HPBW fixed at ',A(4)
      IF (MFIT .EQ. 4) PRINT *,'HPBW to be fitted'
      DO II=1,MA
          AOLD(II)=A(II)
      END DO
C  
C     do fitting now - see Numerical Recipes by Press et al.
C     first call initialises
C
      CALL MRQMIN (X1,X2,YPNT,SIG,NDATA,A,MA,LISTA,MFIT,
     *     COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
C
C     iterate the fit
C
      DO I = 1, 50
          DCHI=(CHISQOLD-CHISQ)
          PRINT *,'CHISQOLD = ',CHISQOLD, ' CHISQ =',CHISQ
          DO J=1,MA
              ADIFF(J) = ABS((A(J)-AOLD(J))/A(J))
              PRINT *,'A(',J,'),AOLD(',J,') = ',A(J),AOLD(J),
     *                ' ADIFF(',J,') = ',ADIFF(J) 
          END DO
          PRINT *, 'A(1,2,3,4)= ',A(1),A(2),A(3),A(4)    
          R = MAX(ADIFF(1),ADIFF(2),ADIFF(3),ADIFF(4))
          PRINT *,'Rmax = ',R,' DCHI=',DCHI
          IF((R.LT.0.0001) .AND. (CHISQ.LE.CHISQOLD) .AND.
     *       (DCHI.LT.0.001)) 
     *       GO TO 1150
C
          CHISQOLD = CHISQ
          DO K=1,MA
              AOLD(K) = A(K)
          END DO
C         PRINT *,'ITER,CHISQ,LAM = ',I,CHISQ,ALAMDA 
          PRINT 2010,(A(IP),IP=1,MA)
 2010     FORMAT(/,' INT = ',1PE10.3,
     *             ' XOFF = ',1PE10.3,
     *             ' YOFF = ',1PE10.3,
     *             ' HPBW = ',1PE10.3)
C
          CALL MRQMIN (X1,X2,YPNT,SIG,NDATA,A,MA,LISTA,MFIT,
     *         COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
      END DO
C
 1150 ALAMDA=0.         ! set to zero for final call to MRQMIN
C     get the covariance and curvature matrices
C
      CALL MRQMIN (X1,X2,YPNT,SIG,NDATA,A,MA,LISTA,MFIT,
     *     COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
C
C     get the uncertainty in each parameter from its covariance matrix
C
      DO I=1,4
          ERR(I) = SQRT(COVAR(I,I))
      END DO
C
      PRINT 2000,(A(I),ERR(I),I=1,4)
 2000 FORMAT(/,' Intensity  = ',1PE11.3,' +/- ',1PE11.3,/,
     * ' X1OFF = (',1PE11.3,' +/- ',1PE11.3,')*HPBW',/,
     * ' X2OFF = (',1PE11.3,' +/- ',1PE11.3,')*HPBW',/,
     * ' HPBW  = ',1PE11.3,' +/- ',1PE11.3)
      PRINT *,'26m HPBW = ',HPBW
c     for calculation with HPBW set to 1.0, scale by actual HPBW   20071025
      NSOFF = A(2) * HPBW
      EWOFF = A(3) * HPBW
c      for calculation using actual HPBW not set at 1.0   20071025
c       NSOFF = A(2) 
c       EWOFF = A(3)
*
*     intensity correction from 2d fit
      POINTCORCALC = A(1)/YPNT(5)
      IF (POINTCORCALC .GE. 1.0D0) THEN
          POINTCOR = POINTCORCALC
      ELSE 
          POINTCOR = 1.0
      END IF
*     calculated intensity on source corrected for pointing
c      POINTCORCALC = EXP (0.693147D0 * (NSOFF**2 + EWOFF**2) / (HPBW/2)**2)
      PRINT *,' Pointcorcalc = ',POINTCORCALC
c      PRINT *,'Pointcor from 2D Gaussian fit = ',POINTCOR
*   
*     write note on processing
      WRITE (BUF,*) ' 2-D iterative Gaussian fit to HPN-HPS-HPE-HPW-On:'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
c
      WRITE (BUF,'(A20,A10,10x,A9,A9,A9)')
     &  'Pointing-for-Object ','DateObs',
     &  'PCHAmdeg','PCDCmdeg','PointCor'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,'(A20,A20,F8.3,1x,F8.3,1x,F9.6)')
     &  OBJECT(MEM), DATE_OBS(MEM)(1:20), 
     &  EWOFF*1000, NSOFF*1000, POINTCOR
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      IF (POINTCORCALC .LT. 1.0D0) THEN
          WRITE (BUF,'(A,F12.6,A)')
     &    'Illegal POINTCOR=',POINTCORCALC,' suggest using PC instead' 
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
      RETURN
      END
