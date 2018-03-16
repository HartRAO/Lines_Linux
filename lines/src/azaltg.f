***********************************************
      SUBROUTINE AZALTG (RLAT,RU,RA,DEC,AZ,ALT)
*********************************************** 
C
C     AZALT generalised for any station
C 
c     taken from program TBCEG    <890712.1021> 
c 
c     inputs
c     RLAT    latitude of station in radians
c     RU      local siderial time in radians  (or 0)
c     RA      current RA in radians           (or HA)
c     DEC     current DEC in radians
c     outputs
c     AZ      azimuth in radians
c     ALT     altitude in radians
c
c     taken from program TBCEG <890712.1021>
C   THIS SUBROUTINE CONVERTS TOPOCENTRIC RIGHT ASCENSION AND DECLINATION
C   TO AZIMUTH AND ALTITUDE, USING AN ALGORITHM PRESENTED ON P.402 OF
C   ESCOBAL,P., METHODS OF ORBIT DETERMINATION.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
  
      DIMENSION STACOR(3),CL(3),CS(3),CE(3),CZ(3),CLH(3)
c
c     COMMON/TOPO/IOPT,AU,REM,STACOR,ETMUTC
c     from common block TOPO :
c     STACOR(2) = cos (latitude of station) = 0.900407079 for Hartrao
c     STACOR(3) = sin (latitude of station) = -0.434097561  "
C      data stacor / 0.0, 0.900407079d0, -0.4340979561/
c
      DATA FLAT/3.35233D-3/
      DATA TWOPI/6.283185307179586D0/
C
C     mod from AZALT to do any station 
      STACOR(1) = 0.0
      STACOR(2) = COS(RLAT)
      STACOR(3) = SIN(RLAT)
C
      FLDFSQ = (1.D0-FLAT)**2
      TANPHI = STACOR(3)/STACOR(2)/FLDFSQ
      GEDLAT = DATAN(TANPHI)
      COSGDL = DCOS(GEDLAT)
      SINGDL = DSIN(GEDLAT)
      COSRU = DCOS(RU)
      SINRU = DSIN(RU)
      CL(1) = DCOS(DEC)*DCOS(RA)
      CL(2) = DCOS(DEC)*DSIN(RA)
      CL(3) = DSIN(DEC)
      CS(1) = SINGDL*COSRU
      CS(2) = SINGDL*SINRU
      CS(3) = -COSGDL
      CE(1) = -SINRU
      CE(2) = COSRU
      CE(3) = 0.D0
      CZ(1) = COSRU*COSGDL
      CZ(2) = SINRU*COSGDL
      CZ(3) = SINGDL
      DO 20 I = 1,3
          CLH(I) = 0.D0
   20     CONTINUE
      DO 30 I = 1,3
          CLH(1) = CLH(1)+CS(I)*CL(I)
          CLH(2) = CLH(2)+CE(I)*CL(I)
          CLH(3) = CLH(3)+CZ(I)*CL(I)
   30     CONTINUE
      COSH = DSQRT(1.D0-CLH(3)**2)
      ALT = DATAN2(CLH(3),COSH)
      COSA = -CLH(1)/COSH
      SINA = CLH(2)/COSH
      AZ = DATAN2(SINA,COSA)
      IF(AZ.LT.0) AZ = AZ+TWOPI
      RETURN
      END
C********

