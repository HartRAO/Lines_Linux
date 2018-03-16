***********************
      SUBROUTINE B50TOG (RA, DEC, GLII, GBII)
***********************
*  Convert B1950 equatorial coordinates to Galactic 
*  MJG 1994 09 01
*
*  This version uses real*8 arguments, CTOG uses real*4 arguments
*
*  Refs: Lang, Astrophysical Formulae, P504 
*        McNally, Positional Astronomy, P50.
*  dtr = pi/180.
*  sind/cosd = sin/cos of dec of north galactic pole. 
*  ranpr = ra of north galactic pole in radians.
* 
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      REAL*8    RA, DEC, GLII, GBII, ARGB,
     &          DTR, SIND, COSD, RANPR, DECR, RADIF, SINDEC,
     &          COSDEC, U, V
      DATA DTR, SIND, COSD, RANPR /0.0174532925D0, 0.460199785D0,
     &     0.887815385D0, 3.35539549D0/
* 
      DECR=DEC*DTR 
      RADIF=RA*DTR-RANPR 
      SINDEC=DSIN(DECR) 
      COSDEC=DCOS(DECR) 
      U=COSD*SINDEC-SIND*COSDEC*DCOS(RADIF) 
      V=COSDEC*DSIN(RADIF)
      GLII=33.D0+DATAN2(U,V)/DTR 
      IF(GLII .LT. 0D0 .OR. GLII .GE. 360D0) THEN 
          GLII=GLII-DSIGN(360D0,GLII) 
      END IF
      ARGB=SIND*SINDEC+COSD*COSDEC*DCOS(RADIF)
      GBII=DATAN(ARGB/DSQRT(1.D0-ARGB*ARGB))/DTR
      IF(GBII.GT.180.D0) GBII=GBII-360.D0 
      RETURN
      END 
