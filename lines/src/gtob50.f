************************
      SUBROUTINE GTOB50 (GLII, GBII, RA, DEC)
************************
*  Convert Galactic to B1950 equatorial coordinates
*  MJG 1994 09 01
*
*  This version uses real*8 arguments, CTOG uses real*4 arguments
*
*  Refs: Lang, Astrophysical Formulae, P504 
*        McNally, Positional Astronomy, P50.
*  dtr = pi/180.
*  sind/cosd = sin/cos of dec of north galactic pole. 
*  glor = origin of galactic longitude in galactic plane in radians.
*
*     other subroutines called:
*     none
* 
      IMPLICIT NONE
      REAL*8    RA, DEC, GLII, GBII, ARGD,  
     &          DTR, SIND, COSD, GLOR, 
     &          U, V, GBR, GLDIF, SINGB, COSGB
      DATA DTR, SIND, COSD, GLOR/0.0174532925D0, 0.460199785D0,
     &     0.887815385D0,  0.575958653D0/
* 
      GBR=GBII*DTR
      GLDIF=GLII*DTR-GLOR 
      SINGB=DSIN(GBR) 
      COSGB=DCOS(GBR) 
      U=COSGB*DCOS(GLDIF) 
      V=COSD*SINGB-SIND*COSGB*DSIN(GLDIF) 
      RA=192.25D0+DATAN2(U,V)/DTR
      IF(RA.LT.0.D0.OR.RA.GE.360.D0) RA=RA-DSIGN(360.D0,RA) 
      ARGD=SIND*SINGB+COSD*COSGB*DSIN(GLDIF)
      DEC=DATAN(ARGD/DSQRT(1.D0-ARGD*ARGD))/DTR 
      IF(DEC.GT.180.D0) DEC=DEC-360.D0
      RETURN
      END 
*****************
