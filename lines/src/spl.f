******************
      FUNCTION SPL (X,XC,H)
******************
*    Computes value of the spline function with centre in XC and
*    "step" H, i.e. SPL(X,XC,H) is non-zero in  (XC-2H,XC+2H) 
*
*     This routine is called by: BELLS
* 
      IMPLICIT REAL*8 (A-H,O-Z)
*
*              to trap problems when SPL=0 (see 2) MW 14/11/00
      LOGICAL  SPL0
*
*              common to SPL, BELLS, SPLNFIT & EKDCFRSCT
      COMMON   /WARNING/ SPL0
*
      A=ABS(X-XC) 
      IF(A.GT.H) GOTO 1 
      P=H-A 
      Q=P+H 
      SPL=Q*Q*Q-4.*P*P*P
      RETURN
    1 IF(A.GT.(2.*H)) GOTO 2
      P=2.*H-A
      SPL=P*P*P 
      RETURN
    2 SPL=0.
*     warning set (program can write rubbish if this happens too often...)
      SPL0 = .TRUE.
      RETURN
      END 
