**********************
      SUBROUTINE DTDMS (DEG,IRD,IRM,RS)
**********************
C     convert real*8 decimal degrees (DEG)
C     to integer degrees (IRD),
C        integer minutes (IRM),
C        real*8  seconds (RS).
C     antique fortran rev MJG 1992 04 17
C
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      INTEGER*4 IRD, IRM
      REAL*8    DEG, RS, DEGA, SECS, 
     &          D0, D60, D180, D360, D3600
      DATA D0/0D0/, D60/60D0/, D180/180D0/, D360/360D0/, D3600/3600D0/
C
10    IF(DEG .LT. D360) GO TO 20
          DEG=DEG-D360
          GO TO 10
20    IF(DEG.GE.-D180) GO TO 30
          DEG=DEG+D360
          GO TO 20
30    DEGA=DEG
      IF(DEGA.LT.D180) GO TO 40
          DEGA=DEGA-D360
40    SECS=DEGA*D3600
      IRD=DEGA
      IRM=(SECS-IRD*D3600)/D60
      RS=SECS-IRD*D3600-IRM*D60
      IF (DABS(RS) .LT. 1D-6) RS = 0.0
      IF(DEGA.LT.D0 .AND. IRD.LT. 0) IRM=IABS(IRM)
      IF(DEGA.LT.D0 .AND. (IRD.LT.0 .OR. IRM.LT.0)) RS=ABS(RS)
      RETURN
      END
