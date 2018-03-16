**********************
      SUBROUTINE DTHMS (DEG,IRH,IRM,RS)
**********************
C     convert real*8 decimal degrees (DEG)
C     to integer hours (IRH),
C        integer minutes (IRM),
C        real*8 seconds (RS)
C     rev MJG 1992 04 17
C
      IMPLICIT NONE
      INTEGER*4   IRH, IRM
      REAL*8      DEG, RS, SECS,
     &            D60, D240, D360, D3600
      DATA D60/60D0/, D240/240D0/, D360/360D0/, D3600/3600D0/
C
      IF(DEG)1,2,2
1     IRH=-1
      IRM=-1
      RS=-1D0
      RETURN
2     IF(DEG-D360)3,3,1
3     SECS=DEG*D240
      IRH=SECS/D3600
      IRM=(SECS-FLOAT(IRH)*D3600)/D60
      RS=SECS-FLOAT(IRH)*D3600-FLOAT(IRM)*D60
      IF (DABS(RS) .LT. 1D-6) RS = 0.0
      RETURN
      END
