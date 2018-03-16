C----------------------------------------------
C     2-D Gaussian function for least squares fit
C
      SUBROUTINE FUNC2DGAUSS(X1,X2,A,Y,DYDA,MA)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(4), DYDA(4)
      S0= A(1)
      X10= A(2)
      X20= A(3)
      TH=A(4)
C      PRINT *,S0,X10,X20,TH
C      PRINT *,X1,X2
      EFX1=-2.773*((X1-X10)/TH)**2
      EFX2=-2.773*((X2-X20)/TH)**2
      EFX=EFX1+EFX2
      Y=S0*EXP(EFX)
C      PRINT *,Y
      DYDA(1)=Y/S0
      DYDA(2)=(-2.*EFX1/(X1-X10))*Y
      DYDA(3)=(-2.*EFX2/(X2-X20))*Y
      DYDA(4)=(-2.*EFX/TH)*Y
C      PRINT *,(DYDA(I),I=1,4)
      RETURN
      END
