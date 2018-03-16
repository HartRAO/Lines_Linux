************************
      SUBROUTINE J2000PM (JD, Matrix)
************************
c     fortran by MJG from JJ pascal 1994 08 31
c
c  Procedure J2000PrecessionMatrix 
c  (JD : Real10; Var Matrix : ThreeMatrix);
c  { Create the rotation matrix to reduce coordinates from the standard
c  mean equinox J2000 to mean equinox of date "JD".
c  The Astronomical Almanac (1995) Page B18. }
c
      implicit none
      real*8 JD            !   input Julian date to precess to
      real*8 Matrix(3,3)   !   output precession matrix 
      real*8 T, Zeta, Zed, Theta, Temp, DTR ! local variables
      data   DTR / 0.017453293 /  ! degrees to radians
c
c Begin
      T  = (JD - 2451544.5d0)/36525.0d0             ! {centuries since J2000.0}
      Temp = 0.0000839d0 + 0.0000050d0 * T
      Temp = 0.6406161d0 + Temp * T
      Zeta  = Temp * T * DTR
      Temp = 0.0003041d0 + 0.0000051d0 * T
      Temp = 0.6406161d0 + Temp * T
      Zed   = Temp * T * DTR
      Temp = 0.0001185d0 + 0.0000116d0 * T
      Temp = 0.5567530d0 + Temp * T
      Theta = Temp * T * DTR
      Matrix(1,1) = + Cos(Zeta)*Cos(Theta)*Cos(Zed)
     &              - Sin(Zeta)*Sin(Zed)
      Matrix(1,2) = - Sin(Zeta)*Cos(Theta)*Cos(Zed)
     &              - Cos(Zeta)*Sin(Zed)
      Matrix(1,3) = - Sin(Theta)*Cos(Zed)
      Matrix(2,1) = + Cos(Zeta)*Cos(Theta)*Sin(Zed)
     &              + Sin(Zeta)*Cos(Zed)
      Matrix(2,2) = - Sin(Zeta)*Cos(Theta)*Sin(Zed)
     &              + Cos(Zeta)*Cos(Zed)
      Matrix(2,3) = - Sin(Theta)*Sin(Zed)
      Matrix(3,1) = + Cos(Zeta)*Sin(Theta)
      Matrix(3,2) = - Sin(Zeta)*Sin(Theta)
      Matrix(3,3) = + Cos(Theta)
      RETURN
      End
*********
