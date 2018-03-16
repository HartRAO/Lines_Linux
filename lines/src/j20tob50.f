*************************
      SUBROUTINE J20TOB50 (J2000RA, J2000DEC, B1950RA, B1950DEC)
*************************
C     convert J2000 RA DEC to B1950 RA DEC
C     based on LC's J2000 pascal program, fortran by MJG 1994 08 31
C     note that proper motion, parallax and radial velocity are NOT computed
c     references:
c     Astronomical Almanac p B43
c     Standish, E.M., 1982, A&A, 115, 20
c     Aoki et al., 1983, A&A, 128, 263
c
*     other subroutines called:
c     SCALPROD
c
c
      IMPLICIT NONE
      integer*4 i,j,iprod ! loop variables, local
      logical   test      ! loop test, local
      real*8    J2000RA   ! J2000 Right Ascension in degrees, input
      real*8    J2000Dec  ! J2000 Declination in degrees, input
      real*8    B1950RA   ! B1950 Right ascension in degrees, output
      real*8    B1950DEC  ! B1950 Declination in degrees, output
      real*8    RAR       ! J2000 Right Ascension in radians, local
      real*8    DecR      ! J2000 Declination in radians, local
      real*8    DTR       ! degrees to radians conversion
      real*8    PMRA      ! Proper Motion in RA, arcseconds per Julian Century
      real*8    PMDec     ! Proper Motion in Dec, arcseconds per Julian Century
      real*8    ParX      ! parallax in seconds of arc, input
      real*8    RadVel    ! Radial Velocity in km/s for J2000, input
      real*8    rdotcons  ! constant for velocity vector, local
      real*8    SCALPROD  ! scalar product function, local
      real*8    r0(3)     ! Rectangular components of the position vector
      real*8    rDot0(3)  ! Rectangular components of velocity vector
      real*8    A(3)      ! constant vector
      real*8    Adot(3)   ! constant vector
      real*8    CapR0(6)  ! the vector (r0,rDot0)
      real*8    R1(6)     ! vector InvM*CapR0
      real*8    InvM(6,6) ! inverse of 6 x 6 matrix M
c
c     variables used in calculating effects of the E-terms of aberration
      real*8    Temp_r(3), r(3), rdot(3), s(3), s1(3), sdot1(3)
      real*8    sDotAdot, sDotA, Scalar_r1
c
c     variables used to obtain FK4 parameters
      real*8    B1950RAR, SB1950RAR, B1950DecR
      real*8    x, y, z, xdot, ydot, zdot, scalar_r, SinDec
c
      DATA InvM(1,1) /+0.9999256795D0/,
     &     InvM(1,2) /+0.0111814828D0/,
     &     InvM(1,3) /+0.0048590039D0/,
     &     InvM(1,4) /-0.00000242389840D0/,
     &     InvM(1,5) /-0.00000002710544D0/,
     &     InvM(1,6) /-0.00000001177742D0/
      DATA InvM(2,1) /-0.0111814828D0/,
     &     InvM(2,2) /+0.9999374849D0/,
     &     InvM(2,3) /-0.0000271771D0/,
     &     InvM(2,4) /+0.00000002710544D0/,
     &     InvM(2,5) /-0.00000242392702D0/,
     &     InvM(2,6) /+0.00000000006585D0/
      DATA InvM(3,1) /-0.0048590040D0/,
     &     InvM(3,2) /-0.0000271557D0/,
     &     InvM(3,3) /+0.9999881946D0/,
     &     InvM(3,4) /+0.00000001177742D0/,
     &     InvM(3,5) /+0.00000000006585D0/,
     &     InvM(3,6) /-0.00000242404995D0/
      DATA InvM(4,1) /-0.000551D0/,
     &     InvM(4,2) /+0.238509D0/,
     &     InvM(4,3) /-0.435614D0/,
     &     InvM(4,4) /+0.99990432D0/,
     &     InvM(4,5) /+0.01118145D0/,
     &     InvM(4,6) /+0.00485852D0/
      DATA InvM(5,1) /-0.238560D0/,
     &     InvM(5,2) /-0.002667D0/,
     &     InvM(5,3) /+0.012254D0/,
     &     InvM(5,4) /-0.01118145D0/,
     &     InvM(5,5) /+0.99991613D0/,
     &     InvM(5,6) /-0.00002717D0/
      DATA InvM(6,1) /+0.435730D0/,
     &     InvM(6,2) /-0.008541D0/,
     &     InvM(6,3) /+0.002117D0/,
     &     InvM(6,4) /-0.00485852D0/,
     &     InvM(6,5) /-0.00002716D0/,
     &     InvM(6,6) /+0.99996684D0/
c
      DATA A(1) /-1.62557D-6/,    ! radians
     &     A(2) /-0.31919D-6/,
     &     A(3) /-0.13843D-6/
c
      DATA Adot(1) /+1.245D-3/,    ! arcsec per tropical century
     &     Adot(2) /-1.580D-3/,
     &     Adot(3) /-0.659D-3/
c
      DATA DTR / 0.017453293D0/
c
c begin procedure InitializeVariables;
c
c     Initialize input variables, especially parallax and radial velocity must
c     be zero if they are unspecified when using the program
      RAR = J2000RA * DTR
      DECR = J2000DEC * DTR
      PMRA =   0d0
      PMDec =  0d0
      ParX =   0d0
      RadVel = 0d0
c
c end procedure InitializeVariables;
c
c begin procedure RectangularComponents;
c
c     Calculate the rectangular components of the position vector r0 and
c     velocity vector rDot0
c
      r0(1) = cos(RAR)*cos(DecR) !   {units of radian}
      r0(2) = sin(RAR)*cos(DecR)
      r0(3) = sin(DecR)
c
c     rDot0 in units of second of arc per tropical century
c
      rdotcons = +21.095*RadVel*ParX
      rDot0(1) = -PMRA*sin(RAR)*cos(decR)-PMDec*cos(RAR)*sin(DecR)
     &           +rdotcons*r0(1)
      rDot0(2) = +PMRA*cos(RAR)*cos(DecR)-PMDec*sin(RAR)*sin(DecR)
     &           +rdotcons*r0(2)
      rDot0(3) = +PMDec*cos(DecR)+rdotcons*r0(3)
c
c end procedure RectangularComponents;
c
c begin procedure VectorCapR0
c
c     Form the vector CapR0 by using r0 as first 3 elements and rDot0 as
c     last 3
c
      CapR0(1) = r0(1)
      CapR0(2) = r0(2)
      CapR0(3) = r0(3)
      CapR0(4) = rDot0(1)
      CapR0(5) = rDot0(2)
      CapR0(6) = rDot0(3)
c
c end procedure VectorCapR0
c
c begin procedure CalculateR1;
c
c     calculate the vector product R1 = CapR0 InvM
c
      do i = 1, 6
          R1(I) = 0D0
          do j = 1, 6
              R1(i) = R1(I) + CapR0(j) * InvM(i,j)
          end do
      end do
c
c begin Procedure ETerms;
c
c     Include the effects of the E-Terms of aberration
c
      Scalar_r1 = sqrt(R1(1)**2 + R1(2)**2 + R1(3)**2)
c
      DO I = 1, 3
c
c         form s1 = vector R1 / scalar R1
          s1(I) = R1(I)/Scalar_r1
c
c         form sdot1 = vector Rdot1 / scalar R1
          sdot1(I) = R1(I+3)/Scalar_r1
c
          S(I) = S1(I)
      end do
c
c begin procedure Calculate_r;
c
c     calculate s dot A, which is a scalar product
c
      iprod = 3
      sDotA = ScalProd (iprod, s, A)
c
c     Finish calculation of r
      DO I = 1, 3
          r(I) = s1(I) + A(I) - sDotA*s(I)
      END DO
c
c end procedure Calculate_r;
c
c     set s=r/Scalar_r  (was Scalar_r1 in pascal)
      Scalar_r = sqrt(R(1)**2 + R(2)**2 + R(3)**2) ! added MJG
      DO I = 1, 3
          s(I) = r(I)/Scalar_r
      END DO
c
c     Iterate the expression for r once or twice until a consistent value for
c     r is obtained
      test = .false.
      do while (.not. test)
          DO I = 1, 3                 ! explicit do loop MJG
              Temp_r(i) = r(i)
          END DO
C
c begin procedure Calculate_r;
c
c         calculate s dot A, which is a scalar product
c
          sDotA = ScalProd (iprod, s, A)
c
c         Finish calculation of r
          DO I = 1, 3
              r(I) = s1(I) + A(I) - sDotA*s(I)
          END DO
c
c end procedure Calculate_r;
c
          test = (r(1) .eq. Temp_r(1) .AND.
     &            r(2) .eq. Temp_r(2) .AND.
     &            r(3) .eq. Temp_r(3))
      end do
c
c end Procedure ETerms;
c
c begin Procedure Calculate_rDot;
c
c     calculate s dot Adot, which is a scalar product
      sDotAdot = ScalProd (iprod, s, Adot)
c
c     calculate rdot
      DO I = 1, 3
          rdot(I) = sdot1(I) + Adot(I) - sDotAdot*s(I)
      END DO
c 
c end Procedure Calculate_rDot;
c
c begin Procedure CalculateFK4;
c
c     mean position, proper motion, parallax and radial velocity for B1950.0
c
      x = r(1)
      y = r(2)
      z = r(3)
c
      xdot = rDot(1)
      ydot = rDot(2)
      zdot = rDot(3)
c
      Scalar_r = sqrt(x**2 + y**2 + z**2)
      SinDec = z/Scalar_r
      B1950DecR = asin(SinDec)
      SB1950RAR = asin((y/Scalar_r)/(cos(B1950DecR)))
      B1950RAR  = acos((x/Scalar_r)/(cos(B1950DecR)))
c
c     convert to degrees and check quadrant
      B1950RA = B1950RAR / DTR
      B1950Dec = B1950DecR / DTR
      if (SB1950RAR .LT. 0d0) then
          B1950RA = 360d0-B1950RA
      end if
c
c end procedure CalculateFK4
      END
****************
