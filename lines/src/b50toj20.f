*************************
      SUBROUTINE B50TOJ20 (B1950RA, B1950DEC, J2000RA, J2000DEC)
*************************
C     convert to B1950 RA DEC to J2000 RA DEC 
C     based on LC's J2000 pascal program, fortran by MJG 1994 08 31
C     note that proper motion, parallax and radial velocity are NOT computed
c     references:
c     Astronomical Almanac p B43
c     Standish, E.M., 1982, A&A, 115, 20
c     Aoki et al., 1983, A&A, 128, 263
c
*     other subroutines called:
c     SCALPROD
*
      IMPLICIT NONE
      integer*4 i,j,iprod ! loop variables, local
      real*8    B1950RA   ! B1950 Right ascension in degrees, input
      real*8    B1950DEC  ! B1950 Declination in degrees, input
      real*8    J2000RA   ! J2000 Right Ascension in degrees, output
      real*8    J2000Dec  ! J2000 Declination in degrees, output
      real*8    RAR       ! B1950 Right Ascension in radians, local
      real*8    DecR      ! B1950 Declination in radians, local
      real*8    DTR       ! degrees to radians conversion
      real*8    PMRA      ! Proper Motion in RA, arcseconds per Julian Century
      real*8    PMDec     ! Proper Motion in Dec, arcseconds per Julian Century
      real*8    ParX      ! parallax in seconds of arc, input
      real*8    RadVel    ! Radial Velocity in km/s for J2000, input
      real*8    rdotcons  ! constant for velocity vector, local
      real*8    SCALPROD  ! scalar product function, local
      real*8    r0(3)     ! Rectangular components of the position vector
      real*8    r1(3)     ! Rectangular components of the position vector
      real*8    rDot0(3)  ! Rectangular components of velocity vector
      real*8    rDot1(3)  ! Rectangular components of velocity vector
      real*8    A(3)      ! constant vector
      real*8    Adot(3)   ! constant vector
      real*8    CapR1(6)  ! the vector (r0,rDot0)
      real*8    R(6)      ! vector M*CapR0
      real*8    M(6,6)    ! matrix M
c
c     variables used in calculating effects of the E-terms of aberration
      real*8    r0DotAdot, rDot0A
c
c     variables used to obtain FK5 parameters
      real*8    J2000RAR, SJ2000RAR, J2000DecR
      real*8    x, y, z, scalar_r, SinDec
c
      DATA M(1,1) /+0.999 925 6782D0/,
     &     M(1,2) /-0.011 182 0611D0/,
     &     M(1,3) /-0.004 857 9477D0/,
     &     M(1,4) /+0.000 002 423 950 18D0/,
     &     M(1,5) /-0.000 000 027 106 63D0/,
     &     M(1,6) /-0.000 000 011 776 56D0/
      DATA M(2,1) /+0.011 182 0610D0/,
     &     M(2,2) /+0.999 937 4784D0/,
     &     M(2,3) /-0.000 027 1765D0/,
     &     M(2,4) /+0.000 000 027 106 63D0/,
     &     M(2,5) /+0.000 002 423 978 78D0/,
     &     M(2,6) /-0.000 000 000 065 87D0/
      DATA M(3,1) /+0.004 857 9479D0/,
     &     M(3,2) /-0.000 027 1474D0/,
     &     M(3,3) /+0.999 988 1997D0/,
     &     M(3,4) /+0.000 000 011 776 56D0/,
     &     M(3,5) /-0.000 000 000 065 82D0/,
     &     M(3,6) /+0.000 002 424 101 73D0/
      DATA M(4,1) /-0.000 551D0/,
     &     M(4,2) /-0.238 565D0/,
     &     M(4,3) /+0.435 739D0/,
     &     M(4,4) /+0.999 947 04D0/,
     &     M(4,5) /-0.011 182 51D0/,
     &     M(4,6) /-0.004 857 67D0/
      DATA M(5,1) /+0.238 514D0/,
     &     M(5,2) /-0.002 667D0/,
     &     M(5,3) /-0.008 541D0/,
     &     M(5,4) /+0.011 182 51D0/,
     &     M(5,5) /+0.999 958 83D0/,
     &     M(5,6) /-0.000 027 18D0/
      DATA M(6,1) /-0.435 623D0/,
     &     M(6,2) /+0.012 254D0/,
     &     M(6,3) /+0.002 117D0/,
     &     M(6,4) /+0.004 857 67D0/,
     &     M(6,5) /-0.000 027 14D0/,
     &     M(6,6) /+1.000 009 56D0/
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
c
c begin procedure InitializeVariables;
c
c     Initialize input variables, especially parallax and radial velocity must
c     be zero if they are unspecified when using the program
      RAR = B1950RA * DTR
      DECR = B1950DEC * DTR
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
c begin Procedure ETerms;
c
c     Remove the effects of the E-Terms of aberration
c
c     calculate r0 dot A, which is a scalar product
c
      iprod  = 3
      rDot0A = ScalProd (iprod, r0, A)
c
c     calculate r0 dot Adot, which is a scalar product
c
      r0DotAdot = ScalProd (iprod, r0, Adot)
c
c     calculate r1 and r1dot
      DO I = 1, 3
          r1(I) = r0(I) - A(I) + rDot0A*r0(I)
          rdot1(I) = rdot0(I) - A(I) + r0DotAdot*r0(I)
      END DO
c
c end Procedure ETerms;
c
c begin procedure VectorCapR1
c
c     Form the vector CapR1 by using r1 as first 3 elements and rDot1 as
c     last 3
c
      CapR1(1) = r1(1)
      CapR1(2) = r1(2)
      CapR1(3) = r1(3)
      CapR1(4) = rDot1(1)
      CapR1(5) = rDot1(2)
      CapR1(6) = rDot1(3)
c
c end procedure VectorCapR1
c
c begin procedure CalculateR;
c
c     calculate the vector product R = M CapR1
c
      do i = 1, 6
          R(I) = 0D0
          do j = 1, 6
              R(i) = R(I) + M(i,j) * CapR1(j)
          end do
      end do
c
c begin Procedure CalculateFK5;
c
c     mean position for J2000
c
      x = r(1)
      y = r(2)
      z = r(3)
c
      Scalar_r = sqrt(x**2 + y**2 + z**2)
      SinDec = z/Scalar_r
      J2000DecR = asin(SinDec)
      SJ2000RAR = asin((y/Scalar_r)/(cos(J2000DecR)))
      J2000RAR  = acos((x/Scalar_r)/(cos(J2000DecR)))
c
c     convert to degrees and check quadrant
      J2000RA = J2000RAR / DTR
      J2000Dec = J2000DecR / DTR
      if (SJ2000RAR .LT. 0d0) then
          J2000RA = 360d0-J2000RA
      end if
c
c end procedure CalculateFK5
      RETURN
      END
******************************
