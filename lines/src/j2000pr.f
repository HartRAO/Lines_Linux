************************
      SUBROUTINE J2000PR (J2000RA, J2000DEC, DJULDAY, RAT, DECT)
************************
C     convert J2000 RA DEC to RA DEC of Julian date epoch
C     based on LC's J2000 pascal program, fortran by MJG 1994 08 31
C     note that proper motion, parallax and radial velocity are NOT computed
c     references:
c     Astronomical Almanac p B43, B18
c
      IMPLICIT NONE
      integer   i, j      ! loop variables
      real*8    DJULDAY   ! full julian date to precess to, input
      real*8    J2000RA   ! J2000 Right Ascension in degrees, input
      real*8    J2000Dec  ! J2000 Declination in degrees, input
      real*8    RAT       ! Right ascension of epoch in degrees, output
      real*8    DECT      ! Declination of epoch in degrees, output
      real*8    RAR       ! J2000 Right Ascension in radians
      real*8    DecR      ! J2000 Declination in radians
      real*8    RATR      ! Right ascension of epoch in radians
      real*8    SRATR     ! Right ascension of epoch in radians, for quadrant
      real*8    DecTR     ! Declination of epoch in radians
      real*8    DTR       ! degrees to radians conversion
      real*8    R0(3)     ! Rectangular components of input position vector
      real*8    R1(3)     ! Rectangular components of output position vector
      real*8    J2PM(3,3) ! J2000 precession matrix
      real*8    scalar_r1 ! magnitude of vector R1
      real*8    SinDec    ! temporary variable
C
      data DTR / 0.017453293 /      
c
c     convert input angles to radians
      RAR = J2000RA * DTR
      DecR = J2000DEC * DTR
c
c     Calculate the rectangular components of the position vector r0 
      r0(1) = cos(RAR)*cos(DecR)
      r0(2) = sin(RAR)*cos(DecR)
      r0(3) = sin(DecR)
c
c     get the precession matrix for J2000 to djulday
c
      CALL J2000PM (DJULDAY, J2PM)
c
c     calculate the precessed position vector: R1 = J2PM R0
c
      do i = 1, 3
          R1(I) = 0D0
          do j = 1, 3
              R1(I) = R1(I) + J2PM(I,J) * R0(J)
          end do
      end do
c
c     mean position of epoch
c
      Scalar_r1 = sqrt(r1(1)**2 + r1(2)**2 + r1(3)**2)
      SinDec = r1(3)/Scalar_r1
      DecTR = asin(SinDec)
      SRATR = asin((r1(2)/Scalar_r1)/(cos(DecTR)))
      RATR  = acos((r1(1)/Scalar_r1)/(cos(DecTR)))
c
c     convert to degrees and check quadrant for RA
      RAT = RATR / DTR
      DecT = DecTR / DTR
      if (SRATR .LT. 0d0) then
          RAT = 360d0 - RAT
      end if
      END
************************
