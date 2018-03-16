      PROGRAM COORDS
********************
*       COORDS.ALL contains all the subroutines INCLUDED in COORDS.FOR
*  Coordinate conversion J2000 to from B1950 RA/DEC to from LII/BII 
*       MJG 1994 09 02
*
      IMPLICIT NONE
      INTEGER*4 INDEX, IRAHR50, IRAMIN50, IDDEG50, IDMIN50,
     &                 IRAHR20, IRAMIN20, IDDEG20, IDMIN20
      REAL*8    RAJ20, DECJ20, RAB50, DECB50, GLII, GBII,
     &          RASEC50, DSEC50, RASEC20, DSEC20
*
      PRINT *,' COORDS converts astronomical coordinates'
      PRINT *,'        to or from B1950, J2000 and Galactic.'
      PRINT *,' What is your input coordinate type ?'
      PRINT *,' Enter 1 for J2000 RA, Dec'
      PRINT *,'       2 for B1950 RA, Dec'
      PRINT *,'       3 for Galactic longitude and latitude'
      READ *,INDEX
*
    1 IF (INDEX .EQ. 1) THEN
   10     RAJ20 = 999.0
          CALL HMSDMS (RAJ20,DECJ20)
          IF (RAJ20 .EQ. 999.0) STOP ' bye'
          IF (RAJ20 .LT. 0. .OR. RAJ20 .GT. 360.
     &        .OR. DECJ20 .LT. -90. .OR. DECJ20 .GT. 360.) THEN
              PRINT 1150
 1150         FORMAT(' COORDINATES OUT OF RANGE')
              GO TO 10
          END IF
*
*         convert J2000 RA, Dec to B1950 RA, Dec
          CALL J20TOB50 (RAJ20, DECJ20, RAB50, DECB50)
*
*         convert B1950 to galactic
          CALL B50TOG (RAB50,DECB50, GLII,GBII)
*
      ELSE IF (INDEX .EQ. 2) THEN
*
*         read 1950 Equatorial coordinates
*
   20     RAB50 = 999.0
          CALL HMSDMS (RAB50,DECB50)
          IF (RAB50 .EQ. 999.0) STOP ' bye'
          IF (RAB50 .LT. 0. .OR. RAB50 .GT. 360.
     &        .OR. DECB50 .LT. -90. .OR. DECB50 .GT. 360.) THEN
              PRINT 1150
              GO TO 20
          END IF
*
*         convert B1950 to J2000
          CALL B50TOJ20 (RAB50, DECB50, RAJ20, DECJ20)
*
*         convert B1950 to galactic
          CALL B50TOG (RAB50,DECB50, GLII,GBII)
*
      ELSE IF (INDEX .EQ. 3) THEN
*
*         read Galactic coordinates
*
   30     GLII = 999.
          PRINT 1200
 1200     FORMAT(/' Enter LII BII in decimal degrees (/ exits)')
          READ *, GLII, GBII 
          IF (GLII .EQ. 999.) STOP ' bye'
          IF (GLII .LT. 0. .OR. GLII .GT. 360. .OR. 
     &        GBII .LT. -90. .OR. GBII .GT. 360.) THEN
              PRINT 1150
              GO TO 30
          END IF
*
*         convert galactic to B1950 RA, Dec
          CALL GTOB50 (GLII,GBII, RAB50,DECB50)
*
*         convert B1950 to J2000
          CALL B50TOJ20 (RAB50, DECB50, RAJ20, DECJ20)
      ELSE
          STOP ' bye'
      END IF
* 
*     convert decimal degrees to hours mins secs
      CALL DTHMS (RAB50,  IRAHR50,IRAMIN50,RASEC50)
      CALL DTDMS (DECB50, IDDEG50,IDMIN50,DSEC50)
*
      CALL DTHMS (RAJ20,  IRAHR20,IRAMIN20,RASEC20)
      CALL DTDMS (DECJ20, IDDEG20,IDMIN20,DSEC20)
c
      PRINT 2100, IRAHR20,IRAMIN20,RASEC20,
     &            IDDEG20,IDMIN20,DSEC20,
     &            IRAHR50,IRAMIN50,RASEC50,
     &            IDDEG50,IDMIN50,DSEC50,
     &            GLII,GBII
 2100 FORMAT(12X,'J2000',25X,'B1950',16X,'Galactic'/
     &   4X,'H',2X,'M',2X,'S',6X,'D',2X,'M',2X,'S',10X,
     &      'H',2X,'M',2X,'S',6X,'D',2X,'M',2X,'S',7X,
     &   'LII',7X,'BII'/
     &   I5,I3,F5.1,I5,I3,F5.1,4X,
     &   I5,I3,F5.1,I5,I3,F5.1,4X,
     &   F7.3,F9.3/)
*
      GO TO 1 
      END 
*******************
c $INCLUDE:'J20TOB50.SUB'
c $INCLUDE:'B50TOJ20.SUB'
c $INCLUDE:'SCALPROD.SUB'
c $INCLUDE:'B50TOG.SUB'
c $INCLUDE:'GTOB50.SUB'
c $INCLUDE:'HMSDMS.SUB'
c $INCLUDE:'DTHMS.SUB'
c $INCLUDE:'DTDMS.SUB'
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
c     warning calls routine SCALPROD
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
      DATA InvM(1,1) /+0.9999256795/,
     &     InvM(1,2) /+0.0111814828/,
     &     InvM(1,3) /+0.0048590039/,
     &     InvM(1,4) /-0.00000242389840/,
     &     InvM(1,5) /-0.00000002710544/,
     &     InvM(1,6) /-0.00000001177742/
      DATA InvM(2,1) /-0.0111814828/,
     &     InvM(2,2) /+0.9999374849/,
     &     InvM(2,3) /-0.0000271771/,
     &     InvM(2,4) /+0.00000002710544/,
     &     InvM(2,5) /-0.00000242392702/,
     &     InvM(2,6) /+0.00000000006585/
      DATA InvM(3,1) /-0.0048590040/,
     &     InvM(3,2) /-0.0000271557/,
     &     InvM(3,3) /+0.9999881946/,
     &     InvM(3,4) /+0.00000001177742/,
     &     InvM(3,5) /+0.00000000006585/,
     &     InvM(3,6) /-0.00000242404995/
      DATA InvM(4,1) /-0.000551/,
     &     InvM(4,2) /+0.238509/,
     &     InvM(4,3) /-0.435614/,
     &     InvM(4,4) /+0.99990432/,
     &     InvM(4,5) /+0.01118145/,
     &     InvM(4,6) /+0.00485852/
      DATA InvM(5,1) /-0.238560/,
     &     InvM(5,2) /-0.002667/,
     &     InvM(5,3) /+0.012254/,
     &     InvM(5,4) /-0.01118145/,
     &     InvM(5,5) /+0.99991613/,
     &     InvM(5,6) /-0.00002717/
      DATA InvM(6,1) /+0.435730/,
     &     InvM(6,2) /-0.008541/,
     &     InvM(6,3) /+0.002117/,
     &     InvM(6,4) /-0.00485852/,
     &     InvM(6,5) /-0.00002716/,
     &     InvM(6,6) /+0.99996684/
c
      DATA A(1) /-1.62557D-6/,    ! radians
     &     A(2) /-0.31919D-6/,
     &     A(3) /-0.13843D-6/
c
      DATA Adot(1) /+1.245D-3/,    ! arcsec per tropical century
     &     Adot(2) /-1.580D-3/,
     &     Adot(3) /-0.659D-3/
c
      DATA DTR / 0.017453293 /
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
c     warning - calls subroutine SCALPROD

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
      DATA M(1,1) /+0.999 925 6782/,
     &     M(1,2) /-0.011 182 0611/,
     &     M(1,3) /-0.004 857 9477/,
     &     M(1,4) /+0.000 002 423 950 18/,
     &     M(1,5) /-0.000 000 027 106 63/,
     &     M(1,6) /-0.000 000 011 776 56/
      DATA M(2,1) /+0.011 182 0610/,
     &     M(2,2) /+0.999 937 4784/,
     &     M(2,3) /-0.000 027 1765/,
     &     M(2,4) /+0.000 000 027 106 63/,
     &     M(2,5) /+0.000 002 423 978 78/,
     &     M(2,6) /-0.000 000 000 065 87/
      DATA M(3,1) /+0.004 857 9479/,
     &     M(3,2) /-0.000 027 1474/,
     &     M(3,3) /+0.999 988 1997/,
     &     M(3,4) /+0.000 000 011 776 56/,
     &     M(3,5) /-0.000 000 000 065 82/,
     &     M(3,6) /+0.000 002 424 101 73/
      DATA M(4,1) /-0.000 551/,
     &     M(4,2) /-0.238 565/,
     &     M(4,3) /+0.435 739/,
     &     M(4,4) /+0.999 947 04/,
     &     M(4,5) /-0.011 182 51/,
     &     M(4,6) /-0.004 857 67/
      DATA M(5,1) /+0.238 514/,
     &     M(5,2) /-0.002 667/,
     &     M(5,3) /-0.008 541/,
     &     M(5,4) /+0.011 182 51/,
     &     M(5,5) /+0.999 958 83/,
     &     M(5,6) /-0.000 027 18/
      DATA M(6,1) /-0.435 623/,
     &     M(6,2) /+0.012 254/,
     &     M(6,3) /+0.002 117/,
     &     M(6,4) /+0.004 857 67/,
     &     M(6,5) /-0.000 027 14/,
     &     M(6,6) /+1.000 009 56/
c
      DATA A(1) /-1.62557D-6/,    ! radians
     &     A(2) /-0.31919D-6/,
     &     A(3) /-0.13843D-6/
c
      DATA Adot(1) /+1.245D-3/,    ! arcsec per tropical century
     &     Adot(2) /-1.580D-3/,
     &     Adot(3) /-0.659D-3/
c
      DATA DTR / 0.017453293 /
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
      Real*8 Function ScalProd (iprod, v1, v2)
******************************
*     compute scalr product of two vectors LC / MJG 02/09/94
      IMPLICIT NONE
      integer*4  i, iprod
      real*8     v1(1), v2(1)
*
      ScalProd = 0D0
      do i = 1, iprod
          ScalProd = Scalprod + v1(i) * v2(i)
      end do
      return
      end
****************
      SUBROUTINE B50TOG (RA, DEC, GLII, GBII)
*********************************************
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
      IF(GLII.LT.0.D0.OR.GLII.GE.360.D0) THEN 
          GLII=GLII-DSIGN(360.D0,GLII) 
      END IF
      ARGB=SIND*SINDEC+COSD*COSDEC*DCOS(RADIF)
      GBII=DATAN(ARGB/DSQRT(1.D0-ARGB*ARGB))/DTR
      IF(GBII.GT.180.D0) GBII=GBII-360.D0 
      RETURN
      END 
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
       SUBROUTINE HMSDMS (RA, DEC)
***********************
*     read RA and DEC in decimal degrees or HMS DMS, 
*     and convert to decimal degrees
*     inputs  : none
*     outputs :
*         RA, DEC     REAL*8    ra and dec in decimal degrees
*
      INTEGER   I, INPUT
      REAL*8    P(6), RA, DEC
      DATA      INPUT / 0 /
*
   20 IF (INPUT .EQ. 0) THEN
          PRINT *,'Is RA, DEC in degrees (1) or H M S, D M S (2) ?'
          READ  *,INPUT
      END IF
*
      IF (INPUT .EQ. 1) THEN
          PRINT *,'Enter RA, DEC in decimal degrees (/ exits)'
          READ  *, RA, DEC
      ELSE IF (INPUT .EQ. 2) THEN
          P(1) = -999.
          PRINT *,'Enter RA, DEC in HMS, DMS (/ exits)'
          READ *, (P(I),I = 1,6)
          IF (P(1) .NE. -999.0) THEN
*             coordinates were entered by the user so convert to degrees
              RA = ((P(1)*60 + P(2))*60 + P(3)) / 240
              DEC = ABS(P(4)) + ABS(P(5))/60 + P(6)/3600
*             check sign on dec
              IF (P(4) .LT. 0 .OR.
     &            P(4) .EQ. 0 .AND. P(5) .LT. 0) THEN
                  DEC = -DEC
              END IF
          END IF
      ELSE
*         illegal choice
          PRINT *,'?'
          GO TO 20
      END IF
      RETURN
      END
****************
      SUBROUTINE DTHMS (DEG,IRH,IRM,RS)
***************************************
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
      RETURN
      END
****************
      SUBROUTINE DTDMS (DEG,IRD,IRM,RS)
***************************************
C     convert real*8 decimal degrees (DEG)
C     to integer degrees (IRD),
C        integer minutes (IRM),
C        real*8  seconds (RS).
C     rev MJG 1992 04 17
C
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
35    IF(DEGA.LT.D180) GO TO 40
          DEGA=DEGA-D360
40    SECS=DEGA*D3600
      IRD=DEGA
      IRM=(SECS-IRD*D3600)/D60
      RS=SECS-IRD*D3600-IRM*D60
      IF(DEGA.LT.D0 .AND. IRD.LT. 0) IRM=IABS(IRM)
      IF(DEGA.LT.D0 .AND. (IRD.LT.0 .OR. IRM.LT.0)) RS=ABS(RS)
      RETURN
      END
*********

