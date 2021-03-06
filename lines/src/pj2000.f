      PROGRAM PJ2000
C     PJ2000.ALL contains all the subroutines INCLUDED in PJ2000.FOR
C     Coordinate transform from J2000 to specified epoch 
C
C     MJG 1994 09 02
C
      IMPLICIT NONE
      INTEGER*4 IY, IT(5), I,
     &          IRAHR, IRAMIN, IDDEG, IDMIN,
     &          IRAHR20, IRAMIN20, IDDEG20, IDMIN20
      REAL*8    RA2000, DEC2000, RAT, DECT, DJULDAY,
     &          RASEC20, DSEC20, RASEC, DSEC
C
    1 PRINT *,'Transform coordinates from J2000 to specified epoch '
*
*     get input coordinates
      RA2000 = 999D0
      CALL HMSDMS (RA2000, DEC2000)
      IF (RA2000 .EQ. 999D0) STOP
c
      IY = 0
      DO I = 1, 5
          IT(I) = 0
      END DO
      PRINT *,'Epoch to transform to (Y,D,H,M,S ; / exits ) ?'
      READ  *, IY, IT(5), IT(4), IT(3), IT(2)
      IF (IY .EQ. 0) STOP
C
C     get the julian date of epoch
      CALL JD (IY, IT, DJULDAY)
c
c     precess to epoch
      CALL J2000PR (RA2000, DEC2000, DJULDAY, RAT, DECT)
*
*     convert decimal degrees to hours mins secs
      CALL DTHMS (RAT,  IRAHR, IRAMIN, RASEC)
      CALL DTDMS (DECT, IDDEG, IDMIN, DSEC)
*
      CALL DTHMS (RA2000,  IRAHR20, IRAMIN20, RASEC20)
      CALL DTDMS (DEC2000, IDDEG20, IDMIN20,  DSEC20)
C
      PRINT *,'Mean coordinates of epoch (no nutation or aberration):'
      PRINT 2100, DJULDAY,
     &            IRAHR20, IRAMIN20, RASEC20,
     &            IDDEG20, IDMIN20,  DSEC20,
     &            IRAHR,   IRAMIN,   RASEC,  
     &            IDDEG,   IDMIN,    DSEC
 2100 FORMAT(12X,'J2000',16X,'epoch JD',F12.3/
     &   4X,'H',2X,'M',2X,'S',6X,'D',2X,'M',2X,'S',10X,
     &      'H',2X,'M',2X,'S',6X,'D',2X,'M',2X,'S',7X/
     &   I5,I3,F5.1,I5,I3,F5.1,4X,
     &   I5,I3,F5.1,I5,I3,F5.1,4X/)
C
      GO TO 1
      END
*********
C $INCLUDE:'HMSDMS.SUB'
C $INCLUDE:'JD.SUB'
C $INCLUDE:'J2000PR.SUB'
C $INCLUDE:'J2000PM.SUB'
C $INCLUDE:'DTHMS.SUB'
C $INCLUDE:'DTDMS.SUB'
************************
       SUBROUTINE HMSDMS (RA, DEC)
************************
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
          PRINT *,'illegal choice'
      END IF
      RETURN
      END
*******************
      SUBROUTINE JD (IY,IT,DJULDA)
*******************
*     convert UT to JD
*     inputs :
*     iy = year
*     it(1)=msec it(2)=sec it(3)=mins it(4)=hours it(5)=days  UT
*
*     outputs :
*     djulda = full Julian date
*
      IMPLICIT NONE
      INTEGER*4  IT(5), IY, NYRM1, IC, JULDA
      REAL*8     DJULDA
*
      NYRM1 = IY - 1
      IC = NYRM1 / 100
      JULDA = 1721425 + 365*NYRM1 + NYRM1/4 - IC + IC/4
      DJULDA = JULDA + (((DBLE(IT(1))/100d0 + DBLE(IT(2)))/60d0
     *         + DBLE(IT(3)))/60d0 + DBLE(IT(4)) - 12d0)/24d0
     *         + DBLE(IT(5))
      RETURN
      END
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
      RETURN
      END
**********************
      SUBROUTINE DTDMS (DEG,IRD,IRM,RS)
**********************
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
