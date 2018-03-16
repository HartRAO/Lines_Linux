*************************
      SUBROUTINE COORDCNV (MEM,ERROR)
*************************
*     ensure that both equatorial and galactic coords are present
*
*     other subroutines called:
*     b50tog, gtob50, j20tob50
*
      IMPLICIT  NONE
*
      INTEGER   ERROR           ! output error status
      INTEGER   IEQUINOX_IN     ! local - save for DOWN conversion checks
      INTEGER   MEM             ! input memory to use
      REAL*8    GL              ! local galactic longitude
      REAL*8    GB              ! local galactic latitude
      REAL*8    RA50            ! local 1950 ra
      REAL*8    DEC50           ! local 1950 dec
      REAL*8    RA20            ! local 2000 ra
      REAL*8    DEC20           ! local 2000 dec
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in COORDCNV - parameters on entry are:'
      ERROR = 0
      IEQUINOX_IN = EQUINOX(MEM)
      IF (DB) PRINT *,'COORDSYS=',COORDSYS(MEM)
      IF (DB) PRINT *,'EQUINOX=',EQUINOX(MEM)
      IF (DB) PRINT *,'RA  =',RA(MEM)
      IF (DB) PRINT *,'DEC =',DEC(MEM)
      IF (DB) PRINT *,'GLII=',GLII(MEM)
      IF (DB) PRINT *,'GBII=',GBII(MEM)
*
      IF (IDINT(EQUINOX(MEM)) .EQ. 0) THEN
          IF (GLII(MEM) .NE. 0D0 .AND. GBII(MEM) .NE. 0D0) THEN
*             convert galactic to 1950 equatorial
              IF (DB) PRINT *,'convert G to B1950'
              GL = GLII(MEM)
              GB = GBII(MEM)
              IF (DB) PRINT *,'L,B=',GL,GB
              CALL GTOB50 (GL,GB,RA50,DEC50)
              IF (DB) PRINT *,'convert B1950 to J2000'
              CALL B50TOJ20 (RA50,DEC50,RA20,DEC20)
              EQUINOX(MEM) = 2000.
              RA(MEM) = RA20
              DEC(MEM) = DEC20
              IF (DB) PRINT *,'J2000 RA,DEC=',RA20,DEC20
          END IF
      END IF
*
      IF (GLII(MEM) .EQ. 0D0 .AND. GBII(MEM) .EQ. 0D0) THEN
          IF (IDINT(EQUINOX(MEM)) .EQ. 2000) THEN
*             convert 2000 equatorial to 1950
              IF (DB) PRINT *,'convert J2000 to B1950'
              RA20 = RA(MEM)
              DEC20 = DEC(MEM)
              IF (DB) PRINT *,'J2000 RA,DEC=',RA20,DEC20
              CALL J20TOB50 (RA20,DEC20,RA50,DEC50)
              IF (DB) PRINT *,'B1950 RA,DEC=',RA50,DEC50
          ELSE IF (IDINT(EQUINOX(MEM)) .EQ. 1950) THEN
              RA50 = RA(MEM)
              DEC50 = DEC(MEM)
              IF (DB) PRINT *,'B1950 RA,DEC=',RA50,DEC50
          END IF
          IF (RA50 .NE. 0D0 .AND. DEC50 .NE. 0D0) THEN
*             convert 1950 equatorial to galactic
              IF (DB) PRINT *,'convert B1950 to G'
              CALL B50TOG (RA50,DEC50,GL,GB)
              GLII(MEM) = GL
              GBII(MEM) = GB
              IF (DB) PRINT *,'GL,GB=',GL,GB
          END IF
      END IF
*
*     repeat for "down" coordinates
*
      IF (DB) PRINT *,'RADOWN  =',RADOWN(MEM)
      IF (DB) PRINT *,'DECDOWN =',DECDOWN(MEM)
      IF (DB) PRINT *,'GLIIDOWN=',GLIIDOWN(MEM)
      IF (DB) PRINT *,'GBIIDOWN=',GBIIDOWN(MEM)
      IF (RADOWN(MEM)  .EQ. 0.0 .AND.
     &    DECDOWN(MEM) .EQ. 0.0 .AND.
     &    GLIIDOWN(MEM) .EQ. 0.0 .AND.
     &    GBIIDOWN(MEM) .EQ. 0.0) THEN
         IF (DB) PRINT *,'no DOWN coord conv'
         RETURN
      END IF
*
      IF (IEQUINOX_IN .EQ. 0) THEN
          IF (GLIIDOWN(MEM) .NE. 0D0 .AND. 
     &        GBIIDOWN(MEM) .NE. 0D0) THEN
*             convert galactic to 1950 equatorial
              IF (DB) PRINT *,'convert DOWN G to B1950'
              GL = GLIIDOWN(MEM)
              GB = GBIIDOWN(MEM)
              CALL GTOB50 (GL,GB,RA50,DEC50)
              IF (DB) PRINT *,'convert DOWN B1950 to J2000'
              CALL B50TOJ20 (RA50,DEC50,RA20,DEC20)
              RADOWN(MEM) = RA20
              DECDOWN(MEM) = DEC20
              IF (DB) PRINT *,'J2000 RADOWN,DECDOWN=',RA20,DEC20
          END IF
      END IF
*
      IF (GLIIDOWN(MEM) .EQ. 0D0 .AND. GBIIDOWN(MEM) .EQ. 0D0) THEN
          IF (IEQUINOX_IN .EQ. 2000) THEN
*             convert 2000 equatorial to 1950
              IF (DB) PRINT *,'convert DOWN J2000 to B1950'
              RA20 = RADOWN(MEM)
              DEC20 = DECDOWN(MEM)
              CALL J20TOB50 (RA20,DEC20,RA50,DEC50)
              IF (DB) PRINT *,'B1950 RADOWN,DECDOWN=',RA50,DEC50
          ELSE IF (IEQUINOX_IN .EQ. 1950) THEN
              RA50 = RADOWN(MEM)
              DEC50 = DECDOWN(MEM)
          END IF
          IF (RA50 .NE. 0D0 .AND. DEC50 .NE. 0D0) THEN
*             convert 1950 equatorial to galactic
              IF (DB) PRINT *,'convert DOWN B1950 to G'
              CALL B50TOG (RA50,DEC50,GL,GB)
              GLIIDOWN(MEM) = GL
              GBIIDOWN(MEM) = GB
          END IF
      END IF
*
      RETURN
      END
*********