************************* 
      SUBROUTINE LINTSTAT (DB,DATA,L1,L2,ISIGN,
     &    CENTER,WMEAN,STDEV,SKEWNESS,PEAKEDNESS) 
*************************
*     calculate statistics on a line and its integral
*     data array limits for the line are l1 and l2. 
*     isign is +1 for a positive line, -1 for a negative line 
*
*     reference: Introduction to mathematical statistics,
*                Paul G Hoel, Wiley, 1947, pp8-18
*
*     other subroutines called:
*     none
*
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL  DB                   ! input debug on/off
      INTEGER  L1                   ! input first channel
      INTEGER  L2                   ! input last channel
      INTEGER  ISIGN                ! input sign of peak
      REAL*8   CENTER               ! output line centroid
      REAL*8   DATA(*)              ! input data array
      REAL*8   PEAKEDNESS           ! output peakedness
      REAL*8   SKEWNESS             ! output skewness
      REAL*8   STDEV                ! output standard deviation
      REAL*8   WMEAN                ! output weighted mean
* 
*     find the line centroid 'center' 
*
      IF (DB) PRINT *,'in LINTSTAT'
      DAJ = 0
      DAJP = 0
      DO J = L1,L2
*         find array index at which area summed from l1 to j
*         becomes greater than area summed from l2 to j+1 
          A1J = 0.0 
          A2J = 0.0 
          DO K = L1,J,1 
              A1J = A1J + DATA(K) * ISIGN
          END DO
          DO K = L2,J,-1
              A2J = A2J + DATA(K) * ISIGN
          END DO
          IF (DB) PRINT *,'J= ',J,' A1J= ',A1J,' A2J= ',A2J
          DAJ = ABS(A1J) - ABS(A2J) 
          IF (DAJ .GT. 0.0) THEN
*             area l1 to j now greater than area j to l2
              GO TO 712 
          ELSE
              DAJP = DAJ
          END IF
      END DO
      IF (DB) PRINT *,'DAJ=',DAJ,' DAJP=',DAJP
*
*     areas are equal somewhere between j-1 and j : 
*     for intersection of lines between a1jprev/a1j and a2jprev/a2j 
*
  712 DJ = DAJP / (DAJP - DAJ)
      CENTER = J - 1 + DJ 
      IF (DB) PRINT *,'Centroid = ',CENTER
* 
*     first moment is the weighted mean 
*
      SUMY = 0.0 
      SUMXY = 0.0
      DO I = L1, L2, 1
          DI = I - CENTER 
          IF (DB) PRINT *,'I=',I,' DI=',DI,' DATA(I)=',DATA(I),
     &            ' DI*DATA=',DI*DATA(I)
          SUMY  = SUMY  + DATA(I) * ISIGN
*         use the sign of peak to keep results positive
          SUMXY = SUMXY + DATA(I) * DI * ISIGN
      END DO
      RANGE = L2 - L1 + 1 
      WMEAN = SUMXY / SUMY + CENTER
      IF (DB) PRINT *,'SUMY=',SUMY,' SUMXY=',SUMXY,' WMEAN=',WMEAN
* 
*     second, third and fourth moments
*
      SUMXXY = 0D0
      SUMXXXY = 0D0
      SUMXXXXY = 0D0
      DO I = L1,L2,1
          DI = I - WMEAN
*         use the sign of peak to keep results positive
          DATAI = DATA(I) * ISIGN * DI * DI 
          SUMXXY = SUMXXY + DATAI 
          SUMXXXY = SUMXXXY + DATAI * DI
          SUMXXXXY = SUMXXXXY + DATAI * DI * DI 
      END DO
      VARIANCE = SUMXXY / SUMY
      IF (DB) PRINT *,'SUMXXY=',SUMXXY,' SUMXXXY=',SUMXXXY,
     &                ' SUMXXXXY=',SUMXXXXY,' VARIANCE=',VARIANCE
      IF (VARIANCE .GT. 0D0) THEN 
*         standard deviation is the root of the variance
          STDEV    = DSQRT (VARIANCE) 
*         skewness
          SKEWNESS = SUMXXXY / VARIANCE**1.5
*         peakedness (kurtosis) 
          PEAKEDNESS = SUMXXXXY / VARIANCE**2
      ELSE
*         prevent negative square root
          STDEV = -1
          SKEWNESS = -1 
          PEAKEDNESS = -1 
      END IF
      RETURN
      END 
