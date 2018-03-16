**************************
      SUBROUTINE SPLINEATX (XIN,YIN,NPTS,XSTART,XEND,
     &                      XSPLINE,YSPLINE,ISPLINE,
     &                      YSPLOUT,DY,RMSERR)
**************************
*     calculate spline at input X values, using parabolic fit to spline 
*     MJG 1995 04 25
*
*     inputs:
*         XIN, YIN input data arrays of NPTS,
*         starting and ending at user defined XSTART and XEND
*         XSPLINE, YSPLINE arrays of fitted splines with ISPLINE points
*     outputs:
*         YSPLOUT array of Y values from spline, interpolated at XIN points
*         DY      array of residuals
*         RMSERR  rms error of spline fit
*
*
*     This routine is called by: SPLNFIT
*
*     This routine calls: PBOLA
*
*
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NPTS, ISPLINE
      REAL*8  XSTART, XEND, RMSERR
      REAL*8 XIN(*), YIN(*), XSPLINE(*), YSPLINE(*), YSPLOUT(*), DY(*)
*
*     initialise totals for error calculation
      SUMDY = 0D0
      SUMSQDY = 0D0
      NVAL = 0
      XSTEP = XSPLINE(2) - XSPLINE(1)
      DO I = 1, NPTS
*         is X value in user specified range ?
          IF (XIN(I) .GE. XSTART .AND. XIN(I) .LE. XEND) THEN
*             print check:
*             PRINT *,'X-start:',XSTART,' X-end:',XEND,' X-in:',XIN(I)
*             find the array index of nearest point in spline array
              ISTART = NINT((XIN(I)-XSTART)/XSTEP) - 1
*             keep array index for parabola interpolation legal
              IF (ISTART .LT. 1) ISTART = 1
              IF (ISTART .GT. ISPLINE-3) ISTART = ISPLINE-3
*             get the parabola coefficients for 3 points centered on XIN(I)
C             PRINT *,'X,YSP=',XSPLINE(ISTART), YSPLINE(ISTART)
              CALL PBOLA (XSPLINE(ISTART),YSPLINE(ISTART),A,B,C)
*             compute new Y value from coefficients
              YSPLOUT(I) = (A*XIN(I) + B)*XIN(I) + C
*             store residuals for writing to output file
              DY(I) = YIN(I) - YSPLOUT(I)
*             accumulate results for RMS error calculation
              NVAL = NVAL + 1
              SUMSQDY = SUMSQDY + DY(I)*DY(I)
          ELSE
*             check whether this criterion is fulfilled:
*             PRINT *,'Zero value coming up...'
              YSPLOUT(I) = 0D0
          END IF
*         print check:
*         PRINT *,'Y-spline at x: ',YSPLOUT(I) 
      END DO
*     rms error from spline fit
      RMSERR = SQRT (SUMSQDY/NVAL)
      RETURN
      END
