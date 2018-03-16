c-----------------------------------------
      SUBROUTINE MAXMIN(Y,NDATA,YMAX,YMIN)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION Y(*),BMAX(200),BMIN(200),CMAX(200),CMIN(200)
      BMAX(1)=Y(1)
      BMIN(1)=Y(1)
      DO 10 J=2,NDATA
          BMAX(J)=MAX(Y(J),BMAX(J-1))
          BMIN(J)=MIN(Y(J),BMIN(J-1))
          CMAX(J)=MAX(BMAX(J),BMAX(J-1))
 10       CMIN(J)=MIN(BMIN(J),BMIN(J-1))     
      YMAX=CMAX(NDATA)
      YMIN=CMIN(NDATA)
      RETURN
      end
