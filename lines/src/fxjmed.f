*************************
      SUBROUTINE FXJMED (ERROR)
*************************
*  Fix jumps in the X values of data (eg drift scan hour angles)
*
*     CMDP command parameters used:
*     1 = command mnemonic FXJ
*     2 = memory to work on, default to set memory (optional)
*
*     other routines called:
*     linfit
*  
*
      IMPLICIT NONE
      INTEGER   ERROR             ! output error status
      INTEGER   I                 ! local loop index
      INTEGER   IOS               ! local file i/o error status
      INTEGER   J                 ! local loop index
      INTEGER   MEM               ! local memory in use
      INTEGER   NCHAR             ! external function
      INTEGER   VERSION           ! median filter 1 or 2
      REAL*8    CONSTANT          ! local intercept from linear regression
      REAL*8    RMSDEV            ! local rms deviation of X's from linear
      REAL*8    SLOPE             ! local slope of line fit
*
      INCLUDE 'lines.inc'
*
      ERROR = 0
      IF (DB) PRINT *,'in FXJMED'
*
*     second parameter is the destination memory
*
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=181) MEM
  181     IF (IOS .NE. 0) THEN
              PRINT *, 'illegal ',CMDP(2)(1:NCHAR(CMDP(2)))
              MEM = 0
          END IF
      END IF
      IF (MEM .EQ. 0) THEN
          PRINT '(A,$)',' Correct X jumps in which memory ? '
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=182) MEM
  182     IF (IOS .NE. 0 .OR. MEM .LT. 1 .OR. MEM .GT. MAXMEM)
     &        MEM = MEMSET
      END IF 
*
*     do linear regression thru X values to check linearity using fit rms
*
      CALL LINFIT(MEM,SLOPE,CONSTANT,RMSDEV)
*
      WRITE (BUF,*) 'input dX/dI: avrg change per point = ',SLOPE
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,*) 'input dX/dI: rms deviation from linear = ',RMSDEV
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      IF (RMSDEV/SLOPE .GT. 1D-2) THEN
          WRITE (BUF,*) 'X(I) are IRREGULARLY spaced'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          IXSPACING(MEM) = 1  ! irregular spacing
      END IF
*
*     select the median filter:
*     filter 1 = 3RSSH,Twice or 2 = 4253H,Twice
*
      VERSION = 2
*
*     summarise the command
*
      WRITE (LBUF,*) 'Fix jumps in X in mem ', MEM,
     & ' using median filter 4253H,twice'
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
*
*     set up data arrays for median filter
*
      NPTS = LAST_CH(MEM) - FIRST_CH(MEM) + 1
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          XSPL(I) = I
          YSPL(I) = XIN(MEM,I)
      END DO
*
*     median filter using arrays for splines (YSPLINE in common block)
*     YSPL = input Y values
*     YSPLINE = median filtered SMOOTH output
*     YMSPLINE = median filtered ROUGH output
*
      CALL EDA_RSM (DB,YSPL,NPTS,YSPLINE,YMSPLINE,VERSION,ERROR)
*
      IF (ERROR .NE. 0) THEN
          PRINT *,'median filter error = ',ERROR
          RETURN
      END IF
*
*     store the median filtered SMOOTH output back in Xin
*
      J = 1
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          XIN(MEM,I) = YSPLINE(J)
          J = J + 1
      END DO
*
      WRITE (BUF,*) 'filtered Xs stored in mem ',MEM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*     do linear regression thru X values to check linearity using fit rms
*
      CALL LINFIT(MEM,SLOPE,CONSTANT,RMSDEV)
*
      CDELT1(MEM) = SLOPE
      CRVAL1(MEM) = CONSTANT + SLOPE
      IXSPACING(MEMRD) = 0 ! preset to regular spacing
*
      WRITE (BUF,*) 'dX/dI: avrg change per point = ',SLOPE
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      WRITE (BUF,*) 'dX/dI: rms deviation from linear = ',RMSDEV
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      IF (RMSDEV/SLOPE .GT. 1D-2) THEN
          WRITE (BUF,*) 'X(I) are IRREGULARLY spaced'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          IXSPACING(MEM) = 1  ! irregular spacing
      END IF
*
      RETURN
      END
******************************
