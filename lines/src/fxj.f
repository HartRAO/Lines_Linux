*************************
      SUBROUTINE FXJ (ERROR)
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
      INTEGER   ISLOPE            ! local sign of dX/dI
      INTEGER   MEM               ! local memory in use
      INTEGER   NCHAR             ! external function
      REAL*8    CONSTANT          ! local intercept from linear regression
      REAL*8    RMSDEV            ! local rms deviation of X's from linear
      REAL*8    SLOPE             ! local slope of line fit
*
      INCLUDE 'lines.inc'
*
      ERROR = 0
      IF (DB) PRINT *,'in FXJ'
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
*     summarise the command
*
      WRITE (LBUF,*) 'Fix jumps in X in mem ', MEM
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
*
*     do linear regression thru X values to check linearity using fit rms
*
      CALL LINFIT(MEM,SLOPE,CONSTANT,RMSDEV)
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
      ISLOPE = +1
      IF (SLOPE .LT. 0.0) ISLOPE = -1
      DO I = FIRST_CH(MEM)+1, LAST_CH(MEM)-1
         IF ((XIN(MEM,I)-XIN(MEM,I-1))*ISLOPE .LT. 0.0) THEN
             WRITE (BUF,*) 'X(I) not monotonic: for I= ',I-1,I,I+1,
     &           ' XIN= ',XIN(MEM,I-1), XIN(MEM,I), XIN(MEM,I+1)
             PRINT *,BUF(1:NCHAR(BUF))
             IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
             IXSPACING(MEM) = 2  ! X spacing not monotonic
*
*            reset point to average of neighbours
             XIN(MEM,I) = (XIN(MEM,I-1) + XIN(MEM,I+1))/2
             WRITE (BUF,*)'X(',I,') now ',XIN(MEM,I)
             PRINT *,BUF(1:NCHAR(BUF))
             IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
         END IF
      END DO
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
