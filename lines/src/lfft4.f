**********************
      SUBROUTINE LFFT4 (ERROR)
********************** 
*     set up FAST FOURIER TRANSFORM OF 2*N POINTS REAL DATA
* 
*     CMDP command parameters used:
*     1 = command mnemonic FT
*     2 = memory in which to fourier transform the data 
*
*     other subroutines called:
*     FORS1, which calls FXRL1 and FOUR1
*
      IMPLICIT  NONE
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local I/O status
      INTEGER   ISIGN           ! local/output sign of transform
      INTEGER   ISYM            ! local/output symmetry of transform
      INTEGER   MEM             ! local memory to use
      INTEGER   N2              ! local/output number of points * 2
      INTEGER   NCHAR           ! external function
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in LFFT4'
      ERROR = 0
*
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) MEM
  210     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' Fourier transform data in which memory ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) MEM
  220     IF (IOS .NE. 0) THEN
              ERROR = 1
              RETURN
          END IF
      END IF
*
      IF (NAXIS1(MEM) .LT. 0) THEN
          PRINT *,'memory ',MEM,' is empty'
          ERROR = 1
          RETURN
      END IF
*      
      IF (FIRST_CH(MEM) .NE. 1 .OR. LAST_CH(MEM) .NE. NAXIS1(MEM)) THEN
          PRINT *,'data have been windowed'
          ERROR = 1
          RETURN
      END IF
*
      ISIGN = -1
      ISYM = +1
      N2 = NAXIS1(MEM)*2
      DO I = 1, NAXIS1(MEM)+1
          Y(I) = YIN(MEM,I)
      END DO
*
*     standard inputs for FORS1:
*     N2 = number of points * 2
*     ISIGN = -1
*     ISYM  = +1
*     DATA  = unwindowed data array
*
      CALL FORS1 (N2,ISIGN,ISYM,Y)
*
      DO I = 1, NAXIS1(MEM)+1
          YIN(MEM,I) = Y(I)
      END DO
*      
      TRANSFRM(MEM) = TRANSFRM(MEM) + 1
      IF (TRANSFRM(MEM) .EQ. 2) TRANSFRM(MEM) = 0
*
      WRITE (BUF,*) NAXIS1(MEM),' point FFT done on mem ',MEM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      RETURN
      END
