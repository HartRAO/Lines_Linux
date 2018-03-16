***********************
      SUBROUTINE XEDIT (ERROR)
***********************
*     fix non-monotonically spaced X() values
*
*
*     CMDP command parameters used:
*     1 = command mnemonic XED
*     2 = array index I / 'test' / 'list' / 'mono'
*     3 = new value for X(I)
*     5 = memory containing data, default to set memory
*     
*     subroutines called:
*
      IMPLICIT  NONE
      CHARACTER OPTION              ! parameter 1
      INTEGER   I                   ! loop index
      INTEGER   ERROR               ! error return
      INTEGER   IOS                 ! i/o status
      INTEGER   ISLOPE              ! sign of slope
      INTEGER   IVAL                ! index of X array val to modify
      INTEGER   MEM                 ! memory in use
      INTEGER   NCHAR               ! external function
      REAL*8    CONSTANT            ! from LINFIT
      REAL*8    RMSDEV              ! from LINFIT
      REAL*8    SLOPE               ! from LINFIT
      REAL*8    XVAL                ! new value for X(I)
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in XED'
      ERROR = 0
*
*     get the memory to use
      MEM = 0
*
      CALL QMEMUSED
      IF (DB) PRINT *,' MEMUSED=',MEMUSED
      IF (MEMUSED .EQ. 0) THEN
          PRINT *,'no data in memories'
          ERROR = 1
          RETURN
      END IF
*
      IF (NCMD .GE. 4) THEN
*         assume memory was specified as last parameter
          READ (CMDP(NCMD),*,IOSTAT=IOS,ERR=30) MEM
   30     CONTINUE
      END IF
*
      IF (MEM .EQ. 0) THEN
          IF (NAXIS(MEMSET) .EQ. 0) THEN 
              PRINT '(A,$)',' alter/test data in which memory ? '
              READ '(A)',CMDP(4)
              READ (CMDP(4),*,ERR=40,IOSTAT=IOS) MEM
   40         IF (IOS .NE. 0) THEN
                  PRINT 2005, CMDP(4)
                  ERROR = 1
                  GO TO 900
              END IF
          ELSE
              PRINT *,'mem=memset'
              MEM = MEMSET
          END IF
      END IF
*
      IF (MEM .LT. 1 .OR. MEM .GT. MAXMEM) THEN
          PRINT 2005,CMDP(4)
          ERROR = 1
          GO TO 900
      END IF
*
      IF (NAXIS(MEM) .EQ. 0) THEN
          PRINT *,'mem ',MEM,' is empty'
          ERROR = 1
          GO TO 900
      END IF
      IF (DB) PRINT *,'MEM = ',MEM
*
*
*     what user option?
*
      OPTION = ' '
      IVAL = 0
      IF (NCMD .GE. 2) THEN
*         see if 'test' or 'mono' given as second parameter
          READ (CMDP(2),'(A)',IOSTAT=IOS,ERR=50) BUF 
   50     IF (IOS .NE. 0) WRITE (BUF,'(A)') ' ' 
          CALL UPCASE (BUF)
          IF (BUF(1:1) .EQ. 'L') OPTION = 'L'
          IF (BUF(1:1) .EQ. 'M') OPTION = 'M'
          IF (BUF(1:1) .EQ. 'T') OPTION = 'T'
          IF (OPTION(1:1) .EQ. ' ') THEN
              READ (CMDP(2),*,IOSTAT=IOS,ERR=51) IVAL
   51         IF (IOS .NE. 0) PRINT 2005, CMDP(2)
          END IF
      END IF
*      
      XVAL = 0D0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=52) XVAL
   52     IF (IOS .NE. 0) PRINT 2005, CMDP(3)
      END IF
*
      IF (OPTION(1:1) .EQ. ' ' .AND. IVAL .EQ. 0) THEN
  101     PRINT *,'Valid I = ',FIRST_CH(MEM),' to ',LAST_CH(MEM)
          PRINT '(a,$)',' enter I, X(I) to be changed (/ exits)? '
          READ '(A)', BUF
          READ (BUF,*,ERR=101,IOSTAT=IOS) IVAL, XVAL
          IF (IOS .NE. 0) THEN
              PRINT 2005, BUF(1:NCHAR(BUF))
              GO TO 900
          END IF
      END IF
      IF (DB) PRINT *,'I=',I,' X(I)=',XVAL
*
      IF (OPTION(1:1) .EQ. 'T') THEN
*
*         do linear regression thru X values to check linearity using fit rms
*
          CALL LINFIT(MEM,SLOPE,CONSTANT,RMSDEV)
*
          PRINT *,'SLOPE = ',SLOPE,' CONSTANT = ',CONSTANT
          PRINT *,'FIT X(',FIRST_CH(MEM),')=',
     &             CONSTANT+SLOPE*FIRST_CH(MEM)
          PRINT *,'FIT X(',LAST_CH(MEM),')=',
     &             CONSTANT+SLOPE*LAST_CH(MEM)
*
          WRITE (BUF,*) 'dX/dI: average change per point  = ',SLOPE 
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
          WRITE (BUF,*) 'dX/dI: rms deviation from linear = ',RMSDEV
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
          IF (RMSDEV .GT. 1D-3) THEN
              WRITE (BUF,*) 'X(I) are IRREGULARLY spaced'
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
              IXSPACING(MEM) = 1  ! irregular spacing
          ELSE
              WRITE (BUF,*) 'X(I) are REGULARLY spaced'
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
              IXSPACING(MEM) = 0  ! regular spacing
          END IF
*
          ISLOPE = +1
          IF (SLOPE .LT. 0.0) ISLOPE = -1
          DO I = FIRST_CH(MEM)+1, LAST_CH(MEM)-1
             IF ((XIN(MEM,I)-XIN(MEM,I-1))*ISLOPE .LT. 0.0) THEN
                 WRITE (BUF,*) 'X(I) are not monotonic: for I= ',
     &               I-1,I,I+1,
     &               ' XIN= ',XIN(MEM,I-1), XIN(MEM,I), XIN(MEM,I+1)
                 PRINT *,BUF(1:NCHAR(BUF))
                 IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
                 IXSPACING(MEM) = 2  ! X spacing not monotonic
             END IF
          END DO
          IF (IXSPACING(MEM) .NE. 2) THEN
              WRITE (BUF,*) 'X(I) change monotonically'
                 PRINT *,BUF(1:NCHAR(BUF))
                 IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          END IF
*
      ELSE IF (OPTION(1:1) .EQ. 'L') THEN
*         "list" option
          DO I = FIRST_CH(MEM), LAST_CH(MEM), 5
              PRINT *,I,XIN(MEM,I),     I+1,XIN(MEM,I+1),
     &                I+2,XIN(MEM,I+2), I+3,XIN(MEM,I+3),
     &                I+4,XIN(MEM,I+4)
          END DO
*
      ELSE IF (OPTION(1:1) .EQ. 'M') THEN
*         "mono" option
          PRINT *,'Option to make monotonic not available'
*
      ELSE IF (OPTION(1:1) .EQ. ' ' .AND.
     &         IVAL .GE. FIRST_CH(MEM) .AND. 
     &         IVAL .LE. LAST_CH(MEM)) THEN
*         alter X(I) option
          XIN(MEM,IVAL) = XVAL
          WRITE (BUF,*) 'At I=',IVAL,' X(I)= ',XIN(MEM,IVAL),
     &        ' in Mem ',MEM
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
  900 RETURN
*
 2005 FORMAT (' illegal: ',A)
*
      END 
