*************************
      SUBROUTINE LOOPPARM
*************************
*     get do loop parameters
*
*     syntax of do loop:
*     DO [LOOPNAME] [LOOPSTART] [LOOPEND] [LOOPINC]
*
*     CMDP command parameters used:
*     1 = command mnemonic DO
*     2 = name of loop index
*     3 = start value of loop index
*     4 = end value of loop index
*     5 = value to increment loop index by 
*
      IMPLICIT NONE
      INTEGER  IOS                     ! file i/o status
      INTEGER  NCHAR                   ! external function
*       
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in LOOPPARM'
*
      WRITE (LOOPNAME,'(A)') ' '
      IOS = 0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),'(A)',IOSTAT=IOS,ERR=20) LOOPNAME
      END IF
*
   20 IF (IOS .NE. 0 .OR. 
     &    (NCHAR(LOOPNAME) .LE. 1 .AND. LOOPNAME(1:1) .EQ. ' ')) THEN
          PRINT '(A,$)','Loop index name (e.g. I) ?'
          READ '(A)',LOOPNAME
      END IF
      IF (DB) PRINT *,'LOOPNAME=',loopname
*
      LOOPSTART = 0
      IOS = 0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=30) LOOPSTART
      END IF
   30 IF (IOS .NE. 0 .OR. LOOPSTART .EQ. 0) THEN
          PRINT '(A,$)','Loop start index (e.g. 1) ?'
          READ '(A)', CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=31) LOOPSTART
   31     IF (IOS .NE. 0) THEN
              PRINT 1010, LOOPSTART
              RETURN
          END IF
      END IF
      IF (DB) PRINT *,'LOOPSTART=',LOOPSTART
*
      LOOPEND = 0
      IOS = 0
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=40) LOOPEND
      END IF
   40 IF (IOS .NE. 0 .OR. LOOPEND .EQ. 0) THEN
          PRINT '(A,$)','Loop end index (e.g. 1) ?'
          READ '(A)', CMDP(4)
          READ (CMDP(4),*,IOSTAT=IOS,ERR=41) LOOPEND
   41     IF (IOS .NE. 0) THEN
              PRINT 1010, LOOPEND
              RETURN
          END IF
      END IF
      IF (DB) PRINT *,'LOOPEND=',LOOPEND
*
      LOOPINC = 1
      IOS = 0
      IF (NCMD .GE. 5) THEN
          READ (CMDP(5),*,IOSTAT=IOS,ERR=50) LOOPINC
      END IF
   50 IF (IOS .NE. 0 .OR. NCMD .LT. 4) THEN
          PRINT '(A,$)','Loop increment (e.g. 2) ?'
          READ '(A)', CMDP(5)
          READ (CMDP(5),*,IOSTAT=IOS,ERR=51) LOOPINC
   51     IF (IOS .NE. 0) THEN
              PRINT 1010, LOOPINC
              RETURN
          END IF
      END IF
      IF (DB) PRINT *,'LOOPINC=',LOOPINC
*
      GETLOOPCMD = .TRUE.
      LOOPCMDNUM = 0
      IF (DB) PRINT *,'LOOPCMDNUM=',LOOPCMDNUM
*
      RETURN
 1010 FORMAT ('illegal ',I8)
      END
