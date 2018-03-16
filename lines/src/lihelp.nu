***********************
      SUBROUTINE LIHELP (ERROR)
***********************
*     list help from helpfile
*
*     other subroutines called:
*     er_close, er_read, er_open, upcase
*
      IMPLICIT  NONE
      CHARACTER TEST*10         ! local start of help on command
      INTEGER   ERROR           ! output file read error code
      INTEGER   IOS             ! local file error status
      INTEGER   LINE_IN_FILE    ! local line in file
      INTEGER   NCHAR           ! external function
      LOGICAL   PRINT_IT        ! local turn printing on/off
      LOGICAL   READ_DONE       ! local do while control
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in LIHELP'
      ERROR = 0
    1 IF (NCMD .LE. 1) THEN
          WRITE (CMDP(2),'(A)') 'HE'
      END IF
      IF (DB) PRINT *,'CDMP(2) =',CMDP(2)(1:NCHAR(CMDP(2)))
*
      TEST(1:2) ='1 '
      TEST(3:)  = CMDP(2)(1:NCHAR(CMDP(2)))
      CALL UPCASE(TEST)
      IF (DB) PRINT *,'TEST =',TEST
*
      IF (DB) PRINT *,'opening ',HLPRDFILE
      OPEN (HLPRDUNIT,FILE=HLPRDFILE,STATUS='OLD',ERR=910,IOSTAT=IOS)
*
      READ_DONE = .FALSE.
      PRINT_IT = .FALSE.
      LINE_IN_FILE = 0
*
      DO WHILE (.NOT. READ_DONE)
          LINE_IN_FILE = LINE_IN_FILE + 1
*
          READ (HLPRDUNIT,'(A)',END=100,ERR=920,IOSTAT=IOS) BUF
*
          IF (BUF(1:1) .EQ. '1' .AND. PRINT_IT) THEN
              READ_DONE = .TRUE.
          END IF
*
          IF (PRINT_IT .AND. .NOT. READ_DONE) 
     &        PRINT *,BUF(1:NCHAR(BUF))
*
          IF (BUF(1:NCHAR(BUF)) .EQ. TEST(1:NCHAR(TEST))) THEN
              PRINT_IT = .TRUE.
          END IF
      END DO
*
   90 CLOSE (HLPRDUNIT,IOSTAT=IOS,ERR=940)
      RETURN
*
*     help file ended without command being found
  100 PRINT *,'No help found on: ',CMDP(2)(1:NCHAR(CMDP(2)))
      GO TO 90
*
  910 CALL ER_OPEN (HLPRDFILE, IOS)
      ERROR = 1
      RETURN
*
  920 CALL ER_READ (HLPRDFILE,IOS,LINE_IN_FILE)
      ERROR = 2
      GO TO 90
*
  940 CALL ER_CLOSE (HLPRDFILE, IOS)
      ERROR = 4
      RETURN
*
      END
*********