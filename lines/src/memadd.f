***********************
      SUBROUTINE MEMADD (ERROR)
***********************
*     add a constant to spectrum
*
*     CMDP command parameters used:
*     1 = command mnemonic AC
*     2 = constant to add to data
*
*     other routines called:
*     nchar
*
      IMPLICIT  NONE
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    CONST           ! local temporary storage
*
      INCLUDE 'lines.inc'
*
      ERROR = 0
      CONST = 0.0
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) CONST
  210     IF (IOS .NE. 0) CONST = 0.0
      END IF  
      IF (CONST .EQ. 0.0) THEN 
          PRINT '(A,$)',' constant to add (Enter = 0) ? '
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) CONST
  220     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      MEM = MEMSET
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' add to data in which memory ? '
          READ '(A)',CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=240) MEM
  240     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      DO I = FIRST_CH(MEM),LAST_CH(MEM)
          YIN(MEM,I) = YIN(MEM,I) + CONST
      END DO
*
      WRITE (BUF,*) CONST,' added to data in mem ',MEM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      RETURN
      END
*********