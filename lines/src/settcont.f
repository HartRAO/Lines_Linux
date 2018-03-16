*************************
      SUBROUTINE SETTCONT (ERROR)
*************************
*     set the continuum antenna temperature and error 
*     for calculating electrom temperature after gaussian fit to 
*     recombination line or line integral on same
*
*     CMDP command parameters used:
*     1 = command mnemonic TC
*     2 = new continuum antenna temperature
*     3 = new uncertainty in the new continuum antenna temperature
*     4 = memory with TC to change
*
*     other subroutines called:
*
      IMPLICIT  NONE
      CHARACTER CAT*30          ! local string
      INTEGER   ERROR           ! output error status
      INTEGER   IOS             ! local file error check
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    TCERIN          ! local Tc error input
      REAL*8    TCONTIN         ! local Tcont input
*
      INCLUDE 'lines.inc'
      DATA    CAT /'Continuum antenna temperature '/
*
      IF (DB) PRINT *,'in SETTCONT'
      ERROR = 0
      TCONTIN = 0D0
      TCERIN = 0D0
*      
      IF (NCMD .GE. 3) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) TCONTIN
  210     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(2)(1:NCHAR(CMDP(2)))
              ERROR = IOS
              RETURN
          END IF
          IF (DB) PRINT *,'TcontIn= ',TCONTIN
*
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) TCERIN
  220     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(3)(1:NCHAR(CMDP(3)))
              ERROR = IOS
              RETURN
          END IF
          IF (DB) PRINT *,'TcerIn= ',TCERIN
      END IF
*
      MEM = MEMSET
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF
      IF (MEM .EQ. 0) THEN
          PRINT '(A,$)','set Tcont in which memory ?'
          READ (*,ERR=240,IOSTAT=IOS) MEM
  240     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(4)(1:NCHAR(CMDP(4)))
              MEM = MEMSET
          END IF
      END IF
*
      IF (NAXIS(MEM) .EQ. 0) THEN
          PRINT *,'mem ',MEM,' is empty'
          ERROR = 1
          RETURN
      END IF
* 
*
      IF (TCONT(MEM) .GT. 0D0) THEN
          WRITE (BUF,2001) CAT,TCONT(MEM),TCER(MEM),MEM
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
      IF (TCONTIN .EQ. 0D0 .AND. TCERIN .EQ. 0D0) THEN 
          PRINT '(1X,A,$)',CAT,'and error ?'
          READ '(A)', BUF
          READ (BUF,*,ERR=250,IOSTAT=IOS) TCONTIN, TCERIN
  250     IF (IOS .NE. 0 .OR. 
     &        TCONTIN .LT. 0D0 .OR. TCERIN .LT. 0D0) THEN
              PRINT 2005, BUF(1:NCHAR(BUF))
              ERROR = 1
              RETURN
          END IF
      END IF
*
      TCONT(MEM) = TCONTIN
      TCER(MEM)  = TCERIN
*
      WRITE (BUF,2001) CAT,TCONT(MEM),TCER(MEM),MEM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      RETURN
*
 2001 FORMAT (A,'=',F10.3,'+-',F6.3,'K in mem',I2)
 2005 FORMAT ('illegal :',A)
      END 
*********