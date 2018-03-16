*************************
      SUBROUTINE ALTERDAT (ERROR)
*************************
*     selectively alter data 
*
*     CMDP command parameter usage:
*     1 = command mnemonic AD
*     2 = array index of first value to alter
*     3 = array index of last value to alter
*     4 = new value for these data
*     5 = memory in use [default to set memory]
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local loop index
      INTEGER   IFIRST          ! first value to alter
      INTEGER   ILAST           ! last value to alter
      INTEGER   IOS             ! file eror check
      INTEGER   MEM             ! memory in use
      INTEGER   NCHAR           ! external function
      REAL*8    STARTVALUE      ! test for value
      REAL*8    VALUE           ! new value for data
*
      INCLUDE 'lines.inc'
*      
*
      IF (DB) PRINT *,'in ALTERDAT'
      ERROR = 0
*
      MEM = MEMSET
      IF (NCMD .GE. 5) THEN
          READ (CMDP(5),*,IOSTAT=IOS,ERR=210) MEM
  210     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' alter data in which memory ?'
          READ *,MEM
      END IF
*
      IF ((MEM .LT. 1) .OR. (MEM .GT. MAXMEM)) THEN
          PRINT *,'error: 1 > Mem=',MEM,' > ',MAXMEM
          ERROR = 1
          RETURN
      END IF
      IF (NAXIS(MEM) .LT. 1) THEN
          PRINT *,'nothing in mem ',MEM
          ERROR = 1
          RETURN
      END IF
*
*
      IFIRST = 0      
      IF (NCMD .GE. 3) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) IFIRST
  220     IF (IOS .NE. 0) IFIRST = 0
      END IF  
      IF (IFIRST .EQ. 0) THEN 
          PRINT '(A,$)',' index of first point to alter ?'
          READ *,IFIRST
      END IF
*
      IF (IFIRST .LT. FIRST_CH(MEM)) THEN
          PRINT 1010, IFIRST,' first ',FIRST_CH(MEM)
          ERROR = 1
          RETURN
      END IF
      IF (IFIRST .GT. LAST_CH(MEM)) THEN
          PRINT 1010, IFIRST,' last ',LAST_CH(MEM)
          ERROR = 1
          RETURN
      END IF
*
*
      ILAST = 0      
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=230) ILAST
  230     IF (IOS .NE. 0) ILAST = 0
      END IF  
      IF (ILAST .EQ. 0) THEN 
          PRINT '(A,$)',' index of last point to alter ?'
          READ *,ILAST
      END IF
*
      IF (ILAST .LT. FIRST_CH(MEM)) THEN
          PRINT 1010, ILAST,' first ',FIRST_CH(MEM)
          ERROR = 1
          RETURN
      END IF
      IF (ILAST .GT. LAST_CH(MEM)) THEN
          PRINT 1010, ILAST,' last ',LAST_CH(MEM)
          ERROR = 1
          RETURN
      END IF
*
      STARTVALUE = -1234321D0
      VALUE = STARTVALUE
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=240) VALUE
  240     IF (IOS .NE. 0) VALUE = STARTVALUE
      END IF  
      IF (VALUE .EQ. STARTVALUE) THEN 
          PRINT '(A,$)',' new value for these points ?'
          READ '(A)',BUF
          READ (BUF,*,IOSTAT=IOS,ERR=250) VALUE
  250     IF (IOS .NE. 0) THEN
              PRINT *,BUF(1:NCHAR(BUF)),' illegal'
              ERROR = 1
              RETURN
          END IF
      END IF
*
      WRITE (BUF,*)'alter ',IFIRST,' - ',ILAST,' to ',VALUE,
     &             ' in mem ',MEM
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      DO I = IFIRST, ILAST
          YIN(MEM,I) = VALUE
      END DO
*
      RETURN
 1010 FORMAT (' index =',I6,' < ',A,' valid index =',I6)
      END
*********      