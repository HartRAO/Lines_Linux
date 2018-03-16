*********************
      SUBROUTINE HAMM (ERROR)
*********************
*     smooth spectrum with Hamming function
*
*     CMDP command parameters used:
*     1 = command mnemonic HM
*     2 = memory with spectrum to be smoothed
*
*     other routines called:
*     arms, nchar
*
      IMPLICIT  NONE
      INTEGER   I               ! loop index
      INTEGER   IOS             ! file error check
      INTEGER   ERROR           ! output error status
      INTEGER   MEM             ! memory to use
      INTEGER   NCHAR           ! external function
      REAL*8    T1              ! temporary storage
      REAL*8    T2              ! temporary storage
*
      INCLUDE 'lines.inc'
*
      ERROR = 0
      MEM = MEMSET
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) MEM
  210     IF (IOS .NE. 0) MEM = 0
      END IF  
      IF (MEM .EQ. 0) THEN 
          PRINT '(A,$)',' Smooth spectrum in which memory ?'
          READ '(A)',CMDP(2)
          READ (CMDP(2),*,IOSTAT=IOS,ERR=220) MEM
  220     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      IF (MEM .LT. 1 .OR. MEM .GT. MAXMEM) THEN
          PRINT *,'illegal mem ',MEM
          ERROR = 1
          RETURN
      END IF
*
      IF (SMOOTHED(MEM) .EQ. 1) THEN
          PRINT *,'already smoothed - smoothing again!'
*          ERROR = 1
*          RETURN
      END IF
*
      IF (TRANSFRM(MEM) .EQ. 1) THEN
          PRINT *,'transform, not spectrum'
          ERROR = 1
          RETURN
      END IF
*      
      T1 = YIN(MEM,FIRST_CH(MEM))
      T2 = 0
      YIN(MEM,FIRST_CH(MEM)) = T1*0.54 +
     &    YIN(MEM,FIRST_CH(MEM)+1)*0.46
      DO I = FIRST_CH(MEM)+1, LAST_CH(MEM)-1
          T2 = YIN(MEM,I)
          YIN(MEM,I) = 0.23*T1 + 0.54*T2 + 0.23*YIN(MEM,I+1)
          T1 = T2
      END DO
      YIN(MEM,LAST_CH(MEM)) = 
     &    YIN(MEM,LAST_CH(MEM))*0.54 + T2*0.46
      SMOOTHED(MEM) = 1
*
      WRITE (BUF,*) 'spectrum in mem ',MEM,' now smoothed'
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      CALL ARMS (ERROR)
      RETURN
      END
*********