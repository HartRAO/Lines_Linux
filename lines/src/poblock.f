***********************
      SUBROUTINE POBLOCK (ERROR)
***********************
*     set velocity limits for polynomial fit to baseline
*
*     CMDP command parameters used:
*     1 = command mnemonic PB
*     2 = CLEAR or SHOW or start velocity of baseline block
*     3 = end velocity of baseline block
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   NCHAR           ! external function
      REAL*8    V1              ! local velocity of first channel
      REAL*8    V2              ! local velocity of last channel
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in POBLOCK'
      ERROR = 0
      V1 = 0d0
      V2 = 0d0
*
*     see if the CLEAR command has been given
      CALL UPCASE (CMDP(2))
      IF (NCMD .GE. 2 .AND. CMDP(2)(1:5) .EQ. 'CLEAR') THEN
          WRITE (BUF,*) 'clear polynomial fit baseline blocks'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          NBASE = 0
          RETURN
      END IF
*
      IF (NCMD .GE. 3) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) V1
  210     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(2)(1:NCHAR(CMDP(2)))
              ERROR = IOS
              RETURN
          END IF
          V1 = V1
          IF (DB) PRINT *,'V1= ',V1,' km/s'
*
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) V2
  220     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(3)(1:NCHAR(CMDP(3)))
              ERROR = IOS
              RETURN
          END IF
          V2 = V2
          IF (DB) PRINT *,'V2= ',V2,' km/s'
      END IF
*
*
*     option to list current blocks
*
      IF (NCMD .GE. 2) CALL UPCASE (CMDP(2))
*
      IF (NCMD .EQ. 1 .OR.
     &    NCMD .EQ. 2 .AND. CMDP(2)(1:4) .EQ. 'SHOW') THEN
          IF (NBASE .GT. 0) THEN
              WRITE (BUF,*) 'current baseline blocks for polynomial fit'
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
              WRITE (BUF,*) ' '
              WRITE (BUF,'(26X,A)') 'Xstart        Xend'
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
              DO I = 1, NBASE
                  WRITE (BUF,'(A7,I2,8X,F15.3,F12.3)')
     &                  ' block ',I,BASESTART(I), BASEEND(I)
                  PRINT *,BUF(1:NCHAR(BUF))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
              END DO
          ELSE
              WRITE (BUF,*) 'No baseline blocks set for polynomial fit'
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          END IF
      END IF
*
      IF (CMDP(2)(1:4) .EQ. 'SHOW') RETURN
*
      IF (V1 .EQ. 0D0 .AND. V2 .EQ. 0D0) THEN 
          PRINT '(A,$)',' block start, end limits (/=none) ?'
          READ '(A)', BUF
          READ (BUF,*,ERR=250,IOSTAT=IOS) V1, V2
  250     IF (IOS .NE. 0) THEN
              PRINT 2005,BUF(1:NCHAR(BUF))
              ERROR = 1
              RETURN
          END IF 
          IF (V1 .EQ. 0D0 .AND. V2 .EQ. 0D0) THEN
              PRINT *,'no change'
              RETURN
          END IF
      END IF
*
*     check that this block does not overlap previous ones
*
      IF (NBASE .GE. 1) THEN
          DO I = 1, NBASE
              IF (V1 .GT. BASESTART(I) .AND. V1 .LT. BASEEND(I) .OR.
     &            V2 .GT. BASESTART(I) .AND. V1 .LT. BASEEND(I)) THEN
                  WRITE (BUF,*) V1,' to ',V2,' overlaps ',
     &                BASESTART(I),' to ',BASEEND(I),' so NOT used'
                  PRINT *,BUF(1:NCHAR(BUF))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
                  ERROR = 1
                  RETURN
              END IF
          END DO
      END IF
*
*     check that there is room for the new pair of points
*
      IF ((NBASE + 1) .GT. MAXBASE) THEN
          WRITE (BUF,*) 'MAXBASE =',MAXBASE,
     &        ' exceeded, increase in "lines.inc"'
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          ERROR = 1
          RETURN
      END IF
*
*     add the new block values to common
*
      NBASE = NBASE + 1
      BASESTART(NBASE) = V1
      BASEEND(NBASE)   = V2
      IF (DB) PRINT *,'base start, end=',BASESTART(NBASE),BASEEND(NBASE)
*      
      RETURN
*
 2005 FORMAT ('illegal: ',A)
      END 
*********
