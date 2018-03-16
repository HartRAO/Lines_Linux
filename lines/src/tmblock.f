************************
      SUBROUTINE TMBLOCK (ERROR)
************************
*     set velocity ranges for writing a time series file
*
*     CMDP command parameters used:
*     1 = command mnemonic TMV
*     2 = CLEAR / SHOW / start velocity
*     3 = end velocity
*
*     other subroutines called:
*     upcase
*
      IMPLICIT  NONE
*
      CHARACTER VSTRING*35      ! local header string
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   NCHAR           ! external function
      REAL*8    V1              ! local velocity of first channel
      REAL*8    V2              ! local velocity of last channel
*
      INCLUDE 'lines.inc'
      DATA VSTRING /'velocity blocks set for time series'/
*

      IF (DB) PRINT *,'in TMBLOCK'
      ERROR = 0
      V1 = 0d0
      V2 = 0d0
        if (db) print *,'cmdp(1)=',cmdp(1)
        if (db) print *,'cmdp(2)=',cmdp(2)
        if (db) print *,'cmdp(3)=',cmdp(3)
        if (db) print *,'cmdp(4)=',cmdp(4)
*
*     see if the CLEAR command has been given
      CALL UPCASE (CMDP(2))
      IF (CMDP(2)(1:5) .EQ. 'CLEAR') THEN
*
          WRITE (BUF,*) 'clear the ',VSTRING
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
          NTMVEL = 0
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
          IF (NTMVEL .GT. 0) THEN
*
              WRITE (BUF,*) VSTRING
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
              WRITE (BUF,*) ' '
              WRITE (BUF,'(26X,A)') 'Vstart        Vend'
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
              DO I = 1, NTMVEL
                  WRITE (BUF,*) ' '
                  WRITE (BUF,'(A7,I2,8X,F15.3,F12.3)')
     &                  ' block ',I,TMVSTART(I), TMVEND(I)
                  PRINT *,BUF(1:NCHAR(BUF))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
              END DO
          ELSE
              WRITE (BUF,*) 'No ',VSTRING
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
          END IF
      END IF
*
      IF (CMDP(2)(1:4) .EQ. 'SHOW') RETURN
*
      IF (V1 .EQ. 0D0 .AND. V2 .EQ. 0D0) THEN 
          PRINT '(2A,$)',' block start, end velocities ',
     &                  '(km/s, ENTER=none) ? '
          READ '(A)', BUF
          READ (BUF,*,ERR=250,IOSTAT=IOS,END=250) V1, V2
  250     IF (IOS .NE. 0 .AND. NCHAR(BUF) .GT. 1) THEN
              PRINT 2005,BUF(1:NCHAR(BUF))
              ERROR = IOS
              RETURN
          END IF 
          IF (V1 .EQ. 0D0 .AND. V2 .EQ. 0D0) THEN
              PRINT *,'no change'
              RETURN
          END IF
      END IF

      NTMVEL = NTMVEL + 1
      TMVSTART(NTMVEL) = V1
      TMVEND(NTMVEL)   = V2
      IF (DB) PRINT *,'NTMVEL = ',NTMVEL
      IF (DB) PRINT *,'TMVSTART,END=',TMVSTART(NTMVEL),TMVEND(NTMVEL)
      RETURN
*
 2005 FORMAT ('illegal: ',A)
      END 
*********
