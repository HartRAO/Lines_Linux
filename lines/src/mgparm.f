***********************
      SUBROUTINE MGPARM (ERROR)
***********************
*     get parameters for multiple gaussian fitting
*
*     CMDP command parameters used:
*     1 = command mnemonic GP
*     2 = position or SHOW or CLEAR or FIXBASE or FREEBASE
*     3 = height
*     4 = width
*     5 = PHWB to constrain position, height, width, baseline
*
*     other subroutines called:
*     none
*
      IMPLICIT  NONE
      CHARACTER FIXED*5         ! string 'fixed'
      CHARACTER FREE*4          ! string 'free'
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! loop index
      INTEGER   IOS             ! file error check
      INTEGER   NCH             ! no. characters in buffer
      INTEGER   NCHAR           ! external function
      INTEGER   ADUM(3)         ! constraint 0=on/=1off switch
      REAL*8    LGP(3)          ! position, height, width guess
      DATA      FIXED /'fixed'/
      DATA      FREE  /'free'/
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in MGPARM'
      ERROR = 0
      DO I = 1, 3
          LGP(I) = 0D0
          ADUM(I) = 1
      END DO
*
      IF (NCMD .GE. 2) CALL UPCASE (CMDP(2))
      IF (NCMD .GT. 4) CALL UPCASE (CMDP(5))
*
      IF (NCMD .EQ. 1 .OR.
     &    NCMD .EQ. 2 .AND. CMDP(2)(1:4) .EQ. 'SHOW') THEN
          IF (DB) PRINT *,'NPARIN = ',NPARIN
          IF (NPARIN .GT. 0) THEN
*
              WRITE (BUF,*) 'Current input parameters and constraints ',
     &                'for Gaussian fit'
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
              WRITE (BUF,*) '  Position     Height     Width'
              PRINT *,BUF(1:NCHAR(BUF))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
              DO I = 1, NPARIN, 3
*                 write out the position, height, width
                  WRITE (BUF,'(F10.3,F12.6,F10.3)')
     &                   PIN(I),PIN(I+1),PIN(I+2)
                  PRINT *,BUF(1:NCHAR(BUF))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*                 write out whether they are fixed or free
                  WRITE (BUF,*) ' '
                  IF (A12IN(I) .EQ. 0) THEN
                      WRITE (BUF(5:),'(A)') FIXED
                  ELSE
                      WRITE (BUF(5:),'(A)') FREE
                  END IF
*
                  IF (A12IN(I+1) .EQ. 0) THEN
                      WRITE (BUF(17:),'(A)') FIXED
                  ELSE
                      WRITE (BUF(17:),'(A)') FREE
                  END IF
*
                  IF (A12IN(I+2) .EQ. 0) THEN
                      WRITE (BUF(28:),'(A)') FIXED
                  ELSE
                      WRITE (BUF(28:),'(A)') FREE
                  END IF
                  PRINT *,BUF(1:NCHAR(BUF))
                  IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
              END DO
          ELSE
              PRINT *,'no Gaussian parameters entered'
          END IF
*         note whether baseline is fixed or free
          WRITE (BUF,'(A)') 'baseline is'
          IF (FIXBASE) THEN
              WRITE (BUF(13:),*) FIXED
          ELSE
              WRITE (BUF(13:),*) FREE
          END IF
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
      IF (CMDP(2)(1:4) .EQ. 'SHOW') RETURN
*
*     see if the CLEAR command has been given
      IF (CMDP(2)(1:5) .EQ. 'CLEAR') THEN
          PRINT *,'clear the Gaussian parameters'
          NPARIN = 0
          FIXBASE = .FALSE.
          RETURN
      END IF
*
*     see if baseline to be fixed
      IF (CMDP(2)(1:3) .EQ. 'FIX') THEN
          FIXBASE = .TRUE.
          RETURN
      END IF
*
*     see if baseline to be fixed
      IF (CMDP(2)(1:4) .EQ. 'FREE') THEN
          FIXBASE = .FALSE.
          RETURN
      END IF
*
      IF (NCMD .GE. 4) THEN
*         read in position
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) LGP(1)
  210     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(2)(1:NCHAR(CMDP(2)))
              ERROR = IOS
              RETURN
          END IF
          IF (DB) PRINT *,'Position = ',LGP(1),' km/s'
*
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) LGP(2)
*         read in height
  220     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(3)(1:NCHAR(CMDP(3)))
              ERROR = IOS
              RETURN
          END IF
          IF (DB) PRINT *,'Height = ',LGP(2)
*
          READ (CMDP(4),*,IOSTAT=IOS,ERR=225) LGP(3)
*         read in width
  225     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(4)(1:NCHAR(CMDP(4)))
              ERROR = IOS
              RETURN
          END IF
          IF (DB) PRINT *,'Width = ',LGP(3),' km/s'
*
*         reading in any constraints
          IF (NCMD .GT. 4) THEN
             DO I=1,4
                IF (CMDP(5)(I:I) .EQ. 'P') THEN
                    ADUM(1) = 0
                END IF
                IF (CMDP(5)(I:I) .EQ. 'H') THEN
                    ADUM(2) = 0
                END IF
                IF (CMDP(5)(I:I) .EQ. 'W') THEN
                    ADUM(3) = 0
                END IF
                IF (CMDP(5)(I:I) .EQ. 'B') THEN
                    FIXBASE = .TRUE.
                END IF
             END DO
          END IF
      END IF
*
      IF (LGP(2) .EQ. 0D0) THEN 
          PRINT '(2A$)',' Gaussian Position, Height, Width ',
     &                  '(Enter = none) ? '
          READ '(A)', BUF
          NCH = NCHAR(BUF)
          BUF(NCH+1:NCH+1) = '/'
          READ (BUF,*,ERR=250,IOSTAT=IOS) LGP(1), LGP(2), LGP(3)
  250     IF (IOS .NE. 0) THEN
              PRINT 2005,BUF(1:NCHAR(BUF))
              ERROR = IOS
              RETURN
          END IF 
          IF (DB) PRINT *,'LGP(1-3)=',(LGP(I),I=1,3)
          IF (LGP(2) .EQ. 0D0) THEN
              PRINT *,'no change'
              RETURN
          END IF
*
          PRINT '(2A,$)',' Constrain position (P), height (H), ',
     &                'width (W), baseline(B) (ENTER = none) ? '
          READ '(A)',BUF
          CALL UPCASE (BUF)
          DO I = 1, 4
             IF (BUF(I:I) .EQ. 'P') THEN
                 ADUM(1) = 0
             END IF
             IF (BUF(I:I) .EQ. 'H') THEN
                 ADUM(2) = 0
             END IF
             IF (BUF(I:I) .EQ. 'W') THEN
                 ADUM(3) = 0
             END IF
             IF (BUF(I:I) .EQ. 'B') THEN
                 FIXBASE = .TRUE.
             END IF
          END DO
      END IF
*
*     add the new block values to common
*
      DO I = 1, 3
          PIN(NPARIN+I) = LGP(I)
          A12IN(NPARIN+I) = ADUM(I)
          IF (DB) PRINT *,'PIN(',NPARIN+I,')=',PIN(NPARIN+I),
     &                    ' A12IN=',A12IN(NPARIN+I)
      END DO
      NPARIN = NPARIN + 3
      IF (DB) PRINT *,'NPARIN now = ',NPARIN
*
      RETURN
*
 2005 FORMAT ('illegal: ',A)
      END 
*********