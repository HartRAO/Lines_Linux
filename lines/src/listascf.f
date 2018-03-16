*************************
      SUBROUTINE LISTASCF (ERROR)
*************************
*     list the contents of an ascii spectra file
*
*     CMDP command parameters used:
*     1 = command mnemonic LF
*     2 = ascii spectra file to list 
*     3 = number of first spectrum to list
*     4 = number of last spectrum to list
*
*     other subroutines called:
*     nchar, rdascfl

      IMPLICIT  NONE
      INTEGER   ERROR           ! output error code
      INTEGER   FIRSTWANTED     ! first spectrum to list
      INTEGER   IOS             ! read error code
      INTEGER   LASTWANTED      ! last spectrum to list
      INTEGER   NCHAR           ! external function
      LOGICAL   EXISTS          ! file existence
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in LISTASCF'
      ERROR = 0
*
      IF (NCMD .GE. 2) THEN
          ASCRDFILE = CMDP(2)
      ELSE
          PRINT '(3A,$)',' ASCII spectra file to list (Enter =',
     &        PREVAFILE(1:NCHAR(PREVAFILE)),') ?'
          READ  '(A)', ASCRDFILE
      END IF
*
      IF (NCHAR(ASCRDFILE) .LE. 1) THEN
          ASCRDFILE = PREVAFILE
      END IF
*
      INQUIRE (FILE=ASCRDFILE,EXIST=EXISTS)
*
      IF (.NOT. EXISTS) THEN
          PRINT *,'File ',ASCRDFILE(1:NCHAR(ASCRDFILE)),' not found'
          ERROR = 1
          RETURN
      END IF
*
      PRINT *,'list file ',ASCRDFILE(1:NCHAR(ASCRDFILE))
      IF (WRITELOG) THEN
          WRITE (LOGWRTUNIT,*) 'list file ',
     &           ASCRDFILE(1:NCHAR(ASCRDFILE))
      END IF
*
      FIRSTWANTED = 1 
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=110) FIRSTWANTED
  110     IF (IOS .NE. 0) FIRSTWANTED = 1
      END IF
*
      LASTWANTED = 32767
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=140) LASTWANTED
  140     IF (IOS .NE. 0) LASTWANTED = 32767
      END IF
*
      IF (LASTWANTED .LT. FIRSTWANTED) LASTWANTED = FIRSTWANTED
*
      DO ASCWANTED = FIRSTWANTED, LASTWANTED
*         read spectra from ascii format file
          CALL RDASCFL (ERROR)
*
          IF (ERROR .EQ. 5) RETURN ! unexpected end of file
*
          IF (ASCWANTED .EQ. FIRSTWANTED) THEN
              PRINT '(2A)',
     &        ' Rec Object               Date       Scan  Freq',
     &        '   BW Pol  HA S F Py Av  PSS  BU'
              IF (WRITELOG) THEN
                  WRITE (LOGWRTUNIT,'(2A)')
     &            ' Rec Object               Date       Scan  Freq',
     &            '   BW Pol  HA S F Py Av  PSS  BU'
              END IF
          END IF
*
          IF (ERROR .EQ. 0) THEN
              PRINT 1100, 
     &        NUMFILE(MEMRD), OBJECT(MEMRD)(1:20),
     &        DATE_OBS(MEMRD), SCAN(MEMRD),
     &        IDINT(RESTFREQ(MEMRD)/1D6), 
     &        BW(MEMRD)/1D6, POL(MEMRD),
     &        IDINT(HA(MEMRD)), SMOOTHED(MEMRD),
     &        FOLDED(MEMRD), POLYFIT(MEMRD), 
     &        IDINT(ADDED(MEMRD)), PSS(MEMRD),
     &        BUNIT(MEMRD)
              IF (WRITELOG) THEN
                  WRITE (LOGWRTUNIT, 1100)
     &            NUMFILE(MEMRD), OBJECT(MEMRD)(1:20),
     &            DATE_OBS(MEMRD), SCAN(MEMRD),
     &            IDINT(RESTFREQ(MEMRD)/1D6),
     &            BW(MEMRD)/1D6, POL(MEMRD),
     &            IDINT(HA(MEMRD)), SMOOTHED(MEMRD),
     &            FOLDED(MEMRD), POLYFIT(MEMRD), 
     &            IDINT(ADDED(MEMRD)), PSS(MEMRD),
     &            BUNIT(MEMRD)
              END IF
          END IF
      END DO
*
 1100 FORMAT (I4,1X,A20,A10,I6,I6,F5.2,I4,I4,I2,I2,I3,I3,F6.1,1X,A2)
      RETURN
      END
*********      