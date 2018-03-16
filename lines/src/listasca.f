*************************
      SUBROUTINE LISTASCA (ERROR)
*************************
*     list the contents of an ascii spectra file for automated processing
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
      CHARACTER OFFSETCODE*2    ! pointing spectra identifier
      CHARACTER SNUMFILE*6      ! ascii version of number in file
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in LISTASCA'
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
*             read to end of file lines.fla
              OPEN (FLAWRTUNIT,FILE=FLAWRTFILE,
     &              ACCESS='SEQUENTIAL',IOSTAT=IOS,ERR=910)
              DO WHILE (.TRUE.)
                  READ (FLAWRTUNIT,'(A)',END=104,IOSTAT=IOS,ERR=102) BUF
  102             IF (IOS .NE. 0) THEN
                      PRINT *,'error ',IOS,' reading ',FLAWRTFILE
                  END IF
              END DO
*         (JQ) backspace over EOF marker so write appends correctly
  104         BACKSPACE (FLAWRTUNIT)
          END IF
*
          IF (ERROR .EQ. 0) THEN
*             set up pointing identifier
              IF (OBJECT(MEMRD)(19:20) .EQ. 'NS') THEN
                  OFFSETCODE(1:2) = 'NS'
              ELSE IF (OBJECT(MEMRD)(19:20) .EQ. 'EW') THEN
                  OFFSETCODE(1:2) = 'EW'
              ELSE IF (OBJECT(MEMRD)(20:20) .EQ. 'N') THEN
                  OFFSETCODE(1:2) = ' N'
              ELSE IF (OBJECT(MEMRD)(20:20) .EQ. 'S') THEN
                  OFFSETCODE(1:2) = ' S'
              ELSE IF (OBJECT(MEMRD)(20:20) .EQ. 'E') THEN
                  OFFSETCODE(1:2) = ' E'
              ELSE IF (OBJECT(MEMRD)(20:20) .EQ. 'W') THEN
                  OFFSETCODE(1:2) = ' W'
              ELSE 
                  OFFSETCODE(1:2) = 'ON'
              ENDIF
*
              WRITE (SNUMFILE,*), NUMFILE(MEMRD)
*
*             create output string with wanted housekeeping     
              WRITE (LBUF,1100)
     &        ASCRDFILE(1:NCHAR(ASCRDFILE)),',',
     &        SNUMFILE(1:NCHAR(SNUMFILE)),',',
     &        OBJECT(MEMRD),',',
     &        OFFSETCODE(1:2),',',
     &        DATE_OBS(MEMRD)(1:NCHAR(DATE_OBS(MEMRD))),',',
     &        SCAN(MEMRD),',',
     &        TSYS(MEMRD)
*
              PRINT '(A)',LBUF(1:NCHAR(LBUF))
              WRITE (FLAWRTUNIT,'(A)') LBUF(1:NCHAR(LBUF))
          END IF
      END DO
*
      CLOSE (FLAWRTUNIT,IOSTAT=IOS,ERR=940)
*
 1100 FORMAT (A,A1, A,A1, A20,A1, A2,A1, A,A1, I6,A1, F8.2)
      RETURN
*
  910 CALL ER_OPEN (FLAWRTFILE, IOS)
      ERROR = 1
      RETURN
*
  940 CALL ER_CLOSE (FLAWRTFILE, IOS)
      ERROR = 4
      RETURN
      END
*********
      
