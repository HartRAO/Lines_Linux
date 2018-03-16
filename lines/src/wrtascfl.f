*************************
      SUBROUTINE WRTASCFL (ERROR)
*************************
*     position to end of ascii-format file to write spectrum
*     close the file at end of write
*
*     CMDP command parameters used:
*     1 = command mnemonic WA
*     2 = ascii spectra file to write to
*     3 = memory with spectrum to write
*
*     other subroutines called:
*     er_close, er_open, er_read, er_write, getdate
*
      IMPLICIT NONE
*
      INTEGER   BEGIN_COUNT     ! local BEGIN keyword counter
      INTEGER   END_COUNT       ! local END keyword counter
      INTEGER   ERROR           ! output error code
      INTEGER   IOS             ! local file i/o error code
      INTEGER   IREC            ! local file record number
      INTEGER   MEM             ! local memory to write from
      INTEGER   NCHAR           ! external function
      LOGICAL   EXISTS          ! local file existence
*
      INCLUDE 'lines.inc'
*
      ERROR = 0
*
      IF (DB) PRINT *,' in WRTASCFL'
*
      IF (NCMD .GE. 2) THEN
          ASCWRTFILE = CMDP(2)
      ELSE
          ASCWRTFILE = ' '
          PRINT '(A,$)',' ascii spectra file to write to ?'
          READ  '(A)', ASCWRTFILE
          IF (ASCWRTFILE(1:1) .EQ. ' ') THEN
              PRINT *,'no file'
              ERROR=1
              RETURN
          END IF
      END IF
*
      MEM = MEMSET
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=50) MEM
   50     IF (IOS .NE. 0) MEM = 0
      END IF
*          
      DO WHILE (MEM .LE. 0 .OR. MEM .GT. MAXMEM)
          PRINT '(A,$)',' from memory ?'
          READ '(A)',BUF
          READ (BUF,*,IOSTAT=IOS,ERR=60) MEM
   60     IF (IOS .NE. 0) MEM = 0
      END DO
*
      IF (NAXIS(MEM) .LT. 1) THEN
          PRINT *,'Nothing in memory ',MEM
          ERROR = 1
          RETURN
      END IF
*
      BEGIN_COUNT = 0
      END_COUNT = 0
      INQUIRE (FILE=ASCWRTFILE,EXIST=EXISTS)
      IF (EXISTS) THEN
          OPEN (ASCWRTUNIT,FILE=ASCWRTFILE,STATUS='OLD',
     &          ERR=910,IOSTAT=IOS)
          PRINT '(3A,$)',' file ',ASCWRTFILE(1:NCHAR(ASCWRTFILE)),
     &         ' exists, format = '
          IREC = 0
          DO WHILE (.TRUE.)
              IREC = IREC + 1
              READ (ASCWRTUNIT,'(A)',ERR=920,END=100) BUF
              IF (IREC .EQ. 1) THEN
                  IF (BUF(1:6) .EQ. 'SIMPLE') THEN
                      PRINT *,'FITS ?'
                      ERROR = 6
                      RETURN
                  ELSE IF (BUF(1:5) .EQ. 'BEGIN') THEN
                      PRINT *,'ASCII spectra'
                  ELSE 
                      PRINT *,'unknown'
                      ERROR = 6
                      RETURN
                  END IF
              END IF
              IF (BUF(1:5) .EQ. 'BEGIN') THEN
                  BEGIN_COUNT = BEGIN_COUNT + 1
              ELSE IF (BUF(1:3) .EQ. 'END') THEN
                  END_COUNT = END_COUNT + 1
              END IF
          END DO
*         (JQ) backspace over EOF marker so write appends correctly
  100     BACKSPACE (ASCWRTUNIT)
      ELSE 
          OPEN (ASCWRTUNIT,FILE=ASCWRTFILE,STATUS='NEW',
     &          ERR=910,IOSTAT=IOS)
      END IF    
*
      IF (DB) PRINT *,'BEGIN_COUNT=',BEGIN_COUNT
      IF (DB) PRINT *,'END_COUNT=',END_COUNT
*
*     write the spectrum at the end of the output ascii format file
*
      CALL WRTASCF2 (MEM, BEGIN_COUNT, ERROR)
*
*     close ASCII file after writing spectra to it
      IF (DB) PRINT *,'closing ',ASCWRTFILE
      CLOSE (ASCWRTUNIT, ERR=940, IOSTAT=IOS)
      RETURN
*
*     error points
*     ------------
*     error opening file
  910 CALL ER_OPEN (ASCWRTFILE,IOS)
      ERROR = 1
      RETURN
*
*     error reading file
  920 CALL ER_READ (ASCWRTFILE,IOS,IREC)
      ERROR = 2
      RETURN
*
*     error closing file
  940 CALL ER_CLOSE (ASCWRTFILE,IOS)
      ERROR = 4
      RETURN
*
      END
*********
