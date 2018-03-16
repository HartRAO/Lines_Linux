*************************
      SUBROUTINE WRTMCAFL (ERROR)
*************************
*     ASCII output in columns for spreadsheets
*     mod 2000/09/11 to put # in front of first two lines, for "grace"
*
*     CMDP command parameters used:
*     1 = command mnemonic WMCA
*     2 = ascii spectra file to write to
*     3 = memory with spectrum to write

*     other subroutines called:
*     er_close, er_open, er_read, er_write
*
      IMPLICIT NONE
      LOGICAL  EXISTS                   ! file existence
      INTEGER  ERROR                    ! error return code
      INTEGER  I                        ! loop index
      INTEGER  IOS                      ! file i/o status
      INTEGER  IREC                     ! record number in file
      INTEGER  MEM                      ! memory in use
      INTEGER  NCHAR                    ! external function
      INTEGER  N                        ! counter
      REAL*8   XVALUE                   ! velocity in km/s or channel
*
      INCLUDE 'lines.inc'
*
      IF (NCMD .GE. 2) THEN
          MCAWRTFILE = CMDP(2)
      ELSE
          PRINT '(A,$)',' multicolumn ascii file to write to ?'
          READ  '(A)', MCAWRTFILE
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
      INQUIRE (FILE=MCAWRTFILE,EXIST=EXISTS)
      IREC = 0
      IF (EXISTS) THEN
          OPEN (MCAWRTUNIT,FILE=MCAWRTFILE,STATUS='OLD',
     &          ERR=910,IOSTAT=IOS)
          PRINT *,'file ',MCAWRTFILE(1:NCHAR(MCAWRTFILE)),' exists'
          DO WHILE (.TRUE.)
              READ (MCAWRTUNIT,'(A)',ERR=920,END=100) BUF
              IREC = IREC + 1
              IF (IREC .EQ. 1) THEN
                  IF (BUF(1:6) .EQ. 'SIMPLE') THEN
                      PRINT 2010,'a FITS'               
                      ERROR = 6
                      RETURN
                  ELSE IF (BUF(1:5) .EQ. 'BEGIN') THEN
                      PRINT 2010,'an ascii spectra'
                      ERROR = 6
                      RETURN
                  ELSE IF (BUF(1:7) .EQ. ' OBJECT') THEN
                      PRINT 2010,'a multicolumn ascii'
                  ELSE 
                      PRINT 2010,'an unidentified format'
                  END IF
              END IF
          END DO
*         (JQ) backspace over EOF marker so write appends correctly
  100     BACKSPACE (MCAWRTUNIT)
      ELSE 
          OPEN (MCAWRTUNIT,FILE=MCAWRTFILE,STATUS='NEW',
     &          ERR=910,IOSTAT=IOS)
          PRINT 2200, MCAWRTFILE
      END IF    
*
*
*     Write data to multi-column ASCII spreadsheet-style file
*
*     header lines, start with # for import by xmgrace
      WRITE (LBUF,'(4A)',IOSTAT=IOS,ERR=930) 
     &    '# ', OBJECT(MEM)(1:NCHAR(OBJECT(MEM))),
     &    ', on, ',DATE_OBS(MEM)(1:NCHAR(DATE_OBS(MEM)))
      WRITE (MCAWRTUNIT,'(A)',IOSTAT=IOS,ERR=930) LBUF(1:NCHAR(LBUF))
*
      WRITE (LBUF,'(A,F16.6,A)',IOSTAT=IOS,ERR=930) 
     &    '# at, ',RESTFREQ(MEM)/1E6,', MHz'
      WRITE (MCAWRTUNIT,'(A)',IOSTAT=IOS,ERR=930) LBUF(1:NCHAR(LBUF))
*
      WRITE (LBUF,'(2A)',IOSTAT=IOS,ERR=930) 
     &    '# in, ',BUNIT(MEM)(1:NCHAR(BUNIT(MEM)))
      WRITE (MCAWRTUNIT,'(A)',IOSTAT=IOS,ERR=930) LBUF(1:NCHAR(LBUF))
*
      WRITE (LBUF,'(A,F8.3,A,F8.3,A,F6.1)',IOSTAT=IOS,ERR=930) 
     &    '# at RA,',RA(MEM),
     &    ', DEC,',DEC(MEM),
     &    ', of, ',EQUINOX(MEM)
      WRITE (MCAWRTUNIT,'(A)',IOSTAT=IOS,ERR=930) LBUF(1:NCHAR(LBUF))
*
*     column headers
      IF (TRANSFRM(MEM) .EQ. 0) THEN
          WRITE (MCAWRTUNIT,'(A2,A16,A,A16)',IOSTAT=IOS,ERR=930) 
     &       '# ',CTYPE1(MEM),',',CTYPE2(MEM)
      ELSE
          WRITE (MCAWRTUNIT,'(A2,A16,A16)',IOSTAT=IOS,ERR=930) 
     &       '# ','CHANNEL,','TRANSFORM'
      END IF
*
      N = 0
      DO I = FIRST_CH(MEM), LAST_CH(MEM)
          N = N + 1
          IF (TRANSFRM(MEM) .EQ. 1) THEN
              XVALUE = N
          ELSE
              XVALUE = XIN(MEM,I)
          END IF
          WRITE (MCAWRTUNIT,'(2X,F16.6,A,F16.6)',IOSTAT=IOS,ERR=930) 
     &        XVALUE,',',YIN(MEM,I)
      END DO
*
      WRITE (BUF,'(2A)'),'data added to multicolumn ascii file ',
     &    MCAWRTFILE(1:NCHAR(MCAWRTFILE))
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
      CLOSE (MCAWRTUNIT,ERR=940,IOSTAT=IOS)
      RETURN
*
 2010 FORMAT ('this looks like ',A,' file')
 2200 FORMAT ('created new file ',A)
*
*     error points
*     ------------
*     error opening file
  910 CALL ER_OPEN (MCAWRTFILE,IOS)
      ERROR = 1
      RETURN
*
*     error reading file
  920 CALL ER_READ (MCAWRTFILE,IOS,IREC)
      ERROR = 2
      RETURN
*
*     error writing to file
  930 CALL ER_WRITE (MCAWRTFILE,IOS)
      ERROR = 3
      RETURN
*
*     error closing file
  940 CALL ER_CLOSE (MCAWRTFILE,IOS)
      ERROR = 4
      RETURN
*
      END
*********
