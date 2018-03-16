*************************
      SUBROUTINE ASCFILAV (ERROR)
*************************
*     set up multiple reads from ascii file and 
*     average spectra obtained within a specified interval
*
*     CMDP command parameters used:
*     1 = command mnemonic RAV
*     2 = ascii spectra file to read from
*     3 = number of first spectrum to read
*     4 = number of last spectrum to read
*     5 = ascii spectra file to write average spectra to
*     6 = averaging interval in days
*     7 = memory to add spectrum to, default to set memory
*
*     called by:
*     cmdselct
*
*     other routines called:
*     er_close, er_open, er_read, listmem, memav, memclr, memcopy, 
*     nchar, rdascfl, upcase, wrtascf2
*
      IMPLICIT NONE
*
      INTEGER   IADDED          ! local int sum for JD and HA
      INTEGER   BEGIN_COUNT     ! local BEGIN keyword counter
      INTEGER   END_COUNT       ! local END keyword counter
      INTEGER   ERROR           ! output file error code
      INTEGER   FIRSTWANTED     ! local first wanted spectrum
      INTEGER   IFCHAR          ! local number of characters in buffer
      INTEGER   IOS             ! local read error code
      INTEGER   IPCHAR          ! local number of characters in buffer
      INTEGER   IREC            ! local file record number
      INTEGER   LASTWANTED      ! local first wanted spectrum
      INTEGER   MEM             ! local memory to store average in
      INTEGER   NCHAR           ! external function
      LOGICAL   ADTOAV          ! local add to average flag
      LOGICAL   EXISTS          ! local file existence
      REAL*8    AVRGINT         ! local averaging interval in days
      REAL*8    HASUM           ! local average hour angle written out
      REAL*8    JULDATEAV       ! local julian date of average spectrum
      REAL*8    JDSUM           ! local average julian date written out
*
      INCLUDE 'lines.inc'
*
*     get the needed parameters, from runstring or user enquiry
*
      IF (DB) PRINT *,'in ASCFILAV'
      ERROR = 0
*
      IF (NCMD .GE. 2) THEN
          ASCRDFILE = CMDP(2)
      ELSE
          IPCHAR = NCHAR(PREVAFILE)
          PRINT '(3A,$)',' ASCII spectra file to read from (/=',
     &        PREVAFILE(1:IPCHAR),') ? '
          READ  '(A)', ASCRDFILE
      END IF
*
      IFCHAR = NCHAR(ASCRDFILE)
      IF (ASCRDFILE(1:1) .EQ. '/' .AND. IFCHAR .EQ. 1) THEN
          WRITE (ASCRDFILE,'(A)') PREVAFILE
          IFCHAR = NCHAR(ASCRDFILE)
      END IF
*
      INQUIRE (FILE=ASCRDFILE,EXIST=EXISTS)
*
      IF (.NOT. EXISTS) THEN
          PRINT *,'file ',ASCRDFILE(1:IFCHAR),' not found'
          ERROR = 1
          RETURN
      END IF
*
      PRINT *,'read ASCII data file ',ASCRDFILE(1:IFCHAR)
*
      FIRSTWANTED = 0 
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=110) FIRSTWANTED
  110     IF (IOS .NE. 0) THEN
              FIRSTWANTED = 0
              PRINT 2005, CMDP(3)(1:NCHAR(CMDP(3)))
          END IF
      END IF
      IF (FIRSTWANTED .EQ. 0) THEN
          PRINT '(A,$)',' first spectrum number to read ? '
          READ '(A)', CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=120) FIRSTWANTED
  120     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(3)(1:NCHAR(CMDP(3)))
              ERROR = IOS
              RETURN
          END IF
      END IF
      IF (FIRSTWANTED .LT. 1) THEN
          ERROR = 1
          RETURN
      END IF
*
      LASTWANTED = 0
      IF (NCMD .EQ. 3) LASTWANTED = FIRSTWANTED
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=140) LASTWANTED
  140     IF (IOS .NE. 0) THEN
              LASTWANTED = 0
              PRINT 2005, CMDP(4)(1:NCHAR(CMDP(4)))
          END IF
      END IF
      IF (LASTWANTED .EQ. 0) THEN
          PRINT '(A,$)',' last spectrum number to read ? '
          READ '(A)', CMDP(4)
          READ (CMDP(4),*,IOSTAT=IOS,ERR=150) LASTWANTED
  150     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(4)(1:NCHAR(CMDP(4)))
              ERROR = IOS
              RETURN
          END IF
      END IF
      IF (LASTWANTED .LT. FIRSTWANTED) LASTWANTED = FIRSTWANTED
*
      IF (NCMD .GE. 5) THEN
          ASCWRTFILE = CMDP(5)
      ELSE
          PRINT '(A,$)',' ascii spectra file to write to ? '
          READ  '(A)', ASCWRTFILE
      END IF
*
      AVRGINT = 1.0
      IF (NCMD .GE. 6) THEN
          READ (CMDP(6),*,IOSTAT=IOS,ERR=160) AVRGINT
  160     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(6)(1:NCHAR(CMDP(6)))
              ERROR = IOS
              RETURN
          END IF
      END IF
      IF (AVRGINT .LE. 0.) THEN
          PRINT '(A,$)',
     &        ' interval for new average (days,/=',AVRGINT,') ? '
          READ '(A)', CMDP(6)
          READ (CMDP(6),*,IOSTAT=IOS,ERR=165) AVRGINT
  165     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(6)(1:NCHAR(CMDP(6)))
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      MEM = MEMSET
      IF (NCMD .GE. 7) THEN
          READ (CMDP(7),*,IOSTAT=IOS,ERR=170) MEM
  170     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(7)(1:NCHAR(CMDP(7)))
              MEM = 0
          END IF
      END IF         
      IF (MEM .EQ. 0) THEN
          PRINT '(A,$)',' store in which memory ? '
          READ '(A)',CMDP(6)
          READ (CMDP(6),*,IOSTAT=IOS,ERR=180) MEM
  180     IF (IOS .NE. 0 .OR. MEM .LT. 1 .OR. MEM .GT. MAXMEM .OR.
     &        MEM .EQ. MEMRD) MEM = MEMSET
      END IF    
*
*     clear the memory that will hold the average spectra
*     added 98/02/11
      WRITE (CMDP(2),*) MEM
      IF (NCMD .LT. 2) NCMD = 2
      CALL MEMCLR (ERROR)
*
*     set up the output file 
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
      IF (DB) PRINT *,'read from ',FIRSTWANTED,
     &                ' to ',LASTWANTED
*
*
      DO ASCWANTED = FIRSTWANTED, LASTWANTED
*
*         clear the memory being read into
          WRITE (CMDP(2),*) MEMRD
          IF (NCMD .LT. 2) NCMD = 2
          CALL MEMCLR (ERROR)
*
          WRITE (BUF,*) ' read spectrum ',ASCWANTED,' from ',
     &            ASCRDFILE(1:IFCHAR)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
*         read spectrum number "ascwanted" from ascii format file
          CALL RDASCFL (ERROR)
*
          IF (ERROR .GT. 0) GO TO 200  ! end of file if error = 5
*
*         summarise the spectrum housekeeping
          WRITE (CMDP(2),*) MEMRD
          WRITE (CMDP(3),*) MEMRD
          CALL LISTMEM (ERROR)
*
          IF (DB) PRINT *,'READFILE: NAXIS(MEM) = ',NAXIS(MEM),
     &        ' ASCWANTED = ',ASCWANTED,' ERROR = ',ERROR,
     &        ' ADTOAV = ',ADTOAV
*
*
          JULDATEAV = JULDATE(MEM)
          IF (ASCWANTED .EQ. FIRSTWANTED) JULDATEAV = JULDATE(MEMRD)
          IF ((JULDATE(MEMRD)-JULDATEAV) .LT. AVRGINT) THEN
*             add read in spectrum to current average
              ADTOAV = .TRUE.
          ELSE
*             start a new average spectrum
              ADTOAV = .FALSE.
*             first write current average to output ascii format file
              IF (NAXIS(MEM) .GT. 0) THEN
                  IF (DB) PRINT *,'IADDED=',IADDED
*                 set JD to the average of those in the averaged spectra
*                  JULDATE(MEM) = JDSUM / ADDED(MEM)
                  JULDATE(MEM) = JDSUM / IADDED                  
*                 set HA to the average of those in the averaged spectra
*                  HA(MEM) = HASUM / ADDED(MEM)
                  HA(MEM) = HASUM / IADDED                  
*                 write to file
                  CALL WRTASCF2 (MEM, BEGIN_COUNT, ERROR)
                  IF (ERROR .EQ. 0) BEGIN_COUNT = BEGIN_COUNT + 1
              END IF
*             allow existing average spectrum to be over-written by MEMCOPY
              NAXIS(MEM) = 0
          END IF
*
          IF (NAXIS(MEM) .GT. 0 .AND. ERROR .EQ. 0 .AND.
     &        ADTOAV) THEN
              PRINT '(A,I3,A,I3,A,$)',' add mem',MEMRD,
     &              ' to average in mem',MEM,'; '
              WRITE (CMDP(2),*) MEMRD
              WRITE (CMDP(3),*) MEM
              IF (NCMD .LT. 3) NCMD = 3
              CALL MEMAV (ERROR)
*             add the JD to the sum of the julian dates of the average spectrum
              JDSUM = JDSUM + JULDATE(MEMRD)
*             add the HA to the sum of the hour angles of the average spectrum
              HASUM = HASUM + HA(MEMRD)
              IADDED = IADDED + 1
              IF (DB) PRINT *,'IADDED=',IADDED
          END IF
*
          IF (NAXIS(MEM) .EQ. 0 .AND. ERROR .EQ. 0) THEN
*             no spectrum in average array - copy 
              PRINT *,'start new average spectrum in mem',MEM
              IF (DB) PRINT *,'copy from',MEMRD,' to empty mem',MEM
              WRITE (CMDP(2),*) MEMRD
              WRITE (CMDP(3),*) MEM
              IF (NCMD .LT. 3) NCMD = 3
              CALL MEMCOPY (ERROR)
*             initialise the sum of the julian dates of the average spectrum
              JDSUM = JULDATE(MEM)
*             initialise the sum of the hour angles of the average spectrum
              HASUM = HA(MEM)
*             integer addition count for HA and JD
              IADDED = 1
              IF (DB) PRINT *,'IADDED=',IADDED
          END IF
      END DO
*
  200 CONTINUE
*
*     write last average spectrum to output ascii format file
      IF (NAXIS(MEM) .GT. 0) THEN
          IF (DB) PRINT *,'IADDED=',IADDED
*         set JD to the mean of those in the set of spectra averaged
*          JULDATE(MEM) = JDSUM / ADDED(MEM)
          JULDATE(MEM) = JDSUM / IADDED
*         set HA to the mean of those in the set of spectra averaged
*          HA(MEM) = HASUM / ADDED(MEM)
          HA(MEM) = HASUM / IADDED
*         write to file
          CALL WRTASCF2 (MEM, BEGIN_COUNT, ERROR)
      END IF
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
 2005 FORMAT ('illegal :',A)
      END
*********      
