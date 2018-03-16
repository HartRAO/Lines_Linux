***********************
      SUBROUTINE WRTTMFL (ERROR)
************************
*     write spectra as time series is mulit-column ascii format
*
*     CMDP command parameters used:
*     1 = command mnemonic TMW
*     2 = ascii spectra file to read from
*     3 = time series file to write to
*     4 = start record
*     5 = end record
*     start and end velocity blocks must be set up first with TMV
*
*     other subroutines called:
*     aitov, er_close, er_open, er_write, rdascfl, upcase, vtoai

      IMPLICIT NONE
*
      CHARACTER ANSWER*1            ! local user answer
      CHARACTER VC*2                ! local column caption start chars
      LOGICAL   EXISTS              ! local file existence
      INTEGER*4 C1                  ! local first character to write to
      INTEGER*4 ERROR               ! output error status
      INTEGER*4 FIRSTWANTED         ! local first wanted spectrum
      INTEGER*4 I                   ! local loop index
      INTEGER*4 IB1                 ! local first non-blank char in BUF
      INTEGER*4 IOS                 ! local i/o status
      INTEGER*4 IVEND(20)           ! local index of TMVEND
      INTEGER*4 IVSTART(20)         ! local index of TMVSTART
      INTEGER*4 J                   ! local loop index
      INTEGER*4 K                   ! local loop index
      INTEGER*4 LASTWANTED          ! local last wanted spectrum
      INTEGER*4 NCHAR               ! external function
      REAL*8    DTIK                ! local error in line temperature at datum
      REAL*8    JDMOD               ! local MJD = JD - 2400000.5d0
      REAL*8    PCDELT1             ! local previous CDELT1(MEMRD)
      REAL*8    PCRVAL1             ! local previous CRVAL1(MEMRD)
      REAL*8    TIK                 ! local line temperature at datum
      REAL*8    TRMSI               ! local rms noise at pixel
      REAL*8    VI                  ! local velocity at index
*      
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in WRTTMFL'
      ERROR = 0
*
      IF (NCMD .GE. 2) THEN
          ASCRDFILE = CMDP(2)
      ELSE
          PRINT '(A,$)',' ASCII spectra file to read from ?'
          READ  '(A)', ASCRDFILE
      END IF
*
      INQUIRE (FILE=ASCRDFILE,EXIST=EXISTS)
      IF (.NOT. EXISTS) THEN
          PRINT *,'File ',ASCRDFILE(1:NCHAR(ASCRDFILE)),' not found'
          ERROR = 1
          RETURN
      END IF
*
      IF (NCMD .GE. 3) THEN
          TMWRTFILE = CMDP(3)
      ELSE 
          PRINT '(A,$)',' Time series file to write to ?'
          READ  '(A)', TMWRTFILE
      END IF 
*
      INQUIRE (FILE=TMWRTFILE,EXIST=EXISTS)
*     only ask to overwrite if user was asked for output file name
      IF (EXISTS .AND. NCMD .LT. 3) THEN
          PRINT *,'file ',TMWRTFILE(1:NCHAR(TMWRTFILE)),' exists'
          PRINT '(A,$)',' okay to overwrite it (Y/N) ?'
          READ  '(A)', ANSWER
          CALL UPCASE (ANSWER)
          IF (ANSWER .NE. 'Y') THEN
              ERROR = 1
              RETURN
          END IF
      END IF
*
      IF (EXISTS) PRINT *,'overwriting output files'
*
*     open output file 1 for intensities
      WRITE (LBUF,*) 'open ',TMWRTFILE(1:NCHAR(TMWRTFILE)),
     &    ' for JDObs, intensities at velocities for spreadsheet'
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
      OPEN (TMWRTUNIT,FILE=TMWRTFILE,ERR=911,IOSTAT=IOS)
*
*     open output file 2 for errors in intensity at each datum
      WRITE (TMWRTFILE2,'(2A)') TMWRTFILE(1:NCHAR(TMWRTFILE)),'2'
      WRITE (LBUF,*) 'open ',TMWRTFILE2(1:NCHAR(TMWRTFILE2)),
     &    ' for JDObs, intensity errors at velocities'
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(LBUF))
      OPEN (TMWRTUNIT2,FILE=TMWRTFILE2,ERR=912,IOSTAT=IOS)
*
*     open output file 3 for JD, DateObs, HA, Tsys, dTsys, PntCorr, GainCorr 
      WRITE (TMWRTFILE3,'(2A)') TMWRTFILE(1:NCHAR(TMWRTFILE)),'3'
      WRITE (LBUF,*) 'open ',TMWRTFILE3(1:NCHAR(TMWRTFILE3)),
     &   ' for JDObs DateObs HA Ts dTs PntCorr'
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
      OPEN (TMWRTUNIT3,FILE=TMWRTFILE3,ERR=913,IOSTAT=IOS)
*
*     open output file 4 for JD, velocities, intensities
*     needed if making contour plot in Axum or gri
      WRITE (TMWRTFILE4,'(2A)') TMWRTFILE(1:NCHAR(TMWRTFILE)),'4'
      WRITE (LBUF,*) 'open ',TMWRTFILE4(1:NCHAR(TMWRTFILE4)),
     &   ' for JDObs, Vlsr, Intensity for GRI irregular contour plot'
      PRINT *,LBUF(1:NCHAR(LBUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) LBUF(1:NCHAR(LBUF))
      OPEN (TMWRTUNIT4,FILE=TMWRTFILE4,ERR=914,IOSTAT=IOS)
*
*
      FIRSTWANTED = 1 
      IF (NCMD .GE. 5) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=110) FIRSTWANTED
  110     IF (IOS .NE. 0) FIRSTWANTED = 1
      END IF
*
      LASTWANTED = 32767
      IF (NCMD .GE. 5) THEN
          READ (CMDP(5),*,IOSTAT=IOS,ERR=140) LASTWANTED
  140     IF (IOS .NE. 0) LASTWANTED = 32767
      END IF
*
      IF (LASTWANTED .LT. FIRSTWANTED) LASTWANTED = FIRSTWANTED
*
*
      PCDELT1 = 0D0
      PCRVAL1 = 0D0      
      DO ASCWANTED = FIRSTWANTED, LASTWANTED
*         read spectra from ascii format file
          CALL RDASCFL (ERROR)
*
          IF (DB) PRINT *,'in WRTTMFL, spectrum ',ASCWANTED,' read' 
          IF (ERROR .EQ. 5) THEN
*              unexpected end of file
              CLOSE (TMWRTUNIT,IOSTAT=IOS,ERR=940)
              GO TO 990
          END IF
*          
          IF (ASCWANTED .EQ. FIRSTWANTED) THEN
              IF (NTMVEL .EQ. 0) THEN
*                 no velocity ranges specified - use full range
                  PRINT *,'no velocity range specified, use all data'
                  NTMVEL = 1
                  CALL AITOV (MEMRD,FIRST_CH(MEMRD),TMVSTART(1))
                  CALL AITOV (MEMRD,LAST_CH(MEMRD),TMVEND(1))
                  IF (DB) PRINT *,'vel range =',TMVSTART(1),TMVEND(1)
              END IF
*
*             write caption for intensity file 1
              IF (DB) PRINT *,'write caption to ',TMWRTFILE
              WRITE (TMWRTUNIT,'(3A)',IOSTAT=IOS,ERR=931)
     &            OBJECT(MEMRD)(1:NCHAR(OBJECT(MEMRD))),
     &            ' time series of intensities F from ',
     &            FROMFILE(MEMRD)(1:NCHAR(FROMFILE(MEMRD)))
*
*             write caption for delta intensity file 2
              WRITE (TMWRTUNIT2,'(3A)',IOSTAT=IOS,ERR=932)
     &            OBJECT(MEMRD)(1:NCHAR(OBJECT(MEMRD))),
     &            ' time series of delta F from ',
     &            FROMFILE(MEMRD)(1:NCHAR(FROMFILE(MEMRD)))
*
*             write caption for HA + Tsys + dTsys file 3
              WRITE (TMWRTUNIT3,'(3A)',IOSTAT=IOS,ERR=933)
     &            OBJECT(MEMRD)(1:NCHAR(OBJECT(MEMRD))),
     &            ' obs HA, Tsys, dTsys from ',
     &            FROMFILE(MEMRD)(1:NCHAR(FROMFILE(MEMRD)))
*
*             write caption for JD, velocity, Intensity file 4
              WRITE (TMWRTUNIT4,'(3A)',IOSTAT=IOS,ERR=934)
     &            OBJECT(MEMRD)(1:NCHAR(OBJECT(MEMRD))),
     &            ' for AXUM irregular contour plot from ',
     &            FROMFILE(MEMRD)(1:NCHAR(FROMFILE(MEMRD)))
*
*             write column headers for file 4
              WRITE (TMWRTUNIT4,'(6A)',IOSTAT=IOS,ERR=934) 
     &            'MJD        ',CTYPE1(MEMRD)(1:NCHAR(CTYPE1(MEMRD))),
     &            '  ',CTYPE2(MEMRD)(1:NCHAR(CTYPE2(MEMRD))),'  ',
     &            BUNIT(MEMRD)(1:NCHAR(BUNIT(MEMRD)))
*
*             clear LBUF and write MJD into first column 
              IF (DB) PRINT *,'write column headers to ',TMWRTFILE
              WRITE (LBUF,'(A)',IOSTAT=IOS,ERR=931) 'MJD,       '
              WRITE (LBUF2,'(A)',IOSTAT=IOS,ERR=932) 'MJD,       '
              WRITE (BUF2,'(A,14X,A,5X,A,4X,A,3X,A,1X,A,1X,A)',
     &                     IOSTAT=IOS,ERR=933) 
     &            ' MJD,       ','DATEOBS,','HA,','TSYS,','DTSYS,',
     &            'PNTCOR,','GAINCOR'
*
              IF (DB) PRINT *,'NTMVEL = ',NTMVEL
              DO J = 1, NTMVEL
*                 get the array indicies of the start and end velocities
                  IF (DB) PRINT *,'vel rng=',TMVSTART(J),TMVEND(J)
                  CALL XTOAI (MEMRD,TMVSTART(J),IVSTART(J))
                  CALL XTOAI (MEMRD,TMVEND(J),IVEND(J))
                  IF (DB) PRINT *,'IVSTART=',IVSTART(J),
     &                ' IVEND=',IVEND(J)
              END DO
*
              K = 0
              DO I = FIRST_CH(MEMRD), LAST_CH(MEMRD)
                  DO J = 1, NTMVEL
                      IF (I .GE. IVSTART(J) .AND. I .LE. IVEND(J)) THEN
                          K = K + 1
*                         channel is within a specified velocity block
                          CALL AITOV (MEMRD,I,VI)
*
C                          IF (ASCWANTED .EQ. FIRSTWANTED) THEN
*                             for first spectrum only
*                             write channel velocity for channel velocity file
C                              WRITE(TMWRTUNIT4,'(E12.6)',
C     &                            IOSTAT=IOS,ERR=934) VI
C                          END IF
*
*                         write VP/Mvelocity as column header
                          IF (DB) PRINT *,'col ',K,' V=',VI
*                         C1 = start character to write velocity
                          C1 = 13*K + 2
                          IF (TMWVNUM) THEN
*                             write velocity in numeric format
*                             e.g. for XESS and similar spreadsheets
                              IF (DB) PRINT *,'NUMERIC VELOCITY'
                              WRITE (LBUF(C1:),'(F10.3,A1)') VI,','
                          ELSE
*                             write velocity with +, - replaced with Vp, Vm
*                             for AXUM and similar spreadsheets
                              IF (DB) PRINT *,'CHARACTER VELOCITY'
                              WRITE (BUF,'(F10.3)') ABS(VI)
*                             find first non-blank character in BUF                          
                              IB1 = 1
                              DO WHILE (BUF(IB1:IB1) .EQ. ' ')
                                  IB1 = IB1+1
                              END DO
*
                              IF (VI .GE. 0D0) THEN
*                                 positive velocity label 
                                  VC = 'Vp'
                              ELSE 
*                                 negative velocity label
                                  VC = 'Vm'
                              END IF
*
                              IF (C1+11 .LT. LBUFLEN) THEN
                                  WRITE (LBUF(C1:),'(3A)') 
     &                                VC,BUF(IB1:10),','
                              ELSE
                                  IF (DB) PRINT 2010
                              END IF
                          END IF
                      END IF
                  END DO
              END DO
*
              IF (.NOT. TMWVNUM) THEN
*                 convert all decimal points in column headers to 'p' for axum
                  DO I = 1, NCHAR(LBUF)
                      IF (LBUF(I:I) .EQ. '.') LBUF(I:I) = 'p'
                  END DO
              END IF
*              
*             write column headers to disk              
              WRITE (TMWRTUNIT,'(A)',IOSTAT=IOS,ERR=931) 
     &            LBUF(1:NCHAR(LBUF))
              WRITE (TMWRTUNIT2,'(A)',IOSTAT=IOS,ERR=932) 
     &            LBUF(1:NCHAR(LBUF))
              WRITE (TMWRTUNIT3,'(A)',IOSTAT=IOS,ERR=933) 
     &            BUF2(1:NCHAR(BUF2))
          END IF
*
*         check for changes in channel velocity
*         which would invalidate column headers
          IF (PCDELT1 .NE. 0D0 .AND. PCRVAL1 .NE. 0D0) THEN
              IF (CDELT1(MEMRD) .NE. PCDELT1) THEN
                  PRINT *,'CDELT1 now ',CDELT1(MEMRD), ' was ',PCDELT1,
     &                ' at spectrum ',ASCWANTED
                  ERROR = 2
              END IF
              IF (CRVAL1(MEMRD) .NE. PCRVAL1) THEN
                  PRINT *,'CRVAL1 now ',CRVAL1(MEMRD), ' was ',PCRVAL1,
     &                ' at spectrum ',ASCWANTED
                  ERROR = ERROR + 3
              END IF
              IF (ERROR .GT. 0) THEN
                  PRINT '(A,$)',' okay to continue (Y/N) ?'
                  READ '(A)', ANSWER
                  CALL UPCASE (ANSWER)
                  IF (ANSWER .NE. 'Y') THEN
                      GO TO 990
                  ELSE 
                      IF (ERROR .EQ. 2) PCDELT1 = CDELT1(MEMRD)
                      IF (ERROR .GT. 2) PCRVAL1 = CRVAL1(MEMRD)
                      ERROR = 0
                  END IF
              END IF    
          END IF
*
*
          IF (DB) PRINT *,'write spectrum ',ASCWANTED,' to ',TMWRTFILE
*         first column has modified julian date MJD = JD - 2400000.5
          JDMOD = JULDATE(MEMRD) - 2400000.5D0
          WRITE (LBUF,'(F11.4,A1)',IOSTAT=IOS,ERR=931) 
     &        JDMOD,','
          WRITE (LBUF2,'(F11.4,A1)',IOSTAT=IOS,ERR=932) 
     &        JDMOD,','
          WRITE (BUF2,'(F11.4,A2,A20,A1,F8.3,A1,F8.2,A1,F8.2,
     &                  A1,F6.3,A1,F6.3)',
     &        IOSTAT=IOS,ERR=933) 
     &        JDMOD,', ',DATE_OBS(MEMRD),',',
     &        HA(MEMRD),',', TSYS(MEMRD),',', DTSYS(MEMRD),',',
     &        PNTCORR(MEMRD),',', GAINCORR(MEMRD)
*
          K = 0
          DO I = FIRST_CH(MEMRD), LAST_CH(MEMRD)
              DO J = 1, NTMVEL
                  IF (I .GE. IVSTART(J) .AND. I .LE. IVEND(J)) THEN
*                     channel is within a specified velocity block
                      K = K + 1
                      C1 = 13*K
*                      
*                     antenna temperature and rms error
                      TIK = YIN(MEMRD,I)
                      DTIK = RMS(MEMRD)
                      IF (PSS(MEMRD) .GT. 0D0) THEN
*                         spectrum had been converted to Jy - convert to K
                          TIK = TIK / PSS(MEMRD)
                          DTIK = DTIK / PSS(MEMRD)
                      END IF
*
*                     system temperature at this point in spectrum
                      TRMSI = DTIK * (TIK+TSYS(MEMRD))/TSYS(MEMRD)
*                     error in K at this point in spectrum
                      DTIK = DSQRT (TRMSI**2 + 
     &                       (TIK*DTSYS(MEMRD)/TSYS(MEMRD))**2)
                      IF (PSS(MEMRD) .GT. 0D0) THEN
                          TIK = TIK * PSS(MEMRD)
                          DTIK = DTIK * PSS(MEMRD)
                      END IF
*
                      IF (C1+12 .LT. LBUFLEN) THEN
*                         write intensity and delta I to separate files
                          WRITE (LBUF(13*K:),'(E12.6,A1)',
     &                        IOSTAT=IOS,ERR=931) TIK,',' 
                          WRITE (LBUF2(13*K:),'(E12.6,A1)',
     &                        IOSTAT=IOS,ERR=932) DTIK,',' 
                      ELSE
                          IF (DB) PRINT 2010
                      END IF                         
*
*                     write JD, channel velocity, intensity to file 4
*                     for AXUM irregular contour plot (manual p280)
*                     or for Gri
                      CALL AITOV (MEMRD,I,VI)
                      WRITE(TMWRTUNIT4,'(F10.3,A,E12.6,A,E12.6)',
     &                    IOSTAT=IOS,ERR=934) JDMOD,'  ',VI,'  ',TIK
                  END IF
              END DO
          END DO
*         write out completed buffers to disc files
          WRITE (TMWRTUNIT,'(A)',IOSTAT=IOS,ERR=931) 
     &        LBUF(1:NCHAR(LBUF))
          WRITE (TMWRTUNIT2,'(A)',IOSTAT=IOS,ERR=932) 
     &        LBUF2(1:NCHAR(LBUF2))
          WRITE (TMWRTUNIT3,'(A)',IOSTAT=IOS,ERR=933) 
     &        BUF2(1:NCHAR(BUF2))
*
*         store channel spacing and offset for comparison with next spectrum
          PCDELT1 = CDELT1(MEMRD)
          PCRVAL1 = CRVAL1(MEMRD)
      END DO
      RETURN
*
*
  911 CALL ER_OPEN (TMWRTFILE, IOS)
      ERROR = 1
      GO TO 990
*
  912 CALL ER_OPEN (TMWRTFILE2, IOS)
      ERROR = 1
      GO TO 990
*      
  913 CALL ER_OPEN (TMWRTFILE3, IOS)
      ERROR = 1
      GO TO 990
*      
  914 CALL ER_OPEN (TMWRTFILE4, IOS)
      ERROR = 1
      GO TO 990
*
  931 CALL ER_WRITE (TMWRTFILE, IOS)
      ERROR = 3
      GO TO 990
*
  932 CALL ER_WRITE (TMWRTFILE2, IOS)
      ERROR = 3
      GO TO 990
*
  933 CALL ER_WRITE (TMWRTFILE3, IOS)
      ERROR = 3
      GO TO 990
*
  934 CALL ER_WRITE (TMWRTFILE4, IOS)
      ERROR = 3
      GO TO 990
*
  940 CALL ER_CLOSE (TMWRTFILE, IOS)
      ERROR = 4
      GO TO 990
*
  990 CLOSE (TMWRTUNIT,ERR=991)
  991 CLOSE (TMWRTUNIT2,ERR=992)
  992 CLOSE (TMWRTUNIT3,ERR=993)
  993 CLOSE (TMWRTUNIT4,ERR=999)
  999 RETURN
*
c 2005 FORMAT ('illegal :',A)
 2010 FORMAT ('LBUFov',$)
      END
*********
      