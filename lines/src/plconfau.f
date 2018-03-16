*************************
      SUBROUTINE PLCONFAU (ERROR)
*************************
*     automatic configuration of PGPLOT (default)
*
*     CMDP command parameters used:
*     1 = command mnemonic PL
*     2/3/4 = ADD to plot on previous plot
*     2/3/4 = X to plot X array instead of Y array
*     2/3/4 = CAP to use set caption
*     2/3/4 = memory from which to plot the spectrum
*
*     other subroutines called:
*     aitov, upcase, nchar, qmemused
*
      IMPLICIT  NONE
*
      LOGICAL   ADD                 ! add to previous plot
      LOGICAL   CAP                 ! use set caption 
      LOGICAL   XPL                 ! plot X values instead of Y values
      INTEGER   ERROR               ! output error status
      INTEGER   I                   ! loop index
      INTEGER   IOS                 ! i/o status
      INTEGER   MEM                 ! memory to plot from
      INTEGER   NCHAR               ! external function
*
      INCLUDE  'lines.inc'
*
      IF (DB) PRINT *,'in PLCONFAU'
      IF (DB) THEN
          DO I = 1, 4
              PRINT *, 'CMDP',I,' = ',CMDP(I)
          END DO
      END IF
*
      ADD = .FALSE.
      XPL = .FALSE.
      CAP = .FALSE.
      ERROR = 0
      MEM = 0
*
      CALL QMEMUSED
      IF (DB) PRINT *,' MEMUSED=',MEMUSED
      IF (MEMUSED .EQ. 0) THEN
          PRINT *,'no data to plot'
          ERROR = 1
          RETURN
      END IF
*
      DO I = 2, NCMD
          CALL UPCASE (CMDP(I))
          IF (CMDP(I) .EQ. 'ADD') THEN
              ADD = .TRUE.
              NEW_PAGE = .FALSE.
          END IF
*
          IF (CMDP(I) .EQ. 'CAP') CAP = .TRUE.
*
          IF (CMDP(I) .EQ. 'X') XPL = .TRUE.
*
          READ (CMDP(I),*,IOSTAT=IOS,ERR=210) MEM
  210     CONTINUE
      END DO 
*
      IF (DB) PRINT *,'MEM=',MEM
*
      IF (MEM .EQ. 0) THEN
          IF (NAXIS(MEMSET) .EQ. 0) THEN 
              PRINT '(A,$)',' plot data in which memory ?'
              READ '(A)',CMDP(3)
              READ (CMDP(3),*,IOSTAT=IOS,ERR=215) MEM
  215         IF (IOS .NE. 0) THEN
                  PRINT *,'illegal ',CMDP(3)
                  ERROR = IOS
                  RETURN
              END IF
          ELSE
              PRINT *,'mem=memset'
              MEM = MEMSET
          END IF
      END IF
*
      IF (DB) PRINT *,'MEM=',MEM
      IF (MEM .LT. 1 .OR. MEM .GT. MAXMEM) THEN
          PRINT *,'illegal MEM =',MEM
          ERROR = 1
          RETURN
      END IF 
*
      IF (NAXIS1(MEM) .LE. 0) THEN
          PRINT *,'mem ',MEM,' is empty'
          ERROR = 1
          RETURN
      END IF
*     keep mem plotted for use in altdatgr
      MEMPLOTTED = MEM
*
*
      IF (DB) THEN
          IF (ADD) THEN
              PRINT *,'ADD to previous plot'
          ELSE 
              PRINT *,'new plot'
          END IF
          IF (XPL) THEN
              PRINT *,'plot X array'
          END IF
          IF (CAP) THEN
              PRINT *,'add CAP:',CAPTION
          ELSE 
              PRINT *,'std toplbl'
          END IF
      END IF
*
*
*     store data in X,Y arrays
*
      IF (DB) PRINT *,'store mem ',MEM,' in plot arrays'
      NPTS = 0
      DO I = FIRST_CH(MEM),LAST_CH(MEM)
          NPTS = NPTS + 1
          IF ((TRANSFRM(MEM) .EQ. 1) .OR. XPL) THEN
              XP(NPTS) = NPTS
          ELSE
              XP(NPTS) = XIN(MEM,I)
          END IF
*
          IF (XPL) THEN
              YP(NPTS) = XIN(MEM,I)
          ELSE
              YP(NPTS) = YIN(MEM,I)
          END IF
      END DO
*
*     plot caption and axis labels
*
      IF (DB) PRINT *,'make plot caption, labels'
      IF (ADD) THEN 
          WRITE (TOPLBL,*) ' '
          WRITE (XLBL,*) ' '
          WRITE (YLBL,*) ' '
      END IF
*
      IF (TRANSFRM(MEM) .EQ. 1 .AND. .NOT. ADD) THEN
          IF (CAP) THEN 
              TOPLBL = CAPTION
          ELSE
              WRITE (TOPLBL,*) 
     &        'Transform of ',OBJECT(MEM)(1:20)
          END IF
          WRITE (XLBL,*) 'CHANNEL'
          WRITE (YLBL,*) 'AMPLITUDE'
      END IF
*
      IF (TRANSFRM(MEM) .EQ. 0 .AND. .NOT. ADD) THEN
          IF (CAP) THEN
              TOPLBL = CAPTION
          ELSE
              IF (RESTFREQ(MEM) .GT. 0D0) THEN
                  TOPLBL(1:) = OBJECT(MEM)(1:20)
*                 add frequency to toplabel
                  WRITE (TOPLBL(NCHAR(TOPLBL)+1:),'(F10.3,A4)',
     &                IOSTAT=IOS) RESTFREQ(MEM)/1E6, 'MHz '
              ELSE
                  WRITE (TOPLBL(1:),'(A)', IOSTAT=IOS) 
     &                OBJECT(MEM)(1:NCHAR(OBJECT(MEM)))
              END IF
              IF (ADDED(MEM) .EQ. 1.0) THEN
                  IF (SCAN(MEM) .GT. 0) THEN
                      WRITE (TOPLBL(NCHAR(TOPLBL)+1:),*,IOSTAT=IOS) 
     &                    ' scan ',SCAN(MEM)
                      IF (IOS .GT. 0) PRINT 3000,'TOPLBL1'
                  END IF 
              ELSE IF (ADDED(MEM) .GT. 1.0) THEN
                  IF (SCAN(MEM) .GT. 0) THEN
                      WRITE (TOPLBL(NCHAR(TOPLBL)+1:),*,IOSTAT=IOS) 
     &                    ' last scan ',SCAN(MEM)
                      IF (IOS .GT. 0) PRINT 3000,'TOPLBL2'
                  END IF
              END IF
              TOPLBL((NCHAR(TOPLBL)+2):) =
     &            DATE_OBS(MEM)(1:NCHAR(DATE_OBS(MEM)))
          END IF
          XLBL(1:) = CTYPE1(MEM)
C          IF (NCHAR(CTYPE2(MEM)) .GT. 1) YLBL(1:) = CTYPE2(MEM)
          IF (CTYPE2(MEM)(1:1) .NE. ' ') YLBL(1:) = CTYPE2(MEM)
          YLBL(NCHAR(YLBL)+2:) = BUNIT(MEM) 
*         correct label of ANTENNA_TEMP with unit Jy
          IF (BUNIT(MEM)(1:1) .EQ. 'J') THEN
              YLBL(1:) = 'FLUX DENSITY Jy'
          END IF
      END IF
*
*     system temperature label is optional
      WRITE (TSYSLBL,*) ' '
*
      IF (ADDTSYS .AND. TSYS(MEM) .GT. 0.0 
     &            .AND. NINT(ADDED(MEM)) .EQ. 1) THEN
          WRITE (TSYSLBL,'(A3,F7.2,A4,A5,F8.2,A2,F8.2,A)',IOSTAT=IOS) 
     &    'HA=',HA(MEM),'deg ',
     &    'Tsys=',TSYS(MEM),'+-',DTSYS(MEM),
     &    TCALUNIT(MEM)(1:NCHAR(TCALUNIT(MEM)))
      END IF
*
      IF (ADDTSYS .AND. NINT(ADDED(MEM)) .GT. 1) THEN
          WRITE (TSYSLBL,'(A5,I5)',IOSTAT=IOS) 
     &    'Avrg=',NINT(ADDED(MEM))
          IF (IOS .NE. 0) PRINT 3000, 'TSYSLBL'
      END IF
*
      IF (XPL) THEN
          WRITE (XLBL(1:),'(A)') 'X index'
          WRITE (YLBL(1:),'(A)') 'X values'
      END IF
*
      RETURN
*
 3000 FORMAT ('error making ',A)
*      
      END
*********
