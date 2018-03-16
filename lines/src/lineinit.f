*************************
      SUBROUTINE LINEINIT (ERROR)
*************************
*     initialise parameters in the common blocks
*
*     other subroutines called:
*     envval, logfopen
*
      IMPLICIT NONE
      INTEGER  ERROR                ! output error status
      INTEGER  I                    ! local loop index
c      INTEGER  IOS                  ! i/o error code
c      INTEGER  IOS2                 ! i/o error code
      INTEGER  LB2                  ! length of string in BUF2
      INTEGER  NCHAR                ! external function
c      LOGICAL  NOEND                ! loop control 
*
      INCLUDE 'lines.inc'
*
***** read file unit numbers
*
*     note unit 50 is reserved for reading the date
      ASCRDUNIT  = 51
      CMDRDUNIT  = 52
      MCARDUNIT  = 53
      NCSVRDUNIT = 54
      NEWSRDUNIT = 55
      NFRDUNIT   = 56
      PSSRDUNIT  = 57
*
***** write file unit numbers
*
      ASCWRTUNIT = 71
      GWWRTUNIT  = 72
      LOGWRTUNIT = 73
      MCAWRTUNIT = 74
      TMWRTUNIT  = 75
      TMWRTUNIT2 = 76
      TMWRTUNIT3 = 77
      TMWRTUNIT4 = 78
      FLAWRTUNIT = 79
*      
***** check environment variable for location of help file
*
      WRITE (BUF,'(A)') 'HLP'
      CALL ENVVAL (BUF,BUF2,LB2)
      IF (LB2 .GT. 0) THEN
          WRITE (HLPRDDIR,'(A)') BUF2(1:LB2)
C          PRINT *,'HLPRDDIR=',HLPRDDIR
      ELSE
          WRITE (HLPRDDIR,'(A)') '/usr/local/src/lines/doc/'
      END IF
*
***** standard file names
*
      WRITE (CMDRDFILE, '(A)') 'user'
      WRITE (LOGWRTFILE,'(A)') 'lines.log'
      WRITE (NEWSRDFILE,'(2A)')HLPRDDIR(1:NCHAR(HLPRDDIR)),'lines.news'
      WRITE (PREVAFILE, '(A)') 'q'
      WRITE (FLAWRTFILE,'(A)') 'lines.fla'
*
***** general lines parameters
*
      ASCAT = 0                 ! at ascii spectrum number
      DO I = 1, MAXBASE
         BASESTART(I) = 0D0     ! polyfit baseline start
         BASEEND(I) = 0D0       ! polyfit baseline end
      END DO
      DB = .FALSE.              ! debug off
      DOLOOP = .FALSE.          ! not in do loop
      FIXBASE = .FALSE.         ! gaussian baseline not fixed
      GETLOOPCMD = .FALSE.      ! not getting do loop command
      MEMGF = 0                 ! no memory for gaussian fit
      MEMRD = MAXMEM-2          ! default memory to read data into
      MEMSET = MAXMEM-1         ! default standard operating memory
      NBASE = 0                 ! number of polynomial baseline pairs
      NGCC = 0                  ! number of gain curve corr. coefficients
      NPAR  = 0                 ! number of gaussian parameters
      NTMVEL = 0                ! time series velocity range blocks in use
      TMWVNUM = .TRUE.          ! numeric not char format for column velocity
*
***** clear and initialise all memories
*
      CMDP(2) = 'ALL'
      NCMD = 2
      CALL MEMCLR (ERROR)
*
***** telescope longitude and latitude / degrees - default to HartRAO
*
      TELLONG = +27.685D0
      TELLAT  = -25.886D0
*
****** precipitable water vapour [mm] for atmospheric absorption calc
*
      PWV = 0.0D0
*
***** parameters for PGPLOT
*
      ADDIDENT = .FALSE.
      ADDTSYS  = .TRUE.
      AUTOCONFIG = .TRUE.
      AXIS = 0
      WRITE (CAPTION,'(A)') ' '
      CHARSIZE = 1.0
      COLOUR = 1
      FONT = 2
      IDPLOT1 = -1
      IDPLOT2 = -1
      IXBAR = 0
      IYBAR = 0
      LINE = 1
      NEW_PAGE = .TRUE.
      NEW_SUBP = .TRUE.
      PGPROMPT = .FALSE.
      WRITE (PLOTDEV,'(A)') 'OFF'
      WRITE (PLOTPS,'(A)') 'OFF'
      NXSUB = 1
      NYSUB = 1
      SYMBOL = 1
      XPMIN = 0
      XPMAX = -1
      YPMIN = 0
      YPMAX = -1
      WIDTH = 1
*
***** list news file to screen
*
c      OPEN (NEWSRDUNIT,FILE=NEWSRDFILE,STATUS='OLD',
c     &      ACCESS='SEQUENTIAL',IOSTAT=IOS,ERR=100)
c  100 IF (IOS .EQ. 0) THEN
c          NOEND = .TRUE.
c          DO WHILE (NOEND)
c              READ (NEWSRDUNIT,'(A)',END=104,IOSTAT=IOS2,ERR=102) BUF
c  102         PRINT *,BUF(1:NCHAR(BUF))
c          END DO
c  104     CLOSE (NEWSRDUNIT,ERR=106)
c  106     CONTINUE
c      END IF
*
***** initial help notification
*
      PRINT *,'HE or ?  provides a list of available commands'
      PRINT *,'HELP is now also provided through webpages'
      PRINT *,'HE CMD   provides more detailed help on CMD'
      PRINT *,'command mnemonics are not case-sensitive'
*
***** turn on logging
*
      CMDP(1) = ' '
      CMDP(2) = 'ON'
      NCMD = 2
      CALL LOGFOPEN (ERROR)
*
      RETURN
      END
*********
