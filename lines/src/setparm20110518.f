************************
      SUBROUTINE SETPARM (ERROR)
************************
*     list or change set parameters
**
*     CMDP command parameters used:
*     1 = command mnemonic SET
*     2 = parameter to alter, optional
*     3 = new value for parameter, optional

*     other subroutines called:
*     pgclos, pgldev, pgopen, pgslct, upcase

      IMPLICIT  NONE
*
      INTEGER   MAXNAME             ! number of variables to set
      PARAMETER (MAXNAME=23)
      CHARACTER NAME(MAXNAME)*10    ! variable names
      CHARACTER NOW*4               ! now
      CHARACTER PARMSTR*8           ! string SETPARM
      CHARACTER TYPE(MAXNAME)*1     ! variable types
      LOGICAL   PARMFOUND           ! true if parameter is valid
      INTEGER   ERROR               ! output error status
      INTEGER   I                   ! loop index
      INTEGER   INEW                ! new integer value
      INTEGER   IOS                 ! i/o error status
      INTEGER   LEN                 ! number of characters in string
      INTEGER   LEN1                ! number of characters in string
      INTEGER   LEN2                ! number of characters in string
      INTEGER   NCHAR               ! external function
      INTEGER   PGOPEN              ! external PGPLOT function
      LOGICAL   LNEW                ! new logical value
      REAL*8    RNEW                ! new real value
*
      DATA   NOW /'now '/
      DATA   NAME(1) /'MEMRD     '/
      DATA   NAME(2) /'MEMSET    '/
      DATA   NAME(3) /'PLOTDEV   '/
      DATA   NAME(4) /'PLOTPS    '/
      DATA   NAME(5) /'NXSUB     '/
      DATA   NAME(6) /'NYSUB     '/
      DATA   NAME(7) /'PGPROMPT  '/
      DATA   NAME(8) /'COLOUR    '/
      DATA   NAME(9) /'LINE      '/
      DATA   NAME(10)/'WIDTH     '/
      DATA   NAME(11)/'SYMBOL    '/
      DATA   NAME(12)/'CHARSIZE  '/
      DATA   NAME(13)/'FONT      '/
      DATA   NAME(14)/'XPMIN     '/
      DATA   NAME(15)/'XPMAX     '/
      DATA   NAME(16)/'YPMIN     '/
      DATA   NAME(17)/'YPMAX     '/
      DATA   NAME(18)/'CAPTION   '/
      DATA   NAME(19)/'ADDIDENT  '/
      DATA   NAME(20)/'ADDTSYS   '/
      DATA   NAME(21)/'TMWVNUM   '/
      DATA   NAME(22)/'TELLONG   '/
      DATA   NAME(23)/'TELLAT    '/
*                   1   2   3   4   5   6   7   8   9   10
      DATA   TYPE /'I','I','C','C','I','I','L','I','I','I',
*                  11  12  13  14  15  16  17  18  19   20
     &             'I','R','I','R','R','R','R','C','L','L',
     &             'L','R','R'/
*
      INCLUDE 'lines.inc'
*
      PARMSTR = 'SETPARM '
      IF (DB) PRINT *,'in ',PARMSTR
      ERROR = 0
*     list parameters only if no parameter name given
      IF (NCMD .LT. 2) THEN
          PRINT *,NAME(1), '=',MEMRD
          PRINT *,NAME(2), '=',MEMSET
          PRINT *,NAME(3), '=',PLOTDEV
          PRINT *,NAME(4) ,'=',PLOTPS
          PRINT *,NAME(5), '=',NXSUB
          PRINT *,NAME(6), '=',NYSUB
          PRINT *,NAME(7), '=',PGPROMPT
          PRINT *,NAME(8), '=',COLOUR
          PRINT *,NAME(9), '=',LINE
          PRINT *,NAME(10),'=',WIDTH
          PRINT *,NAME(11),'=',SYMBOL
          PRINT *,NAME(12),'=',CHARSIZE
          PRINT *,NAME(13),'=',FONT
          PRINT *,NAME(14),'=',XPMIN
          PRINT *,NAME(15),'=',XPMAX
          PRINT *,NAME(16),'=',YPMIN
          PRINT *,NAME(17),'=',YPMAX
          PRINT *,NAME(18),'=',CAPTION(1:NCHAR(CAPTION))
          PRINT *,NAME(19),'=',ADDIDENT
          PRINT *,NAME(20),'=',ADDTSYS
          PRINT *,NAME(21),'=',TMWVNUM
          PRINT *,NAME(22),'=',TELLONG
          PRINT *,NAME(23),'=',TELLAT
          RETURN
      END IF
*
      IF (DB) THEN
          DO I = 1,MAXNAME
              PRINT *,'name(',I,')=',NAME(I),' type =',TYPE(I)
          END DO
      END IF
*
      LEN1 = NCHAR(CMDP(2))
      CALL UPCASE (CMDP(2))
      PARMFOUND = .FALSE.
*
      DO I = 1, MAXNAME
          LEN2 = NCHAR(NAME(I))
          LEN1 = MAX(LEN1,LEN2)
*
          IF (CMDP(2)(1:LEN1) .EQ. NAME(I)(1:LEN1)) THEN
              PARMFOUND = .TRUE.
              IF (DB) PRINT *,PARMSTR,'change ',NAME(I)
*
*
*             give the user some help
*
              IF (NCMD .LT. 3) THEN
                  IF (I .EQ. 1) THEN
                      PRINT *,'memory into which data is read from disk'
                      PRINT *,NAME(I),NOW,MEMRD
                  ELSE IF (I .EQ. 2) THEN
                      PRINT *,'memory to operate on by default'
                      PRINT *,NAME(I),NOW,MEMSET
                  ELSE IF (I .EQ. 3) THEN
*                     list available PGPLOT devices
                      CALL PGLDEV 
                      PRINT *,'   e.g. for LINUX use /xwin'
                      PRINT *,NAME(I),NOW,PLOTDEV
                  ELSE IF (I .EQ. 4) THEN
*                     list available PGPLOT devices
                      CALL PGLDEV 
                      PRINT *,' use filename/type e.g. plot.ps/vcps'
                      PRINT *,NAME(I),NOW,PLOTPS
                  ELSE IF (I .EQ. 5) THEN
                      PRINT *,'number of columns on plot page'
                      PRINT *,'plot in column order if negative'
                      PRINT *,NAME(I),NOW,NXSUB
                  ELSE IF (I .EQ. 6) THEN
                      PRINT *,'number of rows on plot page' 
                      PRINT *,NAME(I),NOW,NYSUB
                  ELSE IF (I .EQ. 7) THEN
                      PRINT *,'prompt user to press Enter for next plot'
                      PRINT *,'True or False'
                      PRINT *,NAME(I),NOW,NYSUB
                  ELSE IF (I .EQ. 8) THEN
                      PRINT *,'Line colour:'
                      PRINT *,'Black=0, White=1, Red=2, Green=3, ',
     &                        'Blue=4, Cyan=5, Magenta=6, Yellow=7'
                      PRINT *,NAME(I),NOW,PGPROMPT
                  ELSE IF (I .EQ. 9) THEN
                      PRINT *,'Line type:'
                      PRINT *,'None=0, Full=1, Dashed=2, Dotdash=3, ',
     &                         'Dotted=4, Dashdotdotdot=5'
                      PRINT *,NAME(I),NOW,LINE
                  ELSE IF (I .EQ. 10) THEN
                      PRINT *,'Line width is in units of 0.13 mm'
                      PRINT *,NAME(I),NOW,WIDTH
                  ELSE IF (I .EQ. 11) THEN
                      PRINT *,'PGPLOT symbol, 1=. 2=+ 3=* 4=o 5=x etc'
                      PRINT *,NAME(I),NOW,SYMBOL
                  ELSE IF (I .EQ. 12) THEN
                      PRINT *,'1.0 equals 1/40 height of view surface'
                      PRINT *,NAME(I),NOW,CHARSIZE
                  ELSE IF (I .EQ. 13) THEN
                      PRINT *,'font: normal=1 roman=2 italic=3 script=4'
                      PRINT *,NAME(I),NOW,FONT
                  ELSE IF (I .EQ. 14) THEN
                      PRINT *,'X axis minimum: set > XPMAX to normalise'
                      PRINT *,NAME(I),NOW,XPMIN
                  ELSE IF (I .EQ. 15) THEN
                      PRINT *,'X axis maximum: set < XPMIN to normalise'
                      PRINT *,NAME(I),NOW,YPMAX
                  ELSE IF (I .EQ. 16) THEN
                      PRINT *,'Y axis minimum: set > YPMAX to normalise'
                      PRINT *,NAME(I),NOW,YPMIN
                  ELSE IF (I .EQ. 17) THEN
                      PRINT *,'Y axis maximum: set < YPMIN to normalise'
                      PRINT *,NAME(I),NOW,YPMAX
                  ELSE IF (I .EQ. 18) THEN
                      PRINT *,'plot caption'
                      PRINT *,NAME(I),NOW,CAPTION(1:NCHAR(CAPTION))
                  ELSE IF (I .EQ. 19) THEN
                      PRINT *,'add plot identifier with name date time'
                      PRINT *,'True or False'
                      PRINT *,NAME(I),NOW,ADDIDENT
                  ELSE IF (I .EQ. 20) THEN
                      PRINT *,'add Tsys to plot'
                      PRINT *,'True or False'
                      PRINT *,NAME(I),NOW,ADDTSYS
                  ELSE IF (I .EQ. 21) THEN
                      PRINT *,'TMW velocities: print numbers not chars'
                      PRINT *,'True or False'
                      PRINT *,NAME(I),NOW,ADDIDENT
                  ELSE IF (I .EQ. 22) THEN
                      PRINT *,'telescope longitude in degrees'
                      PRINT *,NAME(I),NOW,TELLONG
                  ELSE IF (I .EQ. 23) THEN
                      PRINT *,'telescope latitude in degrees'
                      PRINT *,NAME(I),NOW,TELLAT
                  END IF
              END IF
*
*
*             prompt the user for a new value
*
              IF (NCMD .LT. 3) THEN
                  PRINT '(1X,2A,$)',NAME(I)(1:NCHAR(NAME(I))),
     &                ' new value ?'
*                 allow for character strings with spaces     
                  READ '(A)', BUF
                  CMDP(3) = BUF(1:NCHAR(BUF))
                  NCMD = 3
              ELSE IF (NCMD .GE. 3) THEN
*                 put character string into buf
                  IF (DB) PRINT *,'ICMDSTART(3)=',ICMDSTART(3),
     &                            ' END=',ICMDEND(3)
                  BUF = CMD(ICMDSTART(3):LCMD)
              END IF 
              IF (DB) PRINT *,PARMSTR,'CMD=',CMD(1:LCMD)
              IF (DB) PRINT *,PARMSTR,'CMDP(3)=',CMDP(3)
              IF (DB) PRINT *,PARMSTR,'BUF=',BUF(1:NCHAR(BUF)) 
*
*
*             decode the new value from the character buffer
*
              IOS = 0
              IF (TYPE(I) .EQ. 'I') THEN
*                 integer variable
                  READ (CMDP(3),*,IOSTAT=IOS,ERR=201) INEW
              ELSE IF (TYPE(I) .EQ. 'L') THEN
*                 logical variable
                  READ (CMDP(3),'(L1)',IOSTAT=IOS,ERR=201) LNEW
              ELSE IF (TYPE(I) .EQ. 'R') THEN
*                 real variable
                  READ (CMDP(3),*,IOSTAT=IOS,ERR=201) RNEW
              END IF
*
  201         IF (IOS .NE. 0) THEN
                  PRINT *,'error reading ',CMDP(3)(1:NCHAR(CMDP(3)))
                  ERROR = 1
                  RETURN
              END IF
*
*
*             if new value is valid then apply it
*
              IF (I .EQ. 1 .AND. 
     &            INEW .GE. 1 .AND. INEW .LE. MAXMEM .AND. 
     &            INEW .NE. MEMSET) THEN
                  MEMRD = INEW
                  WRITE (BUF2,*) NAME(I),NOW,MEMRD
*
              ELSE IF (I .EQ. 2 .AND. 
     &            INEW .GE. 1 .AND. INEW .LE. MAXMEM .AND. 
     &            INEW .NE. MEMRD) THEN
                  MEMSET = INEW
                  WRITE (BUF2,*) NAME(I),NOW,MEMSET
*
              ELSE IF (I .EQ. 3) THEN
*                 select current PLOTDEV, check if open and if so close it
                  IF (IDPLOT1 .GT. 0) THEN
                      CALL PGSLCT (IDPLOT1)
                      IF (DB) PRINT *,PARMSTR,'closing ',PLOTDEV
                      CALL PGCLOS
                      IDPLOT1  = -1
                  END IF
*
                  WRITE (PLOTDEV,'(A)') BUF(1:NCHAR(BUF))
                  IF (DB) PRINT *,PARMSTR,'call PGOPEN with PLOTDEV=',
     &                PLOTDEV
*
                  IF (PLOTDEV(1:3) .EQ. 'off' .AND. 
     &                PLOTDEV(1:3) .EQ. 'OFF') THEN
                      PLOTCURS = 'NO '
                  ELSE
*                     open the new plot device
                      IDPLOT1 = PGOPEN (PLOTDEV)
                      IF (IDPLOT1 .GT. 0) THEN
                          PRINT *,'opened ',PLOTDEV
                      ELSE
                          PRINT *,'PGOPEN failed on ',PLOTDEV
                          ERROR = 1
                      END IF
                      CALL PGQINF ('CURSOR',PLOTCURS,LEN)
*                     ensure existing subdivisions of plot panel are set
                      NEW_SUBP = .TRUE.
                  END IF
                  WRITE (BUF2,*) NAME(I),NOW,PLOTDEV, 'cursor=',PLOTCURS
*
              ELSE IF (I .EQ. 4) THEN
*                 select current PLOTPS, check if open and if so close it
                  IF (IDPLOT2 .GT. 0) THEN
                      CALL PGSLCT (IDPLOT2)
                      IF (DB) PRINT *,PARMSTR,'closing ',PLOTPS
                      CALL PGCLOS
                      IDPLOT2 = -1
                  END IF
*
*                 use CMDP(3) instead of BUF incase filename 
*                 is in a loop and incorporates loop index, which is 
*                 set to the correct file name in CMDP(3) but not BUF
                  WRITE (PLOTPS,'(A)') CMDP(3)(1:NCHAR(CMDP(3)))
                  IF (DB) PRINT *,PARMSTR,'for PGOPEN, PLOTPS=',PLOTPS
                  IF (PLOTPS(1:3) .NE. 'off' .AND. 
     &                PLOTPS(1:3) .NE. 'OFF') THEN
*                     open the new postscript plot file
                      IDPLOT2 = PGOPEN (PLOTPS)
                      IF (IDPLOT2 .GT. 0) THEN 
                          PRINT *,'opened ',PLOTPS
                      ELSE
                          PRINT *,'PGOPEN failed on ',PLOTPS
                          ERROR = 1
                      END IF
*                     ensure existing subdivisions of plot panel are set
                      NEW_SUBP = .TRUE.
                  END IF
                  WRITE (BUF2,*) NAME(I),NOW,PLOTPS
*
              ELSE IF (I .EQ. 5) THEN
*                 new subdivision of plot panel in X
                  NXSUB = INEW
                  NEW_SUBP = .TRUE.
                  WRITE (BUF2,*) NAME(I),NOW,NXSUB
*
              ELSE IF (I .EQ. 6) THEN
*                 new subdivision of plot panel in Y
                  NYSUB = INEW
                  NEW_SUBP = .TRUE.
                  WRITE (BUF2,*) NAME(I),NOW,NYSUB
*
              ELSE IF (I .EQ. 7) THEN
*                 plot page prompt on/off
                  PGPROMPT = LNEW
                  WRITE (BUF2,*) NAME(I),NOW,PGPROMPT
*
              ELSE IF (I .EQ. 8 .AND.
     &            INEW .GE. 0 .AND. INEW .LE. 15) THEN
*                 plot line colour
                  COLOUR = INEW
                  WRITE (BUF2,*) NAME(I),NOW,COLOUR
*
              ELSE IF (I .EQ. 9 .AND. 
     &            INEW .GE. 0 .AND. INEW .LE. 5) THEN
*                 plot line type
                  LINE = INEW
                  WRITE (BUF2,*) NAME(I),NOW,LINE
*
              ELSE IF (I .EQ. 10 .AND. 
     &            INEW .GE. 0 .AND. INEW .LE. 20) THEN
*                 plot line width
                  WIDTH = INEW
                  WRITE (BUF2,*) NAME(I),NOW,WIDTH
*
              ELSE IF (I .EQ. 11 .AND.
     &            INEW .GE. -8 .AND. INEW .LE. 3000) THEN
*                 plot character size
                  SYMBOL = INEW
                  WRITE (BUF2,*) NAME(I),NOW,SYMBOL
*
              ELSE IF (I .EQ. 12 .AND.
     &            RNEW .GE. 0.2 .AND. RNEW .LE. 10.) THEN
*                 plot character size
                  CHARSIZE = RNEW
                  WRITE (BUF2,*) NAME(I),NOW,CHARSIZE
*
              ELSE IF (I .EQ. 13 .AND.
     &            INEW .GE. 1 .AND. INEW .LE. 4) THEN
*                 plot font type
                  FONT = INEW
                  WRITE (BUF2,*) NAME(I),NOW,FONT
*
              ELSE IF (I .EQ. 14) THEN
*                 plot y axis minimum
                  XPMIN = RNEW
                  WRITE (BUF2,*) NAME(I),NOW,XPMIN
*
              ELSE IF (I .EQ. 15) THEN
*                 plot y axis maximum
                  XPMAX = RNEW
                  WRITE (BUF2,*) NAME(I),NOW,XPMAX
*
              ELSE IF (I .EQ. 16) THEN
*                 plot y axis minimum
                  YPMIN = RNEW
                  WRITE (BUF2,*) NAME(I),NOW,YPMIN
*
              ELSE IF (I .EQ. 17) THEN
*                 plot y axis maximum
                  YPMAX = RNEW
                  WRITE (BUF2,*) NAME(I),NOW,YPMAX
*
              ELSE IF (I .EQ. 18) THEN
*                 plot caption
                  CAPTION = BUF(1:NCHAR(BUF))
                  WRITE (BUF2,*) NAME(I),NOW,CAPTION(1:NCHAR(CAPTION))
*
              ELSE IF (I .EQ. 19) THEN
*                 plot page prompt true/false
                  ADDIDENT = LNEW
                  WRITE (BUF2,*) NAME(I),NOW,ADDIDENT
*
              ELSE IF (I .EQ. 20) THEN
*                 add Tsys to plot true/false
                  ADDTSYS = LNEW
                  WRITE (BUF2,*) NAME(I),NOW,ADDTSYS
*
              ELSE IF (I .EQ. 21) THEN
*                 TMW write velocities as column headers numeric true/false
                  TMWVNUM = LNEW
                  WRITE (BUF2,*) NAME(I),NOW,TMWVNUM
*
              ELSE IF (I .EQ. 22) THEN
*                 telescope longitude
                  TELLONG = RNEW
                  WRITE (BUF2,*) NAME(I),NOW,TELLONG
*
              ELSE IF (I .EQ. 23) THEN
*                 telescope latitude
                  TELLAT = RNEW
                  WRITE (BUF2,*) NAME(I),NOW,TELLAT
              END IF
*
              PRINT *,BUF2(1:NCHAR(BUF2))
              IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF2(1:NCHAR(BUF2))
*
          END IF
      END DO
      IF (.NOT. PARMFOUND) THEN
          PRINT *,'Parameter ',CMDP(2)(1:NCHAR(CMDP(2))),
     &            ' not found'
      END IF
*
      RETURN
      END 
*********