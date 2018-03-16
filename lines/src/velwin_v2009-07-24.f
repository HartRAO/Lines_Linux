***********************
      SUBROUTINE VELWIN (ERROR)
***********************
*     apply a velocity window to the spectrum
*
*     CMDP command parameters used:
*     1 = command mnemonic VW
*     2 = start velocity
*     3 = end velocity
*     4 = memory of spectrum to window
*
*     other subroutines called:
*     aitov, vellim, vtoai
*
      IMPLICIT  NONE
      INTEGER   COLOURP         ! COLOUR on entry, restored on exit
      INTEGER   ERROR           ! output error status
      INTEGER   IOS             ! local file error check
      INTEGER   LINEP           ! LINE on entry, restored on exit
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      INTEGER   N1              ! local array index of first channel
      INTEGER   N2              ! local array index of last channel
      INTEGER   PN1             ! local prev array index of first channel
      INTEGER   PN2             ! local prev array index of last channel
      INTEGER   NXSUBP          ! NXSUB on entry, restored on exit
      INTEGER   NYSUBP          ! NYSUB on entry, restored on exit
      INTEGER   WIDTHP          ! WIDTH on entry, restored on exit
      REAL      CHARSIZEP       ! char size on entry, restored on exit
      REAL      YPMAXP              ! YPMAX on entry, restored on exit
      REAL      YPMINP              ! YPMIN on entry, restored on exit
      REAL*8    PV1             ! local prev velocity of first channel
      REAL*8    PV2             ! local prev velocity of last channel
      REAL*8    V1              ! local velocity of first channel
      REAL*8    V2              ! local velocity of last channel
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in VELWIN'
      ERROR = 0
*     save plot entry values to be restored on exit
      CHARSIZEP = CHARSIZE
      COLOURP = COLOUR
      LINEP   = LINE
      NXSUBP = NXSUB
      NYSUBP = NYSUB
      WIDTHP = WIDTH
      YPMAXP = YPMAX
      YPMINP = YPMIN
*      
      V1 = 0d0
      V2 = 0d0
      IF (NCMD .GE. 3) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) V1
  210     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(2)(1:NCHAR(CMDP(2)))
              ERROR = IOS
              GO TO 900
          END IF
          IF (DB) PRINT *,'V1= ',V1,' km/s'
*
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) V2
  220     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(3)(1:NCHAR(CMDP(3)))
              ERROR = IOS
              GO TO 900
          END IF
          IF (DB) PRINT *,'V2= ',V2,' km/s'
      END IF
*
      MEM = MEMSET
      IF (NCMD .GE. 4) THEN
          READ (CMDP(4),*,IOSTAT=IOS,ERR=230) MEM
  230     IF (IOS .NE. 0) MEM = 0
      END IF
      IF (MEM .EQ. 0) THEN
          PRINT '(A,$)','Window the spectrum in which memory ?'
          READ (*,ERR=240,IOSTAT=IOS) MEM
  240     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(4)(1:NCHAR(CMDP(4)))
              MEM = MEMSET
          END IF
      END IF
*
      IF (NAXIS(MEM) .EQ. 0) THEN
          PRINT *,'mem ',MEM,' is empty'
          ERROR = 1
          GO TO 900
      END IF
* 
      PN1 = FIRST_CH(MEM)
      CALL AITOV (MEM, PN1, PV1)
      PN2 = LAST_CH(MEM)
      CALL AITOV (MEM, PN2, PV2)
*
      CALL VELLIM (PN1, PN2, PV1, PV2, 1)
      CALL VELLIM (PN1, PN2, PV1, PV2, LOGWRTUNIT)
*
      IF (V1 .EQ. 0D0 .AND. V2 .EQ. 0D0) THEN 
*
          IF (IDPLOT1 .GT. 0 .AND. PLOTCURS .EQ. 'YES') THEN
*             plot the spectrum in MEM
*
              IF (DB) PRINT *,'plot spectrum in mem ',MEM
              WRITE (CMDP(2),*) MEM
              NCMD = 2
*             store data and captions in plot arrays
              CALL PLCONFAU (ERROR)
*             force axis autoscaling on plot
              XPMIN = 0
              XPMAX = -1
              YPMIN = YPMINP
              YPMAX = YPMAXP
              CHARSIZE = 1.0
              NEW_PAGE = .TRUE.
              NEW_SUBP = .TRUE.
              NXSUB = 1
              NYSUB = 1
              SYMBOL = 1
              WIDTH = 1
*
*             PLOTDEV has been opened - select as the output device
              IF (DB) PRINT *,'plotting to ',PLOTDEV
              CALL PGSLCT (IDPLOT1)
*             plot the spectrum
              CALL PLOTDATA (ERROR)
*
              PRINT *,'Use the mouse to mark the start and end of ',
     &                'velocity window'
*             get velocity window using cursor on graph
              CALL PLCURSPS (ERROR)
*
              IF (NPTC .GT. 0) THEN
*                 else store first two cursor X positions
                  V1 = XCURS(1)
                  V2 = XCURS(2)
                  IF (DB) PRINT *,'cursor V1=',V1,' V2=',V2
              END IF
          END IF
*          
          IF (IDPLOT1 .LE. 0 .OR. PLOTCURS .EQ. 'NO ' .OR.
     &        NPTC .LT. 2) THEN
              PRINT '(A,$)',' New start and end velocities (as is = /)?'
              READ '(A)', BUF
              READ (BUF,*,ERR=250,IOSTAT=IOS) V1, V2
  250         IF (IOS .NE. 0) THEN
                  PRINT 2005,BUF(1:NCHAR(BUF))
              END IF 
              IF (V1 .EQ. 0D0 .AND. V2 .EQ. 0D0) THEN
                  PRINT *,'no change'
                  ERROR = 1
                  GO TO 900
              END IF
          END IF
      END IF
*
      IF (DB) PRINT *,'wanted V1=',V1,' V2=',V2
*
      CALL XTOAI (MEM, V1, N1)
      N1 = MAX (1, N1)
      N1 = MIN (N1, NAXIS1(MEM))
      CALL AITOV (MEM, N1, V1)
*
      CALL XTOAI (MEM, V2, N2)
      N2 = N2 + 1
      N2 = MIN (N2, NAXIS1(MEM))
      N2 = MAX (N1, N2)
      CALL AITOV (MEM, N2, V2) 
*
      CALL VELLIM (N1, N2, V1, V2, 1)
      CALL VELLIM (N1, N2, V1, V2, LOGWRTUNIT)
*
      FIRST_CH(MEM) = N1
      LAST_CH(MEM) = N2
*
*
  900 CONTINUE
*
*     reset plot parameters to values on entry
*
      CHARSIZE = CHARSIZEP
      COLOUR   = COLOURP
      LINE     = LINEP
      NEW_SUBP = .TRUE.
      NXSUB    = NXSUBP
      NYSUB    = NYSUBP
      WIDTH    = WIDTHP
      YPMAX    = YPMAXP
      YPMIN    = YPMINP
*
      RETURN
*
 2005 FORMAT ('illegal :',A)
      END 
*********