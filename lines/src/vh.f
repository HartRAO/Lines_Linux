      SUBROUTINE VH (ERROR)
***************************
*     find amplitude at given velocity by linear interpolation
*
*     CMDP command parameters used:
*     1 = command mnemonic PC
*     2 = velocity (x value)
*     3 = memory in use
*
      IMPLICIT  NONE
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local array index of vel from VTOAI
      INTEGER   IOS             ! local file error check
      INTEGER   IOFFSET         ! local array index of "down" peak
      INTEGER   MEM             ! local memory to use
      INTEGER   N1              ! local array index of first channel
      INTEGER   N2              ! local array index of last channel
      INTEGER   NCHAR           ! external function
      REAL*8    OFFSET          ! local index offset of "up" - "down" spectra
      REAL*8    VFORH           ! local velocity at which height is wanted
      REAL*8    V1              ! local velocity of first channel
      REAL*8    V2              ! local velocity of last channel
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in VH'
      ERROR = 0
*
      VFORH = 1E10
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) VFORH
  210     IF (IOS .NE. 0) ERROR = IOS
      END IF
      IF (DB) PRINT *,'input V=',VFORH
*
      MEM = MEMSET
      IF (NCMD .GE. 3) THEN
          READ (CMDP(3),*,IOSTAT=IOS,ERR=310) MEM
  310     IF (IOS .NE. 0) MEM = 0
      END IF
*
      IF (MEM .LT. 1 .OR. MEM .GT. MAXMEM) THEN
          PRINT '(A,$)',' memory in use ?'
          READ '(A)', CMDP(3)
          READ (CMDP(3),*,IOSTAT=IOS,ERR=320) MEM
  320     IF (IOS .NE. 0) THEN
              ERROR = IOS
              RETURN
          END IF
      END IF
*
      IF (NAXIS(MEM) .LT. 1) THEN
          PRINT *,'mem ',MEM,' is empty'
      END IF
*
      IF (TRANSFRM(MEM) .EQ. 1) THEN
          PRINT *,'transform, not spectrum'
          ERROR = 1
          RETURN
      END IF
*
      IF (VFORH .EQ. 1E10) THEN
          PRINT '(2A,$)',CTYPE1(MEM)(1:NCHAR(CTYPE1(MEM))),
     &        ' at which the height is wanted (/ exits)?'
          READ '(A)', BUF
          READ (BUF,*,IOSTAT=IOS,ERR=220) VFORH
  220     IF (IOS .NE. 0) THEN
              PRINT 1010, BUF(1:NCHAR(BUF))
              RETURN
          END IF
      END IF
      IF (VFORH .EQ. 1E10) RETURN
*
*     initialise outputs
      HATV1 = 0.0
      HATV2 = 0.0
*     get array index of the point nearest the wanted velocity
      CALL XTOAI (MEM,VFORH,I)
*     check index range is valid
      IF (I .GE. FIRST_CH(MEM) .AND. I+1 .LE. LAST_CH(MEM)) THEN
          HATV1 = YIN(MEM,I)
          WRITE (BUF,*) 'At',VFORH,'km/s, I = ',I,' H = ',
     &        HATV1,BUNIT(MEM)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      ELSE
*         list the valid velocity range
          PRINT 1010,CTYPE1(MEM)
          N1 = FIRST_CH(MEM)
          N2 = LAST_CH(MEM)
          CALL AITOV (MEM,N1,V1)
          CALL AITOV (MEM,N2,V2)
          CALL VELLIM (N1,N2,V1,V2,1)
      END IF
*
      IF (FROFFSET(MEM) .NE. 0 .AND. FOLDED(MEM) .EQ. 0) THEN
*         spectrum is frequency shifted but not yet folded
*         get array index offset to "down" peak
          OFFSET = NAXIS1(MEM)*FROFFSET(MEM)/BW(MEM)
          IF (DB) PRINT *,'OFFSET=',offset
          IOFFSET = I + NINT(OFFSET)
          HATV2 = YIN(MEM,IOFFSET)
          WRITE (BUF,*) 'Idown = ',IOFFSET,' H = ',HATV2,BUNIT(MEM)
          PRINT *,BUF(1:NCHAR(BUF))
          IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      END IF
*
      RETURN
 1010 FORMAT (' illegal ',A)
      END
      