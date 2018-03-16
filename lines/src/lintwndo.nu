*************************
      SUBROUTINE LIWINDOW (ERROR)
*************************
*     set velocity window for calculating line integral
*
      IMPLICIT  NONE
*
      INTEGER   ERROR           ! output error status
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error check
      INTEGER   MEM             ! local memory to use
      INTEGER   NCHAR           ! external function
      INTEGER   N1              ! local array index of first channel
      INTEGER   N2              ! local array index of last channel
      INTEGER   PN1             ! local prev array index of first channel
      INTEGER   PN2             ! local prev array index of last channel
      REAL*8    PV1             ! local prev velocity of first channel
      REAL*8    PV2             ! local prev velocity of last channel
      REAL*8    V1              ! local velocity of first channel
      REAL*8    V2              ! local velocity of last channel
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in LIWINDOW'
      ERROR = 0
      V1 = 0d0
      V2 = 0d0
*
*     see if the CLEAR command has been given
      IF (CMDP(2)(1:5) .EQ. 'CLEAR') THEN
          PRINT *,'clear the velocity window for line integral'
          NLIWINDOW = 0
          RETURN
      END IF
*
      IF (NCMD .GE. 3) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=210) V1
  210     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(2)(1:NCHAR(CMDP(2)))
              ERROR = IOS
              RETURN
          END IF
          V1 = V1
          IF (DB) PRINT *,'V1= ',V1
*
          READ (CMDP(3),*,IOSTAT=IOS,ERR=220) V2
  220     IF (IOS .NE. 0) THEN
              PRINT 2005, CMDP(3)(1:NCHAR(CMDP(3)))
              ERROR = IOS
              RETURN
          END IF
          V2 = V2
          IF (DB) PRINT *,'V2= ',V2
      END IF
*
*     option to list current window
*
      IF (NCMD .GE. 2) CALL UPCASE (CMDP(2))
*
      IF (NCMD .EQ. 1 .OR.
     &    NCMD .EQ. 2 .AND. CMDP(2)(1:4) .EQ. 'SHOW') THEN
          IF (NLIWINDOW .GT. 0) THEN   
              PRINT '(/A)','current velocity window for line integral'
              PRINT '(9X,A,8X,A)',  'Vstart','Vend'
              PRINT '(9X,A,7X,A)',  '(km/s)','(km/s)'
              PRINT '(F15.3,F12.3)', LIVSTART, LIVEND
          ELSE
              PRINT *,'No velocity window assigned for line integral'
          END IF
      END IF
*
      IF (CMDP(2)(1:4) .EQ. 'SHOW') RETURN
*
      IF (V1 .EQ. 0D0 .AND. V2 .EQ. 0D0) THEN 
          PRINT '(A,$)','window start, end velocities (km/s, /=none) ?'
          READ '(A)', BUF
          READ (BUF,*,ERR=250,IOSTAT=IOS) V1, V2
  250     IF (IOS .NE. 0) THEN
              PRINT 2005,BUF(1:NCHAR(BUF))
              ERROR = 1
              RETURN
          END IF 
          IF (V1 .EQ. 0D0 .AND. V2 .EQ. 0D0) THEN
              PRINT *,'no change'
              RETURN
          END IF
      END IF
*
*     add the new block values to common
*
      NLIWINDOW = 1
      LIVSTART = V1
      LIVEND   = V2
      IF (DB) PRINT *,'LIVSTART,END=',LIVSTART,LIVEND
      RETURN
*
 2005 FORMAT ('illegal: ',A)
      END 
*********
