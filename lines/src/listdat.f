************************
      SUBROUTINE LISTDAT
************************
*     list data array to the screen
*
*     CMDP command parameters used:
*     1 = command mnemonic LD
*     2 = memory with data to be listed
*
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      CHARACTER IFMT1*20        ! local output format
      INTEGER   NL              ! local number of lines
      INTEGER   I               ! local loop index
      INTEGER   IOS             ! local file error status
      INTEGER   J               ! local loop index
      INTEGER   JSTART          ! local array index for start of line
      INTEGER   MEM             ! local memory to list from
      INTEGER   NCHAR           ! external function
      INTEGER   NPL             ! local number of points per line
      REAL*8    DATAMAX         ! local data maximum
      REAL*8    DATAMIN         ! local data minimum
*
      INCLUDE 'lines.inc'
*
      IF (NCMD .GE. 2) THEN
          READ (CMDP(2),*,IOSTAT=IOS,ERR=100) MEM
  100     IF (IOS .NE. 0) MEM = MEMSET
      END IF
      IF (MEM .LT. 1 .OR. MEM .GE. MAXMEM) MEM = MEMSET
*
      IF (NAXIS(MEM) .LT. 1) THEN
          PRINT *,'No data in memory ',MEM
          RETURN
      END IF
*
      PRINT *,'data in memory ',MEM
*
      WRITE (BUF(1:),*) 'from spectrum ',NUMFILE(MEM),
     & ' in file ',FROMFILE(MEM)(1:NCHAR(FROMFILE(MEM)))
      PRINT '(A)',BUF(1:NCHAR(BUF))
*
*     find max and min in data
      DATAMAX = YIN(MEM,FIRST_CH(MEM))
      DATAMIN = YIN(MEM,LAST_CH(MEM))
      DO I = FIRST_CH(MEM)+1, LAST_CH(MEM)
          IF (DATAMAX .LT. YIN(MEM,I)) DATAMAX = YIN(MEM,I)
          IF (DATAMIN .GT. YIN(MEM,I)) DATAMIN = YIN(MEM,I)
      END DO
*     prevent format overflow
      IFMT1 = '(I4,1X,8(F8.4,1X))'
      IF (DATAMAX .GT. 1000D0 .OR. DATAMIN .LT. -100D0) THEN
          IFMT1 = '(I4,1X,8(F8.3,1X))'
      ELSE IF (DATAMAX .GT. 10000D0 .OR. DATAMIN .LT. -1000D0) THEN
          IFMT1 = '(I4,1X,8(F8.2,1X))'
      ELSE IF (DATAMAX .GT. 100000D0 .OR. DATAMIN .LT. -10000D0) THEN
          IFMT1 = '(I4,1X,8(F8.1,1X))'
      ELSE IF (DATAMAX .GT. 1000000D0 .OR. DATAMIN .LT. -100000D0) THEN
          IFMT1 = '(I4,1X,8(F8.0,1X))'
      END IF
*
      PRINT '(2A)','index             +1       +2       +3       +4',
     &             '       +5       +6       +7'
*
      NPL = 8            
      NL = (LAST_CH(MEM) - FIRST_CH(MEM) + 1)/ NPL
      IF ((NL * NPL) .LT. NAXIS1(MEM)) NL = NL + 1
      DO I = 1, NL
          JSTART = FIRST_CH(MEM) + (I - 1)*NPL
          WRITE (BUF(1:77),IFMT1)
     &        JSTART, (YIN(MEM,J), J = JSTART, JSTART + NPL-1)
          PRINT '(A)',BUF(1:77)
      END DO
*
      RETURN
      END
*********
