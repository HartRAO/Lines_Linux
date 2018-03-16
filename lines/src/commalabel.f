***************************
      SUBROUTINE COMMALABEL (CBUF,ICOL,LABEL)
***************************
*     find label in comma delimited buffer
      IMPLICIT NONE
      CHARACTER CBUF*(*)      ! input buffer
      CHARACTER LABEL*(*)     ! output label extracted from buffer
      INTEGER   I             ! local
      INTEGER   ICHARSTART    ! local
      INTEGER   ICHAREND      ! local
      INTEGER   ICOL          ! input column number for label
      INTEGER   ICOLREAD      ! local
      INTEGER   LCHAR         ! local
      INTEGER   NBUF          ! local
      INTEGER   NCHAR             ! external function
      INTEGER   NLAB          ! local
      LOGICAL   GOTLAB        ! local true if label found 
      LOGICAL   BLANK         ! local true if blank character
*                             
      NBUF = NCHAR(CBUF)      ! number of non-blank characters in CBUF
      NLAB = LEN(LABEL)       ! length of LABEL
      GOTLAB = .FALSE.        ! initialise do loop control
      ICOLREAD = 0            ! initialise data column number
      ICHARSTART = 1          ! initialise start character count of column
      ICHAREND = -1           ! initialise end character count of column
      I = 0                   ! initialise character counter for loop
      DO WHILE (.NOT. GOTLAB)
          I = I + 1                   ! increment character count
          IF (CBUF(I:I) .EQ. ',') THEN
*             found the end of a column
              ICOLREAD = ICOLREAD + 1     ! column ICOLREAD read completed
              ICHARSTART = ICHAREND + 2   ! reset col start char num
              ICHAREND = I - 1            ! char number at end of column
              IF (ICOLREAD .EQ. ICOL) GOTLAB = .TRUE.  ! terminate loop
          END IF
          IF (I .EQ. NBUF) THEN 
*             at end of buffer
              IF (ICOLREAD+1 .NE. ICOL) THEN
*                 if wanted column is last column, may not end in a comma
                  ICHARSTART = ICHAREND + 2
                  ICHAREND = NBUF
                  GOTLAB = .TRUE. ! terminate loop
              ELSE
                  PRINT *,'could not find label in column ',ICOL,' of:'
                  PRINT *,CBUF(1:NCHAR(CBUF))
                  RETURN
              END IF
          END IF
      END DO
*
      IF (GOTLAB) THEN
*         skip any spaces (char(32)) before first character
          BLANK = .TRUE.
          I = 0
          DO WHILE (BLANK)
              IF (CBUF(ICHARSTART+I:ICHARSTART+I) .EQ. CHAR(32)) THEN
                  I = I + 1
              ELSE
                  BLANK = .FALSE.
              END IF
          END DO
          LABEL(1:NLAB) = CBUF((ICHARSTART+I):ICHAREND)
      END IF
*     change trailing carriage return (char(13)) to space (char(32)
      LCHAR = NCHAR (LABEL)
      IF (LABEL(LCHAR:LCHAR) .EQ. CHAR(13)) THEN
          LABEL(LCHAR:LCHAR) = CHAR(32)
      END IF
      RETURN
      END
************************
