*************************
      SUBROUTINE SPACELBL (CBUF,ICOL,LABEL)
*************************
*     find label in space delimited buffer
*
      IMPLICIT NONE
      CHARACTER CBUF*(*)      ! input buffer
      CHARACTER LABEL*(*)     ! output label extracted from buffer
      INTEGER   I             ! local
      INTEGER   ICHARSTART    ! local
      INTEGER   ICHAREND      ! local
      INTEGER   ICOL          ! input column number for label
      INTEGER   ICOLREAD      ! local
      INTEGER   NBUF          ! local
      INTEGER   NCHAR         ! external function
      INTEGER   NLAB          ! local
      LOGICAL   GOTLAB        ! local true if label found 
      LOGICAL   PREVSPACE     ! local set true if character is a space
      LOGICAL   CHANGETYPE    ! local char changes to space/notspace
*                             
      NBUF = NCHAR(CBUF)      ! number of non-blank characters in CBUF
      NLAB = LEN(LABEL)       ! length of LABEL
      PREVSPACE = .TRUE.      ! initialise previous char = space flag
      ICOLREAD = 0            ! initialise data column number
      ICHARSTART = 1          ! initialise start character count of column
      ICHAREND = -1           ! initialise end character count of column
      I = 0                   ! initialise character counter
      GOTLAB = .FALSE.        ! initialise do loop control
      DO WHILE (.NOT. GOTLAB)
          I = I + 1                   ! increment character count
          CHANGETYPE = .FALSE.        ! initialise no change space/nonspace
          IF (PREVSPACE .AND. CBUF(I:I) .NE. ' ') THEN
*             character not a space - at start of a label
              ICOLREAD = ICOLREAD + 1     ! new column lable read started
              ICHARSTART = I              ! reset col start char num
              CHANGETYPE = .TRUE.         ! space changed to not space
          END IF
          IF (.NOT. PREVSPACE .AND.
     &        (CBUF(I:I) .EQ. ' ' .OR. CBUF(I:I) .EQ. CHAR(13))) THEN
*             character is a space - at end of a label 
              ICHAREND = I - 1
              CHANGETYPE = .TRUE.         ! notspace changed to space
              IF (ICOLREAD .EQ. ICOL) GOTLAB = .TRUE.  ! terminate loop
          END IF
          IF (CHANGETYPE) PREVSPACE = .NOT. PREVSPACE
          IF (I .EQ. NBUF) THEN 
*             at end of buffer
              IF (ICOLREAD .EQ. ICOL) THEN
*                 wanted column may be last column
                  ICHAREND = NBUF
              ELSE
                  PRINT *,'cannot find label in col ',ICOL,' of:'
                  PRINT *,CBUF(1:NCHAR(CBUF))
                  RETURN
              END IF
              GOTLAB = .TRUE. ! terminate loop
          END IF
      END DO
*
      IF (ICOLREAD .EQ. ICOL) THEN
          LABEL(1:NLAB) = CBUF(ICHARSTART:ICHAREND)
      END IF
      PRINT *,'col ',ICOL,' label=',LABEL(1:NCHAR(LABEL)),
     &        ' from char ',ICHARSTART,' to ',ICHAREND
      RETURN
      END
***********************
