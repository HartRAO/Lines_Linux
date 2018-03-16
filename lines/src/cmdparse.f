*************************
      SUBROUTINE CMDPARSE 
*************************
*     separate out command parameters from input string
*
*     other subroutines called:
*     none
*
      IMPLICIT NONE
*
      INTEGER   I                   ! loop index
      INTEGER   NCHAR               ! external function
      LOGICAL   PREVSPACE           ! previous byte = space
*
      INCLUDE 'lines.inc'
*
*
*     get a command from user or input file
*
      IF (DB) PRINT *,'in CMDPARSE'
      LCMD = NCHAR(CMD)
      PREVSPACE = .TRUE.
      NCMD = 0
      DO I = 1, 8
          ICMDSTART(I) = 0
          ICMDEND(I) = 0
          WRITE (CMDP(I),'(A)') ' '
      END DO
*
      DO I = 1, LCMD
          IF (PREVSPACE .AND. CMD(I:I) .NE. ' ') THEN
*             character not a space - at start of new label
              NCMD = NCMD + 1
              ICMDSTART(NCMD) = I
              PREVSPACE = .FALSE.
          END IF
          IF (.NOT. PREVSPACE .AND.
     &        (CMD(I:I) .EQ. ' ' .OR. CMD(I:I) .EQ. CHAR(13))) THEN
*             character is a space, was previously not space - end of parm
              ICMDEND(NCMD) = I - 1
              PREVSPACE = .TRUE.
          END IF
      END DO
      IF (NCMD .GE. 1) ICMDEND(NCMD) = LCMD
      IF (DB) PRINT *,'CMD=',CMD(1:LCMD)
      IF (DB) PRINT *,'CMD has ',LCMD,' chars in ',NCMD,' strings'
*     store command parameters individually
      DO I = 1, NCMD
          CMDP(I)(1:) = CMD(ICMDSTART(I):ICMDEND(I))
          IF (DB) PRINT *,'CMDP(',I,')=',CMDP(I)(1:NCHAR(CMDP(I)))
      END DO
*
      RETURN
*
      END
      