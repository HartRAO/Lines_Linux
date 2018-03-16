************************
      SUBROUTINE SYSCMD 
************************
*     send user command to system
*
*     CMDP command parameters used:
*     1 = command mnemonic SY
*     unparsed CMD is used to pass parameters to system
*
*     other subroutines called:
*     none
*
      INTEGER   I               ! local index
      INTEGER   NCHAR           ! external function
      LOGICAL   STARTPARM       ! local start of CMDP(2) 
*
      INCLUDE 'lines.inc'
*
      IF (DB) PRINT *,'in SYSCMD'
      IF (NCMD .GE. 2) THEN
        STARTPARM = .FALSE.
        I = 0
        DO WHILE (.NOT. STARTPARM)
          I = I + 1
          STARTPARM = (CMD(I:I) .EQ. ' ' .AND. CMD(I+1:I+1) .NE. ' ')
        END DO
        IF (DB) PRINT *,'command: ',CMD(I+1:NCHAR(CMD))
        CALL SYSTEM (CMD(I+1:NCHAR(CMD)))
      ELSE
        IF (DB) PRINT *,'no parameters: ',CMD(1:NCHAR(CMD))
      END IF
      RETURN
      END
*********