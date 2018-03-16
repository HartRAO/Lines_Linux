************************
      SUBROUTINE LISTDIR 
************************
*     list directory
*
*     CMDP command parameters used:
*     1 = command mnemonic LS
*     2 and following = options for ls
*     unparsed command string CMD is used if options are present
*
      INCLUDE 'lines.inc'
*
      IF (NCMD .EQ. 1) THEN
          CALL SYSTEM ("ls")
      ELSE
          CALL SYSTEM (CMD)
      END IF
      RETURN
      END
*********