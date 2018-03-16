************************
      SUBROUTINE LOOPCMD (ERROR,QUIT)
************************
*     execute the commands in the do loop command buffer
*
      IMPLICIT  NONE
      CHARACTER ISTR*8              ! loop index written into a string
      CHARACTER JUNK*64             ! temporary character buffer
      CHARACTER LOOPSTR*8           ! string loopcmd
      CHARACTER OP*1                ! operator +-*/
      LOGICAL   QUIT                ! true to quit
      LOGICAL   LOOPNAMEOP          ! true if operation on loopname
      INTEGER   ERROR               ! error status
      INTEGER   I                   ! loop index
      INTEGER   IOS                 ! i/o error status
      INTEGER   J                   ! loop index
      INTEGER   K                   ! loop index
      INTEGER   L                   ! loop index
      INTEGER   LC                  ! length of command parameter string
      INTEGER   LN                  ! length of loopname string
      INTEGER   NCHAR               ! external function
      INTEGER   NJ                  ! length of string
      INTEGER   NUM                 ! number for operation
      INTEGER   NUMOUT              ! number output from operation
      LOGICAL   TEST                ! check for loop 
*
      INCLUDE 'lines.inc'
      SAVE I,J,K
*
      LOOPSTR = 'LOOPCMD '
      IF (DB) PRINT *,'in ',LOOPSTR
*
*     cycle thru user's do loop
      IF (DB) PRINT *,LOOPSTR, 'start=',LOOPSTART,' end=',LOOPEND,
     &        ' inc=',LOOPINC
*
      DO I = LOOPSTART, LOOPEND, LOOPINC
          IF (DB) PRINT *,LOOPSTR,'I=',I
*         convert the loop index to a character string
          WRITE (ISTR,'(I8)') I
*         make the string left justified instead of right justified
          TEST = .TRUE.
          L = 1
          DO WHILE (TEST)
              IF (ISTR(L:L) .NE. ' ') THEN
                  WRITE (JUNK,'(A)') ISTR(L:LEN(ISTR))
                  WRITE (ISTR,'(A)') JUNK(1:LEN(ISTR))
                  TEST = .FALSE.
              END IF
              L = L + 1
          END DO
*
          IF (DB) PRINT *,LOOPSTR,'ISTR=',ISTR
*
*         cycle thru user's commands in the loop
          DO J = 1, LOOPCMDNUM
              IF (DB) PRINT *,LOOPSTR,'loop cmd num =',J
              WRITE (CMD,'(A)'), LOOPBUF(LOOPCMDSTRT(J):LOOPCMDEND(J))
              CALL CMDPARSE
              IF (DB) PRINT *,LOOPSTR,'look for loop name'
*
*             look for $LOOPNAME amongst the parameters
*
              DO K = 2, NCMD
                  LC = NCHAR(CMDP(K))
                  LN = NCHAR(LOOPNAME)
                  IF (DB) PRINT *,LOOPSTR,'loop name=',LOOPNAME(1:LN),
     &                    ' CMDP(',K,')=',CMDP(K)(1:LC)

*                 see if loop index is embedded in a command parameter
*                 eg an output file name,
*                 where it must be preceded by a $ character
                  IF (DB) PRINT *,LOOPSTR,'look for $',LOOPNAME(1:LN),
     &                            ' in ',CMDP(K)(1:LC)
                  DO L = 1, LC-LN, 1
                      IF ((CMDP(K)(L:L) .EQ. '$') .AND.
     &                   (CMDP(K)(L+1:L+LN) .EQ. LOOPNAME(1:LN))) THEN
*                        replace the $LOOPNAME with its numeric value
*                        as a string, using JUNK to create the new cmd
                         WRITE (JUNK(1:),'(A)') CMDP(K)(1:L)
                         NJ = NCHAR(JUNK)
                         WRITE (JUNK(NJ:),'(A)') ISTR(1:NCHAR(ISTR))
                         NJ = NCHAR(JUNK)
                         WRITE (JUNK(NJ+1:),'(A)') CMDP(K)(L+LN+1:LC)
                         NJ = NCHAR(JUNK)
                         WRITE (CMDP(K),'(A)') JUNK(1:NJ)
                         LC = NCHAR(CMDP(K))
                         IF (DB) PRINT *,LOOPSTR,'new CMDP(',K,')=',
     &                           CMDP(K)(1:LC)
                      END IF
                  END DO
*
*                 see if an operation using the loop index is defined
*
                  OP = CMDP(K)(LN+1:LN+1)
                  NUM = 0
                  NUMOUT = 0
                  LOOPNAMEOP = .FALSE.
                  READ (CMDP(K)(LN+2:),*,ERR=100,IOSTAT=IOS) NUM
  100             IF (IOS .EQ. 0 .AND. 
     &                OP .EQ. '+' .OR. OP .EQ. '-' .OR. 
     &                OP .EQ. '*' .OR. OP .EQ. '/') LOOPNAMEOP = .TRUE.
*
*                 check characters match
*                 note ambiguity if loopname matches part of another parameter
                  IF (CMDP(K)(1:LN) .EQ. LOOPNAME(1:LN)) THEN
*
                      IF (LOOPNAMEOP) THEN 
*                         operate on loopname                      
                          IF (OP .EQ. '+') THEN
                              NUMOUT = I + NUM
                          ELSE IF (OP .EQ. '-') THEN
                              NUMOUT = I - NUM
                          ELSE IF (OP .EQ. '*') THEN
                              NUMOUT = I * NUM
                          ELSE IF (OP .EQ. '/') THEN
                              NUMOUT = I / NUM
                          END IF
                          WRITE (CMDP(K),*) NUMOUT
                      ELSE IF (LN .EQ. LC) THEN
*                         no operation on loopname
                          NUMOUT = I
                          WRITE (CMDP(K),*) NUMOUT
                      END IF
*
*                     if LC > LN and not loopnameop
*                     do not replace CMDP(K) with NUMOUT  
*
                      IF (DB) PRINT *,LOOPSTR,'I=',I,' OP=',OP,
     &                 ' NUM=',NUM,' NUMOUT=',NUMOUT
                      IF (DB) PRINT *,LOOPSTR,'CMDP(',K,') now ',
     &                        CMDP(K)(1:NCHAR(CMDP(K)))
                  END IF
              END DO        
*             select command to execute
              CALL CMDSELCT (ERROR,QUIT)
*             bailout of loop if error occurred
              IF (ERROR .GT. 0) GO TO 900
          END DO
          IF (DB) THEN
              PRINT *,LOOPSTR,'Loop index I =',I,' continue ?'
              READ '(A)',JUNK
          END IF
      END DO
*
  900 CONTINUE
      WRITE (BUF,*) 'quit DO loop'
      IF (ERROR .GT. 0) THEN
          IF (ERROR .EQ. 5) THEN
              WRITE (BUF(14:),*) ' at end of data'
          ELSE 
              WRITE (BUF(14:),*) ' ERROR ',ERROR
          END IF
      END IF
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
      RETURN
      END
      