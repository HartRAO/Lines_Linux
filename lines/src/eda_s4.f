      SUBROUTINE EDA_S4(DB,Y,N,ENDSAV,WORK,SAVE,NW,ERR)
c     smooth by running medians of 4
      IMPLICIT NONE
      LOGICAL DB
      INTEGER N,NW,ERR
      REAL*8  Y(*), ENDSAV, WORK(NW), SAVE(NW), ENDM1,TWO
      DATA TWO/2.0D0/
c     even length medians offset the output sequence to the high end,
c     since they cannot be symmetric.
c     ENDSAV is left holding Y(N) since there is no other room for it.
c     Y(1) is unchanged
c
      IF (DB) PRINT '(A,$)','in EDA_S4 '
      ENDSAV = Y(N)
      ENDM1 = Y(N-1)
      CALL EDA_RUNMED(DB,Y,N,4,WORK,SAVE,NW,ERR)
      Y(2) = (Y(1)+Y(2))/TWO
      Y(N) = (ENDM1+ENDSAV)/TWO
      IF (DB) PRINT *,'exit EDA_S4'
      RETURN
      END
