C-----------------------------------------------------------       
C
C
C    The following programs are taken from NUMERICAL RECIPES
C
C-----------------------------------------------------------
        SUBROUTINE MRQMIN(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     *          COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
C
C    Set following parameter to largest number of fit parameters
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER MA, NCA, NDATA, LISTA(MA), MFIT, MMAX
      PARAMETER (MMAX=4)
      DIMENSION X1(NDATA),X2(NDATA),Y(NDATA),SIG(NDATA),A(MA),
     *     COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRY(MMAX),
     *     BETA(MMAX),DA(MMAX)
      SAVE OCHISQ, ATRY, BETA, DA
C
        IF(ALAMDA.LT.0.)THEN
            KK=MFIT+1
            DO J=1,MA
                IHIT=0
                DO K=1,MFIT
                    IF(LISTA(K).EQ.J)IHIT=IHIT+1
                ENDDO
                IF (IHIT.EQ.0) THEN
                    LISTA(KK)=J
                    KK=KK+1
                ELSE IF (IHIT.GT.1) THEN
                    STOP 'MRQMIN: Improper permutation in LISTA'
                ENDIF
            ENDDO
            IF (KK.NE.(MA+1)) THEN
                STOP 'MRQMIN: Improper permutation in LISTA'
            ENDIF
            ALAMDA=0.001
            CALL MRQCOF(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,
     *                  NCA,CHISQ,FUNCS)
                OCHISQ=CHISQ
            DO J=1,MA
                ATRY(J)=A(J)
            ENDDO
        ENDIF
        DO J=1,MFIT
            DO K=1,MFIT
                COVAR(J,K)=ALPHA(J,K)
            ENDDO
            COVAR(J,J)=ALPHA(J,J)*(1.+ALAMDA)
            DA(J)=BETA(J)
        ENDDO
        CALL GAUSSJ(COVAR,MFIT,NCA,DA,1,1)
        IF(ALAMDA.EQ.0.)THEN
            CALL COVSRT(COVAR,NCA,MA,LISTA,MFIT)
            RETURN
        ENDIF
        DO J=1,MFIT
            ATRY(LISTA(J))=A(LISTA(J))+DA(J)
        ENDDO
        CALL MRQCOF(X1,X2,Y,SIG,NDATA,ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,
     *              CHISQ,FUNCS)
        IF(CHISQ.LT.OCHISQ)THEN
            ALAMDA=0.1*ALAMDA
            OCHISQ=CHISQ
            DO J=1,MFIT
                DO K=1,MFIT
                    ALPHA(J,K)=COVAR(J,K)
                ENDDO
                BETA(J)=DA(J)
                A(LISTA(J))=ATRY(LISTA(J))
            ENDDO
        ELSE
            ALAMDA=10.*ALAMDA
            CHISQ=OCHISQ
        ENDIF
        RETURN
        END
