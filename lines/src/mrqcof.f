c---------------------------------------------------------------------
        SUBROUTINE MRQCOF(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,
     * NALP,CHISQ,FUNCS)
C          
C       Set following parameter to largest number of fit parameters
C
      IMPLICIT REAL*8 (A-H,O-Z)
        PARAMETER (MMAX=4)
        DIMENSION X1(NDATA),X2(NDATA),Y(NDATA),SIG(NDATA),
     * ALPHA(NALP,NALP),BETA(MA), DYDA(MMAX),LISTA(MFIT),A(MA)
        DO J=1,MFIT
                DO K=1,J
                        ALPHA(J,K)=0.
                ENDDO
                BETA(J)=0.
        ENDDO
        CHISQ=0.
        DO I=1,NDATA
                CALL FUNC2DGAUSS(X1(I),X2(I),A,YMOD,DYDA,MA)
                SIG2I=1./(SIG(I)*SIG(I))
                DY=Y(I)-YMOD
                DO J=1,MFIT
                        WT=DYDA(LISTA(J))*SIG2I
                        DO K=1,J
                                ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(LISTA(K))
                        ENDDO
                        BETA(J)=BETA(J)+DY*WT
                ENDDO
                CHISQ=CHISQ+DY*DY*SIG2I
        ENDDO
        DO J=2,MFIT
                DO K=1,J-1
                        ALPHA(K,J)=ALPHA(J,K)
                ENDDO
        ENDDO
        RETURN
        END
