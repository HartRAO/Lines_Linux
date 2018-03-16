C     gausfit2dsubs
c-------------------------------------------------      
C     2-D Gaussian function for least squares fit
C
      SUBROUTINE FUNC2DGAUSS(X1,X2,A,Y,DYDA,MA)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(4), DYDA(4)
      S0= A(1)
      X10= A(2)
      X20= A(3)
      TH=A(4)
C      PRINT *,S0,X10,X20,TH
C      PRINT *,X1,X2
      EFX1=-2.773*((X1-X10)/TH)**2
      EFX2=-2.773*((X2-X20)/TH)**2
      EFX=EFX1+EFX2
      Y=S0*EXP(EFX)
C      PRINT *,Y
      DYDA(1)=Y/S0
      DYDA(2)=(-2.*EFX1/(X1-X10))*Y
      DYDA(3)=(-2.*EFX2/(X2-X20))*Y
      DYDA(4)=(-2.*EFX/TH)*Y
C      PRINT *,(DYDA(I),I=1,4)
      RETURN
      END
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
      DIMENSION X1(NDATA),X2(NDATA),YINPUT(NDATA),SIG(NDATA),A(MA),
     *     COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRYINPUT(MMAX),
     *     BETA(MMAX),DA(MMAX)
      SAVE OCHISQ, ATRY, BETA, DA
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
                                PAUSE 'Improper permutation in LISTA'
                        ENDIF
                ENDDO
                IF (KK.NE.(MA+1)) PAUSE 'Improper permutation in LISTA'
                ALAMDA=0.001
                CALL MRQCOF(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,
     *                      NCA,CHISQ,FUNCS)
                OCHISQ=CHISQ
                DO J=1,MA
                        ATRYINPUT(J)=A(J)
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
                ATRYINPUT(LISTA(J))=A(LISTA(J))+DA(J)
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
                        A(LISTA(J))=ATRYINPUT(LISTA(J))
                ENDDO
        ELSE
                ALAMDA=10.*ALAMDA
                CHISQ=OCHISQ
        ENDIF
        RETURN
        END
c---------------------------------------------------------------------
        SUBROUTINE MRQCOF(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,
     * NALP,CHISQ,FUNCS)
C          
C       Set following parameter to largest number of fit parameters
C
      IMPLICIT REAL*8 (A-H,O-Z)
        PARAMETER (MMAX=4)
        DIMENSION X1(NDATA),X2(NDATA),YINPUT(NDATA),SIG(NDATA),
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
                DY=YINPUT(I)-YMOD
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
c---------------------------------------
        SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)
        IMPLICIT REAL*8 (A-H,O-Z)
        PARAMETER (NMAX=50)
        DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
        DO J=1,N
                IPIV(J)=0
        ENDDO
        DO I=1,N
                BIG=0.
                DO J=1,N
                        IF(IPIV(J).NE.1)THEN
                                DO K=1,N
                                        IF (IPIV(K).EQ.0) THEN
                                                IF (ABS(A(J,K)).GE.BIG)THEN
                                                        BIG=ABS(A(J,K))
                                                        IROW=J
                                                        ICOL=K
                                                ENDIF
                                        ELSE IF (IPIV(K).GT.1) THEN
                                                PAUSE 'Singular matrix'
                                        ENDIF
                                ENDDO
                        ENDIF
                ENDDO
                IPIV(ICOL)=IPIV(ICOL)+1
                IF (IROW.NE.ICOL) THEN
                        DO L=1,N
                                DUM=A(IROW,L)
                                A(IROW,L)=A(ICOL,L)
                                A(ICOL,L)=DUM
                        ENDDO
                        DO L=1,M
                                DUM=B(IROW,L)
                                B(IROW,L)=B(ICOL,L)
                                B(ICOL,L)=DUM
                        ENDDO
                ENDIF
                INDXR(I)=IROW
                INDXC(I)=ICOL
                IF (A(ICOL,ICOL).EQ.0.) PAUSE 'Singular matrix.'
                PIVINV=1./A(ICOL,ICOL)
                A(ICOL,ICOL)=1.
                DO L=1,N
                        A(ICOL,L)=A(ICOL,L)*PIVINV
                ENDDO
                DO L=1,M
                        B(ICOL,L)=B(ICOL,L)*PIVINV
                ENDDO
                DO LL=1,N
                        IF(LL.NE.ICOL)THEN
                                DUM=A(LL,ICOL)
                                A(LL,ICOL)=0.
                                DO L=1,N
                                        A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
                                ENDDO
                                DO L=1,M
                                        B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
                                ENDDO
                        ENDIF
                ENDDO
        ENDDO
        DO L=N,1,-1
                IF(INDXR(L).NE.INDXC(L))THEN
                        DO K=1,N
                                DUM=A(K,INDXR(L))
                                A(K,INDXR(L))=A(K,INDXC(L))
                                A(K,INDXC(L))=DUM
                        ENDDO
                ENDIF
        ENDDO
        RETURN
        END
c--------------------------------------------------
        SUBROUTINE COVSRT(COVAR,NCVM,MA,LISTA,MFIT)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION COVAR(NCVM,NCVM),LISTA(MFIT)
        DO J=1,MA-1
                DO I=J+1,MA
                        COVAR(I,J)=0.
                ENDDO
        ENDDO
        DO I=1,MFIT-1
                DO J=I+1,MFIT
                        IF(LISTA(J).GT.LISTA(I)) THEN
                                COVAR(LISTA(J),LISTA(I))=COVAR(I,J)
                        ELSE
                                COVAR(LISTA(I),LISTA(J))=COVAR(I,J)
                        ENDIF
                ENDDO
        ENDDO
        SWAP=COVAR(1,1)
        DO J=1,MA
                COVAR(1,J)=COVAR(J,J)
                COVAR(J,J)=0.
        ENDDO
        COVAR(LISTA(1),LISTA(1))=SWAP
        DO J=2,MFIT
                COVAR(LISTA(J),LISTA(J))=COVAR(1,J)
        ENDDO
        DO J=2,MA
                DO I=1,J-1
                        COVAR(I,J)=COVAR(J,I)
                ENDDO
        ENDDO
        RETURN
        END
c-----------------------------------------
      SUBROUTINE MAXMIN(Y,NDATA,YMAX,YMIN)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION Y(*),BMAX(200),BMIN(200),CMAX(200),CMIN(200)
      BMAX(1)=Y(1)
      BMIN(1)=Y(1)
      DO 10 J=2,NDATA
          BMAX(J)=MAX(Y(J),BMAX(J-1))
          BMIN(J)=MIN(Y(J),BMIN(J-1))
          CMAX(J)=MAX(BMAX(J),BMAX(J-1))
 10       CMIN(J)=MIN(BMIN(J),BMIN(J-1))     
      YMAX=CMAX(NDATA)
      YMIN=CMIN(NDATA)
      RETURN
      END
