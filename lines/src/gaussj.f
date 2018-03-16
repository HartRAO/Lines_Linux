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
                          STOP 'in GAUSSJ: Singular matrix'
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
          IF (A(ICOL,ICOL).EQ.0.) STOP 'In GAUSSJ: Singular matrix'
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
