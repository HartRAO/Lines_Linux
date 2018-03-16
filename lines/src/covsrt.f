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
