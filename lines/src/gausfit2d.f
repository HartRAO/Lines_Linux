      PROGRAM GAUSFIT2D
C
C     ********************************************************
C     *   originally called GAUFI                            *
C     *   Fits a 2-dimensional Gaussian to a measured        *
C     *   intensity distribution (at least 5 measured points *
C     *   from Karl Menten <kmenten@mpifr-bonn.mpg.de>       *
C     ********************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER  NMAX
      PARAMETER (NMAX=20) ! must change in GAUSJ as well
      DIMENSION X1(NMAX),X2(NMAX),Y(NMAX),SIG(NMAX),A(4),AOLD(4),
     * RA(4),LISTA(4),COVAR(4,4),ALPHA(4,4),ERR(4)
      CHARACTER*8 FILEIN
      NCA=4             ! array size parameter NCA >= MFIT
      MA=4              ! number of coefficients
C     MFIT = 3          ! to keep HPBW fixed
      MFIT=4            ! to adjust HPBW as well
      CHISQOLD = 30.    ! initialise to check convergence
      ALAMDA=-1.        ! initialise MRQMIN on first pass
      DO I=1,4
          LISTA(I)=I    ! number the parameters
      END DO
C
C     read in data from file, with multiple rows in format:
C     X1 position, X2 position, Y intensity, SIG uncertainty in Y
C     test input data (move to file), remove leading 'C'
C 0 20  1.5 .1
C 0 -20 0.3 .1
C 0 0   2.0 .1
C -20 0 1.3 .1
C 20 0  0.8 .1
C
      CALL READDAT(FILEIN,X1,X2,Y,SIG,NMAX,NDATA)
      PRINT 101, NDATA, FILEIN
 101  FORMAT(' Input for ',I3,' points read from ',A)
      DO I = 1, NDATA
          PRINT *,' X1=',X1(I),' X2=',X2(I),' Y=',Y(I),' Sigma=',SIG(I)
      END DO
C       
C     determine starting values
C
      PRINT *,'Enter guesses for Intensity, X1-offset, X2-offset, HPBW'
      READ *,A(1),A(2),A(3),A(4)
C     test inputs for test data: 2 1 3 40
      PRINT *,'HPBW: fix input value (3) or get best fit (4)?'
      READ *,MFITIN
      IF (MFITIN .EQ. 3 .OR. MFITIN .EQ. 4) MFIT = MFITIN
      IF (MFIT .EQ. 3) PRINT *,'HPBW fixed at ',A(4)
      IF (MFIT .EQ. 4) PRINT *,'HPBW to be fitted'
      DO II=1,MA
          AOLD(II)=A(II)
      END DO
C  
C     do fitting now - see Numerical Recipes by Press et al.
C     first call initialises
C
      CALL MRQMIN(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     *     COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
C
C     iterate the fit
C
      DO I = 1, 50
          DCHI=(CHISQOLD-CHISQ)
          PRINT *,'CHISQOLD = ',CHISQOLD, ' CHISQ =',CHISQ
          DO J=1,MA
              RA(J) = ABS((A(J)-AOLD(J))/A(J))
C              PRINT *,'A(',J,'),AOLD(',J,') = ',A(J),AOLD(J),
C     *                ' RA(',J,') = ',RA(J) 
          END DO
          PRINT *, 'A(1,2,3,4)= ',A(1),A(2),A(3),A(4)    
          R = MAX(RA(1),RA(2),RA(3),RA(4))
          PRINT *,'Rmax = ',R,' DCHI=',DCHI
          IF((R.LT.0.0001) .AND. (CHISQ.LE.CHISQOLD) .AND.
     *       (DCHI.LT.0.001)) 
     *       GO TO 115
          CHISQOLD = CHISQ
          DO K=1,MA
              AOLD(K) = A(K)
          END DO
C         PRINT *,'ITER,CHISQ,LAM = ',I,CHISQ,ALAMDA 
          PRINT 201,(A(IP),IP=1,MA)
 201      FORMAT(/,' INT = ',1PE10.3,
     *             ' XOFF = ',1PE10.3,
     *             ' YOFF = ',1PE10.3,
     *             ' HPBW = ',1PE10.3)
          CALL MRQMIN(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     *         COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
      END DO
C
 115  ALAMDA=0.         ! set to zero for final call to MRQMIN
C     get the covariance and curvature matrices
C
      CALL MRQMIN(X1,X2,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     *     COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
C
C     get the uncertainty in each parameter from its covariance matrix
C
      DO I=1,4
          ERR(I) = SQRT(COVAR(I,I))
      END DO
      PRINT 200,(A(I),ERR(I),I=1,4)
 200  FORMAT(/,' INT.  = ',1PE11.3,' +/- ',1PE11.3,/,
     * ' X1OFF = ',1PE11.3,' +/- ',1PE11.3,/,
     * ' X2OFF = ',1PE11.3,' +/- ',1PE11.3,/,
     * ' HPBW  = ',1PE11.3,' +/- ',1PE11.3,//,
     * ' in whatever units you used',/)
      STOP
      END
c-------------------------------------------------      
      SUBROUTINE READDAT(FILEIN,X1,X2,Y,SIG,NMAX,NDATA)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NMAX, NDATA
      DIMENSION X1(*),X2(*),Y(*),SIG(*)
      CHARACTER*(*) FILEIN
      PRINT *,' NAME OF INPUT FILE?'
      READ *, FILEIN
      OPEN(UNIT=1, FILE=FILEIN, STATUS='OLD',
     * FORM='FORMATTED',BLANK='NULL',ACCESS='SEQUENTIAL', RECL=80)
      PRINT *,'NMAX=',NMAX
      DO 10 I=1, NMAX+1
          IF (I .EQ. NMAX+1) THEN
              PRINT *,'data arrays full, increase NMAX everywhere'
              GO TO 13
          END IF
          READ(1,*,IOSTAT=IERR,ERR=12,END=13) X1(I),X2(I),Y(I),SIG(I)
  10  END DO
  12  IF (IERR .NE. 0) PRINT *,' INPUT FILE ERROR =', IERR
  13  NDATA=I-1
      RETURN
      END
C----------------------------------------------
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
      DIMENSION X1(NDATA),X2(NDATA),Y(NDATA),SIG(NDATA),A(MA),
     *     COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRY(MMAX),
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
      end
