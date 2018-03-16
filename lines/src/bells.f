**********************
      SUBROUTINE BELLS (DB,X,Y,NP,XA,XB,NPAPR,NSPL,YAPR,IER) 
**********************
*
*     This routine is called by: SPLNFIT
*
*     This routine calls: SPL
*
****  AAPABELLS.  
****  A subroutine for approximation by cubic splines in the  
****  least squares sense.  J. BOK. 
****  Ref:  Comp. Phys. Commun. 16 (1978) 113 
C        Subroutine Bells 
C 
c     Purpose 
C         To calculate an approximation of a discrete function
c         by the sum of the bell spline functions in the least
c         squares sense 
C 
C 
C     Usage 
C         CALL BELLS (X,Y,NP,XA,XB,NPAPR,NSPL,YAPR,IER) 
C 
C        Description of parameters
c           DB    - logical debug printing on/off
C           X     - array of X-values (abscisses) 
c           Y     - array of Y-values (ordinates) 
C           NP    - number of experimental points (X(I),Y(I)) 
C           XA,XB - define the interval (XA,XB) in which the discrete 
C                   function is approximated
C           NPAPR - number of points in which the approximation is
c                   calculated
C           NSPL  - number of spline functions in (XA,XB) 
C           YAPR  - array of approximated values on return,the values 
C                   are calculated for equidistant arguments
C                   XA,XA+XSTEP,,,XB-XSTEP,XB, where
C                   XSTEP = (XB-XA) / (NPAPR-1) 
C           IER   - error parameter 
C                   IER = 1  means that approximation was calculated
C                   IER =-1  means that XA or XB are too far outside
c                            the interval (X(1),X(NP))
C                   IER =-2  means that the number of splines is less 
c                            than 2 or greater than 100 or greater
c                            than number of points in (XA,XB) 
C 
C     Remarks:
C     1)  The values in array X must be in the ascending order
C     2)  XA or XB or both can be outside (X(1),X(NP)), 
C         but their distances from X(1),resp. X(NP) 
C         must be smaller than "spline step" H, H=(XB-XA)/(NSPL-1)
C     3)  As the number of splines in (XA,XB) approaches the
c         number of points in this interval the approximation 
C         function starts oscillate. In the 
C         limit case (the number of points equals to
c         the number of splines+2) the approximation
c         is usually inapplicable 
c     4)  The subroutine is constructed for call up 
c         to 100 splines, if necessary, it is possible
c         to change the nsplmx variable and the 
c         dimension declaration:
C         ARRAY  XPART,R.....N+2
C         ARRAY  A      .....N+5
C         ARRAY  C      .....7*N+22 
C         for subroutine call up to n splines 
c 
C     Subroutines and function required 
c           none
c 
c     Author: J.BOK   INSTITUTE OF PHYSICS OF THE CHARLES UNIVERSITY
C                     KE KARLOVU 5    12116 PRAGUE 2  CZECHOSLOVAKIA
C 
C 
      IMPLICIT  REAL*8 (A-H,O-Z) 
      LOGICAL   DB, INTER, SPL0  ! SPL0 - see spl.f MW 14/11/00
      REAL*8    X(*),Y(*),YAPR(*) 
      REAL*8    R(102),C(722),XPART(102),A(105) 
*
*               common to SPL, BELLS, SPLNFIT & EKDCFRSCT
      COMMON    /WARNING/ SPL0
* 
      IF (DB) PRINT *,'in BELLS'
      IF (DB) PRINT *,'NP=',NP,' NPAPR=',NPAPR,' NSPL=',NSPL,
     &                ' XA=',XA,' XB=',XB
                 
*     print check;
      PRINT *,'NPAPR (ispline): ',NPAPR
      NSPLMX=100
      IF(NSPL.GT.1.AND.NSPL.LT.(NSPLMX+1)) GOTO 10
C 
C     error return in case that number of splines is less than 2 
C     or greater than NSPLMX 
C
*     print check:
      IF (DB) PRINT *,'Getting IER = -2'
      IER=-2
      RETURN
   10 H=(XB-XA)/FLOAT(NSPL-1) 
      IF (DB) PRINT *,'H=',H,' X(1)=',X(1),' X(NP)=',X(NP)
      IF (DB) PRINT *,' Y(1)=',Y(1),' Y(NP)=',Y(NP)
      INTER = (XB-X(NP)).LT.H 
      IF((X(1)-XA).LT.H.AND.INTER) GOTO 11
C 
C     error return in the case that XA or XB are too far outside (X(1),X(NP)
C
*     print check:
      IF (DB) PRINT *,'Getting IER = -1'
      IER=-1
      RETURN
C 
C     complete number of splines including the splines 
C     with the centres at XA-H,XB+H
C
   11 NSP=NSPL+2
*     print check:
      IF (DB) PRINT *,'NSP = ',NSP   
C 
C     generation of the equidistant partition in (XA-H,XB+H) 
C
      DO 12 L=1,NSP 
   12 XPART(L)=XA+H*FLOAT(L-2)
*     print check: (NB! Put 12 back at XPART(L)=... after printing!)
*   12 PRINT *,'XPART(L) = ',XPART(L)   
C 
C     find the first and the last index I such that X(I) lies
C     in (XA,XB) for all I from (IFIRST,ILAST) 
C
      DO 13 I=1,NP
      IF(X(I).GE.XA) GOTO 14
   13 CONTINUE
   14 IFIRST=I
      DO 15 I=IFIRST,NP 
      IF(X(I).GT.XB) GOTO 16
   15 CONTINUE
   16 ILAST=I-1 
C 
C     number of points in (XA,XB)
C
      NPI=ILAST-IFIRST+1
*     print check:
      IF (DB) PRINT *,'XA, XB & NPI: ',XA,XB,NPI    
      IF (DB) PRINT *,'ILAST & IFIRST: ',ILAST,IFIRST
      IF(NSP.LE.NPI) GOTO 17
C 
C     error return in case of too great number of splines
C
*     print check:
      IF (DB) PRINT *,'Getting IER = -2'
      IER=-2
      RETURN
C 
C     set up coefficients of the system of linear equations
C 
C     find indexes I1,I2 such that for I from (I1,I2)
C     the value SP1 is non-zero
C
   17 LLAST=7*NSP-3 
      DO 18 L=1,LLAST 
   18 C(L)=0. 
      DO 19 L=1,NSP 
   19 R(L)=0. 
      DO 30 M=0,3 
      I1=IFIRST 
      JM=NSP-M
      DO 29 J=1,JM
      IND=7*J-3+M 
      IF(J.GT.3) GOTO 20
      I1=IFIRST 
      GOTO 23 
   20 DO 21 IP=I1,ILAST 
      IF(X(IP).GT.XPART(J-2)) GOTO 22 
   21 CONTINUE
   22 I1=IP 
   23 IF(J.LT.(NSP-2)) GOTO 24
      I2=ILAST
      GOTO 27 
   24 DO 25 IP=I1,ILAST 
      IF(X(IP).GT.XPART(J+2)) GOTO 26 
   25 CONTINUE
   26 I2=IP-1 
*     print check:
      IF (DB) PRINT *,'Got up to 26...'   
C 
C     computation of the coefficients and the right-hand side
C     of the system of linear equations
C
   27 DO 29 I=I1,I2 
      SP1=SPL(X(I),XPART(J),H)
      IF(M.NE.0) GOTO 28
      SP2=SP1 
      R(J)=R(J)+Y(I)*SP1
*     print check:
      IF (DB) PRINT *,'R(J) = ',R(J)      
      GOTO 29 
   28 SP2=SPL(X(I),XPART(J+M),H)
*     print check:
      IF (DB) PRINT *,'SP2,X(I),XPART(J+M),H,J,M: ',
     & SP2,X(I),XPART(J+M),H,J,M 
*     print check:   
*     PRINT *,'SP1 & SP2: ',SP1,SP2
   29 C(IND)=C(IND)+SP1*SP2 
*     print check:  (NB! Put 29 back at C(IND)=... after printing)
*     PRINT *,'C(IND) = ',C(IND)
   30 CONTINUE
*     print check:
      IF (DB) PRINT *,'Got up to 30...'
C 
C     elements of the matrix under the main diagonal
C 
      LP=NSP-1
      DO 31 L=1,LP
      LL=7*L
      C(LL+3)=C(LL-2) 
      C(LL+9)=C(LL-1) 
   31 C(LL+15)=C(LL)
*     print check:
      IF (DB) PRINT *,'Got up to 31 - C matrix'   
C 
C     solving of the system of linear equations
C
      DO 33 L=1,LP
      DO 33 M=1,3 
      II=7*L-3
      JJ=II+6*M 
      FF=-C(JJ)/C(II) 
      DO 32 MM=1,3
      RINTER = FF*C(II+MM)
*     print check:
*     PRINT *,'RINTER, FF, C(II+MM): ',RINTER,FF,C(II+MM)      
   32 C(JJ+MM)=C(JJ+MM)+RINTER
      RINTER = FF*R(L)
   33 R(L+M)=R(L+M)+RINTER
*     print check:
*     PRINT *,'@33: R(L+M) & RINTER: ',R(L+M),RINTER
      LP=NSP+3
      DO 34 L=1,LP
   34 A(L)=0. 
      DO 35 LL=1,NSP
      L=NSP+1-LL
      JJ=7*L
   35 A(L)=(R(L)-C(JJ)*A(L+3)-C(JJ-1)*A(L+2)-C(JJ-2)*A(L+1))/C(JJ-3)
*     print check:
*     PRINT *,'@ 35: A(L), R(L) & C(JJ): ',A(L),R(L),C(JJ)
C 
C     generation of the approximative values 
C
      XSTEP=(XB-XA)/FLOAT(NPAPR-1)
*     print check:
      IF (DB) PRINT *,'XB, XA, NPAPR & XSTEP = ',XB,XA,NPAPR,XSTEP
      DO 36 I=1,NPAPR 
      XX=XA+XSTEP*FLOAT(I-1)
      L1=(XX-XPART(1))/H
      L2=L1+3 
      IF(L1.EQ.0) L1=1
      IF(L2.GT.NSP) L2=NSP
      YAPR(I)=0.
*     print check:
*     PRINT *,'YAPR(I) = ',YAPR(I)
*     PRINT *,'L1 & L2: ',L1,L2
C 
C     the value SPL(X,XL,H) is non-zero only for L from (L1,L2)
C
      DO 36 L=L1,L2 
      RINTER = H*FLOAT(L-1) 
*     print check:
*     PRINT *,'RINTER & H: ',RINTER,H
      XL=XPART(1)+RINTER
*     print check:
*     PRINT *,'XL,RINTER & H: ',XL,RINTER,H
   36 YAPR(I)=YAPR(I)+A(L)*SPL(XX,XL,H) 
*     print check: (NB! Put 36 back at YAPR(I)... after printing)
*  36 PRINT *,'YAPR(I), A(L) & SPL(..) ',YAPR(I),A(L),SPL(XX,XL,H)
      IF (DB) PRINT *,'Got to 36...'    
C 
C     normal return
C
*     print check:
*     PRINT *,'IER = ',IER
      IER=1 
*     print check:
*     PRINT *,'IER = ',IER
*     PRINT *,'NPAPR = ',NPAPR
      IF (DB) PRINT *,'At end of Bells...'
*     DO I = 1,NPAPR
*         PRINT *,'Yspline: ',YAPR(I)
*     END DO
      RETURN
      END 
