*********************
      SUBROUTINE FDEL (DEL,FF)
*********************
*  Part of multiple gaussian fitting routines, called by LGFIT2
*
*  calculates the weighted chi-squared (FF)
*  and its gradients (DEL)
*  in the direction of all adjustable parameters (P),
*  on the data (X,Y)
*  the gradient calculation is analytic.
*  precautions are taken to forestall division by very small numbers
*
*     other subroutines called:
*     none
*
      IMPLICIT REAL*8 (A-H,O-Z)
*
      INTEGER*4 MAXLINESL            ! max number of gaussians to fit
      PARAMETER (MAXLINESL = 50)    ! for local arrays    
*
      REAL*8    DEL(MAXLINESL*3+1)   ! output to DELT1 or DELT
*     local working arrays
      REAL*8    GR(MAXLINESL*3+1)
      REAL*8    PLOC(MAXLINESL*3+1)
      REAL*8    SET(MAXLINESL*3+1)
      REAL*8    SL(MAXLINESL*3+1)
*
      INCLUDE 'lines.inc'
*
      DATA  TEST / 1.0D2 /
*
      IF (DB) PRINT *,'in FDEL'
*
*     presetting variables
*
         NPA = NPAR + 1
         DO 5 J = 1,NPA,1
            PLOC(J) = P(J)
            GR(J) = 0.0D0
            SET(J) = 0.0D0
    5       SL(J) = 0.0D0
         F = 0.0D0
         BG = P(NPA)
         TEST = -TEST
*
*     ff and all del, telescoped and optimized - gaussians
*
         IF (DB) PRINT *,'loop on NP = ',NP,' NPAR = ',NPAR
         DO 200 I = 1,NP,1
            PHI = BG
            DO 110 J = 1,NPAR,3
               IF (DABS(X(I) - PLOC(J)) .LT. 1.0D-11)
     *            PLOC(J) = PLOC(J) + TEST * 1.0D-10
               SS = (X(I) - PLOC(J)) / PLOC(J+2)
               SL(J) = SS
               SET(J) = DEXP(-2.7726D0 * SS * SS)
  110          PHI = PHI + PLOC(J+1) * SET(J)
            S = PHI - Y(I)
            F = F + S * S
            Q = 2.0D0 * S
            GR(NPA) = GR(NPA) + Q
            DO 115 J = 1,NPAR,3
               SAVE = PLOC(J+1) * 5.5452D0 * SET(J) * SL(J) * Q 
     &                / PLOC(J+2)
               GR(J) = GR(J) + SAVE
               GR(J+1) = GR(J+1) + SET(J) * Q
  115          GR(J+2) = GR(J+2) + SL(J) * SAVE
  200       CONTINUE
*
*     copy out of local storage
*
  202    FF = F
         DO 30 J = 1,NPA,1
   30       DEL(J) = GR(J)
         RETURN
         END
************
