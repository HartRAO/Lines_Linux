**********************
      SUBROUTINE PBOLA (X,Y,A,B,C)
**********************
*     Find coefficients A,B,C of polynomial Y=AX**2+BX+C
*     inputs  X       array of 3 X values
*             Y       array of 3 corresponding Y values
*     outputs A,B,C   coefficients of 2nd order polynomial
*
*     This routine is called by: SPLNFIT
*
      REAL*8 X(3), Y(3), A, B, C
*
      A = (X(1)*Y(3)-X(3)*Y(1)+X(3)*Y(2)-X(2)*Y(3)+X(2)*Y(1)-X(1)*Y(2))
     &    / (X(1)*X(3)**2-X(1)**2*X(3)+X(2)**2*X(3)-X(2)*X(3)**2
     &       +X(1)**2*X(2)-X(1)*X(2)**2)
      B = (Y(1)-Y(2)-A*(X(1)**2-X(2)**2)) / (X(1)-X(2))
      C = Y(1)-A*X(1)**2-B*X(1)
      RETURN
      END
