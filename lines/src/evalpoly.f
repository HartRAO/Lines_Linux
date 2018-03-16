*
*  Input arrays :
*        X      contains the independent values
*        F      contains the dependent values
*        DF     contains the errors in the dependent values
*  Output arrays :
*        W      contains the weight for each point
*        A      contains polynomial coefficients

c         evaluate the polynomial over at least 100 evenly spaced points
          XOFF = XMIN
          NEVAL = MAX ((N2-N1+1),100)
          NEVAL = MIN (NEVAL,NS)
          XINTERVAL = (XMAX - XMIN) / (NEVAL - 1)
          DO I = 1, NEVAL
              XVAL(I) = XMIN - XOFF + (I-1) * XINTERVAL
              YVAL(I) = 0.0
              IF (IPOLY .GE. 1) THEN
                  DO J = 1, IPOLY+1
*                     compute using X - Xoff
                      TERM = A(J) * XVAL(I)**(J-1)
                      YVAL(I) = YVAL(I) + TERM
                  END DO
              END IF
*             add on Xoff for plotting 
              XVAL(I) = XVAL(I) + XOFF
          END DO
