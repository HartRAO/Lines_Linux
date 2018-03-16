******************************
      Real*8 Function ScalProd (iprod, v1, v2)
******************************
*     compute scalar product of two vectors LC / MJG 02/09/94
*
*     other subroutines called:
*     none
*
      IMPLICIT NONE
      integer*4  i, iprod
      real*8     v1(1), v2(1)
*
      ScalProd = 0D0
      do i = 1, iprod
          ScalProd = Scalprod + v1(i) * v2(i)
      end do
      return
      end
****************
