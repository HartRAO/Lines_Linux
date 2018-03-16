***********************
      SUBROUTINE LOCASE (ARRAY)
***********************
*     set characters to lower case in character array
*
      character array*(*)
*
      IVALA = ICHAR ('A')
      IVALZ = ICHAR ('Z')
      IDIFF = ICHAR ('a') - IVALA
*     for non-blank characters in the array
      DO I = 1, NCHAR(ARRAY)
          ival = ichar(array(i:i))
          if (ival .ge. IVALA .and. ival .le. IVALZ) then 
              ARRAY(I:I) = CHAR (IVAL + IDIFF)
          end if
      end do
      return
      end
*********

