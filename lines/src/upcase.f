***********************
      SUBROUTINE UPCASE (ARRAY)
***********************
*     set characters to upper case in character array of length n
*
      character array*(*)
*
      IVALA = ICHAR ('a')
      IVALZ = ICHAR ('z')
      IDIFF = ICHAR ('A') - IVALA
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

