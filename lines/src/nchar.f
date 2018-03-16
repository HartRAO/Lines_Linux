********************
      FUNCTION NCHAR (array)
********************
*     find number of non-blank characters in character array
*     nchar is set = 1 if array is blank
      character array*(*)
*
*     find length of the array
      nchar = len (array)
      do while (array(nchar:nchar) .eq. ' ' .and. nchar .gt. 1)
          nchar = nchar - 1
      end do
      return
      end
*********

