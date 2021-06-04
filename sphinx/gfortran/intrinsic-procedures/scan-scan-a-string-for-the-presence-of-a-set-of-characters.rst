  .. _scan:

SCAN --- Scan a string for the presence of a set of characters
**************************************************************

.. index:: SCAN

.. index:: string, find subset

:samp:`{Description}:`
  Scans a :samp:`{STRING}` for any of the characters in a :samp:`{SET}` 
  of characters.

  If :samp:`{BACK}` is either absent or equals ``FALSE``, this function
  returns the position of the leftmost character of :samp:`{STRING}` that is
  in :samp:`{SET}`. If :samp:`{BACK}` equals ``TRUE``, the rightmost position
  is returned. If no character of :samp:`{SET}` is found in :samp:`{STRING}`, the 
  result is zero.

:samp:`{Standard}:`
  Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = SCAN(STRING, SET[, BACK [, KIND]])``

:samp:`{Arguments}:`
  ================  =======================================================
  :samp:`{STRING}`  Shall be of type ``CHARACTER``.
  ================  =======================================================
  :samp:`{SET}`     Shall be of type ``CHARACTER``.
  :samp:`{BACK}`    (Optional) shall be of type ``LOGICAL``.
  :samp:`{KIND}`    (Optional) An ``INTEGER`` initialization
                    expression indicating the kind parameter of the result.
  ================  =======================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
  :samp:`{KIND}` is absent, the return value is of default integer kind.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_scan
      WRITE(*,*) SCAN("FORTRAN", "AO")          ! 2, found 'O'
      WRITE(*,*) SCAN("FORTRAN", "AO", .TRUE.)  ! 6, found 'A'
      WRITE(*,*) SCAN("FORTRAN", "C++")         ! 0, found none
    END PROGRAM

:samp:`{See also}:`
  INDEX intrinsic, 
  VERIFY

