  .. _verify:

``VERIFY`` - Scan a string for characters not a given set
*********************************************************

.. index:: VERIFY

.. index:: string, find missing set

:samp:`{Description}:`
  Verifies that all the characters in :samp:`{STRING}` belong to the set of
  characters in :samp:`{SET}`.

  If :samp:`{BACK}` is either absent or equals ``FALSE``, this function
  returns the position of the leftmost character of :samp:`{STRING}` that is
  not in :samp:`{SET}`. If :samp:`{BACK}` equals ``TRUE``, the rightmost
  position is returned. If all characters of :samp:`{STRING}` are found in
  :samp:`{SET}`, the result is zero.

:samp:`{Standard}:`
  Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = VERIFY(STRING, SET[, BACK [, KIND]])``

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

  .. code-block:: c++

    PROGRAM test_verify
      WRITE(*,*) VERIFY("FORTRAN", "AO")           ! 1, found 'F'
      WRITE(*,*) VERIFY("FORTRAN", "FOO")          ! 3, found 'R'
      WRITE(*,*) VERIFY("FORTRAN", "C++")          ! 1, found 'F'
      WRITE(*,*) VERIFY("FORTRAN", "C++", .TRUE.)  ! 7, found 'N'
      WRITE(*,*) VERIFY("FORTRAN", "FORTRAN")      ! 0' found none
    END PROGRAM

:samp:`{See also}:`
  SCAN, 
  INDEX intrinsic

