  .. _trim:

TRIM --- Remove trailing blank characters of a string
*****************************************************

.. index:: TRIM

.. index:: string, remove trailing whitespace

:samp:`{Description}:`
  Removes trailing blank characters of a string.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = TRIM(STRING)``

:samp:`{Arguments}:`
  ================  ========================================
  :samp:`{STRING}`  Shall be a scalar of type ``CHARACTER``.
  ================  ========================================
  ================  ========================================

:samp:`{Return value}:`
  A scalar of type ``CHARACTER`` which length is that of :samp:`{STRING}`
  less the number of trailing blanks.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_trim
      CHARACTER(len=10), PARAMETER :: s = "GFORTRAN  "
      WRITE(*,*) LEN(s), LEN(TRIM(s))  ! "10 8", with/without trailing blanks
    END PROGRAM

:samp:`{See also}:`
  ADJUSTL, 
  ADJUSTR

