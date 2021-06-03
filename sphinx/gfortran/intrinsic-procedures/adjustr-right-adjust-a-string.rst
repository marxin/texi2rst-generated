  .. _adjustr:

ADJUSTR --- Right adjust a string 
**********************************

.. index:: ADJUSTR

.. index:: string, adjust right

.. index:: adjust string

:samp:`{Description}:`
  ``ADJUSTR(STRING)`` will right adjust a string by removing trailing spaces.
  Spaces are inserted at the start of the string as needed.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ADJUSTR(STRING)``

:samp:`{Arguments}:`
  =============  ================================
  :samp:`{STR}`  The type shall be ``CHARACTER``.
  =============  ================================
  =============  ================================

:samp:`{Return value}:`
  The return value is of type ``CHARACTER`` and of the same kind as
  :samp:`{STRING}` where trailing spaces are removed and the same number of
  spaces are inserted at the start of :samp:`{STRING}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_adjustr
      character(len=20) :: str = 'gfortran'
      str = adjustr(str)
      print *, str
    end program test_adjustr

:samp:`{See also}:`
  ADJUSTL, 
  TRIM

