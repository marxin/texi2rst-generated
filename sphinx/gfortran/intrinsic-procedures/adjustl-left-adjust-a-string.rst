  .. _adjustl:

ADJUSTL --- Left adjust a string 
*********************************

.. index:: ADJUSTL

.. index:: string, adjust left

.. index:: adjust string

:samp:`{Description}:`
  ``ADJUSTL(STRING)`` will left adjust a string by removing leading spaces.
  Spaces are inserted at the end of the string as needed.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ADJUSTL(STRING)``

:samp:`{Arguments}:`
  ================  ================================
  :samp:`{STRING}`  The type shall be ``CHARACTER``.
  ================  ================================
  ================  ================================

:samp:`{Return value}:`
  The return value is of type ``CHARACTER`` and of the same kind as
  :samp:`{STRING}` where leading spaces are removed and the same number of
  spaces are inserted on the end of :samp:`{STRING}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_adjustl
      character(len=20) :: str = '   gfortran'
      str = adjustl(str)
      print *, str
    end program test_adjustl

:samp:`{See also}:`
  ADJUSTR, 
  TRIM

