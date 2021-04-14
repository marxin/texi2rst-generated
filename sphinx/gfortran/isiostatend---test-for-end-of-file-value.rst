  .. _is_iostat_end:

``IS_IOSTAT_END`` - Test for end-of-file value
**********************************************

.. index:: IS_IOSTAT_END

.. index:: IOSTAT, end of file

:samp:`{Description}:`
  ``IS_IOSTAT_END`` tests whether an variable has the value of the I/O
  status 'end of file'. The function is equivalent to comparing the variable
  with the ``IOSTAT_END`` parameter of the intrinsic module
  ``ISO_FORTRAN_ENV``.

:samp:`{Standard}:`
  Fortran 2003 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = IS_IOSTAT_END(I)``

:samp:`{Arguments}:`
  ===========  =================================
  :samp:`{I}`  Shall be of the type ``INTEGER``.
  ===========  =================================
  ===========  =================================

:samp:`{Return value}:`
  Returns a ``LOGICAL`` of the default kind, which ``.TRUE.`` if
  :samp:`{I}` has the value which indicates an end of file condition for
  ``IOSTAT=`` specifiers, and is ``.FALSE.`` otherwise.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM iostat
      IMPLICIT NONE
      INTEGER :: stat, i
      OPEN(88, FILE='test.dat')
      READ(88, *, IOSTAT=stat) i
      IF(IS_IOSTAT_END(stat)) STOP 'END OF FILE'
    END PROGRAM

