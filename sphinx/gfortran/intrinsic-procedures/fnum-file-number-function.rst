  .. _fnum:

FNUM --- File number function
*****************************

.. index:: FNUM

.. index:: file operation, file number

:samp:`{Description}:`
  ``FNUM(UNIT)`` returns the POSIX file descriptor number corresponding to the
  open Fortran I/O unit ``UNIT``.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = FNUM(UNIT)``

:samp:`{Arguments}:`
  ==============  ==============================
  :samp:`{UNIT}`  The type shall be ``INTEGER``.
  ==============  ==============================
  ==============  ==============================

:samp:`{Return value}:`
  The return value is of type ``INTEGER``

:samp:`{Example}:`

  .. code-block:: fortran

    program test_fnum
      integer :: i
      open (unit=10, status = "scratch")
      i = fnum(10)
      print *, i
      close (10)
    end program test_fnum

