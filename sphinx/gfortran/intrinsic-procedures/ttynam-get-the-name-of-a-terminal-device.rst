  .. _ttynam:

TTYNAM --- Get the name of a terminal device.
*********************************************

.. index:: TTYNAM

.. index:: system, terminal

:samp:`{Description}:`
  Get the name of a terminal device. For more information, 
  see ``ttyname(3)``.

  This intrinsic is provided in both subroutine and function forms; 
  however, only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ===========================
  ``CALL TTYNAM(UNIT, NAME)``
  ``NAME = TTYNAM(UNIT)``
  ===========================

:samp:`{Arguments}:`
  ==============  ===============================
  :samp:`{UNIT}`  Shall be a scalar ``INTEGER``.
  :samp:`{NAME}`  Shall be of type ``CHARACTER``.
  ==============  ===============================

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_ttynam
      INTEGER :: unit
      DO unit = 1, 10
        IF (isatty(unit=unit)) write(*,*) ttynam(unit)
      END DO
    END PROGRAM

:samp:`{See also}:`
  ISATTY

