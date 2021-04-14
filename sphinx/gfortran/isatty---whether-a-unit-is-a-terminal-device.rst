  .. _isatty:

``ISATTY`` - Whether a unit is a terminal device.
*************************************************

.. index:: ISATTY

.. index:: system, terminal

:samp:`{Description}:`
  Determine whether a unit is connected to a terminal device.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = ISATTY(UNIT)``

:samp:`{Arguments}:`
  ==============  ==============================
  :samp:`{UNIT}`  Shall be a scalar ``INTEGER``.
  ==============  ==============================
  ==============  ==============================

:samp:`{Return value}:`
  Returns ``.TRUE.`` if the :samp:`{UNIT}` is connected to a terminal 
  device, ``.FALSE.`` otherwise.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_isatty
      INTEGER(kind=1) :: unit
      DO unit = 1, 10
        write(*,*) isatty(unit=unit)
      END DO
    END PROGRAM

:samp:`{See also}:`
  TTYNAM

