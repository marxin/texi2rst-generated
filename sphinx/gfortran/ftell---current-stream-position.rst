  .. _ftell:

FTELL - Current stream position
*******************************

.. index:: FTELL

.. index:: file operation, position

:samp:`{Description}:`
  Retrieves the current position within an open file.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ============================
  ``CALL FTELL(UNIT, OFFSET)``
  ============================
  ``OFFSET = FTELL(UNIT)``
  ============================

:samp:`{Arguments}:`
  ================  ==========================
  :samp:`{OFFSET}`  Shall of type ``INTEGER``.
  ================  ==========================
  :samp:`{UNIT}`    Shall of type ``INTEGER``.
  ================  ==========================

:samp:`{Return value}:`
  In either syntax, :samp:`{OFFSET}` is set to the current offset of unit
  number :samp:`{UNIT}`, or to -1 if the unit is not currently open.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_ftell
      INTEGER :: i
      OPEN(10, FILE="temp.dat")
      CALL ftell(10,i)
      WRITE(*,*) i
    END PROGRAM

:samp:`{See also}:`
  FSEEK

