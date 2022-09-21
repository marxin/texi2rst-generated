..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ble:

.. index:: BLE

.. index:: bitwise comparison

BLE --- Bitwise less than or equal to
*************************************

.. function:: BLE(I, J)

  Determines whether an integral is a bitwise less than or equal to
  another.

  :param I:
    Shall be of ``INTEGER`` type.

  :param J:
    Shall be of ``INTEGER`` type, and of the same kind
    as :samp:`{I}`.

  :return:
    The return value is of type ``LOGICAL`` and of the default kind.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = BLE(I, J)

  :samp:`{See also}:`
    :ref:`BGT`, 
    :ref:`BGE`, 
    :ref:`BLT`

