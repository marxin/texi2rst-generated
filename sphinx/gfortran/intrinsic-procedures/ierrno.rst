..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ierrno:

.. index:: IERRNO

.. index:: system, error handling

IERRNO --- Get the last system error number
*******************************************

.. function:: IERRNO()

  Returns the last system error number, as given by the C ``errno``
  variable.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = IERRNO()

  :samp:`{Arguments}:`
    None

  :samp:`{See also}:`
    :ref:`PERROR`

