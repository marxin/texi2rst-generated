..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _getgid:

.. index:: GETGID

.. index:: system, group ID

GETGID --- Group ID function
****************************

.. function:: GETGID()

  Returns the numerical group ID of the current process.

  :return:
    The return value of ``GETGID`` is an ``INTEGER`` of the default
    kind.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = GETGID()

  :samp:`{Example}:`
    See ``GETPID`` for an example.

  :samp:`{See also}:`
    :ref:`GETPID`, 
    :ref:`GETUID`

