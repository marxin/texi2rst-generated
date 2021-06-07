.. _getuid:

GETUID --- User ID function
***************************

.. index:: GETUID

.. index:: system, user ID

.. index:: user id

.. function:: GETUID

  Returns the numerical user ID of the current process.

  :return:
    The return value of ``GETUID`` is an ``INTEGER`` of the default
    kind.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = GETUID()

  :samp:`{Example}:`
    See ``GETPID`` for an example.

  :samp:`{See also}:`
    GETPID, 
    GETLOG

