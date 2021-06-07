.. _getgid:

GETGID --- Group ID function
****************************

.. index:: GETGID

.. index:: system, group ID

.. function:: GETGID

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
    GETPID, 
    GETUID

