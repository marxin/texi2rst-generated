.. _ierrno:

IERRNO --- Get the last system error number
*******************************************

.. index:: IERRNO

.. index:: system, error handling

.. function:: IERRNO

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
    ``RESULT = IERRNO()``

  :samp:`{Arguments}:`
    None

  :samp:`{See also}:`
    PERROR

