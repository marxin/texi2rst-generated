  .. _perror:

PERROR - Print system error message
***********************************

.. index:: PERROR

.. index:: system, error handling

:samp:`{Description}:`
  Prints (on the C ``stderr`` stream) a newline-terminated error
  message corresponding to the last system error. This is prefixed by
  :samp:`{STRING}`, a colon and a space. See ``perror(3)``.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL PERROR(STRING)``

:samp:`{Arguments}:`
  ================  =========================================
  :samp:`{STRING}`  A scalar of type ``CHARACTER`` and of the
                    default kind.
  ================  =========================================
  ================  =========================================

:samp:`{See also}:`
  IERRNO

