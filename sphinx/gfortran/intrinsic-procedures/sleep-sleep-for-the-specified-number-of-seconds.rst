  .. _sleep:

SLEEP --- Sleep for the specified number of seconds
***************************************************

.. index:: SLEEP

.. index:: delayed execution

:samp:`{Description}:`
  Calling this subroutine causes the process to pause for :samp:`{SECONDS}` seconds.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL SLEEP(SECONDS)``

:samp:`{Arguments}:`
  =================  =========================================
  :samp:`{SECONDS}`  The type shall be of default ``INTEGER``.
  =================  =========================================
  =================  =========================================

:samp:`{Example}:`

  .. code-block:: c++

    program test_sleep
      call sleep(5)
    end

