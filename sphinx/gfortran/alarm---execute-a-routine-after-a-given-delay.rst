  .. _alarm:

``ALARM`` - Execute a routine after a given delay
*************************************************

.. index:: ALARM

.. index:: delayed execution

:samp:`{Description}:`
  ``ALARM(SECONDS, HANDLER [, STATUS])`` causes external subroutine :samp:`{HANDLER}`
  to be executed after a delay of :samp:`{SECONDS}` by using ``alarm(2)`` to
  set up a signal and ``signal(2)`` to catch it. If :samp:`{STATUS}` is
  supplied, it will be returned with the number of seconds remaining until
  any previously scheduled alarm was due to be delivered, or zero if there
  was no previously scheduled alarm.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL ALARM(SECONDS, HANDLER [, STATUS])``

:samp:`{Arguments}:`
  =================  =================================================================
  :samp:`{SECONDS}`  The type of the argument shall be a scalar
                     ``INTEGER``. It is ``INTENT(IN)``.
  =================  =================================================================
  :samp:`{HANDLER}`  Signal handler (``INTEGER FUNCTION`` or
                     ``SUBROUTINE``) or dummy/global ``INTEGER`` scalar. The scalar 
                     values may be either ``SIG_IGN=1`` to ignore the alarm generated 
                     or ``SIG_DFL=0`` to set the default action. It is ``INTENT(IN)``.
  :samp:`{STATUS}`   (Optional) :samp:`{STATUS}` shall be a scalar
                     variable of the default ``INTEGER`` kind. It is ``INTENT(OUT)``.
  =================  =================================================================

:samp:`{Example}:`

  .. code-block:: c++

    program test_alarm
      external handler_print
      integer i
      call alarm (3, handler_print, i)
      print *, i
      call sleep(10)
    end program test_alarm

  This will cause the external routine :samp:`{handler_print}` to be called
  after 3 seconds.

