  .. _signal:

SIGNAL --- Signal handling subroutine (or function)
***************************************************

.. index:: SIGNAL

.. index:: system, signal handling

:samp:`{Description}:`
  ``SIGNAL(NUMBER, HANDLER [, STATUS])`` causes external subroutine
  :samp:`{HANDLER}` to be executed with a single integer argument when signal
  :samp:`{NUMBER}` occurs.  If :samp:`{HANDLER}` is an integer, it can be used to
  turn off handling of signal :samp:`{NUMBER}` or revert to its default
  action.  See ``signal(2)``.

  If ``SIGNAL`` is called as a subroutine and the :samp:`{STATUS}` argument
  is supplied, it is set to the value returned by ``signal(2)``.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ===========================================
  ``CALL SIGNAL(NUMBER, HANDLER [, STATUS])``
  ===========================================
  ``STATUS = SIGNAL(NUMBER, HANDLER)``
  ===========================================

:samp:`{Arguments}:`
  =================  ====================================================
  :samp:`{NUMBER}`   Shall be a scalar integer, with ``INTENT(IN)``
  =================  ====================================================
  :samp:`{HANDLER}`  Signal handler ( ``INTEGER FUNCTION`` or
                     ``SUBROUTINE`` ) or dummy/global ``INTEGER`` scalar.
                     ``INTEGER``. It is ``INTENT(IN)``.
  :samp:`{STATUS}`   (Optional) :samp:`{STATUS}` shall be a scalar
                     integer. It has ``INTENT(OUT)``.
  =================  ====================================================

  .. TODO: What should the interface of the handler be?  Does it take arguments?

:samp:`{Return value}:`
  The ``SIGNAL`` function returns the value returned by ``signal(2)``.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_signal
      intrinsic signal
      external handler_print

      call signal (12, handler_print)
      call signal (10, 1)

      call sleep (30)
    end program test_signal

