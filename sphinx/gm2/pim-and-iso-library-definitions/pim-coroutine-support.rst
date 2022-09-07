.. _pim-coroutine-support:

PIM coroutine support
*********************

.. README.texi describes the PIM coroutine libraries.
    c

This directory contains a PIM ``SYSTEM`` containing the
``PROCESS`` primitives built on top of GNU Pthreads.

The justification for this approach is that it provides a
``SYSTEM`` compatible with Programming in Modula-2 [234] and the
Logitech 3.0 compiler. It also allows higher level executives to be
ported onto GM2 with little effort. The disadvantage with this
approach is that ``IOTRANSFER`` is not
preemptive. ``IOTRANSFER`` will only context switch when a call to
``LISTEN`` is made or a call to ``SYSTEM.TurnInterrupts`` is
made.

In practice this limitation can be tolerated as long as processes
perform IO at some point (or wait for a timer interrupt) or call
``SYSTEM.TurnInterrupts``. But nevertheless a ``LOOP``
``END`` will starve all other processes. However the great
advantage is that GNU Modula-2 can offer users the ability to use
``IOTRANSFER``, ``TRANSFER``, ``NEWPROCESS`` in user space,
on a multi-user operating system and across a range of platforms.

The GNU Modula-2 ``SYSTEM`` works by utilizing the user context
switching mechanism provided by GNU Pthreads. ``NEWPROCESS``
creates a new context, ``TRANSFER`` switches
contexts. ``IOTRANSFER`` is more complex. There is a support module
``SysVec`` which provides pseudo interrupt vectors. These can be
created from input/output file descriptors or timer events
``timeval``. This vector is then passed to ``IOTRANSFER`` which
keeps track of which file descriptors and timevals are active. When a
call to ``TurnInterrupts`` or ``LISTEN`` is made the sub system
calls ``pth_select`` and tests for any ready file descriptor or
timeout. A ready file descriptor or timeout will ultimately cause the
backwards ``TRANSFER`` inside ``IOTRANSFER`` to take effect.

See the :samp:`gm2/examples/executive` directory for an executive and
timerhandler module which provide higher level process creation,
synchronisation and interrupt handling routines. These libraries have
been tested with the examples shown in :samp:`gm2/examples/executive`
and :samp:`gm2/gm2-libs-coroutines`.

Users of these libraries and the libraries in
:samp:`gm2/examples/executive` must link their application against the
GNU Pthread library (typically by using ``-lpth``).

.. toctree::
  :maxdepth: 2

  pim-coroutine-support/gm2-libs-coroutines-debug
  pim-coroutine-support/gm2-libs-coroutines-executive
  pim-coroutine-support/gm2-libs-coroutines-keyboardleds
  pim-coroutine-support/gm2-libs-coroutines-system
  pim-coroutine-support/gm2-libs-coroutines-timerhandler

