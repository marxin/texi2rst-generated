.. _gm2-libs-coroutines-timerhandler:

gm2-libs-coroutines/TimerHandler
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE TimerHandler ;

  (* It also provides the Executive with a basic round robin scheduler.  *)

  EXPORT QUALIFIED TicksPerSecond, GetTicks,
                   EVENT,
                   Sleep, ArmEvent, WaitOn, Cancel, ReArmEvent ;

  CONST
  TicksPerSecond  (const)
     TicksPerSecond =   25 ;  (* Number of ticks per second.  *)

  TYPE
  EVENT (type)
     EVENT ;

  (*
     GetTicks - returns the number of ticks since boottime.
  *)

  GetTicks
  PROCEDURE GetTicks () : CARDINAL ;

  (*
     Sleep - suspends the current process for a time, t.
             The time is measured in ticks.
  *)

  Sleep
  PROCEDURE Sleep (t: CARDINAL) ;

  (*
     ArmEvent - initializes an event, e, to occur at time, t.
                The time, t, is measured in ticks.
                The event is NOT placed onto the event queue.
  *)

  ArmEvent
  PROCEDURE ArmEvent (t: CARDINAL) : EVENT ;

  (*
     WaitOn - places event, e, onto the event queue and then the calling
              process suspends. It is resumed up by either the event
              expiring or the event, e, being cancelled.
              TRUE is returned if the event was cancelled
              FALSE is returned if the event expires.
              The event, e, is always assigned to NIL when the function
              finishes.
  *)

  WaitOn
  PROCEDURE WaitOn (VAR e: EVENT) : BOOLEAN ;

  (*
     Cancel - cancels the event, e, on the event queue and makes
              the appropriate process runnable again.
              TRUE is returned if the event was cancelled and
              FALSE is returned is the event was not found or
                    no process was waiting on this event.
  *)

  Cancel
  PROCEDURE Cancel (e: EVENT) : BOOLEAN ;

  (*
     ReArmEvent - removes an event, e, from the event queue. A new time
                  is given to this event and it is then re-inserted onto the
                  event queue in the correct place.
                  TRUE is returned if this occurred
                  FALSE is returned if the event was not found.
  *)

  ReArmEvent
  PROCEDURE ReArmEvent (e: EVENT; t: CARDINAL) : BOOLEAN ;

  END TimerHandler.

.. -

