.. _gm2-libs-coroutines-executive:

gm2-libs-coroutines/Executive
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Executive ;

  EXPORT QUALIFIED SEMAPHORE, DESCRIPTOR,
                   InitProcess, KillProcess, Resume, Suspend, InitSemaphore,
                   Wait, Signal, WaitForIO, Ps, GetCurrentProcess,
                   RotateRunQueue, ProcessName, DebugProcess ;

  TYPE
  SEMAPHORE (type)
     SEMAPHORE ;         (* defines Dijkstra's semaphores *)
  DESCRIPTOR (type)
     DESCRIPTOR ;        (* handle onto a process         *)

  (*
     InitProcess - initializes a process which is held in the suspended
                   state. When the process is resumed it will start executing
                   procedure, p. The process has a maximum stack size of,
                   StackSize, bytes and its textual name is, Name.
                   The StackSize should be at least 5000 bytes.
  *)

  InitProcess
  PROCEDURE InitProcess (p: PROC; StackSize: CARDINAL;
                         Name: ARRAY OF CHAR) : DESCRIPTOR ;

  (*
     KillProcess - kills the current process. Notice that if InitProcess
                   is called again, it might reuse the DESCRIPTOR of the
                   killed process. It is the responsibility of the caller
                   to ensure all other processes understand this process
                   is different.
  *)

  KillProcess
  PROCEDURE KillProcess ;

  (*
     Resume - resumes a suspended process. If all is successful then the process, p,
              is returned. If it fails then NIL is returned.
  *)

  Resume
  PROCEDURE Resume (d: DESCRIPTOR) : DESCRIPTOR ;

  (*
     Suspend - suspend the calling process.
               The process can only continue running if another process
               Resumes it.
  *)

  Suspend
  PROCEDURE Suspend ;

  (*
     InitSemaphore - creates a semaphore whose initial value is, v, and
                     whose name is, Name.
  *)

  InitSemaphore
  PROCEDURE InitSemaphore (v: CARDINAL; Name: ARRAY OF CHAR) : SEMAPHORE ;

  (*
     Wait - performs dijkstra's P operation on a semaphore.
            A process which calls this procedure will
            wait until the value of the semaphore is > 0
            and then it will decrement this value.
  *)

  Wait
  PROCEDURE Wait (s: SEMAPHORE) ;

  (*
     Signal - performs dijkstra's V operation on a semaphore.
              A process which calls the procedure will increment
              the semaphores value.
  *)

  Signal
  PROCEDURE Signal (s: SEMAPHORE) ;

  (*
     WaitForIO - waits for an interrupt to occur on vector, VectorNo.
  *)

  WaitForIO
  PROCEDURE WaitForIO (VectorNo: CARDINAL) ;

  (*
     Ps - displays a process list together with process status.
  *)

  Ps
  PROCEDURE Ps ;

  (*
     GetCurrentProcess - returns the descriptor of the current running
                         process.
  *)

  GetCurrentProcess
  PROCEDURE GetCurrentProcess () : DESCRIPTOR ;

  (*
     RotateRunQueue - rotates the process run queue.
                      It does not call the scheduler.
  *)

  RotateRunQueue
  PROCEDURE RotateRunQueue ;

  (*
     ProcessName - displays the name of process, d, through
                   DebugString.
  *)

  ProcessName
  PROCEDURE ProcessName (d: DESCRIPTOR) ;

  (*
     DebugProcess - gdb debug handle to enable users to debug deadlocked
                    semaphore processes.
  *)

  DebugProcess
  PROCEDURE DebugProcess (d: DESCRIPTOR) ;

  END Executive.

