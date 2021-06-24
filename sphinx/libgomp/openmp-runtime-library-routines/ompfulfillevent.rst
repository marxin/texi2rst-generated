..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_fulfill_event:

omp_fulfill_event -- Fulfill and destroy an OpenMP event
********************************************************

:samp:`{Description}:`
  Fulfill the event associated with the event handle argument.  Currently, it
  is only used to fulfill events generated by detach clauses on task
  constructs - the effect of fulfilling the event is to allow the task to
  complete.

  The result of calling ``omp_fulfill_event`` with an event handle other
  than that generated by a detach clause is undefined.  Calling it with an
  event handle that has already been fulfilled is also undefined.

:samp:`{C/C++}:`

  ============  =====================================================
  *Prototype*:  ``void omp_fulfill_event(omp_event_handle_t event);``
  ============  =====================================================

:samp:`{Fortran}:`

  ============  =================================================
  *Interface*:  ``subroutine omp_fulfill_event(event)``
                ``integer (kind=omp_event_handle_kind) :: event``
  ============  =================================================

:samp:`{Reference}:`
  `OpenMP specification v5.0 <https://www.openmp.org>`_, Section 3.5.1.

.. -
   OpenMP Environment Variables
   -
