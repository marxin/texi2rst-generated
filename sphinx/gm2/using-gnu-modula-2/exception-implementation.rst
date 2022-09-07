.. _exceptions:

Exception implementation
************************

This section describes how exceptions are implemented in GNU Modula-2
and how command line switches affect their behaviour.  The option
:samp:`-fsoft-check-all` enables all software checking of nil
dereferences, division by zero etc.  Additional code is produced to
check these conditions and exception handlers are invoked if the
conditions prevail.

Without :samp:`-fsoft-check-all` these exceptions will be caught by
hardware (assuming the hardware support exists) and a signal handler
is invoked.  The signal handler will in turn ``THROW`` an exception
which will be caught by the appropriate Modula-2 handler.  However the
action of throwing an exception from within a signal handler is
implementation defined (according to the C++ documentation).  For
example on the x86_64 architecture this works whereas on the i686
architecture it does not.  Therefore to ensure portability it is
recommended to use :samp:`-fsoft-check-all`.

:samp:`-fsoft-check-all` can be effectively combined with
:samp:`-O2` to semantically analyse source code for possible runtime
errors at compile time.

