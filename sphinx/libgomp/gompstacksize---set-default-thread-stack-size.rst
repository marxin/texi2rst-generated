  .. _gomp_stacksize:

:envvar:`GOMP_STACKSIZE` - Set default thread stack size
********************************************************

.. index:: Environment Variable

.. index:: Implementation specific setting

:samp:`{Description}:`
  Set the default thread stack size in kilobytes.  This is different from
  ``pthread_attr_setstacksize`` which gets the number of bytes as an 
  argument.  If the stack size cannot be set due to system constraints, an 
  error is reported and the initial stack size is left unchanged.  If undefined,
  the stack size is system dependent.

:samp:`{See also}:`
  OMP_STACKSIZE

:samp:`{Reference}: `
  https://gcc.gnu.org/ml/gcc-patches/2006-06/msg00493.htmlGCC Patches Mailinglist, 
  https://gcc.gnu.org/ml/gcc-patches/2006-06/msg00496.htmlGCC Patches Mailinglist

