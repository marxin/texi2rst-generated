  .. _acc_wait_all:

``acc_wait_all`` - Waits for completion of all asynchronous operations.
***********************************************************************

Description
  This function waits for the completion of all asynchronous operations.

:samp:`{C/C++}:`
  ========================================  =============================
  *Prototype*:                              ``acc_wait_all(void);``
  ========================================  =============================
  *Prototype (OpenACC 1.0 compatibility)*:  ``acc_async_wait_all(void);``
  ========================================  =============================

:samp:`{Fortran}:`
  ========================================  ===================================
  *Interface*:                              ``subroutine acc_wait_all()``
  ========================================  ===================================
  *Interface (OpenACC 1.0 compatibility)*:  ``subroutine acc_async_wait_all()``
  ========================================  ===================================

:samp:`{Reference}:`
  https://www.openacc.orgOpenACC specification v2.6, section
  3.2.13.

