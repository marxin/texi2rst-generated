  .. _allocated:

ALLOCATED - Status of an allocatable entity
*******************************************

.. index:: ALLOCATED

.. index:: allocation, status

:samp:`{Description}:`
  ``ALLOCATED(ARRAY)`` and ``ALLOCATED(SCALAR)`` check the allocation
  status of :samp:`{ARRAY}` and :samp:`{SCALAR}`, respectively.

:samp:`{Standard}:`
  Fortran 90 and later.  Note, the ``SCALAR=`` keyword and allocatable
  scalar entities are available in Fortran 2003 and later.

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ==============================
  ``RESULT = ALLOCATED(ARRAY)``
  ==============================
  ``RESULT = ALLOCATED(SCALAR)``
  ==============================

:samp:`{Arguments}:`
  ================  ================================================
  :samp:`{ARRAY}`   The argument shall be an ``ALLOCATABLE`` array.
  ================  ================================================
  :samp:`{SCALAR}`  The argument shall be an ``ALLOCATABLE`` scalar.
  ================  ================================================

:samp:`{Return value}:`
  The return value is a scalar ``LOGICAL`` with the default logical
  kind type parameter.  If the argument is allocated, then the result is
  ``.TRUE.`` ; otherwise, it returns ``.FALSE.`` 

:samp:`{Example}:`

  .. code-block:: c++

    program test_allocated
      integer :: i = 4
      real(4), allocatable :: x(:)
      if (.not. allocated(x)) allocate(x(i))
    end program test_allocated

