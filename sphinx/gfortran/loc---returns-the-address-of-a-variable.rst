  .. _loc:

LOC - Returns the address of a variable
***************************************

.. index:: LOC

.. index:: location of a variable in memory

:samp:`{Description}:`
  ``LOC(X)`` returns the address of :samp:`{X}` as an integer.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = LOC(X)``

:samp:`{Arguments}:`
  ===========  =====================
  :samp:`{X}`  Variable of any type.
  ===========  =====================
  ===========  =====================

:samp:`{Return value}:`
  The return value is of type ``INTEGER``, with a ``KIND``
  corresponding to the size (in bytes) of a memory address on the target
  machine.

:samp:`{Example}:`

  .. code-block:: c++

    program test_loc
      integer :: i
      real :: r
      i = loc(r)
      print *, i
    end program test_loc

