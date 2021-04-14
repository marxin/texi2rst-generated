  .. _c_sizeof:

``C_SIZEOF`` - Size in bytes of an expression
*********************************************

.. index:: C_SIZEOF

.. index:: expression size

.. index:: size of an expression

:samp:`{Description}:`
  ``C_SIZEOF(X)`` calculates the number of bytes of storage the
  expression ``X`` occupies.

:samp:`{Standard}:`
  Fortran 2008

:samp:`{Class}:`
  Inquiry function of the module ``ISO_C_BINDING``

:samp:`{Syntax}:`
  ``N = C_SIZEOF(X)``

:samp:`{Arguments}:`
  ===========  ===================================================
  :samp:`{X}`  The argument shall be an interoperable data entity.
  ===========  ===================================================
  ===========  ===================================================

:samp:`{Return value}:`
  The return value is of type integer and of the system-dependent kind
  ``C_SIZE_T`` (from the ``ISO_C_BINDING`` module). Its value is the
  number of bytes occupied by the argument.  If the argument has the
  ``POINTER`` attribute, the number of bytes of the storage area pointed
  to is returned.  If the argument is of a derived type with ``POINTER``
  or ``ALLOCATABLE`` components, the return value does not account for
  the sizes of the data pointed to by these components.

:samp:`{Example}:`

  .. code-block:: c++

       use iso_c_binding
       integer(c_int) :: i
       real(c_float) :: r, s(5)
       print *, (c_sizeof(s)/c_sizeof(r) == 5)
       end

  The example will print ``T`` unless you are using a platform
  where default ``REAL`` variables are unusually padded.

:samp:`{See also}:`
  SIZEOF, 
  STORAGE_SIZE

