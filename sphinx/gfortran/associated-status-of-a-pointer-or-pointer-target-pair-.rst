  .. _associated:

ASSOCIATED - Status of a pointer or pointer/target pair 
********************************************************

.. index:: ASSOCIATED

.. index:: pointer, status

.. index:: association status

:samp:`{Description}:`
  ``ASSOCIATED(POINTER [, TARGET])`` determines the status of the pointer
  :samp:`{POINTER}` or if :samp:`{POINTER}` is associated with the target :samp:`{TARGET}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = ASSOCIATED(POINTER [, TARGET])``

:samp:`{Arguments}:`
  =================  ===============================================================
  :samp:`{POINTER}`  :samp:`{POINTER}` shall have the ``POINTER`` attribute
                     and it can be of any type.
  =================  ===============================================================
  :samp:`{TARGET}`   (Optional) :samp:`{TARGET}` shall be a pointer or
                     a target.  It must have the same type, kind type parameter, and
                     array rank as :samp:`{POINTER}`.
  =================  ===============================================================
  The association status of neither :samp:`{POINTER}` nor :samp:`{TARGET}` shall be
  undefined.

:samp:`{Return value}:`
  ``ASSOCIATED(POINTER)`` returns a scalar value of type ``LOGICAL(4)``.
  There are several cases:

  :samp:`(A) When the optional {TARGET} is not present then`
    ``ASSOCIATED(POINTER)`` is true if :samp:`{POINTER}` is associated with a target; otherwise, it returns false.

  :samp:`(B) If {TARGET} is present and a scalar target, the result is true if`
    :samp:`{TARGET}` is not a zero-sized storage sequence and the target associated with :samp:`{POINTER}` occupies the same storage units.  If :samp:`{POINTER}` is
    disassociated, the result is false.

  :samp:`(C) If {TARGET} is present and an array target, the result is true if`
    :samp:`{TARGET}` and :samp:`{POINTER}` have the same shape, are not zero-sized arrays,
    are arrays whose elements are not zero-sized storage sequences, and
    :samp:`{TARGET}` and :samp:`{POINTER}` occupy the same storage units in array element
    order.
    As in case(B), the result is false, if :samp:`{POINTER}` is disassociated.

  :samp:`(D) If {TARGET} is present and an scalar pointer, the result is true`
    if :samp:`{TARGET}` is associated with :samp:`{POINTER}`, the target associated with
    :samp:`{TARGET}` are not zero-sized storage sequences and occupy the same storage
    units.
    The result is false, if either :samp:`{TARGET}` or :samp:`{POINTER}` is disassociated.

  :samp:`(E) If {TARGET} is present and an array pointer, the result is true if`
    target associated with :samp:`{POINTER}` and the target associated with :samp:`{TARGET}`
    have the same shape, are not zero-sized arrays, are arrays whose elements are
    not zero-sized storage sequences, and :samp:`{TARGET}` and :samp:`{POINTER}` occupy
    the same storage units in array element order.
    The result is false, if either :samp:`{TARGET}` or :samp:`{POINTER}` is disassociated.

:samp:`{Example}:`

  .. code-block:: c++

    program test_associated
       implicit none
       real, target  :: tgt(2) = (/1., 2./)
       real, pointer :: ptr(:)
       ptr => tgt
       if (associated(ptr)     .eqv. .false.) call abort
       if (associated(ptr,tgt) .eqv. .false.) call abort
    end program test_associated

:samp:`{See also}:`
  NULL

