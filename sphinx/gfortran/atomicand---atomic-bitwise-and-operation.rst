  .. _atomic_and:

``ATOMIC_AND`` - Atomic bitwise AND operation
*********************************************

.. index:: ATOMIC_AND

.. index:: Atomic subroutine, AND

:samp:`{Description}:`
  ``ATOMIC_AND(ATOM, VALUE)`` atomically defines :samp:`{ATOM}` with the bitwise
  AND between the values of :samp:`{ATOM}` and :samp:`{VALUE}`. When :samp:`{STAT}` is present
  and the invocation was successful, it is assigned the value 0. If it is present
  and the invocation has failed, it is assigned a positive value; in particular,
  for a coindexed :samp:`{ATOM}` , if the remote image has stopped, it is assigned the
  value of ``ISO_FORTRAN_ENV``'s ``STAT_STOPPED_IMAGE`` and if the remote
  image has failed, the value ``STAT_FAILED_IMAGE``.

:samp:`{Standard}:`
  TS 18508 or later

:samp:`{Class}:`
  Atomic subroutine

:samp:`{Syntax}:`
  ``CALL ATOMIC_AND (ATOM, VALUE [, STAT])``

:samp:`{Arguments}:`
  ===============  ===================================================================
  :samp:`{ATOM}`   Scalar coarray or coindexed variable of integer
                   type with ``ATOMIC_INT_KIND`` kind.
  ===============  ===================================================================
  :samp:`{VALUE}`  Scalar of the same type as :samp:`{ATOM}`. If the kind
                   is different, the value is converted to the kind of :samp:`{ATOM}`.
  :samp:`{STAT}`   (optional) Scalar default-kind integer variable.
  ===============  ===================================================================

:samp:`{Example}:`

  .. code-block:: c++

    program atomic
      use iso_fortran_env
      integer(atomic_int_kind) :: atom[*]
      call atomic_and (atom[1], int(b'10100011101'))
    end program atomic

:samp:`{See also}:`
  ATOMIC_DEFINE, 
  ATOMIC_FETCH_AND, 
  ISO_FORTRAN_ENV, 
  ATOMIC_ADD, 
  ATOMIC_OR, 
  ATOMIC_XOR

