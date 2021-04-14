  .. _atomic_define:

``ATOMIC_DEFINE`` - Setting a variable atomically
*************************************************

.. index:: ATOMIC_DEFINE

.. index:: Atomic subroutine, define

:samp:`{Description}:`
  ``ATOMIC_DEFINE(ATOM, VALUE)`` defines the variable :samp:`{ATOM}` with the value
  :samp:`{VALUE}` atomically. When :samp:`{STAT}` is present and the invocation was
  successful, it is assigned the value 0. If it is present and the invocation
  has failed, it is assigned a positive value; in particular, for a coindexed
  :samp:`{ATOM}` , if the remote image has stopped, it is assigned the value of
  ``ISO_FORTRAN_ENV``'s ``STAT_STOPPED_IMAGE`` and if the remote image has
  failed, the value ``STAT_FAILED_IMAGE``.

:samp:`{Standard}:`
  Fortran 2008 and later; with :samp:`{STAT}` , TS 18508 or later

:samp:`{Class}:`
  Atomic subroutine

:samp:`{Syntax}:`
  ``CALL ATOMIC_DEFINE (ATOM, VALUE [, STAT])``

:samp:`{Arguments}:`
  ===============  ===================================================================
  :samp:`{ATOM}`   Scalar coarray or coindexed variable of either integer
                   type with ``ATOMIC_INT_KIND`` kind or logical type with
                   ``ATOMIC_LOGICAL_KIND`` kind.
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
      call atomic_define (atom[1], this_image())
    end program atomic

:samp:`{See also}:`
  ATOMIC_REF, 
  ATOMIC_CAS, 
  ISO_FORTRAN_ENV, 
  ATOMIC_ADD, 
  ATOMIC_AND, 
  ATOMIC_OR, 
  ATOMIC_XOR

