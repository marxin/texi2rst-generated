..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ATOMIC_ADD, Atomic subroutine, add

.. _atomic_add:

ATOMIC_ADD --- Atomic ADD operation
***********************************

.. function:: ATOMIC_ADD(ATOM, VALUE)

  ``ATOMIC_ADD(ATOM, VALUE)`` atomically adds the value of :samp:`{VALUE}` to the
  variable :samp:`{ATOM}`. When :samp:`{STAT}` is present and the invocation was
  successful, it is assigned the value 0. If it is present and the invocation
  has failed, it is assigned a positive value; in particular, for a coindexed
  :samp:`{ATOM}`, if the remote image has stopped, it is assigned the value of
  ``ISO_FORTRAN_ENV`` 's ``STAT_STOPPED_IMAGE`` and if the remote image has
  failed, the value ``STAT_FAILED_IMAGE``.

  :param ATOM:
    Scalar coarray or coindexed variable of integer
    type with ``ATOMIC_INT_KIND`` kind.

  :param VALUE:
    Scalar of the same type as :samp:`{ATOM}`. If the kind
    is different, the value is converted to the kind of :samp:`{ATOM}`.

  :param STAT:
    (optional) Scalar default-kind integer variable.

  Standard:
    TS 18508 or later

  Class:
    Atomic subroutine

  Syntax:
    .. code-block:: fortran

      CALL ATOMIC_ADD (ATOM, VALUE [, STAT])

  Example:
    .. code-block:: fortran

      program atomic
        use iso_fortran_env
        integer(atomic_int_kind) :: atom[*]
        call atomic_add (atom[1], this_image())
      end program atomic

  See also:
    :ref:`ATOMIC_DEFINE`, 
    :ref:`ATOMIC_FETCH_ADD`, 
    :ref:`ISO_FORTRAN_ENV`, 
    :ref:`ATOMIC_AND`, 
    :ref:`ATOMIC_OR`, 
    :ref:`ATOMIC_XOR`
