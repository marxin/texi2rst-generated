..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _co_min:

CO_MIN --- Minimal value on the current set of images
*****************************************************

.. index:: CO_MIN, Collectives, minimal value

.. function:: CO_MIN(A, RESULT_IMAGE, STAT, ERRMSG)

  ``CO_MIN`` determines element-wise the minimal value of :samp:`{A}` on all
  images of the current team.  If :samp:`{RESULT_IMAGE}` is present, the minimal
  values are returned in :samp:`{A}` on the specified image only and the value
  of :samp:`{A}` on the other images become undefined.  If :samp:`{RESULT_IMAGE}` is
  not present, the value is returned on all images.  If the execution was
  successful and :samp:`{STAT}` is present, it is assigned the value zero.  If the
  execution failed, :samp:`{STAT}` gets assigned a nonzero value and, if present,
  :samp:`{ERRMSG}` gets assigned a value describing the occurred error.

  :param A:
    shall be an integer, real or character variable,
    which has the same type and type parameters on all images of the team.

  :param RESULT_IMAGE:
    (optional) a scalar integer expression; if
    present, it shall have the same value on all images and refer to an
    image of the current team.

  :param STAT:
    (optional) a scalar integer variable

  :param ERRMSG:
    (optional) a scalar character variable

  Standard:
    Technical Specification (TS) 18508 or later

  Class:
    Collective subroutine

  Syntax:
    .. code-block:: fortran

      CALL CO_MIN(A [, RESULT_IMAGE, STAT, ERRMSG])

  Example:
    .. code-block:: fortran

      program test
        integer :: val
        val = this_image ()
        call co_min (val, result_image=1)
        if (this_image() == 1) then
          write(*,*) "Minimal value", val  ! prints 1
        end if
      end program test

  See also:
    :ref:`CO_MAX`, 
    :ref:`CO_SUM`, 
    :ref:`CO_REDUCE`, 
    :ref:`CO_BROADCAST`
