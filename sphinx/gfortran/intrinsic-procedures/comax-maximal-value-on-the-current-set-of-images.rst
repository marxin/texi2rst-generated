.. _co_max:

CO_MAX --- Maximal value on the current set of images
*****************************************************

.. index:: CO_MAX

.. index:: Collectives, maximal value

.. function:: CO_MAX

  ``CO_MAX`` determines element-wise the maximal value of :samp:`{A}` on all
  images of the current team.  If :samp:`{RESULT_IMAGE}` is present, the maximum
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

  :samp:`{Standard}:`
    Technical Specification (TS) 18508 or later

  :samp:`{Class}:`
    Collective subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL CO_MAX(A [, RESULT_IMAGE, STAT, ERRMSG])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test
        integer :: val
        val = this_image ()
        call co_max (val, result_image=1)
        if (this_image() == 1) then
          write(*,*) "Maximal value", val  ! prints num_images()
        end if
      end program test

  :samp:`{See also}:`
    CO_MIN, 
    CO_SUM, 
    CO_REDUCE, 
    CO_BROADCAST

