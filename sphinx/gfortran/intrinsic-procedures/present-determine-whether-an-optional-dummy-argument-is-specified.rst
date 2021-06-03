  .. _present:

PRESENT --- Determine whether an optional dummy argument is specified
*********************************************************************

.. index:: PRESENT

:samp:`{Description}:`
  Determines whether an optional dummy argument is present.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = PRESENT(A)``

:samp:`{Arguments}:`
  ===========  ===============================================================================
  :samp:`{A}`  May be of any type and may be a pointer, scalar or array
               value, or a dummy procedure. It shall be the name of an optional dummy argument
               accessible within the current subroutine or function.
  ===========  ===============================================================================
  ===========  ===============================================================================

:samp:`{Return value}:`
  Returns either ``TRUE`` if the optional argument :samp:`{A}` is present, or
  ``FALSE`` otherwise.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_present
      WRITE(*,*) f(), f(42)      ! "F T"
    CONTAINS
      LOGICAL FUNCTION f(x)
        INTEGER, INTENT(IN), OPTIONAL :: x
        f = PRESENT(x)
      END FUNCTION
    END PROGRAM

