  .. _is_contiguous:

IS_CONTIGUOUS - Test whether an array is contiguous
***************************************************

.. index:: IS_IOSTAT_EOR

.. index:: array, contiguity

:samp:`{Description}:`
  ``IS_CONTIGUOUS`` tests whether an array is contiguous.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = IS_CONTIGUOUS(ARRAY)``

:samp:`{Arguments}:`
  ===============  ==============================
  :samp:`{ARRAY}`  Shall be an array of any type.
  ===============  ==============================
  ===============  ==============================

:samp:`{Return value}:`
  Returns a ``LOGICAL`` of the default kind, which ``.TRUE.`` if
  :samp:`{ARRAY}` is contiguous and false otherwise.

:samp:`{Example}:`

  .. code-block:: c++

    program test
      integer :: a(10)
      a = [1,2,3,4,5,6,7,8,9,10]
      call sub (a)      ! every element, is contiguous
      call sub (a(::2)) ! every other element, is noncontiguous
    contains
      subroutine sub (x)
        integer :: x(:)
        if (is_contiguous (x)) then
          write (*,*) 'X is contiguous'
        else
          write (*,*) 'X is not contiguous'
        end if
      end subroutine sub
    end program test

