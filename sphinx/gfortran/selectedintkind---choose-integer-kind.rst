  .. _selected_int_kind:

``SELECTED_INT_KIND`` - Choose integer kind
*******************************************

.. index:: SELECTED_INT_KIND

.. index:: integer kind

.. index:: kind, integer

:samp:`{Description}:`
  ``SELECTED_INT_KIND(R)`` return the kind value of the smallest integer
  type that can represent all values ranging from -10^R (exclusive)
  to 10^R (exclusive). If there is no integer kind that accommodates
  this range, ``SELECTED_INT_KIND`` returns -1.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = SELECTED_INT_KIND(R)``

:samp:`{Arguments}:`
  ===========  ==========================================
  :samp:`{R}`  Shall be a scalar and of type ``INTEGER``.
  ===========  ==========================================
  ===========  ==========================================

:samp:`{Example}:`

  .. code-block:: c++

    program large_integers
      integer,parameter :: k5 = selected_int_kind(5)
      integer,parameter :: k15 = selected_int_kind(15)
      integer(kind=k5) :: i5
      integer(kind=k15) :: i15

      print *, huge(i5), huge(i15)

      ! The following inequalities are always true
      print *, huge(i5) >= 10_k5**5-1
      print *, huge(i15) >= 10_k15**15-1
    end program large_integers

