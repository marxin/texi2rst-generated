  .. _bit_size:

``BIT_SIZE`` - Bit size inquiry function
****************************************

.. index:: BIT_SIZE

.. index:: bits, number of

.. index:: size of a variable, in bits

:samp:`{Description}:`
  ``BIT_SIZE(I)`` returns the number of bits (integer precision plus sign bit)
  represented by the type of :samp:`{I}`.  The result of ``BIT_SIZE(I)`` is
  independent of the actual value of :samp:`{I}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = BIT_SIZE(I)``

:samp:`{Arguments}:`
  ===========  ==============================
  :samp:`{I}`  The type shall be ``INTEGER``.
  ===========  ==============================
  ===========  ==============================

:samp:`{Return value}:`
  The return value is of type ``INTEGER``

:samp:`{Example}:`

  .. code-block:: c++

    program test_bit_size
        integer :: i = 123
        integer :: size
        size = bit_size(i)
        print *, size
    end program test_bit_size

