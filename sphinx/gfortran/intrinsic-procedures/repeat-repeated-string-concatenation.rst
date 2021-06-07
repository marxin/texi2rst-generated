.. _repeat:

REPEAT --- Repeated string concatenation 
*****************************************

.. index:: REPEAT

.. index:: string, repeat

.. index:: string, concatenate

.. function:: REPEAT

  Concatenates :samp:`{NCOPIES}` copies of a string.

  :param STRING:
    Shall be scalar and of type ``CHARACTER``.

  :param NCOPIES:
    Shall be scalar and of type ``INTEGER``.

  :return:
    A new scalar of type ``CHARACTER`` built up from :samp:`{NCOPIES}` copies 
    of :samp:`{STRING}`.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`
    ``RESULT = REPEAT(STRING, NCOPIES)``

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_repeat
        write(*,*) repeat("x", 5)   ! "xxxxx"
      end program

