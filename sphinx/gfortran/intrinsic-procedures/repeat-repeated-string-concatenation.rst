  .. _repeat:

REPEAT --- Repeated string concatenation 
*****************************************

.. index:: REPEAT

.. index:: string, repeat

.. index:: string, concatenate

:samp:`{Description}:`
  Concatenates :samp:`{NCOPIES}` copies of a string.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = REPEAT(STRING, NCOPIES)``

:samp:`{Arguments}:`
  =================  ==========================================
  :samp:`{STRING}`   Shall be scalar and of type ``CHARACTER``.
  =================  ==========================================
  :samp:`{NCOPIES}`  Shall be scalar and of type ``INTEGER``.
  =================  ==========================================

:samp:`{Return value}:`
  A new scalar of type ``CHARACTER`` built up from :samp:`{NCOPIES}` copies 
  of :samp:`{STRING}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_repeat
      write(*,*) repeat("x", 5)   ! "xxxxx"
    end program

