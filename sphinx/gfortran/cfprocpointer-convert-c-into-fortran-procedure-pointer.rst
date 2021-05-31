  .. _c_f_procpointer:

C_F_PROCPOINTER - Convert C into Fortran procedure pointer
**********************************************************

.. index:: C_F_PROCPOINTER

.. index:: pointer, C address of pointers

:samp:`{Description}:`
  ``C_F_PROCPOINTER(CPTR, FPTR)`` Assign the target of the C function pointer
  :samp:`{CPTR}` to the Fortran procedure pointer :samp:`{FPTR}`.

:samp:`{Standard}:`
  Fortran 2003 and later

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL C_F_PROCPOINTER(cptr, fptr)``

:samp:`{Arguments}:`
  ==============  ==========================================================
  :samp:`{CPTR}`  scalar of the type ``C_FUNPTR``. It is
                  ``INTENT(IN)``.
  ==============  ==========================================================
  :samp:`{FPTR}`  procedure pointer interoperable with :samp:`{cptr}`. It is
                  ``INTENT(OUT)``.
  ==============  ==========================================================

:samp:`{Example}:`

  .. code-block:: c++

    program main
      use iso_c_binding
      implicit none
      abstract interface
        function func(a)
          import :: c_float
          real(c_float), intent(in) :: a
          real(c_float) :: func
        end function
      end interface
      interface
         function getIterFunc() bind(c,name="getIterFunc")
           import :: c_funptr
           type(c_funptr) :: getIterFunc
         end function
      end interface
      type(c_funptr) :: cfunptr
      procedure(func), pointer :: myFunc
      cfunptr = getIterFunc()
      call c_f_procpointer(cfunptr, myFunc)
    end program main

:samp:`{See also}:`
  C_LOC, 
  C_F_POINTER

