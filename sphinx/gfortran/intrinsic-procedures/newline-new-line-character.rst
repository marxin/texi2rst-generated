  .. _new_line:

NEW_LINE --- New line character
*******************************

.. index:: NEW_LINE

.. index:: newline

.. index:: output, newline

:samp:`{Description}:`
  ``NEW_LINE(C)`` returns the new-line character.

:samp:`{Standard}:`
  Fortran 2003 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = NEW_LINE(C)``

:samp:`{Arguments}:`
  ===========  ==============================================
  :samp:`{C}`  The argument shall be a scalar or array of the
               type ``CHARACTER``.
  ===========  ==============================================
  ===========  ==============================================

:samp:`{Return value}:`
  Returns a :samp:`{CHARACTER}` scalar of length one with the new-line character of
  the same kind as parameter :samp:`{C}`.

:samp:`{Example}:`

  .. code-block:: c++

    program newline
      implicit none
      write(*,'(A)') 'This is record 1.'//NEW_LINE('A')//'This is record 2.'
    end program newline

