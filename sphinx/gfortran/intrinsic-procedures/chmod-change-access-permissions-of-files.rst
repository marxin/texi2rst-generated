  .. _chmod:

CHMOD --- Change access permissions of files
********************************************

.. index:: CHMOD

.. index:: file system, change access mode

:samp:`{Description}:`
  ``CHMOD`` changes the permissions of a file.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ====================================
  ``CALL CHMOD(NAME, MODE[, STATUS])``
  ``STATUS = CHMOD(NAME, MODE)``
  ====================================

:samp:`{Arguments}:`
  ================  =============================================================================
  :samp:`{NAME}`    Scalar ``CHARACTER`` of default kind with the
                    file name. Trailing blanks are ignored unless the character
                    ``achar(0)`` is present, then all characters up to and excluding
                    ``achar(0)`` are used as the file name.
  :samp:`{MODE}`    Scalar ``CHARACTER`` of default kind giving the
                    file permission. :samp:`{MODE}` uses the same syntax as the ``chmod`` utility
                    as defined by the POSIX standard. The argument shall either be a string of
                    a nonnegative octal number or a symbolic mode.
  :samp:`{STATUS}`  (optional) scalar ``INTEGER``, which is
                    ``0`` on success and nonzero otherwise.
  ================  =============================================================================

:samp:`{Return value}:`
  In either syntax, :samp:`{STATUS}` is set to ``0`` on success and nonzero
  otherwise.

:samp:`{Example}:`
  ``CHMOD`` as subroutine

  .. code-block:: fortran

    program chmod_test
      implicit none
      integer :: status
      call chmod('test.dat','u+x',status)
      print *, 'Status: ', status
    end program chmod_test

  ``CHMOD`` as function:

  .. code-block:: fortran

    program chmod_test
      implicit none
      integer :: status
      status = chmod('test.dat','u+x')
      print *, 'Status: ', status
    end program chmod_test

