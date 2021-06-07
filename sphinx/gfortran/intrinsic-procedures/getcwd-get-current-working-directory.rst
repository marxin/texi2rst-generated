.. _getcwd:

GETCWD --- Get current working directory
****************************************

.. index:: GETCWD

.. index:: system, working directory

.. function:: GETCWD

  Get current working directory.

  :param C:
    The type shall be ``CHARACTER`` and of default kind.

  :param STATUS:
    (Optional) status flag. Returns 0 on success, 
    a system specific and nonzero error code otherwise.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

    =============================
    ``CALL GETCWD(C [, STATUS])``
    ``STATUS = GETCWD(C)``
    =============================

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_getcwd
        CHARACTER(len=255) :: cwd
        CALL getcwd(cwd)
        WRITE(*,*) TRIM(cwd)
      END PROGRAM

  :samp:`{See also}:`
    CHDIR

