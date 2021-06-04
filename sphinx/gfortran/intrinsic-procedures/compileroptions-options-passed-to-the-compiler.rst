  .. _compiler_options:

COMPILER_OPTIONS --- Options passed to the compiler
***************************************************

.. index:: COMPILER_OPTIONS

.. index:: flags inquiry function

.. index:: options inquiry function

.. index:: compiler flags inquiry function

:samp:`{Description}:`
  ``COMPILER_OPTIONS`` returns a string with the options used for
  compiling.

:samp:`{Standard}:`
  Fortran 2008

:samp:`{Class}:`
  Inquiry function of the module ``ISO_FORTRAN_ENV``

:samp:`{Syntax}:`
  ``STR = COMPILER_OPTIONS()``

:samp:`{Arguments}:`
  None

:samp:`{Return value}:`
  The return value is a default-kind string with system-dependent length.
  It contains the compiler flags used to compile the file, which called
  the ``COMPILER_OPTIONS`` intrinsic.

:samp:`{Example}:`

  .. code-block:: fortran

       use iso_fortran_env
       print '(4a)', 'This file was compiled by ', &
                     compiler_version(), ' using the options ', &
                     compiler_options()
       end

:samp:`{See also}:`
  COMPILER_VERSION, 
  ISO_FORTRAN_ENV

