  .. _compiler_version:

COMPILER_VERSION --- Compiler version string
********************************************

.. index:: COMPILER_VERSION

.. index:: compiler, name and version

.. index:: version of the compiler

:samp:`{Description}:`
  ``COMPILER_VERSION`` returns a string with the name and the
  version of the compiler.

:samp:`{Standard}:`
  Fortran 2008

:samp:`{Class}:`
  Inquiry function of the module ``ISO_FORTRAN_ENV``

:samp:`{Syntax}:`
  ``STR = COMPILER_VERSION()``

:samp:`{Arguments}:`
  None

:samp:`{Return value}:`
  The return value is a default-kind string with system-dependent length.
  It contains the name of the compiler and its version number.

:samp:`{Example}:`

  .. code-block:: fortran

       use iso_fortran_env
       print '(4a)', 'This file was compiled by ', &
                     compiler_version(), ' using the options ', &
                     compiler_options()
       end

:samp:`{See also}:`
  COMPILER_OPTIONS, 
  ISO_FORTRAN_ENV

