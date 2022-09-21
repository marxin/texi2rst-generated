..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _compiler_version:

.. index:: COMPILER_VERSION

.. index:: compiler, name and version

.. index:: version of the compiler

COMPILER_VERSION --- Compiler version string
********************************************

.. function:: COMPILER_VERSION

  ``COMPILER_VERSION`` returns a string with the name and the
  version of the compiler.

  :return:
    The return value is a default-kind string with system-dependent length.
    It contains the name of the compiler and its version number.

  :samp:`{Standard}:`
    Fortran 2008

  :samp:`{Class}:`
    Inquiry function of the module ``ISO_FORTRAN_ENV``

  :samp:`{Syntax}:`

    .. code-block:: fortran

      STR = COMPILER_VERSION()

  :samp:`{Arguments}:`
    None

  :samp:`{Example}:`

    .. code-block:: fortran

         use iso_fortran_env
         print '(4a)', 'This file was compiled by ', &
                       compiler_version(), ' using the options ', &
                       compiler_options()
         end

  :samp:`{See also}:`
    :ref:`COMPILER_OPTIONS`, 
    :ref:`ISO_FORTRAN_ENV`

