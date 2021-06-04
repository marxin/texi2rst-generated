.. _gfortran_list_separator:

GFORTRAN_LIST_SEPARATOR---Separator for list output
***************************************************

This environment variable specifies the separator when writing
list-directed output.  It may contain any number of spaces and
at most one comma.  If you specify this on the command line,
be sure to quote spaces, as in

.. code-block:: fortran

  $ GFORTRAN_LIST_SEPARATOR='  ,  ' ./a.out

when :command:`a.out` is the compiled Fortran program that you want to run.
Default is a single space.

