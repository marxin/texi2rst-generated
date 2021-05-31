.. _translating-to-generic:

Generating the intermediate language for later stages.
------------------------------------------------------

This chapter deals with the transformation of gfortran's frontend data
structures to the intermediate language used by the later stages of
the compiler, the so-called middle end.

Data structures relating to this are found in the source files
trans*.h and trans-*.c.

.. toctree::
  :maxdepth: 2

  basic-data-structures
  converting-expressions
  translating-statements
  accessing-declarations
  converting-expressions-to-tree

