.. _poly_int:

Sizes and offsets as runtime invariants
---------------------------------------

.. index:: polynomial integers

.. index:: poly_int

GCC allows the size of a hardware register to be a runtime invariant
rather than a compile-time constant.  This in turn means that various
sizes and offsets must also be runtime invariants rather than
compile-time constants, such as:

* the size of a general ``machine_mode`` (see :ref:`machine-modes`);

* the size of a spill slot;

* the offset of something within a stack frame;

* the number of elements in a vector;

* the size and offset of a ``mem`` rtx (see :ref:`regs-and-memory`); and

* the byte offset in a ``subreg`` rtx (see :ref:`regs-and-memory`).

The motivating example is the Arm SVE ISA, whose vector registers can be
any multiple of 128 bits between 128 and 2048 inclusive.  The compiler
normally produces code that works for all SVE register sizes, with the
actual size only being known at runtime.

GCC's main representation of such runtime invariants is the
``poly_int`` class.  This chapter describes what ``poly_int``
does, lists the available operations, and gives some general
usage guidelines.

.. toctree::
  :maxdepth: 2

  overview-of-poly_int
  consequences-of-using-poly_int
  comparisons-involving-poly_int
  arithmetic-on-poly_ints
  alignment-of-poly_ints
  computing-bounds-on-poly_ints
  converting-poly_ints
  miscellaneous-poly_int-routines
  guidelines-for-using-poly_int
  overview-of-polyint
  consequences-of-using-polyint
  comparisons-involving-polyint
  arithmetic-on-polyints
  alignment-of-polyints
  computing-bounds-on-polyints
  converting-polyints
  miscellaneous-polyint-routines
  guidelines-for-using-polyint

