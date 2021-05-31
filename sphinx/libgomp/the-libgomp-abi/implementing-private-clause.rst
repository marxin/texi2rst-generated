.. _implementing-private-clause:

Implementing PRIVATE clause
***************************

In association with a PARALLEL, or within the lexical extent
of a PARALLEL block, the variable becomes a local variable in
the parallel subfunction.

In association with FOR or SECTIONS blocks, create a new
automatic variable within the current function.  This preserves
the semantic of new variable creation.

