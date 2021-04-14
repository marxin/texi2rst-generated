.. _loop-analysis-and-representation:

Analysis and Representation of Loops
------------------------------------

GCC provides extensive infrastructure for work with natural loops, i.e.,
strongly connected components of CFG with only one entry block.  This
chapter describes representation of loops in GCC, both on GIMPLE and in
RTL, as well as the interfaces to loop-related analyses (induction
variable analysis and number of iterations analysis).

.. toctree::

  Representation and analysis of loops. <loop-representation>
  Getting information about loops. <loop-querying>
  Loop manipulation functions. <loop-manipulation>
  Loop-closed SSA form. <lcssa>
  Induction variables on GIMPLE. <scalar-evolutions>
  Induction variables on RTL. <loop-iv>
  Number of iterations analysis. <number-of-iterations>
  Data dependency analysis. <dependency-analysis>

.. toctree::

  loop-representation
  loop-querying
  loop-manipulation
  loop-closed-ssa-form
  scalar-evolutions
  iv-analysis-on-rtl
  number-of-iterations-analysis
  data-dependency-analysis

