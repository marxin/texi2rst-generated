.. _loop-analysis-and-representation:

Analysis and Representation of Loops
------------------------------------

GCC provides extensive infrastructure for work with natural loops, i.e.,
strongly connected components of CFG with only one entry block.  This
chapter describes representation of loops in GCC, both on GIMPLE and in
RTL, as well as the interfaces to loop-related analyses (induction
variable analysis and number of iterations analysis).

.. toctree::
  :maxdepth: 2

  loop-representation
  loop-querying
  loop-manipulation
  lcssa
  scalar-evolutions
  loop-iv
  number-of-iterations
  dependency-analysis
  loop-closed-ssa-form
  iv-analysis-on-rtl
  number-of-iterations-analysis
  data-dependency-analysis

