.. _passes:

Passes and Files of the Compiler
--------------------------------

.. index:: passes and files of the compiler

.. index:: files and passes of the compiler

.. index:: compiler passes and files

.. index:: pass dumps

This chapter is dedicated to giving an overview of the optimization and
code generation passes of the compiler.  In the process, it describes
some of the language front end interface, though this description is no
where near complete.

.. toctree::

  The language front end turns text into bits. <parsing-pass>
  The bits are turned into something we can optimize. <gimplification-pass>
  Sequencing the optimization passes. <pass-manager>
  Inter-procedural optimizations. <ipa-passes>
  Optimizations on a high-level representation. <tree-ssa-passes>
  Optimizations on a low-level representation. <rtl-passes>
  Dumping optimization information from passes. <optimization-info>

.. toctree::

  parsing-pass
  gimplification-pass
  pass-manager
  inter-procedural-optimization-passes
  tree-ssa-passes
  rtl-passes
  optimization-info

