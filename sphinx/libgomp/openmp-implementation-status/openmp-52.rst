..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _openmp-5.2:

OpenMP 5.2
**********

New features listed in Appendix B of the OpenMP specification
*************************************************************

========================================================================  ======  ========
Description                                                               Status  Comments
========================================================================  ======  ========
``omp_in_explicit_task`` routine and *implicit-task-var* ICV              N
``omp`` / ``ompx`` / ``omx`` sentinels and ``omp_`` / ``ompx_``           N/A
      namespaces
Clauses on ``end`` directive can be on directive                          N
Deprecation of no-argument ``destroy`` clause on ``depobj``               N
``linear`` clause syntax changes and ``step`` modifier                    N
Deprecation of minus operator for reductions                              N
Deprecation of separating ``map`` modifiers without comma                 N
``declare mapper`` with iterator and ``present`` modifiers                N
If a matching mapped list item is not found in the data environment, the  N
      pointer retains its original value
New ``enter`` clause as alias for ``to`` on declare target directive      Y
Deprecation of ``to`` clause on declare target directive                  N
Extended list of directives permitted in Fortran pure procedures          N
New ``allocators`` directive for Fortran                                  N
Deprecation of ``allocator`` directive for Fortran                        N
      allocatables/pointers
Optional paired ``end`` directive with ``dispatch``                       N
New ``memspace`` and ``traits`` modifiers for ``uses_allocators``         N
Deprecation of traits array following the allocator_handle expression in  N
      ``uses_allocators``
New ``otherwise`` clause as alias for ``default`` on metadirectives       N
Deprecation of ``default`` clause on metadirectives                       N
Deprecation of delimited form of ``declare target``                       N
Reproducible semantics changed for ``order(concurrent)``                  N
``allocate`` and ``firstprivate`` clauses on ``scope``                    Y
``ompt_callback_work``                                                    N
Default map-type for ``map`` clause in ``target enter/exit data``         N
New ``doacross`` clause as alias for ``depend`` with                      N
      ``source`` / ``sink`` modifier
Deprecation of ``depend`` with ``source`` / ``sink`` modifier             N
``omp_cur_iteration`` keyword                                             N
========================================================================  ======  ========

Other new OpenMP 5.2 features
*****************************

============================================================================  ======  ========
Description                                                                   Status  Comments
============================================================================  ======  ========
For Fortran, optional comma between directive and clause                      N
Conforming device numbers and ``omp_initial_device`` and                      Y
      ``omp_invalid_device`` enum/PARAMETER
Initial value of *default-device-var* ICV with                                N
      ``OMP_TARGET_OFFLOAD=mandatory``
*interop_types* in any position of the modifier list for the ``init`` clause  N
      of the ``interop`` construct
============================================================================  ======  ========

.. -
   OpenMP Runtime Library Routines
   -

