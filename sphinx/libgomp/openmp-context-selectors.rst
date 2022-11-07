..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _openmp-context-selectors:

OpenMP Context Selectors
************************

``vendor`` is always ``gnu``. References are to the GCC manual.

.. list-table::
   :header-rows: 1

   * - ``arch``
     - ``kind``
     - ``isa``

   * - ``x86``, ``x86_64``, ``i386``, ``i486``, ``i586``, ``i686``, ``ia32``
     - ``host``
     - See ``-m...`` flags in 'x86 Options' (without ``-m``)
   * - ``amdgcn``, ``gcn``
     - ``gpu``
     - See ``-march=`` in 'AMD GCN Options'
   * - ``nvptx``
     - ``gpu``
     - See ``-march=`` in 'Nvidia PTX Options'

