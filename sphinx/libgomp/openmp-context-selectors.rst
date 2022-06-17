..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _openmp-context-selectors:

OpenMP Context Selectors
************************

``vendor`` is always ``gnu``. References are to the GCC manual.

=======================================================  ========  =====================================================
``arch``                                                 ``kind``  ``isa``
=======================================================  ========  =====================================================
``intel_mic``, ``x86``, ``x86_64``, ``i386``, ``i486``,  ``host``  See ``-m...`` flags in 'x86 Options' (without ``-m``)
      ``i586``, ``i686``, ``ia32``
``amdgcn``, ``gcn``                                      ``gpu``   See ``-march=`` in 'AMD GCN Options'
``nvptx``                                                ``gpu``   See ``-misa=`` in 'Nvidia PTX Options'
=======================================================  ========  =====================================================

.. -
   The libgomp ABI
   -

