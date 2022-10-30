GCC 13 Release Series

Changes, New Features, and Fixes
================================

This page is a "brief" summary of some of the huge number of
improvements in GCC 13. You may also want to check out our `Porting to
GCC 13 <porting_to.html>`__ page and the `full GCC
documentation <../onlinedocs/index.html#current>`__.

Note: GCC 13 has not been released yet, so this document is a
work-in-progress.

Caveats
-------

-  The support for the ``cr16-elf``, ``tilegx*-linux``,
   ``tilepro*-linux``, ``hppa[12]*-*-hpux10*``, ``hppa[12]*-*-hpux11*``
   and ``m32c-rtems`` configurations has been removed.
-  Support for emitting the STABS debugging format (including the
   ``-gstabs`` and ``-gxcoff`` options) has been removed. (This means
   the **dbx** debugger is no longer supported, either.)
-  Legacy debug info compression option ``-gz=zlib-gnu`` was removed and
   the option is ignored right now.
-  New debug info compression option value ``-gz=zstd`` has been added.

.. _general:

General Improvements
--------------------

-  `OpenMP <https://gcc.gnu.org/projects/gomp/>`__

   -  Reverse offload is now supported and the all clauses to the
      ``requires`` directive are now accepted; however, the
      ``requires_offload``, ``unified_address`` and
      ``unified_shared_memory`` clauses cause that the only available
      device is the initial device (the host).
   -  The following OpenMP 5.1 features have been added: the
      ``omp_all_memory`` reserved locator, the ``inoutset`` modifier to
      the ``depend`` clause, the ``nowait`` clause for the ``taskwait``
      directive and the ``omp_target_is_accessible``,
      ``omp_target_memcpy_async``, ``omp_target_memcpy_rect_async`` and
      ``omp_get_mapped_ptr`` API routines. Fortran now supports
      non-rectangular loop nests, which were added for C/C++ in GCC 11.
   -  Initial support for OpenMP 5.2 features have been added: Support
      for ``firstprivate`` and ``allocate`` clauses on the ``scope``
      construct and the OpenMP 5.2 syntax of the ``linear`` clause; the
      new enum/constants ``omp_initial_device`` and
      ``omp_invalid_device``; and optionally omitting the map-type in
      ``target enter/exit data``. The ``enter`` clause (as alias for
      ``to``) has been added to the ``declare target`` directive.
   -  For user defined allocators requesting high bandwidth or large
      capacity memspaces or interleaved partitioning, the
      `memkind <http://memkind.github.io/memkind/>`__ library is used,
      if available at runtime.

-  AddressSanitizer defaults to ``detect_stack_use_after_return=1`` on
   GNU/Linux targets. For compatibility, it can be disabled with
   ``env ASAN_OPTIONS=detect_stack_use_after_return=0``.
-  Link-time optimization improvements:

   -  LTO supports the newly added jobserver of GNU make jobserver that
      uses named pipes (``--jobserver-style=fifo``) by default.
   -  If make's jobserver is active, parallel LTO WPA streaming
      communicates with it and thus avoids system overcommitting.

.. _languages:

New Languages and Language specific improvements
------------------------------------------------

C
~

-  Several C23 features have been implemented:

   -  `N3042 <https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3042.htm>`__,
      Introduce the nullptr constant
   -  Support for empty initializer braces

-  New warnings:

   -  ``-Wenum-int-mismatch`` warns about mismatches between an
      enumerated type and an integer type (:PR:`105131`)

.. _cxx:

C++
~~~

-  Several C++23 features have been implemented:

   -  :P:`2324R1`, Labels at the end of
      compound statements (:PR:`103539`)
   -  `P2255R2 <https://wg21.link/p2255>`__, A type trait to detect
      reference binding to temporary
      (`PR104477 <https://gcc.gnu.org/PR104477>`__)
   -  `P2327R1 <https://wg21.link/p2327>`__, De-deprecating volatile
      compound operations
   -  `P2437R1 <https://wg21.link/p2437>`__, Support for ``#warning``
      (`PR106646 <https://gcc.gnu.org/PR106646>`__)
   -  `P2290R3 <https://wg21.link/p2290>`__, Delimited escape sequences
      (`PR106645 <https://gcc.gnu.org/PR106645>`__)
   -  `P2071R2 <https://wg21.link/p2071>`__, Named universal character
      escapes (`PR106648 <https://gcc.gnu.org/PR106648>`__)
   -  `P2513R3 <https://wg21.link/p2513>`__, ``char8_t`` Compatibility
      and Portability Fix (`PR106656 <https://gcc.gnu.org/PR106656>`__)
   -  `P1169R4 <https://wg21.link/p1169r4>`__, static ``operator()``
      (`PR106651 <https://gcc.gnu.org/PR106651>`__)
   -  `P2266R3 <https://wg21.link/p2266r3>`__, Simpler implicit move
      (`PR101165 <https://gcc.gnu.org/PR101165>`__)

-  New warnings:

   -  ``-Wself-move`` warns when a value is moved to itself with
      ``std::move`` (`PR81159 <https://gcc.gnu.org/PR81159>`__)

-  The ``-Wpessimizing-move`` and ``-Wredundant-move`` warnings have
   been extended to warn in more contexts.

.. _libstdcxx:

Runtime Library (libstdc++)
^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  Improved experimental support for C++23, including:

   -  Additions to the ``<ranges>`` header: ``views::zip``,
      ``views::zip_transform``, ``views::adjacent``,
      ``views::adjacent_transform`` ``views::pairwise``,
      ``views::slide``, ``views::chunk``, ``views::chunk_by``.

-  Support for the ``<experimental/scope>`` header from v3 of the
   Library Fundamentals Technical Specification.

.. _targets:

New Targets and Target Specific Improvements
--------------------------------------------

.. _amdgcn:

AMD Radeon (GCN)
~~~~~~~~~~~~~~~~

-  Support for the Instinct MI200 series devices (
   ```gfx90a`` <https://gcc.gnu.org/onlinedocs/gcc/AMD-GCN-Options.html>`__)
   has been added.

arm
~~~

-  The STAR-MC1 CPU is now supported through the ``star-mc1`` argument
   to the ``-mcpu`` and ``-mtune`` options.

.. _x86:

IA-32/x86-64
~~~~~~~~~~~~

-  For both C and C++ the ``__bf16`` type is supported on x86 systems
   with SSE2 and above enabled.

NVPTX
~~~~~

-  The default value for the
   ```-march`` <https://gcc.gnu.org/onlinedocs/gcc/Nvidia-PTX-Options.html>`__
   option can be now changed when `building
   GCC <https://gcc.gnu.org/install/>`__ using the
   ```--with-arch=`` <https://gcc.gnu.org/install/specific.html#nvptx-x-none>`__
   configure option. GCC's target libraries are then build both with
   ``sm_30`` and the specified target architecture. If not specified,
   GCC defaults to ``sm_30``.

.. _os:

Operating Systems
-----------------

Other significant improvements
------------------------------

.. container:: copyright

   For questions related to the use of GCC, please consult these web
   pages and the `GCC manuals <https://gcc.gnu.org/onlinedocs/>`__. If
   that fails, the gcc-help@gcc.gnu.org mailing list might help.
   Comments on these web pages and the development of GCC are welcome on
   our developer list at gcc@gcc.gnu.org. All of `our
   lists <https://gcc.gnu.org/lists.html>`__ have public archives.
   Copyright (C) `Free Software Foundation,
   Inc. <https://www.fsf.org>`__ Verbatim copying and distribution of
   this entire article is permitted in any medium, provided this notice
   is preserved.

   These pages are `maintained by the GCC
   team <https://gcc.gnu.org/about.html>`__. Last modified
   2022-10-03\ `. <http://validator.w3.org/check/referer>`__
