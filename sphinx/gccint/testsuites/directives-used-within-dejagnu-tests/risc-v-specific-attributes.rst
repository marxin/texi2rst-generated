..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

RISC-V specific attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

``rv32``
  Test system has an integer register width of 32 bits.

``rv64``
  Test system has an integer register width of 64 bits.

Other hardware attributes
~~~~~~~~~~~~~~~~~~~~~~~~~

.. Please keep this table sorted alphabetically.

``autoincdec``
  Target supports autoincrement/decrement addressing.

``avx``
  Target supports compiling ``avx`` instructions.

``avx_runtime``
  Target supports the execution of ``avx`` instructions.

``avx2``
  Target supports compiling ``avx2`` instructions.

``avx2_runtime``
  Target supports the execution of ``avx2`` instructions.

``avxvnni``
  Target supports the execution of ``avxvnni`` instructions.

``avx512f``
  Target supports compiling ``avx512f`` instructions.

``avx512f_runtime``
  Target supports the execution of ``avx512f`` instructions.

``avx512vp2intersect``
  Target supports the execution of ``avx512vp2intersect`` instructions.

``amx_tile``
  Target supports the execution of ``amx-tile`` instructions.

``amx_int8``
  Target supports the execution of ``amx-int8`` instructions.

``amx_bf16``
  Target supports the execution of ``amx-bf16`` instructions.

``cell_hw``
  Test system can execute AltiVec and Cell PPU instructions.

``coldfire_fpu``
  Target uses a ColdFire FPU.

``divmod``
  Target supporting hardware divmod insn or divmod libcall.

``divmod_simode``
  Target supporting hardware divmod insn or divmod libcall for SImode.

``hard_float``
  Target supports FPU instructions.

``non_strict_align``
  Target does not require strict alignment.

``pie_copyreloc``
  The x86-64 target linker supports PIE with copy reloc.

``rdrand``
  Target supports x86 ``rdrand`` instruction.

``sqrt_insn``
  Target has a square root instruction that the compiler can generate.

``sse``
  Target supports compiling ``sse`` instructions.

``sse_runtime``
  Target supports the execution of ``sse`` instructions.

``sse2``
  Target supports compiling ``sse2`` instructions.

``sse2_runtime``
  Target supports the execution of ``sse2`` instructions.

``sync_char_short``
  Target supports atomic operations on ``char`` and ``short``.

``sync_int_long``
  Target supports atomic operations on ``int`` and ``long``.

``ultrasparc_hw``
  Test environment appears to run executables on a simulator that
  accepts only ``EM_SPARC`` executables and chokes on ``EM_SPARC32PLUS``
  or ``EM_SPARCV9`` executables.

``vect_cmdline_needed``
  Target requires a command line argument to enable a SIMD instruction set.

``xorsign``
  Target supports the xorsign optab expansion.

Environment attributes
~~~~~~~~~~~~~~~~~~~~~~

``c``
  The language for the compiler under test is C.

``c++``
  The language for the compiler under test is C++.

``c99_runtime``
  Target provides a full C99 runtime.

``correct_iso_cpp_string_wchar_protos``
  Target ``string.h`` and ``wchar.h`` headers provide C++ required
  overloads for ``strchr`` etc. functions.

``d_runtime``
  Target provides the D runtime.

``d_runtime_has_std_library``
  Target provides the D standard library (Phobos).

``dummy_wcsftime``
  Target uses a dummy ``wcsftime`` function that always returns zero.

``fd_truncate``
  Target can truncate a file from a file descriptor, as used by
  :samp:`libgfortran/io/unix.c:fd_truncate`; i.e. ``ftruncate`` or
  ``chsize``.

``fenv``
  Target provides :samp:`fenv.h` include file.

``fenv_exceptions``
  Target supports :samp:`fenv.h` with all the standard IEEE exceptions
  and floating-point exceptions are raised by arithmetic operations.

``fenv_exceptions_dfp``
  Target supports :samp:`fenv.h` with all the standard IEEE exceptions
  and floating-point exceptions are raised by arithmetic operations for
  decimal floating point.

``fileio``
  Target offers such file I/O library functions as ``fopen``,
  ``fclose``, ``tmpnam``, and ``remove``.  This is a link-time
  requirement for the presence of the functions in the library; even if
  they fail at runtime, the requirement is still regarded as satisfied.

``freestanding``
  Target is :samp:`freestanding` as defined in section 4 of the C99 standard.
  Effectively, it is a target which supports no extra headers or libraries
  other than what is considered essential.

``gettimeofday``
  Target supports ``gettimeofday``.

``init_priority``
  Target supports constructors with initialization priority arguments.

``inttypes_types``
  Target has the basic signed and unsigned types in ``inttypes.h``.
  This is for tests that GCC's notions of these types agree with those
  in the header, as some systems have only ``inttypes.h``.

``lax_strtofp``
  Target might have errors of a few ULP in string to floating-point
  conversion functions and overflow is not always detected correctly by
  those functions.

``mempcpy``
  Target provides ``mempcpy`` function.

``mmap``
  Target supports ``mmap``.

``newlib``
  Target supports Newlib.

``newlib_nano_io``
  GCC was configured with ``--enable-newlib-nano-formatted-io``, which reduces
  the code size of Newlib formatted I/O functions.

``pow10``
  Target provides ``pow10`` function.

``pthread``
  Target can compile using ``pthread.h`` with no errors or warnings.

``pthread_h``
  Target has ``pthread.h``.

``run_expensive_tests``
  Expensive testcases (usually those that consume excessive amounts of CPU
  time) should be run on this target.  This can be enabled by setting the
  :envvar:`GCC_TEST_RUN_EXPENSIVE` environment variable to a non-empty string.

``simulator``
  Test system runs executables on a simulator (i.e. slowly) rather than
  hardware (i.e. fast).

``signal``
  Target has ``signal.h``.

``stabs``
  Target supports the stabs debugging format.

``stdint_types``
  Target has the basic signed and unsigned C types in ``stdint.h``.
  This will be obsolete when GCC ensures a working ``stdint.h`` for
  all targets.

``stdint_types_mbig_endian``
  Target accepts the option :option:`-mbig-endian` and ``stdint.h``
  can be included without error when :option:`-mbig-endian` is passed.

``stpcpy``
  Target provides ``stpcpy`` function.

``sysconf``
  Target supports ``sysconf``.

``trampolines``
  Target supports trampolines.

``two_plus_gigs``
  Target supports linking programs with 2+GiB of data.

``uclibc``
  Target supports uClibc.

``unwrapped``
  Target does not use a status wrapper.

``vxworks_kernel``
  Target is a VxWorks kernel.

``vxworks_rtp``
  Target is a VxWorks RTP.

``wchar``
  Target supports wide characters.

Other attributes
~~~~~~~~~~~~~~~~

``R_flag_in_section``
  Target supports the 'R' flag in .section directive in assembly inputs.

``automatic_stack_alignment``
  Target supports automatic stack alignment.

``branch_cost``
  Target supports :option:`-branch-cost`:samp:`=N`.

``cxa_atexit``
  Target uses ``__cxa_atexit``.

``default_packed``
  .. _default_packed:
  Target has packed layout of structure members by default.

``exceptions``
  Target supports exceptions.

``exceptions_enabled``
  Target supports exceptions and they are enabled in the current
  testing configuration.

``fgraphite``
  Target supports Graphite optimizations.

``fixed_point``
  Target supports fixed-point extension to C.

``fopenacc``
  Target supports OpenACC via :option:`-fopenacc`.

``fopenmp``
  Target supports OpenMP via :option:`-fopenmp`.

``fpic``
  Target supports :option:`-fpic` and :option:`-fPIC`.

``freorder``
  Target supports :option:`-freorder-blocks-and-partition`.

``fstack_protector``
  Target supports :option:`-fstack-protector`.

``gas``
  Target uses GNU :command:`as`.

``gc_sections``
  Target supports :option:`--gc-sections`.

``gld``
  Target uses GNU :command:`ld`.

``keeps_null_pointer_checks``
  Target keeps null pointer checks, either due to the use of
  :option:`-fno-delete-null-pointer-checks` or hardwired into the target.

``llvm_binutils``
  Target is using an LLVM assembler and/or linker, instead of GNU Binutils.

``lra``
  Target supports local register allocator (LRA).

``lto``
  Compiler has been configured to support link-time optimization (LTO).

``lto_incremental``
  Compiler and linker support link-time optimization relocatable linking
  with :option:`-r` and :option:`-flto` options.

``naked_functions``
  Target supports the ``naked`` function attribute.

``named_sections``
  Target supports named sections.

``natural_alignment_32``
  Target uses natural alignment (aligned to type size) for types of
  32 bits or less.

``target_natural_alignment_64``
  Target uses natural alignment (aligned to type size) for types of
  64 bits or less.

``no_alignment_constraints``
  Target defines __BIGGEST_ALIGNMENT__=1.  Hence target imposes
  no alignment constraints.  This is similar, but not necessarily
  the same as :ref:`default_packed`.  Although ``BIGGEST_FIELD_ALIGNMENT``
  defaults to ``BIGGEST_ALIGNMENT`` for most targets, it is possible
  for a target to set those two with different values and have different
  alignment constraints for aggregate and non-aggregate types.

``noinit``
  Target supports the ``noinit`` variable attribute.

``nonpic``
  Target does not generate PIC by default.

``o_flag_in_section``
  Target supports the 'o' flag in .section directive in assembly inputs.

``offload_gcn``
  Target has been configured for OpenACC/OpenMP offloading on AMD GCN.

``persistent``
  Target supports the ``persistent`` variable attribute.

``pie_enabled``
  Target generates PIE by default.

``pcc_bitfield_type_matters``
  Target defines ``PCC_BITFIELD_TYPE_MATTERS``.

``pe_aligned_commons``
  Target supports :option:`-mpe-aligned-commons`.

``pie``
  Target supports :option:`-pie`, :option:`-fpie` and :option:`-fPIE`.

``rdynamic``
  Target supports :option:`-rdynamic`.

``scalar_all_fma``
  Target supports all four fused multiply-add optabs for both ``float``
  and ``double``.  These optabs are: ``fma_optab``, ``fms_optab``,
  ``fnma_optab`` and ``fnms_optab``.

``section_anchors``
  Target supports section anchors.

``short_enums``
  Target defaults to short enums.

``stack_size``
  .. _stack_size_et:
  Target has limited stack size.  The stack size limit can be obtained using the
  STACK_SIZE macro defined by :ref:`stack_size_ao`.

``static``
  Target supports :option:`-static`.

``static_libgfortran``
  Target supports statically linking :samp:`libgfortran`.

``string_merging``
  Target supports merging string constants at link time.

``ucn``
  Target supports compiling and assembling UCN.

``ucn_nocache``
  Including the options used to compile this particular test, the
  target supports compiling and assembling UCN.

``unaligned_stack``
  Target does not guarantee that its ``STACK_BOUNDARY`` is greater than
  or equal to the required vector alignment.

``vector_alignment_reachable``
  Vector alignment is reachable for types of 32 bits or less.

``vector_alignment_reachable_for_64bit``
  Vector alignment is reachable for types of 64 bits or less.

``wchar_t_char16_t_compatible``
  Target supports ``wchar_t`` that is compatible with ``char16_t``.

``wchar_t_char32_t_compatible``
  Target supports ``wchar_t`` that is compatible with ``char32_t``.

``comdat_group``
  Target uses comdat groups.

``indirect_calls``
  Target supports indirect calls, i.e. calls where the target is not
  constant.

``lgccjit``
  Target supports -lgccjit, i.e. libgccjit.so can be linked into jit tests.

``__OPTIMIZE__``
  Optimizations are enabled (``__OPTIMIZE__``) per the current
  compiler flags.

Local to tests in gcc.target/i386
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``3dnow``
  Target supports compiling ``3dnow`` instructions.

``aes``
  Target supports compiling ``aes`` instructions.

``fma4``
  Target supports compiling ``fma4`` instructions.

``mfentry``
  Target supports the ``-mfentry`` option that alters the
  position of profiling calls such that they precede the prologue.

``ms_hook_prologue``
  Target supports attribute ``ms_hook_prologue``.

``pclmul``
  Target supports compiling ``pclmul`` instructions.

``sse3``
  Target supports compiling ``sse3`` instructions.

``sse4``
  Target supports compiling ``sse4`` instructions.

``sse4a``
  Target supports compiling ``sse4a`` instructions.

``ssse3``
  Target supports compiling ``ssse3`` instructions.

``vaes``
  Target supports compiling ``vaes`` instructions.

``vpclmul``
  Target supports compiling ``vpclmul`` instructions.

``xop``
  Target supports compiling ``xop`` instructions.

Local to tests in gcc.test-framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``no``
  Always returns 0.

``yes``
  Always returns 1.

