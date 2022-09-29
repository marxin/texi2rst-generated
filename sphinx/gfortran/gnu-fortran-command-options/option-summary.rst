..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _option-summary:

Option summary
**************

Options
^^^^^^^

Here is a summary of all the options specific to GNU Fortran, grouped
by type.  Explanations are in the following sections.

*Fortran Language Options*
  See :ref:`fortran-dialect-options`.

  :option:`-fall-intrinsics` :option:`-fallow-argument-mismatch` :option:`-fallow-invalid-boz` 
  :option:`-fbackslash` :option:`-fcray-pointer` :option:`-fd-lines-as-code` :option:`-fd-lines-as-comments` 
  :option:`-fdec` :option:`-fdec-char-conversions` :option:`-fdec-structure` :option:`-fdec-intrinsic-ints` 
  :option:`-fdec-static` :option:`-fdec-math` :option:`-fdec-include` :option:`-fdec-format-defaults` 
  :option:`-fdec-blank-format-item` :option:`-fdefault-double-8` :option:`-fdefault-integer-8` 
  :option:`-fdefault-real-8` :option:`-fdefault-real-10` :option:`-fdefault-real-16` :option:`-fdollar-ok` 
  :option:`-ffixed-line-length-n` :option:`-ffixed-line-length-none` :option:`-fpad-source` 
  :option:`-ffree-form` :option:`-ffree-line-length-n` :option:`-ffree-line-length-none` 
  :option:`-fimplicit-none` :option:`-finteger-4-integer-8` :option:`-fmax-identifier-length` 
  :option:`-fmodule-private` :option:`-ffixed-form` :option:`-fno-range-check` :option:`-fopenacc` :option:`-fopenmp` 
  :option:`-freal-4-real-10` :option:`-freal-4-real-16` :option:`-freal-4-real-8` :option:`-freal-8-real-10` 
  :option:`-freal-8-real-16` :option:`-freal-8-real-4` :option:`-std=std` :option:`-ftest-forall-temp`

*Preprocessing Options*
  See :ref:`preprocessing-options`.

  :option:`-A-question` [= :samp:`{answer}` ]
  :option:`-Aquestion` = :samp:`{answer}` :option:`-C` :option:`-CC` :option:`-Dmacro` [= :samp:`{defn}` ]
  :option:`-H` :option:`-P` 
  :option:`-Umacro` :option:`-cpp` :option:`-dD` :option:`-dI` :option:`-dM` :option:`-dN` :option:`-dU` :option:`-fworking-directory`
  :option:`-imultilib` :samp:`{dir}` 
  :option:`-iprefix` :samp:`{file}` :option:`-iquote` :option:`-isysroot` :samp:`{dir}` :option:`-isystem` :samp:`{dir}` :option:`-nocpp` 
  :option:`-nostdinc` 
  :option:`-undef`

*Error and Warning Options*
  See :ref:`error-and-warning-options`.

  :option:`-Waliasing` :option:`-Wall` :option:`-Wampersand` :option:`-Warray-bounds` 
  :option:`-Wc-binding-type` :option:`-Wcharacter-truncation` :option:`-Wconversion` 
  :option:`-Wdo-subscript` :option:`-Wfunction-elimination` :option:`-Wimplicit-interface` 
  :option:`-Wimplicit-procedure` :option:`-Wintrinsic-shadow` :option:`-Wuse-without-only` 
  :option:`-Wintrinsics-std` :option:`-Wline-truncation` :option:`-Wno-align-commons` 
  :option:`-Wno-overwrite-recursive` :option:`-Wno-tabs` :option:`-Wreal-q-constant` :option:`-Wsurprising` 
  :option:`-Wunderflow` :option:`-Wunused-parameter` :option:`-Wrealloc-lhs` :option:`-Wrealloc-lhs-all` 
  :option:`-Wfrontend-loop-interchange` :option:`-Wtarget-lifetime` :option:`-fmax-errors=n` 
  :option:`-fsyntax-only` :option:`-pedantic` 
  :option:`-pedantic-errors` 

*Debugging Options*
  See :ref:`debugging-options`.

  :option:`-fbacktrace` :option:`-fdump-fortran-optimized` :option:`-fdump-fortran-original` 
  :option:`-fdebug-aux-vars` :option:`-fdump-fortran-global` :option:`-fdump-parse-tree` :option:`-ffpe-trap=list` 
  :option:`-ffpe-summary=list`

*Directory Options*
  See :ref:`directory-options`.

  :option:`-Idir`  :option:`-Jdir` :option:`-fintrinsic-modules-path` :samp:`{dir}`

*Link Options*
  See :ref:`link-options`.

  :option:`-static-libgfortran`  :option:`-static-libquadmath`

*Runtime Options*
  See :ref:`runtime-options`.

  :option:`-fconvert=conversion` :option:`-fmax-subrecord-length=length` 
  :option:`-frecord-marker=length` :option:`-fsign-zero`

*Interoperability Options*
  See :ref:`interoperability-options`.

  :option:`-fc-prototypes` :option:`-fc-prototypes-external`

*Code Generation Options*
  See :ref:`code-gen-options`.

  :option:`-faggressive-function-elimination` :option:`-fblas-matmul-limit=n` 
  :option:`-fbounds-check` :option:`-ftail-call-workaround` :option:`-ftail-call-workaround=n` 
  :option:`-fcheck-array-temporaries` 
  :option:`-fcheck=<all|array-temps|bits|bounds|do|mem|pointer|recursion>` 
  :option:`-fcoarray=<none|single|lib>` :option:`-fexternal-blas` :option:`-ff2c` 
  :option:`-ffrontend-loop-interchange` :option:`-ffrontend-optimize` 
  :option:`-finit-character=n` :option:`-finit-integer=n` :option:`-finit-local-zero` 
  :option:`-finit-derived` :option:`-finit-logical=<true|false>` 
  :option:`-finit-real=<zero|inf|-inf|nan|snan>`
  :option:`-finline-matmul-limit=n` 
  :option:`-finline-arg-packing` :option:`-fmax-array-constructor=n` 
  :option:`-fmax-stack-var-size=n` :option:`-fno-align-commons` :option:`-fno-automatic` 
  :option:`-fno-protect-parens` :option:`-fno-underscoring` :option:`-fsecond-underscore` 
  :option:`-fpack-derived` :option:`-frealloc-lhs` :option:`-frecursive` :option:`-frepack-arrays` 
  :option:`-fshort-enums` :option:`-fstack-arrays`

