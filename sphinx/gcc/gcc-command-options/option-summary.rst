..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _option-summary:

Option Summary
**************

Here is a summary of all the options, grouped by type.  Explanations are
in the following sections.

*Overall Options*
  See :ref:`overall-options`.

  :option:`-c`  :option:`-S`  :option:`-E`  :option:`-o` :samp:`{file}` 
  :option:`-dumpbase` :samp:`{dumpbase}`  :option:`-dumpbase-ext` :samp:`{auxdropsuf}` 
  :option:`-dumpdir` :samp:`{dumppfx}`  :option:`-x` :samp:`{language}`  
  :option:`-v`  :option:`-###`  :option:`--help` [= :samp:`{class}` [,...]]  :option:`--target-help`  :option:`--version` 
  :option:`-pass-exit-codes`  :option:`-pipe`  :option:`-specs=file`  :option:`-wrapper`  
  @ :samp:`{file}`  :option:`-ffile-prefix-map=old` = :samp:`{new}`  
  :option:`-fplugin=file`  :option:`-fplugin-arg-name` = :samp:`{arg}`  
  :option:`-fdump-ada-spec`  [:option:`-slim` ]  :option:`-fada-spec-parent=unit`  :option:`-fdump-go-spec=file`

*C Language Options*
  See :ref:`c-dialect-options`.

  :option:`-ansi`  :option:`-std=standard`  :option:`-aux-info` :samp:`{filename}` 
  :option:`-fallow-parameterless-variadic-functions`  :option:`-fno-asm`  
  :option:`-fno-builtin`  :option:`-fno-builtin-function`  :option:`-fcond-mismatch` 
  :option:`-ffreestanding`  :option:`-fgimple`  :option:`-fgnu-tm`  :option:`-fgnu89-inline`  :option:`-fhosted` 
  :option:`-flax-vector-conversions`  :option:`-fms-extensions` 
  :option:`-foffload=arg`  :option:`-foffload-options=arg` 
  :option:`-fopenacc`  :option:`-fopenacc-dim=geom` 
  :option:`-fopenmp`  :option:`-fopenmp-simd` 
  :option:`-fpermitted-flt-eval-methods=standard` 
  :option:`-fplan9-extensions`  :option:`-fsigned-bitfields`  :option:`-funsigned-bitfields` 
  :option:`-fsigned-char`  :option:`-funsigned-char`  :option:`-fsso-struct=endianness`

*C++ Language Options*
  See :ref:`c++-dialect-options`.

  :option:`-fabi-version=n`  :option:`-fno-access-control` 
  :option:`-faligned-new=n`  :option:`-fargs-in-order=n`  :option:`-fchar8_t`  :option:`-fcheck-new` 
  :option:`-fconstexpr-depth=n`  :option:`-fconstexpr-cache-depth=n` 
  :option:`-fconstexpr-loop-limit=n`  :option:`-fconstexpr-ops-limit=n` 
  :option:`-fno-elide-constructors` 
  :option:`-fno-enforce-eh-specs` 
  :option:`-fno-gnu-keywords` 
  :option:`-fno-implicit-templates` 
  :option:`-fno-implicit-inline-templates` 
  :option:`-fno-implement-inlines`  
  :option:`-fmodule-header` [= :samp:`{kind}` ] :option:`-fmodule-only` :option:`-fmodules-ts` 
  :option:`-fmodule-implicit-inline` 
  :option:`-fno-module-lazy` 
  :option:`-fmodule-mapper=specification` 
  :option:`-fmodule-version-ignore` 
  :option:`-fms-extensions` 
  :option:`-fnew-inheriting-ctors` 
  :option:`-fnew-ttp-matching` 
  :option:`-fno-nonansi-builtins`  :option:`-fnothrow-opt`  :option:`-fno-operator-names` 
  :option:`-fno-optional-diags`  :option:`-fpermissive` 
  :option:`-fno-pretty-templates` 
  :option:`-fno-rtti`  :option:`-fsized-deallocation` 
  :option:`-ftemplate-backtrace-limit=n` 
  :option:`-ftemplate-depth=n` 
  :option:`-fno-threadsafe-statics`  :option:`-fuse-cxa-atexit` 
  :option:`-fno-weak`  :option:`-nostdinc++` 
  :option:`-fvisibility-inlines-hidden` 
  :option:`-fvisibility-ms-compat` 
  :option:`-fext-numeric-literals` 
  :option:`-flang-info-include-translate` [= :samp:`{header}` ] 
  :option:`-flang-info-include-translate-not` 
  :option:`-flang-info-module-cmi` [= :samp:`{module}` ] 
  :option:`-stdlib=libstdc++,libc++` 
  :option:`-Wabi-tag`  :option:`-Wcatch-value`  :option:`-Wcatch-value=n` 
  :option:`-Wno-class-conversion`  :option:`-Wclass-memaccess` 
  :option:`-Wcomma-subscript`  :option:`-Wconditionally-supported` 
  :option:`-Wno-conversion-null`  :option:`-Wctad-maybe-unsupported` 
  :option:`-Wctor-dtor-privacy`  :option:`-Wno-delete-incomplete` 
  :option:`-Wdelete-non-virtual-dtor`  :option:`-Wno-deprecated-array-compare` 
  :option:`-Wdeprecated-copy` :option:`-Wdeprecated-copy-dtor` 
  :option:`-Wno-deprecated-enum-enum-conversion` :option:`-Wno-deprecated-enum-float-conversion` 
  :option:`-Weffc++`  :option:`-Wno-exceptions` :option:`-Wextra-semi`  :option:`-Wno-inaccessible-base` 
  :option:`-Wno-inherited-variadic-ctor`  :option:`-Wno-init-list-lifetime` 
  :option:`-Winvalid-imported-macros` 
  :option:`-Wno-invalid-offsetof`  :option:`-Wno-literal-suffix` 
  :option:`-Wmismatched-new-delete` :option:`-Wmismatched-tags` 
  :option:`-Wmultiple-inheritance`  :option:`-Wnamespaces`  :option:`-Wnarrowing` 
  :option:`-Wnoexcept`  :option:`-Wnoexcept-type`  :option:`-Wnon-virtual-dtor` 
  :option:`-Wpessimizing-move`  :option:`-Wno-placement-new`  :option:`-Wplacement-new=n` 
  :option:`-Wrange-loop-construct` :option:`-Wredundant-move` :option:`-Wredundant-tags` 
  :option:`-Wreorder`  :option:`-Wregister` 
  :option:`-Wstrict-null-sentinel`  :option:`-Wno-subobject-linkage`  :option:`-Wtemplates` 
  :option:`-Wno-non-template-friend`  :option:`-Wold-style-cast` 
  :option:`-Woverloaded-virtual`  :option:`-Wno-pmf-conversions` :option:`-Wself-move` :option:`-Wsign-promo` 
  :option:`-Wsized-deallocation`  :option:`-Wsuggest-final-methods` 
  :option:`-Wsuggest-final-types`  :option:`-Wsuggest-override`  
  :option:`-Wno-terminate`  :option:`-Wuseless-cast`  :option:`-Wno-vexing-parse`  
  :option:`-Wvirtual-inheritance`  
  :option:`-Wno-virtual-move-assign`  :option:`-Wvolatile`  :option:`-Wzero-as-null-pointer-constant`

*Objective-C and Objective-C++ Language Options*
  See :ref:`objective-c-and-objective-c++-dialect-options`.

  :option:`-fconstant-string-class=class-name` 
  :option:`-fgnu-runtime`  :option:`-fnext-runtime` 
  :option:`-fno-nil-receivers` 
  :option:`-fobjc-abi-version=n` 
  :option:`-fobjc-call-cxx-cdtors` 
  :option:`-fobjc-direct-dispatch` 
  :option:`-fobjc-exceptions` 
  :option:`-fobjc-gc` 
  :option:`-fobjc-nilcheck` 
  :option:`-fobjc-std=objc1` 
  :option:`-fno-local-ivars` 
  :option:`-fivar-visibility=` [public|protected|private|package] 
  :option:`-freplace-objc-classes` 
  :option:`-fzero-link` 
  :option:`-gen-decls` 
  :option:`-Wassign-intercept`  :option:`-Wno-property-assign-default` 
  :option:`-Wno-protocol` :option:`-Wobjc-root-class` :option:`-Wselector` 
  :option:`-Wstrict-selector-match` 
  :option:`-Wundeclared-selector`

*Diagnostic Message Formatting Options*
  See :ref:`diagnostic-message-formatting-options`.

  :option:`-fmessage-length=n`  
  :option:`-fdiagnostics-plain-output` 
  :option:`-fdiagnostics-show-location=` [once|every :option:`-line` ]  
  :option:`-fdiagnostics-color=` [auto|never|always]  
  :option:`-fdiagnostics-urls=` [auto|never|always]  
  :option:`-fdiagnostics-format=` [text|sarif :option:`-stderr` |sarif :option:`-file` |json|json :option:`-stderr` |json :option:`-file` ]  
  :option:`-fno-diagnostics-show-option`  :option:`-fno-diagnostics-show-caret` 
  :option:`-fno-diagnostics-show-labels`  :option:`-fno-diagnostics-show-line-numbers` 
  :option:`-fno-diagnostics-show-cwe`  
  :option:`-fno-diagnostics-show-rule`  
  :option:`-fdiagnostics-minimum-margin-width=width` 
  :option:`-fdiagnostics-parseable-fixits`  :option:`-fdiagnostics-generate-patch` 
  :option:`-fdiagnostics-show-template-tree`  :option:`-fno-elide-type` 
  :option:`-fdiagnostics-path-format=` [none|separate :option:`-events` |inline :option:`-events` ] 
  :option:`-fdiagnostics-show-path-depths` 
  :option:`-fno-show-column` 
  :option:`-fdiagnostics-column-unit=` [display|byte] 
  :option:`-fdiagnostics-column-origin=origin` 
  :option:`-fdiagnostics-escape-format=` [unicode|bytes]

*Warning Options*
  See :ref:`warning-options`.

  :option:`-fsyntax-only`  :option:`-fmax-errors=n`  :option:`-Wpedantic` 
  :option:`-pedantic-errors` 
  :option:`-w`  :option:`-Wextra`  :option:`-Wall`  :option:`-Wabi=n` 
  :option:`-Waddress`  :option:`-Wno-address-of-packed-member`  :option:`-Waggregate-return` 
  :option:`-Walloc-size-larger-than=byte-size`  :option:`-Walloc-zero` 
  :option:`-Walloca`  :option:`-Walloca-larger-than=byte-size` 
  :option:`-Wno-aggressive-loop-optimizations` 
  :option:`-Warith-conversion` 
  :option:`-Warray-bounds`  :option:`-Warray-bounds=n`  :option:`-Warray-compare` 
  :option:`-Wno-attributes`  :option:`-Wattribute-alias=n` :option:`-Wno-attribute-alias` 
  :option:`-Wno-attribute-warning`  
  :option:`-Wbidi-chars=` [none|unpaired|any|ucn] 
  :option:`-Wbool-compare`  :option:`-Wbool-operation` 
  :option:`-Wno-builtin-declaration-mismatch` 
  :option:`-Wno-builtin-macro-redefined`  :option:`-Wc90-c99-compat`  :option:`-Wc99-c11-compat` 
  :option:`-Wc11-c2x-compat` 
  :option:`-Wc++-compat`  :option:`-Wc++11-compat`  :option:`-Wc++14-compat`  :option:`-Wc++17-compat`  
  :option:`-Wc++20-compat`   
  :option:`-Wno-c++11-extensions`  :option:`-Wno-c++14-extensions` :option:`-Wno-c++17-extensions`  
  :option:`-Wno-c++20-extensions`  :option:`-Wno-c++23-extensions`  
  :option:`-Wcast-align`  :option:`-Wcast-align=strict`  :option:`-Wcast-function-type`  :option:`-Wcast-qual`  
  :option:`-Wchar-subscripts` 
  :option:`-Wclobbered`  :option:`-Wcomment` 
  :option:`-Wconversion`  :option:`-Wno-coverage-mismatch`  :option:`-Wno-cpp` 
  :option:`-Wdangling-else`  :option:`-Wdangling-pointer`  :option:`-Wdangling-pointer=n`  
  :option:`-Wdate-time` 
  :option:`-Wno-deprecated`  :option:`-Wno-deprecated-declarations`  :option:`-Wno-designated-init` 
  :option:`-Wdisabled-optimization` 
  :option:`-Wno-discarded-array-qualifiers`  :option:`-Wno-discarded-qualifiers` 
  :option:`-Wno-div-by-zero`  :option:`-Wdouble-promotion` 
  :option:`-Wduplicated-branches`  :option:`-Wduplicated-cond` 
  :option:`-Wempty-body`  :option:`-Wno-endif-labels`  :option:`-Wenum-compare`  :option:`-Wenum-conversion` 
  :option:`-Wenum-int-mismatch` 
  :option:`-Werror`  :option:`-Werror=*`  :option:`-Wexpansion-to-defined`  :option:`-Wfatal-errors` 
  :option:`-Wfloat-conversion`  :option:`-Wfloat-equal`  :option:`-Wformat`  :option:`-Wformat=2` 
  :option:`-Wno-format-contains-nul`  :option:`-Wno-format-extra-args`  
  :option:`-Wformat-nonliteral`  :option:`-Wformat-overflow=n` 
  :option:`-Wformat-security`  :option:`-Wformat-signedness`  :option:`-Wformat-truncation=n` 
  :option:`-Wformat-y2k`  :option:`-Wframe-address` 
  :option:`-Wframe-larger-than=byte-size`  :option:`-Wno-free-nonheap-object` 
  :option:`-Wno-if-not-aligned`  :option:`-Wno-ignored-attributes` 
  :option:`-Wignored-qualifiers`  :option:`-Wno-incompatible-pointer-types` 
  :option:`-Wimplicit`  :option:`-Wimplicit-fallthrough`  :option:`-Wimplicit-fallthrough=n` 
  :option:`-Wno-implicit-function-declaration`  :option:`-Wno-implicit-int` 
  :option:`-Winfinite-recursion` 
  :option:`-Winit-self`  :option:`-Winline`  :option:`-Wno-int-conversion`  :option:`-Wint-in-bool-context` 
  :option:`-Wno-int-to-pointer-cast`  :option:`-Wno-invalid-memory-model` 
  :option:`-Winvalid-pch`  :option:`-Winvalid-utf8`  :option:`-Wno-unicode`  :option:`-Wjump-misses-init`  
  :option:`-Wlarger-than=byte-size`  :option:`-Wlogical-not-parentheses`  :option:`-Wlogical-op`  
  :option:`-Wlong-long`  :option:`-Wno-lto-type-mismatch` :option:`-Wmain`  :option:`-Wmaybe-uninitialized` 
  :option:`-Wmemset-elt-size`  :option:`-Wmemset-transposed-args` 
  :option:`-Wmisleading-indentation`  :option:`-Wmissing-attributes`  :option:`-Wmissing-braces` 
  :option:`-Wmissing-field-initializers`  :option:`-Wmissing-format-attribute` 
  :option:`-Wmissing-include-dirs`  :option:`-Wmissing-noreturn`  :option:`-Wno-missing-profile` 
  :option:`-Wno-multichar`  :option:`-Wmultistatement-macros`  :option:`-Wnonnull`  :option:`-Wnonnull-compare` 
  :option:`-Wnormalized=` [none|id|nfc|nfkc] 
  :option:`-Wnull-dereference`  :option:`-Wno-odr`  
  :option:`-Wopenacc-parallelism`  
  :option:`-Wopenmp-simd`  
  :option:`-Wno-overflow`  :option:`-Woverlength-strings`  :option:`-Wno-override-init-side-effects` 
  :option:`-Wpacked`  :option:`-Wno-packed-bitfield-compat`  :option:`-Wpacked-not-aligned`  :option:`-Wpadded` 
  :option:`-Wparentheses`  :option:`-Wno-pedantic-ms-format` 
  :option:`-Wpointer-arith`  :option:`-Wno-pointer-compare`  :option:`-Wno-pointer-to-int-cast` 
  :option:`-Wno-pragmas`  :option:`-Wno-prio-ctor-dtor`  :option:`-Wredundant-decls` 
  :option:`-Wrestrict`  :option:`-Wno-return-local-addr`  :option:`-Wreturn-type` 
  :option:`-Wno-scalar-storage-order`  :option:`-Wsequence-point` 
  :option:`-Wshadow`  :option:`-Wshadow=global`  :option:`-Wshadow=local`  :option:`-Wshadow=compatible-local` 
  :option:`-Wno-shadow-ivar` 
  :option:`-Wno-shift-count-negative`  :option:`-Wno-shift-count-overflow`  :option:`-Wshift-negative-value` 
  :option:`-Wno-shift-overflow`  :option:`-Wshift-overflow=n` 
  :option:`-Wsign-compare`  :option:`-Wsign-conversion` 
  :option:`-Wno-sizeof-array-argument` 
  :option:`-Wsizeof-array-div` 
  :option:`-Wsizeof-pointer-div`  :option:`-Wsizeof-pointer-memaccess` 
  :option:`-Wstack-protector`  :option:`-Wstack-usage=byte-size`  :option:`-Wstrict-aliasing` 
  :option:`-Wstrict-aliasing=n`  :option:`-Wstrict-overflow`  :option:`-Wstrict-overflow=n` 
  :option:`-Wstring-compare` 
  :option:`-Wno-stringop-overflow` :option:`-Wno-stringop-overread` 
  :option:`-Wno-stringop-truncation` 
  :option:`-Wsuggest-attribute=` [pure|const|noreturn|format|malloc] 
  :option:`-Wswitch`  :option:`-Wno-switch-bool`  :option:`-Wswitch-default`  :option:`-Wswitch-enum` 
  :option:`-Wno-switch-outside-range`  :option:`-Wno-switch-unreachable`  :option:`-Wsync-nand` 
  :option:`-Wsystem-headers`  :option:`-Wtautological-compare`  :option:`-Wtrampolines`  :option:`-Wtrigraphs` 
  :option:`-Wtrivial-auto-var-init` :option:`-Wtsan` :option:`-Wtype-limits`  :option:`-Wundef` 
  :option:`-Wuninitialized`  :option:`-Wunknown-pragmas` 
  :option:`-Wunsuffixed-float-constants`  :option:`-Wunused` 
  :option:`-Wunused-but-set-parameter`  :option:`-Wunused-but-set-variable` 
  :option:`-Wunused-const-variable`  :option:`-Wunused-const-variable=n` 
  :option:`-Wunused-function`  :option:`-Wunused-label`  :option:`-Wunused-local-typedefs` 
  :option:`-Wunused-macros` 
  :option:`-Wunused-parameter`  :option:`-Wno-unused-result` 
  :option:`-Wunused-value`  :option:`-Wunused-variable` 
  :option:`-Wno-varargs`  :option:`-Wvariadic-macros` 
  :option:`-Wvector-operation-performance` 
  :option:`-Wvla`  :option:`-Wvla-larger-than=byte-size`  :option:`-Wno-vla-larger-than` 
  :option:`-Wvolatile-register-var`  :option:`-Wwrite-strings` 
  :option:`-Wxor-used-as-pow` 
  :option:`-Wzero-length-bounds`

*Static Analyzer Options*
  :option:`-fanalyzer` 
  :option:`-fanalyzer-call-summaries` 
  :option:`-fanalyzer-checker=name` 
  :option:`-fno-analyzer-feasibility` 
  :option:`-fanalyzer-fine-grained` 
  :option:`-fno-analyzer-state-merge` 
  :option:`-fno-analyzer-state-purge` 
  :option:`-fanalyzer-transitivity` 
  :option:`-fno-analyzer-undo-inlining` 
  :option:`-fanalyzer-verbose-edges` 
  :option:`-fanalyzer-verbose-state-changes` 
  :option:`-fanalyzer-verbosity=level` 
  :option:`-fdump-analyzer` 
  :option:`-fdump-analyzer-callgraph` 
  :option:`-fdump-analyzer-exploded-graph` 
  :option:`-fdump-analyzer-exploded-nodes` 
  :option:`-fdump-analyzer-exploded-nodes-2` 
  :option:`-fdump-analyzer-exploded-nodes-3` 
  :option:`-fdump-analyzer-exploded-paths` 
  :option:`-fdump-analyzer-feasibility` 
  :option:`-fdump-analyzer-json` 
  :option:`-fdump-analyzer-state-purge` 
  :option:`-fdump-analyzer-stderr` 
  :option:`-fdump-analyzer-supergraph` 
  :option:`-fdump-analyzer-untracked` 
  :option:`-Wno-analyzer-double-fclose` 
  :option:`-Wno-analyzer-double-free` 
  :option:`-Wno-analyzer-exposure-through-output-file` 
  :option:`-Wno-analyzer-exposure-through-uninit-copy` 
  :option:`-Wno-analyzer-fd-access-mode-mismatch` 
  :option:`-Wno-analyzer-fd-double-close` 
  :option:`-Wno-analyzer-fd-leak` 
  :option:`-Wno-analyzer-fd-use-after-close` 
  :option:`-Wno-analyzer-fd-use-without-check` 
  :option:`-Wno-analyzer-file-leak` 
  :option:`-Wno-analyzer-free-of-non-heap` 
  :option:`-Wno-analyzer-imprecise-fp-arithmetic` 
  :option:`-Wno-analyzer-jump-through-null` 
  :option:`-Wno-analyzer-malloc-leak` 
  :option:`-Wno-analyzer-mismatching-deallocation` 
  :option:`-Wno-analyzer-null-argument` 
  :option:`-Wno-analyzer-null-dereference` 
  :option:`-Wno-analyzer-out-of-bounds` 
  :option:`-Wno-analyzer-possible-null-argument` 
  :option:`-Wno-analyzer-possible-null-dereference` 
  :option:`-Wno-analyzer-putenv-of-auto-var` 
  :option:`-Wno-analyzer-shift-count-negative` 
  :option:`-Wno-analyzer-shift-count-overflow` 
  :option:`-Wno-analyzer-stale-setjmp-buffer` 
  :option:`-Wno-analyzer-tainted-allocation-size` 
  :option:`-Wno-analyzer-tainted-array-index` 
  :option:`-Wno-analyzer-tainted-divisor` 
  :option:`-Wno-analyzer-tainted-offset` 
  :option:`-Wno-analyzer-tainted-size` 
  :option:`-Wanalyzer-too-complex` 
  :option:`-Wno-analyzer-unsafe-call-within-signal-handler` 
  :option:`-Wno-analyzer-use-after-free` 
  :option:`-Wno-analyzer-use-of-pointer-in-stale-stack-frame` 
  :option:`-Wno-analyzer-use-of-uninitialized-value` 
  :option:`-Wno-analyzer-va-arg-type-mismatch` 
  :option:`-Wno-analyzer-va-list-exhausted` 
  :option:`-Wno-analyzer-va-list-leak` 
  :option:`-Wno-analyzer-va-list-use-after-va-end` 
  :option:`-Wno-analyzer-write-to-const` 
  :option:`-Wno-analyzer-write-to-string-literal` 

*C and Objective-C-only Warning Options*
  :option:`-Wbad-function-cast`  :option:`-Wmissing-declarations` 
  :option:`-Wmissing-parameter-type`  :option:`-Wmissing-prototypes`  :option:`-Wnested-externs` 
  :option:`-Wold-style-declaration`  :option:`-Wold-style-definition` 
  :option:`-Wstrict-prototypes`  :option:`-Wtraditional`  :option:`-Wtraditional-conversion` 
  :option:`-Wdeclaration-after-statement`  :option:`-Wpointer-sign`

*Debugging Options*
  See :ref:`debugging-options`.

  :option:`-g`  :option:`-glevel`  :option:`-gdwarf`  :option:`-gdwarf-version` 
  :option:`-gbtf` :option:`-gctf`  :option:`-gctflevel` 
  :option:`-ggdb`  :option:`-grecord-gcc-switches`  :option:`-gno-record-gcc-switches` 
  :option:`-gstrict-dwarf`  :option:`-gno-strict-dwarf` 
  :option:`-gas-loc-support`  :option:`-gno-as-loc-support` 
  :option:`-gas-locview-support`  :option:`-gno-as-locview-support` 
  :option:`-gcolumn-info`  :option:`-gno-column-info`  :option:`-gdwarf32`  :option:`-gdwarf64` 
  :option:`-gstatement-frontiers`  :option:`-gno-statement-frontiers` 
  :option:`-gvariable-location-views`  :option:`-gno-variable-location-views` 
  :option:`-ginternal-reset-location-views`  :option:`-gno-internal-reset-location-views` 
  :option:`-ginline-points`  :option:`-gno-inline-points` 
  :option:`-gvms` :option:`-gz` [= :samp:`{type}` ] 
  :option:`-gsplit-dwarf`  :option:`-gdescribe-dies`  :option:`-gno-describe-dies` 
  :option:`-fdebug-prefix-map=old` = :samp:`{new}`  :option:`-fdebug-types-section` 
  :option:`-fno-eliminate-unused-debug-types` 
  :option:`-femit-struct-debug-baseonly`  :option:`-femit-struct-debug-reduced` 
  :option:`-femit-struct-debug-detailed` [= :samp:`{spec-list}` ] 
  :option:`-fno-eliminate-unused-debug-symbols`  :option:`-femit-class-debug-always` 
  :option:`-fno-merge-debug-strings`  :option:`-fno-dwarf2-cfi-asm` 
  :option:`-fvar-tracking`  :option:`-fvar-tracking-assignments`

*Optimization Options*
  See :ref:`optimize-options`.

  :option:`-faggressive-loop-optimizations` 
  :option:`-falign-functions[=n` [: :samp:`{m}` :[ :samp:`{n2}` [: :samp:`{m2}` ]]]] 
  :option:`-falign-jumps[=n` [: :samp:`{m}` :[ :samp:`{n2}` [: :samp:`{m2}` ]]]] 
  :option:`-falign-labels[=n` [: :samp:`{m}` :[ :samp:`{n2}` [: :samp:`{m2}` ]]]] 
  :option:`-falign-loops[=n` [: :samp:`{m}` :[ :samp:`{n2}` [: :samp:`{m2}` ]]]] 
  :option:`-fno-allocation-dce` :option:`-fallow-store-data-races` 
  :option:`-fassociative-math`  :option:`-fauto-profile`  :option:`-fauto-profile[=path` ] 
  :option:`-fauto-inc-dec`  :option:`-fbranch-probabilities` 
  :option:`-fcaller-saves` 
  :option:`-fcombine-stack-adjustments`  :option:`-fconserve-stack` 
  :option:`-fcompare-elim`  :option:`-fcprop-registers`  :option:`-fcrossjumping` 
  :option:`-fcse-follow-jumps`  :option:`-fcse-skip-blocks`  :option:`-fcx-fortran-rules` 
  :option:`-fcx-limited-range` 
  :option:`-fdata-sections`  :option:`-fdce`  :option:`-fdelayed-branch` 
  :option:`-fdelete-null-pointer-checks`  :option:`-fdevirtualize`  :option:`-fdevirtualize-speculatively` 
  :option:`-fdevirtualize-at-ltrans`  :option:`-fdse` 
  :option:`-fearly-inlining`  :option:`-fipa-sra`  :option:`-fexpensive-optimizations`  :option:`-ffat-lto-objects` 
  :option:`-ffast-math`  :option:`-ffinite-math-only`  :option:`-ffloat-store`  :option:`-fexcess-precision=style` 
  :option:`-ffinite-loops` 
  :option:`-fforward-propagate`  :option:`-ffp-contract=style`  :option:`-ffunction-sections` 
  :option:`-fgcse`  :option:`-fgcse-after-reload`  :option:`-fgcse-las`  :option:`-fgcse-lm`  :option:`-fgraphite-identity` 
  :option:`-fgcse-sm`  :option:`-fhoist-adjacent-loads`  :option:`-fif-conversion` 
  :option:`-fif-conversion2`  :option:`-findirect-inlining` 
  :option:`-finline-functions`  :option:`-finline-functions-called-once`  :option:`-finline-limit=n` 
  :option:`-finline-small-functions` :option:`-fipa-modref` :option:`-fipa-cp`  :option:`-fipa-cp-clone` 
  :option:`-fipa-bit-cp`  :option:`-fipa-vrp`  :option:`-fipa-pta`  :option:`-fipa-profile`  :option:`-fipa-pure-const` 
  :option:`-fipa-reference`  :option:`-fipa-reference-addressable` 
  :option:`-fipa-stack-alignment`  :option:`-fipa-icf`  :option:`-fira-algorithm=algorithm` 
  :option:`-flive-patching=level` 
  :option:`-fira-region=region`  :option:`-fira-hoist-pressure` 
  :option:`-fira-loop-pressure`  :option:`-fno-ira-share-save-slots` 
  :option:`-fno-ira-share-spill-slots` 
  :option:`-fisolate-erroneous-paths-dereference`  :option:`-fisolate-erroneous-paths-attribute` 
  :option:`-fivopts`  :option:`-fkeep-inline-functions`  :option:`-fkeep-static-functions` 
  :option:`-fkeep-static-consts`  :option:`-flimit-function-alignment`  :option:`-flive-range-shrinkage` 
  :option:`-floop-block`  :option:`-floop-interchange`  :option:`-floop-strip-mine` 
  :option:`-floop-unroll-and-jam`  :option:`-floop-nest-optimize` 
  :option:`-floop-parallelize-all`  :option:`-flra-remat`  :option:`-flto`  :option:`-flto-compression-level` 
  :option:`-flto-partition=alg`  :option:`-fmerge-all-constants` 
  :option:`-fmerge-constants`  :option:`-fmodulo-sched`  :option:`-fmodulo-sched-allow-regmoves` 
  :option:`-fmove-loop-invariants`  :option:`-fmove-loop-stores`  :option:`-fno-branch-count-reg` 
  :option:`-fno-defer-pop`  :option:`-fno-fp-int-builtin-inexact`  :option:`-fno-function-cse` 
  :option:`-fno-guess-branch-probability`  :option:`-fno-inline`  :option:`-fno-math-errno`  :option:`-fno-peephole` 
  :option:`-fno-peephole2`  :option:`-fno-printf-return-value`  :option:`-fno-sched-interblock` 
  :option:`-fno-sched-spec`  :option:`-fno-signed-zeros` 
  :option:`-fno-toplevel-reorder`  :option:`-fno-trapping-math`  :option:`-fno-zero-initialized-in-bss` 
  :option:`-fomit-frame-pointer`  :option:`-foptimize-sibling-calls` 
  :option:`-fpartial-inlining`  :option:`-fpeel-loops`  :option:`-fpredictive-commoning` 
  :option:`-fprefetch-loop-arrays` 
  :option:`-fprofile-correction` 
  :option:`-fprofile-use`  :option:`-fprofile-use=path` :option:`-fprofile-partial-training` 
  :option:`-fprofile-values` :option:`-fprofile-reorder-functions` 
  :option:`-freciprocal-math`  :option:`-free`  :option:`-frename-registers`  :option:`-freorder-blocks` 
  :option:`-freorder-blocks-algorithm=algorithm` 
  :option:`-freorder-blocks-and-partition`  :option:`-freorder-functions` 
  :option:`-frerun-cse-after-loop`  :option:`-freschedule-modulo-scheduled-loops` 
  :option:`-frounding-math`  :option:`-fsave-optimization-record` 
  :option:`-fsched2-use-superblocks`  :option:`-fsched-pressure` 
  :option:`-fsched-spec-load`  :option:`-fsched-spec-load-dangerous` 
  :option:`-fsched-stalled-insns-dep[=n` ]  :option:`-fsched-stalled-insns[=n` ] 
  :option:`-fsched-group-heuristic`  :option:`-fsched-critical-path-heuristic` 
  :option:`-fsched-spec-insn-heuristic`  :option:`-fsched-rank-heuristic` 
  :option:`-fsched-last-insn-heuristic`  :option:`-fsched-dep-count-heuristic` 
  :option:`-fschedule-fusion` 
  :option:`-fschedule-insns`  :option:`-fschedule-insns2`  :option:`-fsection-anchors` 
  :option:`-fselective-scheduling`  :option:`-fselective-scheduling2` 
  :option:`-fsel-sched-pipelining`  :option:`-fsel-sched-pipelining-outer-loops` 
  :option:`-fsemantic-interposition`  :option:`-fshrink-wrap`  :option:`-fshrink-wrap-separate` 
  :option:`-fsignaling-nans` 
  :option:`-fsingle-precision-constant`  :option:`-fsplit-ivs-in-unroller`  :option:`-fsplit-loops`
  :option:`-fsplit-paths` 
  :option:`-fsplit-wide-types`  :option:`-fsplit-wide-types-early`  :option:`-fssa-backprop`  :option:`-fssa-phiopt` 
  :option:`-fstdarg-opt`  :option:`-fstore-merging`  :option:`-fstrict-aliasing` :option:`-fipa-strict-aliasing` 
  :option:`-fthread-jumps`  :option:`-ftracer`  :option:`-ftree-bit-ccp` 
  :option:`-ftree-builtin-call-dce`  :option:`-ftree-ccp`  :option:`-ftree-ch` 
  :option:`-ftree-coalesce-vars`  :option:`-ftree-copy-prop`  :option:`-ftree-dce`  :option:`-ftree-dominator-opts` 
  :option:`-ftree-dse`  :option:`-ftree-forwprop`  :option:`-ftree-fre`  :option:`-fcode-hoisting` 
  :option:`-ftree-loop-if-convert`  :option:`-ftree-loop-im` 
  :option:`-ftree-phiprop`  :option:`-ftree-loop-distribution`  :option:`-ftree-loop-distribute-patterns` 
  :option:`-ftree-loop-ivcanon`  :option:`-ftree-loop-linear`  :option:`-ftree-loop-optimize` 
  :option:`-ftree-loop-vectorize` 
  :option:`-ftree-parallelize-loops=n`  :option:`-ftree-pre`  :option:`-ftree-partial-pre`  :option:`-ftree-pta` 
  :option:`-ftree-reassoc`  :option:`-ftree-scev-cprop`  :option:`-ftree-sink`  :option:`-ftree-slsr`  :option:`-ftree-sra` 
  :option:`-ftree-switch-conversion`  :option:`-ftree-tail-merge` 
  :option:`-ftree-ter`  :option:`-ftree-vectorize`  :option:`-ftree-vrp`  :option:`-ftrivial-auto-var-init` 
  :option:`-funconstrained-commons` :option:`-funit-at-a-time`  :option:`-funroll-all-loops` 
  :option:`-funroll-loops` :option:`-funsafe-math-optimizations`  :option:`-funswitch-loops` 
  :option:`-fipa-ra`  :option:`-fvariable-expansion-in-unroller`  :option:`-fvect-cost-model`  :option:`-fvpt` 
  :option:`-fweb`  :option:`-fwhole-program`  :option:`-fwpa`  :option:`-fuse-linker-plugin` :option:`-fzero-call-used-regs` 
  :option:`--param` :samp:`{name}` = :samp:`{value}`
  :option:`-O`  :option:`-O0`  :option:`-O1`  :option:`-O2`  :option:`-O3`  :option:`-Os`  :option:`-Ofast`  :option:`-Og`  :option:`-Oz`

*Program Instrumentation Options*
  See :ref:`instrumentation-options`.

  :option:`-p`  :option:`-pg`  :option:`-fprofile-arcs`  :option:`--coverage`  :option:`-ftest-coverage` 
  :option:`-fprofile-abs-path` 
  :option:`-fprofile-dir=path`  :option:`-fprofile-generate`  :option:`-fprofile-generate=path` 
  :option:`-fprofile-info-section`  :option:`-fprofile-info-section=name` 
  :option:`-fprofile-note=path` :option:`-fprofile-prefix-path=path` 
  :option:`-fprofile-update=method` :option:`-fprofile-filter-files=regex` 
  :option:`-fprofile-exclude-files=regex` 
  :option:`-fprofile-reproducible=` [multithreaded|parallel :option:`-runs` |serial] 
  :option:`-fsanitize=style`  :option:`-fsanitize-recover`  :option:`-fsanitize-recover=style` 
  :option:`-fsanitize-trap`   :option:`-fsanitize-trap=style`  
  :option:`-fasan-shadow-offset=number`  :option:`-fsanitize-sections=s1`, :samp:`{s2}`,... 
  :option:`-fsanitize-undefined-trap-on-error`  :option:`-fbounds-check` 
  :option:`-fcf-protection=` [full|branch|return|none|check] 
  :option:`-fharden-compares` :option:`-fharden-conditional-branches` 
  :option:`-fstack-protector`  :option:`-fstack-protector-all`  :option:`-fstack-protector-strong` 
  :option:`-fstack-protector-explicit`  :option:`-fstack-check` 
  :option:`-fstack-limit-register=reg`  :option:`-fstack-limit-symbol=sym` 
  :option:`-fno-stack-limit`  :option:`-fsplit-stack` 
  :option:`-fvtable-verify=` [std|preinit|none] 
  :option:`-fvtv-counts`  :option:`-fvtv-debug` 
  :option:`-finstrument-functions`  :option:`-finstrument-functions-once` 
  :option:`-finstrument-functions-exclude-function-list=sym`, :samp:`{sym}`,... 
  :option:`-finstrument-functions-exclude-file-list=file`, :samp:`{file}`,...
  -fprofile-prefix-map= :samp:`{old}` = :samp:`{new}`

*Preprocessor Options*
  See :ref:`preprocessor-options`.

  :option:`-Aquestion` = :samp:`{answer}` 
  :option:`-A-question` [= :samp:`{answer}` ] 
  :option:`-C`  :option:`-CC`  :option:`-Dmacro` [= :samp:`{defn}` ] 
  :option:`-dD`  :option:`-dI`  :option:`-dM`  :option:`-dN`  :option:`-dU` 
  :option:`-fdebug-cpp`  :option:`-fdirectives-only`  :option:`-fdollars-in-identifiers`  
  :option:`-fexec-charset=charset`  :option:`-fextended-identifiers`  
  :option:`-finput-charset=charset`  :option:`-flarge-source-files`  
  :option:`-fmacro-prefix-map=old` = :samp:`{new}` :option:`-fmax-include-depth=depth` 
  :option:`-fno-canonical-system-headers`  :option:`-fpch-deps`  :option:`-fpch-preprocess`  
  :option:`-fpreprocessed`  :option:`-ftabstop=width`  :option:`-ftrack-macro-expansion`  
  :option:`-fwide-exec-charset=charset`  :option:`-fworking-directory` 
  :option:`-H`  :option:`-imacros` :samp:`{file}`  :option:`-include` :samp:`{file}` 
  :option:`-M`  :option:`-MD`  :option:`-MF`  :option:`-MG`  :option:`-MM`  :option:`-MMD`  :option:`-MP`  :option:`-MQ`  :option:`-MT` :option:`-Mno-modules` 
  :option:`-no-integrated-cpp`  :option:`-P`  :option:`-pthread`  :option:`-remap` 
  :option:`-traditional`  :option:`-traditional-cpp`  :option:`-trigraphs` 
  :option:`-Umacro`  :option:`-undef`  
  :option:`-Wp,option`  :option:`-Xpreprocessor` :samp:`{option}`

*Assembler Options*
  See :ref:`assembler-options`.

  :option:`-Wa,option`  :option:`-Xassembler` :samp:`{option}`

*Linker Options*
  See :ref:`link-options`.

  :samp:`{object-file-name}`  :option:`-fuse-ld=linker`  :option:`-llibrary` 
  :option:`-nostartfiles`  :option:`-nodefaultlibs`  :option:`-nolibc`  :option:`-nostdlib`  :option:`-nostdlib++` 
  :option:`-e` :samp:`{entry}`  :option:`--entry=entry` 
  :option:`-pie`  :option:`-pthread`  :option:`-r`  :option:`-rdynamic` 
  :option:`-s`  :option:`-static`  :option:`-static-pie`  :option:`-static-libgcc`  :option:`-static-libstdc++` 
  :option:`-static-libasan`  :option:`-static-libtsan`  :option:`-static-liblsan`  :option:`-static-libubsan` 
  :option:`-shared`  :option:`-shared-libgcc`  :option:`-symbolic` 
  :option:`-T` :samp:`{script}`  :option:`-Wl,option`  :option:`-Xlinker` :samp:`{option}` 
  :option:`-u` :samp:`{symbol}`  :option:`-z` :samp:`{keyword}`

*Directory Options*
  See :ref:`directory-options`.

  :option:`-Bprefix`  :option:`-Idir`  :option:`-I-` 
  :option:`-idirafter` :samp:`{dir}` 
  :option:`-imacros` :samp:`{file}`  :option:`-imultilib` :samp:`{dir}` 
  :option:`-iplugindir=dir`  :option:`-iprefix` :samp:`{file}` 
  :option:`-iquote` :samp:`{dir}`  :option:`-isysroot` :samp:`{dir}`  :option:`-isystem` :samp:`{dir}` 
  :option:`-iwithprefix` :samp:`{dir}`  :option:`-iwithprefixbefore` :samp:`{dir}`  
  :option:`-Ldir`  :option:`-no-canonical-prefixes`  :option:`--no-sysroot-suffix` 
  :option:`-nostdinc`  :option:`-nostdinc++`  :option:`--sysroot=dir`

*Code Generation Options*
  See :ref:`code-gen-options`.

  :option:`-fcall-saved-reg`  :option:`-fcall-used-reg` 
  :option:`-ffixed-reg`  :option:`-fexceptions` 
  :option:`-fnon-call-exceptions`  :option:`-fdelete-dead-exceptions`  :option:`-funwind-tables` 
  :option:`-fasynchronous-unwind-tables` 
  :option:`-fno-gnu-unique` 
  :option:`-finhibit-size-directive`  :option:`-fcommon`  :option:`-fno-ident` 
  :option:`-fpcc-struct-return`  :option:`-fpic`  :option:`-fPIC`  :option:`-fpie`  :option:`-fPIE`  :option:`-fno-plt` 
  :option:`-fno-jump-tables` :option:`-fno-bit-tests` 
  :option:`-frecord-gcc-switches` 
  :option:`-freg-struct-return`  :option:`-fshort-enums`  :option:`-fshort-wchar` 
  :option:`-fverbose-asm`  :option:`-fpack-struct[=n` ]  
  :option:`-fleading-underscore`  :option:`-ftls-model=model` 
  :option:`-fstack-reuse=reuse_level` 
  :option:`-ftrampolines`  :option:`-ftrapv`  :option:`-fwrapv` 
  :option:`-fvisibility=` [default|internal|hidden|protected] 
  :option:`-fstrict-volatile-bitfields`  :option:`-fsync-libcalls`

*Developer Options*
  See :ref:`developer-options`.

  :option:`-dletters`  :option:`-dumpspecs`  :option:`-dumpmachine`  :option:`-dumpversion` 
  :option:`-dumpfullversion`  :option:`-fcallgraph-info` [=su,da]
  :option:`-fchecking`  :option:`-fchecking=n`
  :option:`-fdbg-cnt-list`   :option:`-fdbg-cnt=counter-value-list` 
  :option:`-fdisable-ipa-pass_name` 
  :option:`-fdisable-rtl-pass_name` 
  :option:`-fdisable-rtl-pass-name` = :samp:`{range-list}` 
  :option:`-fdisable-tree-pass_name` 
  :option:`-fdisable-tree-pass-name` = :samp:`{range-list}` 
  :option:`-fdump-debug`  :option:`-fdump-earlydebug` 
  :option:`-fdump-noaddr`  :option:`-fdump-unnumbered`  :option:`-fdump-unnumbered-links` 
  :option:`-fdump-final-insns` [= :samp:`{file}` ] 
  :option:`-fdump-ipa-all`  :option:`-fdump-ipa-cgraph`  :option:`-fdump-ipa-inline` 
  :option:`-fdump-lang-all` 
  :option:`-fdump-lang-switch` 
  :option:`-fdump-lang-switch` - :samp:`{options}` 
  :option:`-fdump-lang-switch` - :samp:`{options}` = :samp:`{filename}` 
  :option:`-fdump-passes` 
  :option:`-fdump-rtl-pass`  :option:`-fdump-rtl-pass` = :samp:`{filename}` 
  :option:`-fdump-statistics` 
  :option:`-fdump-tree-all` 
  :option:`-fdump-tree-switch` 
  :option:`-fdump-tree-switch` - :samp:`{options}` 
  :option:`-fdump-tree-switch` - :samp:`{options}` = :samp:`{filename}` 
  :option:`-fcompare-debug` [= :samp:`{opts}` ]  :option:`-fcompare-debug-second` 
  :option:`-fenable-kind` - :samp:`{pass}` 
  :option:`-fenable-kind` - :samp:`{pass}` = :samp:`{range-list}` 
  :option:`-fira-verbose=n` 
  :option:`-flto-report`  :option:`-flto-report-wpa`  :option:`-fmem-report-wpa` 
  :option:`-fmem-report`  :option:`-fpre-ipa-mem-report`  :option:`-fpost-ipa-mem-report` 
  :option:`-fopt-info`  :option:`-fopt-info-options` [= :samp:`{file}` ] 
  :option:`-fprofile-report` 
  :option:`-frandom-seed=string`  :option:`-fsched-verbose=n` 
  :option:`-fsel-sched-verbose`  :option:`-fsel-sched-dump-cfg`  :option:`-fsel-sched-pipelining-verbose` 
  :option:`-fstats`  :option:`-fstack-usage`  :option:`-ftime-report`  :option:`-ftime-report-details` 
  :option:`-fvar-tracking-assignments-toggle`  :option:`-gtoggle` 
  :option:`-print-file-name=library`  :option:`-print-libgcc-file-name` 
  :option:`-print-multi-directory`  :option:`-print-multi-lib`  :option:`-print-multi-os-directory` 
  :option:`-print-prog-name=program`  :option:`-print-search-dirs`  :option:`-Q` 
  :option:`-print-sysroot`  :option:`-print-sysroot-headers-suffix` 
  :option:`-save-temps`  :option:`-save-temps=cwd`  :option:`-save-temps=obj`  :option:`-time` [= :samp:`{file}` ]

*Machine-Dependent Options*
  See :ref:`submodel-options`.

  .. This list is ordered alphanumerically by subsection name.

  .. Try and put the significant identifier (CPU or system) first,

  .. so users have a clue at guessing where the ones they want will be.

  .. program:: See .

  *AArch64 Options*

  .. program:: AArch64

  :option:`-mabi=name`  :option:`-mbig-endian`  :option:`-mlittle-endian` 
  :option:`-mgeneral-regs-only` 
  :option:`-mcmodel=tiny`  :option:`-mcmodel=small`  :option:`-mcmodel=large` 
  :option:`-mstrict-align`  :option:`-mno-strict-align` 
  :option:`-momit-leaf-frame-pointer` 
  :option:`-mtls-dialect=desc`  :option:`-mtls-dialect=traditional` 
  :option:`-mtls-size=size` 
  :option:`-mfix-cortex-a53-835769`  :option:`-mfix-cortex-a53-843419` 
  :option:`-mlow-precision-recip-sqrt`  :option:`-mlow-precision-sqrt`  :option:`-mlow-precision-div` 
  :option:`-mpc-relative-literal-loads` 
  :option:`-msign-return-address=scope` 
  :option:`-mbranch-protection=none` | :samp:`{standard}` | :samp:`{pac-ret}` [+ :samp:`{leaf}`
  + :samp:`{b-key}` ]| :samp:`{bti}` 
  :option:`-mharden-sls=opts` 
  :option:`-march=name`  :option:`-mcpu=name`  :option:`-mtune=name`  
  :option:`-moverride=string`  :option:`-mverbose-cost-dump` 
  :option:`-mstack-protector-guard=guard` :option:`-mstack-protector-guard-reg=sysreg` 
  :option:`-mstack-protector-guard-offset=offset` :option:`-mtrack-speculation` 
  :option:`-moutline-atomics` 

  *Adapteva Epiphany Options*

  .. program:: Adapteva Epiphany

  :option:`-mhalf-reg-file`  :option:`-mprefer-short-insn-regs` 
  :option:`-mbranch-cost=num`  :option:`-mcmove`  :option:`-mnops=num`  :option:`-msoft-cmpsf` 
  :option:`-msplit-lohi`  :option:`-mpost-inc`  :option:`-mpost-modify`  :option:`-mstack-offset=num` 
  :option:`-mround-nearest`  :option:`-mlong-calls`  :option:`-mshort-calls`  :option:`-msmall16` 
  :option:`-mfp-mode=mode`  :option:`-mvect-double`  :option:`-max-vect-align=num` 
  :option:`-msplit-vecmove-early`  :option:`-m1reg-reg`

  *AMD GCN Options*

  .. program:: AMD GCN

  :option:`-march=gpu` :option:`-mtune=gpu` :option:`-mstack-size=bytes`

  *ARC Options*

  .. program:: ARC

  :option:`-mbarrel-shifter`  :option:`-mjli-always` 
  :option:`-mcpu=cpu`  :option:`-mA6`  :option:`-mARC600`  :option:`-mA7`  :option:`-mARC700` 
  :option:`-mdpfp`  :option:`-mdpfp-compact`  :option:`-mdpfp-fast`  :option:`-mno-dpfp-lrsr` 
  :option:`-mea`  :option:`-mno-mpy`  :option:`-mmul32x16`  :option:`-mmul64`  :option:`-matomic` 
  :option:`-mnorm`  :option:`-mspfp`  :option:`-mspfp-compact`  :option:`-mspfp-fast`  :option:`-msimd`  :option:`-msoft-float`  :option:`-mswap` 
  :option:`-mcrc`  :option:`-mdsp-packa`  :option:`-mdvbf`  :option:`-mlock`  :option:`-mmac-d16`  :option:`-mmac-24`  :option:`-mrtsc`  :option:`-mswape` 
  :option:`-mtelephony`  :option:`-mxy`  :option:`-misize`  :option:`-mannotate-align`  :option:`-marclinux`  :option:`-marclinux_prof` 
  :option:`-mlong-calls`  :option:`-mmedium-calls`  :option:`-msdata`  :option:`-mirq-ctrl-saved` 
  :option:`-mrgf-banked-regs`  :option:`-mlpc-width=width`  :option:`-G` :samp:`{num}` 
  :option:`-mvolatile-cache`  :option:`-mtp-regno=regno` 
  :option:`-malign-call`  :option:`-mauto-modify-reg`  :option:`-mbbit-peephole`  :option:`-mno-brcc` 
  :option:`-mcase-vector-pcrel`  :option:`-mcompact-casesi`  :option:`-mno-cond-exec`  :option:`-mearly-cbranchsi` 
  :option:`-mexpand-adddi`  :option:`-mindexed-loads`  :option:`-mlra`  :option:`-mlra-priority-none` 
  :option:`-mlra-priority-compact` :option:`-mlra-priority-noncompact`  :option:`-mmillicode` 
  :option:`-mmixed-code`  :option:`-mq-class`  :option:`-mRcq`  :option:`-mRcw`  :option:`-msize-level=level` 
  :option:`-mtune=cpu`  :option:`-mmultcost=num`  :option:`-mcode-density-frame` 
  :option:`-munalign-prob-threshold=probability`  :option:`-mmpy-option=multo` 
  :option:`-mdiv-rem`  :option:`-mcode-density`  :option:`-mll64`  :option:`-mfpu=fpu`  :option:`-mrf16`  :option:`-mbranch-index`

  *ARM Options*

  .. program:: ARM

  :option:`-mapcs-frame`  :option:`-mno-apcs-frame` 
  :option:`-mabi=name` 
  :option:`-mapcs-stack-check`  :option:`-mno-apcs-stack-check` 
  :option:`-mapcs-reentrant`  :option:`-mno-apcs-reentrant` 
  :option:`-mgeneral-regs-only` 
  :option:`-msched-prolog`  :option:`-mno-sched-prolog` 
  :option:`-mlittle-endian`  :option:`-mbig-endian` 
  :option:`-mbe8`  :option:`-mbe32` 
  :option:`-mfloat-abi=name` 
  :option:`-mfp16-format=name`
  :option:`-mthumb-interwork`  :option:`-mno-thumb-interwork` 
  :option:`-mcpu=name`  :option:`-march=name`  :option:`-mfpu=name`  
  :option:`-mtune=name`  :option:`-mprint-tune-info` 
  :option:`-mstructure-size-boundary=n` 
  :option:`-mabort-on-noreturn` 
  :option:`-mlong-calls`  :option:`-mno-long-calls` 
  :option:`-msingle-pic-base`  :option:`-mno-single-pic-base` 
  :option:`-mpic-register=reg` 
  :option:`-mnop-fun-dllimport` 
  :option:`-mpoke-function-name` 
  :option:`-mthumb`  :option:`-marm`  :option:`-mflip-thumb` 
  :option:`-mtpcs-frame`  :option:`-mtpcs-leaf-frame` 
  :option:`-mcaller-super-interworking`  :option:`-mcallee-super-interworking` 
  :option:`-mtp=name`  :option:`-mtls-dialect=dialect` 
  :option:`-mword-relocations` 
  :option:`-mfix-cortex-m3-ldrd` 
  :option:`-mfix-cortex-a57-aes-1742098` 
  :option:`-mfix-cortex-a72-aes-1655431` 
  :option:`-munaligned-access` 
  :option:`-mneon-for-64bits` 
  :option:`-mslow-flash-data` 
  :option:`-masm-syntax-unified` 
  :option:`-mrestrict-it` 
  :option:`-mverbose-cost-dump` 
  :option:`-mpure-code` 
  :option:`-mcmse` 
  :option:`-mfix-cmse-cve-2021-35465` 
  :option:`-mstack-protector-guard=guard` :option:`-mstack-protector-guard-offset=offset` 
  :option:`-mfdpic`

  *AVR Options*

  .. program:: AVR

  :option:`-mmcu=mcu`  :option:`-mabsdata`  :option:`-maccumulate-args` 
  :option:`-mbranch-cost=cost` 
  :option:`-mcall-prologues`  :option:`-mgas-isr-prologues`  :option:`-mint8` 
  :option:`-mdouble=bits` :option:`-mlong-double=bits` 
  :option:`-mn_flash=size`  :option:`-mno-interrupts` 
  :option:`-mmain-is-OS_task`  :option:`-mrelax`  :option:`-mrmw`  :option:`-mstrict-X`  :option:`-mtiny-stack` 
  :option:`-mfract-convert-truncate` 
  :option:`-mshort-calls`  :option:`-nodevicelib`  :option:`-nodevicespecs` 
  :option:`-Waddr-space-convert`  :option:`-Wmisspelled-isr`

  *Blackfin Options*

  .. program:: Blackfin

  :option:`-mcpu=cpu` [- :samp:`{sirevision}` ] 
  :option:`-msim`  :option:`-momit-leaf-frame-pointer`  :option:`-mno-omit-leaf-frame-pointer` 
  :option:`-mspecld-anomaly`  :option:`-mno-specld-anomaly`  :option:`-mcsync-anomaly`  :option:`-mno-csync-anomaly` 
  :option:`-mlow-64k`  :option:`-mno-low64k`  :option:`-mstack-check-l1`  :option:`-mid-shared-library` 
  :option:`-mno-id-shared-library`  :option:`-mshared-library-id=n` 
  :option:`-mleaf-id-shared-library`  :option:`-mno-leaf-id-shared-library` 
  :option:`-msep-data`  :option:`-mno-sep-data`  :option:`-mlong-calls`  :option:`-mno-long-calls` 
  :option:`-mfast-fp`  :option:`-minline-plt`  :option:`-mmulticore`  :option:`-mcorea`  :option:`-mcoreb`  :option:`-msdram` 
  :option:`-micplb`

  *C6X Options*

  .. program:: C6X

  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-march=cpu` 
  :option:`-msim`  :option:`-msdata=sdata-type`

  *CRIS Options*

  .. program:: CRIS

  :option:`-mcpu=cpu`  :option:`-march=cpu`
  :option:`-mtune=cpu` :option:`-mmax-stack-frame=n` 
  :option:`-metrax4`  :option:`-metrax100`  :option:`-mpdebug`  :option:`-mcc-init`  :option:`-mno-side-effects` 
  :option:`-mstack-align`  :option:`-mdata-align`  :option:`-mconst-align` 
  :option:`-m32-bit`  :option:`-m16-bit`  :option:`-m8-bit`  :option:`-mno-prologue-epilogue` 
  :option:`-melf`  :option:`-maout`  :option:`-sim`  :option:`-sim2` 
  :option:`-mmul-bug-workaround`  :option:`-mno-mul-bug-workaround`

  *C-SKY Options*

  .. program:: C-SKY

  :option:`-march=arch`  :option:`-mcpu=cpu` 
  :option:`-mbig-endian`  :option:`-EB`  :option:`-mlittle-endian`  :option:`-EL` 
  :option:`-mhard-float`  :option:`-msoft-float`  :option:`-mfpu=fpu`  :option:`-mdouble-float`  :option:`-mfdivdu` 
  :option:`-mfloat-abi=name` 
  :option:`-melrw`  :option:`-mistack`  :option:`-mmp`  :option:`-mcp`  :option:`-mcache`  :option:`-msecurity`  :option:`-mtrust` 
  :option:`-mdsp`  :option:`-medsp`  :option:`-mvdsp` 
  :option:`-mdiv`  :option:`-msmart`  :option:`-mhigh-registers`  :option:`-manchor` 
  :option:`-mpushpop`  :option:`-mmultiple-stld`  :option:`-mconstpool`  :option:`-mstack-size`  :option:`-mccrt` 
  :option:`-mbranch-cost=n`  :option:`-mcse-cc`  :option:`-msched-prolog` :option:`-msim`

  *Darwin Options*

  .. program:: Darwin

  :option:`-all_load`  :option:`-allowable_client`  :option:`-arch`  :option:`-arch_errors_fatal` 
  :option:`-arch_only`  :option:`-bind_at_load`  :option:`-bundle`  :option:`-bundle_loader` 
  :option:`-client_name`  :option:`-compatibility_version`  :option:`-current_version` 
  :option:`-dead_strip` 
  :option:`-dependency-file`  :option:`-dylib_file`  :option:`-dylinker_install_name` 
  :option:`-dynamic`  :option:`-dynamiclib`  :option:`-exported_symbols_list` 
  :option:`-filelist`  :option:`-flat_namespace`  :option:`-force_cpusubtype_ALL` 
  :option:`-force_flat_namespace`  :option:`-headerpad_max_install_names` 
  :option:`-iframework` 
  :option:`-image_base`  :option:`-init`  :option:`-install_name`  :option:`-keep_private_externs` 
  :option:`-multi_module`  :option:`-multiply_defined`  :option:`-multiply_defined_unused` 
  :option:`-noall_load`   :option:`-no_dead_strip_inits_and_terms` 
  :option:`-nofixprebinding`  :option:`-nomultidefs`  :option:`-noprebind`  :option:`-noseglinkedit` 
  :option:`-pagezero_size`  :option:`-prebind`  :option:`-prebind_all_twolevel_modules` 
  :option:`-private_bundle`  :option:`-read_only_relocs`  :option:`-sectalign` 
  :option:`-sectobjectsymbols`  :option:`-whyload`  :option:`-seg1addr` 
  :option:`-sectcreate`  :option:`-sectobjectsymbols`  :option:`-sectorder` 
  :option:`-segaddr`  :option:`-segs_read_only_addr`  :option:`-segs_read_write_addr` 
  :option:`-seg_addr_table`  :option:`-seg_addr_table_filename`  :option:`-seglinkedit` 
  :option:`-segprot`  :option:`-segs_read_only_addr`  :option:`-segs_read_write_addr` 
  :option:`-single_module`  :option:`-static`  :option:`-sub_library`  :option:`-sub_umbrella` 
  :option:`-twolevel_namespace`  :option:`-umbrella`  :option:`-undefined` 
  :option:`-unexported_symbols_list`  :option:`-weak_reference_mismatches` 
  :option:`-whatsloaded`  :option:`-F`  :option:`-gused`  :option:`-gfull`  :option:`-mmacosx-version-min=version` 
  :option:`-mkernel`  :option:`-mone-byte-bool`

  *DEC Alpha Options*

  .. program:: DEC Alpha

  :option:`-mno-fp-regs`  :option:`-msoft-float` 
  :option:`-mieee`  :option:`-mieee-with-inexact`  :option:`-mieee-conformant` 
  :option:`-mfp-trap-mode=mode`  :option:`-mfp-rounding-mode=mode` 
  :option:`-mtrap-precision=mode`  :option:`-mbuild-constants` 
  :option:`-mcpu=cpu-type`  :option:`-mtune=cpu-type` 
  :option:`-mbwx`  :option:`-mmax`  :option:`-mfix`  :option:`-mcix` 
  :option:`-mfloat-vax`  :option:`-mfloat-ieee` 
  :option:`-mexplicit-relocs`  :option:`-msmall-data`  :option:`-mlarge-data` 
  :option:`-msmall-text`  :option:`-mlarge-text` 
  :option:`-mmemory-latency=time`

  *eBPF Options*

  .. program:: eBPF

  :option:`-mbig-endian` :option:`-mlittle-endian` :option:`-mkernel=version`
  :option:`-mframe-limit=bytes` :option:`-mxbpf` :option:`-mco-re` :option:`-mno-co-re`
  :option:`-mjmpext` :option:`-mjmp32` :option:`-malu32` :option:`-mcpu=version`

  *FR30 Options*

  .. program:: FR30

  :option:`-msmall-model`  :option:`-mno-lsim`

  *FT32 Options*

  .. program:: FT32

  :option:`-msim`  :option:`-mlra`  :option:`-mnodiv`  :option:`-mft32b`  :option:`-mcompress`  :option:`-mnopm`

  *FRV Options*

  .. program:: FRV

  :option:`-mgpr-32`  :option:`-mgpr-64`  :option:`-mfpr-32`  :option:`-mfpr-64` 
  :option:`-mhard-float`  :option:`-msoft-float` 
  :option:`-malloc-cc`  :option:`-mfixed-cc`  :option:`-mdword`  :option:`-mno-dword` 
  :option:`-mdouble`  :option:`-mno-double` 
  :option:`-mmedia`  :option:`-mno-media`  :option:`-mmuladd`  :option:`-mno-muladd` 
  :option:`-mfdpic`  :option:`-minline-plt`  :option:`-mgprel-ro`  :option:`-multilib-library-pic` 
  :option:`-mlinked-fp`  :option:`-mlong-calls`  :option:`-malign-labels` 
  :option:`-mlibrary-pic`  :option:`-macc-4`  :option:`-macc-8` 
  :option:`-mpack`  :option:`-mno-pack`  :option:`-mno-eflags`  :option:`-mcond-move`  :option:`-mno-cond-move` 
  :option:`-moptimize-membar`  :option:`-mno-optimize-membar` 
  :option:`-mscc`  :option:`-mno-scc`  :option:`-mcond-exec`  :option:`-mno-cond-exec` 
  :option:`-mvliw-branch`  :option:`-mno-vliw-branch` 
  :option:`-mmulti-cond-exec`  :option:`-mno-multi-cond-exec`  :option:`-mnested-cond-exec` 
  :option:`-mno-nested-cond-exec`  :option:`-mtomcat-stats` 
  :option:`-mTLS`  :option:`-mtls` 
  :option:`-mcpu=cpu`

  *GNU/Linux Options*

  .. program:: GNU/Linux

  :option:`-mglibc`  :option:`-muclibc`  :option:`-mmusl`  :option:`-mbionic`  :option:`-mandroid` 
  :option:`-tno-android-cc`  :option:`-tno-android-ld`

  *H8/300 Options*

  .. program:: H8/300

  :option:`-mrelax`  :option:`-mh`  :option:`-ms`  :option:`-mn`  :option:`-mexr`  :option:`-mno-exr`  :option:`-mint32`  :option:`-malign-300`

  *HPPA Options*

  .. program:: HPPA

  :option:`-march=architecture-type` 
  :option:`-mcaller-copies`  :option:`-mdisable-fpregs`  :option:`-mdisable-indexing` 
  :option:`-mfast-indirect-calls`  :option:`-mgas`  :option:`-mgnu-ld`   :option:`-mhp-ld` 
  :option:`-mfixed-range=register-range` 
  :option:`-mjump-in-delay`  :option:`-mlinker-opt`  :option:`-mlong-calls` 
  :option:`-mlong-load-store`  :option:`-mno-disable-fpregs` 
  :option:`-mno-disable-indexing`  :option:`-mno-fast-indirect-calls`  :option:`-mno-gas` 
  :option:`-mno-jump-in-delay`  :option:`-mno-long-load-store` 
  :option:`-mno-portable-runtime`  :option:`-mno-soft-float` 
  :option:`-mno-space-regs`  :option:`-msoft-float`  :option:`-mpa-risc-1-0` 
  :option:`-mpa-risc-1-1`  :option:`-mpa-risc-2-0`  :option:`-mportable-runtime` 
  :option:`-mschedule=cpu-type`  :option:`-mspace-regs`  :option:`-msio`  :option:`-mwsio` 
  :option:`-munix=unix-std`  :option:`-nolibdld`  :option:`-static`  :option:`-threads`

  *IA-64 Options*

  .. program:: IA-64

  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-mgnu-as`  :option:`-mgnu-ld`  :option:`-mno-pic` 
  :option:`-mvolatile-asm-stop`  :option:`-mregister-names`  :option:`-msdata`  :option:`-mno-sdata` 
  :option:`-mconstant-gp`  :option:`-mauto-pic`  :option:`-mfused-madd` 
  :option:`-minline-float-divide-min-latency` 
  :option:`-minline-float-divide-max-throughput` 
  :option:`-mno-inline-float-divide` 
  :option:`-minline-int-divide-min-latency` 
  :option:`-minline-int-divide-max-throughput`  
  :option:`-mno-inline-int-divide` 
  :option:`-minline-sqrt-min-latency`  :option:`-minline-sqrt-max-throughput` 
  :option:`-mno-inline-sqrt` 
  :option:`-mdwarf2-asm`  :option:`-mearly-stop-bits` 
  :option:`-mfixed-range=register-range`  :option:`-mtls-size=tls-size` 
  :option:`-mtune=cpu-type`  :option:`-milp32`  :option:`-mlp64` 
  :option:`-msched-br-data-spec`  :option:`-msched-ar-data-spec`  :option:`-msched-control-spec` 
  :option:`-msched-br-in-data-spec`  :option:`-msched-ar-in-data-spec`  :option:`-msched-in-control-spec` 
  :option:`-msched-spec-ldc`  :option:`-msched-spec-control-ldc` 
  :option:`-msched-prefer-non-data-spec-insns`  :option:`-msched-prefer-non-control-spec-insns` 
  :option:`-msched-stop-bits-after-every-cycle`  :option:`-msched-count-spec-in-critical-path` 
  :option:`-msel-sched-dont-check-control-spec`  :option:`-msched-fp-mem-deps-zero-cost` 
  :option:`-msched-max-memory-insns-hard-limit`  :option:`-msched-max-memory-insns=max-insns`

  *LM32 Options*

  .. program:: LM32

  :option:`-mbarrel-shift-enabled`  :option:`-mdivide-enabled`  :option:`-mmultiply-enabled` 
  :option:`-msign-extend-enabled`  :option:`-muser-enabled`

  *LoongArch Options*

  .. program:: LoongArch

  :option:`-march=cpu-type`  :option:`-mtune=cpu-type` :option:`-mabi=base-abi-type` 
  :option:`-mfpu=fpu-type` :option:`-msoft-float` :option:`-msingle-float` :option:`-mdouble-float` 
  :option:`-mbranch-cost=n`  :option:`-mcheck-zero-division` :option:`-mno-check-zero-division` 
  :option:`-mcond-move-int`  :option:`-mno-cond-move-int` 
  :option:`-mcond-move-float`  :option:`-mno-cond-move-float` 
  :option:`-memcpy`  :option:`-mno-memcpy` :option:`-mstrict-align` :option:`-mno-strict-align` 
  :option:`-mmax-inline-memcpy-size=n` 
  :option:`-mexplicit-relocs` :option:`-mno-explicit-relocs` 
  :option:`-mdirect-extern-access` :option:`-mno-direct-extern-access` 
  :option:`-mcmodel=code-model`

  *M32R/D Options*

  .. program:: M32R/D

  :option:`-m32r2`  :option:`-m32rx`  :option:`-m32r` 
  :option:`-mdebug` 
  :option:`-malign-loops`  :option:`-mno-align-loops` 
  :option:`-missue-rate=number` 
  :option:`-mbranch-cost=number` 
  :option:`-mmodel=code-size-model-type` 
  :option:`-msdata=sdata-type` 
  :option:`-mno-flush-func`  :option:`-mflush-func=name` 
  :option:`-mno-flush-trap`  :option:`-mflush-trap=number` 
  :option:`-G` :samp:`{num}`

  *M32C Options*

  .. program:: M32C

  :option:`-mcpu=cpu`  :option:`-msim`  :option:`-memregs=number`

  *M680x0 Options*

  .. program:: M680x0

  :option:`-march=arch`  :option:`-mcpu=cpu`  :option:`-mtune=tune` 
  :option:`-m68000`  :option:`-m68020`  :option:`-m68020-40`  :option:`-m68020-60`  :option:`-m68030`  :option:`-m68040` 
  :option:`-m68060`  :option:`-mcpu32`  :option:`-m5200`  :option:`-m5206e`  :option:`-m528x`  :option:`-m5307`  :option:`-m5407` 
  :option:`-mcfv4e`  :option:`-mbitfield`  :option:`-mno-bitfield`  :option:`-mc68000`  :option:`-mc68020` 
  :option:`-mnobitfield`  :option:`-mrtd`  :option:`-mno-rtd`  :option:`-mdiv`  :option:`-mno-div`  :option:`-mshort` 
  :option:`-mno-short`  :option:`-mhard-float`  :option:`-m68881`  :option:`-msoft-float`  :option:`-mpcrel` 
  :option:`-malign-int`  :option:`-mstrict-align`  :option:`-msep-data`  :option:`-mno-sep-data` 
  :option:`-mshared-library-id=n`  :option:`-mid-shared-library`  :option:`-mno-id-shared-library` 
  :option:`-mxgot`  :option:`-mno-xgot`  :option:`-mlong-jump-table-offsets`

  *MCore Options*

  .. program:: MCore

  :option:`-mhardlit`  :option:`-mno-hardlit`  :option:`-mdiv`  :option:`-mno-div`  :option:`-mrelax-immediates` 
  :option:`-mno-relax-immediates`  :option:`-mwide-bitfields`  :option:`-mno-wide-bitfields` 
  :option:`-m4byte-functions`  :option:`-mno-4byte-functions`  :option:`-mcallgraph-data` 
  :option:`-mno-callgraph-data`  :option:`-mslow-bytes`  :option:`-mno-slow-bytes`  :option:`-mno-lsim` 
  :option:`-mlittle-endian`  :option:`-mbig-endian`  :option:`-m210`  :option:`-m340`  :option:`-mstack-increment`

  *MeP Options*

  .. program:: MeP

  :option:`-mabsdiff`  :option:`-mall-opts`  :option:`-maverage`  :option:`-mbased=n`  :option:`-mbitops` 
  :option:`-mc=n`  :option:`-mclip`  :option:`-mconfig=name`  :option:`-mcop`  :option:`-mcop32`  :option:`-mcop64`  :option:`-mivc2` 
  :option:`-mdc`  :option:`-mdiv`  :option:`-meb`  :option:`-mel`  :option:`-mio-volatile`  :option:`-ml`  :option:`-mleadz`  :option:`-mm`  :option:`-mminmax` 
  :option:`-mmult`  :option:`-mno-opts`  :option:`-mrepeat`  :option:`-ms`  :option:`-msatur`  :option:`-msdram`  :option:`-msim`  :option:`-msimnovec`  :option:`-mtf` 
  :option:`-mtiny=n`

  *MicroBlaze Options*

  .. program:: MicroBlaze

  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-msmall-divides`  :option:`-mcpu=cpu` 
  :option:`-mmemcpy`  :option:`-mxl-soft-mul`  :option:`-mxl-soft-div`  :option:`-mxl-barrel-shift` 
  :option:`-mxl-pattern-compare`  :option:`-mxl-stack-check`  :option:`-mxl-gp-opt`  :option:`-mno-clearbss` 
  :option:`-mxl-multiply-high`  :option:`-mxl-float-convert`  :option:`-mxl-float-sqrt` 
  :option:`-mbig-endian`  :option:`-mlittle-endian`  :option:`-mxl-reorder`  :option:`-mxl-mode-app-model` 
  :option:`-mpic-data-is-text-relative`

  *MIPS Options*

  .. program:: MIPS

  :option:`-EL`  :option:`-EB`  :option:`-march=arch`  :option:`-mtune=arch` 
  :option:`-mips1`  :option:`-mips2`  :option:`-mips3`  :option:`-mips4`  :option:`-mips32`  :option:`-mips32r2`  :option:`-mips32r3`  :option:`-mips32r5` 
  :option:`-mips32r6`  :option:`-mips64`  :option:`-mips64r2`  :option:`-mips64r3`  :option:`-mips64r5`  :option:`-mips64r6` 
  :option:`-mips16`  :option:`-mno-mips16`  :option:`-mflip-mips16` 
  :option:`-minterlink-compressed`  :option:`-mno-interlink-compressed` 
  :option:`-minterlink-mips16`  :option:`-mno-interlink-mips16` 
  :option:`-mabi=abi`  :option:`-mabicalls`  :option:`-mno-abicalls` 
  :option:`-mshared`  :option:`-mno-shared`  :option:`-mplt`  :option:`-mno-plt`  :option:`-mxgot`  :option:`-mno-xgot` 
  :option:`-mgp32`  :option:`-mgp64`  :option:`-mfp32`  :option:`-mfpxx`  :option:`-mfp64`  :option:`-mhard-float`  :option:`-msoft-float` 
  :option:`-mno-float`  :option:`-msingle-float`  :option:`-mdouble-float` 
  :option:`-modd-spreg`  :option:`-mno-odd-spreg` 
  :option:`-mabs=mode`  :option:`-mnan=encoding` 
  :option:`-mdsp`  :option:`-mno-dsp`  :option:`-mdspr2`  :option:`-mno-dspr2` 
  :option:`-mmcu`  :option:`-mmno-mcu` 
  :option:`-meva`  :option:`-mno-eva` 
  :option:`-mvirt`  :option:`-mno-virt` 
  :option:`-mxpa`  :option:`-mno-xpa` 
  :option:`-mcrc`  :option:`-mno-crc` 
  :option:`-mginv`  :option:`-mno-ginv` 
  :option:`-mmicromips`  :option:`-mno-micromips` 
  :option:`-mmsa`  :option:`-mno-msa` 
  :option:`-mloongson-mmi`  :option:`-mno-loongson-mmi` 
  :option:`-mloongson-ext`  :option:`-mno-loongson-ext` 
  :option:`-mloongson-ext2`  :option:`-mno-loongson-ext2` 
  :option:`-mfpu=fpu-type` 
  :option:`-msmartmips`  :option:`-mno-smartmips` 
  :option:`-mpaired-single`  :option:`-mno-paired-single`  :option:`-mdmx`  :option:`-mno-mdmx` 
  :option:`-mips3d`  :option:`-mno-mips3d`  :option:`-mmt`  :option:`-mno-mt`  :option:`-mllsc`  :option:`-mno-llsc` 
  :option:`-mlong64`  :option:`-mlong32`  :option:`-msym32`  :option:`-mno-sym32` 
  :option:`-Gnum`  :option:`-mlocal-sdata`  :option:`-mno-local-sdata` 
  :option:`-mextern-sdata`  :option:`-mno-extern-sdata`  :option:`-mgpopt`  :option:`-mno-gopt` 
  :option:`-membedded-data`  :option:`-mno-embedded-data` 
  :option:`-muninit-const-in-rodata`  :option:`-mno-uninit-const-in-rodata` 
  :option:`-mcode-readable=setting` 
  :option:`-msplit-addresses`  :option:`-mno-split-addresses` 
  :option:`-mexplicit-relocs`  :option:`-mno-explicit-relocs` 
  :option:`-mcheck-zero-division`  :option:`-mno-check-zero-division` 
  :option:`-mdivide-traps`  :option:`-mdivide-breaks` 
  :option:`-mload-store-pairs`  :option:`-mno-load-store-pairs` 
  :option:`-munaligned-access`  :option:`-mno-unaligned-access` 
  :option:`-mmemcpy`  :option:`-mno-memcpy`  :option:`-mlong-calls`  :option:`-mno-long-calls` 
  :option:`-mmad`  :option:`-mno-mad`  :option:`-mimadd`  :option:`-mno-imadd`  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-nocpp` 
  :option:`-mfix-24k`  :option:`-mno-fix-24k` 
  :option:`-mfix-r4000`  :option:`-mno-fix-r4000`  :option:`-mfix-r4400`  :option:`-mno-fix-r4400` 
  :option:`-mfix-r5900`  :option:`-mno-fix-r5900` 
  :option:`-mfix-r10000`  :option:`-mno-fix-r10000`  :option:`-mfix-rm7000`  :option:`-mno-fix-rm7000` 
  :option:`-mfix-vr4120`  :option:`-mno-fix-vr4120` 
  :option:`-mfix-vr4130`  :option:`-mno-fix-vr4130`  :option:`-mfix-sb1`  :option:`-mno-fix-sb1` 
  :option:`-mflush-func=func`  :option:`-mno-flush-func` 
  :option:`-mbranch-cost=num`  :option:`-mbranch-likely`  :option:`-mno-branch-likely` 
  :option:`-mcompact-branches=policy` 
  :option:`-mfp-exceptions`  :option:`-mno-fp-exceptions` 
  :option:`-mvr4130-align`  :option:`-mno-vr4130-align`  :option:`-msynci`  :option:`-mno-synci` 
  :option:`-mlxc1-sxc1`  :option:`-mno-lxc1-sxc1`  :option:`-mmadd4`  :option:`-mno-madd4` 
  :option:`-mrelax-pic-calls`  :option:`-mno-relax-pic-calls`  :option:`-mmcount-ra-address` 
  :option:`-mframe-header-opt`  :option:`-mno-frame-header-opt`

  *MMIX Options*

  .. program:: MMIX

  :option:`-mlibfuncs`  :option:`-mno-libfuncs`  :option:`-mepsilon`  :option:`-mno-epsilon`  :option:`-mabi=gnu` 
  :option:`-mabi=mmixware`  :option:`-mzero-extend`  :option:`-mknuthdiv`  :option:`-mtoplevel-symbols` 
  :option:`-melf`  :option:`-mbranch-predict`  :option:`-mno-branch-predict`  :option:`-mbase-addresses` 
  :option:`-mno-base-addresses`  :option:`-msingle-exit`  :option:`-mno-single-exit`

  *MN10300 Options*

  .. program:: MN10300

  :option:`-mmult-bug`  :option:`-mno-mult-bug` 
  :option:`-mno-am33`  :option:`-mam33`  :option:`-mam33-2`  :option:`-mam34` 
  :option:`-mtune=cpu-type` 
  :option:`-mreturn-pointer-on-d0` 
  :option:`-mno-crt0`  :option:`-mrelax`  :option:`-mliw`  :option:`-msetlb`

  *Moxie Options*

  .. program:: Moxie

  :option:`-meb`  :option:`-mel`  :option:`-mmul.x`  :option:`-mno-crt0`

  *MSP430 Options*

  .. program:: MSP430

  :option:`-msim`  :option:`-masm-hex`  :option:`-mmcu=`  :option:`-mcpu=`  :option:`-mlarge`  :option:`-msmall`  :option:`-mrelax` 
  :option:`-mwarn-mcu` 
  :option:`-mcode-region=`  :option:`-mdata-region=` 
  :option:`-msilicon-errata=`  :option:`-msilicon-errata-warn=` 
  :option:`-mhwmult=`  :option:`-minrt`  :option:`-mtiny-printf`  :option:`-mmax-inline-shift=`

  *NDS32 Options*

  .. program:: NDS32

  :option:`-mbig-endian`  :option:`-mlittle-endian` 
  :option:`-mreduced-regs`  :option:`-mfull-regs` 
  :option:`-mcmov`  :option:`-mno-cmov` 
  :option:`-mext-perf`  :option:`-mno-ext-perf` 
  :option:`-mext-perf2`  :option:`-mno-ext-perf2` 
  :option:`-mext-string`  :option:`-mno-ext-string` 
  :option:`-mv3push`  :option:`-mno-v3push` 
  :option:`-m16bit`  :option:`-mno-16bit` 
  :option:`-misr-vector-size=num` 
  :option:`-mcache-block-size=num` 
  :option:`-march=arch` 
  :option:`-mcmodel=code-model` 
  :option:`-mctor-dtor`  :option:`-mrelax`

  *Nios II Options*

  .. program:: Nios II

  :option:`-G` :samp:`{num}`  :option:`-mgpopt=option`  :option:`-mgpopt`  :option:`-mno-gpopt` 
  :option:`-mgprel-sec=regexp`  :option:`-mr0rel-sec=regexp` 
  :option:`-mel`  :option:`-meb` 
  :option:`-mno-bypass-cache`  :option:`-mbypass-cache` 
  :option:`-mno-cache-volatile`  :option:`-mcache-volatile` 
  :option:`-mno-fast-sw-div`  :option:`-mfast-sw-div` 
  :option:`-mhw-mul`  :option:`-mno-hw-mul`  :option:`-mhw-mulx`  :option:`-mno-hw-mulx`  :option:`-mno-hw-div`  :option:`-mhw-div` 
  :option:`-mcustom-insn` = :samp:`{N}`  :option:`-mno-custom-insn` 
  :option:`-mcustom-fpu-cfg=name` 
  :option:`-mhal`  :option:`-msmallc`  :option:`-msys-crt0=name`  :option:`-msys-lib=name` 
  :option:`-march=arch`  :option:`-mbmx`  :option:`-mno-bmx`  :option:`-mcdx`  :option:`-mno-cdx`

  *Nvidia PTX Options*

  .. program:: Nvidia PTX

  :option:`-m64`  :option:`-mmainkernel`  :option:`-moptimize`

  *OpenRISC Options*

  .. program:: OpenRISC

  :option:`-mboard=name`  :option:`-mnewlib`  :option:`-mhard-mul`  :option:`-mhard-div` 
  :option:`-msoft-mul`  :option:`-msoft-div` 
  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-mdouble-float` :option:`-munordered-float` 
  :option:`-mcmov`  :option:`-mror`  :option:`-mrori`  :option:`-msext`  :option:`-msfimm`  :option:`-mshftimm` 
  :option:`-mcmodel=code-model`

  *PDP-11 Options*

  .. program:: PDP-11

  :option:`-mfpu`  :option:`-msoft-float`  :option:`-mac0`  :option:`-mno-ac0`  :option:`-m40`  :option:`-m45`  :option:`-m10` 
  :option:`-mint32`  :option:`-mno-int16`  :option:`-mint16`  :option:`-mno-int32` 
  :option:`-msplit`  :option:`-munix-asm`  :option:`-mdec-asm`  :option:`-mgnu-asm`  :option:`-mlra`

  *picoChip Options*

  .. program:: picoChip

  :option:`-mae=ae_type`  :option:`-mvliw-lookahead=N` 
  :option:`-msymbol-as-address`  :option:`-mno-inefficient-warnings`

  *PowerPC Options*
  See RS/6000 and PowerPC Options.

  .. program:: PowerPC
  See RS/6000 and PowerPC.

  *PRU Options*

  .. program:: PRU

  :option:`-mmcu=mcu`  :option:`-minrt`  :option:`-mno-relax`  :option:`-mloop` 
  :option:`-mabi=variant` 

  *RISC-V Options*

  .. program:: RISC-V

  :option:`-mbranch-cost=N-instruction` 
  :option:`-mplt`  :option:`-mno-plt` 
  :option:`-mabi=ABI-string` 
  :option:`-mfdiv`  :option:`-mno-fdiv` 
  :option:`-mdiv`  :option:`-mno-div` 
  :option:`-misa-spec=ISA-spec-string` 
  :option:`-march=ISA-string` 
  :option:`-mtune=processor-string` 
  :option:`-mpreferred-stack-boundary=num` 
  :option:`-msmall-data-limit=N-bytes` 
  :option:`-msave-restore`  :option:`-mno-save-restore` 
  :option:`-mshorten-memrefs`  :option:`-mno-shorten-memrefs` 
  :option:`-mstrict-align`  :option:`-mno-strict-align` 
  :option:`-mcmodel=medlow`  :option:`-mcmodel=medany` 
  :option:`-mexplicit-relocs`  :option:`-mno-explicit-relocs` 
  :option:`-mrelax`  :option:`-mno-relax` 
  :option:`-mriscv-attribute`  :option:`-mno-riscv-attribute` 
  :option:`-malign-data=type` 
  :option:`-mbig-endian`  :option:`-mlittle-endian` 
  :option:`-mstack-protector-guard=guard`  :option:`-mstack-protector-guard-reg=reg` 
  :option:`-mstack-protector-guard-offset=offset`

  *RL78 Options*

  .. program:: RL78

  :option:`-msim`  :option:`-mmul=none`  :option:`-mmul=g13`  :option:`-mmul=g14`  :option:`-mallregs` 
  :option:`-mcpu=g10`  :option:`-mcpu=g13`  :option:`-mcpu=g14`  :option:`-mg10`  :option:`-mg13`  :option:`-mg14` 
  :option:`-m64bit-doubles`  :option:`-m32bit-doubles`  :option:`-msave-mduc-in-interrupts`

  *RS/6000 and PowerPC Options*

  .. program:: RS/6000 and PowerPC

  :option:`-mcpu=cpu-type` 
  :option:`-mtune=cpu-type` 
  :option:`-mcmodel=code-model` 
  :option:`-mpowerpc64` 
  :option:`-maltivec`  :option:`-mno-altivec` 
  :option:`-mpowerpc-gpopt`  :option:`-mno-powerpc-gpopt` 
  :option:`-mpowerpc-gfxopt`  :option:`-mno-powerpc-gfxopt` 
  :option:`-mmfcrf`  :option:`-mno-mfcrf`  :option:`-mpopcntb`  :option:`-mno-popcntb`  :option:`-mpopcntd`  :option:`-mno-popcntd` 
  :option:`-mfprnd`  :option:`-mno-fprnd` 
  :option:`-mcmpb`  :option:`-mno-cmpb`  :option:`-mhard-dfp`  :option:`-mno-hard-dfp` 
  :option:`-mfull-toc`   :option:`-mminimal-toc`  :option:`-mno-fp-in-toc`  :option:`-mno-sum-in-toc` 
  :option:`-m64`  :option:`-m32`  :option:`-mxl-compat`  :option:`-mno-xl-compat`  :option:`-mpe` 
  :option:`-malign-power`  :option:`-malign-natural` 
  :option:`-msoft-float`  :option:`-mhard-float`  :option:`-mmultiple`  :option:`-mno-multiple` 
  :option:`-mupdate`  :option:`-mno-update` 
  :option:`-mavoid-indexed-addresses`  :option:`-mno-avoid-indexed-addresses` 
  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-mbit-align`  :option:`-mno-bit-align` 
  :option:`-mstrict-align`  :option:`-mno-strict-align`  :option:`-mrelocatable` 
  :option:`-mno-relocatable`  :option:`-mrelocatable-lib`  :option:`-mno-relocatable-lib` 
  :option:`-mtoc`  :option:`-mno-toc`  :option:`-mlittle`  :option:`-mlittle-endian`  :option:`-mbig`  :option:`-mbig-endian` 
  :option:`-mdynamic-no-pic`  :option:`-mswdiv`  :option:`-msingle-pic-base` 
  :option:`-mprioritize-restricted-insns=priority` 
  :option:`-msched-costly-dep=dependence_type` 
  :option:`-minsert-sched-nops=scheme` 
  :option:`-mcall-aixdesc`  :option:`-mcall-eabi`  :option:`-mcall-freebsd`  
  :option:`-mcall-linux`  :option:`-mcall-netbsd`  :option:`-mcall-openbsd`  
  :option:`-mcall-sysv`  :option:`-mcall-sysv-eabi`  :option:`-mcall-sysv-noeabi` 
  :option:`-mtraceback=traceback_type` 
  :option:`-maix-struct-return`  :option:`-msvr4-struct-return` 
  :option:`-mabi=abi-type`  :option:`-msecure-plt`  :option:`-mbss-plt` 
  :option:`-mlongcall`  :option:`-mno-longcall`  :option:`-mpltseq`  :option:`-mno-pltseq`  
  :option:`-mblock-move-inline-limit=num` 
  :option:`-mblock-compare-inline-limit=num` 
  :option:`-mblock-compare-inline-loop-limit=num` 
  :option:`-mno-block-ops-unaligned-vsx` 
  :option:`-mstring-compare-inline-limit=num` 
  :option:`-misel`  :option:`-mno-isel` 
  :option:`-mvrsave`  :option:`-mno-vrsave` 
  :option:`-mmulhw`  :option:`-mno-mulhw` 
  :option:`-mdlmzb`  :option:`-mno-dlmzb` 
  :option:`-mprototype`  :option:`-mno-prototype` 
  :option:`-msim`  :option:`-mmvme`  :option:`-mads`  :option:`-myellowknife`  :option:`-memb`  :option:`-msdata` 
  :option:`-msdata=opt`  :option:`-mreadonly-in-sdata`  :option:`-mvxworks`  :option:`-G` :samp:`{num}` 
  :option:`-mrecip`  :option:`-mrecip=opt`  :option:`-mno-recip`  :option:`-mrecip-precision` 
  :option:`-mno-recip-precision` 
  :option:`-mveclibabi=type`  :option:`-mfriz`  :option:`-mno-friz` 
  :option:`-mpointers-to-nested-functions`  :option:`-mno-pointers-to-nested-functions` 
  :option:`-msave-toc-indirect`  :option:`-mno-save-toc-indirect` 
  :option:`-mpower8-fusion`  :option:`-mno-mpower8-fusion`  :option:`-mpower8-vector`  :option:`-mno-power8-vector` 
  :option:`-mcrypto`  :option:`-mno-crypto`  :option:`-mhtm`  :option:`-mno-htm` 
  :option:`-mquad-memory`  :option:`-mno-quad-memory` 
  :option:`-mquad-memory-atomic`  :option:`-mno-quad-memory-atomic` 
  :option:`-mcompat-align-parm`  :option:`-mno-compat-align-parm` 
  :option:`-mfloat128`  :option:`-mno-float128`  :option:`-mfloat128-hardware`  :option:`-mno-float128-hardware` 
  :option:`-mgnu-attribute`  :option:`-mno-gnu-attribute` 
  :option:`-mstack-protector-guard=guard` :option:`-mstack-protector-guard-reg=reg` 
  :option:`-mstack-protector-guard-offset=offset` :option:`-mprefixed` :option:`-mno-prefixed` 
  :option:`-mpcrel` :option:`-mno-pcrel` :option:`-mmma` :option:`-mno-mmma` :option:`-mrop-protect` :option:`-mno-rop-protect` 
  :option:`-mprivileged` :option:`-mno-privileged`

  *RX Options*

  .. program:: RX

  :option:`-m64bit-doubles`  :option:`-m32bit-doubles`  :option:`-fpu`  :option:`-nofpu`
  :option:`-mcpu=`
  :option:`-mbig-endian-data`  :option:`-mlittle-endian-data` 
  :option:`-msmall-data` 
  :option:`-msim`  :option:`-mno-sim`
  :option:`-mas100-syntax`  :option:`-mno-as100-syntax`
  :option:`-mrelax`
  :option:`-mmax-constant-size=`
  :option:`-mint-register=`
  :option:`-mpid`
  :option:`-mallow-string-insns`  :option:`-mno-allow-string-insns`
  :option:`-mjsr`
  :option:`-mno-warn-multiple-fast-interrupts`
  :option:`-msave-acc-in-interrupts`

  *S/390 and zSeries Options*

  .. program:: S/390 and zSeries

  :option:`-mtune=cpu-type`  :option:`-march=cpu-type` 
  :option:`-mhard-float`  :option:`-msoft-float`  :option:`-mhard-dfp`  :option:`-mno-hard-dfp` 
  :option:`-mlong-double-64`  :option:`-mlong-double-128` 
  :option:`-mbackchain`  :option:`-mno-backchain`  :option:`-mpacked-stack`  :option:`-mno-packed-stack` 
  :option:`-msmall-exec`  :option:`-mno-small-exec`  :option:`-mmvcle`  :option:`-mno-mvcle` 
  :option:`-m64`  :option:`-m31`  :option:`-mdebug`  :option:`-mno-debug`  :option:`-mesa`  :option:`-mzarch` 
  :option:`-mhtm`  :option:`-mvx`  :option:`-mzvector` 
  :option:`-mtpf-trace`  :option:`-mno-tpf-trace`  :option:`-mtpf-trace-skip`  :option:`-mno-tpf-trace-skip` 
  :option:`-mfused-madd`  :option:`-mno-fused-madd` 
  :option:`-mwarn-framesize`  :option:`-mwarn-dynamicstack`  :option:`-mstack-size`  :option:`-mstack-guard` 
  :option:`-mhotpatch=halfwords`, :samp:`{halfwords}`

  *Score Options*

  .. program:: Score

  :option:`-meb`  :option:`-mel` 
  :option:`-mnhwloop` 
  :option:`-muls` 
  :option:`-mmac` 
  :option:`-mscore5`  :option:`-mscore5u`  :option:`-mscore7`  :option:`-mscore7d`

  *SH Options*

  .. program:: SH

  :option:`-m1`  :option:`-m2`  :option:`-m2e` 
  :option:`-m2a-nofpu`  :option:`-m2a-single-only`  :option:`-m2a-single`  :option:`-m2a` 
  :option:`-m3`  :option:`-m3e` 
  :option:`-m4-nofpu`  :option:`-m4-single-only`  :option:`-m4-single`  :option:`-m4` 
  :option:`-m4a-nofpu`  :option:`-m4a-single-only`  :option:`-m4a-single`  :option:`-m4a`  :option:`-m4al` 
  :option:`-mb`  :option:`-ml`  :option:`-mdalign`  :option:`-mrelax` 
  :option:`-mbigtable`  :option:`-mfmovd`  :option:`-mrenesas`  :option:`-mno-renesas`  :option:`-mnomacsave` 
  :option:`-mieee`  :option:`-mno-ieee`  :option:`-mbitops`  :option:`-misize`  :option:`-minline-ic_invalidate`  :option:`-mpadstruct` 
  :option:`-mprefergot`  :option:`-musermode`  :option:`-multcost=number`  :option:`-mdiv=strategy` 
  :option:`-mdivsi3_libfunc=name`  :option:`-mfixed-range=register-range` 
  :option:`-maccumulate-outgoing-args` 
  :option:`-matomic-model=atomic-model` 
  :option:`-mbranch-cost=num`  :option:`-mzdcbranch`  :option:`-mno-zdcbranch` 
  :option:`-mcbranch-force-delay-slot` 
  :option:`-mfused-madd`  :option:`-mno-fused-madd`  :option:`-mfsca`  :option:`-mno-fsca`  :option:`-mfsrra`  :option:`-mno-fsrra` 
  :option:`-mpretend-cmove`  :option:`-mtas`

  *Solaris 2 Options*

  .. program:: Solaris 2

  :option:`-mclear-hwcap`  :option:`-mno-clear-hwcap`  :option:`-mimpure-text`  :option:`-mno-impure-text` 
  :option:`-pthreads`

  *SPARC Options*

  .. program:: SPARC

  :option:`-mcpu=cpu-type` 
  :option:`-mtune=cpu-type` 
  :option:`-mcmodel=code-model` 
  :option:`-mmemory-model=mem-model` 
  :option:`-m32`  :option:`-m64`  :option:`-mapp-regs`  :option:`-mno-app-regs` 
  :option:`-mfaster-structs`  :option:`-mno-faster-structs`  :option:`-mflat`  :option:`-mno-flat` 
  :option:`-mfpu`  :option:`-mno-fpu`  :option:`-mhard-float`  :option:`-msoft-float` 
  :option:`-mhard-quad-float`  :option:`-msoft-quad-float` 
  :option:`-mstack-bias`  :option:`-mno-stack-bias` 
  :option:`-mstd-struct-return`  :option:`-mno-std-struct-return` 
  :option:`-munaligned-doubles`  :option:`-mno-unaligned-doubles` 
  :option:`-muser-mode`  :option:`-mno-user-mode` 
  :option:`-mv8plus`  :option:`-mno-v8plus`  :option:`-mvis`  :option:`-mno-vis` 
  :option:`-mvis2`  :option:`-mno-vis2`  :option:`-mvis3`  :option:`-mno-vis3` 
  :option:`-mvis4`  :option:`-mno-vis4`  :option:`-mvis4b`  :option:`-mno-vis4b` 
  :option:`-mcbcond`  :option:`-mno-cbcond`  :option:`-mfmaf`  :option:`-mno-fmaf`  :option:`-mfsmuld`  :option:`-mno-fsmuld`  
  :option:`-mpopc`  :option:`-mno-popc`  :option:`-msubxc`  :option:`-mno-subxc` 
  :option:`-mfix-at697f`  :option:`-mfix-ut699`  :option:`-mfix-ut700`  :option:`-mfix-gr712rc` 
  :option:`-mlra`  :option:`-mno-lra`

  *System V Options*

  .. program:: System V

  :option:`-Qy`  :option:`-Qn`  :option:`-YP,paths`  :option:`-Ym,dir`

  *V850 Options*

  .. program:: V850

  :option:`-mlong-calls`  :option:`-mno-long-calls`  :option:`-mep`  :option:`-mno-ep` 
  :option:`-mprolog-function`  :option:`-mno-prolog-function`  :option:`-mspace` 
  :option:`-mtda=n`  :option:`-msda=n`  :option:`-mzda=n` 
  :option:`-mapp-regs`  :option:`-mno-app-regs` 
  :option:`-mdisable-callt`  :option:`-mno-disable-callt` 
  :option:`-mv850e2v3`  :option:`-mv850e2`  :option:`-mv850e1`  :option:`-mv850es` 
  :option:`-mv850e`  :option:`-mv850`  :option:`-mv850e3v5` 
  :option:`-mloop` 
  :option:`-mrelax` 
  :option:`-mlong-jumps` 
  :option:`-msoft-float` 
  :option:`-mhard-float` 
  :option:`-mgcc-abi` 
  :option:`-mrh850-abi` 
  :option:`-mbig-switch`

  *VAX Options*

  .. program:: VAX

  :option:`-mg`  :option:`-mgnu`  :option:`-munix`  :option:`-mlra`

  *Visium Options*

  .. program:: Visium

  :option:`-mdebug`  :option:`-msim`  :option:`-mfpu`  :option:`-mno-fpu`  :option:`-mhard-float`  :option:`-msoft-float` 
  :option:`-mcpu=cpu-type`  :option:`-mtune=cpu-type`  :option:`-msv-mode`  :option:`-muser-mode`

  *VMS Options*

  .. program:: VMS

  :option:`-mvms-return-codes`  :option:`-mdebug-main=prefix`  :option:`-mmalloc64` 
  :option:`-mpointer-size=size`

  *VxWorks Options*

  .. program:: VxWorks

  :option:`-mrtp`  :option:`-non-static`  :option:`-Bstatic`  :option:`-Bdynamic` 
  :option:`-Xbind-lazy`  :option:`-Xbind-now`

  *x86 Options*

  .. program:: x86

  :option:`-mtune=cpu-type`  :option:`-march=cpu-type` 
  :option:`-mtune-ctrl=feature-list`  :option:`-mdump-tune-features`  :option:`-mno-default` 
  :option:`-mfpmath=unit` 
  :option:`-masm=dialect`  :option:`-mno-fancy-math-387` 
  :option:`-mno-fp-ret-in-387`  :option:`-m80387`  :option:`-mhard-float`  :option:`-msoft-float` 
  :option:`-mno-wide-multiply`  :option:`-mrtd`  :option:`-malign-double` 
  :option:`-mpreferred-stack-boundary=num` 
  :option:`-mincoming-stack-boundary=num` 
  :option:`-mcld`  :option:`-mcx16`  :option:`-msahf`  :option:`-mmovbe`  :option:`-mcrc32` :option:`-mmwait` 
  :option:`-mrecip`  :option:`-mrecip=opt` 
  :option:`-mvzeroupper`  :option:`-mprefer-avx128`  :option:`-mprefer-vector-width=opt` 
  :option:`-mmove-max=bits` :option:`-mstore-max=bits` 
  :option:`-mmmx`  :option:`-msse`  :option:`-msse2`  :option:`-msse3`  :option:`-mssse3`  :option:`-msse4.1`  :option:`-msse4.2`  :option:`-msse4`  :option:`-mavx` 
  :option:`-mavx2`  :option:`-mavx512f`  :option:`-mavx512pf`  :option:`-mavx512er`  :option:`-mavx512cd`  :option:`-mavx512vl` 
  :option:`-mavx512bw`  :option:`-mavx512dq`  :option:`-mavx512ifma`  :option:`-mavx512vbmi`  :option:`-msha`  :option:`-maes` 
  :option:`-mpclmul`  :option:`-mfsgsbase`  :option:`-mrdrnd`  :option:`-mf16c`  :option:`-mfma`  :option:`-mpconfig`  :option:`-mwbnoinvd`  
  :option:`-mptwrite`  :option:`-mprefetchwt1`  :option:`-mclflushopt`  :option:`-mclwb`  :option:`-mxsavec`  :option:`-mxsaves` 
  :option:`-msse4a`  :option:`-m3dnow`  :option:`-m3dnowa`  :option:`-mpopcnt`  :option:`-mabm`  :option:`-mbmi`  :option:`-mtbm`  :option:`-mfma4`  :option:`-mxop` 
  :option:`-madx`  :option:`-mlzcnt`  :option:`-mbmi2`  :option:`-mfxsr`  :option:`-mxsave`  :option:`-mxsaveopt`  :option:`-mrtm`  :option:`-mhle`  :option:`-mlwp` 
  :option:`-mmwaitx`  :option:`-mclzero`  :option:`-mpku`  :option:`-mthreads`  :option:`-mgfni`  :option:`-mvaes`  :option:`-mwaitpkg` 
  :option:`-mshstk` :option:`-mmanual-endbr` :option:`-mcet-switch` :option:`-mforce-indirect-call` 
  :option:`-mavx512vbmi2` :option:`-mavx512bf16` :option:`-menqcmd` 
  :option:`-mvpclmulqdq`  :option:`-mavx512bitalg`  :option:`-mmovdiri`  :option:`-mmovdir64b`  :option:`-mavx512vpopcntdq` 
  :option:`-mavx5124fmaps`  :option:`-mavx512vnni`  :option:`-mavx5124vnniw`  :option:`-mprfchw`  :option:`-mrdpid` 
  :option:`-mrdseed`  :option:`-msgx` :option:`-mavx512vp2intersect` :option:`-mserialize` :option:`-mtsxldtrk`
  :option:`-mamx-tile`  :option:`-mamx-int8`  :option:`-mamx-bf16` :option:`-muintr` :option:`-mhreset` :option:`-mavxvnni`
  :option:`-mavx512fp16` 
  :option:`-mcldemote`  :option:`-mms-bitfields`  :option:`-mno-align-stringops`  :option:`-minline-all-stringops` 
  :option:`-minline-stringops-dynamically`  :option:`-mstringop-strategy=alg` 
  :option:`-mkl` :option:`-mwidekl` 
  :option:`-mmemcpy-strategy=strategy`  :option:`-mmemset-strategy=strategy` 
  :option:`-mpush-args`  :option:`-maccumulate-outgoing-args`  :option:`-m128bit-long-double` 
  :option:`-m96bit-long-double`  :option:`-mlong-double-64`  :option:`-mlong-double-80`  :option:`-mlong-double-128` 
  :option:`-mregparm=num`  :option:`-msseregparm` 
  :option:`-mveclibabi=type`  :option:`-mvect8-ret-in-mem` 
  :option:`-mpc32`  :option:`-mpc64`  :option:`-mpc80`  :option:`-mstackrealign` 
  :option:`-momit-leaf-frame-pointer`  :option:`-mno-red-zone`  :option:`-mno-tls-direct-seg-refs` 
  :option:`-mcmodel=code-model`  :option:`-mabi=name`  :option:`-maddress-mode=mode` 
  :option:`-m32`  :option:`-m64`  :option:`-mx32`  :option:`-m16`  :option:`-miamcu`  :option:`-mlarge-data-threshold=num` 
  :option:`-msse2avx`  :option:`-mfentry`  :option:`-mrecord-mcount`  :option:`-mnop-mcount`  :option:`-m8bit-idiv` 
  :option:`-minstrument-return=type` :option:`-mfentry-name=name` :option:`-mfentry-section=name` 
  :option:`-mavx256-split-unaligned-load`  :option:`-mavx256-split-unaligned-store` 
  :option:`-malign-data=type`  :option:`-mstack-protector-guard=guard` 
  :option:`-mstack-protector-guard-reg=reg` 
  :option:`-mstack-protector-guard-offset=offset` 
  :option:`-mstack-protector-guard-symbol=symbol` 
  :option:`-mgeneral-regs-only`  :option:`-mcall-ms2sysv-xlogues` :option:`-mrelax-cmpxchg-loop` 
  :option:`-mindirect-branch=choice`  :option:`-mfunction-return=choice` 
  :option:`-mindirect-branch-register` :option:`-mharden-sls=choice` 
  :option:`-mindirect-branch-cs-prefix` :option:`-mneeded` :option:`-mno-direct-extern-access`

  *x86 Windows Options*

  .. program:: x86 Windows

  :option:`-mconsole`  :option:`-mcygwin`  :option:`-mno-cygwin`  :option:`-mdll` 
  :option:`-mnop-fun-dllimport`  :option:`-mthread` 
  :option:`-municode`  :option:`-mwin32`  :option:`-mwindows`  :option:`-fno-set-stack-executable`

  *Xstormy16 Options*

  .. program:: Xstormy16

  :option:`-msim`

  *Xtensa Options*

  .. program:: Xtensa

  :option:`-mconst16`  :option:`-mno-const16` 
  :option:`-mfused-madd`  :option:`-mno-fused-madd` 
  :option:`-mforce-no-pic` 
  :option:`-mserialize-volatile`  :option:`-mno-serialize-volatile` 
  :option:`-mtext-section-literals`  :option:`-mno-text-section-literals` 
  :option:`-mauto-litpools`  :option:`-mno-auto-litpools` 
  :option:`-mtarget-align`  :option:`-mno-target-align` 
  :option:`-mlongcalls`  :option:`-mno-longcalls` 
  :option:`-mabi=abi-type` 
  :option:`-mextra-l32r-costs=cycles`

  *zSeries Options*
  See S/390 and zSeries Options.

  .. program:: zSeries
  See S/390 and zSeries.

  .. program:: None

