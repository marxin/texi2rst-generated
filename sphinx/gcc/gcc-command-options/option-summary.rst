..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. program:: gcc

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
  :option:`-v`  :option:`-###`  :option:`--help`:samp:`[={class}[,...]]`  :option:`--target-help`  :option:`--version` 
  :option:`-pass-exit-codes`  :option:`-pipe`  :option:`-specs`:samp:`={file}`  :option:`-wrapper`:samp:`@{file}`
  :option:`-ffile-prefix-map`:samp:`={old}={new}`
  :option:`-fplugin`:samp:`={file}`  :option:`-fplugin-arg-`:samp:`{name}={arg}`
  :option:`-fdump-ada-spec`:samp:`[-{slim}]` :option:`-fada-spec-parent`:samp:`={unit}`  :option:`-fdump-go-spec`:samp:`={file}`

*C Language Options*
  See :ref:`c-dialect-options`.

  :option:`-ansi`  :option:`-std`:samp:`={standard}`  :option:`-aux-info` :samp:`{filename}` 
  :option:`-fallow-parameterless-variadic-functions`  :option:`-fno-asm`  
  :option:`-fno-builtin`  :option:`-fno-builtin-`:samp:`{function}`  :option:`-fcond-mismatch` 
  :option:`-ffreestanding`  :option:`-fgimple`  :option:`-fgnu-tm`  :option:`-fgnu89-inline`  :option:`-fhosted` 
  :option:`-flax-vector-conversions`  :option:`-fms-extensions` 
  :option:`-foffload`:samp:`={arg}`  :option:`-foffload-options`:samp:`={arg}` 
  :option:`-fopenacc`  :option:`-fopenacc-dim`:samp:`={geom}` 
  :option:`-fopenmp`  :option:`-fopenmp-simd` 
  :option:`-fpermitted-flt-eval-methods`:samp:`={standard}` 
  :option:`-fplan9-extensions`  :option:`-fsigned-bitfields`  :option:`-funsigned-bitfields` 
  :option:`-fsigned-char`  :option:`-funsigned-char`  :option:`-fsso-struct`:samp:`={endianness}`

*C++ Language Options*
  See :ref:`c++-dialect-options`.

  :option:`-fabi-version`:samp:`={n}`  :option:`-fno-access-control` 
  :option:`-faligned-new`:samp:`={n}`  :option:`-fargs-in-order`:samp:`={n}`  :option:`-fchar8_t`  :option:`-fcheck-new` 
  :option:`-fconstexpr-depth`:samp:`={n}`  :option:`-fconstexpr-cache-depth`:samp:`={n}` 
  :option:`-fconstexpr-loop-limit`:samp:`={n}`  :option:`-fconstexpr-ops-limit`:samp:`={n}` 
  :option:`-fno-elide-constructors` 
  :option:`-fno-enforce-eh-specs` 
  :option:`-fno-gnu-keywords` 
  :option:`-fno-implicit-templates` 
  :option:`-fno-implicit-inline-templates` 
  :option:`-fno-implement-inlines`  
  :option:`-fmodule-header`:samp:`[={kind}]` :option:`-fmodule-only` :option:`-fmodules-ts` 
  :option:`-fmodule-implicit-inline` 
  :option:`-fno-module-lazy` 
  :option:`-fmodule-mapper`:samp:`={specification}` 
  :option:`-fmodule-version-ignore` 
  :option:`-fms-extensions` 
  :option:`-fnew-inheriting-ctors` 
  :option:`-fnew-ttp-matching` 
  :option:`-fno-nonansi-builtins`  :option:`-fnothrow-opt`  :option:`-fno-operator-names` 
  :option:`-fno-optional-diags`  :option:`-fpermissive` 
  :option:`-fno-pretty-templates` 
  :option:`-fno-rtti`  :option:`-fsized-deallocation` 
  :option:`-ftemplate-backtrace-limit`:samp:`={n}` 
  :option:`-ftemplate-depth`:samp:`={n}` 
  :option:`-fno-threadsafe-statics`  :option:`-fuse-cxa-atexit` 
  :option:`-fno-weak`  :option:`-nostdinc++` 
  :option:`-fvisibility-inlines-hidden` 
  :option:`-fvisibility-ms-compat` 
  :option:`-fext-numeric-literals` 
  :option:`-flang-info-include-translate`:samp:`=[{header}]`
  :option:`-flang-info-include-translate-not` 
  :option:`-flang-info-module-cmi`:samp:`[={module}]`
  :option:`-stdlib`:samp:`={libstdc++,libc++}` 
  :option:`-Wabi-tag`  :option:`-Wcatch-value`  :option:`-Wcatch-value`:samp:`={n}` 
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
  :option:`-Wpessimizing-move`  :option:`-Wno-placement-new`  :option:`-Wplacement-new`:samp:`={n}` 
  :option:`-Wrange-loop-construct` :option:`-Wredundant-move` :option:`-Wredundant-tags` 
  :option:`-Wreorder`  :option:`-Wregister` 
  :option:`-Wstrict-null-sentinel`  :option:`-Wno-subobject-linkage`  :option:`-Wtemplates` 
  :option:`-Wno-non-template-friend`  :option:`-Wold-style-cast` 
  :option:`-Woverloaded-virtual`  :option:`-Wno-pmf-conversions` :option:`-Wsign-promo` 
  :option:`-Wsized-deallocation`  :option:`-Wsuggest-final-methods` 
  :option:`-Wsuggest-final-types`  :option:`-Wsuggest-override`  
  :option:`-Wno-terminate`  :option:`-Wuseless-cast`  :option:`-Wno-vexing-parse`  
  :option:`-Wvirtual-inheritance`  
  :option:`-Wno-virtual-move-assign`  :option:`-Wvolatile`  :option:`-Wzero-as-null-pointer-constant`

*Objective-C and Objective-C++ Language Options*
  See :ref:`objective-c-and-objective-c++-dialect-options`.

  :option:`-fconstant-string-class`:samp:`={class-name}` 
  :option:`-fgnu-runtime`  :option:`-fnext-runtime` 
  :option:`-fno-nil-receivers` 
  :option:`-fobjc-abi-version`:samp:`={n}` 
  :option:`-fobjc-call-cxx-cdtors` 
  :option:`-fobjc-direct-dispatch` 
  :option:`-fobjc-exceptions` 
  :option:`-fobjc-gc` 
  :option:`-fobjc-nilcheck` 
  :option:`-fobjc-std`:samp:`=objc1` 
  :option:`-fno-local-ivars` 
  :option:`-fivar-visibility`:samp:`=[public|protected|private|package]`
  :option:`-freplace-objc-classes` 
  :option:`-fzero-link` 
  :option:`-gen-decls` 
  :option:`-Wassign-intercept`  :option:`-Wno-property-assign-default` 
  :option:`-Wno-protocol` :option:`-Wobjc-root-class` :option:`-Wselector` 
  :option:`-Wstrict-selector-match` 
  :option:`-Wundeclared-selector`

*Diagnostic Message Formatting Options*
  See :ref:`diagnostic-message-formatting-options`.

  :option:`-fmessage-length`:samp:`={n}`  
  :option:`-fdiagnostics-plain-output` 
  :option:`-fdiagnostics-show-location`:samp:`=[once|every-line]`
  :option:`-fdiagnostics-color`:samp:`=[auto|never|always]` 
  :option:`-fdiagnostics-urls`:samp:`=[auto|never|always]`
  :option:`-fdiagnostics-format`:samp:`=[text|json]`
  :option:`-fno-diagnostics-show-option`  :option:`-fno-diagnostics-show-caret` 
  :option:`-fno-diagnostics-show-labels`  :option:`-fno-diagnostics-show-line-numbers` 
  :option:`-fno-diagnostics-show-cwe`  
  :option:`-fdiagnostics-minimum-margin-width`:samp:`={width}` 
  :option:`-fdiagnostics-parseable-fixits`  :option:`-fdiagnostics-generate-patch` 
  :option:`-fdiagnostics-show-template-tree`  :option:`-fno-elide-type` 
  :option:`-fdiagnostics-path-format`:samp:`=[none|separate-events|inline-events]`
  :option:`-fdiagnostics-show-path-depths` 
  :option:`-fno-show-column` 
  :option:`-fdiagnostics-column-unit`:samp:`=[display|byte]`
  :option:`-fdiagnostics-column-origin`:samp:`={origin}`
  :option:`-fdiagnostics-escape-format`:samp:`=[unicode|bytes]`

*Warning Options*
  See :ref:`warning-options`.

  :option:`-fsyntax-only`  :option:`-fmax-errors`:samp:`={n}`  :option:`-Wpedantic` 
  :option:`-pedantic-errors` 
  :option:`-w`  :option:`-Wextra`  :option:`-Wall`  :option:`-Wabi`:samp:`={n}` 
  :option:`-Waddress`  :option:`-Wno-address-of-packed-member`  :option:`-Waggregate-return` 
  :option:`-Walloc-size-larger-than`:samp:`={byte-size}`  :option:`-Walloc-zero` 
  :option:`-Walloca`  :option:`-Walloca-larger-than`:samp:`={byte-size}` 
  :option:`-Wno-aggressive-loop-optimizations` 
  :option:`-Warith-conversion` 
  :option:`-Warray-bounds`  :option:`-Warray-bounds`:samp:`={n}`  :option:`-Warray-compare` 
  :option:`-Wno-attributes`  :option:`-Wattribute-alias`:samp:`={n}` :option:`-Wno-attribute-alias` 
  :option:`-Wno-attribute-warning`  
  :option:`-Wbidi-chars`:samp:`=[none|unpaired|any]`
  :option:`-Wbool-compare`  :option:`-Wbool-operation` 
  :option:`-Wno-builtin-declaration-mismatch` 
  :option:`-Wno-builtin-macro-redefined`  :option:`-Wc90-c99-compat`  :option:`-Wc99-c11-compat` 
  :option:`-Wc11-c2x-compat` 
  :option:`-Wc++-compat`  :option:`-Wc++11-compat`  :option:`-Wc++14-compat`  :option:`-Wc++17-compat`  
  :option:`-Wc++20-compat`   
  :option:`-Wno-c++11-extensions`  :option:`-Wno-c++14-extensions` :option:`-Wno-c++17-extensions`  
  :option:`-Wno-c++20-extensions`  :option:`-Wno-c++23-extensions`  
  :option:`-Wcast-align`  :option:`-Wcast-align`:samp:`=strict`  :option:`-Wcast-function-type`  :option:`-Wcast-qual`  
  :option:`-Wchar-subscripts` 
  :option:`-Wclobbered`  :option:`-Wcomment` 
  :option:`-Wconversion`  :option:`-Wno-coverage-mismatch`  :option:`-Wno-cpp` 
  :option:`-Wdangling-else`  :option:`-Wdate-time` 
  :option:`-Wno-deprecated`  :option:`-Wno-deprecated-declarations`  :option:`-Wno-designated-init` 
  :option:`-Wdisabled-optimization` 
  :option:`-Wno-discarded-array-qualifiers`  :option:`-Wno-discarded-qualifiers` 
  :option:`-Wno-div-by-zero`  :option:`-Wdouble-promotion` 
  :option:`-Wduplicated-branches`  :option:`-Wduplicated-cond` 
  :option:`-Wempty-body`  :option:`-Wno-endif-labels`  :option:`-Wenum-compare`  :option:`-Wenum-conversion` 
  :option:`-Werror`  :option:`-Werror`:samp:`=*`  :option:`-Wexpansion-to-defined`  :option:`-Wfatal-errors` 
  :option:`-Wfloat-conversion`  :option:`-Wfloat-equal`  :option:`-Wformat`  :option:`-Wformat`:samp:`=2` 
  :option:`-Wno-format-contains-nul`  :option:`-Wno-format-extra-args`  
  :option:`-Wformat-nonliteral`  :option:`-Wformat-overflow`:samp:`={n}` 
  :option:`-Wformat-security`  :option:`-Wformat-signedness`  :option:`-Wformat-truncation`:samp:`={n}` 
  :option:`-Wformat-y2k`  :option:`-Wframe-address` 
  :option:`-Wframe-larger-than`:samp:`={byte-size}`  :option:`-Wno-free-nonheap-object` 
  :option:`-Wno-if-not-aligned`  :option:`-Wno-ignored-attributes` 
  :option:`-Wignored-qualifiers`  :option:`-Wno-incompatible-pointer-types` 
  :option:`-Wimplicit`  :option:`-Wimplicit-fallthrough`  :option:`-Wimplicit-fallthrough`:samp:`={n}` 
  :option:`-Wno-implicit-function-declaration`  :option:`-Wno-implicit-int` 
  :option:`-Winfinite-recursion` 
  :option:`-Winit-self`  :option:`-Winline`  :option:`-Wno-int-conversion`  :option:`-Wint-in-bool-context` 
  :option:`-Wno-int-to-pointer-cast`  :option:`-Wno-invalid-memory-model` 
  :option:`-Winvalid-pch`  :option:`-Wjump-misses-init`  :option:`-Wlarger-than`:samp:`={byte-size}` 
  :option:`-Wlogical-not-parentheses`  :option:`-Wlogical-op`  :option:`-Wlong-long` 
  :option:`-Wno-lto-type-mismatch` :option:`-Wmain`  :option:`-Wmaybe-uninitialized` 
  :option:`-Wmemset-elt-size`  :option:`-Wmemset-transposed-args` 
  :option:`-Wmisleading-indentation`  :option:`-Wmissing-attributes`  :option:`-Wmissing-braces` 
  :option:`-Wmissing-field-initializers`  :option:`-Wmissing-format-attribute` 
  :option:`-Wmissing-include-dirs`  :option:`-Wmissing-noreturn`  :option:`-Wno-missing-profile` 
  :option:`-Wno-multichar`  :option:`-Wmultistatement-macros`  :option:`-Wnonnull`  :option:`-Wnonnull-compare` 
  :option:`-Wnormalized`:samp:`=[none|id|nfc|nfkc]`
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
  :option:`-Wshadow`  :option:`-Wshadow`:samp:`=global`  :option:`-Wshadow`:samp:`=local`  :option:`-Wshadow`:samp:`=compatible-local` 
  :option:`-Wno-shadow-ivar` 
  :option:`-Wno-shift-count-negative`  :option:`-Wno-shift-count-overflow`  :option:`-Wshift-negative-value` 
  :option:`-Wno-shift-overflow`  :option:`-Wshift-overflow`:samp:`={n}` 
  :option:`-Wsign-compare`  :option:`-Wsign-conversion` 
  :option:`-Wno-sizeof-array-argument` 
  :option:`-Wsizeof-array-div` 
  :option:`-Wsizeof-pointer-div`  :option:`-Wsizeof-pointer-memaccess` 
  :option:`-Wstack-protector`  :option:`-Wstack-usage`:samp:`={byte-size}`  :option:`-Wstrict-aliasing` 
  :option:`-Wstrict-aliasing`:samp:`=n`  :option:`-Wstrict-overflow`  :option:`-Wstrict-overflow`:samp:`={n}` 
  :option:`-Wstring-compare` 
  :option:`-Wno-stringop-overflow` :option:`-Wno-stringop-overread` 
  :option:`-Wno-stringop-truncation` 
  :option:`-Wsuggest-attribute`:samp:`=[pure|const|noreturn|format|malloc]` 
  :option:`-Wswitch`  :option:`-Wno-switch-bool`  :option:`-Wswitch-default`  :option:`-Wswitch-enum` 
  :option:`-Wno-switch-outside-range`  :option:`-Wno-switch-unreachable`  :option:`-Wsync-nand` 
  :option:`-Wsystem-headers`  :option:`-Wtautological-compare`  :option:`-Wtrampolines`  :option:`-Wtrigraphs` 
  :option:`-Wtsan` :option:`-Wtype-limits`  :option:`-Wundef` 
  :option:`-Wuninitialized`  :option:`-Wunknown-pragmas` 
  :option:`-Wunsuffixed-float-constants`  :option:`-Wunused` 
  :option:`-Wunused-but-set-parameter`  :option:`-Wunused-but-set-variable` 
  :option:`-Wunused-const-variable`  :option:`-Wunused-const-variable`:samp:`={n}` 
  :option:`-Wunused-function`  :option:`-Wunused-label`  :option:`-Wunused-local-typedefs` 
  :option:`-Wunused-macros` 
  :option:`-Wunused-parameter`  :option:`-Wno-unused-result` 
  :option:`-Wunused-value`  :option:`-Wunused-variable` 
  :option:`-Wno-varargs`  :option:`-Wvariadic-macros` 
  :option:`-Wvector-operation-performance` 
  :option:`-Wvla`  :option:`-Wvla-larger-than`:samp:`={byte-size}`  :option:`-Wno-vla-larger-than` 
  :option:`-Wvolatile-register-var`  :option:`-Wwrite-strings` 
  :option:`-Wzero-length-bounds`

*Static Analyzer Options*
  :option:`-fanalyzer` 
  :option:`-fanalyzer-call-summaries` 
  :option:`-fanalyzer-checker`:samp:`={name}` 
  :option:`-fno-analyzer-feasibility` 
  :option:`-fanalyzer-fine-grained` 
  :option:`-fanalyzer-state-merge` 
  :option:`-fanalyzer-state-purge` 
  :option:`-fanalyzer-transitivity` 
  :option:`-fanalyzer-verbose-edges` 
  :option:`-fanalyzer-verbose-state-changes` 
  :option:`-fanalyzer-verbosity`:samp:`={level}` 
  :option:`-fdump-analyzer` 
  :option:`-fdump-analyzer-stderr` 
  :option:`-fdump-analyzer-callgraph` 
  :option:`-fdump-analyzer-exploded-graph` 
  :option:`-fdump-analyzer-exploded-nodes` 
  :option:`-fdump-analyzer-exploded-nodes-2` 
  :option:`-fdump-analyzer-exploded-nodes-3` 
  :option:`-fdump-analyzer-exploded-paths` 
  :option:`-fdump-analyzer-feasibility` 
  :option:`-fdump-analyzer-json` 
  :option:`-fdump-analyzer-state-purge` 
  :option:`-fdump-analyzer-supergraph` 
  :option:`-Wno-analyzer-double-fclose` 
  :option:`-Wno-analyzer-double-free` 
  :option:`-Wno-analyzer-exposure-through-output-file` 
  :option:`-Wno-analyzer-file-leak` 
  :option:`-Wno-analyzer-free-of-non-heap` 
  :option:`-Wno-analyzer-malloc-leak` 
  :option:`-Wno-analyzer-mismatching-deallocation` 
  :option:`-Wno-analyzer-null-argument` 
  :option:`-Wno-analyzer-null-dereference` 
  :option:`-Wno-analyzer-possible-null-argument` 
  :option:`-Wno-analyzer-possible-null-dereference` 
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

  :option:`-g`  :option:`-g`:samp:`{level}`  :option:`-gdwarf`  :option:`-gdwarf-`:samp:`{version}` 
  :option:`-gbtf` :option:`-gctf`  :option:`-gctf`:samp:`{level}` 
  :option:`-ggdb`  :option:`-grecord-gcc-switches`  :option:`-gno-record-gcc-switches` 
  :option:`-gstabs`  :option:`-gstabs+`  :option:`-gstrict-dwarf`  :option:`-gno-strict-dwarf` 
  :option:`-gas-loc-support`  :option:`-gno-as-loc-support` 
  :option:`-gas-locview-support`  :option:`-gno-as-locview-support` 
  :option:`-gcolumn-info`  :option:`-gno-column-info`  :option:`-gdwarf32`  :option:`-gdwarf64` 
  :option:`-gstatement-frontiers`  :option:`-gno-statement-frontiers` 
  :option:`-gvariable-location-views`  :option:`-gno-variable-location-views` 
  :option:`-ginternal-reset-location-views`  :option:`-gno-internal-reset-location-views` 
  :option:`-ginline-points`  :option:`-gno-inline-points` 
  :option:`-gvms`  :option:`-gxcoff`  :option:`-gxcoff+`  :option:`-gz`:samp:`[={type}]`
  :option:`-gsplit-dwarf`  :option:`-gdescribe-dies`  :option:`-gno-describe-dies` 
  :option:`-fdebug-prefix-map`:samp:`={old}={new}`  :option:`-fdebug-types-section` 
  :option:`-fno-eliminate-unused-debug-types` 
  :option:`-femit-struct-debug-baseonly`  :option:`-femit-struct-debug-reduced` 
  :option:`-femit-struct-debug-detailed`:samp:`[={spec-list}]` 
  :option:`-fno-eliminate-unused-debug-symbols`  :option:`-femit-class-debug-always` 
  :option:`-fno-merge-debug-strings`  :option:`-fno-dwarf2-cfi-asm` 
  :option:`-fvar-tracking`  :option:`-fvar-tracking-assignments`

*Optimization Options*
  See :ref:`optimize-options`.

  :option:`-faggressive-loop-optimizations` 
  :option:`-falign-functions`:samp:`[={n}[{m}:[{n2}[:{m2}]]]]`
  :option:`-falign-jumps`:samp:`[={n}[{m}:[{n2}[:{m2}]]]]`
  :option:`-falign-labels`:samp:`[={n}[{m}:[{n2}[:{m2}]]]]`
  :option:`-falign-loops`:samp:`[={n}[{m}:[{n2}[:{m2}]]]]`
  :option:`-fno-allocation-dce` :option:`-fallow-store-data-races` 
  :option:`-fassociative-math`  :option:`-fauto-profile`  :option:`-fauto-profile`:samp:`[={path}]`
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
  :option:`-ffast-math`  :option:`-ffinite-math-only`  :option:`-ffloat-store`  :option:`-fexcess-precision`:samp:`={style}` 
  :option:`-ffinite-loops` 
  :option:`-fforward-propagate`  :option:`-ffp-contract`:samp:`={style}`  :option:`-ffunction-sections` 
  :option:`-fgcse`  :option:`-fgcse-after-reload`  :option:`-fgcse-las`  :option:`-fgcse-lm`  :option:`-fgraphite-identity` 
  :option:`-fgcse-sm`  :option:`-fhoist-adjacent-loads`  :option:`-fif-conversion` 
  :option:`-fif-conversion2`  :option:`-findirect-inlining` 
  :option:`-finline-functions`  :option:`-finline-functions-called-once`  :option:`-finline-limit`:samp:`={n}` 
  :option:`-finline-small-functions` :option:`-fipa-modref` :option:`-fipa-cp`  :option:`-fipa-cp-clone` 
  :option:`-fipa-bit-cp`  :option:`-fipa-vrp`  :option:`-fipa-pta`  :option:`-fipa-profile`  :option:`-fipa-pure-const` 
  :option:`-fipa-reference`  :option:`-fipa-reference-addressable` 
  :option:`-fipa-stack-alignment`  :option:`-fipa-icf`  :option:`-fira-algorithm`:samp:`={algorithm}` 
  :option:`-flive-patching`:samp:`={level}` 
  :option:`-fira-region`:samp:`={region}`  :option:`-fira-hoist-pressure` 
  :option:`-fira-loop-pressure`  :option:`-fno-ira-share-save-slots` 
  :option:`-fno-ira-share-spill-slots` 
  :option:`-fisolate-erroneous-paths-dereference`  :option:`-fisolate-erroneous-paths-attribute` 
  :option:`-fivopts`  :option:`-fkeep-inline-functions`  :option:`-fkeep-static-functions` 
  :option:`-fkeep-static-consts`  :option:`-flimit-function-alignment`  :option:`-flive-range-shrinkage` 
  :option:`-floop-block`  :option:`-floop-interchange`  :option:`-floop-strip-mine` 
  :option:`-floop-unroll-and-jam`  :option:`-floop-nest-optimize` 
  :option:`-floop-parallelize-all`  :option:`-flra-remat`  :option:`-flto`  :option:`-flto-compression-level` 
  :option:`-flto-partition`:samp:`={alg}`  :option:`-fmerge-all-constants` 
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
  :option:`-fprofile-use`  :option:`-fprofile-use`:samp:`={path}` :option:`-fprofile-partial-training` 
  :option:`-fprofile-values` :option:`-fprofile-reorder-functions` 
  :option:`-freciprocal-math`  :option:`-free`  :option:`-frename-registers`  :option:`-freorder-blocks` 
  :option:`-freorder-blocks-algorithm`:samp:`={algorithm}` 
  :option:`-freorder-blocks-and-partition`  :option:`-freorder-functions` 
  :option:`-frerun-cse-after-loop`  :option:`-freschedule-modulo-scheduled-loops` 
  :option:`-frounding-math`  :option:`-fsave-optimization-record` 
  :option:`-fsched2-use-superblocks`  :option:`-fsched-pressure` 
  :option:`-fsched-spec-load`  :option:`-fsched-spec-load-dangerous` 
  :option:`-fsched-stalled-insns-dep`:samp:`[={n}]`  :option:`-fsched-stalled-insns`:samp:`[={n}]` 
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
  :option:`-fstdarg-opt`  :option:`-fstore-merging`  :option:`-fstrict-aliasing` 
  :option:`-fthread-jumps`  :option:`-ftracer`  :option:`-ftree-bit-ccp` 
  :option:`-ftree-builtin-call-dce`  :option:`-ftree-ccp`  :option:`-ftree-ch` 
  :option:`-ftree-coalesce-vars`  :option:`-ftree-copy-prop`  :option:`-ftree-dce`  :option:`-ftree-dominator-opts` 
  :option:`-ftree-dse`  :option:`-ftree-forwprop`  :option:`-ftree-fre`  :option:`-fcode-hoisting` 
  :option:`-ftree-loop-if-convert`  :option:`-ftree-loop-im` 
  :option:`-ftree-phiprop`  :option:`-ftree-loop-distribution`  :option:`-ftree-loop-distribute-patterns` 
  :option:`-ftree-loop-ivcanon`  :option:`-ftree-loop-linear`  :option:`-ftree-loop-optimize` 
  :option:`-ftree-loop-vectorize` 
  :option:`-ftree-parallelize-loops`:samp:`={n}`  :option:`-ftree-pre`  :option:`-ftree-partial-pre`  :option:`-ftree-pta` 
  :option:`-ftree-reassoc`  :option:`-ftree-scev-cprop`  :option:`-ftree-sink`  :option:`-ftree-slsr`  :option:`-ftree-sra` 
  :option:`-ftree-switch-conversion`  :option:`-ftree-tail-merge` 
  :option:`-ftree-ter`  :option:`-ftree-vectorize`  :option:`-ftree-vrp`  :option:`-ftrivial-auto-var-init` 
  :option:`-funconstrained-commons` :option:`-funit-at-a-time`  :option:`-funroll-all-loops` 
  :option:`-funroll-loops` :option:`-funsafe-math-optimizations`  :option:`-funswitch-loops` 
  :option:`-fipa-ra`  :option:`-fvariable-expansion-in-unroller`  :option:`-fvect-cost-model`  :option:`-fvpt` 
  :option:`-fweb`  :option:`-fwhole-program`  :option:`-fwpa`  :option:`-fuse-linker-plugin` :option:`-fzero-call-used-regs` 
  :option:`--param` :samp:`{name}={value}`
  :option:`-O`  :option:`-O0`  :option:`-O1`  :option:`-O2`  :option:`-O3`  :option:`-Os`  :option:`-Ofast`  :option:`-Og`

*Program Instrumentation Options*
  See :ref:`instrumentation-options`.

  :option:`-p`  :option:`-pg`  :option:`-fprofile-arcs`  :option:`--coverage`  :option:`-ftest-coverage` 
  :option:`-fprofile-abs-path` 
  :option:`-fprofile-dir`:samp:`={path}`  :option:`-fprofile-generate`  :option:`-fprofile-generate`:samp:`={path}` 
  :option:`-fprofile-info-section`  :option:`-fprofile-info-section`:samp:`={name}` 
  :option:`-fprofile-note`:samp:`={path}` :option:`-fprofile-prefix-path`:samp:`={path}` 
  :option:`-fprofile-update`:samp:`={method}` :option:`-fprofile-filter-files`:samp:`={regex}` 
  :option:`-fprofile-exclude-files`:samp:`={regex}` 
  :option:`-fprofile-reproducible`:samp:`=[multithreaded|parallel-runs|serial` 
  :option:`-fsanitize`:samp:`={style}`  :option:`-fsanitize-recover`  :option:`-fsanitize-recover`:samp:`={style}` 
  :option:`-fasan-shadow-offset`:samp:`={number}`  :option:`-fsanitize-sections`:samp:`={s1}, {s2},...`
  :option:`-fsanitize-undefined-trap-on-error`  :option:`-fbounds-check` 
  :option:`-fcf-protection`:samp:`=[full|branch|return|none|check]`
  :option:`-fharden-compares` :option:`-fharden-conditional-branches` 
  :option:`-fstack-protector`  :option:`-fstack-protector-all`  :option:`-fstack-protector-strong` 
  :option:`-fstack-protector-explicit`  :option:`-fstack-check` 
  :option:`-fstack-limit-register`:samp:`={reg}`  :option:`-fstack-limit-symbol`:samp:`={sym}` 
  :option:`-fno-stack-limit`  :option:`-fsplit-stack` 
  :option:`-fvtable-verify`:samp:`=[std|preinit|none]` 
  :option:`-fvtv-counts`  :option:`-fvtv-debug` 
  :option:`-finstrument-functions` 
  :option:`-finstrument-functions-exclude-function-list`:samp:`={sym}, {sym},...` 
  :option:`-finstrument-functions-exclude-file-list`:samp:`={file}, {file},...`

*Preprocessor Options*
  See :ref:`preprocessor-options`.

  :option:`-A`:samp:`{question}={answer}` 
  :option:`-A-`:samp:`{question}[={answer}]` 
  :option:`-C`  :option:`-CC`  :option:`-D`:samp:`{macro}[={defn}]`
  :option:`-dD`  :option:`-dI`  :option:`-dM`  :option:`-dN`  :option:`-dU` 
  :option:`-fdebug-cpp`  :option:`-fdirectives-only`  :option:`-fdollars-in-identifiers`  
  :option:`-fexec-charset`:samp:`={charset}`  :option:`-fextended-identifiers`  
  :option:`-finput-charset`:samp:`={charset}`  :option:`-flarge-source-files`  
  :option:`-fmacro-prefix-map`:samp:`={old}={new}` :option:`-fmax-include-depth`:samp:`={depth}` 
  :option:`-fno-canonical-system-headers`  :option:`-fpch-deps`  :option:`-fpch-preprocess`  
  :option:`-fpreprocessed`  :option:`-ftabstop`:samp:`={width}`  :option:`-ftrack-macro-expansion`  
  :option:`-fwide-exec-charset`:samp:`={charset}`  :option:`-fworking-directory` 
  :option:`-H`  :option:`-imacros` :samp:`{file}`  :option:`-include` :samp:`{file}` 
  :option:`-M`  :option:`-MD`  :option:`-MF`  :option:`-MG`  :option:`-MM`  :option:`-MMD`  :option:`-MP`  :option:`-MQ`  :option:`-MT` :option:`-Mno-modules` 
  :option:`-no-integrated-cpp`  :option:`-P`  :option:`-pthread`  :option:`-remap` 
  :option:`-traditional`  :option:`-traditional-cpp`  :option:`-trigraphs` 
  :option:`-U`:samp:`{macro}`  :option:`-undef`  
  :option:`-Wp,`:samp:`{option}`  :option:`-Xpreprocessor` :samp:`{option}`

*Assembler Options*
  See :ref:`assembler-options`.

  :option:`-Wa,`:samp:`{option}`  :option:`-Xassembler` :samp:`{option}`

*Linker Options*
  See :ref:`link-options`.

  :samp:`{object-file-name}`  :option:`-fuse-ld`:samp:`={linker}`  :option:`-l`:samp:`{library}` 
  :option:`-nostartfiles`  :option:`-nodefaultlibs`  :option:`-nolibc`  :option:`-nostdlib` 
  :option:`-e` :samp:`{entry}`  :option:`--entry`:samp:`={entry}` 
  :option:`-pie`  :option:`-pthread`  :option:`-r`  :option:`-rdynamic` 
  :option:`-s`  :option:`-static`  :option:`-static-pie`  :option:`-static-libgcc`  :option:`-static-libstdc++` 
  :option:`-static-libasan`  :option:`-static-libtsan`  :option:`-static-liblsan`  :option:`-static-libubsan` 
  :option:`-shared`  :option:`-shared-libgcc`  :option:`-symbolic` 
  :option:`-T` :samp:`{script}`  :option:`-Wl,`:samp:`{option}`  :option:`-Xlinker` :samp:`{option}` 
  :option:`-u` :samp:`{symbol}`  :option:`-z` :samp:`{keyword}`

*Directory Options*
  See :ref:`directory-options`.

  :option:`-B`:samp:`{prefix}`  :option:`-I`:samp:`{dir}`  :option:`-I-` 
  :option:`-idirafter` :samp:`{dir}` 
  :option:`-imacros` :samp:`{file}`  :option:`-imultilib` :samp:`{dir}` 
  :option:`-iplugindir`:samp:`={dir}`  :option:`-iprefix` :samp:`{file}` 
  :option:`-iquote` :samp:`{dir}`  :option:`-isysroot` :samp:`{dir}`  :option:`-isystem` :samp:`{dir}` 
  :option:`-iwithprefix` :samp:`{dir}`  :option:`-iwithprefixbefore` :samp:`{dir}`  
  :option:`-L`:samp:`{dir}`  :option:`-no-canonical-prefixes`  :option:`--no-sysroot-suffix` 
  :option:`-nostdinc`  :option:`-nostdinc++`  :option:`--sysroot`:samp:`={dir}`

*Code Generation Options*
  See :ref:`code-gen-options`.

  :option:`-fcall-saved-`:samp:`{reg}`  :option:`-fcall-used-`:samp:`{reg}` 
  :option:`-ffixed-`:samp:`{reg}`  :option:`-fexceptions` 
  :option:`-fnon-call-exceptions`  :option:`-fdelete-dead-exceptions`  :option:`-funwind-tables` 
  :option:`-fasynchronous-unwind-tables` 
  :option:`-fno-gnu-unique` 
  :option:`-finhibit-size-directive`  :option:`-fcommon`  :option:`-fno-ident` 
  :option:`-fpcc-struct-return`  :option:`-fpic`  :option:`-fPIC`  :option:`-fpie`  :option:`-fPIE`  :option:`-fno-plt` 
  :option:`-fno-jump-tables` :option:`-fno-bit-tests` 
  :option:`-frecord-gcc-switches` 
  :option:`-freg-struct-return`  :option:`-fshort-enums`  :option:`-fshort-wchar` 
  :option:`-fverbose-asm`  :option:`-fpack-struct`:samp:`[={n}]`
  :option:`-fleading-underscore`  :option:`-ftls-model`:samp:`={model}` 
  :option:`-fstack-reuse`:samp:`={reuse_level}` 
  :option:`-ftrampolines`  :option:`-ftrapv`  :option:`-fwrapv` 
  :option:`-fvisibility`:samp:`=[default|internal|hidden|protected]`
  :option:`-fstrict-volatile-bitfields`  :option:`-fsync-libcalls`

*Developer Options*
  See :ref:`developer-options`.

  :option:`-d`:samp:`{letters}`  :option:`-dumpspecs`  :option:`-dumpmachine`  :option:`-dumpversion` 
  :option:`-dumpfullversion`  :option:`-fcallgraph-info`:samp:`[=su,da]`
  :option:`-fchecking`  :option:`-fchecking`:samp:`={n}`
  :option:`-fdbg-cnt-list`   :option:`-fdbg-cnt`:samp:`={counter-value-list}` 
  :option:`-fdisable-ipa-`:samp:`{pass_name}` 
  :option:`-fdisable-rtl-`:samp:`{pass_name}` 
  :option:`-fdisable-rtl-`:samp:`{pass-name}={range-list}` 
  :option:`-fdisable-tree-`:samp:`{pass_name}` 
  :option:`-fdisable-tree-`:samp:`{pass-name}={range-list}` 
  :option:`-fdump-debug`  :option:`-fdump-earlydebug` 
  :option:`-fdump-noaddr`  :option:`-fdump-unnumbered`  :option:`-fdump-unnumbered-links` 
  :option:`-fdump-final-insns`:samp:`[={file}]`
  :option:`-fdump-ipa-all`  :option:`-fdump-ipa-cgraph`  :option:`-fdump-ipa-inline` 
  :option:`-fdump-lang-all` 
  :option:`-fdump-lang-`:samp:`{switch}` 
  :option:`-fdump-lang-`:samp:`{switch}-{options}` 
  :option:`-fdump-lang-`:samp:`{switch}-{options}={filename}` 
  :option:`-fdump-passes` 
  :option:`-fdump-rtl-`:samp:`{pass}`  :option:`-fdump-rtl-`:samp:`{pass}={filename}` 
  :option:`-fdump-statistics` 
  :option:`-fdump-tree-all` 
  :option:`-fdump-tree-`:samp:`{switch}` 
  :option:`-fdump-tree-`:samp:`{switch}-{options}` 
  :option:`-fdump-tree-`:samp:`{switch}-{options}={filename}` 
  :option:`-fcompare-debug`:samp:`[={opts}]`  :option:`-fcompare-debug-second` 
  :option:`-fenable-`:samp:`{kind}-{pass}` 
  :option:`-fenable-`:samp:`{kind}-{pass}={range-list}` 
  :option:`-fira-verbose`:samp:`={n}` 
  :option:`-flto-report`  :option:`-flto-report-wpa`  :option:`-fmem-report-wpa` 
  :option:`-fmem-report`  :option:`-fpre-ipa-mem-report`  :option:`-fpost-ipa-mem-report` 
  :option:`-fopt-info`  :option:`-fopt-info-`:samp:`{options}:[={file}]`
  :option:`-fprofile-report` 
  :option:`-frandom-seed`:samp:`={string}`  :option:`-fsched-verbose`:samp:`={n}` 
  :option:`-fsel-sched-verbose`  :option:`-fsel-sched-dump-cfg`  :option:`-fsel-sched-pipelining-verbose` 
  :option:`-fstats`  :option:`-fstack-usage`  :option:`-ftime-report`  :option:`-ftime-report-details` 
  :option:`-fvar-tracking-assignments-toggle`  :option:`-gtoggle` 
  :option:`-print-file-name`:samp:`={library}`  :option:`-print-libgcc-file-name` 
  :option:`-print-multi-directory`  :option:`-print-multi-lib`  :option:`-print-multi-os-directory` 
  :option:`-print-prog-name`:samp:`={program}`  :option:`-print-search-dirs`  :option:`-Q` 
  :option:`-print-sysroot`  :option:`-print-sysroot-headers-suffix` 
  :option:`-save-temps`  :option:`-save-temps`:samp:`=cwd`  :option:`-save-temps`:samp:`=obj`  :option:`-time`:samp:`[={file}]`

*Machine-Dependent Options*
  See :ref:`submodel-options`.

  .. This list is ordered alphanumerically by subsection name.

  .. Try and put the significant identifier (CPU or system) first,

  .. so users have a clue at guessing where the ones they want will be.

  *AArch64 Options*

  :option:`AArch64 -mabi`:samp:`={name}`  :option:`AArch64 -mbig-endian`  :option:`AArch64 -mlittle-endian` 
  :option:`AArch64 -mgeneral-regs-only` 
  :option:`AArch64 -mcmodel`:samp:`=tiny`  :option:`AArch64 -mcmodel`:samp:`=small`  :option:`AArch64 -mcmodel`:samp:`=large` 
  :option:`AArch64 -mstrict-align`  :option:`AArch64 -mno-strict-align` 
  :option:`AArch64 -momit-leaf-frame-pointer` 
  :option:`AArch64 -mtls-dialect`:samp:`=desc`  :option:`AArch64 -mtls-dialect`:samp:`=traditional` 
  :option:`AArch64 -mtls-size`:samp:`={size}` 
  :option:`AArch64 -mfix-cortex-a53-835769`  :option:`AArch64 -mfix-cortex-a53-843419` 
  :option:`AArch64 -mlow-precision-recip-sqrt`  :option:`AArch64 -mlow-precision-sqrt`  :option:`AArch64 -mlow-precision-div` 
  :option:`AArch64 -mpc-relative-literal-loads` 
  :option:`AArch64 -msign-return-address`:samp:`={scope}` 
  :option:`AArch64 -mbranch-protection`:samp:`={none}|{standard}|{pac-ret}[+{leaf}+{b-key}|{bti}`
  :option:`AArch64 -mharden-sls`:samp:`={opts}` 
  :option:`AArch64 -march`:samp:`={name}`  :option:`AArch64 -mcpu`:samp:`={name}`  :option:`AArch64 -mtune`:samp:`={name}`  
  :option:`AArch64 -moverride`:samp:`={string}`  :option:`AArch64 -mverbose-cost-dump` 
  :option:`AArch64 -mstack-protector-guard`:samp:`={guard}` :option:`AArch64 -mstack-protector-guard-reg`:samp:`={sysreg}` 
  :option:`AArch64 -mstack-protector-guard-offset`:samp:`={offset}` :option:`AArch64 -mtrack-speculation` 
  :option:`AArch64 -moutline-atomics` 

  *Adapteva Epiphany Options*

  :option:`Adapteva Epiphany -mhalf-reg-file`  :option:`Adapteva Epiphany -mprefer-short-insn-regs` 
  :option:`Adapteva Epiphany -mbranch-cost`:samp:`={num}`  :option:`Adapteva Epiphany -mcmove`  :option:`Adapteva Epiphany -mnops`:samp:`={num}`  :option:`Adapteva Epiphany -msoft-cmpsf` 
  :option:`Adapteva Epiphany -msplit-lohi`  :option:`Adapteva Epiphany -mpost-inc`  :option:`Adapteva Epiphany -mpost-modify`  :option:`Adapteva Epiphany -mstack-offset`:samp:`={num}` 
  :option:`Adapteva Epiphany -mround-nearest`  :option:`Adapteva Epiphany -mlong-calls`  :option:`Adapteva Epiphany -mshort-calls`  :option:`Adapteva Epiphany -msmall16` 
  :option:`Adapteva Epiphany -mfp-mode`:samp:`={mode}`  :option:`Adapteva Epiphany -mvect-double`  :option:`Adapteva Epiphany -max-vect-align`:samp:`={num}` 
  :option:`Adapteva Epiphany -msplit-vecmove-early`  :option:`Adapteva Epiphany -m1reg-`:samp:`{reg}`

  *AMD GCN Options*

  :option:`AMD GCN -march`:samp:`={gpu}` :option:`AMD GCN -mtune`:samp:`={gpu}` :option:`AMD GCN -mstack-size`:samp:`={bytes}`

  *ARC Options*

  :option:`ARC -mbarrel-shifter`  :option:`ARC -mjli-always` 
  :option:`ARC -mcpu`:samp:`={cpu}`  :option:`ARC -mA6`  :option:`ARC -mARC600`  :option:`ARC -mA7`  :option:`ARC -mARC700` 
  :option:`ARC -mdpfp`  :option:`ARC -mdpfp-compact`  :option:`ARC -mdpfp-fast`  :option:`ARC -mno-dpfp-lrsr` 
  :option:`ARC -mea`  :option:`ARC -mno-mpy`  :option:`ARC -mmul32x16`  :option:`ARC -mmul64`  :option:`ARC -matomic` 
  :option:`ARC -mnorm`  :option:`ARC -mspfp`  :option:`ARC -mspfp-compact`  :option:`ARC -mspfp-fast`  :option:`ARC -msimd`  :option:`ARC -msoft-float`  :option:`ARC -mswap` 
  :option:`ARC -mcrc`  :option:`ARC -mdsp-packa`  :option:`ARC -mdvbf`  :option:`ARC -mlock`  :option:`ARC -mmac-d16`  :option:`ARC -mmac-24`  :option:`ARC -mrtsc`  :option:`ARC -mswape` 
  :option:`ARC -mtelephony`  :option:`ARC -mxy`  :option:`ARC -misize`  :option:`ARC -mannotate-align`  :option:`ARC -marclinux`  :option:`ARC -marclinux_prof` 
  :option:`ARC -mlong-calls`  :option:`ARC -mmedium-calls`  :option:`ARC -msdata`  :option:`ARC -mirq-ctrl-saved` 
  :option:`ARC -mrgf-banked-regs`  :option:`ARC -mlpc-width`:samp:`={width}`  :option:`ARC -G` :samp:`{num}` 
  :option:`ARC -mvolatile-cache`  :option:`ARC -mtp-regno`:samp:`={regno}` 
  :option:`ARC -malign-call`  :option:`ARC -mauto-modify-reg`  :option:`ARC -mbbit-peephole`  :option:`ARC -mno-brcc` 
  :option:`ARC -mcase-vector-pcrel`  :option:`ARC -mcompact-casesi`  :option:`ARC -mno-cond-exec`  :option:`ARC -mearly-cbranchsi` 
  :option:`ARC -mexpand-adddi`  :option:`ARC -mindexed-loads`  :option:`ARC -mlra`  :option:`ARC -mlra-priority-none` 
  :option:`ARC -mlra-priority-compact` mlra :option:`ARC -priority-noncompact`  :option:`ARC -mmillicode` 
  :option:`ARC -mmixed-code`  :option:`ARC -mq-class`  :option:`ARC -mRcq`  :option:`ARC -mRcw`  :option:`ARC -msize-level`:samp:`={level}` 
  :option:`ARC -mtune`:samp:`={cpu}`  :option:`ARC -mmultcost`:samp:`={num}`  :option:`ARC -mcode-density-frame` 
  :option:`ARC -munalign-prob-threshold`:samp:`={probability}`  :option:`ARC -mmpy-option`:samp:`={multo}` 
  :option:`ARC -mdiv-rem`  :option:`ARC -mcode-density`  :option:`ARC -mll64`  :option:`ARC -mfpu`:samp:`={fpu}`  :option:`ARC -mrf16`  :option:`ARC -mbranch-index`

  *ARM Options*

  :option:`ARM -mapcs-frame`  :option:`ARM -mno-apcs-frame` 
  :option:`ARM -mabi`:samp:`={name}` 
  :option:`ARM -mapcs-stack-check`  :option:`ARM -mno-apcs-stack-check` 
  :option:`ARM -mapcs-reentrant`  :option:`ARM -mno-apcs-reentrant` 
  :option:`ARM -mgeneral-regs-only` 
  :option:`ARM -msched-prolog`  :option:`ARM -mno-sched-prolog` 
  :option:`ARM -mlittle-endian`  :option:`ARM -mbig-endian` 
  :option:`ARM -mbe8`  :option:`ARM -mbe32` 
  :option:`ARM -mfloat-abi`:samp:`={name}` 
  :option:`ARM -mfp16-format`:samp:`={name}`
  :option:`ARM -mthumb-interwork`  :option:`ARM -mno-thumb-interwork` 
  :option:`ARM -mcpu`:samp:`={name}`  :option:`ARM -march`:samp:`={name}`  :option:`ARM -mfpu`:samp:`={name}`  
  :option:`ARM -mtune`:samp:`={name}`  :option:`ARM -mprint-tune-info` 
  :option:`ARM -mstructure-size-boundary`:samp:`={n}` 
  :option:`ARM -mabort-on-noreturn` 
  :option:`ARM -mlong-calls`  :option:`ARM -mno-long-calls` 
  :option:`ARM -msingle-pic-base`  :option:`ARM -mno-single-pic-base` 
  :option:`ARM -mpic-register`:samp:`={reg}` 
  :option:`ARM -mnop-fun-dllimport` 
  :option:`ARM -mpoke-function-name` 
  :option:`ARM -mthumb`  :option:`ARM -marm`  :option:`ARM -mflip-thumb` 
  :option:`ARM -mtpcs-frame`  :option:`ARM -mtpcs-leaf-frame` 
  :option:`ARM -mcaller-super-interworking`  :option:`ARM -mcallee-super-interworking` 
  :option:`ARM -mtp`:samp:`={name}`  :option:`ARM -mtls-dialect`:samp:`={dialect}` 
  :option:`ARM -mword-relocations` 
  :option:`ARM -mfix-cortex-m3-ldrd` 
  :option:`ARM -munaligned-access` 
  :option:`ARM -mneon-for-64bits` 
  :option:`ARM -mslow-flash-data` 
  :option:`ARM -masm-syntax-unified` 
  :option:`ARM -mrestrict-it` 
  :option:`ARM -mverbose-cost-dump` 
  :option:`ARM -mpure-code` 
  :option:`ARM -mcmse` 
  :option:`ARM -mfix-cmse-cve-2021-35465` 
  :option:`ARM -mfdpic`

  *AVR Options*

  :option:`AVR -mmcu`:samp:`={mcu}`  :option:`AVR -mabsdata`  :option:`AVR -maccumulate-args` 
  :option:`AVR -mbranch-cost`:samp:`={cost}` 
  :option:`AVR -mcall-prologues`  :option:`AVR -mgas-isr-prologues`  :option:`AVR -mint8` 
  :option:`AVR -mdouble`:samp:`={bits}` :option:`AVR -mlong-double`:samp:`={bits}` 
  :option:`AVR -mn_flash`:samp:`={size}`  :option:`AVR -mno-interrupts` 
  :option:`AVR -mmain-is-OS_task`  :option:`AVR -mrelax`  :option:`AVR -mrmw`  :option:`AVR -mstrict-X`  :option:`AVR -mtiny-stack` 
  :option:`AVR -mfract-convert-truncate` 
  :option:`AVR -mshort-calls`  :option:`AVR -nodevicelib`  :option:`AVR -nodevicespecs` 
  :option:`AVR -Waddr-space-convert`  :option:`AVR -Wmisspelled-isr`

  *Blackfin Options*

  :option:`Blackfin -mcpu`:samp:`={cpu}[-{sirevision}]`
  :option:`Blackfin -msim`  :option:`Blackfin -momit-leaf-frame-pointer`  :option:`Blackfin -mno-omit-leaf-frame-pointer` 
  :option:`Blackfin -mspecld-anomaly`  :option:`Blackfin -mno-specld-anomaly`  :option:`Blackfin -mcsync-anomaly`  :option:`Blackfin -mno-csync-anomaly` 
  :option:`Blackfin -mlow-64k`  :option:`Blackfin -mno-low64k`  :option:`Blackfin -mstack-check-l1`  :option:`Blackfin -mid-shared-library` 
  :option:`Blackfin -mno-id-shared-library`  :option:`Blackfin -mshared-library-id`:samp:`={n}` 
  :option:`Blackfin -mleaf-id-shared-library`  :option:`Blackfin -mno-leaf-id-shared-library` 
  :option:`Blackfin -msep-data`  :option:`Blackfin -mno-sep-data`  :option:`Blackfin -mlong-calls`  :option:`Blackfin -mno-long-calls` 
  :option:`Blackfin -mfast-fp`  :option:`Blackfin -minline-plt`  :option:`Blackfin -mmulticore`  :option:`Blackfin -mcorea`  :option:`Blackfin -mcoreb`  :option:`Blackfin -msdram` 
  :option:`Blackfin -micplb`

  *C6X Options*

  :option:`C6X -mbig-endian`  :option:`C6X -mlittle-endian`  :option:`C6X -march`:samp:`={cpu}` 
  :option:`C6X -msim`  :option:`C6X -msdata`:samp:`={sdata-type}`

  *CRIS Options*

  :option:`CRIS -mcpu`:samp:`={cpu}`  :option:`CRIS -march`:samp:`={cpu}`  :option:`CRIS -mtune`:samp:`={cpu}` 
  :option:`CRIS -mmax-stack-frame`:samp:`={n}`  :option:`CRIS -melinux-stacksize`:samp:`={n}` 
  :option:`CRIS -metrax4`  :option:`CRIS -metrax100`  :option:`CRIS -mpdebug`  :option:`CRIS -mcc-init`  :option:`CRIS -mno-side-effects` 
  :option:`CRIS -mstack-align`  :option:`CRIS -mdata-align`  :option:`CRIS -mconst-align` 
  :option:`CRIS -m32-bit`  :option:`CRIS -m16-bit`  :option:`CRIS -m8-bit`  :option:`CRIS -mno-prologue-epilogue`  :option:`CRIS -mno-gotplt` 
  :option:`CRIS -melf`  :option:`CRIS -maout`  :option:`CRIS -melinux`  :option:`CRIS -mlinux`  :option:`CRIS -sim`  :option:`CRIS -sim2` 
  :option:`CRIS -mmul-bug-workaround`  :option:`CRIS -mno-mul-bug-workaround`

  *CR16 Options*

  :option:`CR16 -mmac` 
  :option:`CR16 -mcr16cplus`  :option:`CR16 -mcr16c` 
  :option:`CR16 -msim`  :option:`CR16 -mint32`  :option:`CR16 -mbit-ops`
  :option:`CR16 -mdata-model`:samp:`={model}`

  *C-SKY Options*

  :option:`C-SKY -march`:samp:`={arch}`  :option:`C-SKY -mcpu`:samp:`={cpu}` 
  :option:`C-SKY -mbig-endian`  :option:`C-SKY -EB`  :option:`C-SKY -mlittle-endian`  :option:`C-SKY -EL` 
  :option:`C-SKY -mhard-float`  :option:`C-SKY -msoft-float`  :option:`C-SKY -mfpu`:samp:`={fpu}`  :option:`C-SKY -mdouble-float`  :option:`C-SKY -mfdivdu` 
  :option:`C-SKY -mfloat-abi`:samp:`={name}` 
  :option:`C-SKY -melrw`  :option:`C-SKY -mistack`  :option:`C-SKY -mmp`  :option:`C-SKY -mcp`  :option:`C-SKY -mcache`  :option:`C-SKY -msecurity`  :option:`C-SKY -mtrust` 
  :option:`C-SKY -mdsp`  :option:`C-SKY -medsp`  :option:`C-SKY -mvdsp` 
  :option:`C-SKY -mdiv`  :option:`C-SKY -msmart`  :option:`C-SKY -mhigh-registers`  :option:`C-SKY -manchor` 
  :option:`C-SKY -mpushpop`  :option:`C-SKY -mmultiple-stld`  :option:`C-SKY -mconstpool`  :option:`C-SKY -mstack-size`  :option:`C-SKY -mccrt` 
  :option:`C-SKY -mbranch-cost`:samp:`={n}`  :option:`C-SKY -mcse-cc`  :option:`C-SKY -msched-prolog` :option:`C-SKY -msim`

  *Darwin Options*

  :option:`Darwin -all_load`  :option:`Darwin -allowable_client`  :option:`Darwin -arch`  :option:`Darwin -arch_errors_fatal` 
  :option:`Darwin -arch_only`  :option:`Darwin -bind_at_load`  :option:`Darwin -bundle`  :option:`Darwin -bundle_loader` 
  :option:`Darwin -client_name`  :option:`Darwin -compatibility_version`  :option:`Darwin -current_version` 
  :option:`Darwin -dead_strip` 
  :option:`Darwin -dependency-file`  :option:`Darwin -dylib_file`  :option:`Darwin -dylinker_install_name` 
  :option:`Darwin -dynamic`  :option:`Darwin -dynamiclib`  :option:`Darwin -exported_symbols_list` 
  :option:`Darwin -filelist`  :option:`Darwin -flat_namespace`  :option:`Darwin -force_cpusubtype_ALL` 
  :option:`Darwin -force_flat_namespace`  :option:`Darwin -headerpad_max_install_names` 
  :option:`Darwin -iframework` 
  :option:`Darwin -image_base`  :option:`Darwin -init`  :option:`Darwin -install_name`  :option:`Darwin -keep_private_externs` 
  :option:`Darwin -multi_module`  :option:`Darwin -multiply_defined`  :option:`Darwin -multiply_defined_unused` 
  :option:`Darwin -noall_load`   :option:`Darwin -no_dead_strip_inits_and_terms` 
  :option:`Darwin -nofixprebinding`  :option:`Darwin -nomultidefs`  :option:`Darwin -noprebind`  :option:`Darwin -noseglinkedit` 
  :option:`Darwin -pagezero_size`  :option:`Darwin -prebind`  :option:`Darwin -prebind_all_twolevel_modules` 
  :option:`Darwin -private_bundle`  :option:`Darwin -read_only_relocs`  :option:`Darwin -sectalign` 
  :option:`Darwin -sectobjectsymbols`  :option:`Darwin -whyload`  :option:`Darwin -seg1addr` 
  :option:`Darwin -sectcreate`  :option:`Darwin -sectobjectsymbols`  :option:`Darwin -sectorder` 
  :option:`Darwin -segaddr`  :option:`Darwin -segs_read_only_addr`  :option:`Darwin -segs_read_write_addr` 
  :option:`Darwin -seg_addr_table`  :option:`Darwin -seg_addr_table_filename`  :option:`Darwin -seglinkedit` 
  :option:`Darwin -segprot`  :option:`Darwin -segs_read_only_addr`  :option:`Darwin -segs_read_write_addr` 
  :option:`Darwin -single_module`  :option:`Darwin -static`  :option:`Darwin -sub_library`  :option:`Darwin -sub_umbrella` 
  :option:`Darwin -twolevel_namespace`  :option:`Darwin -umbrella`  :option:`Darwin -undefined` 
  :option:`Darwin -unexported_symbols_list`  :option:`Darwin -weak_reference_mismatches` 
  :option:`Darwin -whatsloaded`  :option:`Darwin -F`  :option:`Darwin -gused`  :option:`Darwin -gfull`  :option:`Darwin -mmacosx-version-min`:samp:`={version}` 
  :option:`Darwin -mkernel`  :option:`Darwin -mone-byte-bool`

  *DEC Alpha Options*

  :option:`DEC Alpha -mno-fp-regs`  :option:`DEC Alpha -msoft-float` 
  :option:`DEC Alpha -mieee`  :option:`DEC Alpha -mieee-with-inexact`  :option:`DEC Alpha -mieee-conformant` 
  :option:`DEC Alpha -mfp-trap-mode`:samp:`={mode}`  :option:`DEC Alpha -mfp-rounding-mode`:samp:`={mode}` 
  :option:`DEC Alpha -mtrap-precision`:samp:`={mode}`  :option:`DEC Alpha -mbuild-constants` 
  :option:`DEC Alpha -mcpu`:samp:`={cpu-type}`  :option:`DEC Alpha -mtune`:samp:`={cpu-type}` 
  :option:`DEC Alpha -mbwx`  :option:`DEC Alpha -mmax`  :option:`DEC Alpha -mfix`  :option:`DEC Alpha -mcix` 
  :option:`DEC Alpha -mfloat-vax`  :option:`DEC Alpha -mfloat-ieee` 
  :option:`DEC Alpha -mexplicit-relocs`  :option:`DEC Alpha -msmall-data`  :option:`DEC Alpha -mlarge-data` 
  :option:`DEC Alpha -msmall-text`  :option:`DEC Alpha -mlarge-text` 
  :option:`DEC Alpha -mmemory-latency`:samp:`={time}`

  *eBPF Options*

  :option:`eBPF -mbig-endian` :option:`eBPF -mlittle-endian` :option:`eBPF -mkernel`:samp:`={version}`
  :option:`eBPF -mframe-limit`:samp:`={bytes}` :option:`eBPF -mxbpf` :option:`eBPF -mco-re` :option:`eBPF -mno-co-re`
  :option:`eBPF -mjmpext` :option:`eBPF -mjmp32` :option:`eBPF -malu32` :option:`eBPF -mcpu`:samp:`={version}`

  *FR30 Options*

  :option:`FR30 -msmall-model`  :option:`FR30 -mno-lsim`

  *FT32 Options*

  :option:`FT32 -msim`  :option:`FT32 -mlra`  :option:`FT32 -mnodiv`  :option:`FT32 -mft32b`  :option:`FT32 -mcompress`  :option:`FT32 -mnopm`

  *FRV Options*

  :option:`FRV -mgpr-32`  :option:`FRV -mgpr-64`  :option:`FRV -mfpr-32`  :option:`FRV -mfpr-64` 
  :option:`FRV -mhard-float`  :option:`FRV -msoft-float` 
  :option:`FRV -malloc-cc`  :option:`FRV -mfixed-cc`  :option:`FRV -mdword`  :option:`FRV -mno-dword` 
  :option:`FRV -mdouble`  :option:`FRV -mno-double` 
  :option:`FRV -mmedia`  :option:`FRV -mno-media`  :option:`FRV -mmuladd`  :option:`FRV -mno-muladd` 
  :option:`FRV -mfdpic`  :option:`FRV -minline-plt`  :option:`FRV -mgprel-ro`  :option:`FRV -multilib-library-pic` 
  :option:`FRV -mlinked-fp`  :option:`FRV -mlong-calls`  :option:`FRV -malign-labels` 
  :option:`FRV -mlibrary-pic`  :option:`FRV -macc-4`  :option:`FRV -macc-8` 
  :option:`FRV -mpack`  :option:`FRV -mno-pack`  :option:`FRV -mno-eflags`  :option:`FRV -mcond-move`  :option:`FRV -mno-cond-move` 
  :option:`FRV -moptimize-membar`  :option:`FRV -mno-optimize-membar` 
  :option:`FRV -mscc`  :option:`FRV -mno-scc`  :option:`FRV -mcond-exec`  :option:`FRV -mno-cond-exec` 
  :option:`FRV -mvliw-branch`  :option:`FRV -mno-vliw-branch` 
  :option:`FRV -mmulti-cond-exec`  :option:`FRV -mno-multi-cond-exec`  :option:`FRV -mnested-cond-exec` 
  :option:`FRV -mno-nested-cond-exec`  :option:`FRV -mtomcat-stats` 
  :option:`FRV -mTLS`  :option:`FRV -mtls` 
  :option:`FRV -mcpu`:samp:`={cpu}`

  *GNU/Linux Options*

  :option:`GNU/Linux -mglibc`  :option:`GNU/Linux -muclibc`  :option:`GNU/Linux -mmusl`  :option:`GNU/Linux -mbionic`  :option:`GNU/Linux -mandroid` 
  :option:`GNU/Linux -tno-android-cc`  :option:`GNU/Linux -tno-android-ld`

  *H8/300 Options*

  :option:`H8/300 -mrelax`  :option:`H8/300 -mh`  :option:`H8/300 -ms`  :option:`H8/300 -mn`  :option:`H8/300 -mexr`  :option:`H8/300 -mno-exr`  :option:`H8/300 -mint32`  :option:`H8/300 -malign-300`

  *HPPA Options*

  :option:`HPPA -march`:samp:`={architecture-type}` 
  :option:`HPPA -mcaller-copies`  :option:`HPPA -mdisable-fpregs`  :option:`HPPA -mdisable-indexing` 
  :option:`HPPA -mfast-indirect-calls`  :option:`HPPA -mgas`  :option:`HPPA -mgnu-ld`   :option:`HPPA -mhp-ld` 
  :option:`HPPA -mfixed-range`:samp:`={register-range}` 
  :option:`HPPA -mjump-in-delay`  :option:`HPPA -mlinker-opt`  :option:`HPPA -mlong-calls` 
  :option:`HPPA -mlong-load-store`  :option:`HPPA -mno-disable-fpregs` 
  :option:`HPPA -mno-disable-indexing`  :option:`HPPA -mno-fast-indirect-calls`  :option:`HPPA -mno-gas` 
  :option:`HPPA -mno-jump-in-delay`  :option:`HPPA -mno-long-load-store` 
  :option:`HPPA -mno-portable-runtime`  :option:`HPPA -mno-soft-float` 
  :option:`HPPA -mno-space-regs`  :option:`HPPA -msoft-float`  :option:`HPPA -mpa-risc-1-0` 
  :option:`HPPA -mpa-risc-1-1`  :option:`HPPA -mpa-risc-2-0`  :option:`HPPA -mportable-runtime` 
  :option:`HPPA -mschedule`:samp:`={cpu-type}`  :option:`HPPA -mspace-regs`  :option:`HPPA -msio`  :option:`HPPA -mwsio` 
  :option:`HPPA -munix`:samp:`={unix-std}`  :option:`HPPA -nolibdld`  :option:`HPPA -static`  :option:`HPPA -threads`

  *IA-64 Options*

  :option:`IA-64 -mbig-endian`  :option:`IA-64 -mlittle-endian`  :option:`IA-64 -mgnu-as`  :option:`IA-64 -mgnu-ld`  :option:`IA-64 -mno-pic` 
  :option:`IA-64 -mvolatile-asm-stop`  :option:`IA-64 -mregister-names`  :option:`IA-64 -msdata`  :option:`IA-64 -mno-sdata` 
  :option:`IA-64 -mconstant-gp`  :option:`IA-64 -mauto-pic`  :option:`IA-64 -mfused-madd` 
  :option:`IA-64 -minline-float-divide-min-latency` 
  :option:`IA-64 -minline-float-divide-max-throughput` 
  :option:`IA-64 -mno-inline-float-divide` 
  :option:`IA-64 -minline-int-divide-min-latency` 
  :option:`IA-64 -minline-int-divide-max-throughput`  
  :option:`IA-64 -mno-inline-int-divide` 
  :option:`IA-64 -minline-sqrt-min-latency`  :option:`IA-64 -minline-sqrt-max-throughput` 
  :option:`IA-64 -mno-inline-sqrt` 
  :option:`IA-64 -mdwarf2-asm`  :option:`IA-64 -mearly-stop-bits` 
  :option:`IA-64 -mfixed-range`:samp:`={register-range}`  :option:`IA-64 -mtls-size`:samp:`={tls-size}` 
  :option:`IA-64 -mtune`:samp:`={cpu-type}`  :option:`IA-64 -milp32`  :option:`IA-64 -mlp64` 
  :option:`IA-64 -msched-br-data-spec`  :option:`IA-64 -msched-ar-data-spec`  :option:`IA-64 -msched-control-spec` 
  :option:`IA-64 -msched-br-in-data-spec`  :option:`IA-64 -msched-ar-in-data-spec`  :option:`IA-64 -msched-in-control-spec` 
  :option:`IA-64 -msched-spec-ldc`  :option:`IA-64 -msched-spec-control-ldc` 
  :option:`IA-64 -msched-prefer-non-data-spec-insns`  :option:`IA-64 -msched-prefer-non-control-spec-insns` 
  :option:`IA-64 -msched-stop-bits-after-every-cycle`  :option:`IA-64 -msched-count-spec-in-critical-path` 
  :option:`IA-64 -msel-sched-dont-check-control-spec`  :option:`IA-64 -msched-fp-mem-deps-zero-cost` 
  :option:`IA-64 -msched-max-memory-insns-hard-limit`  :option:`IA-64 -msched-max-memory-insns`:samp:`={max-insns}`

  *LM32 Options*

  :option:`LM32 -mbarrel-shift-enabled`  :option:`LM32 -mdivide-enabled`  :option:`LM32 -mmultiply-enabled` 
  :option:`LM32 -msign-extend-enabled`  :option:`LM32 -muser-enabled`

  *M32R/D Options*

  :option:`M32R/D -m32r2`  :option:`M32R/D -m32rx`  :option:`M32R/D -m32r` 
  :option:`M32R/D -mdebug` 
  :option:`M32R/D -malign-loops`  :option:`M32R/D -mno-align-loops` 
  :option:`M32R/D -missue-rate`:samp:`={number}` 
  :option:`M32R/D -mbranch-cost`:samp:`={number}` 
  :option:`M32R/D -mmodel`:samp:`={code-size-model-type}` 
  :option:`M32R/D -msdata`:samp:`={sdata-type}` 
  :option:`M32R/D -mno-flush-func`  :option:`M32R/D -mflush-func`:samp:`={name}` 
  :option:`M32R/D -mno-flush-trap`  :option:`M32R/D -mflush-trap`:samp:`={number}` 
  :option:`M32R/D -G` :samp:`{num}`

  *M32C Options*

  :option:`M32C -mcpu`:samp:`={cpu}`  :option:`M32C -msim`  :option:`M32C -memregs`:samp:`={number}`

  *M680x0 Options*

  :option:`M680x0 -march`:samp:`={arch}`  :option:`M680x0 -mcpu`:samp:`={cpu}`  :option:`M680x0 -mtune`:samp:`={tune}` 
  :option:`M680x0 -m68000`  :option:`M680x0 -m68020`  :option:`M680x0 -m68020-40`  :option:`M680x0 -m68020-60`  :option:`M680x0 -m68030`  :option:`M680x0 -m68040` 
  :option:`M680x0 -m68060`  :option:`M680x0 -mcpu32`  :option:`M680x0 -m5200`  :option:`M680x0 -m5206e`  :option:`M680x0 -m528x`  :option:`M680x0 -m5307`  :option:`M680x0 -m5407` 
  :option:`M680x0 -mcfv4e`  :option:`M680x0 -mbitfield`  :option:`M680x0 -mno-bitfield`  :option:`M680x0 -mc68000`  :option:`M680x0 -mc68020` 
  :option:`M680x0 -mnobitfield`  :option:`M680x0 -mrtd`  :option:`M680x0 -mno-rtd`  :option:`M680x0 -mdiv`  :option:`M680x0 -mno-div`  :option:`M680x0 -mshort` 
  :option:`M680x0 -mno-short`  :option:`M680x0 -mhard-float`  :option:`M680x0 -m68881`  :option:`M680x0 -msoft-float`  :option:`M680x0 -mpcrel` 
  :option:`M680x0 -malign-int`  :option:`M680x0 -mstrict-align`  :option:`M680x0 -msep-data`  :option:`M680x0 -mno-sep-data` 
  :option:`M680x0 -mshared-library-id`:samp:`=n`  :option:`M680x0 -mid-shared-library`  :option:`M680x0 -mno-id-shared-library` 
  :option:`M680x0 -mxgot`  :option:`M680x0 -mno-xgot`  :option:`M680x0 -mlong-jump-table-offsets`

  *MCore Options*

  :option:`MCore -mhardlit`  :option:`MCore -mno-hardlit`  :option:`MCore -mdiv`  :option:`MCore -mno-div`  :option:`MCore -mrelax-immediates` 
  :option:`MCore -mno-relax-immediates`  :option:`MCore -mwide-bitfields`  :option:`MCore -mno-wide-bitfields` 
  :option:`MCore -m4byte-functions`  :option:`MCore -mno-4byte-functions`  :option:`MCore -mcallgraph-data` 
  :option:`MCore -mno-callgraph-data`  :option:`MCore -mslow-bytes`  :option:`MCore -mno-slow-bytes`  :option:`MCore -mno-lsim` 
  :option:`MCore -mlittle-endian`  :option:`MCore -mbig-endian`  :option:`MCore -m210`  :option:`MCore -m340`  :option:`MCore -mstack-increment`

  *MeP Options*

  :option:`MeP -mabsdiff`  :option:`MeP -mall-opts`  :option:`MeP -maverage`  :option:`MeP -mbased`:samp:`={n}`  :option:`MeP -mbitops` 
  :option:`MeP -mc`:samp:`={n}`  :option:`MeP -mclip`  :option:`MeP -mconfig`:samp:`={name}`  :option:`MeP -mcop`  :option:`MeP -mcop32`  :option:`MeP -mcop64`  :option:`MeP -mivc2` 
  :option:`MeP -mdc`  :option:`MeP -mdiv`  :option:`MeP -meb`  :option:`MeP -mel`  :option:`MeP -mio-volatile`  :option:`MeP -ml`  :option:`MeP -mleadz`  :option:`MeP -mm`  :option:`MeP -mminmax` 
  :option:`MeP -mmult`  :option:`MeP -mno-opts`  :option:`MeP -mrepeat`  :option:`MeP -ms`  :option:`MeP -msatur`  :option:`MeP -msdram`  :option:`MeP -msim`  :option:`MeP -msimnovec`  :option:`MeP -mtf` 
  :option:`MeP -mtiny`:samp:`={n}`

  *MicroBlaze Options*

  :option:`MicroBlaze -msoft-float`  :option:`MicroBlaze -mhard-float`  :option:`MicroBlaze -msmall-divides`  :option:`MicroBlaze -mcpu`:samp:`={cpu}` 
  :option:`MicroBlaze -mmemcpy`  :option:`MicroBlaze -mxl-soft-mul`  :option:`MicroBlaze -mxl-soft-div`  :option:`MicroBlaze -mxl-barrel-shift` 
  :option:`MicroBlaze -mxl-pattern-compare`  :option:`MicroBlaze -mxl-stack-check`  :option:`MicroBlaze -mxl-gp-opt`  :option:`MicroBlaze -mno-clearbss` 
  :option:`MicroBlaze -mxl-multiply-high`  :option:`MicroBlaze -mxl-float-convert`  :option:`MicroBlaze -mxl-float-sqrt` 
  :option:`MicroBlaze -mbig-endian`  :option:`MicroBlaze -mlittle-endian`  :option:`MicroBlaze -mxl-reorder`  :option:`MicroBlaze -mxl-mode-`:samp:`{app-model}` 
  :option:`MicroBlaze -mpic-data-is-text-relative`

  *MIPS Options*

  :option:`MIPS -EL`  :option:`MIPS -EB`  :option:`MIPS -march`:samp:`={arch}`  :option:`MIPS -mtune`:samp:`={arch}` 
  :option:`MIPS -mips1`  :option:`MIPS -mips2`  :option:`MIPS -mips3`  :option:`MIPS -mips4`  :option:`MIPS -mips32`  :option:`MIPS -mips32r2`  :option:`MIPS -mips32r3`  :option:`MIPS -mips32r5` 
  :option:`MIPS -mips32r6`  :option:`MIPS -mips64`  :option:`MIPS -mips64r2`  :option:`MIPS -mips64r3`  :option:`MIPS -mips64r5`  :option:`MIPS -mips64r6` 
  :option:`MIPS -mips16`  :option:`MIPS -mno-mips16`  :option:`MIPS -mflip-mips16` 
  :option:`MIPS -minterlink-compressed`  :option:`MIPS -mno-interlink-compressed` 
  :option:`MIPS -minterlink-mips16`  :option:`MIPS -mno-interlink-mips16` 
  :option:`MIPS -mabi`:samp:`={abi}`  :option:`MIPS -mabicalls`  :option:`MIPS -mno-abicalls` 
  :option:`MIPS -mshared`  :option:`MIPS -mno-shared`  :option:`MIPS -mplt`  :option:`MIPS -mno-plt`  :option:`MIPS -mxgot`  :option:`MIPS -mno-xgot` 
  :option:`MIPS -mgp32`  :option:`MIPS -mgp64`  :option:`MIPS -mfp32`  :option:`MIPS -mfpxx`  :option:`MIPS -mfp64`  :option:`MIPS -mhard-float`  :option:`MIPS -msoft-float` 
  :option:`MIPS -mno-float`  :option:`MIPS -msingle-float`  :option:`MIPS -mdouble-float` 
  :option:`MIPS -modd-spreg`  :option:`MIPS -mno-odd-spreg` 
  :option:`MIPS -mabs`:samp:`={mode}`  :option:`MIPS -mnan`:samp:`={encoding}` 
  :option:`MIPS -mdsp`  :option:`MIPS -mno-dsp`  :option:`MIPS -mdspr2`  :option:`MIPS -mno-dspr2` 
  :option:`MIPS -mmcu`  :option:`MIPS -mmno-mcu` 
  :option:`MIPS -meva`  :option:`MIPS -mno-eva` 
  :option:`MIPS -mvirt`  :option:`MIPS -mno-virt` 
  :option:`MIPS -mxpa`  :option:`MIPS -mno-xpa` 
  :option:`MIPS -mcrc`  :option:`MIPS -mno-crc` 
  :option:`MIPS -mginv`  :option:`MIPS -mno-ginv` 
  :option:`MIPS -mmicromips`  :option:`MIPS -mno-micromips` 
  :option:`MIPS -mmsa`  :option:`MIPS -mno-msa` 
  :option:`MIPS -mloongson-mmi`  :option:`MIPS -mno-loongson-mmi` 
  :option:`MIPS -mloongson-ext`  :option:`MIPS -mno-loongson-ext` 
  :option:`MIPS -mloongson-ext2`  :option:`MIPS -mno-loongson-ext2` 
  :option:`MIPS -mfpu`:samp:`={fpu-type}` 
  :option:`MIPS -msmartmips`  :option:`MIPS -mno-smartmips` 
  :option:`MIPS -mpaired-single`  :option:`MIPS -mno-paired-single`  :option:`MIPS -mdmx`  :option:`MIPS -mno-mdmx` 
  :option:`MIPS -mips3d`  :option:`MIPS -mno-mips3d`  :option:`MIPS -mmt`  :option:`MIPS -mno-mt`  :option:`MIPS -mllsc`  :option:`MIPS -mno-llsc` 
  :option:`MIPS -mlong64`  :option:`MIPS -mlong32`  :option:`MIPS -msym32`  :option:`MIPS -mno-sym32` 
  :option:`MIPS -G`:samp:`{num}`  :option:`MIPS -mlocal-sdata`  :option:`MIPS -mno-local-sdata` 
  :option:`MIPS -mextern-sdata`  :option:`MIPS -mno-extern-sdata`  :option:`MIPS -mgpopt`  :option:`MIPS -mno-gopt` 
  :option:`MIPS -membedded-data`  :option:`MIPS -mno-embedded-data` 
  :option:`MIPS -muninit-const-in-rodata`  :option:`MIPS -mno-uninit-const-in-rodata` 
  :option:`MIPS -mcode-readable`:samp:`={setting}` 
  :option:`MIPS -msplit-addresses`  :option:`MIPS -mno-split-addresses` 
  :option:`MIPS -mexplicit-relocs`  :option:`MIPS -mno-explicit-relocs` 
  :option:`MIPS -mcheck-zero-division`  :option:`MIPS -mno-check-zero-division` 
  :option:`MIPS -mdivide-traps`  :option:`MIPS -mdivide-breaks` 
  :option:`MIPS -mload-store-pairs`  :option:`MIPS -mno-load-store-pairs` 
  :option:`MIPS -mmemcpy`  :option:`MIPS -mno-memcpy`  :option:`MIPS -mlong-calls`  :option:`MIPS -mno-long-calls` 
  :option:`MIPS -mmad`  :option:`MIPS -mno-mad`  :option:`MIPS -mimadd`  :option:`MIPS -mno-imadd`  :option:`MIPS -mfused-madd`  :option:`MIPS -mno-fused-madd`  :option:`MIPS -nocpp` 
  :option:`MIPS -mfix-24k`  :option:`MIPS -mno-fix-24k` 
  :option:`MIPS -mfix-r4000`  :option:`MIPS -mno-fix-r4000`  :option:`MIPS -mfix-r4400`  :option:`MIPS -mno-fix-r4400` 
  :option:`MIPS -mfix-r5900`  :option:`MIPS -mno-fix-r5900` 
  :option:`MIPS -mfix-r10000`  :option:`MIPS -mno-fix-r10000`  :option:`MIPS -mfix-rm7000`  :option:`MIPS -mno-fix-rm7000` 
  :option:`MIPS -mfix-vr4120`  :option:`MIPS -mno-fix-vr4120` 
  :option:`MIPS -mfix-vr4130`  :option:`MIPS -mno-fix-vr4130`  :option:`MIPS -mfix-sb1`  :option:`MIPS -mno-fix-sb1` 
  :option:`MIPS -mflush-func`:samp:`={func}`  :option:`MIPS -mno-flush-func` 
  :option:`MIPS -mbranch-cost`:samp:`={num}`  :option:`MIPS -mbranch-likely`  :option:`MIPS -mno-branch-likely` 
  :option:`MIPS -mcompact-branches`:samp:`={policy}` 
  :option:`MIPS -mfp-exceptions`  :option:`MIPS -mno-fp-exceptions` 
  :option:`MIPS -mvr4130-align`  :option:`MIPS -mno-vr4130-align`  :option:`MIPS -msynci`  :option:`MIPS -mno-synci` 
  :option:`MIPS -mlxc1-sxc1`  :option:`MIPS -mno-lxc1-sxc1`  :option:`MIPS -mmadd4`  :option:`MIPS -mno-madd4` 
  :option:`MIPS -mrelax-pic-calls`  :option:`MIPS -mno-relax-pic-calls`  :option:`MIPS -mmcount-ra-address` 
  :option:`MIPS -mframe-header-opt`  :option:`MIPS -mno-frame-header-opt`

  *MMIX Options*

  :option:`MMIX -mlibfuncs`  :option:`MMIX -mno-libfuncs`  :option:`MMIX -mepsilon`  :option:`MMIX -mno-epsilon`  :option:`MMIX -mabi`:samp:`=gnu` 
  :option:`MMIX -mabi`:samp:`=mmixware`  :option:`MMIX -mzero-extend`  :option:`MMIX -mknuthdiv`  :option:`MMIX -mtoplevel-symbols` 
  :option:`MMIX -melf`  :option:`MMIX -mbranch-predict`  :option:`MMIX -mno-branch-predict`  :option:`MMIX -mbase-addresses` 
  :option:`MMIX -mno-base-addresses`  :option:`MMIX -msingle-exit`  :option:`MMIX -mno-single-exit`

  *MN10300 Options*

  :option:`MN10300 -mmult-bug`  :option:`MN10300 -mno-mult-bug` 
  :option:`MN10300 -mno-am33`  :option:`MN10300 -mam33`  :option:`MN10300 -mam33-2`  :option:`MN10300 -mam34` 
  :option:`MN10300 -mtune`:samp:`={cpu-type}` 
  :option:`MN10300 -mreturn-pointer-on-d0` 
  :option:`MN10300 -mno-crt0`  :option:`MN10300 -mrelax`  :option:`MN10300 -mliw`  :option:`MN10300 -msetlb`

  *Moxie Options*

  :option:`Moxie -meb`  :option:`Moxie -mel`  :option:`Moxie -mmul.x`  :option:`Moxie -mno-crt0`

  *MSP430 Options*

  :option:`MSP430 -msim` :option:`MSP430 -masm-hex` :option:`MSP430 -mmcu` :option:`MSP430 -mcpu` :option:`MSP430 -mlarge` :option:`MSP430 -msmall` :option:`MSP430 -mrelax` 
  :option:`MSP430 -mwarn-mcu` 
  :option:`MSP430 -mcode-region` :option:`MSP430 -mdata-region` 
  :option:`MSP430 -msilicon-errata` :option:`MSP430 -msilicon-errata-warn` 
  :option:`MSP430 -mhwmult` :option:`MSP430 -minrt` :option:`MSP430 -mtiny-printf` :option:`MSP430 -mmax-inline-shift`

  *NDS32 Options*

  :option:`NDS32 -mbig-endian`  :option:`NDS32 -mlittle-endian` 
  :option:`NDS32 -mreduced-regs`  :option:`NDS32 -mfull-regs` 
  :option:`NDS32 -mcmov`  :option:`NDS32 -mno-cmov` 
  :option:`NDS32 -mext-perf`  :option:`NDS32 -mno-ext-perf` 
  :option:`NDS32 -mext-perf2`  :option:`NDS32 -mno-ext-perf2` 
  :option:`NDS32 -mext-string`  :option:`NDS32 -mno-ext-string` 
  :option:`NDS32 -mv3push`  :option:`NDS32 -mno-v3push` 
  :option:`NDS32 -m16bit`  :option:`NDS32 -mno-16bit` 
  :option:`NDS32 -misr-vector-size`:samp:`={num}` 
  :option:`NDS32 -mcache-block-size`:samp:`={num}` 
  :option:`NDS32 -march`:samp:`={arch}` 
  :option:`NDS32 -mcmodel`:samp:`={code-model}` 
  :option:`NDS32 -mctor-dtor`  :option:`NDS32 -mrelax`

  *Nios II Options*

  :option:`Nios II -G` :samp:`{num}`  :option:`Nios II -mgpopt`:samp:`={option}`  :option:`Nios II -mgpopt`  :option:`Nios II -mno-gpopt` 
  :option:`Nios II -mgprel-sec`:samp:`={regexp}`  :option:`Nios II -mr0rel-sec`:samp:`={regexp}` 
  :option:`Nios II -mel`  :option:`Nios II -meb` 
  :option:`Nios II -mno-bypass-cache`  :option:`Nios II -mbypass-cache` 
  :option:`Nios II -mno-cache-volatile`  :option:`Nios II -mcache-volatile` 
  :option:`Nios II -mno-fast-sw-div`  :option:`Nios II -mfast-sw-div` 
  :option:`Nios II -mhw-mul`  :option:`Nios II -mno-hw-mul`  :option:`Nios II -mhw-mulx` :option:`Nios II -mno-hw-mulx` :option:`Nios II -mno-hw-div` :option:`Nios II -mhw-div` 
  :option:`Nios II -mcustom-`:samp:`{insn}`:samp:`={N}`  :option:`Nios II -mno-custom-`:samp:`{insn}` 
  :option:`Nios II -mcustom-fpu-cfg`:samp:`={name}` 
  :option:`Nios II -mhal`  :option:`Nios II -msmallc` :option:`Nios II -msys-crt0`:samp:`={name}`  option:`Nios II -msys-lib`:samp:`={name}` 
  :option:`Nios II -march`:samp:`={arch}`  :option:`Nios II -mbmx`  :option:`Nios II -mno-bmx`  :option:`Nios II -mcdx`  :option:`Nios II -mno-cdx`

  *Nvidia PTX Options*

  :option:`Nvidia PTX -m64`  :option:`Nvidia PTX -mmainkernel`  :option:`Nvidia PTX -moptimize`

  *OpenRISC Options*

  :option:`OpenRISC -mboard`:samp:`={name}`  :option:`OpenRISC -mnewlib`  :option:`OpenRISC -mhard-mul`  :option:`OpenRISC -mhard-div` 
  :option:`OpenRISC -msoft-mul`  :option:`OpenRISC -msoft-div` 
  :option:`OpenRISC -msoft-float`  :option:`OpenRISC -mhard-float`  :option:`OpenRISC -mdouble-float` :option:`OpenRISC -munordered-float` 
  :option:`OpenRISC -mcmov`  :option:`OpenRISC -mror`  :option:`OpenRISC -mrori`  :option:`OpenRISC -msext`  :option:`OpenRISC -msfimm`  :option:`OpenRISC -mshftimm` 
  :option:`OpenRISC -mcmodel`:samp:`={code-model}`

  *PDP-11 Options*

  :option:`PDP-11 -mfpu`  :option:`PDP-11 -msoft-float`  :option:`PDP-11 -mac0`  :option:`PDP-11 -mno-ac0`  :option:`PDP-11 -m40`  :option:`PDP-11 -m45`  :option:`PDP-11 -m10` 
  :option:`PDP-11 -mint32`  :option:`PDP-11 -mno-int16`  :option:`PDP-11 -mint16`  :option:`PDP-11 -mno-int32` 
  :option:`PDP-11 -msplit`  :option:`PDP-11 -munix-asm`  :option:`PDP-11 -mdec-asm`  :option:`PDP-11 -mgnu-asm`  :option:`PDP-11 -mlra`

  *picoChip Options*

  :option:`picoChip -mae`:samp:`={ae_type}`  :option:`picoChip -mvliw-lookahead`:samp:`={N}` 
  :option:`picoChip -msymbol-as-address`  :option:`picoChip -mno-inefficient-warnings`

  *PowerPC Options*
  See RS/6000 and PowerPC Options.

  *PRU Options*

  :option:`PRU -mmcu`:samp:`={mcu}`  :option:`PRU -minrt`  :option:`PRU -mno-relax`  :option:`PRU -mloop` 
  :option:`PRU -mabi`:samp:`={variant}` 

  *RISC-V Options*

  :option:`RISC-V -mbranch-cost`:samp:`={N-instruction}` 
  :option:`RISC-V -mplt`  :option:`RISC-V -mno-plt` 
  :option:`RISC-V -mabi`:samp:`={ABI-string}` 
  :option:`RISC-V -mfdiv`  :option:`RISC-V -mno-fdiv` 
  :option:`RISC-V -mdiv`  :option:`RISC-V -mno-div` 
  :option:`RISC-V -march`:samp:`={ISA-string}` 
  :option:`RISC-V -mtune`:samp:`={processor-string}` 
  :option:`RISC-V -mpreferred-stack-boundary`:samp:`={num}` 
  :option:`RISC-V -msmall-data-limit`:samp:`={N-bytes}` 
  :option:`RISC-V -msave-restore`  :option:`RISC-V -mno-save-restore` 
  :option:`RISC-V -mshorten-memrefs`  :option:`RISC-V -mno-shorten-memrefs` 
  :option:`RISC-V -mstrict-align`  :option:`RISC-V -mno-strict-align` 
  :option:`RISC-V -mcmodel`:samp:`=medlow`  :option:`RISC-V -mcmodel`:samp:`=medany` 
  :option:`RISC-V -mexplicit-relocs`  :option:`RISC-V -mno-explicit-relocs` 
  :option:`RISC-V -mrelax`  :option:`RISC-V -mno-relax` 
  :option:`RISC-V -mriscv-attribute`  :option:`RISC-V -mmo-riscv-attribute` 
  :option:`RISC-V -malign-data`:samp:`={type}` 
  :option:`RISC-V -mbig-endian`  :option:`RISC-V -mlittle-endian` 
  :option:`RISC-V -mstack-protector-guard`:samp:`={guard}` :option:`RISC-V -mstack-protector-guard-reg`:samp:`={reg}` 
  :option:`RISC-V -mstack-protector-guard-offset`:samp:`={offset}`

  *RL78 Options*

  :option:`RL78 -msim`  :option:`RL78 -mmul`:samp:`=none`  :option:`RL78 -mmul`:samp:`=g13`  :option:`RL78 -mmul`:samp:`=g14`  :option:`RL78 -mallregs` 
  :option:`RL78 -mcpu`:samp:`=g10`  :option:`RL78 -mcpu`:samp:`=g13`  :option:`RL78 -mcpu`:samp:`=g14`  :option:`RL78 -mg10`  :option:`RL78 -mg13`  :option:`RL78 -mg14` 
  :option:`RL78 -m64bit-doubles`  :option:`RL78 -m32bit-doubles`  :option:`RL78 -msave-mduc-in-interrupts`

  *RS/6000 and PowerPC Options*

  :option:`RS/6000 and PowerPC -mcpu`:samp:`={cpu-type}` 
  :option:`RS/6000 and PowerPC -mtune`:samp:`={cpu-type}` 
  :option:`RS/6000 and PowerPC -mcmodel`:samp:`={code-model}` 
  :option:`RS/6000 and PowerPC -mpowerpc64` 
  :option:`RS/6000 and PowerPC -maltivec`  :option:`RS/6000 and PowerPC -mno-altivec` 
  :option:`RS/6000 and PowerPC -mpowerpc-gpopt`  :option:`RS/6000 and PowerPC -mno-powerpc-gpopt` 
  :option:`RS/6000 and PowerPC -mpowerpc-gfxopt`  :option:`RS/6000 and PowerPC -mno-powerpc-gfxopt` 
  :option:`RS/6000 and PowerPC -mmfcrf`  :option:`RS/6000 and PowerPC -mno-mfcrf`  :option:`RS/6000 and PowerPC -mpopcntb`  :option:`RS/6000 and PowerPC -mno-popcntb`  :option:`RS/6000 and PowerPC -mpopcntd`  :option:`RS/6000 and PowerPC -mno-popcntd` 
  :option:`RS/6000 and PowerPC -mfprnd`  :option:`RS/6000 and PowerPC -mno-fprnd` 
  :option:`RS/6000 and PowerPC -mcmpb`  :option:`RS/6000 and PowerPC -mno-cmpb`  :option:`RS/6000 and PowerPC -mhard-dfp`  :option:`RS/6000 and PowerPC -mno-hard-dfp` 
  :option:`RS/6000 and PowerPC -mfull-toc`   :option:`RS/6000 and PowerPC -mminimal-toc`  :option:`RS/6000 and PowerPC -mno-fp-in-toc`  :option:`RS/6000 and PowerPC -mno-sum-in-toc` 
  :option:`RS/6000 and PowerPC -m64`  :option:`RS/6000 and PowerPC -m32`  :option:`RS/6000 and PowerPC -mxl-compat`  :option:`RS/6000 and PowerPC -mno-xl-compat`  :option:`RS/6000 and PowerPC -mpe` 
  :option:`RS/6000 and PowerPC -malign-power`  :option:`RS/6000 and PowerPC -malign-natural` 
  :option:`RS/6000 and PowerPC -msoft-float`  :option:`RS/6000 and PowerPC -mhard-float`  :option:`RS/6000 and PowerPC -mmultiple`  :option:`RS/6000 and PowerPC -mno-multiple` 
  :option:`RS/6000 and PowerPC -mupdate`  :option:`RS/6000 and PowerPC -mno-update` 
  :option:`RS/6000 and PowerPC -mavoid-indexed-addresses`  :option:`RS/6000 and PowerPC -mno-avoid-indexed-addresses` 
  :option:`RS/6000 and PowerPC -mfused-madd`  :option:`RS/6000 and PowerPC -mno-fused-madd`  :option:`RS/6000 and PowerPC -mbit-align`  :option:`RS/6000 and PowerPC -mno-bit-align` 
  :option:`RS/6000 and PowerPC -mstrict-align`  :option:`RS/6000 and PowerPC -mno-strict-align`  :option:`RS/6000 and PowerPC -mrelocatable` 
  :option:`RS/6000 and PowerPC -mno-relocatable`  :option:`RS/6000 and PowerPC -mrelocatable-lib`  :option:`RS/6000 and PowerPC -mno-relocatable-lib` 
  :option:`RS/6000 and PowerPC -mtoc`  :option:`RS/6000 and PowerPC -mno-toc`  :option:`RS/6000 and PowerPC -mlittle`  :option:`RS/6000 and PowerPC -mlittle-endian`  :option:`RS/6000 and PowerPC -mbig`  :option:`RS/6000 and PowerPC -mbig-endian` 
  :option:`RS/6000 and PowerPC -mdynamic-no-pic`  :option:`RS/6000 and PowerPC -mswdiv`  :option:`RS/6000 and PowerPC -msingle-pic-base` 
  :option:`RS/6000 and PowerPC -mprioritize-restricted-insns`:samp:`={priority}` 
  :option:`RS/6000 and PowerPC -msched-costly-dep`:samp:`={dependence_type}` 
  :option:`RS/6000 and PowerPC -minsert-sched-nops`:samp:`={scheme}` 
  :option:`RS/6000 and PowerPC -mcall-aixdesc`  :option:`RS/6000 and PowerPC -mcall-eabi`  :option:`RS/6000 and PowerPC -mcall-freebsd`  
  :option:`RS/6000 and PowerPC -mcall-linux`  :option:`RS/6000 and PowerPC -mcall-netbsd`  :option:`RS/6000 and PowerPC -mcall-openbsd`  
  :option:`RS/6000 and PowerPC -mcall-sysv`  :option:`RS/6000 and PowerPC -mcall-sysv-eabi`  :option:`RS/6000 and PowerPC -mcall-sysv-noeabi` 
  :option:`RS/6000 and PowerPC -mtraceback`:samp:`={traceback_type}` 
  :option:`RS/6000 and PowerPC -maix-struct-return`  :option:`RS/6000 and PowerPC -msvr4-struct-return` 
  :option:`RS/6000 and PowerPC -mabi`:samp:`={abi-type}`  :option:`RS/6000 and PowerPC -msecure-plt`  :option:`RS/6000 and PowerPC -mbss-plt` 
  :option:`RS/6000 and PowerPC -mlongcall`  :option:`RS/6000 and PowerPC -mno-longcall`  :option:`RS/6000 and PowerPC -mpltseq`  :option:`RS/6000 and PowerPC -mno-pltseq`  
  :option:`RS/6000 and PowerPC -mblock-move-inline-limit`:samp:`={num}` 
  :option:`RS/6000 and PowerPC -mblock-compare-inline-limit`:samp:`={num}` 
  :option:`RS/6000 and PowerPC -mblock-compare-inline-loop-limit`:samp:`={num}` 
  :option:`RS/6000 and PowerPC -mno-block-ops-unaligned-vsx` 
  :option:`RS/6000 and PowerPC -mstring-compare-inline-limit`:samp:`={num}` 
  :option:`RS/6000 and PowerPC -misel`  :option:`RS/6000 and PowerPC -mno-isel` 
  :option:`RS/6000 and PowerPC -mvrsave`  :option:`RS/6000 and PowerPC -mno-vrsave` 
  :option:`RS/6000 and PowerPC -mmulhw`  :option:`RS/6000 and PowerPC -mno-mulhw` 
  :option:`RS/6000 and PowerPC -mdlmzb`  :option:`RS/6000 and PowerPC -mno-dlmzb` 
  :option:`RS/6000 and PowerPC -mprototype`  :option:`RS/6000 and PowerPC -mno-prototype` 
  :option:`RS/6000 and PowerPC -msim`  :option:`RS/6000 and PowerPC -mmvme`  :option:`RS/6000 and PowerPC -mads`  :option:`RS/6000 and PowerPC -myellowknife`  :option:`RS/6000 and PowerPC -memb`  :option:`RS/6000 and PowerPC -msdata` 
  :option:`RS/6000 and PowerPC -msdata`:samp:`={opt}`  :option:`RS/6000 and PowerPC -mreadonly-in-sdata`  :option:`RS/6000 and PowerPC -mvxworks`  :option:`RS/6000 and PowerPC -G` :samp:`{num}` 
  :option:`RS/6000 and PowerPC -mrecip`  :option:`RS/6000 and PowerPC -mrecip`:samp:`={opt}`  :option:`RS/6000 and PowerPC -mno-recip`  :option:`RS/6000 and PowerPC -mrecip-precision` 
  :option:`RS/6000 and PowerPC -mno-recip-precision` 
  :option:`RS/6000 and PowerPC -mveclibabi`:samp:`={type}`  :option:`RS/6000 and PowerPC -mfriz`  :option:`RS/6000 and PowerPC -mno-friz` 
  :option:`RS/6000 and PowerPC -mpointers-to-nested-functions`  :option:`RS/6000 and PowerPC -mno-pointers-to-nested-functions` 
  :option:`RS/6000 and PowerPC -msave-toc-indirect`  :option:`RS/6000 and PowerPC -mno-save-toc-indirect` 
  :option:`RS/6000 and PowerPC -mpower8-fusion`  :option:`RS/6000 and PowerPC -mno-mpower8-fusion`  :option:`RS/6000 and PowerPC -mpower8-vector`  :option:`RS/6000 and PowerPC -mno-power8-vector` 
  :option:`RS/6000 and PowerPC -mcrypto`  :option:`RS/6000 and PowerPC -mno-crypto`  :option:`RS/6000 and PowerPC -mhtm`  :option:`RS/6000 and PowerPC -mno-htm` 
  :option:`RS/6000 and PowerPC -mquad-memory`  :option:`RS/6000 and PowerPC -mno-quad-memory` 
  :option:`RS/6000 and PowerPC -mquad-memory-atomic`  :option:`RS/6000 and PowerPC -mno-quad-memory-atomic` 
  :option:`RS/6000 and PowerPC -mcompat-align-parm`  :option:`RS/6000 and PowerPC -mno-compat-align-parm` 
  :option:`RS/6000 and PowerPC -mfloat128`  :option:`RS/6000 and PowerPC -mno-float128`  :option:`RS/6000 and PowerPC -mfloat128-hardware`  :option:`RS/6000 and PowerPC -mno-float128-hardware` 
  :option:`RS/6000 and PowerPC -mgnu-attribute`  :option:`RS/6000 and PowerPC -mno-gnu-attribute` 
  :option:`RS/6000 and PowerPC -mstack-protector-guard`:samp:`={guard}` :option:`RS/6000 and PowerPC -mstack-protector-guard-reg`:samp:`={reg}` 
  :option:`RS/6000 and PowerPC -mstack-protector-guard-offset`:samp:`={offset}` :option:`RS/6000 and PowerPC -mprefixed` :option:`RS/6000 and PowerPC -mno-prefixed` 
  :option:`RS/6000 and PowerPC -mpcrel` :option:`RS/6000 and PowerPC -mno-pcrel` :option:`RS/6000 and PowerPC -mmma` :option:`RS/6000 and PowerPC -mno-mmma` :option:`RS/6000 and PowerPC -mrop-protect` :option:`RS/6000 and PowerPC -mno-rop-protect` 
  :option:`RS/6000 and PowerPC -mprivileged` :option:`RS/6000 and PowerPC -mno-privileged`

  *RX Options*

  :option:`RX -m64bit-doubles`  :option:`RX -m32bit-doubles`  :option:`RX -fpu`  :option:`RX -nofpu`
  :option:`RX -mcpu` =
  :option:`RX -mbig-endian-data`  :option:`RX -mlittle-endian-data` 
  :option:`RX -msmall-data` 
  :option:`RX -msim`  :option:`RX -mno-sim`
  :option:`RX -mas100-syntax`  :option:`RX -mno-as100-syntax`
  :option:`RX -mrelax`
  :option:`RX -mmax-constant-size` =
  :option:`RX -mint-register` =
  :option:`RX -mpid`
  :option:`RX -mallow-string-insns`  :option:`RX -mno-allow-string-insns`
  :option:`RX -mjsr`
  :option:`RX -mno-warn-multiple-fast-interrupts`
  :option:`RX -msave-acc-in-interrupts`

  *S/390 and zSeries Options*

  :option:`S/390 and zSeries -mtune`:samp:`={cpu-type}`  :option:`S/390 and zSeries -march`:samp:`={cpu-type}` 
  :option:`S/390 and zSeries -mhard-float`  :option:`S/390 and zSeries -msoft-float`  :option:`S/390 and zSeries -mhard-dfp`  :option:`S/390 and zSeries -mno-hard-dfp` 
  :option:`S/390 and zSeries -mlong-double-64`  :option:`S/390 and zSeries -mlong-double-128` 
  :option:`S/390 and zSeries -mbackchain`  :option:`S/390 and zSeries -mno-backchain`  :option:`S/390 and zSeries -mpacked-stack`  :option:`S/390 and zSeries -mno-packed-stack` 
  :option:`S/390 and zSeries -msmall-exec`  :option:`S/390 and zSeries -mno-small-exec`  :option:`S/390 and zSeries -mmvcle`  :option:`S/390 and zSeries -mno-mvcle` 
  :option:`S/390 and zSeries -m64`  :option:`S/390 and zSeries -m31`  :option:`S/390 and zSeries -mdebug`  :option:`S/390 and zSeries -mno-debug`  :option:`S/390 and zSeries -mesa`  :option:`S/390 and zSeries -mzarch` 
  :option:`S/390 and zSeries -mhtm`  :option:`S/390 and zSeries -mvx`  :option:`S/390 and zSeries -mzvector` 
  :option:`S/390 and zSeries -mtpf-trace`  :option:`S/390 and zSeries -mno-tpf-trace`  :option:`S/390 and zSeries -mtpf-trace-skip`  :option:`S/390 and zSeries -mno-tpf-trace-skip` 
  :option:`S/390 and zSeries -mfused-madd`  :option:`S/390 and zSeries -mno-fused-madd` 
  :option:`S/390 and zSeries -mwarn-framesize`  :option:`S/390 and zSeries -mwarn-dynamicstack`  :option:`S/390 and zSeries -mstack-size`  :option:`S/390 and zSeries -mstack-guard` 
  :option:`S/390 and zSeries -mhotpatch`:samp:`={halfwords},{halfwords}`

  *Score Options*

  :option:`Score -meb`  :option:`Score -mel` 
  :option:`Score -mnhwloop` 
  :option:`Score -muls` 
  :option:`Score -mmac` 
  :option:`Score -mscore5`  :option:`Score -mscore5u`  :option:`Score -mscore7`  :option:`Score -mscore7d`

  *SH Options*

  :option:`SH -m1`  :option:`SH -m2`  :option:`SH -m2e` 
  :option:`SH -m2a-nofpu`  :option:`SH -m2a-single-only`  :option:`SH -m2a-single`  :option:`SH -m2a` 
  :option:`SH -m3`  :option:`SH -m3e` 
  :option:`SH -m4-nofpu`  :option:`SH -m4-single-only`  :option:`SH -m4-single`  :option:`SH -m4` 
  :option:`SH -m4a-nofpu`  :option:`SH -m4a-single-only`  :option:`SH -m4a-single`  :option:`SH -m4a`  :option:`SH -m4al` 
  :option:`SH -mb`  :option:`SH -ml`  :option:`SH -mdalign`  :option:`SH -mrelax` 
  :option:`SH -mbigtable`  :option:`SH -mfmovd`  :option:`SH -mrenesas`  :option:`SH -mno-renesas`  :option:`SH -mnomacsave` 
  :option:`SH -mieee`  :option:`SH -mno-ieee`  :option:`SH -mbitops`  :option:`SH -misize`  :option:`SH -minline-ic_invalidate`  :option:`SH -mpadstruct` 
  :option:`SH -mprefergot`  :option:`SH -musermode`  :option:`SH -multcost`:samp:`={number}`  :option:`SH -mdiv`:samp:`={strategy}` 
  :option:`SH -mdivsi3_libfunc`:samp:`={name}`  :option:`SH -mfixed-range`:samp:`={register-range}` 
  :option:`SH -maccumulate-outgoing-args` 
  :option:`SH -matomic-model`:samp:`={atomic-model}` 
  :option:`SH -mbranch-cost`:samp:`={num}`  :option:`SH -mzdcbranch`  :option:`SH -mno-zdcbranch` 
  :option:`SH -mcbranch-force-delay-slot` 
  :option:`SH -mfused-madd`  :option:`SH -mno-fused-madd`  :option:`SH -mfsca`  :option:`SH -mno-fsca`  :option:`SH -mfsrra`  :option:`SH -mno-fsrra` 
  :option:`SH -mpretend-cmove`  :option:`SH -mtas`

  *Solaris 2 Options*

  :option:`Solaris 2 -mclear-hwcap`  :option:`Solaris 2 -mno-clear-hwcap`  :option:`Solaris 2 -mimpure-text`  :option:`Solaris 2 -mno-impure-text` 
  :option:`Solaris 2 -pthreads`

  *SPARC Options*

  :option:`SPARC -mcpu`:samp:`={cpu-type}` 
  :option:`SPARC -mtune`:samp:`={cpu-type}` 
  :option:`SPARC -mcmodel`:samp:`={code-model}` 
  :option:`SPARC -mmemory-model`:samp:`={mem-model}` 
  :option:`SPARC -m32`  :option:`SPARC -m64`  :option:`SPARC -mapp-regs`  :option:`SPARC -mno-app-regs` 
  :option:`SPARC -mfaster-structs`  :option:`SPARC -mno-faster-structs`  :option:`SPARC -mflat`  :option:`SPARC -mno-flat` 
  :option:`SPARC -mfpu`  :option:`SPARC -mno-fpu`  :option:`SPARC -mhard-float`  :option:`SPARC -msoft-float` 
  :option:`SPARC -mhard-quad-float`  :option:`SPARC -msoft-quad-float` 
  :option:`SPARC -mstack-bias`  :option:`SPARC -mno-stack-bias` 
  :option:`SPARC -mstd-struct-return`  :option:`SPARC -mno-std-struct-return` 
  :option:`SPARC -munaligned-doubles`  :option:`SPARC -mno-unaligned-doubles` 
  :option:`SPARC -muser-mode`  :option:`SPARC -mno-user-mode` 
  :option:`SPARC -mv8plus`  :option:`SPARC -mno-v8plus`  :option:`SPARC -mvis`  :option:`SPARC -mno-vis` 
  :option:`SPARC -mvis2`  :option:`SPARC -mno-vis2`  :option:`SPARC -mvis3`  :option:`SPARC -mno-vis3` 
  :option:`SPARC -mvis4`  :option:`SPARC -mno-vis4`  :option:`SPARC -mvis4b`  :option:`SPARC -mno-vis4b` 
  :option:`SPARC -mcbcond`  :option:`SPARC -mno-cbcond`  :option:`SPARC -mfmaf`  :option:`SPARC -mno-fmaf`  :option:`SPARC -mfsmuld`  :option:`SPARC -mno-fsmuld`  
  :option:`SPARC -mpopc`  :option:`SPARC -mno-popc`  :option:`SPARC -msubxc`  :option:`SPARC -mno-subxc` 
  :option:`SPARC -mfix-at697f`  :option:`SPARC -mfix-ut699`  :option:`SPARC -mfix-ut700`  :option:`SPARC -mfix-gr712rc` 
  :option:`SPARC -mlra`  :option:`SPARC -mno-lra`

  *System V Options*

  :option:`System V -Qy`  :option:`System V -Qn`  :option:`System V -YP,`:samp:`{paths}`  :option:`System V -Ym,`:samp:`{dir}`

  *TILE-Gx Options*

  :option:`TILE-Gx -mcpu`:samp:`=CPU`  :option:`TILE-Gx -m32`  :option:`TILE-Gx -m64`  :option:`TILE-Gx -mbig-endian`  :option:`TILE-Gx -mlittle-endian` 
  :option:`TILE-Gx -mcmodel`:samp:`={code-model}`

  *TILEPro Options*

  :option:`TILEPro -mcpu`:samp:`={cpu}`  :option:`TILEPro -m32`

  *V850 Options*

  :option:`V850 -mlong-calls`  :option:`V850 -mno-long-calls`  :option:`V850 -mep`  :option:`V850 -mno-ep` 
  :option:`V850 -mprolog-function`  :option:`V850 -mno-prolog-function`  :option:`V850 -mspace` 
  :option:`V850 -mtda`:samp:`={n}`  :option:`V850 -msda`:samp:`={n}`  :option:`V850 -mzda`:samp:`={n}` 
  :option:`V850 -mapp-regs`  :option:`V850 -mno-app-regs` 
  :option:`V850 -mdisable-callt`  :option:`V850 -mno-disable-callt` 
  :option:`V850 -mv850e2v3`  :option:`V850 -mv850e2`  :option:`V850 -mv850e1`  :option:`V850 -mv850es` 
  :option:`V850 -mv850e`  :option:`V850 -mv850`  :option:`V850 -mv850e3v5` 
  :option:`V850 -mloop` 
  :option:`V850 -mrelax` 
  :option:`V850 -mlong-jumps` 
  :option:`V850 -msoft-float` 
  :option:`V850 -mhard-float` 
  :option:`V850 -mgcc-abi` 
  :option:`V850 -mrh850-abi` 
  :option:`V850 -mbig-switch`

  *VAX Options*

  :option:`VAX -mg`  :option:`VAX -mgnu`  :option:`VAX -munix`  :option:`VAX -mlra`

  *Visium Options*

  :option:`Visium -mdebug`  :option:`Visium -msim`  :option:`Visium -mfpu`  :option:`Visium -mno-fpu`  :option:`Visium -mhard-float`  :option:`Visium -msoft-float` 
  :option:`Visium -mcpu`:samp:`={cpu-type}`  :option:`Visium -mtune`:samp:`={cpu-type}`  :option:`Visium -msv-mode`  :option:`Visium -muser-mode`

  *VMS Options*

  :option:`VMS -mvms-return-codes`  :option:`VMS -mdebug-main`:samp:`={prefix}`  :option:`VMS -mmalloc64` 
  :option:`VMS -mpointer-size`:samp:`={size}`

  *VxWorks Options*

  :option:`VxWorks -mrtp`  :option:`VxWorks -non-static`  :option:`VxWorks -Bstatic`  :option:`VxWorks -Bdynamic` 
  :option:`VxWorks -Xbind-lazy`  :option:`VxWorks -Xbind-now`

  *x86 Options*

  :option:`x86 -mtune`:samp:`={cpu-type}`  :option:`x86 -march`:samp:`={cpu-type}` 
  :option:`x86 -mtune-ctrl`:samp:`={feature-list}`  :option:`x86 -mdump-tune-features`  :option:`x86 -mno-default` 
  :option:`x86 -mfpmath`:samp:`={unit}` 
  :option:`x86 -masm`:samp:`={dialect}`  :option:`x86 -mno-fancy-math-387` 
  :option:`x86 -mno-fp-ret-in-387`  :option:`x86 -m80387`  :option:`x86 -mhard-float`  :option:`x86 -msoft-float` 
  :option:`x86 -mno-wide-multiply`  :option:`x86 -mrtd`  :option:`x86 -malign-double` 
  :option:`x86 -mpreferred-stack-boundary`:samp:`={num}` 
  :option:`x86 -mincoming-stack-boundary`:samp:`={num}` 
  :option:`x86 -mcld`  :option:`x86 -mcx16`  :option:`x86 -msahf`  :option:`x86 -mmovbe`  :option:`x86 -mcrc32` :option:`x86 -mmwait` 
  :option:`x86 -mrecip`  :option:`x86 -mrecip`:samp:`={opt}` 
  :option:`x86 -mvzeroupper`  :option:`x86 -mprefer-avx128`  :option:`x86 -mprefer-vector-width`:samp:`={opt}` 
  :option:`x86 -mmmx`  :option:`x86 -msse`  :option:`x86 -msse2`  :option:`x86 -msse3`  :option:`x86 -mssse3`  :option:`x86 -msse4.1`  :option:`x86 -msse4.2`  :option:`x86 -msse4`  :option:`x86 -mavx` 
  :option:`x86 -mavx2`  :option:`x86 -mavx512f`  :option:`x86 -mavx512pf`  :option:`x86 -mavx512er`  :option:`x86 -mavx512cd`  :option:`x86 -mavx512vl` 
  :option:`x86 -mavx512bw`  :option:`x86 -mavx512dq`  :option:`x86 -mavx512ifma`  :option:`x86 -mavx512vbmi`  :option:`x86 -msha`  :option:`x86 -maes` 
  :option:`x86 -mpclmul`  :option:`x86 -mfsgsbase`  :option:`x86 -mrdrnd`  :option:`x86 -mf16c`  :option:`x86 -mfma`  :option:`x86 -mpconfig`  :option:`x86 -mwbnoinvd`  
  :option:`x86 -mptwrite`  :option:`x86 -mprefetchwt1`  :option:`x86 -mclflushopt`  :option:`x86 -mclwb`  :option:`x86 -mxsavec`  :option:`x86 -mxsaves` 
  :option:`x86 -msse4a`  :option:`x86 -m3dnow`  :option:`x86 -m3dnowa`  :option:`x86 -mpopcnt`  :option:`x86 -mabm`  :option:`x86 -mbmi`  :option:`x86 -mtbm`  :option:`x86 -mfma4`  :option:`x86 -mxop` 
  :option:`x86 -madx`  :option:`x86 -mlzcnt`  :option:`x86 -mbmi2`  :option:`x86 -mfxsr`  :option:`x86 -mxsave`  :option:`x86 -mxsaveopt`  :option:`x86 -mrtm`  :option:`x86 -mhle`  :option:`x86 -mlwp` 
  :option:`x86 -mmwaitx`  :option:`x86 -mclzero`  :option:`x86 -mpku`  :option:`x86 -mthreads`  :option:`x86 -mgfni`  :option:`x86 -mvaes`  :option:`x86 -mwaitpkg` 
  :option:`x86 -mshstk` :option:`x86 -mmanual-endbr` :option:`x86 -mforce-indirect-call`  :option:`x86 -mavx512vbmi2` :option:`x86 -mavx512bf16` :option:`x86 -menqcmd` 
  :option:`x86 -mvpclmulqdq`  :option:`x86 -mavx512bitalg`  :option:`x86 -mmovdiri`  :option:`x86 -mmovdir64b`  :option:`x86 -mavx512vpopcntdq` 
  :option:`x86 -mavx5124fmaps`  :option:`x86 -mavx512vnni`  :option:`x86 -mavx5124vnniw`  :option:`x86 -mprfchw`  :option:`x86 -mrdpid` 
  :option:`x86 -mrdseed`  :option:`x86 -msgx` :option:`x86 -mavx512vp2intersect` :option:`x86 -mserialize` :option:`x86 -mtsxldtrk`
  :option:`x86 -mamx-tile`  :option:`x86 -mamx-int8`  :option:`x86 -mamx-bf16` :option:`x86 -muintr` :option:`x86 -mhreset` :option:`x86 -mavxvnni`
  :option:`x86 -mavx512fp16` 
  :option:`x86 -mcldemote`  :option:`x86 -mms-bitfields`  :option:`x86 -mno-align-stringops`  :option:`x86 -minline-all-stringops` 
  :option:`x86 -minline-stringops-dynamically`  :option:`x86 -mstringop-strategy`:samp:`={alg}` 
  :option:`x86 -mkl` :option:`x86 -mwidekl` 
  :option:`x86 -mmemcpy-strategy`:samp:`={strategy}`  :option:`x86 -mmemset-strategy`:samp:`={strategy}` 
  :option:`x86 -mpush-args`  :option:`x86 -maccumulate-outgoing-args`  :option:`x86 -m128bit-long-double` 
  :option:`x86 -m96bit-long-double`  :option:`x86 -mlong-double-64`  :option:`x86 -mlong-double-80`  :option:`x86 -mlong-double-128` 
  :option:`x86 -mregparm`:samp:`={num}`  :option:`x86 -msseregparm` 
  :option:`x86 -mveclibabi`:samp:`={type}`  :option:`x86 -mvect8-ret-in-mem` 
  :option:`x86 -mpc32`  :option:`x86 -mpc64`  :option:`x86 -mpc80`  :option:`x86 -mstackrealign` 
  :option:`x86 -momit-leaf-frame-pointer`  :option:`x86 -mno-red-zone`  :option:`x86 -mno-tls-direct-seg-refs` 
  :option:`x86 -mcmodel`:samp:`={code-model}`  :option:`x86 -mabi`:samp:`={name}`  :option:`x86 -maddress-mode`:samp:`={mode}` 
  :option:`x86 -m32`  :option:`x86 -m64`  :option:`x86 -mx32`  :option:`x86 -m16`  :option:`x86 -miamcu`  :option:`x86 -mlarge-data-threshold`:samp:`={num}` 
  :option:`x86 -msse2avx`  :option:`x86 -mfentry`  :option:`x86 -mrecord-mcount`  :option:`x86 -mnop-mcount`  :option:`x86 -m8bit-idiv` 
  :option:`x86 -minstrument-return`:samp:`={type}` :option:`x86 -mfentry-name`:samp:`={name}` :option:`x86 -mfentry-section`:samp:`={name}` 
  :option:`x86 -mavx256-split-unaligned-load`  :option:`x86 -mavx256-split-unaligned-store` 
  :option:`x86 -malign-data`:samp:`={type}`  :option:`x86 -mstack-protector-guard`:samp:`={guard}` 
  :option:`x86 -mstack-protector-guard-reg`:samp:`={reg}` 
  :option:`x86 -mstack-protector-guard-offset`:samp:`={offset}` 
  :option:`x86 -mstack-protector-guard-symbol`:samp:`={symbol}` 
  :option:`x86 -mgeneral-regs-only`  :option:`x86 -mcall-ms2sysv-xlogues` :option:`x86 -mrelax-cmpxchg-loop` 
  :option:`x86 -mindirect-branch`:samp:`={choice}`  :option:`x86 -mfunction-return`:samp:`={choice}` 
  :option:`x86 -mindirect-branch-register` :option:`x86 -mharden-sls`:samp:`={choice}` 
  :option:`x86 -mindirect-branch-cs-prefix` :option:`x86 -mneeded`

  *x86 Windows Options*

  :option:`x86 Windows -mconsole`  :option:`x86 Windows -mcygwin`  :option:`x86 Windows -mno-cygwin`  :option:`x86 Windows -mdll` 
  :option:`x86 Windows -mnop-fun-dllimport`  :option:`x86 Windows -mthread` 
  :option:`x86 Windows -municode`  :option:`x86 Windows -mwin32`  :option:`x86 Windows -mwindows`  :option:`x86 Windows -fno-set-stack-executable`

  *Xstormy16 Options*

  :option:`Xstormy16 -msim`

  *Xtensa Options*

  :option:`Xtensa -mconst16`  :option:`Xtensa -mno-const16` 
  :option:`Xtensa -mfused-madd`  :option:`Xtensa -mno-fused-madd` 
  :option:`Xtensa -mforce-no-pic` 
  :option:`Xtensa -mserialize-volatile`  :option:`Xtensa -mno-serialize-volatile` 
  :option:`Xtensa -mtext-section-literals`  :option:`Xtensa -mno-text-section-literals` 
  :option:`Xtensa -mauto-litpools`  :option:`Xtensa -mno-auto-litpools` 
  :option:`Xtensa -mtarget-align`  :option:`Xtensa -mno-target-align` 
  :option:`Xtensa -mlongcalls`  :option:`Xtensa -mno-longcalls` 
  :option:`Xtensa -mabi`:samp:`={abi-type}`

  *zSeries Options*

  See :ref:`s-390-and-zseries-options`.
