Introduction
============

I'm a demo documentation page.

.. option:: -Wno-shift-overflow, -Wshift-overflow=n, -Wshift-overflow

  .. note::

    Default value is :option:`-Wno-shift-overflow`, :option:`-Wshift-overflow` is enabled by :option:`-Wall`.

  These options control warnings about left shift overflows.

  ``-Wshift-overflow=1``
    This is the warning level of :option:`-Wshift-overflow` and ...


.. option:: -Wno-shift-overflow2, -Wshift-overflow2=n (non-default), -Wshift-overflow2 (non-default)

  These options control warnings about left shift overflows.

  ``-Wshift-overflow2=1``
    This is the warning level of :option:`-Wshift-overflow2` and ...

.. option:: -Wno-shift-overflow3

  Default option value for :option:`-Wshift-overflow3`.

.. option:: -Wshift-overflow3=n, -Wshift-overflow3

  These options control warnings about left shift overflows.

  ``-Wshift-overflow3=1``
    This is the warning level of :option:`-Wshift-overflow3` and ...

  Enabled by :option:`-Wall`.


.. option:: -Wall

Enable it all :)

I am a :command:`super-command`.
I am **strong** and I am *emphasis*.

Show :samp:`Samp with a {variable}.`

Show it::

  gcc a.c
  ./a.out

Code block:

.. code-block::

  gcc a.c
  ./a.out

Note1: ([#]_)
Note2: ([#]_)

.. [#] Future versions of GCC may zero-extend, or use a target-defined ``ptr_extend`` pattern.  Do not rely on sign extension.
.. [#] I am note 2.

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
