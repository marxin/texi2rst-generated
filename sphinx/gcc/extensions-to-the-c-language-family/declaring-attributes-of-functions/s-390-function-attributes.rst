.. _s-390-function-attributes:

S/390 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on the S/390:

.. option:: hotpatch (halfwords-before-function-label,halfwords-after-function-label)

  .. index:: hotpatch function attribute, S/390

  On S/390 System z targets, you can use this function attribute to
  make GCC generate a 'hot-patching' function prologue.  If the
  :option:`-mhotpatch` = command-line option is used at the same time,
  the ``hotpatch`` attribute takes precedence.  The first of the
  two arguments specifies the number of halfwords to be added before
  the function label.  A second argument can be used to specify the
  number of halfwords to be added after the function label.  For
  both arguments the maximum allowed value is 1000000.

  If both arguments are zero, hotpatching is disabled.

.. option:: target (options)

  .. index:: target function attribute

  As discussed in Common Function Attributes, this attribute
  allows specification of target-specific compilation options.

  On S/390, the following options are supported:

  :samp:`arch=` :samp:`tune=` :samp:`stack-guard=` :samp:`stack-size=` :samp:`branch-cost=`
  :samp:`warn-framesize=` :samp:`backchain` :samp:`no-backchain` :samp:`hard-dfp`
  :samp:`no-hard-dfp` :samp:`hard-float` :samp:`soft-float` :samp:`htm` :samp:`no-htm`
  :samp:`vx` :samp:`no-vx` :samp:`packed-stack` :samp:`no-packed-stack` :samp:`small-exec`
  :samp:`no-small-exec` :samp:`mvcle` :samp:`no-mvcle` :samp:`warn-dynamicstack`
  :samp:`no-warn-dynamicstack`

  The options work exactly like the S/390 specific command line
  options (without the prefix :samp:`-m` ) except that they do not
  change any feature macros.  For example,

  .. code-block:: c++

    target("no-vx")

  does not undefine the ``__VEC__`` macro.