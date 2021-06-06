.. _bpf-function-attributes:

BPF Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the BPF back end:

.. option:: kernel_helper

  .. index:: kernel helper, function attribute, BPF

  use this attribute to indicate the specified function declaration is a
  kernel helper.  The helper function is passed as an argument to the
  attribute.  Example:

  .. code-block:: c++

    int bpf_probe_read (void *dst, int size, const void *unsafe_ptr)
      __attribute__ ((kernel_helper (4)));

