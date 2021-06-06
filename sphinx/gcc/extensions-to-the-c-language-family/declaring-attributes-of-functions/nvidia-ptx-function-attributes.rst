.. _nvidia-ptx-function-attributes:

Nvidia PTX Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Nvidia PTX back end:

.. option:: kernel

  .. index:: kernel attribute, Nvidia PTX

  This attribute indicates that the corresponding function should be compiled
  as a kernel function, which can be invoked from the host via the CUDA RT 
  library.
  By default functions are only callable only from other PTX functions.

  Kernel functions must have ``void`` return type.

