.. _xstormy16-function-attributes:

Xstormy16 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Xstormy16 back end:

.. option:: interrupt

  .. index:: interrupt function attribute, Xstormy16

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

