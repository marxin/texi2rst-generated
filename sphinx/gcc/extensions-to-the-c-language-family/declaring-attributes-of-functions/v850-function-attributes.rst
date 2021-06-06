.. _v850-function-attributes:

V850 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

The V850 back end supports these function attributes:

.. option:: interrupt

  .. index:: interrupt function attribute, V850

  .. index:: interrupt_handler function attribute, V850

  Use these attributes to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when either attribute is present.

