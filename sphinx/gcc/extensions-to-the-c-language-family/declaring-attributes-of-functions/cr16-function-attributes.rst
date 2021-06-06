.. _cr16-function-attributes:

CR16 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the CR16 back end:

.. option:: interrupt

  .. index:: interrupt function attribute, CR16

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

