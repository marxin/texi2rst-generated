.. _accessing-declarations:

Accessing declarations
**********************

``gfc_symbol``, ``gfc_charlen`` and other front-end structures
contain a ``backend_decl`` variable, which contains the ``tree``
used for accessing that entity in the middle-end.

Accessing declarations is usually done by functions called
``gfc_get*``.

.. -
   LibGFortran
   -

