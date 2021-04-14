.. _translating-statements:

Translating statements
**********************

Translating statements to ``tree`` is done by functions called
``gfc_trans_*``.  These functions usually get passed a
``gfc_code`` structure, evaluate any expressions and then
return a ``tree`` structure.

