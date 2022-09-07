Integer division, remainder and modulus
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The most dangerous set of mutually exclusive features found in the
four dialects supported by GNU Modula-2 are the ``INTEGER``
division, remainder and modulus arithmetic operators.  It is important
to note that the same source code can be compiled to give different
runtime results depending upon these switches!  The reference manual
for the various dialects of Modula-2 are quite clear about this
behaviour and sadly there are three distinct definitions.

The table below illustrates the problem when a negative operand is
used.

.. code-block:: modula2

                    Pim2/3          Pim4                ISO
                 -----------    -----------    ----------------------
  lval    rval   DIV     MOD    DIV     MOD    DIV    MOD    /    REM
   31      10      3       1      3       1      3      1     3     1
  -31      10     -3      -1     -4       9     -4      9    -3    -1
   31     -10     -3       1     -3       1     Exception    -3     1
  -31     -10      3      -1      4       9     Exception     3    -1

See also P24 of PIM2, P27 of PIM3, P29 of PIM4 and P201 of the ISO
Standard.  At present all dialect division, remainder and modulus are
implemented as above, apart from the exception calling in the ISO
dialect. Instead of exception handling the results are the same as the
PIM4 dialect. This is a temporary implementation situation.

