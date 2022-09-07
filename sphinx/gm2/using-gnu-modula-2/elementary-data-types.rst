.. _elementary-data-types:

Elementary data types
*********************

This section describes the elementary data types supported by GNU
Modula-2. It also describes the relationship between these data types
and the equivalent C data types.

The following data types are supported: ``INTEGER``,
``LONGINT``, ``SHORTINT``, ``CARDINAL``, ``LONGCARD``,
``SHORTCARD``, ``BOOLEAN``, ``REAL``, ``LONGREAL``,
``SHORTREAL``, ``COMPLEX``, ``LONGCOMPLEX``,
``SHORTCOMPLEX`` and ``CHAR``.

An equivalence table is given below:

.. code-block:: modula2

  GNU Modula-2              GNU C
  ======================================
  INTEGER                   int
  LONGINT                   long long int
  SHORTINT                  short int
  CARDINAL                  unsigned int
  LONGCARD                  long long unsigned int
  SHORTCARD                 short unsigned int
  BOOLEAN                   int
  REAL                      double
  LONGREAL                  long double
  SHORTREAL                 float
  CHAR                      char
  SHORTCOMPLEX              complex float
  COMPLEX                   complex double
  LONGCOMPLEX               complex long double

Note that GNU Modula-2 also supports fixed sized data types which are
exported from the ``SYSTEM`` module.
See :ref:`the-pim-system-module`.
See :ref:`the-iso-system-module`.

