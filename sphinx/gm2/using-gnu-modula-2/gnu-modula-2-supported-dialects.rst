.. _dialect:

GNU Modula-2 supported dialects
*******************************

This section describes the dialects understood by GNU Modula-2.
It also describes the differences between the dialects and
any command line switches which determine dialect behaviour.

The GNU Modula-2 compiler is compliant with four dialects of Modula-2.
The language as defined in 'Programming in Modula-2' 2nd Edition,
Springer Verlag, 1982, 1983 by Niklaus Wirth (PIM2), 'Programming in
Modula-2', 3rd Corrected Edition, Springer Verlag, 1985 (PIM3) and
'Programming in Modula-2', 4th Edition, Springer Verlag, 1988 (PIM4)
http://freepages.modula2.org/report4/modula-2.html and the ISO
Modula-2 language as defined in ISO/IEC Information technology -
programming languages - part 1: Modula-2 Language, ISO/IEC 10514-1
(1996) (ISO).

The command line switches :samp:`-fpim2`, :samp:`-fpim3`, :samp:`-fpim4`
and :samp:`-fiso` can be used to force mutually exclusive
features.  However by default the compiler will not aggressively fail
if a non mutually exclusive feature is used from another dialect.  For
example it is possible to specify :samp:`-fpim2` and still utilise
:samp:`DEFINITION` :samp:`MODULES` which have no export list.

Some dialect differences will force a compile time error, for example
in PIM2 the user must ``IMPORT`` ``SIZE`` from the module
``SYSTEM``, whereas in PIM3 and PIM4 ``SIZE`` is a pervasive
function.  Thus compiling PIM4 source code with the :samp:`-fpim2`
switch will cause a compile time error.  This can be fixed quickly
with an additional ``IMPORT`` or alternatively by compiling with
the :samp:`-fpim4` switch.

However there are some very important differences between the dialects
which are mutually exclusive and therefore it is vital that users
choose the dialects with care when these language features are used.

.. toctree::
  :maxdepth: 2

  integer-division-remainder-and-modulus

