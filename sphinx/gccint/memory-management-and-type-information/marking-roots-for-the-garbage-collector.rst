.. _ggc-roots:

Marking Roots for the Garbage Collector
***************************************

.. index:: roots, marking

.. index:: marking roots

In addition to keeping track of types, the type machinery also locates
the global variables (:dfn:`roots`) that the garbage collector starts
at.  Roots must be declared using one of the following syntaxes:

* ``extern GTY(([options])) typename;``

* ``static GTY(([options])) typename;``

The syntax

* ``GTY(([options])) typename;``

is *not* accepted.  There should be an ``extern`` declaration
of such a variable in a header somewhere-mark that, not the
definition.  Or, if the variable is only used in one file, make it
``static``.
