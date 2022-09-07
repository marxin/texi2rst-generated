Expression compatibility
^^^^^^^^^^^^^^^^^^^^^^^^

Modula-2 restricts the types of expressions to the same type.
Expression compatibility is a symmetric relation.

For example two sub expressions of ``INTEGER`` and ``CARDINAL``
are not expression compatible
(http://freepages.modula2.org/report4/modula-2.html and ISO
Modula-2).

In GNU Modula-2 this rule is also extended across all fixed sized data
types (imported from SYSTEM).

