Parameter compatibility
^^^^^^^^^^^^^^^^^^^^^^^

Parameter compatibility is divided into two areas, pass by value and
pass by reference (``VAR``).  In the case of pass by value the
rules are exactly the same as assignment.  However in the second case,
pass by reference, the actual parameter and formal parameter must be
the same size and family.  Furthermore ``INTEGER`` and
``CARDINAL`` s are not treated as compatible in the pass by
reference case.

The types ``BYTE``, ``LOC``, ``WORD`` and ``WORD`` n
derivitives are assignment and parameter compatible with any data type
of the same size.

