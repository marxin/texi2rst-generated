.. _target-structure:

The Global ``targetm`` Variable
*******************************

.. index:: target hooks

.. index:: target functions

.. index:: targetm

Variable struct gcc_target targetmThe target .c file must define the global ``targetm`` variable
which contains pointers to functions and data relating to the target
machine.  The variable is declared in target.h;
target-def.h defines the macro ``TARGET_INITIALIZER`` which is
used to initialize the variable, and macros for the default initializers
for elements of the structure.  The .c file should override those
macros for which the default definition is inappropriate.  For example:

.. code-block:: c++

  #include "target.h"
  #include "target-def.h"

  /* Initialize the GCC target structure.  */

  #undef TARGET_COMP_TYPE_ATTRIBUTES
  #define TARGET_COMP_TYPE_ATTRIBUTES machine_comp_type_attributes

  struct gcc_target targetm = TARGET_INITIALIZER;

Where a macro should be defined in the .c file in this manner to
form part of the ``targetm`` structure, it is documented below as a
'Target Hook' with a prototype.  Many macros will change in future
from being defined in the .h file to being part of the
``targetm`` structure.

Similarly, there is a ``targetcm`` variable for hooks that are
specific to front ends for C-family languages, documented as 'C
Target Hook'.  This is declared in c-family/c-target.h, the
initializer ``TARGETCM_INITIALIZER`` in
c-family/c-target-def.h.  If targets initialize ``targetcm``
themselves, they should set ``target_has_targetcm=yes`` in
config.gcc; otherwise a default definition is used.

Similarly, there is a ``targetm_common`` variable for hooks that
are shared between the compiler driver and the compilers proper,
documented as 'Common Target Hook'.  This is declared in
common/common-target.h, the initializer
``TARGETM_COMMON_INITIALIZER`` in
common/common-target-def.h.  If targets initialize
``targetm_common`` themselves, they should set
``target_has_targetm_common=yes`` in config.gcc; otherwise a
default definition is used.

Similarly, there is a ``targetdm`` variable for hooks that are
specific to the D language front end, documented as 'D Target Hook'.
This is declared in d/d-target.h, the initializer
``TARGETDM_INITIALIZER`` in d/d-target-def.h.  If targets
initialize ``targetdm`` themselves, they should set
``target_has_targetdm=yes`` in config.gcc; otherwise a default
definition is used.

