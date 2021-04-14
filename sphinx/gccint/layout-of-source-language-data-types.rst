.. _type-layout:

Layout of Source Language Data Types
************************************

These macros define the sizes and other characteristics of the standard
basic data types used in programs being compiled.  Unlike the macros in
the previous section, these apply to specific features of C and related
languages, rather than to fundamental aspects of storage layout.

.. index:: INT_TYPE_SIZE

MacroINT_TYPE_SIZEA C expression for the size in bits of the type ``int`` on the
target machine.  If you don't define this, the default is one word.

.. index:: SHORT_TYPE_SIZE

MacroSHORT_TYPE_SIZEA C expression for the size in bits of the type ``short`` on the
target machine.  If you don't define this, the default is half a word.
(If this would be less than one storage unit, it is rounded up to one
unit.)

.. index:: LONG_TYPE_SIZE

MacroLONG_TYPE_SIZEA C expression for the size in bits of the type ``long`` on the
target machine.  If you don't define this, the default is one word.

.. index:: ADA_LONG_TYPE_SIZE

MacroADA_LONG_TYPE_SIZEOn some machines, the size used for the Ada equivalent of the type
``long`` by a native Ada compiler differs from that used by C.  In
that situation, define this macro to be a C expression to be used for
the size of that type.  If you don't define this, the default is the
value of ``LONG_TYPE_SIZE``.

.. index:: LONG_LONG_TYPE_SIZE

MacroLONG_LONG_TYPE_SIZEA C expression for the size in bits of the type ``long long`` on the
target machine.  If you don't define this, the default is two
words.  If you want to support GNU Ada on your machine, the value of this
macro must be at least 64.

.. index:: CHAR_TYPE_SIZE

MacroCHAR_TYPE_SIZEA C expression for the size in bits of the type ``char`` on the
target machine.  If you don't define this, the default is
``BITS_PER_UNIT``.

.. index:: BOOL_TYPE_SIZE

MacroBOOL_TYPE_SIZEA C expression for the size in bits of the C++ type ``bool`` and
C99 type ``_Bool`` on the target machine.  If you don't define
this, and you probably shouldn't, the default is ``CHAR_TYPE_SIZE``.

.. index:: FLOAT_TYPE_SIZE

MacroFLOAT_TYPE_SIZEA C expression for the size in bits of the type ``float`` on the
target machine.  If you don't define this, the default is one word.

.. index:: DOUBLE_TYPE_SIZE

MacroDOUBLE_TYPE_SIZEA C expression for the size in bits of the type ``double`` on the
target machine.  If you don't define this, the default is two
words.

.. index:: LONG_DOUBLE_TYPE_SIZE

MacroLONG_DOUBLE_TYPE_SIZEA C expression for the size in bits of the type ``long double`` on
the target machine.  If you don't define this, the default is two
words.

.. index:: SHORT_FRACT_TYPE_SIZE

MacroSHORT_FRACT_TYPE_SIZEA C expression for the size in bits of the type ``short _Fract`` on
the target machine.  If you don't define this, the default is
``BITS_PER_UNIT``.

.. index:: FRACT_TYPE_SIZE

MacroFRACT_TYPE_SIZEA C expression for the size in bits of the type ``_Fract`` on
the target machine.  If you don't define this, the default is
``BITS_PER_UNIT * 2``.

.. index:: LONG_FRACT_TYPE_SIZE

MacroLONG_FRACT_TYPE_SIZEA C expression for the size in bits of the type ``long _Fract`` on
the target machine.  If you don't define this, the default is
``BITS_PER_UNIT * 4``.

.. index:: LONG_LONG_FRACT_TYPE_SIZE

MacroLONG_LONG_FRACT_TYPE_SIZEA C expression for the size in bits of the type ``long long _Fract`` on
the target machine.  If you don't define this, the default is
``BITS_PER_UNIT * 8``.

.. index:: SHORT_ACCUM_TYPE_SIZE

MacroSHORT_ACCUM_TYPE_SIZEA C expression for the size in bits of the type ``short _Accum`` on
the target machine.  If you don't define this, the default is
``BITS_PER_UNIT * 2``.

.. index:: ACCUM_TYPE_SIZE

MacroACCUM_TYPE_SIZEA C expression for the size in bits of the type ``_Accum`` on
the target machine.  If you don't define this, the default is
``BITS_PER_UNIT * 4``.

.. index:: LONG_ACCUM_TYPE_SIZE

MacroLONG_ACCUM_TYPE_SIZEA C expression for the size in bits of the type ``long _Accum`` on
the target machine.  If you don't define this, the default is
``BITS_PER_UNIT * 8``.

.. index:: LONG_LONG_ACCUM_TYPE_SIZE

MacroLONG_LONG_ACCUM_TYPE_SIZEA C expression for the size in bits of the type ``long long _Accum`` on
the target machine.  If you don't define this, the default is
``BITS_PER_UNIT * 16``.

.. index:: LIBGCC2_GNU_PREFIX

MacroLIBGCC2_GNU_PREFIXThis macro corresponds to the ``TARGET_LIBFUNC_GNU_PREFIX`` target
hook and should be defined if that hook is overriden to be true.  It
causes function names in libgcc to be changed to use a ``__gnu_``
prefix for their name rather than the default ``__``.  A port which
uses this macro should also arrange to use t-gnu-prefix in
the libgcc config.host.

.. index:: WIDEST_HARDWARE_FP_SIZE

MacroWIDEST_HARDWARE_FP_SIZEA C expression for the size in bits of the widest floating-point format
supported by the hardware.  If you define this macro, you must specify a
value less than or equal to the value of ``LONG_DOUBLE_TYPE_SIZE``.
If you do not define this macro, the value of ``LONG_DOUBLE_TYPE_SIZE``
is the default.

.. index:: DEFAULT_SIGNED_CHAR

MacroDEFAULT_SIGNED_CHARAn expression whose value is 1 or 0, according to whether the type
``char`` should be signed or unsigned by default.  The user can
always override this default with the options :option:`-fsigned-char`
and :option:`-funsigned-char`.

.. function:: bool TARGET_DEFAULT_SHORT_ENUMS(void )

  This target hook should return true if the compiler should give an
  ``enum`` type only as many bytes as it takes to represent the range
  of possible values of that type.  It should return false if all
  ``enum`` types should be allocated like ``int``.

  The default is to return false.

.. index:: SIZE_TYPE

MacroSIZE_TYPEA C expression for a string describing the name of the data type to use
for size values.  The typedef name ``size_t`` is defined using the
contents of the string.

The string can contain more than one keyword.  If so, separate them with
spaces, and write first any length keyword, then ``unsigned`` if
appropriate, and finally ``int``.  The string must exactly match one
of the data type names defined in the function
``c_common_nodes_and_builtins`` in the file c-family/c-common.c.
You may not omit ``int`` or change the order-that would cause the
compiler to crash on startup.

If you don't define this macro, the default is ``"long unsigned
int"``.

.. index:: SIZETYPE

MacroSIZETYPEGCC defines internal types (``sizetype``, ``ssizetype``,
``bitsizetype`` and ``sbitsizetype``) for expressions
dealing with size.  This macro is a C expression for a string describing
the name of the data type from which the precision of ``sizetype``
is extracted.

The string has the same restrictions as ``SIZE_TYPE`` string.

If you don't define this macro, the default is ``SIZE_TYPE``.

.. index:: PTRDIFF_TYPE

MacroPTRDIFF_TYPEA C expression for a string describing the name of the data type to use
for the result of subtracting two pointers.  The typedef name
``ptrdiff_t`` is defined using the contents of the string.  See
``SIZE_TYPE`` above for more information.

If you don't define this macro, the default is ``"long int"``.

.. index:: WCHAR_TYPE

MacroWCHAR_TYPEA C expression for a string describing the name of the data type to use
for wide characters.  The typedef name ``wchar_t`` is defined using
the contents of the string.  See ``SIZE_TYPE`` above for more
information.

If you don't define this macro, the default is ``"int"``.

.. index:: WCHAR_TYPE_SIZE

MacroWCHAR_TYPE_SIZEA C expression for the size in bits of the data type for wide
characters.  This is used in ``cpp``, which cannot make use of
``WCHAR_TYPE``.

.. index:: WINT_TYPE

MacroWINT_TYPEA C expression for a string describing the name of the data type to
use for wide characters passed to ``printf`` and returned from
``getwc``.  The typedef name ``wint_t`` is defined using the
contents of the string.  See ``SIZE_TYPE`` above for more
information.

If you don't define this macro, the default is ``"unsigned int"``.

.. index:: INTMAX_TYPE

MacroINTMAX_TYPEA C expression for a string describing the name of the data type that
can represent any value of any standard or extended signed integer type.
The typedef name ``intmax_t`` is defined using the contents of the
string.  See ``SIZE_TYPE`` above for more information.

If you don't define this macro, the default is the first of
``"int"``, ``"long int"``, or ``"long long int"`` that has as
much precision as ``long long int``.

.. index:: UINTMAX_TYPE

MacroUINTMAX_TYPEA C expression for a string describing the name of the data type that
can represent any value of any standard or extended unsigned integer
type.  The typedef name ``uintmax_t`` is defined using the contents
of the string.  See ``SIZE_TYPE`` above for more information.

If you don't define this macro, the default is the first of
``"unsigned int"``, ``"long unsigned int"``, or ``"long long
unsigned int"`` that has as much precision as ``long long unsigned
int``.

.. index:: SIG_ATOMIC_TYPE

MacroSIG_ATOMIC_TYPE
.. index:: INT8_TYPE

MacroINT8_TYPE
.. index:: INT16_TYPE

MacroINT16_TYPE
.. index:: INT32_TYPE

MacroINT32_TYPE
.. index:: INT64_TYPE

MacroINT64_TYPE
.. index:: UINT8_TYPE

MacroUINT8_TYPE
.. index:: UINT16_TYPE

MacroUINT16_TYPE
.. index:: UINT32_TYPE

MacroUINT32_TYPE
.. index:: UINT64_TYPE

MacroUINT64_TYPE
.. index:: INT_LEAST8_TYPE

MacroINT_LEAST8_TYPE
.. index:: INT_LEAST16_TYPE

MacroINT_LEAST16_TYPE
.. index:: INT_LEAST32_TYPE

MacroINT_LEAST32_TYPE
.. index:: INT_LEAST64_TYPE

MacroINT_LEAST64_TYPE
.. index:: UINT_LEAST8_TYPE

MacroUINT_LEAST8_TYPE
.. index:: UINT_LEAST16_TYPE

MacroUINT_LEAST16_TYPE
.. index:: UINT_LEAST32_TYPE

MacroUINT_LEAST32_TYPE
.. index:: UINT_LEAST64_TYPE

MacroUINT_LEAST64_TYPE
.. index:: INT_FAST8_TYPE

MacroINT_FAST8_TYPE
.. index:: INT_FAST16_TYPE

MacroINT_FAST16_TYPE
.. index:: INT_FAST32_TYPE

MacroINT_FAST32_TYPE
.. index:: INT_FAST64_TYPE

MacroINT_FAST64_TYPE
.. index:: UINT_FAST8_TYPE

MacroUINT_FAST8_TYPE
.. index:: UINT_FAST16_TYPE

MacroUINT_FAST16_TYPE
.. index:: UINT_FAST32_TYPE

MacroUINT_FAST32_TYPE
.. index:: UINT_FAST64_TYPE

MacroUINT_FAST64_TYPE
.. index:: INTPTR_TYPE

MacroINTPTR_TYPE
.. index:: UINTPTR_TYPE

MacroUINTPTR_TYPEC expressions for the standard types ``sig_atomic_t``,
``int8_t``, ``int16_t``, ``int32_t``, ``int64_t``,
``uint8_t``, ``uint16_t``, ``uint32_t``, ``uint64_t``,
``int_least8_t``, ``int_least16_t``, ``int_least32_t``,
``int_least64_t``, ``uint_least8_t``, ``uint_least16_t``,
``uint_least32_t``, ``uint_least64_t``, ``int_fast8_t``,
``int_fast16_t``, ``int_fast32_t``, ``int_fast64_t``,
``uint_fast8_t``, ``uint_fast16_t``, ``uint_fast32_t``,
``uint_fast64_t``, ``intptr_t``, and ``uintptr_t``.  See
``SIZE_TYPE`` above for more information.

If any of these macros evaluates to a null pointer, the corresponding
type is not supported; if GCC is configured to provide
``<stdint.h>`` in such a case, the header provided may not conform
to C99, depending on the type in question.  The defaults for all of
these macros are null pointers.

.. index:: TARGET_PTRMEMFUNC_VBIT_LOCATION

MacroTARGET_PTRMEMFUNC_VBIT_LOCATIONThe C++ compiler represents a pointer-to-member-function with a struct
that looks like:

.. code-block:: c++

    struct {
      union {
        void (*fn)();
        ptrdiff_t vtable_index;
      };
      ptrdiff_t delta;
    };

The C++ compiler must use one bit to indicate whether the function that
will be called through a pointer-to-member-function is virtual.
Normally, we assume that the low-order bit of a function pointer must
always be zero.  Then, by ensuring that the vtable_index is odd, we can
distinguish which variant of the union is in use.  But, on some
platforms function pointers can be odd, and so this doesn't work.  In
that case, we use the low-order bit of the ``delta`` field, and shift
the remainder of the ``delta`` field to the left.

GCC will automatically make the right selection about where to store
this bit using the ``FUNCTION_BOUNDARY`` setting for your platform.
However, some platforms such as ARM/Thumb have ``FUNCTION_BOUNDARY``
set such that functions always start at even addresses, but the lowest
bit of pointers to functions indicate whether the function at that
address is in ARM or Thumb mode.  If this is the case of your
architecture, you should define this macro to
``ptrmemfunc_vbit_in_delta``.

In general, you should not have to define this macro.  On architectures
in which function addresses are always even, according to
``FUNCTION_BOUNDARY``, GCC will automatically define this macro to
``ptrmemfunc_vbit_in_pfn``.

.. index:: TARGET_VTABLE_USES_DESCRIPTORS

MacroTARGET_VTABLE_USES_DESCRIPTORSNormally, the C++ compiler uses function pointers in vtables.  This
macro allows the target to change to use 'function descriptors'
instead.  Function descriptors are found on targets for whom a
function pointer is actually a small data structure.  Normally the
data structure consists of the actual code address plus a data
pointer to which the function's data is relative.

If vtables are used, the value of this macro should be the number
of words that the function descriptor occupies.

.. index:: TARGET_VTABLE_ENTRY_ALIGN

MacroTARGET_VTABLE_ENTRY_ALIGNBy default, the vtable entries are void pointers, the so the alignment
is the same as pointer alignment.  The value of this macro specifies
the alignment of the vtable entry in bits.  It should be defined only
when special alignment is necessary. */

.. index:: TARGET_VTABLE_DATA_ENTRY_DISTANCE

MacroTARGET_VTABLE_DATA_ENTRY_DISTANCEThere are a few non-descriptor entries in the vtable at offsets below
zero.  If these entries must be padded (say, to preserve the alignment
specified by ``TARGET_VTABLE_ENTRY_ALIGN``), set this to the number
of words in each data entry.

