..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _integer-overflow-builtins:

Built-in Functions to Perform Arithmetic with Overflow Checking
***************************************************************

The following built-in functions allow performing simple arithmetic operations
together with checking whether the operations overflowed.

.. function:: bool __builtin_add_overflow (type1 a, type2 b, type3 *res)

  These built-in functions promote the first two operands into infinite precision signed
  type and perform addition on those promoted operands.  The result is then
  cast to the type the third pointer argument points to and stored there.
  If the stored result is equal to the infinite precision result, the built-in
  functions return ``false``, otherwise they return ``true``.  As the addition is
  performed in infinite signed precision, these built-in functions have fully defined
  behavior for all argument values.

  The first built-in function allows arbitrary integral types for operands and
  the result type must be pointer to some integral type other than enumerated or
  boolean type, the rest of the built-in functions have explicit integer types.

  The compiler will attempt to use hardware instructions to implement
  these built-in functions where possible, like conditional jump on overflow
  after addition, conditional jump on carry etc.

.. function:: bool __builtin_sub_overflow (type1 a, type2 b, type3 *res)

  These built-in functions are similar to the add overflow checking built-in
  functions above, except they perform subtraction, subtract the second argument
  from the first one, instead of addition.

.. function:: bool __builtin_mul_overflow (type1 a, type2 b, type3 *res)

  These built-in functions are similar to the add overflow checking built-in
  functions above, except they perform multiplication, instead of addition.

The following built-in functions allow checking if simple arithmetic operation
would overflow.

.. function:: bool __builtin_add_overflow_p (type1 a, type2 b, type3 c)

  These built-in functions are similar to ``__builtin_add_overflow``,
  ``__builtin_sub_overflow``, or ``__builtin_mul_overflow``, except that
  they don't store the result of the arithmetic operation anywhere and the
  last argument is not a pointer, but some expression with integral type other
  than enumerated or boolean type.

  The built-in functions promote the first two operands into infinite precision signed type
  and perform addition on those promoted operands. The result is then
  cast to the type of the third argument.  If the cast result is equal to the infinite
  precision result, the built-in functions return ``false``, otherwise they return ``true``.
  The value of the third argument is ignored, just the side effects in the third argument
  are evaluated, and no integral argument promotions are performed on the last argument.
  If the third argument is a bit-field, the type used for the result cast has the
  precision and signedness of the given bit-field, rather than precision and signedness
  of the underlying type.

  For example, the following macro can be used to portably check, at
  compile-time, whether or not adding two constant integers will overflow,
  and perform the addition only when it is known to be safe and not to trigger
  a :option:`-Woverflow` warning.

  .. code-block:: c++

    #define INT_ADD_OVERFLOW_P(a, b) \
       __builtin_add_overflow_p (a, b, (__typeof__ ((a) + (b))) 0)

    enum {
        A = INT_MAX, B = 3,
        C = INT_ADD_OVERFLOW_P (A, B) ? 0 : A + B,
        D = __builtin_add_overflow_p (1, SCHAR_MAX, (signed char) 0)
    };

  The compiler will attempt to use hardware instructions to implement
  these built-in functions where possible, like conditional jump on overflow
  after addition, conditional jump on carry etc.

