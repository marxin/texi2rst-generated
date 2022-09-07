.. _built-ins:

Accessing GNU Modula-2 Built-ins
********************************

This section describes the built-in constants and functions defined in
GNU Modula-2.  The following compiler constants can be accessed using
the ``__ATTRIBUTE__`` ``__BUILTIN__`` keywords. These are not
part of the Modula-2 language and they may differ depending upon the
target architecture but they provide a method whereby common
libraries can interface to a different underlying architecture.

The built-in constants are: ``BITS_PER_UNIT``, ``BITS_PER_WORD``,
``BITS_PER_CHAR`` and ``UNITS_PER_WORD``. They are integrated into
GNU Modula-2 by an extension to the ``ConstFactor`` rule:

.. code-block:: modula2

  ConstFactor := ConstQualidentOrSet | Number | ConstString |
                 "(" ConstExpression ")" | "NOT" ConstFactor |
                 ConstAttribute =:

  ConstAttribute := "__ATTRIBUTE__" "__BUILTIN__" "(" "(" Ident ")" ")" =:

Here is an example taken from the ISO library ``SYSTEM.def`` :

.. code-block:: modula2

  CONST
     BITSPERLOC    = __ATTRIBUTE__ __BUILTIN__ ((BITS_PER_UNIT)) ;
     LOCSPERWORD   = __ATTRIBUTE__ __BUILTIN__ ((UNITS_PER_WORD)) ;

Built-in functions are transparent to the end user. All built-in
functions are declared in ``DEFINITION MODULE`` s and are imported
as and when required.  Built-in functions are declared in definition
modules by using the ``__BUILTIN__`` keyword. Here is a section of
the ISO library ``LongMath.def`` which demonstrates this feature.

.. code-block:: modula2

  PROCEDURE __BUILTIN__ sqrt (x: LONGREAL): LONGREAL;
    (* Returns the square root of x *)

This indicates that the function ``sqrt`` will be implemented using
the gcc built-in maths library.  If gcc cannot utilise the built-in
function (for example if the programmer requested the address of
``sqrt``) then code is generated to call the alternative function
implemented in the ``IMPLEMENTATION`` ``MODULE``.

Sometimes a function exported from the ``DEFINITION`` ``MODULE``
will have a different name from the built-in function within gcc. In
such cases the mapping between the GNU Modula-2 function name and the
gcc name is expressed using the keywords ``__ATTRIBUTE__``
``__BUILTIN__`` ``((Ident))``. For example the function
``sqrt`` in ``LongMath.def`` maps onto the gcc built-in function
``sqrtl`` and this is expressed as:

.. code-block:: modula2

  PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((sqrtl)) sqrt
                                      (x: LONGREAL) : LONGREAL;
    (* Returns the positive square root of x *)

The following module ``Builtins.def`` enumerates the list of
built-in functions which can be accessed in GNU Modula-2. It also
serves to define the parameter and return value for each function:

.. code-block:: modula2

  DEFINITION MODULE Builtins ;

  FROM SYSTEM IMPORT ADDRESS ;

  (* floating point intrinsic procedure functions *)

  isfinitef
  PROCEDURE __BUILTIN__ isfinitef (x: SHORTREAL) : BOOLEAN ;
  isfinite
  PROCEDURE __BUILTIN__ isfinite (x: REAL) : BOOLEAN ;
  isfinitel
  PROCEDURE __BUILTIN__ isfinitel (x: LONGREAL) : BOOLEAN ;

  sinf
  PROCEDURE __BUILTIN__ sinf (x: SHORTREAL) : SHORTREAL ;
  sin
  PROCEDURE __BUILTIN__ sin (x: REAL) : REAL ;
  sinl
  PROCEDURE __BUILTIN__ sinl (x: LONGREAL) : LONGREAL ;

  cosf
  PROCEDURE __BUILTIN__ cosf (x: SHORTREAL) : SHORTREAL ;
  cos
  PROCEDURE __BUILTIN__ cos (x: REAL) : REAL ;
  cosl
  PROCEDURE __BUILTIN__ cosl (x: LONGREAL) : LONGREAL ;

  sqrtf
  PROCEDURE __BUILTIN__ sqrtf (x: SHORTREAL) : SHORTREAL ;
  sqrt
  PROCEDURE __BUILTIN__ sqrt (x: REAL) : REAL ;
  sqrtl
  PROCEDURE __BUILTIN__ sqrtl (x: LONGREAL) : LONGREAL ;

  atan2f
  PROCEDURE __BUILTIN__ atan2f (x, y: SHORTREAL) : SHORTREAL ;
  atan2
  PROCEDURE __BUILTIN__ atan2 (x, y: REAL) : REAL ;
  atan2l
  PROCEDURE __BUILTIN__ atan2l (x, y: LONGREAL) : LONGREAL ;

  fabsf
  PROCEDURE __BUILTIN__ fabsf (x: SHORTREAL) : SHORTREAL ;
  fabs
  PROCEDURE __BUILTIN__ fabs (x: REAL) : REAL ;
  fabsl
  PROCEDURE __BUILTIN__ fabsl (x: LONGREAL) : LONGREAL ;

  logf
  PROCEDURE __BUILTIN__ logf (x: SHORTREAL) : SHORTREAL ;
  log
  PROCEDURE __BUILTIN__ log (x: REAL) : REAL ;
  logl
  PROCEDURE __BUILTIN__ logl (x: LONGREAL) : LONGREAL ;

  expf
  PROCEDURE __BUILTIN__ expf (x: SHORTREAL) : SHORTREAL ;
  exp
  PROCEDURE __BUILTIN__ exp (x: REAL) : REAL ;
  expl
  PROCEDURE __BUILTIN__ expl (x: LONGREAL) : LONGREAL ;

  log10f
  PROCEDURE __BUILTIN__ log10f (x: SHORTREAL) : SHORTREAL ;
  log10
  PROCEDURE __BUILTIN__ log10 (x: REAL) : REAL ;
  log10l
  PROCEDURE __BUILTIN__ log10l (x: LONGREAL) : LONGREAL ;

  exp10f
  PROCEDURE __BUILTIN__ exp10f (x: SHORTREAL) : SHORTREAL ;
  exp10
  PROCEDURE __BUILTIN__ exp10 (x: REAL) : REAL ;
  exp10l
  PROCEDURE __BUILTIN__ exp10l (x: LONGREAL) : LONGREAL ;

  ilogbf
  PROCEDURE __BUILTIN__ ilogbf (x: SHORTREAL) : INTEGER ;
  ilogb
  PROCEDURE __BUILTIN__ ilogb (x: REAL) : INTEGER ;
  ilogbl
  PROCEDURE __BUILTIN__ ilogbl (x: LONGREAL) : INTEGER ;

  huge_val
  PROCEDURE __BUILTIN__ huge_val () : REAL ;
  huge_valf
  PROCEDURE __BUILTIN__ huge_valf () : SHORTREAL ;
  huge_vall
  PROCEDURE __BUILTIN__ huge_vall () : LONGREAL ;

  significand
  PROCEDURE __BUILTIN__ significand (r: REAL) : REAL ;
  significandf
  PROCEDURE __BUILTIN__ significandf (s: SHORTREAL) : SHORTREAL ;
  significandl
  PROCEDURE __BUILTIN__ significandl (l: LONGREAL) : LONGREAL ;

  modf
  PROCEDURE __BUILTIN__ modf (x: REAL; VAR y: REAL) : REAL ;
  modff
  PROCEDURE __BUILTIN__ modff (x: SHORTREAL;
                               VAR y: SHORTREAL) : SHORTREAL ;
  modfl
  PROCEDURE __BUILTIN__ modfl (x: LONGREAL; VAR y: LONGREAL) : LONGREAL ;

  signbit
  PROCEDURE __BUILTIN__ signbit (r: REAL) : INTEGER ;
  signbitf
  PROCEDURE __BUILTIN__ signbitf (s: SHORTREAL) : INTEGER ;
  signbitl
  PROCEDURE __BUILTIN__ signbitl (l: LONGREAL) : INTEGER ;

  nextafter
  PROCEDURE __BUILTIN__ nextafter (x, y: REAL) : REAL ;
  nextafterf
  PROCEDURE __BUILTIN__ nextafterf (x, y: SHORTREAL) : SHORTREAL ;
  nextafterl
  PROCEDURE __BUILTIN__ nextafterl (x, y: LONGREAL) : LONGREAL ;

  nexttoward
  PROCEDURE __BUILTIN__ nexttoward (x, y: REAL) : LONGREAL ;
  nexttowardf
  PROCEDURE __BUILTIN__ nexttowardf (x, y: SHORTREAL) : LONGREAL ;
  nexttowardl
  PROCEDURE __BUILTIN__ nexttowardl (x, y: LONGREAL) : LONGREAL ;

  scalb
  PROCEDURE __BUILTIN__ scalb (x, n: REAL) : REAL ;
  scalbf
  PROCEDURE __BUILTIN__ scalbf (x, n: SHORTREAL) : SHORTREAL ;
  scalbl
  PROCEDURE __BUILTIN__ scalbl (x, n: LONGREAL) : LONGREAL ;

  scalbln
  PROCEDURE __BUILTIN__ scalbln (x: REAL; n: LONGINT) : REAL ;
  scalblnf
  PROCEDURE __BUILTIN__ scalblnf (x: SHORTREAL; n: LONGINT) : SHORTREAL ;
  scalblnl
  PROCEDURE __BUILTIN__ scalblnl (x: LONGREAL; n: LONGINT) : LONGREAL ;

  scalbn
  PROCEDURE __BUILTIN__ scalbn (x: REAL; n: INTEGER) : REAL ;
  scalbnf
  PROCEDURE __BUILTIN__ scalbnf (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
  scalbnl
  PROCEDURE __BUILTIN__ scalbnl (x: LONGREAL; n: INTEGER) : LONGREAL ;

  (* complex arithmetic intrincic procedure functions *)

  cabsf
  PROCEDURE __BUILTIN__ cabsf (z: SHORTCOMPLEX) : SHORTREAL ;
  cabs
  PROCEDURE __BUILTIN__ cabs (z: COMPLEX) : REAL ;
  cabsl
  PROCEDURE __BUILTIN__ cabsl (z: LONGCOMPLEX) : LONGREAL ;

  cargf
  PROCEDURE __BUILTIN__ cargf (z: SHORTCOMPLEX) : SHORTREAL ;
  carg
  PROCEDURE __BUILTIN__ carg (z: COMPLEX) : REAL ;
  cargl
  PROCEDURE __BUILTIN__ cargl (z: LONGCOMPLEX) : LONGREAL ;

  conjf
  PROCEDURE __BUILTIN__ conjf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  conj
  PROCEDURE __BUILTIN__ conj (z: COMPLEX) : COMPLEX ;
  conjl
  PROCEDURE __BUILTIN__ conjl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  cpowerf
  PROCEDURE __BUILTIN__ cpowerf (base: SHORTCOMPLEX;
                                 exp: SHORTREAL) : SHORTCOMPLEX ;
  cpower
  PROCEDURE __BUILTIN__ cpower (base: COMPLEX; exp: REAL) : COMPLEX ;
  cpowerl
  PROCEDURE __BUILTIN__ cpowerl (base: LONGCOMPLEX;
                                 exp: LONGREAL) : LONGCOMPLEX ;

  csqrtf
  PROCEDURE __BUILTIN__ csqrtf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  csqrt
  PROCEDURE __BUILTIN__ csqrt (z: COMPLEX) : COMPLEX ;
  csqrtl
  PROCEDURE __BUILTIN__ csqrtl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  cexpf
  PROCEDURE __BUILTIN__ cexpf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  cexp
  PROCEDURE __BUILTIN__ cexp (z: COMPLEX) : COMPLEX ;
  cexpl
  PROCEDURE __BUILTIN__ cexpl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  clnf
  PROCEDURE __BUILTIN__ clnf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  cln
  PROCEDURE __BUILTIN__ cln (z: COMPLEX) : COMPLEX ;
  clnl
  PROCEDURE __BUILTIN__ clnl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  csinf
  PROCEDURE __BUILTIN__ csinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  csin
  PROCEDURE __BUILTIN__ csin (z: COMPLEX) : COMPLEX ;
  csinl
  PROCEDURE __BUILTIN__ csinl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  ccosf
  PROCEDURE __BUILTIN__ ccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  ccos
  PROCEDURE __BUILTIN__ ccos (z: COMPLEX) : COMPLEX ;
  ccosl
  PROCEDURE __BUILTIN__ ccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  ctanf
  PROCEDURE __BUILTIN__ ctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  ctan
  PROCEDURE __BUILTIN__ ctan (z: COMPLEX) : COMPLEX ;
  ctanl
  PROCEDURE __BUILTIN__ ctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  carcsinf
  PROCEDURE __BUILTIN__ carcsinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  carcsin
  PROCEDURE __BUILTIN__ carcsin (z: COMPLEX) : COMPLEX ;
  carcsinl
  PROCEDURE __BUILTIN__ carcsinl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  carccosf
  PROCEDURE __BUILTIN__ carccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  carccos
  PROCEDURE __BUILTIN__ carccos (z: COMPLEX) : COMPLEX ;
  carccosl
  PROCEDURE __BUILTIN__ carccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  carctanf
  PROCEDURE __BUILTIN__ carctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  carctan
  PROCEDURE __BUILTIN__ carctan (z: COMPLEX) : COMPLEX ;
  carctanl
  PROCEDURE __BUILTIN__ carctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  (* memory and string intrincic procedure functions *)

  alloca
  PROCEDURE __BUILTIN__ alloca (i: CARDINAL) : ADDRESS ;
  memcpy
  PROCEDURE __BUILTIN__ memcpy (dest, src: ADDRESS;
                                nbytes: CARDINAL) : ADDRESS ;
  index
  PROCEDURE __BUILTIN__ index (s: ADDRESS; c: INTEGER) : ADDRESS ;
  rindex
  PROCEDURE __BUILTIN__ rindex (s: ADDRESS; c: INTEGER) : ADDRESS ;
  memcmp
  PROCEDURE __BUILTIN__ memcmp (s1, s2: ADDRESS;
                                nbytes: CARDINAL) : INTEGER ;
  memset
  PROCEDURE __BUILTIN__ memset (s: ADDRESS; c: INTEGER;
                                nbytes: CARDINAL) : ADDRESS ;
  memmove
  PROCEDURE __BUILTIN__ memmove (s1, s2: ADDRESS;
                                 nbytes: CARDINAL) : ADDRESS ;
  strcat
  PROCEDURE __BUILTIN__ strcat (dest, src: ADDRESS) : ADDRESS ;
  strncat
  PROCEDURE __BUILTIN__ strncat (dest, src: ADDRESS;
                                 nbytes: CARDINAL) : ADDRESS ;
  strcpy
  PROCEDURE __BUILTIN__ strcpy (dest, src: ADDRESS) : ADDRESS ;
  strncpy
  PROCEDURE __BUILTIN__ strncpy (dest, src: ADDRESS;
                                 nbytes: CARDINAL) : ADDRESS ;
  strcmp
  PROCEDURE __BUILTIN__ strcmp (s1, s2: ADDRESS) : INTEGER ;
  strncmp
  PROCEDURE __BUILTIN__ strncmp (s1, s2: ADDRESS;
                                 nbytes: CARDINAL) : INTEGER ;
  strlen
  PROCEDURE __BUILTIN__ strlen (s: ADDRESS) : INTEGER ;
  strstr
  PROCEDURE __BUILTIN__ strstr (haystack, needle: ADDRESS) : ADDRESS ;
  strpbrk
  PROCEDURE __BUILTIN__ strpbrk (s, accept: ADDRESS) : ADDRESS ;
  strspn
  PROCEDURE __BUILTIN__ strspn (s, accept: ADDRESS) : CARDINAL ;
  strcspn
  PROCEDURE __BUILTIN__ strcspn (s, accept: ADDRESS) : CARDINAL ;
  strchr
  PROCEDURE __BUILTIN__ strchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
  strrchr
  PROCEDURE __BUILTIN__ strrchr (s: ADDRESS; c: INTEGER) : ADDRESS ;

  (*
     longjmp - this GCC builtin restricts the val to always 1.
  *)
  (* do not use these two builtins, as gcc, only really
     anticipates that the Ada front end should use them
     and it only uses them in its runtime exception handling.
     We leave them here in the hope that someday they will
     behave more like their libc counterparts.  *)

  longjmp
  PROCEDURE __BUILTIN__ longjmp (env: ADDRESS; val: INTEGER) ;
  setjmp
  PROCEDURE __BUILTIN__ setjmp (env: ADDRESS) : INTEGER ;

  (*
     frame_address - returns the address of the frame.
                     The current frame is obtained if level is 0,
                     the next level up if level is 1 etc.
  *)

  frame_address
  PROCEDURE __BUILTIN__ frame_address (level: CARDINAL) : ADDRESS ;

  (*
     return_address - returns the return address of function.
                      The current function return address is
                      obtained if level is 0,
                      the next level up if level is 1 etc.
  *)

  return_address
  PROCEDURE __BUILTIN__ return_address (level: CARDINAL) : ADDRESS ;

  (*
     alloca_trace - this is a no-op which is used for internal debugging.
  *)

  alloca_trace
  PROCEDURE alloca_trace (returned: ADDRESS; nBytes: CARDINAL) : ADDRESS ;

  END Builtins.

Although this module exists and will result in the generation of
in-line code if optimization flags are passed to GNU Modula-2, users
are advised to utilize the same functions from more generic libraries.
The built-in mechanism will be applied to these generic
libraries where appropriate. Note for the mathematical routines to
be in-lined you need to specify the :samp:`-ffast-math -O` options.

