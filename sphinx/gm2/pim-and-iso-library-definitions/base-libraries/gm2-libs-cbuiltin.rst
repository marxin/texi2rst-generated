.. _gm2-libs-cbuiltin:

gm2-libs/cbuiltin
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FOR "C" cbuiltin ;

  FROM SYSTEM IMPORT ADDRESS ;
  EXPORT UNQUALIFIED alloca, memcpy,
  		   isfinite, isfinitef, isfinitel,
  		   isinf_sign, isinf_signf, isinf_signl,
                     sinf, sinl, sin,
                     cosf, cosl, cos,
                     atan2f, atan2l, atan2,
                     sqrtf, sqrtl, sqrt,
                     fabsf, fabsl, fabs,
                     logf, logl, log,
                     expf, expl, exp,
                     log10f, log10l, log10,
                     exp10f, exp10l, exp10,
                     ilogbf, ilogbl, ilogb,
                     significand, significandf, significandl,
                     modf, modff, modfl,
                     nextafter, nextafterf, nextafterl,
                     nexttoward, nexttowardf, nexttowardl,
                     scalb, scalbf, scalbl,
                     scalbn, scalbnf, scalbnl,
                     scalbln, scalblnf, scalblnl,

                     cabsf, cabsl, cabs,
                     cargf, carg, cargl,
                     conjf, conj, conjl,
                     cpowf, cpow, cpowl,
                     csqrtf, csqrt, csqrtl,
                     cexpf, cexp, cexpl,
                     clogf, clog, clogl,
                     csinf, csin, csinl,
                     ccosf, ccos, ccosl,
                     ctanf, ctan, ctanl,
                     casinf, casin, casinl,
                     cacosf, cacos, cacosl,
                     catanf, catan, catanl,

                     index, rindex,
                     memcmp, memset, memmove,
                     strcat, strncat, strcpy, strncpy, strcmp, strncmp,
                     strlen, strstr, strpbrk, strspn, strcspn, strchr, strrchr ;

  alloca
  PROCEDURE alloca (i: CARDINAL) : ADDRESS ;
  memcpy
  PROCEDURE memcpy (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
  isfinite
  PROCEDURE isfinite (x: REAL) : BOOLEAN ;
  isfinitel
  PROCEDURE isfinitel (x: LONGREAL) : BOOLEAN ;
  isfinitef
  PROCEDURE isfinitef (x: SHORTREAL) : BOOLEAN ;
  isinf_sign
  PROCEDURE isinf_sign (x: REAL) : BOOLEAN ;
  isinf_signl
  PROCEDURE isinf_signl (x: LONGREAL) : BOOLEAN ;
  isinf_signf
  PROCEDURE isinf_signf (x: SHORTREAL) : BOOLEAN ;
  sinf
  PROCEDURE sinf (x: SHORTREAL) : SHORTREAL ;
  sin
  PROCEDURE sin (x: REAL) : REAL ;
  sinl
  PROCEDURE sinl (x: LONGREAL) : LONGREAL ;
  cosf
  PROCEDURE cosf (x: SHORTREAL) : SHORTREAL ;
  cos
  PROCEDURE cos (x: REAL) : REAL ;
  cosl
  PROCEDURE cosl (x: LONGREAL) : LONGREAL ;
  atan2f
  PROCEDURE atan2f (x, y: SHORTREAL) : SHORTREAL ;
  atan2
  PROCEDURE atan2 (x, y: REAL) : REAL ;
  atan2l
  PROCEDURE atan2l (x, y: LONGREAL) : LONGREAL ;
  sqrtf
  PROCEDURE sqrtf (x: SHORTREAL) : SHORTREAL ;
  sqrt
  PROCEDURE sqrt (x: REAL) : REAL ;
  sqrtl
  PROCEDURE sqrtl (x: LONGREAL) : LONGREAL ;
  fabsf
  PROCEDURE fabsf (x: SHORTREAL) : SHORTREAL ;
  fabs
  PROCEDURE fabs (x: REAL) : REAL ;
  fabsl
  PROCEDURE fabsl (x: LONGREAL) : LONGREAL ;
  logf
  PROCEDURE logf (x: SHORTREAL) : SHORTREAL ;
  log
  PROCEDURE log (x: REAL) : REAL ;
  logl
  PROCEDURE logl (x: LONGREAL) : LONGREAL ;
  expf
  PROCEDURE expf (x: SHORTREAL) : SHORTREAL ;
  exp
  PROCEDURE exp (x: REAL) : REAL ;
  expl
  PROCEDURE expl (x: LONGREAL) : LONGREAL ;
  log10f
  PROCEDURE log10f (x: SHORTREAL) : SHORTREAL ;
  log10
  PROCEDURE log10 (x: REAL) : REAL ;
  log10l
  PROCEDURE log10l (x: LONGREAL) : LONGREAL ;
  exp10f
  PROCEDURE exp10f (x: SHORTREAL) : SHORTREAL ;
  exp10
  PROCEDURE exp10 (x: REAL) : REAL ;
  exp10l
  PROCEDURE exp10l (x: LONGREAL) : LONGREAL ;
  ilogbf
  PROCEDURE ilogbf (x: SHORTREAL) : INTEGER ;
  ilogb
  PROCEDURE ilogb (x: REAL) : INTEGER ;
  ilogbl
  PROCEDURE ilogbl (x: LONGREAL) : INTEGER ;

  significand
  PROCEDURE significand (r: REAL) : REAL ;
  significandf
  PROCEDURE significandf (s: SHORTREAL) : SHORTREAL ;
  significandl
  PROCEDURE significandl (l: LONGREAL) : LONGREAL ;

  modf
  PROCEDURE modf (x: REAL; VAR y: REAL) : REAL ;
  modff
  PROCEDURE modff (x: SHORTREAL; VAR y: SHORTREAL) : SHORTREAL ;
  modfl
  PROCEDURE modfl (x: LONGREAL; VAR y: LONGREAL) : LONGREAL ;

  nextafter
  PROCEDURE nextafter (x, y: REAL) : REAL ;
  nextafterf
  PROCEDURE nextafterf (x, y: SHORTREAL) : SHORTREAL ;
  nextafterl
  PROCEDURE nextafterl (x, y: LONGREAL) : LONGREAL ;

  nexttoward
  PROCEDURE nexttoward (x, y: REAL) : REAL ;
  nexttowardf
  PROCEDURE nexttowardf (x, y: SHORTREAL) : SHORTREAL ;
  nexttowardl
  PROCEDURE nexttowardl (x, y: LONGREAL) : LONGREAL ;

  scalb
  PROCEDURE scalb (x, n: REAL) : REAL ;
  scalbf
  PROCEDURE scalbf (x, n: SHORTREAL) : SHORTREAL ;
  scalbl
  PROCEDURE scalbl (x, n: LONGREAL) : LONGREAL ;

  scalbn
  PROCEDURE scalbn (x: REAL; n: INTEGER) : REAL ;
  scalbnf
  PROCEDURE scalbnf (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
  scalbnl
  PROCEDURE scalbnl (x: LONGREAL; n: INTEGER) : LONGREAL ;

  scalbln
  PROCEDURE scalbln (x: REAL; n: LONGINT) : REAL ;
  scalblnf
  PROCEDURE scalblnf (x: SHORTREAL; n: LONGINT) : SHORTREAL ;
  scalblnl
  PROCEDURE scalblnl (x: LONGREAL; n: LONGINT) : LONGREAL ;

  cabsf
  PROCEDURE cabsf (z: SHORTCOMPLEX) : SHORTREAL ;
  cabs
  PROCEDURE cabs (z: COMPLEX) : REAL ;
  cabsl
  PROCEDURE cabsl (z: LONGCOMPLEX) : LONGREAL ;

  cargf
  PROCEDURE cargf (z: SHORTCOMPLEX) : SHORTREAL ;
  carg
  PROCEDURE carg (z: COMPLEX) : REAL ;
  cargl
  PROCEDURE cargl (z: LONGCOMPLEX) : LONGREAL ;

  conjf
  PROCEDURE conjf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  conj
  PROCEDURE conj (z: COMPLEX) : COMPLEX ;
  conjl
  PROCEDURE conjl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  cpowf
  PROCEDURE cpowf (base: SHORTCOMPLEX; exp: SHORTREAL) : SHORTCOMPLEX ;
  cpow
  PROCEDURE cpow (base: COMPLEX; exp: REAL) : COMPLEX ;
  cpowl
  PROCEDURE cpowl (base: LONGCOMPLEX; exp: LONGREAL) : LONGCOMPLEX ;

  csqrtf
  PROCEDURE csqrtf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  csqrt
  PROCEDURE csqrt (z: COMPLEX) : COMPLEX ;
  csqrtl
  PROCEDURE csqrtl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  cexpf
  PROCEDURE cexpf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  cexp
  PROCEDURE cexp (z: COMPLEX) : COMPLEX ;
  cexpl
  PROCEDURE cexpl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  clogf
  PROCEDURE clogf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  clog
  PROCEDURE clog (z: COMPLEX) : COMPLEX ;
  clogl
  PROCEDURE clogl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  csinf
  PROCEDURE csinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  csin
  PROCEDURE csin (z: COMPLEX) : COMPLEX ;
  csinl
  PROCEDURE csinl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  ccosf
  PROCEDURE ccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  ccos
  PROCEDURE ccos (z: COMPLEX) : COMPLEX ;
  ccosl
  PROCEDURE ccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  ctanf
  PROCEDURE ctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  ctan
  PROCEDURE ctan (z: COMPLEX) : COMPLEX ;
  ctanl
  PROCEDURE ctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  casinf
  PROCEDURE casinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  casin
  PROCEDURE casin (z: COMPLEX) : COMPLEX ;
  casinl
  PROCEDURE casinl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  cacosf
  PROCEDURE cacosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  cacos
  PROCEDURE cacos (z: COMPLEX) : COMPLEX ;
  cacosl
  PROCEDURE cacosl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  catanf
  PROCEDURE catanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
  catan
  PROCEDURE catan (z: COMPLEX) : COMPLEX ;
  catanl
  PROCEDURE catanl (z: LONGCOMPLEX) : LONGCOMPLEX ;

  index
  PROCEDURE index (s: ADDRESS; c: INTEGER) : ADDRESS ;
  rindex
  PROCEDURE rindex (s: ADDRESS; c: INTEGER) : ADDRESS ;
  memcmp
  PROCEDURE memcmp (s1, s2: ADDRESS; n: CARDINAL) : INTEGER ;
  memmove
  PROCEDURE memmove (s1, s2: ADDRESS; n: CARDINAL) : ADDRESS ;
  memset
  PROCEDURE memset (s: ADDRESS; c: INTEGER; n: CARDINAL) : ADDRESS ;
  strcat
  PROCEDURE strcat (dest, src: ADDRESS) : ADDRESS ;
  strncat
  PROCEDURE strncat (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
  strcpy
  PROCEDURE strcpy (dest, src: ADDRESS) : ADDRESS ;
  strncpy
  PROCEDURE strncpy (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
  strcmp
  PROCEDURE strcmp (s1, s2: ADDRESS) : INTEGER ;
  strncmp
  PROCEDURE strncmp (s1, s2: ADDRESS; n: CARDINAL) : INTEGER ;
  strlen
  PROCEDURE strlen (s: ADDRESS) : INTEGER ;
  strstr
  PROCEDURE strstr (haystack, needle: ADDRESS) : ADDRESS ;
  strpbrk
  PROCEDURE strpbrk (s, accept: ADDRESS) : ADDRESS ;
  strspn
  PROCEDURE strspn (s, accept: ADDRESS) : CARDINAL ;
  strcspn
  PROCEDURE strcspn (s, accept: ADDRESS) : CARDINAL ;
  strchr
  PROCEDURE strchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
  strrchr
  PROCEDURE strrchr (s: ADDRESS; c: INTEGER) : ADDRESS ;

  END cbuiltin.

