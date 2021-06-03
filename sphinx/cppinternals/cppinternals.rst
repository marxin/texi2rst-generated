.. @smallbook
   @cropmarks
   @finalout

.. _top:

.. toctree::
  :maxdepth: 2

  cpplib-the-gnu-c-preprocessor

.. _conventions:

Conventions
===========

.. index:: interface

.. index:: header files

cpplib has two interfaces---one is exposed internally only, and the
other is for both internal and external use.

The convention is that functions and types that are exposed to multiple
files internally are prefixed with :samp:`_cpp_`, and are to be found in
the file internal.h.  Functions and types exposed to external
clients are in cpplib.h, and prefixed with :samp:`cpp_`.  For
historical reasons this is no longer quite true, but we should strive to
stick to it.

We are striving to reduce the information exposed in cpplib.h to the
bare minimum necessary, and then to keep it there.  This makes clear
exactly what external clients are entitled to assume, and allows us to
change internals in the future without worrying whether library clients
are perhaps relying on some kind of undocumented implementation-specific
behavior.

.. _lexer:

The Lexer
=========

.. index:: lexer

.. index:: newlines

.. index:: escaped newlines

.. toctree::
  :maxdepth: 2

  overview
  lexing-a-token
  lexing-a-line

.. _hash-nodes:

Hash Nodes
==========

.. index:: hash table

.. index:: identifiers

.. index:: macros

.. index:: assertions

.. index:: named operators

When cpplib encounters an 'identifier', it generates a hash code for
it and stores it in the hash table.  By 'identifier' we mean tokens
with type ``CPP_NAME`` ; this includes identifiers in the usual C
sense, as well as keywords, directive names, macro names and so on.  For
example, all of ``pragma``, ``int``, ``foo`` and
``__GNUC__`` are identifiers and hashed when lexed.

Each node in the hash table contain various information about the
identifier it represents.  For example, its length and type.  At any one
time, each identifier falls into exactly one of three categories:

* Macros

  These have been declared to be macros, either on the command line or
  with ``#define``.  A few, such as ``__TIME__`` are built-ins
  entered in the hash table during initialization.  The hash node for a
  normal macro points to a structure with more information about the
  macro, such as whether it is function-like, how many arguments it takes,
  and its expansion.  Built-in macros are flagged as special, and instead
  contain an enum indicating which of the various built-in macros it is.

* Assertions

  Assertions are in a separate namespace to macros.  To enforce this, cpp
  actually prepends a ``#`` character before hashing and entering it in
  the hash table.  An assertion's node points to a chain of answers to
  that assertion.

* Void

  Everything else falls into this category---an identifier that is not
  currently a macro, or a macro that has since been undefined with
  ``#undef``.

  When preprocessing C++, this category also includes the named operators,
  such as ``xor``.  In expressions these behave like the operators they
  represent, but in contexts where the spelling of a token matters they
  are spelt differently.  This spelling distinction is relevant when they
  are operands of the stringizing and pasting macro operators ``#`` and
  ``##``.  Named operator hash nodes are flagged, both to catch the
  spelling distinction and to prevent them from being defined as macros.

The same identifiers share the same hash node.  Since each identifier
token, after lexing, contains a pointer to its hash node, this is used
to provide rapid lookup of various information.  For example, when
parsing a ``#define`` statement, CPP flags each argument's identifier
hash node with the index of that argument.  This makes duplicated
argument checking an O(1) operation for each argument.  Similarly, for
each identifier in the macro's expansion, lookup to see if it is an
argument, and which argument it is, is also an O(1) operation.  Further,
each directive name, such as ``endif``, has an associated directive
enum stored in its hash node, so that directive lookup is also O(1).

.. _macro-expansion:

Macro Expansion Algorithm
=========================

.. index:: macro expansion

Macro expansion is a tricky operation, fraught with nasty corner cases
and situations that render what you thought was a nifty way to
optimize the preprocessor's expansion algorithm wrong in quite subtle
ways.

I strongly recommend you have a good grasp of how the C and C++
standards require macros to be expanded before diving into this
section, let alone the code!.  If you don't have a clear mental
picture of how things like nested macro expansion, stringizing and
token pasting are supposed to work, damage to your sanity can quickly
result.

.. toctree::
  :maxdepth: 2

  internal-representation-of-macros
  macro-expansion-overview
  scanning-the-replacement-list-for-macros-to-expand
  looking-for-a-function-like-macros-opening-parenthesis
  marking-tokens-ineligible-for-future-expansion

.. _token-spacing:

Token Spacing
=============

.. index:: paste avoidance

.. index:: spacing

.. index:: token spacing

First, consider an issue that only concerns the stand-alone
preprocessor: there needs to be a guarantee that re-reading its preprocessed
output results in an identical token stream.  Without taking special
measures, this might not be the case because of macro substitution.
For example:

.. code-block:: c++

  #define PLUS +
  #define EMPTY
  #define f(x) =x=
  +PLUS -EMPTY- PLUS+ f(=)
          → + + - - + + = = =
  not
          → ++ -- ++ ===

One solution would be to simply insert a space between all adjacent
tokens.  However, we would like to keep space insertion to a minimum,
both for aesthetic reasons and because it causes problems for people who
still try to abuse the preprocessor for things like Fortran source and
Makefiles.

For now, just notice that when tokens are added (or removed, as shown by
the ``EMPTY`` example) from the original lexed token stream, we need
to check for accidental token pasting.  We call this :dfn:`paste
avoidance`.  Token addition and removal can only occur because of macro
expansion, but accidental pasting can occur in many places: both before
and after each macro replacement, each argument replacement, and
additionally each token created by the :samp:`#` and :samp:`##` operators.

Look at how the preprocessor gets whitespace output correct
normally.  The ``cpp_token`` structure contains a flags byte, and one
of those flags is ``PREV_WHITE``.  This is flagged by the lexer, and
indicates that the token was preceded by whitespace of some form other
than a new line.  The stand-alone preprocessor can use this flag to
decide whether to insert a space between tokens in the output.

Now consider the result of the following macro expansion:

.. code-block:: c++

  #define add(x, y, z) x + y +z;
  sum = add (1,2, 3);
          → sum = 1 + 2 +3;

The interesting thing here is that the tokens :samp:`1` and :samp:`2` are
output with a preceding space, and :samp:`3` is output without a
preceding space, but when lexed none of these tokens had that property.
Careful consideration reveals that :samp:`1` gets its preceding
whitespace from the space preceding :samp:`add` in the macro invocation,
*not* replacement list.  :samp:`2` gets its whitespace from the
space preceding the parameter :samp:`y` in the macro replacement list,
and :samp:`3` has no preceding space because parameter :samp:`z` has none
in the replacement list.

Once lexed, tokens are effectively fixed and cannot be altered, since
pointers to them might be held in many places, in particular by
in-progress macro expansions.  So instead of modifying the two tokens
above, the preprocessor inserts a special token, which I call a
:dfn:`padding token`, into the token stream to indicate that spacing of
the subsequent token is special.  The preprocessor inserts padding
tokens in front of every macro expansion and expanded macro argument.
These point to a :dfn:`source token` from which the subsequent real token
should inherit its spacing.  In the above example, the source tokens are
:samp:`add` in the macro invocation, and :samp:`y` and :samp:`z` in the
macro replacement list, respectively.

It is quite easy to get multiple padding tokens in a row, for example if
a macro's first replacement token expands straight into another macro.

.. code-block:: c++

  #define foo bar
  #define bar baz
  [foo]
          → [baz]

Here, two padding tokens are generated with sources the :samp:`foo` token
between the brackets, and the :samp:`bar` token from foo's replacement
list, respectively.  Clearly the first padding token is the one to
use, so the output code should contain a rule that the first
padding token in a sequence is the one that matters.

But what if a macro expansion is left?  Adjusting the above
example slightly:

.. code-block:: c++

  #define foo bar
  #define bar EMPTY baz
  #define EMPTY
  [foo] EMPTY;
          → [ baz] ;

As shown, now there should be a space before :samp:`baz` and the
semicolon in the output.

The rules we decided above fail for :samp:`baz`: we generate three
padding tokens, one per macro invocation, before the token :samp:`baz`.
We would then have it take its spacing from the first of these, which
carries source token :samp:`foo` with no leading space.

It is vital that cpplib get spacing correct in these examples since any
of these macro expansions could be stringized, where spacing matters.

So, this demonstrates that not just entering macro and argument
expansions, but leaving them requires special handling too.  I made
cpplib insert a padding token with a ``NULL`` source token when
leaving macro expansions, as well as after each replaced argument in a
macro's replacement list.  It also inserts appropriate padding tokens on
either side of tokens created by the :samp:`#` and :samp:`##` operators.
I expanded the rule so that, if we see a padding token with a
``NULL`` source token, *and* that source token has no leading
space, then we behave as if we have seen no padding tokens at all.  A
quick check shows this rule will then get the above example correct as
well.

Now a relationship with paste avoidance is apparent: we have to be
careful about paste avoidance in exactly the same locations we have
padding tokens in order to get white space correct.  This makes
implementation of paste avoidance easy: wherever the stand-alone
preprocessor is fixing up spacing because of padding tokens, and it
turns out that no space is needed, it has to take the extra step to
check that a space is not needed after all to avoid an accidental paste.
The function ``cpp_avoid_paste`` advises whether a space is required
between two consecutive tokens.  To avoid excessive spacing, it tries
hard to only require a space if one is likely to be necessary, but for
reasons of efficiency it is slightly conservative and might recommend a
space where one is not strictly needed.

.. _line-numbering:

Line numbering
==============

.. index:: line numbers

.. toctree::
  :maxdepth: 2

  just-which-line-number-anyway
  representation-of-line-numbers

.. _guard-macros:

The Multiple-Include Optimization
=================================

.. index:: guard macros

.. index:: controlling macros

.. index:: multiple-include optimization

Header files are often of the form

.. code-block:: c++

  #ifndef FOO
  #define FOO
  ...
  #endif

to prevent the compiler from processing them more than once.  The
preprocessor notices such header files, so that if the header file
appears in a subsequent ``#include`` directive and ``FOO`` is
defined, then it is ignored and it doesn't preprocess or even re-open
the file a second time.  This is referred to as the :dfn:`multiple
include optimization`.

Under what circumstances is such an optimization valid?  If the file
were included a second time, it can only be optimized away if that
inclusion would result in no tokens to return, and no relevant
directives to process.  Therefore the current implementation imposes
requirements and makes some allowances as follows:

* There must be no tokens outside the controlling ``#if`` - ``#endif``
  pair, but whitespace and comments are permitted.

* There must be no directives outside the controlling directive pair, but
  the :dfn:`null directive` (a line containing nothing other than a single
  :samp:`#` and possibly whitespace) is permitted.

* The opening directive must be of the form

  .. code-block:: c++

    #ifndef FOO

  or

  .. code-block:: c++

    #if !defined FOO     [equivalently, #if !defined(FOO)]

* In the second form above, the tokens forming the ``#if`` expression
  must have come directly from the source file---no macro expansion must
  have been involved.  This is because macro definitions can change, and
  tracking whether or not a relevant change has been made is not worth the
  implementation cost.

* There can be no ``#else`` or ``#elif`` directives at the outer
  conditional block level, because they would probably contain something
  of interest to a subsequent pass.

First, when pushing a new file on the buffer stack,
``_stack_include_file`` sets the controlling macro ``mi_cmacro`` to
``NULL``, and sets ``mi_valid`` to ``true``.  This indicates
that the preprocessor has not yet encountered anything that would
invalidate the multiple-include optimization.  As described in the next
few paragraphs, these two variables having these values effectively
indicates top-of-file.

When about to return a token that is not part of a directive,
``_cpp_lex_token`` sets ``mi_valid`` to ``false``.  This
enforces the constraint that tokens outside the controlling conditional
block invalidate the optimization.

The ``do_if``, when appropriate, and ``do_ifndef`` directive
handlers pass the controlling macro to the function
``push_conditional``.  cpplib maintains a stack of nested conditional
blocks, and after processing every opening conditional this function
pushes an ``if_stack`` structure onto the stack.  In this structure
it records the controlling macro for the block, provided there is one
and we're at top-of-file (as described above).  If an ``#elif`` or
``#else`` directive is encountered, the controlling macro for that
block is cleared to ``NULL``.  Otherwise, it survives until the
``#endif`` closing the block, upon which ``do_endif`` sets
``mi_valid`` to true and stores the controlling macro in
``mi_cmacro``.

``_cpp_handle_directive`` clears ``mi_valid`` when processing any
directive other than an opening conditional and the null directive.
With this, and requiring top-of-file to record a controlling macro, and
no ``#else`` or ``#elif`` for it to survive and be copied to
``mi_cmacro`` by ``do_endif``, we have enforced the absence of
directives outside the main conditional block for the optimization to be
on.

Note that whilst we are inside the conditional block, ``mi_valid`` is
likely to be reset to ``false``, but this does not matter since
the closing ``#endif`` restores it to ``true`` if appropriate.

Finally, since ``_cpp_lex_direct`` pops the file off the buffer stack
at ``EOF`` without returning a token, if the ``#endif`` directive
was not followed by any tokens, ``mi_valid`` is ``true`` and
``_cpp_pop_file_buffer`` remembers the controlling macro associated
with the file.  Subsequent calls to ``stack_include_file`` result in
no buffer being pushed if the controlling macro is defined, effecting
the optimization.

A quick word on how we handle the

.. code-block:: c++

  #if !defined FOO

case.  ``_cpp_parse_expr`` and ``parse_defined`` take steps to see
whether the three stages :samp:`!`, :samp:`defined-expression` and
:samp:`end-of-directive` occur in order in a ``#if`` expression.  If
so, they return the guard macro to ``do_if`` in the variable
``mi_ind_cmacro``, and otherwise set it to ``NULL``.
``enter_macro_context`` sets ``mi_valid`` to false, so if a macro
was expanded whilst parsing any part of the expression, then the
top-of-file test in ``push_conditional`` fails and the optimization
is turned off.

.. _files:

File Handling
=============

.. index:: files

Fairly obviously, the file handling code of cpplib resides in the file
files.c.  It takes care of the details of file searching,
opening, reading and caching, for both the main source file and all the
headers it recursively includes.

The basic strategy is to minimize the number of system calls.  On many
systems, the basic ``open ()`` and ``fstat ()`` system calls can
be quite expensive.  For every ``#include`` -d file, we need to try
all the directories in the search path until we find a match.  Some
projects, such as glibc, pass twenty or thirty include paths on the
command line, so this can rapidly become time consuming.

For a header file we have not encountered before we have little choice
but to do this.  However, it is often the case that the same headers are
repeatedly included, and in these cases we try to avoid repeating the
filesystem queries whilst searching for the correct file.

For each file we try to open, we store the constructed path in a splay
tree.  This path first undergoes simplification by the function
``_cpp_simplify_pathname``.  For example,
/usr/include/bits/../foo.h is simplified to
/usr/include/foo.h before we enter it in the splay tree and try
to ``open ()`` the file.  CPP will then find subsequent uses of
foo.h, even as /usr/include/foo.h, in the splay tree and
save system calls.

Further, it is likely the file contents have also been cached, saving a
``read ()`` system call.  We don't bother caching the contents of
header files that are re-inclusion protected, and whose re-inclusion
macro is defined when we leave the header file for the first time.  If
the host supports it, we try to map suitably large files into memory,
rather than reading them in directly.

The include paths are internally stored on a null-terminated
singly-linked list, starting with the ``"header.h"`` directory search
chain, which then links into the ``<header.h>`` directory chain.

Files included with the ``<foo.h>`` syntax start the lookup directly
in the second half of this chain.  However, files included with the
``"foo.h"`` syntax start at the beginning of the chain, but with one
extra directory prepended.  This is the directory of the current file;
the one containing the ``#include`` directive.  Prepending this
directory on a per-file basis is handled by the function
``search_from``.

Note that a header included with a directory component, such as
``#include "mydir/foo.h"`` and opened as
/usr/local/include/mydir/foo.h, will have the complete path minus
the basename :samp:`foo.h` as the current directory.

Enough information is stored in the splay tree that CPP can immediately
tell whether it can skip the header file because of the multiple include
optimization, whether the file didn't exist or couldn't be opened for
some reason, or whether the header was flagged not to be re-used, as it
is with the obsolete ``#import`` directive.

For the benefit of MS-DOS filesystems with an 8.3 filename limitation,
CPP offers the ability to treat various include file names as aliases
for the real header files with shorter names.  The map from one to the
other is found in a special file called :samp:`header.gcc`, stored in the
command line (or system) include directories to which the mapping
applies.  This may be higher up the directory tree than the full path to
the file minus the base name.

.. _concept-index:
