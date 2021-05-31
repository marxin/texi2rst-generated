Cpplib-the GNU C Preprocessor
-----------------------------

The GNU C preprocessor is
implemented as a library, :dfn:`cpplib`, so it can be easily shared between
a stand-alone preprocessor, and a preprocessor integrated with the C,
C++ and Objective-C front ends.  It is also available for use by other
programs, though this is not recommended as its exposed interface has
not yet reached a point of reasonable stability.

The library has been written to be re-entrant, so that it can be used
to preprocess many files simultaneously if necessary.  It has also been
written with the preprocessing token as the fundamental unit; the
preprocessor in previous versions of GCC would operate on text strings
as the fundamental unit.

This brief manual documents the internals of cpplib, and explains some
of the tricky issues.  It is intended that, along with the comments in
the source code, a reasonably competent C programmer should be able to
figure out what the code is doing, and why things have been implemented
the way they have.

.. toctree::
  :maxdepth: 2

  cpplib-the-gnu-c-preprocessor/conventions
  cpplib-the-gnu-c-preprocessor/lexer
  cpplib-the-gnu-c-preprocessor/hash-nodes
  cpplib-the-gnu-c-preprocessor/macro-expansion
  cpplib-the-gnu-c-preprocessor/token-spacing
  cpplib-the-gnu-c-preprocessor/line-numbering
  cpplib-the-gnu-c-preprocessor/guard-macros
  cpplib-the-gnu-c-preprocessor/files
  cpplib-the-gnu-c-preprocessor/concept-index

