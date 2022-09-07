.. _why-use-gnu-modula-2:

Why use GNU Modula-2
********************

There are a number of advantages of using GNU Modula-2 rather than
translate an existing project into another language.

The first advantage is of maintainability of the original sources
and the ability to debug the original project source code using a
combination of gm2 and gdb.

The second advantage is that gcc runs on many processors and
platforms.  gm2 builds and runs on powerpc64le, amd64, i386, aarch64
to name but a few processors.

The compiler provides semantic analysis and runtime checking (full ISO
Modula-2 checking is implemented) and there is a plugin which can,
under certain conditions, detect runtime errors at compile time.

gm2 can produce swig interface headers to allow access from Python and
other scripting languages.  The compiler supports PIM2, PIM3, PIM4 and
ISO dialects of Modula-2, work is underway to implement M2R10.  Many
of the GCC builtins are available and access to assembly programming
is achieved using the same syntax as that used by GCC.

