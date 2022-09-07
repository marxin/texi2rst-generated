.. _unbounded-by-reference:

Unbounded by reference
**********************

This section documents a GNU Modula-2 compiler switch which implements
a language optimisation surrounding the implementation of unbounded
arrays.  In GNU Modula-2 the unbounded array is implemented by
utilising an internal structure ``struct {dataType *address,
unsigned int high}``.  So given the Modula-2 procedure declaration:

.. code-block:: modula2

  PROCEDURE foo (VAR a: ARRAY OF dataType) ;
  BEGIN
     IF a[2]= (* etc *)
  END foo ;

it is translated into GCC ``tree`` s, which can be represented
in their C form thus:

.. code-block:: modula2

  void foo (struct {dataType *address, unsigned int high} a)
  {
     if (a.address[2] == /* etc */
  }

Whereas if the procedure ``foo`` was declared as:

.. code-block:: modula2

  PROCEDURE foo (a: ARRAY OF dataType) ;
  BEGIN
     IF a[2]= (* etc *)
  END foo ;

then it is implemented by being translated into the following
GCC ``tree`` s, which can be represented in their C form thus:

.. code-block:: modula2

  void foo (struct {dataType *address, unsigned int high} a)
  {
     dataType *copyContents = (dataType *)alloca (a.high+1);
     memcpy(copyContents, a.address, a.high+1);
     a.address = copyContents;

     if (a.address[2] == /* etc */
  }

This implementation works, but it makes a copy of each non VAR
unbounded array when a procedure is entered.  If the unbounded array
is not changed during procedure ``foo`` then this implementation
will be very inefficient.  In effect Modula-2 lacks the ``REF``
keyword of Ada.  Consequently the programmer maybe tempted to
sacrifice semantic clarity for greater efficiency by declaring the
parameter using the ``VAR`` keyword in place of ``REF``.

The ``-funbounded-by-reference`` switch instructs the compiler to
check and see if the programmer is modifying the content of any
unbounded array.  If it is modified then a copy will be made upon
entry into the procedure.  Conversely if the content is only read and
never modified then this non ``VAR`` unbounded array is a candidate
for being passed by reference.  It is only a candidate as it is still
possible that passing this parameter by reference could alter the
meaning of the source code.  For example consider the following case:

.. code-block:: modula2

  PROCEDURE StrConCat (VAR a: ARRAY OF CHAR; b, c: ARRAY OF CHAR) ;
  BEGIN
     (* code which performs string a := b + c *)
  END StrConCat ;

  PROCEDURE foo ;
  VAR
     a: ARRAY [0..3] OF CHAR ;
  BEGIN
     a := 'q' ;
     StrConCat(a, a, a)
  END foo ;

In the code above we see that the same parameter, ``a``, is being
passed three times to ``StrConCat``.  Clearly even though parameters
``b`` and ``c`` are never modified it would be incorrect to
implement them as pass by reference.  Therefore the compiler checks to
see if any non ``VAR`` parameter is type compatible with any
``VAR`` parameter and if so it generates runtime procedure entry
checks to determine whether the contents of parameters ``b`` or
``c`` matches the contents of ``a``.  If a match is detected
then a copy is made and the ``address`` in the unbounded
``struct`` ure is modified.

The compiler will check the address range of each candidate against
the address range of any ``VAR`` parameter, providing they are type
compatible.  For example consider:

.. code-block:: modula2

  PROCEDURE foo (a: ARRAY OF BYTE; VAR f: REAL) ;
  BEGIN
     f := 3.14 ;
     IF a[0]=BYTE(0)
     THEN
        (* etc *)
     END
  END foo ;

  PROCEDURE bar ;
  BEGIN
     r := 2.0 ;
     foo(r, r)
  END bar ;

Here we see that although parameter, ``a``, is a candidate for the
passing by reference, it would be incorrect to use this
transformation.  Thus the compiler detects that parameters, ``a``
and ``f`` are type compatible and will produce runtime checking
code to test whether the address range of their respective contents
intersect.

.. _linking:

This section describes the linking related options.  There are three
linking strategies available which are dynamic scaffold, static
scaffold and user defined.  The dynamic scaffold is enabled by default
and each module will register itself to the runtime :samp:`M2RTS` via
a constructor.  The static scaffold mechanism will invoke each modules
:samp:`_init` and :samp:`_finish` function in turn via a sequence of
calls from within :samp:`main`.  Lastly the user defined strategy
can be implemented by turning off the dynamic and static options via
:samp:`-fno-scaffold-dynamic` and :samp:`-fno-scaffold-static`.

In the simple test below:

.. code-block:: modula2

  $ gm2 hello.mod

the driver will add the options :samp:`-fscaffold-dynamic` and
:samp:`-fgen-module-list=-` which generate a list of application
modules and also creates the :samp:`main` function with calls to
:samp:`M2RTS`.  It can be useful to add the option :samp:`-fsources`
which displays the source files as they are parsed and summarizes
whether the source file is required for compilation or linking.

If you wish to split the above command line into a compile and link
then you could use these steps:

.. code-block:: modula2

  $ gm2 -c -fscaffold-main hello.mod
  $ gm2 hello.o

The :samp:`-fscaffold-main` informs the compiler to generate the
:samp:`main` function and scaffold.  You can enable the environment
variable :samp:`GCC_M2LINK_RTFLAG` to trace the construction and
destruction of the application.  The values for
:samp:`GCC_M2LINK_RTFLAG` are shown in the table below:

.. code-block:: modula2

  value   | meaning
  =================
  all     | turn on all flags below
  module  | trace modules as they register themselves
  pre     | generate module list prior to dependency resolution
  dep     | trace module dependency resolution
  post    | generate module list after dependency resolution
  force   | generate a module list after dependency and forced
          | ordering is complete

The values can be combined using a comma separated list.

One of the advantages of the dynamic scaffold is that the driver
behaves in a similar way to the other front end drivers.
For example consider a small project consisting of 4 definition
implementation modules (:samp:`a.def`, :samp:`a.mod`, :samp:`b.def`,
:samp:`b.mod`, :samp:`c.def`, :samp:`c.mod`, :samp:`d.def`, :samp:`d.mod`)
and a program module :samp:`program.mod`.

To link this project we could:

.. code-block:: modula2

  $ gm2 -g -c a.mod
  $ gm2 -g -c b.mod
  $ gm2 -g -c c.mod
  $ gm2 -g -c d.mod
  $ gm2 -g program.mod a.o b.o c.o d.o

The module initialization sequence is defined by the ISO standard to
follow the import graph traversal.  The initialization order is the
order in which the corresponding separate modules finish the
processing of their import lists.

However, if required, you can override this using
:samp:`-fruntime-modules=a,b,c,d` for example which forces the
initialization sequence to :samp:`a`, :samp:`b`, :samp:`c` and :samp:`d`.

