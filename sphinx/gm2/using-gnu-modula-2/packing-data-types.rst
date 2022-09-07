.. _packed:

Packing data types
******************

The pragma ``<* bytealignment(0) *>`` can be used to specify that
the fields within a ``RECORD`` are to be packed.  Currently this
only applies to fields which are declared as subranges, ordinal types
and enumerated types.  Here is an example of how two subranges might
be packed into a byte.

.. code-block:: modula2

  TYPE
     bits3c =  [0..7] ;
     bits3i = [-4..3] ;

     byte = RECORD
                <* bytealignment(0) *>
                x: bits3c ;
                <* bitsunused(2) *>
                y: bits3i ;
            END ;

Notice that the user has specified that in between fields ``x`` and
``y`` there are two bits unused.

Now the user wishes to create a record with byte numbers zero and one
occupied and then an ``INTEGER32`` field which is four byte
aligned.  In this case byte numbers two and three will be unused.  The
pragma ``bytealignment`` can be issued at the start of the record
indicating the default alignment for the whole record and this can be
overridden by individual fields if necessary.

.. code-block:: modula2

     rec = RECORD
              <* bytealignment (1) *> ;
              a, b: byte ;
              x: INTEGER32 <* bytealignment(4) *> ;
           END ;

In the following example the user has specified that a record has two
fields ``p`` and ``q`` but that there are three bytes unused between
these fields.

.. code-block:: modula2

     header = RECORD
                 <* bytealignment(1) *>
                 p: byte ;
                 <* bytesunused(3) *>
                 q: byte ;
              END ;

The pragma ``<* bytesunused(x) *>`` can only be used if the current
field is on a byte boundary.  There is also a ``SYSTEM`` pseudo
procedure function ``TBITSIZE(T)`` which returns the minimum number of
bits necessary to represent type ``T``.

Another example of packing record bit fields is given below:

.. code-block:: modula2

  MODULE align21 ;

  FROM libc IMPORT exit ;

  TYPE
     colour = (red, blue, green, purple, white, black) ;

     soc = PACKEDSET OF colour ;

     rec = RECORD
              <* bytealignment(0) *>
              x: soc ;
              y: [-1..1] ;
           END ;

  VAR
     r: rec ;
     v: CARDINAL ;
  BEGIN
     v := SIZE(r) ;
     IF SIZE(r)#1
     THEN
        exit(1)
     END ;
     r.x := soc{blue} ;
     IF r.x#soc{blue}
     THEN
        exit(2)
     END
  END align21.

Here we see that the total size of this record is one byte and consists
of a six bit set type followed by a 2 bit integer subrange.

