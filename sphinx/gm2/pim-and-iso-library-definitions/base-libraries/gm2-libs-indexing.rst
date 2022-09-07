.. _gm2-libs-indexing:

gm2-libs/Indexing
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Indexing ;

  FROM SYSTEM IMPORT ADDRESS ;
  EXPORT QUALIFIED Index, InitIndex, KillIndex, GetIndice, PutIndice,
                   HighIndice, LowIndice, InBounds, IsIndiceInIndex,
                   RemoveIndiceFromIndex, IncludeIndiceIntoIndex,
                   ForeachIndiceInIndexDo, DeleteIndice, DebugIndex ;

  TYPE
  Index (type)
     Index ;
  IndexProcedure (type)
     IndexProcedure = PROCEDURE (ADDRESS) ;

  (*
     InitIndex - creates and returns an Index.
  *)

  InitIndex
  PROCEDURE InitIndex (low: CARDINAL) : Index ;

  (*
     KillIndex - returns Index to free storage.
  *)

  KillIndex
  PROCEDURE KillIndex (i: Index) : Index ;

  (*
     DebugIndex - turns on debugging within an index.
  *)

  DebugIndex
  PROCEDURE DebugIndex (i: Index) : Index ;

  (*
     InBounds - returns TRUE if indice, n, is within the bounds
                of the dynamic array.
  *)

  InBounds
  PROCEDURE InBounds (i: Index; n: CARDINAL) : BOOLEAN ;

  (*
     HighIndice - returns the last legally accessible indice of this array.
  *)

  HighIndice
  PROCEDURE HighIndice (i: Index) : CARDINAL ;

  (*
     LowIndice - returns the first legally accessible indice of this array.
  *)

  LowIndice
  PROCEDURE LowIndice (i: Index) : CARDINAL ;

  (*
     PutIndice - places, a, into the dynamic array at position i[n]
  *)

  PutIndice
  PROCEDURE PutIndice (i: Index; n: CARDINAL; a: ADDRESS) ;

  (*
     GetIndice - retrieves, element i[n] from the dynamic array.
  *)

  GetIndice
  PROCEDURE GetIndice (i: Index; n: CARDINAL) : ADDRESS ;

  (*
     IsIndiceInIndex - returns TRUE if, a, is in the index, i.
  *)

  IsIndiceInIndex
  PROCEDURE IsIndiceInIndex (i: Index; a: ADDRESS) : BOOLEAN ;

  (*
     RemoveIndiceFromIndex - removes, a, from Index, i.
  *)

  RemoveIndiceFromIndex
  PROCEDURE RemoveIndiceFromIndex (i: Index; a: ADDRESS) ;

  (*
     DeleteIndice - delete i[j] from the array.
  *)

  DeleteIndice
  PROCEDURE DeleteIndice (i: Index; j: CARDINAL) ;

  (*
     IncludeIndiceIntoIndex - if the indice is not in the index, then
                              add it at the end.
  *)

  IncludeIndiceIntoIndex
  PROCEDURE IncludeIndiceIntoIndex (i: Index; a: ADDRESS) ;

  (*
     ForeachIndiceInIndexDo - for each j indice of i, call procedure p(i[j])
  *)

  ForeachIndiceInIndexDo
  PROCEDURE ForeachIndiceInIndexDo (i: Index; p: IndexProcedure) ;

  END Indexing.

