.. _gm2-libs-iso-charclass:

gm2-libs-iso/CharClass
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE CharClass;

    (* Classification of values of the type CHAR *)

  IsNumeric
  PROCEDURE IsNumeric (ch: CHAR): BOOLEAN;
    (* Returns TRUE if and only if ch is classified as a numeric character *)

  IsLetter
  PROCEDURE IsLetter (ch: CHAR): BOOLEAN;
    (* Returns TRUE if and only if ch is classified as a letter *)

  IsUpper
  PROCEDURE IsUpper (ch: CHAR): BOOLEAN;
    (* Returns TRUE if and only if ch is classified as an upper case letter *)

  IsLower
  PROCEDURE IsLower (ch: CHAR): BOOLEAN;
    (* Returns TRUE if and only if ch is classified as a lower case letter *)

  IsControl
  PROCEDURE IsControl (ch: CHAR): BOOLEAN;
    (* Returns TRUE if and only if ch represents a control function *)

  IsWhiteSpace
  PROCEDURE IsWhiteSpace (ch: CHAR): BOOLEAN;
    (* Returns TRUE if and only if ch represents a space character or a format effector *)

  END CharClass.

