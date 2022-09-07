.. _gm2-libs-iso-realstr:

gm2-libs-iso/RealStr
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RealStr;

    (* REAL/string conversions *)

  IMPORT
    ConvTypes;

  TYPE
    (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
  ConvResults (type)
    ConvResults = ConvTypes.ConvResults;

  (* the string form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit}, [".",
       {decimal digit}]
  *)

  (* the string form of a signed floating-point real number is
       signed fixed-point real number, "E", ["+" | "-"],
       decimal digit, {decimal digit}
  *)

  StrToReal
  PROCEDURE StrToReal (str: ARRAY OF CHAR; VAR real: REAL;
                       VAR res: ConvResults);
    (* Ignores any leading spaces in str. If the subsequent characters
       in str are in the format of a signed real number, assigns a
       corresponding value to real.  Assigns a value indicating the
       format of str to res.
    *)

  RealToFloat
  PROCEDURE RealToFloat (real: REAL; sigFigs: CARDINAL;
                         VAR str: ARRAY OF CHAR);
    (* Converts the value of real to floating-point string form, with
       sigFigs significant figures, and copies the possibly truncated
       result to str.
    *)

  RealToEng
  PROCEDURE RealToEng (real: REAL; sigFigs: CARDINAL;
                       VAR str: ARRAY OF CHAR);
    (* Converts the value of real to floating-point string form, with
       sigFigs significant figures, and copies the possibly truncated
       result to str.  The number is scaled with one to three digits
       in the whole number part and with an exponent that is a multiple
       of three.
    *)

  RealToFixed
  PROCEDURE RealToFixed (real: REAL; place: INTEGER;
                         VAR str: ARRAY OF CHAR);
    (* Converts the value of real to fixed-point string form, rounded
       to the given place relative to the decimal point, and copies
       the possibly truncated result to str.
    *)

  RealToStr
  PROCEDURE RealToStr (real: REAL; VAR str: ARRAY OF CHAR);
    (* Converts the value of real as RealToFixed if the sign and
       magnitude can be shown within the capacity of str, or
       otherwise as RealToFloat, and copies the possibly truncated
       result to str. The number of places or significant digits are
       implementation-defined.
    *)

  END RealStr.

