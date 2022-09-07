.. _gm2-libs-coroutines-keyboardleds:

gm2-libs-coroutines/KeyBoardLEDs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE KeyBoardLEDs ;

  EXPORT QUALIFIED SwitchLeds,
                   SwitchScroll, SwitchNum, SwitchCaps ;

  (*
     SwitchLeds - switch the keyboard LEDs to the state defined
                  by the BOOLEAN variables. TRUE = ON.
  *)

  SwitchLeds
  PROCEDURE SwitchLeds (NumLock, CapsLock, ScrollLock: BOOLEAN) ;

  (*
     SwitchScroll - switchs the scroll LED on or off.
  *)

  SwitchScroll
  PROCEDURE SwitchScroll (Scroll: BOOLEAN) ;

  (*
     SwitchNum - switches the Num LED on or off.
  *)

  SwitchNum
  PROCEDURE SwitchNum (Num: BOOLEAN) ;

  (*
     SwitchCaps - switches the Caps LED on or off.
  *)

  SwitchCaps
  PROCEDURE SwitchCaps (Caps: BOOLEAN) ;

  END KeyBoardLEDs.

