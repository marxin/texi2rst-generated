.. _gm2-libs-ascii:

gm2-libs/ASCII
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ASCII ;

  EXPORT QUALIFIED
       nul, soh, stx, etx, eot, enq, ack, bel,
       bs , ht , nl , vt , np , cr , so , si ,
       dle, dc1, dc2, dc3, dc4, nak, syn, etb,
       can, em , sub, esc, fs , gs , rs , us ,
       sp ,  (* All the above are in order *)
       lf, ff, eof, del, tab, EOL ;

  (*
     Note that lf, eof and EOL are added.
  *)

  CONST
  nul (const)
  soh (const)
  stx (const)
  etx (const)
       nul=000C; soh=001C; stx=002C; etx=003C;
  eot (const)
  enq (const)
  ack (const)
  bel (const)
       eot=004C; enq=005C; ack=006C; bel=007C;
  bs  (const)
  ht  (const)
  nl  (const)
  vt  (const)
       bs =010C; ht =011C; nl =012C; vt =013C;
  np  (const)
  cr  (const)
  so  (const)
  si  (const)
       np =014C; cr =015C; so =016C; si =017C;
  dle (const)
  dc1 (const)
  dc2 (const)
  dc3 (const)
       dle=020C; dc1=021C; dc2=022C; dc3=023C;
  dc4 (const)
  nak (const)
  syn (const)
  etb (const)
       dc4=024C; nak=025C; syn=026C; etb=027C;
  can (const)
  em  (const)
  sub (const)
  esc (const)
       can=030C; em =031C; sub=032C; esc=033C;
  fs  (const)
  gs  (const)
  rs  (const)
  us  (const)
       fs =034C; gs =035C; rs =036C; us =037C;
  sp  (const)
       sp =040C; (* All the above are in order *)
  lf  (const)
  ff  (const)
  eof (const)
  tab (const)
       lf =nl  ; ff =np  ; eof=eot ; tab=ht  ;
  del (const)
  EOL (const)
       del=177C; EOL=nl  ;

  END ASCII.

