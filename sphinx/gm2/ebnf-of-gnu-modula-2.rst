.. _ebnf:

EBNF of GNU Modula-2
--------------------

This chapter contains the EBNF of GNU Modula-2. This grammer currently
supports both PIM and ISO dialects.  The rules here are automatically
extracted from the grammer files in GNU Modula-2 and serve to document
the syntax of the extensions described earlier and how they fit in
with the base language.

Note that the first six productions are built into the lexical analysis
phase.

.. This file is part of GCC.
   Permission is granted to copy, distribute and/or modify this document
   under the terms of the GNU Free Documentation License, Version 1.2 or
   any later version published by the Free Software Foundation.

.. code-block:: modula2

  Ident := is a builtin and checks for an identifier
         =: 
  Ident (ebnf)

.. code-block:: modula2

  Integer := is a builtin and checks for an integer
           =: 
  Integer (ebnf)

.. code-block:: modula2

  Real := is a builtin and checks for an real constant
        =: 
  Real (ebnf)

.. code-block:: modula2

  string := is a builtin and checks for an string constant
          =: 
  string (ebnf)

.. code-block:: modula2

  FileUnit := ( DefinitionModule  | 
                ImplementationOrProgramModule  ) 
            =: 
  FileUnit (ebnf)

.. code-block:: modula2

  ProgramModule := 'MODULE' Ident [ Priority  ] ';' { 
     Import  } Block Ident '.' 
                 =: 
  ProgramModule (ebnf)

.. code-block:: modula2

  ImplementationModule := 'IMPLEMENTATION' 'MODULE' Ident 
                          [ Priority  ] ';' { Import 
                                                } Block 
                          Ident '.' 
                        =: 
  ImplementationModule (ebnf)

.. code-block:: modula2

  ImplementationOrProgramModule := ImplementationModule  | 
                                   ProgramModule 
                                 =: 
  ImplementationOrProgramModule (ebnf)

.. code-block:: modula2

  Number := Integer  | Real 
          =: 
  Number (ebnf)

.. code-block:: modula2

  Qualident := Ident { '.' Ident  } 
             =: 
  Qualident (ebnf)

.. code-block:: modula2

  ConstantDeclaration := Ident '=' ConstExpression 
                       =: 
  ConstantDeclaration (ebnf)

.. code-block:: modula2

  ConstExpression := SimpleConstExpr [ Relation SimpleConstExpr  ] 
                   =: 
  ConstExpression (ebnf)

.. code-block:: modula2

  Relation := '='  | '#'  | '<>'  | '<'  | '<='  | 
              '>'  | '>='  | 'IN' 
            =: 
  Relation (ebnf)

.. code-block:: modula2

  SimpleConstExpr := UnaryOrConstTerm { AddOperator 
                                         ConstTerm  } 
                   =: 
  SimpleConstExpr (ebnf)

.. code-block:: modula2

  UnaryOrConstTerm := '+' ConstTerm  | 
                      '-' ConstTerm  | 
                      ConstTerm 
                    =: 
  UnaryOrConstTerm (ebnf)

.. code-block:: modula2

  AddOperator := '+'  | '-'  | 'OR' 
               =: 
  AddOperator (ebnf)

.. code-block:: modula2

  ConstTerm := ConstFactor { MulOperator ConstFactor  } 
             =: 
  ConstTerm (ebnf)

.. code-block:: modula2

  MulOperator := '*'  | '/'  | 'DIV'  | 'MOD'  | 
                 'REM'  | 'AND'  | '&' 
               =: 
  MulOperator (ebnf)

.. code-block:: modula2

  ConstFactor := Number  | ConstString  | 
                 ConstSetOrQualidentOrFunction  | 
                 '(' ConstExpression ')'  | 
                 'NOT' ConstFactor  | 
                 ConstAttribute 
               =: 
  ConstFactor (ebnf)

.. code-block:: modula2

  ConstString := string 
               =: 
  ConstString (ebnf)

.. code-block:: modula2

  ComponentElement := ConstExpression [ '..' ConstExpression  ] 
                    =: 
  ComponentElement (ebnf)

.. code-block:: modula2

  ComponentValue := ComponentElement [ 'BY' ConstExpression  ] 
                  =: 
  ComponentValue (ebnf)

.. code-block:: modula2

  ArraySetRecordValue := ComponentValue { ',' ComponentValue  } 
                       =: 
  ArraySetRecordValue (ebnf)

.. code-block:: modula2

  Constructor := '{' [ ArraySetRecordValue  ] '}' 
               =: 
  Constructor (ebnf)

.. code-block:: modula2

  ConstSetOrQualidentOrFunction := Constructor  | 
                                   Qualident [ Constructor  | 
                                               ConstActualParameters  ] 
                                 =: 
  ConstSetOrQualidentOrFunction (ebnf)

.. code-block:: modula2

  ConstActualParameters := '(' [ ExpList  ] ')' 
                         =: 
  ConstActualParameters (ebnf)

.. code-block:: modula2

  ConstAttribute := '__ATTRIBUTE__' '__BUILTIN__' '(' 
                    '(' ConstAttributeExpression ')' 
                    ')' 
                  =: 
  ConstAttribute (ebnf)

.. code-block:: modula2

  ConstAttributeExpression := Ident  | '<' Qualident 
                              ',' Ident '>' 
                            =: 
  ConstAttributeExpression (ebnf)

.. code-block:: modula2

  ByteAlignment := '<*' AttributeExpression '*>' 
                 =: 
  ByteAlignment (ebnf)

.. code-block:: modula2

  Alignment := [ ByteAlignment  ] 
             =: 
  Alignment (ebnf)

.. code-block:: modula2

  TypeDeclaration := Ident '=' Type Alignment 
                   =: 
  TypeDeclaration (ebnf)

.. code-block:: modula2

  Type := SimpleType  | ArrayType  | RecordType  | 
          SetType  | PointerType  | ProcedureType 
        =: 
  Type (ebnf)

.. code-block:: modula2

  SimpleType := Qualident [ SubrangeType  ]  | 
                Enumeration  | SubrangeType 
              =: 
  SimpleType (ebnf)

.. code-block:: modula2

  Enumeration := '(' IdentList ')' 
               =: 
  Enumeration (ebnf)

.. code-block:: modula2

  IdentList := Ident { ',' Ident  } 
             =: 
  IdentList (ebnf)

.. code-block:: modula2

  SubrangeType := '[' ConstExpression '..' ConstExpression 
                  ']' 
                =: 
  SubrangeType (ebnf)

.. code-block:: modula2

  ArrayType := 'ARRAY' SimpleType { ',' SimpleType  } 
               'OF' Type 
             =: 
  ArrayType (ebnf)

.. code-block:: modula2

  RecordType := 'RECORD' [ DefaultRecordAttributes  ] 
                FieldListSequence 'END' 
              =: 
  RecordType (ebnf)

.. code-block:: modula2

  DefaultRecordAttributes := '<*' AttributeExpression 
                             '*>' 
                           =: 
  DefaultRecordAttributes (ebnf)

.. code-block:: modula2

  RecordFieldPragma := [ '<*' FieldPragmaExpression { 
     ',' FieldPragmaExpression  } '*>'  ] 
                     =: 
  RecordFieldPragma (ebnf)

.. code-block:: modula2

  FieldPragmaExpression := Ident [ '(' ConstExpression 
                                   ')'  ] 
                         =: 
  FieldPragmaExpression (ebnf)

.. code-block:: modula2

  AttributeExpression := Ident '(' ConstExpression ')' 
                       =: 
  AttributeExpression (ebnf)

.. code-block:: modula2

  FieldListSequence := FieldListStatement { ';' FieldListStatement  } 
                     =: 
  FieldListSequence (ebnf)

.. code-block:: modula2

  FieldListStatement := [ FieldList  ] 
                      =: 
  FieldListStatement (ebnf)

.. code-block:: modula2

  FieldList := IdentList ':' Type RecordFieldPragma  | 
               'CASE' CaseTag 'OF' Varient { '|' Varient  } 
               [ 'ELSE' FieldListSequence  ] 'END' 
             =: 
  FieldList (ebnf)

.. code-block:: modula2

  TagIdent := [ Ident  ] 
            =: 
  TagIdent (ebnf)

.. code-block:: modula2

  CaseTag := TagIdent [ ':' Qualident  ] 
           =: 
  CaseTag (ebnf)

.. code-block:: modula2

  Varient := [ VarientCaseLabelList ':' FieldListSequence  ] 
           =: 
  Varient (ebnf)

.. code-block:: modula2

  VarientCaseLabelList := VarientCaseLabels { ',' VarientCaseLabels  } 
                        =: 
  VarientCaseLabelList (ebnf)

.. code-block:: modula2

  VarientCaseLabels := ConstExpression [ '..' ConstExpression  ] 
                     =: 
  VarientCaseLabels (ebnf)

.. code-block:: modula2

  CaseLabelList := CaseLabels { ',' CaseLabels  } 
                 =: 
  CaseLabelList (ebnf)

.. code-block:: modula2

  CaseLabels := ConstExpression [ '..' ConstExpression  ] 
              =: 
  CaseLabels (ebnf)

.. code-block:: modula2

  SetType := ( 'SET'  | 'PACKEDSET'  ) 'OF' SimpleType 
           =: 
  SetType (ebnf)

.. code-block:: modula2

  PointerType := 'POINTER' 'TO' Type 
               =: 
  PointerType (ebnf)

.. code-block:: modula2

  ProcedureType := 'PROCEDURE' [ FormalTypeList  ] 
                 =: 
  ProcedureType (ebnf)

.. code-block:: modula2

  FormalTypeList := '(' ( ')' FormalReturn  | 
                          ProcedureParameters ')' FormalReturn  ) 
                  =: 
  FormalTypeList (ebnf)

.. code-block:: modula2

  FormalReturn := [ ':' OptReturnType  ] 
                =: 
  FormalReturn (ebnf)

.. code-block:: modula2

  OptReturnType := '[' Qualident ']'  | 
                   Qualident 
                 =: 
  OptReturnType (ebnf)

.. code-block:: modula2

  ProcedureParameters := ProcedureParameter { ',' ProcedureParameter  } 
                       =: 
  ProcedureParameters (ebnf)

.. code-block:: modula2

  ProcedureParameter := '...'  | 'VAR' FormalType  | 
                        FormalType 
                      =: 
  ProcedureParameter (ebnf)

.. code-block:: modula2

  VarIdent := Ident [ '[' ConstExpression ']'  ] 
            =: 
  VarIdent (ebnf)

.. code-block:: modula2

  VariableDeclaration := VarIdentList ':' Type Alignment 
                       =: 
  VariableDeclaration (ebnf)

.. code-block:: modula2

  VarIdentList := VarIdent { ',' VarIdent  } 
                =: 
  VarIdentList (ebnf)

.. code-block:: modula2

  Designator := Qualident { SubDesignator  } 
              =: 
  Designator (ebnf)

.. code-block:: modula2

  SubDesignator := '.' Ident  | '[' ExpList ']'  | 
                   '^' 
                 =: 
  SubDesignator (ebnf)

.. code-block:: modula2

  ExpList := Expression { ',' Expression  } 
           =: 
  ExpList (ebnf)

.. code-block:: modula2

  Expression := SimpleExpression [ Relation SimpleExpression  ] 
              =: 
  Expression (ebnf)

.. code-block:: modula2

  SimpleExpression := [ '+'  | '-'  ] Term { AddOperator 
                                              Term  } 
                    =: 
  SimpleExpression (ebnf)

.. code-block:: modula2

  Term := Factor { MulOperator Factor  } 
        =: 
  Term (ebnf)

.. code-block:: modula2

  Factor := Number  | string  | SetOrDesignatorOrFunction  | 
            '(' Expression ')'  | 
            'NOT' Factor  | ConstAttribute 
          =: 
  Factor (ebnf)

.. code-block:: modula2

  SetOrDesignatorOrFunction := ( Qualident [ Constructor  | 
                                             SimpleDes 
                                             [ ActualParameters  ]  ]  | 
                                 Constructor  ) 
                             =: 
  SetOrDesignatorOrFunction (ebnf)

.. code-block:: modula2

  SimpleDes := { '.' Ident  | '[' ExpList ']'  | 
                  '^'  } 
             =: 
  SimpleDes (ebnf)

.. code-block:: modula2

  ActualParameters := '(' [ ExpList  ] ')' 
                    =: 
  ActualParameters (ebnf)

.. code-block:: modula2

  Statement := [ AssignmentOrProcedureCall  | 
                 IfStatement  | CaseStatement  | 
                 WhileStatement  | RepeatStatement  | 
                 LoopStatement  | ForStatement  | 
                 WithStatement  | AsmStatement  | 
                 'EXIT'  | 'RETURN' [ Expression  ]  | 
                 RetryStatement  ] 
             =: 
  Statement (ebnf)

.. code-block:: modula2

  RetryStatement := 'RETRY' 
                  =: 
  RetryStatement (ebnf)

.. code-block:: modula2

  AssignmentOrProcedureCall := Designator ( ':=' Expression  | 
                                            ActualParameters  | 
                                             ) 
                             =: 
  AssignmentOrProcedureCall (ebnf)

.. code-block:: modula2

  StatementSequence := Statement { ';' Statement  } 
                     =: 
  StatementSequence (ebnf)

.. code-block:: modula2

  IfStatement := 'IF' Expression 'THEN' StatementSequence 
                 { 'ELSIF' Expression 'THEN' StatementSequence  } 
                 [ 'ELSE' StatementSequence  ] 'END' 
               =: 
  IfStatement (ebnf)

.. code-block:: modula2

  CaseStatement := 'CASE' Expression 'OF' Case { '|' 
                                                  Case  } 
                   [ 'ELSE' StatementSequence  ] 'END' 
                 =: 
  CaseStatement (ebnf)

.. code-block:: modula2

  Case := [ CaseLabelList ':' StatementSequence  ] 
        =: 
  Case (ebnf)

.. code-block:: modula2

  WhileStatement := 'WHILE' Expression 'DO' StatementSequence 
                    'END' 
                  =: 
  WhileStatement (ebnf)

.. code-block:: modula2

  RepeatStatement := 'REPEAT' StatementSequence 'UNTIL' 
                     Expression 
                   =: 
  RepeatStatement (ebnf)

.. code-block:: modula2

  ForStatement := 'FOR' Ident ':=' Expression 'TO' Expression 
                  [ 'BY' ConstExpression  ] 'DO' StatementSequence 
                  'END' 
                =: 
  ForStatement (ebnf)

.. code-block:: modula2

  LoopStatement := 'LOOP' StatementSequence 'END' 
                 =: 
  LoopStatement (ebnf)

.. code-block:: modula2

  WithStatement := 'WITH' Designator 'DO' StatementSequence 
                   'END' 
                 =: 
  WithStatement (ebnf)

.. code-block:: modula2

  ProcedureDeclaration := ProcedureHeading ';' ( ProcedureBlock 
                                                 Ident 
                                                  ) 
                        =: 
  ProcedureDeclaration (ebnf)

.. code-block:: modula2

  DefineBuiltinProcedure := [ '__ATTRIBUTE__' '__BUILTIN__' 
                              '(' '(' Ident ')' ')'  | 
                              '__INLINE__'  ] 
                          =: 
  DefineBuiltinProcedure (ebnf)

.. code-block:: modula2

  ProcedureHeading := 'PROCEDURE' DefineBuiltinProcedure 
                      ( Ident [ FormalParameters  ] AttributeNoReturn  ) 
                    =: 
  ProcedureHeading (ebnf)

.. code-block:: modula2

  AttributeNoReturn := [ '<*' Ident '*>'  ] 
                     =: 
  AttributeNoReturn (ebnf)

.. code-block:: modula2

  Builtin := [ '__BUILTIN__'  | '__INLINE__'  ] 
           =: 
  Builtin (ebnf)

.. code-block:: modula2

  DefProcedureHeading := 'PROCEDURE' Builtin ( Ident 
                                               [ DefFormalParameters  ] 
                                               AttributeNoReturn  ) 

                       =: 
  DefProcedureHeading (ebnf)

.. code-block:: modula2

  ProcedureBlock := { Declaration  } [ 'BEGIN' BlockBody  ] 
                    'END' 
                  =: 
  ProcedureBlock (ebnf)

.. code-block:: modula2

  Block := { Declaration  } InitialBlock FinalBlock 
           'END' 
         =: 
  Block (ebnf)

.. code-block:: modula2

  InitialBlock := [ 'BEGIN' BlockBody  ] 
                =: 
  InitialBlock (ebnf)

.. code-block:: modula2

  FinalBlock := [ 'FINALLY' BlockBody  ] 
              =: 
  FinalBlock (ebnf)

.. code-block:: modula2

  BlockBody := NormalPart [ 'EXCEPT' ExceptionalPart  ] 
             =: 
  BlockBody (ebnf)

.. code-block:: modula2

  NormalPart := StatementSequence 
              =: 
  NormalPart (ebnf)

.. code-block:: modula2

  ExceptionalPart := StatementSequence 
                   =: 
  ExceptionalPart (ebnf)

.. code-block:: modula2

  Declaration := 'CONST' { ConstantDeclaration ';'  }  | 
                 'TYPE' { TypeDeclaration ';'  }  | 
                 'VAR' { VariableDeclaration ';'  }  | 
                 ProcedureDeclaration ';'  | 
                 ModuleDeclaration ';' 
               =: 
  Declaration (ebnf)

.. code-block:: modula2

  DefFormalParameters := '(' [ DefMultiFPSection  ] ')' 
                         FormalReturn 
                       =: 
  DefFormalParameters (ebnf)

.. code-block:: modula2

  DefMultiFPSection := DefExtendedFP  | 
                       FPSection [ ';' DefMultiFPSection  ] 
                     =: 
  DefMultiFPSection (ebnf)

.. code-block:: modula2

  FormalParameters := '(' [ MultiFPSection  ] ')' FormalReturn 
                    =: 
  FormalParameters (ebnf)

.. code-block:: modula2

  MultiFPSection := ExtendedFP  | FPSection [ ';' MultiFPSection  ] 
                  =: 
  MultiFPSection (ebnf)

.. code-block:: modula2

  FPSection := NonVarFPSection  | VarFPSection 
             =: 
  FPSection (ebnf)

.. code-block:: modula2

  DefExtendedFP := DefOptArg  | '...' 
                 =: 
  DefExtendedFP (ebnf)

.. code-block:: modula2

  ExtendedFP := OptArg  | '...' 
              =: 
  ExtendedFP (ebnf)

.. code-block:: modula2

  VarFPSection := 'VAR' IdentList ':' FormalType 
                =: 
  VarFPSection (ebnf)

.. code-block:: modula2

  NonVarFPSection := IdentList ':' FormalType 
                   =: 
  NonVarFPSection (ebnf)

.. code-block:: modula2

  OptArg := '[' Ident ':' FormalType [ '=' ConstExpression  ] 
            ']' 
          =: 
  OptArg (ebnf)

.. code-block:: modula2

  DefOptArg := '[' Ident ':' FormalType '=' ConstExpression 
               ']' 
             =: 
  DefOptArg (ebnf)

.. code-block:: modula2

  FormalType := { 'ARRAY' 'OF'  } Qualident 
              =: 
  FormalType (ebnf)

.. code-block:: modula2

  ModuleDeclaration := 'MODULE' Ident [ Priority  ] ';' 
                       { Import  } [ Export  ] Block 
                       Ident 
                     =: 
  ModuleDeclaration (ebnf)

.. code-block:: modula2

  Priority := '[' ConstExpression ']' 
            =: 
  Priority (ebnf)

.. code-block:: modula2

  Export := 'EXPORT' ( 'QUALIFIED' IdentList  | 
                       'UNQUALIFIED' IdentList  | 
                       IdentList  ) ';' 
          =: 
  Export (ebnf)

.. code-block:: modula2

  Import := 'FROM' Ident 'IMPORT' IdentList ';'  | 
            'IMPORT' IdentList ';' 
          =: 
  Import (ebnf)

.. code-block:: modula2

  DefinitionModule := 'DEFINITION' 'MODULE' [ 'FOR' string 
                                               ] Ident 
                      ';' { Import  } [ Export  ] { 
     Definition  } 'END' Ident '.' 
                    =: 
  DefinitionModule (ebnf)

.. code-block:: modula2

  Definition := 'CONST' { ConstantDeclaration ';'  }  | 
                'TYPE' { Ident ( ';'  | '=' Type Alignment 
                                  ';'  )  }  | 
                'VAR' { VariableDeclaration ';'  }  | 
                DefProcedureHeading ';' 
              =: 
  Definition (ebnf)

.. code-block:: modula2

  AsmStatement := 'ASM' [ 'VOLATILE'  ] '(' AsmOperands 
                  ')' 
                =: 
  AsmStatement (ebnf)

.. code-block:: modula2

  NamedOperand := '[' Ident ']' 
                =: 
  NamedOperand (ebnf)

.. code-block:: modula2

  AsmOperandName := [ NamedOperand  ] 
                  =: 
  AsmOperandName (ebnf)

.. code-block:: modula2

  AsmOperands := string [ ':' AsmList [ ':' AsmList [ 
     ':' TrashList  ]  ]  ] 
               =: 
  AsmOperands (ebnf)

.. code-block:: modula2

  AsmList := [ AsmElement  ] { ',' AsmElement  } 
           =: 
  AsmList (ebnf)

.. code-block:: modula2

  AsmElement := AsmOperandName string '(' Expression 
                ')' 
              =: 
  AsmElement (ebnf)

.. code-block:: modula2

  TrashList := [ string  ] { ',' string  } 
             =: 
  TrashList (ebnf)

