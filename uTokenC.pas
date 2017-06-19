unit uTokenC;
(* C preprocessor tokens.
Eventually add the TTokenStream base class and a TToken object?
Eventually redesign RToken/TToken:
  Optimize strings a bit more:
    String literals without escape sequences can refer to the source text,
      reducing string table size.

  Add whitespace following the token?

V2: prepare for remapping of ambiguous tokens (opMul... not yet used)
V1: include C keywords, with some Microsoft and C-99 extensions.
*)

interface

{$I config.pas}

type
(* the following pseudo tokens are defined:
  t_empty:  token already processed.
  t_eof:    end of file or token stream
  t_bol:    begin of line (line number, indentation...)
  t_NoEol:  escaped end of line
  t_rem:    comment (if requested?)
  t_err:    invalid chars in source, or #error
  t_str..t_flt:  literals
  t_sym:    symbol, identifier, possibly keyword or macro
  t_arg:    macro argument, reference inside macro body
  t_symNX, t_argNX:  don't expand this symbol or argument, because:
    macro name inside macro body (sym)
    macro already expanding, called without required arguments...
    left hand side of ## operator (sym or arg)
    right hand side of # operator (arg)
*)
  eToken = (
    t_empty,
    t_eof,
    t_bol,  //begin of line, + file and line numbers
    t_NoEol,  //dto., escaped (continuation line)
    t_rem,
    t_err,
    t_str, //string literal "s", L"s", header name, + ID
    t_car, //char 'c', L'c', + chars/cval
{$IFDEF t_num}
    t_num,  //unclassified preprocessor number (not yet implemented)
{$ENDIF}
    t_int, t_Lint, //int number, + value
    t_flt, //real number, + value
    t_sym, t_symNX,  //symbol, + ID
    t_arg, t_argNX,
  //parser specific, mapped from t_sym
    //t_type, t_const,
  //preprocessor operators
    opSharp, op2Sharp, opSharpAt, // # ## #@
  //operators - sorted within lines
{$IFDEF opMul}
    opAmpersAnd, letAND, logAND, // & &= &&, ambiguous: &
{$ELSE}
    binAnd_, letAND, logAND, // & &= &&, ambiguous: &
{$ENDIF}
    opADD, letADD, opINC,   // + += ++
    opLT, opLE, opSHL, letSHL, // < <= << <<=
    opGT, opGE, opSHR, letSHR, // > >= >> >>=
    binOR, letOR, logOR,    // | |= ||

    logNOT, opNE,   // ! !=
    opMOD, letMOD,  // % %=
{$IFnDEF opMul}
    opStar_, letMUL, // * *=, ambiguous: *
{$ELSE}
    opStar0, letMUL, // * *=, ambiguous: *
  //replacements
    opMul2, opPtr1, opDeref1, //classified "*" (binary mul, ptr decl., dereference)
    binAnd2, opAddr1,   //classified "&" (unary)
    opSub2, opMinus1,  //classified "-" (unary)
{$ENDIF}
    opLet, opEQ,    // = ==
    opXor, letXOR,  // ^ ^=

    opLPar, opRPar, // ( )
    opComma, opColon, opSemi, // , : ; C++: ::
    opTERN, // ?
    opLBra, opRBra, // [ ]
    opBeg, opEnd,   // { }
    binNot,  //~
  //not in op1$
    opDot, op3Dot, // . ..., C++: .*
    opDiv, letDiv, OpDivDiv, // / /= //     {RP}
{$IFDEF opMul}
    opSub0, letSub, opDec, opTo, // - -= -- ->, C++: ->*
{$ELSE}
    opSub_, letSub, opDec, opTo, // - -= -- ->, C++: ->*
{$ENDIF}
  //keywords, sorted
  //operators
    Ksizeof,
  //statements
    Kbreak, Kcase, Kcontinue, Kdefault, Kdo,
    Kelse, Kfor, Kgoto, Kif,
    Kreturn, Kswitch, Kwhile,
  //storage classes: ? ? ? "!" *
    Kauto, Kextern, Kregister, Kstatic, Ktypedef,
  //calling convention: "I"
    Kinline,
  //complex types
    Kenum, Kstruct, Kunion,
  //simple types, somewhat sorted by size
    Kvoid, Kchar, {Kwchar_t,}
    Kint, {Kwint_t,} Kfloat, Kdouble,
    Kint8_t, Kint16_t, Kint32_t, Kint64_t,
    Kuint8_t, Kuint16_t, Kuint32_t, Kuint64_t,
  //type modifiers (size)
    Klong, Kshort,
  //type modifiers (signedness) "-", "+"
    Ksigned, Kunsigned,
  //cv-qualifiers  "#", "V"
    Kconst, Kvolatile,
  //Windows relevant! "C", "F", "" + "I"=inline
    Kcdecl, Kfastcall, Kstdcall
(*
  //extensions: Microsoft/Windows
    K__asm,
    K__based,
    K__cdecl,
    K__declspec,
    K__except,
    K__fastcall,
    K__finally,
    K__inline,
    K__int8,
    K__int16,
    K__int32,
    K__int64,
    K__stdcall,
    K__try,
    Kdllexport,
    Kdllimport,
    Knear,
    Kfar,
    Knaked,
    Kpascal,
    Kthread
*)
  );
(* Note:
  The definition of recognized keywords should be up to the application.
  For a simple mapping from preprocessor tokens into parser keywords
  it's desireable that all keywords are contiguous,
  regardless of the language (C/C++) and extensions.
*)
  eScannerTokens = t_empty..opTo; //created by scanner, everything else by parser

  //eLangKeys = Kauto..Kvolatile;
  //eLangKeys = Kauto..Kpascal;
  //eLangKeys = Kauto..Kfastcall;
  eLangKeys = Ksizeof..Kstdcall;
{$IFDEF extStorage}
  eStorageClass = Kauto..Kunion;
{$ELSE}
  eStorageClass = Kauto..Ktypedef;
{$ENDIF}
  eSimpleTypes = Kvoid..Kuint64_t;

const //for the parser!
  storage_class_specifierS = [Kauto..Ktypedef];
    //Kauto, Kextern, Kregister, Kstatic, Ktypedef,
    //Kinline,
  extended_storage_class_specifierS = storage_class_specifierS + [Kinline];

    //??? friend, mutable, thread, naked, dllimport, dllexport

  calling_conventionS = [Kcdecl..Kstdcall];
    //Kcdecl, Kfastcall, Kinline, Kstdcall
  cv_qualifierS = [Kconst, Kvolatile];
    //Kconst, Kvolatile,
  type_qualifierS = cv_qualifierS + calling_conventionS;  // [Kconst..Kstdcall];

  type_modifierS = [Klong, Kshort, Ksigned, Kunsigned];

  simple_type_specifierS = [Kvoid..Kuint64_t];
  {simple types, somewhat sorted by size:
    Kvoid, Kchar, //Kwchar_t, Kwint_t,
    Kint, Kfloat, Kdouble,
    Kint8_t, Kint16_t, Kint32_t, Kint64_t,
    Kuint8_t, Kuint16_t, Kuint32_t, Kuint64_t,
  }
  complex_type_specifierS = [Kenum, Kstruct, Kunion];

const //meta convention(s)
  //opType = opSharp;   //#typename - unquoted typename!
  opCast = opSharpAt; //#@(#typename,expr);
  opPostInc = opInc;  //PreInc = letAdd;
  opPostDec = opDec;  //PreDec = letSub;

const
{$IFDEF opMul}
  cast_modifierS = [ opStar0, opPtr1, opAddr1 ];
{$ELSE}
  cast_modifierS = [ opStar_ ];
{$ENDIF}
  declarator_qualifierS = type_qualifierS + cast_modifierS;

const
  //MaxTokenKind = pdPragma; //update if enum changed!
  //MaxTokenKind = Kvolatile; //update if enum changed!
  MaxTokenKind = Kstdcall; //update if enum changed!

//t_eof never must be ignored. t_bol is significant for #define (only).
  WhiteTokensPrep = [t_empty, t_NoEol, t_rem]; //not EOF, not t_bol
  WhiteTokens = WhiteTokensPrep + [t_bol];
{$IFDEF opMul}
  ConstTokens = [t_empty..t_NoEol, opAmpersAnd..MaxTokenKind];
{$ELSE}
  ConstTokens = [t_empty..t_NoEol, binAnd..MaxTokenKind];
{$ENDIF}

//allow for duplicate op (& -> && etc.)
{$IFDEF opMul}
  DupOps = [opAmpersAnd, opAdd, opSub0, opLT, opGT, binOR];
{$ELSE}
  DupOps = [binAnd, opAdd, opSub, opLT, opGT, binOR];
{$ENDIF}
  //C++: "::"?
//also allow for assignment (& -> &= etc.)
  LetOps = [
{$IFDEF opMul}
    opAmpersAnd, opADD,
    opSub0, //special handling for "->"
{$ELSE}
    binAnd_, opADD,
    opSub_, //special handling for "->"
{$ENDIF}
    opLT, opSHL, opGT, opSHR,   //both single and duplicated
    binOR,
    logNOT,
    opMOD,
    opDiv, //special handling for "//" and "/*"
{$IFDEF opMul}
    opStar0,
{$ELSE}
    opStar_,
{$ENDIF}
    opLet,
    opXor
  ];

  TokenNames: array[eToken] of string = (
    '<empty>',  //token consumed, retry get next token
    '<eof>',  //no more valid tokens, this file or macro
    '<bol>',
    '<NoEol>',  //escaped
    '<rem>',
    '<error>',
    '<string>', //'<Lstring>',
    '<char>', //'<Lchar>',
{$IFDEF t_num}
    '<num>',
{$ENDIF}
    '<int>', '<int64>',
    '<flt>',
    '<sym>', '<Csym>',  //symbols
    '<arg>', '<(arg)>',
  //parser specific, mapped from t_sym
    //'<typename>', '<constname>',
  //preprocessor operators
    '#', '##', '#@',
  //operators - sorted!
    '&', '&=', '&&',
    '+', '+=', '++',
    '<', '<=', '<<', '<<=',
    '>', '>=', '>>', '>>=',
    '|', '|=', '||',
    '!', '!=',
    '%', '%=',
    '*', '*=',
{$IFDEF opMul}
  //replacements
  //unless better encoding: post-processor must count number of operands!
  //enclose binary operators in " "
    ' * ', ' *', '->', //opMul, opPtr, opDeref, //classified "*"
    ' & ', ' &',  //binAnd, opAddr,   //classified "&" (unary)
    ' - ', ' -',  //opSub, opMinus,  //classified "-" (unary)
{$ENDIF}
    '=', '==',
    '^', '^=',
    '(', ')',
    ',', ':', ';',
    '?',
    '[', ']',
    '{', '}',
    '~',
  //not in op1$
    '.', '...',
    '/', '/=', '//',
    '-', '-=', '--', '->',
(*
    //'defined',
  //predefined macros
    //'__FILE__', '__LINE__',
  //preprocessor symbols
    '#endif', {'#if',} '#ifdef', '#ifndef', '#elif', {'#else',}
    '#include', '#include_next',
    '#define', '#undef',
    '#error', '#file', '#pragma',
  //overlapping
    'if', 'else'
*)
  //C keywords, sorted
  //operators
    'sizeof',
  //statements
    'break', 'case', 'continue', 'default', 'do',
    'else', 'for', 'goto', 'if',
    'return', 'switch', 'while',
  //storage classes
    'auto', 'extern', 'register', 'static', 'typedef',
    '__inline', 
  //complex types
    'enum', 'struct', 'union',
  //simple types
    'void', 'char', {'wchar_t',}
    'int', {'wint_t',} 'float', 'double',
    '__int8', '__int16', '__int32', '__int64',
    '__uint8', '__uint16', '__uint32', '__uint64',
  //type modifiers (size)
    'long', 'short',
  //type modifiers (signedness)
    'signed', 'unsigned',
  //cv-qualifiers
    'const', 'volatile',
  //Windows relevant!
    '__cdecl', '__fastcall', '__stdcall'
(*
  //add more "standard" data types, like __int8, __uint8?
  //extensions: Microsoft/Windows
    //'cdecl', 'pascal', 'far', 'huge', 'near'
    '__asm',
    '__based',
    '__cdecl',
    '__declspec',
    '__except',
    '__fastcall',
    '__finally',
    '__inline',
    '__int8',
    '__int16',
    '__int32',
    '__int64',
    '__stdcall',
    '__try',
    'dllexport',
    'dllimport',
    'near',
    'far',
    'naked',
    'pascal',
    'thread'
*)
  );

type
(* various token attributes, interpretation depending on token type
  (see constants below)
taWhiteBefore:
  Possibly should be changed into taWhiteAfter,
    to simplify attaching rems to the final C tokens? Intended usage:
      while taWhiteAfter do token.rem := token.rem + TokenString; nextToken;
    (requires some overhead in the scanner!)
taLong:
  Long integer, Wide string or char, double
taLongLong:
  Int64(?), long double = extended
taRemBeg, taRemEnd:
  Begin and end of comment, for parts of multi-line comments
iaBase8, iaBase16:
  Support number format conversion in a cross compiler. Default base = decimal.
*)
  eTokenAttrs = (
    taWhiteBefore,  //token preceded by whitespace - followed?
    taUnsigned, //int
    taLong, taLongLong, //int, flt, char, string
    taRemBeg, taRemEnd,
    iaBase8, iaBase16 //int base
  );
  sTokenAttrs = set of eTokenAttrs;
const //alias for special token kinds
  faDbl = taLong; //=saWide
  faLDbl = taLongLong;
  raEOL = [taRemBeg, taRemEnd];
  saSysHdr = taUnsigned;
  //saRule = taRemBeg;  //syntax tree node - what token kind?

type
  RPreToken = packed record
    pc:   PChar;  //token start in source file (file.Strings[linenum])
    len:  word;   //and token length, for display and other purposes
    attrs:  sTokenAttrs;
    case kind: eToken of
    t_bol, t_NoEol: (f: integer; line: integer);   //+ file,line
    t_str:  (fShort: LongBool; stringID: integer); //string literal, + index
    	//if fShort then use t_car chars, else strID
    t_car:  (dummy: array[1..3] of byte; chars: string[4]);
    //t_num,
    t_rem:  (uval, cval: cardinal);  //cval overlays chars in t_car.chars
    t_int:  (ival: integer);
    t_Lint: (lval: int64);
    t_flt:  (d: extended);  //longest argument - use float table?
    t_sym, t_symNX: (symID, typeID: integer); //dict. index
      //symID -> Symbols, typeID -> Types, ruleID -> Rules
    t_arg, t_argNX: (argID: integer); //macro argument index
  end;
  PPreToken = ^RPreToken;

function  IntToOct(v: integer): string;


implementation

function  IntToOct(v: integer): string;
const
  NullChar = ord('0');
begin
  Result := '';
  //assert(v >= 0, 'invalid oct const'); no matter here
  while v > 0 do begin
    Result := char(NullChar + (v and 7)) + Result;
    v := v shr 3; //does an UNSIGNED shift!
  end;
  Result := '0' + Result;
end;

end.
