unit uToPas;
(* C to Delphi type converter.
V3: multi-purpose class
V2: TFileMeta scanner class.
V1: all declarations, but no initialization of complex data structures.
V0: to clarify: some strange (unnamed) members.
ToDo:
- last procedure is not converted?
- convert type "void" into what? (void* is Pointer, but void for itself?)
- double {const} should be separated by something else.
- classify literals and constants!!!
  string <> char <> int(base?) ...

- show macros in implementation section.
+ remap ambiguous names, case insensitive! (x <-> X, type <-> Delphi keyword...)
+ compress: struct in union (at least if unnamed!)
  U{:S{...}} -> case 0: (...);
+ array dim [0..n-1]
*)

(* Problems in WinTest.c
S__IMAGE_RESOURCE_DIRECTORY_ENTRY
  record with multiple case (variant) sections.
S__devicemodeA
  dto., embedded union
*)

{ TODO :
show Pascal strings (cvt in *.txt loader from C to Ansi???)
enum members - nostore as constants - recover/make constants }

interface

{$I config.pas}

{ $DEFINE MacroObjects}  //undef requires Globals.DefFile

uses
  Classes,
  uTranslator,
  uTokenC, uTablesC;

const //todo: show macros from new deffile format!!!
  ShowMacs = False; //format problem!!!

//procedure ToPas(const fn: string);
procedure ToPas(const fn: string; fSort: boolean = false);
//procedure CheckNames(filename: string);
//function  ExpressionString(leftOp: eToken): string;
{$IFDEF old}
function  PascalStringLit(const s: string): string;
{$ELSE}
function  AnsiToPascal(const s: string): string;
function  AnsiToP(const s: string; delim: char; fLong: boolean): string;
  //callback for TokenString
{$ENDIF}

{$IFDEF new}
function  CheckMeta(mf: TModule): boolean;
//function  CheckMeta(const fn: string): boolean;
{$ELSE}
{$ENDIF}

type
  ePrjType = (
    ptUnit, //default, create an standard unit *.pas
    ptPrg,  //create a main (program) unit *.dpr
    ptLib,  //create a library, external... -> *.pas
    //ptObjLib, //library from object file, $L -> *.pas
    ptPkg   //package, requires, contains -> *.pkg
  );

  eUnitType = (
    utUnit, //may vary according to PrjType?
    //utForm, - not yet
    utImport, //for header file
    utPrg   //program main file
  );

  eSectionType = (
  //utPrg
    stProgram,
  //utLib
    stExports,
  //utPkg
    //stRequires, stContains
  //utUnit
    stIntf, stImpl,
    stInit, stFinal
  );

//special interface
  TToPascal = class(TTranslator)
  public
    PrjType:  ePrjType;
      //default: create unit
    UnitType: eUnitType;
      //default: create unit
    SectionType:  eSectionType;
    LibName:  string; //for external references
  //standard projects
    function  hToPas(const AFileName, ALibName: string): boolean;
      //create import module for header file
      //determine library how???
    function  ToUnit(mf: TModule): boolean;
  //more to come?
    //function  ToPrg(mf: TModules?): boolean;
    //function  ToLib(mf: TModules?): boolean;
    //function  ToPkg(mf: TModules?): boolean;
  end;

var //access Delphi extensions
  XToPas: TToPascal;

implementation

uses
  SysUtils, StrUtils,
  uXStrings, uHashList, uUI,
  uScanC, uParseC,
  uTablesPrep, uMacros;

(* Handle external references
1) when building a library module?
  todo...
2) imported by $L
  todo...
3) reference to external library
  Assume 1 library, or symbols sorted by library.
  Create a constant for the library name,
    for constant reference expressions.
*)
const //external __lib;
  LibRef = '__lib';

type
  ePasOp = (
    poDone, //end of list...
    poStop, //not part of expression
    poSep,  //expression separator
    poIgnore, //comments etc.
    poErr,  //unxepected or impossible to translate
    poInt,  //integral value
    poStr,  //string
    poLit,  //literal (no interpretation)
    poSym,  //symbol, possibly function call,
  //operators
    poPrefix, //prefix operator, e.g. not <expr>
    poInfix,  //infix operator, e.g. <expr> + <expr>
    poPostfix,  //<ptr>^
    poFunc,   //function operator, e.g. inc(<expr>)
    poCall,   //((proc,args)
    poType,   //#typename - unquoted typename!
    poCast,   //#@(#typename,expr)
    poArray   //"[" begin of array selection "["
  );

  eOperatorPrecedence = (
    prLowest,
    prRelOp,  // =, <>, <, >, <=, >=, [as,]? in, is
    prAddOp,  // +, �, or, xor
    prMulOp,  // *, /, div, mod, and, shl, shr, as, [in, is]?
    prUnary,  // @, not, -, (
		prSelect,	//.
    prHighest // function?
  );

  RPasOp = record
    n:  string;
    t:  ePasOp;
    p:  eOperatorPrecedence;
    m:  eToken; //binop as monop
  end;
(* Operator priority - help text
highest..lowest
5 "(" expression enclosed by parentheses
? ., ^
4 @, not
3 *, /, div, mod, and, shl, shr, as
2 +, �, or, xor
1 =, <>, <, >, <=, >=, in, is
0 (top expression)

Grammar:
  as, in, is - RelOp
*)
const
  aPasOp: array[eScannerTokens] of RPasOp = (
    (n:''; t:poIgnore), //t_empty,
    (n:''; t:poStop), //t_eof,
    (n:''; t:poIgnore), //t_bol,  //begin of line, + file and line numbers
    (n:''; t:poIgnore), //t_NoEol,  //dto., escaped (continuation line)
    (n:''; t:poIgnore), //t_rem,
    (n:''; t:poErr), //t_err,
    (n:''; t:poStr; p:prUnary), //t_str, //string literal "s", L"s", header name, + ID
    (n:''; t:poStr; p:prUnary), //t_car, //char 'c', L'c', + chars/cval
{$IFDEF t_num}
    (n:''; t:poErr), //t_num,  //unclassified preprocessor number (not yet implemented)
{$ENDIF}
    (n:''; t:poInt; p:prUnary), //t_int,
    (n:''; t:poInt; p:prUnary), //t_Lint, //int number, + value
    (n:''; t:poLit; p:prUnary), //t_flt, //real number, + value
    (n:''; t:poSym; p:prUnary), //t_sym,
    (n:''; t:poErr), //t_symNX,  //symbol, + ID
    (n:''; t:poErr), //t_arg,
    (n:''; t:poErr), //t_argNX,
  //parser specific, mapped from t_sym
    //t_type, t_const,
  //preprocessor operators
    (n:'???'; t:poType), //opSharp,
    (n:''; t:poErr), //op2Sharp,
  //!!!if opCast=opSharpAt!!!
    (n:'???'; t:poCast), //opSharpAt, // # ## #@
    //(n:''; t:poErr), //opSharpAt, // # ## #@
  //operators - sorted within lines
    (n:' and '; t:poInfix; p:prMulOp; m:opAddr1), //& binAnd/opAddr
    (n:'__and'; t:poFunc), //&= letAND,
    (n:' and '; t:poInfix; p:prMulOp), //&& logAND,
    (n:'+'; t:poInfix; p:prAddOp), //+ opADD,
    (n:' inc'; t:poFunc), //+= letADD,
    (n:' __postinc'; t:poFunc), //++ opINC: postfix! prefix as +=(x,1)
    (n:'<'; t:poInfix; p:prRelOp), //opLT,
    (n:'<='; t:poInfix; p:prRelOp), //opLE,
    (n:' shl '; t:poInfix; p:prMulOp), //opSHL,
    (n:' __shl'; t:poFunc), //letSHL, // < <= << <<=
    (n:'>'; t:poInfix; p:prRelOp), //opGT,
    (n:'>='; t:poInfix; p:prRelOp), //opGE,
    (n:' shr '; t:poInfix; p:prMulOp), //opSHR,
    (n:' __shr'; t:poFunc), //letSHR, // > >= >> >>=
    (n:' or '; t:poInfix; p:prAddOp), //binOR,
    (n:' __or'; t:poFunc), //letOR,
    (n:' or '; t:poInfix; p:prAddOp), //logOR,    // | |= ||

    (n:' not '; t:poPrefix; p:prUnary), //logNOT,
    (n:'<>'; t:poInfix; p:prRelOp), //opNE,   // ! !=
    (n:' mod '; t:poInfix; p:prMulOp), //opMOD,
    (n:' __mod'; t:poFunc), //letMOD,  // % %=
    (n:'*'; t:poInfix; p:prMulOp; m:opDeref1), //* opStar/opDeref1 - ambiguous: mul or deref?
      //deref as opTo "->"
    (n:' mul'; t:poFunc), //*= letMUL,
//!!! we don't have disambiguated tokens here!!!
//unless the writer emitted special codes...
{$IFDEF opMul}
  // *
    (n:'*'; t:poInfix; p:prMulOp), //opMul,
    (n:'^'; t:poPrefix; p:prUnary), //opPtr, //classified "*"
    (n:'^'; t:poPostfix; p:prUnary), //opDeref1, //classified "*"
  // &
    (n:' and '; t:poInfix; p:prMulOp; m:opAddr1), //binAnd,
    (n:'@'; t:poPrefix; p:prUnary), //opAddr,   //classified "&" (unary)
  // -
    (n:'-'; t:poInfix; p:prAddOp; m:opMinus1), //- opSub/opMinus1
    (n:'-'; t:poPrefix; p:prUnary), //opMinus1,  //classified "-" (unary)
{$ENDIF}
    (n:':='; t:poInfix), //opLet,
    (n:'='; t:poInfix; p:prRelOp), //opEQ,    // = ==
    (n:' xor '; t:poInfix; p:prAddOp), //opXor,
    (n:' __xor'; t:poFunc), //letXOR,  // ^ ^=

  {$IFDEF ExprTerm}
  //old!!!
    (n:''; t:poList; p:prHighest), //opLPar,
    (n:''; t:poErr), //opRPar, // ( )
    (n:', '; t:poLit; p:prUnary), //opComma,
    (n:': '; t:poErr), //opColon: label!
    (n:''; t:poDone), //opSemi, // , : ; C++: ::
  {$ELSE}
    {$IFDEF old}
      (n:''; t:poStop), //opLPar,
    {$ELSE}
      (n:'('; t:poCall; p:prLowest), //opLPar,  //prio???
    {$ENDIF}
    (n:''; t:poStop), //opRPar, // ( )
    (n:''; t:poSep), //opComma: consume and exit
    (n:''; t:poStop), //opColon: label!
    (n:''; t:poStop), //opSemi, // , : ; C++: ::
  {$ENDIF}
    (n:' __if'; t:poFunc), //opTERN, ?(cond,t,f)
    (n:''; t:poArray; p:prLowest), //opLBra,
  {$IFDEF ExprTerm}
    (n:''; t:poErr), //opRBra, // [ ]
    (n:''; t:poErr), //opBeg,
    (n:''; t:poErr), //opEnd,   // { }
  {$ELSE}
    (n:''; t:poStop), //opRBra, // [ ]
    (n:''; t:poStop), //opBeg,
    (n:''; t:poStop), //opEnd,   // { }
  {$ENDIF}
    (n:' not '; t:poPrefix; p:prUnary), //binNot,  //~
  //not in op1$
    (n:'.'; t:poInfix; p:prSelect), //opDot,
    (n:''; t:poErr), //op3Dot, // . ..., C++: .*
    (n:'/'; t:poInfix; p:prMulOp), //opDiv,
    (n:' __div'; t:poFunc), //letDiv, // / /=
    (n:''; t:poIgnore), //opDivDiv - rem!!!
    (n:'-'; t:poInfix; p:prAddOp), //opSub,
    (n:' dec'; t:poFunc), //letSub, -= or --arg!!!
    (n:' __postdec'; t:poFunc), //-- opDec: postfix! prefix: -=(x,1)
    (n:'^.'; t:poInfix; p:prSelect; m:opDeref1) //opTo, // - -= -- ->, C++: ->*
  );

(* AnsiToPascal - convert AnsiString into Pascal format.
(* PascalStringLit - convert meta string into Pascal format.
Control characters are inserted as hex character codes (#$), without '+'.
Quote chars are doubled.
Note: when the parts are concatenated with '+', then this operator must not
  occur before the first part of the string! (when the first char is a ctrl)
ToDo: handle wide strings/chars, including "\u" escaped UCNs.
  Add parameter: fWide?
  Cast into WideString/WideChar? - requires knowledge about t_str or t_car!
*)
function  AnsiToPascal(const s: string): string;
var
  i, d, nc, l: integer;
  c: char;
  fStr: boolean;
const
  HexChar: array[0..15] of char = '0123456789ABCDEF';
begin
  l := Length(s);
  nc := 0;
  for i := 1 to l do
    if (s[i] < ' ') or (s[i] = '''') then
      inc(nc);
  if nc > 0 then begin
    SetLength(Result, l + nc*6 + 2);
    fStr := False;  //not in string
    d := 1;
    for i := 1 to Length(s) do begin
      c := s[i];
      if c < ' ' then begin
        if fStr then begin
          Result[d] := ''''; inc(d);  //terminate string
          fStr := False;
        end;
        Result[d] := '#'; inc(d);
        Result[d] := '$'; inc(d);
        if c < #16 then begin
          Result[d] := '0'; inc(d);
          Result[d] := HexChar[ord(c)]; inc(d);
        end else begin
          Result[d] := '1'; inc(d);
          Result[d] := HexChar[ord(c) and $F]; inc(d);
        end;
      end else begin
        if not fStr then begin
          Result[d] := ''''; inc(d);
          fStr := True;
        end;
        if c = '''' then begin
          Result[d] := c; inc(d); //double quote char
        end;
        Result[d] := c; inc(d);
      end;
    end;
    if fStr then begin
      Result[d] := ''''; inc(d);  //terminate the string
    end;  //else ctrl char needs no termination
    SetLength(Result, d-1);
  end else begin
    Result := '''' + s + '''';  //simply quote the whole string
  end;
end;

function  AnsiToP(const s: string; delim: char; fLong: boolean): string;
begin
  Result := AnsiToPascal(s);
  if fLong then begin
  //unicode cast
    if delim = '''' then
      Result := 'WideChar(' + Result + ')'
    else
      Result := 'WideString(' + Result + ')'
  end;
end;


//const  digits = ['0'..'9']; in uScanC

type
  eSU = (inNone, inStruct, inUnion);

//Delphi translator
  //TToPas = class(TTranslator)
  TToPas = class(TToPascal)
  private
    procedure WriteInc(const s: string);
    procedure WriteName(d1, d2: char);
    function  Unquoted(delim: char): string;
  protected //type related, pc based parser!!!
    typeswritten: boolean;  //standard types
    procedure TypeSection;  //also write default types...
    procedure WriteTypeRef;
    procedure WriteArray;
    procedure WriteBitfield;
    procedure WriteEnum;
    procedure WriteExprPc;
    procedure WritePointer;
    procedure WriteProcType;
      procedure WriteArgumentList;
    procedure WriteSized(fSigned: boolean; len: char);
    procedure WriteStruct(fIn: eSU);
    procedure WriteTypePc(fIn: eSU);
    procedure WriteUnion(fIn: eSU);
    procedure WriteUnSigned(fSigned: boolean);
  protected //in file order
    //fImpl: boolean; //write implementation?
    curkind: eSymType;  //section: const, var, type, proc
    fLibName: boolean;
    procedure WriteHeader;  //unit...
    procedure WriteIntf;
    procedure WriteImpl;
  protected //by symbol type
    procedure WriteConstSym;
      procedure ConstSection;
    procedure WriteVarSym;
      procedure VarSection;
    procedure WriteProcSym;
      procedure WriteParams;
      //procedure WriteLocal; //(ScanText)
      procedure WriteProcBody;
    procedure WriteTypeSym;
  protected //expressions...
    procedure WriteType;
    procedure WriteExpr;
      function  ExpressionString(leftOp: eToken): string;
  protected //from function style translator
    //s: string;  //strictly temporary string
    typ: TSymType;
    proc: TSymProc;
    sym:  TSymbolC; //from Scanner, on t_sym
    pc: PChar;  //in typedef
    function  nextToken: eToken;
    function  expect(t: eToken; const msg: string): boolean;
    function  skip(t: eToken): boolean;
    procedure InitSrc; override;
  public
    //function  ToPasLib(mf: TModule): boolean;
    function  Translate: boolean; override;
    function  TranslateSym(ASym: TSymbolC): boolean; override;
  end;

{ TToPascal }

function TToPascal.hToPas(const AFileName, ALibName: string): boolean;
begin
  ForHeader(AFileName);
  if ALibName = '' then begin
  //a name should always be specified!
    self.LibName := self.ModuleName + '.dll' //???
  end else
    LibName := ALibName;  //must include '.dll'!?
  self.PrjType := ptUnit;
  self.UnitType := utImport;  //using LibName
//todo: handle macros and imports properly
  Result := Self.Translate;
end;

function TToPascal.ToUnit(mf: TModule): boolean;
begin
  ForModule(mf);
  self.PrjType := ptUnit;
  self.UnitType := utUnit;
//todo: handle macros and statics properly
  Result := Translate;
end;

{ TToPas }

procedure TToPas.InitSrc;
begin
//from Translate
  inherited;
  //if filename = '' then filename := 'KDelphi.txt';
  CheckNames('KDelphi.txt');
  if Target = nil then begin
    Target := TStringList.Create;
    fOwnDst := True;
  end else
    Target.Clear;
end;

function TToPas.Translate: boolean;
begin
  inherited Translate;
  case PrjType of
  ptUnit:
    begin
      WriteHeader;
      WriteIntf;
    //ToDo: Macros should go to both interface and implementation!!!
      WriteImpl;
      Result := True;  //nothing done, for now
    end;
  //ptPrg,
  else
    Result := False;  //not yet implemented
  end;
end;

function TToPas.TranslateSym(ASym: TSymbolC): boolean;
begin
  Result := Asym <> nil;
  if not Result then
    exit;
  self.sym := ASym;
  case sym.kind of
  stConst:      WriteConstSym;
  stEnumMember: if fDebugEnums then WriteConstSym;
  stVar:        WriteVarSym;
  stProc:       WriteProcSym;
  stTypedef:    WriteTypeSym;
  end;
end;

function  TToPas.nextToken: eToken;
var  i: integer;
begin
  Result := Scanner.nextRaw;  //no preprocessing...
//map symbols??? at least C keys
  if (Result = t_sym) then begin
    if (ScanSym.mackind = skCKey) then
      Result := ScanSym.appkind
    else begin
    //map symbol
      sym := Globals.getSym(ScanText);
      if sym <> nil then begin
      //global sym?
        //AllSyms.AddSym(sym); //already done in initialization?
      end else begin
        i := Pos('$', ScanText);
        if i > 0 then
          ScanText[i] := '_';
      end;
    end;
  end;
  i_ttyp := Result;
end;

function  TToPas.skip(t: eToken): boolean;
begin
  Result := i_TTYP = t;
  if Result then
    nextToken;
end;

function TToPas.expect(t: eToken; const msg: string): boolean;
begin
//Result := skip(t);
  Result := i_ttyp = t;
  if Result then
    nextToken
  else
//if not Result then
    Log(msg, lkSynErr);
end;

procedure TToPas.WriteHeader;
begin
  indent := 0;  //fix possible garbage from preceding aborted run
  case PrjType of
  ptUnit:
    begin
      WriteLn('unit ' + self.ModuleName + ';');
      WriteLn(' ');
      WriteLn('interface');
      //WriteLn(' ');
    //init output
      curkind := stUnknown; //-> insert empty line before diff. section
      typeswritten := False;
      //self.fImpl := False;  //unless program!???
      self.SectionType := stIntf;
    end;
  ptPrg:
    begin
      WriteLn('program ' + self.ModuleName + ';');
      self.SectionType := stProgram;
    end;
  else
    Log('unhandled project type', lkTodo);
  end;
end;

procedure TToPas.WriteInc(const s: string);
begin
  Write(s);
  inc(pc);
end;

(* WriteName - write name, delimited by given char(s)
Called for member names in .Def strings - assume scope $1?
  Problem: multiple delimiters are possible!
  Better scan for valid chars? Problem with embedded ":"???
*)
procedure TToPas.WriteName(d1, d2: char);
var
  n : string;
  pd: PChar;
begin
  pd := pc;
  //while not (pc^ in delims) do
  while (pc^ <> d1) and (pc^ <> d2) do
    inc(pc);
  SetString(n, pd, pc - pd);  //assume delim is not the first char!

//don't unify local names (for now)
  if fUniqueNames then begin
    if (Pos('$', n) <= 0) then
    //external symbol?
      Write(UniqueName(n))
    else
      Write(n {+ '$1'});  //expect member name???
  end else begin
    Write(n);
  end;
end;

(* ExpressionString - expression as string
  Either expect an argument (identifier...), without special termination,
  or an expression op(lhs[,rhs])
  or exit with an empty result.
Operators consume up to and including the trailing ")".
  A rhs is indicated by a "," (ExprSep).
Without a rhs, prefix and postfix work as expected/designed.
  A infix operator without rhs can be transformed, into op.m.

Sometimes more situations should be distinguished:
  := only at top (stmt) level, else fn: __assign...(lhs,rhs) for all assign ops.
  ! only "not" in general expressions, in boolean: lhs <> 0.
*)
function  TToPas.ExpressionString(leftOp: eToken): string;

//!!! this is an argument list (only)!!!
//other expr-lists must be translated into multiple stmts!
  function  ExprList: string;
  begin
    Result := ExpressionString(t_empty);
    while i_ttyp = opComma do begin
      nextToken;
      Result := Result + ', ' + ExpressionString(t_empty);
    end;
  end;

var
  op: eToken;
  lhs, rhs: string;
  //s: string absolute Result;
begin //ExpressionString
//i_ttyp is the current token - literal or operator?
(* when poIgnore tokens can occur, a token filter must be used!
*)
  case aPasOp[i_ttyp].t of
  poInt:
    begin
      if iaBase16 in ScanToken.attrs then begin
      //prefix hex numbers with '$' (OPL hex format)?
        if i_ttyp = t_int then
          Result := '$' + IntToHex(ScanToken.uval, 0)
        else  //t_Lint (int64)
          Result := '$' + IntToHex(ScanToken.lval, 0);
      end else begin
      //no octal format!
        if i_ttyp = t_int then
          Result := IntToStr(ScanToken.uval)
        else
          Result := IntToStr(ScanToken.lval)
      end;
      nextToken;
    end;
  poStr:
    begin
      Result := AnsiToP(ScanText, '''', taLong in ScanToken.attrs);
      nextToken;
    end;
  poLit,
  poSym:
    begin
      Result := ScanText;  //TokenString(False);
      if nextToken = opLPar then begin
        nextToken;  //expect(opLPar, 'no argument list');  //skip "("
        if i_ttyp <> opRPar then
          Result := Result + '(' + ExprList + ')';  //stop on ")"
        expect(opRPar, 'no op(...")"');
      end;
    end;
  poPrefix,
  poInfix,
  poPostfix:
    begin
      op := i_ttyp; nextToken;  //.nextRaw;  //why raw?
      Result := aPasOp[op].n;
      expect(opLPar, 'no operand list');
      lhs := ExpressionString(op);
      if skip(opComma) then begin
        rhs := ExpressionString(op);
        Result := lhs + Result + rhs;
      end else begin
        //rhs := '';
      //special conversion: binop as monop!
        if (aPasOp[op].t = poInfix) and (aPasOp[op].m > t_empty) then begin
          op := aPasOp[op].m; //substitute by indicated monop
          Result := aPasOp[op].n;
        end;
        case aPasOp[op].t of
        poPrefix:
          Result := Result + lhs;
        poInfix: //substitute infix used as pre/postfix/func???
        //hint: possibly syntax error
          Result := Result + '{?}(' + lhs + ')'; //syntax-error if no such operator or function!
        poPostfix:
          Result := lhs + Result;
        end;
      end;
      expect(opRPar, 'no op(...")"');
      if aPasOp[op].p < aPasOp[leftOp].p then
        Result := '(' + Result + ')';
    end;
  poFunc: //intrinsic function
    begin //operator name!!!
      op := i_ttyp; nextToken;
      Result := aPasOp[op].n;
      expect(opLPar, 'no argument list');  //skip "("
      if i_ttyp <> opRPar then  //empty arg-list?
        Result := Result + '(' + ExprList + ')';  //stop on ")"
      expect(opRPar, 'no op(...")"');
    end;
  poCall: //explicit call
    begin //proc = first expr
      op := i_ttyp; nextToken;
      expect(opLPar, 'no argument list');  //skip "("
      //Result := aPasOp[op].n;
      Result := ExpressionString(op); //prio???
    {$IFDEF old}
      if i_ttyp <> opRPar then begin  //empty arg-list?
        expect(opComma, 'bad argument list');
        Result := Result + '(' + ExprList + ')';  //stop on ")"
      end;
    {$ELSE}
      if skip(opComma) then
        Result := Result + '(' + ExprList + ')'  //stop on ")"
      else if fEmptyCall then
        Result := Result + '()';  //show empty argument list
    {$ENDIF}
      expect(opRPar, 'no op(...")"');
    end;
  {
  poList: //explicit, if "(" after sym
    begin
      expect(opLPar, 'no argument list');  //skip "("
      Result := '(' + ExprList + ')';  //stop on ")"
      expect(opRPar, 'no op(...")"');
    end;
  }
  poArray:
    begin
      op := i_ttyp; nextToken;
      expect(opLPar, 'no "("args)');
      Result := ExpressionString(op) + '[';  //prio?
      if skip(opComma) then
        Result := Result + ExpressionString(op);
      Result := Result + ']';
      expect(opRPar, 'no op(...")"');
    end;
  //poType: - only expected in typecasts!!! (see just below)
    //dropped!
  poCast:
    begin
      nextToken;
      expect(opLPar, 'no argument list');  //skip "("
    //handle type ref
      //expect(opType, 'no type to cast');
      //WriteType;
      Result := ScanText;
      expect(t_sym, 'no typename');
      expect(opComma, 'no arg","');
    //hint: typecast
      Result := Result + '{as}(' + ExpressionString(t_empty) + ')';
      expect(opRPar, 'no arg")"');
    end;
  else
    Result := TokenNames[i_ttyp];
    //Log('unexpected op: ' + ScanText, lkBug);  //should never occur
    Log('unexpected op: ' + Result, lkBug);  //should never occur
    //Result := ''; //no valid expression?
  end;
end;

procedure TToPas.WriteExpr;
begin
  Write(ExpressionString(t_empty));
end;

(* WriteExprPc - write expression
  called from type writer, use pc!!!
*)
procedure TToPas.WriteExprPc;
var
  s: string;
begin //switch to scanner mode
//init
  assert(scanner <> nil, 'init scanner');
  Scanner.ScanPChar(pc);  //not fully tested - looks good, so far
  s := ExpressionString(t_empty);
  pc := ScanToken.pc; //at delimiting token (")", "]"...
(* Break long strings at ',' list separators.
  Should not break nested lists (unless required).
  Problem: may break string literals with embedded ","!
    try: exclude known string/char delimiters.
*)
  //if Length(s) > 256 then
  if (Length(s) > 256) and not (s[1] in ['''', '#']) then begin
  //problem: casts: WideString(), WideChar() - possibly more?
    s := StringReplace(s, ',', ',' + EOLstr, [rfReplaceAll]);
  end;
  Write(s);
end;

procedure TToPas.WriteArray;
begin //"["
  inc(pc);
  if pc^ = ']' then
  //[] = dynamic array
    Write('array of ')
  else begin
    Write('array[0..');
    WriteExprPc;  //(']');
    Write('-1] of ');
  end;
  inc(pc);  //skip ']'
end;

procedure TToPas.WriteArgumentList;
var
  i: integer; //argument counter, for naming anonymous arguments
  cc: char;
  ci: boolean;
  fVarargs: boolean;

  procedure ShowVarargs;
  begin
  (* How to convert C "..."?
    For internal (cross compiled) procedures "Array of Const" is an
      appropriate replacement, but what for calling external C functions?
    Solution: assume cdecl, append attribute "varargs" (not in D4!)
  *)
    if self.UnitType = utImport then begin
    //assume cdecl...
      fVarargs := True; //append "varargs;"
    end else begin
      //Write('~Array of Const');  //intentionally: produce syntax error
      Write('va_arg: array of const');
    end;
  end;

  procedure WriteParamName;
  var
    n : string;
    pd: PChar;
  begin
    pd := pc;
    //while not (pc^ in delims) do
    while (pc^ <> ':') do
      inc(pc);
    SetString(n, pd, pc - pd);  //assume delim is not the first char!

  //don't unify local names (for now)
  {$IFDEF old}
    Write(n + '$1');
  {$ELSE}
    Write(n);
  {$ENDIF}
  end;

begin
  i := 0;
  fVarargs := False;
  while pc^ <> ')' do begin
  //write parameter
    if pc^ = ':' then begin
    //unnamed parameter - varargs?
      if pc[1] = '~' then begin
      //":~,)" = varargs
      end else if (pc[1] = 'v') and (pc[3] = ')') then begin
      //":v,)" is empty argument list - should no more occur?
        { DONE : f(void) - fix in parser }
        assert(False, 'empty paramlist'); //assume done
        inc(pc, 3);
        break;
      end else
        Write('arg_' + IntToStr(i));
    end else begin
      WriteParamName; //(':', #0);
    end;
    inc(i); //argument counter
  //parameter type
    inc(pc);  //skip ":"
    Write(': ');
  //check special cases: varargs
    if (pc^ = '~') then begin
      ShowVarargs;
      inc(pc);
    end else if (StrLComp(pc, '"va_list"', 9) = 0) then begin
    //what's this good for?
      ShowVarargs;
      inc(pc, 9);
    end else
      WriteTypePc(inNone);
    if pc^ = ListTerm then
      inc(pc)
    else  //what???
      break;
    if pc^ <> ')' then
      Write('; ');  //more params follow
  end;
//procedure or function?
  assert(pc^ = ')', 'bad argument list');
  inc(pc);  //skip ")"
  Write(')');
//calling convention
  cc := ' '; ci := False;
  while True do begin
    case pc^ of
    'C', 'F': cc := pc^;
    'I': ci := True;
    else  break;
    end;
    inc(pc);
  end;
  if pc^ = 'v' then
    inc(pc)  //void means procedure
  else begin
    Write(': ');
    WriteTypePc(inNone);
  end;
//append calling convention - a trailing ";" will be appended by the caller
  case cc of
  'C':  Write('; cdecl');
  'F':  Write('{; fastcall}'); //unsupported by Delphi?
  else  Write('; stdcall');  //default calling convention for C
  end;
  if fVarargs then
    Write('; varargs'); //assume cdecl
  if ci then
    Write(' {inline}'); //doesn't make sense for imported (API...) procedures
end;

procedure TToPas.WriteProcType;
var
  i: integer;
  pe: PChar;
begin
  inc(pc);  //skip "("
//distinguish proc from func
//here we *must* search for the end of the parameter list - more may follow!
  pe := pc;
  i := 1;
  while i > 0 do begin
    case pe^ of
    '(':  inc(i);
    ')':  dec(i);
    end;
    inc(pe);
  end;
//pe now is past matching ")" - skip calling conventions etc.
  while pe^ in ['C', 'F', 'I'] do
    inc(pe);  //expect: cdecl, fastcall, inline - what else?
  if pe^ = 'v' then
    Write('procedure(')
  else
    Write('function(');
//argument list
  WriteArgumentList;
end;

procedure TToPas.WritePointer;
begin
  inc(pc);
  if pc^ = '#' then
    WriteInc('{const}');
  case pc^ of
  'v':  WriteInc('pointer');
  'c':  WriteInc('PChar');
  '(':  WriteProcType; //procedural type is implicit pointer!
  else  Write('^');
  end;
end;

function  TToPas.Unquoted(delim: char): string;
var
  pc0:  PChar;
begin
  if pc^ = delim then
    inc(pc);
  pc0 := pc;
  repeat
    inc(pc)
  until pc^ = delim;
  SetString(Result, pc0, pc - pc0);
  inc(pc);  //skip delim
end;

procedure TToPas.WriteTypeRef;
var
  s: string;
begin //quoted typename - unquote
  s := Unquoted(typeQuote);
  if (Length(s) > 2) and (s[2] = ':') then begin
  //remove S/U/E from tagged ref
    s[2] := '_';
  end;
  Write(s);
end;

procedure TToPas.WriteSized(fSigned: boolean; len: char);
const
  aSigned: array[boolean] of string = ('UInt', 'SInt');
begin
  Write(aSigned[fSigned] + len);
end;

procedure TToPas.WriteUnSigned(fSigned: boolean);
var
  s: string;
const
  acSigned: array[boolean] of char = 'US';
  asSigned: array[boolean] of string = ('{unsigned}', '{signed}');
  asShort:  array[boolean] of string = ('SmallInt', 'Word');
  asInt:    array[boolean] of string = ('Cardinal', 'Integer');
  asLong:   array[boolean] of string = ('LongWord', 'LongInt');
begin
  if pc^ in ['+', '-'] then
    inc(pc);
  case pc^ of
  '"':  //try decode base type
    begin //single level search, for now
      s := Unquoted(typeQuote);
      typ := Globals.getType(s);
      if (typ <> nil) and (Length(typ.def) = 2) and (typ.Def[2] in digits) then
        WriteSized(fSigned, typ.Def[2])
      else begin
        Write(asSigned[fSigned] + s);
      end;
      dec(pc);  //inc after switch
    end;
  '1','2','4','8': WriteSized(fSigned, pc^);
  'L':  Write(acSigned[fSigned] + 'Int8');
  'c':  Write(acSigned[fSigned] + 'Char');
//use Delphi standard types
  'i':  Write(asInt[fSigned]);  // Write(acSigned[fSigned] + 'Int');
  'l':  Write(asLong[fSigned]); // Write(acSigned[fSigned] + 'Long');
  's':  Write(asShort[fSigned]);// Write(acSigned[fSigned] + 'Short');
  else
    Write(asSigned[fSigned]);
    //bug in D4: "exit" in "case" is not always compiled properly!
    dec(pc);  //exit; //don't inc pc!
  end;
  inc(pc);
end;

procedure TToPas.WriteEnum;
begin //E{...}
  if pc^ = 'E' then
    inc(pc);  //in declaration ...E{
  assert(pc^ = '{', 'expected "{"');
  inc(pc);
  WriteLn('(');
  inc(indent);
//write members
  while pc^ <> '}' do begin
  //expect: <name>=<#>,
  //can be: <name>, !!!?
    WriteName('=', ListTerm);
    if pc^ = '=' then begin
      inc(pc);  //skip "="
      if pc^ <> ListTerm then begin
      (* ',' how here?
        Hide explicit values? (become non-std enum!?)
      *)
        Write('{=');
        WriteExprPc;  //(',');
        Write('}');
      end;
    end;
    if pc^ = ListTerm then begin
      inc(pc);
      if pc^ <> '}' then
        WriteLn(',');  //not after last member!
    end else
      break;  //unexpected
  end;
  WriteLn('');  //flush last member
  dec(indent);
  inc(pc);  //skip "}"
//end
  Write(')'); //add ';' later
end;

procedure TToPas.WriteStruct(fIn: eSU);
var
  iMbr: integer;  //member count
  //fNamed: boolean;
  nowIn: eSU;
begin //S{...}
(* Problem: multiple unions - should retain record..end delimiters!
*)
  if fIn = inUnion then
    WriteLn('{record}')
  else
    WriteLn('record');
  if pc^ = 'S' then
    inc(pc);  //in declaration ...S{
  if pc^ = '{' then begin
  //explicit declaration - inUnion only show members
    iMbr := 0;
    inc(pc);
    inc(indent);
  //write members
    while pc^ <> '}' do begin
      nowIn := inNone;
      inc(iMbr);
      if pc^ = ':' then begin
      //anonymous legal?
        if pc[1] = 'U' then begin
          nowIn := inStruct;  //no name required
        end else begin
          if fNameMembers then
            Write('_mbr' + IntToStr(iMbr) + ': ')
          else
            Write('{???:}')
        end;
      end else begin
        WriteName(':', #0);
        Write(': ');
      end;
      inc(pc);  //skip ":"
      WriteTypePc(nowIn);
      WriteLn(';');
      if pc^ = ListTerm then
        inc(pc)
      else
        break;
    end;
    dec(indent);
    if pc^ <> '}' then
      assert(pc^ = '}', 'unterminated struct');
    inc(pc);
  end else begin //UNDEFINED!!! bug in win???.h, or external declaration?
    //WriteLn(f, #9, #9, '//undefined!');
    WriteLn('//undefined!');
  end;
  if fIn = inUnion then
    Write('{end}')
  else
    Write('end');
end;

procedure TToPas.WriteUnion(fIn: eSU);
var
  i, iMbr: integer;
  nowIn:  eSU;
begin //U{...}
  if pc^ = 'U' then
    inc(pc);  //in declaration ...U{
  assert(pc^ = '{', 'expected "{"');
  inc(pc);
  if fIn = inStruct then
    WriteLn('{record}')
  else
    WriteLn('record');
  inc(indent);
  WriteLn('case integer of');
  i := 0;
//write members
  iMbr := 0;
  while pc^ <> '}' do begin
    inc(iMbr);
    nowIn := inNone;
    Write(IntToStr(i) + ':' + #9 + '('); inc(i);
    if pc^ = ':' then begin
    //anonymous legal?
      if pc[1] = 'S' then begin
        nowIn := inUnion;  //no name required
      end else begin
        if fNameMembers then
          Write('_mbr' + IntToStr(iMbr) + ': ')
        else
          Write('{???:}')
      end;
    end else begin
      WriteName(':', #0);
      Write(': ');
    end;
    inc(pc);  //skip ":"
    WriteTypePc(nowIn);
    WriteLn(');');
    if pc^ = ListTerm then
      inc(pc)
    else
      break;
  end;
  dec(indent);
  assert(pc^ = '}', 'unterminated struct');
  inc(pc);
  if fIn <> inStruct then begin
    Write('end');
  end else  //syntax error if more members follow!
    Write('{end}');
end;

procedure TToPas.WriteBitfield;
begin //todo: make Object with access methods
  inc(pc);  //skip "<"
  Write('{bitfield[');
  WriteExprPc;  //('>');
  Write(']}');
  assert(pc^ = BitFieldEnd, 'unterminated bitfield');
  inc(pc);  //skip ">"
end;

procedure TToPas.WriteTypePc(fIn: eSU);
begin
  while pc^ <> #0 do begin
    case pc^ of
    '[':  WriteArray;
    '*':  WritePointer;
    '(':  WriteProcType;
    typeQuote:  WriteTypeRef;
    '+':  WriteUnSigned(False);
    '-':  WriteUnSigned(True);
    //'<':  WriteBitfield;
    BitFieldStart:  WriteBitfield;  //"<" or "{"
    '#':  WriteInc('{const}');
    'D':  WriteInc('Extended');
    'E':  WriteEnum;
    'L':  WriteUnSigned(True);  //WriteInc('Int64');
    'S':  WriteStruct(fIn);
    'U':  WriteUnion(fIn);
    'V':  WriteInc('{volatile}'); // inc(pc);  //skip "volatile"
    'c':  WriteInc('Char');
    'd':  WriteInc('Double');
    'f':  WriteInc('Float');
    'i':  WriteUnSigned(True);  //WriteInc('Integer');
    'l':  WriteUnSigned(True);  //WriteInc('LongInt');
    's':  WriteUnSigned(True);  //WriteInc('SmallInt'); //assume "short" is 16 bit
    '~':  WriteInc('~Array of Const');  //invalid in mixed language projects
    else  break;  //unexpected char, ends current type
    end;
  end;
end;

procedure TToPas.WriteType;
begin //switch to pc scanning
  pc := ScanToken.pc;
  WriteTypePc(inNone);
  scanner.ScanPChar(pc);
end;  // wrapper


procedure TToPas.TypeSection;
begin
  if curkind <> stTypedef then begin
    WriteLn(' ');
    Outdent('type');
    curkind := stTypeDef;
  end;
  if not typeswritten then begin
    typeswritten := True;
//standard types
    Outdent('//"S"igned and "U"nsigned types of specific size');
      WriteLn('SInt1 = ShortInt;');
      WriteLn('SInt2 = SmallInt;');
      WriteLn('SInt4 = LongInt;');
      WriteLn('SInt8 = Int64;');

      WriteLn('UInt1 = Byte;');
      WriteLn('UInt2 = Word;');
      WriteLn('UInt4 = LongWord;');
      WriteLn('//UInt8 = {unsigned}Int64; //no equivalent');
    Outdent('//"S"igned and "U"nsigned types of unspecific size');
    //C char is compatible with "int", not so in Pascal
      WriteLn('UChar = Char;');
      WriteLn('SChar = Char;');
      WriteLn('UShort = UInt2;');
      WriteLn('UInt = Cardinal;');
      WriteLn('ULong = Cardinal;');
      WriteLn('SShort = SInt2;');
      WriteLn('SInt = Integer;');
      WriteLn('SLong = Integer;');
  //more?
    //dec(indent);
    WriteLn(' ');
  end;
end;

procedure TToPas.WriteTypeSym;
var
  s: string;
begin
  try
    typ := sym as TTypeDef;
    if typ <> nil then begin
      TypeSection;
      s := typ.Name;
      pc := ScanDef(typ.Def);
      if (Length(s) > 2) and (s[2] = ':') then begin
      //convert tag name reference "t:tag" -> "t_tag"
        s[2] := '_';
        Write(s + ' = ');
        case s[1] of
        'E':  WriteEnum;
        'S':  WriteStruct(inNone);
        'U':  WriteUnion(inNone);
        else  assert(False, 'expected S/U/E');
        end;
      end else begin
        Write(s + ' = ');
        WriteTypePc(inNone);
        if pc^ <> #0 then begin
        //not everything converted
          s := string(pc);
          if s = 'v' then
            Write('record {void} end')
          else
            Write(' ??? ' + string(pc) + ' ???');
        end;
      end;
      WriteLn(';');
    end;
  except
    WriteLn('???' + string(pc) + '???');
  end;
end;


procedure TToPas.ConstSection;
begin
  if curkind <> stConst then begin
    WriteLn(' ');
    Outdent('const');
    curkind := stConst;
  end;
end;

procedure TToPas.WriteConstSym;
begin
  pc := ScanDef(sym.Def); //sets CurDef
(* Handling of enum members:
  'E' means a member of an anonymous enum.
  '"E:<enumname>"' means a member of a tagged enum.
*)
  if (CurDef = 'E') or (Copy(CurDef, 1, 3) = '"E:') then begin
    if fDebugEnums then //debug?
      WriteLn('//' + sym.UniqueName + ' in ' + CurDef);  //skip
  end else begin
    ConstSection;
    Write(sym.UniqueName);
    if (CurDef <> '') and (CurDef <> '#') then begin
    //typed constants
      Write(': ');
      WriteTypePc(inNone);
    end;
    Write(' = ');
    if sym.StrVal <> '' then begin
    //string literal or number?
      pc := ScanDef(sym.StrVal);  // PChar(sym.StrVal);
      WriteExprPc;
    end else
      Write('0');  // sym.IntVal); //, ';');
    WriteLn(';');
  end;
end;


procedure TToPas.VarSection;
begin
  if curkind <> stVar then begin
    WriteLn(' ');
    Outdent('var');
    curkind := stVar;
  end;
end;

procedure TToPas.WriteVarSym;
begin
  VarSection;
  pc := ScanDef(sym.Def);
  if pc^ = StaticScope then begin
  //static var - noshow in interface section! (done)
    Write('{static} '); //debug only!
    inc(pc);  //for now
  end;
  Write(sym.UniqueName);
//untyped vars possible?
  if CurDef <> '' then begin
    Write(': ');
    WriteTypePc(inNone);
  end else  //seems not to occur :-)
    Write(':?');
  if sym.StrVal <> '' then begin
    Write(' = ');
  //string literal or number?
    pc := ScanDef(sym.StrVal);  // PChar(sym.StrVal);
    WriteExprPc;
  end;
  WriteLn(';');
end;

procedure TToPas.WriteProcSym;
var
  pe: PChar;
begin
  if curkind <> stProc then begin
    WriteLn(' ');
    curkind := stProc;  //not a section, but break any other section
  end;
  proc := sym as TSymProc;  // SymbolsC.Procs.getProc(i);
  pc := ScanDef(proc.Def);
  pe := @CurDef[Length(CurDef)];
//specials - what exactly?
  if pc^ = StaticScope then begin
  //should not show in interface section!
    Write('{static} ');
    inc(pc);
  end;
//determine proc/func
{ TODO : what about calling convention here? }
(* all procedure definitions must begin with '(',
  and end with a Type (name or 'v' for procedure)
*)
  if (pe[0] = 'v') then
    Write('procedure ')
  else
    Write('function ');
  //s := proc.UniqueName;
  Write(proc.UniqueName + '(');
  //pc := PChar(CurDef);
  if pc^ = '(' then begin
    inc(pc);
    //WriteArguments; //...and function type etc...
    WriteParams;  //etc., from param scope
  end else begin
    Write('?' + CurDef); //warning: is not a function prototype!
  end;
  Outdent(';');
  if SectionType = stIntf then
    exit; //noshow declaration in interface
  if proc.hasBody then begin
  //show definition = body
    WriteProcBody;  //(proc)
  end else begin
  //external
    Write('external');
    if self.fLibName then
      Write(' ' + LibRef);
    WriteLn(';');
  end;
end;

procedure TToPas.WriteParams;
//from pc (after "(")
var
  i: integer; //argument counter, for naming anonymous arguments
  cc: char;
  ci: boolean;
  fVarargs: boolean;

  procedure ShowVarargs;
  begin
  (* How to convert C "..."?
    For internal (cross compiled) procedures "Array of Const" is an
      appropriate replacement, but what for calling external C functions?
    Solution: assume cdecl, append attribute "varargs" (not in D4!)
  *)
    if self.UnitType = utImport then begin
    //assume cdecl...
      fVarargs := True; //append "varargs;"
    end else begin
      //Write('~Array of Const');  //intentionally: produce syntax error
      Write('va_arg: array of const');
    end;
  end;

  procedure WriteParamName;
  var
    n : string;
    pd: PChar;
  begin
    pd := pc;
    //while not (pc^ in delims) do
    while (pc^ <> ':') do
      inc(pc);
    SetString(n, pd, pc - pd);  //assume delim is not the first char!

  //don't unify local names (for now)
    Write(n + '_1');
  end;

begin
  i := 0;
  fVarargs := False;
  while pc^ <> ')' do begin
  //write parameter
    if pc^ = ':' then begin
    //unnamed parameter - varargs?
      if pc[1] = '~' then begin
      //":~,)" = varargs
      end else if (pc[1] = 'v') and (pc[3] = ')') then begin
      //":v,)" is empty argument list - should no more occur?
        { DONE : f(void) - fix in parser }
        assert(False, 'empty paramlist'); //assume done
        inc(pc, 3);
        break;
      end else
        Write('arg_' + IntToStr(i));
    end else begin
      WriteParamName; //(':', #0);
    end;
    inc(i); //argument counter
  //parameter type
    inc(pc);  //skip ":"
    Write(': ');
  //check special cases: varargs
    if (pc^ = '~') then begin
      ShowVarargs;
      inc(pc);
    end else if (StrLComp(pc, '"va_list"', 9) = 0) then begin
    //what's this good for?
      ShowVarargs;
      inc(pc, 9);
    end else
      WriteTypePc(inNone);
    if pc^ = ListTerm then
      inc(pc)
    else  //what???
      break;
    if pc^ <> ')' then
      Write('; ');  //more params follow
  end;
//procedure or function?
  assert(pc^ = ')', 'bad argument list');
  inc(pc);  //skip ")"
  Write(')');
//calling convention
  cc := ' '; ci := False;
  while True do begin
    case pc^ of
    'C', 'F': cc := pc^;
    'I': ci := True;
    else  break;
    end;
    inc(pc);
  end;
  if pc^ = 'v' then
    inc(pc)  //void means procedure
  else begin
    Write(': ');
    WriteTypePc(inNone);
  end;
//append calling convention - a trailing ";" will be appended by the caller
  case cc of
  'C':  Write('; cdecl');
  'F':  Write('{; fastcall}'); //unsupported by Delphi?
  else  Write('; stdcall');  //default calling convention for C
  end;
  if fVarargs then
    Write('; varargs'); //assume cdecl
  if ci then
    Write(' {inline}'); //doesn't make sense for imported (API...) procedures
end;

procedure TToPas.WriteProcBody; //(proc)
var
  body: TSymBlock;
  stmts, vars:  TStringList;
  stmt: string;
  ln: integer;

  function  firstToken: eToken;
  begin
    //Result := Scanner.scanString(stmt);
    Scanner.SetSource(stmt);
    Result := nextToken;
    //i_ttyp := Result;
  end;

  function AddVar: boolean; //from scanner, after "auto"
  begin
    nextToken;  //skip auto/static...
    expect(opLPar, 'no auto"("');
    if i_ttyp = t_sym then
      vars.Add(stmt);
    expect(t_sym, 'no auto("sym"');
    //expect(opColon, 'no lcl":"');
    while nextToken <> t_eof do begin
      if i_ttyp = opLet then begin
        Result := True; //var is initialized
        exit;
      end;
    end;
    Result := False;
  end;

  procedure AddStatic;
  begin
  //todo
    AddVar; //for now
  end;

  procedure AddConst;
  begin
  //todo
    AddVar; //for now
  end;

  procedure AddLabel;
  begin
    expect(opColon, 'no ":"label');
    expect(t_sym, 'no :"sym"');
    outdent('label ' + ScanText); //immediate output!?
  end;

  procedure ShowTV(fDecl: boolean; fConst: boolean);

    procedure ShowType;
    begin
    //!!!type decoding is based on pc, NOT on Scanner!!!
      pc := ScanToken.pc; //past sym
      if pc^ <> ':' then
        exit; //not type to show
      WriteInc(': ');
      //skip(uTokenC.logNOT);  //ignore "static" flag
      if pc^ = StaticScope then
        inc(pc);
      WriteTypePc(inNone);
      //now pc back into scanner
      //Scanner.ScanPChar(pc);
    end;

    procedure HideType(fDecl: boolean);
    begin
      fNoShow := not fDecl; //hide unless decl
      ShowType;
      fNoShow := False; //restore normal mode
    end;

  begin //ShowTV
    Write(ScanText);
    expect(t_sym, 'no "var":...');
    HideType(fDecl);  //advance pc!!!
    if pc^ = '=' then begin
    //has value
      if fDecl and fConst then begin
      //assume: static initialization?
        WriteInc(' = ');
        WriteExprPc;
      end;
      if not fDecl then begin
      //assume dynamic init, vars only?
      //suppress listing by NOT adding the const(...) to stmts!
        WriteInc(' := ');
        WriteExprPc;
      end;
    end;
    WriteLn(';'); //always terminate
  end;

  procedure ShowVar(fDecl: boolean = False);
  begin
    if fDecl then begin
    //auto(var[:type][=val]);
      nextToken;  //auto...
      expect(opLPar, 'no "("var...)');
      VarSection;
    end;
    ShowTV(fDecl, False);
  end;

  procedure ShowConst(fDecl: boolean = False);
  begin
    if fDecl then begin
      nextToken;  //auto...
      expect(opLPar, 'no "("var...)');
      ConstSection;
    end;
    ShowTV(fDecl, True);
  end;

  function  getLine(fParse: boolean): boolean;
  begin
    Result := ln < stmts.Count;
    if Result then begin
      stmt := stmts[ln];
      if fParse then begin
        Scanner.SetSource(stmt);
        nextToken;
      end;
    end;
    inc(ln);
  end;

  procedure WriteBlock;
  begin
  //to come...
    Outdent(stmt);
  end;

  procedure FlushStmt(fSemi: boolean = True);
  begin
  //handle in/outdent properly
    if outbuf = '' then
      exit;
    if fSemi then
      Write(';');
    WriteLn('');
  end;

  procedure ShowExprList(fFlush: boolean);
  var
    steps: string;
  begin //here: as statements, filter assign-ops!
    //Write(ExpressionString(t_empty) + '; ');
    repeat
      steps := ExpressionString(t_empty);
      if copy(steps, 1, 7) = ' __post' then
        Delete(steps, 1, 7);  //make simple inc/dec
      Write(steps + '; ');
    until not skip(opComma);
    if fFlush then
      FlushStmt(False);
  end;
{
  procedure WriteExprStmt;
  var
    steps: string;
  begin
    steps := ExpressionString(t_empty);
    if copy(steps, 1, 7) = ' __post' then
      Delete(steps, 1, 7);  //make simple inc/dec
    Write(steps);
  end;
}

(* Statements, some considerations
- before "else" no ";" is allowed
  -> always enclose: then begin ... end else ...
- indent stmt-lists - one or {more}
  !no termination of for ... begin stmts steps! end;
  -> handle stmt-list-indentation in caller.
Flag: stmt[-list] with/out indentation and begin/end.
? keep preceding stmt unflushed? -> allow for append "begin"
  leave "end" after stmt-list.
cases:
  case ... of //NO begin!!!
  labels: (outdented)
    begin //HERE: mark and in/outdent blocks
      stmts
      //break HERE: suppress!!! -> fInSwitch (immediate only!)
      //NO break/return/goto: FORCE goto <next_case>
      __NOBREAK; //leave for post-processor:
    end;
  end;

  if ... then begin
    stmts
  end [else...];

  repeat
    stmts
  until ...;

  while ... do begin
    stmts
  end;
*)

  function ShowStmt: boolean; forward;


type
  eStmtFlags = (
    sfShowBegin,
    sfIndent,
    sfShowEnd,  //and outdent
    sfAsBlock   //always treat as block
  );
  sStmtFlags = set of eStmtFlags;
const
  sfOneOrBlock = [sfShowBegin, sfIndent, sfShowEnd{, sfAsBlock}];
  sfAlwaysBlock = [sfShowBegin, sfIndent, sfShowEnd, sfAsBlock];

  sfForWithSteps = [sfAsBlock];
  sfForNoSteps = sfForWithSteps;  // [{sfShowBegin,} sfIndent, sfShowEnd, sfAsBlock];
  sfIf = sfAlwaysBlock; // [sfShowBegin, sfIndent, sfShowEnd, sfAsBlock];
  sfSwitch = [{sfShowBegin,} sfIndent, sfShowEnd, sfAsBlock];
  sfRepeat = [{sfShowBegin,} sfIndent{, sfShowEnd}{, sfAsBlock}];
  sfBlock = sfAlwaysBlock;  // [sfShowBegin, sfIndent, sfShowEnd, sfAsBlock];

(* ShowStmts: show one or more statements, indented
*)
  //function ShowStmts(fEnd: boolean): boolean;
  function ShowStmts(flags: sStmtFlags): boolean;
  begin
  //first determine block/single stmt
    Result := i_ttyp = opBeg;
    if Result then begin
      skip(opBeg);
      skip(opLPar);
      include(flags, sfAsBlock);
    end;
  //append/show begin
    if sfAsBlock in flags then begin //forced or real block
      if sfShowBegin in flags then begin
        WriteSep('begin')
        //include(flags, slBegin);
      end;
    end;
  //flush before indenting
    FlushStmt(false); //always flush possible preamble
  //now indent
    if sfIndent in flags then
      inc(indent);
  //now show stmt(s)
    if Result then begin
    //show block
      while getLine(True) and not skip(opEnd) do begin
        ShowStmt;
        if i_ttyp <> t_eof then begin
          Log('too many stmts in line', lkBug);
          //ShowStmt;
          WriteLn('??? ' + stmt + ' ...');
          //break;  //prevent infinite loop, here
        end;
      end;
      //opEnd already consumed!
      //Result :=
      if i_ttyp <> t_eof then
        expect(opSemi, 'no stmt";"');
      //else already at eof?
    end else
      ShowStmt; //flushed, with ";"!
  //finish up
    if (sfShowEnd in flags) then begin
    //NOT in for!
    //now outdent - if we indented
      if sfIndent in flags then
        dec(indent);
      //else BOTH indent and outdent up to caller!
    //now handle end
      if (sfAsBlock in flags) then begin //begin shown
        Write('end'); //not in "for"
        //FlushStmt(False);
      end;  // stmts may follow (in "for"!)
    end;
  //always return past ";"
  end;

  procedure ShowFor;
  var
    //conds,
    steps:  string;
  begin //(init-list;cond;step-list;stmt);
  //Outdent('//for...');  //really a block remark
  Outdent('//' + stmt);  //really a block remark
  //init-list
    if i_ttyp <> opSemi then
      ShowExprList(True);
    expect(opSemi, 'no for(init";"');
  //cond
    Write('while ');
    if i_ttyp <> opSemi then
    //for now: always show as: begin stmts steps end;
      Write(ExpressionString(t_empty))
    else
      Write('True');
    Write(' do begin //for');
    FlushStmt(False);  //conds := outbuf; outbuf := '';
    expect(opSemi, 'no for(init;cond";"');
  //steps
    if i_ttyp <> opSemi then begin
      ShowExprList(False);
      steps := outbuf; outbuf := '';
    end else begin  //no steps
      steps := '';
    end;
    expect(opSemi, 'no step";"');
  //stmt
    //Result := ShowStmts(False);  //consume ";", no extra indent
  //steps
    inc(indent);
    ShowStmts(sfForWithSteps);
    if steps <> '' then begin
    outdent('//continue_for:');
      WriteLn(steps);
    end;
    dec(indent);
  //finish - always a block!
    Write('end; //for');
    FlushStmt(False); //prevent appending another ";"
  end;

  procedure ShowSwitch;
  var
    fBreak: boolean;
    fDefault: boolean;
  const
    Labels = [Kcase, Kdefault];
    Breaks = Labels + [t_eof, opEnd];
  begin //switch(arg;{(...};);
  //wrap into repeat...until True;
    WriteLn('repeat //switch');
    inc(indent);
  //now the switch
    Write('case ' + ExpressionString(t_empty) + ' of');
    FlushStmt(False);
    inc(indent);
    expect(opSemi, 'no switch(expr";"');
    expect(opBeg, 'no switch"{"');
    getLine(True);
    fDefault := False;
    while i_ttyp in Labels do begin
      if fDefault then  //should be handled properly!!!
        Log('"default" not last in switch', lkBug);
      fDefault := i_ttyp = Kdefault;
      ShowStmt; //outdented
      if not fDefault then begin
        WriteLn('begin');
        inc(indent);
      end;
      fBreak := False;
      while getLine(True) and not (i_ttyp in Breaks) do begin
        if i_ttyp = Kbreak then
          fBreak := True
        else begin
        //also could filter: redundant blocks!
          if fBreak then begin
            WriteLn('break;');
            fBreak := False;
          end;
          ShowStmt;
        end;
      end;
      if not fDefault then begin
      //debug?
        if fBreak then
          WriteLn('//break;')
        else
          WriteLn('__fall_through!;');
        dec(indent);
        WriteLn('end;');
      end;
    end;
    dec(indent);
    WriteLn('end; //switch');
    //expect: now at "};);"
    expect(opEnd, 'no switch{"}"');
    skip(opSemi);
    //leave: ");" standard trailer
  //finish wrapper
    dec(indent);
    WriteLn('until True;');
  end;

var //reduce try/finally blocks
  skey: string;

  function ShowStmt: boolean;
  var
    stype:  eToken;
  begin
  //problem: sym etc. -> expression!
    stype := i_ttyp;
    if stype < Kbreak then begin
    //expression, just at start
      case stype of
    //operators?
      opColon:  //:label;
        begin
          nextToken;
          Write(ScanText + ':'); expect(t_sym, 'no "label":');
          Outdent('');  //following FlushStmt does nothing
        end;
      opBeg:
        begin
          //ShowStmtList(True);
          ShowStmts(sfAlwaysBlock);
        end;
      opEnd:  //at top level only?
        begin
          Result := False;  //break loop?
          exit;
        end;
      opSemi: //empty stmt!
        begin
          //Result := True; //False?  //break loop?
          //exit;
        end;
      else  //??? really always expression?
        ShowExprList(True); //WriteExprStmt;
        //flush to prevent more ";"
      end;
      FlushStmt(True);
    end else begin
      skey := ScanText;
      nextToken;  //skip key
      skip(opLPar); //usually "("args);
      case stype of
    //simple
      Kbreak, //? match meaning?
      Kcontinue:  //op;
        Write(skey);
      Kcase:  //case(labels); //!!!+stmts!!!
        begin //expressions???
          //Write(ScanText); nextToken; // expect(t_sym, 'no lab sym');
          WriteExpr;
          while skip(opComma) do begin
            Write(', ');  // + ScanText); nextToken;  // expect(t_sym, 'no lab sym');  // nextToken;
            WriteExpr;
          end;
          Outdent(':');
          //Result := skip(opRPar) and expect(opSemi, 'no case(lab");"');
          //exit;
        end;
      Kdefault: //default;
        begin
          Outdent('else //case');
          //Result := expect(opSemi, 'no stmt";"');
          //exit;
        end;
      Kdo:  //do(stmt;while-expr);
        begin
          WriteLn('repeat');  //nextToken;
          inc(indent);
          ShowStmt; //works - "," instead of ";" ???
          //expect(opComma, 'no do","stmt');
          dec(indent);
          Write('until not ');
          Write(ExpressionString(logNOT));
        end;
      Kfor:
        ShowFor;
      Kgoto:  //goto(label);
        begin
          Write('goto ' + ScanText); nextToken;
        end;
      Kif:  //if(cond;stmt;else[;]); - !!! else if...???
        begin
          Write('if ' + ExpressionString(t_empty) + ' then');
          expect(opSemi, 'no if(expr";"');
        //then
          //ShowStmts(True);  //problem: NO ";" before else allowed!
          ShowStmts(sfIf);
        //else part?
          if i_ttyp <> opRPar then begin
            WriteSep('else');
            if i_ttyp = Kif then begin
            //it's NOT a block, so...
              Write(' ');  //need a blank before "if"!
              ShowStmt;
            end else
              ShowStmts(sfOneOrBlock);
          end;
        end;
      //Kelse,
      Kreturn:  //return[(expr)];
        begin
          if i_ttyp <> opSemi then begin
          //possible problem: now these are 2 statements! - may require begin...end
            Write('begin Result := ' + ExpressionString(t_empty) + '; exit; end');
          end else
            Write('exit;');
        end;
      Kswitch: //switch(expr, {stmt-list});
        begin
          ShowSwitch;
        end;
      Kwhile: //while(expr;stmt;);
        begin
          Write('while ' + ExpressionString(t_empty) + ' do');
          expect(opSemi, 'no while(expr";"');
          ShowStmts(sfOneOrBlock);
        end;
    //init locals
    {$IFDEF new}
      Kextern:  //debug!
        WriteLn('//' + stmt);
    {$ELSE}
      //not here!
    {$ENDIF}
      Kauto,
      Kregister,
      //Kstatic, - assume: init once???
      //Ktypedef,
      //Kinline, ???
      Kconst:
        begin
          ShowVar(False);
        end;
      else  //case
        Write(skey + '???');
        Log(skey, lkBug);
      end;
    //typical trailer ");"
      FlushStmt;  //no ";" if outbuf empty
      skip(opRPar);
    end;
    Result := (i_ttyp = t_eof) or expect(opSemi, 'no stmt";"');
  end;

var
  i: integer;
begin //WriteProcBody
(* Special handling required:
Assume NO local procedures, i.e. indent is always 1.
  Body strings already are indented with tabs!
- Must collect all local symbols, by scope!
  - Add params before locals.
- Must emit variables before "begin".

First try:
  scan body for locals, add to vars list,
  output list.
*)
  body := proc.Body;
  vars := TStringList.Create;
  stmts := TStringList.Create;
  try
    stmts.Capacity := body.Count + 1;
    //stmts.Add('{'); //part of the Proc.Def! ???
    for i := 0 to body.Count - 1 do begin
      stmt := body.Strings[i];
    //filter declarations
      //case scanner.scanString(stmt) of
      case firstToken of
      Kauto, Kregister: //register not expected, but does no harm
        if AddVar then
          stmts.Add(stmt);  //var is initialized!
      Kextern:  //debug!
      {$IFDEF old}
        ; //ignore, for now. Lookup global sym? (simply use the name!)
      {$ELSE}
        WriteLn('//' + stmt);
      {$ENDIF}
      Kstatic:
        AddStatic;  //currently: as const -> not normally writeable...
      Kconst:
        AddConst; //as const
      opColon:
        AddLabel;
      else
        stmts.Add(stmt); //to be translated
      end;
    end;
  //show vars
    if vars.Count > 0 then begin
      //outdent('var');
      for i := 0 to vars.count - 1 do begin
        stmt := vars[i];
        case firstToken of
        Kauto,
        Kregister:
          ShowVar(True);  //hide value
        Kstatic,
        Kconst:
          ShowConst(True);  //must show both type and value!
        else
          Outdent(stmt);
          Log('unhandled var...', lkBug);
        end;
      end;
    end;
    ln := 0;
    indent := 0;
    while getLine(True) and ShowStmt do
      ;
    //expect(opEnd, 'no body"}"');  //really???
  finally
    vars.Free;
    stmts.Free;
  end;
end;

procedure TToPas.WriteIntf;
var
  i: integer;
begin
//"interface" already shown.
//"uses"???
//imports?
  if (self.LibName <> '') and not fLibName then begin
    ConstSection;
    WriteLn(LibRef + ' = ''' + LibName + ''';');
    fLibName := True; //use libref
  end;
  for i := 0 to Scope.Count - 1 do begin
    sym := Scope.getSym(i);
  //hide static symbols
    if (sym.Def <> '') and (sym.Def[1] <> '!') then
      TranslateSym(sym);
  end;
end;

{$IFDEF MacroObjects}
//problem: macros in *.txt only as text, not as tokens and objects!!!
  procedure WriteImpl;
  var
    i: integer;
    sym:  PSymPrep; //TSymPrep;
    mac:  TMacro;
    body, args: string;
  begin
    WriteLn(f, 'implementation');
    WriteLn(f);
    for i := 0 to Symbols.Count - 1 do begin
      sym := Symbols.getSymbol(i);
      if (sym.mackind = skMacro) and (sym.FMember <> nil) then begin
        mac := TMacro(sym.FMember);
        body := mac.BodyString; //(False);
        if body > ' ' then begin
          Write(f, 'Procedure'#9, mac.name);
          args := mac.argList;
          if Length(args) > 2 then
            args := '(const ' + copy(args, 2, Length(args));
          WriteLn(f, args, ';');
          WriteLn(f, 'begin {');
          WriteLn(f, s);
          WriteLn(f, '} end;');
          WriteLn(f);
        end;  //else empty body
      end;
    end;
    WriteLn(f, 'end.');
  end;
{$ELSE}
procedure TToPas.WriteImpl;
var
  fin: TextFile;
  s, c: string;
  i: integer;
//new format
  ln: string;
  symc: char;

  //procedure ReadLine;
  function  ReadLine: boolean;
  const
    fNew = True;
  begin
    ReadLn(fin, ln);
    if fNew then begin
    //c[ln]<tab>...
    {$IFDEF old}
      i := Pos(#9, ln);
      i := PosEx(#9, ln, i+1);
      symc := ln[i+1];  //type character
      ln := Copy(ln, i+2, Length(ln));
    {$ELSE}
      symc := ln[1];
      i := Pos(#9, ln);
      ln := Copy(ln, i+1, Length(ln));
    {$ENDIF}
    end;
    Result := symc < 'a';
    if Result then begin
    //macro format
      s := ln;
    {
      while ln[Length(ln)] = '\' do begin
      //merge continued line
        ReadLn(fin, s);
        assert(s[1] = #9, 'expected <tab>');
        ln := ln + Copy(s, 2, Length(s));
      end;
    }
    end else begin
    //non-macro format, handle continuation lines
      while ln[Length(ln)] = ListTerm do begin
      //merge continued line
        ReadLn(fin, s);
        assert(s[1] = #9, 'expected <tab>');
        ln := s;  //ln := ln + Copy(s, 2, Length(s));
      end;
    end;
    //Result := ln[1] <> '-'; //matters only for old format
  end;

  procedure ReadCont;
  begin
    while s[Length(s)] = ListTerm do begin
    //append wrapped lines
      ReadLn(fin, c);
      s := s + Copy(c, 2, Length(c));
    end;
  end;

  procedure ConvertMac;
  begin
    //if s[length(s)] = '\' then
    if s[length(s)] <> '\' then
      exit; //show nothing for empty macro
    s[length(s)] := ';';  //subst \ by terminating ;
    if curkind <> stProc then begin
      WriteLn(' ');
      curkind := stProc;
    end;
    i := Pos('(', s);
    if (i > 0) and (s[i+1] <> ')') then
    //non-empty parameter list - todo: handle dupes!
      Write('procedure'#9 + Copy(s, 1, i) + 'const ' + Copy(s, i+1, length(s)))
    else
      Write('procedure'#9 + s);
    {
    if s[length(s)] <> ';' then begin
    //empty body
      Outdent(';');
      Outdent('begin');
    end else }
    begin
      Outdent('');  //write the declaration line
      Outdent('begin (*');
      while True do begin
        ReadLn(fin, s);
      //replace "*)" by "* )" to prevent end of comment
        s := StringReplace(s, '*)', '* )', [rfReplaceAll]);
        if s[length(s)] <> '\' then
          break;  //last line
        if length(s) > 3 then begin //exacter: length(trim(s)) > 1
          SetLength(s, length(s) - 1);  //strip \
          WriteLn(s);
        end;
      end;
      WriteLn(s);  //last line
      Write('*) ');
    end;
    Outdent('end;');
    //WriteLn(' ');
  end;

begin //WriteImpl
  //self.fImpl := True;
  self.SectionType := stImpl;
  WriteLn(' ');
  Outdent('implementation');
  //WriteLn(' ');
  self.curkind := stUnknown;  //-> empty line before next clause
//show statics and declarations
  for i := 0 to scope.Count - 1 do begin
    sym := Scope.getSym(i);
    if (sym.Def <> '') and (sym.Def[1] = '!') then
      TranslateSym(sym)
    else if sym.kind = stProc then
      WriteProcSym;
  end;
//show macros
if ShowMacs then begin
  if Globals.DefFile <> '' then begin
  { TODO 1 : use new DefFile interface!!! }
    AssignFile(fin, Globals.DefFile);
    Reset(fin);
    ReadLn(fin, s);
    if s[1] = '#' then begin
    //new format
      while not EOF(fin) do begin
        if ReadLine then
          ConvertMac;
      end;
    end else begin
    //old format
    //seek the macro definitions
      repeat
        ReadLn(fin, s);
      until Copy(s, 1, 5) = '--- m';
    //convert the macro definitions
      while not EOF(fin) do begin
        ReadLn(fin, s); //#define, name and parameter list
        //if s[1] = '#' then begin
        if (s[1] = '#') and (s[length(s)] = '\') then begin
        //exclude #defines without body! -> {$DEFINE???}
          s := Copy(s, 9, Length(s));
          ConvertMac;
        end;
      end;  //else format error, or dummy without value
    end;
  end;
end;
  indent := 0;
  WriteLn('end.');
end;
{$ENDIF}



//procedure ToPas(const fn: string);
procedure ToPas(const fn: string; fSort: boolean = false);
begin //ToPas
  if XToPas.hToPas(fn, '') then begin
    XToPas.Target.SaveToFile(XToPas.ModuleName + '.pas');
  end;
{$IFDEF old}
  if fUniqueNames then
    CheckNames('');
  s := ChangeFileExt(fn, '.pas');
  AssignFile(f, s);
  Rewrite(f);
  curkind := stUnknown;
  typeswritten := False;
  try
    WriteHeader;
    if fSort then begin
      WriteAllTypes(Globals);
      WriteAllConsts(Globals);
      WriteAllVars(Globals);
      WriteAllProcs(Globals);
    end else
      WriteIntf(Globals);
  //ToDo: Macros should go to both interface and implementation!!!
    WriteImpl;
  finally
    CloseFile(f);
  end;
{$ELSE}
{$ENDIF}
end;

{$IFDEF new}
function  CheckMeta(mf: TModule): boolean;
//function  CheckMeta(const fn: string): boolean;
var
  i: integer;
  f: TextFile;
  ln, s: string;

  procedure ReadLine;
  begin
    ReadLn(f, ln);
    if fFmt in [fmt10, fmt11] then begin
      i := Pos(#9, ln);
      i := PosEx(#9, ln, i+1);
      symc := ln[i+1];  //type character
      ln := Copy(ln, i+2, Length(ln));
    end;
    if symc < 'a' then begin
    //macro format
      while ln[Length(ln)] = '\' do begin
      //merge continued line
        ReadLn(f, s);
        assert(s[1] = #9, 'expected <tab>');
        ln := ln + Copy(s, 2, Length(s));
      end;
    end else begin
      while ln[Length(ln)] = ListTerm do begin
      //merge continued line
        ReadLn(f, s);
        assert(s[1] = #9, 'expected <tab>');
        ln := ln + Copy(s, 2, Length(s));
      end;
    end;
  end;

begin //CheckMeta
  s := mf.Deffile;
  if False then
    mf.LoadFromFile(s); //hyperlink only
    ...
  AssignFile(f, s);
  Reset(f);
  while not EOF(f) do begin
    ReadLine;
    case symc of
    //symbols (without macros)
    'S':  //skSym,    //default: any symbol
      ; //ignore: #define without body
    'K':  //skCKey,   //treat as keyword (of appkind), disallow redefinition
      ; //ignore: C keyword
    //if macro defined, treat definition as:
    'C':  //skConst,  //named constant, macro should be deleted?
      ; //todo: handle how?
    'T':  //skType,   //typename
      ; //todo: handle how?
    'P':  //skProc,   //procedure/function
      ; //todo: handle how?
    'M':  //skMacro   //expand as macro
      ; //todo: handle how?
    //not yet typed
    'u':  //stUnknown,
      ; //unexpected - ignore
    //preprocessor symbols???
    'm':  //stMacro,  //to be expanded
      ; //todo: handle how?
    'i':  //stInline, //treat as procedure, don't expand
      ; //todo: handle how?
    //both preprocessor and parser???
    'c':  //stConst,
    { TODO : redesign Globals.Read... }
      begin
        ReadCVP;
        sym := Globals.defConst(n, t, v);
        if typ <> nil then sym.BaseType := typ;
      end;
    'e':  //stEnumMember,  //const variable, enum member, #define???
      begin
        ReadCVP;
        sym := Globals.defEnumMember(n, t, v);
        if typ <> nil then sym.BaseType := typ;
      end;
    //parser created
    'v':  //stVar,
      begin
        ReadCVP;
        sym := Globals.defVar(n, t, v);
        if typ <> nil then sym.BaseType := typ;
      end;
    'p':  //stProc,
      begin
        ReadCVP;
        sym := Globals.defProc(n, t);
      end;
    't':  //stTypedef //stay last
      begin
        ReadTP;
        typ := defType(n, t, 0);
      end;
    end;
  end;
  CloseFile(f);
end;
{$ELSE}
{$ENDIF}

initialization
//hard coded cast operator - import from here???
  assert(opCast=opSharpAt, 'update aPasOp table');
  XToPas := TToPas.Create;
  Translator := XToPas;
finalization
  FreeAndNil(Translator);
end.
