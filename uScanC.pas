unit uScanC;
(* Implement an static scanner/preprocessor API.
User:
ScanFile: start scanner/preprocessor
nextTokenC: get next token

ToDo:
- add token type to SetTokenString
+ option: fWantRems (default: True)
+ handle directive ends in handleDirective
+ protect all string scans, ret t_err on unpaired quotes
+ Added smEol scan mode for skipping to EOL.
+ fully conforming macro expansion, argument prescan, # and ## operators
+ lookahead for macro argument list "("
~ C keywords
- conforming preprocessor number scan, value handling
- option: wantNoEol
- digraphs, trigraphs
- optional C++ support (operators, keywords, alternate tokens)
*)


interface

uses
  Classes,
  config, //{$I config.pas}
  uTablesPrep, uFiles, uTokenC;

// ----------------- config -------------------

const
  fWarnRedef = False;
var
  fVerbose: boolean = False;  //log source lines? (nextLine)
  fLogLines: boolean = True;  //log non-preprocessor lines? (nextTokenC)
  fWantRems: boolean = False; //show comments? (otherwise as empty strings)
  //OnLineChange: TLineCallback; - see below!

// ------------------ API ---------------------

function  ScanFile(const fn: string): TFile;
function  nextTokenC: eToken;  //preprocessed!

type
  TStrCvt = function(const s: string; delim: char; fLong: boolean): string;
function  AnsiToC(const s: string; delim: char; fLong: boolean): string;

procedure	SetTokenString(var t:	RPreToken; var s: string);
function  TokenText(const t: RPreToken): string;  //for ##
//function  TokenString(fFirst: boolean = False): string; //for #
function  TokenString(fFirst: boolean = False; strcvt: TStrCvt = nil): string; //for #
  //quoted string literals

//preprocessing
function nextNoWhite: eToken;
//internal
function  const_expressionC(fSkip: integer = 0): integer;

// -------------- low level, internals --------------

type

(* Scan modes
The source mode is a static property of the scanner.
eSrcMode specifies the "language" to use:
  C (default)
  Meta (reading meta file)

The scan mode is an argument to nextRaw.
eScanMode specifies special actions:
  smStd (default)
  smHeader (scan header name for #include)
  smEol (make everything up to EOL an comment)
    The scanner returns t_rem or t_empty, stops at EOL
*)

{$IF __Filter}
  eScanMode = (
    //smCSource,  //default, C source files
    smStd,      //default language
    smHeader,   //header name for #include
    smEol       //skip to EOL, return t_rem or t_empty at EOL
    //smMeta      //meta file, internal format for strings...
  );

  eSrcMode = (
    smCSource,  //default, C source files
    smMeta      //meta file, internal format for strings, numbers...
  );
{$ELSE}
  eScanMode = (
    smCSource,  //default, C source files
    smHeader,   //header name for #include
    smEol,      //skip to EOL, return t_rem or t_empty at EOL
    smMeta      //meta file, internal format for strings, chars...
  );
{$IFEND}

type
  eScanState = (
    ssDefault, ssInRem
  );

//scanner with C preprocessor
  TFileC = class(TTokenStream)
  protected
    LineBuf:  string;
    pc0, pc1, pcN: PChar;  //for preprocessor
    Flock: integer;
    FLockDelay:  boolean;
    state:  eScanState;
    function  nextLine: boolean;
    function  _nextRaw(fSkip: boolean; fMode: eScanMode): eToken;
    function  handleDirective: eToken;
  public
    atBOL: boolean; //# means directive
    mode: eSrcMode; //C, Meta...
    src:  TFile;
    CurLine: integer; //to be renamed?
  //native
    constructor Create;
    constructor CreateFromFile(Asrc: TFile);
    function  scanString(const s: string): eToken; //not file based
  //inherited
    function  unGet: boolean; override;
    function  nextRaw: eToken; override;    //no interpretation
    function  nextToken:  eToken; override; //handle locks, directives
    procedure rewind; override;
  //properties
    property  LockCount: integer read Flock;
    property  LineText: string read LineBuf;
  end;

//implement #include and macro substitution
  TTokenStack = object  //singleton
  public
    Stack: array[0..64] of TTokenStream;
    Count:  integer;
    CurFile:  TFileC;
    procedure Init; //~constructor
    function  pushFile(src: TFile): TFileC;
    procedure push(src: TTokenStream);
    function  pop: boolean;
    function  Expand(sym: PSymPrep): eToken;
    function  BaseFile: TFileC;
  end;

  eLineChange = (
    lcOpenFile,
    lcResumeFile,
    lcCloseFile,
    lcNewLine
  );

  TLineCallback = procedure(change: eLineChange; f: TFileC) of object;

var
  OnLineChange: TLineCallback;

var
  StringScanner:  TFileC; //=TokenStack.Stack[0]
  TokenStack: TTokenStack;
  pSrc: TTokenStream;
//for #line
  ScanningLine: integer;
  ScanningFile: string;
//Debugging...
  MonitoringFile: string; //break when file+line reached
  MonitoringLine: integer;
  InMonitorFile: boolean; //if in MonitoringFile (reduce string comparisons)

  ScanningText:  string;   //the current source line
  ScanningNext: PChar;    //next text to scan

const
//char classes
  whitespace = [#1..' '];
  OctDigits = ['0'..'7'];
  Digits = ['0'..'9'];
  HexDigits = Digits + ['A'..'F', 'a'..'f'];
  Alpha = ['A'..'Z', 'a'..'z'];
  AlNum = Digits + Alpha;
  SymFirst = Alpha + ['_'];
  SymNext = SymFirst + Digits + ['$'];  //mangled names!
  FltExp = ['e', 'E']; //['d', 'D', 'e', 'E', 'f', 'F'];  //what's really acceptable?
  FltPunct = FltExp + ['.'];

implementation

uses
  SysUtils, Windows, //DebugBreak
  uMacros, uDirectives, uParseC, uUI;

procedure CheckMonitor;
begin
//called when input file changes
  InMonitorFile := ScanningFile = MonitoringFile;
end;

(* SetTokenString - assign an short or long string.
*)
procedure	SetTokenString(var t:	RPreToken; var s: string);
begin
  assert(t.kind in [t_err, t_car, t_str], 'no string-token');
	if Length(s) <= 4 then begin
  //short string, store within token
  	t.fShort := True;
    t.cval := 0;	//clear all chars, for retrieval as integer value
    t.chars := s;
  end else begin
  	t.fShort := False;
    t.stringID := StringTable.Add(s); //mapString(s);
  //map to unique string!
    s := StringTable.Strings[t.stringID];
  end;
end;

(* StringLiteralC - convert embedded control chars.
  s is an AnsiString, possibly with embedded control chars.
  Output format is C style.
*)
//function  StringLiteralC(const s: string; delim: char; fLong: boolean): string;
function  AnsiToC(const s: string; delim: char; fLong: boolean): string;
var
  i, d, nc: integer;
  c: char;
const
  RepChar: array[#7..#13] of char = 'abtnvfr';
  HexChar: array[0..15] of char = '0123456789ABCDEF';
begin
  nc := 0;
  for i := 1 to length(s) do
    if (s[i] < ' ') or (s[i] = '\') or (s[i] = delim) then
      inc(nc);
  if nc > 0 then begin
  //string needs expansion
    SetLength(Result, Length(s) + nc*3 + 3);
    //Result := StringOfChar(' ', Length(s) + nc*4);
    //pSrc := PChar(s);
    //pDst := PChar(Result);
    d := 1;
    if fLong then begin
      Result[d] := 'L'; inc(d);
    end;
    Result[d] := delim; inc(d);
    for i := 1 to Length(s) do begin
      c := s[i];
      case c of
      #0..#6,
      #14..#15:
        begin
          Result[d] := '\'; inc(d);
          Result[d] := 'x'; inc(d);
          Result[d] := '0'; inc(d);
          Result[d] := HexChar[ord(c) and $F]; inc(d);
        end;
      #7..#13:
        begin
          Result[d] := '\'; inc(d);
          Result[d] := RepChar[c]; inc(d);
        end;
      #16..#31:
        begin
          Result[d] := '\'; inc(d);
          Result[d] := 'x'; inc(d);
          Result[d] := '1'; inc(d);
          Result[d] := HexChar[ord(c) and $F]; inc(d);
        end;
      '\':
        begin
          Result[d] := '\'; inc(d);
          Result[d] := '\'; inc(d);
        end;
      else
        if c = delim then begin
          Result[d] := '\'; inc(d);
          Result[d] := c; inc(d);
        end else
          Result[d] := c; inc(d);
      end;
    end;
    Result[d] := delim;
    SetLength(Result, d);
  end else if fLong then
    Result := 'L' + delim + s + delim
  else
    Result := delim + s + delim
end;

(* TokenText - primarily for "##"

Note: strings and chars
  Since string concatenation is done in the last scanner phase,
  we don't assume that string and char literals occur as ## operands.
  For a more general usage of this function only the characters are returned,
  as single byte (not wide!) chars, unescaped and unquoted.
  UCNs are retained as provided in the source code.
    !causes problem in converting back to C style!

ToDo: implement text reconstruction for synthetic tokens (##, builtin macros...)
*)
function  TokenText(const t: RPreToken): string;
begin
  case  t.kind of
  t_err, t_car,
  t_str:  //should never occur?
    if t.fShort then
      Result := t.chars
    else
      Result := StringTable.Strings[t.stringID];
  t_sym, t_symNX:
    Result := Symbols.Strings[t.symID];
  //t_arg: - unexpected
  t_int:
    Result := IntToStr(t.ival); //__LINE__
  else  //Warning: is this appropriate for ALL other tokens?
    if t.kind in ConstTokens then
      Result := TokenNames[t.kind]
    else if t.pc <> nil then
      SetString(Result, t.pc, t.len)
    else begin
      Log('ToDo: no token text available', lkBug);
      Result := '???';
    end;
  end;
end;

(* TokenString - for "#"
  Return display string - convert controls in string/char?
  Various string formats: C, Pascal, Ansi, UTF-8, Meta!
  (Pascal is NOT lossless!!!)
*)
function  TokenString(fFirst: boolean = False; strcvt: TStrCvt = nil): string;
const
  delims: array[t_str..t_car] of char = '"''';
begin
  case ScanToken.kind of
  t_bol, t_NoEol:
    if fWantRems then begin
      SetString(Result, ScanToken.pc, ScanToken.len); //expand tabs?
      //Result := StringOfChar(' ', ScanToken.len);
    end;
  t_rem:  //todo: if really required?
    if fWantRems then begin
      SetString(Result, ScanToken.pc, ScanToken.len);
      if not (taRemBeg in ScanToken.attrs) then begin
      //for optimization reasons no t_bol is output inside comments.
        Result := EOLstr + Result;
      end;
    end else if fFirst then
      Result := ''
    else
      Result := ' ';
  t_err:
    Result := TokenText(ScanToken); //.chars;
  t_str,
  t_car:
    if assigned(strcvt) then
      Result := strcvt(TokenText(ScanToken),
        delims[ScanToken.kind], taLong in ScanToken.attrs)
    else
      Result := AnsiToC(TokenText(ScanToken),
        delims[ScanToken.kind], taLong in ScanToken.attrs);
  t_int:
    Result := IntToStr(ScanToken.ival);
  t_sym, t_symNX:
    Result := Symbols.Strings[ScanToken.symID];
{argument handling is up to the caller!
  t_arg, t_argNX:
    Result := 'arg#' + IntToStr(ScanToken.argID) + '??? ';
}
  else
    if ScanToken.kind in ConstTokens then
      Result := TokenNames[ScanToken.kind]
    else if ScanToken.pc <> nil then
      SetString(Result, ScanToken.pc, ScanToken.len)
    else begin
      Log('ToDo: no token text available', lkBug);
      Result := '???';
    end;
  end;
  if not fFirst and (taWhiteBefore in ScanToken.attrs) then
    Result := ' ' + Result;
end;


(* nextNoEof - filter t_eof
Drop macros after expansion, source files at EOF.
Called from 2 places:
  constant_expression (raw, for preprocessor expressions)
  nextTokenC (cooked, of course)
*)
function nextNoEof(fRaw: boolean): eToken;
begin
  repeat
    if fRaw then
      Result := pSrc.nextRaw
    else
      Result := pSrc.nextToken;
    if Result = t_eof then begin
      if pSrc.fCont and TokenStack.pop then
        //continue
      else
        exit; //return EOF when all files are processed
    end;
  until Result > t_eof;
end;

(* nextNoWhite
  Skip rems, eol and other low-level tokens.
  In preprocessor mode reading file (raw, unlocked) is assumed.

  fPre is always True!
*)
//function nextNoWhite(fPre: boolean): eToken;
function nextNoWhite: eToken;
begin
  repeat
    Result := pSrc.nextRaw
  until not (Result in WhiteTokensPrep);
end;

// ---------------------------

(* nextTokenC - top level scanner method.
  Returns the next fully preprocessed token.
  Macro expansion is implemented here,
    all other low-level filters are implemented in subordinate layers.
  Filtering whitespace etc. depends on the actual application.
  We may introduce appropriate global flags?
    fWantRems:  introduced but not yet used here
    fWantWhite: (to come) want white tokens (for cross-compiler)
  ToDo:
    include mapping of C keywords (here).
    concatenate strings - if desired, for full C compatibility.
    improved handling of rems: attach to preceding token (see uTokenC)
*)
var
  CurLineC: string;
function  nextTokenC: eToken;  //preprocessed!
begin
  Result := nextNoEof(False); //close files and macros
  while (Result = t_sym) and IsExpandableC(@ScanSym) do begin
  //perform macro expansion
    Result := TokenStack.Expand(@ScanSym);
  end;
(* Handling of /##/, as suggested by RP
  Problem: cannot call pSrc._nextRaw(False, smEol);
    which would scan and return an proper comment.
  ToDo: drop all open macros, call TokenStack.CurFile._nextRaw(True?, smEol)
*)
  if Result = opDivDiv then begin     // RP added  these 3 lines
  (* MSC assumes that /##/ is treated as a comment start!
    Since such constructs do not occur in ordinary code,
    we do what MSC means and skip to the next line.
  *)
    while not (Result in [t_bol, t_eof]) do
       //Result := pSrc.nextToken;
       Result := nextNoEof(False); //returns t_eof only if file stack empty
  end;
  if fLogLines then begin
  //delay logging by 1 token
    if Result = t_bol then
    //remember line
      CurLineC := TokenStack.CurFile.LineBuf
    else begin
      if (CurLineC <> '') and not (Result in whiteTokens) then begin
        Log(CurLineC);
        CurLineC := '';  //mark shown
      end;
    end;
  end;
end;

function ScanFile(const fn: string): TFile;
begin
  Log('Processing ' + fn);
  //Result := TFile.CreateFrom('', fn);
  //Result := Files.NewFile('', fn, fkSource);
  Result := GetFile(fn);
  assert(Result <> nil, 'file not found or already processed?');
  pSrc := TokenStack.pushFile(Result);
end;

(* const_expression - constant expression evaluator.
  For low level use only, recognizes only part of the expression syntax.
  fSkip should be zero, is used only internally (recursion).
*)
//function const_expression(fPre: boolean; fSkip: integer): integer;
//function const_expression(fSkip: integer): integer;
function const_expressionC(fSkip: integer): integer;
var
  i_ttyp: eToken;

  (* filter - skip over unwanted tokens, expand macros
    True: skip token
  *)
  function  filter: boolean;
  begin
    i_ttyp := ScanToken.kind;
    case i_ttyp of
    t_empty:  Result := True;
    t_eof:  Result := False;  //unexpected!
    t_bol:  Result := False;  //not fPre;
    t_NoEol, //skip always
    t_rem:  //skip always
      Result := True;
    t_sym:
      if IsExpandablePrep(@ScanSym) then begin
      //problem: empty bodies? -> no value, should be zero!
        i_ttyp := TokenStack.Expand(@ScanSym);
        Result := filter;
      end else
        Result := False;
    //t_symNX: - as is
    opDivDiv: //handle /##/
      begin
        while not (i_ttyp in [t_eof, t_bol]) do
          pSrc.nextRaw;
        Result := False;  //don't filter these tokens, see above
      end;
    else
      Result := False;
    end;
  end;

  (* gtok - get next token, across macros, expanded.
    Note: "defined" requires unexpanded macros - special handling there!
    Note: preprocessor expressions eval even in file locks! (must use raw mode)
  *)
  procedure gtok;
  begin
    repeat
      i_ttyp := nextNoEof(True);  //fPre);
    until not filter;
  end;

  function  expect(t: eToken; const msg: string): boolean;
  begin
    Result := ScanToken.kind = t;
    if Result then
      gtok
    else
      Log(msg, lkSynErr);
  end;

(*
primary_expression :
identifier
constant
//NOT numeric! string_literal
"(" expression ")"

------- NOT const! ------
postfix_expression :
primary_expression {
  "[" expression "]"
  "(" [argument_expression_list] ")"
  "." identifier
  "->" identifier
  "++"
  "--"
}
----------

unary_expression :
postfix_expression -> primary_expression
//NOT const! "++" unary_expression
//NOT const! "--" unary_expression
//unary_operator cast_expression ->
unary_operator unary_expression
"sizeof" unary_expression
"sizeof" "(" type_name ")"
*)
  function unary_expression: integer; // + - ! ~ ()
  begin
    case i_TTYP of
    t_eof,  //means real end of input, all files processed
    t_bol:  //returned only in preprocessor mode, means end of expression
      Result := 0;  //flag end-of-expression how???
    //t_num,  //requires conversion!
    t_int:
      begin
        Result := ScanToken.ival;
        gtok;
      end;
    t_Lint:
      begin
        Result := ScanToken.lval;
        gtok;
      end;
    t_car:
      begin
        Result := integer(ScanToken.cval);
        GTOK();
      end;
    t_flt:
      begin
        Result  := Round(ScanToken.d);
        GTOK();
      end;
    t_sym:
      if (ScanText = 'defined') then begin
      //no macro expansion here!
        i_ttyp := nextNoWhite;
        if i_ttyp = opLPar then begin
        //defined ( <sym> )
          i_ttyp := nextNoWhite;
          assert(i_ttyp = t_sym, 'expected symbol');
          Result := ord(IsDefined(@ScanSym));
          gtok;
          expect(opRPar, '")" after "defined("');
        end else if i_ttyp = t_sym then begin
        // defined <sym>
          Result := ord(IsDefined(@ScanSym));
          gtok;
        end else if i_ttyp = t_symNX then begin
        //who said that this symbol should not be expanded?
          Log('unexpected const t_symNX', lkBug);
          Result := ord(IsDefined(@ScanSym));
          gtok;
        end else begin //assume undefined
          Result := 0;
          gtok;
        end;
      end else begin  //symbol, not #defined
      //expect const name
        Result := 0;  //undefined symbols default to value 0.
        gtok;
      end;
    t_symNX:  //unexpandable macro?
      begin
        Result := 0;  //assume undefined
        gtok;
      end;
    opADD:  begin //+ (ignore unary +)
        GTOK();
        Result  := unary_expression ;
      end;
{$IF __opMul}
    opSub0:  begin //-
{$ELSE}
    opSub_:  begin //-
{$IFEND}
        GTOK();
        Result  := -unary_expression ;
      end;
    logNOT: begin //!
        GTOK();
        Result  := ord(unary_expression  = 0);
      end;
    binNot:  begin //~
        GTOK();
        Result  := NOT unary_expression ;
      end;
    opLPar:  //'('
      begin
        GTOK();
        Result  := const_expressionC(fSkip);
        expect(opRPar, 'expected ")"');
      end;
    else
      include(ParserFlags, pfNotConst);
      assert(false, 'not const?');  //what if happens?
      Result := 0;  //?
    end;
  end;

(*
multiplicative_expression :
cast_expression {( "*" | "/" | "%") cast_expression}

cast_expression :
unary_expression
//NOT const! "(" type_name ")" cast_expression

*)
  function multiplicative_expression: integer; // * / %
  var
    d: integer;
  begin
    Result  := unary_expression ;
    WHILE True do begin
      case i_TTYP of
{$IF __opMul}
      opStar0: // '*'
{$ELSE}
      opStar_: // '*'
{$IFEND}
        begin
          GTOK();
          Result := Result * unary_expression;
        end;
      opDiv:  // '/'
        begin
          GTOK();
        //D4: problem catching /0 exception!
          d := unary_expression;
          if d = 0 then
            Result := -1
          else
            Result := Result div d;
        end;
      opMOD:  // '%'
        begin
          GTOK();
          Result  := Result  MOD unary_expression ;
        end;
      else  //DEFAULT
        break;
      end;
    end;
  end;

(*
additive_expression :
multiplicative_expression {("+" | "-") multiplicative_expression}
*)
  function additive_expression: integer; // + -
  begin
    Result  := multiplicative_expression ;
    WHILE True do begin
      case i_TTYP of
      opADD:  //'+'
        begin
          GTOK();
          inc(Result, multiplicative_expression);
        end;
{$IF __opMul}
      opSub0:  //'-'
{$ELSE}
      opSub_:  //'-'
{$IFEND}
        begin
          GTOK();
          dec(Result, multiplicative_expression);
        end;
      else  //DEFAULT
        break;
      end;
    end;
  end;

(*
shift_expression :
additive_expression {("<<" | ">>") additive_expression}
*)
  function shift_expression: integer; // << >>
  begin
    Result  := additive_expression ;
    WHILE True do begin
      case i_TTYP of
      opSHL: //'<<'
        begin
          GTOK();
          Result  := Result  shl additive_expression;
        end;
      opShr:  //'>>'
        begin
          GTOK();
          Result  := Result  shr additive_expression;
        end;
      else  //DEFAULT
        break;
      end;
    end;
  end;

(*
relational_expression :
shift_expression {("<" | ">" | "<=" | ">=") shift_expression}
*)
  function  relational_expression: integer;
  begin
    Result := shift_expression;
    while True do begin
      case i_ttyp of
      opLT:
        begin
          gtok;
          Result := ord(Result < shift_expression);
        end;
      opLE:
        begin
          gtok;
          Result := ord(Result <= shift_expression);
        end;
      opGT:
        begin
          gtok;
          Result := ord(Result > shift_expression);
        end;
      opGE:
        begin
          gtok;
          Result := ord(Result >= shift_expression);
        end;
      else  //no relational operator
        break;
      end;
    end;
  end;
(*
equality_expression :
relational_expression {("==" | "!=") relational_expression}
*)
  function equality_expression: integer; // == !=
  begin
    Result  := relational_expression;
    WHILE True do begin
      case i_TTYP of
      opEQ: // '=='
        begin
          GTOK();
          Result  := ord(Result = relational_expression);
        end;
      opNE: // '!='
        begin
          GTOK();
          Result  := ord(Result <> relational_expression);
        end;
      else  //DEFAULT
        break;
      end;
    end;
  end;

(*
AND_expression :
equality_expression {"&" equality_expression}
*)
  function AND_expression: integer; // &
  begin
    Result  := equality_expression ;
{$IF __opMul}
    WHILE i_TTYP  = opAmpersAnd  do begin
{$ELSE}
    WHILE i_TTYP  = binAnd_  do begin
{$IFEND}
      GTOK();
      Result  := Result  AND equality_expression ;
    end;
  end;

(*
exclusive_OR_expression :
AND_expression {"^" AND_expression}
*)
  function exclusive_OR_expression: integer; // ^
  begin
    Result  := AND_expression;
    WHILE i_TTYP  = opXor  do begin
      GTOK();
      Result  := Result  XOR AND_expression;
    end;
  end;

(*
inclusive_OR_expression :
exclusive_OR_expression {"|" exclusive_OR_expression}
*)
  function  inclusive_OR_expression: integer;
  begin
    Result := exclusive_OR_expression;
    while i_ttyp = binOR do begin
      gtok;
      Result := Result or exclusive_OR_expression;
    end;
  end;

(*
logical_AND_expression :
inclusive_OR_expression {"&&" inclusive_OR_expression}
*)
  function  logical_AND_expression: integer;
  begin
    Result := inclusive_OR_expression;
    while i_ttyp = logAnd do begin
      gtok;
      if Result = 0 then begin
      //short circuit FALSE AND <anything>
        inc(fSkip);
        inclusive_or_expression;
        dec(fSkip);
      end else begin
        Result := ord(inclusive_OR_expression <> 0);
      end;
    end;
  end;

(*
logical_OR_expression :
logical_AND_expression {"||" logical_AND_expression}
*)
  function  logical_OR_expression: integer;
  begin
    Result := logical_AND_expression;
    while i_ttyp = logOr do begin
      gtok;
      if Result <> 0 then begin
      //short circuit TRUE OR <anything>
        Result := 1;  //True
        inc(fSkip);
        logical_and_expression;
        dec(fSkip);
      end else
        Result := {FALSE or} ord(logical_AND_expression <> 0);
    end;
  end;

(*
constant_expression :
conditional_expression

conditional_expression :
logical_OR_expression ["?" constant_expression ":" conditional_expression]
logical_OR_expression {"?" constant_expression ":" logical_OR_expression}
*)
var
  cond: boolean;
begin //const_expression
//skip to the first usable token
  if filter then
    gtok;  //filter current token
//ternary eypression
  Result := logical_OR_expression;
  //["?" constant_expression ":" conditional_expression]
  if i_ttyp = opTern then begin
    gtok;
    cond := Result <> 0;
    if not cond then
      inc(fSkip); //skip True branch
    Result := const_expressionC(fSkip);
    expect(opColon, 'expected ":"');
    if cond then begin
    //don't eval False branch
      const_expressionC(1);
    end else begin
    //eval False branch
      dec(fSkip);
      Result := const_expressionC(fSkip);
    end;
  end;
end;

{ TFileC }

procedure TFileC.rewind;
begin
  self.CurLine := 0;
  self.Flock := 0;
  self.state := ssDefault;
  if self.src <> nil then begin
    ScanningFile := self.src.name;
    ScanningLine := CurLine;
    //ScanningText := LineBuf; - not yet valid
    atBOL := True;
    CheckMonitor; //debugging aid
  end;  //else not file based
  nextLine;
end;

constructor TFileC.Create;
begin
  self.fTemp := True;
  self.fFile := src <> nil; //when called from CreateFromFile
  self.fCont := self.fFile; //same condition
  rewind;
end;

constructor TFileC.CreateFromFile(Asrc: TFile);
begin
  src := Asrc;
  Create;
  //self.fCont := True; //not for base file!
end;

function TFileC.handleDirective: eToken;
var
  dir:  TDirective;
  fError: boolean;

  procedure handleElif;
  begin
    FLockDelay := True;
    if const_expressionC({True,} 0) <> 0 then
      Flock := 0;  //unlock
  end;

  procedure handleIf;
  begin
  (* called only when not locked.
    Simulate waiting for True condition, process as #elif
  *)
    Flock := 1;  //wait for cond. True
    handleElif;
  end;

  procedure handleIfDef(fDef: boolean);
  begin
    FLockDelay := True;
    if Result <> t_sym then begin
      Log('expected #ifdef SYM!', lkSynErr);
    end else begin
      if IsDefined(@ScanSym) <> fDef then
      //condition false
        inc(Flock);
      //else true, don't block
    end;
  end;

  (* handleDefine - #define macro
    ToDo: prevent redefinition of certain (builtin...) macros.
    What if a non-macro object of the same name exists already?
  *)
  procedure handleDefine;
  begin
    if Result <> t_sym then begin
      Log('expected #define SYM!', lkSynErr);
    end else begin
      if ScanSym.FObject <> nil then begin
      //check exist/override
        if fWarnRedef and IsDefined(@ScanSym) then
          Log('redef macro ' + ScanSym.FString, lkTrace);
      end;
      if pcN^ = '(' then
        TMacroFunc.Define(self)
      else
        TMacro.Define(self);
    end;
  end;

  procedure handleUndef;
  begin
    if Result <> t_sym then begin
      Log('expected #undef SYM!', lkSynErr);
    end else if IsDefined(@ScanSym) then begin
    //remove macro definition
      Symbols.defMacro(ScanToken.symID, skSym);
    //get updated symbol entry
      ScanSym := Symbols.getSymbol(ScanToken.symID)^;
    end;  //else is already undefined
  end;


type
  eIncludeKind = (
    ikStd,  //"file"
    ikSys,  //<file>
    ikNext  //#include_next
  );

  procedure handleInclude(ikind: eIncludeKind);
  var
    fn: string;
    f:  TFile;
    strm: TTokenStream;
  begin
  (* The immediately following token has been read, in header scan mode.
    It must be a string (header name) or a macro name (to be expanded).
    After expansion, a '<' token may indicate a system file name,
      but is not handled yet. !Macro has not been scanned in header mode!
  *)
    case Result of
    t_str:  //pp header-name, saSysHdr is set for <...> format
      begin
        fn := ScanText;
        if (ikind = ikStd) and (saSysHdr in ScanToken.attrs) then
          ikind := ikSys;
      end;
    t_sym:  //allow for macros - untested!
      begin
        Log('#include t_sym untested', lkTodo);
        if IsExpandablePrep(@ScanSym) then begin
          strm := TMacro(ScanSym.FMacro).Expand(self);
          if strm <> nil then begin
            strm.firstToken;
            strm.rewind;
            if strm.fTemp then
              strm.Free;
          end;
          if ScanToken.kind <> t_str then
            assert(false, 'unexpected header macro');
            //todo: get stream text and scan in header mode?
          fn := TokenText(ScanToken); // StringTable.Strings[ScanToken.strID];
          //handle sys files? - #define mac <...> will most likely scan as garbage
        end; //expand
      end;  //t_sym
    else
      Log('unexpected #include filename', lkSynErr);
      //exit;
    end; //case Result

//consume tokens up to t_eol here???

//check for parsed filename
    if fn = '' then begin
      //Log('unexpected #include filename representation', lkErr);
      Log('unexpected #include filename', lkSynErr);
      //Result := ???
       exit;
    end;

    case ikind of
    ikStd:  f := IncludeFile(fn);
    ikSys:  f := IncludeSysFile(fn);
    ikNext: f := IncludeNextFile(fn, src.foundWhere, fkIncludeOnce);
    else  //keep compiler happy?
      f := nil;
      assert(False, 'error include kind');  //should never happen
    end;
    if f <> nil then begin
      TokenStack.pushFile(f);
      if assigned(PreInclude) then
        PreInclude(f);
    end;
  end;

  procedure handleModule;
  var
    fn: string;
  begin
    fn := ScanText;
    ParseCModule(fn, True);
  end;

  procedure handleError;
  begin
    //ScanToken.kind := t_err;
  //get error message - throw exception?
    if self._nextRaw(False, smEol) = t_rem then
      //Log('#error ' + ScanSym.FString, lkSynErr)
      Log('#error ' + ScanText, lkSynErr)
    else  //no text?
      Log('#error', lkSynErr);
    Result := t_err;  //treat as error, regardless of actual token kind
    fError := True;
  end;

  procedure handleWarning;
  begin
    //ScanToken.kind := t_empty;
  //get error message - throw exception?
    if self._nextRaw(False, smEol) = t_rem then
      //Log('#warning ' + ScanSym.FString, lkWarning)
      Log('#warning ' + ScanText, lkWarning)
    else
      Log('#warning', lkWarning);
    //Result := t_empty;  //??? - how to preserve the string?
  end;

  procedure handleLine;
  begin
  (* Syntax: #line number ["filename"]
    Expand macros.
    Expect linenumber (num) and optional filename (string).
    Overwrite actual file and line number.
  *)
    //untested!
    if nextNoWhite = t_int then begin
      ScanningLine := ScanToken.ival - 1; //incremented in nextLine()
      if nextNoWhite = t_str then
        ScanningFile := TokenText(ScanToken);
    end;
  end;

  procedure handlePragma;
  var
    prag: TPragma;
    root: string;
  begin
    if Result <> t_sym then
      exit;
    //prag := Pragmas.findPragma(ScanToken.symID);
    prag := Pragmas.getPragma(ScanSym.pragkind);
    if prag = nil then
      exit;
    case prag.kind of
    psNULL: ; //undefined
    psInclude,  //add search path
    psSource:
      begin
        while _nextRaw(false, smHeader) = t_str do begin
          if ScanText[1] = '+' then begin
          //relative path
            AddIncludeDir(root + copy(ScanText, 2, Length(ScanText)), prag.kind = psInclude);
          end else begin
          //absolute path
            root := ScanText;
            AddIncludeDir(root, prag.kind = psInclude);
          end;
        end;
      end;
    psModule: //process source module
      if _nextRaw(false, smHeader) = t_str then
        handleModule;
    psUser: if assigned(prag.Handler) then prag.Handler(self);
    else  //pragma should be builtin!
      assert(False, 'pragma not implemented');
    end;
  end;

var
  dirName: string;
const
  aScanMode: array[boolean] of eScanMode = (smStd, smHeader);
begin //handleDirective - called at '#'
(* handle preprocessor directives
  What about the Null directive (no sym?)
  ToDo: add directives to symbol table,
    names: "#"<dirname>
    kind: skDirective
*)
  fError := False;  //set in #error
//get directive name/sym
  Result := self._nextRaw(False, smStd);
  if Result = t_sym then begin
    dirName := ScanSym.FString;
    dir := Directives.getDirective(ScanSym.dirkind);
    if dir = nil then begin
      Log('unhandled #' + dirName, lkConfig);
    end else begin
    //read ahead next token - special scan for header names!
    (* Synchronization problem: lock count vs. reading next line!
      Don't read ahead on #else/#endif, adjust lock count before.
      Problem with #if/#elif: condition stops on t_bol, having read the next line.
      Pass an (intended) adjustment of the lock count to the OnLineChange callback?
    *)
      if not (dir.kind in [pdError, pdWarning, pdElse, pdEndif]) then
        Result := self._nextRaw(False, aScanMode[dir.attrib = daInclude]);
    //decode directive
      case Flock of
      0:  //unlocked - use stored handler?
        if assigned(dir.Handler) then
          dir.Handler(self)
        else
          case dir.kind of
          pdNULL: ;
          pdEndif:  Flock := 0;  //handleEndif;
          pdIf:     handleIf;
          pdIfDef:  handleIfDef(True);
          pdIfNDef: handleIfDef(False);
          pdElif,
          pdElse:   Flock := 2;  //handleElse;
          pdInclude:  handleInclude(ikStd);
          pdIncludeNext:  handleInclude(ikNext);
          pdDefine: handleDefine;
          pdUndef:  handleUnDef;
          pdError:  handleError;
          pdLine:   handleLine;
          pdWarning:  handleWarning;
          pdPragma: handlePragma; //also used for _Pragma!
          else  //default
              Log('unhandled #' + dirName, lkTodo);
          end;
      1:  //wait for true
      //may become true/unlocked by #elseif, #else or #endif
        case dir.kind of
        pdEndif: Flock := 0;  //handleEndif;
        pdIf..pdIfNDef: inc(Flock, 2);
        pdElif: handleElif;
        pdElse: Flock := 0;  //handleELSE; - stop waiting!
        //pdInclude, pdDefine, pdUndef, pdError, pdFile, pdPragma: ; //skip
        end;
      else  //deep lock, only unlock at #endif
        case dir.kind of
        pdEndif:  dec(Flock, 2); //deepUnlock
        pdIf, pdIfDef, pdIfNDef:  inc(Flock, 2); //deeper lock
        end;
      end;
    end;  //else NULL directive?
  end;
//finish directive, skip to EOL
  if not fError then begin
    Result := ScanToken.kind;
    if not (Result in [t_eof, t_err, t_bol]) then begin
    //at least #pragma can have continuation lines
      repeat
        Result := self._nextRaw(True, smStd);
      until Result in [t_eof, {t_err,} t_bol];
    end;
  //must exit after #include!
    if dir.kind in [pdInclude, pdIncludeNext] then
      Result := t_empty; //always?
  end;  //else #error directive, return as scanned.
  if FLockDelay then begin
    FLockDelay := False;
    if assigned(OnLineChange) then
      OnLineChange(lcNewLine, self);
  end;
end;

function TFileC.nextLine: boolean;
const
  empty: string = #0;
begin
//handle no-file (src = nil)
  Result := (src <> nil) and (CurLine < src.Count);
  if Result then begin
    atBOL := True; //# directive flag
    LineBuf := src.Strings[CurLine];
    ScanningText := LineBuf;
    if fVerbose then
      Log(LineBuf);
    pcN := PChar(LineBuf);
    inc(CurLine); //1-based!
    inc(ScanningLine);  //may be different after #line!
  (* distinguish here between lcFileOpen and lcLineChange?
    NO! rewind is called during close, and also positions to line 1.
    Only TokenStack knows the correct operation!
  *)
    if not FLockDelay and assigned(OnLineChange) then begin
      if (CurLine > 1) then
        OnLineChange(lcNewLine, self)
      //else OnLineChange(lcOpenFile, self);
    end;
  //debug
    if InMonitorFile and (ScanningLine = MonitoringLine) then begin
      //MonitorSet.edLineBuf.Text := LineBuf;
      DebugBreak; //you wanted it
    end;
  end else
    self.pcN := PChar(empty);
end;

(* _nextRaw - file lexer
Flags:
  fSkip:  skip some time/memory consuming actions (in #if FALSE branches)
  fHeader:  scan header name
*)
//function TFileC._nextRaw(fSkip: boolean; fHeader: boolean = False): eToken;
function TFileC._nextRaw(fSkip: boolean; fMode: eScanMode): eToken;
var
  pc: PChar;
  c:  char;

  procedure scanRem(ra: sTokenAttrs);
  begin
    while pc^ = #0 do begin
    //left at EOL
      if nextLine then begin
        pc := pcN;
        pc1 := pc;
        ScanToken.pc := pc;
      end else begin
        Log('unterminated REM at EOF', lkWarning);  //SynErr?
        Result := t_eof;
        exit;
      end;
    end;
  //no nested comments, for now
    while True do begin
      inc(pc);
      case pc^ of
      #0:
        begin
        //unterminated rem
          self.state := ssInRem;
          break;
        end;
      '/':
        if pc[-1] = '*' then begin
          self.state := ssDefault;
          include(ra, taRemEnd);
          inc(pc);  //skip '/'
          break;
        end;
      end;
    end;
    ScanToken.attrs := ra;
    if not fSkip then begin
      assert(pc > pc1, 'wrong pc1');
      assert((pc - pc1) <= Length(self.LineBuf), 'bad pc1');
      SetString(ScanText, pc1, pc - pc1);
    end;
    Result := t_rem;
  end;

  procedure scanRemEol;
  begin
    ScanToken.attrs := ScanToken.attrs + raEol;
    ScanToken.pc := pc1;
    pc := strscan(pc, #0);
    if not fSkip then
      SetString(ScanText, pc1, pc - pc1);
    Result := t_rem;
  end;

  procedure mkOp2(kind: eToken);
  begin
    inc(pc);  //skip second char of operator
    Result := kind;
  end;

  procedure mkOp(kind: eToken);
  begin
    if (kind in DupOps) and (c = pc^) then begin
      inc(kind, 2);
      inc(pc);
    end;
    if (kind in LetOps) and (pc^ = '=') then begin
      inc(kind);
      inc(pc);
    end;
    Result := kind;
  end;

  procedure scanReal;
  begin
    ScanToken.attrs := [faDbl];
  //at begin or after int?
    if pc^ = '.' then begin
    //fraction
      repeat
        inc(pc);
      until not (pc^ in digits);
    end;
    if pc^ in FltExp then begin //mark 'f'?
    //exponent
      inc(pc);
      if pc^ in ['+','-'] then
        inc(pc);
      while pc^ in digits do
        inc(pc);
    end;
    if not fSkip then begin
      SetString(ScanText, pc1, pc - pc1);
      ScanToken.d := StrToFloat(ScanText);
    end;
  //postfix
    case pc^ of
    'f', 'F': ; //begin inc(pc); ScanToken.fa := faFlt; end;
    'l', 'L': begin inc(pc); include(ScanToken.attrs, faLDbl); end;
    else
      include(ScanToken.attrs, taLong);
    end;
    Result := t_flt;
  end;

  function  parseNum(base: integer): int64;
  var
    i: integer;
    v: int64;
  begin
    v := 0;
  //handle base, skip base indicator 'x'
    case base of
     8:  begin include(ScanToken.attrs, iaBase8); end;
    10:  begin {ScanToken.ia := [];} v := ord(c) - ord('0'); end;
    16:  begin include(ScanToken.attrs, iaBase16); inc(pc); end;
    end;
    while True do begin
      c := char(ord(pc^) or $20); //lowercase
      i := pos(c, '0123456789abcdef') - 1;  //string index 1-based!
      if (i < 0) or (i >= base) then
        break;
      v := v * base + i;
      inc(pc);
    end;
    Result := v;
  end;

  (* scanNum - scan a number.
    Expect pc at 'x' or first digit.
    Currently we immediately scan for final numbers, not pp-number.
    ToDo: implement pp-number format?
  *)
  procedure scanNum(base: integer);
  begin
    Result := t_int;  //t_num; ?
    ScanToken.lval := parseNum(base);
  //scan suffix
    if pc^ in ['u','U'] then begin
      include(ScanToken.attrs, taUnsigned);
      inc(pc);
      if pc^ in ['L', 'l'] then begin
        include(ScanToken.attrs, taLong);
        inc(pc);
      end;
    end else if pc^ in ['L', 'l'] then begin
      include(ScanToken.attrs, taLong);
      inc(pc);
      if pc^ in ['U', 'u'] then begin
        include(ScanToken.attrs, taUnsigned);
        inc(pc);
      end;
    end else if (base = 10) and (pc^ in FltPunct) then begin
    //maybe real? "0." checked in case '0'!
    //problem: NO reals when called for escape sequence!!!
      scanReal;
    end;
  end;

  function  scanEscape: char;
  begin
  (* todo: \u, \U Unicode escapes?
    Unicode at all?
  *)
    inc(pc);  //skip '\'
    case pc^ of
    '0'..'7':
      begin
        Result := char(parseNum(8));
        exit;
      end;
    'a':  Result := #7;
    'b':  Result := #8;
    't':  Result := #9;
    'n':  Result := #10;
    'v':  Result := #11;
    'f':  Result := #12;
    'r':  Result := #13;
    'u', 'U': //unicode?
      begin
        Result := pc[-1]; //retain '\' or whatever
        exit; //don't move pc
      end;
    'x', 'X':
      begin
        Result := char(parseNum(16));
        exit;
      end;
    else  //default: the char itself
      Result := pc^;
    end;
    inc(pc);  //for all simple chars
  end;

  (* _scanString - scan both string "" and char '' literals.
    Optimized scan for fSkip, no string mapped.
    Handle unpaired quotes, thanks to RP :-)
  *)
  procedure _scanString(kind: eToken);
  var
    delim:  char;
  begin
    if c = 'L' then begin
    //pc just past 'L'
      include(ScanToken.attrs, taLong);
      c := pc^;
      inc(pc);  //skip delimiter
    end;
    delim := c;
    pc1 := pc;
    ScanToken.kind := t_str;
    if fSkip then begin //simple string scan
      while pc^ <> delim do begin
        case pc^ of
        #0: //unpaired delimiter
          begin
            Result := t_err;
            pc := pc1 - 1;  //back to first delimiter
            break;
          end;
        '\':
          inc(pc,2) //skip any escaped (first) char
        else
          inc(pc);
        end;
      end;
      inc(pc);  //skip delimiter (first or second)
    end else begin
    //copy chars, eval escape sequences
      ScanText := '';
      while pc^ <> delim do begin
        case pc^ of
        #0: //unpaired delimiter
          begin
            Result := t_err;
            pc := pc1 - 1;  //back to first delimiter
            break;
          end;
        '\':
          ScanText := ScanText + scanEscape
        else
          ScanText := ScanText + pc^;
          inc(pc);
        end;
      end;
      inc(pc);  //skip delimiter
     	SetTokenString(ScanToken, ScanText);
    end;
    Result := kind;
  end;

  procedure scanHeader(delim: char);
  begin //pc1 at " or <, pc ref first h/q-char
    pc1 := pc;
    while (pc^ <> delim) and (pc^ <> #0) do
      inc(pc);
    SetString(ScanText, pc1, pc-pc1);
    inc(pc);  //skip delim
    Result := t_str;  //t_hdr?
    //if (fMode = smHeader) and (delim = '>') then
    if delim = '>' then
      include(ScanToken.attrs, saSysHdr);
    //ScanToken.strID := mapString(ScanText);
    ScanToken.kind := Result;
    SetTokenString(ScanToken, ScanText);
  end;

  (* scanDelimited
    //Todo: specify purpose!
    Define the handling of #0 and delimiters inside an string accordingly.
    When meta strings can contain #0, then they must guaranteed
      to be properly delimited
      to have embedded delimiters properly doubled (or escaped?)
  *)
  procedure scanDelimited(delim: char; t: eToken);
  begin //pc1 at starting delimiter, pc ref first char
    pc1 := pc;
    while (pc^ <> delim) and (pc^ <> #0) do
    //while (pc^ <> delim) do //accept #0 inside meta-literals!?
      inc(pc);
    SetString(ScanText, pc1, pc-pc1);
    if pc^ = delim then
      inc(pc);  //skip delim
    Result := t;
    ScanToken.kind := t;
		if t in [t_err, t_str, t_car] then
    	SetTokenString(ScanToken, ScanText);
  end;

  procedure _scanSym;
  var
    r: PSymPrep;
  begin
    assert(pc = pc1 + 1);
    while pc^ in SymNext do
      inc(pc);
{
  //todo: check for possibly wrapped symbols
    if (pc^ = '\') and (pc[1] = #0) and not fSkip then begin
    //todo: try continue on next line
    (* requires a copy of the final symbol! -> emit "##"?
    *)
    end;
}
    if fSkip then
      ScanToken.symID := -1  //flag no string
    else begin
    //following Scan... mapping ALSO must be done in token streams!!!
      try //bug seems to have been fixed
        ScanText := '';
        SetString(ScanText, pc1, pc - pc1);
        ScanToken.symID := mapSym(ScanText);
        r := Symbols.getSymbol(ScanToken.symID);
        if not r.loc.valid and (self.src <> nil) then begin
        //first occurence, initial location
          r.loc.id := self.src.id; //src can be nil?
          r.loc.line := self.CurLine;
          ScanSym := r^; //as just updated
        end else begin
        //actual occurence, copy def and update loc of copy
          ScanSym := r^;
          if src <> nil then begin
            ScanSym.loc.id := self.src.id;
            ScanSym.loc.line := self.CurLine;
          end;
        end; // else //StringScanner!
        ScanText := ScanSym.FString;  //unique string
      except
        assert(pc > pc1);
      end;
    end;
    Result := t_sym;
  end;

  procedure tryNextLine(t: eToken);
  begin
  //get line
    if nextLine then begin
    //only pcN defined - indentation???
      pc1 := pcN;
      pc := pc1;
      while pc^ in whitespace do
        inc(pc);
      pcN := pc;
      //ScanToken.len := pc - pc1;
      ScanToken.f := src.id;
      ScanToken.line := CurLine;
      Result := t;
    end else begin
      if t = t_NoEol then
        Log('escaped EOF', lkWarning);
      //else Log('<eof>', lkDiag)
      pc := pcN;
      pc1 := pc;
      //ScanToken.len := 0;
      Result := t_eof;
      ScanToken.line := CurLine - 1;  //-1?
    end;
    ScanToken.pc := pc1; //len is computed later, based on this and pc
  end;

begin
//switch to state
  pc0 := pcN; //for unGet
  case state of
  ssInRem:
    begin //at eol
      pc := pcN;
      pc1 := pc;  //for SetString!
      ScanToken.pc := pc;
      scanRem([]);
    end;
  else  //ssDefault
  //get start position
    pc := pcN;
  //skip whitespace
    while pc^ in whitespace do
      inc(pc);
    pc1 := pc;
    ScanToken.pc := pc;
    if pc = pcN then
      ScanToken.attrs := []
    else
      ScanToken.attrs := [taWhiteBefore];
    if fMode = smEol then begin
    //properly skip over contents, up to EOL
      if pc^ = #0 then
        Result := t_empty
      else if pc^ = '/' then begin
      //handle delimited rems properly - might be begin of an multi-line rem!
        inc(pc);
        case pc^ of
        '*':  scanRem([taRemBeg]);
        '/':  scanRemEol;
        else  ScanRemEol;
        end;
      end else
        ScanRemEol;
    end else begin
    //get token
      c := pc^; inc(pc);
      case c of
      #0: //eol
          tryNextLine(t_bol); //set atBol
      '!':  mkOp(logNOT);
      '"':  //if fHeader then scanHeader('"') else _scanString(t_str);
        case fMode of
        smStd:
          case mode of
          smCSource:  _scanString(t_str);
          smMeta:     scanDelimited('"', t_str);
          else  assert(False, 'unhandled src mode');
          end;
        smHeader:   scanHeader('"');
        //smMeta:     Result := ScanUntil('"'); - maybe useful in expression scanner?
        else  assert(False, 'unhandled src mode');  //scanDelimited('"', t_str);
        end;
      '#':  //preprocessor - directive or operator
        case pc^ of
        '#':  mkOp2(nop2Sharp); //## only valid in macro body
        '@':  mkOp2(opSharpAt); //MSC only!?
        else  Result := opSharp; //always valid
        end;
      '%':  mkOp(opMOD);
{$IF __opMul}
      '&':  mkOp(opAmpersAnd);
{$ELSE}
      '&':  mkOp(binAnd_);
{$IFEND}
      '''': if Mode = smMeta then scanDelimited('''', t_car) else _scanString(t_car);
      '(':  Result := opLPar;
      ')':  Result := opRPar;
{$IF __opMul}
      '*':  mkOp(opStar0);
{$ELSE}
      '*':  mkOp(opStar_);
{$IFEND}
      '+':  mkOp(opADD);
      ',':  Result := opComma;
      '-':
        if pc^ = '>' then
          mkOp2(opTo)
        else
{$IF __opMul}
          mkOp(opSub0);
{$ELSE}
          mkOp(opSub_);
{$IFEND}
      '.':
        if pc^ = '.' then begin
          if pc[1] = '.' then begin
            Result := op3Dot; inc(pc, 2);
          end else
            Result := t_err
        end else
          Result := opDot;
      '/':  // div, rem
        case pc^ of
        '*':  scanRem([taRemBeg]);
        '/':  scanRemEol;
        '=':  mkOp2(letDiv);
        '#':  //DoDi: Microsoft extension: /##/
          if (pc[1] = '#') and (pc[2] = '/') then begin
            Result := opDivDiv; inc(pc, 3);
          end else
            Result := opDiv;
        else  Result := opDiv;
        end;
      '0':  // const
        case pc^ of
        'x', 'X': begin {inc(pc);} scanNum(16); end;
        '.':  scanReal;
        else  scanNum(8);
        end;
      '1' .. '9':  // const
          scanNum(10);
      ':':  Result := opColon;  //C++ "::" ???
      ';':  Result := opSemi;
      '<':  if fMode = smHeader then scanHeader('>') else mkOp(opLT);
      '=':  mkOp(opLet);
      '>':  mkOp(opGT);
      '?':  Result := opTERN; //digraphs "?>" "?<" ???
      'L':
        case pc^ of
        '''': _scanString(t_car);
        '"':  _scanString(t_str);
        else  _scanSym;
        end;
      '[':  Result := opLBra;
      '\':  //escaped eol?
        begin
          if pc^ <> #0 then
            Result := t_err
          else
            tryNextLine(t_NoEol);
        end;
      ']':  Result := opRBra;
      '^':  mkOp(opXor);
      //'_':
      '{':  Result := opBeg;
      '|':  mkOp(binOR);
      '}':  Result := opEnd;
      '~':  Result := binNot;
      else  //default
        if c in SymFirst then
          _scanSym
        else begin
          Log('unexpected: "' + c + '"', lkSynErr);
          Result := t_err;
          //ScanToken.chars := c;
          ScanToken.kind := t_err;
          ScanText := c;
          SetTokenString(ScanToken, ScanText);
        end;
      end;
    end;
  end;  //state
//finish
  pcN := pc;
  ScanToken.kind := Result;
  ScanToken.len := pc - ScanToken.pc;
  ScanningNext := pc; //pcN
//track BOL?
  if atBOL and not (Result in [opSharp, t_empty, t_rem]) then
    atBOL := Result = t_bol; // False;
end;

(* nextToken: handle directives, locks
  High level, not for use by directive handlers!

Problem with open macros on #include!
  solution: all macros are popped prior to pushing an new source file,
    in TTokenStack.pushFile.
*)
function TFileC.nextToken:  eToken;
begin
  repeat
    Result := self._nextRaw(Flock > 0, smStd);
    if Result = opSharp then begin
      if atBOL then
        Result := handleDirective  //even if locked!
      else  //else unexpected #, operator allowed only inside macro body
        Log('unexpected #', lkWarning);
    end;
  until (Flock = 0) or (Result = t_eof);
end;

function TFileC.nextRaw: eToken;
begin
  Result := self._nextRaw(False, smStd);
end;

function TFileC.scanString(const s: string): eToken;
begin
  if s = '' then begin
    Result := t_empty;  //or what?
    ScanToken.kind := Result;
  end else begin
    assert(self.src = nil, 'overwrite src file');
    self.state := ssDefault;
    self.mode := smCSource;
    self.LineBuf := s;
    self.pcN := PChar(s);
    Result := self._nextRaw(False, smStd);  //return t_eof if white!
  end;
end;

function TFileC.unGet: boolean;
begin
  pcN := pc0;
  Result := Self.state = ssDefault; // True;
end;

{ TTokenStack }

function TTokenStack.BaseFile: TFileC;
begin
  Result := TFileC(Stack[1])
end;

(* Expand - try expand symbol
  Return first token of expansion.
*)
function TTokenStack.Expand(sym: PSymPrep): eToken;
var
  strm: TTokenStream;
begin
  if IsExpandablePrep(sym) then begin
    strm := TMacro(Sym.FMacro).Expand(pSrc);
    if strm = nil then
    //expands to a single token
      Result := ScanToken.kind
    else begin
    //push expanded stream
      strm.fCont := True; //for top level macro
      push(strm);
      Result := strm.firstToken;
      //or strm.rewind and Result := t_empty?
    end;
  end else begin
  //mark symbol as not expandable
    Result := t_symNX;
    ScanToken.kind := Result;
  end;
end;

procedure TTokenStack.Init;
begin
  Count := 0;
  self.CurFile := nil;
  //if Stack[0] = nil then begin
  if StringScanner = nil then begin
    //Stack[0] := TTokenStream.Create;  //the default (empty) stream
    StringScanner := TFileC.Create;
  //never pop this
    StringScanner.fFile := True;
    //StringScanner.fCont := False; - is default
  //init somehow
    StringScanner.scanString('');
    Stack[0] := StringScanner;
  end;
  pSrc := Stack[0];
end;

function TTokenStack.pop: boolean;
var
  fFileClosed:  boolean;
begin
//check stack empty
  Result := Count > 0;
  if not Result then
    exit;
  fFileClosed := pSrc.fFile;
  //if (pSrc = CurFile) and assigned(OnLineChange) then
  if fFileClosed and assigned(OnLineChange) then
    OnLineChange(lcCloseFile, CurFile);
//close old stream
  pSrc.rewind;  //re-enable macros...
  if pSrc.fTemp then
    pSrc.Free;
  Stack[Count] := nil;
//retrieve next stream
  dec(Count);
  pSrc := Stack[Count];
//track files
  if (pSrc is TFileC) and (pSrc <> StringScanner) then begin
    CurFile := TFileC(pSrc);
    ScanningFile := CurFile.src.name;
    ScanningLine := CurFile.CurLine;
    ScanningText := CurFile.LineBuf;
    CheckMonitor;
    if fFileClosed and assigned(OnLineChange) then
      OnLineChange(lcResumeFile, CurFile);
  end else //remember last source file?
    CurFile := nil; //if last file is being popped?
end;

procedure TTokenStack.push(src: TTokenStream);
begin
  inc(Self.Count);
  self.Stack[Count] := src;
  pSrc := src;
end;

(* pushFile - first source or #include
  Close all pending macros!!!
*)
function TTokenStack.pushFile(src: TFile): TFileC;
begin
//pop all macros
  while (Count > 0) and not pSrc.fFile do begin
    if not self.pop then begin
    //debug
      Log('macro not popped?', lkBug);
      pop;
    end;
  end;
  if src <> nil then begin
    Result := TFileC.CreateFromFile(src); //sets Scanning...
    CurFile := Result;
    push(Result);
    if assigned(OnLineChange) then begin
    //logging of change of first line suppressed in nextLine!
      OnLineChange(lcOpenFile, CurFile);
    end;
  end else
    Result := nil;
end;

initialization
  DecimalSeparator := '.';  //for StrToFloat conversion!
  TokenStack.Init;
end.
