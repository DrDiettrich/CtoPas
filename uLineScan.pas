unit uLineScan;
(* Line Scanners, with standard methods for rems, strings etc.
  distinct for C and non-C (=Pascal) rems and strings.
V0.1: use pointers internally!
V0.0: mix of indexed and pointers (bad!)
*)

interface

uses
  Classes,
  uTokenC;

type
(* general token classes
- continued lines
  rem1, rem2, prepc
  condFalse?
- special line starts, finally ->
  { rem1, } -> wkRem
  /* rem2, */ -> wkRem
  # prep, no \ -> wkPrepc
*)
(* better(?)
  ScanType: C, P, Cprep, Pprep, Crem, Prem
  Base class: C/P, modify: normal, prep, rem, meta?
  firstWord checks for special BOL conditions and scan types.
  nextWord checks for scan types -> virtual methods!
*)
  eWordKind = (
  //line ends
    wkEol,
    wkNoEol,  //escaped EOL
    //wkRemEol, //single line rem
    wkRem,  //terminated rem
    wkPrep, //C:#... (unless #define?) P:{$ (*$
  //continued
    wkPrep1, wkPrep2,   //C:1?..\, P:1={$.. 2=(*$..
    wkRem1, wkRem2, //C:2=/* P:1={... 2=(*...
  //normal
    wkOther,
    wkNum,
    wkStr1, wkStr2, //delimiter ' or "
    wkOp, wkPunct,  //optional?
    wkIdent,  //unclassified
  //interpreted identifiers
    wkMac,
    wkKey
  );
const
  wkRemEol = wkRem;

type
{$IFDEF new}
  eLineKind = (
    lkSrc, lkPrep, lkMeta,
    lkRem1, lkRem2
  );
{$ELSE}
  eLineKind = wkEol..wkOther;
{$ENDIF}
  TLineStarts = array of eLineKind;

  RScanWord = record
    txt:  string;
    iLeft, iRight: integer;
    lnum: integer;  //optional
    //fileID?
    case wkind:  eWordKind of
    wkOther:  (token: eToken);  //not portable!
    wkIdent:  (symbol: integer);
    //wkOp: wkPunct:  //according enum type
    //wkPrep:
    //wkKey:
  end;

(* [...   ttt?...]
   |   |  |  |
   pL  pW pT pN
*)
  TScanLine = class
  protected
  //current line
    pL, pW, pT, pN: PChar;
    procedure skipHex;
      //skip pN - IS hex indicator
    procedure skipOct;
    procedure skipWhite;  //pW<-pN->pT
    function  scanRem1(var r: RScanWord): eWordKind;
      //pN past {
    function  scanRem2(var r: RScanWord): eWordKind;
      //pN past (* /*
    function  scanRemEol(var r: RScanWord): eWordKind;
    //function  scanString(var r: RScanWord; term: char): eWordKind;
    function  scanString(var r: RScanWord): eWordKind;
      //pT^= ' or " (or 'L'!)
    function  scanEsc(var r: RScanWord): eWordKind;
      //pN past \
    function  scanNum(var r: RScanWord): eWordKind;
      //pT at: # $ . digit
    //function  scanPrep(var r: RScanWord): eWordKind;
  public
    CStyle: boolean;
    CurWord:  RScanWord;
    function  InitLines(Lines: TStrings; var LineStarts: TLineStarts): integer; virtual;
    function  firstWord(var r: RScanWord; mode: eLineKind): eWordKind; virtual;
    function  nextWord(var r: RScanWord): eWordKind; virtual;
    function  TokenStr(const r: RScanWord): string;
  end;

  TLineC = class(TScanLine)
  public
    constructor Create;
    //function  firstWord(var r: RScanWord): eWordKind; override;
    function  nextWord(var r: RScanWord): eWordKind; override;
  end;

var
  DefaultScanner, CScanner: TScanLine;

implementation

uses
  SysUtils,
  uScanC;

{ TScanLine }

//called at begin of firstWord/nextWord
procedure TScanLine.skipWhite;
begin
  pW := pN; //possible whitespace before token
  while pN^ in Whitespace do
    inc(pN);
  pT := pN; //begin of token
  //cannot set iLeft without r!
end;

procedure TScanLine.skipHex;
begin
  inc(pN);  //possibly x/X/$
  while pN^ in HexDigits do inc(pN);
end;

procedure TScanLine.skipOct;
begin
//past '0'
  while pN^ in OctDigits do inc(pN);
end;

function TScanLine.scanEsc(var r: RScanWord): eWordKind;
var
  c: char;
begin
//pN past \
  c := pN^;
  if not CStyle then
    Result := wkOther //error!
  else if c = #0 then
    Result := wkNoEol
  else begin
    inc(pN);
    case c of
    '0':      while pN^ in OctDigits do inc(pN);
    '1'..'9': while pN^ in Digits do inc(pN);
    //'u', 'U': ???
    'x', 'X': while pN^ in HexDigits do inc(pN);
    else      inc(pN);  //skip 1 char -> wkChar?
    end;
    Result := wkNum;  //or char!!!
  end;
//pN past num/char
  r.iRight := pN - pL;
end;

function TScanLine.scanNum(var r: RScanWord): eWordKind;
//var  c: char;

  procedure skipDec;
  begin
    while pN^ in digits do
      inc(pN);
  end;

//decimal, possibly real!
  procedure StdNum;
  begin
    skipDec;
    if pN^ = '.' then begin
      inc(pN);
      skipDec;
    end;
    if pN^ in ['e', 'E'] then begin
      inc(pN);
      if pN^ in ['+', '-'] then
        inc(pN);
      skipDec;
    end;
  end;

begin
(* start with
  P: $*, #$, #*
  digit*
*)
  if not CStyle then begin
    case pT^ of
    '#':  if pN^ = '$' then skipHex else skipDec;
    '$':  skipHex;
    else  StdNum;
    end;
  end else begin
  // 0oct, 0Xhex, ...
    if pT^ = '0' then begin
      case pT[1] of
      'x', 'X': begin pN := pT+2; skipHex; end;
      else  skipOct;
      end;
    end else
      StdNum;
  end;
//here: everything skipped
  r.iRight := pN - pL;
  Result := wkNum;
end;

function TScanLine.scanRem1(var r: RScanWord): eWordKind;
//Pascal style: {...}
//pn past { (no matter)
begin
  Result := wkRem1;
  while pN^ <> #0 do begin
    if pN^ = '}' then begin
      inc(pN);
      Result := wkRem;
      break;
    end;
    inc(pN);
  end;
  r.iRight := pL - pN;
end;

function TScanLine.scanRem2(var r: RScanWord): eWordKind;
//C: until */
//P: until *)
//pN past start
var
  d:  char;
begin
  Result := wkRem2;
  if CStyle then d := '/' else d := ')';
  while pN^ <> #0 do begin
    inc(pN);
    if (pN^ = d) and (pN[-1] = '*') then begin
      inc(pN);
      Result := wkRem;
      break;
    end;
  end;
  r.iRight := pN - pL;
end;

function TScanLine.scanRemEol(var r: RScanWord): eWordKind;
// until EOL
begin
  //r.iRight := Length(r.txt) + 1;
  pN := pL + Length(r.txt); //r.iRight;
  Result := wkRemEol; //wkRem?
end;

//function TScanLine.scanString(var r: RScanWord; term: char): eWordKind;
function TScanLine.scanString(var r: RScanWord): eWordKind;
//C: handle escaped chars
//P: handle duplicated terminator (only ' expected)
//pN past delimiter (maybe L"..."!)
var
  term: char;
begin
(* simplification: determine type from last char:
  ' wkStr1
  " wkStr2
  else wkOther (error!)
*)
  term := pN[-1];
  if CStyle then begin
    if term = 'L' then begin
    //unicode! L"..." or L'...'
      term := pN^; inc(pn);
    end;
    while pN^ <> #0 do begin
      if pN^ = term then
        break;
      if pN^ = '\' then begin
        inc(pN); //skip at least 1 escaped char
      //assert pN^ <> #0!
        if pN^ = #0 then
          break;  //error: unterminated string!
      end;
      inc(pN);
    end;
  end else begin
    while pN^ <> #0 do begin
      if pN^ = term then begin
        if pN[1] <> term then
          break;
        inc(pN, 2)  //skip double delimiter
      end else
        inc(pN);
    end;
  end;
//now pN at delimiter or at #0
  if pn^ = #0 then begin
    //Log(
    Result := wkOther;  //error: unterminated string
  end else begin
    inc(pN);
    if term = '"' then
      Result := wkStr2
    else
      Result := wkStr1;
  end;
  r.iRight := pN - pL;
  r.wkind := Result;
end;

function TScanLine.firstWord(var r: RScanWord; mode: eLineKind): eWordKind;
(* Return both item and line kind.
  LineKind is static in the scanner object?
*)
  procedure CheckPrep;
  begin
    //r.iRight := Length(r.txt) + 1;
    pN := pL + Length(r.txt); //ref #0!
    //if r.txt[r.iRight-1] = '\' then
    if pN[-1] = '\' then
      Result := wkPrep1
    else
      Result := wkPrep;
  end;

begin
//skip empty lines
  pL := PChar(r.txt); //for index calculation, line changes...
  pN := pL;
  r.iLeft := 0;
  //iMax := Length(r.txt);
  if r.txt = '' then begin
    if mode in [wkRem1, wkRem2] then
      Result := mode
    else
      Result := wkEol;
  end else begin
    //LineKind := mode;
    case mode of
    wkPrep1:  if CStyle then CheckPrep else Result := scanRem1(r);
    wkPrep2:  Result := scanRem2(r);
    wkRem1: Result := ScanRem1(r);
    wkRem2: Result := ScanRem2(r);
    else
      skipWhite;
      if CStyle and (pN^ = '#') then begin
        CheckPrep;
      end else begin
        Result := nextWord(r);
        exit;
      end;
    end;
    r.iLeft := 0; //BOL
    r.iRight := pN - pL;
    if not CStyle and (mode in [wkPrep1, wkPrep2]) then begin
      if Result <> wkRem then //still unterminated prep-rem
        Result := mode;
    end;
  end;
  r.iRight := pN - pL;  //maybe zero!
  r.wkind := Result;
end;

const
  StdPunct = ['.', ',', ';', ':', '(', ')', '[', ']', '{', '}'];
  StdOps = ['+', '-', '*', '/', '<', '>', '=', '!', '~', '?', '^', '%', '#', '*'];
  Specials = SymFirst + StdPunct + StdOps
    + ['{', '/', '#', '\', '''', '"', '$'];

function TScanLine.nextWord(var r: RScanWord): eWordKind;
var
  //i: integer;
  c: char;

  procedure ScanIdent;
  begin
    while pN^ in SymNext do
      inc(pN);
    Result := wkIdent;
  end;

begin
(* only recognize:
  identifiers
  rems (language specific)
  strings (dto.)
  //directives? #... - only at BOL!
  punctuators?
  escaped EOL?
  numbers?
  other
*)
  skipWhite;  //pW<-pN->pT->pN
  c := pN^; inc(pN);
  begin
    case c of
    #0: begin dec(pN); Result := wkEol; end;
  //rems
    '/':  // /* //
      case pN^ of
      '/':  Result := scanRemEol(r);
      '*':
        if CStyle then begin
          inc(pN);
          Result := scanRem2(r)
        end else
          Result := wkOther;
      else  Result := wkOp;
      end;
    '(':  // (* (.
      if not CStyle then begin
        case pN^ of
        '*':  begin inc(pN); Result := ScanRem2(r); end;
        '.':  begin inc(pN); Result := wkPunct; end; //(. = [
        else  Result := wkPunct;
        end;
      end else
        Result := wkPunct;
    '{':
      if CStyle then Result := wkPunct else Result := scanRem1(r);
  //strings
    '''', '"':
      Result := scanString(r);
    'L':
      if CStyle then begin
        case pN^ of
        '''', '"':  Result := scanString(r);
        else  ScanIdent;
        end;
      end else
        ScanIdent;
  //continuation line?
    '\':  //\eol, \...
      if not CStyle then
        Result := wkOther //error!
      else if pn^ = #0 then
        Result := wkNoEol
      else
        Result := scanEsc(r);
  //numbers?
    '#':  //not at BOL -> #, ##, #@
      if not CStyle then
        Result := scanNum(r)  //#digs #$xdigs
      else begin
        case pN^ of
        '#', '@': inc(pN);
        end;
        Result := wkOp;
      end;
    '.':  //. .. ...
      begin
      //questionable: general .. and ...?
        if pN^ = '.' then begin
          inc(pN);
          if not CStyle then
            //Result := wkPunct //..
          else if pN^ = '.' then
            inc(pn) //...
          else
            dec(pN);  //.
        end;
        Result := wkPunct;  //. or .)
      end;
    '0'..'9':
      Result := scanNum(r);
    '$':
      if CStyle then Result := wkOther else Result := scanNum(r);
    else
      if c in SymFirst then begin
        ScanIdent;
      end else if c in StdPunct then begin
        while pN^ in StdPunct do
          inc(pN);
        Result := wkPunct;
      end else if c in StdOps then begin
        while pN^ in StdOps do
          inc(pN);
        Result := wkOp;
      end else begin
        while not (pN^ in (Specials + [#0])) do
          inc(pN);
        Result := wkOther;
      end;
    end;  //case
  end;
  r.wkind := Result;
  r.iLeft := pT - pL;
  r.iRight := pN - pL;
end;

function TScanLine.InitLines(Lines: TStrings;
  var LineStarts: TLineStarts): integer;
var
  i, n: integer;
  l: integer absolute Result;
  r:  RScanWord;
  ls: eLineKind;
  t:  eWordKind;
const
  MinLen = 80;  //really?
begin
  l := MinLen;
  SetLength(LineStarts, Lines.Count);
  if Lines.Count = 0 then
    exit; //no source, so far
  ls := wkEol;
  LineStarts[0] := ls;
  for i := 0 to Lines.Count - 2 do begin
    r.txt := Lines[i];
    t := firstWord(r, ls);  //may expand line!
    n := Length(r.txt);
    if n > l then
      l := n;
    while True do begin
      case t of
      wkEol:  begin ls := wkEol; break; end;
      wkNoEol:  break;  //keep state!
      wkPrep1, wkPrep2, wkRem1, wkRem2:
        begin
          ls := t;
          break;
        end;
      end;
      t := nextWord(r);
    end;
    LineStarts[i+1] := ls;
  end;
  //Result := l;
end;

{ TLineC }

constructor TLineC.Create;
begin
  inherited;
  self.CStyle := True;
end;

function TLineC.nextWord(var r: RScanWord): eWordKind;
var
  c: char;
  //p: PChar; //simple handling of multi-character sequences
const
//allow for duplicate op (& -> && etc.)
  DupOps = ['&', '+', '-', '/', '<', '=', '>', '|'];
  //C++: "::"?
//also allow for assignment (& -> &= etc.)
  LetOps = ['&', '+', '-', //special handling for "->"
    '<', '>',   //both single and duplicated
    '|', '!', '%', '/', //special handling for "//" and "/*"
    '*', '=', '^'
  ];
  DupLetOps = ['<', '>'];

  procedure MkOp2;
  begin
    inc(pN);
    Result := wkOp;
  end;

  procedure MkOp;
  begin
    if (pN^ = c) and (c in DupOps) then begin
      inc(pN);
      if (pN^ = '=') and (c in DupLetOps) then
        inc(pN);
    end else if (pN^ = '=') and (c in LetOps) then
      inc(pN);
    Result := wkOp;
  end;

  procedure MkPunct(l: integer);
  begin
    inc(pN, l-1);
    Result := wkPunct;
  end;

begin //TLineC.nextWord
//only handle operators and punctuators?
  skipWhite;
  c := pN^;
  if c = #0 then
    Result := wkEol
  else begin
    inc(pN);
    case c of
    #0: begin dec(pN); Result := wkEol; end;
    '-': //- -= -- ->
      if pN^ = '>' then MkPunct(2) else MkOp;
    '/': // / // /= /*
      case pN^ of
      '/': Result := scanRemEol(r);
      '*': begin inc(pN); Result := scanRem2(r); end;
      else MkOp;
      end;
    '+', '*', '<', '>', '=', '!', '~', '&', '|', '%': //+ ++ +=
      MkOp;
    '.': //. .num ...
      if (pN[0] = '.') and (pN[1] = '.') then
        MkPunct(3)
      else
        Result := wkPunct;
    ':',  //: C++ ::
    ',', ';',
    '{', '}', '[', ']', '(', ')':
      Result := wkPunct;
    '\':
      if pN^ = #0 then
        Result := wkNoEol
      else
        Result := scanEsc(r);
    '''', '"':
      Result := scanString(r);
    '#':  //preprocessor - directive or operator
      case pN^ of
      '#':  mkOp2;  //(op2Sharp);
      '@':  mkOp2;  //(opSharpAt);
      else  Result := wkOp; // opSharp;
      end;
    else
      pN := pT;
      Result := inherited nextWord(r);
      exit;
    end;
  end;
  r.iLeft := pT - pL;
  r.iRight := pN - pL;
  r.wkind := Result;
end;

function TScanLine.TokenStr(const r: RScanWord): string;
var
  l:  integer;
begin
  l := r.iRight-r.iLeft;
  if l > 0 then
    Result := Copy(r.txt, r.iLeft+1, r.iRight-r.iLeft)
  else
    Result := '';
end;

initialization
  DefaultScanner := TScanLine.Create;
  CScanner := TLineC.Create;
finalization
  FreeAndNil(DefaultScanner);
  FreeAndNil(CScanner);
end.
