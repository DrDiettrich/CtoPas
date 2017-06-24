unit uMacros;
(* Handle #defines, expansion...
ToDo:
+ rescan result of ##
- use TMacroTokens.FCount (instead of iNext)
*)

interface

{$I config.pas}

uses
  uXStrings,
  uTokenC, uTablesPrep, uDirectives, uScanC;

type
  TMacro = class;

  TMacroTokens = class(TTokenArray)
  protected //only use here
    procedure concat;
      //exec ## operator - for use by TMacroFunc.Expand only!
    //procedure endRecord;
    function  endRecord: integer;
    procedure startRecord;
    procedure addToken; //virtual; ? //add ScanToken (deprecated)
    procedure putToken(pToken: PPreToken);  //really required!
    property  Tokens: aTokens read FTokens;
  public  //inherited
    fromMacro:  TMacro; //allow for reset of fromMacro.fExpanding
    function  firstToken: eToken; override;
    function  nextRaw: eToken; override;  //no interpretation
    procedure rewind; override; //reset for next invocation
  end;

  TMacro = class(TSymMacro)
  protected
{$IFDEF DEBUG_EXPAND}
    fHasArgs: boolean;  //True even for "mac()"
    fEmpty: boolean;  //no tokens in body
    fAlias: boolean;  //1 token in body, NOT arg!
    //Xkind: eXkind;  //treatment in cross compiler - to come?
{$ENDIF}
  //track macro usage
    _fExpanding:  boolean; //no more recursive expansion
    procedure SetExpanding(state: boolean);
    property fExpanding: boolean read _fExpanding write SetExpanding;
  protected
    mackind:  eSymbolKind;
    Body: TMacroTokens;
    function  argName(id: integer): string; virtual;
    procedure addToken; virtual;
  public
    //destructor  Destroy; override;
    function  GetTokens: TTokenArray; override;
    function  toString: string; override;
    function  argList: string; virtual;
    class function  Define(pFile: TFileC): TMacro;
    procedure _Define(pFile: TFileC);  virtual;
    function  Expand(src: TTokenStream): TMacroTokens; virtual;
    function  peekBody(offset: integer): PPreToken;
    function  BodyString: string; override;
    property  Expanding: boolean read _fExpanding write SetExpanding;
      //for use in macro analyzer
  end;

  TMacroArg = record  //class(TTokenArray)
    symID:  integer;
    rawRefs, expRefs: integer;  //optimize expansion
  end;

  TMacroFunc = class(TMacro)
  protected
    Args: array of TMacroArg; //descriptions
    numArgs:  integer;
    fVariadic:  boolean;
    fConcat:  boolean;
    procedure addArg(symID: integer);
    function  argName(id: integer): string; override;
    procedure addToken; override;
  public
    function  argList: string; override;
    procedure _Define(pFile: TFileC);  override;
    function  Expand(src: TTokenStream): TMacroTokens; override;
  end;

  eBuiltin = (
    bmFile, bmLine,
    bmPragma  //_Pragma ( "str" )
  );

  TMacroBuiltin = class(TMacro)
  public
    which: eBuiltin;
    class procedure Init;
    function  Expand(src: TTokenStream): TMacroTokens; override;
  end;

  eKnownLibs = (
    advapi32,
    kernel32,
    mpr,
    version,
    comctl32,
    gdi32,
    opengl32,
    user32,
    wintrust,
    msimg32
  );

  TMacroLibs = class(TMacro)
  public
    //symID: integer; //of const ... = '...dll';
    libID: eKnownLibs;
    class procedure Init;
    function  Expand(src: TTokenStream): TMacroTokens; override;
  end;

type
  RKnownLibs = record
    mac: string; //WIN...API
    lib: string; //lib name
    sym: string; //lib name constant
  end;

const //public for interface section in ToPas
//todo: check API symbol names
  KnownLibs: array[eKnownLibs] of RKnownLibs = (
    //_RTLENTRY ? -> allow to use C functions?
    (mac:'WINADVAPI'; lib:'advapi32.dll'; sym: 'advapi32'),
    (mac:'WINBASEAPI'; lib:'kernel32.dll'; sym:'kernel32'),
    (mac:'WINMPRAPI'; lib:'mpr.dll'; sym:'mpr'),
    (mac:'WINVERAPI'; lib:'version.dll'; sym:'version'),
    (mac:'WINCOMAPI'; lib:'comctl32.dll'; sym:'comctl32'),
    (mac:'WINGDIAPI'; lib:'gdi32.dll'; sym:'gdi32'),
    (mac:'WINGLAPI'; lib:'opengl32.dll'; sym:'opengl32'),
    (mac:'WINUSERAPI'; lib:'user32.dll'; sym:'user32'),
    (mac:'WINTRUSTAPI'; lib:'wintrust.dll'; sym:'wintrust'),
    (mac:'WINIMGAPI'; lib:'msimg32.dll'; sym:'msimg32')
  );

implementation

uses
  SysUtils,
  uUI, uTablesC;


{ TMacroTokens }

procedure TMacroTokens.putToken(pToken: PPreToken);
begin
  if iNext >= Length(FTokens) then begin
    if iNext = 0 then
      SetLength(FTokens, 32)
    else
      SetLength(FTokens, Length(FTokens) * 2);
    if iNext > 256 then //some definitions can be quite long!
      Log('infinite recursion?', lkTodo);
  end;
  FTokens[iNext] := pToken^;
  inc(iNext);
end;

procedure TMacroTokens.addToken;
begin
  putToken(@ScanToken);
end;

(* concat - eval ## operators
*)
procedure TMacroTokens.concat;
var
  i:  integer;
  s1, s2: string;

  function  getText(index: integer): string;
  begin
    case  FTokens[index].kind of
    t_empty,
    t_eof:    Result := ''; //TokenText(Tokens[index]);
    else      Result := TokenText(FTokens[index]);
    end;
  end;

begin
//## invalid as first or last (before EOF) token
(* ToDo: Needs update according to old ToPas!
  add and mark invalid occurence as nop2Sharp.
*)
  if FTokens[0].kind = op2Sharp then begin
    Log('Tokens[0]=##', lkSynErr);
  end;
  for i := 1 to high(FTokens) - 2 do begin  //exclude first and last
    if FTokens[i].kind = op2Sharp then begin
      s1 := getText(i-1);
      s2 := getText(i+1);
      //s := TokenText(Tokens[i-1]) + TokenText(Tokens[i+1]);
    //replace tokens: <lhs>##<rhs> => <empty><empty><result>
      if s1 = '' then begin
      //result is (remains) second token
        //nop
      end else if s2 = '' then begin
      //result is first token
        FTokens[i+1] := FTokens[i-1];
      end else begin
      //create new symbol
        ScanText := s1 + s2;
{$IFDEF old}
        ScanToken.kind := t_sym;
        ScanToken.symID := mapSym(ScanText);
        //ScanText := Symbols.Strings[ScanToken.symID]; //unique string
{$ELSE}
        StringScanner.scanString(ScanText);
        if ScanningNext^ <> #0 then
          Log('bad ## result', lkSynErr);
{$ENDIF}
        ScanToken.pc := nil;  //means: synthetic token!
        FTokens[i+1] := ScanToken;
      end;
      FTokens[i-1].kind := t_empty;  //first arg
      FTokens[i].kind := t_empty;    //##
    end;
  end;
  i := high(FTokens) - 1;
  if FTokens[i].kind = op2Sharp then begin
  //special case, in GNU bits/mathcalls.h
    FTokens[i].kind := t_eof; // t_empty; //retain first arg
  end;
//rewind, but don't destroy!
  iNext := 0;
end;

(* endRecord
  The macro version might classify the macro as:
    empty [0], alias [1]
    ??? const [1] -> .ival
*)
//procedure TMacroTokens.endRecord;
function TMacroTokens.endRecord: integer;
begin
//append eof marker
//reduce buffer size
{$IFDEF DEBUG_EXPAND}
  Count := iNext + 1;
{$ENDIF}
  FCount := iNext;  //exclude EOF token
  SetLength(FTokens, iNext + 1); //include eof marker!
  FTokens[iNext].kind := t_eof;
  iNext := 0;
//check for non-whites
  Result := iNext;
//? preserve comments for crosscompiler?
  while (Result >= 0) and (FTokens[Result].kind <= t_rem) do
    dec(Result);
  inc(Result);  //true count
end;

function TMacroTokens.firstToken: eToken;
begin
{$IFDEF DEBUG_EXPAND}
  //if Count < 0 then begin
  if not locked then begin
    Log('still recording', lkErr);
  //repair
    endRecord;
  end;
  if iNext <> 0 then begin
    Log(self.fromMacro.name + ' already replaying', lkErr);
    iNext := 0;
  end;
{$ENDIF}
  //self.locked := True;
  iNext := 0;
  Result := nextRaw;
end;

(* nextToken - from token stream
*)
function TMacroTokens.nextRaw: eToken;
//function TTokenArray.nextToken(sf: eSpecialFilter = sfStd):  eToken;
//function TTokenArray.nextToken:  eToken;
begin
{$IFDEF DEBUG_EXPAND}
  //if self.iNext > high(FTokens) then
  if self.iNext > Count then
    assert(false, 'bad token stream');
{$ENDIF}
  ScanToken := FTokens[iNext];
  Result := ScanToken.kind;
  if Result <> t_eof then
    inc(iNext);
//further init ScanToken?
  case Result of
  //t_err, t_car, //todo: these also have a string argument!
  t_str:  ScanText := TokenText(ScanToken);
  t_sym, t_symNX: //then begin  //and not fSkip then...
    begin
      ScanSym := Symbols.getSymbol(ScanToken.symID)^;
      ScanText := ScanSym.FString;
    end;
  end;
end;

procedure TMacroTokens.rewind;
begin
  iNext := 0;
  if self.fromMacro <> nil then
    fromMacro.fExpanding := False;
end;

procedure TMacroTokens.startRecord;
begin
  SetLength(self.FTokens, 100);
  self.iNext := 0;
{$IFDEF DEBUG_EXPAND}
  Count := -1;  //debug missing endRecord
{$ENDIF}
end;

{ TMacro }

(* Define - record #define
Problem:
GNU redefines "const", in a way that it becomes a symbol?

Allow to create special (built-in) macro as TMacroBuiltIn?
*)
//class function TMacro.Define(pFile: TFileC; fFunc: boolean): TMacro;
class function TMacro.Define(pFile: TFileC): TMacro;
var
  sym:  RSymPrep;  //symbol to define
  parent: TSymList;
  //id: integer;
begin
  sym := ScanSym;
  parent := Symbols;
//macro kind? - passed as Self
//reuse existing (predefined) macro?
  TObject(Result) := sym.GetMacro;
  if (Result = nil) or not (Result is TMacro) then
    Result := self.CreateIn(parent, ScanText)  //, False);
  else if Result is TMacroLibs then
    ; //beep; //debug: expect predefined macro
//record definition
  Result._Define(pFile);  //at symbol
//finish macro treatment
  //Result.DefState := msDefined;
  case sym.mackind of
  skSym, skCKey:
    case sym.appkind of
    Kinline,  //debug
    Kconst:
      begin
        Log('attempt to redef keyword', lkTodo);
        exit;
      end;
    end;  //sym: case appkind
  end;  //case mackind
  Symbols.defMacro(Result.ListIndex, skMacro);
end;

procedure TMacro.SetExpanding(state: boolean);
begin
  _fExpanding := state;
  if state then
  //expanding - mark used
    Symbols.ChangeState(self.symID, msUsed);
end;

procedure TMacro.addToken;
begin
  case ScanToken.kind of
  t_empty, t_eof, t_bol:  exit;  //don't record!
  //t_rem?
  t_sym:  //translate macro and arguments
    begin
      if ScanToken.symID = self.symID then begin
      //don't expand recursively
        ScanToken.kind := t_symNX;
      end;
    end;
  end;
//now add it
  Body.addToken;
end;

function TMacro.argList: string;
begin
  Result := '';
end;

function TMacro.argName(id: integer): string;
begin
  Result := '';
end;

procedure TMacro._Define(pFile: TFileC);
begin
//record body
  if Body = nil then
    Body := TMacroTokens.Create;
  //else created in TMacroFunc!
  Body.fromMacro := self;
  Body.startRecord;
  while not (pFile.nextRaw in [t_bol, t_eof]) do begin
    self.addToken;
  end;
  if Body.endRecord = 0 then begin
  //contains only white tokens
    Body.Free;
    Body := nil;
    //self.fEmpty := True;
  end;
//else classify macro?
  //else if Length(Body.Tokens) = 2 then fAlias ...
end;

function TMacro.Expand(src: TTokenStream): TMacroTokens;
begin
  if self.fExpanding then begin
    Log('recursive expansion attempt', lkBug);
    Result := nil;
    ScanToken.kind := t_symNX;  //use symbol unexpanded
  end else begin
  //debug
    //if fVerbose then Log('Expanding ' + name, lkDiag);
    Result := Body; //MacroFunc: get args, prescan and substitute...
  //handle empty definition, i.e. Body=nil or has only 1 EOF token.
    if (Result = nil) or (Length(Result.Tokens) <= 1) then begin
      Result := nil;
      ScanToken.kind := t_empty; //empty substitution
    end else begin
      if Result.Tokens[0].pc = '/##/' then begin  // {RP}
        Result := nil;
        ScanToken.kind := opDivDiv;
      end else
        Self.fExpanding := True; //reset when expansion finished
    end;
  end;
end;

(* toString - dump macro definition
Format:
  1 line header
  n body lines
*)
function TMacro.toString: string;
begin
  Result := BodyString;
  if Result = '' then
    Result := {'#define'#9 +} name + argList
  else
    Result := {'#define'#9 +} name + argList + '\' + EOLstr + Result;
end;

type
  RBuiltin = record
    name: string;
    kind: eBuiltin;
  end;

const
  Builtins: array[eBuiltin] of RBuiltin = (
    (name:'__FILE__'; kind:bmFile),
    (name:'__LINE__'; kind:bmLine),
    (name:'_Pragma'; kind:bmPragma)
  );

function TMacro.peekBody(offset: integer): PPreToken;
begin
//problem: empty Body???
  Result := nil;
  if (Body = nil) or (Body.Tokens = nil) then
    exit;
  if offset >= Length(Body.Tokens) then
    offset := high(Body.Tokens);  //refer to EOF token
  Result := @Body.Tokens[offset];
end;

(* BodyString - return macro body as string.
  The general format is:
    tab tokens {\ EOL tab tokens}
*)
function TMacro.BodyString: string;
var
  i:  integer;
  s:  string;
  fBOL: boolean;
begin
  if Body = nil then begin
    Result := '';
    exit;
  end;
  Result := #9;  //for append to name
  //t := inherited firstToken; error: calls nextToken!
  fBOL := True;
  for i := 0 to high(Body.Tokens) - 1 do begin
    ScanToken := Body.Tokens[i];
    case ScanToken.kind of
    t_NoEol:
      begin
        Result := Result + ' \' + EOLstr + #9;
        fBOL := True;
      end;
    t_rem:  //if desired
      begin
        SetString(s, ScanToken.pc, ScanToken.len);
        if fBOL then
          fBOL := False
        else if taWhiteBefore in ScanToken.attrs then
          s := ' ' + s;
        Result := Result + s;
      end;
    t_str,
    t_car:  //convert embedded control chars!
      begin
        Result := Result + TokenString(fBOL);
        fBOL := False;
      end;
    t_arg, t_argNX:  //only for TMacroFunc
      begin
        s := argName(ScanToken.argID);
        if fBOL then
          fBOL := False
        else if taWhiteBefore in ScanToken.attrs then
          s := ' ' + s;
        Result := Result + s;
      end;
    else  //default
      Result := Result + TokenString(fBOL);
      fBOL := False;
    end;
  end;
end;

function TMacro.GetTokens: TTokenArray;
begin
  Result := Body;
end;

{ TMacroFunc }

(* add formal parameter
*)
procedure TMacroFunc.addArg(symID: integer);
begin
  if numArgs >= high(Args) then
    SetLength(Args, numArgs + 8);
  self.Args[numArgs].symID := symID;
  inc(numArgs);
end;

procedure TMacroFunc.addToken;
var
  i: integer;
  id: integer;
  t: PPreToken;
begin
  case ScanToken.kind of
  t_empty, t_eof, t_bol:  exit;  //don't record!
  //keep t_rem and t_NoEol for pretty-printing!
  t_sym:  //translate self-refs and arguments
    begin
      id := ScanToken.symID;
      if id = self.symID then begin
      //don't expand recursively
        ScanToken.kind := t_symNX;
      end else begin
        for i := 0 to numArgs - 1 do begin
          if Args[i].symID = id then begin
          //replace by actual argument
            ScanToken.argID := i;
          //check for RHS of ##!
            if (Body.iNext > 0)
            and (Body.Tokens[Body.iNext - 1].kind = op2Sharp) then begin
              ScanToken.kind := t_argNX;
              inc(Args[i].rawRefs);
            end else begin
              ScanToken.kind := t_arg;
              inc(Args[i].expRefs); //assume expanded ref
            end;
            break;
          end;
        end;
      end;
    end;
  op2Sharp: //mark LHS as no-expand, inc. kind to ...NX. RHS see above!
    begin
      self.fConcat := True; //requires postscan to eval ## operators
      t := @Body.Tokens[Body.iNext - 1];  //iNext past ##???
      case t.kind of
      t_sym:   t.kind := t_symNX;
      t_arg:
        begin
          t.kind := t_argNX;
        //adjust ref. count
          dec(Args[t.argID].expRefs);
          inc(Args[t.argID].rawRefs); //rawRefs ever used???
        end;
      end;
    end;
  end;
//now really store it
  Body.putToken(@ScanToken);
end;

procedure TMacroFunc._Define(pFile: TFileC);
var
  t: eToken;
begin
//scan state: after macro name
//record args
  SetLength(self.args, 32);
  t := nextNoWhite; //skip symbol
  if t <> opLPar then
    Log('expected "("', lkBug); //wrong object type created
  t := nextNoWhite; //skip "("
  while t = t_sym do begin
    addArg(ScanToken.symID);
    t := nextNoWhite;
    if t = op3Dot then begin
    //named variadic arg
      self.fVariadic := True;
      t := nextNoWhite;
      break;  //expect ")" after "..."
    end else if t = opComma then
      t := nextNoWhite;
  end;
//no more symbols, variadic?
  if t = op3Dot then begin
  //unnamed variadic arg
    ScanToken.symID := mapSym('__VA_ARGS__');
    addArg(ScanToken.symID);
    self.fVariadic := True;
    t := nextNoWhite;
  end;
//now a ")" should come
  if t <> opRPar then
    Log('parameter list missing ")"', lkSynErr);
//shrink arguments
  SetLength(self.args, numArgs);
//record body
  inherited;
end;

(* expand - handle possible macro expansion

1. When the macro expects arguments...
1.1 When no "(" is found, no expansion/substitution takes place.
1.2 The argument list is parsed.
1.3 Each argument is prescanned, for macros, which are expanded.
    The result is put into an new token stream,
    which also is part of the argument object.
2. The macro body is expanded.
2.1 The macro is flagged as no-expand, to prevent recursive expansion.
2.2 The macro tokens are copied to the output stream,
    arguments are inlined, either in the expanded form or,
    after # or around ##, in the non-expanded form.
    When the token of a just expanding macro is encountered, it is not expanded,
    and it is marked for no-expand in the future (t_sym -> t_symNX).
2.3 The # operator expects an macro argument, whose tokens are put into a single
    new string literal. Whitespace between the tokens is reduced to 1 space.
  Note: the constructed string doesn't fully conform to the C specs,
    instead the string is usable in other programming languages.
2.4 The ## operator is processed, by removing every ## operator and
    concatenating the immediately preceding and following tokens into a new token.
    The result shall be a new valid token - presumably of the kind of the left
    operand? (symbol, number - what else?) ToDo!
3. The resulting token stream replaces the macro invocation tokens.
    Each token is subject to macro expansion when the stream is processed.
3.1 When the end of the macro replacement stream has been reached, the
    macro expansion is re-enabled.

Stream limits
  During argument prescan only the tokens of the actual argument are available
  for expansion, no other tokens can be read from the source file.
  Nested macro expansions are inserted into the argument replacement stream.
  For the top level macro the input stream is the current source file,
    the output stream is the expanded token stream in the macro object.
  For a macro in an actual argument, the input stream is the unexpanded actual
    argument stream, the output stream is the expanded token stream in the macro.
    After expansion, the substitution stream is appended to the expanded
    argument stream.
  For a macro expansion in the body of a macro, the input stream is the expanded
    macro body stream (arguments substituted!),
    an additional output stream must be supplied!

Parentheses
  Since every argument list is delimited by a pair of "(" and ")" parentheses,
  inside an actual macro argument only pairs of these tokens can occur.


Lookahead
  The test for the presence of an actual argument list requires lookahead.

  Also the ## operator deserves lookahead. This kind of lookahead is implemented
  in the macro body token stream.

# operator
  The # operator expects an macro argument to follow. All unexpanded(!) tokens
  of the actual argument are put into an common string, with a blank in between
  when the tokens were separated by one or more spaces. The resulting string
  token replaces both the # operator and argument tokens.

## operator
  The ## operator is handled after argument substitution in macro expansion.
  The created (expanded) token stream is inspected for ## operators, and every
  such operator and its immediately circumvening tokens are substituted by a
  new token.
  To prevent reallocation, the result replaces the last token (right hand side
  operand), the other two tokens are marked as empty. This process can be
  repeated, when the next token again is an ## operator.
*)
//function TMacroFunc.expand: eToken;
function  TMacroFunc.Expand(src: TTokenStream): TMacroTokens;
var
  pArg,
  pBody: PPreToken;
  RawArgs, ExpArgs: array of TMacroTokens;

{$IFNDEF DEBUG_EXPAND}
  function  ArgPtr(id: integer; fExpanded: boolean): PPreToken;
  begin
    if fExpanded then
      Result := @(ExpArgs[id].Tokens[0])
    else
      Result := @(RawArgs[id].Tokens[0]);
  end;
{$ELSE} //debug version
  function  ArgPtr(id: integer; fExpanded: boolean): PPreToken;
  var
    p: TMacroTokens;
  begin
  //handle empty args?
    if fExpanded then begin
      //Result := @(ExpArgs[id].Tokens[0])
      p := ExpArgs[id];
      assert(Args[id].expRefs > 0, 'uncounted expanded ref');
    end else
      //Result := @(RawArgs[id].Tokens[0])
      p := RawArgs[id];
    assert(p <> nil, 'arg not scanned');
    assert(p.Tokens <> nil, 'arg without tokens');
    Result := @p.Tokens[0];
  end;
{$ENDIF}

  procedure scanActualArgs;
  var
    i: integer;
    pArg: TMacroTokens;
    iArg: integer;
    level: integer;
  begin
    SetLength(RawArgs, numArgs);
    SetLength(ExpArgs, numArgs);
    if numArgs = 0 then
      exit;
    //src.expectRaw(opLPar, 'expected argument list'); - already skipped!
    for i := 0 to numArgs - 1 do
      RawArgs[i] := TMacroTokens.Create;
    level := 1;
    iArg := 0;
    pArg := RawArgs[iArg]; inc(iArg);
    pArg.startRecord;
  //collect raw args
    while level > 0 do begin
      case src.nextRaw of
      t_eof:  //error!
        assert(False, 'EOF in macro argument list');
      t_empty, t_bol, t_NoEol, t_rem:
        ; //skip
      opComma:
        if (level > 1) or (iArg >= numArgs) then
        //shielded comma, or variadic arg (check!?)
          pArg.addToken
        else begin
          pArg.endRecord;
          pArg := RawArgs[iArg]; inc(iArg);
          pArg.startRecord;
        end;
      opLPar:
        begin
          inc(level);
          pArg.addToken;
        end;
      opRPar:
        begin
          dec(level);
          if level > 0 then
            pArg.addToken
          else begin
            pArg.endRecord;
            pArg := nil;
          end;
        end;
      else
        pArg.addToken;
      end;
    end;
  //current token should be ")" (or erroneous EOF)
  //safety check, finish even last arg
    if pArg <> nil then
      pArg.endRecord;
{$IFDEF DEBUG_EXPAND}
  //more debugging
    for i := 0 to numArgs - 1 do begin
      if RawArgs[i].Count <= 0 then
        Log('arg still recording', lkErr);
    end;
{$ENDIF}
  end;

  function  prescan: boolean;
  var
    iArg: integer;
    pArg, pExp, subst: TMacroTokens;
    //pTok: PPreToken;
    //mac:  TMacro absolute ScanSym.FMember;
    t:  eToken;
  begin
    Result := False;
    for iArg := 0 to numArgs - 1 do begin
{$IFDEF DEBUG_EXPAND}
      if True then begin  //debugging!
        if (Args[iArg].expRefs > 0) or (Args[iArg].rawRefs > 0) then
          Result := True;
{$ELSE}
      if Args[iArg].expRefs > 0 then begin
      //only expand used arguments
        Result := True;
{$ENDIF}
        pExp := TMacroTokens.Create;
        pExp.startRecord;
        ExpArgs[iArg] := pExp;
        pArg := RawArgs[iArg];
        t := pArg.firstToken;
      //try: handle empty args around ## operator - occurs in GNU headers!
        if t = t_eof then begin
          ScanToken.kind := t_empty;
          pExp.addToken;
        end else begin
          while t <> t_eof do begin
            case t of
            //t_eof
            t_empty, t_bol, t_NoEol, t_rem:
              ; //skip
            t_sym:  //expand macro?
              if IsExpandableC(@ScanSym) then begin
                subst := TMacro(ScanSym.FMacro).Expand(pArg);
                if subst = nil then begin
                //check token
                  if ScanToken.kind <> t_empty then
                    pExp.addToken;
                  //else no tokens at all
                end else begin
                //inline stream
                  t := subst.firstToken;
                  while t <> t_eof do begin
                    pExp.addToken;
                    t := subst.nextToken;
                  end;
                //discard emptied stream - but NOT macro bodies!
                  subst.rewind; //unlock macro!
                  if subst.fTemp then begin
                    subst.Free;
                  end;
                end;
              end else  //no macro
                pExp.addToken;
            else  //default
              pExp.addToken;
            end;
            t := pArg.nextRaw;
          end;
        end;
        pExp.endRecord;
        pArg.rewind;
      end else if Args[iArg].rawRefs > 0 then
        Result := True;
    end;
{$IFDEF DEBUG_EXPAND}
  //debugging
    for iArg := 0 to numArgs - 1 do begin
      if Args[iArg].expRefs > 0 then begin
        if ExpArgs[iArg].Count <= 0 then
          Log('ExpArg still recording' lkErr);
      end;
    end;
{$ENDIF}
  end;

  procedure stringify(pArg: TMacroTokens);
  var
    t: eToken;
    s: string;
  begin //# operator
    s := '';
    t := pArg.firstToken;
    while t <> t_eof do begin
      s := s + TokenString;
      t := pArg.nextRaw;
    end;
    pArg.rewind;
  //create string token
    ScanToken.kind := t_str;
    //ScanToken.strID := mapString(s);
    SetTokenString(ScanToken, s);
    Result.addToken;
  end;

begin
//step 1: check for argument list present - else no expansion!
  Result := TMacroTokens.Create;
  Result.fromMacro := self;
  Result.startRecord;
  ScanToken.kind := t_symNX;  //don't expand self again
//start lookahead
  repeat
    Result.addToken;  //starting with ourself as no-expanding token
  until not (src.nextRaw in WhiteTokens);
//check again
  if ScanToken.kind <> opLPar then begin
  //no expansion, undo lookahead
    Result.addToken;  //the non-"("!
    Result.endRecord;
    Result.fTemp := True; //discard after use
    exit;
  end;
//now we just got an "("

//step 2: parse argument list
  //src.nextNoEof(True);  //skip "("
  //src.nextRaw;  //skip "("
  if numArgs > 0 then
    scanActualArgs; //stop on ")"

//step 3: check expansion required (args used?)
  ScanToken.kind := t_empty;  //mark invocating token consumed
  if self.Body = nil then begin
    Result.Free;
    Result := nil;
    exit;
  end else if not prescan and not fConcat then begin
  //no expansion, no ##, but body is not empty
    Result.Free;
    Result := Body;
    ScanToken.kind := t_empty;  //mark invocating token consumed
    exit;
  end;  // else ...
//discard lookahead
  Result.startRecord;

//step 4: expand body
{$IFDEF DEBUG_EXPAND}
  if fVerbose then WriteLn('Expand "', name, '"[', numArgs, ']');
{$ENDIF}
//debug
//start substitution
  pBody := @Body.Tokens[0];
  while pBody.kind <> t_eof do begin
    case pBody.kind of
    t_empty, t_NoEol, t_rem:  //skip?
      ;
    t_eof:  break;  //in outer loop itself?
    t_arg, t_argNX:  //insert arg
      begin
        pArg := ArgPtr(pBody.argID, pBody.kind = t_arg);
        assert(pArg <> nil, 'arg not expanded?');
        while pArg.kind <> t_eof do begin
          Result.putToken(pArg);
          inc(pArg);
        end;
      end;
    opSharp:  //stringify arg
      begin
        inc(pBody);
        assert(pBody.kind = t_argNX, 'expected argNX');
        stringify(RawArgs[pBody.argID]);
      end;
    else
      Result.putToken(pBody);
    end;
    inc(pBody);
  end;
  Result.endRecord; //copy finished

//step 5: concatenate ##
  if self.fConcat then
    Result.concat;

//ScanToken may be anything now, so...
  ScanToken.kind := t_empty;  //mark invocating token consumed
end;


function TMacroFunc.argList: string;
var
  i: integer;
begin
  if numArgs = 0 then begin
    Result := '()';
  end else begin
    Result := '(';
    for i := 0 to numArgs - 1 do begin
      Result := Result + argName(i)
{$IFDEF DEBUG_EXPAND}
        //debugging
        + '{' + IntToStr(Args[i].RawRefs) + '/' + IntToStr(Args[i].ExpRefs) + '}'
{$ENDIF}
      + ',';
    end;
    Result[Length(Result)] := ')';  //overwrite last ','
  end;
end;

function TMacroFunc.argName(id: integer): string;
begin
  Result := Symbols.Strings[Args[id].symID]
end;


{ TMacroBuiltin }

function TMacroBuiltin.Expand(src: TTokenStream): TMacroTokens;

  procedure _Pragma;
  var
    s: string;
  begin
  (* Problem: handlePragma should be called from here,
    using a temporary StringScanner.
    Line end(s) must be handled properly, i.e. #pragma will skip to t_bol
    after pragma execution - not so for _Pragma(str)!

    Current implementation skips until ")", then flags current token handled.
  *)
    Result := nil; //should mean: error!
    //ScanToken.kind := t_err; - will be overridden
    if (src.nextRaw <> opLPar) then
      exit;
    if (src.nextRaw <> t_str) then
      exit;
    s := ScanText; //unquoted?
    if src.nextRaw <> opRPar then
      exit;
    Log('_Pragma not executed', lkTodo);
    ScanToken.kind := t_empty; //flag handled
    exit;
  //todo: make handlePragma accessible from here!
  {
    if Pos('\', s) > 0 then begin
      s := StringReplace(s, '\\', '\', [rfReplaceAll]);
      s := StringReplace(s, '\"', '"', [rfReplaceAll]);
    end;
  //now invoke the #pragma directive!
    StringScanner.scanString(s); //first token becomes current token = pragma name
    handlePragma(StringScanner);
    ScanToken.kind := t_empty; //flag handled
  }
  end;

begin
  case which of
  bmFile:
    begin
      ScanToken.kind := t_str;
      ScanText := TokenStack.CurFile.src.dir + TokenStack.CurFile.src.name;
      //ScanToken.strID := mapString(ScanText);
      SetTokenString(ScanToken, ScanText);
    end;
  bmLine:
    begin
      ScanToken.kind := t_int;
      ScanToken.ival := TokenStack.CurFile.CurLine;
    end;
  bmPragma: _Pragma();
  else  //default
    assert(False, 'unhandled builtin macro');
    ScanToken.kind := t_empty;
  end;
//done - return nothing, meaning re-interpret ScanToken
  ScanToken.pc := nil;  //meaning synthetic text
  Result := nil; //except _Pragma?
end;

class procedure TMacroBuiltin.Init;
var
  i: eBuiltin;
  mac: TMacroBuiltin;
begin
  for i := low(i) to high(i) do begin
    mac := TMacroBuiltin.CreateIn(Symbols, Builtins[i].name); //, False);
    mac.which := Builtins[i].kind;
  end;
end;


{ TMacroLibs }

function TMacroLibs.Expand(src: TTokenStream): TMacroTokens;
begin
//remember name for declaration of the current procedure (being parsed)
//todo: change lib into sym, when const symbols have been created
  FromLib := KnownLibs[self.libID].sym;
  Result := inherited Expand(src); //expand defined macro
end;

class procedure TMacroLibs.Init;
var
  i: eKnownLibs;
  mac: TMacroLibs;
begin
(* Create the preprocessor symbol
  and const symbol? (no macro filter active!)
*)
  for i := low(i) to high(i) do begin
  //no symbols defined right now!
  //todo: create const sym in ToPas
    mac := TMacroLibs.CreateIn(Symbols, KnownLibs[i].mac);
    mac.libID := i;
    Symbols.defMacro(mac.ListIndex, skMacro);
  //flag as undefined, allow for redefinition
    Symbols.ChangeState(mac.ListIndex, msUndef);
  end;
end;

initialization
  assert(Symbols <> nil, 'init Symbols first!');
  TMacroBuiltin.Init;
  TMacroLibs.Init;
end.
