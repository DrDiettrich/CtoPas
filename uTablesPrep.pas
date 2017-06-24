unit uTablesPrep;
(* Preprocessor symbol table and objects

The STB contains all symbol names and (some) according objects:
  pragmas
  directives
  keywords
  macros (+ macro objects)
  other (all other symbols) (objects to come?)

Symbol names can be mapped into:
- C keywords, e.g. map '__cdecl', 'cdecl' etc. into Kcdecl
- t_empty (ignore), e.g. map 'near', '__near' etc. into nothing
- typenames (parser must distinguish typenames from identifiers), e.g. '__int8'
- directive names
- pragma names

#defined symbols (with macros) can be treated as
- macros (expand, default behaviour)
- inline procedures (don't expand), deserves manual editing of argument types
- typedefs (interpret macro as typedef)
- named constants (typed or untyped?)
- enum members (of which enum?), having value
- ignore (never expand), e.g. redefinition #define xyz xyz

Typedefs can be handled in an application specific list, or in the preprocessor
symbol table. Option: macro as C typedef, typedef object addable?

ToDo:
- simple and consistent descriptions and objects in the STB.
  The objects should represent the (unique) public definition of each symbol,
    e.g. by their membership in the appropriate tables.
  Whenever a public high-level symbol object is created, it should be added to
    the general symbol table!
- unique token arrays (nil - empty - t_eof)? for empty token lists.
*)

{$INCLUDE config.pas}

interface

uses
  Classes,
  uHashList, uXStrings,
  uFiles, uTokenC;

type
  TTokenStream = class
  public  //for use in token stack push/pop operations
    fCont:  boolean;  //continue reading past EOF?
    fTemp:  boolean;  //destroy after EOF?
    fFile:  boolean;  //is file?
  public  //methods also implemented by a scanner
    function  unGet: boolean; virtual;
    procedure rewind; virtual;
    function  firstToken: eToken; virtual; //really?
    function  nextToken:  eToken; virtual; //filter locks...
    function  nextRaw: eToken; virtual; //no interpretation
  end;

  TargName = function(id: integer): string of object;

  aTokens = array of RPreToken;

//recorded token lists, for macro expansion and symbol definitions
(* base class: read only!
Overwrite in derived (writing) classes:
  nextRaw - if appending
  rewind - handle macro recording, locking, redimension FTokens...
*)
  TTokenArray = class(TTokenStream)
  protected
    iNext:  integer;
    FCount: integer;  //tokens recorded
    locked: boolean;
    FTokens: aTokens; //array of RPreToken;
  //backtracking and lookahead
    function  GetPosition: integer;
    procedure SetPosition(pos: integer);
    function  _getToken(index: integer): eToken;
      //set ScanToken and related variables, index unchecked! (internal only)
  public
    iErr:   integer;  //error index (parsing)
    //expected: eToken; //expected token kind(?)
    function  DefString(argName: TArgName): string;
  //inherited
    function  unGet: boolean; override;
    //procedure rewind; override; //mark locked
    //function  firstToken: eToken; override; //opt. start auto-record
    //function  nextToken:  eToken; override; //HLL filter
    function  nextRaw: eToken; override; //no interpretation, opt. record
  //relative tokens
    function  peekKind(offset: integer): eToken;
    function  peekToken(offset: integer): eToken;
  //parsing
    function  firstC: eToken; //expanded, C-keys, noWhite
    function  nextC: eToken; //expanded, C-keys, noWhite
    function  rescanC(pos: integer): eToken;  //restart at pos
    function  skip(t: eToken): boolean;
    function  expect(t: eToken; const msg: string): boolean;
  //properties
    property  Count: integer read FCount;
    property  Position: integer read GetPosition write SetPosition;
  end;

//implement writing
  TScanTokens = class(TTokenArray)
  protected
    function addRaw: eToken;
  public
    procedure rewind; override;
    function  nextRaw: eToken; override; //no interpretation, opt. record
    procedure retypeToken(oldkind, newkind: eToken);
  end;

(* under construction
Classify symbols:
- macro/global/local
- for macros: eventual C meaning (const, proc...)
? add skPKey: Pascal keyword, sym must be renamed and flagged
? add skPType: Pascal typename, no re-definition nor other manipulations
? add skLib: for external 'libname', WINBASEAPI -> lib 'kernel32.dll'
? add skBuiltIn: for built-in macros?
*)
  eSymbolKind = (
  //symbols (without macros)
    skSym,    //default: any symbol
    skCKey,   //treat as keyword (of appkind), disallow redefinition
  //if macro defined, treat definition as:
    skConst,  //named constant, macro should be deleted?
    skType,   //typename
    skProc,   //procedure/function
    skMacro   //expand as macro
  );
const
  aSymbolKind: array[eSymbolKind] of char = (
    'S',  //?
    'K', //never streamed
    'C', 'T', 'P', 'M'
  );

type
//was: base type for both preprocessor and compiler symbols!
  //TSymMacro = class(TXStrings)
  TSymMacro = class(TSymbol)
  public
  {$IFDEF Origin} //v2004+
    //nameID:   integer;  //for non-macros: index of symbol entry
    //OrgFile:  TTokenStream; //  TFileC; //in uScanC - circular ref!
    OrgName:  string; //symbol origin file
    OrgLine:  integer;
  {$ELSE}
  {$ENDIF}
    //in TSymbol
    function  BodyString: string; override;  //definition (tokens, only)
    constructor Create(const AName: string; AKey: integer = 0); override;
    destructor  Destroy; override;  //destroy token array
  //real macros only!
    function  GetTokens: TTokenArray; virtual; abstract;
  end;

  eMacroState = (
    msUndef,    //never defined, or unused before undef
    msDefined,  //defined, not used
    msUsed,     //defined and used
    msRedefined, //redefined after use
    msReused    //used after redefinition
  );

  RSymPrep = Object
  public
    r: THashItem;
  //various predefined interpretations
    mackind:  eSymbolKind;  //as macro (name)
    macstate: eMacroState;  //trace undef, redef...
    dirkind:  byte;   //as directive name, index into Directives[]
    pragkind: byte;   //as pragma name, index into Pragmas[]
    appkind:  eToken; //as application recognized keyword, typename...
  //found where (first? defined?)
    loc:  RFileLoc;
    function  GetMacro: TSymMacro;
    property  FMacro:  TSymMacro read GetMacro;
    property  FObject: TObject read r.FObject write r.FObject;
    property  FString: string read r.FString write r.FString;
  end;
  PSymPrep = ^RSymPrep;

//customize hash table
  TSymList = class(TXCustomHashList)
  protected //implement default item array
    Items: array of RSymPrep; // TSymPrep;
  //callbacks, virtual for any derived THashItem descriptors
    function  GetItem(index: integer): PXHashItem; override;
    function  GetHashItem(index: integer): PHashItem;
    function  Resize(newCapacity: integer): integer; override;
  public
    constructor Create(const AName: string; AKey: integer = 0); override;
    procedure ChangeState(index: integer; change: eMacroState);
    function  getSymbol(index: integer): PSymPrep;
    //function  getMacro(index: integer): TMacro;
    procedure defMacro(index: integer; kind: eSymbolKind);
    function  addDirective(const AName: string; index: integer): integer;
    function  addPragma(const AName: string; index: integer): integer;
    function  addKey(const AName: string; appkind: eToken): integer;
  end;

  //TMacroFilter = procedure(id: integer; newkind: eSymbolKind) of object;
  TMacroFilter = function(id: integer; newkind: eSymbolKind): eSymbolKind of object;

  TSymViewer = procedure(const locs: TFileLocs) of object;

var
  Symbols:  TSymList;
  StringTable:  TStringList;
  MacroFilter:  TMacroFilter; //user defined filter for new macros

var //current token
  ScanToken: RPreToken;
  ScanSym:  RSymPrep;  //for symbols
  ScanText: string;
  i_ttyp: eToken; //token kind determined by the parser (keywords mapped...)

//symbol display
var
  SymViewer:  TSymViewer;

procedure showSym(const locs: TFileLocs); overload;

function  mapSym(const s: string): integer;
function  IsDefined(const sym: PSymPrep): boolean;
//function  IsExpandable(const sym: TSymPrep): boolean;
function  IsExpandableC(const sym: PSymPrep): boolean;
function  IsExpandablePrep(const sym: PSymPrep): boolean;

//unused!!!
//procedure fixTables;
procedure resetTables;
//procedure InitKeys; -> InitSymbols?
procedure InitSymbols;

implementation

uses
  SysUtils, uUI,
  uScanC;

var
  //numFiles, numMacs,  //currently unused
  numSyms, numStrings: integer;


(* IsDefined - is symbol #defined?
*)
function  IsDefined(const sym: PSymPrep): boolean;
begin
  Result := (sym.FObject <> nil);
end;

(* IsExpandable - is symbol #defined and classified as preprocessor macro?
Macros furthermore can be classified as named constants or inline procedures,
  then they are considered as not expandable.
*)
function  IsExpandableC(const sym: PSymPrep): boolean;
begin
  Result := (sym.FObject <> nil) and (sym.mackind = skMacro);
end;

function  IsExpandablePrep(const sym: PSymPrep): boolean;
begin
  Result := (sym.FObject <> nil); // and (sym.mackind = skMacro);
end;

procedure fixTables;
begin
  numSyms := Symbols.Count;
  numStrings := StringTable.Count;
  //numFiles := Files.Count;
  //numMacs := Macros...
end;

procedure resetTables;
begin
  if numSyms = 0 then
    fixTables
  else begin
    Symbols.Pop(Symbols.Count - numSyms);
    StringTable.Capacity := numStrings;
  end;
  Files.Clear;
  IncludePath.Clear;
//problem: what if ConfigFiles not initialized by application?
  if ConfigFiles = nil then
    ConfigFiles := TStringList.Create
  else
    ConfigFiles.Clear;
end;

(* mapSym - create if not found, return symbol index.
Update ScanString, for unique strings???
(ScanString not in uses)
*)
function  mapSym(const s: string): integer;
begin
  Result := Symbols.Add(s);
  //ScanString := Symbols.Strings[Result]; //unique string
end;

procedure showSym(const locs: TFileLocs);
begin
  if assigned(SymViewer) then
    SymViewer(locs);
end;

(* InitKeys - init C keyword table
*)
var
  KeysInited: boolean = False;

(* Problem: no current file available for positions
*)
procedure InitKeys;
var
  i: eLangKeys;
begin
  if KeysInited then
    exit;
  for i := low(i) to high(i) do begin
    if i in [Ksizeof] then begin
    //allow for redefinition
      //nop
    end else
      Symbols.addKey(TokenNames[i], i);
  end;
  KeysInited := True;
end;

{ RSymPrep }

function RSymPrep.GetMacro: TSymMacro;
begin
  if r.FObject is TSymMacro then
    TObject(Result) := r.FObject //as TSymMacro
  else
    Result := nil;
end;

{ TSymList }

function TSymList.addDirective(const AName: string;
  index: integer): integer;
begin
  Result := self.Add(AName);  //no dupes
  Items[Result].dirkind := index;
end;

function TSymList.addKey(const AName: string; appkind: eToken): integer;
begin
  Result := self.Add(AName);  //no dupes
  Items[Result].mackind := skCKey;
  Items[Result].appkind := appkind;
end;

function TSymList.addPragma(const AName: string; index: integer): integer;
begin
  Result := self.Add(AName);  //no dupes
  Items[Result].pragkind := index;
end;

procedure TSymList.ChangeState(index: integer; change: eMacroState);
var
  ms: eMacroState;
begin
  ms := Items[index].macstate;
  if ms < msUsed then
  //always take new state
    Items[index].macstate := change
  else if change = msUsed then begin
    if ms = msRedefined then
    //change to redef after use
      Items[index].macstate := msReused;
  end else if ms = msUsed then
  //redefine after use - critical only when used afterwards!
    Items[index].macstate := msRedefined;
end;

constructor TSymList.Create(const AName: string; AKey: integer);
begin
  FList.GetItem := GetHashItem;
  FList.Resize := Resize;
  inherited;
end;

(* defMacro - mark macro as defined, undefined ...
Handle
  destruction of #undefined macros,
  restoration of keywords, (solution: #define rejected)
  prevent illegal overrides.

Symbol states:
  ordinary symbol (skSym)
  keyword (skCKey)
  else - macro, various interpretations possible
*)
//function TSymList.defMacro(index: integer; kind: eSymbolKind): integer;
procedure TSymList.defMacro(index: integer; kind: eSymbolKind);
begin
//define or undefine?
  if kind = skSym then begin  //#undef
  //undefine and destroy macro, if exists
    Members[index] := nil;  // = self.PutObject(index, nil);
    ChangeState(index, msUndef);
  end else begin
    ChangeState(index, msDefined);
    if assigned(MacroFilter) then begin
      kind := MacroFilter(index, kind);
    end;
  end;
//prevent redefinition of keywords
  if Items[index].mackind <> skCKey then begin
    Items[index].mackind := kind;
  end;  //else never change!
end;

function TSymList.GetHashItem(index: integer): PHashItem;
begin //callback
  Result := @Items[index];  //PHashItem(
end;

function TSymList.GetItem(index: integer): PXHashItem;
begin
  Result := PXHashItem(@Items[index]);
end;

function TSymList.getSymbol(index: integer): PSymPrep;
begin
  Result := @Items[index];
end;

function TSymList.Resize(newCapacity: integer): integer;
begin //callback
  if newCapacity <= 0 then
    newCapacity := 32;  //some size
  SetLength(Items, newCapacity);
  Result := newCapacity;
end;

{ TSymMacro }

constructor TSymMacro.Create(const AName: string; AKey: integer);
begin
  inherited Create(AName, AKey);
//file loc
  if ScanSym.loc.valid then
    self.loc := ScanSym.loc;  //first occurence
end;

destructor TSymMacro.Destroy;
begin
  GetTokens.Free;
  inherited;
end;

//strings to be redesigned...

function TSymMacro.BodyString: string;
begin
  if self.GetTokens = nil then begin
    //Result := #9'<undefined>';
    Result := '';
  end else
    Result := self.GetTokens.DefString(nil);
end;


{ TTokenStream }

function TTokenStream.firstToken: eToken;
begin
  Result := nextRaw;
end;

function TTokenStream.nextRaw: eToken;
begin
//empty stream, always return EOF
  Result := t_eof;
  ScanToken.kind := Result;
end;

function TTokenStream.nextToken:  eToken;
begin
  Result := nextRaw;
//derived classes filter further tokens
end;

procedure TTokenStream.rewind;
begin
  //nop
end;

function TTokenStream.unGet: boolean;
begin
//debug
  Result := False;  //illegal call
  assert(False, 'illegal call of unGet');
end;

{ TTokenArray }

function TTokenArray.DefString(argName: TArgName): string;
var
  i:  integer;
  s:  string;
  fBOL: boolean;
begin
  Result := #9;  //for append to name
  fBOL := True;
  //for i := 0 to high(FTokens) - 1 do begin
  for i := 0 to FCount - 1 do begin
    ScanToken := FTokens[i];
    case ScanToken.kind of
    t_eof:  break;
    t_bol:
      begin
        Result := Result + EOLstr + #9;
        fBOL := True;
      end;
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
    t_arg, t_argNX:  //only for TMacroFunc
      begin
        if assigned(argName) then
          s := argName(ScanToken.argID)
        else
          s := 'arg???';
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

function TTokenArray.expect(t: eToken; const msg: string): boolean;
begin
  Result := i_TTYP = t;
  if Result then
    nextC()
  else begin
    iErr := Position;
    Log(msg, lkSynErr);
  end;
end;

function TTokenArray.firstC: eToken;
begin
  rewind; //alloc tokens, start recording
//now get (filtered) token
  Result := nextC;  //handles first token appropriately
end;

function TTokenArray.nextC: eToken;
begin
  repeat
    Result := nextRaw;
  until not (Result in WhiteTokens);
//map keywords
  if Result = t_symNX then
    Result := t_sym;  //should be suppressed in nextTokenC?
  if (Result = t_sym) and (ScanSym.mackind = skCKey) then
    Result := ScanSym.appkind;
//set high level token kind
  i_ttyp := Result;
end;

function TTokenArray.nextRaw: eToken;
begin
  if iNext < FCount then begin
  //preloaded
    Result := self._getToken(iNext);
  end else  //limit exceeded
    Result := t_eof;
  if Result <> t_eof then
    inc(iNext);
end;

function TTokenArray.unGet: boolean;
begin
  Result := iNext > 0;
  if Result and (iNext < FCount) then //else next=eof
    dec(iNext);
end;

function TTokenArray.GetPosition: integer;
begin
  Result := iNext - 1;  //index of last returned token
end;

procedure TTokenArray.SetPosition(pos: integer);
begin
  assert(pos < FCount, 'can only rewind');
  iNext := pos; //next token to return on nextRaw.
end;

function TTokenArray.rescanC(pos: integer): eToken;
begin
  SetPosition(pos);
  Result := self.nextC;
end;

function TTokenArray.peekKind(offset: integer): eToken;
begin
//check count, using cardinal for both <0 and >FCount?
  inc(offset, iNext);
  //assert(cardinal(offset) < cardinal(FCount), 'illegal peek');
  if cardinal(offset) >= cardinal(FCount) then
    Result := t_eof
  else
    Result := FTokens[offset].kind;
end;

function TTokenArray.peekToken(offset: integer): eToken;
begin
//check count, using cardinal for both <0 and >FCount?
  inc(offset, iNext);
  //assert(cardinal(offset) < cardinal(FCount), 'illegal peek');
  if cardinal(offset) >= cardinal(FCount) then begin
    Result := t_eof;
    ScanToken.kind := Result; //force dummy token
  end else begin
    Result := _getToken(offset);
  end;
end;

function TTokenArray.skip(t: eToken): boolean;
begin
  Result := i_ttyp = t;
  if Result then
    nextC;
end;

function TTokenArray._getToken(index: integer): eToken;
begin
  ScanToken := FTokens[index];
  Result := ScanToken.kind;
//further init ScanToken?
  case Result of
  //t_rem: requires explicit request!
  t_err, t_car, //these also have a string argument!
  t_str:  ScanText := TokenText(ScanToken);
  t_sym, t_symNX:
    begin
      ScanSym := Symbols.getSymbol(ScanToken.symID)^;
      ScanText := ScanSym.FString;
    end;
  end;
end;

{ TScanTokens }

function TScanTokens.addRaw: eToken;
begin
  Result := ScanToken.kind;
//remember token
  if FCount >= Length(FTokens) then
    SetLength(FTokens, Length(FTokens) * 2);
  FTokens[FCount] := ScanToken;
  inc(FCount);
end;

function TScanTokens.nextRaw: eToken;
begin
//copied from TTokenArray.nextRaw
  if iNext < FCount then begin
  //preloaded
    Result := peekToken(0); //set ScanSym etc.
  end else if not locked then begin
  //add current token, first token is the current one!
    if iNext = 0 then begin
    //here (rarely called) check for dimensioned token array
      if FTokens = nil then
        SetLength(FTokens, 8);
      Result := ScanToken.kind;
    end else begin
    //get next non-empty token, after preprocessing
      repeat
        Result := nextTokenC; //from current source, preprocessed...
      until Result <> t_empty;
    end;
    addRaw; //increments FCount, not iNext
  end else  //limit exceeded
    Result := t_eof;
  if Result <> t_eof then
    inc(iNext);
end;

procedure TScanTokens.retypeToken(oldkind, newkind: eToken);
var
  p:  PPreToken;
begin
  p := @self.FTokens[iNext-1];
  assert(p.kind = oldkind, 'retype wrong token');
  p.kind := newkind;  //permanent change
  i_ttyp := newkind;
  ScanToken.kind := newkind;
end;

procedure TScanTokens.rewind;
begin
  iNext := 0;
  if not locked then begin
  //make sure that rewind is not called before start of recording!
  //we assume that a first call will only allocate a token array
    if FTokens = nil then begin
      SetLength(FTokens, 8);
    end else begin
      SetLength(FTokens, FCount);
      locked := True;
    end;
  end;
end;

procedure InitSymbols;
begin
  if Symbols <> nil then
    exit; //done!
  StringTable := TStringList.Create;
  StringTable.Add('');  //String ID = 0
  Symbols := TSymList.Create('symbols');
  InitKeys;
end;

initialization
  InitSymbols;
end.
