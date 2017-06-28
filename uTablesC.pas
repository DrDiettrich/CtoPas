unit uTablesC;
(* all tables for C parser:
  - symbols
  - typedefs
  - names (vars, members...)

ToDo:
- add (public) symbol objects to preprocessor/scanner table.
  (requires symID passed to symbol constructor)
  (handle constants etc. created from macros?)
+ added scope to type (static only, for now)
- problem: define unique (internal) sized types!
  int#_t is typedef'd! -> restore __int8 etc. type names?
~ mark constants from enum members as synthetic!
  (flag and don't save)
  Workaround: Check sym.Def for 'E' or '"E:...'
*)

(* typedef grammar overview
(...) sequence
[...] optional [0..1]
{...} repetition [0..n]
| alternative

Type = Qualified | Array | Pointer | ProcType | Struct_Union | Enum | Typename | Basetype.
Qualified = ( "#" | "V" ) Type.
  /* #=const (locked) V=volatile */
Array = "[" [dim] "]" Type.
Pointer = "*" Type. //add: "&" reference
ProcType = "(" { Param "," } [ ":~," ] ")" CallConv Type.
  /* ":~," stands for a "..." variable argument list */
  Param = [ name ] ":" [PMod] [Type]. /* either name or Type must be specified */
    PMod = "r"|"v"|"c"|"o"|"?" /* Ref=Var/Val/Const=In/Out/unknown */
  CallConv = ["I"] ["F" | "C"]. /* Inline, Fastcall, Cdecl (default Stdcall) */
/* final types */
Struct_Union = ( "S" | "U" ) ( "#" num | ":" tagname | "{" { Field "," }"}" ).
  Field = [ name ] ":" BitType. /* only here a Bitfield type is allowed */
    BitType = [ "<" bitsize ";>" ] Basetype.
Enum = "E" ( "#" num | ":" tagname | "{" { name ["=" Value]} "," } "}".
Typename = '"' name '"'. /* in double quotes */
/* basic types are: unsigned, signed, void, char, short, int, long, "long long", float, double, "long double", "..." */
Basetype = [ ("+"|"-") [size] ] ( "v"|"c"|"s"|"u"|"i"|"l"|"L"|"f"|"d"|"D"|"~" ).
    /* a "signed" ("-") prefix may be used for char types */
Value = (name | num";" | ["L"]("string"|'char')";"). /* also expression? */
*)
(* Problems:
  Distinguish proc/func? -> first args optional: this, result
  Distinguish Struct/Union/Enum from other types?
  Char types?
  Modifiers? (scope, callconv, param, bitfields, strings)
a   sym:argument
b
C class, pmod:cdecl
c char, pmod:constructor, sym:const
D long double (extended)
d double, pmod:destructor
E enum
e   sym:enum-member
F pmod:fastcall
f float, sym:field
g
h
I interface [amod:const/in, pmod:inline]
i int
j
k
L int64, [#mod:Unicode]
l long
M   sym:macro
m   sym:method
n
o amod:out, pmod:operator
P   sym:proc
p pmod:procedure
q
R amod:Result
r amod:ref
S struct
s short
T amod:this,  sym: template
t   sym: type
U union
u unsigned (int) = cardinal
V amod:value, vmod:volatile
v void, sym:var
w
x
y
z
--------
set?
string?
*)
(* Updates:
  Change S/U/E into ("S"|"U"|"E") ("#"(num|tagname) | "{" Fields "}").
    (":"num -> "#"num, for simpler parsing)
  Remove Scope from Type.
*)

(* Meta Files
ToDo:
- don't break strings with embedded ','
+ add Scope - for now: static only
+ change Proc from Typedef into SymVar
VarConst = name [":" Scope Type] [ = value ]
Typedef = name "=" Type
Proc = name ":" Scope ProcType [ "=" "{" code "}" ]
Scope = [ "!" | "?" ] /* "!"=static (module level), "?"=local, else global/public(?) */
Macro = "#define" name ["(" { param "," } ")" ] [ "\" body ]
  /* body are all continuation lines (ending with '\') */
  /* alternative: procedure: [ "{" body ] ? */
*)

interface

uses
  Classes,  //TStream
  //Contnrs,  //TObjectList
  config,  //{$I config.pas}
  uFiles, //include handler
  uTablesPrep, //macro handler
  uXStrings, uUI,
  uMacros, uScanC, uTokenC;

var
  fAutoConst: boolean = False;  // True; //convert macros into constants?
  fDebug: boolean = False;  // True; //debug macro output
  fMetaNames: boolean;
    //True during meta-output of procedures (parse body)

(* remember library name from WIN...API macros
  for external <lib>

  Applies to currently defined procedure only, clear after use!
*)
var
  FromLib: string;

type
  eKey = eLangKeys;

  eSymType = (
  //not yet typed
    stUnknown,
  //preprocessor symbols???
    stMacro,  //to be expanded
    stInline, //treat as procedure, don't expand
    //stLocal,  //special mangled names
  //both preprocessor and parser???
    stConst, //subtypes: int, str, expr?
      stEnumMember,  //special scoping in C
  //parser created
    stVar,
  {$IF __proto}
    stProto,
  {$IFEND}
    stProc,
    stTypedef //stay last, special scoping in C
  );

  CreatableSyms = succ(stInline)..stTypeDef; //high level language symbols
  sSymType = set of eSymType;
  TSymDef = string;
  TSymVal = string;
var //map char to SymType
  mSymType: array[char] of eSymType;
const
  aSymType: array[eSymType] of char = (
  //not yet typed
    'u',  //stUnknown,
  //preprocessor symbols???
    'm',  //stMacro,  //to be expanded
    'i',  //stInline, //treat as procedure, don't expand
  //both preprocessor and parser???
    //'l',  //stLocal,
    'c',  //stConst,
    'e',  //stEnumMember,
  //parser created
    'v',  //stVar,
  {$IF __proto}
    'o',  //stProto,
  {$IFEND}
    'p',  //stProc,
    't'   //stTypedef //stay last
  );
  SymKinds: array[eSymType] of string = (
    'ident ', //stUnknown,
  //preprocessor symbols???
    'macro ', //stMacro,  //to be expanded
    'inline ',  //stInline, //treat as procedure, don't expand
  //both preprocessor and parser???
    //'local ', //stLocal
    'const ', 'enum ',  //stConst, stEnumMember,  //const variable, enum member, #define???
  //parser created
    'var ',   //stVar,
  {$IF __proto}
    'prototype ',
  {$IFEND}
    'proc ',  //stProc,
    'type '   //stTypedef //stay last
  );

const
  NoDef = '';
  NoVal = '';

const
  //InitableSyms = [stLocal, stConst, stVar];
  InitableSyms = [stConst, stVar];
  DefaultStorage = Kauto;

type
  TTypeDef = class;
  TScope = class;

  TSymbolC = class(TSymbol)
  protected
    function  GetCaption: string; override;
    function  GetName: string; override;
      //questionable!
  public
    kind: eSymType;
    DupeCount:  byte;
      //intended purpose: substitution of conflicting names
    ScopeNum:   byte;  //byte should be sufficient?
    //name: string; //? for independence from owner list?
    Def:  string;
    BaseType: TTypeDef; //TSymbolC;
      //intended purpose: dummy typedefs for anonymous complex types.
  //values, depending on kind:
    StrVal: string; //format: Ansi?
    destructor  Destroy; override;
    function  SetID(nameID: integer): integer;
    function  toString: string; override;  //full (meta) string
    function  MetaName: string;
    function  TypedName: string; virtual;  //-> name[:type]
    function  UniqueName: string; virtual;
  //reduce confusion
    function  UniqueID: integer; override;
    function  GlobalID: integer; override;
  end;

  TSymClass = class of TSymbolC;

  TSymVar = class(TSymbolC)
  protected
    function  GetCaption: string; override;
  public
    function  BodyString: string; override;
  end;

  TSymConst = TSymVar;
  TSymEnum = TSymConst;
{$IF __proto}
  TSymProto = TSymbolC;  //eventually with virtual extensions for TSymProc?
{$ELSE}
  TSymProto = TSymVar;  //really common ancestor?
{$IFEND}

  TTypeDef = class(TSymbolC)
  protected
    function  GetCaption: string; override;
  public
    constructor Create(const AName: string; AKey: integer = 0); override;
    //property  Ref: string read GetName; //quote???
    function  Ref: string;
    //property  Def: string read StrVal write StrVal;
    //property  toString: string read GetCaption;
  end;
  TSymType = TTypeDef;  //alias for use in parsetrees

//These scopes are based on definition strings.
//new symbols: force local, or find in parent scopes???
//also used for parameter lists?
//C scopes do not contain types etc. - are all globals!
(* Scopes can be classified by owner:
none - global (also by address, it's a singleton)
global - module scope (for static storage duration)
other - local
*)
  TScope = class(TSymbols)
  protected
    fParentScope: TScope;  //may be different from owner
    function defSym(AKind: CreatableSyms; const AName, ADef: string;
      const AVal: TSymVal = ''): TSymbolC; virtual;
  public
    constructor CreateScope(AOwner: TXStrings; AParent: TScope; const AName: string = '');
    function  getSym(const AName: string): TSymbolC; overload;
    function  getSym(index: integer): TSymbolC; overload;
    function  defConst(const AName, ADef, AVal: string): TSymbolC;
    function  defProc(const AName, ADef: string): TSymProto; virtual;
    function  defVar(const AName, ADef, AVal: string): TSymbolC;
  //added - implemented only in GlobalScope/TTypeDefs
    function  defEnumMember(const AName, ADef, AVal: string): TSymbolC; virtual;
    function  isType(const AName: string): integer; virtual;
      //>= 0 (type index) if typename
    function  defType(const AName, ADef: string; id: integer): TTypeDef; virtual;
    function  getType(const AName: string): TSymType; overload; virtual;
    function  getType(index: integer): TSymType; overload; virtual;
  //nested scopes
    function  findSym(const AName: string): TSymbolC;
      //search all scopes
    property  ParentScope: TScope read fParentScope;  // write fParentScope;
  end;

  TOnSymRead = function(symt: eSymType; const n, t, v: string; var f: TextFile): boolean of object;

  TTypeDefs = class(TScope)
  public
    DefFile:  string;
    //OnProcRead: TOnProcRead;
    OnSymRead: TOnSymRead;
  {$IF __PreInclude}
    //DefPos: integer; - not for text files :-(
    procedure HandleDefFiles(f: TFile);
  {$IFEND}
    //constructor Create(const AName: string; AKey: integer = 0); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromFile(const fn: string); override;
    //function  LoadDefFile(cbProc: TOnProcRead): boolean;
    function  LoadDefFile(cbProc: TOnSymRead): boolean;
    function  MacroFilter(id: integer; newkind: eSymbolKind): eSymbolKind;
  //global only symbols:
    function  defEnumMember(const AName, ADef, AVal: string): TSymbolC; override;
    function  defProc(const AName, ADef: string): TSymProto; override;
    function  isType(const AName: string): integer; override;
      //>= 0 (type index) if typename
    function  defType(const AName, ADef: string; id: integer): TTypeDef; override;
    function  getType(const AName: string): TSymType; overload; override;
    function  getType(index: integer): TSymType; overload; override;
    function  closestType(const ADef: string): string;
    property  TypeCount: integer read GetCount;
  end;
  TGlobalScope = TTypeDefs;
  //TScope = TScopeC; //use as base class
  TScopeC = TScope; //use as base class


  TLocalScope = class(TScope)
  protected
    ScopeNum: integer;
    function defSym(AKind: CreatableSyms; const AName, ADef: string;
      const AVal: TSymVal = ''): TSymbolC; override;
  public
  end;

  TParamScope = class(TLocalScope)
  public
    constructor Create(const AName: string; AKey: integer = 0); override;
  end;

  TScopeList = class(TList)
  public
    destructor  Destroy; override;
      //destroy owned scopes
    function  getScope(index: integer): TScope;
  end;

  TSymBlock = class(TStringList)  //NO hash!
  protected
    ScopeLvl:  integer;
    ScopeNum:  integer;
  public
    CurScope:  TScope;  //TLocalScope;
    Scopes: TScopeList;  //hold ALL local scopes - including params
    constructor CreateBlock(AOwner: TXStrings; AScope: TScope);
      //arguments for Scope.CreateScope
    destructor  Destroy; override;
    function  Add(const s: string): integer; override;
    function  toString: string; //override;  //full (meta) string
  //parser API - the string is cleared/re-initialized
    procedure addStmt(var s: string);
    procedure addLcls;
    function  enterBlock(var s: string): TSymBlock;
    function  leaveBlock(var s: string): TSymBlock;
  end;

  TSymProc = class(TSymProto)
  protected
    FBody: TSymBlock;  // TScopeC;  //Block!!! both scope and decl-str?
    function  getBody: TSymBlock;
    function  GetCaption: string; override;  //for meta-file output
  public
    //ExtName, //for: external name <name>
    LibName: string; //for: external <lib>
    Params: TScopeC;
    //Locals: TScopeC;
    destructor  Destroy; override;
    procedure checkLocals;
    //procedure PreScan(ext: TScope);
    procedure takeParams(var AScope: TScope);
    function  BodyString: string; override;
    function  toString: string; override;  //full (meta) string
    //function  TypedName: string; override;  //-> name[:type]
    property  Body: TSymBlock read getBody;
    function  hasBody: boolean;
  end;

  TModule = class(TTypeDefs)
  public
    Source: TFile;
    procedure Save;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TModuleList = class(TXStringList)
  public
    function  AddModule(const fn: string): TModule;
    procedure Save;
  end;

// --- a helper object, used while parsing declarations ---

  eSized = (szNone, szShort, szLong, szLongLong);

  TDeclAttrs = set of Kconst..Kvolatile;
  //eCallConv = Kcdecl..Kstdcall;

//unused!
  TPublic = (
    pubAuto,  //typically global symbol
    //pubGlobal,
    pubStatic,  //???no/static symbol?
    pubMember,  //as struct member -> in declSym, no symbol created
    pubParam,   //as parameter -> in??? (explicit scope!?)
    pubLocal    //as local variable -> explicit scope
  );

(* Manage type declarations, as strings.
*)
  RType = object
  protected
    procedure handleStorage;
    function  todo: boolean;
  public
  //scopes
    mbrScope, //params
    declScope:  TScope;
    fTempScope: boolean;  //own mbrScope?
  //declaration specifiers
    storage: eStorageClass; //eToken;
      //linkage, duration
    signed: eToken;
    sized: eSized;
    specToken: eToken;  //finish spec, handle implied 'int'
    attrs: TDeclAttrs;  //spec only?
    call: eToken; //TCallConv;
    _inline:  boolean;
  //declarator, based on common declaration specifiers
    spec, pre, post: string;
    name: string;
    loc: RFileLoc;
    nameID:  integer; //scanner symbol ID
    Value: TSymVal;
    basetype: TTypeDef; //complex type, tagged -> S:<name>, U:<name>, E:<name>
    declSym: TSymbolC;  //as soon as created
    procedure Init;   //~constructor, clear everything
    procedure Reset;  //init declarator, keep specification
    procedure makeTagRef(sue: eKey; const tag: string);
    procedure setNameSym; //from ScanSym
    procedure propagateName(const t: RType);
    procedure type_specifier(i_ttyp: eToken; fDefault: boolean);
    procedure makeDim(const dim: string);
    procedure makePointer;
    function  qualify(t: eKey; fSpec: boolean): boolean; //const or volatile
  {$IF __lclScopes}
  //add to mbrScope
    procedure makeScope;
    function  makeEnumMember(const t: RType): TSymbolC;
    function  makeStructMember(const t: RType): TSymbolC;
    function  makeParam(const t: RType): string;
    function  makeVararg: string;
    //procedure makeParams;
    procedure makeParams(const params: string);
  {$ELSE} //no local scopes
    procedure makeEnumMember(const n, t, v: string; id: integer);
    procedure makeStructMember(const t: RType; const bitsize: string);
    function  makeParam(const Aname, Adef: string): string;
    function  makeVarargs: string;
    procedure makeParams(const params: string);
  {$IFEND}
    procedure finishComplex;
    procedure finishEnum;
    procedure endDecl;  //(fPublic: boolean);
      //create symbol.
      //arg to become scope object?
    function  getDef: string;
  end;

const
  MemberScope = Ktypedef;

function  quoteName(const Aname: string): string;
function  quoteType(const Aname: string): string;
//function  unifyName(const n: string; cnt: integer): string;
function  unifiedName(const n: string; cnt: integer): string;
  //common format for disambiguated names

procedure InitAlias;

// --- parsing without token stream ---

function  nextToken: eToken;
function  skip(t: eToken): boolean;
function  expect(t: eToken; const msg: string): boolean;
procedure SynErr(const msg: string);
procedure LogBug(const msg: string);

var
//parser scopes
  Globals:  TGlobalScope;
  Statics:  TModule; //non-exported types are allowed?
  enumScope: TScope;
//modules
  Modules:  TModuleList;
  //CurModule:  TModule;
  SymLocs:  TFileLocs;

procedure showSym(id: integer); overload;

//config

const
  StreamVersion = '#1.2';
//distinguish type names from var names
  typeQuote = '"';  // '''';
  nameQuote = ''''; //'"';
  HexPrefix = '0x'; //meta format
//scopes
  //LocalScope = '?';
  StaticScope = '!';
  //PublicScope = 'p';
//lists
const
  ListTerm = ';';
  ListTermT = opSemi;
  {$IFDEF ExprTerm}
    BitFieldStart = '<';
    BitFieldEnd = '>';
    ExpTerm = ',';  //expression terminator, distinct from list separator
    ExpTermT = opComma;
    ExpSep = '';
  {$ELSE}
    BitFieldStart = '{';
    BitFieldEnd = '}';
    ExpTerm = '';  //use separator, instead of terminator
    ExpTermT = t_empty;
    ExpSep = ',';
  {$ENDIF}

var
  TypePrefix: string; //= 'T';

implementation

uses
  SysUtils,
  uDirectives,  //pragma handlers
  uParseC, StrUtils;

var
  fFinalizing: boolean = False; //prevent race conditions during shutdown

function quoteName(const Aname: string): string;
begin
  if Aname = '' then
    Result := ''
  else
    Result := nameQuote + Aname + nameQuote;
end;

function quoteType(const Aname: string): string;
begin
  if Aname = '' then
    Result := ''
  else
    Result := typeQuote + Aname + typeQuote;
end;

function  unifiedName(const n: string; cnt: integer): string;
begin
  if cnt > 0 then
    Result := n + '_' + IntToStr(cnt)
  else
    Result := n;
end;

procedure InitSymTypes;
var
  i: eSymType;
begin
  for i := low(i) to high(i) do
    mSymType[aSymType[i]] := i;
end;

procedure showSym(id: integer);
var
  r:  PSymPrep;
  mac:  TSymbol;  //TSymMacro;
  sym:  TSymbolC;
begin
  r := Symbols.getSymbol(id);
  if r.loc.valid then
    SymLocs[2] := r.loc
  else
    SymLocs[2].invalidate;
  sym := nil; //if not otherwise specified
  mac := r.GetMacro;
  if (mac <> nil) {and (mac.loc.src <> nil)} then begin
    SymLocs[1] := mac.loc;
    sym := Globals.getSym(mac.altID);
  end else begin
    SymLocs[1].invalidate;
    TObject(mac) := r.FObject;
    if mac is TSymbolC then
      sym := TSymbolC(mac);
  end;
  if sym <> nil then
    SymLocs[0] := sym.loc;
  showSym(SymLocs);
end;

// --------- not using token stream --------------

procedure SynErr(const msg: string);
begin
  uUI.Log(msg, lkSynErr);
end;

procedure LogBug(const msg: string);
begin
  uUI.Log(msg, lkBug);
end;

(* nextToken - token filter for parser
- map keywords
- concat strings
*)
var
  Concatenated: string;
function  nextToken: eToken;
var
  fConcat: boolean;
  StrToken: RPreToken;
const
  StrTokens = WhiteTokens + [t_str];
begin
  repeat  //suppress unwanted tokens
    Result := nextTokenC;
  //map keywords
    if Result = t_symNX then
      Result := t_sym;  //should be suppressed in nextTokenC?
    if (Result = t_sym) and (ScanSym.mackind = skCKey) then
      Result := ScanSym.appkind;
  until not (Result in WhiteTokens);
//concat strings
  if Result = t_str then begin
  //save token
    StrToken := ScanToken;
    Concatenated := ScanText;
    fConcat := False;
  //look ahead
    i_ttyp := nextTokenC;
    while i_ttyp in StrTokens do begin
      if i_ttyp = t_str then begin
        Concatenated := Concatenated + ScanText;
        fConcat := True;
      end;  //else white token
      i_ttyp := nextTokenC;
    end;
  //unget non-string token
    if not pSrc.unGet then
      Log('unget failed', lkBug);
  //restore string
    ScanText := Concatenated;
    if fConcat then begin
    //map string
     	SetTokenString(StrToken, ScanText);
    end;
  //restore token
    ScanToken := StrToken;
  end;
  i_ttyp := Result;
end;

function  skip(t: eToken): boolean;
begin
  Result := i_TTYP = t;
  if Result then
    nextToken();
end;

(* skip expected token.
  Log msg if it's not the expected token.
*)
function  expect(t: eToken; const msg: string): boolean;
begin
  Result := i_TTYP = t;
  if Result then
    nextToken() //updates i_ttyp
  else
    //Log(msg, lkDiag);
    SynErr(msg);
end;


(* Calling conventions
MSC:  __cdecl, __stdcall, __fastcall
Delphi: cdecl, stdcall, register, pascal, safecall
__fastcall has no equivalent in Delphi?
Type markers:
F __fastcall
C __cdecl
I __inline
*)

(* InitAlias - init predefined types and keyword alias.
  Some compiler specific keywords and types deserve special handling.
  Here we define all these "__..." symbols.
*)
procedure InitAlias;
begin
{$IF __PreInclude}
//include handler
  uFiles.PreInclude := AllTypes.HandleDefFiles;
{$IFEND}
  ScanningFile := 'built-in'; //prevent error
//keyword alias
  Symbols.addKey('__const', Kconst);
//"inline" may not be known to all compilers?
  Symbols.addKey('inline', Kinline);
//predefined types
  Globals.Clear;
  Globals.defType('__int8',  '-1', 0);
  Globals.defType('__int16', '-2', 0);
  Globals.defType('__int32', '-4', 0);
  Globals.defType('__int64', '-8', 0);
end;

{ TScope }

constructor TScope.CreateScope(AOwner: TXStrings; AParent: TScope;
  const AName: string);
begin
  CreateIn(AOwner, AName);
  fParentScope := AParent;
end;

function TScope.findSym(const AName: string): TSymbolC;
begin
  Result := getSym(AName);
  if Result <> nil then
    exit;
  if fParentScope <> nil then
    Result := fParentScope.findSym(AName);
end;

function TScope.getSym(index: integer): TSymbolC;
begin
  TObject(Result) := Members[index];
end;

function TScope.getSym(const AName: string): TSymbolC;
var
  i: integer;
begin
  i := IndexOf(AName);
  if i >= 0 then
    TObject(Result) := Members[i]
  else
    Result := nil;
end;

function TScope.defSym(AKind: CreatableSyms; const AName, ADef: string;
  const AVal: TSymVal): TSymbolC;
const
  SymClasses: array[CreatableSyms] of TSymClass = (
    //TSymLocal,  //stLocal
    TSymConst,  //stConst,
    TSymEnum,  //stEnumMember,  //const variable, enum member, #define???
    //stField,  //local in scope (proc, struct...)
    TSymVar,  //stVar,
  {$IF __proto}
    TSymProto,
  {$IFEND}
    TSymProc, //stProto, stProc,
    TSymType  //stTypedef //stay last
  );
var
  i: integer;
  sc: TSymClass;
  proc: TSymProc absolute Result;
begin
  sc := SymClasses[AKind];
  i := IndexOf(AName);
  if i >= 0 then begin
    TObject(Result) := Members[i];  // := getSym(i);
    if (Result.kind <> AKind) then begin
    {$IFDEF appSym}
      if Result.kind = stLocal then
        Result := nil //always destroy untyped symbol
      else
    {$ELSE}
    {$ENDIF}
      if (Result.ClassType <> sc) then begin
        Log('morphing ' + AName, lkTodo); //should never occur!
        //Delete(i); - not implemented!
        Result := nil;  //PutObject(nil)?
      end;
    end;
  end else
    Result := nil;
//checked update
  if Result = nil then begin
    if i >= 0 then begin
    //todo: properly replace objects!
      Result := sc.Create(AName);
      Objects[i] := Result;
      Result.Owner := self;
    end else
      Result := sc.CreateIn(self, AName);
    Result.kind := AKind;
    Result.Def := ADef;
    Result.StrVal := AVal;
  end else begin
  //check
    if (Result.kind <> AKind) and (Result.kind >= low(AKind)) then begin
      //if AKind <> stLocal then begin
      if True then begin
        Log('retype ' + AName, lkTodo);
        Result.kind := AKind; //assume compatible? really decrease kind???
      end;
    end;
    if ADef <> '' then begin
      if (Result.Def <> '') and (Result.Def <> ADef) then begin
      //filter old style procedure redefinitions - detect how?
        if Pos('()', Result.Def) <= 0 then begin
        //if Result.Def <> '()i' then begin
          Log('redef ' + AName, lkTodo);
          Log('as ' + ADef, lkTodo);
        end;
      end;
      Result.Def := ADef; //always redef
    end;  //else keep old def?
    if AVal <> NoVal then begin
      if (Result.StrVal <> NoVal) and (Result.StrVal <> AVal) then begin
        Log('reval ' + AName, lkTodo);
        Result.StrVal := AVal;
      end;
    end;  //else keep old val?
  end;
//external reference?
  if (FromLib <> '') and (Result is TSymProc) then begin
    proc.LibName := FromLib;
    FromLib :=''; //valid only for this proc
  end;
end;

function TScope.defConst(const AName, ADef, AVal: string): TSymbolC;
begin
  Result := defSym(stConst, AName, ADef, AVal);
end;

function TScope.defProc(const AName, ADef: string): TSymProto;
begin
  Result := defSym(stProc, AName, ADef) as TSymProto;
end;

function TScope.defVar(const AName, ADef, AVal: string): TSymbolC;
begin
  Result := defSym(stVar, AName, ADef, AVal);
end;

// --- all these defer to the global scope ---

function TScope.defEnumMember(const AName, ADef, AVal: string): TSymbolC;
begin
  //Result := defSym(stEnumMember, AName, ADef, AVal);
  Result := Globals.defEnumMember(AName, ADef, AVal);
end;

function TScope.getType(const AName: string): TSymType;
begin
  Result := Globals.getType(AName);
end;

function TScope.getType(index: integer): TSymType;
begin
  Result := Globals.getType(index);
end;

function TScope.isType(const AName: string): integer;
begin
  Result := Globals.isType(AName);
end;

function TScope.defType(const AName, ADef: string; id: integer): TTypeDef;
begin
  Result := Globals.defType(AName, ADef, id);
end;


{ TTypeDefs }

function TTypeDefs.defType(const AName, ADef: string; id: integer): TTypeDef;
begin
  if (Length(Aname) > 2)
  and (Aname[2] = ':') and (Aname[3] in Digits) then begin
    Log('create dummy: ' +  AName, lkDebug);
  end;
  TSymbolC(Result) := defSym(stTypedef, AName, ADef);
  Result.SetID(id);
end;

function TTypeDefs.isType(const AName: string): integer;
var
  sym: TSymbolC;
begin
  Result := IndexOf(AName);
  if Result >= 0 then begin
    sym := getSym(Result);
    if (sym.kind <> stTypedef) then
      Result := -1;
  end;
end;


{$IF __PreInclude}

(* HandleDefFiles - process definition files
*)
procedure TTypeDefs.HandleDefFiles(f: TFile);
var
  s, n, v:  string;
  r:  TextFile;
  i, id:  integer;
  symC: TSymbolC;
  //symP: TSymPrep;
begin
  s := f.dir + f.name + '.defs';
  if not FileExists(s) then
      exit;
  AssignFile(r, s);
  Reset(r);
  while not EOF(r) do begin
    ReadLn(r, s);
    i := Pos('=', s);
    if i > 1 then begin
      n := Copy(s, 1, i-1);
      v := Copy(s, i+2, Length(s));
      id := Symbols.Add(n);
      case s[i+1] of
      'c':  //constant
        if not fAutoConst then begin
          //if v = '' then v := '#i';  //default: integer const?
          symC := self.defConst(n, v, '');
          Symbols.defMacro(id, skConst);
        end;
{$IFDEF MakeProcedures}
//to be debugged!
      'p':  //procedure
        begin
          //symC := self.Procs.defType..Types.defType(n, v);
          symC := self.defProc(n, v);
          Symbols.defMacro(id, skProc);
        end;
{$ELSE}
{$ENDIF}
      't':  //typedef
        begin
          symC := self.defType(n, v);
          Symbols.defMacro(id, skType);
        end;
      end;
    end;
  end;
  CloseFile(r);
end;

{$ELSE}
  //no *.defs handler
{$IFEND}

type
  eDefFmt = (fmtUnknown, fmt0, fmt10, fmt11, fmt12);
  eLineType = (
    ltEof,
    ltOldData,
    ltOldBreak,
    ltMac,
    ltProc,
    ltOther
  );
//callbacks
  TProcessProc = function(const ln: string; var f: TextFile): boolean;
  TProcessOther = function(symc: char; const ln: string): boolean;

procedure TTypeDefs.LoadFromFile(const fn: string);
var
  f: TextFile;
  ln, s: string;
  n, t, v: string;
  typ: TTypeDef;
  sym: TSymbolC;
  cv: TSymVar absolute sym;
  proc: TSymProc absolute sym;
  i: integer;
  fFmt: eDefFmt; //boolean;
  symc: char;
  symt: eSymType;
  symln:  integer;
  symf: string;

  procedure CheckFmt(const s: string);
  begin
    if s = '#1.0' then
      fFmt := fmt10
    else if s = '#1.1' then
      fFmt := fmt11
    else if s = '#1.2' then
      fFmt := fmt12
    else if Copy(s, 1, 4) = '--- ' then
      fFmt := fmt0
    else
      fFmt := fmtUnknown;
  end;

  //function  ReadLine: boolean;
  function  ReadLine: eLineType;
  begin
    ReadLn(f, ln);
    while ln[1] = '[' do begin
      symf := Copy(ln, 2, Length(ln) - 2);  //strip '[]'
      ReadLn(f, ln);
    end;
    if fFmt in [fmt10, fmt11, fmt12] then begin
    //new format
      if fFmt = fmt12 then begin
        symc := ln[1];  //type character
        i := Pos(#9, ln);
        if i > 2 then begin
          symln := StrToInt(Copy(ln, 2, i-2));
        end else
          symln := 0;
        //ln := Copy(ln, i+1, Length(ln));
        inc(i);
        while ln[i] <= ' ' do
          inc(i);
        ln := Copy(ln, i, Length(ln));
      end else begin
        i := Pos(#9, ln);
        i := PosEx(#9, ln, i+1);
        symc := ln[i+1];  //type character
        ln := Copy(ln, i+2, Length(ln));
      end;
      symt := mSymType[symc];
      if symc < 'a' then begin
      //macro format
        while ln[Length(ln)] = '\' do begin
        //merge continued line
          ReadLn(f, s);
          assert(s[1] = #9, 'expected <tab>');
          ln := ln + Copy(s, 2, Length(s));
        end;
        Result := ltMac;
      end else if symt = stProc then begin
      (* Problem: why are procedure prototypes ";" terminated???
        Proc lines end with "{(", the entire proc ends with "};"
      *)
        //if ln[Length(ln)] = '{' then
        if ln[Length(ln)] = '(' then
          Result := ltProc //proc with definition
        else
          Result := ltOther;  //???
      end else begin
        while ln[Length(ln)] = ListTerm do begin
        //merge continued line
          ReadLn(f, s);
          assert(s[1] = #9, 'expected <tab>');
          ln := ln + Copy(s, 2, Length(s));
        end;
        Result := ltOther;
        //processOther(symc, ln)
      end;
    end else begin
    //old format
      while ln[Length(ln)] = '\' do begin
      //merge continued line
        ReadLn(f, s);
        assert(s[1] = #9, 'expected <tab>');
        ln := ln + Copy(s, 2, Length(s));
      end;
      if ln[1] = '-' then
        Result := ltOldBreak  //was: False
      else
        Result := ltOldData;  //was: True
    end;
  end;

  procedure SkipSection;
  begin
    while not EOF(f) do begin
      if ReadLine = ltOldBreak then
        break;
    end;
  end;

  function  ReadTP: boolean;
  begin
    if fFmt > fmt0 then
      Result := True
    else
      Result := ReadLine = ltOldData;
    if not Result then
      exit;
  //assume: <name> : <typedef>
    i := Pos('=', ln);
    case fFmt of
    fmt0, fmt10:  //<name> : <typedef>
      begin
        n := Copy(ln, 1, i-2);
        t := Copy(ln, i+2, Length(ln));
      end;
    fmt11, fmt12:  //<name>:<typedef>
      begin
        n := Copy(ln, 1, i-1);
        t := Copy(ln, i+1, Length(ln));
      end;
    else
      assert(False, 'unexpected DEF format');
    end;
    v := '';  //types have no value
  end;

  procedure LoadTypes;
  begin
    while ReadTP do begin
      typ := defType(n, t, 0);
    end;
  end;

  function  ReadCVP: boolean;
  begin // <name>: <type> [= <val>]
    if fFmt > fmt0 then
      Result := True
    else
      Result := ReadLine = ltOldData;
    if not Result then
      exit;
    i := Pos(':', ln);
    if i > 0 then begin
      n := Copy(ln, 1, i-1);
      if fFmt > fmt0 then
        t := Copy(ln, i+1, Length(ln))
      else
        t := Copy(ln, i+2, Length(ln));
    end else begin
      n := ln;
      t := '';
    end;
    i := Pos('=', t);
    if i > 0 then begin
      if fFmt > fmt0 then begin
        v := Copy(t, i+1, Length(t));
        SetLength(t, i-1);
      end else begin
        v := Copy(t, i+2, Length(t));
        SetLength(t, i - 2);  //???
      end;
    end else
      v := '';
  // RP added test for empty
    typ := nil;
    if (t <> '') and (t[1] = typeQuote) then begin
      s := Copy(t, 2, Length(t) - 2); //unquote
      typ := Globals.getType(s);
    end;
  end;

  function  ReadP: boolean;
  begin
    Result := ReadCVP;
    if not Result then
      exit;
  //strip start of body, if present
  { TODO : check proc fmt decoding }
    if t[Length(t)] = '(' then begin
      SetLength(t, Length(t) - 2);
      v := '{(';
    end else
      v := '';
  end;

  procedure LoadVars;
  begin
    while ReadCVP do begin
      sym := Globals.defVar(n, t, v);
      if typ <> nil then sym.BaseType := typ;
    end;
  end;

  procedure LoadConsts;
  begin
    //SkipSection;  //for now
    //while ReadCV(True) do
    while ReadCVP do begin
      sym := Globals.defConst(n, t, v);
      if typ <> nil then sym.BaseType := typ;
    end;
  end;

  procedure LoadProcs;
  begin
    //while ReadTP do begin
    while ReadCVP do begin
      sym := Globals.defProc(n, t);
    end;
  end;

  function  LoadMacros: boolean;
  begin //macros have special file format
  //skip, for now
    Result := False;
    //file positioned at first macro definition
  end;

begin
  DefFile := fn;
  AssignFile(f, fn);
  Reset(f);
try
  ReadLn(f, ln);
  { DONE : also read new file format }
  CheckFmt(ln);
  case fFmt of
  fmt0: //if Copy(ln, 1, 4) = '--- ' then
    while not EOF(f) do begin
      case ln[5] of //expected in this order
      't':  LoadTypes;
      'v':  LoadVars;
      'c':  LoadConsts;
      'p':  LoadProcs;
      'm':  if not LoadMacros then break;
      else  assert(false, 'bad file format');
      end;
    end;
  fmt10, fmt11, fmt12:
    while not EOF(f) do begin
      case ReadLine of
      ltMac:  //ignore, for now
      { TODO : decode macro stream format }
        if assigned(self.OnSymRead) and OnSymRead(symt, ln, '', '', f) then
          ;
      (*
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
        else
          ...
        end;
      *)
      ltProc: //defined proc!
        if ReadP
        and assigned(self.onSymRead) and OnSymRead(symt, n, t, v, f) then
          //done
        else begin //here: skip body only
          sym := Globals.defProc(n, t);
          if v <> '' then begin
            proc.Body.Add(v);
            while not eof(f) do begin
              ReadLn(f, ln);
              if ln = ';' then
                break;  //found guard
              proc.Body.Add(ln);
            end;
          end;
        end;
      else  //ltOther:
        { DONE : redesign Globals.Read... }
        if (((symt = stTypedef) and ReadTP)
        or ((symt <> stTypedef) and ReadCVP))
        and assigned(self.onSymRead) and OnSymRead(symt, n, t, v, f) then
          //done?
        else begin
          case symt of
          stConst:  // 'c':
            begin
              sym := Globals.defConst(n, t, v);
              if typ <> nil then sym.BaseType := typ;
            end;
          stEnumMember:  //const variable, enum member, #define???
            begin
              sym := Globals.defEnumMember(n, t, v);
              if typ <> nil then sym.BaseType := typ;
            end;
          //parser created
          stVar:
            begin
              sym := Globals.defVar(n, t, v);
              if typ <> nil then sym.BaseType := typ;
            end;
          stProc: //should have been caught above!
              sym := Globals.defProc(n, t);
          stTypedef: //stay last
              typ := defType(n, t, 0);
          end;
        end;
      end;
    end;
  else
    Log('Unknown file format: ' + ln, lkConfig);
  end;
finally
  CloseFile(f);
end;
end;

function TTypeDefs.LoadDefFile(cbProc: TOnSymRead): boolean;
begin
  Result := DefFile <> '';
  if not Result then
    exit; //file must have been written before!!!
  OnSymRead := cbProc;
  self.LoadFromFile(DefFile);
  OnSymRead := nil;
end;

(* MacroFilter - classify macros after #define
  Returns the kind of the macro:
  skSym:    delete the macro, treat as symbol.
  skMacro:  install as macro (expand...)

Current implementation:
  If fAutoConst is True, convert all macros with a single numeric token
  into named constants.

ToDo:
  Better recognition of multi-token constants, i.e. (value) etc.
  Strings should be Ansi style -> loader?
*)
function TTypeDefs.MacroFilter(id: integer;
  newkind: eSymbolKind): eSymbolKind;
var
  sym:  TXStrings;  //TSymPrep;
  mac:  TMacro absolute sym;
  t:    PPreToken;
  iBody:  integer;
  CSym: TSymbolC;
  psym: PSymPrep;

  (* getToken - filter "white" tokens.
  *)
  function  getToken: PPreToken;
  begin
    repeat
      Result := mac.peekBody(iBody);  inc(iBody);
    until (Result = nil) or not (Result.kind in whiteTokens);
  end;

  function  checkToken: eToken;
  var
    t2:    PPreToken;
  begin
    t2 := getToken;
    if t2 = nil then
      Result := t_eof
    else
      Result := t2.kind;
  end;

begin //MacroFilter
  Result := newkind;  //default
  if not fAutoConst then
    exit;
  sym := Symbols.Members[id];
  if (sym = nil) or (mac.ClassType <> TMacro) then
    exit; //only convert macros without arguments
  iBody := 0; //start iteration
  t := getToken;
  if (t <> nil) and (checkToken = t_eof) then begin
  //effective macro body is a single literal token
    Result := skConst;  //assume literal
    CSym := nil;
  //??? are untyped constants '#' okay???
    case t.kind of
    t_str, t_car:
      begin
        ScanToken := t^;
        CSym := self.defConst(mac.name, '#', TokenString(False));
      end;
    t_int, t_Lint:
      begin //preserve format information
        if iaBase16 in t.attrs then begin
        //hex number
          CSym := self.defConst(mac.name, '#', HexPrefix + IntToHex(t.lval, 0));
        end else if iaBase8 in t.attrs then begin
          CSym := self.defConst(mac.name, '#', IntToOct(t.lval));
        end else begin
          CSym := self.defConst(mac.name, '#', IntToStr(t.lval));
        end;
      end;
    t_flt:  CSym := self.defConst(mac.name, '#', FloatToStr(t.d));
    t_sym:  //alias?
      begin
        if t.symID >= 0 then begin
          psym := Symbols.getSymbol(t.symID);
          case psym.mackind of
          skConst:  CSym := self.defConst(mac.name, '#', psym.FString);
          skType:   CSym := self.defType(mac.name, typeQuote + psym.FString + typeQuote, id);
          skProc:   CSym := self.defConst(mac.name, '#', psym.FString);
          end;
        end;
      end;
    else
      Result := newkind;
    end;
    if CSym <> nil then
      CSym.SetID(mac.symID);  //nameID
  end;
end;

procedure TTypeDefs.SaveToStream(Stream: TStream);

  procedure WriteLn(const s: string; fSplit: boolean);
  var
    s2: string;
  begin
    if fSplit and (Length(s) > 80) then begin
    //stupid IDE truncates long lines!
    //todo: this may split strings with embedded ','!!!
      s2 := StringReplace(s, ListTerm, ListTerm + EOLstr + #9, [rfReplaceAll]);
      Stream.Write(s2[1], Length(s2));
    end else
      Stream.Write(s[1], Length(s));
    Stream.Write(EOLstr, Length(EOLstr));
  end;

(*
Most important is the order of HLL symbol definitions!
  (names can be used before, as parameter names...)
The Globals table is dumped in order,
  macro definitions are inserted as required, from the Symbols table.
  [Some names have no associated objects: macro args, macros without body...]
*)
  procedure SaveAllSymbols;
  var
    i: integer;
    mac:  TSymMacro;
    csym: TSymbolC absolute mac;
    psym: PSymPrep;
    nextMac: integer; //to be shown
    s: string;
    //f: string; //filename, was: TFile;
    FileID: integer;

    procedure WriteBug(const id1, id2: string);
    const
      q: string = '?';
      t: string = #9;
    begin
      Stream.Write(q, 1);
      Stream.Write(id1, Length(id1));
      Stream.Write(t, 1);
      WriteLn(id2, Length(id2) > 1000);
    end;

    procedure WriteASym(sym: TSymbol; c: char; const l: string);
    begin
    //source file
      if sym.loc.valid then begin
        if sym.loc.id <> FileID then begin
          FileID := sym.loc.id; // f := sym.loc.src;
          //WriteLn('[' + f + ']', False);
          WriteLn('[' + sym.loc.name + ']', False);
        end;
      //id and line number
        s := c + IntToStr(sym.loc.line) + #9;
      end else begin
      //symbol is not in the current file?
        s := c + #9;
      end;
      Stream.Write(s[1], Length(s));
    //don't split unless really long!
      WriteLn(l, Length(l) > 1000);
    end;

    procedure WritePSym;
    begin
      if nextMac >= Symbols.Count then
      //problem - how???
        WriteBug(IntToHex(nextMac, 4), '???')
      else begin
        psym := Symbols.getSymbol(nextMac);
        case psym.mackind of
        skSym,  //should be a TSymbolC
        //skCKey,
        skConst, skType, skProc,
        skMacro: //suppress constants etc!
          if (psym.macstate <> msUndef) and (psym.FObject <> nil) then begin
          //problem: invalid psym.FObject!?
            if psym.FObject is TMacro then begin
              mac := psym.FMacro;
              WriteASym(mac, aSymbolKind[psym.mackind], mac.toString);
            end;
          end else if fDebug then begin
            if (psym.dirkind = 0) and (psym.pragkind = 0) then begin
              if psym.appkind <> t_empty then begin
              //write tokenstring?
              end else begin
                WriteBug(IntToHex(nextMac,4), aSymbolKind[psym.mackind] + psym.FString);
              end;
            end;
          end;
        end;
      end;
      inc(nextMac);
    end;

    procedure logBug;
    begin
    //error??? - should never occur
      WriteBug(IntToHex(i,4), '???');
    end;

    procedure WriteCSym(c: char; const l: string);
    var
      m: integer;
    begin
      try
      //check for macros to emit
        if csym = nil then begin
          //WriteLn(l + ': no symbol???', False);
          WriteBug('no symbol?', l);
          exit;
        end;
        m := csym.UniqueID; //???
        if m >= nextMac then begin
          while m > nextMac do
            WritePSym;  //exceptions?
          //nextMac := m;
          WritePSym;  //debug: non-macs listed?
        end;
      //std
        if csym.altID = 0 then
          Log('missing altID: ' + l, lkDebug);
        WriteASym(csym, c, l);
      except
        logBug;
      end;
    end;

  begin //SaveAllSymbols
    FileID := -1; // NoFile;
    nextMac := 0;
    for i := 0 to Count - 1 do begin
      try
        csym := getSym(i);
        if csym = nil then begin
          logBug;
        end else begin
          WriteCSym(aSymType[csym.kind], csym.toString);
        end;
      except
        logBug;
      end;
    end;
  //more macros?
    //while m > nextMac do
    for i := nextMac to Symbols.Count - 1 do
      WritePSym;
  end;

begin //Globals.SaveToStream
//version identifier
  //WriteLn('#1.0', False);
  WriteLn(StreamVersion, False);
  SaveAllSymbols;
end;

function TTypeDefs.getType(const AName: string): TSymType;
begin
  TSymbolC(Result) := getSym(AName);
  if (Result <> nil) and (Result.kind <> stTypedef) then
    Result := nil;
end;

function TTypeDefs.getType(index: integer): TSymType;
begin
  TObject(Result) := self.Members[index];
  if (Result <> nil) and (Result.kind <> stTypedef) then
    Result := nil;
end;

function TTypeDefs.defEnumMember(const AName, ADef,
  AVal: string): TSymbolC;
begin
  Result := defSym(stEnumMember, AName, ADef, AVal);
end;

function TTypeDefs.defProc(const AName, ADef: string): TSymProto;
begin
{$IF __proto}
//here: prototype
  Result := defSym(stProto, AName, ADef) as TSymProto;
{$ELSE}
  Result := defSym(stProc, AName, ADef) as TSymProc;
{$IFEND}
end;

function TTypeDefs.closestType(const ADef: string): string;
const
  BaseTypeChars = 'vcwsilLfdD';  //-+"';
  BaseTypeNames: array[1..10] of string = (
    'VOID', 'CHAR', 'WCHAR', 'SHORT', 'INT', 'LONG', 'LONGLONG',
    'FLOAT', 'DOUBLE', 'EXTENDED'
  );
var
  i:  integer;
  sym:  TTypeDef;
  post:  string;

  function  lookup(const ADef: string): boolean;
  var
    i: integer;
  begin
    for i := 0 to Count - 1 do begin
      sym := self.getType(i);
      Result := (sym <> nil) and (sym.Def = ADef);
      if Result then
        exit;
    end;
    Result := False;
  end;

begin //try find typename - synthesize if required?
//check for typeref
  if ADef[1] = TypeQuote then begin
    Result := copy(ADef, 2, Length(ADef)-2);
    exit;
  end;
//check for existing symbol
  if lookup(ADef) then begin
    Result := sym.name;
    exit;
  end;
//check for basetype
  i := Pos(ADef[1], BaseTypeChars);
  if i > 0 then begin
    Result := BaseTypeNames[i];
    exit;
  end;
//check modified
  case ADef[1] of
  '*':  Result := 'P';
  '#':  Result := 'C';
  'V':  Result := 'V';
  else  Result := ''; //fail!
  end;
  if Result <> '' then begin
    post := closestType(copy(ADef, 2, Length(ADef)-1));
    if post <> '' then begin
      Result := Result + post;
      exit;
    end;
  end;
//everything failed, synthesize
  Result := '__T' + IntToStr(Count);
//create new typedef - add to Symbols
  i := Symbols.Add(Result);
  sym := self.defType(Result, ADef, i);
end;

{ RType }

procedure RType.handleStorage;
begin
  self.storage := i_ttyp;
  nextToken;
end;

//constructor RType.Init;
procedure RType.Init;
begin
  self.Reset;
  self.spec := '';
  self.storage := DefaultStorage; // Kauto; //t_empty;
  self.signed := t_empty;
  self.sized := szNone;
  self.specToken := t_empty;
  self.attrs := [];
  self.basetype := nil;
  self.call := t_empty;
  self._inline := False;
end;

(* restart declarator parsing
*)
procedure RType.Reset;
begin
(* in Init:
    storage: eToken;
    signed: eToken;
    sized: eSized;
    attrs: ...
    name: string;
    basetype: TTypeDef;
  //self.attrs := [];
  //self.spec := ''; - remains defined
*)
  self.name := '';
  self.nameID := 0; //or -1?
  self.pre := '';
  self.post := '';
  self.declSym := nil;
  self.Value := '';
  if self.fTempScope then begin
    FreeAndNil(self.mbrScope);
    self.fTempScope := False;
  end else
    mbrScope := nil; //only clear reference
end;

procedure RType.type_specifier(i_ttyp: eToken; fDefault: boolean);
var
  s_typ0: string;
const
  SizedInt: array[eSized] of char = 'islL';
  aSized: array[Kint8_t..Kuint64_t] of string = (
    '-1', '-2', '-4', '-8',
    '+1', '+2', '+4', '+8'
  );
begin
//check spec already finished?
{$IFDEF new}
  if fDefault and (i_ttyp = Kint) then begin
    Log('default type', lkDebug);
  end;
{$ELSE}
{$ENDIF}
  if self.specToken <> t_empty then begin
    //assert(specToken = i_ttyp, 'redef base type');
    if specToken = i_ttyp then
      exit;
    Log('redef base type', lkTodo);
    //continue to debug
  end;
//make base type
  case i_ttyp of
  Kvoid:  s_typ0 := 'v';
  Kchar:
    case signed of
    Ksigned:    s_typ0 := '-c';
    Kunsigned:  s_typ0 := '+c';
    else        s_typ0 := 'c';
    end;
  t_sym,  //possibly modified typename?
  Kint:
    begin
      if i_ttyp = t_sym then begin
      //base type: typename (ref) or typedef of typename?
        if basetype = nil then
          basetype := Globals.getType(ScanText); //ScanSym;
        s_typ0 := quoteType(ScanText); //getType(...).Def; ?
        case sized of { TODO : is <typemod>"typename" really handled properly? }
        szNone: ;
        szShort:  s_typ0 := 's' + s_typ0;
        szLong:   s_typ0 := 'l' + s_typ0;
        else
          Log('questionable typename modifier', lkTodo);
        end;
      end else
        s_typ0 := SizedInt[sized];
      case signed of
      Kunsigned: s_typ0 := '+' + s_typ0;
      //Ksigned: ignore
      end;
    end;
  Kfloat: s_typ0 := 'f';
  Kdouble:
    case sized of
    szLong, szLongLong: s_typ0 := 'D';
    else s_typ0 := 'd';
    end;
  Kint8_t..Kint64_t:
    begin
      if signed = Kunsigned then
        inc(i_ttyp, ord(Kuint8_t) - ord(Kint8_t));
      s_typ0 := aSized[i_ttyp];
    end;
  Kuint8_t..Kuint64_t:
    s_typ0 := aSized[i_ttyp];
  else
    assert(False, 'unknown simple type');
    s_typ0 := '?';
  end;
  spec := spec + s_typ0;  //other attributes: const, volatile?
  self.specToken := i_ttyp; //flag finished
  if self.basetype = nil then begin
    //log('need dummy basetype', lkBug);
  end;
end;

function RType.todo: boolean;
begin
  Result := false;
end;

(* finishComplex - compile specification
  The member declarations, stored in .post, become part of the base type.
  For anonymous types the declaration is stored in .spec,
  for tagged types the declaration is stored in the explicit type (.basetype)
    and .spec becomes a reference to this type.
*)
procedure RType.finishComplex;
begin
(* struct/union/enum becomes spec!
  either t:{fields} //anonymous
  or "t:tag"  //tagged
*)
  post := '{' + post + '}';
  if self.basetype <> nil then begin
  //define already created (tagged) type
    basetype.Def := post;
    //spec := quoteType(basetype.Name); - already done on basetype creation
    if self.specToken = Kenum then
      finishEnum;
  end else begin
  //anonymous?
    spec := spec + post;
  end;
  post := '';
end;

(* makeTagRef - remember basetype
  Format: <t> [":"<name>] [add later: "{"<def>"}"]
Called from parser.handleTag
*)
procedure RType.makeTagRef(sue: eKey; const tag: string);
begin
  self.specToken := sue;
  case sue of
  Kenum:    spec := 'E';
  Kstruct:  spec := 'S';
  Kunion:   spec := 'U';
  else
    assert(False, 'bad basetype');
  end;
  if tag <> '' then begin
    spec := spec + ':' + tag;
    basetype := Globals.defType(spec, '', nameID); //typename without quotes
    if not loc.valid then begin
    //default location?
      loc := ScanSym.loc;
      //pSrc.??? ScanningFile???
      loc.line := ScanningLine;
    end;
    basetype.loc := loc;
    spec := quoteType(spec);  //for further refs: quoted
  end else //debug only?
    basetype := nil;  //missing???
end;

procedure RType.makePointer;
begin
  pre := '*' + pre;
end;

(* qualify - remember const/volatile and function qualifiers
  Const and volatile qualifiers are added in front of previous description strings.
  Specifier attributes are added to .spec,
  declarator attributes are added to .pre.

Calling conventions (and more?) can occur both inside an specifier
  or an (possibly nested!) declarator.
  At the end of an nested declarator all components must be copied
  into the outer declaration, and processed there again. This means,
  in the case of calling conventions, that the spec string must be updated.
It seems as if calling conventions can be overriden in VC.
*)
function  RType.qualify(t: eKey; fSpec: boolean): boolean;
begin
(* position dependence of "const":
  const [int] c = val //constant var
  const char  * p  //ptr to const string
  char const  * p  //ptr to const (same)
  char  * const p  //const ptr to string - applies to declarator, not spec!
    (the string can be modified, but not the pointer)
  const char  * const p  //const ptr to const string
*)
  Result := True;
  if t in calling_conventionS then begin
    if call = t then
      exit; //already done
    Result := self.call = t_empty;
    if Result then
      call := t  //no checks, no string modifications
    else
      Log('different calling conventions', lkDebug);
  end else if fSpec then begin //assume spec!
    if t in attrs then
      exit; //prevent multiple insertion into the spec string.
    include(attrs, t);
    case t of
    Kconst:     spec := '#' + spec;
    KRestrict:  spec := 'R' + spec;
    Kvolatile:  spec := 'V' + spec;
    end;
  end else begin
    //include(declattrs, t);  //not correct, as long as only 1 set of attrs
    case t of
    Kconst:    pre := '#' + pre;
    KRestrict: pre := 'R' + pre;
    Kvolatile: pre := 'V' + pre;
    else  //what other qualifiers can occur?
      Log('unexpected declarator qualifier', lkTodo);
      Result := False;
    end;
  end;
end;

(* endDecl - end declarator, before initializer list!
Create named object (type, proc, const, var) in the current scope.
Called at end of declarator(), before optional initializers.

For now only public and non-public scopes are distinguished.
  Objects only are created in public scope.
A full blown parser (for compound statements)
  also should create objects in the local (block) scope,
  and also should distinguish "external" from "static" scope.
In C++ a visibility attribute may be used for private, protected and public
  visibility of the created objects.

Scope types:
fix
  TypeScope (global)
  External  (global)  all globals, re-parent symbols into module scopes?
    (declarations)
init dynamic: module
  Module  interface (non-static definitions!)
  Module  static <- needs static initialization!
temporary
  Proc...
  Qualified (struct...)
*)
procedure RType.endDecl;
var
  symkind: eSymType;
  scope:  TScope;
  typ:  TTypeDef;
begin
(* What about parameters, SUE members...?
  We assume that we are called ONLY IF a symbol MUST be created!
  Then declScope must be a valid scope (possibly a local one).
  Parameters and SUE members are created otherwise!
*)
(* The default (top level) scope is Globals?
  C: only "static" is not exported!
*)
  if (self.name = '') or (declSym <> nil) then
    exit; //nothing to do
  scope := declScope;
  case storage of
  Kextern:  scope := Globals; //never create in a local scope!
  Kstatic: //ambiguous!
    if (scope = Globals) and (Statics <> nil) then
      scope := Statics;  //what if nil? (header translation???)
  Ktypedef:
  {$IF __delayTags}
    begin //try substitute synthetic name of basetype
      //expect: spec=t{mbrs}, or t:name{mbrs} was already created!
      if basetype = nil then begin
        if (specToken in [Kenum, Kstruct, Kunion])
        and (spec[2] = '{') then begin
        //untagged struct
          //if (pre = '') and (post = '') then begin
          if True then begin  //always create t:name
          //direct reference to struct - use as tagname
            typ := Globals.defType(spec[1] + ':' + name, Copy(spec, 2, Length(spec)), 0);
            typ.loc := self.loc;
            basetype := typ;
            spec := quoteType(basetype.name);
            //basetype.symID? - stay zero?
          //finish enums? - problem: mbrScope cleared before decl spec!
            if spectoken = Kenum then begin
              finishEnum;
            end;
          end else begin
          //requires synthetic name
          end;
        end else if (spec = '') then begin
        //more cases!!!
          self.type_specifier(Kint, True);  //???
        end else if self.specToken = t_empty then begin
          log('spec?', lkDebug);
          self.type_specifier(Kint, True);  //???
        end else if getDef = '' then
          Log('unknown basetype', lkBug);
        //else assume valid spec???
      end;
      declSym := Globals.defType(name, getDef, nameID);
      declSym.loc := self.loc;
      declSym.BaseType := self.basetype;
    end;
  {$ELSE}
    begin
      declSym := Globals.defType(name, getDef, nameID);
      declSym.loc := self.loc;
    end;
  {$IFEND}
  end;  //case
  if (scope = nil) or (declSym <> nil) then
    exit; //assume nothing to create, or done

//now create a polymorphic symbol (very hidden in code)
//detect procedures how???
  if (post <> '') and (post[1] = '(') then
    symkind := stProc
  else if Kconst in attrs then
    symkind := stConst
  else
    symkind := stVar;
  declSym := Scope.defSym(symkind, name, getDef, Value); //polymorphic create
  if self.loc.valid then
    declSym.loc := self.loc;
//no type symbols here, only proc, const, var (global, local, param)
  if (scope = Globals) and (storage <> Kextern) then
  //symbol defined, not only declared?
    Statics.AddObject(name, declSym);

//link to prep. Symbol
  if declSym.altID = 0 then begin
    if scope = Globals then begin
    //bidirectional link
      declSym.SetID(nameID);
    end else //else handle static symbol?
      declSym.altID := nameID;  //NOT back from Symbols->static sym
  end;
//more common cleanup?
end;

procedure RType.makeDim(const dim: string);
begin
//what about expressions with named constants?
  post := post + '[' + dim + ']';
end;

(* makeParams - finish parameter list declaration
Should flag the declaration as procedure declaration!
But also should distinguish procedures from procedure pointers!

Since calling conventions can be overridden in VC, they are finished here!
__inline can be combined with other calling conventions!
*)
{$IF __lclScopes}
//procedure RType.makeParams;
procedure RType.makeParams(const params: string);
var
  i: integer;
  sym: TSymbolC;
  proc: TSymProc;
begin
  if (mbrScope <> nil) and (mbrScope.Count > 0) then begin
  //use local scope
    post := post + '('; // + params + ')';
    for i := 0 to mbrScope.Count - 1 do begin
      sym := mbrScope.getSym(i);
      post := post + sym.TypedName + ListTerm;
    end;
    post := post {+ '(' + params} + ')';
  end else
    post := post + '(' + params + ')';
//handle final calling convention (more?)
  //if self.storage = Kinline then
  if self._inline then
    post := post + 'I'; //hint (only)
  if self.call <> t_empty then begin
    case call of  //attach to spec or post???
    Kcdecl:     post := post + 'C';
    Kfastcall:  post := post + 'F';
    //Kinline:    post := post + 'I'; //treat as storage class!
    //Kstdcall: - not flagged
    end;
  end;
//definitely flag declarator as proc/func/pointer?
  if self.specToken = t_empty then
    self.type_specifier(Kint, True);
//handle local scope?
  if (name <> '') and (declsym = nil) then begin
  //declaration or definition{...}?
    if (self.storage <> Ktypedef) and (i_ttyp <> opBeg) then begin
      storage := Kextern;
      FreeAndNil(mbrScope); //not stored for external procs!
    end;
    self.endDecl; //should create proc sym
    if (declSym <> nil) and (mbrScope <> nil)
    and (declSym is TSymProc) then begin  //exclude typedefs?
    //old style declaration/definition - mbrScope typically temporary
      proc := declSym as TSymProc;
      //proc.Params := mbrScope;
      mbrScope.fParentScope := declScope;
      proc.takeParams(mbrScope);
      assert(declScope <> nil, 'proc without scope');
    end;
  end;
end;

function  RType.makeParam(const t: RType): string;
var
  sym: TSymbolC;
begin
(* Parameter lists always are parsed from first to last parameter!?
  Syntax: (<name>:<type>, <name>:<type>, ...)
*)
(* Two problems:
  "void" - indicates *empty* parameter list!
  anonymous parameters - assume external only, ignore!
*)
  //self.symkind := stParam; proc or typedef!!!
  if mbrScope <> nil then begin
  //assume old style declaration
    if t.name = '' then
      //Log('anonymous parameter', lkTodo)
    else begin
      sym := mbrScope.defSym(stVar, t.name, t.getDef);
      sym.loc := t.loc;
    end;
  end;
  Result := t.name + ':' + t.getDef + ListTerm;
end;

function RType.makeVararg: string;
begin
  if mbrScope <> nil then
    mbrScope.defSym(stVar, '', '~');
  Result := ':~' + ListTerm;
end;

procedure RType.makeScope;
begin
  if mbrScope = nil then begin
    mbrScope := TParamScope.Create('params'); //or enum elements?
    //name: proc name?
    self.fTempScope := True;
  end;
end;

(* makeEnumMember
Problem: no enum type specified here!
  All member symbols should refer to their basetype!
  Require values, for analysis purposes.
*)
function RType.makeEnumMember(const t: RType): TSymbolC;
begin
  self.post := post + t.name;
  if t.Value <> '' then
    self.post := post + '=' + t.Value;
  self.post := post + ListTerm;
//create in Globals, copy into member scope
  Result := Globals.defEnumMember(t.name, t.spec, t.Value); //for lookup in further definitions!
  Result.loc := t.loc;
  if mbrScope <> nil then
    mbrScope.AddObject(t.name, Result);
//handle ID
  if t.nameID = 0 then
    Log('no nameID', lkBug)
  else begin
    //Result.SetID(id);
    Result.altID := t.nameID; //local, no backref!???
  end;
end;

(* finishEnum - base and value for all members
Call whenever the basetype has been created (endDecl or ...?)
*)
procedure RType.finishEnum;
var
  i: integer;
  sym: TSymbolC;
  n: string;
begin
  if basetype = nil then
    exit; //must have been created, for completeness
  n := '';
  for i := 0 to enumScope.Count - 1 do begin
    sym := enumScope.getSym(i);
    sym.BaseType := basetype;
    if sym.StrVal = '' then begin
      if n = '' then
        sym.StrVal := '0'
      else
        sym.StrVal := '+(' + n + ',1)';  //syntax??? exprSep...
    end;
    n := sym.name;
  end;
//flag done
  enumScope.Clear;
end;

function RType.makeStructMember(const t: RType): TSymbolC;
begin
  post := post + t.name + ':';
  if t.Value <> '' then
    post := post + BitFieldStart + t.Value + BitFieldEnd;
  post := post + t.getDef + ListTerm;
  if mbrScope = nil then
    Result := nil //strings only
  else begin
    Result := mbrScope.defSym(stVar, t.name, getDef);
    Result.loc := t.loc;
  end;
end;

{$ELSE} //no __lclScopes
procedure RType.makeParams(const params: string);
begin
  post := post + '(' + params + ')';
//handle final calling convention (more?)
  //if self.storage = Kinline then
  if self._inline then
    post := post + 'I'; //hint (only)
  if self.call <> t_empty then begin
    case call of  //attach to spec or post???
    Kcdecl:     post := post + 'C';
    Kfastcall:  post := post + 'F';
    //Kinline:    post := post + 'I'; //treat as storage class!
    //Kstdcall: - not flagged
    end;
  end;
//definitely flag declarator as proc/func/pointer?
end;

function RType.makeParam(const Aname, Adef: string): string;
begin
{ TODO : handle old style (name-only) parameters? }
(* Parameter lists always are parsed from first to last parameter!?
  Syntax: (<name>:<type>, <name>:<type>, ...)
*)
  Result := AName + ':' + Adef + ListTerm;
  //if Aname <> '' then    Result := Aname + Result;
end;

function RType.makeVarargs: string;
begin
  Result := makeParam('', '~');
end;
{$IFEND}


function RType.getDef: string;
begin
if false then
  if self.specToken = t_empty then begin
  //make int type by default - really??? (could stay 'const'?)
    //specToken := Kconst; ???
    type_specifier(Kint, True);
  end;
//else type stays undefined - check explicitly for (global) procs!

  Result := self.post + self.pre + self.spec;
if False then //better, but should not be required any more
  if Result = '' then begin
    type_specifier(Kint, True);
    Result := self.post + self.pre + self.spec;
  end;
(* static - storage duration or linkage?
  Here we only flag the existence of "static".
  The applicable scope (linkage) is only affected by "extern".
  Storage duration must be handled explicitly when required,
    based on the actual scope (if neither global nor module).
*)
  if self.storage = Kstatic then begin
    Result := StaticScope + Result;
  end;
end;

procedure RType.propagateName(const t: RType);
begin
  self.name := t.name;
  self.nameID := t.nameID;
  self.loc := t.loc;
end;

procedure RType.setNameSym;
begin
  self.name := ScanText;
  self.nameID := ScanToken.symID;
  self.loc := ScanSym.loc;
end;

{ TTypeDef }

constructor TTypeDef.Create(const AName: string; AKey: integer);
begin
  inherited;
  self.kind := stTypedef;
  //org...
end;

function TTypeDef.GetCaption: string;
begin
//okay for caption, but not for toString!
  //Result := self.Name + ' = ' + self.Def;
  Result := self.Name + '=' + self.Def;
end;

function TTypeDef.Ref: string;
begin
  Result := GetName;
  if Result[2] = ':' then begin
  //anonymous SUE type? or what?
    Result := QuoteType(Result);
  end;
end;

{ TSymbolC }

destructor TSymbolC.Destroy;
begin
  if (altID > 0) and not fFinalizing then begin
  //remove from Symbols!!!
    if Symbols.Objects[altID] = self then
      Symbols.Objects[altID] := nil;
  end;
  inherited;
end;

function TSymbolC.GlobalID: integer;
begin
  Result := symID;  //if global (>0): in Globals table
end;

function TSymbolC.UniqueID: integer;
begin
  Result := self.altID; //in preprocessor table
end;

function TSymbolC.GetCaption: string;
begin
  Result := name;
  if self.StrVal <> '' then
    Result := Result + ' = ' + StrVal;
end;

{$IF __ParseTree}
function TSymbolC.GetTokens: TTokenArray;
begin
  Result := Definition;
end;
{$IFEND}

(* SetID - remember scanner/preprocessor symID
This is an approach to create a common public STB.
Every C symbol object is added to the symbol list,
  provided that no macro of the same name exists.
*)
function TSymbolC.SetID(nameID: integer): integer;
begin
  self.altID := nameID;
//todo: check for public/private symbol?
//assume: nameID > 0 means public symbol?
  if nameID > 0 then begin
    if Symbols.getSymbol(nameID).FObject = nil then begin
      Symbols.Objects[nameID] := self;
    end;
  end;
  Result := nameID; //or what?
end;

function TSymbolC.toString: string;
begin
  Result := BodyString;
  if Result = '' then begin
  //ToDo: BodyString should be overridden!
    if (self.Def = '') and (self.StrVal = '') then begin
      if (basetype <> nil) and (self.kind = stEnumMember) then
        Result := name + ' in ' + self.BaseType.name
      else
        Result := name + ' = what?';  //should be overridden in derived class!
    end else begin
      //Result := name + ':' + Def + ' = ' + StrVal;  //default
      Result := name;
      if Def <> '' then begin
        if self.kind = stTypedef then begin
          Result := Result + '=' + Def;
          if StrVal <> '' then
            log('type=val?', lkBug);
        end else
          Result := Result + ':' + Def;
      end;
      if StrVal <> '' then
        Result := Result + '=' + StrVal;  //default
    end;
  end else
    Result := name + Result;
end;

function TSymbolC.TypedName: string;
begin
  Result := name;
  if (Def <> '') {and (Def <> '#')} then begin
    { TODO : parameter names with/out $1 scope? }
    Result := Result + ':' + Def
  end else if BaseType <> nil then
    Result := Result + ':' + BaseType.Ref;
end;

(* UniqueName - add dupe count postfix
Other possible purposes:
  prefix/postfix identifiers for type/const/var/proc, depending on .kind.
  (Must be synchronized with the determination of DupeCount!)
*)
function TSymbolC.UniqueName: string;
begin
  Result := Name;
  if self.DupeCount > 0 then
    Result := Result + '_' + IntToStr(DupeCount);
  if (TypePrefix <> '') and (kind = stTypedef) then
    Result := TypePrefix + Result;
end;

function TSymbolC.GetName: string;
begin
(* modes:
  normal: name (possibly including $scope)
  meta: name + $scope
  target: name [-$scope + _dupe]?
*)
  if fMetaNames then
    Result := MetaName
  else
    Result := inherited GetName;
end;

function TSymbolC.MetaName: string;
begin
  Result := inherited GetName;
    //which parent table???
  if self.ScopeNum > 0 then
    Result := Result + '$' + IntToStr(ScopeNum);
end;

{ TSymVar }

function TSymVar.BodyString: string;
begin
//Result := [name +] ':' + Def + ' = ' + StrVal;  //default
  Result := '';
  if Def <> '' then
    Result := Result + ':' + Def; //as in typed name!
  if StrVal <> '' then
    Result := Result + '=' + StrVal;  //default
end;

function TSymVar.GetCaption: string;
begin
  Result := self.TypedName;
  if self.StrVal <> '' then
    Result := Result + '=' + StrVal
  //never set?
  else if {(IntVal <> 0) or} (self.kind = stConst) then
    Result := Result + '=0';  //always must have value?
end;


{ TModuleList }

function TModuleList.AddModule(const fn: string): TModule;
var
  i: integer;
  s: string;
begin
  s := ExtractFileName(fn);
  s := ChangeFileExt(s, '');
  i := IndexOf(s);
  if i >= 0 then
    TObject(Result) := Objects[i]
  else begin
    Result := TModule.CreateIn(self, s);
    Result.fParentScope := Globals;
    Result.Source := uFiles.GetFile(fn);
  end;
end;

procedure TModuleList.Save;
var
  i: integer;
  m: TModule;
begin
  for i := 0 to Count - 1 do begin
    m := Objects[i] as TModule;
    m.Save;
  end;
end;

{ TModule }

procedure TModule.Save;
begin
  SaveToFile(self.fName + '.txt');  //better extension?
end;

procedure TModule.SaveToStream(Stream: TStream);
//procedure TTypeDefs.SaveToStream(Stream: TStream);
(* Only save HL-symbols!
*)
const
  fPrepSyms = False;  //no save preprocessor symbols
var
  symf: integer; // string; //filename TFile;

  procedure WriteLn(const s: string; fSplit: boolean);
  var
    s2: string;
  begin
    if fSplit and (Length(s) > 80) then begin
    //stupid IDE truncates long lines!
    //todo: this may split strings with embedded ','!!!
      s2 := StringReplace(s, ListTerm, ListTerm + EOLstr + #9, [rfReplaceAll]);
      Stream.Write(s2[1], Length(s2));
    end else
      Stream.Write(s[1], Length(s));
    Stream.Write(EOLstr, Length(EOLstr));
  end;

  procedure SaveDefines;
  var
    i: integer;
    sym:  PSymPrep;
  begin
    for i := 0 to Symbols.Count - 1 do begin
      sym := Symbols.getSymbol(i);
      if (sym.mackind = skMacro) then begin  //suppress constants etc!
        if sym.FObject <> nil then begin
          WriteLn('#define'#9 + sym.FMacro.toString, False);
        end else if fDebug then begin
        //WriteLn('//#define'#9 + TMacro(sym.FMember).name + '???', False);
          WriteLn('//#define'#9 + sym.FString + '???', False);
        end;
      end;
    end;
  end;


(* We assume that all names are stored in the preprocessor Symbols table.
  Some names have no associated objects (macro args, macros without body...).
  Every symbol object has two IDs:
    symID index in the parent table (IndexOf)
    altID index in the Globals table (=symID for C symbols)???
      or index in the OTHER table?
Most important is the order of HLL symbol definitions!
  (names can be used before, as parameter names...)
-> The Globals table is dumped in order,
    macros are inserted as required (at least up to the UniqueID of the name)
*)
  procedure SaveAllSymbols;
  var
    i: integer;
    csym: TSymbolC;
    psym: PSymPrep;
    nextMac: integer; //to be shown
    s: string;

    procedure WriteASym(sym: TSymbol; c: char; const l: string);
    begin
      //s := s + IntToHex(sym.GlobalID,4) + #9 + c;
      if sym.loc.id = symf then begin
        s := c + IntToStr(sym.loc.line) + #9;
      end else
        s := c + #9;
      Stream.Write(s[1], Length(s));
    //don't split unless really long!
      WriteLn(l, False);  //assume bodies are formatted!
    end;

    procedure WritePSym;
    begin
      psym := Symbols.getSymbol(nextMac);
      case psym.mackind of
      skSym,  //should be a TSymbolC
      //skCKey,
      skConst, skType, skProc,
      skMacro: //suppress constants etc!
        if psym.FObject <> nil then begin
          if psym.FObject is TMacro then
            WriteASym(psym.FMacro, aSymbolKind[psym.mackind], psym.FMacro.toString);
        end else if fDebug then begin
          if (psym.dirkind = 0) and (psym.pragkind = 0) then begin
            if psym.appkind <> t_empty then begin
            //write tokenstring?
            end else begin
              s := IntToHex(nextMac,4) + #9 + #9
                + aSymbolKind[psym.mackind] + psym.FString;
              WriteLn(s, Length(s) > 1000);
            end;
          end;
        end;
      end;
      inc(nextMac);
    end;

    procedure WriteCSym(c: char; const l: string);
    var
      m: integer;
    begin
      if fPrepSyms then begin
      //check for macros to emit
        m := csym.UniqueID;
        while m > nextMac do
          WritePSym;
      end;
    //std
      if csym.altID = 0 then
        Log('missing altID: ' + l, lkDebug);
      WriteASym(csym, c, l);
    end;

  begin //TModule.SaveAllSymbols
    nextMac := 0;
    for i := 0 to Count - 1 do begin
      try
        csym := getSym(i);
        if csym = nil then begin
        //error??? - should never occur
          //s := #9 + IntToHex(i,4) + #9 + '???';
          s := '???' + #9 + IntToHex(i,4) + '???';;
          WriteLn(s, False);
        end else begin
          WriteCSym(aSymType[csym.kind], csym.toString); //really .Caption???
        end;
      except
        WriteLn('???exception???'+#9+Strings[i], False);
        {csym is an invalid reference - how that???
      //debug only!!!
        csym := getSym(i);
        WriteCSym(aSymType[csym.kind], csym.toString); //really .Caption???
        }
      end;
    end;
  //more macros?
    if fPrepSyms then begin
      //while m > nextMac do
      for i := nextMac to Symbols.Count - 1 do
        WritePSym;
    end;
  end;

begin //TModule.SaveToStream
//problem: header conversion -> needs globals
  if Count = 0 then
    Globals.SaveToStream(Stream)
  else begin
  //version identifier
    symf := self.Source.id;
    WriteLn(StreamVersion, False);
    WriteLn('[' + Files.Strings[symf] + ']', False);
    SaveAllSymbols;
  end;
end;

{ TSymProc }

destructor TSymProc.Destroy;
begin
  FreeAndNil(FBody);  //linked to Params!
  FreeAndNil(Params);
{$IFDEF ProcTokens}
  FreeAndNil(Tokens);
{$ELSE}
{$ENDIF}
  inherited;
end;

procedure TSymProc.takeParams(var AScope: TScope);
begin
  if AScope = nil then
    exit; //nothing to do???
  if (Params <> nil) and (Params <> AScope) then
    Params.Free;
  Params := AScope;
  AScope := nil;  //taken!
end;

function TSymProc.getBody: TSymBlock;
var
  p: TXStrings;
begin
  p := nil; //p := self.Scopes
  if FBody = nil then begin
    if Params = nil then
      Params := TScope.CreateScope(p, self.owner as TScope);
    FBody := TSymBlock.CreateBlock(p, Params); //'{...}');
  end;
  Result := FBody;
end;

function TSymProc.BodyString: string;
begin
//bug - proc instead of proc-var!
  if FBody <> nil then
    Result := FBody.toString;
  //else  //";" should be reserved for wrapped lines!
end;

function TSymProc.toString: string;
begin
  Result := TypedName + BodyString;
end;

function TSymProc.hasBody: boolean;
begin
  Result := fBody <> nil;
end;

procedure TSymProc.checkLocals;
begin
(* purpose: properly unify local symbols
*)
{ TODO : check locals - what exactly? }
end;

function TSymProc.GetCaption: string;
begin
  if FBody = nil then
    Result := name
  else
    Result := MetaName;
end;

{ TSymBlock }

constructor TSymBlock.CreateBlock(AOwner: TXStrings; AScope: TScope);
begin
//based on TStringList!
  inherited Create;
  Scopes := TScopeList.Create;
//add params and parent scope of params
  if AScope <> nil then
    Scopes.Add(AScope.fParentScope)
  else
    Scopes.Add(nil);  //(AOwner); //???
  Scopes.Add(AScope); //assume: Params
  //CurScope := TLocalScope.CreateScope(AOwner, AScope);  //owner???
  CurScope := AScope; //parent of scopes to come
  ScopeNum := 1;  //1=parameters
end;

destructor TSymBlock.Destroy;
begin
  CurScope := nil;
  FreeAndNil(Scopes);
  inherited;
end;

function TSymBlock.Add(const s: string): integer;
begin
  Result := inherited Add(StringOfChar(#9, ScopeLvl) + s);
end;

(* addBlock/leaveBlock
enterBlock
  addStmt(s) if <> ''
  s := '{' [+ local variables to come] ? (see below)
  clear and return scope
addLcls
  as defined in scope
addStmt
  get locals, if any (on first stmt after declarations)
  add(s)
leaveBlock
  addStmt(s) if <> ''
  s := '}' [+ stmt end to come]
  clear and return scope
*)
(* enclose blocks:
...{( <- "(" really required?
...
};
*)
(* A Scope-Tree has to be maintained! (and searched!)
*)
function TSymBlock.enterBlock(var s: string): TSymBlock;
var
  newScope: TLocalScope;
begin
//!!!s applies to outer scope (->auto-vars!!!)
  add(s + '{(');
  s := '';
  inc(ScopeLvl);
  //CurScope := Scopes.getScope(ScopeNum);
  newScope := TLocalScope.CreateScope(nil, CurScope);
  ScopeNum := Scopes.Add(newScope);
  newScope.ScopeNum := ScopeNum;
  CurScope := newScope;
  Result := self; //somewhat obsolete...
end;

procedure TSymBlock.addLcls;
var
  i: integer;
  l: string;
  sym:  TSymbolC;
begin
  for i := 0 to CurScope.Count - 1 do begin
    sym := CurScope.getSym(i);
    if sym.Def[1] = StaticScope then
      l := 'static('
    else begin
      case sym.kind of
      stConst:  l := 'const(';  // + sym.toString + ');' + s;
      stVar:    l := 'auto(';
      stProc:   l := 'extern(';
      else
        LogBug('unhandled local');
        l := '???(';
      end;
    end;
    //l := l + '__' + IntToStr(ScopeNum) + '_' + sym.toString + ');';
    //sym.DupeCount := ScopeNum;  //for local symbols - special type!!!???
    l := l + sym.toString + ');';
    add(l);
  end;
end;

procedure TSymBlock.addStmt(var s: string);
begin
  if s <> '' then begin
    Add(s);
    s := '';
  end;
end;

function TSymBlock.leaveBlock(var s: string): TSymBlock;
begin
  if s <> '' then
    add(s); //???
  s := '};'; //block end flag, + stmt end to come
  CurScope := CurScope.ParentScope;
  dec(ScopeLvl);
  Result := self;  //or: return Scope.parentScope?
end;

function TSymBlock.toString: string;
begin
//better save to stream?
  Result := self.Text + ListTerm;  //term for safety!
  //streamed block is guaranteed to end with ";" [ListTerm]
end;

{ TLocalScope }

function TLocalScope.defSym(AKind: CreatableSyms; const AName,
  ADef: string; const AVal: TSymVal): TSymbolC;
begin
//modify into local symbols
  Result := inherited defSym(AKind, AName, ADef, AVal);
  Result.ScopeNum := ScopeNum;
end;

{ TParamScope }

constructor TParamScope.Create(const AName: string; AKey: integer);
begin
  inherited;
  self.ScopeNum := 1;
end;

{ TScopeList }

destructor TScopeList.Destroy;
var
  i: integer;
begin
//don't destroy scopes #0 (parent) and #1 (params)?
  for i := 2 to Count - 1 do
    getScope(i).Free;
  inherited;
end;

function TScopeList.getScope(index: integer): TScope;
begin
  Pointer(Result) := self.Get(index);
//no auto-create!
  if Result = nil then
    Log('bad scope number', lkBug);
end;

initialization
  InitSymTypes;
  Globals := TGlobalScope.Create('Globals');
  Modules := TModuleList.Create('Modules');
  if fAutoConst then
    uTablesPrep.MacroFilter := Globals.MacroFilter;
finalization
  fFinalizing := True;
  FreeAndNil(Modules);
  Globals.Free;
end.
