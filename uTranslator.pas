unit uTranslator;

interface

uses
  Classes,
  uXStrings,
  uTokenC,
  uScanC, //TFileC...
  uTablesC;

type
//general procedures and interface
  TTranslator = class
  private
  protected //output interface
    dst:  TStrings;
    outbuf:   string;
    indent:   integer;
    fOwnDst:  boolean;
    fNoShow:  boolean;
    procedure SetTarget(Strings: TStrings);
    function  UniqueName(const s: string): string;
    procedure Write(const s: string);
    procedure WriteSep(const s: string);
    procedure WriteLn(const s: string);
    procedure Outdent(const s: string);
  protected //source
    Scope:  TScope; //TListSymC;  //frame (module, Globals...)
    //sym:  TSymbolC; //from Scanner, on t_sym
    procedure InitSrc; virtual;
      //creates default dst, if nil
    procedure CheckNames(filename: string);
      //currently: according to global flags below (suitable for Pascal)
  protected //parsing meta data
    CurDef: string;
    function  ScanDef(const def: string): PChar;
  public
    ModuleName: string;
    ProjectDir: string;
    procedure ForHeader(const fn: string);
    procedure ForScope(AScope: TScope);
    procedure ForModule(mf: TModule);
    destructor  Destroy; override;
      //handle owned dst
    property  Target: TStrings read dst write SetTarget;
    function  Translate: boolean; virtual;
    function  TranslateSym(ASym: TSymbolC): boolean; virtual;
  end;

//meta text scanner - singleton
  TFileMeta = class(TFileC)
  public
    procedure SetSource(const ASrc: string);
    function  ScanPChar(Asrc: PChar): eToken;
  end;

  TDupeList = class(TXStringList)
  public
    constructor Create(const AName: string; AKey: integer = 0); override;
    function  AddKey(const s: string): integer;
      //define language keywords
    function  AddSym(sym: TSymbolC): integer;
      //define identifier, assign sym.dupecount
    function  UniqueName(const s: string): string;
  end;


var //singletons
  AllSyms:  TDupeList;  //for current project (source language)
  Translator: TTranslator;  //switch language???
  Scanner:  TFileMeta;

const //config
  fDebugEnums = False;
  fNameMembers: boolean = True;
  fEmptyCall = True;  //False; //show empty argument list?
//for Pascal
  fUniqueNames: boolean = True;
  fCasedNames:  boolean = False;

implementation

uses
  SysUtils,
  uUI,
  uTablesPrep;

{ TDupeList }

function TDupeList.AddKey(const s: string): integer;
begin
  Result := FList.Add(s, False);  //no dupes here
end;

function TDupeList.AddSym(sym: TSymbolC): integer;
var
  s: string;
  cnt:  byte;
  i: integer absolute Result;
begin
  //sym.DupeCount := 0;
  s := sym.UniqueName;  //possibly modified with type prefix...
  cnt := 0;
  i := -1;
  while True do begin
    i := FList.FindNext(s, i);
    if i < 0 then
      break; //no more occurences
    if Items[i].FObject = sym then begin
    (* non-static symbols occur in multiple scopes,
      but must be added only once to the dupe list.
    *)
      //Log('dupe: ' + s, lkTrace);
      exit; //is already stored
    end;
    inc(cnt);
  end;
  Result := self.FList.Push(s); //FList.Add(s, True); //always add
  Items[Result].FObject := sym;
  sym.DupeCount := cnt;
end;

constructor TDupeList.Create;
begin
  inherited;
(* When the source language is case insensitive,
  only one occurence of an name must be stored.

  Problem: how to handle clashes with predefined names of the *target* language?
  Adding such an name would result in just finding this one...
  -> Entries must distinguish between source and target symbols!
*)
  FList.fCaseInsensitive := not fCasedNames;  // True;
end;

function TDupeList.UniqueName(const s: string): string;
var
  id, cnt: integer;
  sym:  TSymbolC;
begin
  cnt := 0;
  id := -1;
  while True do begin
    id := FList.FindNext(s, id);
    if id < 0 then
      break;
    if Items[id].FString = s then begin
      TObject(sym) := Items[id].FObject;
      if sym <> nil then begin
        Result := sym.UniqueName;
        exit;
      end;  //else keyword
    end;
    inc(cnt);
  end;
//not found, create synthetic name
{$IFDEF old}
  FList.Add(s, True); //remember for later occurences!
{$ELSE}
  //do NOT increment dupecount on every occurence!
{$ENDIF}
  if cnt > 0 then
    Result := s + '_' + IntToStr(cnt)
  else
    Result := s;
end;

{ TFileMeta }

(* ScanPChar - scan intermediate text
Problem:
  When meta strings can contain #0, this can terminate the scan prematurely!
  Meta strings must be properly delimited.
  What about quote chars inside meta strings?
Solution:
  Strings are stored in C format, with escape sequences for
    all special (control...) chars
    Unicode
*)
function TFileMeta.ScanPChar(Asrc: PChar): eToken;
//function TFileC.ScanPChar(Asrc: PChar; Amode: eSrcMode): eToken;
begin
//debug
  assert(Asrc <> nil, 'bad pointer');
  assert((Asrc >= PChar(LineBuf)) and (Asrc < PChar(LineBuf) + Length(LineBuf)),
    'pointer outside buffer');
//init
  rewind;
  pcN := Asrc;
  Result := self._nextRaw(False, smStd);
  i_ttyp := Result;
end;

(* SetSource - set source string
*)
procedure TFileMeta.SetSource(const ASrc: string);
begin
  LineBuf := Asrc;
  self.mode := smMeta;
  //Result := self.ScanPChar(PChar(Asrc));
  rewind;
  pcN := PChar(Asrc);
end;

{ TTranslator }

destructor TTranslator.Destroy;
begin
  SetTarget(nil);
  inherited;
end;

(* CheckNames - name clashes with keywords or case insensitive duplicates

A list of all identifers is created, and all names are added to this list,
  counting dupes and storing the result in the Symbol.DupeCount field.
The application then must use DupeCount to modify duplicate names.

Problems:
  - Local names have no associated symbols!
  - Only types are distinguished by quotes, the type of other names is unknown.

Solution:
  Typenames and their associated symbols can be found in AllTypes.Types.
  Other names are searched in AllSyms, for exact (case insensitive) match first.
  In case of an exact match the name is retrieved from the associated symbol.
  Otherwise the dupes of the name are counted and used to create an name which
  should be unique in its local scope.
*)

(* CheckNames - update DupeCount of all symbols
  Argument is a textfile with language specific reserved words
*)
procedure TTranslator.CheckNames(filename: string);
var
  f:  TextFile;
  s:  string;

  procedure CheckList(syms: TScope);
  var
    i: integer;
    sym: TSymbolC;
  begin
    if syms = nil then begin
      LogBug('missing scope');  //further errors will occur!
      exit;
    end;
    for i := 0 to syms.Count - 1 do begin
      sym := syms.getSym(i);
      AllSyms.AddSym(sym);
    end;
  end;

begin
  AllSyms.Clear;
//load keywords
  //if filename = '' then filename := 'KDelphi.txt';
  if filename <> '' then begin
    AssignFile(f, filename);
    Reset(f);
    while not EOF(f) do begin
      ReadLn(f, s);
    //exclude phantoms and comments
      if (s <> '') and (s[1] <> '/') then begin
        AllSyms.AddKey(s);
      end;
    end;
  end;
  CloseFile(f);
//load globals
  CheckList(Globals);
  if Scope = nil then
    Scope := Globals  //special mode (header translation...)???
  else if self.Scope <> Globals then
  //load module
    CheckList(Scope);
end;

//constructor TTranslator.ForHeader(const fn: string);
procedure TTranslator.ForHeader(const fn: string);
begin
  ModuleName := ChangeFileExt(fn, '');
  self.Scope := Globals;
  InitSrc;
end;

//constructor TTranslator.ForModule(mf: TModule);
procedure TTranslator.ForModule(mf: TModule);
begin
  ModuleName := mf.Name;
  //directory? - use appdir, for now...
  if False and (ProjectDir = '') then
    ProjectDir := mf.Source.dir;
  self.scope := mf;
  InitSrc;
end;

procedure TTranslator.ForScope(AScope: TScope);
begin
  if AScope is TModule then
    ForModule(TModule(AScope))
  else begin
    ModuleName := AScope.Name;
    self.Scope := AScope;
    InitSrc;
  end;
end;

procedure TTranslator.InitSrc;
begin
//check dir
  if ProjectDir = '' then
    ProjectDir := GetCurrentDir;
  if ProjectDir[Length(ProjectDir)] <> '\' then
    ProjectDir := ProjectDir + '\';
end;

procedure TTranslator.SetTarget(Strings: TStrings);
begin
  if fOwnDst then
    dst.Free;
  fOwnDst := False;
  dst := Strings;
end;

function TTranslator.Translate: boolean;
begin
  InitSrc;
  Result := False;  //nothing done, must override in derived class
end;

function TTranslator.TranslateSym(ASym: TSymbolC): boolean;
begin
//do what?
  Result := False;  //nothing done, must override in derived class
end;

procedure TTranslator.Write(const s: string);
begin
  if not fNoShow then
    self.outbuf := outbuf + s;
end;

procedure TTranslator.WriteSep(const s: string);
begin
  if (outbuf <> '') and (outbuf[Length(outbuf)] in AlNum) then
    Write(' '); //separate from preceding contents
  Write(s);
end;

procedure TTranslator.WriteLn(const s: string);
begin
//must output space for empty lines?
  if not fNoShow then begin
    if s <> '' then
      outbuf := outbuf + s;
    if outbuf = ' ' then
      dst.Add(EmptyStr)
    else if outbuf <> '' then begin
    //handle indentation
      if indent > 0 then
        outbuf := StringOfChar(#9, indent) + outbuf;
    //to come: break long lines? depending on BreakChar?
      self.dst.Add(outbuf);
    end;
  end;
  outbuf := '';
end;

function TTranslator.UniqueName(const s: string): string;
begin
  Result := AllSyms.UniqueName(s);
end;

function TTranslator.ScanDef(const def: string): PChar;
begin
  CurDef := def;
  Scanner.SetSource(def);
  Result := PChar(def);
end;

procedure TTranslator.Outdent(const s: string);
begin
  if indent > 0 then
    dec(indent);
  WriteLn(s);
  inc(indent);
end;

initialization
  AllSyms := TDupeList.Create('dupes');
  Scanner :=  TFileMeta.Create;
finalization
  FreeAndNil(Scanner);
  FreeAndNil(AllSyms);
end.
