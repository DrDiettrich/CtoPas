unit uTypeList;
(* Attempt to track all used type declarations/references,
  so that synthetic types
  - can be created on demand,
  - can show before usage
  and simplified handling (lookup) of composed type references.

The idea is a list of abstract type definitions, with member names removed,
that can be searched for existing typedefs.
This list is populated when the C sources are parsed.
Delphi predefined types are added first to this list,
  possibly in their C conforming form (DWORD for LongWord).
  They are read from a user configurable file (preferred names first)

Restrictions:
- Stored definitions can not be changed (are hash keys)
- Once a definition has been shown, it can not be renamed.
Flag: fShown???
*)

interface

uses
  Classes,
  uHashList, uXStrings,
  uTokenC,
  //uScanC, //TFileC...
  uTablesC;

const
  __namedDef = False; // True; //for now, drop later

type
  RTypeDef = Record //strings are not safe in Object
  //public
    r: THashItem; //holds Def and TypeSym
  {$IF __namedDef}
    typename: string; //for Delphi names - unique?
  {$IFEND}
    baseID: integer; //ID of base entry, this table (composed types)
      //no pointers, the list can be moved when resized!
      //can be combined with 1-2 flag bytes?
  end;
  PTypeDef = ^RTypeDef;

  //TDummyType = class(TSymType) end;
  TDummyType = TSymType;

  TTypeList = class(TXCustomHashList)
  private
    JustFound: PTypeDef;
    function todo: boolean; //debug only
    function AddObjectName(const ADef: string; ASym: TSymType; AName: string = ''): integer;
    function Find(const ADef: string): integer;
  protected //implement default item array
  //callbacks inherited from TXCustomHashList
    function  GetItem(index: integer): PXHashItem; override;
    function  Resize(newCapacity: integer): integer; override;
  public
    Items: array of RTypeDef; //for list viewer
    constructor Create(const AName: string; AKey: integer = 0); override;
    function FindSym(const ADef: string): TTypeDef;
  {$IFDEF old}
    function  AddKey(const name, def: string): integer;
      //language types, reject multiple names for same def? (no lookup by name)
  {$ELSE}
  {$ENDIF}
    function  AddAlias(ASym: TTypeDef; ADef: string): integer;
    function  AddRef(Adef: string): integer;
      //Before symbol creation: add first usage only, create dummy type, return what?
    function  AddDef(sym: TTypeDef): integer;
      //After type sym creation: add first type def only, store as dummy TypeSym (and typename?)
  end;

var
  Types: TTypeList;

implementation

{ TTypeList }

(* Structured types deserve special handling.
  First a typesym is created, with def='', name 'S:tag' (basetype).
  Def is inserted later, too late to use here!
  This symbol is required for possible member type refs!
  Can be used as forward ptr sym to the type.
  This symbol can be followed by member typesyms,
    which must be shown *before* the struct definition.
  If no namesym is given, a synthetic type name must be assigned and shown.
    'S_tag' can be used herefore.

  Similarly synthetic type names must be created for other synthetic
    or intermediate types.
  These names should be overridden, whenever a type of the same definition
    is created (named) explicitly!
*)


function TTypeList.todo: boolean;
begin
  Result := True; //means: abort?
end;

(* Utility: find/create entry with Def, Sym and TypeName.
  Sets JustFound for more updates.

Problem: always change Sym and TypeName???
  --> use nil/'' if conditional update is required
*)
function TTypeList.AddObjectName(const ADef: string; ASym: TSymType;
  AName: string): integer;
begin
  //Result := FList.AddObject(ADef, ASym);
  Result := FList.Add(ADef);
  if Result < 0 then begin
    JustFound := nil;
    exit;
  end;
  JustFound := PTypeDef(FList.JustFound);
  if ASym <> nil then
    JustFound.r.FObject := ASym;
{$IF __namedDef}
  if AName <> '' then
    JustFound.typename := AName;
{$ELSE}
{$IFEND}
end;

function TTypeList.AddAlias(ASym: TTypeDef; ADef: string): integer;
begin
  Result := Find(ADef);
  if Result < 0 then
    Result := AddObjectName(ADef, ASym, ASym.name)
  else if JustFound.r.FObject = nil then
    JustFound.r.FObject := ASym
  else
    LogBug('redef alias');
end;

(* Find and set JustFound
*)
function TTypeList.Find(const ADef: string): integer;
begin
  Result := FList.FindNext(ADef);
  if Result < 0 then
    JustFound := nil
  else
    JustFound := PTypeDef(FList.JustFound);
end;

function TTypeList.FindSym(const ADef: string): TTypeDef;
begin
  if Find(ADef) > 0 then
    Result := JustFound.r.FObject as TTypeDef
  else
    Result := nil;
end;

(* The type symbols must be created in/by Globals!
  Here we only need reference entries, can safely ignore many cases
  Allow for redefinition - at least for current scope!!!
*)
function TTypeList.AddDef(sym: TTypeDef): integer;
var
  i: integer; //Pos of ':' in sym.name
begin
(* strip member names?
  direct declarations have no member names
  reject typename refs? add direct ref for "..."?
  Proc type should preserve names? '(...' or '*(...'?
  reject struct type defs 'S{...', only 'S:tag' or "S:..."
  member def's don't have a name?
  reject Post ([...)
*)
  Result := -1; //some error?
  if sym.Def = '' then
    exit;

  Result := Find(sym.Def);
  if Result >= 0 then begin //exists already
  //something to update in the existing entry or sym?
    if JustFound.r.FObject = nil then begin
      JustFound.r.FObject := sym;
    {$IF __namedDef}
      JustFound.typename := sym.name; //name existing (synthetic) type?
    {$ELSE}
    {$IFEND}
      //sym.typeID := Result; ???
    end; //else?
    //more?
    exit; //already entered
  end;

//ignore immediate structured types(?) remember for base?
  i := Pos(':', sym.name);
  if (i = 2) then begin
    Result := AddObjectName(sym.name, sym, sym.name); //?
    exit; //ignore?
  end else if i > 1 then //can also occur in expression (C_ASSERT!)
    exit;

//special proc handling required?
  i := Pos('(', sym.Def);
  case i of
  1: //direct (C) typedef
    begin
    //strip parameter names?
      Result := AddObjectName(sym.Def, sym, sym.name);
      AddObjectName('*' + sym.Def, sym, sym.name); //same for pointer
      //if todo then
        exit; //for now
    end;
  2:  //*(...)
    if sym.Def[1] <> '*' then
      exit
    else begin
      AddRef(sym.Def); //down to proc type?
    end;
  end;

//unstructured types
  Result := AddObjectName(sym.Def, sym, sym.name);
  //more?
end;

{$IFDEF old}
function TTypeList.AddKey(const name, def: string): integer;
var
  sym: TTypeDef;
begin
(* No typesyms created for Delphi System types?
  Special typesym, flagged as DelphiImport?
  Not listed!
  Prevent override HOW???
*)
  if name = '' then
    sym := nil
  else begin
    sym := Globals.defType(name, def, 0);
    //flag loc???
    sym.storage := Kextern; //noshow!
  end;
  Result := AddObjectName(def, sym, name);
end;
{$ELSE}
{$ENDIF}

(* Add type def, return typeIdx
  TypeSym should also get baseIdx (of base type)
*)
function TTypeList.AddRef(Adef: string): integer;
var
  i, baseIdx: integer;
begin
(* does not contain member names?
  recurse until a base type is found!
  single entry for ptr and direct target? (using sym.ptrname?)
*)
  assert(Adef <> '', 'bad call');
  i := pos(':', Adef); //recject ()?...:...
  Result := Find(Adef);
  if Result < 0 then begin
  //recurse?
    case Adef[1] of
    typeQuote,  //should be handled in Globals
    '{':
      //if todo then
      exit; //reject (anonymous?) structs?
    '[', //arrays must be handled!
    '(': //proc
      begin //do what?
        if todo then
          exit;
      end;
    //allowed simple type components?
      //should result in distinct symbols without recursing!?
    //allowed/expected modifiers?
      //recurse here?
    else //case
    //recurse
      Result := AddRef(copy(Adef,2,length(Adef)));
      baseIDx := Result;
    //now create modified type
      Result := FList.Push(Adef); //known as new
      JustFound := @Items[Result]; //not reset by Add/Push!
      JustFound.baseID := baseIdx;
    {
      JustFound.typename := Adef[1] + quoteType(Items[baseIdx].typename); //*"..."
      Globals.defType(JustFound.typename, Adef, 0); //indirect recursion!?
      if Result < 0 then
        exit; //give up???
    }
    end;
  end;
//all done?
end;

constructor TTypeList.Create(const AName: string; AKey: integer);
begin
  FList.GetItem := GetItem; // GetHashItem;
  FList.Resize := Resize;
  inherited;
//add some string for index 0 (assumed invalid/undefined)
  Add(''); //or void?
end;

function TTypeList.GetItem(index: integer): PXHashItem;
begin
  Result := @Items[index];
end;

function TTypeList.Resize(newCapacity: integer): integer;
var
  n:  integer;
begin //callback
  if newCapacity <= 0 then
    newCapacity := 32;  //some size
  SetLength(Items, newCapacity);
//init mem?
  n := FList.FCount;
  FillChar(Items[n], (newCapacity - n) * sizeof(Items[0]), 0);
  Result := newCapacity;
end;

procedure InitTypes;
begin
//todo: enter Delphi types to use - from TDelphi.txt?
end;

initialization
  Types := TTypeList.Create('Types');
  InitTypes;
end.

