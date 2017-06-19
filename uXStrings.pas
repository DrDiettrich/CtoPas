unit uXStrings;
(* replacement for the old XStrings unit.

XStrings are extended TStrings, with explicit member/owner relationship.
  Useful in trees and other hierarchical data structures.

TXStrings is the base class, implementing the abstract methods of TStrings.
  This class has no members.

TXStringList implements a list of TXStrings, comparable to TStringList.
  This implementation uses a hash list (see uHashList.pas)
*)

interface

uses
  Classes,  //TStrings
  uHashList;  //OHashList

type
  TXStrings = class(TStrings)
  private //don't modify arbitrarily!
    FOwner: TXStrings;
    FIndex: integer;  //index in owner
  protected
  //implement abstract methods of TStrings
    function  Get(Index: Integer): string; override;
    function  GetCount: Integer; override;
  //extensions
    function  GetCaption: string; virtual;  //for display in list controls
    function  GetName: string; virtual;
    function  GetMember(index: integer): TXStrings; virtual;
    procedure PutMember(index: integer; AMember: TXStrings); virtual;
    procedure SetOwner(AOwner: TXStrings); //virtual;
  public
    constructor Create(const AName: string; AKey: integer = 0); virtual;
    constructor CreateIn(AOwner: TXStrings; const AName: string; AKey: integer = 0);
    destructor  Destroy; override;
  //implement abstract methods of TStrings
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;

    property  Caption: string read GetCaption;
    property  Name: string read GetName;
    property  Members[index: integer]: TXStrings read GetMember write PutMember;
    property  ListIndex: integer read FIndex;
    property  Owner: TXStrings read FOwner write SetOwner;
  end;

//extend hash item
{$IFDEF old}
  TXHashItem = Object(THashItem)
  public
    FMember:  TXStrings;
  end;
  PXHashItem = ^TXHashItem;
  TXHashGetItem = function(index: integer): PXHashItem;
{$ELSE}
  TXHashItem = THashItem;
  PXHashItem = PHashItem;
{$ENDIF}

  TXCustomHashList = class(TXStrings)
  protected
  //note: no Items[] here!
    //Items: array of TXHashItem;
    FList: OHashList; //the list implementation
{$IFDEF Duplicates}
    FDuplicates: TDuplicates;
{$ENDIF}
  //callbacks, virtual for any derived THashItem descriptors
    function  GetItem(index: integer): PXHashItem; virtual; abstract;
    function  Resize(newCapacity: integer): integer; virtual; abstract;
    procedure Removed(index: integer; item: PHashItem);
  //override list related methods
    function  Get(Index: Integer): string; override;
    procedure Put(Index: Integer; const S: string); override;
    function  GetCapacity: Integer; override;
    procedure SetCapacity(newCapacity: integer); override;
    function  GetCount: Integer; override;
    function  GetObject(Index: integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function  GetMember(index: integer): TXStrings; override;
    procedure PutMember(index: integer; AMember: TXStrings); override;
  public
  //override list related methods
    constructor Create(const AName: string; AKey: integer = 0); override;
    destructor Destroy; override;
    procedure Clear; override;
  //these are invalidated:
    //procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  //meaningful
    function  Add(const S: string): Integer; override;
    function  IndexOf(const S: string): Integer; override;
  //useful
    property  FindFirst[const S: string]: Integer read IndexOf;
    function  FindNext(index: integer): integer;  //use after IndexOf
    procedure Pop(cnt: integer); //remove cnt entries from top
  //properties
    property  Capacity: integer read FList.FCapacity write SetCapacity;
    property  Count: integer read FList.FCount;
  end;

  TXStringList = class(TXCustomHashList)
  protected //implement default item array
    Items: array of TXHashItem;
  //callbacks, virtual for any derived THashItem descriptors
  {$IFDEF old}
    function  GetItem(index: integer): PXHashItem; override;
    function  GetHashItem(index: integer): PHashItem;
      //D4 doesn't allow for casting :-(
  {$ELSE}
    function  GetItem(index: integer): PXHashItem; override;
  {$ENDIF}
    function  Resize(newCapacity: integer): integer; override;
  public
    constructor Create(const AName: string; AKey: integer = 0); override;
  end;
  TDict = TXStringList;

implementation

(* This implementation almost duplicates uHashTable.
  A redesign of uHashTable might be a good idea...
*)

const
  SListIndexError = 'List index out of bounds (%d)';

{ TXStrings }

(* Clear - clear member list

  This method is meaningful only in classes with lists, but since the method
  is abstract in the base class, we can do something here already.
*)
procedure TXStrings.Clear;
var
  i: integer;
  o: TXStrings;
begin
  for i := 0 to Count - 1 do begin
    o := Members[i];
    if (o <> nil) and (o.FOwner = self) then begin
    //destroy owned member
      o.FOwner := nil;  //prevent recursion
      o.Free;
    end;
  end;
end;

constructor TXStrings.Create(const AName: string; AKey: integer);
begin
  //nop, unless .FName and .FKey fields exists in a derived class
end;

constructor TXStrings.CreateIn(AOwner: TXStrings; const AName: string;
  AKey: integer);
begin
//init the private fields first?
  FOwner := AOwner; //for use in Create
  if AOwner <> nil then
    self.FIndex := AOwner.addObject(AName, self)
  else
    FIndex := -1;
//now the virtual initialization
  Create(AName, AKey);
end;

procedure TXStrings.Delete(Index: Integer);
begin
//invalid call
  Error('invalid Delete', 0);
end;

procedure TXStrings.SetOwner(AOwner: TXStrings);
begin
  if (AOwner <> nil) then begin
  //find name in the owner's stringlist
    self.FIndex := AOwner.IndexOfObject(self);
    //assert(FIndex >= 0, 'invalid owner');
  end else
    FIndex := -1; //???
  FOwner := AOwner;
end;

function TXStrings.Get(Index: Integer): string;
begin
  if Index < 0 then //may be useful, sometimes?
    Result := self.GetName
  else if Index >= Count then
    //Result := ''; ?
    Error(SListIndexError, Index);
end;

function TXStrings.GetCaption: string;
begin
  Result := GetName;  //default
end;

function TXStrings.GetCount: Integer;
begin
  Result := 0;
end;

function TXStrings.GetMember(index: integer): TXStrings;
begin
  if index < 0 then
    Result := self
  else
    Result := nil;  //no members in base class
end;

function TXStrings.GetName: string;
begin
  if FOwner <> nil then
    Result := FOwner.Strings[FIndex]
  else
    Result := '???';  //indicate unnamed?
end;

procedure TXStrings.Insert(Index: Integer; const S: string);
begin
//invalid call
  Error('invalid Insert', 0);
end;

procedure TXStrings.PutMember(index: integer; AMember: TXStrings);
begin
  //no member list - raise an error if called?
end;

{ TXCustomHashList }

function TXCustomHashList.Add(const S: string): Integer;
begin
{$IFDEF Duplicates}
  case FDuplicates of
  dupIgnore:  Result := FList.FindNext(s);
  dupAccept:  Result := -1;
  else  //dupError: - prevent compiler warning: result might be undefined
    begin
      Result := FList.FindNext(s);
      if Result >= 0 then
        Error(SDuplicateString, Result);
    end;
  end;
{$ELSE}
//we assume unique strings in a parser application
  Result := FList.FindNext(s);
{$ENDIF}
  if Result < 0 then
    Result := FList.Push(s);
end;

procedure TXCustomHashList.Clear;
begin
  FList.Clear(0); //handle member destruction in callback
end;

constructor TXCustomHashList.Create(const AName: string; AKey: integer);
begin
  inherited;
//does this work with virtual methods?
  //FList.GetItem := GetItem;
  FList.Resize := Resize;
  FList.Removed := Removed;
  Clear;  //init empty hash list
end;

{
procedure TXCustomHashList.Delete(Index: Integer);
begin
//invalid call
  Error('invalid Delete', 0);
end;
}

destructor TXCustomHashList.Destroy;
begin
  inherited Clear;  //fast destroy all owned members
  //FList.Pop(FList.FCount);  //destroy all owned members
  inherited;
end;

function TXCustomHashList.FindNext(index: integer): integer;
begin
  if index >= 0 then
    Result := FList.FindNext('', index)
  else
    Result := index;  //invalid next
end;

function TXCustomHashList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FList.FCount) then
    Result := inherited Get(Index)  //Error(SListIndexError, Index);
  else
    Result := GetItem(Index).FString;
end;

function TXCustomHashList.GetCapacity: Integer;
begin
  Result := FList.FCapacity;
end;

function TXCustomHashList.GetCount: Integer;
begin
  Result := FList.FCount;
end;

function TXCustomHashList.GetMember(index: integer): TXStrings;
begin
{$IFDEF slow}
  if (Index < 0) then
    Result := inherited GetMember(index)
  else if (Index >= FList.FCount) then begin
    Error(SListIndexError, Index);
    Result := nil;  //keep compiler happy
  end else
{$ELSE}
  if (Index < 0) or (Index >= FList.FCount) then Error(SListIndexError, Index);
{$ENDIF}
{$IFDEF old}
    Result := GetItem(index).FMember;
{$ELSE}
    TObject(Result) := GetItem(index).FObject;
{$ENDIF}
end;

function TXCustomHashList.GetObject(Index: integer): TObject;
begin
  if (Index < 0) or (Index >= FList.FCount) then Error(SListIndexError, Index);
{$IFDEF old}
    Result := GetItem(index).FMember;
{$ELSE}
    TObject(Result) := GetItem(index).FObject;
{$ENDIF}
end;

function TXCustomHashList.IndexOf(const S: string): Integer;
begin
  Result := FList.FindNext(s);
end;

procedure TXCustomHashList.Insert(Index: Integer; const S: string);
begin
//prevent improper use of Insert (into list)
  if Index <> FList.FCount then
    Error('invalid Insert', 0)
  else  //append, is correct
    FList.Push(S);
end;

procedure TXCustomHashList.Pop(cnt: integer);
begin
  FList.Pop(cnt);
end;

procedure TXCustomHashList.Put(Index: Integer; const S: string);
begin
{$IFDEF RandomPut}
  FList.Put...
{$ELSE}
  assert(Index = FList.FCount, 'illegal insert');
  FList.Push(s);
{$ENDIF}
end;

procedure TXCustomHashList.PutMember(index: integer; AMember: TXStrings);
var
  item: PXHashItem;
begin
  item := GetItem(index);
  if item.FObject = AMember then
    exit; //nothing to do
  Removed(index, item); //destroy eventual owned member
  item.FObject := AMember;
end;

procedure TXCustomHashList.PutObject(Index: Integer; AObject: TObject);
var
  item: PXHashItem;
begin
  item := GetItem(index);
  if item.FObject = AObject then
    exit; //nothing to do
  if item.FObject <> nil then
    Removed(index, item); //destroy eventual owned member
  item.FObject := AObject;
end;

procedure TXCustomHashList.Removed(index: integer; item: PHashItem);
var
  o: TXStrings;
begin //callback
  TObject(o) := item.FObject;
  if (o <> nil) and (o.FOwner = self) then begin
    o.FOwner := nil;  //prevent recursion
    o.Free;
  end;
  item.FObject := nil;
end;

procedure TXCustomHashList.SetCapacity(newCapacity: integer);
begin
  FList.Rebuild(newCapacity);
end;

{ TXStringList }

constructor TXStringList.Create(const AName: string; AKey: integer);
begin
  FList.GetItem := GetItem; // GetHashItem;
  FList.Resize := Resize;
  inherited;
end;

{$IFDEF old}
function TXStringList.GetHashItem(index: integer): PHashItem;
begin //callback
  PXHashItem(Result) := @Items[index];
end;
{$ELSE}
{$ENDIF}

function TXStringList.GetItem(index: integer): PXHashItem;
begin //internal use
  Result := @Items[index];
end;

function TXStringList.Resize(newCapacity: integer): integer;
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

destructor TXStrings.Destroy;
begin
  //debug only!
  inherited;
end;

end.
