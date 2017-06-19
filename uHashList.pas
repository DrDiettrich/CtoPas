unit uHashList;
(* THashList is a list of objects (TStrings), combined with a hash table.
  The code is based on TStringList, with some modifications:
  The intended use are symbol lists for compilers etc., with dynamic scopes.
  - retain list order (option: allow for removal/insertion of entries)
  - possibly owned entries
  - find first/next entry of same name, search top-down (LIFO)
  - extensible item descriptors

Case sensitivity can be specified in OHashList.fCaseInsensitive.
  
Override methods of TStrings:
Must override:
  abstract: Clear, Delete, Insert, Get, GetCount
  inappropriate:  Add
Since in D4 Insert is used from (non-virtual) InsertObject,
  Insert must be implemented properly, even if its use can destroy the structure!
  Assert index=Count, as is when called from Add.

The hash table is implemented in an array[Capacity], which must be reconstructed
  entirely when the capacity changes, or when entries are reordered, deleted
  or inserted - what's not the intended use of this implementation.

  The definition and management of the Items array is up to the container class.
  OHashList implements the hash functionality, using some callback methods
  to communicate with the container object:
    GetItem:  retrieve pointer to the item descriptor. (@Items[index])
    Resize:   resize the Items array (SetLength).
    Removed:  notification of removal of an entry, for finalization...

Possible extensions:
  AddName(s), with s = <name>=<value>.
    Requires: strip value in computation of hashcode and compares
  Hash code case insensitive, find functions (first/next...) both case in/sensitive.
    This feature might simplify and speed up tests for conflicting names
    in a conversion from C (case sensitive) to OPL (case insensitive).

Problems:
  Add() and handling of dupes:
    Default list dupes property is not always appropriate,
    even in lists with dupes a new entry is not always the desired behaviour.
    Every call should specify whether an existing entry should be reused!
    -> add method AddNew(name, obj=nil), to indicate really new object desired.
    or: (CreateIn)
*)

{ $DEFINE events} //if change events are desired
{$UNDEF RandomPut}  //code deserves some updates for mutable items (renaming)

interface

uses
  Classes;

type
//hased item descriptor, extensible (requires Object instead of Record)
  PHashItem = ^THashItem;
  THashItem = record
    iFirst, iNext:  integer;  //hash chains, -1 for end of list or empty
    FString: string;
    FObject: TObject; //- extensions by container!
  end;

//callbacks
  THashGetItem = function(index: integer): PHashItem of object;
  THashResize = function(newCapacity: integer): integer of object;
  TRemoved = procedure(index: integer; item: PHashItem) of object;
  TGetHash = function(const s: string): integer of object;

  OHashList = Object
  public
  //restricted (don't modify arbitrarily)
    FCount:   Integer;
    FCapacity: Integer;
  //initialization mandatory!
    GetItem:  THashGetItem;
    Resize:   THashResize;
    Removed:  TRemoved;
    fCaseInsensitive: boolean;  //default: case sensitive
  protected
  //methods, internal
    function  GetHash(const AName: string): integer;  //case sensitive
    procedure Extend; //increase (double) capacity
    procedure Unlink(Index: integer; h: integer = -1);
{$IFDEF RandomPut}
    procedure Link(Index: integer; h: integer = -1);
{$ENDIF}
  public  //API
    procedure Clear(newCapacity: integer);
    procedure Rebuild(newCapacity: integer);
    function  FindNext(const S: string; after: integer = -1): integer;
    function  Add(const s: string; fNew: boolean = False): integer;
{$IFDEF RandomPut}
    procedure Put(Index: integer; const s: string);
{$ENDIF}
  //beware! Push valid only with accept dupes!
    function  Push(const s: string): integer; //always add new
    procedure Pop(cnt: integer);
  end;

//sample container class
{$IFDEF old}
  THashedObject = object(THashItem)
  public
    FObject: TObject;
  end;
{$ELSE}
  THashedObject = THashItem;
{$ENDIF}

  THashList = class(TStrings)
  protected
    Items: array of THashedObject;
    FList: OHashList;
    FDuplicates: TDuplicates;
  //callbacks
    function  GetItem(index: integer): PHashItem;
    function  Resize(newCapacity: integer): integer;
    procedure Removed(index: integer; item: PHashItem);
  //abstract
    function  Get(Index: Integer): string; override;
    function  GetCount: Integer; override;
  //mandatory
    function  GetCapacity: Integer; override;
    function  GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
  public
  //mandatory
    constructor Create; virtual;  //support polymorphism
    //destructor Destroy; override; - not necessary for dynamic array of items
  //abstract
    procedure Clear; override;
  //these are invalidated:
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  //meaningful
    function  Add(const S: string): Integer; override;
    function  IndexOf(const S: string): Integer; override;
  //useful
    property  FindFirst[const S: string]: Integer read IndexOf;
    function  FindNext(index: integer): integer; //use after IndexOf
    property  Capacity: integer read GetCapacity write SetCapacity;
    property  Count: integer read FList.FCount;
  //stack behaviour
    function  Push(const s: string; obj: TObject): integer; //always add new
    procedure Pop(cnt: integer); //remove cnt entries from top
  end;

implementation

uses
  SysUtils, Windows,
  uUI;  //debugging only

//error strings, from Const.pas or local?
const
  SListIndexError = 'List index out of bounds (%d)';
  SSortedListError = 'Operation not allowed on sorted string list';
  SDuplicateString = 'String list does not allow duplicates';

{ THashList }

function THashList.Add(const S: string): Integer;
begin
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
  if Result < 0 then
    Result := FList.Push(s);
end;

procedure THashList.Clear;
begin
  FList.Clear(0);  //(32);  //item list must never become empty!
end;

constructor THashList.Create;
begin
  inherited;
  FList.GetItem := GetItem;
  FList.Resize := Resize;
  FList.Removed := Removed;
  Clear;  //init empty hash list
end;

procedure THashList.Delete(Index: Integer);
begin
  Error('invalid Delete', 0);
end;

{
destructor THashList.Destroy;
begin
  FList.Pop(FList.FCount);  //destroy all owned members
  //Resize(0); //really release
  inherited;
end;
}

function THashList.FindNext(index: integer): integer;
begin
  if index >= 0 then
    Result := FList.FindNext('', index)
  else
    Result := index;  //invalid next
end;

function THashList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FList.FCount) then Error(SListIndexError, Index);
  //Result := GetItem(index).FString; - allow for virtual GetItem?
  Result := Items[Index].FString;
end;

function THashList.GetCapacity: Integer;
begin
  Result := FList.FCapacity;  //or Length(Items)
end;

function THashList.GetCount: Integer;
begin
  Result := FList.FCount;
end;

function THashList.GetItem(index: integer): PHashItem;
begin //callback
  Result := @Items[index];
end;

function THashList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FList.FCount) then Error(SListIndexError, Index);
  //Result := GetItem(index).FObject; - allow for virtual GetItem?
  Result := Items[index].FObject;
end;

function THashList.IndexOf(const S: string): Integer;
begin
  Result := FList.FindNext(s);
end;

procedure THashList.Insert(Index: Integer; const S: string);
begin
//prevent improper use of Insert (into list)
  if Index <> FList.FCount then
    Error('invalid Insert', 0)
  else  //append, is correct
    FList.Push(S);
end;

procedure THashList.Pop(cnt: integer);
begin
  FList.Pop(cnt);
end;

function THashList.Push(const s: string; obj: TObject): integer;
begin
  Result := FList.Push(s);
  Items[Result].FObject := obj;
end;

procedure THashList.Put(Index: Integer; const S: string);
begin
{$IFDEF RandomPut}
  FList.Put...
{$ELSE}
  assert(Index = FList.FCount, 'illegal insert');
  FList.Push(s);
{$ENDIF}
end;

procedure THashList.PutObject(Index: Integer; AObject: TObject);
begin
//handle overwrite existing object?
  Items[Index].FObject := AObject;
end;

procedure THashList.Removed(index: integer; item: PHashItem);
begin //callback
//handle removed item
  item.FString := ''; //at least clear the string
end;

function THashList.Resize(newCapacity: integer): integer;
begin //callback
  if newCapacity <= 0 then
    newCapacity := 32;  //some size
  SetLength(Items, newCapacity);
  Result := newCapacity;
end;

procedure THashList.SetCapacity(NewCapacity: Integer);
begin
  FList.Rebuild(NewCapacity);
end;


{ OHashList }

(* Add - add item, optional if not already in the list
  Return index of new/old item.
*)
function OHashList.Add(const s: string; fNew: boolean): integer;
begin
  if not fNew then begin
    Result := self.FindNext(s);
    if Result >= 0 then
      exit; //item exists already
  end;
//really add new
  Result := Push(s);
end;

(* Push - add new item, don't check duplicates
*)
function OHashList.Push(const s: string): integer;
var
  hash: integer;
  item, root:  PHashItem;
begin
  Result := FCount; //index of new item
//check capacity
  if Result >= FCapacity then
    Extend; //hash value becomes invalid!
//add new entry
  inc(FCount);
  hash := self.GetHash(s);
  item := GetItem(Result);
  item.FString := S;
  item.FObject := nil;
  root := GetItem(hash);
  item.iNext := root.iFirst;
  root.iFirst := Result;
end;

(* Unlink - unlink item at Index
  Used by Clear.
  Notify container of the removal.
    (problematic when items have to be re-linked!)
*)
procedure OHashList.Unlink(Index: integer; h: integer);
var
  item, prev: PHashItem;
begin
  item := GetItem(Index);
//get Hashcode?
  if h < 0 then
    h := GetHash(item.FString);
  prev := GetItem(h); //root
//check unlink first entry
  if prev.iFirst = Index then begin
    prev.iFirst := item.iNext;  //that's all!
  end else begin
    prev := GetItem(prev.iFirst); //first element
  //search the entry
    while prev.iNext > Index do
      prev := GetItem(prev.iNext);
  //check fount
    assert(prev.iNext = Index, 'corrupt hash chain');
  //unlink
    prev.iNext := item.iNext;
  end;
//success, notify container
{$IFDEF RandomPut}
  ToDo: distinguish between relinked and really removed items!
{$ELSE}
  if assigned(Removed) then
    Removed(Index, item);
{$ENDIF}
end;

(* pop - remove topmost cnt items
*)
procedure OHashList.Pop(cnt: integer);
begin
//check for remaining entries
  if cnt >= FCount then
    cnt := FCount;
//drop elements
  while cnt > 0 do begin
    dec(FCount);  //become index of entry to remove
    unlink(FCount);
    dec(cnt);
  end;
end;

{$IFDEF RandomPut}
//redesign required!

procedure OHashList.Link(Index, h: integer);
var
  prev, next: integer;
begin
  if h < 0 then
    h := GetHash(FList^[Index].FString);
  prev := FList^[h].iFirst;
//check for first entry
  //if (prev < 0) or (prev < Index) then begin - redundant!
  if (prev < Index) then begin
  //link first entry
    FList^[Index].iNext := prev;
    FList^[h].iFirst := Index;
    exit;
  end;
//check remaining entries
  while True do begin
    next := FList^[prev].iNext;
    if next < Index then begin
    //link in here
      FList^[Index].iNext := next;
      FList^[prev].iNext := Index;
      exit;
    end;
    prev := next;
  end;
end;

procedure OHashList.Put(Index: integer; const s: string);
var
  h, hOld: integer;
  p:  PHashItem;
begin
//check for same string
  if FList^[Index].FString = s then
    exit; //nothing to do
//check for same hashcode
  h := GetHash(s);
  hOld := GetHash(FList^[Index].FString);
  if h = hOld then begin
    FList^[Index].FString := s;
    exit; //stay in same chain, same place
  end;
//find entry in old chain
  Unlink(Index, hOld);
  FList^[Index].FString := s;
  Link(Index, h);
end;
{$ELSE}
  //only append allowed, no changes of existing items
{$ENDIF}

procedure OHashList.Clear(newCapacity: integer);
begin
  FCount := 0;
  Rebuild(newCapacity);
end;

procedure OHashList.Extend;
begin
  Rebuild(FCapacity * 2);
end;

function OHashList.FindNext(const S: string; after: integer): integer;
var
  p:  PHashItem;
begin
//determine starting place
  if after < 0 then begin
  //from start
    after := GetHash(S);
    Result := GetItem(after).iFirst;
  end else
    Result := GetItem(after).iNext;
//search - the list can contain various names, match exactly!
  while (Result >= 0) do begin
    p := GetItem(Result);
    if fCaseInsensitive then begin
      if StrIComp(PChar(p.FString), PChar(S)) = 0 then
        break;
    end else begin
      if p.FString = S then
        break;
    end;
    Result := p.iNext;
  end;
end;

procedure OHashList.Rebuild(newCapacity: integer);
var
  i:  integer;
  h:  integer;
  p, root:  PHashItem;
begin
  FCapacity := Resize(newCapacity);
//must clear the entire first/next entries!
  for i := 0 to FCapacity - 1 do begin
    p := GetItem(i);
    p.iFirst := -1;
    p.iNext := -1;
  end;
//now rebuild, bottom up to maintain LIFO order.
  for i := 0 to FCount - 1 do begin
    p := GetItem(i);
    //assert(p <> nil);
//OutputDebugString(PChar('rehash ' + p.FString));
    h := GetHash(p.FString);
    root := GetItem(h);
    p.iNext := root.iFirst;
    root.iFirst := i;
  end;
end;


{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
function OHashList.GetHash(const AName: string): integer;
var
  i: integer;
  h: integer; //cardinal;
  s: string;
  lc: integer;
const
  MAGIC_PRIME = 7;
begin
  if fCaseInsensitive then
    //s := UpperCase(AName)
    lc := $20 //OR for conversion from lowercase to uppercase ASCII characters.
  else
    lc := 0;
  s := AName;
  h := Length(s) * 3;
  for i := 1 to Length(s) do begin
    h := h * 251 + (ord(s[i]) or lc);
  end;
//modulo is signed!!!
  Result := (h * MAGIC_PRIME) and high(Result); //make positive
  Result := Result mod FCapacity;
end;
{$OVERFLOWCHECKS ON}  //todo: restore previous state!

end.
