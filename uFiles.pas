unit uFiles;
(* Pathes and files

An attempt was made to prevent multiple inclusion of the same file.
Note: THashList is case sensitive, Windows file names are not!

Further optimizations should load all searched directories,
  and search for files in these lists.
  Note: proper implementation of relative pathes "..\xyz" is not trivial!
*)

interface

uses
  Classes,
  uHashList, uXStrings;

type
  TFile = class(TStringList)
  private //internal use only
    constructor CreateFrom(const ADir, fn: string);
  public
    name, dir:  string;
    id: integer;  //index in file list
    foundWhere: integer;  //index in include path
    procedure UpdateFrom(lst: TStrings);
  end;

  eFileKind = (
    fkSource,   //search SourcePath
    fkInclude,  //search IncludePath
    fkIncludeOnce  //dto., process only once
  );
//all processed files
  TFileList = class(THashList)
  public
  //internal
    function  Exists(const dir, fn: string): boolean;
    function  Mapped(const dir, fn: string): TFile;
    function  NewFile(const dir, fn: string; fKind: eFileKind): TFile;
      //nil if already in list!
  //API
    function  GetFile(index: integer): TFile;
  end;

  TSymbol = class;

  //TListSymC = class(TXStringList)
  TSymbols = class(TXStringList)
  protected
    fName: string;
    function  GetName: string; override; //doesn't have an owner!
  public
    constructor Create(const AName: string; AKey: integer = 0); override;
    function  getSym(const AName: string): TSymbol; overload;
    function  getSym(index: integer): TSymbol; overload;
  end;

{$IFDEF new}
  TScope = class(TSymbols)
  public
  end;

  TModule = class(TScope)
  public
  end;
{$ELSE}
{$ENDIF}

(* problem: TFile reference can become invalid!
  Can a filename be stored in a record, not handled when copying?
*)
  RFileLoc = record
    //src:  TFile; //can be closed!
    name: string;
    line: integer;
  end;
  TFileLocs = array[0..2] of RFileLoc;  //mac, decl, def

  TSymbol = class(TXStrings)
  public
    //id = index in owning list (scope)
    altID:    integer;  //for non-macros: typeless index of symbol entry
    loc:  RFileLoc;
    function  toString: string; virtual;  //full (meta) string
    function  BodyString: string; virtual;  //definition (tokens, only)
    property  name: string read GetName;
  //reduce confusion
    function  UniqueID: integer; virtual; //index in Symbols
    function  GlobalID: integer; virtual; //index in Globals, if >0?
    property  symID: integer read UniqueID;
  end;

{intended: directory cache - somewhat tricky with ..\ etc.
  TDirList = class(THashList)
  public
    //procedure AddDir(const dir: string);
    function  AddFile(const fn, dir: string): TFile;
      //nil if file already exists
    function  AddFile(const fn, dir: string): TFile;
  end;
}

  TSearchPath = class(TStringList)
  public
    function  FindFile(const fn: string): integer;
      //return dir index
    procedure AddDir(const dn: string);
  end;

//user handler, called before processing #included file.
  TPreInclude = procedure(f: TFile) of object;

var
  fIncludeOnlyOnce: boolean = True;  //include files only once?
  Files:  TFileList;
  ConfigFiles:  TStrings;
  SourcePath,
  IncludePath:  TSearchPath;
  PreInclude: TPreInclude;

//Unix portability (for D4)

const
{$IFDEF WIN32}
  DirSeparator = '\';
{$ELSE}
  DirSeparator = '/';
{$ENDIF}

//function  AddIncludeDir(dir: string): integer;
function  AddIncludeDir(dir: string; fIncl: boolean): integer;
  //return index in include path
function  AddFile(f: TFile): integer;
  //return index in file list
function  GetFile(const fn: string): TFile;
  //always return file, search SourcePath

//from #include...
function  IncludeFile(const fn: string): TFile;
  //search all directories, return created file
function  IncludeSysFile(const fn: string): TFile;
  //search include path only
function  IncludeNextFile(const fn: string; startwhere: integer; fKind: eFileKind): TFile;
  //search include path only

implementation

uses
  SysUtils, uUI;

(* UnifiedPath - all lowercase and '\' separated for Windows
*)
function  UnifiedPath(const s: string): string;
begin
{$IFDEF WIN32}
  Result := LowerCase(StringReplace(s, '/', DirSeparator, [rfReplaceAll]));
{$ELSE}
  Result := s;
{$ENDIF}
end;

function  AddIncludeDir(dir: string; fIncl: boolean): integer;
begin
//return index in include path
  dir := UnifiedPath(dir);
//todo: check directory exists, is not empty?
//assert trailing "\"
  if dir[Length(dir)] <> DirSeparator then
    dir := dir + DirSeparator;
  if DirectoryExists(Copy(dir, 1, Length(dir) - 1)) then begin
    if fIncl then //and FileExists(dir + '*.h') then
      Result := IncludePath.Add(dir)
    else //if FileExists(dir + '*.c')
      Result := SourcePath.Add(dir);
  end else begin
  //directory does not exist, or is empty - don't add to search path!
    //Log('no such directory or empty: ' + dir, lkWarn);
    Log('no such directory: ' + dir, lkConfig);
    Result := -1;
  end;
end;

function  AddFile(f: TFile): integer;
begin
//return index in file list
  Result := Files.AddObject(f.name, f);
end;

function  GetFile(const fn: string): TFile;
var
  i, di: integer;
  d, f: string;
  fo: TFile absolute Result;
begin
(* find source file
*)
  f := UnifiedPath(fn);
//try absolute path first
  if FileExists(f) then begin
    di := 0;
  end else begin
    d := ExtractFilePath(f);
    if d <> '' then begin
    //file with path - not found
      Log('no such file: ' + fn, lkConfig);
      Result := nil;  //no such file
      exit;
    end;
  //file without path, search
    //f := ExtractFileName(f);
    di := -1;
  //search parent dirs?
    if False then begin
      for i := Files.Count - 1 downto 0 do begin
        fo := Files.GetFile(i);
        if Files.Exists(fo.dir, f) then begin
          d := fo.dir;
          di := i;
          break;
        end;
      end;
    end;
  //search source path
    if di < 0 then begin
    //not found
      for i := 0 to SourcePath.Count - 1 do begin
        d := SourcePath.Strings[i];
        if Files.Exists(d, f) then begin
          di := i;
          break;
        end;
      end;
    end;
  end;
//check found
  if di < 0 then
    Result := nil
  else begin
    Result := Files.NewFile(d, f, fkSource);
    if Result <> nil then
      Result.foundWhere := -1;  //not in include path!
  end;
end;

//search include path only
function  IncludeNextFile(const fn: string; startwhere: integer; fKind: eFileKind): TFile;
var
  i: integer;
  d, f: string;
begin
  f := UnifiedPath(fn);
  for i := startwhere + 1 to IncludePath.Count - 1 do begin
    d := IncludePath.Strings[i];
    if Files.Exists(d, f) then begin
      Result := Files.NewFile(d, f, fKind);  //TFile.CreateFrom(d, f);
      if Result <> nil then
        Result.foundWhere := i;
      //AddFile(Result);
      exit;
    end;
  end;
//file not found!
  Log(f + ' not found', lkConfig);
  Result := nil;
end;

//search include path only
function  IncludeSysFile(const fn: string): TFile;
begin
//search full include path
  Result := IncludeNextFile(fn, -1, fkIncludeOnce);
end;

//search all directories, return created file
function  IncludeFile(const fn: string): TFile;
var
  i, di: integer;
  d, f: string;
  fo: TFile;
begin
  f := UnifiedPath(fn);
  di := -1;
//try absolute path first
  if FileExists(f) then begin
    di := 0;
  end else begin
  //search parent dirs
    for i := Files.Count - 1 downto 0 do begin
      fo := Files.GetFile(i);
      if Files.Exists(fo.dir, f) then begin
        d := fo.dir;
        di := i;
        break;
      end;
    end;
  end;
//check found
  if di < 0 then
    Result := IncludeSysFile(f)
  else begin
    Result := Files.NewFile(d, f, fkInclude);
    if Result <> nil then
      Result.foundWhere := -1;  //not in include path!
  end;
end;

{ TFile }

constructor TFile.CreateFrom(const ADir, fn: string);
begin
  inherited Create;
  if ADir <> '' then begin
    name := fn;
    dir := ADir;
  end else begin
    self.name := ExtractFileName(fn);
    self.dir := ExtractFilePath(fn);  //include trailing '\'
  end;
  if dir = '' then
    dir := GetCurrentDir + DirSeparator; //+?
  dir := UnifiedPath(dir);
  name := UnifiedPath(name);
  self.id := AddFile(self);
  self.LoadFromFile(dir + name);
//manage config file display
  if (ConfigFiles <> nil) then begin
  //list *.def and *.c?
    //if (ExtractFileExt(name) = '.def')
    //or (ExtractFileExt(name) = '.c') then
  //exclude *.h* and *.inc - assume every file has an extension!
    if not (ExtractFileExt(name)[2] in ['h', 'i']) then begin
      ConfigFiles.AddObject(name, self);
      if SourcePath.IndexOf(dir) < 0 then
        SourcePath.Add(dir);
    end;
  end;
end;

procedure TFile.UpdateFrom(lst: TStrings);
begin
  self.Assign(lst);
  self.SaveToFile(dir+name);
  { TODO : mark changed!? }
end;

{ TFileList }

function TFileList.Exists(const dir, fn: string): boolean;
var
  f: TFile;
begin
//search loaded files
  f := Mapped(dir, fn);
  if f <> nil then
    Result := True
  else  //check physical dir
    Result := FileExists(dir + fn);
end;

function TFileList.GetFile(index: integer): TFile;
begin
  TObject(Result) := Objects[index];
end;


function TFileList.Mapped(const dir, fn: string): TFile;
var
  i: integer;
  d, f: string;
begin
//search loaded files
  d := UnifiedPath(dir);
  f := UnifiedPath(fn);
  i := self.FindFirst[f];
  while i >= 0 do begin
    Result := GetFile(i);
    if (Result <> nil) and ((d = '') or (Result.dir = d)) then
      exit;
    i := self.FindNext(i);
  end;
//not yet loaded
  Result := nil;
end;

function TFileList.NewFile(const dir, fn: string; fKind: eFileKind): TFile;
begin
  Result := Mapped(dir, fn);
  if Result = nil then  //really new file
    Result := TFile.CreateFrom(dir, fn)
  //else if fIncludeOnlyOnce then  //don't process twice
  else if (fKind = fkIncludeOnce) then  //don't process twice
    Result := nil;
  //else process already loaded file again
end;

{ TSearchPath }

procedure TSearchPath.AddDir(const dn: string);
begin
  if IndexOf(dn) < 0 then
    Add(dn);
end;

function TSearchPath.FindFile(const fn: string): integer;
var
  i: integer;
begin
  for i := 0 to Count-1 do begin
    if Files.Exists(Strings[i], fn) then begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

{ TSymbol }

function TSymbol.BodyString: string;
begin
  Result := '';
end;

function TSymbol.GlobalID: integer;
begin
//if in Symbols table
  Result := self.altID; //if global: in Globals table
end;

function TSymbol.toString: string;
begin
  Result := name +  BodyString;
end;

function TSymbol.UniqueID: integer;
begin
//if in Symbols table
  Result := self.ListIndex; //in preprocessor table
end;

{ TSymbols }

constructor TSymbols.Create(const AName: string; AKey: integer);
begin
  inherited;
  fName := AName;
end;

function TSymbols.GetName: string;
begin
  Result := self.fName;
end;

function TSymbols.getSym(const AName: string): TSymbol;
var
  i: integer;
begin
  i := IndexOf(AName);
  if i >= 0 then
    TObject(Result) := Members[i]
  else
    Result := nil;
end;

function TSymbols.getSym(index: integer): TSymbol;
begin
  TObject(Result) := Members[index];
end;

initialization
  Files := TFileList.Create;
  IncludePath := TSearchPath.Create;
  SourcePath := TSearchPath.Create;

finalization
  FreeAndNil(Files);
  FreeAndNil(IncludePath);
  FreeAndNil(SourcePath);
end.
