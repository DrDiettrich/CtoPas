unit fScopeView;
(* View Globals... scopes
*)

interface

{$INCLUDE config}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  uTablesC, ComCtrls;

type
  TScopeView = class(TForm)
    lbSyms: TListBox;
    Panel1: TPanel;
    swSorted: TCheckBox;
    Splitter1: TSplitter;
    edDef: TMemo;
    lbScopes: TTabControl;
    buTranslate: TButton;
    buUnit: TButton;
    procedure swSortedClick(Sender: TObject);
    procedure lbSymsClick(Sender: TObject);
    procedure lbScopesChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure buTranslateClick(Sender: TObject);
    procedure buUnitClick(Sender: TObject);
  private
    CurScope: TScope;
  public
    procedure ShowUnsorted;
  end;

var
  ScopeView: TScopeView;

implementation

{$R *.DFM}

uses
  uToPas,
  uTablesPrep,  //PSymPrep
  uFiles, //RFileLoc
  uUI;

{ TSymView }

procedure TScopeView.ShowUnsorted;
var
  i:  integer;
  sym:  TSymbolC;
begin //ShowUnsorted;
  lbSyms.Clear;
  for i := 0 to CurScope.Count - 1 do begin
    sym := CurScope.getSym(i);
    lbSyms.Items.AddObject(sym.name, sym);
  end;
end;

procedure TScopeView.swSortedClick(Sender: TObject);
begin
  if swSorted.Checked then
    self.lbSyms.Sorted := True
  else
    ShowUnsorted;
end;

procedure TScopeView.lbSymsClick(Sender: TObject);
var
  i:  integer;
  sym:  TSymbolC;
  s:  string;
  loc:  RFileLoc;
  r:  PSymPrep;
  mac:  TSymbol;  //TSymMacro;
  locs: TFileLocs;
begin
  self.edDef.Clear;
  i := lbSyms.ItemIndex;
  if i < 0 then
    exit;
  TObject(sym) := lbSyms.Items.Objects[i];
  if sym = nil then begin
    edDef.Text := {aSymbolKind} '<no definition>';  //to come: search C symbols
  end else begin
    s := SymKinds[sym.kind];
    s := s + '[' + IntToStr(sym.UniqueID) + '/'  //global STB
      + IntToStr(sym.GlobalID) + ']' + EOLstr;  //C symbols
  //locs
    if sym.loc.src <> nil then begin
      loc := sym.loc;
      locs[0] := loc;
      s := s + 'def ' + loc.src.name + ':' + IntToStr(loc.line) + EOLstr;
      if sym.UniqueID > 0 then begin
        r := Symbols.getSymbol(sym.UniqueID);
        mac := r.GetMacro;
        if (mac <> nil) and (mac <> sym) and (mac.loc.src <> nil) then begin
          loc := mac.loc;
          locs[1] := loc;
          s := s + 'mac ' + loc.src.name + ':' + IntToStr(loc.line) + EOLstr;
        end else
          locs[1].src := nil;
        if (r.loc.src <> nil)
        and ((r.loc.src <> loc.src) or (r.loc.line <> loc.line)) then begin
          locs[2] := r.loc;
          s := s + 'sym ' + r.loc.src.name + ':' + IntToStr(r.loc.line) + EOLstr;
        end else
          locs[2].src := nil;
      end;
      showSym(locs);  //even if not in Symbols!
    end;
  //def
    s := s + sym.toString; //definition?
    edDef.Text := s;
  end;
end;

procedure TScopeView.buTranslateClick(Sender: TObject);
var
  i:  integer;
  sym:  TSymbolC;
begin
  self.edDef.Clear;
  i := lbSyms.ItemIndex;
  if i < 0 then
    exit;
  TObject(sym) := lbSyms.Items.Objects[i];
  if sym = nil then begin
    edDef.Text := {aSymbolKind} '<no definition>';  //to come: search C symbols
  end else begin
    XToPas.Target := edDef.Lines;
    XToPas.ForScope(CurScope);
    XToPas.TranslateSym(sym);
    //edDef.Assign(XToPas.Target);
  end;
end;

procedure TScopeView.FormActivate(Sender: TObject);
var
  i: integer;
  scope: TScope;
begin
  if CurScope <> nil then
    exit; //already initialized
//set tabstops in memo
  SetMemoTabs(edDef);
//init the scopes list
  lbScopes.Tabs.AddObject(Globals.Name, Globals);
  for i := 0 to Modules.Count - 1 do begin
    scope := Modules.Objects[i] as TScope;
    lbScopes.Tabs.AddObject(scope.name, scope);
  end;
  lbScopes.TabIndex := 0; //force trigger show?
  lbScopesChange(self);
end;

procedure TScopeView.lbScopesChange(Sender: TObject);
var
  i: integer;
begin
//activate the scope
  lbSyms.Clear;
  i := lbScopes.TabIndex;
  if i < 0 then
    exit;
  CurScope := lbScopes.Tabs.Objects[i] as TScope;
  ShowUnsorted;
end;

procedure TScopeView.buUnitClick(Sender: TObject);
begin
  self.edDef.Clear;
  //CurScope := lbScopes.Tabs.Objects[i] as TScope;
  XToPas.Target := edDef.Lines;
  if CurScope is TModule then
    XToPas.ToUnit(CurScope as TModule)
  else begin
  //assume globals?
    XToPas.Translate;
  end;
end;

end.
