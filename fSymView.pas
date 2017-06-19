unit fSymView;

interface

{$INCLUDE config}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TSymView = class(TForm)
    lbSyms: TListBox;
    Panel1: TPanel;
    swSorted: TCheckBox;
    Splitter1: TSplitter;
    edDef: TMemo;
    buTestMacro: TButton;
    procedure swSortedClick(Sender: TObject);
    procedure lbSymsClick(Sender: TObject);
    procedure buTestMacroClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ShowUnsorted;
  end;

var
  SymView: TSymView;

implementation

{$R *.DFM}

uses
  uUI,
  uParseC, uFiles,
  uTablesPrep, uTablesC;

{ TSymView }

procedure TSymView.ShowUnsorted;
var
  i:  integer;
  mac:  PSymPrep;
  sym:  TSymMacro;

  procedure addSyms(lst: TSymbols);
  var
    i: integer;
    sym:  TSymbol;
  begin
    for i := 0 to lst.Count - 1 do begin
      sym := lst.getSym(i);
      lbSyms.Items.AddObject(sym.name, sym);
    end;
  end;

begin //ShowUnsorted;
  lbSyms.Clear;
  //prevent dupes?
//add all macro symbols
  for i := 0 to Symbols.Count - 1 do begin
    mac := Symbols.getSymbol(i);
    if mac <> nil then begin
      sym := mac.GetMacro;
      if (sym <> nil) and (sym.altID = 0) then
        lbSyms.Items.AddObject(mac.FString, mac.FObject);
    end;
  end;
//other symbols
  addSyms(Globals);
end;

procedure TSymView.swSortedClick(Sender: TObject);
begin
  if swSorted.Checked then
    self.lbSyms.Sorted := True
  else
    ShowUnsorted;
end;

procedure TSymView.lbSymsClick(Sender: TObject);
var
  i, id:  integer;
  sym:  TSymMacro;
  s:  string;
  //r:  PSymPrep;
const
  aSymbolKind: array[eSymbolKind] of string = (
  //symbols (without macros)
    'Sym',    //default: any symbol
    'CKey',   //treat as keyword (of appkind), disallow redefinition
  //if macro defined, treat definition as:
    'Const',  //named constant, macro should be deleted?
    'Type',   //typename
    'Proc',   //procedure/function
    'Macro'   //expand as macro
  );
begin
  self.edDef.Clear;
  i := lbSyms.ItemIndex;
  if i < 0 then
    exit;
  id := -1;
  TObject(sym) := lbSyms.Items.Objects[i];
  if sym = nil then begin
    edDef.Text := {aSymbolKind} '<no definition>';  //to come: search C symbols
    id := Symbols.IndexOf(lbSyms.Items[i]);
  end else begin
    if sym.GlobalID = 0 then
    //no C sym
      s := '<macro>'
    else
      s := '<C sym>';
    s := s + '[' + IntToStr(sym.UniqueID) + '/'  //global STB
      + IntToStr(sym.GlobalID) + ']'; // + EOLstr;  //C symbols
    id := sym.UniqueID;
    if id >= 0 then
      s := s + ' in ' + {sym.loc.src.dir +} sym.loc.src.name
        + ':' + IntToStr(sym.loc.line) + EOLstr;
    s := s + sym.toString; //definition?
    edDef.Text := s;
  end;
//show loc?
  if id >= 0 then
    showSym(id);
end;

procedure TSymView.buTestMacroClick(Sender: TObject);
var
  i:  integer;
  sym:  TSymMacro;
  s:  string;
begin
  i := lbSyms.ItemIndex;
  if i < 0 then
    exit;
  TObject(sym) := lbSyms.Items.Objects[i];
  if (sym <> nil) and (sym.GlobalID = 0) then begin
    case ParseMacro(sym, s) of
    mcFailed: edDef.Text := 'parse failed' + EolStr + s;
    mcEmpty:  edDef.Text := '<empty>';
    else      edDef.Text := s // 'parsed okay'
    end;
  end;
end;

procedure TSymView.FormCreate(Sender: TObject);
begin
//set tabstops in memo
  SetMemoTabs(edDef);
end;

end.
