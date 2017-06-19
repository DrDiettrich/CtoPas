unit fMacros;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TMacroChecker = class(TForm)
    edMac: TMemo;
    pnlConst: TGroupBox;
    pnlProc: TGroupBox;
    pnlUnk: TGroupBox;
    Panel1: TPanel;
    lbMac: TListBox;
    procedure lbMacClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Init;
  end;

var
  MacroChecker: TMacroChecker;

implementation

{$R *.dfm}

uses
  uTablesPrep;

{ TMacroChecker }

procedure TMacroChecker.Init;
var
  i: integer;
  p: PSymPrep;
  mac: TSymMacro;
  tokens: TTokenArray;
begin
  lbMac.Clear;
  for i := 0 to symbols.count - 1 do begin
    p := Symbols.getSymbol(i);
    mac := p.GetMacro;
    if mac <> nil then begin
      tokens := mac.GetTokens;
      if (tokens <> nil) and (tokens.Count > 0) then begin
        lbMac.AddItem(p.FString, mac);
      end;
    end;
  end;
  self.Show;
end;

procedure TMacroChecker.lbMacClick(Sender: TObject);
var
  i: integer;
  lb: TListBox absolute Sender;
  mac: TSymMacro;
begin
  edMac.Clear;
  i := lb.ItemIndex;
  if i < 0 then
    exit;
  TObject(mac) := lb.Items.Objects[i];
  edMac.Text := mac.BodyString;
end;

end.
