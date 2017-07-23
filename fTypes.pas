unit fTypes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  config //{$INCLUDE config}
  ;

type
  TTypeDefList = class(TForm)
    lbTypes: TListBox;
  private
    { Private declarations }
  public
    procedure ShowTypes;
  end;

var
  TypeDefList: TTypeDefList;

implementation

uses
  uXStrings, uTablesC;

{$R *.DFM}

{ TTypeDefList }

procedure TTypeDefList.ShowTypes;
var
  i: integer;
  types:  TGlobalScope;
  sym:  TTypeDef;
begin
  Screen.Cursor := crHourGlass;
  lbTypes.Visible := False;
  lbTypes.Clear;
  lbTypes.Items.Capacity := Globals.TypeCount; //actually is Count
//dump Types
  types := Globals;
  for i := 0 to types.Count - 1 do begin
    sym := types.getType(i);
    if sym <> nil then
      self.lbTypes.Items.Add(sym.toString);
  end;
  lbTypes.Items.Capacity := lbTypes.Items.Count; //shrink to required
  lbTypes.Visible := True;
  Screen.Cursor := crDefault;
end;

end.
