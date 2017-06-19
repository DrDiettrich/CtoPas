unit fConfiguration;
(* intended purpose:
  user configuration editor.
The files (TFile) are stored in the Tab.Objects,
initalized during project processing (#include *.def...)
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls,
  uFiles;

type
  TConfigViewer = class(TForm)
    lbConfig: TMemo;
    PopupMenu1: TPopupMenu;
    Save1: TMenuItem;
    Undo1: TMenuItem;
    lbTabs: TTabControl;
    Label1: TLabel;
    edFile: TEdit;
    procedure Save1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lbTabsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbConfigChange(Sender: TObject);
  protected
    Changed: boolean;
    CurFile: TFile;
    CurIndex: integer;
    procedure SaveChanges;
    procedure ShowFile(i: integer);
  public
    procedure StartProject(f: TFile);
  end;

var
  ConfigViewer: TConfigViewer;

implementation

{$R *.dfm}

{ TConfigViewer }

procedure TConfigViewer.FormActivate(Sender: TObject);
begin
  //if FileName = '' then ShowFile(''); //open the default file
  //lbTabs.TabIndex := 0;
  ShowFile(0);
end;

procedure TConfigViewer.FormCreate(Sender: TObject);
begin
  Changed := False;
  CurIndex := -1;
  CurFile := nil;
  uFiles.ConfigFiles := self.lbTabs.Tabs;
end;

procedure TConfigViewer.StartProject(f: TFile);
begin
  //SaveChanges; - maybe too late!
  Changed := False;
  lbConfig.Clear;
  lbTabs.Tabs.Clear;
  lbTabs.Tabs.AddObject(f.name, f);
  lbTabs.TabIndex := 0; //trigger file display?
end;

procedure TConfigViewer.SaveChanges;
begin
  if not Changed then
    exit;
  Changed := False;
  if CurFile <> nil then
    CurFile.UpdateFrom(lbConfig.Lines);
end;

//procedure TConfigViewer.ShowFile(const fn: string);
procedure TConfigViewer.ShowFile(i: integer);
begin
  if (i < 0) or (i >= lbTabs.Tabs.Count) then
    exit; //invalid call
  SaveChanges;
  lbConfig.Clear;
  edFile.Text := '';
  CurIndex := i;
  CurFile := lbTabs.Tabs.Objects[i] as TFile;
  if CurFile = nil then
    exit;
  edFile.Text := CurFile.dir+CurFile.name;
  lbConfig.Lines.Assign(CurFile); //.LoadFromFile(FileName);
  Changed := False;
{
  if fn <> '' then
    FileName := fn
  else if FileName = '' then
    FileName := ExtractFilePath(Application.ExeName) + 'user.def';
}
  Show; //if not already visible
end;

procedure TConfigViewer.Save1Click(Sender: TObject);
begin
  SaveChanges;
end;

procedure TConfigViewer.Undo1Click(Sender: TObject);
begin
  Changed := False;
  ShowFile(CurIndex);
end;

procedure TConfigViewer.lbTabsChange(Sender: TObject);
begin
  SaveChanges;
  ShowFile(lbTabs.TabIndex);
end;

procedure TConfigViewer.lbConfigChange(Sender: TObject);
begin
//??? called during loading, for every line???
  if lbConfig.Modified then
    Changed := True;
end;

end.
