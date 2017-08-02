unit fScanLog;
(* A raw GUI for DoDi's C scanner - by DoDi.
This form is designed for debugging purposes only!
A really usable GUI will require much more efforts, contributions are appreciated :-)
At least you'll have to update StartFile and searchdirs as appropriate for your system.
Then run and click File|Test to start scanning the StartFile.
*)

{$DEFINE vlb} //virtual msg listbox?

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, Contnrs,
  uScanC, uFiles, 
  uLineScan, uTablesPrep, uTablesC,
  uUI;

type
  TSyntaxPainter = class
  private
    function FindIdent(x, y: integer): boolean;
    procedure HintSym;
    function MapSym: boolean;
    procedure SetSrc(lst: TStrings; fCSrc: boolean);
    procedure InitWidth;
  {$IFDEF vlb}
  private
    Items:  TStrings;
    procedure lbSrcData(Control: TWinControl; Index: Integer;
      var Data: String);
    function lbSrcDataFind(Control: TWinControl;
      FindString: String): Integer;
    procedure lbSrcDataObject(Control: TWinControl; Index: Integer;
      var DataObject: TObject);
  {$ELSE}
  {$ENDIF}
  protected
  //controls
    lbSrc:  TListBox;
    StatusBar:  TStatusBar;
    lw, gw, cw: integer;
    scanner:  TScanLine;
  //file
    LineStarts: TLineStarts;
  //current token
    row, col: integer;
    w: RScanWord;
    wk: eWordKind;
    sym:  PSymPrep;
    csym: TSymbolC; //under mouse
    procedure lbSrcDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbSrcMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbSrcMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    constructor CreateFor(AListBox: TListBox; ABar: TStatusBar = nil);
  end;

  TScanLog = class(TForm)
    lbMsg: TListBox;
    Splitter1: TSplitter;
    lbSrc: TListBox;
    StatusBar: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Stop1: TMenuItem;
    View1: TMenuItem;
    mnuViewTypes: TMenuItem;
    mnuParse: TMenuItem;
    mnuFileSel: TMenuItem;
    LoadTest1: TMenuItem;
    Test1: TMenuItem;
    mnuTestScan: TMenuItem;
    mnuTestParse: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    edFile: TEdit;
    dlgOpen: TOpenDialog;
    Preprocess1: TMenuItem;
    mnuSaveMetadata: TMenuItem;
    mnuPrepTrad: TMenuItem;
    mnuPrepLock: TMenuItem;
    mnuViewSymbols: TMenuItem;
    Config1: TMenuItem;
    Scopes1: TMenuItem;
    udLinks: TUpDown;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure mnuWinTestClick(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
    procedure lbMsgClick(Sender: TObject);
    procedure mnuViewTypesClick(Sender: TObject);
    procedure mnuParseClick(Sender: TObject);
    procedure mnuTestScanClick(Sender: TObject);
    procedure LoadTest1Click(Sender: TObject);
    procedure mnuFileSelClick(Sender: TObject);
    procedure Preprocess1Click(Sender: TObject);
    procedure mnuSaveMetadataClick(Sender: TObject);
    procedure mnuPrepLockClick(Sender: TObject);
    procedure mnuViewSymbolsClick(Sender: TObject);
    procedure Config1Click(Sender: TObject);
    procedure Scopes1Click(Sender: TObject);
    procedure udLinksClick(Sender: TObject; Button: TUDBtnType);
    procedure LinkPush(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure N2Click(Sender: TObject);
    procedure lbSrcContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    fAbort: boolean;
    fBusy:  boolean;
    fLogProgress: boolean;
    prep: TextFile;
    Painter:  TSyntaxPainter;
    procedure SetFile(const fn: string);
    procedure LogMsg(const msg: string; kind: eLogKind);
    procedure TestScan(const StartFile: string);
    procedure TestParse(const StartFile: string);
    function  MainFile: string;
    procedure PrepLockCallback(change: eLineChange; f: TFileC);
    procedure SetSrc(SrcFile: TFile); overload;
    procedure SetSrc(SrcID: integer); overload;
    procedure ShowLoc(const r: RFileLoc);
    procedure JmpPrev;
    procedure JmpNext;
    procedure JmpSym;
  public
    t0: TDateTime;
    //MainFile: string;
    FileID: integer;
    CurFile:  TFile;
    tc: integer;
    locs: TFileLocs;
    Links:  TObjectList;
    iLink:  integer;
    procedure ShowLocs(const r: TFileLocs);
    procedure StartTime;
    procedure EndTime;
  end;

var
  ScanLog: TScanLog;

implementation

uses
  //fDefines,
  fTypes, fSymView,
  fConfiguration, fScopeView,
  fMacros,
  uTokenC, uParseC, //uTablesC,
  uToPas;

{$R *.DFM}

type
  TFilePos = class
  public
    loc: RFileLoc;
    constructor Create(AFile: TFile; ALine: integer);
  end;

  TFilePos2 = class(TFilePos)
  public
    dst: RFileLoc;
    property src: RFileLoc read loc write loc;
  end;

{ TFilePos }

const
  MonitorFile = 'Monitor.txt';

constructor TFilePos.Create(AFile: TFile; ALine: integer);
begin
  loc.id := AFile.id;
  loc.line := ALine;
end;

{ TScanLog }

procedure TScanLog.LogMsg(const msg: string; kind: eLogKind);
var
  pos: TFilePos;
  i: integer;
begin
  case kind of
  lkProgress:
    if fLogProgress then begin
      //if lbMsg.Items.Count > 100 then lbMsg.Clear;  //destroy objects?
      self.lbMsg.Items.Add(msg);  //no objects?
    end;
  else  //log message, remember and show position
    if TokenStack.CurFile <> nil then begin
    //CurFile.src seems to be invalid, sometimes???
      if TokenStack.CurFile.src <> nil then begin
        if ScanToken.line > 0 then
          pos := TFilePos.Create(TokenStack.CurFile.src, ScanToken.line)
        else
          pos := nil;
      end else
        pos := nil; //debug - msg not while parsing?
      lbMsg.AddItem(ScanningFile + ':' + IntToStr(ScanningLine), pos); // nil);
      self.lbMsg.Items.AddObject(msg, pos);
    //scroll into view
      i := lbMsg.Items.Count - 5;
      if i < 0 then
        i := 0;
      lbMsg.TopIndex := i;
    end else
      self.lbMsg.Items.Add(msg);
    Application.ProcessMessages;
  end;
end;

procedure TScanLog.FormCreate(Sender: TObject);
var
  lst: TStringList;
begin
  Application.ProcessMessages;
  uUI.LogLink := self.LogMsg;
  uTablesPrep.SymViewer := self.ShowLocs;
  Links := TObjectList.Create(True); //contains TFilePos
  //iLink := -1;  //current link: stack empty
  Painter := TSyntaxPainter.CreateFor(self.lbSrc, self.StatusBar);
//file option?
  if ParamCount >= 1 then
    edFile.Text := ParamStr(1);
//restore the last Monitor file/line
  if FileExists(MonitorFile) then begin
    lst := TStringList.Create;
    lst.LoadFromFile(MonitorFile);
    if lst.Count > 1 then begin
      //lst.CommaText := lst[0]; //split file,line
      MonitoringFile := lst[0];
      MonitoringLine := StrToInt(lst[1]);
    end;
    lst.Free;
  end;
end;

procedure TScanLog.lbSrcContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  lst: TStringList;
begin
//try set monitor location
  MonitoringFile := self.CurFile.name;
  MonitoringLine := lbSrc.ItemIndex + 1; //source lines are 1-based
//save the current monitor, for use on next program start
  lst := TStringList.Create;
  lst.Add(MonitoringFile);
  lst.Add(IntToStr(MonitoringLine));
  lst.SaveToFile(MonitorFile);
  lst.Free;
end;

procedure TScanLog.Exit1Click(Sender: TObject);
begin
  fAbort := True;
  Close;
end;

(* TestScan - scan predefined file with predefined search path.
  A once-only procedure, as it is (nothing reset)
*)
procedure TScanLog.TestScan(const StartFile: string);
var
  SrcFile: TFile;
begin
  Screen.Cursor := crHourGlass;
  fBusy := True;
  fLogProgress := False;
//open (test) file
  tc := 0;
  ScanFile(StartFile);
  FileID := -1; // NoFile; //force file change detect
//scan
  try
    while nextTokenC <> t_eof do begin
      inc(tc);
      case ScanToken.kind of
      t_bol:
        begin
          //WriteLn(prep);
          self.StatusBar.Panels[0].Text := IntToStr(ScanToken.line);
          if ScanToken.f <> FileID then begin
          //log switch to included file, or #line directive (if implemented?!)
            SrcFile := Files.GetFile(ScanToken.f);
            if ScanToken.f > FileID then
            //only log newly opened files
              LogMsg('processing ' + SrcFile.name, lkProgress);
            FileID := ScanToken.f;
            //WriteLn(prep, '---', SrcFile.dir, SrcFile.name, '---');
            self.StatusBar.Panels[1].Text := SrcFile.dir + SrcFile.name;
            SetSrc(SrcFile); //self.lbSrc.Items := SrcFile;
            Application.ProcessMessages;
            if fAbort then
              break; //ProcessMessages is not enough!
          end;
        end;
      end;
    end;  //while
    self.LogMsg('Scan finished', lkProgress);
  except
    self.LogMsg('Scan aborted', lkProgress);
  end;
  Screen.Cursor := crDefault;
  fBusy := False;
end;

//procedure TScanLog.SetSrc(SrcFile: TStrings);
procedure TScanLog.SetSrc(SrcFile: TFile);
begin
  CurFile := SrcFile;
  FileId := SrcFile.id;
  Painter.SetSrc(SrcFile, True);
end;

procedure TScanLog.SetSrc(SrcID: integer);
begin
  CurFile := Files.GetFile(SrcID);
  SetSrc(CurFile);
end;

procedure TScanLog.mnuTestScanClick(Sender: TObject);
//const StartFile = 'scantest.c';
begin
  TestScan(MainFile);
end;

procedure TScanLog.TestParse(const StartFile: string);
begin
  StartTime;
  Screen.Cursor := crHourGlass;
  FileID := -1;
  fAbort := False;
  self.LogMsg('Start parsing ' + StartFile, lkProgress);
  fLogProgress := True;
  ParseCMain(StartFile, True);
  if fAbort then
    self.LogMsg('Scan aborted', lkConfig)
  else
    self.LogMsg('Scan finished', lkProgress);
//log results?
  //Globals.SaveToFile(ChangeFileExt(StartFile, '.txt'));
  //Statics.Save...ToFile(ChangeFileExt(StartFile, '.txt'));
  Modules.Save;
  EndTime;
  Screen.Cursor := crDefault;
end;

procedure TScanLog.mnuParseClick(Sender: TObject);
const StartFile = 'parsetest.c';
begin
  TestParse(StartFile);
end;


procedure TScanLog.mnuWinTestClick(Sender: TObject);
//const StartFile = 'wintest.c';
begin
  TestParse(MainFile);
end;

procedure TScanLog.Stop1Click(Sender: TObject);
begin
  fAbort := True;
end;

procedure TScanLog.ShowLoc(const r: RFileLoc);
var
  i: integer;
begin
try //don't know when/why r.src can be invalid :-(
//try: replace TFile by string (filename)
  if (r.line <= 0) or (not r.valid) then // or (cardinal(r.src) < $400000) then
    exit; //???
//show file
  if r.id <> FileID then begin
  //load file
    SetSrc(r.id);  //self.lbSrc.Items := pos.f;
    StatusBar.Panels[1].Text := CurFile.dir + CurFile.name;
  end;
  i := r.line - 1; //start index at 0, line numbers at 1 (???)
//scroll into view - raw guess
  if i > 5 then
    lbSrc.TopIndex := i - 5
  else
    lbSrc.TopIndex := 0;
  lbSrc.ItemIndex := i;
//update status bar
  StatusBar.Panels[0].Text := IntToStr(r.line);
except
  //don't know when/why r.src can be invalid :-(
end;
end;

procedure TScanLog.lbMsgClick(Sender: TObject);
var
  i: integer;
  o: TObject;
  pos: TFilePos absolute o;
begin
  if fBusy then
    exit;
//show msg?
  i := self.lbMsg.ItemIndex;
  if i < 0 then
    exit;
//show filepos?
  o := lbMsg.Items.Objects[i];
  if (o <> nil) and (o is TFilePos) then begin
    ShowLoc(pos.loc);
  end;
end;

procedure TScanLog.mnuViewTypesClick(Sender: TObject);
begin
  TypeDefList.Show;
  TypeDefList.ShowTypes;
end;

(* This version is for loading everything from file!
  No more applicable, file only contains procedures, no types etc.!
*)
procedure TScanLog.LoadTest1Click(Sender: TObject);
var
  s: string;
begin
  StartTime;
  Screen.Cursor := crHourGlass;
  s := ChangeFileExt(MainFile, '.txt');
  while not FileExists(s) do begin
    fAbort := False;
    TestParse(MainFile);
    if fAbort then
      exit;
  end;
  fLogProgress := True;
{$IFDEF old}
  LogMsg('Convert ' + s, lkProgress);
  Globals.LoadFromFile(s);
{$ELSE}
  LogMsg('Translate...', lkProgress);
{$ENDIF}
  uToPas.ToPas(MainFile);
  self.LogMsg('Conversion finished', lkProgress);
  EndTime;
  Screen.Cursor := crDefault;
end;

procedure TScanLog.mnuFileSelClick(Sender: TObject);
begin
  if dlgOpen.Execute then begin
    SetFile(dlgOpen.FileName);
  end;
end;

procedure TScanLog.SetFile(const fn: string);
begin
  edFile.Text := fn;
end;

function TScanLog.MainFile: string;
begin
  Result := self.edFile.Text;
end;

procedure TScanLog.Preprocess1Click(Sender: TObject);
var
  s: string;
  SrcFile: TFile;
  indent: integer;  //>= 0 after BOL token, -1 after indentation has been shown.
  lc: integer;
  lastLn, nextLn: integer;

const
  fCompress = True;
begin
  Screen.Cursor := crHourGlass;
  fBusy := True;
  fLogProgress := False;
  fWantRems := True;
//open (test) file
  tc := 0;
  lc := 0;
  indent := 0; //?
  s := MainFile;
  ScanFile(s);
  AssignFile(prep, ChangeFileExt(s, '.i'));
  Rewrite(prep);
  FileID := -1; //force file change detect
  lastLn := 0;
  nextLn := 0;
//scan
  try // ... finally
    try // ... except
      while nextTokenC <> t_eof do begin
        inc(tc);
        case ScanToken.kind of
        t_empty:  ; //skip - unless fine for debugging
        //t_eof:  WriteLn(prep, '<eof>'); //should never be reached
        t_bol:
          begin
            inc(lc);
            if not fCompress then
              WriteLn(prep);
            indent := ScanToken.len;
            self.StatusBar.Panels[0].Text := IntToStr(ScanToken.line);
            if ScanToken.f <> FileID then begin
            //log switch to included file, or #line directive (if implemented?!)
              SrcFile := Files.GetFile(ScanToken.f);
              if ScanToken.f > FileID then
              //only log newly opened files
                LogMsg('processing ' + SrcFile.name, lkProgress);
              FileID := ScanToken.f;
              WriteLn(prep);  //assert file switch on new line
              Write(prep, '#line ', ScanToken.Line, ' "', SrcFile.dir, SrcFile.name, '"');
              lastLn := ScanToken.line; //just shown
              self.StatusBar.Panels[1].Text := SrcFile.dir + SrcFile.name;
              SetSrc(SrcFile); //self.lbSrc.Items := SrcFile;
            end;
            nextLn := ScanToken.line;
          //move the following lines into the above "if" if too slow
            Application.ProcessMessages;
            if fAbort then
              break; //ProcessMessages is not enough!
          end;
        t_NoEol:
          begin
            inc(lc);
            WriteLn(prep, '\');
            indent := ScanToken.len;
          end;
        else
          s := TokenString(indent >= 0);  //first?
          if s <> '' then begin
            if nextLn > lastLn + 1 then begin
              WriteLn(prep);
              Write(prep, '#line ', nextLn);  //, ' "', SrcFile.dir, SrcFile.name, '"');
            end;
            lastLn := nextLn;
            if indent >= 0 then begin
              WriteLn(prep);
              if indent > 0 then
                Write(prep, StringOfChar(' ', indent));
              indent := -1; //indentation shown
            end;
            Write(prep, s);
          end;
        end;
      end;  //while
      WriteLn(prep);
      WriteLn(prep, '//EOF');
      self.LogMsg('Scan finished after ' + IntToStr(tc) + ' tokens, '
        + IntToStr(lc) + ' lines', lkProgress);
    except
      self.LogMsg('Scan aborted', lkConfig);
    end;
  finally
    CloseFile(prep);
    fWantRems := False;
    Screen.Cursor := crDefault;
    fBusy := False;
  end;
end;

procedure TScanLog.mnuSaveMetadataClick(Sender: TObject);
begin
  Globals.SaveToFile(ChangeFileExt(MainFile, '.txt'));
end;

procedure TScanLog.mnuPrepLockClick(Sender: TObject);
var
  cbOld:  TLineCallback;
  s:  string;
begin
  Screen.Cursor := crHourGlass;
  cbOld := uScanC.OnLineChange;
  uScanC.OnLineChange := PrepLockCallback;
  try
  //process file
    s := MainFile;
    AssignFile(prep, ChangeFileExt(s, '.l'));
    Rewrite(prep);
    //WriteLn(prep, '//start...');
    ScanFile(s);
  //parse it
    while nextTokenC <> t_eof do
      {everything done in callback} ;
    WriteLn(prep, '// EOF');
  finally
    CloseFile(prep);
  //restore line change callback...
    uScanC.OnLineChange := cbOld;
    Screen.Cursor := crDefault;
  end;
end;

procedure TScanLog.PrepLockCallback(change: eLineChange; f: TFileC);
var
  SrcFile:  TFile;
begin
(* synchronization problems:
When a file is opened, the first line is read during construction!
  This means that the first line should not be logged
  on lcNewLine (suppressed in nextLine), but instead on lcOpenFile.
When a file is closed, rewind() will result in a report of line 1.

Solution:
  Line 1 is never reported from nextLine.
  Instead TokenStack.push() reports lcOpenFile,
    whereupon #line and then the first line can be written.

The lock level seems to be out of sync???
  Reason: #if evaluates the condition until EOL,
    reading the next line prior to finishing the evaluation and adjustment of
    the lock counter.
  Solution:
    A flag is set during evaluation of a condition, preventing lcNewLine.
    At the end of the directive handling the flag is tested...

Lock level description:
0     normal processing.
1     waiting in False condition branch, for #else, #elif or #endif.
2     waiting for #endif, after True branch processed.
else  multiple #if with False condition (every added #if counts 2 lock levels).
*)
  case change of
  lcOpenFile, lcResumeFile:
    begin
      SrcFile := f.src;
      WriteLn(prep, '#line ', f.CurLine, ' "', SrcFile.dir, SrcFile.name, '"');
      if change = lcOpenFile then
        WriteLn(prep, f.LineText);
    end;
  lcCloseFile:  //debugging only
    WriteLn(prep, '//EOF "', f.src.name, '"');
  lcNewLine:
    begin
      if f.LockCount > 0 then
        Write(prep, StringOfChar('>', f.LockCount));
      WriteLn(prep, f.LineText);
    end;
  end;
end;

procedure TScanLog.mnuViewSymbolsClick(Sender: TObject);
begin
  SymView.ShowUnsorted;
  SymView.Show;
end;

procedure TScanLog.ShowLocs(const r: TFileLocs);
var
  i: integer;
begin
  locs := r;
  for i := low(locs) to high(locs) do begin
    if locs[i].valid then begin
      self.ShowLoc(locs[i]);
      exit;
    end;
  end;
end;

procedure TScanLog.Config1Click(Sender: TObject);
begin
  ConfigViewer.Show;
end;

procedure TScanLog.Scopes1Click(Sender: TObject);
begin
  ScopeView.Show;
end;

procedure TScanLog.udLinksClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
  btNext: JmpNext;
  btPrev: JmpPrev;
  end;
end;

procedure TScanLog.JmpNext;
var
  //sym:  TSymMacro;
  loc:  TFilePos2;
begin
//first: [0].dst
//[i].dst, ++i
  if iLink >= Links.Count then
    iLink := Links.Count //invalid move
  else begin
    TObject(loc) := Links.Items[iLink];
    self.ShowLoc(loc.dst);
    inc(iLink);
  end;
end;

procedure TScanLog.JmpPrev;
var
  //sym:  TSymMacro;
  loc:  TFilePos2;
begin
//--i, [i].src
  dec(iLink);
  if iLink < 0 then
    iLink := 0 //invalid move
  else begin
    TObject(loc) := Links.Items[iLink];
    self.ShowLoc(loc.src);
  end;
end;

procedure TScanLog.JmpSym;
var
  sym:  TSymbol;  //TSymMacro;
  pos:  TFilePos2;
begin
  if Painter.csym <> nil then
    sym := Painter.csym //highest priority (C symbol)
  else if (self.Painter.sym <> nil) then
    sym := Painter.sym.FMacro //C or preprocessor symbol
  else
    sym := nil;
  if (sym = nil) or not sym.loc.valid then
    exit;
//truncate the stack
  Links.Count := iLink;
//push return address
  pos := TFilePos2.Create(self.CurFile, Painter.row + 1);
  pos.dst := sym.loc;
  Links.Add(pos); //return address
  //iLink := Links.Count;
  udLinks.Position := Links.Count;
  JmpNext;
  //self.ShowLoc(sym.loc);
end;

procedure TScanLog.EndTime;
var
  dt: TDateTime;
  s: string;
  i: integer;
begin
  dt := now-t0; //timediff in days
  dt := dt * 24 * 60 * 60; //in seconds
  str(dt:2:3, s);
  self.LogMsg('in '+s, lkTrace); //lkProgress clears the display!

{//clears the display???
  //scroll into view
  i := lbMsg.Items.Count - 5;
  if i > 0 then
    lbMsg.TopIndex := i;
}
end;

procedure TScanLog.StartTime;
begin
  t0 := now;
end;

{ TSyntaxPainter }

const
  fLineNumbers = True;  //show line numbers?

const
  SymTypes: array[eSymbolKind] of string = (
    'Sym ',
    'CKey ',
    'Const ',
    'Type ',
    'Proc ',
    'Macro '
  );

type
  eWordStyle = (
    wsOther,
    wsNoEol,
    wsRem, wsPrep,
    wsNum, wsStr,
    wsOp, wsPunct,
    wsIdent, wsMac, wsKey
  );
  RWordStyle = record
    fg, bg: TColor;
    attrs: TFontStyles;
  end;

const
  StyleMap: array[eWordKind] of eWordStyle = (
  //line ends
    wsOther,  //wkEol,
    wsNoEol,  //wkNoEol,  //escaped EOL
    wsRem,  //wkRem,  //terminated rem
    wsPrep, //wkPrep, //C:#... (unless #define?) P:{$ (*$
  //continued
    wsPrep, wsPrep, //wkPrep1, wkPrep2,   //C:1?..\, P:1={$.. 2=(*$..
    wsRem, wsRem,   //wkRem1, wkRem2, //C:2=/* P:1={... 2=(*...
  //normal
    wsOther,  //wkOther,
    wsNum,    //wkNum,
    wsStr, wsStr,   //wkStr1, wkStr2, //delimiter ' or "
    wsOp, wsPunct,  //wkOp, wkPunct,  //optional?
    wsIdent,  //wkIdent,  //unclassified
  //interpreted identifiers
    wsMac,  //wkMac,
    wsKey  //wkKey
  );

  WordStyles: array[eWordStyle] of RWordStyle = (
    (fg:clBlack), //wsOther,
    (fg:clRed; attrs:[fsBold]), //wsNoEol,
    //(fg:clLtGray; attrs:[fsItalic]), //wsRem,
    (fg:clNavy; attrs:[fsItalic]), //wsRem,
    //(fg:clGreen; bg:clYellow; attrs:[fsItalic]), //wsPrep,
    (fg:clBlack; bg:clYellow), //wsPrep,
    (fg:clMaroon), //wsNum,
    (fg:clDkGray), //wsStr,
    (fg:clRed; attrs:[fsBold]), //wsOp,
    (fg:clGreen; attrs:[fsBold]), //wsPunct,
    //(fg:clBlue; attrs:[fsUnderline]), //wsIdent,
    //(fg:clBlue; attrs:[fsBold]), //wsIdent,
    (fg:clBlue), //wsIdent,
    (fg:clTeal), //wsMac,
    (fg:clBlack; attrs:[fsBold]) //wsKey
  );

constructor TSyntaxPainter.CreateFor(AListBox: TListBox; ABar: TStatusBar);
begin
  StatusBar := ABar;
  lbSrc := AListBox;
  lbSrc.Style := lbOwnerDrawFixed;
  lbSrc.OnDrawItem := self.lbSrcDrawItem;
  lbSrc.OnMouseMove := self.lbSrcMouseMove;
  //lbSrc.OnMouseUp := self.lbSrcMouseUp;
{$IFDEF vlb}
  lbSrc.OnData := self.lbSrcData;
  lbSrc.OnDataFind := self.lbSrcDataFind;
  lbSrc.OnDataObject := self.lbSrcDataObject;
{$ELSE}
{$ENDIF}
//default init - canvas doesn't work?
  //cw := lbSrc.Canvas.TextWidth(' ');  //if first call or font changed
  SetSrc(nil, True);
end;

procedure TSyntaxPainter.SetSrc(lst: TStrings; fCSrc: boolean);
begin
  if lst = nil then begin
  //non-virtual
    lbSrc.Style := lbOwnerDrawFixed;
    Items := lbSrc.Items;
  end else begin
  //virtual
    lbSrc.Style := lbVirtualOwnerDraw;
    Items := lst;
    //lbSrc.Items := Items;
    lbSrc.Count := Items.Count;
  end;
//line starts
  if fCSrc then
    scanner := CScanner
  else
    scanner := DefaultScanner;
  lw := scanner.InitLines(Items, LineStarts);
  cw := 0;  //force init from within Paint handler!
  //lbSrc.HandleNeeded; //helps?
  //InitWidth;
  lbSrc.Refresh;
  //InitWidth;
end;

procedure TSyntaxPainter.InitWidth;
begin
(* Nothing helps!
  The text characteristics must be retrieved from within the paint handler!
*)
//default init
  cw := lbSrc.Canvas.TextWidth('M');  //if first call or font changed
//init content specifics
  if fLineNumbers then
    gw := (Length(IntToStr(Items.Count)) + 1) * cw
  else
    gw := 0;
  lbSrc.ScrollWidth := gw + lw*cw; //allow to scroll horizontally
end;

procedure TSyntaxPainter.lbSrcDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  s: string;
  ws: eWordStyle;
  FCanvas: TCanvas;

  procedure LineNumber;
  begin
    //if cw = 0 then InitWidth;
    if gw > 0 then begin
      FCanvas.Font.Color := clWindowText;
      FCanvas.TextOut(Rect.Left, Rect.Top, IntToStr(Index + 1));
      inc(Rect.Left, gw);
    end;
  end;

begin
  FCanvas := lbSrc.Canvas;
  if cw = 0 then
    InitWidth;  //required, but only once.
//current position
  if FCanvas.Brush.Color <> clWindow then begin
  //all but the selected line use clWindow
    FCanvas.Brush.Color := clAqua;  // clInfoBk;  //light yellow
  end;
  FCanvas.FillRect(Rect);
  //if Index < Items.Count then begin
//syntax highlighting
  if Index < Length(LineStarts) then begin
    LineNumber;
    s := Items[Index];
    //s := StringReplace(s, #9, ' ', [rfReplaceAll]);
    if scanner = nil then begin
      FCanvas.TextOut(Rect.Left, Rect.Top, s);  //updates PenPos
    end else begin
      w.txt := s;
      wk := scanner.firstWord(w, LineStarts[index]);
    //special background? (assume bg affects whole line?)
      ws := StyleMap[wk];
      if WordStyles[ws].bg <> WordStyles[wsOther].bg then begin
        FCanvas.Brush.Color := WordStyles[ws].bg;
        FCanvas.FillRect(Rect);
      end;
    //debug!
      //if wk = wkRem then  wk := scanner.firstWord(w, LineStarts[index]);
      while wk > wkEol do begin
      //problem: break after some line-ends <> wkEol?
        if w.iRight > w.iLeft then begin
          MapSym;
          ws := StyleMap[wk];
          FCanvas.Font.Color := WordStyles[ws].fg;  // clBlack;
          FCanvas.Font.Style := WordStyles[ws].attrs; // [fsBold];
          TextOut(FCanvas.Handle, Rect.Left + w.iLeft*cw, Rect.Top,
            @s[w.iLeft+1], w.iRight - w.iLeft);
        end; //else ws := wsOther;
        wk := scanner.nextWord(w);
      end;
    end;
  end;
end;

procedure TSyntaxPainter.lbSrcMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then begin
    if lbSrc.ItemIndex >= 0 then
      lbSrc.ItemIndex := -1;
    if FindIdent(x,y) then begin
      HintSym;
    end else if StatusBar <> nil then
      StatusBar.Panels[1].Text := '';
  end;
end;

procedure TSyntaxPainter.lbSrcMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) and FindIdent(x,y) {and MapSym} then
    HintSym;  //or show definition???
end;

procedure TScanLog.LinkPush(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) then
    JmpSym;
end;

function TSyntaxPainter.FindIdent(x, y: integer): boolean;
begin
  Col := ((X - gw) div cw) + 1;
  Row := (Y div lbSrc.ItemHeight) + lbSrc.TopIndex;
  if StatusBar <> nil then
    StatusBar.Panels[0].Text := IntToStr(Row) + ':' + IntToStr(Col);
  if (Col > 0) and (Row < lbSrc.Items.Count) and assigned(scanner) then begin
    w.txt := lbSrc.Items[Row];
    wk := scanner.firstWord(w, LineStarts[Row]);
    while wk > wkNoEol do begin
      if w.iRight >= Col then begin
        Result := (w.iLeft < Col) and (wk = wkIdent);
        if Result then
          MapSym;
        exit;
      end;
      wk := scanner.nextWord(w);
    end;
  end;
  Result := False;
end;

function TSyntaxPainter.MapSym: boolean;
var
  i:  integer;
begin
  Result := False;
  if w.wkind = wkIdent then begin
    i := Symbols.FindFirst[scanner.TokenStr(w)];
    Result := i >= 0;
    if Result then begin
      sym := Symbols.getSymbol(i);
      case sym.mackind of
      skCKey:   wk := wkKey;
      skMacro:  wk := wkMac;
      end;
    end else
      sym := nil;
  end;
end;

procedure TSyntaxPainter.HintSym;
var
  //csym: TSymbolC;
  s:  string;
begin
  s := scanner.TokenStr(w);
  csym := Globals.getSym(s);
  if StatusBar <> nil then begin
    if csym <> nil then begin
      StatusBar.Panels[1].Text :=
        SymKinds[csym.kind] + csym.TypedName;
    end else if sym = nil then
      StatusBar.Panels[1].Text := 'ident ' + s
    else begin
      StatusBar.Panels[1].Text := SymTypes[sym.mackind] + sym.FString;
    end;
  end;
end;

{$IFDEF vlb}
function TSyntaxPainter.lbSrcDataFind(Control: TWinControl;
  FindString: String): Integer;
begin
  if Items <> nil then begin
    Result := Items.IndexOf(FindString);
  end else
    Result := -1;
end;

procedure TSyntaxPainter.lbSrcDataObject(Control: TWinControl; Index: Integer;
  var DataObject: TObject);
begin
  if Items <> nil then begin
    DataObject := Items.Objects[Index];
  end else
    DataObject := nil;
end;

procedure TSyntaxPainter.lbSrcData(Control: TWinControl; Index: Integer;
  var Data: String);
begin
  if Items <> nil then begin
    Data := Items.Strings[Index];
    Data := StringReplace(Data, #9, ' ', [rfReplaceAll]);
  end else
    Data := '';
end;
{$ELSE}
{$ENDIF}

procedure TScanLog.N2Click(Sender: TObject);
begin
  MacroChecker.Init;
end;

end.
