unit uUI;
(* User interface, console or GUI.
*)

interface

{$IFDEF CONSOLE}
{$ELSE}
uses
  Windows,
  StdCtrls;
{$ENDIF}

type
  eLogKind = (
    lkProgress, //file/line specific messages
    lkConfig,   //file not found, user abort...
    lkSynErr,   //lkDiag, //errors in processed source text
    lkWarning,  //questionable construct in source text
  //app specific:
    lkDebug,  //hooks for debugging...
    lkTrace,  //on demand: fTrace
    lkTodo, //lkWarn - possibly a bug, needs further investigations
    lkBug   //lkErr - bug in this code, not in processed source text!
  );

  eParserFlags = (
    pfNone,
    pfSynErr, //Log(?)
    pfNotConst, //non-const expression
    pfLclVar,   //using variables other than global or parameters
    pfCode,     //contains code, not only an expression
    pfBug,      //analysis unreliable, due to bug in parser...
    pfOther
  );
  sParserFlags = set of eParserFlags;
var
  ParserFlags: sParserFlags;
  fTrace: boolean = False;

const
  aParserFlags: array[eLogKind] of eParserFlags = (
    pfNone, //lkProgress, //file/line specific messages
    pfOther, //lkConfig,   //file not found...
    pfSynErr, //lkSynErr,   //lkDiag, //errors in processed source text
    pfOther, //lkWarning,  //questionable construct in source text
  //app specific:
    pfNone, //lkDebug,  //hooks for debugging...
    pfNone, //lkTrace,  //on demand: fTrace
    pfNone, //lkTodo, //lkWarn - possibly a bug, needs further investigations
    pfBug //lkBug   //lkErr - bug in this code, not in processed source text!
  );

procedure Log(const msg: string; kind: eLogKind = lkProgress);
{$IFDEF CONSOLE}
{$ELSE}
type
  TLog = procedure(const msg: string; kind: eLogKind = lkProgress) of object;
var
  LogLink: TLog;

procedure SetMemoTabs(m: TMemo);
const
  charsPerTab = 2;

{$ENDIF}

const
{$IFDEF WIN32}
  EOLstr = #13#10;
{$ELSE}
  EOLstr = #10;
{$ENDIF}


implementation

{$IFDEF CONSOLE}

var
  s: string;

const //not up-to-date!!!
  MsgPrompt: array[eLogKind] of string = (
    '', '>', 'Warning>', 'Error!!!>'
  );

procedure Log(const msg: string; kind: eLogKind = lkProgress);
begin
  WriteLn(msg);
  if kind <> lkProgress then begin
    Write(MsgPrompt[kind]);
    ReadLn(s);
  end;
end;

{$ELSE}

uses
  Messages;
  
//GUI: supply default logging function (OutputDebugString)

procedure Log(const msg: string; kind: eLogKind = lkProgress);
var
  pf: eParserFlags;
begin
  pf := aParserFlags[kind];
  if pf <> pfNone then
    include(ParserFlags, pf);
  if assigned(LogLink) then
    LogLink(msg, kind);
  //else OutputDebugString(PChar(msg));
end;

procedure SetMemoTabs(m: TMemo);
const
//1 dlg-unit is 1/4 char width
  DlgUnitsPerTab: integer = charsPerTab * 4;
begin
  SendMessage(m.Handle, EM_SETTABSTOPS, 1, integer(@DlgUnitsPerTab));
end;

{$ENDIF}
end.
