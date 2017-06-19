program WinToPas;

uses
  Forms,
  fScanLog in 'fScanLog.pas' {ScanLog},
  uUI in 'uUI.pas',
  uParseC in 'uParseC.pas',
  uTablesC in 'uTablesC.pas',
  uXStrings in 'uXStrings.pas',
  uMacros in 'uMacros.pas',
  uScanC in 'uScanC.pas',
  fTypes in 'fTypes.pas' {TypeDefList},
  uToPas in 'uToPas.pas',
  uDirectives in 'uDirectives.pas',
  uFiles in 'uFiles.pas',
  uHashList in 'uHashList.pas',
  uTablesPrep in 'uTablesPrep.pas',
  uTokenC in 'uTokenC.pas',
  fScopeView in 'fScopeView.pas' {ScopeView},
  uLineScan in 'uLineScan.pas',
  fConfiguration in 'fConfiguration.pas' {ConfigViewer},
  fSymView in 'fSymView.pas' {SymView},
  uTranslator in 'uTranslator.pas',
  fMacros in 'fMacros.pas' {MacroChecker};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TScanLog, ScanLog);
  Application.CreateForm(TTypeDefList, TypeDefList);
  Application.CreateForm(TScopeView, ScopeView);
  Application.CreateForm(TConfigViewer, ConfigViewer);
  Application.CreateForm(TSymView, SymView);
  Application.CreateForm(TMacroChecker, MacroChecker);
  Application.Run;
end.
