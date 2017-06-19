unit uDirectives;
(* Collections for preprocessor directive and pragma handlers.
Hooks for adding app specific handlers:
  registerDirective
  registerPragma
*)

interface

uses
  uXStrings,
  uScanC;

type
//preprocessor directives
  eDirectives = (
    pdNULL, //# (only)
    pdEndif, pdIf, pdIfDef, pdIfNDef, pdElif, pdElse,
    pdDefine, pdUndef, pdInclude, pdIncludeNext,
    pdError, pdLine,
    pdWarning,
    pdPragma,
  //external
    pdUser
  );
  eStdDirectives = pdEndif..pred(pdUser);

  eDirectiveAttrs = (
    daExpression, //expect const_expression argument
    daInclude     //expect #include filespec
  );

  TDirectiveHandler = procedure(f: TFileC);

  TDirective = class(TXStrings)
  public
    Handler:  TDirectiveHandler;
    kind:     eDirectives;
    attrib:   eDirectiveAttrs;
  end;

  TDirectives = class(TDict)
  public
    function  getDirective(index: integer): TDirective;
  end;

  ePragmas = (
    psNULL,
  //built in
    psInclude,
    psSource,
    psModule,
  //external
    psUser //default - user defined
  );
  eStdPragmas = psInclude..pred(psUser);

  TPragma = class(TXStrings)
  public
    Handler:  TDirectiveHandler;
    kind: ePragmas;
  end;

  TPragmas = class(TDict)
  public
    function  getPragma(index: integer): TPragma;
  end;

//add (app specific) directives and pragmas
function  registerDirective(const name: string; proc: TDirectiveHandler; attrib: eDirectiveAttrs): TDirective;
function  registerPragma(const name: string; kind: ePragmas; proc: TDirectiveHandler): TPragma;

var
  Directives: TDirectives;
  Pragmas:  TPragmas;


implementation

uses
  uTablesPrep,
  uUI, uHashList;

type
  TDirRec = record
    n:  string;
    a:  eDirectiveAttrs;
  end;
const //all handlers implemented in uScanC.pas
  aDirectives: array[eStdDirectives] of TDirRec = (
  //blocking
    (n: 'endif'), (n: 'if'), (n: 'ifdef'), (n: 'ifndef'),
    (n: 'elif'), (n: 'else'),
  //other
    (n: 'define'), (n: 'undef'),
    (n: 'include'; a: daInclude), (n: 'include_next'; a: daInclude),
    (n: 'error'),
    (n: 'line'),
    (n: 'warning'),
    (n: 'pragma')
  );

procedure InitDirectives;
var
  d:    eStdDirectives;
  dir:  TDirective;
begin
  InitSymbols;  //Symbols table must exist!
//init directives
  Directives := TDirectives.Create('directives');
  {dir :=} TDirective.CreateIn(Directives, ''); //dummy NULL directive
  for d := low(d) to high(d) do begin
    dir := TDirective.CreateIn(Directives, aDirectives[d].n); //, True);
    dir.kind := d;
    dir.attrib := aDirectives[d].a;
    Symbols.addDirective(aDirectives[d].n, dir.ListIndex);
  end;
end;

function  registerDirective(const name: string; proc: TDirectiveHandler;
  attrib: eDirectiveAttrs): TDirective;
begin
  Result := TDirective.CreateIn(Directives, name);  //, False);
  Result.kind := pdUser;
  Result.attrib := attrib;
  Result.Handler := proc;
  Symbols.addDirective(name, Result.ListIndex);
end;

{ TDirectives }

function TDirectives.getDirective(index: integer): TDirective;
begin
  TObject(Result) := GetItem(index).FObject;
end;

//--- pragmas ---

(* aPragmas - the identifiers of the builtin pragmas.
  The mixed case of these identifiers is intended; it's very unlikely that such
  names conflict with pragma names of other C compilers.
*)
const
  aPragmas: array[eStdPragmas] of string = (
    'Include',  //include path
    'Source',   //source path
    'Module'    //translation unit
  );

function  registerPragma(const name: string; kind: ePragmas; proc: TDirectiveHandler): TPragma;
begin
  Result := TPragma.CreateIn(Pragmas, name);
  Result.kind := kind;
  Result.Handler := proc;
  Symbols.addPragma(name, Result.ListIndex);
end;

procedure InitPragmas;
var
  i:  eStdPragmas;
  p:  TPragma;
begin
  Pragmas := TPragmas.Create('pragmas');
  {p :=} TPragma.CreateIn(Pragmas, ''); //dummy
  for i := low(i) to high(i) do begin
    p := registerPragma(aPragmas[i], i, nil);
    p.kind := i;
  end;
end;

{ TPragmas }

function TPragmas.getPragma(index: integer): TPragma;
begin
  TObject(Result) := GetItem(index).FObject;
end;

initialization
  InitDirectives;
  InitPragmas;
end.
