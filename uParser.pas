unit uParser;
(* a parser model

A parser corresponds to a grammar rule.
Every parser creates an parse tree (except: no-tree rules)
Multiple parsers (rules) can share an token stream.
*)

interface

uses
  uTablesPrep;  //TScanTokens

type
  PParseTree = ^TParseTree;
  TParseTree = object
  public
    id: integer;  //rule id or token index (+/-)
    nodes:  array of TParseTree;
    //scanner: implied!
    function  addNode:  PParseTree;
    procedure finish;
  end;

  TParser = object
  public
    id: integer;  //rule id
    scanner:  TScanTokens;
    tree: TParseTree;
    //???
    //function  parse(const r: TVBL): boolean;
  end;

  PVBL = ^TVBL;
  TVBL = record
    scanner:  TScanTokens;
    root: TParseTree;
  //add app-specifics as appropriate
  end;

{
  Texternal_declaration = object(TParser)
  public
    function  external_declaration(r: PVBL): boolean;
  end;
}

implementation


function  external_declaration(r: PVBL): boolean;
begin
  Result := declaration_specifiers(r) and (function_definition(r) or declaration(r));
end;

//template
function  translation_unit: boolean;
var
  r:  TVBL;
begin
  Result := True;  //can be empty?
  r.scanner := UnitScanner;
  r.root := nil;
  while i_ttyp <> t_eof do begin
    Result := external_declaration(@r);
    if not Result then
      break;
  end;
end;

end.
