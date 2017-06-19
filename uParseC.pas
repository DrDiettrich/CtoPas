unit uParseC;
(* C parser
V2: parse procedure bodies.
V1: parse all declarations, including procedures, vars and consts;
  don't parse complex initializers and definitions of procedures.
V0: parse declarations (typedefs only)
*)
(* Some ToDo's:
- make TDeclaration a class (compiler bug!)
- flag proc vs. func, add attributes (calling convention...)
- drop empty arg-list (void)?

- load previously stored declarations from textfile.
- record const_expression and compound_statement as TTokenStream,
    for display of non-numeric or otherwise complex expressions
    or inline procedures.
- safe detection of procedures (as opposed to pointers to procedures).
- classify macros as no-expand, emulate typedefs/refs.
Done:
+ typedef procedure without "*"?
+ Nested struct/union has double braces {{...}}.
+ list procedures.

More:
Constant expressions as expressions or value?
  (in dimensions [expr]?)
*)

interface

{$I config.pas}

uses
  uTablesPrep,
  uTablesC, uTokenC;  //only because of eToken!

//API
procedure ParseCMain(const fn: string; fStopOnError: boolean = False);
  //init tables and parse
procedure ParseCModule(const fn: string; fStopOnError: boolean = False);
  //parse c module

function  cond_expression: string;
  //single expression
function  expression: string;
//function  expression(fList: boolean = False): string;
  //single expression, by default!
function  statement(scope: TSymBlock; var stmt: string): boolean;
function  ScopedName: string;

type
  eMacroCode = (
    mcFailed,
    mcEmpty,
    mcExpr,
    mcNotConst,
    mcStmt
  );
function  ParseMacro(mac: TSymMacro; var s: string): eMacroCode;

implementation

uses
  SysUtils,
  //uExprC, - if ParseTree?
  uFiles, uMacros,
  uScanC, uUI;

type
(* Compiler bug:
  The initialization doesn't work for derived objects!
  See various attempts to initialize/reset object of this type...
  Should be converted into a class (also: base type RType)
*)
(* Making RType a member makes the initialize/finalize work!
-> separate Init, Reset...
*)
  TDeclaration = object //(RType)
  private
    procedure startDecl;
    function  declaration_specifiers: boolean;
    function  init_declarator_list: boolean;
      procedure finishDeclaration;
    function  declarator: boolean;
    function  recordBlock: boolean; //load {...}
    function  recordExpr: boolean;
    function  compound_statement(proc: TSymProc): boolean;
  protected //was: inherited
    r:  RType;
    fOldParams: boolean;  //prevent default type "int"
    //procedure Init;
  {$IFDEF lclScopes}
    constructor Create(AScope: TScopeC);
    procedure Clear;
  {$ELSE}
    constructor Create(fPub: TPublic);  //Create; (AScope: TScope)
    procedure Clear(fPub: TPublic); //Clear;
  {$ENDIF}
    destructor  Done;
    property  name: string read r.name;
  public
    //function  declaration(AScope: TScopeC): boolean;
    function  declaration: boolean;
  end;

const
  ItemEnd = ExpTerm + ')'; // or ')' only?

//begin op(...) -> "op("
function  startOp: string;
begin
  Result := TokenNames[i_ttyp] + '('; //ScanText???
  nextToken;
end;

//add op(arg) -> item+arg+term
//OR item+sep?+arg
function  addArg(const item, arg: string): string;
begin
  if ExpTerm = '' then begin
    if item[Length(item)] = '(' then
    //first arg
      Result := item + arg
    else
      Result := item + ExpSep + arg;
  end else
  //terminate
    Result := item + arg + ExpTerm;
end;

//finish op(arg) -> item+arg+term+")"
//OR item+sep?+arg+")"
function  endOp(const item, arg: string): string;
begin
  Result := addArg(item, arg) + ItemEnd;
end;

//finish expr -> item+")", no more sep/term added
function  endItem(const item: string): string;
begin
  Result := item + ')';
end;

(* cond_expression - transform (constant) expressions into string.
  Other: expression, assignment_expression are not constant expressions!
*)
function  cond_expression: string;

(*
type_name :
specifier_qualifier_list [abstract_declarator]
*)
  function  type_name: string;
  var
    typ: TDeclaration;
  begin
    typ.Create(nil);
    //if typ.declaration then
    if typ.declaration_specifiers then begin
      typ.r.specToken := Ktypedef;  //fake typedef!
      //typ.r.symkind := stTypedef; - initialized or unused
      typ.declarator;
      { TODO : lookup or create anonymous typename }
      typ.r.endDecl;  //.type_name()? create (anonymous) typedef
      Result := typ.r.getDef; //.r.basetype.name;  //quote? - not required?
      //Result := TokenNames[opType] {+ '('} + Globals.closestType(Result); // + ')';
      Result := Globals.closestType(Result);
    end else
      Result := '';
  end;

  function unary_expression: string; forward;
  function cast_expression: string; forward;

(*
postfix_expression :
primary_expression {
  "[" expression "]"
  "(" [argument_expression_list] ")"
  "." identifier
  "->" identifier
  "++"
  "--"
}

primary_expression :
identifier
  "sizeof" unary_expression
  "sizeof" "(" type_name ")"
constant
string_literal
"(" expression ")"
*)
  function  postfix_expression(fLpar: boolean = False): string;
  begin
  //handle "(" already seen...
    if not fLPar then
      fLPar := skip(opLPar);
  //primary_expression
    if fLPar then begin
    //( expr ), "(" already consumed
      Result := expression;
      expect(opRPar, 'missing ")"');
    end else begin
    //other primary expressions
      case i_ttyp of
      t_int, t_Lint,  //retain numbers literally
      t_car, t_str, //internal representation? L...???
      t_flt:
        begin
          Result  := TokenString(True); // Round(ScanToken.d);
          nextToken;
        end;
      t_symNX, //- unexpected
      t_sym:  //sizeof moved here, syntactially only
        if ScanText = 'sizeof' then begin
        (*  "sizeof" unary_expression
            "sizeof" "(" type_name ")"
        *)
          Result := ScanText + '(';
          nextToken;
          if skip(opLPar) then begin
          //"sizeof" "(" type_name ")"
            //Result := Result + '_t(';
            Result := endOp(Result, type_name);
            expect(opRPar, 'unterminated sizeof( ")"');
          end else begin
            Result := endOp(Result {+ '_x('}, unary_expression);
          end;
        end else begin
          { TODO : check for const/not const symbol }
        {$IFDEF old}
          Result := ScanText;
          //if ScanSym.mackind <> skConst...
            //include(ParserFlags, pfNotConst);
        {$ELSE}
          Result := ScopedName; //search symbol in scopes
        {$ENDIF}
          nextToken;
        end;
      t_arg, t_argNX:
        begin
        { TODO : arguments require arg-name-list }
          //include(ParserFlags, pfLclVar); macro args allowed!
          //Result := ScanText;
          Result := 'arg#' + IntToStr(ScanToken.argID);
          nextToken;
        end;
      end;  //case
    end;
  //repeatable postfixes...
    while i_ttyp <> t_eof do begin
      case i_ttyp of
      opLBra: // [ array select
        begin
          include(ParserFlags, pfNotConst); //unless const array!
          Result := addArg(startOp, Result);
          Result := endOp(Result, expression);  //array selection
          expect(opRBra, 'missing [dim"]"');
        end;
      opLPar: // ( here: call!
        begin
          ParserFlags := ParserFlags + [pfNotConst, pfCode];  //unless intrinsic!?
        {$IFDEF old} //fn-name(args) !!! not only name, expr's as well!!!
          Result := Result + '('; // startOp;
          nextToken;
        {$ELSE}
          Result := addArg(startOp, Result);  // "(("proc
        {$ENDIF}
          if i_ttyp = opRPar then  //"("
          //empty argument list
            Result := endItem(Result) // ((proc)
          else
            Result := endOp(Result, expression);  //((proc,arg-list)
          expect(opRPar, 'missing call(args")"');
        end;
      opDot, opTo:  // . -> selector
        begin
          Result := addArg(startOp, Result);
          Result := endOp(Result, ScanText);
          expect(t_sym, '.no-ident'); //t_symNX substituted
        end;
      opINC, opDec: // ++ -- here: postfix! ++(arg)
        begin
          ParserFlags := ParserFlags + [pfNotConst, pfCode];
        //prefix versions translate into +=/-=
          Result := endOp(startOp, Result);
        end;
      else
        break;  //done
      end;  //case
    end;
  end;

(*
unary_expression :
"++" unary_expression
"--" unary_expression
unary_operator cast_expression  //"&" | "*" | "+" | "-" | "~" | "!"
postfix_expression
*)
  function unary_expression: string; // + - ! ~ ()   * & ++ --
  begin
    case i_TTYP of
    //t_num,  //requires conversion!
  {$IFDEF old}
    opINC, opDec: //here: prefix! equivalent to arg+=1 [for lhs arg!]
      begin //op(arg) denotes prefix, op(1,arg) denotes postfix
        Result := startOp;
        Result := endOp(Result, unary_expression);
      end;
  {$ELSE}
    opINC: //here: prefix! equivalent to arg+=1 [for lhs arg!]
      begin //+=(arg) denotes prefix, ++(arg) denotes postfix
        ParserFlags := ParserFlags + [pfNotConst, pfCode];
        Result := '+=('; nextToken;
        Result := endOp(Result, unary_expression);
          //no second arg stored, defaults to 1.
      end;
    opDec: //here: prefix! equivalent to arg+=1 [for lhs arg!]
      begin //-=(arg) denotes prefix, --(arg) denotes postfix
        ParserFlags := ParserFlags + [pfNotConst, pfCode];
        Result := '-=('; nextToken;
        Result := endOp(Result, unary_expression);
      end;
  {$ENDIF}
    opADD:  //+ here: unary!
      begin
        nextToken;
        Result  := cast_expression; //unary_expression;
      end;
{$IFDEF opMul}
    opSub0,  //- here: unary!
    opMinus1:
      begin
        i_ttyp := opMinus1;
        Result := startOp;  //must skip before proceeding!
        Result := endOp(Result, cast_expression); //unary_expression);
      end;
    opStar0,
    opDeref1: //*
      begin
        include(ParserFlags, pfNotConst); //assume!
        i_ttyp := opDeref1;
        Result := startOp;
        Result := endOp(Result, cast_expression); //unary_expression);
      end;
    opAmpersAnd:  //& (AddrOf)
      begin
        i_ttyp := opAddr1;
        Result := startOp;
        Result := endOp(Result, cast_expression); //unary_expression);
      end;
{$ELSE}
    opStar_,  //here: deref
    opSub_,  //- here: unary!
    binAnd_,  //here: AddrOf
{$ENDIF}
    logNOT, //!
    binNot: //~
      begin
        Result := startOp;  //must skip before proceeding!
        Result := endOp(Result, cast_expression); //unary_expression);
      end;
   {$IFDEF const_expr}
    opLPar:  //'(' call - moved into postfix_expression
      begin
        nextToken;
        Result  := '(' + cond_expression + ItemEnd;  //(fPre, fSkip);
        expect(opRPar, 'expected ")"');
      end;
   {$ENDIF}
    else
      Result := postfix_expression(False);
    end;
  end;
(*
cast_expression :
unary_expression
"(" type_name ")" cast_expression
*)
  function  cast_expression: string;
  const
    cast_specifierS = type_modifierS + simple_type_specifiers
      + complex_type_specifierS + cast_modifierS;
  begin
    if skip(opLPar) then begin
    //typename from typedef, or simple, or "*"
      //if True then begin
      if ((i_ttyp = t_sym) and (Globals.isType(ScanText) >= 0))
      or (i_ttyp in cast_specifierS) then begin
        Result := type_name;
        expect(opRPar, '(cast")"');
        //Result := endOp('@(' + Result, cast_expression);  //recursion???
        Result := endOp(TokenNames[opCast] + '(' + Result, cast_expression);  //recursion???
      end else
        Result := postfix_expression(True);
    end else
      Result := unary_expression;
  end;

(*
multiplicative_expression :
cast_expression {( "*" | "/" | "%") cast_expression}
additive_expression :
multiplicative_expression {("+" | "-") multiplicative_expression}
shift_expression :
additive_expression {("<<" | ">>") additive_expression}
relational_expression :
shift_expression {("<" | ">" | "<=" | ">=") shift_expression}
equality_expression :
relational_expression {("==" | "!=") relational_expression}
AND_expression :
equality_expression {"&" equality_expression}
exclusive_OR_expression :
AND_expression {"^" AND_expression}
inclusive_OR_expression :
exclusive_OR_expression {"|" exclusive_OR_expression}
logical_AND_expression :
inclusive_OR_expression {"&&" inclusive_OR_expression}
logical_OR_expression :
logical_AND_expression {"||" logical_AND_expression}
*)

type
  ePrio = (
    prErr,    //unexpected, should have been handled -> exit
    prDone,   //force exit, on ")" etc.
    prLowest, //for first invocation
    prLogOr,
    prLogAnd,
    prIncOr,
    prExOr,
    prBinAnd,
    prEq,
    prRel,
    prShift,
    prAdd,
    prMul,
    prHighest
  );

{$IFDEF opMul}
  binops = opAmpersAnd..opTo;
{$ELSE}
  binops = binAnd_..opTo;
{$ENDIF}

const
  prLet = prDone;  //force exit?
  prPost = prErr;  //here: error! (postfix_expression)
    //should have been handled in cast->unary!

{$IFDEF opMul}
  binopsS = [opAmpersAnd..opTo];
{$ELSE}
  binopsS = [binAnd_..opTo];
{$ENDIF}

  Prio: array[binops] of ePrio = (
  //operators - sorted within lines
{$IFDEF opMul}
    prBinAnd, prLet, prLogAnd,  //opAmpersAnd, letAND, logAND, // & &= &&, ambiguous: &
{$ELSE}
    prBinAnd, prLet, prLogAnd,  //binAnd_, letAND, logAND, // & &= &&, ambiguous: &
{$ENDIF}
    prAdd, prLet, prPost, //opADD, letADD, opINC,   // + += ++
    prRel, prRel, prShift, prLet, //opLT, opLE, opSHL, letSHL, // < <= << <<=
    prRel, prRel, prShift, prLet, //opGT, opGE, opSHR, letSHR, // > >= >> >>=
    prIncOr, prLet, prLogOr,  //binOR, letOR, logOR,    // | |= ||

    prErr, prEq, //logNOT, opNE,   // ! !=
    prMul, prLet, //opMOD, letMOD,  // % %=
{$IFnDEF opMul}
    prMul, prLet, //opStar_, letMUL, // * *=, ambiguous: *
{$ELSE}
    prMul, prLet, //opStar0, letMUL, // * *=, ambiguous: *
  //replacements
    prMul, prErr, prErr,  //opMul2, opPtr1, opDeref1, //classified "*" (binary mul, ptr decl., dereference)
    prbinAnd, prErr,  //binAnd2, opAddr1,   //classified "&" (unary)
    prAdd, prErr, //opSub2, opMinus1,  //classified "-" (unary)
{$ENDIF}
    prLet, prEq,  //opLet, opEQ,    // = ==
    prExOr, prLet, //opXor, letXOR,  // ^ ^=

    prErr, prDone,  //opLPar, opRPar, // ( )
    prDone, prDone, prDone, //opComma, opColon, opSemi, // , : ; C++: ::
    prDone,  //opTERN, // ?
    prDone, prDone, //opLBra, opRBra, // [ ]
    prPost, prDone, //opBeg, opEnd,   // { }
    prDone,  //binNot,  //~
  //not in op1$
    prPost, prErr, //opDot, op3Dot, // . ..., C++: .*
    prMul, prLet, prErr, //opDiv, letDiv, OpDivDiv, // / /= //     {RP}
{$IFDEF opMul}
    prAdd, prLet, prPost, prPost //opSub0, letSub, opDec, opTo, // - -= -- ->, C++: ->*
{$ELSE}
    prAdd, prLet, prPost, prPost //opSub_, letSub, opDec, opTo, // - -= -- ->, C++: ->*
{$ENDIF}
  );

  function  bin_expr(leftOp: ePrio): string;
  var
    //lhs, rhs: string;
    op: eToken;
  begin
    Result := cast_expression;
    while (i_ttyp in binopsS) and (Prio[i_ttyp] > leftOp) do begin
      op := i_ttyp;
      if Prio[op] = prErr then
        SynErr('unexpected operator');
      Result := addArg(startOp, Result);
      Result := endOp(Result, bin_expr(Prio[op]));
    end;
  end;

(*
constant_expression :
conditional_expression

conditional_expression :
logical_OR_expression ["?" constant_expression ":" conditional_expression]
logical_OR_expression {"?" constant_expression ":" logical_OR_expression}
*)
//var  cond: boolean;
begin //cond_expression = conditional_expression
//ternary eypression
  Result := bin_expr(prLowest);
  //["?" constant_expression ":" conditional_expression]
  if i_ttyp = opTern then begin
    Result := addArg(startOp, Result);
    Result := addArg(Result, expression); //(fPre, fSkip);
    expect(opColon, 'expected ":"');
    Result := endOp(Result, cond_expression); //(fPre, fSkip);
  end;
end;

(*
assignment_expression :
    conditional_expression    //not recursive!
  | unary_expression assignment_operator assignment_expression
  | logical_OR_expression assignment_operator assignment_expression    //C99!!!

    assignment_operator : "="|"*="|"/="|"%="|"+="|"-="|"<<="|">>="|"&="|"^="|"|="
*)
function  assignment_expression: string;
const
  assignOps = [
    opLet, letMul, letDiv, letMod, letAdd, letSub,
    letShl, letShr, letAnd, letOr, letXor
  ];
begin
  Result := cond_expression;
  while i_ttyp in assignOps do begin
    ParserFlags := ParserFlags + [pfNotConst, pfCode];
    Result := addArg(startOp, Result);
    Result := endOp(Result, cond_expression);
  end;
end;

(*
expression : assignment_expression { "," assignment_expression }
//argument_expression_list : assignment_expression { "," assignment_expression }
*)
//function  expression(fList: boolean): string;
function  expression: string;
begin
  Result := assignment_expression;
  if Result = '' then
    Result := '???';  //or exit?
  while {fList and} skip(opComma) do begin
    //flag: multiple expressions?
    Result := addArg(Result, assignment_expression);
  end;
  //term???
end;

// ---------------------------------------------

(* skipList - skip list in parentheses (...)
  Currently unused.
*)
function  skipList: boolean;
var
  lvl: integer;
begin
//skip (...)
  lvl := 1;
  repeat
    nextToken;
    case i_ttyp of
    t_eof:  break;
    opLPar:  inc(lvl);
    opRPar:  dec(lvl);
    end;
  until lvl = 0;
  Result := i_ttyp = opRPar;
  if Result then
    nextToken;
end;

(* skipBlock - skip list in braces {...}
  Should simply pass through all tokens?
*)
function  skipBlock: boolean;
var
  lvl: integer;
begin
  Result := i_ttyp = opBeg;
  if not Result then begin
    LogBug('bad call of skipBlock, no "{"');
    exit;
  end;
//skip {...}
  lvl := 1;
  repeat
    nextToken;
    case i_ttyp of
    t_eof:  break;
    opBeg:  inc(lvl);
    opEnd:  dec(lvl);
    end;
  until lvl = 0;
  Result := expect(opEnd, 'unterminated block');
end;

// -------------------------------

//optimization: for strictly temporary use, to bypass implied try/finally
var
  tempstr: string;
  fParseMacro: boolean;
  CurScope: TScope;
  CurSym: TSymbolC;

function  ScopedName: string;
begin
  if CurScope = nil then
    Result := ScanText  //default
  else begin
    CurSym := CurScope.findSym(ScanText);
    if CurSym = nil then
      Result := ScanText
    else begin
      Result := CurSym.name;
    end;
  end;
end;

(* statement - return both an string and an error condition.
  Append 1 stmt to s.
*)
function  statement(scope: TSymBlock; var stmt: string): boolean;

  function  cond_expr: boolean;
  begin
    Result := expect(opLPar, 'no "("cond)');
    if not Result then
      exit;
    stmt := stmt + expression + ListTerm;
    Result := expect(opRPar, 'no (cond")"');
  end;

  function  addStmt: boolean;
  begin
    Result := statement(scope, stmt);
  end;

  //function  compound(var stmt: string): boolean;
  function  compound: boolean;
  var
    lcl: TDeclaration;
    inner: TSymBlock;
  begin
    Result := expect(opBeg, 'no "{"block}');
    if not Result then
      exit;
    inner := scope.enterBlock(stmt);
    CurScope := inner.CurScope;  //back: how???
    lcl.Create(inner.CurScope);
    lcl.r.storage := Kauto;
    Result := True;
    while Result and lcl.declaration do begin
      Result := lcl.r.declSym <> nil;
      if not Result then
        LogBug('no local symbol created?');
    end;
    inner.addLcls;
    while Result do begin
      case i_ttyp of
      t_eof, opEnd:
        break;
      else
        Result := statement(inner, stmt);
        inner.addStmt(stmt);  //clear stmt
      end;
    end;
    if Result then
      Result := expect(opEnd, 'no {block"}"');
    lcl.Done;
    inner.leaveBlock(stmt);
    CurScope := inner.CurScope;  //back: how???
    //exit with s='}'
  end;

{
var
  SavedFlags: sParserFlags;
  SavedFlags := ParserFlags;
}
begin //statement
  //include(ParserFlags, pfCode); //assume! - of little help :-(
  Result := True; //assume
(* ScanText valid only for non-op's?
  Happens to contain comment instead of token (operator!) name
*)
  tempstr := TokenString; //in rare cases this token is NOT added to stmt!!!
//default: cases fall through to common check for ";" terminator
  case i_ttyp of
  Kbreak,     //"break;"
  Kcontinue:  //"continue;"
    begin
      stmt := stmt + tempstr;
      nextToken;
    end;
//Treat case and default als self-contained labels.
  Kcase:  //"case" constant_expression ":" [ statement
    begin //-> case(expr,expr);
      stmt := stmt + tempstr + '(';
      while skip(Kcase) do begin
      //concat case labels
        stmt := addArg(stmt, expression); // + ScanText);  // + ExpTerm;  //???
        Result := expect(opColon, 'no case":"');
        if not Result then
          exit; //unterminated!
      end;
    //for now: label-list only!
      stmt := stmt + ')' + ListTerm; // endItem(Result);
      exit; //done
    //statement is NOT related to an case label!
    end;
  Kdefault: //"default" ":" [ statement
    begin //-> default;
      stmt := stmt + tempstr + ListTerm; nextToken;
      Result := expect(opColon, 'no default":"');
      exit;
    end;
  Kdo:  //"do" statement "while" "(" expression ");"
    begin //-> do(stmt;cond;);
      stmt := stmt + tempstr + '('; nextToken;
      Result := addStmt;
      Result := Result and expect(Kwhile, 'do without "while"') and cond_expr;
      if not Result then
        exit;
      stmt := stmt + ')';
    end;
  Kfor: //"for" "(" [expression] ";" [expression] ";" [expression] ")" statement
    begin //-> for(expr?;expr?;expr?;stmt;);
      stmt := stmt + tempstr + '('; nextToken;
      Result := expect(opLPar, 'no for"("');
      if not Result then
        exit; //error
    //expr = init
      if i_ttyp <> opSemi then
        stmt := stmt + expression;
      Result := expect(opSemi, 'no for-init');
      if not Result then
        exit;
      stmt := stmt + ListTerm;
    //expr = test
      if i_ttyp <> opSemi then
        stmt := stmt + expression;
      Result := expect(opSemi, 'no for-test');
      if not Result then
        exit;
      stmt := stmt + ListTerm;
    //expr = init
      if i_ttyp <> opSemi then
        stmt := stmt + expression;
      Result := expect(opRPar, 'no for( ")"');
      if not Result then
        exit;
      stmt := stmt + ListTerm;
      Result := addStmt;
      if Result then
        stmt := stmt + ')' + ListTerm;
      exit;
    end;
  Kgoto:  //"goto" identifier ";"
    begin //-> goto(ident);
      stmt := stmt + tempstr + '('; nextToken;
      stmt := stmt + ScanText + ')';  //identifier
      Result := expect(t_sym, 'no goto"label"');
    end;
  Kif:  //"if" "(" expression ")" statement ["else" statement]
    begin //-> if(expr;stmt;[stmt;]);
      stmt := stmt + tempstr + '('; nextToken;
      Result := cond_expr;
      if not Result then
        exit;
      Result := addStmt;
      if not Result then
        exit;
      if skip(Kelse) then
        Result := addStmt;
      stmt := stmt + ')' + ListTerm;
      exit; //okay
    end;
  Kreturn:  //"return" [expression] ";"
    begin //-> return; or return(expr);
      stmt := stmt + tempstr; nextToken;
      if i_ttyp <> opSemi then
        stmt := stmt + '(' + expression + ')';
    end;
  Kswitch,  //"switch" "(" expression ")" statement
  Kwhile:   //"while" "(" expression ")" statement
    begin //-> op(expr;stmt;);
      stmt := stmt + tempstr + '('; nextToken;
      Result := cond_expr;
      if not Result then
        exit;
      Result := addStmt;
      //if Result then
      stmt := stmt + ')' + ListTerm;
      exit; //done
    end;
  opBeg: //"{" [declaration_list] +{statement} "}"
    begin //-> {stmt;*} - {} added by enter/leave block!
      Result := compound;
      exit; //done, ret "}"
    end;
  opEnd:
    begin
      exit; //no stmt, terminate compound (should never occur!)
    end;
  else
    tempstr := expression;
    if skip(opColon) then begin
    //labeled_statement : identifier ":" statement
      stmt := stmt + ':' + tempstr + ListTerm;  //or :(sym)?
      Result := addStmt;  // statement(scope, s);
      exit; //okay, label only!
    end else
      stmt := stmt + tempstr;
  end;
//expect ";"
  if Result then begin
    if fParseMacro then
      skip(opSemi)  //macros are not terminated!
    else
      Result := expect(opSemi, 'no stmt";"');
    if Result then
      stmt := stmt + ListTerm;
  end;
end;

// ------------------------------

function  translation_unit : boolean;
var
  decl: TDeclaration;
begin
(*
{external_declaration}
*)
  decl.Create(Globals); //only "static" is not exported!
  repeat   //while external_declaration do ;
    if i_ttyp = opBeg then begin //not expected at global scope!
    //todo: catch compound statement in proc definition
      Result := skipBlock;
      skip(opSemi); //what's this? should be catched
    end else
      Result := decl.declaration; //(extern)
  until not Result or (i_ttyp = t_eof);
  decl.Done;
end;

// ---------------------------------------------

{ TDeclaration }

procedure TDeclaration.Clear;
begin
  Finalize(r);
  Create(r.declScope);  //preserve this!
end;

constructor TDeclaration.Create(AScope: TScopeC);
begin
  FillChar(r, sizeof(r), 0);  //exclude some?
  fOldParams := False;
  r.declScope := AScope;
end;

destructor  TDeclaration.Done;
begin
  r.Reset;
  //FreeAndNil(r.LclScope); //???
  //FreeAndNil(r.InScope);
end;

procedure TDeclaration.startDecl;
begin
  r.Reset;
end;

(* declaration_specifiers
  Allow for typename argument (lookahead)
*)
function TDeclaration.declaration_specifiers: boolean;
var
  mbr: TDeclaration;

  function  storage_class_specifier: boolean;
  begin
    if i_ttyp = Kinline then begin
    //special case
      r._inline := True;
    {$IFDEF proto}
      r.symkind := stInline;  //??? - reserved for macros?
    {$ELSE}
    {$ENDIF}
      Result := True;
      nextToken;
      exit;
    end;
  //real storage classes
    Result := i_ttyp in storage_class_specifierS;
    if Result then begin
      //handleStorage(i_ttyp);
      if not (r.storage in [t_empty, DefaultStorage]) then
        LogBug('redef storage class');
      r.storage := i_ttyp;
      nextToken();
    end;
  end;

  function  type_qualifier : boolean;
  begin
    Result := r.qualify(i_ttyp, True);
    nextToken;
    //Result := True;
  end;

  (* handleTag - check and handle tag name
    Return False if member list must follow (untagged case).
  *)
  function  handleTag: boolean;
  var
    sue: eKey;
  begin
    sue := i_ttyp;
    nextToken;
    Result := i_ttyp = t_sym;
    if Result then begin
    {$IFDEF named}
      r.name := ScanText;
      r.nameID := ScanToken.symID;
      r.loc := ScanSym.loc;
    {$ELSE}
      r.setNameSym;
    {$ENDIF}
      Result := nextToken <> opBeg;  //done?
  {$IFDEF delayTags}
    //nothing to create for untagged type
  {$ELSE}
    end else if r.storage = KTypedef then begin
    //create public name??? - delay
      r.name := IntToStr(Globals.TypeCount);
      r.nameID := 0;  //todo: substitute typename, if found
      r.loc := ScanSym.loc; //???
  {$ENDIF}
    end else
      r.name := '';
    r.makeTagRef(sue, r.name);
  end;

  function  enum_specifier: boolean;
  begin
    Result := handleTag;
    if Result then
      exit; //named type ref
  //else type def
    Result := skip(opBeg);
    if Result then begin
    //create persistent scope!
      if enumScope = nil then
        enumScope := TScope.Create('enum')
      else
        enumScope.Clear;
      mbr.Create(Globals);
      r.mbrScope := enumScope;  //fill this one
    //initializer list
      while i_ttyp = t_sym do begin
      {$IFDEF named}
        mbr.r.Name := ScanText;
        mbr.r.nameID := ScanToken.symID;  //method: takeName?
        mbr.r.loc := ScanSym.loc;
      {$ELSE}
        mbr.r.setNameSym;
      {$ENDIF}
        //s_mbr := ScanText;  //nameID?
        //id? //id := ScanToken.symID;
        nextToken;
        if skip(opLet) then begin
        //todo: remember format (hex/dec/char...)
          mbr.r.Value := cond_expression;
        end;
        r.makeEnumMember(mbr.r);  //(s_mbr, r.spec, nxt, id);
          //spec can be "E" or "E:<name>"
        mbr.Clear;  // nxt := '';  //inc(nxt);
        if not skip(opComma) then
          break;
      end;
      Result := expect(opEnd, 'unterminated enum');
      r.finishComplex;
      mbr.Done;
    end;
  end;

  function  struct_or_union: boolean;
  begin
    Result := handleTag;
    if Result then
      exit; //named type ref
  //else type def
(*
struct_declaration :
specifier_qualifier_list struct_declarator_list ";"

specifier_qualifier_list :
+{type_specifier | type_qualifier}
(= declaration_specifiers without storage class)
-> {type_qualifier} type_specifier

struct_declarator_list :
struct_declarator {"," struct_declarator}

struct_declarator :
declarator
type_specifier [declarator] ":" constant_expression
*)
    Result := expect(opBeg, 'missing struct member list');
    if Result then begin
      mbr.Create(self.r.mbrScope);
      //mbr.r.storage := Kstruct; //prevent symbol creation?
      repeat  //struct_declaration
        Result := mbr.declaration_specifiers;
        if not Result then
          break;
        repeat  //struct_declarator
          mbr.declarator; //opt
          if skip(opColon) then
            mbr.r.Value := cond_expression;
          r.makeStructMember(mbr.r);
          if not skip(opComma) then
            break;
        until i_ttyp = opSemi;
        Result := expect(opSemi, 'missing ";" after struct member decl');
        if not Result then
          break;
      until i_ttyp = opEnd;
      Result := expect(opEnd, 'unterminated struct/union');
      r.finishComplex;
      mbr.Done; //helps? - still required?
    end;
    //else forward declaration?
  end;

  function  type_modifier: boolean;
  begin
    Result := i_ttyp in type_modifierS;
    if Result then begin
      case i_ttyp of
      Klong:  if r.sized = szLong then r.sized := szLongLong else r.sized := szLong;
      Kshort: r.sized := szShort;
      Ksigned, Kunsigned: r.signed := i_ttyp;
      else
        assert(False, 'unhandled modifier');
      end;
      nextToken;
    end;
  end;

begin //declaration_specifiers
(*
[storage_class_specifier] {type_qualifier | type_modifier} [type_specifier]
->r.inline, r.storage     ->r.call, r.attrs ->r.sized, r.signed ->r.specToken
*)
  Clear;
  Result := False;
//collect specifier items
  while True do begin
    if i_ttyp in extended_storage_class_specifierS then
      Result := storage_class_specifier
    else if i_ttyp in type_qualifierS then
      Result := type_qualifier
    else if i_ttyp in type_modifierS then
      Result := type_modifier
  //various type specifiers
    else if i_ttyp in simple_type_specifierS then begin
      Result := True;
      r.type_specifier(i_ttyp, False);
      nextToken;
    end else if i_ttyp = Kenum then begin
      Result := enum_specifier;
      //done := True;
    end else if i_ttyp in complex_type_specifiers then begin
      Result := struct_or_union;
    //end else if not done and (i_ttyp = t_sym)
    end else if (i_ttyp = t_sym) and (r.specToken = t_empty)
    and (Globals.isType(ScanText) >= 0) then begin
    //typename - possibly modified?
      Result := True;
      r.type_specifier(t_sym, True);  //handle modifiers? Else: makeTypeRef(ScanText);
      nextToken;
    end else //assume declarator name, of default base type "int"
      break;
  end;
//finish - allow for old style parameter lists, with names only!
{$DEFINE defType}
{$IFDEF defType}
  if Result and not fOldParams and (r.specToken = t_empty) then
    r.type_specifier(Kint, True); //really required?
{$ELSE}
{$ENDIF}
end;

function TDeclaration.init_declarator_list: boolean;

  function initializer: boolean;
  begin
  (*
  initializer :
  assignment_expression
  "{" initializer_list [ "," ] "}"  /* For aggregate initialization */

  initializer_list :
  initializer {"," initializer}
  *)
  (* ToDo:
    - initializer for complex objects.
      This requires special storage of the initializers (concat strings?)
      and passing in all required information as parameters!
    - non-numeric initializers.
      This requires an extension to constant_expression!
    - format info for numeric values (hex/dec/char...)
  *)
    Result := r.declSym <> nil;
    if not Result then begin
      LogBug('no place to store const value');
      exit;
    end;
    Result := r.declSym.kind in InitableSyms; //[skConst, skVar];
    if not Result then begin
      LogBug('no var/const to initialize');
      exit;
    end;
{$IFDEF ParseTree}
    declSym.Definition := TDefinition.Create;
{$ENDIF}
    r.declSym.StrVal := '<todo: parse expression>';
    if i_ttyp = opBeg then begin
    //initialize structure
      Result := self.recordBlock; //todo: parse
    end else begin
    //assignment_expression
      Result := self.recordExpr;  //todo: parse
    end;
  end;

begin //init_declarator_list
(*
init_declarator_list :
init_declarator {"," init_declarator}

init_declarator :
declarator ["=" initializer] /* For scalar initialization */
declarator [declaration_list] compound_statement /* function definition */
*)
  repeat
    Result := declarator;
    if not Result then
      break;
    if skip(opLet) then
      Result := initializer;
    finishDeclaration;  //really always? scope?
  until not skip(opComma);
end;

(* finishDeclaration - consistency check
Called by init_declarator_list() for every finished init_declarator.
*)
procedure TDeclaration.finishDeclaration;
begin
  Log(r.name + ':' + r.getDef, lkTrace);
  if r.declSym = nil then begin
  //in case of: typedef struct <ident>;
    Log('no symbol created', lkTodo);
  //end else if r.nameID = 0 then begin
  end else begin
    //r.declSym.loc := ScanSym.loc; //???
  {$IFDEF altSym}
    if r.declSym.symID = 0 then
      Log('no symID', lkBug);
  {$ELSE}
    if r.declSym.altID = 0 then begin
      Log('no nameID', lkBug);
    end;
  {$ENDIF}
  end;
end;

function TDeclaration.declarator: boolean;
var
  s: string;
  pr: TDeclaration; //for subtypes

//we should create a local scope for the parameters?
  function  OldParams: boolean;
  begin
  //old style parameter list
    r.makeScope;  //need temp scope, to merge names (in order) and declarations
    pr.Create(r.mbrScope);
    pr.fOldParams := True;
    try
      pr.r.name := ScanText;
      r.makeParam(pr.r);
      nextToken;
      while skip(opComma) do begin
        if i_ttyp = t_sym then begin
          pr.r.name := ScanText;
          r.makeParam(pr.r);
          nextToken;
        end else if i_ttyp = opRPar then begin
        //,) means: ...)
          r.makeVararg;
          nextToken;
          break;
        end else
          break;
      end;
      Result := expect(opRPar, 'unterminated (parameter list)');
    //[declaration_list]?
      while pr.declaration do begin
        r.makeParam(pr.r);
      end;
      r.makeParams('');
    finally
      pr.fOldParams := False;
      pr.Done;
    end;
  end;

  function  parameter_list: boolean;
  begin
    Result := expect(opLPar, 'no "("parameter list)');
    if not Result then
      exit;
  {$IFDEF old}
    pr.Clear;
  {$ELSE}
  //try prevent scope creation for external procedures - storage here valid???
  //external storage *only* known on ";" instead of "{"!!!
    if r.storage <> Kextern then begin
      r.makeScope;
      pr.Create(r.mbrScope);
    end;
  {$ENDIF}
    //if i_ttyp <> opRPar then begin
    Result := skip(opRPar);
    if Result then begin
      r.makeParams('');
      exit; //empty list
    end;
  (* This works only if all types really are known,
      not when #included files are missing!
  *)
    if pr.declaration_specifiers then begin
    //new style parameter list {type[+name]}
    (*
      parameter_type_list :                           /* The parameter list */
      parameter_list ["," | "..."]

      parameter_list :
      parameter_declaration {"," parameter_declaration}

      parameter_declaration :
      declaration_specifiers declarator                /* Named declarator */
      declaration_specifiers [abstract_declarator] /* Anonymous declarator */
    *)
    (* also: "void)" - how??? [might be: "void*..."]
    *)
      if (i_ttyp = opRPar) and (pr.r.getDef = 'v') then begin
        s := '';
        //Result := expect(opRPar, 'unterminated (parameter list)')
        //Result := True  //is empty list!!!
      end else begin
        pr.declarator;
        s := r.makeParam(pr.r);
        while i_ttyp = opComma do begin
          nextToken;
          if i_ttyp in [opRPar, op3Dot] then
            break;  //means: varargs???
          pr.Clear;
          pr.declaration_specifiers;
          pr.declarator;
          s := s + r.makeParam(pr.r);
        end;
        //Result := s;
      //check for possible varargs
        if skip(op3Dot) then
          s := s + r.makeVararg; // '~,';
      end;
      Result := expect(opRPar, 'unterminated (parameter list)');
      r.makeParams(s);  //"(...)" and proc symbol
    end else begin
    //old style parameter list (names only)
      Result := OldParams;
    end;
  end;

begin //declarator
(*
declarator :
[pointer] direct_declarator
MS extension: qualifiers!!!
*)
  pr.Create(r.mbrScope);  //if exists?
  startDecl; //!!! non-reentrant !!!
  while i_ttyp in declarator_qualifierS do begin
{$IFDEF opMul}
    if i_ttyp in [opStar0, opPtr1] then begin
{$ELSE}
    if i_ttyp = opStar_ then begin
{$ENDIF}
    (*
    pointer :
    {"*" [type_qualifier_list]}
    *)
      r.makePointer;  //*+pre
    end else begin  //while i_ttyp in type_qualifierS do begin
      r.qualify(i_ttyp, False);  //?+pre
    end;
    nextToken;
  end;
  (*
  direct_declarator :
  identifier
  "(" declarator ")"
  *)
  r.loc.src := nil; //invalidate file position
  if i_ttyp = opLPar then begin
  (* A nested declarator specifies the topmost type.
    A name can occur only inside this declarator!
    No postfix modifiers can have occured by now.

    Initialize an temporary declarator, fake an (non-existing) specification.
  *)
    pr.Clear;  //start nested declaration
    pr.r.specToken := i_ttyp; //prevent modification of spec
    nextToken;
    Result := pr.declarator and expect(opRPar, 'unterminated (declarator)');  //->post+pre (+name)
  //get declarator (prefix + name + postfix)
    r.post := pr.r.post + pr.r.pre;
  {$IFDEF named}
    r.name := pr.name;
    r.nameID := pr.r.nameID;
    r.loc := pr.r.loc;
  {$ELSE}
    r.propagateName(pr.r);
  {$ENDIF}
    if pr.r.call <> t_empty then
      r.qualify(pr.r.call, True);
  end else begin
  //also: abstract declarator, no name!
    if i_ttyp = t_sym then begin
    {$IFDEF named}
      r.name := ScanText;
      r.nameID := ScanToken.symID;
      r.loc := ScanSym.loc;
    {$ELSE}
      r.setNameSym;
    {$ENDIF}
      nextToken;
    end;  //else abstract (anonymous) declarator
    Result := True; //if anonymous!
  end;
  (*
  direct_declarator "[" [constant_expression] "]"
  direct_declarator "(" parameter_type_list ")"   /* New_style declarator */
  direct_declarator "(" [identifier_list] ")"      /* Obsolete_style declarator */
  *)
  while i_ttyp in [opLPar, opLBra] do begin
    if skip(opLBra) then begin //"["
    //array
      if i_ttyp <> opRBra then
        //s := IntToStr(cond_expression)
        s := cond_expression
      else
        s := '';  // empty []
      r.makeDim(s); //(const_expression);
      Result := expect(opRBra, 'unterminated [dim]');
    end else begin  // "("
    //procedure
      Result := parameter_list; //start with LPar
    {$IFDEF proto}
      if i_ttyp = opBeg then
        r.symkind := stProc
      else
        r.symkind := stProto;
    {$ELSE}
    {$ENDIF}
    end;
  end;
  if Result then
    r.endDecl;  //create object (type, const, var, proc).
  //else implicit type declaration?
end;

function TDeclaration.recordExpr: boolean;
{$IFNDEF ParseTree}
begin
{ TODO : try: don't terminate expression string - at least here! }
  r.declSym.StrVal := cond_expression; //(False);
  Result := True;  //nothing recorded
end;
{$ELSE}
var
  def:  TDefinition;
  lvl:  integer;
begin
  def := nil; //keep compiler happy
  Result := False;  //unless checks signal okay
  if declSym = nil then
    Log('no sym for initializer', lkErr)
  else if declSym.Definition = nil then begin
    def := TDefinition.Create;
    declSym.Definition := def;
    Result := True;
  end else {if declSym.Definition <> nil then} begin
  //todo: handle already scanned definition
    def := declSym.Definition;
    if def.Count > 0 then
      Log('redef symbol', lkErr);
    Result := True; //really?
  end;
//check for available declaration
  if not Result then begin
  //error!
    Result := skipBlock;  //todo
    exit;
  end;
//collect all tokens to parse
  if def.Count = 0 then begin
  //just created, record expression
    while True do begin
      case def.nextC of
      t_eof,  //msg?
      opComma,
      opSemi: break;
      end;
    end;
  //here the current token is opComma or opSemi - add rems?
    def.rewind; //finalize stream, including current token <--- ???
    //Result := expect(opSemi, 'unterminated expression');
      //check/skip only when recording
    Result := def.Count > 1;  //if , or ; included!?
  end;
end;
{$ENDIF}


function TDeclaration.recordBlock: boolean;
{$IFNDEF ParseTree}
begin
//start with "{"
  //r.declSym ???
  Result := skipBlock;  //nothing recorded
end;
{$ELSE}
var
  def:  TDefinition;
  lvl:  integer;
begin
  def := nil; //keep compiler happy
  Result := False;  //unless checks signal okay
  if declSym = nil then
    Log('no proc for body', lkErr)
  else if declSym.Definition = nil then begin
    def := TDefinition.Create;
    declSym.Definition := def;
    Result := True;
  end else {if declSym.Definition <> nil then} begin
  //todo: handle already scanned definition
    def := declSym.Definition;
    if def.Count > 0 then
      Log('redef symbol', lkErr);
    Result := True; //really?
  end;
//check for available declaration
  if not Result then begin
  //error!
    Result := skipBlock;  //todo
    exit;
  end;
//collect all tokens to parse
  if def.Count = 0 then begin
  //just created, record whole block
    //def.rewind;
    Result := def.firstC = opBeg;
    //Result := def.nextToken = opBeg;
    if not Result then begin
      Log('expected "{"', lkDiag);
      exit;
    end;
    lvl := 1;
    while lvl > 0 do begin
      case def.nextC of
      t_eof:  break;  //assert(False, 'EOF before "}"');
      opBeg:  inc(lvl);
      opEnd:  dec(lvl);
      end;
    end;
  //here the current token is opEnd - add rems?
    def.rewind; //finalize stream, including current token
    Result := expect(opEnd, 'unterminated block');
      //check/skip only when recording
  end;
end;
{$ENDIF}

function  TDeclaration.compound_statement(proc: TSymProc): boolean;
var
  s: string;
begin
  CurScope := proc.Params;
  fMetaNames := True;
  Result := statement(proc.Body, s);
  //ignore dummy string, everything should have been added to proc.Body
  if s <> '' then
  //final '}' ???
    proc.Body.addStmt(s); //+ Term?
  fMetaNames := False;
end;

function TDeclaration.declaration: boolean;

  procedure attribute_seq;
  begin
  (*
  attribute_seq :            /* attribute_seq is Microsoft Specific */
  attribute {attribute}

  attribute : one of      /* Microsoft Specific */
  __asm
  __based (16 bit only)
  __fastcall __inline __cdecl __stdcall
  *)
  (* todo: __asm deserves special handling, in compound statements!
    on __asm {...} skip block,
    else skip line
  *)
    //Result := True;
  end;

begin //declaration
(*
declaration :
declaration_specifiers [attribute_seq] [init_declarator_list] ";"
                        /* attribute_seq is Microsoft Specific */
function_definition :         /* Declarator here is the function declarator */
declaration_specifiers declarator [declaration_list] compound_statement
*)
  Clear;
  Result := declaration_specifiers;
  if Result then
    attribute_seq;
  if not Result and (r.declScope = Globals) and (i_ttyp = t_sym) then begin
  //assume missing 'int' at external scope
    r.type_specifier(Kint, True);
    Result := True;
  end;
  if Result then
    Result := init_declarator_list;
  if not Result then
    exit; //no more declarator?
  //handle (inline) function definitions
  if i_ttyp = opBeg then begin
  //should be handled before!?
    Result := compound_statement(r.declSym as TSymProc);
    //skip(opSemi); //??? unexpected ???
  end else if Result then
    Result := expect(opSemi, 'unterminated declaration";"'); //";"
  //no symbols created here!!!
end;

// -----------------------------

procedure ParseCModule(const fn: string; fStopOnError: boolean = False);
begin
//open the file
  Statics := Modules.AddModule(fn);
  ScanFile(fn);
  ParserFlags := [];
  nextToken();
  WHILE i_TTYP  > T_EOF  do begin
  //expect translation_unit
    if not translation_unit then begin
      Log('error translation_unit', lkSynErr);
      if fStopOnError then
        break;  //debug
    //on error try to resync
      WHILE i_TTYP  > T_EOF  do begin
        if skip(opSemi) then
          break;  //try restart
        nextToken;
      end;
    end;
  end;
end;

procedure ParseCMain(const fn: string; fStopOnError: boolean = False);
begin
  resetTables;  //preprocessor
  InitAlias;
  ParseCModule(fn, fStopOnError);
end;

//function  ParseMacro(mac: TSymMacro): boolean;
function  ParseMacro(mac: TSymMacro; var s: string): eMacroCode;
var
  blk:  TSymBlock;
  tokens: TTokenArray;
  //s:  string;

  procedure init;
  begin
    FreeAndNil(blk);
    tokens := mac.GetTokens;
    if tokens = nil then
      Result := mcEmpty
    else begin
      //tokens.firstC;
      tokens.rewind;
      TokenStack.push(tokens);
      if nextToken = t_eof then
        Result := mcEmpty;
    end;
    if Result <> mcEmpty then begin
      blk := TSymBlock.CreateBlock(nil, Globals);
      ParserFlags := [];
      s := '';
      //mac.Expanding := True;
      (mac as TMacro).expanding := True;
    end;
  end;

begin //ParseMacro
//init parser mode
  fParseMacro := True;
  Result := mcFailed;
  blk := nil;
  init;
  if Result <> mcEmpty then begin
  //not empty, try expression
    //if i_ttyp <> opLBra then begin
      //assume "{" means procedure?
    s := expression;
    //Result := (s <> '') and (ParserFlags = []);
    if ParserFlags = [] then
      Result := mcExpr
    else if ParserFlags = [pfNotConst] then
      Result := mcNotConst
  //end;
    else if Result < mcExpr then begin
    //not expression, try statement
      init;
      if statement(blk, s) then begin
        Result := mcStmt;
        if s <> '' then
          blk.Add(s);
        s := blk.Text;  //.toString;
      end;
    end;
    (mac as TMacro).expanding := False;
    blk.Free;
  end;
//reset parser mode
  fParseMacro := False;
end;

end.

