unit uDeclC;
(* C declarations
This version builds parse trees (obsolete?)
  and should build type trees.
*)

interface

uses
  uTablesC;

type
  TDeclaration = object
  private
    procedure Init;
    function  declaration_specifiers: boolean;
    function  init_declarator_list: boolean;
    function  declarator: boolean;
    function  compound_statement: boolean;
  public
    function  declaration(scope: TScope): boolean;
    function  type_name():  boolean;
  end;

function  translation_unit(scope: TScope): boolean;

implementation

uses
  uUI,
  uTokenC,
  uScanC,
  uTablesPrep,
  uExprC;

(*
declarator :
[pointer] direct_declarator

direct_declarator :
identifier
"(" declarator ")"
direct_declarator "[" [constant_expression] "]"
direct_declarator "(" parameter_type_list ")"   /* New_style declarator */
direct_declarator "(" [identifier_list] ")"      /* Obsolete_style declarator */

pointer :
{"*" [type_qualifier_list]}

parameter_type_list :                           /* The parameter list */
parameter_list ["," | "..."]

parameter_list :
parameter_declaration {"," parameter_declaration}

type_qualifier_list :
type_qualifier {type_qualifier}

enum_specifier :
"enum" [identifier] "{" enumerator_list "}"
"enum" identifier

enumerator_list :
enumerator {"," enumerator}

enumerator :
enumeration_constant ["=" constant_expression]

enumeration_constant :
identifier

struct_or_union_specifier :
struct_or_union [identifier] "{" struct_declaration_list "}"
struct_or_union identifier

struct_or_union :
struct
union

struct_declaration_list :
struct_declaration {struct_declaration}

struct_declaration :
specifier_qualifier_list struct_declarator_list ";"

specifier_qualifier_list :
type_specifier [specifier_qualifier_list]
type_qualifier [specifier_qualifier_list]

struct_declarator_list :
struct_declarator {"," struct_declarator}

struct_declarator :
declarator
type_specifier [declarator] ":" constant_expression

parameter_declaration :
declaration_specifiers declarator                /* Named declarator */
declaration_specifiers [abstract_declarator] /* Anonymous declarator */

identifier_list :      /* For old_style declarator */
identifier {"," identifier}

abstract_declarator :         /* Used with anonymous declarators */
pointer
[pointer] direct_abstract_declarator

direct_abstract_declarator :
"(" abstract_declarator ")"
[direct_abstract_declarator] "[" [constant_expression] "]"
[direct_abstract_declarator] "(" [parameter_type_list] ")"

initializer :
assignment_expression
"{" initializer_list [","] "}"  /* For aggregate initialization */

initializer_list :
initializer {"," initializer}

type_name :
specifier_qualifier_list [abstract_declarator]

typedef_name :
identifier

extended_decl_modifier_seq : /*   Microsoft Specific */
{extended_decl_modifier}

extended_decl_modifier :   /* Microsoft Specific */
thread
naked
dllimport
dllexport
*)

function  specifier_qualifier_list(): boolean;  //found?
begin //specifier_qualifier_list
(*
specifier_qualifier_list :
type_specifier [specifier_qualifier_list]
type_qualifier [specifier_qualifier_list]
*)
end;

{ TDeclaration }

function TDeclaration.compound_statement: boolean;
begin
  assert(false, 'todo: compound_statement');
  Result := False;
end;

function TDeclaration.declaration(scope: TScope): boolean;
(*
attribute_seq :            /* attribute_seq is Microsoft Specific */
attribute {attribute}

attribute : one of      /* Microsoft Specific */
__asm __fastcall
__based __inline
__cdecl __stdcall
*)
  //function  attribute_seq:  boolean;
  procedure attribute_seq;
  begin
    //handle calling conventions elsewhere? (declaration_specifiers?)
    //others ignored (#define <empty>)
  end;
(*
external_declaration :       /* Allowed only at external (file) scope */
function_definition
declaration

function_definition :         /* Declarator here is the function declarator */
declaration_specifiers declarator [declaration_list] compound_statement
  //declaration_list for old style definition (arguments)
  //missing declaration_specifiers default to "int"
declaration :
declaration_specifiers [attribute_seq] [init_declarator_list] ";"
                        /* attribute_seq is Microsoft Specific */
*)
begin //external_declaration
  Result := declaration_specifiers; //('');
  if Result then begin
    attribute_seq;  //opt
    init_declarator_list;
    //declaration_list?
  //handle (inline) function definitions
    if i_ttyp = opBeg then begin
      Result := compound_statement;
      skip(opSemi); //??? unexpected ???
    end else if Result then
      Result := expect(opSemi, 'unterminated declaration";"'); //";"
  end else
    Log('error spec', lkErr);
end;

function TDeclaration.declaration_specifiers: boolean;
(*
storage_class_specifier :
auto extern register static typedef
- __declspec "(" extended_decl_modifier_seq ")" /* Microsoft Specific */
+ __inline

extended_decl_modifier_seq : /*   Microsoft Specific */
{extended_decl_modifier}

extended_decl_modifier :   /* Microsoft Specific */
- thread naked dllimport dllexport

type_specifier :
//simple_type_specifierS=
void char int float double
__int8 __int16 __int32 __int64      /* Microsoft Specific */
//complex_type_specifierS=
struct union  //=struct_or_union_specifier
enum  //=enum_specifier
//type_modifiers=
short long signed unsigned
//explicit=
typedef_name  //=identifier (TSymType)

type_qualifier :
const volatile
+
attribute : one of      /* Microsoft Specific */
- __asm __based
__inline //in extended_storage_class_specifiers
//calling_conventions=
__cdecl __fastcall __stdcall
*)
begin //declaration_specifiers
(*
declaration_specifiers :
{storage_class_specifier | type_specifier | type_qualifier}
->
{storage_class_specifier | type_qualifier | type_modifier} [type_specifier]
*)
  Init;
  Result := False;  //means: none seen
//collect specifier items
  while True do begin
    if i_ttyp = Kinline then begin //special case
      _inline := True; nextToken; Result := True;
    end else if i_ttyp in extended_storage_class_specifierS then begin
      //Result := storage_class_specifier
      if storage <> DefaultStorage then
        Log('redef storage class', lkDiag);
      self.storage := i_ttyp; nextToken(); Result := True;
    end else if i_ttyp in type_qualifierS then begin
      //Result := type_qualifier
      Result := qualify(i_ttyp, True); nextToken;
    end else if i_ttyp in type_modifierS then begin
      //Result := type_modifier
      case i_ttyp of
      Klong:  if sized = szLong then sized := szLongLong else sized := szLong;
      Kshort: sized := szShort;
      Ksigned, Kunsigned: signed := i_ttyp;
      else
        assert(False, 'unhandled modifier');
      end;
      nextToken; Result := True;
  //various type specifiers
    end else if i_ttyp in simple_type_specifierS then begin
      Result := True;
      type_specifier(i_ttyp);
      nextToken;
    end else if i_ttyp = Kenum then begin
      Result := enum_specifier;
      //done := True;
    end else if i_ttyp in complex_type_specifiers then begin
      Result := struct_or_union;
    //end else if not done and (i_ttyp = t_sym)
    end else if (i_ttyp = t_sym) and (specToken = t_empty)
    and (Globals.isType(ScanText) >= 0) then begin
    //typename - possibly modified?
      Result := True;
      type_specifier(t_sym);  //handle modifiers? Else: makeTypeRef(ScanText);
      nextToken;
    end else //assume declarator name, of default base type "int"
      break;
    nextToken;
  end;
//finish
  if specToken = t_empty then
    type_specifier(Kint);
end;

function TDeclaration.declarator: boolean;
begin
...
end;

function TDeclaration.init_declarator_list: boolean;
begin
...
end;

function  TDeclaration.type_name():  boolean;
begin
(*
type_name :
specifier_qualifier_list [abstract_declarator]
*)
...
end;

// --------------------

function  translation_unit(scope: TScope): boolean;
var
  decl: TDeclaration;
begin
(*
translation_unit :
{external_declaration}
*)
  Result := True;
  while Result and (i_ttyp <> t_eof) do begin
    Result := decl.{external_}declaration(scope);
  end;
end;

end.
