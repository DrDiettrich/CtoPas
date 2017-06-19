(*** config.pas - include file with global $DEFINEs. ***

$DEFINE or undefine the symbols below as appropriate.

Other explicit constants. VARs can be set at runtime.

uFiles:
var
  fIncludeOnlyOnce - include files only once?

uHashList:
{ $DEFINE events} //if change events are desired
{$UNDEF RandomPut}  //code deserves some updates for mutable items (renaming)

uScanC:
const
  fWarnRedef - warn on macro redefinition (for C freaks).
var
  fVerbose - log source lines?
  fLogLines - log non-preprocessor lines?
  fWantRems - show comments?
  OnLineChange - logging callback, can replace fVerbose and fLogLines

uTablesC:
var
  fAutoConst - convert (simple) macros into constants?
  TypePrefix - prefix for type names ('' or 'T' or whatever you like)

uToPas/uTranslator:
const
  fDebugEnums - debug relict.
  fEmptyCall - show empty argument list as "()"?
  fNameMembers - create synthetic names for unnamed struct/union (Record) members.
  fUniqueNames - rename conflicting symbols.
    Rename symbols conflicting with OPL keywords,
    or names which only differ in case (C is case sensitive, Pascal is not!)

**********************************************************)

(* altSym - add alternative (HLL) symbols to the STB
Introduced/Dropped 2006-06
Purpose: handle high-level symbol names.
OFF:  use TSymMacro.altID as reference into the other (Globals, Symbols) table.
ON:   use RSymPrep.altSym for HLL symbols.
  Add stLocal symbol type (untyped, for naming purposes only)
*)
{no more $DEFINE altSym}

(* delayTags - delay creation of synthetic names for untagged structures?
Introduced 2006-06
OFF
  Synthetic names and symbols are created immediately.
ON
  Untagged structures are unnamed until really required.
  The first following typename will become the struct name.
*)
{$DEFINE delayTags}

(* extStorage - use extended storage specifiers?
Introduced 05-2006
  Problem: creation of struct members - symbols not normally to be created!
  extStorage includes inline and struct/union/enum in the allowed storage class.
  Must propagate into proc def - no sym if no scope given!
*)
{.$DEFINE extStorage}

(* ExprTerm - are expressions terminated, or separated?
Introduced 05-2006.
  In old style (ExprTerm) list items have been terminated by ",",
  and expressions have been terminated by ";".
  In new style list items are terminated by ";",
  and expressions are separated by "," (more C-style)
*)
{.$DEFINE ExprTerm}

(* Filter - use common filter function for tokens.
Introduced 07-2004.
  nextToken() and nextRaw() functions with mode and filter arguments.
  Common filter function replaces nextNoEof, nextNoWhite etc.
*)
{$DEFINE Filter}

(* lclScopes - create and use local scopes
Introduced 05-2006
  Another step towards cross compiler.
  Use scopes for parameters, struct/union/enum members, blocks.
    (Not for external procedures?)

Better extend static scopes into Globals and Statics (=module scope).
Procedure prototypes go into the Globals scope,
static procedure definitions go into the Statics scope,
non-static procedures are copies of (or references to) global prototypes,
  owned by the Globals scope.
*)
{$DEFINE lclScopes}

(* oneSTB - scopes have a single symbol table
Removed 05-2006 - furthermore only support (the new) single lists.
Introduced 05-2006
  A single symbol table allows to list all symbols in declaration order.
*)
//{$DEFINE oneSTB}

(* opMul - alternate interpretation of ambiguous C operators.
  Define additional unary operators for "*" (^), "&" (@), "+", "-".
  Currently not used, perhaps in the cross compiler?
*)
{$DEFINE opMul}


(* ParseTree - parse expressions etc. into trees?
  maybe used? (uParseTree<-uExprC<-uParseC?)
  Currently unused, perhaps in the cross compiler?

Procedure implementations deserve further considerations.
Blocks can be implemented as real trees, at least for use as nested scopes.
*)
{ $DEFINE ParseTree}

(* PreInclude - process *.defs files?
  Currently not enabled, conflicts with fAutoConst
*)
{ $DEFINE PreInclude}

(* proto - separate prototypes from procedure-definitions
Introduced 05-2006
  This symbol is OFF by default!
ON (okay, so far)
  With prototypes the procedures have two symbols (prototype and definition).
OFF (bug: error saving symbols!!!)
  All non-static definitions go into the Globals scope.
  Local scopes contain static (or local) definitions,
    AND copies of the exported symbols.
  All (global) symbols refer to their defining module/scope.
    Modules never must be destroyed, can only be cleared if required.
  Procedures have a definition member with additional scopes etc.
*)
{.$DEFINE proto}

(* t_num - strict handling of preprocessor numbers.
  I couldn't find any use for that funny number definition.
*)
{$UNDEF t_num}


