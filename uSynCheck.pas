unit uSynCheck;
(* Test for allowed syntax, mostly non-simple types in var/param declarations.
*)

interface

var
  arr: array of Byte;
  ptr: ^Byte;
  aptr: array of ^Byte;

type
  enu = (
    null,
    five=word(5),
    six=-(word(6))
  );
  
//also checked if not exported?
procedure test1(par: array of byte); //okay, also without open array!?
//procedure test2(ppar: ^byte);
//function test3(): ^byte;
//procedure test4(par: array[boolean] of char);

implementation

procedure test1(par: array of byte);
begin
end;

(*
procedure test2(ppar: ^byte);
begin
end;

function test3(): ^byte;
begin
  Result := nil;
end;
*)

end.
