unit parsetest;

interface

type
//"S"igned and "U"nsigned types of specific size
	SInt1 = ShortInt;
	SInt2 = SmallInt;
	SInt4 = LongInt;
	SInt8 = Int64;
	UInt1 = Byte;
	UInt2 = Word;
	UInt4 = LongWord;
	//UInt8 = {unsigned}Int64; //no equivalent
//"S"igned and "U"nsigned types of unspecific size
	UChar = Char;
	SChar = Char;
	UShort = UInt2;
	UInt = Cardinal;
	ULong = Cardinal;
	SShort = SInt2;
	SInt = Integer;
	SLong = Integer;

	__int8 = SInt1;
	__int16 = SInt2;
	__int32 = SInt4;
	__int64 = SInt8;
	ULONG = ULong;
	PULONG = ^ULONG;
	FnCallback = Function(arg: SLong): SInt; stdcall;
	S__tag = Record
		ms: Record
		a: SInt;
		b: SInt;
		c: SInt;
	end;
		l: SLong;
	end;
	S_8 = Record
		ms: Record
	case integer of
	0:	(a: SInt);
	1:	(b: SInt);
	2:	(c: {bitfield[5]}SInt);
	end;
		l: SLong;
	end;
	s = S_8;

const
	MAX_PATH = 260;
	ls: {const}PChar = WideString('unicode');
	aChar: {const}PChar = 'x';
	c: {const}SInt = 99;

var
	path: array[0..MAX_PATH-1] of Char;
	i: SInt;
	pubint: SInt;
{static} 	lcllong: SLong;
	LongLong: SInt8;

Function	Proc(arg: SInt){; fastcall};
Procedure	public(); stdcall;
 {static} Procedure	private(); stdcall;

implementation

Procedure	macro(const arg);
begin (*
	puts("macro" ## arg);
*) end;

end.
