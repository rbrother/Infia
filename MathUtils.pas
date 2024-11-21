unit MathUtils;

interface

uses Dialogs, SysUtils;

const
  Sqrt2 = 1.4142135623730950488016887242097;

procedure Switch(var a,b : double); overload;
procedure Switch(var a,b : integer); overload;
function SumError(n1,n2 : double; n3 : double = 0; n4 : double = 0; n5 : double = 0) : double;
function StrToFloatDef(s : string) : double;
function QuickSqrt(x : integer) : real;

implementation

//************    S M A L L   S U P P O R T   R U T I N E S   *************

procedure Switch(var a,b : double); overload;
var
  c : double;
begin
  c := a;
  a := b;
  b := c;
end;

procedure Switch(var a,b : integer); overload;
var
  c : integer;
begin
  c := a;
  a := b;
  b := c;
end;

function SumError(n1,n2 : double; n3 : double = 0; n4 : double = 0; n5 : double = 0) : double;
begin
  SumError := sqrt(sqr(n1)+sqr(n2)+sqr(n3)+sqr(n4)+sqr(n5));
end;

function StrToFloatDef(s : string) : double;
var
  code : integer;
  res : double;
begin
  val(s, res, code);
  if code > 0 then res := 0.0; // Invalid strings give 0.0 for value
  StrToFloatDef := res;
end;

function QuickSqrt(x : integer) : real;
// Uses precomputer square roots for integers
begin
  QuickSqrt := 0;
  if x <= 20 then case x of
  0 : QuickSqrt := 0.0;
  1 : QuickSqrt := 1.0;
  2 : QuickSqrt := 1.4142135623730950488016887242097;
  3 : QuickSqrt := 1.73205080756887729352744634150587;
  4 : QuickSqrt := 2.0;
  5 : QuickSqrt := 2.23606797749978969640917366873128;
  6 : QuickSqrt := 2.44948974278317809819728407470589;
  7 : QuickSqrt := 2.64575131106459059050161575363926;
  8 : QuickSqrt := 2.8284271247461900976033774484194;
  9 : QuickSqrt := 3.0;
  10 : QuickSqrt := 3.16227766016837933199889354443272;
  11 : QuickSqrt := 3.31662479035539984911493273667069;
  12 : QuickSqrt := 3.46410161513775458705489268301174;
  13 : QuickSqrt := 3.6055512754639892931192212674705;
  14 : QuickSqrt := 3.74165738677394138558374873231655;
  15 : QuickSqrt := 3.8729833462074168851792653997824;
  16 : QuickSqrt := 4.0;
  17 : QuickSqrt := 4.12310562561766054982140985597408;
  18 : QuickSqrt := 4.24264068711928514640506617262909;
  19 : QuickSqrt := 4.35889894354067355223698198385962;
  20 : QuickSqrt := 4.47213595499957939281834733746255;
  end
  else QuickSqrt := Sqrt(x);
end;

end.
