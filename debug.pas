unit debug;

interface

uses NonlinearOptimisation, ParameterUnit, linalg;

type

  TTestNewton = class(TNewton)
  public
    function DataCount : integer; override;
    procedure CalculateValues; override;
    function YObs(n : integer) : real; override;
    function YCalc(n : integer) : real; override;
    function Weight(n : integer) : real; override;
  end;

  procedure TestNonlinearFit;
  procedure TestMatrix;

var
  TrueParameters : TParameters;

implementation

uses ScriptUnit;

// ---------------------- TEST MATRIX OPERATIONS -----------------------

procedure TestMatrix;
var
  m : TSquareMatrix;
  m2 : TSymmetricMatrix;
  EigVals : TVector;
  EigVecs : TSquareMatrix;
  i,j : integer;
begin
  display('');
  display('Testing matrix inversion:');
  m := TSquareMatrix.create(2);
  for i := 1 to 2 do for j := 1 to 2 do
    if j=i then m[i,j] := i else m[i,j] := 1;
  m[1,2] := 0.5;
  m.Print();
  m.Invert();
  display('Inverted matrix:');
  m.Print;
  display('Should be: 1.3333, -0.6666, -0.3333, 0.6666');

  display('');
  display('Testing diagonalisation:');
  m2 := TSymmetricMatrix.Create(3);
  for i := 1 to 3 do for j := 1 to 3 do
    if j=i then m2[i,j] := i else m2[i,j] := 1;
  m2[1,1] := 2;
  m2[2,2] := 1;
  m2.Print;
  m2.Diagonalize(EigVals, EigVecs);
  EigVals.Print;
  display('Should be: 4.21431, 1.460811, 0.324869');
  display('Ordered according to diagonal elements:');
  EigVals.Free;
  EigVals := m2.OrderedEigenvals;
  EigVals.Print;
end;

// ---------------------- TEST NONLINEAR FIT -----------------------

function TTestNewton.DataCount : integer;
begin
  DataCount := 16;
end;

procedure TTestNewton.CalculateValues;
begin
  // Don't need to do anyting here
end;

function TTestNewton.YObs(n : integer) : real;
const
  p1 = 0.5; // True parameters
  p2 = 8.0;
var
  x1, x2 : integer;
begin
  x1 := (n-1) div 4 + 3;
  x2 := (n-1) mod 4 + 3;
  YObs := x1*sqr(p1) + x2/(p1 + p2 + x1);
end;

function TTestNewton.YCalc(n : integer) : real;
var
  x1, x2 : integer;
begin
  x1 := (n-1) div 4 + 3;
  x2 := (n-1) mod 4 + 3;
  with par do YCalc := x1*sqr(par(1).v) + x2/(par(1).v+par(2).v+x1);
end;

function TTestNewton.Weight(n : integer) : real;
begin
  Weight := 1.0;
end;

procedure TestNonlinearFit;
var
  Newton : TTestNewton;
  n : integer;
begin
  display('');
  display('TESTING NON-LINEAR OPTIMISATION with GAUSS-NEWTON');  
  Newton := TTestNewton.Create;
  Newton.par := TParameters.CreateConst([2.0,3.0]);
  Newton.debugging := true;
  Newton.Iterate;
  Newton.Free;
  Display('Should converge like this:');
  display('1.062100, 6.33423');
  display('0.648691, 7.74117');
  display('0.517041, 7.98186');
  display('0.500281, 7.99972');
  display('0.500000, 8.00000');
end;

end.
