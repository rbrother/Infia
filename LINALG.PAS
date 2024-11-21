unit linalg;

interface

uses imslmath, classes, SysUtils;

const
  MaxDegree = 10;
  MaxParameters = 50;

type

  TMatrix = class(TPersistent)
  protected
    Element : PHugeArray;
    function GetE(x,y:integer) : double;
    procedure SetE(x,y:integer; v:double);
    function GetElements : PHugeArray;
  Public
    xsize, ysize : integer;
    property EVal[x,y : integer] : double read GetE write SetE; default;
    property Elements : PHugeArray read GetElements;
    constructor Create(Newxsize,Newysize : integer);
    constructor CreateEmpty;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetSize(NewXsize,NewYsize : integer);
    procedure Zero;
    procedure MulConst(c : double);
    // Addition
    procedure Add(b : TMatrix);
    procedure Sub(b : TMatrix);
    procedure AddCoeff(b : TMatrix; coeff : double);
    procedure Sum(a,b : TMatrix);
    procedure Difference(a,b : TMatrix);
    procedure SumCoeff(a : TMatrix; acoeff : double; b : TMatrix; bcoeff : double);
    // Multiplication
    procedure RightMultiply(b : TMatrix);
    procedure LeftMultiply(a : TMatrix);
    procedure Product(a,b : TMatrix);
    // Large Operations
    procedure Transpose;
    function Transposition : TMatrix;
    // Other
    function SquareNorm : double;
    procedure Print(Form: TFloatFormat; Precision,Digits: Integer); overload;
    procedure Print(); overload;
  end;

  TVector = class(TMatrix)
  protected
    function GetE(y:integer) : double;
    procedure SetE(y:integer; v:double);
  public
    property EVal[y : integer] : double read GetE write SetE; default;
    constructor Create(newysize : integer);
    function size : integer;
    procedure SetSize(NewSize : integer);
    function PolynomeValue(x : double) : double;
  end;

  TSquareMatrix = class(TMatrix)
  public
    constructor Create(Newxysize : integer);
    procedure Invert;
  end;

  TSymmetricMatrix = class(TSquareMatrix)
  protected
    procedure SetE(x,y:integer; v:double);
  public
    property EVal[x,y : integer] : double read GetE write SetE; default;
    procedure Diagonalize(var EigVals : TVector; var EigVecs : TSquareMatrix);
    function OrderedEigenvals : TVector;
  end;

function PolynomeFit(X,Y,Weight,Coeff,CoeffErr : TVector; Degree : integer) : double;

implementation

uses MathUtils, Math, Dialogs, ScriptUnit;

// **********    G E N E R A L   M A T R I X   R O U T I N E S   *********

constructor TMatrix.Create(Newxsize,Newysize : integer);
begin
  xsize := NewXSize;
  ysize := NewYSize;
  Element := AllocMem(xsize*ysize*sizeof(double));
end;

constructor TMatrix.CreateEmpty;
begin
  Element := NIL;
end;

destructor TMatrix.Destroy;
begin
  FreeMem(Element, xsize*ysize*sizeof(double));
end;

procedure TMatrix.Assign(Source: TPersistent);
var
  n : integer;
begin
  if Element<>NIL then FreeMem(Element, xsize*ysize*sizeof(double));
  xsize := TMatrix(Source).xsize;
  ysize := TMatrix(Source).ysize;
  Element := AllocMem(xsize*ysize*sizeof(double));
  for n := 1 to xsize*ysize do Element^[n-1] := TMatrix(Source).Element^[n-1];
end;

procedure TMatrix.SetSize(NewXsize,NewYsize : integer);
// If the matrix exists, resize it. The resised matrix will have
// same values in common elements as the old one.
var
  NewElement : PHugeArray;
  x,y : integer;
begin
  NewElement := AllocMem(NewXSize*NewYSize*sizeof(double));
  if Element = NIL then
    Element := NewElement
  else begin
    for x := 1 to min(xsize, NewXSize) do
      for y := 1 to min(ysize, NewYSize) do
        NewElement^[(y-1)*xsize+(x-1)] := Element^[(y-1)*xsize+(x-1)];
    FreeMem(Element, xsize*ysize*sizeof(double));
    Element := NewElement;
  end;
  xsize := NewXSize;
  ysize := NewYSize;
end;

function TMatrix.GetE(x,y:integer) : double;
begin
  if (x>xsize) or (y>ysize) then errormessage('TMatrix.GetE: element indexes out of range');
  GetE := Element^[(y-1)*xsize+(x-1)];
end;

procedure TMatrix.SetE(x,y:integer; v:double);
begin
  if (x>xsize) or (y>ysize) then errormessage('TMatrix.GetE: element indexes out of range');
  Element^[(y-1)*xsize+(x-1)] := v;
end;

function TMatrix.GetElements : PHugeArray;
begin
  GetElements := Element;
end;

procedure TMatrix.Zero;
var
  n : integer;
begin
  for n := 1 to xsize*ysize do element^[n-1] := 0.0;
end;

procedure TMatrix.MulConst(c : double);
var
  n : integer;
begin
  for n := 1 to xsize*ysize do element^[n-1] := element^[n-1] * c;
end;

procedure TMatrix.Add(b : TMatrix);
// Makes a linear combination of two vectors
begin
  AddCoeff(b, 1.0);
end;

procedure TMatrix.Sub(b : TMatrix);
begin
  AddCoeff(b, -1.0);
end;

procedure TMatrix.AddCoeff(b : TMatrix; coeff : double);
// Makes a linear combination of two vectors
var
  x,y : integer;
begin
  if (xsize<>b.xsize) or (ysize<>b.ysize) then errormessage('TMatrix.Add: can not add matrices with different sizes');
  for x := 1 to xsize do
    for y := 1 to ysize do
      EVal[x,y] := EVal[x,y] + coeff * b[x,y];
end;

procedure TMatrix.Sum(a,b : TMatrix);
// Makes a linear combination of two vectors
begin
  SumCoeff(a,1.0, b,1.0);
end;

procedure TMatrix.Difference(a,b : TMatrix);
// Makes a linear combination of two vectors
begin
  SumCoeff(a,1.0,b,-1.0);
end;

procedure TMatrix.SumCoeff(a : TMatrix; acoeff : double; b : TMatrix; bcoeff : double);
// Makes a linear combination of two vectors
var
  x,y : integer;
begin
  if (a.xsize<>b.xsize) or (a.ysize<>b.ysize) then errormessage('TMatrix.Sum: can not add matrices with different sizes');
  SetSize(a.xsize, a.ysize);
  for x := 1 to a.xsize do
    for y := 1 to a.ysize do
      EVal[x,y] := acoeff*a[x,y] + bcoeff*b[x,y];
end;

function TMatrix.SquareNorm : double;
var
  sum : double;
  x,y : integer;
begin
  sum := 0.0;
  for x := 1 to xsize do
    for y := 1 to ysize do sum := sum + sqr(EVal[x,y]);
  SquareNorm := sqrt(sum);
end;

procedure TMatrix.RightMultiply(b : TMatrix);
var
  a : TMatrix;
begin
  a := TMatrix.CreateEmpty;
  a.Assign(self);
  Product(a,b);
  a.Free;
end;

procedure TMatrix.LeftMultiply(a : TMatrix);
var
  b : TMatrix;
begin
  b := TMatrix.CreateEmpty;
  b.assign(self);
  Product(a,b);
  b.Free;
end;

procedure TMatrix.Product(a,b : TMatrix);
var
  x,y,n : integer;
  sum : double;
begin
  if a.xsize<>b.ysize then errormessage('TMatrix.Product: a.xsize<>b.ysize');
  SetSize(b.xsize, a.ysize);
  for x := 1 to xsize do
    for y := 1 to ysize do begin
      sum := 0.0;
      for n := 1 to a.XSize do
        sum := sum + a[n,y]*b[x,n];
      EVal[x,y] := sum;
    end;
end;

procedure TMatrix.Transpose;
// Exchange rows and columns.
// Tested with 3 x 3 square matrix 9.11.97
var
  x,y : integer;
  Original : PHugeArray;
begin
  Original := Element;
  // Create a new matrix with number of columns and rows exchanged
  switch(xsize, ysize);
  Element := AllocMem(xsize*ysize*sizeof(double));
  for x := 1 to XSize do
    for y := 1 to YSize do
      Element^[(y-1)*xsize+(x-1)] := Original^[(x-1)*ysize+(y-1)];
  FreeMem(Original, xsize*ysize*sizeof(double));
end;

function TMatrix.Transposition : TMatrix;
// Exchange rows and columns.
// Tested with 3 x 3 square matrix 9.11.97
var
  x,y : integer;
  t : TMatrix;
begin
  // Create a new matrix with number of columns and rows exchanged
  t := TMatrix.Create(ysize, xsize);
  for x := 1 to XSize do
    for y := 1 to YSize do
      t[y,x] := EVal[x,y];
  Transposition := t;
end;

procedure TMatrix.Print();
begin
  Print(ffFixed,6,6);
end;

procedure TMatrix.Print(Form: TFloatFormat; Precision,Digits: Integer);
// Floatformat: ffGeneral, ffExponent, ffFixed, ffNumber
// default parameters (ffFixed, 6, 6)
var
  x,y : integer;
  s : string;
begin
  display(format('Matrix size %d x %d. Matrix elements:',[xsize,ysize]));
  for y := 1 to ysize do begin
    s := '';
    for x := 1 to xsize do s := s +
       floattostrF(EVal[x,y],Form,precision,digits) +' ';
    display(s);
  end;
end;

//**************    S Q U A R E   M A T R I X   R O U T I N E S    **************

constructor TSquareMatrix.Create(Newxysize : integer);
begin
  xsize := NewXYSize;
  ysize := NewXYSize;
  Element := AllocMem(xsize*ysize*sizeof(double));
end;

procedure TSquareMatrix.Invert;
// Invert a general square matrix to solve the system of linear equations
var
  Size, ErrorCode : longint;
  n : integer;
  Inv : PHugeArray;
begin
  if XSize <> YSize then Errormessage('TMatrix.Invert: cannot invert non-square matrix');
  Size := XSize;
  Inv := AllocMem(size*size*sizeof(double));
  ErrorCode := InvMatrix(Size, Element, Inv);
  if ErrorCode = 1 then display('Warning: TMatrix.Invert: Matrix badly conditioned');
  if ErrorCode = 2 then Errormessage('TMatrix.Invert: matrix singular');
  for n := 1 to size*size do Element^[n-1] := Inv^[n-1]; // Copy results to original matrix
  FreeMem(Inv, size*size*sizeof(double));
end;

// ***************    Symmetric matrix routines     *************************

procedure TSymmetricMatrix.SetE(x,y:integer; v:double);
begin
  // For a symmetric matrix, both symmetric elements are set
  // with one assignment
  if (x>xsize) or (y>ysize) then errormessage('TSymmetric.SetE: element indexes out of range');
  Element^[(y-1)*xsize+(x-1)] := v;
  Element^[(x-1)*xsize+(y-1)] := v;
end;

procedure TSymmetricMatrix.Diagonalize(var EigVals : TVector; var EigVecs : TSquareMatrix);
var
  size, ErrorCode : longint;
  a,b,x,delta : double;
begin
  if XSize <> YSize then Errormessage('TMatrix.Invert: cannot invert non-square matrix');
  Size := XSize;
  EigVals := TVector.Create(size);
  EigVecs := TSquareMatrix.Create(size);
  if Size = 1 then
    EigVals[1] := Eval[1,1] // Just copy the single element
  else if Size = 2 then begin // 2 x 2 matrix: eigenvalues readily available
    a := Eval[1,1]; b := Eval[2,2]; x := Eval[1,2];
    delta := sqrt(a*a - 2*b*a + b*b + 4*x*x);
    EigVals[1] := 0.5*(a+b+delta); // Bigger eigenvalue first (like in DiagSymMatrix)
    EigVals[2] := 0.5*(a+b-delta);
  end else begin
    ErrorCode := DiagSymMatrix(Size, Element, EigVals.GetElements, EigVecs.GetElements);
    if ErrorCode <> 0 then ErrorMessage('Diagonalize: failed to converge');
  end;
end;

function TSymmetricMatrix.OrderedEigenvals : TVector;
// Sort the eigenvalues to the same order as the
// diagonal elements of the original matrix
var
  EigenVals, OrdVals : TVector;
  EigenVecs : TSquareMatrix;
  // Variables for ordering
  size, n, nn : integer;
  prevmin, newmin : double;
  minindex : integer;
begin
  if xsize < 2 then errormessage('TSymmetricMatrix.OrderedEigenvals: too small matrix');
  Diagonalize(EigenVals, EigenVecs); // Get eigenvalues
  size := Xsize;
  OrdVals := TVector.create(size);
  // Do the ordering
  prevmin := 0.0;
  for n := 1 to Size do begin
    minindex := 1;
    newmin := 1e100; // initiate with a large value
    // Find smallest value that is larger than previous smallest
    for nn := 1 to Size do
      if (EVal[nn,nn] < newmin) and
         (EVal[nn,nn] > prevmin) then begin
            minindex := nn;
            newmin := EVal[nn,nn];
         end;
    // The unordered weigenvalues are in *decreasing* order, so must be careful:
    OrdVals[minindex] := EigenVals[size-n+1];
    prevmin := newmin;
  end;
  OrderedEigenvals := OrdVals;
end;

// ************   V E C T O R   R O U T I N E S    *************

constructor TVector.Create(newysize : integer);
begin
  xsize := 1;
  ysize := NewYSize;
  Element := AllocMem(xsize*ysize*sizeof(double));
end;

function TVector.size : integer;
begin
  size := ysize;
end;

function TVector.GetE(y:integer) : double;
begin
  GetE := Element^[y-1];
end;

procedure TVector.SetE(y:integer; v:double);
begin
  Element^[y-1] := v;
end;

procedure TVector.SetSize(NewSize : integer);
// If the vector exists, resize it. The resised vector will have
// same values in common elements as the old one.
var
  NewElement : PHugeArray;
  y : integer;
begin
  NewElement := AllocMem(NewSize*sizeof(double));
  if Element = NIL then
    Element := NewElement
  else begin
    for y := 1 to min(size, NewSize) do NewElement^[y-1] := Element^[y-1];
    FreeMem(Element, Size*sizeof(double));
    Element := NewElement;
  end;
  xsize := 1;
  ysize := NewSize;
end;

function TVector.PolynomeValue(x : double) : double;
// Gives the value of the polynome
// p = c1 + c2*x + c3*x^2 + ... + cn*x^(n-1)
// where the coefficients cn are the elements of the vector
var
  term : integer;
  p : double;
begin
  p := 0;
  for term := 1 to size do p := p + Eval[term] * IntPower(x, term-1);
  PolynomeValue := p;
end;


// ***************  P O L Y N O M E   F I T   *************

function PolynomeFit(X,Y,Weight,Coeff,CoeffErr : TVector; Degree : integer) : double;
// polynomefit fits an n-order polynome to the (X,Y) points using
// the given weights for each point. Returns the RMS Residual.
// This procedure is used by procedures Fitup and GSCD.
// Tested 29.11.96 with general 5 point data. Gives correct results in 1st and 2nd order fit for coefficients and errors.
var
  n : integer;
  I, J, K : integer; // index variables for matrix manipulation
  S,T : array[0..2*MaxDegree] of double;
  YPred, RMSError, WeightSum : double;
  matrix : TSquareMatrix;
begin
  if Degree > MaxDegree then ErrorMessage(format('Linalg/PolynomeFit: too large degree (%d). Maxdegree=%d',[Degree,MaxDegree]));
  matrix := TSquareMatrix.Create(Degree+1);
  // Setup the Matrix (checked)
  for I := 0 to Degree do begin S[I]:=0.0; S[I+Degree]:=0.0; T[I]:=0.0; end;
  for n := 1 to X.Size do begin
    for K := 0 to Degree do
      T[K] := T[K] + Weight[n]*Y[n]*IntPower(X[n],K);
    for K := 0 to 2*Degree do
      S[K] := S[K] + Weight[n]*IntPower(X[n],K);
  end;
  for J := 0 to Degree do begin
    I := 2*J;
    for K := J to Degree do begin
      matrix[J+1,K+1] := S[I];
      matrix[K+1,J+1] := S[I];
      inc(I);
    end;
  end;
  matrix.invert; // Inverting the matrix solves the system of linear equations
  // Calculate the final coefficients for y = Coeff[0] + Coeff[1]*x + Coeff[2]*x^2 + ...
  for J := 0 to Degree do begin
    Coeff[J+1] := 0;
    for K := 0 to Degree do
      Coeff[J+1] := Coeff[J+1] + matrix[J+1,K+1]*T[K];
  end;
  // Calculate error of unit weight error (=RMS error) of the fit.
  RMSError := 0; WeightSum := 0;
  for n := 1 to X.Size do begin
    // Calculate the value of the polynomial at point X[n]
    YPred := 0; for J := 0 to Degree do
      YPred := YPred + Coeff[J+1] * IntPower(X[n], J);
    RMSError := RMSError + Weight[n] * sqr(YPred - Y[n]);
    WeightSum := WeightSum + Weight[n];
  end;
  RMSError := sqrt(RMSError/WeightSum); // Now the unit weight error is calculated
  // The divider on previous line should really be (Weightsum-Degree-1) but having small weights would then cause problems
  // Calculare then the errors of coefficients
  for J := 0 to Degree do CoeffErr[J+1] := RMSError * sqrt(matrix[J+1,J+1]);
  // Return the RMS error as function value
  matrix.free;
  PolynomeFit := RMSError;
end;

end.
