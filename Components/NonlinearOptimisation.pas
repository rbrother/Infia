unit NonlinearOptimisation;

interface

uses classes, linalg, sysutils, ParameterUnit, AssignmentUnit;

const
  SimplexMethod = 'Simplex';
  NewtonMethod = 'Newton';

type

  // Class to implement nonlinear least squares fit with multidim newton method
  TNewton = class
  public
    Par : TParameters;
    debugging : boolean; // More verbose output if this is set to true
    constructor Create;
    function DataCount : integer; virtual; abstract;
    procedure CalculateValues; virtual; abstract;
    function YObs(n : integer) : real; virtual; abstract;
    function YCalc(n : integer) : real; virtual; abstract;
    function Weight(n : integer) : real; virtual; abstract;
    function Residuals : TVector;
    function RMSResidual : double;
    function StandardError : double;
    function Iterate : boolean;
    function IterateOnce : double;
    function DerivativeMatrix : TMatrix;
    procedure CalcParErrors;
  end;

implementation

uses ScriptUnit;

// ********  N O N L I N E A R   L E A S T   S Q U A R E   F I T   **********

constructor TNewton.Create;
begin
  debugging := false; // Assume no debugging - set it manuallt if needed
end;

function TNewton.Residuals : TVector;
// Returns the weighted residuals of observed minus calculated values as a vector
var
  n : integer;
begin
  Residuals := TVector.Create(DataCount);
  for n := 1 to DataCount do
    Residuals[n] := (YObs(n) - YCalc(n)) * Weight(n);
end;

function TNewton.RMSResidual : double;
// Reutrn the RMS residual as function value
var
  sum, weightsum : double;
  n : integer;
begin
  sum := 0.0;
  weightsum := 0.0;
  for n := 1 to DataCount do begin
    sum := sum + sqr(YObs(n) - YCalc(n)) * Weight(n);
    weightsum := weightsum + Weight(n);
  end;
  if weightsum = 0.0 then errormessage('TOptimizationData.RMSResidual: weightsum = 0');
  RMSResidual := sqrt(sum/weightsum);
end;

function TNewton.StandardError : double;
begin
  StandardError := RMSResidual * DataCount / (DataCount - Par.count);
end;

function TNewton.Iterate : boolean;
const
  MaxIterations = 100;
var
  iteration : integer;
  RMSError, PrevError, StartError : double;
  converged, failed : boolean;
begin
  iteration := 0;
  converged := false;
  failed := false;
  CalculateValues; // Calculate with starting parameters
  RMSError := RMSResidual;
  StartError := RMSError;
  display(format('Starting: Weighted RMSError = %.8f',[RMSError]));
  if debugging then par.Print;
  repeat
    inc(Iteration);
    PrevError := RMSError;
    RMSError := IterateOnce;
    display(format('Iteration %d: RMSError = %.8f',[Iteration, RMSError]));
    if debugging then par.Print; // Print parameter values for debugging
    if (iteration >= MaxIterations) or
       (RMSError > 1000.0*StartError) then failed := true;
    if RMSError < 10e-10 then converged := true; // Ready is error is small
    // Ready if error has been stabilised (no change over an iteration)
    if abs(RMSError-PrevError)/RMSError < 0.001 then converged := true;
  until failed or converged;
  if failed then display('No convergence achieved.')
  else display('Optimization converged.');
  Iterate := not failed;
end;

function TNewton.IterateOnce : double;
// Makes one Newton iteration with weighting. Returns the RMS error after the iteration.
// Returns the RMS residual after the step
// Tested 18.1.1999 against mathematica opt. program - identical results
var
  DerMatrix, TransposeMatrix : TMatrix;
  SquareMatrix : TSquareMatrix;
  DeltaY, DeltaX : TVector;
  RMSResidual1, RMSResidual2 : double;
  n : integer;
begin
  RMSResidual1 := RMSResidual; // Get original RMS Residual
  // Construct matrices
  SquareMatrix := TSquareMatrix.CreateEmpty;
  DeltaX := TVector.Create(DataCount);
  // Calculate
  DeltaY := Residuals;
  DerMatrix := DerivativeMatrix; //DerMatrix.Print;  // debuggning
  TransposeMatrix := DerMatrix.Transposition;
  SquareMatrix.Product(TransposeMatrix, DerMatrix);
  if debugging then SquareMatrix.Print; // debugging
  SquareMatrix.invert;
  SquareMatrix.RightMultiply(TransposeMatrix);
  DeltaX.Product(SquareMatrix, DeltaY);
  par.Modify(DeltaX);
  CalculateValues; // Calculate new wavenumbers since parameters modified
  RMSResidual2 := RMSResidual;
{  // Look, how well we did with the step:
  if RMSResidual2 > RMSResidual1 * 2.0 then begin // Not doing well... try taking halfstep back
    DeltaX.MulConst(-0.5);
    par.Modify(DeltaX); // Remove the change that was done to parameters
    CalculateValues; // Calculate new wavenumbers since parameters modified
  end;
}
  // Free resources
  DeltaX.Free;
  DeltaY.Free;
  SquareMatrix.Free;
  TransposeMatrix.Free;
  DerMatrix.Free;
  // Return the RMS Error
  IterateOnce := RMSResidual;
end;

procedure TNewton.CalcParErrors;
var
  n : integer;
  StdDev : double;
  DerMatrix, TransposeMatrix : TMatrix;
  SquareMatrix : TSquareMatrix;
begin
  StdDev := StandardError;
  // Calculate (Trans(J)*J)^-1 where J is the Jacobi derivative matrix
  SquareMatrix := TSquareMatrix.CreateEmpty;
  DerMatrix := DerivativeMatrix;
  TransposeMatrix := DerMatrix.Transposition;
  SquareMatrix.Product(TransposeMatrix, DerMatrix);
  SquareMatrix.invert;
  for n := 1 to par.count do
    par.par(n).e := StdDev * sqrt(SquareMatrix[n,n]);
  DerMatrix.Free;
  TransposeMatrix.Free;
  SquareMatrix.Free;
end;

function TNewton.DerivativeMatrix : TMatrix;
// Calculate Jacobian derivative matrix
const
  // Constant used to calculate the stepsize in numerical
  // derivative calculation. Problems seem to arise if this is
  // larger than 0.0001 !
  SmallConst = 0.0001;
var
  npar, ndata : integer;
  Jacobian : TMatrix;
  delta : double;
begin
  Jacobian := TMatrix.Create(Par.count, DataCount);
  // Fill the matrix first with values calculated from the present parameter values
  // (we will substract these later from the modified values later to get f(x+h)-f(x) / h  for the derivative)


  // One by one, modify slightly the parameters. After each modification
  // recalculate wavenumbers and fill one column on the Jacobian
  for npar := 1 to par.count do with par.par(npar) do begin
    delta := v * SmallConst; // Step is a fraction of the value of the parameter
    if Delta = 0 then ErrorMessage('TNewton.DerivativeMatrix: Delta = 0');

    v := v - delta; // Modify downwards...
    CalculateValues;
    // Copy calculated wavenumbers to jacobian column
    for ndata := 1 to DataCount do
      Jacobian[npar, ndata] := YCalc(ndata);
    v := v + 2*delta; // Modify the parameter value upwards...
    CalculateValues;  // ...and recalculate wavenumbers
    for ndata := 1 to DataCount do
      Jacobian[npar, ndata] :=
        YCalc(ndata) - Jacobian[npar, ndata] / (2*delta); // der = f(x+delta)-f(x-delta) / 2delta

    // How exactly should the weights be incorporated??
    // Check and be sure!! Now the weights in resonancefit-class
    // have been set to unity for debugging
    for ndata := 1 to DataCount do Jacobian[npar, ndata] := Jacobian[npar, ndata] * Weight(ndata);

    v := v - Delta; // Return the parameter to original value
  end;
  DerivativeMatrix := Jacobian;
end;

end.
