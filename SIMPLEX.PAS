unit simplex;

interface

{type

  TSimplex = class
  private
    Points, FreeParameters : integer;
    FitData : pointer;
    iter, Hi, Mid, Lo : integer;
    Point : array[1..MaxParameters] of TVector;
    PointValue : array[1..MaxParameters] of double;
    step : double;
    // variables for storage of different function testing points
    Center, Delta, Reflect, Expand, Contract : TVector;
    ReflectValue, ExpandValue : double;
  public
    constructor Create(NewMinFunc : TFuncValue; PFitData : pointer;
     StartPoint : TVector; Fixed : TFixingArray; ErrorFactor : double);
    destructor Destroy; override;
    procedure InitPoints(StartPoint : TVector; ErrorFactor : double; Fixed : TFixingArray);
    procedure Iterate(var par : TVector);
    procedure CalcCenter;
  end;
             }
implementation

// ************   S I M P L E X     R O U T I N E S   *************

{
constructor TSimplex.Create(NewMinFunc : TFuncValue; PFitData : pointer;
  StartPoint : TVector; Fixed : TFixingArray; ErrorFactor : double);
var
  n, parameters : integer;
begin
  MinFunc := NewMinFunc; // Store reference to the function to minimize
  FitData := PFitData;
  parameters := StartPoint.size;
  FreeParameters := 0;
  for n := 1 to Parameters do if not Fixed[n] then inc(FreeParameters);
  Points := FreeParameters + 1;
  for n := 1 to Points do Point[n] := TVector.create(Parameters);
  Center := TVector.create(Parameters);
  Delta := TVector.create(Parameters);
  Reflect := TVector.create(Parameters);
  Expand := TVector.create(Parameters);
  Contract := TVector.create(Parameters);
  InitPoints(StartPoint, ErrorFactor, Fixed);
end;

destructor TSimplex.Destroy;
var
  n : integer;
begin
  for n := 1 to Points do Point[n].Free;
  Center.Free;
  Delta.Free;
  Reflect.Free;
  Expand.Free;
  Contract.Free;
end;

procedure TSimplex.CalcCenter;
var
  n : integer;
begin
  Center.zero;
  for n := 1 to Points do if n <> Hi then Center.Add(Point[n]);
  Center.MulConst(1.0/(Points-1));
end;

procedure TSimplex.InitPoints(StartPoint : TVector; ErrorFactor : double; Fixed : TFixingArray);
// Calculates random initial points near the fiven StartPoints
// ErrorFactor determines, how far away the points should be from the StartPoint
// To fix a parameter, set respective value of Fixed-array to true.
// NOTE: The simplex iteration itself does not even need to know which parameters
//       are fixed - it suffices that those parameters have same values in all points.
var
  i, n : integer;
begin
  // Make all the points near the
  for i := 1 to Points do begin
    Point[i].Assign(StartPoint);
    for n := 1 to Point[i].size do
      if not Fixed[n] then // If parameter is not fixed, make small random modification to it
        if Point[i][n] = 0.0 then
          Errormessage('TSimplex.InitPoints: Free parameter should not start with zero value.')
        else
          Point[i][n] := Point[i][n] * ((1.0-0.5*ErrorFactor)+random*ErrorFactor);
    PointValue[i] := MinFunc(Point[i], FitData);
  end;
end;

procedure TSimplex.Iterate(var par : TVector);
const
  MaxIter = 5000;
var
  i : integer;
begin
  iter := 0;
  display('iter    FuncValue    Step');
  display('=========================');
  repeat // Main loop
    // Find low and hi points
    Hi := 1; Lo := 1;
    for i := 2 to Points do begin
      if PointValue[i] > PointValue[Hi] then Hi := i;
      if PointValue[i] < PointValue[Lo] then Lo := i;
    end;
    Mid := Hi;
    for i := 1 to Points do if i<>Lo then
      if PointValue[i] < PointValue[Mid] then Mid := i;
    // Calculate new progress alternatives
    CalcCenter;
    Delta.Difference(Center, Point[Hi]); // Substract vector Hi to center
    Step := Delta.SquareNorm; // Length
    if iter mod 100 = 0 then begin
      display(format('%4d %.8f %.8f',[iter, PointValue[Lo], Step])); // Display current information
    end;
    Reflect.Sum(Center, Delta);
    ReflectValue := MinFunc(Reflect, FitData); // Calculate function value at reflection point
    if ReflectValue < PointValue[Lo] then begin
      Expand.SumCoeff(Center, 1.0, Delta, 2.0); // go twice the distance
      ExpandValue := MinFunc(Expand, FitData);
      if ReflectValue < ExpandValue
        then Point[Hi].Assign(Reflect)
        else Point[Hi].Assign(Expand);
    end
    else if ReflectValue < PointValue[Mid] then
      Point[Hi].Assign(Reflect) // Reflect
    else if ReflectValue < PointValue[Hi] then
      Point[Hi].SumCoeff(Center, 1.0, Delta, 0.5) // Contract outwards
    else
      Point[Hi].SumCoeff(Center, 1.0, Delta, -0.5); // Contract inwards
    PointValue[Hi] := MinFunc(Point[Hi], FitData); // Function value at the new point
    inc(iter);
  until (iter>MaxIter) or (abs(Step) < 1e-11);
  if (iter>MaxIter) then
    display('NOT CONVERGED after maximum number of iterations')
  else
    display('CONVERGED!');
  par.Assign(Point[Lo]);
end;

}
end.
