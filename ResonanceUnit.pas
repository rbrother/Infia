unit ResonanceUnit;

interface

uses
  Forms, Classes, Controls, Grids, Windows, Graphics,
  AdvancedList, ParameterUnit, BandUnit, AssignmentUnit, LinAlg,
  StateUnit, MoleculeUnit, math, NonlinearOptimisation;

const
  ResGridColumns = 8;
  // Resonance field names
  ResMolecule = 'Molecule';
  ResState1 = 'State1';
  ResState2 = 'State2';
  ResElementType = 'ElementType';  // We assume that the resonance is in the upper states
  ResPar1 = 'Parameter1';
  ResPar1Error = 'Par1Error';
  ResPar1Fixed = 'Par1Fixed';
  ResPar2 = 'Parameter2';
  ResPar2Error = 'Par2Error';
  ResPar2Fixed = 'Par2Fixed';
  ResDescription = 'Description';
  // Resonance types
  LResText = 'LResonance';
  CoriolisResText = 'Coriolis';
  FermiResText = 'Fermi';
  UnknownResText = 'UnknownResonance';

type
  TResForm = class(TForm)
    ResDrawGrid: TDrawGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TResType = (LResonance,CoriolisResonance,FermiResonance,UnknownResonance);

  TResonance = class(TSaveListItem)  // Resonance element
  public
    restype : TResType;
    Molecule : TMolecule;
    State1, State2 : TState; // band in resonance
    q, qj : TParameter;
    comment : string;
    InResonance : boolean; // Is the resonance element part of the current block calculation
    HamiltonX, HamiltonY : integer; // Temporary: position of the resonance element in Hamilton matrix block
    constructor Create; override;
    destructor Destroy; override;
    function ElementValue(J : integer) : double;
    procedure SetTypeFromString(s : string);
    function TypeAsString : string;
    procedure SetFieldsFromStrings(strings : TStringList); override;
    function AsCSV : string; override;
    // Grid functionality
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState); override;
    function GetEditText(Col : integer) : string; override;
    procedure SetEditText(Col : integer; s : string); override;
  end;

  TResonances = class(TSaveList)
  public
    constructor Create;
    function CreateEmptyItem : TAdvListItem; override;
    function Res(nRes : integer) : TResonance;
    procedure SetUpDrawGrid(Grid : TDrawGrid); override;
    function GridHeaderName(col : integer) : string; override;
    function ExistState(state : TState) : boolean;
  end;

  TResonanceFit = class(TNewton) // Inherit TNewton!
  public
    MaxUpperJ, MinUpperJ : integer;
    ResBands : TBands;
    Resonances : TResonances;
    Assignments : TAssignments; // Assignments (no pred) of *all* res. system bands
    // Basic optimization functions defined in TNewton
    function DataCount : integer; override;
    procedure CalculateValues; override;
    function YObs(n : integer) : real; override;
    function YCalc(n : integer) : real; override;
    function Weight(n : integer) : real; override;
    // Additional methods
    constructor Create(BaseBand : TBand);
    destructor Destroy; override;
    procedure PrintParameters;
    procedure PrintAssignments;
    procedure TempToOriginal;
    // Calculations
    procedure Calculate;
    procedure PredictPeaks;
  end;

var
  ResForm: TResForm; { The resonance form }
  AllResonances : TResonances;
  ResonanceFit : TResonanceFit;

implementation

uses AnalWin, ScriptUnit, MathUtils, SysUtils, simplex, Utils;

{$R *.DFM}

// ***********   R E S O N A N C E   M E T H O D S   **************

constructor TResonance.Create;
begin
  restype := UnknownResonance;
  q := TParameter.CreateNamed('q',5,5);
  qj := TParameter.CreateNamed('qj',5,10);
  qj.Fixed := true;
  Molecule := UnknownMolecule;
  State1 := UnknownState;
  State2 := UnknownState;
end;

destructor TResonance.Destroy;
begin
  q.Free;
  qj.Free;
end;

function TResonance.ElementValue(J : integer) : double;
var
  Value, VibTerm, RotTerm : double;
  k, JJ : integer;
  // NOTE: The elements do not include the vibrational parts and
  // hence their effect must be deduced by hand after calculation
begin
  ElementValue := 0.0;
  JJ := J*(J+1);
  if restype = LResonance then begin
    if abs(State1.L - State2.L) <> 2 then errormessage('Incorrect L-resonance: abs(State1.L - State2.L) <> 2');
    k := min(State1.L,State2.L) + 1; // the k in the matrix element formula
    // Note: the matrix element value is calculated in the general form
    RotTerm := sqrt( (JJ-k*(k+1)) * (JJ-k*(k-1)) );
    ElementValue := (q.v + qj.v*JJ) * RotTerm;
    end
  else if restype = CoriolisResonance then begin
    if abs(State1.L - State2.L) <> 1 then errormessage('Incorrect Coriolis-resonance: abs(State1.L - State2.L) <> 1');
    k := min(State1.L,State2.L); // the k in the matrix element formula
    RotTerm := sqrt( JJ - k*(k+1) );
    ElementValue := (q.v + qj.v*JJ) * RotTerm;
    end
  else if restype = FermiResonance then begin
    if abs(State1.L - State2.L) <> 0 then errormessage('Incorrect Fermi-resonance: abs(State1.L - State2.L) <> 0');
    ElementValue := q.v + qj.v*JJ;
    end
  else
    errormessage('TFitData.UpperResEnergies: Unknown Resonance type');
end;

procedure TResonance.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState);
begin
  case col of
  0 : TextAlign(canvas, Molecule.Name, rect, AlignLeft);
  1 : TextAlign(canvas, State1.Name, rect, AlignLeft);
  2 : TextAlign(canvas, State2.Name, rect, AlignLeft);
  3 : TextAlign(canvas, TypeAsString, rect, AlignLeft);
  4 : TextAlign(canvas, q.ValueAsString, rect, AlignDecimal);
  5 : TextAlign(canvas, qj.ValueAsString, rect, AlignDecimal);
  6 : TextAlign(canvas, BooleanToText(qj.fixed), rect, AlignCenter);
  7 : TextAlign(canvas, comment, rect, AlignLeft);
  end;
end;

function TResonance.GetEditText(Col : integer) : string;
begin
  GetEditText := '';
  case col of
  0 : GetEditText := Molecule.Name;
  1 : GetEditText := State1.Name;
  2 : GetEditText := State2.Name;
  3 : GetEditText := TypeAsString;
  4 : GetEditText := q.ValueAsString;
  5 : GetEditText := qj.ValueAsString;
  6 : GetEditText := BooleanToText(qj.fixed);
  7 : GetEditText := comment;
  else
    ResForm.ResDrawGrid.Editormode := false;
  end;
end;

procedure TResonance.SetEditText(Col : integer; s : string);
var
  newmol : TMolecule;
begin
  case col of
  0 : begin
        newmol := Molecules.Find(s);
        if newmol <> UnknownMolecule then begin
          // To change the molecule, state table has to have correct states
          Molecule := newmol;
          State1 := States.Find(State1.name, Molecule);
          State2 := States.Find(State2.name, Molecule);
        end;
      end;
  1 : if States.Find(s, Molecule) <> UnknownState then State1 := States.Find(s, Molecule);
  2 : if States.Find(s, Molecule) <> UnknownState then State2 := States.Find(s, Molecule);
  3 : SetTypeFromString(s);
  4 : q.ValueErrorFromString(s);
  5 : qj.ValueErrorFromString(s);
  6 : qj.fixed := TextToBoolean(s);
  7 : comment := s;
  end;
end;

procedure TResonance.SetTypeFromString(s : string);
begin
  if s = LResText then ResType := LResonance
  else if s = CoriolisResText then ResType := CoriolisResonance
  else if s = FermiResText then ResType := FermiResonance
  else ResType := UnknownResonance;
end;

function TResonance.TypeAsString : string;
begin
  if ResType = LResonance then TypeAsString := LResText
  else if ResType = CoriolisResonance then TypeAsString := CoriolisResText
  else if ResType = FermiResonance then TypeAsString := FermiResText
  else TypeAsString := UnknownResText;
end;

procedure TResonance.SetFieldsFromStrings(strings : TStringList);
begin
  molecule := Molecules.Find(strings.values[ResMolecule]);
  state1 := States.Find(strings.values[ResState1], molecule);
  state2 := States.Find(strings.values[ResState2], molecule);
  SetTypeFromString(strings.values[ResElementType]);
  q.v := strtofloatdef(strings.values[ResPar1], 0.0);
  q.e := strtofloatdef(strings.values[ResPar1Error], 0.0);
  q.Fixed := boolean(strtointdef(strings.values[ResPar1Fixed],0));
  qj.v := strtofloatdef(strings.values[ResPar2], 0.0);
  qj.e := strtofloatdef(strings.values[ResPar2Error], 0.0);
  qj.Fixed := boolean(strtointdef(strings.values[ResPar2Fixed],0));
  comment := strings.values[ResDescription];
end;

function TResonance.AsCSV : string;
var
  s : TStringList;
begin
  s := TStringList.create;
  s.values[ResMolecule] := Molecule.Name;
  s.values[ResState1] := State1.Name;
  s.values[ResState2] := State2.Name;
  s.values[ResElementType] := TypeAsString;
  s.values[ResPar1] := floattostr(q.v);
  s.values[ResPar1Error] := floattostr(q.e);
  s.values[ResPar1Fixed] := inttostr(integer(q.fixed));
  s.values[ResPar2] := floattostr(qj.v);
  s.values[ResPar2Error] := floattostr(qj.e);
  s.values[ResPar2Fixed] := inttostr(integer(qj.fixed));
  s.values[ResDescription] := comment;
  AsCSV := s.CommaText;
  s.Free;
end;

// *****************     R E S O N A N C E S    ****************

constructor TResonances.create;
begin
  Capacity := 10;
  ConfirmDelete := true;
  AllowDelete := true;
  AllowInsert := true;
end;

function TResonances.CreateEmptyItem : TAdvListItem;
var
  NewRes : TResonance;
begin
  NewRes := TResonance.Create;
  CreateEmptyItem := NewRes;
end;

function TResonances.Res(nRes : integer) : TResonance;
begin
  Res := TResonance(Items[nRes]);
end;

procedure TResonances.SetUpDrawGrid(Grid : TDrawGrid);
const
  colwidth : array[0..ResGridColumns-1] of integer =
    (70,100,100,100,100,100,60,200);
var
  n : integer;
begin
  with Grid do begin
    ColCount := ResGridColumns;
    for n := 0 to ColCount-1 do ColWidths[n] := ColWidth[n];
  end;
end;

function TResonances.GridHeaderName(col : integer) : string;
const
  headername : array[0..ResGridColumns-1] of string =
    ('Molecule', 'State 1', 'State 2', 'Element type', 'Par1 10^5', 'Par2 10^10', 'Par2 fixed', 'Notes');
begin
  GridHeaderName := headername[col];
end;

function TResonances.ExistState(state : TState) : boolean;
var
  n : integer;
begin
  ExistState := false;
  for n := 0 to count-1 do
  if (res(n).state1 = state) or (res(n).state2 = state) then ExistState := true;
end;

// ***********   R E S O N A N C E   S Y S T E M    ************

constructor TResonanceFit.Create(BaseBand : TBand);
{ Make a resonance system with all bands that are connected to the
  current band through upper state resonance directly or indirectly }
var
  n,m : integer;
  changes : boolean;
begin
  ResBands := TBands.Create;
  Assignments := TAssignments.Create;
  // Initialize Resonance bands
  with Bands do for n := 0 to count-1 do band(n).InResonance := false;
  BaseBand.InResonance := true;
  ResBands.Add(BaseBand);
  // Initialize Resonance elements
  Resonances := TResonances.Create;
  with AllResonances do for n := 0 to count-1 do res(n).InResonance := false;
  // Add bands and resonances to the resonance system until all connected bands have been added
  repeat
    changes := false;
    // Add res elements that concern the bands
    with AllResonances do for n := 0 to count-1 do with res(n) do
      if (not InResonance) and
         (ResBands.ExistUpperstate(state1) or ResBands.ExistUpperstate(state2)) then begin
        InResonance := true;
        Resonances.Add(res(n));
        changes := true;
      end;
    // add all bands that concern the elements
    with Bands do for n := 0 to count-1 do with band(n) do
      if (not InResonance) and Resonances.ExistState(UpperState) then begin
        InResonance := true;
        ResBands.Add(band(n));
        changes := true;
      end;
  until not changes;
  // Make a global assignment set: It is automatically sorted according to m
  with ResBands do for n := 0 to count-1 do Assignments.LinkAssignments(band(n).assignments, false);
  // Find Min and Max J in upper state
  MaxUpperJ := 0; MinUpperJ := 1000;
  for n := 1 to Assignments.count do with Assignments[n] do begin
    if J1 < MinUpperJ then MinUpperJ := J1;
    if J1 > MaxUpperJ then MaxUpperJ := J1;
  end;
  // Display summary
  ClearDisplay;
  display(format('Initialising a resonance system of %d bands:',[resbands.count]));
  with ResBands do for n := 0 to count-1 do display(band(n).name);
  display('Resonance elements:');
  with Resonances do for n := 0 to count-1 do display(res(n).TypeAsString);
  display(format('Number of assignments: %d',[Assignments.count]));
  display(format('MinUpperJ = %d, MaxUpperJ = %d',[MinUpperJ, MaxUpperJ]));
end;

function TResonanceFit.DataCount : integer;
begin
  DataCount := Assignments.Count;
end;

function TResonanceFit.YObs(n : integer) : real;
begin
  YObs := Assignments[n].Peak.Wn;
end;

function TResonanceFit.YCalc(n : integer) : real;
begin
  YCalc := Assignments[n].WnCalc;
end;

function TResonanceFit.Weight(n : integer) : real;
begin
  Weight := 1.0; //Assignments[n].Weight;  // debugging
end;

procedure TResonanceFit.Calculate;
var
  n : integer;
  // Optimization methods
  optmethod : string;
  converged : boolean;
//  Simplex : TSimplex;
begin
  // Initialize parameters of the fit: Take upper state parameters of
  // all bands except when Parameter is fixed.
  // Make improvement: If Parameter is already in the fit, do not add it twice
  // (eq. when 2 bands share same upper state, or have same vib. energy but different B)
  Par := TParameters.Create;
  with ResBands do for n := 0 to count-1 do with band(n).UpperState do begin
    OriginalToTemp; // Start using temporary parameters
    if not Energy.fixed then Par.Add(Energy);
    if not B.fixed then Par.Add(B);
    if not D.fixed then Par.Add(D);
    if not H.fixed then Par.Add(H);
  end;
  with Resonances do for n := 0 to count-1 do with res(n) do begin
    q.UseTemporary;
    qj.UseTemporary;
    if not q.fixed then Par.Add(q);
    if not qj.fixed then Par.Add(qj);
  end;
  // Do the optimization
  optmethod := ProjFile.ReadString('Settings','OptimizationMethod', NewtonMethod);
  converged := false;
  if optmethod = NewtonMethod then begin
    display('Optimizing parameters with Newton method:');
    converged := Iterate;
  end else if optmethod = SimplexMethod then begin
    display('Optimizing parameters with Simplex method: (not implemented)');
    //Simplex := TSimplex.Create(ResonanceRMSError, @FitData, Parameters, Fixed, 0.001);
    //Simplex.Iterate(Parameters);
    //converged := true; // assume convergence always with simplex
    //Simplex.Free;
  end;
  PrintParameters;
  if converged then PredictPeaks;
  // Use original parameters
  with ResBands do for n := 0 to count-1 do band(n).UpperState.UseOriginal;
  with Resonances do for n := 0 to count-1 do begin
    res(n).q.UseOriginal;
    res(n).qj.UseOriginal;
  end;
  // Release resources
  Par.Free;
  display('Ready!');
end;

destructor TResonanceFit.Destroy;
begin
  // Only free the container objects: dont free the bands or assignments inside!
  ResBands.Free;
  Assignments.Free;
end;

procedure TResonanceFit.PrintParameters;
var
  n : integer;
begin
  with ResBands do for n := 0 to count-1 do band(n).UpperState.Print;
  display('Resonance parameters:');
  with Resonances do for n := 0 to count-1 do begin
    res(n).q.Print;
    res(n).qj.Print;
  end;
end;

procedure TResonanceFit.PrintAssignments;
var
  n: integer;
begin
  for n := 1 to Assignments.count do Assignments[n].print;
end;

procedure TResonanceFit.CalculateValues;
// Assume that resonance on upper states only.
// 9.10.98: After large modifications in data types, seems to work: returns
// correct wavenumbers for a resonance system with correct parameters.
var
  J, n, m, x, y : integer;
  Hamilton : TSymmetricMatrix;
  UpperResEnergies : TVector;
  b : TBand; Upper, Lower : TState;
  Element, UpperEnergy : double;
begin
  // Determine the position of resonance elements in Hamiltonian matrix
  with ResBands do for n := 0 to count-1 do with band(n) do
  with Resonances do for m := 0 to count-1 do begin
    if UpperState = res(m).State1 then res(m).HamiltonX := n+1;
    if UpperState = res(m).State2 then res(m).HamiltonY := n+1;
  end;
  // Create and diagonalize hamilton matrix for each J:
  // store the resonant energies in each states ResRotEnergies array
  for J := 0 to MaxUpperJ+20 do begin
    Hamilton := TSymmetricMatrix.Create(ResBands.count);
    Hamilton.Zero;
    // Set the diagonal values of the Hamilton matrix from unperturbed wavenumbers
    with ResBands do for n := 0 to count-1 do begin
      b := ResBands.band(n);
      UpperEnergy := b.UpperState.TotEnergy(J); /// HERE!!!!!!
      Hamilton[n+1,n+1] := UpperEnergy;
    end;
    // Add the resonance matrix elements to the Hamilton matrix off-diagonal locations
    for n := 0 to Resonances.count-1 do
      with Resonances.res(n) do
        Hamilton[HamiltonX, HamiltonY] := Hamilton[HamiltonX, HamiltonY] + ElementValue(J);
    // Solve the post-resonance energy values of upper states
    UpperResEnergies := Hamilton.OrderedEigenvals;
    // Store the resonance energies to state arrays
    with ResBands do for n := 0 to count-1 do
      with ResBands.band(n).UpperState do
        ResEnergy[J] := UpperResEnergies[n+1];
    UpperResEnergies.Free;
  end;
  // Go through all assignments and set their calculated wavenumbers based
  // on the just calculated resonance energies
  for n := 1 to Assignments.count do with Assignments[n] do begin
    Upper := TBand(Band).UpperState;
    Lower := TBand(Band).LowerState;
    WnCalc := Upper.ResEnergy[J1] - Lower.TotEnergy(J0);
  end;
end;

procedure TResonanceFit.PredictPeaks;
var
  nBand : integer;
begin
  Display('Predicting peak positions for all bands...');
  for nBand := 1 to ResBands.Count do ResBands.band(nBand-1).PredictResonance
end;

procedure TResonanceFit.TempToOriginal;
var
  n : integer;
begin
  with ResBands do for n := 0 to count-1 do band(n).UpperState.TempToOriginal;
  with Resonances do for n := 0 to count-1 do with res(n) do begin
    q.AcceptTemporary;
    qj.AcceptTemporary;
  end;
end;

end.
