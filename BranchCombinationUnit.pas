unit BranchCombinationUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids,
  AssignmentUnit, PeakUnit, AdvancedList, StateUnit;

const
  CombGridColumns = 5;

type
  TCombForm = class(TForm)
    CombDrawGrid: TDrawGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TPrediction = class(TAssignment) // To be used in combinebranches prediction
  public
    NearestPeak : TPeak;
    constructor CreateFromAss(assignment : TAssignment; LowerState : TState; deltaM : integer);
    function Delta : double; // difference of pred-obs
    function PeakFitness : double;
    function PeakIntFitness : double;
  end;

  TPredictions = class(TAdvancedList)
  // All predictions of a single lower state / m-shift combination
  public
    constructor create;
    constructor CreateFromAss(Assignments : TAssignments; LowerState : TState; DeltaM : integer);
    function CreateEmptyItem : TAdvListItem; override;
    function Pred(n : integer) : TPrediction;
    function NearestAboveDelta(newdelta : double) : integer;
    function Accepted : integer; // number of best predictions to accept
    function FitnessFunction : double;
    function IntFitnessFunction : double;
  end;

  TCombination = class(TViewGridListItem)
  public
    // Single lower state / m-shift combination
    DeltaM : integer;
    LowerState : TState;
    Fitness, IntFitness : double;
    constructor create; override;
    constructor CreateFromAss(Assignments : TAssignments;
                NewLowerState : TState; NewDeltaM : integer);
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas;
              CellState: TGridDrawState); override;
  end;

  TCombinations = class(TGridViewList)
  public
    Predictions : TPredictions; // Store only predictions of one combination at a time
    constructor create;
    function CreateEmptyItem : TAdvListItem; override;
    function Comb(n : integer) : TCombination;
    procedure SetUpDrawGrid(Grid : TDrawGrid); override;
    function GridHeaderName(col : integer) : string; override;
    procedure DrawGridSelectCell(Sender: TObject; Col,
       Row: Integer; var CanSelect: Boolean); override;
    function NearestAboveFitness(newfitness : double) : integer;
  end;

var
  CombForm: TCombForm;
  Combinations : TCombinations;
  Predictions : TPredictions;
  PrevCombRow : integer;

implementation

uses Math, MathUtils, AnalWin, Utils, Specwin, BandUnit;

{$R *.DFM}

//****************   P R E D I C T I O N   ****************

constructor TPrediction.CreateFromAss(assignment : TAssignment; LowerState : TState; deltaM : integer);
// Calculate a prediction in the opposing branch from the assignment
// using combination difference formula
var
  DeltaPR : double;
begin
  Assign(assignment); // Copy info of the assignment
  m := -(m + deltam)-1; // Use modifier and change branch
  // Calculate the difference between J+1 -> J (P) and J-1 -> J (R). No H in account
  with LowerState do DeltaPR := (4.0*B.v-6.0*D.v)*(J1+0.5) - 8.0*D.v*IntPower(J1+0.5, 3);
  if m > 0 then WnCalc := Peak.wn + DeltaPR else WnCalc := Peak.wn - DeltaPR;
  NearestPeak := Peaks.NearestPeak(WnCalc);
end;

function TPrediction.Delta : double; // difference of pred-obs
begin
  Delta := abs(WnCalc - NearestPeak.Wn);
end;

function TPrediction.PeakFitness : double;
begin
  PeakFitness := ln(max(delta, PeakError)/PeakError);
end;

function TPrediction.PeakIntFitness : double;
var
  d1, d2 : double;
begin
  if (Peak.Intensity = 0) or (NearestPeak.Intensity = 0) then PeakIntFitness := 1.0
  else begin
    d1 := Peak.Intensity / NearestPeak.Intensity;
    d2 := NearestPeak.Intensity/Peak.Intensity;
    PeakIntFitness := sqr(max(d1,d2)-1.0);
  end;
end;

//****************   P R E D I C T I O N S   *****************

constructor TPredictions.create;
begin
  Capacity := 200;
end;

function TPredictions.CreateEmptyItem : TAdvListItem;
var
  NewPred : TPrediction;
begin
  NewPred := TPrediction.Create;
  CreateEmptyItem := NewPred;
end;

function DeltaValue(Item : TObject) : double;
begin
  DeltaValue := TPrediction(Item).Delta;
end;

function TPredictions.NearestAboveDelta(newdelta : double) : integer;
begin
  NearestAboveDelta := NearestAbove(DeltaValue, newdelta);
end;

function TPredictions.Pred(n : integer) : TPrediction;
begin
  Pred := TPrediction(Items[n]);
end;

constructor TPredictions.CreateFromAss(Assignments : TAssignments; LowerState : TState; DeltaM : integer);
// Create predictions in the P (or R) branch using assignments
// from R (or P) branch and the combination difference principle.
var
  n, firstm, lastm : integer;
  NewPrediction : TPrediction;
  newindex : integer;
begin
  inherited Create;
  firstm := Assignments.FirstAss.m;
  lastm := Assignments.LastAss.m;
  for n := 1 to Assignments.count do with Assignments[n] do
  if ((AssType = Assigned) or (AssType = Predicted)) and
     ((m >= firstm) or (m <= lastm)) then begin
    NewPrediction := TPrediction.CreateFromAss(Assignments[n], LowerState, DeltaM);
    newindex := NearestAboveDelta(NewPrediction.delta);
    Insert(newindex,NewPrediction);
  end;
end;

function TPredictions.Accepted : integer;
var
  DiscardRatio : double;
begin
  DiscardRatio := ProjFile.ReadInteger('Settings','DiscardPercentage', 0) * 0.01;
  Accepted := round(count*(1.0-DiscardRatio)); // Number of best matches to accept
end;

function TPredictions.FitnessFunction : double;
// Compare only accepted lines: assume best lines are first
var
  n : integer;
  PeakFitness, sum : double;
begin
  sum := 0.0;
  for n := 0 to Accepted-1 do Sum := Sum + pred(n).PeakFitness;
  FitnessFunction := (Sum / Accepted) * 100.0; // Scale the result by convenient constant
end;

function TPredictions.IntFitnessFunction : double;
var
  n : integer;
  IntFactor, Sum : double;
begin
  Sum := 0.0;
  // Now let's see, how well the predicted line positions match those in the spectra
  for n := 0 to Accepted-1 do Sum := Sum + TPrediction(items[n]).PeakIntFitness;
  IntFitnessFunction := (Sum / Accepted) * 10.0;
end;

//****************   C O M B I N A T I O N      ****************

constructor TCombination.create;
begin
  DeltaM := 0;
  LowerState := UnknownState;
end;

constructor TCombination.CreateFromAss(Assignments : TAssignments; NewLowerState : TState; NewDeltaM : integer);
begin
  DeltaM := NewDeltaM;
  LowerState := NewLowerState;
  Predictions.Free;
  Predictions := TPredictions.CreateFromAss(Assignments, LowerState, DeltaM);
  Fitness := Predictions.FitnessFunction;
  IntFitness := Predictions.IntFitnessFunction;
end;

procedure TCombination.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState);
begin
  Case Col of
  0 : TextAlign(canvas, inttostr(DeltaM), rect, AlignRight);
  1 : TextAlign(canvas, LowerState.Molecule.Name, rect, AlignLeft);
  2 : TextAlign(canvas, LowerState.Name, rect, AlignLeft);
  3 : TextAlign(canvas, floattostrF(Fitness, ffFixed, 18, 1), rect, AlignRight);
  4 : TextAlign(canvas, floattostrF(IntFitness, ffFixed, 18, 1), rect, AlignRight);
  end;
end;

//****************   C O M B I N A T I O N S    ****************

constructor TCombinations.create;
begin
  Capacity := 1000;
end;

function TCombinations.CreateEmptyItem : TAdvListItem;
var
  NewComb : TCombination;
begin
  NewComb := TCombination.Create;
  CreateEmptyItem := NewComb;
end;

function TCombinations.Comb(n : integer) : TCombination;
begin
  Comb := TCombination(Items[n]);
end;

function CombFitnessValue(Item : TCollectionItem) : double;
begin
  CombFitnessValue := TCombination(Item).Fitness;
end;

procedure TCombinations.SetUpDrawGrid(Grid : TDrawGrid);
const
  ColWidth : array[0..CombGridColumns-1] of integer =
    (50,60,100,60,60);
var
  n : integer;
begin
  with Grid do begin
    ColCount := CombGridColumns;
    RowCount := max(Count+1, 2);
    FixedRows := 1;
    for n := 0 to ColCount-1 do ColWidths[n] := ColWidth[n];
  end;
end;

function TCombinations.GridHeaderName(col : integer) : string;
const
  HeaderName : array[0..CombGridColumns-1] of string =
    ('Delta m', 'Molecule', 'Lower State', 'Fitness', 'Int Fitness');
begin
  GridHeaderName := HeaderName[col];
end;

function FitnessValue(Item : TObject) : double;
begin
  FitnessValue := TCombination(Item).Fitness;
end;

function TCombinations.NearestAboveFitness(newfitness : double) : integer;
begin
  NearestAboveFitness := NearestAbove(FitnessValue, newfitness);
end;

procedure TCombinations.DrawGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  if Row <> PrevCombRow then begin
    ActiveBand.FillOpposing(Combinations.comb(Row-1));
    AssignmentForm.AssDrawGrid.Invalidate;
    SpectraWindow.Invalidate;
    PrevCombRow := Row;
  end;
end;


end.
