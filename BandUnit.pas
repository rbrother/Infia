unit BandUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, math,
  StateUnit, linalg, PeakUnit, AssignmentUnit,
  AdvancedList, MoleculeUnit, BranchCombinationUnit, ExtCtrls,
  ParameterUnit;

const
  BandColors = 7;
  BandTypeP = 'P';
  BandTypeR = 'R';
  BandTypePR = 'PR';
  BandTypeQ = 'Q';
  BandGridColumns = 12;
  // Band Field Names
  BandNameText = 'BandName';
  BandTypeText = 'type';
  BandCommentText ='comment';
  BandColor1Text = 'color1';
  BandColor2Text = 'color2';
  BandMoleculeText = 'Molecule';
  BandLowerstateText = 'lowerstate';
  BandUpperstateText = 'upperstate';
  BandRMSResText = 'RMSRes';
  BandOriginText = 'Origin';

type
  TCalcType = (LowerUpperCalc, UpperCalc);

  TBandForm = class(TForm)
    BandDrawGrid: TDrawGrid;
    Panel2: TPanel;
    OuterColorGroup: TGroupBox;
    InnerColorGroup: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CreateColorSelectors(Group : TGroupBox);
    procedure ColorSelectorClicked(Sender : TObject);
  end;

  TBandType = (P,R,PR,Q,UnknownBandType);

  TBand = class(TSaveListItem)
  public
    Assignments : TAssignments;
    Name, Comment : string;
    ColorIndex1, ColorIndex2 : integer;
    BandType : TBandType;
    Molecule : TMolecule; // Store information about the molecule separately
    UpperState, LowerState : TState; // References to the state list
    RMSRes : double;
    Origin : TParameter;
    InResonance : boolean; // Band belongs to the currently analysed resonance system
    // Methods
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure CopyAssignments(Source : TAssignments; IncludePredictions : boolean);
    function col1 : TColor;
    function col2 : TColor;
    function TypeAsString : string;
    function TypeFromString(s : string) : TBandType;
    procedure SetFieldsFromStrings(strings : TStringList); override;
    function AsCSV : string; override;
    function LongInfoString : string; // For exporting data for publications
    class function LongInfoTitle : string; // For exporting data for publications
    procedure DrawBandLabel(MyCanvas : TCanvas; x,y : integer; t : string; FrameColor : TColor);
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState); override;
    function GetEditText(Col : integer) : string; override;
    procedure SetEditText(Col : integer; s : string); override;
    procedure SetActive;
    procedure ExportAssigmentsAsText;
    // Modification and manipulation
    function AddAssignment(NewM : integer; NewAssType : TAssType;
      NewPeak : TPeak; NewWnCalc : double) : TAssignment;
    procedure MergeToPR;
    // calculation
    procedure TempToOriginal; // move temp parameters to original
    procedure OriginalToTemp; // use temporary parameters
    procedure UseOriginal;
    procedure InitFitVariables(var x, y, w, Coeff, CoeffErr : TVector);
    procedure CalculateParameters(CalcType : TCalcType);
    procedure GetPredictionRange(var StartM, EndM : integer);
    procedure PredictPolynome;
    function PolyFitDegree : integer;
    procedure PredictFromParams(NewParams : boolean);
    procedure PredictResonance;
    procedure CoeffToParams(Degree : integer; Coeff, CoeffErr : TVector);
    procedure CoeffToParams4(Coeff, CoeffErr : TVector);
    procedure FillOpposing(Combination : TCombination);
    procedure CombineHalfbands;
  end;

  TBands = class(TSaveList)
  public
    constructor Create;
    function CreateEmptyItem : TAdvListItem; override;
    procedure ExportData;
    function Band(n : integer) : TBand;
    function AddCopyOf(Source : TBand) : TBand;
    function FindBand(TestBandName : string) : TBand;
    function FindIndex(TestBandName : string) : integer;
    procedure RemoveWithState(sta : TState);
    procedure SetUpDrawGrid(Grid : TDrawGrid); override;
    function GridHeaderName(col : integer) : string; override;
    procedure DrawGridSelectCell(Sender: TObject; Col,
      Row: Integer; var CanSelect: Boolean); override;
    procedure AutomaticColors;
    function ExistUpperstate(sta : TState) : boolean;
  end;

  function MarkerColor(colorindex : integer) : TColor;
  procedure DrawBandLabel2(MyCanvas : TCanvas; x,y,width,height : integer;
          color1,color2,FrameColor : TColor);

var
  BandForm: TBandForm;
  ActiveBand : TBand;
  Bands : TBands;
  UnknownBand : TBand;
  PrevBandName : string;

implementation

uses Analwin, Specwin, ScriptUnit, NonLinearOptimisation, utils,
  MathUtils;

{$R *.DFM}

//***************   B A N D   F O R M   ****************

procedure TBandForm.FormCreate(Sender: TObject);
begin
  UnknownBand := TBand.Create;
  CreateColorSelectors(OuterColorGroup);
  CreateColorSelectors(InnerColorGroup);
end;

procedure TBandForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UnknownBand.Free;
end;

procedure TBandForm.CreateColorSelectors(Group : TGroupBox);
var
  n : integer;
  Button : TPanel; // Use panels as buttons!
begin
  for n := 0 to BandColors-1 do begin
    Button := TPanel.Create(Group);
    with Button do begin
      Caption := '';
      top := 20; left := 10 + n*24;
      width := 20; height := 20;
      Color := MarkerColor(n);
      Tag := n; // Save the color code
      OnClick := ColorSelectorClicked;
      Parent := Group; // Make button connected... and visible!
    end;
  end;
  Group.width := 24 * BandColors + 20;
end;

procedure TBandForm.ColorSelectorClicked(Sender : TObject);
var
  colorindex : integer;
begin
  colorindex := TRadioButton(Sender).Tag;
  if TRadioButton(Sender).Parent = OuterColorGroup then
    ActiveBand.ColorIndex1 := colorindex
  else
    ActiveBand.ColorIndex2 := colorindex;
  BandForm.BandDrawGrid.Invalidate;
  SpectraWindow.Invalidate;
end;

//******************    B A N D     ********************

constructor TBand.Create;
begin
  Name := '?';
  BandType := P;
  Assignments := TAssignments.Create;
  Molecule := UnknownMolecule;
  LowerState := UnknownState;
  UpperState := UnknownState;
  Origin := TParameter.CreateNamed('Origin',6,0);
end;

destructor TBand.Destroy;
begin
  Assignments.FreeAssignments;
  Assignments.Free;
  Origin.Free;
end;

procedure TBand.TempToOriginal; // move temp parameters to original
begin
  Origin.AcceptTemporary;
  LowerState.TempToOriginal;
  UpperState.TempToOriginal;
end;

procedure TBand.OriginalToTemp; // use temporary parameters
begin
  Origin.UseTemporary;
  LowerState.OriginalToTemp;
  UpperState.OriginalToTemp;
end;

procedure TBand.UseOriginal;
begin
  Origin.UseOriginal;
  LowerState.UseOriginal;
  UpperState.UseOriginal;
end;

procedure TBand.Assign(Source : TPersistent);
begin
  if Source is TBand then begin
    CopyAssignments(TBand(Source).Assignments, true);
    Name := TBand(Source).Name;
    Comment := TBand(Source).Comment;
    ColorIndex1 := TBand(Source).ColorIndex1;
    ColorIndex2 := TBand(Source).ColorIndex2;
    BandType := TBand(Source).BandType;
    Molecule := TBand(Source).Molecule;
    UpperState := TBand(Source).UpperState;
    LowerState := TBand(Source).LowerState;
    RMSRes := TBand(Source).RMSRes;
    Origin := TBand(Source).Origin;
  end else
    Raise EConvertError.Create(format('Cannot assign band from %s',[Source.ClassName]));
end;

procedure TBand.CopyAssignments(Source : TAssignments; IncludePredictions : boolean);
// Copies assignments from one set to this band. Makes physical copies of the
// assignments.
var
  n : integer;
  newass : TAssignment;
begin
  for n := 1 to Source.count do
  if IncludePredictions or (Source[n].AssType = assigned) then begin
    newass := TAssignment.Create;
    newass.Assign(Source[n]);
    newass.Band := self; // Change band of the copied assignments to this band
    Assignments.add(newass);
  end;
  Assignments.SortM;
end;


function TBand.col1 : TColor;
begin
  col1 := MarkerColor(ColorIndex1);
end;

function TBand.col2 : TColor;
begin
  col2 := MarkerColor(ColorIndex2);
end;

procedure TBand.SetActive;
begin
  ActiveBand := Self;
  Assignments.ConnectDrawGrid(AssignmentForm.AssDrawGrid);
  AssignmentForm.Caption := format('Band %s assignments',[Name]);
  // Enable/disable calculation menu items based on band type
  with AnalyseWindow do
  if (BandType = P) or (BandType = R) then begin
    CalculateAll.enabled := false;
    CalculateUpper.enabled := false;
    HalfBand2.enabled := true;
    Predictfromcalcparams1.enabled := false;
    AcceptResults1.enabled := false;
    end
  else if BandType = PR then begin
    CalculateAll.enabled := true;
    CalculateUpper.enabled := true;
    HalfBand2.enabled := false;
    Predictfromcalcparams1.enabled := true;
    AcceptResults1.enabled := true;
    end
  else if BandType = Q then begin
    CalculateAll.enabled := false;
    CalculateUpper.enabled := true;
    HalfBand2.enabled := false;
    Predictfromcalcparams1.enabled := true;
    AcceptResults1.enabled := true;
  end;
  SpectraWindow.Invalidate;
end;

function TBand.TypeAsString : string;
begin
  if BandType = P then TypeAsString := BandTypeP
  else if BandType = R then TypeAsString := BandTypeR
  else if BandType = PR then TypeAsString := BandTypePR
  else if BandType = Q then TypeAsString := BandTypeQ
  else TypeAsString := '?';
end;

function TBand.TypeFromString(s : string) : TBandType;
begin
  s := Uppercase(s);
  TypeFromString := P; // To avoid 'return value might be undefined...' warning
  if s = BandTypeP then TypeFromString := P
  else if s = BandTypeR then TypeFromString := R
  else if s = BandTypePR then TypeFromString := PR
  else if s = BandTypeQ then TypeFromString := Q
  else TypeFromString := UnknownBandType;
end;

procedure TBand.SetFieldsFromStrings(strings : TStringList);
begin
  Name := strings.values[BandNameText];
  BandType := TypeFromString(strings.values[BandTypeText]);
  Comment := strings.values[BandCommentText];
  colorindex1 := strtointdef(strings.values[BandColor1Text],1);
  colorindex2 := strtointdef(strings.values[BandColor2Text],2);
  Molecule := Molecules.Find(strings.values[BandMoleculeText]);
  LowerState := States.Find(strings.values[BandLowerStateText], molecule);
  UpperState := States.Find(strings.values[BandUpperStateText], molecule);
  RMSRes := StrToFloatDef(strings.values[BandRMSResText]);
  Origin.v := StrToFloatDef(strings.values[BandOriginText]);
end;

function TBand.AsCSV : string;
var
  s : TStringList;
begin
  s := TStringList.create;
  s.values[BandNameText] := Name;
  s.values[BandTypeText] := TypeAsString;
  s.values[BandCommentText] := Comment;
  s.values[BandColor1Text] := inttostr(colorindex1);
  s.values[BandColor2Text] := inttostr(colorindex2);
  s.values[BandMoleculeText] := Molecule.Name;
  s.values[BandLowerStateText] := LowerState.Name;
  s.values[BandUpperStateText] := UpperState.Name;
  s.values[BandRMSResText] := floattostr(RMSRes);
  s.values[BandOriginText] := floattostr(Origin.v);
  AsCSV := s.CommaText;
  s.Free;
end;

procedure TBand.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState);
begin
  case col of
  0 : TextAlign(canvas, Name, rect, AlignLeft);
  1 : DrawBandLabel2(Canvas, Rect.Left+16, Rect.Top+1, 26, 18, Col1, Col2, clBlack);
  2 : TextAlign(canvas, Molecule.Name, rect, AlignLeft);
  3 : TextAlign(canvas, TypeAsString, rect, AlignLeft);
  4 : TextAlign(canvas, UpperState.Name, rect, AlignLeft);
  5 : TextAlign(canvas, LowerState.Name, rect, AlignLeft);
  6 : TextAlign(canvas, Origin.ValueAsString, rect, AlignDecimal);
  7 : TextAlign(canvas, inttostr(Assignments.RealAssignments), rect, AlignRight);
  8 : if Assignments.RealAssignments > 0
        then TextAlign(canvas, inttostr(Assignments.FirstAss.M), rect, AlignRight);
  9 : if Assignments.RealAssignments > 0
        then TextAlign(canvas, inttostr(Assignments.LastAss.M), rect, AlignRight);
  10: TextAlign(canvas, floattostrF(RMSRes,ffFixed,18,5), rect, AlignRight);
  11: TextAlign(canvas, Comment, rect, AlignLeft);
  end;
end;

function TBand.GetEditText(Col : integer) : string;
begin
  GetEditText := '';
  case col of
  0 : GetEditText := name;
  2 : GetEditText := Molecule.Name;
  3 : GetEditText := TypeAsString;
  4 : GetEditText := UpperState.Name;
  5 : GetEditText := LowerState.Name;
  11: GetEditText := Comment;
  else
    BandForm.BandDrawGrid.Editormode := false;
  end;
end;

procedure TBand.SetEditText(Col : integer; s : string);
var
  newmol : TMolecule;
  newupper, newlower : TState;
begin
  case col of
  0 : if Bands.FindBand(s) = UnknownBand then name := s; // Change name, if different from others
  2 : begin
        newmol := Molecules.Find(s);
        if newmol <> UnknownMolecule then begin
          // To change the molecule, state table has to have correct states
          Molecule := newmol;
          LowerState := States.Find(LowerState.name, Molecule);
          UpperState := States.Find(UpperState.name, Molecule);
        end;
      end;
  3 : if TypeFromString(s) <> UnknownBandType then begin
        BandType := TypeFromString(s);
        SetActive; // Updates the menu choises
      end;
  4 : begin
        newupper := States.Find(s, molecule);
        if newupper <> UnknownState then UpperState := newupper;
      end;
  5 : begin
        newlower := States.Find(s, molecule);
        if newlower <> UnknownState then LowerState := newlower;
      end;
  11: Comment := s;
  end;
end;

function TBand.AddAssignment(NewM : integer; NewAssType : TAssType;
  NewPeak : TPeak; NewWnCalc : double) : TAssignment;
var
  NewAss : TAssignment;
begin
  NewAss := TAssignment.create;
  with NewAss do begin
    Band := Self;
    m := NewM;
    Peak := NewPeak;
    WnCalc := NewWnCalc;
    AssType := NewAssType;
    Weight := 1.0;
  end;
  with Assignments do Insert(NearestAboveM(Newass.m)-1,NewAss);
  AddAssignment := NewAss;
end;

class function TBand.LongInfoTitle : string;
var
  s : string;
begin
  s :=     'Name   ';
  s := s + 'Type ';
  s := s + 'Molecule       ';
  s := s + 'LowerState           ';
  s := s + 'UpperState           ';
  s := s + 'RMSRes    ';
  s := s + 'Origin          ';

  // Lisää mahdollisesti myös
  // B" Delta
  // D" Delta
  // H" Delta
  // B'   D'    H'

  LongInfoTitle := s;
end;

function TBand.LongInfoString : string;
begin
  LongInfoString := format('%6s %4s %14s %20s %20s %9.6f %15s ',
    [Name, TypeAsString, Molecule.Name, LowerState.Name, UpperState.Name,
    RMSRes, Origin.ValueAsString]);
end;

procedure TBand.DrawBandLabel(MyCanvas : TCanvas;
          x,y : integer; t : string; FrameColor : TColor);
{ Draw peak band assignment label - high level }
var
  Width : integer;
begin
  Width := MyCanvas.TextWidth(t) + 4;
  DrawBandLabel2(MyCanvas, x,y, Width, 24, Col1, Col2, FrameColor);
  with MyCanvas do begin
    Font.Name := 'Courier New'; Font.size := 8;
    TextOut(x-(Width div 2)+2, y+4, t);
  end;
end;

procedure TBand.MergeToPR;
// Convert the predictions of opposite branch peaks to assignments
var
  SelectedComb : TCombination;
  n : integer;
begin
  SelectedComb := Combinations.Comb(CombForm.CombDrawGrid.Row-1);
  With Assignments do for n := 1 to Count do with Assignments[n] do begin
    if AssType = assigned then m := m + SelectedComb.DeltaM
    else if AssType = opposing then AssType := assigned;
  end;
  BandType := PR;
  LowerState := SelectedComb.LowerState;
  Molecule := LowerState.Molecule;
  SetActive; // Updates the menu choises
end;

procedure TBand.FillOpposing(Combination : TCombination);
// Adds to the band assignments records for the opposing branch predicted lines
// as featured in the Pred table
var
  n : integer;
begin
  AssignmentForm.Freeze(true);
  Assignments.RemovePredictions;
  Predictions.Free; // Free previous predictions
  Predictions := TPredictions.CreateFromAss(Assignments, Combination.LowerState, Combination.DeltaM);
  // Fill the lines
  with Predictions do for n := 0 to Accepted-1 do with pred(n) do
    AddAssignment(m, opposing, NearestPeak, WnCalc);
  // Display discarede lines as predictions
  with Predictions do for n := Accepted to Count-1 do with Pred(n) do
    AddAssignment(m, predicted, NearestPeak, WnCalc);
  Assignments.SortM;
  Assignments.UpdateRowcount;
  AssignmentForm.Freeze(false);
end;

procedure TBand.CalculateParameters(CalcType : TCalcType);
var
  Coeff,CoeffErr,x,y,w : TVector;
  n, Degree : integer;
  wnshift : double;
begin
  ScriptForm.ScriptMemo.Clear;
  Assignments.RemovePredictions;
  if Assignments.count < 2 then exit; // Too few points for any calculation
  InitFitVariables(x, y, w, Coeff, CoeffErr);
  // Calc type specific parameter setup
  if CalcType = LowerUpperCalc then begin
    If LowerState.H.fixed or UpperState.H.fixed then Degree := 4 else Degree := 6;
    wnshift := Assignments[Assignments.count div 2].Peak.Wn; // substract a constant from all wn-values to increase fit accuracy.
  end else begin // CalcType = UpperCalc
    if UpperState.H.Fixed then Degree := 2 else Degree := 3;
    with Assignments[Assignments.count div 2] do wnshift := Peak.wn + LowerState.TotEnergy(J0);
  end;
  // Setup data points and do the fit
  for n := 1 to Assignments.count do with Assignments[n] do begin
    if CalcType = LowerUpperCalc then begin
      x[n] := m;
      y[n] := Peak.wn - wnShift;
    end else if CalcType = UpperCalc then begin // Reduced wavenumber fit
      x[n] := m*(m+1) - sqr(UpperState.L);
      y[n] := Peak.wn + LowerState.TotEnergy(J0) - wnshift;
    end;
    w[n] := Weight;
  end;
  RMSRes := PolynomeFit(x,y,w,Coeff,CoeffErr,Degree); // Do the fit
  x.Free; y.Free; w.Free;
  // Determine rotational parameters from fit results
  OriginalToTemp; // Switch to temporary parameters
  if CalcType = LowerUpperCalc then begin
    Coeff[1] := Coeff[1] + wnshift; // Compensate for shifting the wavenumbers
    CoeffToParams(Degree, Coeff, CoeffErr);
  end else if CalcType = UpperCalc then with UpperState do begin
    Energy.v := Coeff[1] + wnshift;
    Energy.e := SumError(LowerState.Energy.e, CoeffErr[1]);
    Origin.v := Energy.v - LowerState.Energy.v;
    Origin.e := CoeffErr[1];
    B.v := Coeff[2];   B.e := SumError(CoeffErr[2], LowerState.B.e);
    D.v := -Coeff[3];  D.e := SumError(CoeffErr[3], LowerState.D.e);
    if H.Fixed then begin
      H.v := 0; H.e := 0;
    end else begin
      H.v := Coeff[4];  H.e := SumError(CoeffErr[4], LowerState.H.e);
    end;
  end;
  Coeff.Free; CoeffErr.Free;
  // Print the new parameters with comparison to the old ones
  Display(format('Fit results for band %s of %s',[Name, Molecule.name]));
  Display(format('RMS Error of the fit = %.5f',[RMSRes]));
  display('Lower vibrational state:');
  if CalcType = LowerUpperCalc then LowerState.PrintCompare else LowerState.Print;
  display('Upper vibrational state:');
  if UpperState <> UnknownState then UpperState.PrintCompare else UpperState.Print;
  // Predict peaks with new parameters
  PredictFromParams(true);
  UseOriginal; // Return to original parameters
end;

procedure TBand.PredictFromParams(NewParams : boolean);
// Predicts band peak positions when upper and lower state are known.
// NewParams = true uses the recently calculated parameters in New
var
  MaxM, StartM, EndM, newm : integer;
  NewAss : TAssignment;
begin
  if NewParams then
    Assignments.RemovePredictions
  else
    Assignments.RemoveAll; // discard existing assignments and calculate from scratch
  // Do the prediction
  GetPredictionRange(StartM, EndM);
  for newm := StartM to EndM do begin
    // If predicting from old parameters or assignment on given m value is
    // not found, make a new one
    if NewParams and (Assignments.AssNearestM(newm).m = newm) then
      newass := Assignments.AssNearestM(newM)
    else
      newass := AddAssignment(newm, predicted, NIL, 0.0);
    with newass do begin
      WnCalc := UpperState.TotEnergy(J1) - LowerState.TotEnergy(J0); // Peak calc wn
      if newass.asstype = predicted then Peak := Peaks.NearestPeak(WnCalc);
    end;
  end;
  Assignments.UpdateRowCount;
end;

procedure TBand.PredictResonance;
// Use the ResEnergy array that has been calculated to the upper state of the band
// to predict peak positions
var
  MaxM, StartM, EndM, newm : integer;
  NewAss : TAssignment;
begin
  Assignments.RemovePredictions;
  GetPredictionRange(StartM, EndM);
  for newm := StartM to EndM do begin
    // If predicting from old parameters or assignment on given m value is
    // not found, make a new one
    if Assignments.AssNearestM(newm).m = newm then
      newass := Assignments.AssNearestM(newM)
    else
      newass := AddAssignment(newm, predicted, NIL, 0.0);
    with newass do begin
      WnCalc := UpperState.ResEnergy[J1] - LowerState.TotEnergy(J0); // Peak calc wn
      Peak := Peaks.NearestPeak(WnCalc);
    end;
  end;
  ActiveBand.Assignments.UpdateRowCount;
end;

procedure TBand.CoeffToParams(Degree : integer; Coeff, CoeffErr : TVector);
// Calculate the upper and lower state parameters from the polynome fit coefficients
// See documentation for explanation of the formulas used.
var
  L0, L1 : integer;
  D0, D1, B0, B1, H0, H1, E0, E1 : TParameter;
begin
  with Lowerstate do begin
    L0 := L; D0 := D; B0 := B; H0 := H; E0 := Energy; end;
  with Upperstate do begin
    L1 := L; D1 := D; B1 := B; H1 := H; E1 := Energy; end;
  // Do the calculation
  if Degree = 4 then begin
    // Calculate parameter values and errors
    H0.v := 0; H1.v := 0;
    H0.e := 0; H1.E := 0;
    CoeffToParams4(Coeff, CoeffErr);
    end
  else if Degree = 6 then begin
    // Calculate H parameter values and errors
    H1.v := 0.166666667*Coeff[6] + 0.5*Coeff[7];
    H0.v := -Coeff[7] + H1.v;
    H1.e := SumError(0.166666667*CoeffErr[6], 0.5*CoeffErr[7]);
    H0.e := H1.e;
    // Substract away the H-terms from rest of coefficients
    Coeff[5] := Coeff[5] + 3.0*H0.v*(1.0 - sqr(L0)) - 3.0*H1.v*(1-sqr(L1));
    Coeff[4] := Coeff[4] - H0.v*(1.0-6.0*sqr(L0)) - H1.v*(1.0-6.0*sqr(L1));
    Coeff[3] := Coeff[3] - 3.0*H0.v*(sqr(L0)-intpower(L0,4)) + 3.0*H1.v*(sqr(L1)-intpower(L1,4));
    Coeff[2] := Coeff[2] - 3.0*H0.v*intpower(L0,4) - 3.0*H1.v*intpower(L1,4);
    Coeff[1] := Coeff[1] - H0.v*intpower(L0,6) + H1.v*intpower(L1,6);
    // Calculate rest of the parameters as in 4th order fit
    CoeffToParams4(Coeff, CoeffErr);
  end;
end;

procedure TBand.CoeffToParams4(Coeff, CoeffErr : TVector);
// Fit coefficient to molecular parameters for 4th degree fit.
var
  L0, L1 : integer;
  D0, D1, B0, B1, E0, E1 : TParameter;
begin
  L0 := Lowerstate.L;
  L1 := Upperstate.L;
  // Store the resulting parameters in NEW lowerstate and NEW upperstate to preserve the old values
  with Lowerstate do begin D0 := D; B0 := B; E0 := Energy; end;
  with Upperstate do begin D1 := D; B1 := B; E1 := Energy; end;
  E0.v := LowerState.Energy.v; // Lower state energy must be known beforehand
  // Do the calculation
  D0.v := 0.5*Coeff[5] - 0.25*coeff[4];
  D1.v := -0.5*Coeff[5] - 0.25*coeff[4];
  D0.e := SumError(0.5*CoeffErr[5], 0.25*CoeffErr[4]);
  D1.e := D0.e;
  B0.v := 0.5 * (Coeff[2] - Coeff[3] - D1.v + D0.v);
  B1.v := 0.5 * (Coeff[2] + Coeff[3] + D1.v * (1-4*sqr(L1)) - D0.v * (1-4*sqr(L0)));
  B0.e := 0.5 * SumError(CoeffErr[2], CoeffErr[3]);
  B1.e := B0.e;
  E1.v := E0.v + Coeff[1] - B0.v*sqr(L0) + B1.v*sqr(L1) + D1.v*intpower(L1,4) - D0.v*intpower(L0, 4);
  E1.e := SumError(E0.e, CoeffErr[1], B0.e*sqr(L0), B1.e*sqr(L1));
end;

procedure TBand.GetPredictionRange(var StartM, EndM : integer);
// NOTE: It is assumed that the band is free from predictions when this is called
var
  MaxM, PredRange : integer;
begin
  if Assignments.Count = 0 then begin
    MaxM := strtointdef(InputBox('Predict peak positions','Give maximum m to predict','50'),0);
    if MaxM = 0 then exit;
    if BandType = PR then begin
      StartM := -MaxM; EndM := MaxM;
    end else begin // Q branch
      StartM := 1; EndM := MaxM;
    end;
  end else begin
    // Calculate the range around the assigned area
    PredRange := ProjFile.ReadInteger('Settings','PredRange', 10);
    StartM := Assignments[1].m - PredRange;
    EndM := Assignments[Assignments.count].m + PredRange;
  end;
end;

procedure TBand.InitFitVariables(var x, y, w, Coeff, CoeffErr : TVector);
begin
  Coeff := TVector.Create(10);
  CoeffErr := TVector.Create(10);
  x := TVector.Create(Assignments.count);
  y := TVector.Create(Assignments.count);
  w := TVector.Create(Assignments.count);
end;

procedure TBand.PredictPolynome;
var
  x, y, w, Coeff, CoeffErr : TVector;
  n, StartM, EndM, AvgM, m : integer;
  AvgWn, wn : double;
  ThisAss : TAssignment;
begin
  if Assignments.RealAssignments < 2 then exit; // Cant do any prediction
  Assignments.RemovePredictions;
  InitFitVariables(x, y, w, Coeff, CoeffErr);
  GetPredictionRange(StartM, EndM);
  // Reduce X- and Y-values for more accurate prediction fit
  with Assignments[Assignments.count div 2] do begin AvgM := m; AvgWn := Peak.wn; end;
  // Set up data
  for n := 1 to Assignments.count do with Assignments[n] do begin
    x[n] := m - AvgM;
    y[n] := Peak.wn - AvgWn;
    w[n] := Weight;
  end;
  RMSRes := PolynomeFit(x,y,w,Coeff,CoeffErr,PolyFitDegree);
  // Calculate predictions
  for m := StartM to EndM do begin
    Wn := Coeff.PolynomeValue(m-AvgM) + AvgWn; // Calculate the value of the polynome at X[n]
    ThisAss := Assignments.AssNearestM(m); // Assignment nearest to the present m value
    if ThisAss.m = m then ThisAss.WnCalc := wn // assignment exists on this m: change calculate wavenumber
    else AddAssignment(m, predicted, Peaks.NearestPeak(wn), wn);
  end;
  x.Free; y.Free; w.Free; Coeff.Free; CoeffErr.Free;
end;

function TBand.PolyFitDegree : integer;
begin
  if Assignments.count <= 4 then PolyfitDegree := 1
  else if Assignments.count <= 7 then PolyfitDegree := 2
  else if Assignments.count <= 12 then PolyfitDegree := 3
  else begin
    if Lowerstate.H.Fixed or UpperState.H.Fixed then PolyfitDegree := 4
    else if Assignments.count < 20 then PolyFitDegree := 4 else PolyfitDegree := 6;
  end;
end;

procedure TBand.CombineHalfbands;
// Routine originally added 8.1.1997
// This routine compares the selected halfband to *all* peaks in data
// as done in the original fortran program find_j
var
  DeltaM, MaxMError, FirstDeltaM, LastDeltaM : integer;
  nState : integer;
  NewCombination : TCombination;
begin
  Combinations.Free;
  Combinations := TCombinations.Create;
  // Determine the range of m-shifts
  MaxMError := ProjFile.ReadInteger('Settings','MaxMError', 10);
  FirstDeltaM := -MaxMError;
  LastDeltaM := MaxMError;
  if BandType = P then begin
    if Assignments.LastAss.m + LastDeltaM > -1 then
      LastDeltaM := -1 - Assignments.LastAss.m;
  end else if BandType = R then begin
    if Assignments.FirstAss.m + FirstDeltaM < 1 then
      FirstDeltaM := 1 - Assignments.FirstAss.m;
  end;
  // Here come the double nested loops to test combinations
  for nState := 0 to States.count-1 do with States.sta(nState) do
  if (B.v > 0) and UseAsLower then  // Try lower state if it is not empty
    for DeltaM := FirstDeltaM to LastDeltaM do begin // try different m values
      AnalyseWindow.ShowStatus(format('Trying lower state %s of %s with m shift %d',[name,molecule.name,DeltaM]));
      NewCombination := TCombination.CreateFromAss(Assignments, States.sta(nState), DeltaM);
      with Combinations do
        Insert(NearestAboveFitness(NewCombination.Fitness), NewCombination);
    end;
  Combinations.ConnectDrawGrid(CombForm.CombDrawGrid);
  FillOpposing(Combinations.Comb(0));
  AnalyseWindow.CombAssignSpec1Click(nil);
end;

procedure TBand.ExportAssigmentsAsText;
begin
  display('Band '+Name+' assignments:');
  Assignments.ExportAsText;
end;

//******************   B A N D S    ***************

constructor TBands.create;
begin
  Capacity := 200;
  ConfirmDelete := true;
  AllowDelete := true;
  AllowInsert := true;
end;



function TBands.Band(n : integer) : TBand;
begin
  Band := TBand(Items[n]);
end;

function TBands.AddCopyOf(Source : TBand) : TBand;
var
  newband : TBand;
begin
  newband := TBand.Create;
  newband.Assign(Source);
  newband.name := newband.name + '+';
  add(newband);
  UpdateRowCount;
  AddCopyOf := newband;
end;

procedure TBands.DrawGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  if (Row = 0) or (Count = 0) then exit;
  if band(Row-1).name <> PrevBandName then begin
    band(Row-1).SetActive;
    PrevBandName := ActiveBand.Name;
    AssignmentForm.AssDrawGrid.Invalidate;
  end;
end;

procedure TBands.ExportData;
// Write information of all bands to the script window
var
  band : integer;
begin
  display(ActiveBand.LongInfoTitle);
  for band := 0 to count-1 do display(TBand(Items[band]).LongInfoString);
end;

function TBands.FindIndex(TestBandName : string) : integer;
// Returns -1 if band with the given name not found
var
  n : integer;
  found : integer;
begin
  n := 0;
  found := -1;
  while (n<count) and (found<0) do begin
    if TBand(Items[n]).name = TestBandName then found := n;
    inc(n);
  end;
  FindIndex := found;
end;

function TBands.FindBand(TestBandName : string) : TBand;
var
  index : integer;
begin
  index := FindIndex(TestBandName);
  if index >= 0 then FindBand := TBand(Items[index]) else FindBand := UnknownBand;
end;

procedure TBands.SetUpDrawGrid(Grid : TDrawGrid);
const
  ColWidth : array[0..BandGridColumns-1] of integer =
    (50, 35, 75, 40, 110, 110, 130, 50, 50, 50, 70, 200);
var
  n : integer;
begin
  with Grid do begin
    ColCount := BandGridColumns;
    for n := 0 to ColCount-1 do ColWidths[n] := ColWidth[n];
  end;
end;

function TBands.GridHeaderName(col : integer) : string;
const
  HeaderName : array[0..BandGridColumns-1] of string =
    ('Name','Col','Molecule','Type','Upper State','Lower State',
     'Origin', '#Ass', 'Min m', 'Max m', 'RMS Res', 'Comment');
begin
  GridHeaderName := HeaderName[col];
end;

procedure TBands.AutomaticColors;
// Automatically set different color combinations for all bands
var
  color1, color2, n : integer;
begin
  color1 := 0; color2 :=1;
  for n := 0 to count-1 do with band(n) do begin
    colorindex1 := color1;
    colorindex2 := color2;
    inc(color2);
    if color2 = color1 then inc(color2); // don't allow same color
    if color2 >= BandColors then begin
      color2 := 0;
      inc(color1);
      if color1 >= BandColors then color1 := 1;
    end;
  end;
  BandForm.BandDrawGrid.Invalidate;
  Spectrawindow.Invalidate;
end;

procedure TBands.RemoveWithState(sta : TState);
var
  n : integer;
begin
  for n := 0 to Count-1 do
  if (band(n).LowerState = sta) or (band(n).UpperState = sta) then begin
    band(n).Free;
    items[n] := NIL;
  end;
  Pack;
  UpdateRowCount;
  BandForm.BandDrawGrid.Invalidate;
end;

function TBands.ExistUpperstate(sta : TState) : boolean;
var
  n : integer;
begin
  ExistUpperstate := false;
  for n := 0 to count-1 do
  if band(n).UpperState = sta then ExistUpperstate := true;
end;

function TBands.CreateEmptyItem : TAdvListItem;
var
  NewBand : TBand;
begin
  NewBand := TBand.Create;
  CreateEmptyItem := NewBand;
end;

// ****************   H E L P E R   R O U T I N E S    ****************

function MarkerColor(colorindex : integer) : TColor;
const
  Color : array[0..BandColors-1] of TColor =
  ( $00FFFF00, $0000FFFF, $00FF00FF, $00FF0000, $0000FF00,
    $000000FF, $00FFFFFF);
begin
  MarkerColor := Color[colorIndex];
end;

procedure DrawBandLabel2(MyCanvas : TCanvas;
          x,y,width,height : integer; color1,color2 : TColor; FrameColor : TColor);
{ Draw peak band assignment label - low level }
var
  w : integer;
begin
  w := Width div 2;
  with MyCanvas do begin
    Brush.Color := FrameColor;
    FrameRect(rect(x-w,y,x+w,y+height));
    Brush.Color := color1;
    FillRect(rect(x-w+1,y+1,x+w-1,y+height-1));
    Brush.Color := color2;
    FillRect(rect(x-w+1,y+4,x+w-1,y+height-4));
  end;
end;

end.

