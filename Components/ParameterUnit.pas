unit ParameterUnit;

interface

uses SysUtils, ScriptUnit, linalg, math, classes, AdvancedList, grids,
  windows, graphics;

const
  ParGridColumns = 4;
  ParName = 'Name';
  ParValue = 'Value';
  ParError = 'Error';
  ParStatus = 'Status';
  ParFormula = 'formula';

  ParFreeText = 'Free';
  ParPredText = 'Predicated';
  ParFixedText = 'Fixed';
  ParConstText = 'Constant';
  ParCalcText = 'Calculated';

type

  TParameterStatus = (ParFree, ParFixed, ParPredicated, ParConstant, ParCalculated);

  TParameter = class(TSaveListItem)   // Spectral parameter class
  private
    Name : string;
    DecimalPlaces, DecimalShift : integer;
    ValueMult, ErrorMult : double;
    value : array[1..2] of double; // Original and temporary value
    error : array[1..2] of double; // Original and temporary error
    AccessIndex : 1..2; // Use original or temporary
    procedure SetValue(newvalue : double);
    function GetValue : double; virtual;
    procedure SetError(newvalue : double);
    function GetError : double;
    function GetFixedStatus : boolean;
    procedure SetFixedStatus(NewFixed : boolean);
    function GetStatusText : string;
    procedure SetStatusText(NewStatus : string);
  public
    Status : TParameterStatus;
    property StatusText : string read GetStatusText write SetStatusText;
    property fixed : boolean read GetFixedStatus write SetFixedStatus;
    property v : double read GetValue write SetValue; // Value
    property e : double read GetError write SetError; // Error
    constructor create; override;
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState); override;
    function GetEditText(Col : integer) : string; override;
    procedure SetEditText(Col : integer; s : string); override;
    procedure SetFieldsFromStrings(strings : TStringList); override;
    function AsCSV : string; override; // Item field names and values as strings
    constructor CreateNamed(NewName : string; NewDecimalPlaces, NewDecimalShift : integer);
    procedure SetDecimalShift(NewDecimalShift : integer);
    procedure AcceptTemporary;  // Copy temp to orig and move to use original parameters
    procedure UseOriginal; // Move to use original parameters
    procedure UseTemporary; // Copy orig to temp and use temp
    function ValueAsString : string;
    function DeltaAsString : string;
    function QualityAsString : string;
    procedure ValueErrorFromString(s : string);
    procedure Print;
    procedure PrintCompare;
  end;

  TCalcParameter = class(TParameter)
  private
    function GetValue : double; override;
  public
    formula : pointer;
    FormulaText : string;
    NeedsCalculation : boolean;
    constructor create; override;
    destructor Destroy; override;
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState); override;
    procedure SetFieldsFromStrings(strings : TStringList); override;
    function AsCSV : string; override;
    procedure ParseFormula;
    procedure Calculate;
  end;

  TParameters = class(TSaveList)
  public
    constructor CreateConst(a : array of real);
    function CreateItem(fields : tstringlist) : TSaveListItem; override;
    destructor Destroy; override;
    procedure ReCalculate;
    procedure ParseFormulas;
    procedure SetUpDrawGrid(Grid : TDrawGrid); override;
    function GridHeaderName(col : integer) : string; override;
    function CreateEmptyItem : TAdvListItem; override;
    function par(n : integer) : TParameter;
    procedure Modify(delta : TVector);
    procedure Print;
    function Find(FindName : string) : TParameter;
  end;

  var
    // This is *NOT* used by Infia: rather by the formula parsing unit
    FitParameters : TParameters;

implementation

Uses MathUtils, Utils, FormulaUnit;

// ***********   Parameter Methods **************

constructor TParameter.create;
begin
  Name := 'p';
  DecimalPlaces := 8;
  SetDecimalShift(0);
  Status := ParFree;
  AccessIndex := 1; // Original parameters
end;

constructor TParameter.CreateNamed(NewName : string; NewDecimalPlaces, NewDecimalShift : integer);
begin
  Name := NewName;
  DecimalPlaces := NewDecimalPlaces;
  SetDecimalShift(NewDecimalShift);
  Status := ParFree;
  AccessIndex := 1; // Original parameters
end;

function TParameter.GetStatusText : string;
begin
  case Status of
    ParFree : GetStatusText := ParFreeText;
    ParFixed : GetStatusText := ParFixedText;
    ParConstant : GetStatusText := ParConstText;
    ParPredicated : GetStatusText := ParPredText;
    ParCalculated : GetStatusText := ParCalcText;
  end;
end;

procedure TParameter.SetStatusText(NewStatus : string);
var
  start : string;
begin
  start := UpperCase(copy(NewStatus,1,2));
  if start = UpperCase(copy(ParFreeText,1,2)) then Status := ParFree
  else if start = UpperCase(copy(ParFixedText,1,2)) then Status := ParFixed
  else if start = UpperCase(copy(ParPredText,1,2)) then Status := ParPredicated
  else if start = UpperCase(copy(ParConstText,1,2)) then Status := ParConstant;
//  else if start = UpperCase(copy(ParCalcText,1,2)) then Status := ParCalculated;
end;

function TParameter.GetFixedStatus : boolean;
begin
  GetFixedStatus := ((Status = ParFixed) or (Status = ParConstant));
end;

procedure TParameter.SetFixedStatus(NewFixed : boolean);
begin
  if NewFixed then Status := ParFixed else Status := ParFree;
end;

procedure TParameter.SetDecimalShift(NewDecimalShift : integer);
begin
  DecimalShift := NewDecimalShift;
  ValueMult := intpower(10.0, DecimalShift);
  ErrorMult := intpower(10.0, DecimalShift + DecimalPlaces);
end;

procedure TParameter.SetValue(newvalue : double);
begin
  value[AccessIndex] := newvalue;
end;

function TParameter.GetValue : double;
begin
  GetValue := value[AccessIndex];
end;

procedure TParameter.SetError(newvalue : double);
begin
  error[AccessIndex] := newvalue;
end;

function TParameter.GetError : double;
begin
  GetError := error[AccessIndex];
end;

procedure TParameter.AcceptTemporary;  // Copy temp to orig and move to use original parameters
begin
  value[1] := value[2];
  error[1] := error[2];
  UseOriginal;
end;

procedure TParameter.UseOriginal; // Move to use original parameters
begin
  AccessIndex := 1;
end;

procedure TParameter.UseTemporary; // Copy orig to temp and use temp
begin
  value[2] := value[1];
  error[2] := error[1];
  AccessIndex := 2;
end;

procedure TParameter.Print;
var
  s : string;
begin
  s := format('%-3s * 10^%-2d = %s',[Name, DecimalShift, ValueAsString]);
  Display(s);
end;

procedure TParameter.PrintCompare;
var
  s : string;
begin
  s := format('%-3s * 10^%-2d = %s',[Name, DecimalShift, ValueAsString]);
  s := s + ' ('+ StatusText +')';
  s := s + '  delta to fixed = ' + DeltaAsString;
  s := s + ' ' + QualityAsString;
  Display(s);
end;

function TParameter.ValueAsString : string;
// Returns the value in format  1.22344(56)
begin
  ValueAsString := floattostrF(v * ValueMult,ffFixed,18,decimalplaces)+
                   '('+inttostr(trunc(e * ErrorMult))+')';
end;

procedure TParameter.ValueErrorFromString(s : string);
// String must be of form   "1234.54566(566)" where the
// brackets show parameter error in last decimal places
var
  StartBracket, EndBracket, Decimals : integer;
  code, code2 : integer;
  a1,a2 : double;
begin
  StartBracket := pos('(',s);
  EndBracket := pos(')',s);
  Decimals := StartBracket - pos('.',s) - 1;
  if (StartBracket = 0) and (EndBracket = 0) then begin // No brackets - just take the value
    val(s, a1, code);
    if code = 0 then begin
      v := a1 / ValueMult; // No errors - set the value
    end;
  end else if (StartBracket <> 0) and (EndBracket <> 0) then begin // Both brakets
    val(copy(s,1,StartBracket-1), a1, code); // Read the value part
    val(copy(s,StartBracket+1, EndBracket-StartBracket-1), a2, code2); // Read the error part
    if (code = 0) and (code2 = 0) then begin // No errors - set the value and error
      v := a1 / ValueMult;
      e := a2 / ErrorMult * intpower(10.0,DecimalPlaces - Decimals);
    end;
  end;
end;

function TParameter.DeltaAsString : string;
// Shows how much the optimised value (value[2]) differs from the original
// or official value (value[1])
var
  delta,e : double;
begin
  delta := (value[2] - value[1]) * ErrorMult;
  e := SumError(error[1], error[2]) * ErrorMult;
  DeltaAsString := format('%.0f(%.0f)',[delta,e]);
end;

function TParameter.QualityAsString : string;
// Shows how much the error of the optimised value (value1) differs from the
// erro in the original value (value1)
begin
  QualityAsString := '';
  if (error[1] <> 0) and (error[2] <> 0) then
    QualityAsString := format('(error %d%% of previous)',[round(error[2] / error[1] * 100.0)]);
end;

procedure TParameter.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState);
begin
  // 'Name', 'Value', 'Status'
  with Canvas do Case Col of
    0 : TextAlign(canvas, Name, rect, AlignLeft);
    1 : TextAlign(canvas, ValueAsString, rect, AlignRight);
    2 : TextAlign(canvas, StatusText, rect, AlignRight);
  end;
end;

function TParameter.GetEditText(Col : integer) : string;
begin
  GetEditText := '';
  case col of
    0 : GetEditText := Name;
    1 : GetEditText := ValueAsString;
    2 : GetEditText := StatusText;
  end;
end;

procedure TParameter.SetEditText(Col : integer; s : string);
begin
  case col of
    0 : Name := s;
    1 : ValueErrorFromString(s);
    2 : StatusText := s;
  end;
end;

procedure TParameter.SetFieldsFromStrings(strings : TStringList);
begin
  try
    v := strtofloat(strings.values[ParValue]);
    e := strtofloat(strings.values[ParError]);
  except
    on EConvertError do; // Ignore conversion errors
  end;
end;

function TParameter.AsCSV : string;  // Item field names and values as strings
var
  s : TStringList;
begin
  s := TStringList.create;
  s.values[ParName] := Name;
  s.values[ParName] := floattostr(v);
  s.values[ParName] := floattostr(e);
  s.values[ParName] := StatusText;
  AsCSV := s.CommaText;
  s.Free;
end;

//************   C A L C U L A T E D   P A R A M E T E R S   **********

constructor TCalcParameter.create;
begin
  inherited;
  Status := ParCalculated;
end;

destructor TCalcParameter.Destroy;
begin
  TFormula(Formula).Free;
end;

procedure TCalcParameter.SetFieldsFromStrings(strings : TStringList);
begin
  FormulaText := strings.values[ParFormula];
end;

function TCalcParameter.GetValue : double;
begin
  if NeedsCalculation then Calculate;
  GetValue := value[AccessIndex];
end;

procedure TCalcParameter.Calculate;
begin
  v := TFormula(Formula).Value;
  NeedsCalculation := false;
end;

procedure TCalcParameter.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState);
begin
  if Col <=2 then inherited
  else if Col = 3 then TextAlign(canvas, TFormula(Formula).FormulaText, rect, AlignLeft);
end;

function TCalcParameter.AsCSV : string;  // Item field names and values as strings
var
  s : TStringList;
begin
  s := TStringList.create;
  s.values[ParName] := Name;
  s.values[ParName] := StatusText;
  s.values[ParFormula] := FormulaText;
  AsCSV := s.CommaText;
  s.Free;
end;

procedure TCalcParameter.ParseFormula;
begin
  Formula := FormulaParser.Parse(FormulaText);
end;

//********************   P a r a m e t e r s  ********************

function TParameters.par(n : integer) : TParameter;
// Note that n ranges from 1...NMAX although items is in 0...NMAX-1
begin
  par := TParameter(items[n-1]);
end;

procedure TParameters.Modify(delta : TVector);
var
  n : integer;
begin
  for n := 1 to count do with par(n) do v := v + delta[n];
end;

constructor TParameters.CreateConst(a : array of real);
var
  n : integer;
  newpar : TParameter;
begin
  for n := Low(a) to High(a) do begin
    newpar := TParameter.CreateNamed('p'+inttostr(n), 10, 0);
    newpar.v := a[n];
    add(newpar);
  end;
end;

procedure TParameters.Print;
var
  n : integer;
begin
  for n := 1 to count do par(n).Print;
end;

function TParameters.Find(FindName : string) : TParameter;
var
  n : integer;
begin
  Find := NIL;
  for n := 1 to count do
    if par(n).name = FindName then begin
      Find := par(n);
      exit;
    end;
end;

procedure TParameters.SetUpDrawGrid(Grid : TDrawGrid);
const
  ColWidth : array[0..ParGridColumns-1] of integer =
    (90,100,70,500);
var
  n : integer;
begin
  with Grid do begin
    ColCount := ParGridColumns;
    RowCount := max(Count+1, 2);
    FixedRows := 1;
    for n := 0 to ColCount-1 do ColWidths[n] := ColWidth[n];
  end;
end;

function TParameters.GridHeaderName(col : integer) : string;
const
  HeaderName : array[0..ParGridColumns-1] of string =
    ('Name', 'Value', 'Status', 'Formula');
begin
  GridHeaderName := HeaderName[col];
end;

function TParameters.CreateEmptyItem : TAdvListItem;
begin
  // **** NOTE: Only regular parameters created as empty - no other types!
  //            Change if required (ask type in dialog box etc.)
  CreateEmptyItem := TParameter.Create;
end;

function TParameters.CreateItem(fields : tstringlist) : TSaveListItem;
var
  NewPar : TParameter;
  s : string;
begin
  s := fields.values[ParStatus];
  if s = ParCalcText then
    NewPar := TCalcParameter.Create
  else
    NewPar := TParameter.Create;
  NewPar.StatusText := s;
  NewPar.Name := fields.values[ParName];
  NewPar.SetFieldsFromStrings(fields);
  CreateItem := NewPar;
end;

procedure TParameters.ReCalculate;
// Mark parameters to need recalculation. Actual recalculation is done
// when values are requested.
var
  n : integer;
begin
  for n := 1 to count do
    if par(n) is TCalcParameter then
      TCalcParameter(par(n)).NeedsCalculation := true;
end;

procedure TParameters.ParseFormulas;
// Mark parameters to need recalculation. Actual recalculation is done
// when values are requested.
var
  n : integer;
begin
  for n := 1 to count do
    if par(n) is TCalcParameter then
      TCalcParameter(par(n)).ParseFormula;
end;

destructor TParameters.Destroy;
var
  n : integer;
begin
  // Do *NOT* Free the parameters here! When doing the resonance fit
  // we form a new list of existing state parameters. This list is
  // in the end of the fit freed but we don't wan't to free the
  // original parameters!
  //for n := 1 to count do par(n).Free; // NO NO!
end;

end.
