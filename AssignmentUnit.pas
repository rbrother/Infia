unit AssignmentUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, Math,
  linalg, StateUnit, AdvancedList, PeakUnit;

const
  assFields = 6;
  assGridColumns = 7;
  // field names
  assBandName = 'BandName';
  assM = 'm';
  assAssType = 'Type';
  assWnObs = 'WnObs';
  assWnCalc = 'WnCalc';
  assWeight = 'Weight';
  // assignment type codes
  AssText = 'assigned';    // *real* assignments
  PredText = 'predicted';  // predicted peaks on yet unassinded area
  OpposeText = 'opposing'; // predicted peaks from opposing branch
  UnknownAssText = 'Unknown ass type';

type
  TAssignmentForm = class(TForm)
    AssDrawGrid: TDrawGrid;
    procedure FormCreate(Sender : TObject);
    procedure Freeze(frozen : boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TAssType = (assigned, predicted, opposing, unknown);
  TLabelType = (MLabel, JLabel, NameLabel, NoLabel);

  TAssignment = class(TSaveListItem)
  private
    BandName : string; // Used temporarily in the loading process
  public
    Band : TObject;
    m : integer;
    AssType : TAssType;
    Peak : TPeak; // The assigned spectrum peak
    WnCalc : double;
    Weight : single;
    // Methods
    constructor Create; override;
    procedure Print;
    procedure Assign(SourceItem : TPersistent); override;
    procedure SetFieldsFromStrings(strings : TStringList); override;
    function AssTypeAsString : string;
    function AssTypeFromString(s : string) : TAssType;
    function AsCSV : string; override;
    function J1 : integer; // Upper J
    function J0 : integer; // Lower J
    function AssLabel : string; // Text to display in assignment labels
    // Grid operations
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState); override;
    function GetEditText(Col : integer) : string; override;
    procedure SetEditText(Col : integer; s : string); override;
    procedure AssDrawGridDblClick(Sender: TObject);
    procedure GridSelect; // User has selected the assignment
  end;

  TAssignments = class(TSaveList)
  private
    function GetAssignment(n : integer) : TAssignment;
  public
    constructor Create;
    function CreateEmptyItem : TAdvListItem; override;
    function CreateItem(fields : tstringlist) : TSaveListItem; override;
    Procedure FreeAssignments;
    procedure LinkAssignments(Source : TAssignments; IncludePredictions : boolean);
    property Assignment[n : Integer] : TAssignment read GetAssignment; default;
    procedure SortM;
    function AssNearestM(m : integer) : TAssignment;
    function NearestM(m : integer) : integer;
    function NearestAboveM(m : integer) : integer;
    function NearestWnobs(wn : double) : integer;
    function NearestAboveWnobs(wn : double) : integer;
    function FirstAss : TAssignment; // First REAL assignment (not pred)
    function LastAss : TAssignment;  // Last REAL assignment (not pred)
    function Predictions : integer;
    function RealAssignments : integer;
    procedure RemovePredictions;
    procedure RemoveExtendedPredictions; // Remove predictions outside the assigned range
    procedure RemoveAll;
    procedure ShiftM(delta : integer);
    procedure SetWeight1;
    procedure ExportAsText; // For publications
    // Grid operations
    procedure DrawGridSelectCell(Sender: TObject; Col, Row: Integer; var CanSelect: Boolean); override;
    procedure SetUpDrawGrid(Grid : TDrawGrid); override;
    function GridHeaderName(col : integer) : string; override;
  end;

  TAllAssignments = class(TObject) // Meta-class to refer to all assignments
  public
    function count : integer;
    procedure LoadFromTextFile(filename : string);
    procedure SaveToTextFile(filename : string);
  end;

var
  AssignmentForm : TAssignmentForm;
  PrevAssM : integer;
  PeakError : double; // Mean error of peak wavenumber positions
  LabelType : TLabelType;
  AllAss : TAllAssignments; // pseudoList that handles the assignments by accessing bands

implementation

{$R *.DFM}

uses Analwin, Specwin, ScriptUnit, Utils, BandUnit, LoomisUnit, MathUtils;

//****************   F O R M   *****************

procedure TAssignmentForm.FormCreate(Sender: TObject);
begin
  PeakError := 0.0001;
  AllAss := TAllAssignments.create;
end;

procedure TAssignmentForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  AllAss.Free;
end;

procedure TAssignmentForm.Freeze(frozen : boolean);
begin
  if frozen then begin
    PrevAssM := ActiveBand.Assignments[AssDrawGrid.row].m;
  end else begin
    ActiveBand.Assignments.UpdateRowcount;
    AssDrawGrid.row := ActiveBand.Assignments.NearestM(PrevAssM);
    if AssignmentForm.active then AssDrawGrid.SetFocus;
    AssDrawGrid.Repaint;
  end;
end;

//*********************    A S S I G N M E N T  ******************

constructor TAssignment.Create;
begin
  m := 0;
  AssType := assigned;
  Peak := UnknownPeak;
  Band := UnknownBand;
  WnCalc := 0.0;
  Weight := 1.0;
end;

procedure TAssignment.Print;
begin
  display(format('%s m:%d Obs:%.5f Calc:%.5f Res:%.5f',[TBand(Band).name, m, Peak.Wn, WnCalc, Peak.Wn-WnCalc]));
end;

function TAssignment.AssTypeAsString : string;
begin
  if AssType = assigned then AssTypeAsString := AssText
  else if AssType = predicted then AssTypeAsString := PredText
  else if AssType = opposing then AssTypeAsString := OpposeText;
end;

function TAssignment.AssTypeFromString(s : string) : TAssType;
var
  c : string;
begin
  c := copy(s,1,1); // compare only first letters!
  AssTypeFromString := assigned; // To avoid 'return value might be undefined...' warning
  if c = copy(AssText,1,1) then AssTypeFromString := assigned
  else if c = copy(PredText,1,1) then AssTypeFromString := predicted
  else if c = copy(OpposeText,1,1) then AssTypeFromString := opposing
  else AssTypeFromString := unknown;
end;

procedure TAssignment.SetFieldsFromStrings(strings : TStringList);
begin
  m := strtoint(strings.values[assM]);
  AssType := AssTypeFromString(strings.values[assAssType]);
  Peak := Peaks.NearestPeak(strtofloatdef(strings.values[assWnObs]));
  WnCalc := strtofloatdef(strings.values[assWnCalc]);
  Weight := strtofloatdef(strings.values[assWeight]);
end;

function TAssignment.AsCSV : string;
var
  s : TStringList;
begin
  s := TStringList.create;
  s.values[assBandName] := TBand(Band).Name;
  s.values[assM] := inttostr(m);
  s.values[assAssType] := AssTypeAsString;
  s.values[assWnObs] := floattostr(Peak.Wn);
  s.values[assWnCalc] := floattostr(WnCalc);
  s.values[assWeight] := floattostrF(Weight,ffFixed,18,4);
  AsCSV := s.CommaText;
  s.Free;
end;

procedure TAssignment.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState);
begin
  with Canvas do Case Col of
  0 : TextAlign(canvas, inttostr(m), rect, AlignRight);
  1 : if AssType = assigned then begin
        Brush.Color := clBlack;
        Font.Color := clWhite;
        FillRect(Rect);
        TextAlign(canvas, AssText, rect, AlignLeft);
        end
      else if AssType = predicted then begin
        Font.Color := clBlue;
        TextAlign(canvas, PredText, rect, AlignLeft);
        end
      else if AssType = opposing then begin
        Font.Color := clRed;
        TextAlign(canvas, OpposeText, rect, AlignLeft);
      end;
  2 : TextAlign(canvas, floattostrF(Peak.wn, ffFixed, 18, 6), rect, AlignRight);
  3 : TextAlign(canvas, floattostrF(WnCalc, ffFixed, 18, 6), rect, AlignRight);
  4 : TextAlign(canvas, inttostr(trunc((Peak.wn - WnCalc)*10e5)), rect, AlignRight);
  5 : DrawResidualBar(canvas, rect, (Peak.wn - WnCalc) / PeakError * 0.1);
  6 : TextAlign(canvas, floattostrF(Weight, ffFixed, 18, 3), rect, AlignRight);
  end;
end;

function TAssignment.GetEditText(Col : integer) : string;
begin
  GetEditText := '';
  case col of
  1 : GetEditText := copy(AssTypeAsString,1,1);
  6 : GetEditText := floattostrF(Weight, ffFixed, 18, 3);
  else
    AssignmentForm.AssDrawGrid.Editormode := false;
  end;
end;

procedure TAssignment.SetEditText(Col : integer; s : string);
begin
  if s = '' then exit;
  case col of
  1 : if AssTypeFromString(s) <> Unknown then AssType := AssTypeFromString(s);
  6 : Weight := strtofloat(s);
  end;
end;

procedure TAssignment.GridSelect; // User has selected the assignment
begin
  SpectraWindow.SetViewCenter(WnCalc);
  LoomisForm.Show(Peak.wn);
end;

procedure TAssignment.AssDrawGridDblClick(Sender: TObject);
// DoubleClick changes the type of assignment
begin
  if AssType = predicted then AssType := assigned else AssType := predicted;
  TDrawGrid(Sender).Invalidate;
end;

function TAssignment.J1 : integer; // Upper J calculated from m
var
  BandType : TBandType;
begin
  BandType := TBand(band).BandType;
  if BandType = Q then J1 := m
  else begin
    if m < 0 then J1 := -m-1 else J1 := m;
  end;
end;

function TAssignment.J0 : integer; // Lower J calculated from m
var
  BandType : TBandType;
begin
  BandType := TBand(band).BandType;
  if BandType = Q then J0 := m
  else begin
    if m < 0 then J0 := -m else J0 := m-1;
  end;
end;

procedure TAssignment.Assign(SourceItem : TPersistent);
// Don't be confused: 'Assign' method is the methods derived from
// TPersistent and copies contents of objects to other objects.
// It has nothing to do with spectral assignments.
// *** NOTE that after assignment the band pointer still points
// to the original band! For routines that copy assignments from
// one band to anouther the band pointer must be manually changed!
var
  SourceAss : TAssignment;
begin
  if SourceItem is TAssignment then begin
    SourceAss := TAssignment(SourceItem);
    Band := SourceAss.Band;
    m := SourceAss.m;
    AssType := SourceAss.AssType;
    Peak := SourceAss.Peak;
    WnCalc := SourceAss.WnCalc;
    Weight := SourceAss.Weight;
  end else
    Raise EConvertError.Create(format('Cannot assign assignment from %s',[SourceItem.ClassName]));
end;

function TAssignment.AssLabel : string;
begin
  if LabelType = MLabel then AssLabel := inttostr(m)
  else if LabelType = JLabel then AssLabel := format('%d<-%d',[J1,J0])
  else if LabelType = NameLabel then AssLabel := TBand(Band).Name
  else AssLabel := ' ';
end;

//*********************  A S S I G N M E N T S *********************

constructor TAssignments.create;
begin
  Capacity := 10000;
  ConfirmDelete := false;
  AllowDelete := true;
  AllowInsert := false;
end;

function TAssignments.CreateEmptyItem : TAdvListItem;
begin
  CreateEmptyItem := TAssignment.Create;
end;

function TAssignments.CreateItem(fields : tstringlist) : TSaveListItem;
var
  NewAssignment : TAssignment;
begin
  NewAssignment := TAssignment.Create;
  NewAssignment.SetFieldsFromStrings(Fields);
  CreateItem := NewAssignment;
end;

Procedure TAssignments.FreeAssignments;
var
  n : integer;
begin
  for n := 1 to count do Assignment[n].free;
end;

function TAssignments.GetAssignment(n : integer) : TAssignment;
// Indexes in range 1...nAss
begin
  GetAssignment := TAssignment(items[n-1]);
end;

function TAssignments.FirstAss : TAssignment; // First REAL assignment (not pred)
var
  n : integer;
begin
  n := 1;
  FirstAss := NIL;
  if count = 0 then exit;
  while (Assignment[n].AssType <> Assigned) and (n < count) do inc(n);
  FirstAss := Assignment[n];
end;

function TAssignments.LastAss : TAssignment;  // Last REAL assignment (not pred)
var
  n : integer;
begin
  n := count;
  LastAss := NIL;
  if count = 0 then exit;
  while (Assignment[n].AssType <> Assigned) and (n >= 1) do dec(n);
  LastAss := Assignment[n];
end;

function CompareAssM(Item1, Item2: Pointer): Integer;
begin
  if TAssignment(Item1).m > TAssignment(Item2).m
    then CompareAssM := 1
    else CompareAssM := -1;
end;

procedure TAssignments.SortM;
begin
  BubbleSort(CompareAssM);
end;

function WnValue(Item : TObject) : double;
begin
  WnValue := TAssignment(Item).Peak.wn;
end;

function TAssignments.NearestWnobs(wn : double) : integer;
begin
  // Corrected to give range 1...MAX
  NearestWnobs := Nearest(WnValue, wn) + 1;
end;

function MValue(Item : TObject) : double;
begin
  MValue := TAssignment(Item).m;
end;

function TAssignments.NearestM(m : integer) : integer;
begin
  // Corrected to give range 1...MAX
  NearestM := Nearest(MValue, m) + 1;
end;

function TAssignments.AssNearestM(m : integer) : TAssignment;
begin
  // Corrected to give range 1...MAX
  AssNearestM := Assignment[NearestM(m)];
end;

function TAssignments.NearestAboveM(m : integer) : integer;
begin
  // Corrected to give range 1...MAX
  NearestAboveM := NearestAbove(MValue, m) + 1;
end;

function TAssignments.NearestAboveWnobs(wn : double) : integer;
begin
  // Corrected to give range 1...MAX
  NearestAboveWnobs := NearestAbove(WnValue, wn) + 1;
end;

function TAssignments.GridHeaderName(col : integer) : string;
const
  HeaderName : array[0..assGridColumns-1] of string =
    ('m','Type','Obs Wn','Calc Wn','Res 10^5','Residual','Weight');
begin
  GridHeaderName := HeaderName[col];
end;

procedure TAssignments.SetUpDrawGrid(Grid : TDrawGrid);
const
  ColWidth : array[0..assGridColumns-1] of integer =
    (30,60,80,80,55,90,40);
var
  n : integer;
begin
  with Grid do begin
    ColCount := assGridColumns;
    RowCount := max(Count+1, 2);
    FixedRows := 1;
    for n := 0 to ColCount-1 do ColWidths[n] := ColWidth[n];
  end;
end;

procedure TAssignments.DrawGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  if count = 0 then exit; // no assignments -> exit
  if Assignment[Row].m <> PrevAssM then begin
    Assignment[Row].GridSelect;
    PrevAssM := Assignment[Row].m;
  end;
end;

function TAssignments.Predictions : integer;
var
  n, npred : integer;
begin
  npred := 0;
  for n := 1 to count do if Assignment[n].AssType = predicted then inc(npred);
  Predictions := npred;
end;

function TAssignments.RealAssignments : integer;
var
  n, nass : integer;
begin
  nass := 0;
  for n := 1 to count do if Assignment[n].AssType = assigned then inc(nass);
  RealAssignments := nass;
end;

procedure TAssignments.ShiftM(delta : integer);
var
  n : integer;
begin
  for n := 1 to count do with Assignment[n] do m := m + delta;
end;

procedure TAssignments.RemovePredictions;
var
  n : integer;
begin
  for n := 1 to count do if Assignment[n].AssType <> assigned then begin
    Assignment[n].Free;
    Items[n-1] := nil;
  end;
  Pack;
end;

procedure TAssignments.RemoveExtendedPredictions; // Remove predictions outside the assigned range
var
  n : integer;
  mstart, mend : integer;
begin
  mstart := FirstAss.m;
  mend := LastAss.m;
  for n := 1 to count do with Assignment[n] do
  if (AssType <> assigned) and ((m < mstart) or (m > mend)) then begin
    Assignment[n].Free;
    Items[n-1] := nil;
  end;
  Pack;
end;

procedure TAssignments.RemoveAll;
var
  n : integer;
begin
  for n := 1 to count do Assignment[n].Free;
  Clear;
end;

procedure TAssignments.SetWeight1;
var
  n : integer;
begin
  for n := 1 to count do Assignment[n].weight := 1.0;
end;

procedure TAssignments.LinkAssignments(Source : TAssignments; IncludePredictions : boolean);
// Copies assignments from one set to another. Does not make physical copies, but
// only adds a pointer.
var
  n : integer;
  newass : TAssignment;
begin
  for n := 1 to Source.count do
    if IncludePredictions or (Source[n].AssType = assigned) then add(Source[n]);
  SortM;
end;

procedure TAssignments.ExportAsText; // For publications
var
  row : string;
  n : integer;
begin
  row :=       'm     Type       WnObs           WnCalc          Weight     ';
  display(row);
  for n := 0 to Count-1 do with TAssignment(items[n]) do begin
    row := format('%5d %10s %15.6f %15.6f %10.3f', [m, AssTypeAsString, Peak.Wn, WnCalc, Weight]);
    display(row);
  end;
end;

//**************   A L L   A S S I G N M E N T S   **************

function TAllAssignments.count : integer;
var
  n, sum : integer;
begin
  sum := 0;
  with Bands do
  for n := 0 to count-1 do with band(n) do inc(sum, Assignments.count);
  count := sum;
end;

procedure TAllAssignments.LoadFromTextFile(filename : string);
var
  tablefile : textfile;
  line : string;
  Fields : TStringList;
  bandname : string;
  newass : TAssignment; currentband : TBand;
begin
  currentband := UnknownBand;
  assignfile(tablefile, filename);
  reset(tablefile);
  Fields := TStringList.create;
  while not eof(tablefile) do begin
    readln(tablefile, line);
    Fields.clear; // remove existing strings
    Fields.commatext := line; // Parse line into field values and names
    bandname := Fields.values[assBandName];
    if bandname <> currentband.name then begin
      if currentband <> UnknownBand then currentband.assignments.sortM;
      currentband := Bands.FindBand(bandname);
    end;
    if CurrentBand <> UnknownBand then begin
      Newass := TAssignment.Create;
      Newass.Band := CurrentBand;
      Newass.SetFieldsFromStrings(Fields);
      CurrentBand.Assignments.add(Newass);
    end;
  end;
  currentband.assignments.sortM;
  closefile(tablefile);
  Fields.free;
end;

procedure TAllAssignments.SaveToTextFile(filename : string);
var
  tablefile : textfile;
  n, m : integer;
  backupname : string;
begin
  backupname := changefileext(extractfilename(filename), '.BAK');
  DeleteFile(backupname);
  RenameFile(filename, backupname);
  assignfile(tablefile, filename);
  rewrite(tablefile);
  // In writing the file, use the own routine of the item class
  with Bands do for n := 0 to count-1 do with band(n) do
  for m := 1 to Assignments.count do
    writeln(tablefile, Assignments[m].AsCSV);
  closefile(tablefile);
end;

end.
