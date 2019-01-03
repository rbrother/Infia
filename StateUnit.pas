unit StateUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables, ExtCtrls, DBCtrls, Grids, DBGrids,

  linalg, ParameterUnit, AdvancedList, Moleculeunit;

const
  StateFields = 15;
  StateGridColumns = 11;
  // Field names
  StateMolecule = 'Molecule';
  StateName = 'StateName';
  StateL = 'L';
  StateUseAsLower = 'UseAsLower';
  StateEnergy = 'Energy';
  StateEnergyError = 'EnergyError';
  StateB = 'B';
  StateBError = 'BError';
  StateD = 'D';
  StateDError = 'DError';
  StateH = 'H';
  StateHError = 'HError';
  StateHFixed = 'HFixed';
  StateBand = 'Band';
  StateNote = 'Note';

type
  TStateForm = class(TForm)
    StateDrawGrid: TDrawGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StateDrawGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TState = class(TSaveListItem)
  private
    SourceBandName : string;
  public
    Name, Comment : string;
    Molecule : TMolecule;
    SourceBand : pointer;
    UseAsLower : boolean;
    L : integer; // Total rotational impulse moment of the state
    Energy, B, D, H : TParameter;
    ResEnergy : Array[0..200] of double; // Rotational energies for resonant systems
    constructor Create; override;
    destructor Destroy; override;
    procedure TempToOriginal;
    procedure OriginalToTemp;
    procedure UseOriginal;
    procedure Assign(Source : TPersistent); override;
    procedure SetFieldsFromStrings(strings : TStringList); override;
    function AsCSV : string; override;
    function RotEnergy(J : double) : double;
    function TotEnergy(J : double) : double;
    procedure Print;
    procedure PrintCompare;
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState); override;
    function GetEditText(Col : integer) : string; override;
    procedure SetEditText(Col : integer; s : string); override;
  end;

  TStates = class(TSaveList)
  public
    constructor Create;
    function CreateEmptyItem : TAdvListItem; override;
    function sta(n : integer) : TState;
    function Find(teststatename : string; testmolecule : TMolecule) : TState;
    procedure ExportAsText;
    procedure RemoveWithMolecule(mol : TMolecule);
    procedure SetUpDrawGrid(Grid : TDrawGrid); override;
    function GridHeaderName(col : integer) : string; override;
    procedure ConnectToSourceBands;
  end;

var
  StateForm : TStateForm;
  States : TStates;
  UnknownState : TState;

implementation

uses ScriptUnit, BandUnit, ResonanceUnit, AssignmentUnit, math,
  mathutils, Utils;

{$R *.DFM}

//*************   S T A T E F O R M   M E T H O D S   *************

procedure TStateForm.FormCreate(Sender: TObject);
begin
  UnknownMolecule := TMolecule.Create;
  UnknownState := TState.Create;
end;

procedure TStateForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  UnknownState.Free;
  UnknownMolecule.Free;
end;

procedure TStateForm.StateDrawGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Column, Row : integer;
begin
  if States.count = 0 then exit;
  Column := TDrawGrid(Sender).col;
  Row := TDrawGrid(sender).row;
  with States.sta(row-1) do
  case Column of
  3 : if Key = 45 then UseAsLower := true
      else if Key = 46 then UseAsLower := false;
  8 : if Key = 45 then H.Fixed := true
      else if Key = 46 then H.Fixed := false;
  end;
  TDrawGrid(sender).Invalidate;
end;

// ***********   S T A T E   M E T H O D S   ************

constructor TState.Create;
begin
  Name := 'Unknown State';
  Molecule := UnknownMolecule;
  SourceBand := UnknownBand;
  Energy := TParameter.CreateNamed('G',6,0);
  B := TParameter.CreateNamed('B',8,0);
  D := TParameter.CreateNamed('D',4,8);
  H := TParameter.CreateNamed('H',2,14); H.Fixed := true;
end;

destructor TState.Destroy;
begin
  Energy.Free;
  B.Free;
  D.Free;
  H.Free;
  Bands.RemoveWithState(Self); // Remove bands that depend on the deleted state
end;

procedure TState.TempToOriginal;
begin
  Energy.AcceptTemporary;
  B.AcceptTemporary;
  D.AcceptTemporary;
  H.AcceptTemporary;
end;

procedure TState.OriginalToTemp;
begin
  Energy.UseTemporary;
  B.UseTemporary;
  D.UseTemporary;
  H.UseTemporary;
end;

procedure TState.UseOriginal;
begin
  Energy.UseOriginal;
  B.UseOriginal;
  D.UseOriginal;
  H.UseOriginal;
end;

procedure TState.Assign(Source : TPersistent);
begin
  if Source is TState then begin
    Name := TState(Source).Name;
    Comment := TState(Source).Comment;
    Molecule := TState(Source).Molecule;
    SourceBand := TState(Source).SourceBand;
    UseAsLower := TState(Source).UseAsLower;
    L := TState(Source).L;
    Energy.Assign(TState(Source).Energy);
    B.Assign(TState(Source).B);
    D.Assign(TState(Source).D);
    H.Assign(TState(Source).H);
  end else
    Raise EConvertError.Create(format('Cannot assign state from %s',[Source.ClassName]));
end;

procedure TState.SetFieldsFromStrings(strings : TStringList);
begin
  Molecule := Molecules.Find(strings.values[StateMolecule]);
  Name := strings.values[StateName];
  L := strtointdef(strings.values[StateL],0);
  UseAsLower := boolean(strtointdef(strings.values[StateUseAsLower],0));
  Energy.v := strtofloatdef(strings.values[StateEnergy]);
  Energy.e := strtofloatdef(strings.values[StateEnergyError]);
  B.v := strtofloatdef(strings.values[StateB]);
  B.e := strtofloatdef(strings.values[StateBError]);
  D.v := strtofloatdef(strings.values[StateD]);
  D.e := strtofloatdef(strings.values[StateDError]);
  H.v := strtofloatdef(strings.values[StateH]);
  H.e := strtofloatdef(strings.values[StateHError]);
  H.Fixed := boolean(strtointdef(strings.values[StateHFixed], 1));
  SourceBandName := strings.values[StateBand];
  Comment := strings.values[StateNote];
end;

function TState.AsCSV : string;
var
  s : TStringList;
begin
  s := TStringList.create;
  s.values[StateMolecule] := Molecule.Name;
  s.values[StateName] := Name;
  s.values[StateL] := inttostr(L);
  s.values[StateUseAsLower] := inttostr(integer(UseAsLower));
  s.values[StateEnergy] := floattostr(Energy.v);
  s.values[StateEnergyError] := floattostr(Energy.e);
  s.values[StateB] := floattostr(B.v);
  s.values[StateBError] := floattostr(B.e);
  s.values[StateD] := floattostr(D.v);
  s.values[StateDError] := floattostr(D.e);
  s.values[StateH] := floattostr(H.v);
  s.values[StateHError] := floattostr(H.e);
  s.values[StateHFixed] := inttostr(integer(H.Fixed));
  s.values[StateBand] := TBand(SourceBand).name;
  s.values[StateNote] := comment;
  AsCSV := s.CommaText;
  s.Free;
end;

function TState.TotEnergy(J : double) : double;
begin
  TotEnergy := Energy.v + RotEnergy(J);
end;

function TState.RotEnergy(J : double) : double;
var
  JJ : double;
begin
  JJ := J*(J+1) - sqr(L);
  RotEnergy := B.v*(JJ) - D.v*sqr(JJ) + H.v*IntPower(JJ,3);
end;

procedure TState.Print;
begin
  // Get the corresponding record from state table to look the existing
  // parameter values and compare them to the ones in here
  Display(format('State %s (L=%d) of %s parameters:',[Name, L, Molecule.name]));
  Energy.Print;
  B.Print;
  D.Print;
  H.Print;
end;

procedure TState.PrintCompare;
begin
  // Get the corresponding record from state table to look the existing
  // parameter values and compare them to the ones in here
  Display(format('State %s (L=%d) of %s parameters:',[Name, L, Molecule.name]));
  Energy.PrintCompare;
  B.PrintCompare;
  D.PrintCompare;
  H.PrintCompare;
end;

procedure TState.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState);
begin
  case col of
  0 : TextAlign(canvas, Molecule.Name, rect, AlignLeft);
  1 : TextAlign(canvas, Name, rect, AlignLeft);
  2 : TextAlign(canvas, inttostr(L), rect, AlignCenter);
  3 : TextAlignBool(canvas, UseAsLower, rect, AlignCenter);
  4 : TextAlign(canvas, Energy.ValueAsString, rect, AlignDecimal);
  5 : TextAlign(canvas, B.ValueAsString, rect, AlignDecimal);
  6 : TextAlign(canvas, D.ValueAsString, rect, AlignDecimal);
  7 : TextAlign(canvas, H.ValueAsString, rect, AlignDecimal);
  8 : TextAlignBool(canvas, H.Fixed, rect, AlignCenter);
  9 : TextAlign(canvas, TBand(SourceBand).Name, rect, AlignLeft);
  10: TextAlign(canvas, Comment, rect, AlignLeft);
  end;
end;

function TState.GetEditText(Col : integer) : string;
begin
  case col of
  0 : GetEditText := Molecule.Name;
  1 : GetEditText := Name;
  2 : GetEditText := inttostr(L);
  3 : GetEditText := BooleanToText(UseAsLower);
  4 : GetEditText := Energy.ValueAsString;
  5 : GetEditText := B.ValueAsString;
  6 : GetEditText := D.ValueAsString;
  7 : GetEditText := H.ValueAsString;
  8 : GetEditText := BooleanToText(H.Fixed);
  10: GetEditText := Comment;
  else
    StateForm.STateDrawGrid.Editormode := false;
  end;
end;

procedure TState.SetEditText(Col : integer; s : string);
begin
  case col of
  0 : if Molecules.Find(s) <> UnknownMolecule then Molecule := Molecules.Find(s);
  1 : if States.Find(s,Molecule) = UnknownState then name := s; // Change name, if different from others
  2 : L := strtointdef(s,0);
  3 : UseAsLower := TextToBoolean(s);
  4 : Energy.ValueErrorFromString(s);
  5 : B.ValueErrorFromString(s);
  6 : D.ValueErrorFromString(s);
  7 : H.ValueErrorFromString(s);
  8 : H.Fixed := TextToBoolean(s);
  10: Comment := s;
  end;
end;

// ********************   S T A T E S    ***********************

constructor TStates.create;
begin
  Capacity := 100;
  ConfirmDelete := true;
  AllowDelete := true;
  AllowInsert := true;
end;

function TStates.sta(n : integer) : TState;
begin
  sta := TState(Items[n]);
end;

procedure TStates.ConnectToSourceBands;
var
  n : integer;
begin
  for n := 0 to Count-1 do with sta(n) do
    if Bands.FindIndex(SourceBandName) >= 0
    then sourceband := Bands.FindBand(SourceBandName)
    else sourceband := UnknownBand;
end;

function TStates.Find(teststatename : string; testmolecule : TMolecule) : TState;
var
  n : integer;
  sta : TState;
begin
  sta := UnknownState;
  n := 0;
  while (sta = UnknownState) and (n<count) do begin
    with TState(Items[n]) do
      if (name = teststatename) and (molecule = testmolecule) then
        sta := TState(Items[n]);
    inc(n);
  end;
  Find := sta;
end;

procedure TStates.ExportAsText;
var
  row : string;
  n : integer;
begin
  row :=       'Molecule        State              VibEnergy         ';
  row := row + 'B                D               H             Source';
  display(row);
  for n := 0 to Count-1 do with sta(n) do begin
    row := format('%15s %18s %17s %16s %15s %13s %s',
      [Molecule.Name, Name, Energy.ValueAsString, B.ValueAsString,
      D.ValueAsString, H.ValueAsString, TBand(SourceBand).Name]);
    display(row);
  end;
end;

procedure TStates.SetUpDrawGrid(Grid : TDrawGrid);
const
  colwidth : array[0..StateGridColumns-1] of integer =
    (65,110,32,32,100,110,80,60,40,80,260);
var
  n : integer;
begin
  with Grid do begin
    ColCount := StateGridColumns;
    for n := 0 to ColCount-1 do ColWidths[n] := ColWidth[n];
  end;
end;

function TStates.GridHeaderName(col : integer) : string;
const
  headername : array[0..StateGridColumns-1] of string =
    ('Molecule', 'State Name', 'L', 'Use', 'Energy', 'B', 'D 10^8', 'H 10^14',
     'H Fixed', 'Source Band', 'Note');
begin
  GridHeaderName := headername[col];
end;

procedure TStates.RemoveWithMolecule(mol : TMolecule);
var
  n : integer;
begin
  for n := 0 to Count-1 do if sta(n).molecule = mol then begin
    sta(n).Free;
    items[n] := NIL;
  end;
  Pack;
  UpdateRowCount;
  StateForm.StateDrawGrid.Invalidate;
end;

function TStates.CreateEmptyItem : TAdvListItem;
var
  NewState : TState;
begin
  NewState := TState.Create;
  CreateEmptyItem := NewState;
end;

end.
