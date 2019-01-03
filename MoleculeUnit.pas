unit MoleculeUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls,

  AdvancedList;

const
  MoleculeFields = 1;
  MolNameText = 'Molecule';

type
  TMoleculeForm = class(TForm)
    MolDrawGrid: TDrawGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TMolecule = class(TSaveListItem)
  public
    Name : string;
    constructor Create; override;
    destructor Destroy; override;
    procedure SetFieldsFromStrings(strings : TStringList); override;
    function AsCSV : string; override; // Item field values as strings
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; State: TGridDrawState); override;
    function GetEditText(Col : integer) : string; override;
    procedure SetEditText(Col : integer; s : string); override;
  end;

  TMolecules = class(TSaveList)
  public
    constructor Create;
    function CreateEmptyItem : TAdvListItem; override;
    function Find(molname : string) : TMolecule;
    procedure SetUpDrawGrid(Grid : TDrawGrid); override;
    function GridHeaderName(col : integer) : string; override;
  end;

var
  MoleculeForm: TMoleculeForm;
  Molecules : TMolecules;
  UnknownMolecule : TMolecule; // For those bands and states whose molecule is not known

implementation

{$R *.DFM}

uses ScriptUnit, utils, math, StateUnit, BandUnit;

//**************    M O L E C U L E S    **********************

constructor TMolecules.create;
begin
  Capacity := 20;
  ConfirmDelete := true;
  AllowDelete := true;
  AllowInsert := true;
end;

function TMolecules.CreateEmptyItem : TAdvListItem;
var
  NewMol : TMolecule;
begin
  NewMol := TMolecule.Create;
  CreateEmptyItem := NewMol;
end;

function TMolecules.Find(molname : string) : TMolecule;
var
  n : integer;
  mol : TMolecule;
begin
  mol := UnknownMolecule;
  n := 0;
  while (mol = UnknownMolecule) and (n<count) do begin
    with TMolecule(Items[n]) do
      if (name = molname) then mol := TMolecule(Items[n]);
    inc(n);
  end;
  Find := mol;
end;

procedure TMolecules.SetUpDrawGrid(Grid : TDrawGrid);
begin
  with Grid do begin
    ColCount := 1;
    ColWidths[0] := 120;
    Invalidate;
  end;
  UpdateRowCount;
  Grid.OnSelectCell := NIL;
end;

function TMolecules.GridHeaderName(col : integer) : string;
begin
  GridHeaderName := '';
  if col = 0 then GridHeaderName := 'Name';
end;

//****************    M O L E C U L E    ******************

constructor TMolecule.Create;
begin
  Name := 'Unknown Molecule';
end;

destructor TMolecule.Destroy;
// Delete all states (and indirectly bands) that depend on the delete molecule
begin
  States.RemoveWithMolecule(Self);
end;

procedure TMolecule.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; State: TGridDrawState);
begin
  if col = 0 then TextAlign(canvas, Name, rect, AlignLeft);
end;

function TMolecule.AsCSV : string; // Item field values as Comma Separated Values -string
var
  s : TStringList;
begin
  s := TStringList.create;
  s.append(MolNameText + '=' + Name);
  AsCSV := s.CommaText;
  s.Free;
end;

procedure TMolecule.SetFieldsFromStrings(strings : TStringList);
begin
  Name := strings.values[MolNameText];
end;

function TMolecule.GetEditText(Col : integer) : string;
begin
  if Col = 0 then GetEditText := name;
end;

procedure TMolecule.SetEditText(Col : integer; s : string);
begin
  if s = '' then exit;
  if Col = 0 then if Molecules.Find(s) = UnknownMolecule then name := s;
end;

end.
