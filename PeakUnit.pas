unit PeakUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Dialogs,
  Grids, Controls, graphics, math,
  AdvancedList;

const
  PeakFields = 2;
  PeakGridColumns = 3;
  PeakWnText = 'Wavenumber';
  PeakIntText = 'Intensity';

type
  TPeakForm = class(TForm)
    PeakDrawGrid: TDrawGrid;
    procedure PeakDrawGridSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TPeak = class(TSaveListItem)
  public
    wn, intensity : double;
    LabelCount : integer; // number of assignment labels on top of the peak
    constructor Create; override;
    procedure SetFieldsFromStrings(strings : TStringList); override;
    function AsCSV : string; override;
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; State: TGridDrawState); override;
    function GetEditText(Col : integer) : string; override;
    procedure SetEditText(Col : integer; s : string); override;
  end;

  TPeaks = class(TSaveList)
  public
    constructor Create;
    function CreateEmptyItem : TAdvListItem; override;
    procedure AddPeak(wn : double);
    function Peak(n : integer) : TPeak;
    function NearestIndex(wn : double) : integer;
    function NearestPeak(wn : double) : TPeak;
    function NearestWn(wn : double) : double;
    procedure SetUpDrawGrid(Grid : TDrawGrid); override;
    function GridHeaderName(col : integer) : string; override;
    procedure DrawGridSelectCell(Sender: TObject; Col,
      Row: Integer; var CanSelect: Boolean); override;
    procedure DeleteRange(startwn, endwn : double);
    procedure RecalculateIntensities;
    procedure ZeroLabelCount(drawstart, drawend : double);
  end;

var
  PeakForm: TPeakForm;
  Peaks : TPeaks;
  PrevRow : integer;
  UnknownPeak : TPeak;

implementation

{$R *.DFM}

uses Utils, ScriptUnit, spectra, Specwin, MathUtils;

// ************** Helper routines ***********

function WnValue(Item : TObject) : double;
begin
  WnValue := TPeak(Item).Wn;
end;

//****************  P E A K   ********************

constructor TPeak.Create;
begin
  wn := 0.0;
  Intensity := 1.0;
end;

procedure TPeak.DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; State: TGridDrawState);
var
  dist : integer;
begin
  case col of
  0 : TextAlign(canvas, floattostrF(Wn, ffFixed, 18, 6), rect, AlignRight);
  1 : TextAlign(canvas, floattostrF(Intensity, ffFixed, 18, 4), rect, AlignRight);
  2 : begin // Draw a bar showing the intensity
        dist := trunc((Rect.Right-Rect.Left-4)*min(Intensity, 1.0));
        canvas.brush.color := clGray;
        canvas.rectangle(Rect.Left+2, Rect.Top+2, Rect.left + 2 + Dist, Rect.Bottom-2);
      end;
  end;
end;

procedure TPeak.SetFieldsFromStrings(strings : TStringList);
begin
  wn := strtofloatdef(strings.values[PeakWnText]);
  Intensity := strtofloatdef(strings.values[PeakIntText]);
end;

function TPeak.AsCSV : string;
var
  s : TStringList;
begin
  s := TStringList.create;
  s.values[PeakWnText] := floattostr(wn);
  s.values[PeakIntText] := floattostr(Intensity);
  AsCSV := s.CommaText;
  s.Free;
end;

function TPeak.GetEditText(Col : integer) : string;
begin
  GetEditText := '';
  case Col of
  0 : GetEditText := floattostrF(Wn, ffFixed, 18, 6);
  1 : GetEditText := floattostrF(Intensity, ffFixed, 18, 4);
  end;
end;

procedure TPeak.SetEditText(Col : integer; s : string);
begin
  if s = '' then exit;
  case Col of
  0 : wn := StrToFloatDef(s);
  1 : Intensity := StrToFloatDef(s);
  end;
end;

//****************    P E A K S      ****************

constructor TPeaks.create;
begin
  Capacity := 10000;
  ConfirmDelete := false;
  AllowDelete := true;
  AllowInsert := true;
end;

function TPeaks.CreateEmptyItem : TAdvListItem;
var
  NewPeak : TPeak;
begin
  NewPeak := TPeak.Create;
  CreateEmptyItem := NewPeak;
end;

procedure TPeaks.AddPeak(wn : double);
var
  NewPeak : TPeak;
  i : integer;
begin
  NewPeak := TPeak.Create;
  NewPeak.wn := wn;
  NewPeak.intensity := 0.1; // Assume weak peak
  i := NearestAbove(WnValue, wn);
  Insert(i, NewPeak);
end;

function TPeaks.Peak(n : integer) : TPeak;
begin
  Peak := TPeak(items[n]);
end;

function TPeaks.NearestIndex(wn : double) : integer;
begin
  NearestIndex := Nearest(WnValue, wn);  // why doesnt this work?
end;

function TPeaks.NearestPeak(wn : double) : TPeak;
begin
  NearestPeak := peak(NearestIndex(wn));
end;

function TPeaks.NearestWn(wn : double) : double;
begin
  NearestWn := peak(NearestIndex(wn)).wn;
end;

procedure TPeaks.SetUpDrawGrid(Grid : TDrawGrid);
const
  ColWidth : array[0..PeakGridColumns-1] of integer = (100, 70, 100);
var
  n : integer;
begin
  Grid.ColCount := PeakGridColumns;
  for n := 0 to Grid.ColCount-1 do Grid.ColWidths[n] := ColWidth[n];
  UpdateRowCount;
  Grid.Invalidate;
end;

function TPeaks.GridHeaderName(col : integer) : string;
const
  HeaderName : array[0..PeakGridColumns-1] of string = ('Wavenumber', 'Intensity', 'Intensity');
begin
  GridHeaderName := HeaderName[col];
end;

procedure TPeaks.DeleteRange(startwn, endwn : double);
begin
// Note: must first delete assignments that are assigned to the deleted peaks,
// or, perhaps more easily, within the startwn endwn range
end;

procedure TPeaks.RecalculateIntensities;
var
  n : integer;
  maxint : double;
begin
  maxint := 0.0;
  // Set the peak intensities based on spectrum peak heights
  for n := 0 to count-1 do with peak(n) do begin
    Intensity := myspectra.value(wn);
    if Intensity > MaxInt then Maxint := intensity;
  end;
  // Normalise intensities to max 1.0
  for n := 0 to count-1 do with peak(n) do
    Intensity := Intensity / MaxInt;
  PeakForm.PeakDrawGrid.Invalidate;
  MessageDlg('Peak intensities recalculated from the spectrum.',mtInformation,[mbOK],0);
end;

procedure TPeakForm.PeakDrawGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  if Row<>PrevRow then begin
    SpectraWindow.SetViewCenter(Peaks.peak(Row-1).wn);
    PrevRow := Row;
  end;
end;

procedure TPeakForm.FormCreate(Sender: TObject);
begin
  UnknownPeak := TPeak.Create;
end;

procedure TPeaks.ZeroLabelCount(drawstart, drawend : double);
var
  n,n0,n1 : integer;
begin
  n0 := NearestIndex(drawstart);
  n1 := NearestIndex(drawend);
  for n := n0 to n1 do peak(n).LabelCount := 0;
end;
                   
procedure TPeaks.DrawGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  SpectraWindow.SetViewCenter(peak(row-1).wn);
end;

end.
