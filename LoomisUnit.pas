unit LoomisUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PeakUnit, ExtCtrls, math;

type
  // Declaration of the window containing the loomis-wood plot
  TLoomisForm = class(TForm)
    LoomisImage: TImage;
    procedure LoomisImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LoomisImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LoomisImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    columns, StartCol, EndCol, mMinusCol : integer;
    B, StartOffset, EndOffset : double;
    Center : TPoint;
    WnOrigin : array[1..500] of double;
    Zooming : boolean;
    ZoomOrigin, ZoomCursor : TPoint;
    // View Options
    ShowAssignments : boolean;
    MinIntensity : double;
    // Peak showing options
    Inverted : boolean;
    InvertedPoint : TPoint;
    // Smooth draw bitmap
    TempBitmap : TBitmap;
    Drawing : boolean;
  public
    { Public declarations }
    procedure StraightenBand;
    procedure DrawPlot;
    procedure DrawPeak(canvas : TCanvas; Peak : TPeak; color1, color2 : TColor);
    function NearestColumn(wn : double) : integer;
    procedure InvertZoomRect;
    procedure ZoomRegion;
    procedure ZoomAll;
    function WnToPoint(wn : double) : TPoint;
    function PointToWn(p : TPoint) : double;
    procedure SaveImage;
    procedure SetParameters;
    procedure InvertPoint;
    procedure Show(wn : double);
    function xstep : double;
    function height : integer;
  end;

var
  LoomisForm: TLoomisForm;

implementation

{$R *.DFM}

uses StateUnit, BandUnit, AssignmentUnit, linalg, Specwin,
  Analwin, LoomisParamsUnit, mathutils, utils;

function TLoomisForm.xstep : double;
begin
  if Drawing then xstep := (TempBitmap.Width) / (EndCol - StartCol + 1)
             else xstep := (LoomisImage.Width) / (EndCol - StartCol + 1);
end;

function TLoomisForm.height : integer;
begin
  if Drawing then height := TempBitmap.Height
             else height := LoomisImage.Height;
end;

procedure TLoomisForm.StraightenBand;
var
  column, MidAss : integer;
begin
  // Find a suitable value for the B to be used in the y-scale
  with ActiveBand do begin
    // Take as an approximation of B-value half of the difference between peaks in the center
    MidAss := Assignments.count div 2;
    B := 0.5 * (Assignments[MidAss].Peak.wn - Assignments[MidAss-1].Peak.wn)/
               (Assignments[MidAss].m - Assignments[MidAss-1].m);
    // Gather info about the assignments of selected band
    columns := Assignments.count;
    for Column := 1 to Columns do
      WnOrigin[column] := Assignments[column].WnCalc; // Use calculated wavenumbers to make a straight line
  end;
  ZoomAll;
end;

procedure TLoomisForm.ZoomAll;
begin
  StartCol := 1; EndCol := Columns;
  StartOffset := -B; EndOffset := B;
end;

procedure TLoomisForm.Show(wn : double);
begin
  if columns = 0 then exit;
  if Inverted then InvertPoint;
  InvertedPoint := WnToPoint(wn);
  InvertPoint;
end;

procedure TLoomisForm.InvertPoint;
var
  x,y : integer;
begin
  x := InvertedPoint.x; y := InvertedPoint.y;
  with LoomisImage.Canvas do begin
    Pen.Mode := pmNot;
    MoveTo(x-10, y-10); LineTo(x-4, y-4);
    MoveTo(x+10, y-10); LineTo(x+4, y-4);
    MoveTo(x-10, y+10); LineTo(x-4, y+4);
    MoveTo(x+10, y+10); LineTo(x+4, y+4);
  end;
  Inverted := not inverted;
end;

procedure TLoomisForm.DrawPlot;
var
  FirstPeak, LastPeak, Peak, n, m : integer;
begin
  BringToFront;
  Drawing := true;
  LoomisImage.Picture.Bitmap.Width := LoomisImage.Width;
  LoomisImage.Picture.Bitmap.Height := LoomisImage.Height;
  TempBitmap := LoomisImage.Picture.Bitmap;
  if Columns = 0 then StraightenBand; // Must calculate scale before drawing
  // Establish a connection between the columns and band m-values
  with ActiveBand do with Assignments[Assignments.count div 2] do
    mMinusCol := m - NearestColumn(Peak.wn);
  // Locate X- ans Y-origin of the plot
  center.x := 10 + round((-mMinusCol - StartCol) * xstep);
  center.y := round(-StartOffset/(EndOffset - StartOffset) * height);
  // Filter assignments of *all* bands
  with TempBitmap.Canvas do begin
    // Black background
    Pen.Mode := pmCopy;
    Brush.style := bsSolid;
    brush.Color := clBlack;
    FillRect(rect(0,0,TempBitmap.Width,TempBitmap.Height));
    // Draw green axis
    pen.color := $0000FF00;
    moveto(0,Center.y); lineto(TempBitmap.Width, center.y);
    moveto(Center.x,0); lineto(Center.x, TempBitmap.height);
  end;
  FirstPeak := Peaks.NearestIndex(WnOrigin[StartCol]);
  LastPeak := Peaks.NearestIndex(WnOrigin[EndCol]);
  Caption := 'Loomis-Wood plot drawing...';
  // Draw peaks with white balls
  for peak := FirstPeak to LastPeak do DrawPeak(TempBitmap.canvas, Peaks.peak(peak), clWhite, clBlack);
  // Draw assignments using respective band colors
  with Bands do for n := 0 to count-1 do with Band(n)do
  for m := 1 to Assignments.count do with Assignments[m] do
    if AssType = assigned then
    if (peak.wn > WnOrigin[StartCol]) and (peak.wn < WnOrigin[EndCol]) then begin
      if ShowAssignments then DrawPeak(TempBitmap.canvas, peak, col1, col2)
      else DrawPeak(TempBitmap.canvas, peak, clBlack, clBlack);
    end;
  // Finally, draw active band, including predictions
  with ActiveBand do
  for m := 1 to Assignments.count do with Assignments[m] do
  if (peak.wn > WnOrigin[StartCol]) and (peak.wn < WnOrigin[EndCol]) then begin
    if AssType = assigned then DrawPeak(TempBitmap.canvas, peak, col1, col2)
    else DrawPeak(TempBitmap.canvas, peak, clBlack, col2);
  end;
  // To supersample, reduce plot size
  Caption := format('Loomis-Wood plot - Band %s straightened',[ActiveBand.Name]);
  Inverted := false;
  Drawing := false;
  Invalidate;
end;

function TLoomisForm.WnToPoint(wn : double) : TPoint;
var
  p : TPoint;
  column : integer;
  StartWn, EndWn : double;
begin
  column := NearestColumn(wn);
  StartWn := WnOrigin[column] + StartOffset;
  EndWn := WnOrigin[column] + EndOffset;
  p.x := round((column-StartCol) * xstep);
  p.y := round((Wn - StartWn)/(EndWn - StartWn) * height);
  WnToPoint := p;
end;

function TLoomisForm.PointToWn(p : TPoint) : double;
var
  column : integer;
  StartWn, EndWn : double;
begin
  column := round(p.x/xstep) + StartCol;
  StartWn := WnOrigin[column] + StartOffset;
  EndWn := WnOrigin[column] + EndOffset;
  PointToWn := p.y*(EndWn - StartWn)/height + StartWn;
end;

procedure TLoomisForm.DrawPeak(canvas : TCanvas; Peak : TPeak; color1, color2 : TColor);
var
  ColorRatio : double; size : integer;
  p : tpoint;
begin
  Canvas.Pen.Mode := pmCopy;
  if Peak.intensity < MinIntensity then exit; // show only peaks above se limit
  // Determine color of the ball
  ColorRatio := sqrt(Peak.Intensity); // Intensities should be scaled to 0 ... 1
  if ColorRatio > 1.0 then ColorRatio := 1.0;
  if ColorRatio < 0.3 then ColorRatio := 0.3;
  size := round(xstep*0.5 + 1.0);
  // Blend from black to the band color according to peak intensity
  color1 := BlendColor(clBlack, color1, ColorRatio);
  color2 := BlendColor(clBlack, color2, ColorRatio);
  p := WnToPoint(Peak.wn);
  with Canvas do begin
    Brush.Color := color2; Pen.color := color2;
    Ellipse(p.x-size,p.y-size,p.x+size,p.y+size);
    size := round(size * 0.6);
    Brush.Color := color1; Pen.color := color1;
    Ellipse(p.x-size,p.y-size,p.x+size,p.y+size);
  end;
end;

function TLoomisForm.NearestColumn(wn : double) : integer;
var
  Diff, MinDiff : double;
  column, MinCol : integer;
begin
  MinDiff := abs(WnOrigin[1] - wn);
  MinCol := 1;
  for column := 1 to columns do begin
    Diff := abs(WnOrigin[column] - wn);
    if Diff < MinDiff then begin
      MinDiff := Diff;
      MinCol := column;
    end;
  end;
  NearestColumn := MinCol;
end;

procedure TLoomisForm.LoomisImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  m : integer;
  NearAss : TAssignment;
  peak : TPeak;
  isassigned : boolean;
begin
  if columns = 0 then exit;
  if Button = mbLeft then begin // LeftClick = assign or remove assignment
    // Find peak nearest to the clicking position
    peak := Peaks.NearestPeak(PointToWn(point(x,y)));
    m := NearestColumn(Peak.wn) + mMinusCol;
    IsAssigned := false;
    With ActiveBand.Assignments do if Count>0 then begin
      NearAss := Assignment[NearestWnobs(Peak.wn)];
      if abs(NearAss.Peak.wn - Peak.wn)<1e-5 then IsAssigned := true;
    end;
    // Act according to shift state
    if (ssShift in Shift) and IsAssigned then begin  // Remove assigment
      NearAss.AssType := predicted;
      DrawPeak(LoomisImage.canvas, peak, clBlack, ActiveBand.col2);
    end else if ssCtrl in Shift then with ActiveBand.Assignments do begin // add assignment
      if count = 0 then
        ActiveBand.AddAssignment(m, assigned, peak, peak.wn)
      else begin
        NearAss := AssNearestM(m); // ANY assignment with given m?
        if NearAss.m = m then begin// Move assignment with correct M to the peak
          NearAss.AssType := assigned;
          NearAss.Peak := peak;
        end else
          ActiveBand.AddAssignment(m, assigned, peak, peak.wn);
      end;
      DrawPeak(LoomisImage.canvas, Peak, ActiveBand.col1, ActiveBand.col2);
      ActiveBand.Assignments.UpdateRowCount;
    end;
    // Show the present peak in all three windows
    with AssignmentForm.AssDrawGrid do begin
      Row := ActiveBand.Assignments.NearestWnobs(peak.wn)+1;
      Invalidate;
    end;
    Show(Peak.wn);
    SpectraWindow.SetViewCenter(Peak.wn);
  end else if Button = mbRight then begin // RightClick = Zoom
    Zooming := true;
    ZoomOrigin := point(x,y);
    ZoomCursor := ZoomOrigin;
    InvertZoomRect;
  end;
end;

procedure TLoomisForm.InvertZoomRect;
var
  p1, p2 : TPoint;
begin
  p1 := point(min(ZoomOrigin.X,ZoomCursor.X), min(ZoomOrigin.Y, ZoomCursor.Y));
  p2 := point(max(ZoomOrigin.X,ZoomCursor.X), max(ZoomOrigin.Y, ZoomCursor.Y));
  p1 := ScreenToClient(LoomisImage.ClientToScreen(p1));
  p2 := ScreenToClient(LoomisImage.ClientToScreen(p2));
  With Canvas do begin
    Pen.Mode := pmNot;
    Brush.Style := bsClear;
    Canvas.Rectangle(p1.x, p1.y, p2.x, p2.y);
    Pen.Mode := pmCopy;
    Brush.Style := bsSolid;
  end;
end;

procedure TLoomisForm.LoomisImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Zooming then begin
    InvertZoomRect;
    ZoomCursor.X := X;
    ZoomCursor.Y := Y;
    InvertZoomRect;
  end;
end;

procedure TLoomisForm.LoomisImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  result : word;
begin
  if not Zooming then exit;
  result := MessageDlg('Zoom the selected region?',mtConfirmation,[mbOK, mbCancel],0);
  InvertZoomRect;
  Zooming := false;
  if result = mrOK then ZoomRegion;
end;

procedure TLoomisForm.ZoomRegion;
var
  col1, col2 : integer;
  o1, o2 : double;
begin
  // Calculate zoom region
  col1 := round((ZoomOrigin.x - 10)/xstep + StartCol);
  col2 := round((ZoomCursor.x - 10)/xstep + StartCol);
  StartCol := min(col1,col2); if StartCol < 1 then StartCol := 1;
  EndCol := max(col1,col2); if EndCol > Columns then EndCol := Columns;
  o1 := StartOffset + min(ZoomOrigin.y, ZoomCursor.y)/height*(EndOffset - StartOffset);
  o2 := StartOffset + max(ZoomOrigin.y, ZoomCursor.y)/height*(EndOffset - StartOffset);
  StartOffset := o1;
  EndOffset := o2;
  DrawPlot;
end;

procedure TLoomisForm.FormCreate(Sender: TObject);
begin
  columns := 0; // Indicator, that nothing has been drawn yet.
  ShowAssignments := true;
  MinIntensity := 0.0;
  Drawing := false;
end;

procedure TLoomisForm.SaveImage;
begin
  LoomisImage.Picture.Bitmap.SaveToFile(
    InputBox('Save Loomis-Wood Image to file',
             'Give filename to save (ending with .BMP)',
             '\temp\loomis.bmp'));
end;

procedure TLoomisForm.SetParameters;
var
  code : integer;
begin
  with LoomisParamsDialog do begin
    ShowAssignedBox.Checked := ShowAssignments;
    IntensityEdit.text := format('%.3f',[MinIntensity]);
    if ShowModal = 2 {ok} then begin
      ShowAssignments := ShowAssignedBox.Checked;
      val(IntensityEdit.text, MinIntensity, code);
    end;
  end;
end;

end.
