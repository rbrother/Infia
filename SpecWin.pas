unit Specwin;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Buttons, StdCtrls, Menus, PeakUnit;

const
  PeakMarkerColor = $0200D0FF;

type
  TSpectraWindow = class(TForm)
    OptionPanel: TPanel;
    UnzoomAll: TSpeedButton;
    WavenumberPanel: TPanel;
    ViewStartPanel: TPanel;
    ViewEndPanel: TPanel;
    LineMenu: TPopupMenu;
    AutomaticAssign1: TMenuItem;
    DeletePeak1: TMenuItem;
    ManualAssign1: TMenuItem;
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToggleMarkers(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UnzoomAllClick(Sender: TObject);
    procedure AutomaticAssign1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DeletePeak1Click(Sender: TObject);
    procedure ManualAssign1Click(Sender: TObject);
  private
  public
    SpectrumLoaded : boolean;
    DrawLineMarkers, DrawSelBandLabels, DrawAllBandLabels,
      DrawPredMarkers : boolean;
    ViewStart, ViewEnd : double;
    VerticalZoom : single; // Vertical multiplier
    DoingSelection : boolean;
    RangeSelected : boolean;
    SelectionStart, SelectionEnd : word;
    SelectedPeak : TPeak;
    ScrollStep : integer;
    // Our own methods
    function SpectraRect : TRect;
    procedure DrawGraphics(startx, endx : integer);
    // Spectra drawing
    procedure DrawSpectra(startx, endx : integer);
    procedure DrawSpectraLow(startx, endx : integer); { For low zoom levels }
    procedure DrawSpectraHigh(startx, endx : integer); { For low zoom levels }
    // Additional items to display in spectra window
    procedure DrawPeakMarkers(StartWn, EndWn : double);
    procedure DrawVertLine(wn : double; color : TColor; width : integer);
    procedure DrawAssMarkers(drawstart, drawend : double);
    procedure DrawPredLabels(drawstart, drawend : double);
    procedure DrawPredLines(drawstart, drawend : double);
    procedure DrawpredLine(LineWn : double);
    // Other
    function WaveToWindowX(wavenumber : double) : integer;
    function WaveToWindowY(wavenumber : double) : integer;
    function WindowXtoWave(X : integer) : double;
    function IntensityToWindowY(y : single) : integer;
    procedure InvertSelection;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure Scroll(Dir : integer);
    procedure ScrollLarge(Dir : integer);
    procedure SetViewStart(x : double);
    procedure SetViewEnd(x : double);
    procedure SetViewCenter(wn : double);
    procedure LineAssignment(mousex, mousey : integer);
  end;

var
  SpectraWindow: TSpectraWindow;

implementation

uses Spectra, BandUnit, AssignmentUnit,
  Analwin, ScrollUnit, ScriptUnit, utils;

{$R *.DFM}

procedure TSpectraWindow.FormCreate(Sender: TObject);
begin
  SpectrumLoaded := false;
  DrawLineMarkers := true;
  DrawSelBandLabels := true;
  DrawAllBandLabels := true;
  DrawPredMarkers := true;
  DoingSelection := false;
  RangeSelected := false;
  VerticalZoom := 1.0;
  LabelType := MLabel;
end;

function TSpectraWindow.SpectraRect : TRect;
var
  r : TRect;
begin
  r := ClientRect;
  inc(r.Top, OptionPanel.Height);
  SpectraRect := r;
end;

procedure TSpectraWindow.FormResize(Sender: TObject);
begin
  Invalidate; { Must redraw the spectra if window size changes }
  RangeSelected := false;
end;

procedure TSpectraWindow.FormPaint(Sender: TObject);
begin
  if SpectrumLoaded then DrawGraphics(SpectraRect.Left, SpectraRect.Right);
  if RangeSelected then InvertSelection;
end;

procedure TSpectraWindow.DrawGraphics(startx, endx : integer);
var
  drawstart, drawend : double;
begin
  drawstart := WindowXtoWave(startx-20); // Draw a bit wider region than requested
  drawend := WindowXtoWave(endx+20);
  if DrawLineMarkers then DrawPeakMarkers(drawstart,drawend);
  if (Bands.count > 0) and DrawPredMarkers then DrawPredLines(drawstart,drawend);
  DrawSpectra(startx, endx); // draw the spectra
  peaks.ZeroLabelCount(drawstart, drawend);
  if (Bands.count > 0) and DrawPredMarkers and DrawAllBandLabels then DrawPredLabels(drawstart,drawend);
  if (Bands.count > 0) and (DrawAllBandLabels or DrawSelBandLabels) then DrawAssMarkers(drawstart,drawend);
end;

procedure TSpectraWindow.DrawSpectra(startx, endx : integer);
var
  WnWidth, ViewWidth, points_per_pixel : double;
begin
  WnWidth := MySpectra.RangeEnd - MySpectra.RangeStart;
  ViewWidth := ViewEnd - ViewStart;
  points_per_pixel := MySpectra.npoints * (ViewWidth / WnWidth) / ClientWidth;
  if points_per_pixel > 5 then
    DrawSpectraLow(startx, endx)
  else
    DrawSpectraHigh(startx, endx);
end;

procedure TSpectraWindow.DrawSpectraHigh(startx, endx : integer);
{ This is for High zoom levels: take one sample of the spectra per pixel }
var
  screenx, y : integer;
  x, xstep : double;
  firstpoint : boolean;
begin
  Canvas.Pen.Mode := pmBlack;
  xstep := (ViewEnd - ViewStart) / ClientWidth;
  x := ViewStart + xstep * (startx-SpectraRect.Left);
  firstpoint := true;
  repeat
    y := WaveToWindowY(x);
    screenx := WaveToWindowX(x);
    if (screenx >= startx) and (screenx <= endx) then
      if firstpoint then begin
        Canvas.MoveTo(screenx, y);
        firstpoint := false;
        end
      else
        Canvas.LineTo(screenx, y);
    x := x + xstep;
  until screenx >= endx;
end;

procedure TSpectraWindow.DrawSpectraLow(startx, endx : integer);
{ For low zoom levels: for every x-value draw a line between the
  minimum and maximum y-values. Take many samples from pixels area.}
var
  screenx : integer;
  x, xstep, xx : double;
  y, miny, maxy : single;
begin
  Canvas.Pen.Mode := pmBlack;
  xstep := (ViewEnd - ViewStart) / ClientWidth;
  screenx := startx;
  x := ViewStart + xstep * (startx-SpectraRect.Left);
  repeat
    { Now find the minimum and maximum y-value in the pixels range }
    maxy := MySpectra.Value(x); miny := maxy;
    xx := x - xstep * 0.6;
    repeat
        y := MySpectra.Value(xx);
        if y > maxy then maxy := y;
        if y < miny then miny := y;
      xx := xx + 0.001; // Take samples so near each other that no peak is missed!
    until xx > x + xstep*0.6;
    Canvas.MoveTo(screenx, IntensityToWindowY(miny));
    Canvas.LineTo(screenx, IntensityToWindowY(maxy)-1);
    x := x + xstep;
    inc(screenx);
  until screenx >= endx;
end;

procedure TSpectraWindow.DrawPeakMarkers(StartWn, EndWn : double);
// Peak markers are small yellow vertical lines that mark the peaks of the spectra
var
  x,y,PrevX,n : integer;
begin
  Canvas.Pen.Mode := pmCopy;
  Prevx := 0;
  // Iterate through lines
  n := Peaks.NearestIndex(StartWn);
  with peaks do
  while (n<Count) and (peak(n).wn<EndWn) do with peaks.peak(n) do begin
    x := WaveToWindowX(wn);
    if x<>Prevx then with canvas do begin
      y := WaveToWindowY(wn);
      // Select color for peak marker line
      Pen.Color := PeakMarkerColor;
      MoveTo(x,0); LineTo(x,y); // Draw peak marker line
      Prevx := x;
    end;
    inc(n);
  end;
end;

procedure TSpectraWindow.DrawPredLabels(drawstart, drawend : double);
// Draw labels on top of assigned lines showing their band and m-value
var
  n, m : integer;
  LabelText : string;
  PeakError : double;
begin
  PeakError := strtofloat(ProjFile.ReadString('Settings','PeakError', '0.0001'));
  with Bands do for n := 0 to count-1 do with band(n) do
  with Assignments do if count > 0 then
    for m := NearestWnObs(drawstart) to NearestWnObs(drawend) do with Assignments[m] do
    if (AssType = predicted) and (Band <> ActiveBand) then begin
      TBand(Band).DrawBandLabel(Canvas, WaveToWindowX(WnCalc),
        WaveToWindowY(WnCalc)-25, AssLabel, PeakMarkerColor);
      if abs(WnCalc - Peak.wn) < PeakError*20.0 then inc(Peak.LabelCount);
    end;
end;

procedure TSpectraWindow.DrawAssMarkers(drawstart, drawend : double);
// Draw labels on top of assigned lines showing their band and m-value
// This can be made quicker by not going through ALL assigned lines.
var
  n, m : integer;
  LabelText : string;
begin
  with Bands do
  for n := 0 to count-1 do with band(n).assignments do if count>0 then begin
    //display('band '+inttostr(n)+' drawing at m-range ('+inttostr(NearestWnobs(drawstart))+' - '+inttostr(NearestWnobs(drawend))+')');
    //display('      drawrange ('+floattostr(drawstart)+'-'+floattostr(drawend)+')');
    for m := NearestWnObs(drawstart) to NearestWnObs(drawend) do with Assignment[m] do
    if (AssType = assigned) and
     (DrawAllBandLabels or (DrawSelBandLabels and (Band = ActiveBand))) then
    begin
      TBand(Band).DrawBandLabel(Canvas, WaveToWindowX(Peak.wn),
        WaveToWindowY(Peak.wn)-25*(Peak.LabelCount + 1), AssLabel, clBlack);
      inc(Peak.LabelCount);
    end;
  end;
end;

procedure TSpectraWindow.DrawPredLines(drawstart, drawend : double);
// Draw blue prediction lines for active band
var
  n : integer;
begin
  // Iterate through *all* assignments/predictions of the band
  with ActiveBand.Assignments do
    for n := 1 to Count do with Assignment[n] do
      if (wncalc > drawstart) and (wncalc < drawend) then DrawPredLine(WnCalc);
end;

function TSpectraWindow.WaveToWindowX(wavenumber : double) : integer;
// Note: can result negative value or value out from the sctual screen
begin
  WaveToWindowX := 0;
  try
    WaveToWindowX :=
    SpectraRect.Left +
    round((wavenumber - ViewStart)/(ViewEnd - ViewStart)*ClientWidth);
  except
    on EDivByZero do begin
      ViewStart := MySpectra.RangeStart;
      ViewEnd := MySpectra.RangeEnd;
      Errormessage('TSpectraWindow.WaveToWindowX: ViewEnd equal to ViewStart. Resetted to spectra range.');
    end;
    on ERangeError do begin
      Display('TSpectraWindow.WaveToWindowX: Range Error');
    end;
  end;
end;

function TSpectraWindow.WindowXtoWave(X : integer) : double;
begin
  WindowXtoWave :=
    (X - SpectraRect.Left) * (ViewEnd-ViewStart) / ClientWidth  + ViewStart;
end;

function TSpectraWindow.WaveToWindowY(wavenumber : double) : integer;
begin
  WaveToWindowY := IntensityToWindowY(MySpectra.Value(wavenumber));
end;

function TSpectraWindow.IntensityToWindowY(y : single) : integer;
// Converts the intensityvalue y to screen y-position
var
  SpectraHeight, ScreenHeight : single;
  GapAtTop : integer;
begin
  GapAtTop := 40;
  ScreenHeight := SpectraRect.Bottom - SpectraRect.Top - GapAtTop; // in pixels
  SpectraHeight := ScreenHeight * VerticalZoom; // zoom the spectra to zoom%
  IntensityToWindowY :=
    round( ScreenHeight - y * SpectraHeight + SpectraRect.Top ) + GapAtTop-1;
end;

procedure TSpectraWindow.ToggleMarkers(Sender: TObject);
begin
  Invalidate; { Redraw the spectra }
end;

procedure TSpectraWindow.InvertSelection;
var
  a,b,x : word;
begin
  if SelectionStart <= SelectionEnd then
    begin a := SelectionStart; b := SelectionEnd; end
  else
    begin b := SelectionStart; a := SelectionEnd; end;
  Canvas.Pen.Mode := pmNot;
  for x := a to b do
  begin
    Canvas.Moveto(x,0);
    Canvas.Lineto(x,Height);
  end;
end;

{ ********************************
  Mouse Action and range selection
  ******************************** }

procedure TSpectraWindow.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    // Left button: range selection }
    if RangeSelected then InvertSelection;
    SelectionStart := X;
    SelectionEnd := X;
    DoingSelection := true;
    RangeSelected := true;
    InvertSelection;
    end
  else if (Button = mbRight) then LineAssignment(X,Y);
end;

procedure TSpectraWindow.LineAssignment(mousex, mousey : integer);
begin
  if ActiveBand = NIL then exit;
  // Hilight the line nearest cursor
  SelectedPeak := Peaks.NearestPeak(WindowXtoWave(mousex));
  DrawVertLine(SelectedPeak.wn, clBlack, 3);
  // Write selected band number to some popup-menu items
  with ManualAssign1 do begin
    Caption := format('&Manually assign to band %s',[ActiveBand.name]);
  end;
  with AutomaticAssign1 do begin
    Caption := format('&Automatically assign to band %s',[ActiveBand.name]);
    // Allow only automatic assignment, if 3 or more assignments
    if ActiveBand.Assignments.Count < 3 then enabled := false else enabled := true;
  end;
  // Bring in the menu!
  LineMenu.Popup(Left+mousex+40,Top+mousey+100);
  // Remove dark line after menu has been used
  DrawVertLine(SelectedPeak.wn, clWhite, 3);
  DrawVertLine(SelectedPeak.wn, PeakMarkerColor, 1);
end;

procedure TSpectraWindow.DrawPredLine(LineWn : double);
// Draw a line showing the calculated position of a peak.
// The width of the drawn line corresponds to the standard error in the calculated value.
const
  LineError = 0.0005;
var
  x, xstart, xend, y : integer;
  Wn, WnStep, prob : double;
begin
  Canvas.Pen.Mode := pmCopy;
  xstart := WaveToWindowX(LineWn - LineError * 2.0);
  xend := WaveToWindowX(LineWn + LineError * 2.0);
  WnStep := WindowXtoWave(1)-WindowXtoWave(0);
  Wn := LineWn - LineError * 2.0;
  for x := xstart to xend do begin
    // Calculate probability
    prob := exp(-0.5*sqr((wn-LineWn)/LineError))/sqrt(LineError)*WnStep*100.0;
    if prob < 0.0 then prob := 0.0;
    if prob > 1.0 then prob := 1.0;
    y := WaveToWindowY(wn);
    with canvas do begin
      Pen.Color := Blend3Color(clWhite, clBlue+clGreen, clBlue, prob);
      MoveTo(x,Height);
      LineTo(x,y);
    end;
    Wn := Wn + WnStep;
  end;
end;

procedure TSpectraWindow.DrawVertLine(wn : double; color : TColor; width : integer);
// Draws a vertical line to above the spectra curve.
// Used to indicate peaks, predicted lines etc.
var
  x,xx,y : integer;
begin
  x := WaveToWindowX(wn);
  y := WaveToWindowY(wn);
  Canvas.Pen.Color := color;
  Canvas.Pen.Mode := pmCopy;
  with canvas do for xx := x - (width div 2) to x - (width div 2) + width - 1 do begin
    MoveTo(xx,20); LineTo(xx,y);
  end;
end;

procedure TSpectraWindow.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  wavenumber : double; xx,a,b : integer;
begin
  if not SpectrumLoaded then exit;
  if DoingSelection and (X <> SelectionEnd) then begin
    if SelectionEnd < SelectionStart then begin
      if X < SelectionEnd then begin a:=X; b:=SelectionEnd-1; end
      else begin a:=SelectionEnd; b:=X-1; end;
      end
    else begin
      if X < SelectionEnd then begin a:=X+1; b:=SelectionEnd; end
      else begin a:=SelectionEnd+1; b:=X; end;
    end;
    if ((a<SelectionStart)and(b<SelectionStart)) or
       ((a>SelectionStart)and(b>SelectionStart)) then begin
      Canvas.Pen.Mode := pmNot;
      for xx := a to b do
      begin
        Canvas.Moveto(xx,0);
        Canvas.Lineto(xx,Height);
      end;
      SelectionEnd := X;
      end
    else begin
      InvertSelection;
      SelectionEnd := X;
      InvertSelection;
    end;
  end;
  { Refresh the panels that contain info about cursor position }
  wavenumber := WindowXtoWave(X);
  WavenumberPanel.Caption := format('%.4f',[wavenumber]);
end;

procedure TSpectraWindow.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then DoingSelection := false;
end;

{ ********** ZOOMING and UNZOOMING ************** }

procedure TSpectraWindow.ZoomIn;
var
  a,b : word;
  NewStart, NewEnd : double;
begin
  if not RangeSelected then begin
    NewStart := ViewStart + (ViewEnd - ViewStart) * 0.2;
    NewEnd := ViewEnd - (ViewEnd - ViewStart) * 0.2;
    end
  else
  begin
    if SelectionStart <= SelectionEnd then
      begin a := SelectionStart; b := SelectionEnd; end
    else
      begin b := SelectionStart; a := SelectionEnd; end;
    NewStart := ViewStart + (ViewEnd - ViewStart) * (a / ClientWidth);
    NewEnd := ViewStart + (ViewEnd - ViewStart) * (b / ClientWidth);
  end;
  if abs(NewEnd-NewStart) > 0.005 then begin
    SetViewStart(NewStart);
    SetViewEnd(NewEnd);
    rangeselected := false;
    invalidate;
  end;
end;

procedure TSpectraWindow.ZoomOut;
var
  ViewRange : double;
begin
  ViewRange := ViewEnd - ViewStart;
  RangeSelected := false; // Zooming removes range selection
  SetViewStart(ViewStart - ViewRange * 0.2);
  SetViewEnd(ViewEnd + ViewRange * 0.2);
  invalidate;
end;

procedure TSpectraWindow.UnzoomAllClick(Sender: TObject);
begin
  if not SpectrumLoaded then exit;
  SetViewStart(MySpectra.RangeStart);
  SetViewEnd(MySpectra.RangeEnd);
  invalidate;
end;

procedure TSpectraWindow.SetViewStart(x : double);
begin
  with MySpectra do begin
    if x < RangeStart then x := RangeStart;
    if x > RangeEnd then x := RangeEnd;
  end;
  ViewStart := x;
  with ViewStartPanel do begin
    Caption := format('%.4f',[ViewStart]);
    Invalidate;
  end;
end;

procedure TSpectraWindow.SetViewEnd(x : double);
begin
  with MySpectra do begin
    if x > RangeEnd then x := RangeEnd;
    if x < RangeStart then x:= RangeStart;
  end;
  ViewEnd := x;
  with ViewEndPanel do begin
    Caption := format('%.4f',[ViewEnd]);
    Invalidate;
  end;
end;

procedure TSpectraWindow.SetViewCenter(wn : double);
var
  ViewRange : double;
begin
  if not SpectrumLoaded then exit;
  ViewRange := ViewEnd - ViewStart;
  if (wn - ViewRange > MySpectra.RangeStart) and
     (wn + ViewRange < MySpectra.RangeEnd) then begin
    SetViewStart(wn - ViewRange * 0.5);
    SetViewEnd(wn + ViewRange * 0.5);
    Invalidate;
  end;
end;

procedure TSpectraWindow.Scroll(Dir : integer);
var
  source, dest : TRect;
  ViewRange, delta : double;
begin
  ViewRange := ViewEnd - ViewStart;
  delta := ScrollStep * ViewRange / ClientWidth;
  with Canvas do begin
    // Scroll the screen and 'draw new graphics
    RangeSelected := false; // Scrolling removes range selection
    // Exit, if user tries to scroll outside available range
    if (ViewStart + delta*dir < MySpectra.RangeStart) or (ViewEnd + delta*dir > MySpectra.RangeEnd) then exit;
    SetViewStart(ViewStart + delta * dir);
    SetViewEnd(ViewEnd + delta * dir);
    source := SpectraRect; dest := source;
    dest.Left := dest.Left - ScrollStep * dir;
    dest.Right := dest.Right - ScrollStep * dir;
    CopyRect(dest,Canvas,source); // Do the scrolling
    dest := SpectraRect;
    if dir = 1 then
      dest.Left := dest.Right - ScrollStep
    else
      dest.Right := dest.Left + ScrollStep;
    Brush.Style := bsSolid;  Brush.Color := clWhite;
    FillRect(dest); // Clear old area
    DrawSpectra(dest.Left-1, dest.Right+1); // Draw spectra there
  end;
end;

procedure TSpectraWindow.ScrollLarge(dir : integer);
var
  ViewRange, delta : double;
begin
  ViewRange := ViewEnd - ViewStart;
  delta := 0.75 * ViewRange;
  with Canvas do begin
    // Scroll the screen and draw new graphics
    RangeSelected := false; // Scrolling removes range selection
    // Exit, if user tries to scroll outside available range
    if (ViewStart + delta*dir < MySpectra.RangeStart) or (ViewEnd + delta*dir > MySpectra.RangeEnd) then exit;
    SetViewStart(ViewStart + delta * dir);
    SetViewEnd(ViewEnd + delta * dir);
  end;
  RangeSelected := false; // Scrolling removes range selection
  Invalidate; // Draw everything again
end;

procedure TSpectraWindow.AutomaticAssign1Click(Sender: TObject);
// Convert the nearest prediction (or assignment!) to an assignment
begin
  with ActiveBand.Assignments do begin
    if Predictions = 0 then begin
      MessageDlg('You must first calculate some predicted peaks for the automatic assignment to work (Use band/calculate menu command)',mtInformation,[mbOK],0);
      exit;
    end;
    with Assignment[NearestWnobs(SelectedPeak.wn)] do
    if AssType = Assigned then
      MessageDlg('Automatic assign: could not find prediction near the selected peak',mtWarning,[mbOK],0)
    else begin
      Peak := SelectedPeak;
      WnCalc := SelectedPeak.wn;
      AssType := Assigned;
      Weight := 1.0;
    end;
  end;
  Invalidate;
  AssignmentForm.AssDrawGrid.Invalidate;
end;

procedure TSpectraWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // 37=Left, 39=Right, 38=Up, 40=Down, PageUp=33, PageDown=34
  // Shift uses smaller scroll steps
  if ((Key = 37{Left}) or ( Key = 39{Right}))
  and ScrollThread.Suspended and not (ssCtrl in Shift) then
  with ScrollThread do begin
    if Key = 37 then Direction := -1
    else if Key = 39 then Direction := 1;
    if ssShift in Shift then ScrollStep := 1 else ScrollStep := 4;
    horisontal := true;
    Suspended := false;
  end;
  if ((Key = 38{Up}) or ( Key = 40{Down}))
  and ScrollThread.Suspended then with ScrollThread do begin
    if Key = 40 then Direction := -1
    else if Key = 38 then Direction := 1;
    horisontal := false;
    Suspended := false;
  end;
end;

procedure TSpectraWindow.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=37{Left}) or (Key=39{Right}) or (Key=38{Up}) or (Key=40{Down}) then begin
    ScrollThread.Suspended := true;
    Invalidate;
  end;
  if Key = 33 {PageUp} then ZoomOut;
  if Key = 34 {PageUp} then ZoomIn;
end;

procedure TSpectraWindow.DeletePeak1Click(Sender: TObject);
begin
{
  Peak.Delete;
  Invalidate;
}
end;

procedure TSpectraWindow.ManualAssign1Click(Sender: TObject);
var
  m : integer;
begin
  m := StrToInt(InputBox('New Assignment', 'Give assignment m value', '0'));
  ActiveBand.AddAssignment(m, assigned, SelectedPeak, SelectedPeak.wn);
  with ActiveBand.Assignments do begin
    UpdateRowCount;
    SortM;
  end;
  AssignmentForm.AssDrawGrid.Invalidate;
  SpectraWindow.Invalidate;
end;

END.
