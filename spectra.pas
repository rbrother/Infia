unit Spectra;

interface

uses Classes, WinProcs, WinTypes, Dialogs, SysUtils, Math;

type
  // Use very large maximum index in the arrays: we only allocate
  // the memory required though!
  TPointArray = array [1..10000000] of Single;

  TSpectra = class
  private
    changed : boolean;
    oldfilename : string;
    Header : array[0..319] of longint;
    function ReadSpectra(FileName : TFileName) : boolean; // Reads the spectra in programs standard mode
  public
    npoints : integer;
    Point : ^TPointArray;
    RangeStart, RangeEnd : double; // Wavenumbers of start and end
    Step : double; // wn difference between two points. Calculated from other constants
    // Methods
    constructor CreateFromFile(Filename : string);
    function Value(wn : double) : single;
    procedure ReadDIVSpectra(FileName : TFileName);
    procedure ReadDATSpectra(FileName : TFileName);
    function WriteSpectra : boolean;
    function WriteSpectraAs(filename : string) : boolean;
    procedure Crop(wn1, wn2 : double);
    procedure Invert;
    procedure Normalise;
    procedure ShiftSpectra(DeltaWn : double);
    procedure CorrectBaseline(SampleStep : double);
    procedure ExportSpectra(Filename : string; First, Last, Step : double);
    destructor Destroy; override;
  end;

var
  MySpectra : TSpectra;

implementation

uses linalg, analwin, specwin, ScriptUnit;

constructor TSpectra.CreateFromFile(Filename : string);
var
  LoadSuccess : boolean;
begin
  LoadSuccess := ReadSpectra(FileName);
  if LoadSuccess then with SpectraWindow do begin
    ViewStart := RangeStart;
    ViewEnd := RangeEnd;
    Invalidate; { Draw the newly loaded spectra }
    caption := format('Spectrum %s',[FileName]);
    AnalyseWindow.Spectra1.Enabled := true;
  end else begin
    errormessage(format('TSpectra.Create: Spectrum file %s not found',[filename]));
  end;
  changed := false;
end;

function TSpectra.Value(wn : double) : single;
var
  point2 : integer;
  y0,y1,y2,y3, z1,z2,z3, a1,a2,a3, x0,dx : double;
begin
  if (wn < RangeStart) then wn := RangeStart;
  if (wn > RangeEnd) then wn := RangeEnd;
  // Approximation made by third order polynome fit.
  point2 := trunc((wn-RangeStart)/Step)+1;
  if point2 < 2 then point2 := 2;
  if point2 > npoints-2 then point2 := npoints-2;
  x0 := RangeStart + (point2-1)*Step;
  dx := wn - x0;
  y0 := Point[point2-1];
  y1 := Point[point2];
  y2 := Point[point2+1];
  y3 := Point[point2+2];
  z1 := y1 - y0; z2 := y2 - y0; z3 := y3-y0;
  a1 := (3.0*z1  - 1.5*z2 + (1/3)*z3)/ Step;
  a2 := (-2.5*z1 + 2.0*z2 - 0.5*z3)  /(Step*Step);
  a3 := (0.5*z1  - 0.5*z2 + (1/6)*z3)/(Step*Step*Step);
  Value := a3*dx*dx*dx + a2*dx*dx + a1*dx + y0;
end;

destructor TSpectra.Destroy;
begin
  FreeMem(Point, sizeof(Single)*npoints); // Free datapoint memory
  inherited Destroy;
end;

procedure TSpectra.ReadDIVSpectra(FileName : TFileName);
// For conversion purposes. The format of the spectra file is '.DIV'
const
  RS_NPOINTS = 65;
  RS_INTFREQ = 69;
  RS_FRACFREQ = 70;
  RS_EINTFREQ = 71;
  RS_EFRACFREQ = 72;
  RS_MULT = 5.9604644e-8;
var
  DivFile : file; // Untyped binary file
  actualread : integer;
begin
  assignfile(DivFile, FileName);
  reset(DivFile, 1); // Open file with recordlength of 1 byte
  BlockRead(DivFile, Header, sizeof(Header)); // Read the header
  npoints := header[RS_NPOINTS];
  Point := AllocMem(sizeof(Single)*npoints); // Allocate memory for spectra datapoints
  // Read the spectrum data point (stored as singles)
  BlockRead(DivFile, Point^, sizeof(Single)*npoints, actualread);
  CloseFile(DivFile);
  Normalise; // Rescale to 0..1
  Invert;
  RangeStart := Header[RS_INTFREQ] + Header[RS_FRACFREQ] * RS_MULT;
  RangeEnd := Header[RS_EINTFREQ] + Header[RS_EFRACFREQ] * RS_MULT;
  changed := true;
end;

procedure TSpectra.ReadDATSpectra(FileName : TFileName);
// Reads the spectra in ASCII text file of format:
// wn  intensity
// wn  intensity
// ...
// Note: the wn:s need not be equally spaced. The data is converted
// to equally spaced format by interpolating (fitting 3th order polynome to
// 4 consequent points of data and resampling)
var
  SpectraFile : TextFile;
  wn : double; intensity : single;
  X,Y : Variant; // variables for the 3th order fit
  SampleWn, SampleStep : double;
  code, n, m : integer;
begin
  // Ask the resampling step
  SampleStep := strtofloat(InputBox('Spectrum File Conversion', 'Give spectrum resampling step', '0.001'));
  // Count the lines in the file and calculate required amount of data points
  // Also find the min and max values
  AssignFile(SpectraFile, Filename);
  Reset(SpectraFile);
  read(SpectraFile, wn, intensity);
  RangeStart := wn; // First wavenumber
  while not eof(SpectraFile) do begin
    read(SpectraFile, wn, intensity);
    if not eof(SpectraFile) then RangeEnd := wn;
  end;
  npoints := trunc((RangeEnd - RangeStart) / SampleStep);
  Point := AllocMem(sizeof(Single)*npoints); // Allocate memory for spectra datapoints
  X := VarArrayCreate([0,10], varDouble);
  Y := VarArrayCreate([0,10], varDouble);
  // Now do the real reading of the file
  reset(SpectraFile);
  for n := 1 to 4 do begin // Read the first four points
    read(SpectraFile, wn, intensity);
    x[n] := wn; y[n] := intensity;
  end;
  SampleWn := x[1];
  for n := 1 to npoints do begin // Sample the points and read the file
    if n mod 50 = 0 then Analysewindow.ShowStatus(format('Resampling at wavenumber %.1f',[SampleWn]));
    // Calculate predicted intensity by average
    if X[2] = X[3] then
      intensity := Y[2]
    else
      intensity := Y[2] + (SampleWn-X[2])*((Y[3]-Y[2])/(X[3]-X[2]));
    point[n] := intensity;
    SampleWn := SampleWn + SampleStep;
    if SampleWn > x[3] then begin
      // Read a new point from the file and make it the fourth datapoint
      read(SpectraFile, wn, intensity);
      while (wn < X[4]+SampleStep) and not eof(SpectraFile) do read(SpectraFile, wn, intensity); // if two consequent points have same wavenumber, continue reading.
      if not eof(SpectraFile) then begin
        for m := 1 to 3 do begin X[m] := X[m+1]; Y[m] := Y[m+1]; end;
        X[4] := wn; Y[4] := intensity;
      end;
    end;
  end;
  CloseFile(SpectraFile);
  Normalise; // Normalise the spectra range from 0 to 1
  changed := true;
end;

function TSpectra.ReadSpectra(FileName : TFileName) : boolean;
// Reads the spectra in programs standard format
var
  SpectraFile : TextFile;
  n, code : longint;
  line : string;
begin
  oldfilename := filename;
  ReadSpectra := false;
  try
    AssignFile(SpectraFile, Filename);
    reset(SpectraFile);
    readln(SpectraFile, line); // Read the header line
  except
    on EInOutError do errormessage('TSpectra.ReadSpectra: Problems reading the spectra file of the project');
  end;
  if line <> 'Spectra Data File' then begin
    MessageDlg('TSpectra.ReadSpectra: Unknown file format',mtError,[mbOK],0);
    exit;
  end;
  // Read range start
  readln(SpectraFile, line);
  delete(line, 1, length('StartWavenumber '));
  val(line , RangeStart, code);
  // Read range end
  readln(SpectraFile, line);
  delete(line, 1, length('EndWavenumber '));
  val(line, RangeEnd, code);
  // Read number of points and allocate memory
  readln(SpectraFile, line);
  delete(line, 1, length('Points '));
  val(line, npoints, code);
  Point := AllocMem(sizeof(Single)*npoints); // Allocate memory for spectra datapoints
  // Read points
  readln(SpectraFile, line); // Read the line 'Intensity list:'
  for n := 1 to npoints do readln(SpectraFile, point[n]);
  closefile(SpectraFile);
  Step := (RangeEnd - RangeStart) / (npoints-1);  // Step unit = wavenumbers
  ReadSpectra := true; // success!
  changed := false;
end;

procedure TSpectra.Normalise;
// scales the spectra intensity values to the standard range 0..1
var
  min, max : double; // Min and max y-values
  n : integer;
begin
  min := Point[1]; max := min;
  for n := 1 to npoints do begin
    if point[n]<min then min := point[n];
    if point[n]>max then max := point[n];
  end;
  for n := 1 to npoints do point[n] := (point[n]-min)/(max-min);
  changed := true;
end;

procedure TSpectra.Invert;
// Inverts a normalised spectra 0..1 -> 1..0
var
  n : integer;
begin
  for n := 1 to npoints do point[n] := 1 - point[n];
  changed := true;
end;

function TSpectra.WriteSpectraAs(filename : string) : boolean;
begin
  OldFileName := filename;
  WriteSpectraAs := WriteSpectra;
end;

function TSpectra.WriteSpectra : boolean;
// Write spectra in the simple standard format.
var
  SpectraFile : TextFile;
  n : integer;
begin
  Analysewindow.ShowStatus(format('Saving spectra file %s...',[OldFileName]));
  if changed then begin
    assign(SpectraFile, OldFileName);
    rewrite(SpectraFile);
    writeln(SpectraFile,'Spectra Data File');
    writeln(SpectraFile,format('StartWavenumber %.8f',[RangeStart]));
    writeln(SpectraFile,format('EndWavenumber %.8f',[RangeEnd]));
    writeln(SpectraFile,format('Points %d',[npoints]));
    writeln(SpectraFile,'Intensity List:');
    for n := 1 to npoints do
      writeln(SpectraFile, format('%.4f',[point[n]]));
    closefile(SpectraFile);
  end;
  changed := false;
  WriteSpectra := true; // Success!
  Analysewindow.ShowStatus('');
end;

procedure TSpectra.Crop(wn1, wn2 : double);
// Leaves only the area of spectra between the two given wavenumbers remaining
// and removes everyting else. Useful for tidying up the spectra of unwanted regions
var
  StartPoint, EndPoint, n : integer;
  StartWn, EndWn : double;
begin
  if wn1<wn2 then
    begin StartWn := wn1; EndWn := wn2; end
  else
    begin StartWn := wn2; EndWn := wn1; end;
  // Find nearest points to start- and endwavenumbers
  StartPoint := trunc((StartWn - RangeStart)/Step)+1;
  EndPoint := trunc((EndWn - RangeStart)/Step)+1;
  // Make the _new_ start- and endwn:s equal to wn:s of those points.
  StartWn := (StartPoint-1) * Step + RangeStart;
  EndWn := (EndPoint-1) * Step + RangeStart;
  // Move the points
  npoints := EndPoint - StartPoint + 1; // number of remaining points
  for n := 1 to npoints do point[n] := point[n+StartPoint-1];
  // Calculate new step
  RangeStart := StartWn;
  RangeEnd := EndWn;
  Step := (RangeEnd - RangeStart) / (npoints-1);
  changed := true;
end;

procedure TSpectra.ShiftSpectra(DeltaWn : double);
begin
  RangeStart := RangeStart + DeltaWn;
  RangeEnd := RangeEnd + DeltaWn;
  changed := true;
end;

procedure TSpectra.CorrectBaseline(SampleStep : double);
{ The idea of this baseline correction algorithm:
  - Divide spectrum into ranges according to sample step
  - Find minimum of each range.
  - Make a linear interpolation function connecting the minimums.
  - Sunstract from all spectrum points the value of the interpolation function at that point
  - Re-normalize }
var
  n,m,j,PointStep : integer;
  MinPoint : array of real;
  correction : real;
begin
  // Determine how many spectrum points is one sample step
  PointStep := trunc(npoints / ((RangeEnd - RangeStart) / SampleStep));
  // Find minimum points in sampling ranges (=baseline)
  SetLength(MinPoint, npoints div PointStep + 2);
  m := 0; n := 1;
  while n <= npoints do begin
    MinPoint[m] := point[n];
    for j := 0 to PointStep - 1 do if n+j <= npoints then
      if point[n+j] < MinPoint[m] then MinPoint[m] := point[n+j];
    inc(n, PointStep); // Next group of points
    inc(m); // Next min range
  end;
  MinPoint[m] := MinPoint[m-1]; // Duplicate the last value
  // Adjust points according to the determined baseline
  m := 0; n := 1;
  while n <= npoints do begin
    for j := 0 to PointStep - 1 do if n+j <= npoints then begin
      correction := MinPoint[m] + (MinPoint[m+1] - MinPoint[m]) * (j/PointStep);
      point[n+j] := point[n+j] - correction;
    end;
    inc(n, PointStep); // Next group of points
    inc(m); // Next min range
  end;
  MinPoint := nil; // Free the array
  Normalise;
  changed := true;
end;

procedure TSpectra.ExportSpectra(Filename : string; First, Last, Step : double);
var
  wn : double;
  ExportFile : TextFile;
begin
  AssignFile(ExportFile, Filename);
  Rewrite(ExportFile);
  writeln(ExportFile, 'Wavenumber Intensity');
  wn := First;
  while wn <= Last do begin
    writeln(ExportFile, format('%.6f %.4f',[wn, value(wn)]));
    wn := wn + Step;
  end;
  CloseFile(ExportFile);
end;

end.
