{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}

unit Analwin;

interface

uses
  WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, SysUtils, ComCtrls, Registry, IniFiles,
  FileCtrl;

const
  RegistryKey = 'Infia';
  SpectraFormName = 'SpectraForm';
  AssignmentFormName = 'AssignmentForm';
  StateFormName = 'StateForm';
  PeakFormName = 'PeakForm';
  BandFormName = 'BandForm';
  LoomisFormName = 'LoomisForm';
  ScriptFormName = 'ScriptForm';
  LoomisImageName = 'LoomisImage';

type

  TAnalyseWindow = class(TForm)
    StatusPanel: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Options1: TMenuItem;
    SaveFileDialog: TSaveDialog;
    LoadProject1: TMenuItem;
    Window1: TMenuItem;
    View1: TMenuItem;
    LineMarkers1: TMenuItem;
    Help1: TMenuItem;
    SelectedBand1: TMenuItem;
    Band1: TMenuItem;
    About2: TMenuItem;
    AllBands1: TMenuItem;
    Predictionmarkers2: TMenuItem;
    ex: TMenuItem;
    Shiftmvalues1: TMenuItem;
    CombinaBranchesGlobal1: TMenuItem;
    HalfBand2: TMenuItem;
    TileAss1: TMenuItem;
    ProgressBar1: TProgressBar;
    SpectraFileConversion1: TMenuItem;
    Spectra1: TMenuItem;
    Croptoselected1: TMenuItem;
    Substractbaseline1: TMenuItem;
    HorisontalShift1: TMenuItem;
    Normalize1: TMenuItem;
    Invert1: TMenuItem;
    CalculateAll: TMenuItem;
    NewProject1: TMenuItem;
    Peaks1: TMenuItem;
    AddPeak1: TMenuItem;
    TileStateBand: TMenuItem;
    MergeOpposing1: TMenuItem;
    Duplicate1: TMenuItem;
    DeleteSelectedPeaks1: TMenuItem;
    StraightenLoomis1: TMenuItem;
    LoomisWood1: TMenuItem;
    CalculateUpper: TMenuItem;
    Export1: TMenuItem;
    SaveLoomisImage1: TMenuItem;
    Vibrational1: TMenuItem;
    StateAnalysis1: TMenuItem;
    SetParameters1: TMenuItem;
    ExportStates1: TMenuItem;
    SaveProject1: TMenuItem;
    AssignSpecLoomisBand1: TMenuItem;
    NextWindow1: TMenuItem;
    Recalcpeakintensities1: TMenuItem;
    Resonance1: TMenuItem;
    CalculateLowerFixed1: TMenuItem;
    PolynomePrediction1: TMenuItem;
    CombAssignSpec1: TMenuItem;
    AutomaticColors1: TMenuItem;
    Predictfromcalcparams1: TMenuItem;
    AcceptResults1: TMenuItem;
    TransferResults1: TMenuItem;
    ExportBands1: TMenuItem;
    ExportAssignments1: TMenuItem;
    procedure Options1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure LoadProject1Click(Sender: TObject);
    procedure LineMarkers1Click(Sender: TObject);
    procedure SelectedBand1Click(Sender: TObject);
    procedure CalculateAllClick(Sender: TObject);
    procedure AllBands1Click(Sender: TObject);
    procedure Predictionmarkers2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Print1Click(Sender: TObject);
    procedure Shiftmvalues1Click(Sender: TObject);
    procedure TileAss1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure SpectraFileConversion1Click(Sender: TObject);
    procedure Croptoselected1Click(Sender: TObject);
    procedure HorisontalShift1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Invert1Click(Sender: TObject);
    procedure NewProject1Click(Sender: TObject);
    procedure CombinaBranchesGlobal1Click(Sender: TObject);
    procedure Substractbaseline1Click(Sender: TObject);
    procedure AddPeak1Click(Sender: TObject);
    procedure MergeOpposing1Click(Sender: TObject);
    procedure Duplicate1Click(Sender: TObject);
    procedure DeleteSelectedPeaks1Click(Sender: TObject);
    procedure StraightenLoomis1Click(Sender: TObject);
    procedure CalculateUpperClick(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure Exportdata1Click(Sender: TObject);
    procedure SaveLoomisImage1Click(Sender: TObject);
    procedure StateAnalysis1Click(Sender: TObject);
    procedure Exportallbands1Click(Sender: TObject);
    procedure SetParameters1Click(Sender: TObject);
    procedure exClick(Sender: TObject);
    procedure ExportStates1Click(Sender: TObject);
    procedure SaveProject1Click(Sender: TObject);
    procedure TileStateBandClick(Sender: TObject);
    procedure AssignSpecLoomisBand1Click(Sender: TObject);
    procedure NextWindow1Click(Sender: TObject);
    procedure Recalcpeakintensities1Click(Sender: TObject);
    procedure CalculateLowerFixed1Click(Sender: TObject);
    procedure PolynomePrediction1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CombAssignSpec1Click(Sender: TObject);
    procedure AutomaticColors1Click(Sender: TObject);
    procedure Predictfromcalcparams1Click(Sender: TObject);
    procedure AcceptResults1Click(Sender: TObject);
    procedure Normalize1Click(Sender: TObject);
    procedure TransferResults1Click(Sender: TObject);
    procedure ExportBands1Click(Sender: TObject);
    procedure ExportAssignments1Click(Sender: TObject);
  private
    { Private declarations }
  public
    ProjectFileName : string; // Includes the path
    procedure RefreshStatusInfo;
    procedure LoadProject(filename : string);
    procedure SaveProject;
    procedure LoadPeakFile(filename : string);
    procedure LoadMoleculeFile(filename : string);
    procedure LoadStateFile(filename : string);
    procedure LoadResonanceFile(filename : string);
    procedure LoadBandFile(filename : string);
    procedure LoadAssignmentFile(filename : string);
    function AskFileName(Title, Filter, DefaultExt : string) : string;
    procedure ShowStatus(statusmessage : string);
  end;

var
  AnalyseWindow: TAnalyseWindow;
  MyRegistry : TRegistry;
  ProjFile : TIniFile;

implementation

uses Specwin, Spectra, Options, About,
  BandUnit, AssignmentUnit, StateUnit, PeakUnit, ScrollUnit,
  MoleculeUnit, LoomisUnit, LinAlg, ScriptUnit,
  Vibrational, ResonanceUnit, InfiaLogoUnit, math, MathUtils,
  nonlinearoptimisation, utils, BranchCombinationUnit, AdvancedList, debug;

{$R *.DFM}

procedure TAnalyseWindow.FormCreate(Sender: TObject);
begin
  ShowStatus('Select from File menu "New Project" or "Load Project"');
  ProjectFileName := '';
  // Initalize registry
  MyRegistry := TRegistry.Create;
  if not MyRegistry.OpenKey('Software',false) then ErrorMessage('TAnalyseWindow.FormCreate: Can not find open registry key SOFTWARE');
  with MyRegistry do if not KeyExists(RegistryKey) then begin
    ShowMessage(format('It appears that the software has not been run on this '+
     'machine previously. Registry key %s will be created for program '+
     'configuration storage.',[RegistryKey]));
    OpenKey(RegistryKey, true); // Creates the key
    WriteString('DefaultDirectory','c:\');
    end
  else
  OpenKey(RegistryKey, false);
  // Initialise scrolling thread
  ScrollThread := TScrollThread.create(true);  // Start Suspended
end;

procedure TAnalyseWindow.ShowStatus(statusmessage : string);
begin
  StatusPanel.Caption := statusmessage;
  StatusPanel.Refresh;
end;

function TAnalyseWindow.AskFileName(Title, Filter, DefaultExt : string) : string;
// Returns empty string, if cancel pressed
var
  OK_Pressed : boolean;
  OpenDialog : TOpenDialog;
  newdir : string;
begin
  OpenDialog := TOpenDialog.Create(NIL);
  OpenDialog.Title := title;
  OpenDialog.Filter := Filter;
  OpenDialog.DefaultExt := DefaultExt;
  OpenDialog.FileName := '';
  newdir := MyRegistry.ReadString('DefaultDirectory');
  if not SysUtils.DirectoryExists(newdir) then newdir := '';
  OpenDialog.InitialDir := newdir;
  OK_Pressed := OpenDialog.Execute; // return the return value of the dialog
  if OK_Pressed then begin
    AskFileName := OpenDialog.FileName;
    newdir := ExtractFileDir(OpenDialog.FileName);
    MyRegistry.WriteString('DefaultDirectory',newdir);
    ChDir(newdir);
   end else
    AskFileName := '';
  OpenDialog.Free;
end;

procedure TAnalyseWindow.RefreshStatusInfo;
var
  StatusString : string[120];
begin
  StatusString := '';
  if SpectraWindow.SpectrumLoaded then begin
    StatusString := format('Spectrum range %.4f - %.4f cm-1. Bands %d. Assignments %d. ',
      [MySpectra.RangeStart, MySpectra.RangeEnd, Bands.Count, AllAss.count]);
    StatusString := StatusString + 'Peaks ' + inttostr(Peaks.Count);
  end;
  ShowStatus(StatusString);
end;

procedure TAnalyseWindow.Options1Click(Sender: TObject);
var
  optmethod : string;
begin
  with OptionDialog do begin
    // Set up initial values in dialog items
    MErrorEdit.Text := inttostr(ProjFile.ReadInteger('Settings','MaxMError', 10));
    PeakErrorEdit.text := ProjFile.ReadString('Settings','PeakError', '0.0001');
    DiscardEdit.text := inttostr(ProjFile.ReadInteger('Settings','DiscardPercentage', 0));
    with LabelTypeCombo do case LabelType of
      MLabel    : ItemIndex := 0;
      JLabel    : ItemIndex := 1;
      NameLabel : ItemIndex := 2;
      NoLabel   : ItemIndex := 3;
    end;
    optmethod := ProjFile.ReadString('Settings','OptimizationMethod', NewtonMethod);
    with OptMethodBox do
    if optmethod = SimplexMethod then itemIndex := 0
    else if optmethod = NewtonMethod then itemIndex := 1
    else display('TAnalyseWindow.Options1Click: Warning: unknown optimization method');
    PredRangeEdit.Text := inttostr(ProjFile.ReadInteger('Settings','PredRange', 10));
    // Show the Dialog and read data on ok
    if ShowModal = mrOK then begin
      ProjFile.WriteString('Settings','MaxMError', MErrorEdit.Text);
      ProjFile.WriteString('Settings', 'PeakError', PeakErrorEdit.text);
      ProjFile.WriteString('Settings','DiscardPercentage', DiscardEdit.Text);
      PeakError := strtofloatdef(ProjFile.ReadString('Settings','PeakError', '0.0001'));
      with SpectraWindow do case LabelTypeCombo.ItemIndex of
        0 : LabelType := MLabel;
        1 : LabelType := JLabel;
        2 : LabelType := NameLabel;
        3 : LabelType := NoLabel;
      end;
      case OptMethodBox.ItemIndex of
        0 : optmethod := SimplexMethod;
        1 : optmethod := NewtonMethod;
      end;
      ProjFile.WriteString('Settings','OptimizationMethod', optmethod);
      ProjFile.WriteInteger('Settings','LabelMode',integer(LabelType));
      ProjFile.WriteInteger('Settings','PredRange',strtointdef(PredRangeEdit.Text, 10));
    end;
  end;
  SpectraWindow.Invalidate;
end;

procedure TAnalyseWindow.About1Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

//****************     F I L E    L O A D I N G     ******************

procedure TAnalyseWindow.LoadProject1Click(Sender: TObject);
begin
  ProjectFileName := AskFileName('Load Project file', 'Project PRJ files|*.PRJ', 'PRJ');
  If ProjectFileName = '' then exit;
  LoadProject(ProjectFileName);
end;

procedure TAnalyseWindow.LoadProject(filename : string);
begin
  ProjFile.Free;
  ProjFile := TIniFile.Create(FileName);
  with ProjFile do begin
    // Window positions and sizes
    SpectraWindow.BoundsRect := readboundsrect(ProjFile, SpectraFormName);
    AssignmentForm.BoundsRect := readboundsrect(ProjFile, AssignmentFormName);
    PeakForm.BoundsRect := readboundsrect(ProjFile, PeakFormName);
    BandForm.BoundsRect := readboundsrect(ProjFile, BandFormName);
    StateForm.BoundsRect := readboundsrect(ProjFile, StateFormName);
    LoomisForm.BoundsRect := readboundsrect(ProjFile, LoomisFormName);
    ScriptForm.FormStyle := fsMDIChild;
    ScriptForm.BoundsRect := readboundsrect(ProjFile, ScriptFormName);
    LoomisForm.LoomisImage.Width := ReadInteger(LoomisImageName,'Width', 700);
    LoomisForm.LoomisImage.Height := ReadInteger(LoomisImageName,'Height', 500);
    // Load Data files
    MySpectra.Free;
    MySpectra := TSpectra.CreateFromFile(ReadString('Filenames','SpectraFile',''));
    LoadPeakFile(ExtractFileName(ReadString('Filenames','PeakFile','')));
    LoadMoleculeFile(ExtractFileName(ReadString('Filenames','MoleculeFile','')));
    LoadStateFile(ExtractFileName(ReadString('Filenames','StateFile','')));
    LoadResonanceFile(ExtractFileName(ReadString('Filenames','ResonanceFile','')));
    LoadBandFile(ExtractFileName(ReadString('Filenames','BandFile','')));
    LoadAssignmentFile(ExtractFileName(ReadString('Filenames','AssignmentFile','')));
    // Set the active band
    Bands.FindBand(ReadString('Settings','Band','')).SetActive;
    BandForm.BandDrawGrid.Row := Bands.IndexOf(ActiveBand) + 1;
    ActiveBand.Assignments.ConnectDrawGrid(AssignmentForm.AssDrawGrid);
    // Settings
    SpectraWindow.ViewStart := strtofloatdef(ReadString('Settings','ViewStart','0'));
    SpectraWindow.ViewEnd := strtofloatdef(ReadString('Settings','ViewEnd','1000'));
    with SpectraWindow do If ViewEnd <= ViewStart then begin
      ViewStart := MySpectra.RangeStart;
      ViewEnd := MySpectra.RangeEnd;
    end;
    LabelType := TLabelType(ReadInteger('Settings','LabelMode', 0));
    PeakError := strtofloatdef(ReadString('Settings','PeakError', '0.0001'));
  end;
  caption := 'INFIA Analysis Project ' + ProjectFileName;
  SpectraWindow.SpectrumLoaded := true;
  RefreshStatusInfo;
  SpectraWindow.BringToFront;
  AssignmentForm.BringToFront;
  BandForm.BringToFront;
end;

procedure TAnalyseWindow.LoadPeakFile(filename : string);
begin
  PeakForm.PeakDrawGrid.RowCount := 2;
  Peaks.Free;
  Peaks := TPeaks.Create;
  if FileExists(filename) then Peaks.LoadFromTextFile(filename);
  Peaks.ConnectDrawGrid(PeakForm.PeakDrawGrid);
end;

procedure TAnalyseWindow.LoadMoleculeFile(filename : string);
begin
  MoleculeForm.MolDrawGrid.RowCount := 2;
  Molecules.Free;
  Molecules := TMolecules.Create;
  if FileExists(filename) then Molecules.LoadFromTextFile(filename);
  Molecules.ConnectDrawGrid(MoleculeForm.MolDrawGrid);
end;

procedure TAnalyseWindow.LoadStateFile(filename : string);
begin
  StateForm.StateDrawGrid.RowCount := 2;
  States.Free;
  States := TStates.Create;
  if FileExists(filename) then States.LoadFromTextFile(filename);
  States.ConnectDrawGrid(StateForm.StateDrawGrid);
end;

procedure TAnalyseWindow.LoadResonanceFile(filename : string);
begin
  ResForm.ResDrawGrid.RowCount := 2;
  AllResonances.Free;
  AllResonances := TResonances.Create;
  if FileExists(filename) then AllResonances.LoadFromTextFile(filename);
  AllResonances.ConnectDrawGrid(ResForm.ResDrawGrid);
end;

procedure TAnalyseWindow.LoadBandFile(filename : string);
begin
  BandForm.BandDrawGrid.Rowcount := 2;
  Bands.Free;
  Bands := TBands.Create;
  if FileExists(filename) then Bands.LoadFromTextFile(filename);
  Bands.ConnectDrawGrid(BandForm.BandDrawGrid);
  States.ConnectToSourceBands;
end;

procedure TAnalyseWindow.LoadAssignmentFile(filename : string);
begin
  ShowStatus('Loading assignments...');
  if FileExists(filename) then AllAss.LoadFromTextFile(filename);
  ShowStatus('');
end;

//*****************     F I L E   S A V I N G    *******************

procedure TAnalyseWindow.SaveProject1Click(Sender: TObject);
begin
  // Save Data files (except spectra file, which presumably has not changed)
  SaveProject;
  ShowMessage('Molecules, States, Bands, Assignments, Peaks and Spectrum have been saved.');
end;

procedure TAnalyseWindow.SaveProject;
begin
  showstatus('Saving project data files...');
  Molecules.SaveToTextFile(ExtractFileName(ProjFile.ReadString('Filenames','MoleculeFile','')));
  States.SaveToTextFile(ExtractFileName(ProjFile.ReadString('Filenames','StateFile','')));
  Bands.SaveToTextFile(ExtractFileName(ProjFile.ReadString('Filenames','BandFile','')));
  AllAss.SaveToTextFile(ExtractFileName(ProjFile.ReadString('Filenames','AssignmentFile','')));
  Peaks.SaveToTextFile(ExtractFileName(ProjFile.ReadString('Filenames','PeakFile','')));
  AllResonances.SaveToTextFile(ExtractFileName(ProjFile.ReadString('Filenames','ResonanceFile','')));
  MySpectra.WriteSpectra;
  RefreshStatusInfo;
end;

//*****************  C A L C U L A T I O N S   ****************

procedure TAnalyseWindow.CalculateAllClick(Sender: TObject);
begin
  AssignmentForm.Freeze(true);
  ActiveBand.CalculateParameters(LowerUpperCalc);
  AssignmentForm.Freeze(false);
  BandForm.BandDrawGrid.Invalidate;
  SpectraWindow.Invalidate;
end;

procedure TAnalyseWindow.SelectedBand1Click(Sender: TObject);
begin
  with SpectraWindow do begin
    DrawSelBandLabels := not DrawSelBandLabels;
    SelectedBand1.Checked := DrawSelBandLabels;
    invalidate;
  end;
end;

procedure TAnalyseWindow.AllBands1Click(Sender: TObject);
begin
  with SpectraWindow do begin
    DrawAllBandLabels := not DrawAllBandLabels;
    AllBands1.Checked := DrawAllBandLabels;
    invalidate;
  end;
end;

procedure TAnalyseWindow.Predictionmarkers2Click(Sender: TObject);
begin
  with SpectraWindow do begin
    DrawPredMarkers := not DrawPredMarkers;
    PredictionMarkers2.Checked := DrawPredMarkers;
    invalidate;
  end;
end;

procedure TAnalyseWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ProjFile <> NIL then with ProjFile do begin
    WriteString('Settings','ViewStart',format('%g',[SpectraWindow.ViewStart]));
    WriteString('Settings','ViewEnd',format('%g',[SpectraWindow.ViewEnd]));
    if ActiveBand <> NIL then WriteString('Settings','Band',ActiveBand.Name);
    // Normalise window sizes
    SpectraWindow.WindowState := wsNormal;
    AssignmentForm.WindowState := wsNormal;
    StateForm.WindowState := wsNormal;
    PeakForm.WindowState := wsNormal;
    BandForm.WindowState := wsNormal;
    LoomisForm.WindowState := wsNormal;
    // Write window sizes to project file
    WriteBoundsRect(ProjFile, SpectraFormName, SpectraWindow.BoundsRect);
    WriteBoundsRect(ProjFile, AssignmentFormName, AssignmentForm.BoundsRect);
    WriteBoundsRect(ProjFile, StateFormName, StateForm.BoundsRect);
    WriteBoundsRect(ProjFile, PeakFormName, PeakForm.BoundsRect);
    WriteBoundsRect(ProjFile, BandFormName, BandForm.BoundsRect);
    WriteBoundsRect(ProjFile, LoomisFormName, LoomisForm.BoundsRect);
    WriteBoundsRect(ProjFile, ScriptFormName, ScriptForm.BoundsRect);
    WriteInteger(LoomisImageName,'Width', LoomisForm.LoomisImage.Width);
    WriteInteger(LoomisImageName,'Height', LoomisForm.LoomisImage.Height);
  end;
  MyRegistry.Free;
  ProjFile.Free;
  MySpectra.Free;
end;

procedure TAnalyseWindow.Print1Click(Sender: TObject);
begin
  Print;
end;

procedure TAnalyseWindow.Shiftmvalues1Click(Sender: TObject);
var
  result : string;
begin
  if InputQuery('Shift band m-values','Give the integer shift to band m-values',result) then begin
    ActiveBand.Assignments.ShiftM(strtoint(result));
    SpectraWindow.Invalidate;
    AssignmentForm.AssDrawGrid.Invalidate;
  end;
end;

// *******************  T I L E   W I N D O W S   ********************

procedure TAnalyseWindow.TileAss1Click(Sender: TObject);
var
  HalfWidth, Height : integer;
begin
  HalfWidth := (ClientWidth - 20) div 2;
  Height := ClientHeight - 30;
  // Tile only the detailtable and spectrawindow. Good for small screens.
  SpectraWindow.SetBounds(0,0,HalfWidth,Height);
  SpectraWindow.BringToFront;
  AssignmentForm.SetBounds(HalfWidth, 0, HalfWidth,Height);
  AssignmentForm.BringToFront;
end;

procedure TAnalyseWindow.AssignSpecLoomisBand1Click(Sender: TObject);
var
  HalfWidth, HalfHeight : integer;
begin
  HalfWidth := (ClientWidth - 20) div 2;
  HalfHeight := (ClientHeight - 30) div 2;
  SpectraWindow.SetBounds(0,HalfHeight,HalfWidth,HalfHeight);
  SpectraWindow.BringToFront;
  LoomisForm.SetBounds(HalfWidth, HalfHeight, HalfWidth,HalfHeight);
  LoomisForm.BringToFront;
  BandForm.SetBounds(HalfWidth,0,HalfWidth,HalfHeight);
  BandForm.BringToFront;
  AssignmentForm.SetBounds(0,0,HalfWidth,HalfHeight);
  AssignmentForm.BringToFront;
end;

procedure TAnalyseWindow.TileStateBandClick(Sender: TObject);
var
  Width, HalfHeight : integer;
begin
  Width := ClientWidth - 20;
  HalfHeight := (ClientHeight - 30) div 2;
  BandForm.SetBounds(0,0,Width,HalfHeight);
  BandForm.BringToFront;
  StateForm.SetBounds(0,HalfHeight,Width,HalfHeight);
  StateForm.BringToFront;
end;

procedure TAnalyseWindow.CombAssignSpec1Click(Sender: TObject);
var
  Height, HalfWidth, HalfHeight : integer;
begin
  HalfWidth := (ClientWidth - 20) div 2;
  Height := ClientHeight - 30;
  HalfHeight := Height div 2;
  CombForm.SetBounds(0,0,HalfWidth,HalfHeight);
  CombForm.BringToFront;
  AssignmentForm.SetBounds(HalfWidth,0,HalfWidth,Height);
  AssignmentForm.BringToFront;
  SpectraWindow.SetBounds(0,HalfHeight,HalfWidth,HalfHeight);
  SpectraWindow.BringToFront;
end;

// ****************************************************************

procedure TAnalyseWindow.Exit1Click(Sender: TObject);
begin
  close;
end;

procedure TAnalyseWindow.SpectraFileConversion1Click(Sender: TObject);
var
  filename,ext : string;
begin
  filename := AskFileName('Spectrum file conversion', 'Spectrum DIV files|*.DIV|DAT text file format|*.DAT', '');
    if filename = '' then exit;
  // Create a spectra for conversion purposes
  MySpectra.Free;
  MySpectra := TSpectra.Create;
  ext := uppercase(ExtractFileExt(filename)); // Get the extension
  ShowStatus('Loading the spectrum for conversion...');
  if ext = '.DIV' then MySpectra.ReadDIVSpectra(filename)
  else if ext = '.DAT' then MySpectra.ReadDATSpectra(filename)
  else errormessage('Unknown spectra file extension ' + ext);
  // Save spectra with same filename but different extension (SPE)
  ShowStatus('Writing the spectrum in SPE format...');
  filename := ChangeFileExt(filename, '.SPE');
  MySpectra.WriteSpectraAs(filename);
  ShowStatus('');
  ShowMessage('Spectrum successfully converted and saved to file ' + filename);
end;

procedure TAnalyseWindow.Croptoselected1Click(Sender: TObject);
begin
  with SpectraWindow do if RangeSelected then begin
    MySpectra.Crop(WindowXtoWave(SelectionStart), WindowXtoWave(SelectionEnd));
    ViewStart := MySpectra.RangeStart;
    ViewEnd := MySpectra.RangeEnd;
    RangeSelected := false;
    Invalidate;
    end
  else begin
    ShowMessage('You must select a range of spectra with mouse before cropping');
  end;
end;

procedure TAnalyseWindow.HorisontalShift1Click(Sender: TObject);
var
  result : string;
begin
  if InputQuery('Spectrum contour horisontal shift',
        'Give amount of shift (cm-1)', result) then begin
     MySpectra.ShiftSpectra(strtofloatdef(result));
     SpectraWindow.invalidate;
  end;
end;

procedure TAnalyseWindow.Invert1Click(Sender: TObject);
begin
  MySpectra.Invert;
  SpectraWindow.Invalidate;
end;

procedure TAnalyseWindow.NewProject1Click(Sender: TObject);
var
  ProjNameBase, SpectraFileName, ResFileName : string;
  PeakFileName, StateFileName, MolFileName : string;
begin
  ProjectFileName := AskFileName('New analysis project file name and location','Project PRJ files|*.PRJ', 'PRJ');
    if ProjectFileName = '' then exit;
  ProjFile := TIniFile.Create(ProjectFileName);
  // Ask project file names
  SpectraFileName := ExtractFileName(AskFileName('Spectrum file to analyse (required)','Spectrum SPE files|*.SPE', 'SPE'));
    if SpectraFileName = '' then exit;
    SpectraFileName := ExtractFileName(SpectraFileName);
  PeakFileName := ExtractFileName(AskFileName('Peak file to analyse (required)','Peak CSV files|*.CSV', 'CSV'));
    if PeakFileName = '' then exit;
    PeakFileName := ExtractFileName(PeakFileName);
  MolFileName := ExtractFileName(askfilename('Molecule file for the project (existing or new)','Molecule CSV files|*.CSV', 'CSV'));
    if MolFileName = '' then exit;
    MolFileName := ExtractFileName(MolFileName);
  StateFileName := askfilename('State file for the project (existing or new)','State CSV files|*.CSV', 'CSV');
    if StateFileName = '' then exit;
    StateFileName := ExtractFileName(StateFileName);
  ResFileName := askfilename('Resonance file for the project (existing or new)','Resonancne CSV files|*.CSV', 'CSV');
    if ResFileName = '' then exit;
    ResFileName := ExtractFileName(ResFileName);
  // Write file names into project file
  ProjNameBase := ChangeFileExt(ExtractFileName(ProjectFileName),'');
  with ProjFile do begin
    WriteString('Filenames','SpectraFile',SpectraFileName);
    WriteString('Filenames','PeakFile',PeakFileName);
    WriteString('Filenames','MoleculeFile',MolFileName);
    WriteString('Filenames','StateFile',StateFileName);
    WriteString('Filenames','ResonanceFile',ResFileName);
    WriteString('Filenames','BandFile',ProjNameBase + '_bands.csv');
    WriteString('Filenames','AssignmentFile',ProjNameBase+'_assignments.csv');
  end;
  LoadProject(ProjectFileName); // Start the project by loading it!
  SpectraWindow.UnzoomAllClick(NIL);
end;

procedure TAnalyseWindow.CombinaBranchesGlobal1Click(Sender: TObject);
begin
  ActiveBand.CombineHalfBands;
  RefreshStatusInfo;
end;

procedure TAnalyseWindow.Substractbaseline1Click(Sender: TObject);
var
  result : string;
begin
  if InputQuery('Baseline correction', 'Baseline sample interval (cm-1) (hint: 100 times peak width is good)', result) then begin
    MySpectra.CorrectBaseline(strtofloatdef(result));
    RefreshStatusInfo;
    SpectraWindow.Invalidate;
  end;
end;

procedure TAnalyseWindow.AddPeak1Click(Sender: TObject);
var
  wn : double;
begin
  if not SpectraWindow.RangeSelected then begin
    ShowMessage('To add a peak, first click to the desired spectra position with mouse.');
    exit;
  end;
  with SpectraWindow do wn :=
    0.5 * (WindowXtoWave(SelectionStart) + WindowXtoWave(SelectionEnd));
  Peaks.AddPeak(wn);
  Spectrawindow.Invalidate;
end;

procedure TAnalyseWindow.MergeOpposing1Click(Sender: TObject);
var
  DialogResult : word;
begin
  DialogResult := MessageDlg('Are you sure you want to change opposing predictions to assignments?',mtConfirmation,[mbOK, mbCancel],0);
  if DialogResult = 1 {OK} then ActiveBand.MergeToPR;
  SpectraWindow.Invalidate;
  AssignmentForm.AssDrawGrid.Invalidate;
  BandForm.BandDrawGrid.Invalidate;
end;

procedure TAnalyseWindow.Duplicate1Click(Sender: TObject);
begin
  Bands.AddCopyOf(ActiveBand);
  BandForm.BandDrawGrid.Invalidate;
end;

procedure TAnalyseWindow.DeleteSelectedPeaks1Click(Sender: TObject);
var
  wn1, wn2 : double;
begin
  with SpectraWindow do begin
    if not RangeSelected then ShowMessage('To delete a range of peaks, first select the desired spectra range with mouse.')
    else if MessageDlg('Are you sure you want to delete permananetly all spectra peaks from selected area?',mtConfirmation, mbOkCancel, 0) = mrOK then begin
      wn1 := WindowXToWave(SelectionStart);
      wn2 := WindowXToWave(SelectionEnd);
      Peaks.DeleteRange(min(wn1,wn2), max(wn1,wn2));
      SpectraWindow.RangeSelected := false;
      SpectraWindow.Invalidate;
    end;
  end;
end;

procedure TAnalyseWindow.StraightenLoomis1Click(Sender: TObject);
begin
  LoomisForm.StraightenBand;
  LoomisForm.DrawPlot;
end;

procedure TAnalyseWindow.CalculateUpperClick(Sender: TObject);
begin
  ShowStatus(format('Calculating band %s parameters...',[ActiveBand.Name]));
  AssignmentForm.Freeze(true);
  ActiveBand.CalculateParameters(UpperCalc);
  AssignmentForm.Freeze(false);
  BandForm.BandDrawGrid.Invalidate;
  SpectraWindow.Invalidate;
  RefreshStatusInfo;
end;

procedure TAnalyseWindow.Export1Click(Sender: TObject);
var
  Filename, Samplestep : string;
begin
  if not InputQuery('Export visible spectra region',
    'Give name for the export file', Filename) then exit;
  if not InputQuery('Export visible spectra region',
    format('Give resampling step (current %.6f)',
    [MySpectra.Step]), Samplestep) then exit;
  MySpectra.ExportSpectra(Filename, SpectraWindow.ViewStart,
    SpectraWindow.ViewEnd, strtofloatdef(SampleStep));
end;

procedure TAnalyseWindow.Exportdata1Click(Sender: TObject);
begin
  ScriptForm.BringToFront;
  //Ass.ExportData;
end;

procedure TAnalyseWindow.SaveLoomisImage1Click(Sender: TObject);
begin
  LoomisForm.SaveImage;
end;

procedure TAnalyseWindow.StateAnalysis1Click(Sender: TObject);
var
  result : string;
begin
  if not InputQuery('List vibrational states',
    'Give maximum state energy to display (cm-1)', result) then exit;
  ListVibStates(strtofloatdef(result));
end;

procedure TAnalyseWindow.Exportallbands1Click(Sender: TObject);
begin
  ScriptForm.BringToFront;
  Bands.ExportData;
end;

procedure TAnalyseWindow.SetParameters1Click(Sender: TObject);
begin
  LoomisForm.BringToFront;
  LoomisForm.SetParameters;
end;

procedure TAnalyseWindow.ExportStates1Click(Sender: TObject);
begin
  States.ExportAsText;
end;

procedure TAnalyseWindow.NextWindow1Click(Sender: TObject);
begin
  if BandForm.Active then AssignmentForm.BringToFront
  else if AssignmentForm.Active then SpectraWindow.BringToFront
  else BandForm.BringToFront;
end;

procedure TAnalyseWindow.Recalcpeakintensities1Click(Sender: TObject);
begin
  Peaks.RecalculateIntensities;
end;

procedure TAnalyseWindow.CalculateLowerFixed1Click(Sender: TObject);
begin
  ResonanceFit.Free;
  ResonanceFit := TResonanceFit.Create(ActiveBand);
  ResonanceFit.Calculate;
  AssignmentForm.AssDrawGrid.Invalidate;
  BandForm.BandDrawGrid.Invalidate;
  SpectraWindow.Invalidate;
  RefreshStatusInfo;
end;

procedure TAnalyseWindow.LineMarkers1Click(Sender: TObject);
begin
  with SpectraWindow do begin
    DrawLineMarkers := not DrawLineMarkers;
    LineMarkers1.Checked := DrawLineMarkers;
    invalidate;
  end;
end;

procedure TAnalyseWindow.PolynomePrediction1Click(Sender: TObject);
begin
  AssignmentForm.Freeze(true);
  ActiveBand.PredictPolynome;
  AssignmentForm.Freeze(false);
  BandForm.BandDrawGrid.Invalidate;
  SpectraWindow.Invalidate;
end;

procedure TAnalyseWindow.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  result : word;
begin
  CanClose := true;
  result := MessageDlg(format('Do you want to save the project %s before exiting?',[ProjectFileName]),mtConfirmation,[mbYes,mbNo,mbCancel],0);
  if result = mrCancel then CanClose := false
  else if result = mrYes then SaveProject;
end;

procedure TAnalyseWindow.AutomaticColors1Click(Sender: TObject);
begin
  Bands.AutomaticColors;
end;

procedure TAnalyseWindow.Predictfromcalcparams1Click(Sender: TObject);
begin
  ShowStatus('Predicting peaks using state table parameters...');
  ActiveBand.PredictFromParams(false); // Predict using old parameters
  RefreshStatusInfo;
  SpectraWindow.Invalidate;
  AssignmentForm.AssDrawGrid.Invalidate;
end;

procedure TAnalyseWindow.AcceptResults1Click(Sender: TObject);
begin
  if MessageDlg('Do you want to update the state table with the parameters of the recent calculation?',
    mtConfirmation,[mbYes,mbCancel],0) = mrYes then
  begin
    ActiveBand.TempToOriginal;
  end;
end;

procedure TAnalyseWindow.Normalize1Click(Sender: TObject);
begin
  MySpectra.Normalise;
  SpectraWindow.Invalidate;
end;

procedure TAnalyseWindow.TransferResults1Click(Sender: TObject);
begin
  if MessageDlg('Do you want to update the state table with the parameters of the recent calculation?',
    mtConfirmation,[mbYes,mbCancel],0) = mrYes then
  ResonanceFit.TempToOriginal;
end;

procedure TAnalyseWindow.ExportBands1Click(Sender: TObject);
begin
  Bands.ExportData;
end;

procedure TAnalyseWindow.ExportAssignments1Click(Sender: TObject);
begin
  ActiveBand.ExportAssigmentsAsText;
end;

//-------------------- DEBUGGING

procedure TAnalyseWindow.exClick(Sender: TObject);
// Testing routines started with debug command.
// The debug routines are located in "debug" unit
begin
  // Do not remove this comment line
  TestMatrix;
  TestNonlinearFit; // Test this - not working...
end;

END.

