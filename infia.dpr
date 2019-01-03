program infia;

uses
  Forms,
  Specwin in 'SPECWIN.PAS' {SpectraWindow},
  StateUnit in 'StateUnit.pas',
  Analwin in 'ANALWIN.PAS' {AnalyseWindow},
  Options in 'OPTIONS.PAS',
  About in 'ABOUT.PAS' {AboutBox},
  spectra in 'spectra.pas',
  AssignmentUnit in 'AssignmentUnit.pas' {AssignmentForm},
  PeakUnit in 'PeakUnit.pas' {PeakForm},
  BandUnit in 'BandUnit.pas' {BandForm},
  ScrollUnit in 'ScrollUnit.pas',
  MoleculeUnit in 'MoleculeUnit.pas' {MoleculeForm},
  LoomisUnit in 'LoomisUnit.pas' {LoomisForm},
  Vibrational in 'Vibrational.pas',
  LoomisParamsUnit in 'LoomisParamsUnit.pas' {LoomisParamsDialog},
  ResonanceUnit in 'ResonanceUnit.pas' {ResForm},
  InfiaLogoUnit in 'InfiaLogoUnit.pas' {InfiaDialog},
  BranchCombinationUnit in 'BranchCombinationUnit.pas' {CombForm},
  debug in 'debug.pas',
  AdvancedList in '..\Delphi Components\AdvancedList.pas',
  Utils in '..\Delphi Components\Utils.pas',
  ScriptUnit in '..\Delphi Components\ScriptUnit.pas' {ScriptForm},
  MathUtils in '..\Delphi Components\MathUtils.pas',
  linalg in '..\Delphi Components\LINALG.PAS',
  imslmath in '..\Delphi Components\IMSLMATH.PAS',
  ParameterUnit in '..\Delphi Components\ParameterUnit.pas',
  FormulaUnit in '..\Delphi Components\FormulaUnit.pas',
  NonlinearOptimisation in '..\Delphi Components\NonlinearOptimisation.pas',
  resample in '..\Delphi Components\RESAMPLE.PAS',
  simplex in '..\Delphi Components\SIMPLEX.PAS';

{$R *.RES}

begin
  Application.Title := 'INFIA spectrum analyser';
  // Main application window
  Application.CreateForm(TAnalyseWindow, AnalyseWindow);
  Application.CreateForm(TInfiaDialog, InfiaDialog);
  Application.CreateForm(TCombForm, CombForm);
  Application.CreateForm(TScriptForm, ScriptForm);
  InfiaDialog.Show;
  InfiaDialog.Refresh;
  // Construct the rest of the windows
  Application.CreateForm(TScriptForm, ScriptForm);
  Application.CreateForm(TLoomisForm, LoomisForm);
  Application.CreateForm(TSpectraWindow, SpectraWindow);
  Application.CreateForm(TAssignmentForm, AssignmentForm);
  Application.CreateForm(TStateForm, StateForm);
  Application.CreateForm(TResForm, ResForm);
  Application.CreateForm(TBandForm, BandForm);
  Application.CreateForm(TPeakForm, PeakForm);
  Application.CreateForm(TMoleculeForm, MoleculeForm);
  Application.CreateForm(TLoomisParamsDialog, LoomisParamsDialog);
  Application.CreateForm(TAboutBox, AboutBox);
  OptionDialog := TOptionDialog.Create(NIL);
  BandForm.BringToFront;
  InfiaDialog.Close; // Close the logo
  Application.Run;
end.
