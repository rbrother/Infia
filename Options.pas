unit Options;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls;

type
  TOptionDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    CalcGroup: TGroupBox;
    DisplayGroup: TGroupBox;
    Label6: TLabel;
    MErrorEdit: TEdit;
    Label4: TLabel;
    LabelTypeCombo: TComboBox;
    Label10: TLabel;
    PredRangeEdit: TEdit;
    Label5: TLabel;
    PeakErrorEdit: TEdit;
    Label1: TLabel;
    DiscardEdit: TEdit;
    GroupBox1: TGroupBox;
    OptMethodBox: TComboBox;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionDialog: TOptionDialog;

implementation

{$R *.DFM}

end.
