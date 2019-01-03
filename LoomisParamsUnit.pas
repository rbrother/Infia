unit LoomisParamsUnit;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TLoomisParamsDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    GetSizeButton: TButton;
    HeightEdit: TEdit;
    Label9: TLabel;
    WidthLabel: TLabel;
    WidthEdit: TEdit;
    GroupBox2: TGroupBox;
    IntensityEdit: TEdit;
    Label7: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    ShowAssignedBox: TCheckBox;
    procedure GetSizeButtonClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LoomisParamsDialog: TLoomisParamsDialog;

implementation

uses LoomisUnit;

{$R *.DFM}

procedure TLoomisParamsDialog.GetSizeButtonClick(Sender: TObject);
begin
  WidthEdit.text := inttostr(LoomisForm.ClientWidth);
  HeightEdit.text := inttostr(LoomisForm.ClientHeight);
end;

procedure TLoomisParamsDialog.OKBtnClick(Sender: TObject);
begin
  with LoomisForm do begin
    LoomisImage.Width := strtoint(WidthEdit.text);
    LoomisImage.Height := strtoint(HeightEdit.text);
  end;
  Close;
end;

procedure TLoomisParamsDialog.FormShow(Sender: TObject);
begin
  WidthEdit.text := inttostr(LoomisForm.LoomisImage.Width);
  HeightEdit.text := inttostr(LoomisForm.LoomisImage.Height);
end;

end.
