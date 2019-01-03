unit Analyse;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

type
  TSpectraWindow = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SpectraWindow: TSpectraWindow;

implementation

{$R *.DFM}

procedure TSpectraWindow.FormShow(Sender: TObject);
begin
  { Ladataan t‰ss‰ spektritiedosto ja laitetaan se muistiin }
end;

procedure TSpectraWindow.FormPaint(Sender: TObject);
begin
  { T‰ss‰ piirret‰‰n spektri n‰kyviin }
end;

end.
