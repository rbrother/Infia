unit ScrollUnit;

interface

uses
  Classes, SysUtils;

type
  TScrollThread = class(TThread)
  private
    { Private declarations }
    procedure ScrollSpectra;
  protected
    procedure Execute; override;
  public
    Direction : integer;
    horisontal : boolean;
  end;

var
  ScrollThread : TScrollThread;

implementation

uses SpecWin;

{ TScrollThread }

procedure TScrollThread.Execute;
begin
  // Infinite scroll loop
  repeat Synchronize(ScrollSpectra) until false;
end;

procedure TScrollThread.ScrollSpectra;
begin
  with SpectraWindow do
  if horisontal then Scroll(Direction)
  else begin
    VerticalZoom := VerticalZoom * (1.0 + Direction * 0.05);
    DrawSpectra(SpectraRect.Left, SpectraRect.Right);
  end;
end;

end.
