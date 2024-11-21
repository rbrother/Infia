unit ScriptUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TScriptForm = class(TForm)
    ScriptMemo: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScriptForm: TScriptForm;

procedure ClearDisplay;
procedure Display(s : string);
procedure ErrorMessage(ErrorText : string);

implementation

{$R *.DFM}

procedure ClearDisplay;
begin
  ScriptForm.ScriptMemo.Clear;
end;

procedure Display(s : string);
begin
  ScriptForm.ScriptMemo.Lines.Add(s);
end;

procedure ErrorMessage(ErrorText : string);
begin
  with ScriptForm.ScriptMemo.Lines do begin
    Add('');
    Add('ERROR');
    Add(ErrorText);
    Add('Operation aborted');
  end;
  ScriptForm.BringToFront;
  Beep;
  Abort;
end;

end.
