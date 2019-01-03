unit Vibrational;

interface

procedure ListVibStates(MaxEnergy : double);

implementation

uses ScriptUnit, SysUtils;

procedure ListVibStates(MaxEnergy : double);
const
  Vibrations = 5;
  MaxQuantas = 5;
var
  Quantas : array[1..Vibrations] of integer;
  VibEnergy : array[1..Vibrations] of double;
  TotalEnergy : double;
  n, Current : integer;
  Complete, SwitchCurrent : boolean;
begin
  ScriptForm.ScriptMemo.Clear;
  Display(format('Rough list of vibrational states with energy < %.1f',[MaxEnergy]));
  Display('State   Energy (cm-1)');
  VibEnergy[1] := 2600;
  VibEnergy[2] := 1950;
  VibEnergy[3] := 606;
  VibEnergy[4] := 480;
  VibEnergy[5] := 283;
  for n := 1 to Vibrations do Quantas[n] := 0;
  Complete := false;
  repeat
    TotalEnergy := 0.0;
    for n := 1 to Vibrations do
      TotalEnergy := TotalEnergy + VibEnergy[n]*Quantas[n];
    if TotalEnergy < MaxEnergy then
      Display(format('%d%d%d%d%d   %.1f',[Quantas[1],Quantas[2],Quantas[3],Quantas[4],Quantas[5],TotalEnergy]));
    // Go to next state
    Current := 1;
    repeat
      inc(Quantas[Current],1);
      if Quantas[Current] > MaxQuantas then begin
        Quantas[Current] := 0;
        inc(Current,1); // Move to next mode
        SwitchCurrent := true;
        if Current > Vibrations then Complete := true;
      end else
        SwitchCurrent := false;
    until (not SwitchCurrent) or Complete;
  until Complete;
end;

end.
