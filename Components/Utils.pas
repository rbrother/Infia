unit Utils;

{ Miscallaneous helper procedures }

interface

uses SysUtils, graphics, windows, inifiles, classes,
  WinTypes, WinProcs, Messages, Controls, Forms,
  Dialogs, ExtCtrls, Menus, ComCtrls, Registry;

type
  TTextAlign = (AlignLeft, AlignRight, AlignCenter, AlignDecimal);

function BooleanToText(b : boolean) : string;
function TextToBoolean(s : string) : boolean;
function TextToColor(s : string) : TColor;
procedure DrawResidualBar(canvas : TCanvas; rect : TRect; res : double);
function BlendColor(color1, color2 : TColor; weight2 : double) : TColor;
function Blend3Color(color1, color2, color3 : TColor; weight : double) : TColor;
procedure TextAlign(canvas : TCanvas; s : string; rect : TRect; Align : TTextAlign);
procedure TextAlignBool(canvas : TCanvas; b : boolean; rect : TRect; Align : TTextAlign);
function FileEOF(var f : textfile) : boolean;
function ReadBoundsRect(inifile : tinifile; formname : string) : trect;
procedure WriteBoundsRect(inifile : tinifile; formname : string; r : trect);
procedure ShowMessage(s : string);
function ConfirmMessage(s : string) : boolean;
function strleft(s : string; n : integer) : string;
function strright(s : string; n : integer) : string;
function dropleft(s : string; n : integer) : string;
function dropright(s : string; n : integer) : string;

implementation

function BooleanToText(b : boolean) : string;
begin
  if b then BooleanToText := 'True' else BooleanToText := 'False';
end;

function TextToBoolean(s : string) : boolean;
var
  firstletter : string;
begin
  firstletter := uppercase(copy(s,1,1));
  TextToBoolean := false;
  if (FirstLetter = 'T') or (FirstLetter = 'Y') then TextToBoolean := true;
end;

function TextToColor(s : string) : TColor;
var
  t : string;
begin
  t := UpperCase(s);                  
  TextToColor := clBlack;
  if t = 'RED' then TextToColor := clRed
  else if t = 'GREEN' then TextToColor := clGreen
  else if t = 'BLUE' then TextToColor := clBlue
  else if t = 'YELLOW' then TextToColor := clYellow
  else if t = 'GRAY' then TextToColor := clGray
  else if t = 'PURPLE' then TextToColor := clPurple
  else if t = 'BLACK' then TextToColor := clBlack;
end;

procedure DrawResidualBar(canvas : TCanvas; rect : TRect; res : double);
// res = -1...1 describing the portion of the total filling of rect by the bar
var
  width, Rwidth, center : integer;
  x : integer;
begin
  Width := rect.right - rect.left;
  Center := rect.left + width div 2;
  RWidth := Width div 2 - 2;
  x := abs(trunc(Res * RWidth));
  if x > RWidth then x := RWidth;
  // Make a mixture of blue and red, depending residual size
  with canvas.brush do
  if Res > 0 then Color := BlendColor(clWhite, clBlue, x/RWidth)
             else Color := BlendColor(clWhite, clRed, x/RWidth);
  with canvas.pen do begin
    mode := pmCopy; Color := $00A0A0A0;
  end;
  with canvas do if Res > 0 then
    Rectangle(Center, rect.top + 2, Center + x, rect.bottom - 2)
  else
    Rectangle(Center - x, rect.top + 2, Center, rect.bottom - 2);
end;

function BlendColor(color1, color2 : TColor; weight2 : double) : TColor;
const
  Blue = $00FF0000;
  Green = $0000FF00;
  Red = $000000FF;
var
  r1,g1,b1, r2,g2,b2, r,g,b : double;
begin
  r1 := (color1 and Red) / 255.0;
  g1 := ((color1 and Green) shr 8) / 255.0;
  b1 := ((color1 and Blue) shr 16) / 255.0;
  r2 := (color2 and Red) / 255.0;
  g2 := ((color2 and Green) shr 8) / 255.0;
  b2 := ((color2 and Blue) shr 16) / 255.0;
  r := r1*(1.0-weight2) + r2*weight2;
  g := g1*(1.0-weight2) + g2*weight2;
  b := b1*(1.0-weight2) + b2*weight2;
  BlendColor := trunc(r*$FF) + trunc(g*$FF) shl 8 + trunc(b*$FF) shl 16;
end;

function Blend3Color(color1, color2, color3 : TColor; weight : double) : TColor;
begin
 if weight < 0.5 then
   Blend3Color := BlendColor(color1, color2, weight * 2.0)
 else
   Blend3Color := BlendColor(color2, color3, (weight-0.5) * 2.0);
end;

function FileEOF(var f : textfile) : boolean;
begin
  FileEOF := EOF(f);
end;

procedure TextAlign(canvas : TCanvas; s : string; rect : TRect; Align : TTextAlign);
var
  x, textwidth : integer;
  start : string;
begin
  textwidth := canvas.textwidth(s);
  if Align = AlignLeft then x := Rect.Left + 2
  else if Align = AlignRight then x := Rect.Right - 2 - TextWidth
  else if Align = AlignDecimal then begin
    start := copy(s,1,pos('.',s));
    x := Rect.Left + (Rect.Right - Rect.Left) div 3 - canvas.textwidth(start);
  end else x := Rect.left + (Rect.right - Rect.left - textwidth) div 2;
  canvas.textrect(rect, x, Rect.Top+2, s);
end;

procedure TextAlignBool(canvas : TCanvas; b : boolean; rect : TRect; Align : TTextAlign);
var
  s : string;
begin
  s := BooleanToText(b);
  if b then with canvas do begin
    Brush.Color := clBlack;
    Font.Color := clWhite;
  end;
  canvas.FillRect(Rect);
  TextAlign(canvas, s, rect, Align);
end;

function ReadBoundsRect(inifile : tinifile; formname : string) : trect;
var
  r : trect;
begin
  r.left := inifile.ReadInteger(formname,'Left', 1);
  r.top := inifile.ReadInteger(formname,'top', 1);
  r.right := inifile.ReadInteger(formname,'right', 500);
  r.bottom := inifile.ReadInteger(formname,'bottom', 400);
  ReadBoundsRect := r;
end;

procedure WriteBoundsRect(inifile : tinifile; formname : string; r : trect);
begin
  inifile.WriteInteger(formname,'Left',r.left);
  inifile.WriteInteger(formname,'Top',r.top);
  inifile.WriteInteger(formname,'Right',r.right);
  inifile.WriteInteger(formname,'Bottom',r.bottom);
end;

procedure ShowMessage(s : string);
begin
  MessageDlg(s, mtInformation, [mbOK], 0);
end;

function ConfirmMessage(s : string) : boolean;
begin
  if MessageDlg(s, mtConfirmation, [mbYes, mbCancel], 0) = mrYes
    then ConfirmMessage := true
    else ConfirmMessage := false;
end;

function strleft(s : string; n : integer) : string;
begin
  strleft := copy(s, 1, n);
end;

function strright(s : string; n : integer) : string;
begin
  strright := copy(s, length(s) - n + 1, n);
end;

function dropleft(s : string; n : integer) : string;
// drops first n characters from the string and returns the rest
begin
  dropleft := copy(s, n+1, length(s) - n);
end;

function dropright(s : string; n : integer) : string;
// drops last n characters from the string and returns the rest
begin
  dropright := copy(s, 1, length(s) - n);
end;

end.
