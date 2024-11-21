unit FormulaUnit;

interface

uses math, SysUtils, ScriptUnit, ParameterUnit;

type
  TFormula = class(TObject)
  public
    function Value : double; virtual; abstract;
    function FormulaText : string; virtual; abstract;
  end;

  TConstant = class(TFormula)
  private
    ConstantValue : double;
  public
    constructor Create(NewValue : double);
    function Value : double; override;
    function FormulaText : string; override;
  end;

  TFormulaParameter = class(TFormula)
  private
    parameter : TParameter;
    negative : boolean;
  public
    constructor Create(NewParameter : TParameter; NewNegative : boolean);
    function Value : double; override;
    function FormulaText : string; override;
  end;

  TSum = class(TFormula)
  private
    term1, term2 : TFormula;
  public
    constructor Create(a,b : TFormula);
    destructor Destroy; override;
    function Value : double; override;
    function FormulaText : string; override;
  end;

  TProduct = class(TFormula)
  private
    factor1, factor2 : TFormula;
  public
    constructor Create(a,b : TFormula);
    destructor Destroy; override;
    function Value : double; override;
    function FormulaText : string; override;
  end;

  TFraction = class(TFormula)
  private
    nominator, denominator : TFormula;
  public
    constructor Create(NewNominator, NewDenominator : TFormula);
    destructor Destroy; override;
    function Value : double; override;
    function FormulaText : string; override;
  end;

  TPower = class(TFormula)
  private
    base, exponent : TFormula;
  public
    constructor Create(NewBase, NewExponent : TFormula);
    destructor Destroy; override;
    function Value : double; override;
    function FormulaText : string; override;
  end;

  TFormulaParser = class(TObject)
  private
    level1 : string;
  public
    function Parse(s : string) : TFormula;
    procedure MaskBrackets;
  end;

var
  FormulaParser : TFormulaParser;

implementation

uses Utils;

//**************************************************************
//                     C O N S T A N T
//**************************************************************

constructor TConstant.Create(NewValue : double);
begin
  ConstantValue := NewValue;
end;

function TConstant.Value : double;
begin
  Value := ConstantValue;
end;

function TConstant.FormulaText : string;
var
  s : string;
begin
  s := floattostrF(ConstantValue, ffFixed, 6, 3);
  if ConstantValue > 0.0 then FormulaText := s
  else FormulaText := '(' + s + ')';
end;

//**************************************************************
//                    P A R A M E T E R
//**************************************************************

constructor TFormulaParameter.Create(NewParameter : TParameter; NewNegative : boolean);
begin
  Parameter := NewParameter;
  Negative := NewNegative;
end;

function TFormulaParameter.Value : double;
begin
  if negative then
    Value := -Parameter.v
  else
    Value := Parameter.v;
end;

function TFormulaParameter.FormulaText : string;
begin
  FormulaText := Parameter.Name;
end;

//**************************************************************
//                          S U M
//**************************************************************

constructor TSum.create(a,b : TFormula);
begin
  term1 := a;
  term2 := b;
end;

destructor TSum.Destroy;
begin
  term1.free;
  term2.free;
end;


function TSum.Value : double;
var
  a,b : double;
begin
  a := term1.value;
  b := term2.value;
  Value := a + b;
  //display('SUM ' + floattostr(a) + '+' + floattostr(b) + ' = ' + floattostr(a+b));
end;

function TSum.FormulaText : string;
begin
  FormulaText := term1.FormulaText + '+' + term2.FormulaText;
end;

//**************************************************************
//                      P R O D U C T
//**************************************************************

constructor TProduct.Create(a,b : TFormula);
begin
  factor1 := a;
  factor2 := b;
end;

destructor TProduct.Destroy;
begin
  factor1.free;
  factor2.free;
end;

function TProduct.Value : double;
var
  a,b : double;
begin
  a := factor1.value;
  b := factor2.value;
  Value := a * b;
  //display('PRODUCT ' + floattostr(a)+'*'+floattostr(b)+' = '+floattostr(a*b));
end;

function TProduct.FormulaText : string;
begin
  FormulaText := factor1.FormulaText + '*' + factor2.FormulaText;
end;

//**************************************************************
//                      F R A C T I O N
//**************************************************************

constructor TFraction.Create(NewNominator, NewDenominator : TFormula);
begin
  nominator := NewNominator;
  denominator := NewDenominator;
end;

destructor TFraction.Destroy;
begin
  nominator.free;
  denominator.free;
end;

function TFraction.Value : double;
begin
  Value := nominator.value / denominator.value;
end;

function TFraction.FormulaText : string;
begin
  FormulaText := '(' + Nominator.FormulaText + ')/(' + Denominator.FormulaText + ')';
end;

//**************************************************************
//                        P O W E R
//**************************************************************

constructor TPower.Create(NewBase, NewExponent : TFormula);
begin
  Base := NewBase;
  Exponent := NewExponent;
end;

destructor TPower.Destroy;
begin
  base.free;
  exponent.free;
end;

function TPower.Value : double;
begin
  Value := power(Base.value, Exponent.value);
end;

function TPower.FormulaText : string;
begin
  FormulaText := Base.FormulaText + '^' + exponent.FormulaText;
end;

//**************************************************************
//                F O R M U L A   P A R S E R
//**************************************************************

function TFormulaParser.Parse(s : string) : TFormula;
var
  x : integer;
  constant : double;
  Parameter : TParameter;
  minus : boolean;
begin
  s := trim(s); // Remove spaces
  if (strleft(s,1) = '(') and (strright(s,1)=')') then s := copy(s,2,length(s)-2);
  s := trim(s); // Remove spaces
  level1 := s;
  MaskBrackets;
  x := pos('+',level1);
  if x > 0 then begin
    Parse := TSum.Create(Parse(strleft(s,x-1)),Parse(dropleft(s,x)));
    exit;
  end;
  x := pos('-',dropleft(level1,1))+1;
  if x > 1 then begin // Leave possible initial minus to the term
    Parse := TSum.Create(Parse(strleft(s,x-1)),Parse(dropleft(s,x-1)));
    exit;
  end;
  x := pos('*',level1);
  if x > 0 then begin
    Parse := TProduct.Create(Parse(strleft(s,x-1)),Parse(dropleft(s,x)));
    exit;
  end;
  x := pos('/',level1);
  if x > 0 then begin
    Parse := TFraction.Create(Parse(strleft(s,x-1)),Parse(dropleft(s,x)));
    exit;
  end;
  x := pos('^',level1);
  if x > 0 then begin
    Parse := TPower.Create(Parse(strleft(s,x-1)),Parse(dropleft(s,x)));
    exit;
  end;
  try
    constant := strtofloat(s);
    Parse := TConstant.Create(constant);
  except on
    EConvertError do begin
      if pos('-',level1) = 1 then begin
        minus := true;
        s := dropleft(s,1);
      end else
        minus := false;
      Parameter := FitParameters.Find(s);
      if Parameter = NIL then errormessage('TFormulaParser.Parse: Parameter '+s+' not found');
      Parse := TFormulaParameter.Create(Parameter, minus);
    end;
  end;
end;

procedure TFormulaParser.MaskBrackets;
// Mask bracket areas with ?????? to get level1 formula
var
  x : integer;
  bracketlevel : integer;
begin
  bracketlevel := 0;
  for x := 1 to length(level1) do begin
    if level1[x] = '(' then inc(bracketlevel);
    if level1[x] = ')' then begin
      dec(bracketlevel);
      level1[x] := '?';
    end;
    if bracketlevel > 0 then level1[x] := '?';
  end;
end;

end.
