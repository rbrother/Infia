unit AdvancedList;

{ Implementation of TList, and TListItem with the possibility of saving the
  list items into a text table file. Special advantice of the used
  file format is that additional fields can be added to the file
  format without the old format files becoming unusable. This is
  achived by reading the field names from the header row of the file and
  parsing the file fields accordingly into TListItem fields.}

interface

uses classes, grids, windows, graphics, sysutils, math;

const MaxListFields = 50;

type

  // ADVANCED LIST ITEMS

  TAdvListItem = class(TPersistent)
  public
    constructor create; virtual; abstract;
  end;

  TViewGridListItem = class(TAdvListItem)
  public
    procedure DrawGridCell(Col : integer; Rect : TRect; canvas : TCanvas; CellState: TGridDrawState); virtual; abstract;
  end;

  TEditGridListItem = class(TViewGridListItem)
  public
    function GetEditText(Col : integer) : string; virtual; abstract;
    procedure SetEditText(Col : integer; s : string); virtual; abstract;
  end;

  TSaveListItem = class(TEditGridListItem)  // Abstract class: NO methods are declared explicitly!
  public
    name : string; // Key to be used to find the item
    { the strings parameter of SetFieldsFromStrings contains strings of form
      fieldname=valuestring. Use the value-method of TStringList to extract the values.}
    procedure SetFieldsFromStrings(strings : TStringList); virtual; abstract;
    function AsCSV : string; virtual; abstract; // Item field names and values as strings
  end;

  TParentItem = class(TSaveListItem)
  public
    procedure AddChildItem(NewItem : TSaveListItem); virtual; abstract;
  end;

  // ADVANCED LISTS

  TObjectValue = function(Item : TObject) : double;
  TListItemClass = class of TAdvListItem;

  TAdvancedList = class(TList)
  public
    function CreateEmptyItem : TAdvListItem; virtual; abstract;
    function NearestAbove(ItemValue : TObjectValue; value : double) : integer;
    function Nearest(ItemValue : TObjectValue; value : double) : integer;
    procedure BubbleSort(Compare: TListSortCompare);
    procedure QuickSort(Compare: TListSortCompare);
  end;

  TGridViewList = class(TAdvancedList)
  private
    ConnectedGrid : TDrawGrid;
  public
    procedure ConnectDrawGrid(Grid : TDrawGrid); virtual;
    procedure UpdateRowCount;
    procedure GridDrawCell(Sender: TObject; Col, Row: Integer;
              Rect:TRect; State: TGridDrawState);
    procedure DrawGridHeader(Col : integer; Rect : TRect; canvas : TCanvas);
    procedure DrawGridSelectCell(Sender: TObject; Col,
              Row: Integer; var CanSelect: Boolean); virtual;
    // Grid functionality: abstract methods
    procedure SetUpDrawGrid(Grid : TDrawGrid); virtual; abstract;
    function GridHeaderName(col : integer) : string; virtual; abstract;
  end;

  TGridEditList = class(TGridViewList)
  protected
    ConfirmDelete, AllowDelete, AllowInsert : boolean; // Should the deletion of an item be confirmed
  public
    procedure ConnectDrawGrid(Grid : TDrawGrid); override;
    procedure DrawGridKeyDown(Sender: TObject; var Key: Word;
              Shift: TShiftState); virtual;
    procedure DrawGridGetEditText(Sender: TObject; ACol, ARow: Integer;
              var Value: String);
    procedure DrawGridSetEditText(Sender: TObject; ACol, ARow:
              Longint; const Value: string);
  end;

  TSaveList = class(TGridEditList)
  public
    procedure LoadFromTextFile(filename : string);
    procedure SaveToTextFile(filename : string);
    function CreateItem(fields : tstringlist) : TSaveListItem; virtual;
    procedure LoadChildren(filename, keyfield : string; ChildClass : TListItemClass);
    function FindName(NameToFind : string) : TSaveListItem;
  end;

implementation

uses utils, scriptunit, mathutils, Dialogs;

//***************  A d v a n c e d   L i s t   ******************

function TAdvancedList.Nearest(ItemValue : TObjectValue; value : double) : integer;
var
  n : integer;
  vlow, vhigh : double;
begin
  if count < 2 then begin Nearest := 0; exit; end;
  n := NearestAbove(ItemValue, value);
  if n = 0 then begin Nearest := 0; exit; end;
  if n = count then begin Nearest := count-1; exit; end;
  vlow := ItemValue(items[n-1]);
  vhigh := ItemValue(items[n]);
  if abs(vlow - value) < abs(vhigh - value)
    then Nearest := n-1
    else Nearest := n;
end;

function TAdvancedList.NearestAbove(ItemValue : TObjectValue; value : double) : integer;
{ return the index of the item that is nearest above to the given value as
  measured by the ItemValue-function. Items must be sorted in the ascending
  order of value before calling this routine. NOTE: It requested value is
  higher than ANY current item value, an index *after* the last item will
  be given. }
var
  low, high, mid : integer;
  vmid : double;
begin
  if count < 2 then begin NearestAbove := 0; exit; end;
  if ItemValue(items[0]) > value then begin NearestAbove := 0; exit; end;
  if ItemValue(items[count-1]) < value then begin NearestAbove := count; exit; end;
  low  := 0;
  mid  := count div 2;
  high := count-1;
  repeat
    vmid  := ItemValue(items[mid]);
    if vmid > value then begin
      high := mid;
      mid := (low + mid) div 2;
    end else begin
      low := mid;
      mid := (high + mid) div 2;
    end;
  until abs(high-low) <= 1;
  NearestAbove := high;
end;

procedure TAdvancedList.BubbleSort(Compare: TListSortCompare);
var
  sorted : boolean;
  n : integer;
begin
  repeat
    sorted := true;
    for n := 0 to count-2 do
      if Compare(Items[n],Items[n+1]) > 0 then begin
        exchange(n,n+1);
        sorted := false;
      end;
  until sorted;
end;

procedure TAdvancedList.QuickSort(Compare: TListSortCompare);

  procedure QuickSort(var A: array of Integer; iLo, iHi: Integer);
  var
    Lo, Hi, Mid, T: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := A[(Lo + Hi) div 2];
    repeat
      while A[Lo] < Mid do Inc(Lo);
      while A[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        //VisualSwap(A[Lo], A[Hi], Lo, Hi);
        T := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(A, iLo, Hi);
    if Lo < iHi then QuickSort(A, Lo, iHi);
  end;

begin
  //QuickSort(A, Low(A), High(A));
end;

//***************  G R I D   L I S T   ****************

procedure TGridViewList.ConnectDrawGrid(Grid : TDrawGrid);
begin
  // Direct grid painting routines to given Draw Grid
  ConnectedGrid := Grid;
  UpDateRowCount;
  SetupDrawGrid(Grid);
  Grid.OnDrawCell := GridDrawCell;
  Grid.OnSelectCell := DrawGridSelectCell;
  Grid.Invalidate;
end;

procedure TGridEditList.ConnectDrawGrid(Grid : TDrawGrid);
begin
  inherited ConnectDrawGrid(Grid);
  Grid.OnGetEditText := DrawGridGetEditText;
  Grid.OnSetEditText := DrawGridSetEditText;
  Grid.OnKeyDown := DrawGridKeyDown;
end;

procedure TGridViewList.DrawGridHeader(Col : integer; Rect : TRect; canvas : TCanvas);
var
  s : string;
begin
  s := gridheadername(Col);
  TextAlign(canvas, s, rect, AlignCenter);
end;

procedure TGridViewList.GridDrawCell(Sender: TObject; Col,
  Row: Integer; Rect: TRect; State: TGridDrawState);
begin
  if Row = 0 then DrawGridHeader(Col, Rect, TDrawGrid(Sender).Canvas)
  else if count = 0 then exit
  else begin
    if gdSelected in State then with ConnectedGrid.canvas do begin
      Brush.color := $0000FFFF;
      FillRect(Rect);
    end;
    TViewGridListItem(Items[Row-1]).DrawGridCell(Col, Rect, ConnectedGrid.Canvas, State);
  end;
end;

procedure TGridEditList.DrawGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  itemindex : integer;
  item : TEditGridListItem;
  canselect : boolean;
begin
  itemindex := ConnectedGrid.row-1;
  if ((Key = 46) or (Key = 45)) and (ssCtrl in Shift) then begin // Insert or delete with CTRL
    if (Key = 46) and AllowDelete then begin // DEL
      if (count = 0) or (ConnectedGrid.row = 0) then exit; // no records -> exit
      item := TEditGridListItem(items[itemindex]);
      if not ConfirmDelete or ConfirmMessage('WARNING: All objects depending on the selected record will be DELETED! Do you want to delete the selected record?') then begin
        Delete(itemindex);
        item.Free;
      end;
    end else if (Key = 45) and AllowInsert then begin // INS
      Insert(itemindex, CreateEmptyItem);
    end;
    UpdateRowCount;
    ConnectedGrid.Invalidate;
    // Next, *Call the selectcell Event* (rather unusual thing to do)
    // to notify the Grid to do whatever action needed when band changes.
    with ConnectedGrid do
      OnSelectCell(ConnectedGrid, col, row, canselect);
  end;
end;

procedure TGridEditList.DrawGridGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: String);
begin
  if count = 0 then exit;
  value := TEditGridListItem(Items[ARow-1]).GetEditText(ACol);
end;

procedure TGridEditList.DrawGridSetEditText(Sender: TObject;
  ACol, ARow: Longint; const Value: string);
begin
  if count = 0 then exit;
  TEditGridListItem(Items[ARow-1]).SetEditText(ACol, value);
end;

procedure TGridViewList.UpdateRowCount;
begin
  ConnectedGrid.RowCount := max(count+1, 2);
end;

procedure TGridViewList.DrawGridSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  // Dont do anything: child objects can override this method if
  // they want functionality
end;

//***************  S A V E   L I S T   ****************

function TSaveList.CreateItem(fields : tstringlist) : TSaveListItem;
var
  NewItem : TSaveListItem;
begin
  NewItem := TSaveListItem(CreateEmptyItem);
  NewItem.SetFieldsFromStrings(Fields);
  CreateItem := NewItem;
end;

procedure TSaveList.SaveToTextFile(filename : string);
var
  tablefile : textfile;
  n : integer;
  backupname : string;
begin
  backupname := changefileext(extractfilename(filename), '.BAK');
  DeleteFile(backupname);
  RenameFile(filename, backupname);
  assignfile(tablefile, filename);
  rewrite(tablefile);
  // In writing the file, use the own routine of the item class
  for n := 0 to count-1 do writeln(tablefile, TSaveListItem(items[n]).AsCSV);
  closefile(tablefile);
end;

procedure TSaveList.LoadFromTextFile(filename : string);
{ Reads a data text file of format:
  valename1=value1,valuename2=value2,...,valuenameN=valueN
  valename1=value1,valuename2=value2,...,valuenameN=valueN
  valename1=value1,valuename2=value2,...,valuenameN=valueN
  ...
  A name-based assignment system is used so that when file format can be changed
  old files remain readable without complex conversion routines.
  *Note that the class of the items in the list is passed as a parameter! }
var
  tablefile : textfile;
  line : string;
  Fields : TStringList;
  newitem : TSaveListItem;
begin
  Clear;
  assignfile(tablefile, filename);
  reset(tablefile);
  Fields := TStringList.create;
  while not eof(tablefile) do begin
    readln(tablefile, line);
    Fields.commatext := line; // Parse line into field values and names
    NewItem := TSaveListItem(CreateItem(fields));
    add(NewItem);
    Fields.clear; // remove existing strings
  end;
  closefile(tablefile);
  Fields.free;
end;

procedure TSaveList.LoadChildren(filename, keyfield : string; ChildClass : TListItemClass);
var
  tablefile : textfile;
  line, key : string;
  Fields : TStringList;
  newitem : TSaveListItem;
  ParentItem : TParentItem;
begin
  assignfile(tablefile, filename);
  reset(tablefile);
  Fields := TStringList.create;
  while not eof(tablefile) do begin
    readln(tablefile, line);
    Fields.commatext := line; // Parse line into field values and names
    NewItem := TSaveListItem(ChildClass.Create);
    Newitem.SetFieldsFromStrings(Fields);
    key := Fields.Values[keyfield]; // Get the value of the keyfield used to locate sublist
    ParentItem := TParentItem(FindName(key));
    ParentItem.AddChildItem(NewItem);
    Fields.clear; // remove existing strings
  end;
  closefile(tablefile);
  Fields.free;
end;

function TSaveList.FindName(NameToFind : string) : TSaveListItem;
var
  n : integer;
begin
  n := 0;
  repeat
    if TSaveListItem(Items[n]).name = NameToFind then begin
      FindName := TSaveListItem(Items[n]);
      exit;
    end;
    inc(n,1);
  until n >= count;
  FindName := NIL;
end;

end.
