{*********************************************}
{  TeeBI Software Library                     }
{  TBIFilter class using Expressions          }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expression.Filter;

interface

uses
  {System.}Classes, BI.DataItem, BI.Expression, BI.CollectionItem,
  BI.Arrays, BI.Expressions;

type
  TFloat=Double;

  TFilterItem=class;

  TFilterPart=class(TPersistent)
  private
    IItem : TFilterItem;

    procedure DoChanged;
  public
    Constructor Create(const AItem:TFilterItem);
  end;

  TNumericValue=class(TFilterPart)
  private
    FEnabled: Boolean;
    FValue: TFloat;

    IOperand : TLogicalOperand;

    procedure SetEnabled(const Value: Boolean);
    procedure SetValue(const Value: TFloat);
    function GetEqual: Boolean;
    procedure SetEqual(const Value: Boolean);
  public
    Constructor Create(const AItem:TFilterItem; const AOperand:TLogicalOperand);

    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;
    procedure Reset;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default False;
    property Equal:Boolean read GetEqual write SetEqual default True;
    property Value:TFloat read FValue write SetValue;
  end;

  TMonths=class(TFilterPart)
  private
    IMonths : Array[0..11] of Boolean;

    function Get(const Index: Integer): Boolean;
    procedure Put(const Index: Integer; const Value: Boolean);
    function ZeroOrAll:Boolean;
  protected
    procedure SetMonths(const Value:TBooleanArray);
  public
    function Filter:TLogicalExpression;

    procedure Assign(Source:TPersistent); override;
    procedure Reset;

    property Month[const Index:Integer]:Boolean read Get write Put; default;
  published
    property January:Boolean index 0 read Get write Put default False;
    property February:Boolean index 1 read Get write Put default False;
    property March:Boolean index 2 read Get write Put default False;
    property April:Boolean index 3 read Get write Put default False;
    property May:Boolean index 4 read Get write Put default False;
    property June:Boolean index 5 read Get write Put default False;
    property July:Boolean index 6 read Get write Put default False;
    property August:Boolean index 7 read Get write Put default False;
    property September:Boolean index 8 read Get write Put default False;
    property October:Boolean index 9 read Get write Put default False;
    property November:Boolean index 10 read Get write Put default False;
    property December:Boolean index 11 read Get write Put default False;
  end;

  TWeekdays=class(TFilterPart)
  private
    IWeekdays : Array[0..6] of Boolean;

    function Get(const Index: Integer): Boolean;
    procedure Put(const Index: Integer; const Value: Boolean);
    function ZeroOrAll:Boolean;
  protected
    procedure SetWeekDays(const Value:TBooleanArray);
  public
    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;
    procedure Reset;

    property Weekday[const Index:Integer]:Boolean read Get write Put; default;
  published
    property Monday:Boolean index 0 read Get write Put default False;
    property Tuesday:Boolean index 1 read Get write Put default False;
    property Wednesday:Boolean index 2 read Get write Put default False;
    property Thursday:Boolean index 3 read Get write Put default False;
    property Friday:Boolean index 4 read Get write Put default False;
    property Saturday:Boolean index 5 read Get write Put default False;
    property Sunday:Boolean index 6 read Get write Put default False;
  end;

  TDateTimeSelected=class(TFilterPart)
  private
    FDateTime : TDateTime;
    FEnabled: Boolean;
    FPart : TDateTimePart;
    FValue : Integer;

    function EqualPart:TLogicalExpression;

    procedure SetDateTime(const Value: TDateTime);
    procedure SetEnabled(const Value: Boolean);
    procedure SetPart(const Value: TDateTimePart);
    procedure SetValue(const Value: Integer);
  public
    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;
    procedure Reset;
  published
    property DateTime:TDateTime read FDateTime write SetDateTime;
    property Enabled:Boolean read FEnabled write SetEnabled default False;
    property Part:TDateTimePart read FPart write SetPart default TDateTimePart.None;
    property Value:Integer read FValue write SetValue default 0;
  end;

  TDateTimeFilterStyle=(All,Custom,Today,Yesterday,Tomorrow,This,Last,Next);

  TDateTimeFilter=class(TFilterPart)
  private
    FMonths : TMonths;
    FPeriod: TDateTimeSpan;
    FQuantity : Integer;
    FSelected: TDateTimeSelected;  // "Style N Period" = "Last 10 Weeks"
    FStyle : TDateTimeFilterStyle;
    FWeekdays : TWeekdays;

    function GetFrom: TDateTime;
    function GetFromEqual: Boolean;
    function GetTo: TDateTime;
    function GetToEqual: Boolean;

    procedure SetFrom(const Value: TDateTime);
    procedure SetFromEqual(const Value: Boolean);
    procedure SetMonths(const Value: TMonths);
    procedure SetPeriod(const Value: TDateTimeSpan);
    procedure SetQuantity(const Value: Integer);
    procedure SetSelected(const Value: TDateTimeSelected);
    procedure SetStyle(const Value: TDateTimeFilterStyle);
    procedure SetTo(const Value: TDateTime);
    procedure SetToEqual(const Value: Boolean);
    procedure SetWeedays(const Value: TWeekdays);
  public
    Constructor Create(const AItem:TFilterItem);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;
    procedure Reset;

    property FromDate:TDateTime read GetFrom write SetFrom;
    property FromEqual:Boolean read GetFromEqual write SetFromEqual default True;

    property ToDate:TDateTime read GetTo write SetTo;
    property ToEqual:Boolean read GetToEqual write SetToEqual default True;

  published
    property Months:TMonths read FMonths write SetMonths;
    property Period:TDateTimeSpan read FPeriod write SetPeriod default TDateTimeSpan.None;
    property Quantity:Integer read FQuantity write SetQuantity default 1;
    property Selected:TDateTimeSelected read FSelected write SetSelected;
    property Style:TDateTimeFilterStyle read FStyle write SetStyle default TDateTimeFilterStyle.All;
    property Weekdays:TWeekdays read FWeekdays write SetWeedays;
  end;

  TBooleanFilter=class(TFilterPart)
  private
    IValue : Array[Boolean] of Boolean;

    function GetFalse: Boolean; inline;
    function GetTrue: Boolean; inline;
    procedure SetFalse(const Value: Boolean);
    procedure SetTrue(const Value: Boolean);
  public
    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;

    procedure Reset;
  published
    property IncludeTrue:Boolean read GetTrue write SetTrue default False;
    property IncludeFalse:Boolean read GetFalse write SetFalse default False;
  end;

  TNumericFilter=class(TFilterPart)
  private
    FFrom : TNumericValue;
    FSelected : TNumericValue;
    FTo : TNumericValue;

    procedure SetFrom(const Value: TNumericValue);
    procedure SetSelected(const Value: TNumericValue);
    procedure SetTo(const Value: TNumericValue);
  protected
    function RangeFilter:TLogicalExpression;
  public
    Constructor Create(const AItem:TFilterItem);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;
    procedure Reset;
  published
    property FromValue:TNumericValue read FFrom write SetFrom;
    property Selected:TNumericValue read FSelected write SetSelected;
    property ToValue:TNumericValue read FTo write SetTo;
  end;

  TTextFilterStyle=(Contains,IsEqual,Starts,Ends,IsEmpty);

  TTextFilter=class(TFilterPart)
  private
    FStyle : TTextFilterStyle;
    FText : String;
    FCase: Boolean;

    procedure SetStyle(const Value:TTextFilterStyle);
    procedure SetText(const Value:String);
    procedure SetCase(const Value: Boolean);
  public
    procedure Assign(Source:TPersistent); override;

    function Filter:TExpression;
    procedure Reset;
  published
    property CaseSensitive:Boolean read FCase write SetCase default False;
    property Style:TTextFilterStyle read FStyle write SetStyle default TTextFilterStyle.Contains;
    property Text:String read FText write SetText;
  end;

  TFilterItem=class(TDataCollectionItem)
  private
    FBool : TBooleanFilter;
    FDateTime : TDateTimeFilter;
    FEnabled: Boolean;
    FExcluded: TStringList;
    FExpression : TLogicalExpression;
    FIncluded: TStringList;
    FInverted: Boolean;
    FNumeric : TNumericFilter;
    FText : TTextFilter;

    procedure DoChanged(Sender:TObject);
    function GetFilter: TExpression;
    function MainData:TDataItem;
    function NewLogical(const AOperand:TLogicalOperand):TLogicalExpression;
    procedure SetBool(const Value: TBooleanFilter);
    procedure SetDateTime(const Value: TDateTimeFilter);
    procedure SetEnabled(const Value: Boolean);
    procedure SetExcluded(const Value: TStringList);
    procedure SetIncluded(const Value: TStringList);
    procedure SetInverted(const Value: Boolean);
    procedure SetNumeric(const Value: TNumericFilter);
    procedure SetText(const Value: TTextFilter);
    procedure TryAdd(const AText:String; const A,B:TStrings);
    function GetKind: TDataKind;
  protected
    procedure Changed; override;
    function DataExpression:TDataItemExpression;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure ExcludeText(const AText:String; const Add:Boolean=True);
    procedure IncludeText(const AText:String; const Add:Boolean=True);

    class function FromData(const AData:TDataItem):TFilterItem; static;

    procedure Reset;

    property Expression:TLogicalExpression read FExpression write FExpression;
    property Filter:TExpression read GetFilter;
    property Kind:TDataKind read GetKind;
  published
    property BoolFilter:TBooleanFilter read FBool write SetBool;
    property DateTime:TDateTimeFilter read FDateTime write SetDateTime;
    property Enabled:Boolean read FEnabled write SetEnabled default True;
    property Excluded:TStringList read FExcluded write SetExcluded;
    property Included:TStringList read FIncluded write SetIncluded;
    property Inverted:Boolean read FInverted write SetInverted default False;
    property Numeric:TNumericFilter read FNumeric write SetNumeric;
    property Text:TTextFilter read FText write SetText;
  end;

  TBIFilter=class;

  TFilterItems=class(TOwnedCollection)
  private
    IFilter : TBIFilter;

    procedure DoChanged(Sender:TObject);
    function Get(const Index: Integer): TFilterItem;
    procedure Put(const Index: Integer; const Value: TFilterItem);
  public
    Constructor Create(AOwner: TPersistent);

    function Add(const AData:TDataItem):TFilterItem;
    function Filter:TExpression;

    property Items[const Index:Integer]:TFilterItem read Get write Put; default;
  end;

  TBIFilter=class(TPersistent)
  private
    FEnabled : Boolean;
    FItems: TFilterItems;
    FText : String;

    procedure Changed;
    function Get(const AIndex: Integer): TFilterItem;
    function IsItemsStored: Boolean;
    procedure Put(const AIndex: Integer; const Value: TFilterItem);
    procedure SetEnabled(const Value: Boolean);
    procedure SetItems(const Value: TFilterItems);
    procedure SetText(const Value: String);
  protected
    IChanged : TNotifyEvent;
    ICustom : TExpression;
    IUpdating : Boolean;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function Add(const AData:TDataItem):TFilterItem; overload; inline;
    function Add(const AExpression:TLogicalExpression):TFilterItem; overload; inline;

    procedure Clear;
    function Custom:TLogicalExpression;
    procedure Delete(const AIndex:Integer);
    function Filter:TExpression;

    function ItemOf(const AData:TDataItem):TFilterItem;

    property Item[const AIndex:Integer]:TFilterItem read Get write Put; default;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default True;
    property Items:TFilterItems read FItems write SetItems stored IsItemsStored;
    property Text:String read FText write SetText;
  end;

implementation

uses
  {System.}SysUtils, {System.}DateUtils, BI.Expression.DateTime;

{ TFilterPart }

Constructor TFilterPart.Create(const AItem: TFilterItem);
begin
  inherited Create;
  IItem:=AItem;
end;

procedure TFilterPart.DoChanged;
begin
  IItem.Changed;
end;

{ TNumericFilter }

Constructor TNumericFilter.Create(const AItem:TFilterItem);
begin
  inherited Create(AItem);

  FFrom:=TNumericValue.Create(AItem,TLogicalOperand.GreaterOrEqual);
  FSelected:=TNumericValue.Create(AItem,TLogicalOperand.Equal);
  FTo:=TNumericValue.Create(AItem,TLogicalOperand.LowerOrEqual);
end;

Destructor TNumericFilter.Destroy;
begin
  FTo.Free;
  FSelected.Free;
  FFrom.Free;

  inherited;
end;

procedure TNumericFilter.Assign(Source: TPersistent);
begin
  if Source is TNumericFilter then
  begin
    FromValue:=TNumericFilter(Source).FromValue;
    Selected:=TNumericFilter(Source).Selected;
    ToValue:=TNumericFilter(Source).ToValue;
  end
  else
    inherited;
end;

function TNumericFilter.Filter: TLogicalExpression;
begin
  result:=FSelected.Filter;

  if result=nil then
     result:=RangeFilter;
end;

function TNumericFilter.RangeFilter: TLogicalExpression;
var tmp : TLogicalExpression;
begin
  result:=FFrom.Filter;

  tmp:=FTo.Filter;

  if tmp<>nil then
     result:=TLogicalExpression.Join(result,tmp);
end;

procedure TNumericFilter.Reset;
begin
  FFrom.Reset;
  FTo.Reset;
  FSelected.Reset;

  DoChanged;
end;

procedure TNumericFilter.SetFrom(const Value: TNumericValue);
begin
  FFrom.Assign(Value);
end;

procedure TNumericFilter.SetSelected(const Value: TNumericValue);
begin
  FSelected.Assign(Value);
end;

procedure TNumericFilter.SetTo(const Value: TNumericValue);
begin
  FTo.Assign(Value);
end;

{ TFilterItem }

Constructor TFilterItem.Create(Collection: TCollection);
begin
  inherited;

  FEnabled:=True;

  FIncluded:=TStringList.Create;
  FIncluded.OnChange:=DoChanged;

  FExcluded:=TStringList.Create;
  FExcluded.OnChange:=DoChanged;

  FBool:=TBooleanFilter.Create(Self);
  FDateTime:=TDateTimeFilter.Create(Self);
  FNumeric:=TNumericFilter.Create(Self);
  FText:=TTextFilter.Create(Self);
end;

Destructor TFilterItem.Destroy;
begin
  FText.Free;
  FNumeric.Free;
  FDateTime.Free;
  FBool.Free;
  FExcluded.Free;
  FIncluded.Free;

  FExpression.Free;

  inherited;
end;

procedure TFilterItem.Changed;
begin
  if IUpdating=0 then
  begin
    inherited;

    if Collection<>nil then
       TFilterItems(Collection).DoChanged(Self);
  end;
end;

procedure TFilterItem.DoChanged(Sender: TObject);
begin
  Changed;
end;

procedure TryRemove(const AText:String; const AList:TStrings);
var tmp : Integer;
begin
  tmp:=AList.IndexOf(AText);

  if tmp<>-1 then
     AList.Delete(tmp);
end;

procedure TFilterItem.TryAdd(const AText:String; const A,B:TStrings);
begin
  TryRemove(AText,B);

  if A.IndexOf(AText)=-1 then
     A.Add(AText);
end;

procedure TFilterItem.IncludeText(const AText: String; const Add: Boolean);
begin
  BeginUpdate;
  try
    if Add then
       TryAdd(AText,Included,Excluded)
    else
       TryRemove(AText,Included);
  finally
    EndUpdate;
  end;
end;

procedure TFilterItem.ExcludeText(const AText: String; const Add: Boolean);
begin
  BeginUpdate;
  try
    if Add then
       TryAdd(AText,Excluded,Included)
    else
       TryRemove(AText,Excluded);
  finally
    EndUpdate;
  end;
end;

class function TFilterItem.FromData(const AData: TDataItem): TFilterItem;
begin
  result:=TFilterItem.Create(nil);
  result.Data:=AData;
end;

procedure TFilterItem.Assign(Source: TPersistent);
begin
  if Source is TFilterItem then
  begin
    BoolFilter:=TFilterItem(Source).BoolFilter;
    DateTime:=TFilterItem(Source).DateTime;
    FEnabled:=TFilterItem(Source).FEnabled;
    Excluded:=TFilterItem(Source).Excluded;
    Numeric:=TFilterItem(Source).Numeric;
    Included:=TFilterItem(Source).Included;
    Text:=TFilterItem(Source).Text;

    // Not done at ancestor:
    Data:=TFilterItem(Source).Data;
  end;

  inherited;
end;

function TFilterItem.DataExpression:TDataItemExpression;
begin
  result:=TDataItemExpression.Create(Data,False,MainData);
end;

function TFilterItem.NewLogical(const AOperand:TLogicalOperand):TLogicalExpression;
begin
  result:=TLogicalExpression.Create;
  result.Operand:=AOperand;
  result.Left:=DataExpression;
end;

procedure TFilterItem.Reset;
begin
  BeginUpdate;
  try
    FBool.Reset;
    FDateTime.Reset;
    FExcluded.Clear;
    FIncluded.Clear;
    FNumeric.Reset;
    FText.Reset;

    FExpression.Free;
    FExpression:=nil;
  finally
    EndUpdate;
  end;
end;

function TFilterItem.GetFilter: TExpression;

  function NewEqualsText(const AValue:String; const AOperand:TLogicalOperand):TLogicalExpression;
  begin
    result:=NewLogical(AOperand);
    result.Right:=TTextExpression.Create(AValue);
  end;

  function IncludeText(const AList:TStrings; const IsEqual:Boolean):TLogicalExpression;
  var t : Integer;
      tmpEqual,
      tmpOp : TLogicalOperand;
  begin
    result:=nil;

    if IsEqual then
    begin
      tmpEqual:=TLogicalOperand.Equal;
      tmpOp:=TLogicalOperand.&Or;
    end
    else
    begin
      tmpEqual:=TLogicalOperand.NotEqual;
      tmpOp:=TLogicalOperand.&And;
    end;

    for t:=0 to AList.Count-1 do
        result:=TLogicalExpression.Join(result,NewEqualsText(AList[t],tmpEqual),tmpOp);
  end;

  function JoinWith(const A,B:TExpression):TExpression;
  begin
    if A=nil then
       result:=B
    else
    if B=nil then
       result:=A
    else
       result:=TLogicalExpression.Create(A,TLogicalOperand.&And,B);
  end;

begin
  result:=FExpression;

  if FIncluded.Count>0 then
     result:=JoinWith(result,IncludeText(FIncluded,True));

  if FExcluded.Count>0 then
     result:=JoinWith(result,IncludeText(FExcluded,False));

  if Data<>nil then
  begin
    if Data.Kind=TDataKind.dkDateTime then
       result:=JoinWith(result,FDateTime.Filter)
    else
    if Data.Kind.IsNumeric then
       result:=JoinWith(result,FNumeric.Filter)
    else
    if Data.Kind=TDataKind.dkBoolean then
       result:=JoinWith(result,FBool.Filter)
    else
    if Data.Kind=TDataKind.dkText then
       result:=JoinWith(result,FText.Filter);
  end;

  if FInverted and (result<>nil) then
     result:=TUnaryNotExpression.Create(result);
end;

function TFilterItem.GetKind: TDataKind;
begin
  if Data=nil then
     result:=TDataKind.dkUnknown
  else
     result:=Data.Kind;
end;

function TFilterItem.MainData: TDataItem;
begin
  result:=nil;
end;

procedure TFilterItem.SetBool(const Value: TBooleanFilter);
begin
  FBool.Assign(Value);
end;

procedure TFilterItem.SetDateTime(const Value: TDateTimeFilter);
begin
  FDateTime.Assign(Value);
end;

procedure TFilterItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    Changed;
  end;
end;

procedure TFilterItem.SetExcluded(const Value: TStringList);
begin
  FExcluded.Assign(Value);
end;

procedure TFilterItem.SetIncluded(const Value: TStringList);
begin
  FIncluded.Assign(Value);
end;

procedure TFilterItem.SetInverted(const Value: Boolean);
begin
  if FInverted<>Value then
  begin
    FInverted:=Value;
    Changed;
  end;
end;

procedure TFilterItem.SetNumeric(const Value: TNumericFilter);
begin
  FNumeric.Assign(Value);
end;

procedure TFilterItem.SetText(const Value: TTextFilter);
begin
  FText.Assign(Value);
end;

{ TNumericValue }

Constructor TNumericValue.Create(const AItem:TFilterItem; const AOperand: TLogicalOperand);
begin
  inherited Create(AItem);
  IOperand:=AOperand;
end;

procedure TNumericValue.Assign(Source: TPersistent);
begin
  if Source is TNumericValue then
  begin
    FEnabled:=TNumericValue(Source).FEnabled;
    FValue:=TNumericValue(Source).FValue;
  end
  else
    inherited;
end;

function TNumericValue.Filter: TLogicalExpression;
begin
  if FEnabled then
  begin
    result:=IItem.NewLogical(IOperand);
    result.Right:=TFloatExpression.Create(FValue);
  end
  else
    result:=nil;
end;

function TNumericValue.GetEqual: Boolean;
begin
  result:=(IOperand=TLogicalOperand.Equal) or
          (IOperand=TLogicalOperand.GreaterOrEqual) or
          (IOperand=TLogicalOperand.LowerOrEqual);
end;

procedure TNumericValue.Reset;
begin
  FEnabled:=False;
  FValue:=0;
  Equal:=True;

  DoChanged;
end;

procedure TNumericValue.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    DoChanged;
  end;
end;

procedure TNumericValue.SetEqual(const Value: Boolean);
begin
  if Value then
  begin
    if IOperand=TLogicalOperand.Greater then
       IOperand:=TLogicalOperand.GreaterOrEqual
    else
    if IOperand=TLogicalOperand.Lower then
       IOperand:=TLogicalOperand.LowerOrEqual
    else
    if IOperand=TLogicalOperand.NotEqual then
       IOperand:=TLogicalOperand.Equal;
  end
  else
  begin
    if IOperand=TLogicalOperand.GreaterOrEqual then
       IOperand:=TLogicalOperand.Greater
    else
    if IOperand=TLogicalOperand.LowerOrEqual then
       IOperand:=TLogicalOperand.Lower
    else
    if IOperand=TLogicalOperand.Equal then
       IOperand:=TLogicalOperand.NotEqual;
  end;
end;

procedure TNumericValue.SetValue(const Value: TFloat);
begin
  if (not FEnabled) or (FValue<>Value) then
  begin
    FEnabled:=True;
    FValue:=Value;
    DoChanged;
  end;
end;

function SelectedCount(const Value:Array of Boolean):Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to High(Value) do
      if Value[t] then
         Inc(result);
end;

function CalcZeroOrAll(const Value:Array of Boolean):Boolean;
var tmp : Integer;
begin
  tmp:=SelectedCount(Value);

  result:=(tmp=0) or (tmp=Length(Value));
end;

{ TMonths }

function TMonths.Get(const Index: Integer): Boolean;
begin
  result:=IMonths[Index];
end;

procedure TMonths.Put(const Index: Integer; const Value: Boolean);
begin
  if IMonths[Index]<>Value then
  begin
    IMonths[Index]:=Value;
    DoChanged;
  end;
end;

procedure TMonths.Reset;
var t : Integer;
begin
  for t:=Low(IMonths) to High(IMonths) do
      IMonths[t]:=False;

  DoChanged;
end;

procedure TMonths.SetMonths(const Value: TBooleanArray);
var t : Integer;
begin
  for t:=0 to Value.Count-1 do
      IMonths[t]:=Value[t];
end;

function TMonths.ZeroOrAll: Boolean;
begin
  result:=CalcZeroOrAll(IMonths);
end;

procedure TMonths.Assign(Source: TPersistent);
begin
  if Source is TMonths then
     IMonths:=TMonths(Source).IMonths
  else
     inherited;
end;

function TMonths.Filter:TLogicalExpression;

  function EqualsMonth(const AIndex:Integer; const AOperand:TLogicalOperand):TLogicalExpression;
  var tmp : TDateTimePartExpression;
  begin
    tmp:=TDateTimePartExpression.Create(IItem.DataExpression);
    tmp.Part:=TDateTimePart.Month;

    result:=TLogicalExpression.Create(tmp,AOperand,TIntegerExpression.Create(AIndex+1));
  end;

var t : Integer;
begin
  result:=nil;

  if SelectedCount(IMonths)>6 then
  begin
    for t:=0 to High(IMonths) do
        if not IMonths[t] then
           result:=TLogicalExpression.Join(result,EqualsMonth(t,TLogicalOperand.NotEqual));
  end
  else
    for t:=0 to High(IMonths) do
        if IMonths[t] then
           result:=TLogicalExpression.Join(result,EqualsMonth(t,TLogicalOperand.Equal),TLogicalOperand.&Or);
end;

{ TWeekdays }

procedure TWeekdays.Assign(Source: TPersistent);
begin
  if Source is TWeekdays then
     IWeekdays:=TWeekdays(Source).IWeekdays
  else
     inherited;
end;

function TWeekdays.Filter: TLogicalExpression;
var t : Integer;
    tmp : TDateTimePartExpression;
    tmpEquals : TLogicalExpression;
begin
  result:=nil;

  for t:=0 to High(IWeekdays) do
      if IWeekdays[t] then
      begin
        tmp:=TDateTimePartExpression.Create(IItem.DataExpression);
        tmp.Part:=TDateTimePart.WeekDay;

        tmpEquals:=TLogicalExpression.Create(tmp,TLogicalOperand.Equal,TIntegerExpression.Create(t+1));

        result:=TLogicalExpression.Join(result,tmpEquals,TLogicalOperand.&Or);
      end;
end;

function TWeekdays.Get(const Index: Integer): Boolean;
begin
  result:=IWeekdays[Index];
end;

procedure TWeekdays.Put(const Index: Integer; const Value: Boolean);
begin
  if IWeekdays[Index]<>Value then
  begin
    IWeekdays[Index]:=Value;
    DoChanged;
  end;
end;

procedure TWeekdays.Reset;
var t : Integer;
begin
  for t:=Low(IWeekdays) to High(IWeekdays) do
      IWeekdays[t]:=False;

  DoChanged;
end;

procedure TWeekdays.SetWeekDays(const Value: TBooleanArray);
var t : Integer;
begin
  for t:=0 to Value.Count-1 do
      IWeekdays[t]:=Value[t];
end;

function TWeekdays.ZeroOrAll: Boolean;
begin
  result:=CalcZeroOrAll(IWeekdays);
end;

{ TBooleanFilter }

procedure TBooleanFilter.Assign(Source: TPersistent);
begin
  if Source is TBooleanFilter then
     IValue:=TBooleanFilter(Source).IValue
  else
     inherited;
end;

function TBooleanFilter.Filter: TLogicalExpression;
begin
  if IValue[True]=IValue[False] then
     result:=nil
  else
  begin
    result:=IItem.NewLogical(TLogicalOperand.Equal);

    result.Right:=TBooleanExpression.Create(IValue[True]);
  end;
end;

function TBooleanFilter.GetFalse: Boolean;
begin
  result:=IValue[False];
end;

function TBooleanFilter.GetTrue: Boolean;
begin
  result:=IValue[True];
end;

procedure TBooleanFilter.Reset;
begin
  IValue[True]:=False;
  IValue[False]:=False;

  DoChanged;
end;

procedure TBooleanFilter.SetFalse(const Value: Boolean);
begin
  if IValue[False]<>Value then
  begin
    IValue[False]:=Value;
    DoChanged;
  end;
end;

procedure TBooleanFilter.SetTrue(const Value: Boolean);
begin
  if IValue[True]<>Value then
  begin
    IValue[True]:=Value;
    DoChanged;
  end;
end;

{ TDateTimeFilter }

Constructor TDateTimeFilter.Create(const AItem:TFilterItem);
begin
  inherited Create(AItem);

  FMonths:=TMonths.Create(AItem);
  FWeekdays:=TWeekdays.Create(AItem);
  FSelected:=TDateTimeSelected.Create(AItem);

  FQuantity:=1;
end;

Destructor TDateTimeFilter.Destroy;
begin
  FSelected.Free;
  FWeekdays.Free;
  FMonths.Free;

  inherited;
end;

procedure TDateTimeFilter.Assign(Source: TPersistent);
begin
  if Source is TDateTimeFilter then
  begin
    Months:=TDateTimeFilter(Source).Months;
    FPeriod:=TDateTimeFilter(Source).FPeriod;
    FQuantity:=TDateTimeFilter(Source).FQuantity;
    FStyle:=TDateTimeFilter(Source).FStyle;
    Selected:=TDateTimeFilter(Source).Selected;
    Weekdays:=TDateTimeFilter(Source).Weekdays;
  end
  else
    inherited;
end;

function TDateTimeFilter.Filter: TLogicalExpression;

  function DateOf:TDateExpression;
  begin
    result:=TDateExpression.Create(IItem.DataExpression);
  end;

  function SingleDay(const ADay:TDateExpression):TLogicalExpression;
  begin
    result:=TLogicalExpression.Create(DateOf,TLogicalOperand.Equal,ADay);
  end;

  function JoinAnd(const A,B:TExpression):TLogicalExpression;
  begin
    result:=TLogicalExpression.Create(A,TLogicalOperand.&And,B);
  end;

  function DateRange(const AFrom,ATo:TDateTime):TLogicalExpression;
  var tmpFirst,
      tmpLast : TDateTimeExpression;

      tmpFrom,
      tmpTo : TLogicalExpression;
  begin
    tmpFirst:=TDateTimeExpression.Create(AFrom);
    tmpLast:=TDateTimeExpression.Create(ATo);

    tmpFrom:=TLogicalExpression.Create(DateOf,TLogicalOperand.GreaterOrEqual,tmpFirst);
    tmpTo:=TLogicalExpression.Create(DateOf,TLogicalOperand.LowerOrEqual,tmpLast);
    result:=JoinAnd(tmpFrom,tmpTo);
  end;

  function MonthRange(const y,m:Word):TLogicalExpression;
  begin
    result:=DateRange(EncodeDate(y,m,1),EncodeDate(y,m,DaysInAMonth(y,m)));
  end;

  function ThisMonth(const ADelta:Integer):TLogicalExpression;
  var d,m,y : Word;
  begin
    DecodeDate(Date,y,m,d);

    if ADelta<>0 then
       IncAMonth(y,m,d,ADelta);

    result:=MonthRange(y,m);
  end;

  function ThisWeek(const ADelta:Integer):TLogicalExpression;
  var tmp : TDateTime;
  begin
    tmp:=Date+(7*ADelta);
    result:=DateRange(StartOfTheWeek(tmp),EndOfTheWeek(tmp));
  end;

  function YearOf:TDateTimePartExpression;
  begin
    result:=TDateTimePartExpression.Create(IItem.DataExpression);
    result.Part:=TDateTimePart.Year;
  end;

  function ThisYear(const ADelta:Integer):TLogicalExpression;
  var y : Word;
  begin
    y:=TBIDateTime.YearOf(Date)+ADelta;
    result:=TLogicalExpression.Create(YearOf,TLogicalOperand.Equal,TIntegerExpression.Create(y));
  end;

  function FilterPeriod(const AStyle:TDateTimeFilterStyle;
                        const AQuantity:Integer;
                        const ASpan:TDateTimeSpan):TLogicalExpression;
  var tmpFrom,
      tmpTo : TLogicalExpression;
  begin
    result:=nil;

    case AStyle of
      TDateTimeFilterStyle.This:
         case ASpan of
           TDateTimeSpan.Day: result:=SingleDay(TDateExpression.Today);
          TDateTimeSpan.Week: result:=ThisWeek(0);
         TDateTimeSpan.Month: result:=ThisMonth(0);
          TDateTimeSpan.Year: result:=ThisYear(0);
         end;

      TDateTimeFilterStyle.Next: begin
         case ASpan of
           TDateTimeSpan.Month: result:=ThisMonth(AQuantity);
            TDateTimeSpan.Week: result:=ThisWeek(AQuantity);
            TDateTimeSpan.Year: result:=ThisYear(AQuantity);
         else
          tmpFrom:=TLogicalExpression.Create(DateOf,TLogicalOperand.GreaterOrEqual,TDateExpression.Today);
          tmpTo:=TLogicalExpression.Create(DateOf,TLogicalOperand.Lower,TDateTimeExpression.DateSpan(AQuantity));
          result:=JoinAnd(tmpFrom,tmpTo);
         end;
       end;

      TDateTimeFilterStyle.Last: begin

         case ASpan of
           TDateTimeSpan.Month: result:=ThisMonth(-AQuantity);
            TDateTimeSpan.Week: result:=ThisWeek(-AQuantity);
            TDateTimeSpan.Year: result:=ThisYear(-AQuantity);
         else
        tmpFrom:=TLogicalExpression.Create(DateOf,TLogicalOperand.Greater,TDateTimeExpression.DateSpan(-AQuantity));
        tmpTo:=TLogicalExpression.Create(DateOf,TLogicalOperand.LowerOrEqual,TDateExpression.Today);
        result:=JoinAnd(tmpFrom,tmpTo);
         end;
       end;
    end;
  end;

begin
  if FSelected.Enabled then
     result:=FSelected.Filter
  else
  begin
    case FStyle of
         TDateTimeFilterStyle.All: result:=nil; // all time
      TDateTimeFilterStyle.Custom: result:=IItem.Numeric.RangeFilter;
       TDateTimeFilterStyle.Today: result:=SingleDay(TDateExpression.Today);
   TDateTimeFilterStyle.Yesterday: result:=SingleDay(TDateExpression.Yesterday);
    TDateTimeFilterStyle.Tomorrow: result:=SingleDay(TDateExpression.Tomorrow);
    else
       result:=FilterPeriod(FStyle,FQuantity,FPeriod);
    end;
  end;

  if not FMonths.ZeroOrAll then
     result:=TLogicalExpression.Join(result,FMonths.Filter);

  if not FWeekDays.ZeroOrAll then
     result:=TLogicalExpression.Join(result,FWeekDays.Filter);
end;

function TDateTimeFilter.GetFrom: TDateTime;
begin
  result:=IItem.Numeric.FromValue.Value;
end;

function TDateTimeFilter.GetFromEqual: Boolean;
begin
  result:=IItem.Numeric.FromValue.Equal;
end;

function TDateTimeFilter.GetTo: TDateTime;
begin
  result:=IItem.Numeric.ToValue.Value;
end;

function TDateTimeFilter.GetToEqual: Boolean;
begin
  result:=IItem.Numeric.ToValue.Equal;
end;

procedure TDateTimeFilter.Reset;
begin
  FMonths.Reset;
  FWeekdays.Reset;
  FQuantity:=1;
  FStyle:=TDateTimeFilterStyle.All;
  FPeriod:=TDateTimeSpan.None;
  FSelected.Reset;

  DoChanged;
end;

procedure TDateTimeFilter.SetFrom(const Value: TDateTime);
begin
  IItem.Numeric.FromValue.Value:=Value;
  IItem.Numeric.FromValue.Enabled:=True;
end;

procedure TDateTimeFilter.SetFromEqual(const Value: Boolean);
begin
  IItem.Numeric.FromValue.Equal:=Value;
end;

procedure TDateTimeFilter.SetMonths(const Value: TMonths);
begin
  FMonths.Assign(Value);
end;

procedure TDateTimeFilter.SetPeriod(const Value: TDateTimeSpan);
begin
  if FPeriod<>Value then
  begin
    FPeriod:=Value;
    DoChanged;
  end;
end;

procedure TDateTimeFilter.SetQuantity(const Value: Integer);
begin
  if FQuantity<>Value then
  begin
    FQuantity:=Value;
    DoChanged;
  end;
end;

procedure TDateTimeFilter.SetSelected(const Value: TDateTimeSelected);
begin
  FSelected.Assign(Value);
end;

procedure TDateTimeFilter.SetStyle(const Value: TDateTimeFilterStyle);
begin
  if FStyle<>Value then
  begin
    FStyle:=Value;
    DoChanged;
  end;
end;

procedure TDateTimeFilter.SetTo(const Value: TDateTime);
begin
  IItem.Numeric.ToValue.Value:=Value;
  IItem.Numeric.ToValue.Enabled:=True;
end;

procedure TDateTimeFilter.SetToEqual(const Value: Boolean);
begin
  IItem.Numeric.ToValue.Equal:=Value;
end;

procedure TDateTimeFilter.SetWeedays(const Value: TWeekdays);
begin
  FWeekdays.Assign(Value);
end;

{ TFilterItems }

Constructor TFilterItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TFilterItem);
end;

procedure TFilterItems.DoChanged(Sender:TObject);
begin
  IFilter.Changed;
end;

function TFilterItems.Add(const AData: TDataItem): TFilterItem;
begin
  result:=inherited Add as TFilterItem;
  result.Data:=AData;
end;

function TFilterItems.Filter: TExpression;
var t : Integer;
    tmp : TExpression;
begin
  result:=nil;

  for t:=0 to Count-1 do
      if Self[t].FEnabled then
      begin
        tmp:=Self[t].Filter;

        if tmp<>nil then
           if result=nil then
              result:=tmp
           else
              result:=TLogicalExpression.Create(result,TLogicalOperand.&And,tmp);
      end;
end;

function TFilterItems.Get(const Index: Integer): TFilterItem;
begin
  result:=TFilterItem(inherited Items[Index]);
end;

procedure TFilterItems.Put(const Index: Integer; const Value: TFilterItem);
begin
  inherited Items[Index]:=Value;
end;

{ TBIFilter }

Constructor TBIFilter.Create;
begin
  inherited Create;

  FEnabled:=True;

  FItems:=TFilterItems.Create(Self);
  FItems.IFilter:=Self;
end;

procedure TBIFilter.Delete(const AIndex: Integer);
begin
  Self[AIndex].{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
end;

Destructor TBIFilter.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TBIFilter.Changed;
begin
  if (not IUpdating) and Assigned(IChanged) then
     IChanged(Self);
end;

procedure TBIFilter.Clear;
begin
  Items.Clear;
  FText:='';
  ICustom:=nil;
end;

function TBIFilter.Custom: TLogicalExpression;
begin
  if FText='' then
     result:=TExpression.Clone(ICustom) as TLogicalExpression
  else
     result:=TLogicalExpression.FromString(FText) as TLogicalExpression
end;

function TBIFilter.Filter: TExpression;
begin
  if Enabled then
  begin
    result:=Items.Filter;

    if (FText<>'') or (ICustom<>nil) then
       if result=nil then
          result:=Custom
       else
          result:=TLogicalExpression.Create(result,TLogicalOperand.&And,Custom);
  end
  else
    result:=nil;
end;

function TBIFilter.Get(const AIndex: Integer): TFilterItem;
begin
  result:=Items[AIndex];
end;

function TBIFilter.IsItemsStored: Boolean;
begin
  result:=Items.Count>0;
end;

function TBIFilter.ItemOf(const AData: TDataItem): TFilterItem;
var t : Integer;
begin
  for t:=0 to Items.Count-1 do
      if Items[t].Data=AData then
         Exit(Items[t]);

  result:=nil;
end;

procedure TBIFilter.Put(const AIndex: Integer; const Value: TFilterItem);
begin
  Items[AIndex]:=Value;
end;

function TBIFilter.Add(const AData: TDataItem): TFilterItem;
begin
  result:=Items.Add(AData);
end;

function TBIFilter.Add(const AExpression: TLogicalExpression): TFilterItem;
begin
  result:=Items.Add(nil);
  result.FExpression:=AExpression;
end;

procedure TBIFilter.Assign(Source: TPersistent);
begin
  if Source is TBIFilter then
  begin
    FEnabled:=TBIFilter(Source).FEnabled;
    Items:=TBIFilter(Source).FItems;
    FText:=TBIFilter(Source).FText;
  end
  else
    inherited;
end;

procedure TBIFilter.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    Changed;
  end;
end;

procedure TBIFilter.SetItems(const Value: TFilterItems);
begin
  FItems.Assign(Value);
end;

procedure TBIFilter.SetText(const Value: String);
begin
  if FText<>Value then
  begin
    FText:=Value;
    Changed;
  end;
end;

{ TDateTimeSelected }

procedure TDateTimeSelected.Assign(Source: TPersistent);
begin
  if Source is TDateTimeSelected then
  begin
    FDateTime:=TDateTimeSelected(Source).FDateTime;
    FEnabled:=TDateTimeSelected(Source).FEnabled;
    FPart:=TDateTimeSelected(Source).FPart;
    FValue:=TDateTimeSelected(Source).FValue;
  end
  else
    inherited;
end;

function TDateTimeSelected.EqualPart:TLogicalExpression;
var tmp : TDateTimePartExpression;
begin
  result:=TLogicalExpression.Create;
  result.Operand:=TLogicalOperand.Equal;

  tmp:=TDateTimePartExpression.Create(IItem.DataExpression);
  tmp.Part:=FPart;

  result.Left:=tmp;
  result.Right:=TIntegerExpression.Create(FValue); // +Part.Low !!
end;

function TDateTimeSelected.Filter: TLogicalExpression;
begin
  if FEnabled then
  begin
    if Part=TDateTimePart.None then
    begin
      result:=IItem.NewLogical(TLogicalOperand.Equal);
      result.Right:=TFloatExpression.Create(FDateTime);
    end
    else
      result:=EqualPart;
  end
  else
    result:=nil;
end;

procedure TDateTimeSelected.Reset;
begin
  FValue:=0;
  FDateTime:=0;
  FEnabled:=False;
  FPart:=TDateTimePart.None;

  DoChanged;
end;

procedure TDateTimeSelected.SetDateTime(const Value: TDateTime);
begin
  if FDateTime<>Value then
  begin
    FDateTime:=Value;
    DoChanged;
  end;
end;

procedure TDateTimeSelected.SetEnabled(const Value: Boolean);
begin
  if FEnabled<>Value then
  begin
    FEnabled:=Value;
    DoChanged;
  end;
end;

procedure TDateTimeSelected.SetPart(const Value: TDateTimePart);
begin
  if FPart<>Value then
  begin
    FPart:=Value;
    DoChanged;
  end;
end;

procedure TDateTimeSelected.SetValue(const Value: Integer);
begin
  if FValue<>Value then
  begin
    FValue:=Value;
    DoChanged;
  end;
end;

{ TTextFilter }

procedure TTextFilter.Assign(Source: TPersistent);
begin
  if Source is TTextFilter then
  begin
    FStyle:=TTextFilter(Source).FStyle;
    FText:=TTextFilter(Source).FText;
  end
  else
    inherited;
end;

function TTextFilter.Filter: TExpression;

  function TextExpression:TTextExpression;
  begin
    result:=TTextExpression.Create(FText);
  end;

  function TextLogical(const AOperand:TTextLogicalOperand):TTextLogicalExpression;
  begin
    if FText='' then
       result:=nil
    else
    begin
      result:=TTextLogicalExpression.Create(IItem.DataExpression,AOperand,TextExpression);
      result.CaseSensitive:=CaseSensitive;
    end;
  end;

begin
  case FStyle of
    Contains: result:=TextLogical(TTextLogicalOperand.Contains);
     IsEqual: result:=TLogicalExpression.Create(IItem.DataExpression,TLogicalOperand.Equal,TextExpression);
      Starts: result:=TextLogical(TTextLogicalOperand.Starts);
        Ends: result:=TextLogical(TTextLogicalOperand.Ends);
  else
    begin
      result:=TUnaryTextExpression.Create(IItem.DataExpression);
      TUnaryTextExpression(result).Operand:=TTextUnaryOperand.IsEmpty;
    end;
  end;
end;

procedure TTextFilter.Reset;
begin
  FStyle:=TTextFilterStyle.Contains;
  FText:='';
end;

procedure TTextFilter.SetCase(const Value: Boolean);
begin
  if FCase<>Value then
  begin
    FCase:=Value;
    DoChanged;
  end;
end;

procedure TTextFilter.SetStyle(const Value: TTextFilterStyle);
begin
  if FStyle<>Value then
  begin
    FStyle:=Value;
    DoChanged;
  end;
end;

procedure TTextFilter.SetText(const Value: String);
begin
  if FText<>Value then
  begin
    FText:=Value;
    DoChanged;
  end;
end;

end.
