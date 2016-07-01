{*********************************************}
{  TeeBI Software Library                     }
{  TBIFilter class using Expressions          }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expression.Filter;

interface

uses
  System.Classes, BI.Data, BI.Expression, BI.Data.CollectionItem,
  BI.Arrays;

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
  public
    Constructor Create(const AItem:TFilterItem; const AOperand:TLogicalOperand);

    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default False;
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

    procedure SetDateTime(const Value: TDateTime);
    procedure SetEnabled(const Value: Boolean);
    procedure SetPart(const Value: TDateTimePart);
    procedure SetValue(const Value: Integer);
  public
    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;
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
    function GetTo: TDateTime;
    procedure SetFrom(const Value: TDateTime);
    procedure SetMonths(const Value: TMonths);
    procedure SetPeriod(const Value: TDateTimeSpan);
    procedure SetQuantity(const Value: Integer);
    procedure SetSelected(const Value: TDateTimeSelected);
    procedure SetStyle(const Value: TDateTimeFilterStyle);
    procedure SetTo(const Value: TDateTime);
    procedure SetWeedays(const Value: TWeekdays);
  public
    Constructor Create(const AItem:TFilterItem);
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;

    property FromDate:TDateTime read GetFrom write SetFrom;
    property ToDate:TDateTime read GetTo write SetTo;
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

    function GetFalse: Boolean;
    function GetTrue: Boolean;
    procedure SetFalse(const Value: Boolean);
    procedure SetTrue(const Value: Boolean);
  public
    procedure Assign(Source:TPersistent); override;

    function Filter:TLogicalExpression;
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
  published
    property FromValue:TNumericValue read FFrom write SetFrom;
    property Selected:TNumericValue read FSelected write SetSelected;
    property ToValue:TNumericValue read FTo write SetTo;
  end;

  TFilterItem=class(TDataCollectionItem)
  private
    FBool : TBooleanFilter;
    FDateTime : TDateTimeFilter;
    FEnabled: Boolean;
    FExcluded: TStringList;
    FExpression : TLogicalExpression;
    FIncluded: TStringList;
    FNumeric : TNumericFilter;

    procedure DoChanged(Sender:TObject);
    function GetFilter: TLogicalExpression;
    function MainData:TDataItem;
    function NewLogical(const AOperand:TLogicalOperand):TLogicalExpression;
    procedure SetBool(const Value: TBooleanFilter);
    procedure SetDateTime(const Value: TDateTimeFilter);
    procedure SetEnabled(const Value: Boolean);
    procedure SetExcluded(const Value: TStringList);
    procedure SetIncluded(const Value: TStringList);
    procedure SetNumeric(const Value: TNumericFilter);
    procedure TryAdd(const AText:String; const A,B:TStrings);
  protected
    procedure Changed; override;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure ExcludeText(const AText:String; const Add:Boolean=True);
    procedure IncludeText(const AText:String; const Add:Boolean=True);

    property Expression:TLogicalExpression read FExpression write FExpression;
    property Filter:TLogicalExpression read GetFilter;
  published
    property BoolFilter:TBooleanFilter read FBool write SetBool;
    property DateTime:TDateTimeFilter read FDateTime write SetDateTime;
    property Enabled:Boolean read FEnabled write SetEnabled default True;
    property Excluded:TStringList read FExcluded write SetExcluded;
    property Included:TStringList read FIncluded write SetIncluded;
    property Numeric:TNumericFilter read FNumeric write SetNumeric;
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
    function Filter:TLogicalExpression;

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
    function Filter:TLogicalExpression;

    function ItemOf(const AData:TDataItem):TFilterItem;

    property Item[const AIndex:Integer]:TFilterItem read Get write Put; default;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default True;
    property Items:TFilterItems read FItems write SetItems stored IsItemsStored;
    property Text:String read FText write SetText;
  end;

implementation
