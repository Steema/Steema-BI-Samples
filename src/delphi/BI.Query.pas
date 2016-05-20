{*********************************************}
{  TeeBI Software Library                     }
{  TBIQuery Component                         }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Query;

interface

uses
  System.Classes, BI.Data, BI.DataSource, BI.Summary, BI.Expression,
  BI.Persist;

{
  TBIQuery is a component capable of executing queries against TDataItem objects.

  BIQuery.Items collection property contains the desired query output.
  Each Item defines a data item (field or table) and an optional aggregation kind.

  BIQuery automatically determines if the calculation must be done using
  a TSummary class (because there is at least one item with aggregation),
  or a TDataSelect class (a normal "select" query without any "Group By").

  Items can also define expressions (ie: "sum(a+b)" or "Upper(ProductName)" ) and
  can refer to data items from multiple databases or tables without the need to
  specify the links between them (no "join" clauses).

  The BIQuery Filter property can be set as a string (ie: City="London") or
  as an expression object, and can also refer to any data item even it is not
  included in the query output.
}

type
  TDataCollectionItem=class(TCollectionItem)
  private
    FData : TDataItem;
    FProvider : TComponent;

    FOnChange: TNotifyEvent;

    IOrigin : String;

    procedure AddNotify;
    function GetData: TDataItem;
    procedure InternalSetProvider(const Value: TComponent);
    function LoadOrigin:TDataItem;
    procedure Notify(const AEvent:TBIEvent);
    procedure NotifyDataDestroy(const AEvent:TBIEvent);
    function Origin:String;
    procedure ReadOrigin(Reader: TReader);
    procedure RemoveNotify;
    procedure SetData(const Value: TDataItem);
    procedure SetDataDirect(const Value: TDataItem);
    procedure SetProvider(const Value: TComponent);
    procedure WriteOrigin(Writer: TWriter);
  protected
    procedure Changed; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; virtual;
    function Owner:TComponent; virtual;
    procedure ValidateData(const AData:TDataItem); virtual;
  public
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Data:TDataItem read GetData write SetData;
    property Provider:TComponent read FProvider write SetProvider;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

  TBIQuery=class;

  TQueryItemStyle=(Automatic,Row,Column,Measure);

  TQueryItem=class(TDataCollectionItem)
  private
    FStyle: TQueryItemStyle;

    FSelectIndex : Integer;
    FSummaryItem : TSummaryItem;

    // Only used during csLoading, see Loaded
    IActive : Boolean;
    IAggregate : TAggregate;
    IDatePart : TDateTimePart;
    IExpression : TExpression;

    function CanChange:Boolean;
    procedure DoRemove;
    function GetAggregate: TAggregate;
    function GetActive: Boolean;
    function GetDatePart:TDateTimePart;
    function GetExpression: TExpression;
    function Query:TBIQuery;
    function RealData:TDataItem;
    procedure Recreate;
    procedure SetStyle(const Value: TQueryItemStyle);
    procedure SetActive(const Value: Boolean);
    procedure SetAggregate(const Value: TAggregate);
    procedure SetDatePart(const Value: TDateTimePart);
    procedure SetExpression(const Value: TExpression);
    procedure TryReconnect;
  protected
    procedure Changed; override;
    procedure Loaded; override;
    procedure ValidateData(const AData:TDataItem); override;
  public
    Constructor Create(Collection: TCollection); override;

    procedure Assign(Source:TPersistent); override;

    function GroupBy:TGroupBy;
    function Measure:TMeasure;
    function RealStyle:TQueryItemStyle;

    function ToString:String; override;

    property Expression:TExpression read GetExpression write SetExpression;
  published
    property Active:Boolean read GetActive write SetActive default True;
    property Aggregate:TAggregate read GetAggregate write SetAggregate default TAggregate.Count;
    property DatePart:TDateTimePart read GetDatePart write SetDatePart default TDateTimePart.None;
    property Style:TQueryItemStyle read FStyle write SetStyle default TQueryItemStyle.Automatic;
  end;

  TQueryItems=class(TOwnedCollection)
  private
    function AddItem(const AData: TDataItem;
                     const AStyle: TQueryItemStyle):TQueryItem;
    function Get(const Index: Integer): TQueryItem;
    function GetQuery: TBIQuery;
    procedure Put(const Index: Integer; const Value: TQueryItem);
    procedure SetupItem(const AItem:TQueryItem; const ForceSummary,IsActive:Boolean);
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    Constructor Create(AOwner: TPersistent);

    function Add(const AData:TDataItem;
                 const AStyle:TQueryItemStyle=TQueryItemStyle.Automatic;
                 const IsActive:Boolean=True): TQueryItem; overload;

    function Add(const AData:TDataItem;
                 const AMeasure:TAggregate): TQueryItem; overload;

    procedure Exchange(const A,B:TQueryItem);
    procedure Swap;

    property Items[const Index:Integer]:TQueryItem read Get write Put; default;
    property Query:TBIQuery read GetQuery;
  end;

  TQueryRemove=class(TPersistent)
  private
    IQuery : TBIQuery;

    procedure SetColumns(const Value: Boolean);
    procedure SetRows(const Value: Boolean);
    function GetColumns: Boolean;
    function GetRows: Boolean;
  published
    property Columns:Boolean read GetColumns write SetColumns default False;
    property Rows:Boolean read GetRows write SetRows default False;
  end;

  (* Pending
  TQueryFilter=class(TPersistent)
  private
    FEnabled : Boolean;
    FExpression : TLogicalExpression;
    FText : String;

    // Temporary during csLoading
    IFilter : String;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    property Expression:TLogicalExpression read FExpression write SetExpression;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default True;
    property Text:String read FText write SetText;
  end;
  *)

  TQueryStyle=(Unknown,Select,Summary);

  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TBIQuery=class(TBaseDataImporter)
  private
    FItems: TQueryItems;

    FOnError : TErrorProc;
    FSelect : TDataSelect;
    FSummary : TSummary;
    FRemove: TQueryRemove;

    // Temporary during csLoading
    IFilter : String;

    ILoading : Boolean;
    IClearing : Boolean;

    procedure AddItemsSelect(const ASelect:TDataSelect);
    procedure AddItemsSummary(const ASummary:TSummary);
    procedure DeleteSelect(const AIndex:Integer);
    procedure DoClearData(const AData:TDataItem);
    procedure DoSetFilter(const Value: String);
    function GetDistinct: Boolean;
    function GetFilter: String;
    function GetMax: Int64;
    procedure SetItems(const Value: TQueryItems);
    procedure SetSelect(const Value: TDataSelect);
    procedure SetSummary(const Value: TSummary);
    procedure SetDistinct(const Value: Boolean);
    procedure SetFilter(const Value: String);
    procedure SetMax(const Value: Int64);
    procedure SetRemove(const Value: TQueryRemove);
    procedure SetUseFilter(const Value: Boolean);
    function GetUseFilter: Boolean;
  protected
    procedure Changed; override;
    procedure GetItems(const AData: TDataItem); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Loaded; override;

    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;

    procedure SetFilterExpression(const AFilter:TLogicalExpression);

    property Select:TDataSelect read FSelect write SetSelect;
    property Summary:TSummary read FSummary write SetSummary;
  public
    Constructor Create(AOwner:TComponent); override;
    Constructor From(const AOwner:TComponent; const ASelect:TDataSelect); overload;
    Constructor From(const AOwner:TComponent; const ASummary:TSummary); overload;

    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function Calculate:TDataItem;
    class function CanBeMeasure(const AData:TDataItem):Boolean; static;
    procedure Clear;

    function IsEmpty:Boolean;
    function MainData:TDataItem;

    function Style:TQueryStyle;

    procedure Parse(const AData:TDataItem; const SQL:String);
    function ToString:String; override;

    property OnError:TErrorProc read FOnError write FOnError;
  published
    property Distinct:Boolean read GetDistinct write SetDistinct default False;
    property Filter:String read GetFilter write SetFilter;
    property Items:TQueryItems read FItems write SetItems;
    property MaxRows:Int64 read GetMax write SetMax default 0;
    property RemoveMissing:TQueryRemove read FRemove write SetRemove;
    property UseFilter:Boolean read GetUseFilter write SetUseFilter default True;
  end;

implementation
