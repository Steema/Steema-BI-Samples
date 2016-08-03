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
  BI.Persist, BI.Expression.Filter, BI.Data.CollectionItem,
  BI.Data.Expressions;

{
  TBIQuery is a component capable of executing queries against TDataItem objects.

  The BIQuery Dimensions and Measures properties contain the desired output items.

  Each item defines a data item (field or table) and optional parameters like
  aggregation kind for Measures.

  BIQuery automatically determines if the calculation must be done using
  a TSummary class (because there is at least one item with aggregation),
  or a TDataSelect class (a normal "select" query without any "Group By").

  Items can also define expressions (ie: "sum(a+b)" or "Upper(ProductName)" ) and
  can refer to data items from multiple databases or tables without the need to
  specify the links between them (no "join" clauses).

  The BIQuery Filter property can be set as a string:

   Filter.Text:= 'City="London"';

  or as an expression object:

   Filter.Custom:= MyExpression;

  and can also refer to any data item, even if it is not included in the
  query output.

  The BIQuery SortBy property is a collection of TQuerySortItem objects.
  They define the sort order of the Query output rows and columns.
}

type
  TBIQuery=class;

  // Base class for Dimensions and Measures
  TQueryItem=class(TDataCollectionItem)
  private
    FEnabled: Boolean;

    procedure DoChanged(Sender:TObject);
    function GetQuery: TBIQuery;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Changed; override;
  public
    Constructor Create(Collection: TCollection); override;

    procedure Assign(Source:TPersistent); override;

    property Query:TBIQuery read GetQuery;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default True;
  end;

  TDimensionStyle=(Automatic,Row,Column);

  TQueryDimension=class(TQueryItem)
  private
    FGroupBy : TGroupBy;
    FStyle: TDimensionStyle;

    function GetDatePart:TDateTimePart;
    function GetExpression: TExpression;
    function GetHistogram: THistogram;

    procedure SetDatePart(const Value: TDateTimePart);
    procedure SetExpression(const Value: TExpression);
    procedure SetHistogram(const Value: THistogram);
    procedure SetStyle(const Value: TDimensionStyle);
  protected
    property GroupBy:TGroupBy read FGroupBy;
    function Layout:TGroupByLayout;
    procedure SetData(const Value: TDataItem); override;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    function RealData:TDataItem;
    function RealStyle:TDimensionStyle;
    function ToString:String; override;

    property Expression:TExpression read GetExpression write SetExpression;
  published
    property DatePart:TDateTimePart read GetDatePart write SetDatePart default TDateTimePart.None;
    property Histogram:THistogram read GetHistogram write SetHistogram;
    property Style:TDimensionStyle read FStyle write SetStyle default TDimensionStyle.Automatic;
  end;

  TQueryMeasure=class(TQueryItem)
  private
    FMeasure : TMeasure;

    function GetAggregate:TAggregate;
    function GetCalculation: TMeasureCalculation;
    function GetExpression: TExpression;
    function GetMissing: TMeasureMissing;

    procedure SetAggregate(const Value: TAggregate);
    procedure SetCalculation(const Value: TMeasureCalculation);
    procedure SetExpression(const Value: TExpression);
    procedure SetMissing(const Value: TMeasureMissing);
  protected
    property Measure:TMeasure read FMeasure;
    procedure SetData(const Value: TDataItem); override;
  public
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    property Expression:TExpression read GetExpression write SetExpression;
    function RealData:TDataItem;
    function ToString:String; override;
  published
    property Aggregate:TAggregate read GetAggregate write SetAggregate default TAggregate.Count;
    property Calculation : TMeasureCalculation read GetCalculation write SetCalculation;
    property Missing : TMeasureMissing read GetMissing write SetMissing;
  end;

  // Base class for Dimension and Measure collections
  TQueryCollection=class(TOwnedCollection)
  private
    IUpdating : Boolean;

    procedure DoChanged;
    function GetQuery: TBIQuery;
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    property Query:TBIQuery read GetQuery;
  end;

  // Zero or more Dimensions (to select or group-by)
  TQueryDimensions=class(TQueryCollection)
  private
    function AddItem(const AData: TDataItem;
                     const AStyle: TDimensionStyle):TQueryDimension;
    function EnabledCount:Integer;
    function Get(const Index: Integer): TQueryDimension;
    procedure Put(const Index: Integer; const Value: TQueryDimension);
    procedure Removed(const AComponent:TComponent);
  protected
    function Add(const AGroupBy:TGroupBy):TQueryDimension; overload;
  public
    Constructor Create(AOwner: TPersistent);

    function Add(const AData:TDataItem;
                 const AStyle:TDimensionStyle=TDimensionStyle.Automatic;
                 const IsActive:Boolean=True): TQueryDimension; overload;

    procedure Exchange(const A,B:TQueryDimension);
    function IndexOf(const ADimension:TQueryDimension):Integer;
    procedure Swap;

    property Items[const Index:Integer]:TQueryDimension read Get write Put; default;
  end;

  // Zero or more Aggregations (Sum, Count, Average, Min, Max)
  TQueryMeasures=class(TQueryCollection)
  private
    function Get(const Index: Integer): TQueryMeasure;
    procedure Put(const Index: Integer; const Value: TQueryMeasure);
    procedure Removed(const AComponent:TComponent);
  protected
    function Add(const AMeasure:TMeasure):TQueryMeasure; overload;
    function EnabledCount:Integer;
  public
    Constructor Create(AOwner: TPersistent);

    function Add(const AData:TDataItem;
                 const AMeasure:TAggregate;
                 const IsActive:Boolean=True): TQueryMeasure; overload;

    procedure Exchange(const A,B:TQueryMeasure);
    function IndexOf(const AMeasure:TQueryMeasure):Integer;

    property Items[const Index:Integer]:TQueryMeasure read Get write Put; default;
  end;

  TQuerySortItem=class(TQueryItem)
  private
    FAscending: Boolean;
    FIgnoreTextCase: Boolean; // Default is False (ie: case-sensitive text order)

    procedure SetAscending(const Value: Boolean);
    procedure SetIgnoreTextCase(const Value: Boolean);
  public
    Constructor Create(ACollection:TCollection); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Ascending:Boolean read FAscending write SetAscending default True;
    property IgnoreTextCase: Boolean read FIgnoreTextCase write SetIgnoreTextCase default True;
  end;

  TQuerySort=class(TQueryCollection)
  private
    procedure CopyFrom(const ASort:TSortItems);
    function Get(const Index: Integer): TQuerySortItem;
    procedure Put(const Index: Integer; const Value: TQuerySortItem);
    function Sort:TSortItems;
  public
    Constructor Create(AOwner: TPersistent);

    function AddSort(const AData:TDataItem;
                     const Ascending:Boolean=True;
                     const IgnoreTextCase:Boolean=True):TQuerySortItem;

    function ItemOf(const AData:TDataItem):TQuerySortItem;

    property Items[const Index:Integer]:TQuerySortItem read Get write Put; default;
  end;

  TQueryStyle=(Unknown,Select,Summary);

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  {$ENDIF}
  TBIQuery=class(TBaseDataImporter)
  private
    FDimensions: TQueryDimensions;
    FDistinct : Boolean;
    FFilter : TBIFilter;
    FMax : Int64;
    FMeasures: TQueryMeasures;
    FOnError : TErrorProc;
    FRemoveMissing : TRemoveMissing;
    FSort : TQuerySort;
    FStart : Int64;

    IMain : TDataItem;
    ILoading : Boolean;
    IClearing : Boolean;

    procedure AddDimensions(const ASummary:TSummary);
    procedure AddMeasures(const ASummary:TSummary);
    procedure DoChanged(Sender:TObject);
    procedure DoClearData(const AData:TDataItem);
    procedure SetDimensions(const Value: TQueryDimensions);
    procedure SetDistinct(const Value: Boolean);
    procedure SetFilter(const Value: TBIFilter);
    procedure SetMax(const Value: Int64);
    procedure SetMeasures(const Value: TQueryMeasures);
    procedure SetRemoveMissing(const Value: TRemoveMissing);
    procedure SetStartRow(const Value: Int64);
    procedure SetSort(const Value: TQuerySort);
    function IsDimensionsStored: Boolean;
    function IsMeasuresStored: Boolean;
    function IsSortStored: Boolean;
  protected
    procedure Changed; override;
    function CreateProvider:TDataProvider;
    procedure GetItems(const AData: TDataItem); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Loaded; override;

    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
  public
    Style : TQueryStyle;

    Constructor Create(AOwner:TComponent); override;
    Constructor From(const AOwner:TComponent; const ASelect:TDataSelect); overload;
    Constructor From(const AOwner:TComponent; const ASummary:TSummary); overload;

    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function Calculate:TDataItem;
    class function CanBeMeasure(const AData:TDataItem):Boolean; static;
    procedure Clear;

    procedure Parse(const AData:TDataItem; const SQL:String);
    procedure Refresh;
    function ToString:String; override;

    property Main:TDataItem read IMain;
    property OnError:TErrorProc read FOnError write FOnError;
  published
    property Dimensions:TQueryDimensions read FDimensions write SetDimensions stored IsDimensionsStored;
    property Distinct:Boolean read FDistinct write SetDistinct default False;
    property Filter:TBIFilter read FFilter write SetFilter;
    property MaxRows:Int64 read FMax write SetMax default 0;
    property Measures:TQueryMeasures read FMeasures write SetMeasures stored IsMeasuresStored;
    property RemoveMissing:TRemoveMissing read FRemoveMissing write SetRemoveMissing;
    property SortBy:TQuerySort read FSort write SetSort stored IsSortStored;
    property StartRow:Int64 read FStart write SetStartRow default 0;
  end;

implementation
