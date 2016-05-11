unit BI.Query;

interface

uses
  System.Classes, BI.Data, BI.DataSource, BI.Summary, BI.Expression;

type
  TBIQuery=class;

  TQueryItemStyle=(Automatic,Row,Column,Measure);

  TQueryItem=class(TCollectionItem)
  private
    FData : TDataItem;
    FOnChange: TNotifyEvent;
    FProvider : TComponent;
    FStyle: TQueryItemStyle;

    FSelectIndex : Integer;
    FSummaryItem : TSummaryItem;

    // Only used during csLoading, see Loaded
    IActive : Boolean;
    IAggregate : TAggregate;

    procedure Changed;
    procedure DoRemove;
    function GetAggregate: TAggregate;
    function GetActive: Boolean;
    procedure Loaded;
    function Query:TBIQuery;
    procedure ReadOrigin(Reader: TReader);
    function RealData:TDataItem;
    procedure Recreate;
    procedure SetData(const Value: TDataItem);
    procedure SetStyle(const Value: TQueryItemStyle);
    procedure SetActive(const Value: Boolean);
    procedure SetAggregate(const Value: TAggregate);
    procedure ValidateNew(const AData:TDataItem);
    procedure WriteOrigin(Writer: TWriter);
    procedure SetProvider(const Value: TComponent);
 protected
    procedure DefineProperties(Filer: TFiler); override;
 public
    Constructor Create(Collection: TCollection); override;

    procedure Assign(Source:TPersistent); override;

    function GroupBy:TGroupBy;
    function Measure:TMeasure;
    function RealStyle:TQueryItemStyle;

    function ToString:String; override;
  published
    property Active:Boolean read GetActive write SetActive default True;
    property Aggregate:TAggregate read GetAggregate write SetAggregate default TAggregate.Count;
    property Data:TDataItem read FData write SetData;
    property Provider:TComponent read FProvider write SetProvider;
    property Style:TQueryItemStyle read FStyle write SetStyle default TQueryItemStyle.Automatic;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
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

  TQueryStyle=(Unknown,Select,Summary);

  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TBIQuery=class(TDataProvider)
  private
    FItems: TQueryItems;

    FOnError : TErrorProc;
    FSelect : TDataSelect;
    FSummary : TSummary;
    FRemove: TQueryRemove;
    FUseFilter : Boolean;

    procedure AddItemsSelect(const ASelect:TDataSelect);
    procedure AddItemsSummary(const ASummary:TSummary);
    procedure DeleteSelect(const AIndex:Integer);
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
  protected
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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
    property UseFilter:Boolean read FUseFilter write SetUseFilter default True;
  end;

implementation
