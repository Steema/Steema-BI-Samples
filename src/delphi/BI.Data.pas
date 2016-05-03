{*********************************************}
{  TeeBI Software Library                     }
{  Main TDataItem classes                     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data;

interface

uses
  {$IFNDEF FPC}

  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  {$ENDIF}

  System.Diagnostics,
  {$ENDIF}

  {$IFDEF FPC}
  BI.FPC,
  {$ENDIF}

  System.Classes, System.SysUtils,
  BI.Arrays, BI.Streams, BI.Expression;

type
  TDataKind=(dkInt32,
             dkInt64,
             dkSingle,
             dkDouble,
             dkExtended,
             dkText,
             dkDateTime,
             dkBoolean,
             dkUnknown);


  TDataKindHelper=record helper for TDataKind
  public
    class function FromString(const AText:String; out AKind:TDataKind):Boolean; static;

    // True when Kind is of types int32, int64, single, double or extended
    function IsNumeric:Boolean;

    // Returns TDataKind as a human readable string
    function ToString:String;
  end;

  TMissingData=record
  private
    // Returns True if the data value at Index position is null (missing)
    function GetItem(const Index:TInteger):Boolean; inline;

    // Sets a data value at Index position to missing (True) or not missing (False)
    procedure SetItem(const Index:TInteger; const Value:Boolean);
  public
    Count : TInteger;
    Items : TBooleanArray;

    function All:Boolean;
    procedure Init(const ACount:TInteger);
    function MissingCount:TInteger;

    property Item[const Index:TInteger]:Boolean read GetItem write SetItem; default;
  end;

  TDataItem=class;

  TDataArray=Array of TDataItem;

  TDataArrayHelper=record helper for TDataArray
  public
    procedure Add(const AData:TDataItem);
    function Count:Integer; inline;
    procedure Delete(const Index:Integer);
    procedure Exchange(const A,B:Integer);
    function Find(const AName:String):TDataItem;
    function IndexOf(const AData:TDataItem):Integer; overload;
    function IndexOf(const AName:String):Integer; overload;
    procedure Insert(const AData:TDataItem; const AIndex:Integer);
  end;

  TDataItems=class
  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FParent : TDataItem;

    FItems : TDataArray;

    procedure DuplicateError(const AName:String);
    function GetItem(const Index: Integer): TDataItem; inline;
    function GetNames(const AName:String):TDataItem;
    function GetLast: TDataItem;
    procedure InsertData(const AtIndex:TInteger);
  protected
    procedure AddDirect(const AData:TDataItem);
    procedure DestroyAll;
    function GetEmptyName:String;
  public
    Constructor Create(const AParent:TDataItem);
    Destructor Destroy; override;

    procedure Add(const AData:TDataItem); overload;
    function Add(const AName: String; const AKind: TDataKind; const Tag:TObject=nil): TDataItem; overload;
    function Add(const AName,AExpression: String): TDataItem; overload;
    function Add(const AName:String; const AExpression: TExpression): TDataItem; overload;

    procedure Clear;
    function Count:Integer; inline;
    procedure Delete(const AData:TDataItem);
    function Exists(const AName:String):Boolean; inline;

    function Find(const AName: String): TDataItem; inline;
    function FromExpression(const AName: String; const AExpression: TExpression): TDataItem; overload;
    function FromExpression(const AName:String; const AExpression:String; const Error:TErrorProc=nil):TDataItem; overload;

    function IndexOf(const AName:String):Integer;
    procedure Insert(const AData:TDataItem; const AIndex:Integer);

    function New(const AName:String; const AKind:TDataKind; const Tag:TObject=nil):TDataItem;
    procedure Resize(const Size:TInteger);
    function Select(const Indices:Array of Integer):TDataArray; overload;
    function Select(const Start,Finish:Integer):TDataArray; overload;

    property AsArray:TDataArray read FItems;
    property Item[const Index:Integer]:TDataItem read GetItem; default;
    property Last:TDataItem read GetLast;
    property Names[const Index:String]:TDataItem read GetNames;
    property Parent:TDataItem read FParent;
  end;

  TDataTimes=record
  public
    Opening     : Cardinal;
    LoadingInfo : Cardinal;
    LoadingData : Cardinal;
    Calculating : Cardinal;
    Total       : Cardinal;
  end;

  TImportHistory=class
  public
    DateTime : TDateTime;
    Times    : TDataTimes;
    Memory   : TInteger;

    Constructor Create;
  end;

  TSortItem=record
  public
    Active : Boolean;

    Data : TDataItem; // The item we want the data to be sorted by.
    Descending: Boolean;  // Default is False (ie: Ascending sort order)

    // For Text Data Kind only:
    IgnoreTextCase: Boolean; // Default is False (ie: case-sensitive text order)
  end;

  // Event Observers (Consumers)

  TBIEvent=(Changed,Destroyed);
  TBIConsumer=procedure(const AEvent:TBIEvent) of object;

  TBIConsumers=record
  private
    Items : TArray<TBIConsumer>;
  public
    procedure Add(const AConsumer:TBIConsumer);
    procedure Broadcast(const AEvent:TBIEvent);
    function IndexOf(const AConsumer:TBIConsumer):Integer;
    procedure Remove(const AConsumer:TBIConsumer);
  end;

  // Base class for TDataItem "Providers"
  TDataProvider=class(TComponent)
  protected
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FData : TDataItem;

    var
      FConsumers : TBIConsumers;

    function GetStream(const AData,ANext:TDataItem):TStream; overload; virtual;
    function GetStream(const AItems:TDataArray):TStream; overload; virtual;
    procedure GetItems(const AData:TDataItem); virtual;
    procedure Load(const AData:TDataItem; const Children:Boolean); overload; virtual; abstract;
    function Origin:String; virtual;
  public
    Destructor Destroy; override;

    function NewData:TDataItem;
  end;

  TNumericData=(Normal, Percentages);

  TDataItem=class
  private
    procedure SetName(const Value: String);
  type
    TMasterDetail=record
    public
      Data : TDataItem;

      Index : TNativeIntArray;    // For self-detail relationship
      BitIndex : TNativeIntArray; // For multiple item search

      Multiplier : TInt32Array;  // Contains length of each BitIndex arrays (for speed)

      // Add a new entry in Index, accumulating the ACount parameter
      procedure AppendIndex(const ACount:Integer);

      // Returns the array of detail Indexes for given master AIndex position
      function GetIndex(const AMaster:TDataItem; const AIndex:TInteger):TNativeIntArray;
    end;

  var
    FDataMap : TDataMap;
    FHistory : TImportHistory;
    FItems   : TDataItems;

    // The data description:
    FName : String;

    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FParent : TDataItem;

    // Provider (default is nil), is responsible to obtain and fill this TDataItem
    // data and structure. For example when "delay-loading" just-in-time from a BIWeb or disk file,
    // or when this DataItem is the output of a query or algorithm calculation.
    FProvider : TDataProvider;

    FStartTime: TStopwatch;

    //IKeepParent : Boolean;

    function GetItem(const AName:String):TDataItem; inline;

    {$IFNDEF FPC}
    function GetItemByIndex(const AIndex:Integer):TDataItem; inline;
    {$ENDIF}

    function GetItems:TDataItems;

    function GetHistory:TImportHistory;
    function GetStats:TDataStats;
    function GetMaster: TDataItem;
    procedure Notify(const AEvent:TBIEvent);
    procedure SetMaster(const Value: TDataItem);
    procedure SetProvider(const Value: TDataProvider);
  protected
    FCount   : TInteger;
    FKind    : TDataKind;
    FStats   : TDataStats;

    //[Weak]
    TagObject : TObject;

    // When summaries groupby Week, Month or Quarter, they do it using the
    // group integer number. This field allows displaying it as a string in grids, etc.
    IDate : TDateTimePart;
    IHasDate : Boolean;

    SavedDelayPos,
    DelayPos : Int64;

    // When True, Provider is NOT destroyed at TDataItem destructor
    KeepProvider : Boolean;

    // foreign key
    IMaster : TMasterDetail;

    // Used only during link loading:
    IMasterOrigin : String;

    FUnique : Boolean;

    // MaxTextLength overrides automatic calculation at TBIDBEngine.AddFields and
    // at TBIDataSetSource.AddItemField

    IMaxTextLength : Integer;

    FConsumers : TBIConsumers;

    function MaxTextLength:Integer;

    procedure SetInternalDate(const ADate:TDateTimePart);
    procedure CheckEmptyName;
    procedure ClearData;
    procedure ClearDelay;
    procedure CloneData(const ASource:TDataItem; const AStart,ACount:TInteger);
    function CreateMap:TDataMap;
    function CreateStats:TDataStats;

    function ExistsBefore(const AIndex:TInteger):Boolean;

    function GetProvider:TDataProvider;

    function HasItems:Boolean; inline;
    function HasMaster:Boolean;

    procedure SwapRows(const A,B:TInteger); //virtual;
  public
    // Arrays containing the data values:
    Int32Data : TInt32Array;
    Int64Data : TInt64Array;
    SingleData : TSingleArray;
    DoubleData : TDoubleArray;
    ExtendedData : TExtendedArray;
    TextData : TTextArray;
    DateTimeData : TDateTimeArray;
    BooleanData : TBooleanArray;

    // Null values:
    Missing : TMissingData;

    // True when this data is the "primary key" of its parent:
    Primary : Boolean;

    // True when this data items are representing a "grid" (a table):
    AsTable : Boolean;

    // Internal stats:
    LastAccess : TDateTime;
    UsedMemory : Cardinal;

    // Used for example when calculating summaries, ParentGroup is the data that
    // is the "master" of this data.
    // Also used when hiding "duplicate" values in TBIGrid, and when calculating the
    // "tree" of values to create multiple charts.
    ParentGroup : TDataItem;

    // Defines if this data values are numeric or percentages %
    NumericValues : TNumericData;

    Constructor Create(const Table:Boolean=False); overload; virtual;
    Constructor Create(const AData:TDataArray); overload;
    Constructor Create(const AProvider:TDataProvider); overload;
    Constructor Create(const AKind: TDataKind); overload;

    class function LoadFromFile(const AFileName:String):TDataItem; inline;

    Destructor Destroy; override;

    procedure Clear;

    function Compare(const A,B:TInteger; const IgnoreTextCase:Boolean=False):SmallInt;
    procedure CopyFrom(const DestIndex:TInteger; const Source:TDataItem; const SourceIndex:TInteger);

    procedure CreateIndexMulti(const Items:TDataArray);
    procedure CreateMasterIndex;

    procedure Delete(const Row:TInteger; const ACount:TInteger=1);
    function DataToString(const Index:TInteger):String;
    function FindInMap(const Index:TInteger; out ABin:TNativeInteger):Boolean;
    procedure Finish;
    function FullName:String;
    procedure Insert(const AtIndex:TInteger; const AMissing:Boolean=True);
    function IsChildOf(const AParent:TDataItem):Boolean;
    procedure Load(const Children:Boolean); overload; virtual;
    procedure Load; overload; inline;
    procedure ReCalculate(const Parallel:Boolean=False); virtual;
    procedure Resize(const Size:TInteger);
    function SameData(const A,B:TInteger):Boolean;

    procedure SaveToFile(const AFileName:String); inline;

    procedure SortBy(const AData: TDataItem; const Ascending: Boolean=True; const IgnoreTextCase:Boolean=False); overload;
    procedure SortBy(const AExpression:String; const Ascending: Boolean=True; const IgnoreTextCase:Boolean=False); overload;
    procedure SortBy(const ASort: TSortItem); overload;
    procedure SortBy(const AExpression : TExpression; const Ascending: Boolean=True; const IgnoreTextCase:Boolean=False); overload;

    function TotalColumns: TInteger;
    function TotalRows: TInteger; // Int64 as it might overflow in 32bit cpu
    procedure UnLoadData; virtual;

    property Count:TInteger read FCount;
    property DataMap:TDataMap read FDataMap;
    property History:TImportHistory read GetHistory;

    {$IFNDEF FPC}
    property Item[const AIndex:Integer]:TDataItem read GetItemByIndex; default;
    {$ENDIF}

    property Item[const Name:String]:TDataItem read GetItem; default;

    property Items:TDataItems read GetItems;
    property Kind:TDataKind read FKind; // <-- Read-only. It should never be made writeable
    property Master:TDataItem read GetMaster write SetMaster;

    property Name:String read FName write SetName;

    // The data owning this data:
    property Parent:TDataItem read FParent;

    // The object that will fill this TDataItem data when needed (just-in-time)
    property Provider:TDataProvider read FProvider write SetProvider;

    property Stats:TDataStats read GetStats;

    // True when all values are distinct:
    property Unique:Boolean read FUnique;
  end;

  TDataHops=class;

  TColumnExpression=class(TUnaryExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); virtual; abstract;
    function KindOf:TDataKind; virtual; abstract;
    class function TryParse(const S:String):TColumnExpression; virtual; abstract;
  end;

  TExpressionColumn=class(TDataItem)
  private
    procedure LoadData(const Item:TExpression);
    procedure SetExpression(const Value: TExpression);
  protected
    FExpression : TExpression;

    procedure SetValue(const ADest:TDataItem; const AIndex:TInteger);
  public
    Data : TDataItem;

    Constructor Create(const AExpression:TExpression); overload;
    Constructor Create(const AData:TDataItem; const AExpression:String); overload;

    Destructor Destroy; override;

    procedure Fill(const AParent:TDataItem=nil);
    procedure Load(const Children:Boolean=False); override;
    function Sort(const Ascending:Boolean=True):TNativeIntArray;

    property Expression:TExpression read FExpression write SetExpression;
  end;

  TSortItems=record
  private
    procedure CheckExpression(const AItem:TSortItem; const AParent:TDataItem);
    function DataCount(const AData:TDataItem):TInteger;
    function NotInOrder(const A,B:TInteger):Boolean;
    procedure VerifySort(const AData:TDataItem);
  public
    Items : Array of TSortItem;

    function ActiveCount:Integer;

    procedure Add(const AData:TDataItem; const Ascending:Boolean=True; const IgnoreTextCase:Boolean=False); overload;

    procedure Clear;
    function Count:Integer; inline;

    procedure Delete(const AIndex:Integer);
    procedure Exchange(const A,B:Integer);

    function IndexOf(const AData:TDataItem):Integer;

    // Switches sort order from ascending to descending and viceversa:
    function InvertOrder(const AData:TDataItem):Boolean;

    // Calculates and returns the sorted array of indexes,
    // without reordering AData rows:
    function Sort(const AData:TDataItem): TNativeIntArray; overload;
    function Sort(const AData:TDataItem; const AIndex:TNativeIntArray): TNativeIntArray; overload;

    // Orders AData, reordering its rows:
    procedure SortData(const AData:TDataItem);

    // Sort Items, Human-readable
    function ToString: String;
  end;

  TMemory=record
  private
    {$IFNDEF FPC}
    {$IFDEF MSWINDOWS}
    class function Status:MemoryStatusEx; static;
    {$ENDIF}
    {$ENDIF}
  public
    class var MinLimit:UInt64; // "Desired" minimum amount of free memory

    class procedure Init; static;

    class function Allocated:Int64; static;
    class function Available:UInt64; static;
    class function TotalPhysical:UInt64; static;
  end;

  TDataInfo=class(TDataItem)
  private
    FData : TDataItem;
  public
    Constructor Create(const AData:TDataItem); overload;

    property Data:TDataItem read FData;
  end;

  TIdentifiers=record
    // Geo World tree identifiers (Continents->Countries->Regions->Provinces->Cities, etc)
    // Colors ('Black'-> TAlphaColor.Black...)
  end;

  THops=class
  private
  type
    THop=class
    private
      IData : TDataItem; // Cached
      IsBool : Boolean;
      Inverted : Boolean;
      Index : TNativeIntArray;

      Data : TDataItem;
      Items : TDataArray; // Multiple

      function CreateInvertedIndex(const AIndex:TNativeIntArray):TNativeIntArray;
      function Find(const AIndex:TInteger):TInteger;
      procedure Prepare;
    public
      Constructor Create(const AData:TDataItem); overload;
      Constructor Create(const AData:TDataArray); overload;

      function ToString:String; override;
    end;

    THopArray=Array of THop;

    THopArrayHelper=record helper for THopArray
    public
      procedure Add(const Hop:THop);
      procedure AddTop(const Hop:THop);
      function Find(const Index:TInteger):TInteger;
    end;

    function InternalFind(const Start,Dest:TDataItem; var Visited:TDataArray):THops.THopArray;

  public
  var
    Items : THopArray;
    RealSource : TDataItem;
    Valid : Boolean;
    SourceIndex : TInteger;

    Destructor Destroy; override;

    function Index:TInteger;
    procedure Init(const ADetail,AMaster:TDataItem);
  end;

  TDataHops=class
  private
    IChangeMain : Boolean;

    function IsDetail(const ADetail,AMaster:TDataItem):Boolean;
    procedure TraverseExpression(const Item:TExpression);
  public
    Data : TDataArray;
    Main : TDataItem;
    Hops : Array of THops;

    Destructor Destroy; override;

    procedure Add(const AExpression:TExpression; const ChangeMain:Boolean); overload;
    function Add(const AData:TDataItem):Integer; overload;
    procedure Init;
    procedure Invalidate(const AIndex:TInteger);
    function Valid:Boolean;
  end;

  TDataExpression=class
  public
    type
      TResolver={$IFNDEF FPC}reference to{$ENDIF} function(const AData:TDataItem;
                                      const AExpression: String;
                                      const Error:TErrorProc=nil): TExpression;

  private
    class var
      FResolver : TResolver;
  protected
    class function Resolve(const AData:TDataItem; const AText:String;
                           const Error:TErrorProc):TExpression; virtual;
  public
    class function FromString(const AData:TDataItem; const AExpression: String; const Error:TErrorProc=nil): TExpression;
    class function KindOf(const Expression:TExpression):TDataKind; static;

    class property Resolver:TResolver read FResolver write FResolver;
  end;

  TDataFilter=record
  public
    class function FromString(const AData:TDataItem; const AExpression: String;
                              const Error:TErrorProc=nil): TExpression; static;

    class function VerifyLogical(const AExpression:TExpression;
                                      const AText: String;
                                      const Error:TErrorProc): TExpression; static;
  end;

  TDataItemExpression=class(TExpression)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FData : TDataItem;

    FHops : THops;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FMain : TDataItem;

    KeepData : Boolean;
  protected
    procedure Notify(const AEvent:TBIEvent);
  public
    Constructor Create(const AData:TDataItem; const FreeData:Boolean=False; const AMain:TDataItem=nil);
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    function IsLogical:Boolean;
    class procedure LookupFill(const ASource,ADest:TDataItem); static;
    class function NewLookup(const AName:String; const ADetail:TDataItem; const AMaster:TDataItem):TDataItem;
    function ToString:String; override;
    function Value:TData; override;

    property Data:TDataItem read FData;
    property Hops:THops read FHops;
  end;

  TDetailData=class(TDataItem)
  public
    Detail : TDataItem;
  end;

  TColumnExpressionClass=class of TColumnExpression;

  TDataFunctions=record
  private
    class function IndexOf(const AClass:TColumnExpressionClass):Integer; static;
  public
    class var
      Count : Integer;
      Items : Array of TColumnExpressionClass;

    class procedure Register(const AClass:TColumnExpressionClass); static;
    class function TryParse(const S:String):TExpression; static;
    class procedure UnRegister(const AClass:TColumnExpressionClass); static;
  end;

  TIsNullData=class(TColumnExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); override;
    function KindOf:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    function Value:TData; override;
  end;

implementation
