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
  // Available data types:
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
    // True when Kind is of types int32, int64, single, double or extended
    function IsNumeric:Boolean;

    // Returns TDataKind as a human readable string
    function ToString:String;
  end;

  TDataItem=class;

  // Represents "null" values of a data item.
  // The Items field contains a list of booleans (True=missing, False=not missing)
  TMissingData=record
  private
    // Returns True if the data value at Index position is null (missing)
    function GetItem(Index:TInteger):Boolean; inline;

    // Sets a data value at Index position to missing (True) or not missing (False)
    procedure SetItem(Index:TInteger; const Value:Boolean);
  public
    Count : TInteger; // <-- this exists to load it from *.bi streams.
    Items : TBooleanArray;

    function All:Boolean;
    procedure Init(const ACount:TInteger);

    property Item[Index:TInteger]:Boolean read GetItem write SetItem; default;
  end;

  // Simple array of TDataItem (ie: the equivalent of a database connection or group of datasets)
  TDataArray=Array of TDataItem;

  TDataArrayHelper=record helper for TDataArray
  public
    procedure Add(const AData:TDataItem);
    function Count:Integer; inline;
    procedure Delete(const Index:Integer);
    procedure Exchange(const A,B:Integer);
    function IndexOf(const AData:TDataItem):Integer; overload;
    function IndexOf(const AName:String):Integer; overload;
  end;

  // List of TDataItem.
  // Note: This class cannot be a simple TDataArray because when adding or
  // deleting an item, this item should be re-parented.
  TDataItems=class
  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FParent : TDataItem;

    FItems : TDataArray;

    function GetItem(const Index: Integer): TDataItem; inline;
    function GetNames(const AName:String):TDataItem;
    function GetLast: TDataItem;
  protected
    procedure AddDirect(const AData:TDataItem);
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

    function Find(const AName: String): TDataItem;
    function FromExpression(const AName: String; const AExpression: TExpression): TDataItem; overload;
    function FromExpression(const AName:String; const AExpression:String; const Error:TErrorProc=nil):TDataItem; overload;

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

  // When sorting data, each item in the sort order must be specified using
  // this record.
  TSortItem=record
  public
    Active : Boolean;

    Data : TDataItem; // The item we want the data to be sorted by.
    Descending: Boolean;  // Default is False (ie: Ascending sort order)

    // For Text Data Kind only:
    IgnoreTextCase: Boolean; // Default is False (ie: case-sensitive text order)
  end;

  // Abstract class to handle automatic filling of data into a TDataItem.
  // An example of use of this class is at TDelayHandlerStream (BI.Persist unit), that
  // enables delay-load of data from a stream.
  TDataProvider=class abstract
  protected
    {$IFDEF NEXTGEN}[Weak]{$ENDIF}FData : TDataItem;
    function GetStream(const AData,ANext:TDataItem):TStream; overload; virtual;
    function GetStream(const AItems:TDataArray):TStream; overload; virtual;
    procedure GetItems(const AData:TDataItem); virtual;
    procedure Load(const AData:TDataItem; const Children:Boolean); virtual; abstract;
  end;

  // Used by Summary class, when measuring numeric data as "Percentages"
  TNumericData=(Normal, Percentages);

  // Main class.
  // Several mode to represent:
  //  1) A "column" of data (ie: the equivalent of a dataset field) when Kind<>dkUnknown
  //  2) A "table" or "grid" (when AsTable=True)
  //  3) A "list" or collection of other data items (when Kind=dkUnknown and AsTable=False)

  TDataItem=class
  private
  type
    // Represents a master-detail relationship between one TDataItem and another
    TMasterDetail=record
    public
      Data : TDataItem;

      Index : TInt64Array;    // For self-detail relationship
      BitIndex : TInt64Array; // For multiple item search

      Multiplier : TInt32Array;  // Contains length of each BitIndex arrays (for speed)

      // Add a new entry in Index, accumulating the ACount parameter
      procedure AppendIndex(const ACount:Integer);

      // Returns the array of detail Indexes for given master AIndex position
      function GetIndex(const AMaster:TDataItem; const AIndex:TInteger):TInt64Array;
    end;

  var
    FDataMap : TDataMap;
    FHistory : TImportHistory;
    FKind    : TDataKind;
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

    IKeepParent : Boolean;

    function GetItem(const AName:String):TDataItem; inline;

    {$IFNDEF FPC}
    function GetItemByIndex(const AIndex:Integer):TDataItem; inline;
    {$ENDIF}
    
    function GetItems:TDataItems;

    function GetHistory:TImportHistory;
    function GetStats:TDataStats;
    function GetMaster: TDataItem;
    procedure SetMaster(const Value: TDataItem);
    procedure SetProvider(const Value: TDataProvider);
  protected
    FCount   : TInteger;

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

    procedure CalcHasDate;
    procedure CheckEmptyName;
    procedure ClearData;
    procedure ClearDelay;
    function CreateMap:TDataMap;
    function CreateStats:TDataStats;

    function HasItems:Boolean; inline;
    function HasMaster:Boolean;

    procedure SwapRows(const A,B:TInteger); virtual;
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

    // True when all values are distinct:
    Unique  : Boolean;

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
    Constructor Create(const Datas:TDataArray); overload;
    Constructor Create(const AProvider:TDataProvider); overload;

    Destructor Destroy; override;

    function Compare(const A,B:TInteger; const IgnoreTextCase:Boolean=False):SmallInt;
    procedure CopyFrom(const DestIndex:TInteger; const Source:TDataItem; const SourceIndex:TInteger);

    procedure CreateIndexMulti(const Items:TDataArray);
    procedure CreateMasterIndex;

    procedure Delete(const Row:TInteger);
    function DataToString(const Index:TInteger):String;
    function FindInMap(const Index:TInteger; out ABin:TInteger):Boolean;
    procedure Finish;
    function FullName:String;
    function IsChildOf(const AParent:TDataItem):Boolean;
    procedure Load(const Children:Boolean=False); virtual;
    procedure ReCalculate(const Parallel:Boolean=False); virtual;
    procedure Resize(const Size:TInteger);
    function SameData(const A,B:TInteger):Boolean;

    procedure SaveToFile(const AFile:String);

    procedure SortBy(const AData: TDataItem; const Ascending: Boolean=True; const IgnoreTextCase:Boolean=False); overload;
    procedure SortBy(const AExpression:String; const Ascending: Boolean=True; const IgnoreTextCase:Boolean=False); overload;
    procedure SortBy(const ASort: TSortItem); overload;
    procedure SortBy(const AExpression : TExpression; const Ascending: Boolean=True; const IgnoreTextCase:Boolean=False); overload;

    function TotalColumns: TInteger;
    function TotalRows: TInteger;
    procedure UnLoadData; virtual;

    property Count:TInteger read FCount write FCount;
    property DataMap:TDataMap read FDataMap;
    property History:TImportHistory read GetHistory;

    {$IFNDEF FPC}
    property Item[const AIndex:Integer]:TDataItem read GetItemByIndex; default;
    {$ENDIF}

    property Item[const Name:String]:TDataItem read GetItem; default;

    property Items:TDataItems read GetItems;
    property Kind:TDataKind read FKind write FKind;
    property Master:TDataItem read GetMaster write SetMaster;

    property Name:String read FName write FName;

    // The data owning this data:
    property Parent:TDataItem read FParent;

    // The object that will fill this TDataItem data when needed (just-in-time)
    property Provider:TDataProvider read FProvider write SetProvider;

    property Stats:TDataStats read GetStats;
  end;

  TDataHops=class;

  // Base class for Row-oriented and Column-oriented functions that can be used in expressions
  TColumnExpression=class(TUnaryExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); virtual; abstract;
    function KindOf:TDataKind; virtual; abstract;
    class function TryParse(const S:String):TColumnExpression; virtual; abstract;
  end;

  // Data Column created from an Expression
  TExpressionColumn=class(TDataItem)
  private
    procedure LoadData(const Item:TExpression);
    procedure SetExpression(const Value: TExpression);
  protected
    FExpression : TExpression;
  public
    Data : TDataItem;

    Constructor Create(const AExpression:TExpression); overload;
    Constructor Create(const AData:TDataItem; const AExpression:String); overload;

    Destructor Destroy; override;

    procedure Fill(const AParent:TDataItem=nil);
    procedure Load(const Children:Boolean=False); override;
    procedure SetValue(const ADest:TDataItem; const AIndex:TInteger);
    function Sort(const Ascending:Boolean):TInt64Array;

    property Expression:TExpression read FExpression write SetExpression;
  end;

  // Contains a list of Items used to sort a given data.
  TSortItems=record
  private
    procedure CheckExpression(const AItem:TSortItem; const AParent:TDataItem);
    function NotInOrder(const A,B:TInteger):Boolean;
    function SortCount(const AData:TDataItem): TInteger;
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
    function Sort(const AData:TDataItem): TInt64Array;

    // Orders AData, reordering its rows:
    procedure SortData(const AData:TDataItem);

    // Sort Items, Human-readable
    function ToString: String;
  end;

  // Helper memory-related methods
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

  // Represents the structure of a AData TDataItem
  TDataInfo=class(TDataItem)
  public
    Data : TDataItem;

    Constructor Create(const AData:TDataItem); overload;
  end;

  TIdentifiers=record
    // Geo World tree identifiers (Continents->Countries->Regions->Provinces->Cities, etc)
    // Colors ('Black'-> TAlphaColor.Black...)
  end;

  THops=class;

  // Represents a list of THops, used to access data from a detail data to its
  // master data.
  TDataHops=class
  private
    IChangeMain : Boolean;

    function IsDetail(const ADetail,AMaster:TDataItem):Boolean;
    procedure TraverseExpression(const Item:TExpression);
  public
    Datas : TDataArray;
    Main : TDataItem;
    Hops : Array of THops;

    Destructor Destroy; override;

    procedure Add(const AExpression:TExpression; const ChangeMain:Boolean); overload;
    function Add(const AData:TDataItem):Integer; overload;
    procedure Init;
    procedure Invalidate(const AIndex:TInteger);
    function Valid:Boolean;
  end;

  // Return an expression from a string.
  // The expression string might refer data Items or functions.
  TDataExpression=class
  protected
    class function Resolve(const AData:TDataItem; const AText:String;
                           const Error:TErrorProc):TExpression; virtual;
  public
    //class function FromString(const AData:TDataItem; const AExpression: String): TExpression; overload;
    class function FromString(const AData:TDataItem; const AExpression: String; const Error:TErrorProc=nil): TExpression;
    class function KindOf(const Expression:TExpression):TDataKind; static;
  end;

  // Same as TDataExpression, but it also validates the expression
  // is suitable as a filter (derives from TBaseLogicalExpression boolean expression)
  TDataFilter=record
  public
    class function FromString(const AData:TDataItem; const AExpression: String;
                              const Error:TErrorProc=nil): TExpression; static;

    class function VerifyLogical(const AExpression:TExpression;
                                      const AText: String;
                                      const Error:TErrorProc): TExpression; static;
  end;

  // Expression just containing a TDataItem
  TDataItemExpression=class(TExpression)
  private
    FData : TDataItem;
    KeepData : Boolean;
  public
    Hops : THops;

    Constructor Create(const AData:TDataItem; const FreeData:Boolean=False);
    Destructor Destroy; override;

    function IsLogical:Boolean;
    class procedure LookupFill(const ASource,ADest:TDataItem); static;
    class function NewLookup(const AName:String; const ADetail:TDataItem; const AMaster:TDataItem):TDataItem;
    function ToString:String; override;
    function Value:Variant; override;

    property Data:TDataItem read FData;
  end;

  // Lookups
  THops=class
  private
  type
    THop=class
    private
      IData : TDataItem; // Cached
      IsBool : Boolean;
      Inverted : Boolean;
      Index : TInt64Array;

      Data : TDataItem;
      Items : TDataArray; // Multiple

      function CreateInvertedIndex(const AIndex:TInt64Array):TInt64Array;
      function Find(const AIndex:TInteger):TInteger;
      procedure Prepare;
    public
      Constructor Create(const AData:TDataItem); overload;
      Constructor Create(const ADatas:TDataArray); overload;

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

  // A special class of data that refers to a Detail data.
  // ie: Products->Orders
  TDetailData=class(TDataItem)
  public
    Detail : TDataItem;
  end;

  TExpressionClass=class of TColumnExpression;

  // Globals list of custom registered functions that can be used in expressions.
  TDataFunctions=record
  private
    class function IndexOf(const AClass:TExpressionClass):Integer; static;
  public
    class var
      Count : Integer;
      Items : Array of TExpressionClass;

    class procedure Register(const AClass:TExpressionClass); static;
    class function TryParse(const S:String):TExpression; static;
    class procedure UnRegister(const AClass:TExpressionClass); static;
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
