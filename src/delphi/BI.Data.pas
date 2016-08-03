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
  BI.Arrays, BI.Expression;

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


  // Helper methods for TDataKind enumeration type

  TDataKindHelper=record helper for TDataKind
  public
    class function FromString(const AText:String; out AKind:TDataKind):Boolean; static;

    // True when Kind is of types int32, int64, single, double or extended
    function IsNumeric:Boolean;

    // Returns TDataKind as a human readable string
    function ToString:String;
  end;

  TDataItem=class;

  // Maintains the list of "null" (empty / missing) values of a TDataItem
  // as a TBooleanArray

  TMissingData=record
  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    IData : TDataItem;

    procedure DestroyStats; inline;

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

  TDataArray=Array of TDataItem;

  // Helper methods for TDataArray

  TDataArrayHelper=record helper for TDataArray
  public
    procedure Add(const AData:TDataItem);
    function Count:Integer; inline;
    procedure Delete(const Index:Integer);
    procedure Exchange(const A,B:Integer);
    function Exists(const AData:TDataItem):Boolean; inline;
    function Find(const AName:String):TDataItem;
    function IndexOf(const AData:TDataItem):Integer; overload;
    function IndexOf(const AName:String):Integer; overload;
    procedure Insert(const AData:TDataItem; const AIndex:Integer);
  end;

  // Contains the list of sub-items of a TDataItem

  TDataItems=class
  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FParent : TDataItem;

    FItems : TDataArray;

    ISort : TTextArray;

    procedure DuplicateError(const AName:String);
    function GetItem(const Index: Integer): TDataItem; inline;
    function GetNames(const AName:String):TDataItem;
    function GetLast: TDataItem;
    procedure InsertData(const AtIndex:TInteger);
    procedure Swap(const A,B:TInteger);
  protected
    Valid : Boolean;

    procedure AddDirect(const AData:TDataItem);
    procedure DestroyAll;
    function GetEmptyName:String;
  public
    Constructor Create(const AParent:TDataItem);
    Destructor Destroy; override;

    procedure Add(const AData:TDataItem); overload;
    function Add(const AName: String; const AKind: TDataKind; const Tag:TObject=nil): TDataItem; overload;

    procedure Clear;
    function Count:Integer; inline;
    procedure Delete(const AData:TDataItem);
    function Exists(const AName:String):Boolean; inline;

    function Find(const AName: String): TDataItem; inline;

    function IndexOf(const AName:String):Integer;
    procedure Insert(const AData:TDataItem; const AIndex:Integer);

    function New(const AName:String; const AKind:TDataKind; const Tag:TObject=nil):TDataItem;
    procedure Resize(const Size:TInteger);
    procedure Reverse;
    function Select(const Indices:Array of Integer):TDataArray; overload;
    function Select(const Start,Finish:Integer):TDataArray; overload;
    procedure SortByName(const Ascending:Boolean=True; const IgnoreCase:Boolean=True);

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
    Items : {$IFDEF FPC}Array of TBIConsumer{$ELSE}TArray<TBIConsumer>{$ENDIF};
  public
    Changing : Integer;

    procedure Add(const AConsumer:TBIConsumer);
    procedure Broadcast(const AEvent:TBIEvent);
    function IndexOf(const AConsumer:TBIConsumer):Integer;
    procedure Remove(const AConsumer:TBIConsumer);
  end;

  // Base class for TDataItem "Providers".
  // See TDataItem.Provider property

  TDataProvider=class(TComponent)
  private
    FTitle : String;
  protected
    FConsumers : TBIConsumers;

    procedure BeginUpdate; inline;
    procedure EndUpdate;
    procedure Changed; virtual;
    function Changing:Boolean;
    function GetStream(const AData,ANext:TDataItem):TStream; overload; virtual;
    function GetStream(const AItems:TDataArray):TStream; overload; virtual;
    procedure GetItems(const AData:TDataItem); virtual;
    procedure Load(const AData:TDataItem; const Children:Boolean); overload; virtual; abstract;
  public
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function NewData:TDataItem;
  published
    property Title:String read FTitle write FTitle;
  end;

  TNumericData=(Normal, Percentages);

  // Main class.
  // TDataItem enables creating complex in-memory structures of column-based
  // tables and trees.

  TDataItem=class
  private
    procedure SetName(const Value: String);
  type
    TMasterDetail=record
    private
    public
      Data : TDataItem;

      Index : TNativeIntArray;    // For self-detail relationship
      BitIndex : TNativeIntArray; // For multiple item search

      Multiplier : TInt32Array;  // Contains length of each BitIndex arrays (for speed)

      // Used only during link loading:
      Origin : String;

      // Add a new entry in Index, accumulating the ACount parameter
      procedure AppendIndex(const ACount:Integer);

      // Direct assign
      procedure Assign(const Value:TMasterDetail);

      // Returns the array of detail Indexes for given master AIndex position
      function GetIndex(const AMaster:TDataItem; const AIndex:TInteger):TNativeIntArray;

      // Returns True when Origin='..', meaning our Master is our Parent (self-detail)
      function IsSelfDetail:Boolean;
    end;

  var
    FHistory : TImportHistory;
    FItems   : TDataItems;

    // The data description:
    FName : String;

    // Provider (default is nil), is responsible to obtain and fill this TDataItem
    // data and structure. For example when "delay-loading" just-in-time from a BIWeb or disk file,
    // or when this DataItem is the output of a query or algorithm calculation.
    FProvider : TDataProvider;

    FStartTime: TStopwatch;

    // Temporary. Anti re-entrancy, see usage
    IGettingItems : Boolean;

    procedure DestroyStats;

    function FindMapRow(const AMap:TDataMap; const ARow:TLoopInteger; out AIndex:TNativeInteger):Boolean; overload;
    function FindMapRow(const ARow:TLoopInteger; out AIndex:TNativeInteger):Boolean; overload; inline;

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
    FDataMap : TDataMap;
    FKind    : TDataKind;

    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FParent : TDataItem;

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

    FUnique : Boolean;

    // MaxTextLength overrides automatic calculation at TBIDBEngine.AddFields and
    // at TBIDataSetSource.AddItemField

    IMaxTextLength : Integer;

    FConsumers : TBIConsumers;

    function MaxTextLength:Integer;

    procedure CheckEmptyName;
    procedure ClearDelay;
    procedure CloneData(const ASource:TDataItem; const AStart,ACount:TInteger);
    class function CreateMap(const AKind:TDataKind):TDataMap;
    function CreateStats:TDataStats;

    function ExistsBefore(const AIndex:TInteger):Boolean;

    function GetProvider:TDataProvider;

    function HasItems:Boolean; inline;
    function HasMaster:Boolean;

    procedure SetInternalDate(const ADate:TDateTimePart);
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
    procedure ClearData(const Recursive:Boolean=False);

    function Compare(const A,B:TInteger; const IgnoreTextCase:Boolean=False):SmallInt;
    procedure CopyFrom(const DestIndex:TInteger; const Source:TDataItem; const SourceIndex:TInteger);

    procedure CreateIndexMulti(const Items:TDataArray);
    procedure CreateMasterIndex;

    procedure Delete(const Row:TInteger; const ACount:TInteger=1);
    function DataToString(const Index:TInteger):String; overload;
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

    procedure SortBy(const AData: TDataItem;
                     const Ascending: Boolean=True;
                     const IgnoreTextCase:Boolean=False); overload;

    procedure SortBy(const ASort: TSortItem); overload;

    function TotalColumns: TInteger;
    function TotalRows: TInteger; // Int64 as it might overflow in 32bit cpu
    procedure UnLoadData(const InvalidItems:Boolean=False); virtual;

    property Count:TInteger read FCount;
    property DataMap:TDataMap read FDataMap;
    property History:TImportHistory read GetHistory;

    {$IFNDEF FPC}
    property Item[const Index:Integer]:TDataItem read GetItemByIndex; default;
    {$ENDIF}

    {$IFNDEF FPC}
    {$IF CompilerVersion>28} // C++ Builder compatibility
    // *** RSP-14999 Workaround: https://quality.embarcadero.com/browse/RSP-14999
    [HPPGEN('__property TDataItem* Item2[const System::UnicodeString Name] = {read=GetItem/*, default*/};'+
            sLineBreak+
            'TDataItem* operator[](const System::UnicodeString Name) { return this->Item2[Name]; }'
            )]
    {$ENDIF}
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

  // Memory management helper methods

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

  TDetailData=class(TDataItem)
  public
    Detail : TDataItem;

    Constructor Create(const ADetail:TDataItem; const AName:String);
  end;

implementation
