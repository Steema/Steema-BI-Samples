{*********************************************}
{  TeeBI Software Library                     }
{  Main TDataItem classes                     }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DataItem;

interface

{

  This is the main unit of TeeBI, where the base core class TDataItem
  is implemented.

  TDataItem is like a column of a table, that can own sub-columns.
}

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

  {System.}Classes, {System.}SysUtils,
  BI.Arrays, BI.Expression;

{$IF Declared(CompilerVersion)}
 {$IF CompilerVersion>=23}
const
  TeeAllComponentPlatformIDs=(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or
                  {$IF CompilerVersion>=33}pidiOSSimulator32{$ELSE}pidiOSSimulator{$ENDIF}
                  or
                  {$IF CompilerVersion>=33}pidiOSDevice32{$ELSE}pidiOSDevice{$ENDIF}
              {$ENDIF}
              {$IF CompilerVersion>=26}or
                  {$IF CompilerVersion>=35}
                     pidAndroidArm32
                  {$ELSE}
                    {$IF CompilerVersion>=33}pidAndroid32Arm{$ELSE}pidAndroid{$ENDIF}
                  {$ENDIF}
              {$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              );
 {$ENDIF}
{$ENDIF}

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
    procedure Swap(const A,B:TInteger);

    property Item[const Index:TInteger]:Boolean read GetItem write SetItem; default;
  end;

  TDataArray=Array of TDataItem;

  // Helper methods for TDataArray

  TDataArrayHelper=record helper for TDataArray
  public
    procedure Add(const AData:TDataItem); overload;
    procedure Add(const AData:TDataArray); overload;
    procedure Clear; inline;
    function Copy:TDataArray;
    function Count:Integer; inline;
    procedure Delete(const Index:Integer);
    function Equal(const Value:TDataArray):Boolean;
    procedure Exchange(const A,B:Integer);
    function Exists(const AData:TDataItem):Boolean; inline;
    function Find(const AName:String):TDataItem;
    procedure FreeAll;
    function IndexOf(const AData:TDataItem):Integer; overload;
    function IndexOf(const AName:String):Integer; overload;
    procedure Insert(const AData:TDataItem; const AIndex:Integer);
    procedure Remove(const AData:TDataItem);
    procedure Resize(const ACount: Integer); inline;
    function Subtract(const AItems:TDataArray):TDataArray; overload;
    function Subtract(const AItem:TDataItem):TDataArray; overload;
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
    procedure DoChanged;
    function GetEmptyName:String;
  public
    Constructor Create(const AParent:TDataItem);
    Destructor Destroy; override;

    procedure Add(const AData:TDataItem); overload;
    function Add(const AName: String; const AKind: TDataKind; const Tag:TObject=nil): TDataItem; overload;

    procedure Clear;
    function Count:Integer; inline;
    procedure Delete(const AData:TDataItem);
    procedure Exchange(const A,B:Integer);
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
    function UniqueName(const APrefix:String):String;

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

  TBIEvent=(Changed,Destroyed,ChangedValues);
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
      // Used only during link loading:
      FOrigin : String;

      procedure Notify(const AEvent: TBIEvent);
      procedure Removed;
      procedure SetData(const Value: TDataItem);
      procedure SetOrigin(const Value: String);
    public
      FData : TDataItem;

      Index : TNativeIntArray;    // For self-detail relationship

      IsSelfDetail : Boolean;  // True when Origin='..', meaning our Master is our Parent (self-detail)

      // Add a new entry in Index, accumulating the ACount parameter
      procedure AppendIndex(const ACount:Integer);

      // Direct assign
      procedure Assign(const Value:TMasterDetail);

      // Returns the array of detail Indexes for given master AIndex position
      function GetIndex(const AMaster:TDataItem; const AIndex:TInteger):TNativeIntArray;

      property Data:TDataItem read FData write SetData;
      property Origin:String read FOrigin write SetOrigin;
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

    // Temporary. To avoid broadcasting when removing children items, see usage
    IDestroying : Boolean;

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
    procedure RemoveItem(const AItem:TDataItem);
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
    Constructor Create(const AKind: TDataKind; const AName:String); overload;

    class function LoadFromFile(const AFileName:String):TDataItem; inline;

    Destructor Destroy; override;

    procedure Clear;
    procedure ClearData(const Recursive:Boolean=False);

    function Compare(const A,B:TInteger; const IgnoreTextCase:Boolean=False):Integer;
    procedure CopyFrom(const DestIndex:TInteger;
                       const Source:TDataItem;
                       const SourceIndex:TInteger); overload;

    procedure CopyFrom(const DestIndex:TInteger;
                       const Source:TDataMap;
                       const SourceIndex:TInteger); overload;

    class function CreateIndexMulti(const Items:TDataArray):TNativeIntArray; static;
    procedure CreateMasterIndex;

    procedure Delete(const Row:TInteger; const ACount:TInteger=1);
    function DataToString(const Index:TInteger):String; overload;
    function EqualsMap(const A,B:TInteger):Boolean;
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

{$IFNDEF FPC}
{$IF CompilerVersion>27}
{$DEFINE THREADING}   // RAD XE7 and up
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF FPC}
  // MTProcs, <-- DoParallel
  {$ELSE}
  {$IFDEF THREADING}
  System.Threading,
  {$ENDIF}
  {$ENDIF}

  BI.Persist, BI.Languages.English;

{ TMissingData }

// Pending: Convert "All" to a maintained field (for speed)
function TMissingData.All: Boolean;
var t : TLoopInteger;
begin
  result:=(Count>0);

  if result then
     for t:=0 to High(Items) do
         if not Items[t] then
            Exit(False);
end;

procedure TMissingData.Init(const ACount: TInteger);
begin
  Count:=ACount;
  Items.Resize(Count);
  Items.Initialize(True);
end;

// Pending: Evaluate if better to keep MissingCount as a field
// instead of a function (costly)
function TMissingData.MissingCount: TInteger;
var t : TLoopInteger;
begin
  result:=0;

  for t:=0 to Items.Count-1 do
      if Items[t] then
         Inc(result);
end;

function TMissingData.GetItem(const Index: TInteger): Boolean;
begin
  result:=(Count>Index) and Items[Index];
end;

procedure TMissingData.DestroyStats;
begin
  IData.DestroyStats;
end;

procedure TMissingData.SetItem(const Index: TInteger; const Value: Boolean);
begin
  if Value then
  begin
    if Count<Index+1 then
    begin
      Count:=Index+1;
      Items.Resize(Count);
      Items[Index]:=True;
    end
    else
    if not Items[Index] then
    begin
      Items[Index]:=True;
      DestroyStats;
    end;
  end
  else
  if Count>Index then
     if Items[Index] then
     begin
       Items[Index]:=False;
       DestroyStats;
     end;
end;


procedure TMissingData.Swap(const A, B: TInteger);
var tmp : Boolean;
begin
  tmp:=GetItem(A);
  SetItem(A,GetItem(B));
  SetItem(B,tmp);
end;

{ TDataItems }

function TDataItems.Count: Integer; // moved here to enable inline
begin
  result:=Length(FItems);
end;

{ TDataItem }

Constructor TDataItem.Create(const Table:Boolean=False);
begin
  inherited Create;

  FKind:=dkUnknown;
  AsTable:=Table;
  Missing.IData:=Self;
end;

Constructor TDataItem.Create(const AData:TDataArray);
var tmp : TDataItem;
begin
  Create;

  for tmp in AData do
      if tmp<>nil then
         Items.Add(tmp);
end;

Constructor TDataItem.Create(const AProvider: TDataProvider);
begin
  Create;
  Provider:=AProvider;
end;

Constructor TDataItem.Create(const AKind: TDataKind);
begin
  Create;
  FKind:=AKind;
end;

Constructor TDataItem.Create(const AKind: TDataKind; const AName: String);
begin
  Create(AKind);
  Name:=AName;
end;

Destructor TDataItem.Destroy;
begin
  IDestroying:=True;

  ClearData;

  FItems.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
  FItems:=nil;

  FHistory.Free;

  IMaster.Removed;

  if FProvider<>nil then
  begin
    FProvider.FConsumers.Remove(Notify);

    if not KeepProvider then
       FProvider.Free;
  end;

  FConsumers.Broadcast(TBIEvent.Destroyed);
  FConsumers.Items:=nil;

  if Parent<>nil then
     Parent.RemoveItem(Self);

  inherited;
end;

// Remove an Item and broadcast a Changed event
procedure TDataItem.RemoveItem(const AItem:TDataItem);
begin
  // Remove an Item from FItems array
  if FItems<>nil then
     FItems.Delete(AItem); // <-- do not use "Items" to avoid call to Provider.GetItems

  if not IDestroying then
     FConsumers.Broadcast(TBIEvent.Changed);
end;

function TDataItem.DataToString(const Index: TInteger): String;
begin
  if Missing[Index] then
     result:=''
  else
  case Kind of
    dkInt32: if IHasDate then
                result:=IDate.AsString(Int32Data[Index]-1)
             else
                result:=IntToStr(Int32Data[Index]); // Str(Int32Data[Index],result); <-- speed?

    dkInt64: result:=IntToStr(Int64Data[Index]);
   dkSingle: result:=FloatToStr(SingleData[Index]);
   dkDouble: result:=FloatToStr(DoubleData[Index]);
 dkExtended: result:=FloatToStr(ExtendedData[Index]);
     dkText: result:=TextData[Index];
 dkDateTime: result:=DateTimeToStr(DateTimeData[Index]);
  dkBoolean: result:=BoolToStr(BooleanData[Index],True);
  else
    result:='?';
  end;
end;

procedure TDataItem.Delete(const Row: TInteger; const ACount:TInteger=1);
var t : TLoopInteger;
    tmp : TDataArray;
begin
  if Count>=(Row+ACount) then
  begin
    if AsTable then
    begin
      tmp:=Items.AsArray;

      for t:=0 to tmp.Count-1 do
          tmp[t].Delete(Row,ACount);
    end
    else
    begin
      case Kind of
          dkInt32: Int32Data.Delete(Row,ACount);
          dkInt64: Int64Data.Delete(Row,ACount);
         dkSingle: SingleData.Delete(Row,ACount);
         dkDouble: DoubleData.Delete(Row,ACount);
       dkExtended: ExtendedData.Delete(Row,ACount);
           dkText: TextData.Delete(Row,ACount);
       dkDateTime: DateTimeData.Delete(Row,ACount);
        dkBoolean: BooleanData.Delete(Row,ACount);
        dkUnknown: ;
      end;

      if Missing.Items<>nil then
      begin
        // Pending: Check Missing.Count>=(Row+ACount), delete up to Count only
        Missing.Items.Delete(Row,ACount);
        Dec(Missing.Count,ACount);
      end;

      DestroyStats;
    end;

    Dec(FCount,ACount);
  end;
end;

function TDataItem.FullName: String;
begin
  if Parent=nil then
     result:=Name
  else
     result:=Parent.FullName+' '+Name;
end;

// Recalculates all statistics for this TDataItem, and for all Items

procedure TDataItem.ReCalculate(const Parallel:Boolean=False);
var t : Integer;
begin
  DestroyStats;

  Stats;

  if Items.Count>0 then
  begin
    {$IFDEF THREADING}
    if Parallel then
    begin
      // Each Item is calculated using a background Thread
      TParallel.&For(1,0,Items.Count-1,procedure(Idx:Integer)
      begin
        Items[Idx].ReCalculate; // No parameter (no need to sub parallelize !)
      end);
    end
    else
    {$ENDIF}
    for t:=0 to Items.Count-1 do
        Items[t].ReCalculate;
  end;
end;

// Inserts a new item in this TDataItem and in all of its children items,
// at AtIndex position, with a missing value or with a default value.
// Default value = (0, empty string, minimum datetime or False)
procedure TDataItem.Insert(const AtIndex:TInteger; const AMissing:Boolean=True);
begin
  if AsTable then
  begin
    if FItems<>nil then
       FItems.InsertData(AtIndex);
  end
  else
  begin
    case Kind of
         dkInt32 : Int32Data.Insert(AtIndex,0);
         dkInt64 : Int64Data.Insert(AtIndex,0);
        dkSingle : SingleData.Insert(AtIndex,0);
        dkDouble : DoubleData.Insert(AtIndex,0);
      dkExtended : ExtendedData.Insert(AtIndex,0);
          dkText : TextData.Insert(AtIndex,'');
      dkDateTime : DateTimeData.Insert(AtIndex,MinDateTime);
       dkBoolean : BooleanData.Insert(AtIndex,False);
    end;

    if AMissing then
       Missing[AtIndex]:=True
    else
    if Missing.Items<>nil then
    begin
      Missing.Items.Insert(AtIndex,False);
      Inc(Missing.Count);
    end;
  end;

  Inc(FCount);
end;

// Change the length of the data arrays of this TDataItem,
// and of all sub Items recursively (in case its in "table" mode)
procedure TDataItem.Resize(const Size:TInteger);
begin
  if AsTable then
  begin
    // Change all sub-items size
    if FItems<>nil then
       FItems.Resize(Size);
  end
  else
  begin
    // Change size of the data array
    case Kind of
         dkInt32 : Int32Data.Resize(Size);
         dkInt64 : Int64Data.Resize(Size);
        dkSingle : SingleData.Resize(Size);
        dkDouble : DoubleData.Resize(Size);
      dkExtended : ExtendedData.Resize(Size);
          dkText : TextData.Resize(Size);
      dkDateTime : DateTimeData.Resize(Size);
       dkBoolean : BooleanData.Resize(Size);
    end;

    // Change also the size of the Missing array
    if Missing.Items<>nil then
    begin
      Missing.Items.Resize(Size);
      Missing.Count:=Size;
    end;
  end;

  // Finally, set the FCount integer to the new size
  FCount:=Size;
end;

// Exchange data row A<-->B, including table-mode sub-items and Missing array
procedure TDataItem.SwapRows(const A, B: TInteger);
var t : Integer;
begin
  // Very bad codegen due to Swap inlining. Alternatives?
  case Kind of
      dkInt32: Int32Data.Swap(A,B);
      dkInt64: Int64Data.Swap(A,B);
     dkSingle: SingleData.Swap(A,B);
     dkDouble: DoubleData.Swap(A,B);
   dkExtended: ExtendedData.Swap(A,B);
       dkText: TextData.Swap(A,B);
   dkDateTime: DateTimeData.Swap(A,B);
    dkBoolean: BooleanData.Swap(A,B);
    dkUnknown: ;
  end;

  if Missing.Items<>nil then
     Missing.Swap(A,B);

  if AsTable then
     for t:=0 to FItems.Count-1 do
         FItems.FItems[t].SwapRows(A,B);
end;

// Returns Index of first value equal to value in 0..AIndex position,
// or -1 if not found.

// This function is currently only used by TDataSelect.FoundLast to detect
// duplicate rows when the TDataSelect is configured with Distinct:=True
function TDataItem.ExistsBefore(const AIndex:TInteger):Boolean;
begin
  case Kind of
      dkInt32: result:=Int32Data.ExistsBefore(AIndex);
      dkInt64: result:=Int64Data.ExistsBefore(AIndex);
     dkSingle: result:=SingleData.ExistsBefore(AIndex);
     dkDouble: result:=DoubleData.ExistsBefore(AIndex);
   dkExtended: result:=ExtendedData.ExistsBefore(AIndex);
       dkText: result:=TextData.ExistsBefore(AIndex);
   dkDateTime: result:=DateTimeData.ExistsBefore(AIndex);
    dkBoolean: result:=BooleanData.ExistsBefore(AIndex);
  else
    result:=False;
  end;
end;

// Returns True when data in position A equals to data in position B
function TDataItem.SameData(const A,B:TInteger):Boolean;
begin
  case Kind of
      dkInt32: result:=Int32Data[A]=Int32Data[B];
      dkInt64: result:=Int64Data[A]=Int64Data[B];
     dkSingle: result:=SingleData[A]=SingleData[B];
     dkDouble: result:=DoubleData[A]=DoubleData[B];
   dkExtended: result:=ExtendedData[A]=ExtendedData[B];
       dkText: result:=TextData[A]=TextData[B];
   dkDateTime: result:=DateTimeData[A]=DateTimeData[B];
    dkBoolean: result:=BooleanData[A]=BooleanData[B];
  else
    result:=False;
  end;
end;

// Returns True when data in position A is equal to DataMap in position B
function TDataItem.EqualsMap(const A,B:TInteger):Boolean;
begin
  case Kind of
      dkInt32: result:=Int32Data[A]=TInt32Map(DataMap)[B];
      dkInt64: result:=Int64Data[A]=TInt64Map(DataMap)[B];
     dkSingle: result:=SingleData[A]=TSingleMap(DataMap)[B];
     dkDouble: result:=DoubleData[A]=TDoubleMap(DataMap)[B];
   dkExtended: result:=ExtendedData[A]=TExtendedMap(DataMap)[B];
       dkText: result:=TextData[A]=TTextMap(DataMap)[B];
   dkDateTime: result:=DateTimeData[A]=TDateTimeMap(DataMap)[B];
    dkBoolean: result:=BooleanData[A]=(B=1);
   else
     result:=False;
  end;
end;

procedure TDataItem.SaveToFile(const AFileName:String);
begin
  TDataItemPersistence.Save(Self,AFileName);
end;

procedure TDataItem.SetMaster(const Value: TDataItem);
begin
  if IMaster.Data<>Value then
  begin
    IMaster.Data:=Value;

    if Value=nil then
       IMaster.Origin:=''
    else
       IMaster.Origin:=TStore.OriginOf(Value,'',Self);
  end;
end;

procedure TDataItem.SetName(const Value: String);
begin
  if Name<>Value then
  begin
    if not SameText(Name,Value) then
    begin
      // Name is valid if no other Parent child Items are equally named
      if (Parent=nil) or (not Parent.Items.Exists(Value)) then
         FName:=Value
      else
         Parent.Items.DuplicateError(Value);
    end
    else
      FName:=Value; // just for case changes in Name

    if FParent=nil then
       FConsumers.Broadcast(TBIEvent.Changed)
    else
       FParent.FConsumers.Broadcast(TBIEvent.Changed);
  end;
end;

procedure TDataItem.Notify(const AEvent:TBIEvent);
begin
  if AEvent=TBIEvent.Destroyed then
     FProvider:=nil
  else
  if AEvent=TBIEvent.Changed then
  begin
    UnLoadData;
    FConsumers.Broadcast(TBIEvent.Changed);

    if Parent<>nil then
       Parent.Notify(AEvent);
  end;
end;

procedure TDataItem.SetProvider(const Value: TDataProvider);
begin
  if FProvider<>Value then
  begin
    if FProvider<>nil then
    begin
      FProvider.FConsumers.Remove(Notify);

      if not KeepProvider then
         FProvider.Free;
    end;

    FProvider:=Value;

    if FProvider<>nil then
    begin
      FProvider.FConsumers.Add(Notify);

      DelayPos:=-1;
    end;
  end;
end;

// This will effectively change (reorder) the rows
procedure TDataItem.SortBy(const AData: TDataItem; const Ascending: Boolean; const IgnoreTextCase:Boolean);
begin
  if AsTable and (AData.Parent=Self) then // pending: AData.IsParentOf(Self) for sub-tables
     case AData.Kind of
        dkInt32: AData.Int32Data.Sort(Ascending,SwapRows);
        dkInt64: AData.Int64Data.Sort(Ascending,SwapRows);
       dkSingle: AData.SingleData.Sort(Ascending,SwapRows);
       dkDouble: AData.DoubleData.Sort(Ascending,SwapRows);
     dkExtended: AData.ExtendedData.Sort(Ascending,SwapRows);
     dkDateTime: AData.DateTimeData.Sort(Ascending,SwapRows);
         dkText: AData.TextData.Sort(Ascending,IgnoreTextCase,SwapRows);
      dkBoolean: AData.BooleanData.Sort(Ascending,SwapRows);
    end
  else
    raise EBIException.Create('Error: Cannot sort data: '+Name+' using data: '+AData.Name);
end;

// Liberate memory. Always recursive (all Items and sub-Items)
procedure TDataItem.UnLoadData(const InvalidItems:Boolean);
var tmp : TDataItem;
begin
  ClearData;

  if FItems<>nil then
  begin
    for tmp in FItems.AsArray do
        tmp.UnLoadData;

    if InvalidItems then
       FItems.Valid:=False; // <-- flag to re-import Items later on
  end;

  // Flag position again for later re-loading:
  if DelayPos=0 then
  begin
    DelayPos:=SavedDelayPos;
    SavedDelayPos:=0;
  end;
end;

// Create Stats and DataMap for this item
function TDataItem.GetStats:TDataStats;

  procedure CalcInt32;
  var tmp : TInt32Array;
  begin
    if Missing.MissingCount>0 then
       tmp:=Int32Data.Copy(Missing.Items)
    else
       tmp:=Int32Data;

    FStats:=tmp.Stats;
    FDataMap:=tmp.Map(TInt32Stats(FStats).Median,TInt32Stats(FStats).Mode);
  end;

  procedure CalcInt64;
  var tmp : TInt64Array;
  begin
    if Missing.MissingCount>0 then
       tmp:=Int64Data.Copy(Missing.Items)
    else
       tmp:=Int64Data;

    FStats:=tmp.Stats;
    FDataMap:=tmp.Map(TInt64Stats(FStats).Median,TInt64Stats(FStats).Mode);
  end;

  procedure CalcSingle;
  var tmp : TSingleArray;
  begin
    if Missing.MissingCount>0 then
       tmp:=SingleData.Copy(Missing.Items)
    else
       tmp:=SingleData;

    FStats:=tmp.Stats;
    FDataMap:=tmp.Map(TSingleStats(FStats).Median,TSingleStats(FStats).Mode);
  end;

  procedure CalcDouble;
  var tmp : TDoubleArray;
  begin
    if Missing.MissingCount>0 then
       tmp:=DoubleData.Copy(Missing.Items)
    else
       tmp:=DoubleData;

    FStats:=tmp.Stats;
    FDataMap:=tmp.Map(TDoubleStats(FStats).Median,TDoubleStats(FStats).Mode);
  end;

  procedure CalcExtended;
  var tmp : TExtendedArray;
  begin
    if Missing.MissingCount>0 then
       tmp:=ExtendedData.Copy(Missing.Items)
    else
       tmp:=ExtendedData;

    FStats:=tmp.Stats;
    FDataMap:=tmp.Map(TExtendedStats(FStats).Median,TExtendedStats(FStats).Mode);
  end;

  procedure CalcDateTime;
  var tmp : TDateTimeArray;
  begin
    if Missing.MissingCount>0 then
       tmp:=DateTimeData.Copy(Missing.Items)
    else
       tmp:=DateTimeData;

    FStats:=tmp.Stats;
    FDataMap:=tmp.Map(TDateTimeStats(FStats).Median,TDateTimeStats(FStats).Mode);
  end;

  procedure CalcText;
  var tmp : TTextArray;
  begin
    if Missing.MissingCount>0 then
       tmp:=TextData.Copy(Missing.Items)
    else
       tmp:=TextData;

    FStats:=tmp.Stats;
    FDataMap:=tmp.Map({True}); // IgnoreTextCase ?
  end;

  procedure CalcBoolean;
  var tmp : TBooleanArray;
  begin
    if Missing.MissingCount>0 then
       tmp:=BooleanData.Copy(Missing.Items)
    else
       tmp:=BooleanData;

    FStats:=tmp.Stats;
    FDataMap:=tmp.Map;
  end;

var t1 : TStopwatch;
begin
  if FStats=nil then
  begin
    FDataMap.Free;
    FDataMap:=nil;

    Load;

    t1:=TStopwatch.StartNew;

    case Kind of
         dkInt32 : CalcInt32;
         dkInt64 : CalcInt64;
        dkSingle : CalcSingle;
        dkDouble : CalcDouble;
      dkExtended : CalcExtended;
          dkText : CalcText;
      dkDateTime : CalcDateTime;
       dkBoolean : CalcBoolean;
    else
      FStats:=nil;
    end;

    History.Times.Calculating:=t1.ElapsedMilliseconds;
  end;

  // True when all records are valid and unique:
  if not Unique then
     FUnique:=(Missing.Count=0) and (FDataMap<>nil) and (Count=FDataMap.Count);

  result:=FStats;
end;

function TDataItem.HasItems: Boolean;
begin
  result:=FItems<>nil;
end;

function TDataItem.HasMaster: Boolean;
begin
  result:=(IMaster.Data<>nil) or (IMaster.Origin<>'');
end;

// Returns True when this data item has AParent (recursive ancestors)
function TDataItem.IsChildOf(const AParent: TDataItem): Boolean;
var tmp : TDataItem;
begin
  tmp:=Parent;

  if tmp<>nil then
  repeat
    if tmp=AParent then
       Exit(True)
    else
       tmp:=tmp.Parent;

  until tmp=nil;

  result:=False;
end;

function TDataItem.GetProvider:TDataProvider;
var tmpParent : TDataItem;
begin
  result:=Provider;

  if result=nil then
  begin
    tmpParent:=Parent;

    while tmpParent<>nil do
    begin
      result:=tmpParent.Provider;

      if result<>nil then
         break
      else
         tmpParent:=tmpParent.Parent;
    end;
  end;
end;

procedure TDataItem.Load(const Children:Boolean);
var tmp : TDataProvider;
begin
  if DelayPos<>0 then
  begin
    tmp:=GetProvider;

    if tmp=nil then
       raise EBIException.Create('Internal Error: Missing Provider for data item: '+FullName)
    else
    begin
      Inc(FConsumers.Changing);
      try
        tmp.Load(Self,Children);
      finally
        Dec(FConsumers.Changing);
      end;
    end;
  end;
end;

procedure TDataItem.Load;
begin
  Load(AsTable);
end;

function TDataItem.GetItem(const AName: String): TDataItem;
begin
  result:=Items.GetNames(AName);
end;

{$IFNDEF FPC}
function TDataItem.GetItemByIndex(const AIndex:Integer):TDataItem;
begin
  result:=Items[AIndex];
end;
{$ENDIF}

function TDataItem.GetItems: TDataItems;

  // Pending: Ugly re-entrancy problem (ie: TDataSelect.AddItem to check duplicate Name).
  // Eliminate ?
  procedure ProviderGetItems;
  begin
    if Provider=nil then
       FItems.Valid:=True
    else
    if not IGettingItems then
    begin
      IGettingItems:=True;
      try
        Provider.GetItems(Self);

        FItems.Valid:=True;
      finally
        IGettingItems:=False;
      end;
    end;
  end;

begin
  if FItems=nil then
  begin
    FItems:=TDataItems.Create(Self);
    ProviderGetItems;
  end
  else
  if not FItems.Valid then
     ProviderGetItems;

  result:=FItems;
end;

function TDataItem.GetHistory: TImportHistory;
begin
  if FHistory=nil then
     FHistory:=TImportHistory.Create;

  result:=FHistory;
end;

function TDataItem.GetMaster: TDataItem;
begin
  result:=IMaster.Data;

  if (result=nil) and (IMaster.Origin<>'') then
  begin
    result:=TStore.OriginToData(Self,'',IMaster.Origin);
    IMaster.Data:=result;
  end;
end;

// Special IHasDate is True when data Kind is Integer but
// IDate is not None.
procedure TDataItem.SetInternalDate(const ADate:TDateTimePart);
begin
  IDate:=ADate;

  case IDate of
    TDateTimePart.ShortWeekDayName,
    TDateTimePart.LongWeekDayName,
    TDateTimePart.ShortMonthName,
    TDateTimePart.LongMonthName,
    TDateTimePart.Quarter:  IHasDate:=True;
  else
    IHasDate:=False;
  end;
end;

procedure TDataItem.CheckEmptyName;
begin
  if Name='' then
     if Parent=nil then
        FName:='Item_0'
     else
        FName:=Parent.Items.GetEmptyName;
end;

procedure TDataItem.Clear;
begin
  ClearData;

  if FItems<>nil then
  begin
    if IGettingItems then
       FItems.DestroyAll
    else
    begin
      FItems.Free;
      FItems:=nil;
    end;
  end;

  FCount:=0;
  FKind:=TDataKind.dkUnknown;
  AsTable:=False;

  ClearDelay;

// Investigate: FConsumers.Broadcast(TBIEvent.Changed);
end;

procedure TDataItem.DestroyStats;
begin
  // Destroy statistics
  if FStats<>nil then
  begin
    FStats.Free;
    FStats:=nil;
  end;

  // Destroy datamap
  if FDataMap<>nil then
  begin
    FDataMap.Free;
    FDataMap:=nil;
  end;
end;

procedure TDataItem.ClearData(const Recursive:Boolean=False);
var tmp : TDataItem;
begin
  case Kind of
      dkInt32: Int32Data:=nil;
      dkInt64: Int64Data:=nil;
     dkSingle: SingleData:=nil;
     dkDouble: DoubleData:=nil;
   dkExtended: ExtendedData:=nil;
       dkText: TextData:=nil;
   dkDateTime: DateTimeData:=nil;
    dkBoolean: BooleanData:=nil;
  end;

  DestroyStats;

  Missing.Count:=0;
  Missing.Items:=nil;

  // Not possible to destroy map, as it can be later required if reloading
  // the data item at TDataPersistence.Load
  // FDataMap.Free;
  // FDataMap:=nil;
  // Master.Index:=nil;  ?

  FCount:=0; // <--- ??

  if Recursive and (FItems<>nil) then
     for tmp in Items.AsArray do
         tmp.ClearData(True);
end;

class function TDataItem.CreateMap(const AKind:TDataKind):TDataMap;
const MapClasses:Array[TDataKind] of TDataMapClass=(
    TInt32Map,
    TInt64Map,
    TSingleMap,
    TDoubleMap,
    TExtendedMap,
    TTextMap,
    TDateTimeMap,
    TBooleanMap,
    nil
   );
begin
  if AKind=TDataKind.dkUnknown then
     result:=nil
  else
     result:=MapClasses[AKind].Create;
end;

function TDataItem.FindInMap(const Index:TInteger; out ABin:TNativeInteger):Boolean;
begin
  // Try to replace this with CreateIndex direct access
  case Kind of
      dkInt32: ABin:=TInt32Map(FDataMap).Items.SortedFind(Int32Data[Index],result);
      dkInt64: ABin:=TInt64Map(FDataMap).Items.SortedFind(Int64Data[Index],result);
     dkSingle: ABin:=TSingleMap(FDataMap).Items.SortedFind(SingleData[Index],result);
     dkDouble: ABin:=TDoubleMap(FDataMap).Items.SortedFind(DoubleData[Index],result);
   dkExtended: ABin:=TExtendedMap(FDataMap).Items.SortedFind(ExtendedData[Index],result);
   dkDateTime: ABin:=TDateTimeMap(FDataMap).Items.SortedFind(DateTimeData[Index],result);
       dkText: ABin:=TTextMap(FDataMap).Find(TextData[Index],result);
    dkBoolean: begin
                 ABin:=Ord(BooleanData[Index]);
                 result:=True;
               end;
  else
    begin
      ABin:=Index;
      result:=True;
    end;
  end;
end;

procedure TDataItem.CreateMasterIndex;

  procedure MissingNoMap(var AIndex:TNativeIntArray; const AMaster:TDataItem);
  var t : TLoopInteger;
  begin
    case Kind of
      dkInt32: for t:=0 to AIndex.Count-1 do
                   if Missing[t] then
                      AIndex[t]:=-1
                   else
                      AIndex[t]:=AMaster.Int32Data.IndexOf(Int32Data[t]);

      dkInt64: for t:=0 to AIndex.Count-1 do
                   if Missing[t] then
                      AIndex[t]:=-1
                   else
                      AIndex[t]:=AMaster.Int64Data.IndexOf(Int64Data[t]);

       dkText: for t:=0 to AIndex.Count-1 do
                   if Missing[t] then
                      AIndex[t]:=-1
                   else
                      AIndex[t]:=AMaster.TextData.IndexOf(TextData[t]);
    end;
  end;

  procedure MissingMap(var AIndex:TNativeIntArray; const AMap:TDataMap);
  var t : TLoopInteger;
  begin
    case Kind of
      dkInt32: for t:=0 to AIndex.Count-1 do
                   if Missing[t] then
                      AIndex[t]:=-1
                   else
                   if not TInt32Map(AMap).Find(Int32Data[t],AIndex[t]) then
                      AIndex[t]:=-1;

      dkInt64: for t:=0 to AIndex.Count-1 do
                   if Missing[t] then
                      AIndex[t]:=-1
                   else
                   if not TInt64Map(AMap).Find(Int64Data[t],AIndex[t]) then
                      AIndex[t]:=-1;

       dkText: for t:=0 to AIndex.Count-1 do
                   if Missing[t] then
                      AIndex[t]:=-1
                   else
                   if not TTextMap(AMap).Find(TextData[t],AIndex[t]) then
                      AIndex[t]:=-1;
    end;
  end;

  procedure NoMap(var AIndex:TNativeIntArray; const AMaster:TDataItem);
  var t : TLoopInteger;
  begin
    case Kind of
      dkInt32: case AMaster.Kind of
                dkInt32: for t:=0 to AIndex.Count-1 do
                             AIndex[t]:=AMaster.Int32Data.IndexOf(Int32Data[t]);

                dkInt64: for t:=0 to AIndex.Count-1 do
                             AIndex[t]:=AMaster.Int64Data.IndexOf(Int32Data[t]);
               end;

      dkInt64: case AMaster.Kind of
                dkInt32: for t:=0 to AIndex.Count-1 do
                             AIndex[t]:=AMaster.Int32Data.IndexOf(Int64Data[t]);

                dkInt64: for t:=0 to AIndex.Count-1 do
                             AIndex[t]:=AMaster.Int64Data.IndexOf(Int64Data[t]);
               end;

       dkText: for t:=0 to AIndex.Count-1 do
                   AIndex[t]:=AMaster.TextData.IndexOf(TextData[t]);
    end;
  end;

  procedure UseMap(var AIndex:TNativeIntArray; const AMaster:TDataKind; const AMap:TDataMap);

    procedure DoInt32;
    var t : TLoopInteger;
    begin
      case AMaster of
       dkInt32: for t:=0 to AIndex.Count-1 do
                    if not TInt32Map(AMap).Find(Int32Data[t],AIndex[t]) then
                       AIndex[t]:=-1;

       dkInt64: for t:=0 to AIndex.Count-1 do
                    if not TInt64Map(AMap).Find(Int32Data[t],AIndex[t]) then
                       AIndex[t]:=-1;
      end;
    end;

    procedure DoInt64;
    var t : TLoopInteger;
    begin
      case AMaster of
       dkInt32: for t:=0 to AIndex.Count-1 do
                    if not TInt32Map(AMap).Find(Int64Data[t],AIndex[t]) then
                       AIndex[t]:=-1;

       dkInt64: for t:=0 to AIndex.Count-1 do
                    if not TInt64Map(AMap).Find(Int64Data[t],AIndex[t]) then
                       AIndex[t]:=-1;
      end;
    end;

    procedure DoText;
    var t : TLoopInteger;
    begin
      for t:=0 to AIndex.Count-1 do
          if not TTextMap(AMap).Find(TextData[t],AIndex[t]) then
             AIndex[t]:=-1;
    end;

  begin
    case Kind of
      dkInt32: DoInt32;
      dkInt64: DoInt64;
       dkText: DoText;
    end;
  end;

var Map : TDataMap;
    tmpMaster : TDataItem;
begin
  Load;

  IMaster.Index.Resize(Parent.Count);

  tmpMaster:=Master;

  tmpMaster.Load;
  tmpMaster.Stats;

  // Note: "tmpMaster.Unique" condition is very important for speed reasons,
  // as the DataMap will be used to calculate the index (fast way), instead of
  // the slow NoMap search.

  if (tmpMaster.FDataMap<>nil) and
     (tmpMaster.FDataMap.Sorted=TDataOrder.Ascending) and tmpMaster.Unique then
     Map:=tmpMaster.FDataMap
  else
     Map:=nil;

  if Missing.Count>0 then
    if Map=nil then
       MissingNoMap(IMaster.Index,tmpMaster)
    else
       MissingMap(IMaster.Index,Map)
  else
  if Map=nil then
     NoMap(IMaster.Index,tmpMaster)
  else
     UseMap(IMaster.Index,tmpMaster.Kind,Map);
end;

function TDataItem.FindMapRow(const AMap:TDataMap; const ARow:TLoopInteger; out AIndex:TNativeInteger):Boolean;
begin
  case Kind of
   dkInt32: result:=TInt32Map(AMap).Find(Int32Data[ARow],AIndex);
   dkInt64: result:=TInt64Map(AMap).Find(Int64Data[ARow],AIndex);
    dkText: result:=TTextMap(AMap).Find(TextData[ARow],AIndex);
  else
    result:=False;
  end;
end;

function TDataItem.FindMapRow(const ARow:TLoopInteger; out AIndex:TNativeInteger):Boolean;
begin
  result:=FindMapRow(DataMap,ARow,AIndex);
end;

type
  THashIndex=class
  private
    Hash : TInt64Array;
    Pos : TNativeIntArray;

    procedure Swap(const A,B:TInteger);
  public
    Constructor Create(const ACount:TInteger);
    procedure Sort;
  end;

Constructor THashIndex.Create(const ACount:TInteger);
var t : TLoopInteger;
begin
  inherited Create;

  Hash.Resize(ACount);
  Pos.Resize(ACount);

  for t:=0 to ACount-1 do
      Pos[t]:=t;
end;

procedure THashIndex.Sort;
begin
  Hash.Sort(True,Swap);
end;

procedure THashIndex.Swap(const A,B:TInteger);
begin
  Hash.Swap(A,B);
  Pos.Swap(A,B);
end;

type
  TIndexHelper=record
  private
    Masters : TDataArray;
    Details : TDataArray;
    Multi : TInt32Array;
    Num : TInteger;

    function Calculate:TNativeIntArray;
    function CalculateMultipliers:TInt32Array;
    procedure FindMasters;
    function IndexOfRow(const ARow:TLoopInteger):TInteger;
    function SearchIndex(const AIndex: TInteger): TInteger;
    function TotalMapCount:TInteger;
    function UseBitIndex(const AMaster,ADetail,Num:TInteger):TNativeIntArray;
    function UseHashIndex(const AMaster,ADetail:TInteger):TNativeIntArray;
  public
    function From(const AItems:TDataArray):TNativeIntArray;
  end;

function TIndexHelper.From(const AItems:TDataArray):TNativeIntArray;
begin
  Details:=AItems;
  FindMasters;
  Num:=TotalMapCount;
  Multi:=CalculateMultipliers;
  result:=Calculate;
end;

function TIndexHelper.CalculateMultipliers:TInt32Array;
var L,
    t,
    tt : Integer;
    Num : TInteger;
begin
  L:=High(Masters);

  {$IFDEF FPC}
  result:=nil;
  {$ENDIF}

  result.Resize(L+1);

  for t:=L downto 0 do
  begin
    Num:=1;

    for tt:=L downto t+1 do
        Num:=Num*Masters[tt].DataMap.Count;

    result[t]:=Num;
  end;
end;

procedure TIndexHelper.FindMasters;
var t : TLoopInteger;
begin
  {$IFDEF FPC}
  Masters:=nil;
  {$ENDIF}

  // speed opt
  for t:=0 to High(Details) do
      Masters.Add(Details[t].Master);
end;

function TIndexHelper.TotalMapCount:TInteger;
var tmp : TDataItem;
begin
  result:=1;

  for tmp in Masters do
  begin
    tmp.Load;
    tmp.Stats;

    result:=result*tmp.DataMap.Count;
  end;
end;

// Returns the position in BitIndex that corresponds to all Items
function TIndexHelper.SearchIndex(const AIndex: TInteger): TInteger;
var tmp : TNativeInteger;
    t   : Integer;
begin
  result:=0;

  for t:=0 to High(Details) do
      if Details[t].FindMapRow(Masters[t].DataMap,AIndex,tmp) then
         Inc(result,Multi[t]*tmp)
      else
         Exit(-1);
         //DoError('Cannot find '+Items[t].Name+' value at index: '+AIndex.ToString+' in master map');
end;

function TIndexHelper.IndexOfRow(const ARow:TLoopInteger):TInteger;

  procedure DoError(const AMessage:String);
  begin
    raise EBIException.Create(AMessage);
  end;

var t : Integer;
    tmp : TNativeInteger;
begin
  result:=0;

  for t:=0 to High(Masters) do
      if not Masters[t].Missing[ARow] then // <-- investigate, is this ok ?
      begin
        if Masters[t].FindMapRow(ARow,tmp) then
           Inc(result,Multi[t]*tmp)
        else
           DoError('Cannot find value in '+Masters[t].Name+' map at index: '+IntToStr(ARow));
      end;
end;

// Slow way to create the index, when there is not enough memory to allocate
// the "BitIndex" huge array
function TIndexHelper.UseHashIndex(const AMaster,ADetail:TInteger):TNativeIntArray;
var t : TLoopInteger;

    tmpHash : THashIndex;

    tmp : TInteger;

    tmpExists : Boolean;
begin
  tmpHash:=THashIndex.Create(AMaster);
  try
    for t:=0 to AMaster-1 do
         tmpHash.Hash[t]:=IndexOfRow(t);

    tmpHash.Sort;

    {$IFDEF FPC}
    result:=nil;
    {$ENDIF}

    result.Resize(ADetail);

    for t:=0 to ADetail-1 do
    begin
      tmp:=SearchIndex(t);

      // Orphan detail rows !
      if tmp<>-1 then
      begin
        tmp:=tmpHash.Hash.SortedFind(tmp,tmpExists);

        if tmpExists then
           tmp:=tmpHash.Pos[tmp]
        else
           tmp:=-1; // Orphan row !
      end;

      result[t]:=tmp;
    end;
  finally
    tmpHash.Free;
  end;
end;

// Faster way to create the index, using a huge "BitIndex" array
function TIndexHelper.UseBitIndex(const AMaster,ADetail,Num:TInteger):TNativeIntArray;
var BitIndex : TNativeIntArray;
    tmp : TInteger;
    t : TLoopInteger;
begin
  {$IFDEF FPC}
  BitIndex:=nil;
  result:=nil;
  {$ENDIF}

  BitIndex.Resize(Num);

  for t:=0 to Num-1 do
      BitIndex[t]:=-1;

  for t:=0 to AMaster-1 do
      BitIndex[IndexOfRow(t)]:=t;

  result.Resize(ADetail);

  for t:=0 to ADetail-1 do
  begin
    tmp:=SearchIndex(t);

    // Orphan detail rows !
    if tmp<>-1 then
       tmp:=BitIndex[tmp];

    result[t]:=tmp;
  end;
end;

function TIndexHelper.Calculate:TNativeIntArray;
const
  // Max amount of Int32 memory allocatable
  MaxBitIndexSize={$IFDEF CPUX64}1000000000{$ELSE}100000000{$ENDIF};

var tmpMaster,
    tmpDetail : TInteger;

    tmpCol : TDataItem;
begin
  tmpCol:=Details[0];
  tmpMaster:=tmpCol.Master.Parent.Count;
  tmpDetail:=tmpCol.Parent.Count;

  if Num<MaxBitIndexSize then
     result:=UseBitIndex(tmpMaster,tmpDetail,Num)
  else
     result:=UseHashIndex(tmpMaster,tmpDetail);
end;

// Create a master-detail index when there are multiple keys (linked Items)
class function TDataItem.CreateIndexMulti(const Items:TDataArray):TNativeIntArray;
var tmp : TIndexHelper;
begin
  result:=tmp.From(Items);
end;

function TDataItem.CreateStats:TDataStats;
const StatsClasses:Array[TDataKind] of TDataStatsClass=(
    TInt32Stats,
    TInt64Stats,
    TSingleStats,
    TDoubleStats,
    TExtendedStats,
    TTextStats,
    TDateTimeStats,
    TBooleanStats,
    nil
   );
begin
  if (FStats=nil) and (Kind<>TDataKind.dkUnknown) then
     FStats:=StatsClasses[Kind].Create;

  result:=FStats;
end;

procedure TDataItem.ClearDelay;
begin
  if DelayPos<>0 then
  begin
    SavedDelayPos:=DelayPos;
    DelayPos:=0;
  end;
end;

procedure TDataItem.CloneData(const ASource: TDataItem; const AStart,ACount:TInteger);
begin
  // Assert(Kind=ASource.Kind) and ACount>0

  Missing.Items:=ASource.Missing.Items.Copy(AStart,ACount);
  Missing.Count:=Missing.Items.Count;

  case Kind of
      dkInt32: Int32Data:=ASource.Int32Data.Copy(AStart,ACount);
      dkInt64: Int64Data:=ASource.Int64Data.Copy(AStart,ACount);
     dkSingle: SingleData:=ASource.SingleData.Copy(AStart,ACount);
     dkDouble: DoubleData:=ASource.DoubleData.Copy(AStart,ACount);
   dkExtended: ExtendedData:=ASource.ExtendedData.Copy(AStart,ACount);
       dkText: TextData:=ASource.TextData.Copy(AStart,ACount);
   dkDateTime: DateTimeData:=ASource.DateTimeData.Copy(AStart,ACount);
    dkBoolean: BooleanData:=ASource.BooleanData.Copy(AStart,ACount);
    dkUnknown: ;
  end;

  FCount:=ACount;
end;

function TDataItem.Compare(const A,B:TInteger; const IgnoreTextCase:Boolean=False):Integer;
begin
  // check missing here???
  // if Missing[A] and Missing[B] then
  //    result:=0
  // else

  case Kind of
    dkInt32: if Int32Data[A]<Int32Data[B] then result:=-1 else
             if Int32Data[A]>Int32Data[B] then result:=1 else result:=0;

    dkInt64: if Int64Data[A]<Int64Data[B] then result:=-1 else
             if Int64Data[A]>Int64Data[B] then result:=1 else result:=0;

   dkSingle: if SingleData[A]<SingleData[B] then result:=-1 else
             if SingleData[A]>SingleData[B] then result:=1 else result:=0;

   dkDouble: if DoubleData[A]<DoubleData[B] then result:=-1 else
             if DoubleData[A]>DoubleData[B] then result:=1 else result:=0;

 dkExtended: if ExtendedData[A]<ExtendedData[B] then result:=-1 else
             if ExtendedData[A]>ExtendedData[B] then result:=1 else result:=0;

     dkText: if IgnoreTextCase then
                result:=CompareText(TextData[A],TextData[B])
             else
                result:=CompareStr(TextData[A],TextData[B]);

 dkDateTime: if DateTimeData[A]<DateTimeData[B] then result:=-1 else
             if DateTimeData[A]>DateTimeData[B] then result:=1 else result:=0;

  dkBoolean: result:=BooleanData.Compare(A,B);

  else
    if AsTable or (Items.Count=0) then
       result:=0 // default to equal
    else
    if IgnoreTextCase then
       result:=CompareText(Items[A].Name,Items[B].Name)
    else
       result:=CompareStr(Items[A].Name,Items[B].Name);
  end;
end;

procedure TDataItem.CopyFrom(const DestIndex: TInteger;
                             const Source: TDataItem;
                             const SourceIndex: TInteger);
begin
  if (Missing.Count<>0) or (Source.Missing.Count<>0) then
     Missing[DestIndex]:=Source.Missing[SourceIndex];

  case Kind of
    dkInt32: Int32Data[DestIndex]:=Source.Int32Data[SourceIndex];
    dkInt64: Int64Data[DestIndex]:=Source.Int64Data[SourceIndex];
   dkSingle: SingleData[DestIndex]:=Source.SingleData[SourceIndex];
   dkDouble: DoubleData[DestIndex]:=Source.DoubleData[SourceIndex];
 dkExtended: ExtendedData[DestIndex]:=Source.ExtendedData[SourceIndex];
     dkText: TextData[DestIndex]:=Source.TextData[SourceIndex];
 dkDateTime: DateTimeData[DestIndex]:=Source.DateTimeData[SourceIndex];
  dkBoolean: BooleanData[DestIndex]:=Source.BooleanData[SourceIndex];
  dkUnknown: ;
  end;
end;

procedure TDataItem.CopyFrom(const DestIndex: TInteger;
                             const Source: TDataMap;
                             const SourceIndex: TInteger);
begin
  case Kind of
       dkInt32: Int32Data[DestIndex]:=TInt32Map(Source)[SourceIndex];
       dkInt64: Int64Data[DestIndex]:=TInt64Map(Source)[SourceIndex];
      dkSingle: SingleData[DestIndex]:=TSingleMap(Source)[SourceIndex];
      dkDouble: DoubleData[DestIndex]:=TDoubleMap(Source)[SourceIndex];
    dkExtended: ExtendedData[DestIndex]:=TExtendedMap(Source)[SourceIndex];
        dkText: TextData[DestIndex]:=TTextMap(Source)[SourceIndex];
    dkDateTime: DateTimeData[DestIndex]:=TDateTimeMap(Source)[SourceIndex];
     dkBoolean: BooleanData[DestIndex]:=(SourceIndex=1);
  end;
end;

function TDataItem.TotalRows: TInteger;
var t : Integer;
begin
  if AsTable then
     result:=Count
  else
  begin
    result:=0;

    for t:=0 to Items.Count-1 do
        Inc(result,Items[t].TotalRows);
  end;
end;

function TDataItem.TotalColumns: TInteger;
var t : Integer;
begin
  if AsTable then
     result:=Items.Count // <-- pending: recursive item count?
  else
  begin
    result:=0;

    for t:=0 to Items.Count-1 do
        Inc(result,Items[t].TotalColumns);
  end;
end;

procedure TDataItem.Finish;
var tmp : NativeInt; //UInt64;
begin
  tmp:=TMemory.Allocated;

  if tmp>History.Memory then
     History.Memory:=tmp-History.Memory
  else
     History.Memory:=0;

  History.Times.Total:=FStartTime.ElapsedMilliseconds;
end;

class function TDataItem.LoadFromFile(const AFileName:String):TDataItem;
begin
  result:=TDataItemPersistence.Load(AFileName);
end;

function TDataItem.MaxTextLength: Integer;
begin
  result:=IMaxTextLength;

  if result=0 then
  begin
    result:=TextData.MaxLength;

    if result=0 then
       result:=64; // ??
  end;
end;

procedure TDataItem.SortBy(const ASort: TSortItem);
begin
  SortBy(ASort.Data,not ASort.Descending,ASort.IgnoreTextCase);
end;

{ TDataArrayHelper }

procedure TDataArrayHelper.Add(const AData: TDataItem);
var L : Integer;
begin
  L:=Count;
  SetLength(Self,L+1);
  Self[L]:=AData;
end;

procedure TDataArrayHelper.Add(const AData:TDataArray);
var L,
    t : Integer;
begin
  L:=Count;
  SetLength(Self,L+AData.Count);

  for t:=0 to AData.Count-1 do
      Self[L+t]:=AData[t];
end;

procedure TDataArrayHelper.Clear;
begin
  Self:=nil;
end;

function TDataArrayHelper.Copy: TDataArray;
var t : Integer;
begin
  SetLength(result,Count);

  for t:=Low(Self) to High(Self) do
      result[t-Low(Self)]:=Self[t];
end;

function TDataArrayHelper.Count: Integer;
begin
  result:=Length(Self);
end;

procedure TDataArrayHelper.Delete(const Index: Integer);
var t : Integer;
begin
  for t:=Index to Count-2 do
      Self[t]:=Self[t+1];

  SetLength(Self,Count-1);
end;

// Returns True when Value array items are exactly as Self items
function TDataArrayHelper.Equal(const Value: TDataArray): Boolean;
var t : Integer;
begin
  result:=Value.Count=Count;

  if result then
     for t:=0 to Count-1 do
         if Self[t]<>Value[t] then
            Exit(False);
end;

// Replaces item at position A with item at position B
procedure TDataArrayHelper.Exchange(const A, B: Integer);
var tmp : TDataItem;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

// Case-insensitive
function TDataArrayHelper.Find(const AName: String): TDataItem;
var t : Integer;
begin
  // Pending speed opt:
  // Self should be a TDataItem.Kind=dkText to reuse array TextData.Find

  for t:=0 to Count-1 do
      if SameText(Self[t].Name,AName) then
         Exit(Self[t]);

  result:=nil;
end;

procedure TDataArrayHelper.FreeAll;
var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      Self[t].Free;
end;

// Speed opt? (Binary search)
// Returns the index of the AData item in the array
function TDataArrayHelper.IndexOf(const AData: TDataItem):Integer;
var t : Integer;
begin
  for t:=0 to High(Self) do
      if Self[t]=AData then
         Exit(t);

  result:=-1;
end;

// Returns the index of the item with Name=AName
function TDataArrayHelper.IndexOf(const AName: String):Integer;
var t : Integer;
begin
  for t:=0 to High(Self) do
      if Self[t].Name=AName then
         Exit(t);

  result:=-1;
end;

// Warning: Below IndexOf (to be inlined)
function TDataArrayHelper.Exists(const AData:TDataItem):Boolean;
begin
  result:=IndexOf(AData)<>-1;
end;

// Inserts AData item at AIndex position in the array
procedure TDataArrayHelper.Insert(const AData: TDataItem;
  const AIndex: Integer);
var t,
    L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);

  for t:=L downto AIndex do
      Self[t]:=Self[t-1];

  Self[AIndex]:=AData;
end;

procedure TDataArrayHelper.Remove(const AData: TDataItem);
var tmp : Integer;
begin
  tmp:=IndexOf(AData);

  if tmp=-1 then
     // raise ?
  else
     Delete(tmp);
end;

procedure TDataArrayHelper.Resize(const ACount: Integer);
begin
  SetLength(Self,ACount);
end;

function TDataArrayHelper.Subtract(const AItem: TDataItem): TDataArray;
var tmp : TDataItem;
begin
  result:=nil;

  for tmp in Self do
      if tmp<>AItem then
         result.Add(tmp);
end;

// Returns a new copy of the array with all items except AItems
function TDataArrayHelper.Subtract(const AItems: TDataArray): TDataArray;
var tmp : TDataItem;
begin
  result:=nil;

  for tmp in Self do
      if not AItems.Exists(tmp) then
         result.Add(tmp);
end;

{ TDataItems }

Constructor TDataItems.Create(const AParent: TDataItem);
begin
  inherited Create;
  FParent:=AParent;
end;

procedure TDataItems.DestroyAll;
begin
  while Count>0 do
        {$IFDEF AUTOREFCOUNT}
        Self[0].DisposeOf;
        {$ELSE}
        Self[0].Free;
        {$ENDIF}

  FItems:=nil;
end;

Destructor TDataItems.Destroy;
begin
  DestroyAll;
  inherited;
end;

function TDataItems.GetEmptyName:String;
var t : Integer;
begin
  t:=0;
  result:='Item_'+IntToStr(t);

  while Exists(result) do
  begin
    Inc(t);
    result:='Item_'+IntToStr(t);
  end;
end;

function TDataItems.New(const AName: String; const AKind: TDataKind; const Tag:TObject=nil): TDataItem;
begin
  result:=TDataItem.Create(AKind);

  result.Name:=AName;
  result.TagObject:=Tag;
end;

procedure TDataItems.AddDirect(const AData: TDataItem);
begin
  FItems.Add(AData);

  if AData.FParent<>FParent then
  begin
    if AData.FParent<>nil then
       AData.FParent.RemoveItem(AData); // Remove and broadcast Changed

    AData.FParent:=FParent;

    DoChanged;
  end;
end;

procedure TDataItems.Add(const AData: TDataItem);
begin
  if not FItems.Exists(AData) then
     //if Find(AData.Name)=nil then  <-- problem at TSummary test 9 (duplicate)
        AddDirect(AData)
     //else
     //   DuplicateError(AData.Name);
end;

procedure TDataItems.DuplicateError(const AName:String);
begin
  raise EBIException.CreateFmt(BIMsg_DuplicatedItem,[AName]);
end;

procedure TDataItems.DoChanged;
begin
  if FParent<>nil then
     FParent.FConsumers.Broadcast(TBIEvent.Changed);
end;

procedure TDataItems.Exchange(const A, B: Integer);
begin
  if A<>B then
  begin
    FItems.Exchange(A,B);
    DoChanged;
  end;
end;

function TDataItems.Exists(const AName: String): Boolean;
begin
  result:=Find(AName)<>nil;
end;

function TDataItems.Add(const AName: String; const AKind: TDataKind; const Tag:TObject=nil): TDataItem;
begin
  if Find(AName)=nil then
  begin
    result:=New(AName,AKind,Tag);
    AddDirect(result);
  end
  else
  begin
    DuplicateError(AName);
    result:=nil; // <-- just to skip warning
  end;
end;

procedure TDataItems.Clear;
var tmp : TDataItem;
begin
  for tmp in FItems do
      tmp.FParent:=nil;

  FItems:=nil;
end;

procedure TDataItems.Delete(const AData: TDataItem);
var t,
    tmp : Integer;
begin
  tmp:=FItems.IndexOf(AData);

  if tmp<>-1 then
  begin
    for t:=tmp to Count-2 do
        FItems[t]:=FItems[t+1];

    SetLength(FItems,Count-1);

    AData.FParent:=nil;
  end;
end;

// Returns child data item "Name", or nil if it does not exist.
// NOTE: This method should not raise an exception. (See GetByName)
function TDataItems.Find(const AName: String): TDataItem;
begin
  result:=FItems.Find(AName);
end;

function TDataItems.GetNames(const AName: String): TDataItem;
begin
  result:=Find(AName);

  if result=nil then
     raise EBIException.CreateFmt(BIMsg_DataItem_ChildNotFound,[AName]);
end;

function TDataItems.GetLast: TDataItem;
begin
  if Count=0 then
     result:=nil
  else
     result:=FItems[Count-1];
end;

function TDataItems.GetItem(const Index: Integer): TDataItem;
begin
  result:=FItems[Index];
end;

procedure TDataItems.Resize(const Size: TInteger);
var t : Integer;
    tmp : TDataItem;
begin
  for t:=0 to Count-1 do
  begin
    tmp:=FItems[t];

    if not tmp.IMaster.IsSelfDetail then
       tmp.Resize(Size);
  end;
end;

procedure TDataItems.Reverse;
begin
  TArrayReverse<TDataItem>.Reverse(FItems);
end;

function TDataItems.IndexOf(const AName: String): Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if SameText(FItems[t].Name,AName) then
         Exit(t);

  result:=-1;
end;

procedure TDataItems.Insert(const AData: TDataItem; const AIndex: Integer);
begin
  FItems.Insert(AData,AIndex);
  AData.FParent:=FParent;
end;

procedure TDataItems.InsertData(const AtIndex:TInteger);
var t : Integer;
begin
  for t:=0 to Count-1 do
      if FItems[t].Master<>Parent then
         FItems[t].Insert(AtIndex);
end;

function TDataItems.Select(const Indices: Array of Integer): TDataArray;
var t, L : Integer;
begin
  L:=Length(Indices);
  SetLength(result,L);

  for t:=0 to L-1 do
      result[t]:=FItems[Indices[t]];
end;

function TDataItems.Select(const Start, Finish: Integer): TDataArray;
var t : Integer;
begin
  SetLength(result,Finish-Start+1);

  for t:=Start to Finish do
      result[t-Start]:=FItems[t];
end;

procedure TDataItems.Swap(const A,B:TInteger);
begin
  ISort.Swap(A,B);
  FItems.Exchange(A,B);
end;

// Returns a valid Item Name
// (One that does not exist in the list of items, case-insensitive)
function TDataItems.UniqueName(const APrefix: String): String;
var t : Integer;
begin
  t:=0;

  result:=APrefix;

  while Exists(result) do
  begin
    Inc(t);
    result:=APrefix+'_'+IntToStr(t);
  end;
end;

procedure TDataItems.SortByName(const Ascending, IgnoreCase: Boolean);
var t : Integer;
begin
  ISort.Resize(Count);

  for t:=0 to Count-1 do
      ISort[t]:=Self[t].Name;

  ISort.Sort(Ascending,IgnoreCase,Swap);
end;

{ TImportHistory }

Constructor TImportHistory.Create;
begin
  inherited;
  DateTime:=Now;
end;

{ TDataKindHelper }

class function TDataKindHelper.FromString(const AText:String; out AKind:TDataKind):Boolean;
begin
  result:=True;

  if SameText(AText,dkInt32.ToString) then
     AKind:=dkInt32
  else
  if SameText(AText,dkInt64.ToString) then
     AKind:=dkInt64
  else
  if SameText(AText,dkSingle.ToString) then
     AKind:=dkSingle
  else
  if SameText(AText,dkDouble.ToString) then
     AKind:=dkDouble
  else
  if SameText(AText,dkExtended.ToString) then
     AKind:=dkExtended
  else
  if SameText(AText,dkText.ToString) then
     AKind:=dkText
  else
  if SameText(AText,dkDateTime.ToString) then
     AKind:=dkDateTime
  else
  if SameText(AText,dkBoolean.ToString) then
     AKind:=dkBoolean
  else
  if SameText(AText,dkUnknown.ToString) then
     AKind:=dkUnknown
  else
     result:=False;
end;

function TDataKindHelper.IsNumeric: Boolean;
begin
  result:=(Self=dkSingle) or
          (Self=dkDouble) or
          (Self=dkExtended) or
          (Self=dkInt32) or
          (Self=dkInt64);
end;

function TDataKindHelper.ToString: String;
begin
  case Self of
      dkInt32: result:='Integer';
      dkInt64: result:='Integer 64bit';
     dkSingle: result:='Single';
     dkDouble: result:='Double';
   dkExtended: result:='Extended';
       dkText: result:='Text';
   dkDateTime: result:='Date Time';
    dkBoolean: result:='Logical';
  else
    //dkUnknown
    result:='Unknown';
  end;
end;

{ TDataItem.TMasterDetail }

procedure TDataItem.TMasterDetail.AppendIndex(const ACount: Integer);
var tmp : TInteger;
begin
  // Add index of last data + 1
  tmp:=Index.Count;

  if tmp>0 then
     tmp:=Index[tmp-1]+ACount
  else
     tmp:=ACount;

  Index.Append(tmp);
end;

procedure TDataItem.TMasterDetail.Assign(const Value: TMasterDetail);
begin
  Data:=Value.Data;

  if Value.Origin='' then
     Origin:=''
  else
     Origin:=Value.Origin; // <-- pending: relative to absolute
end;

function TDataItem.TMasterDetail.GetIndex(const AMaster: TDataItem;
  const AIndex: TInteger): TNativeIntArray;
var t : TLoopInteger;
    tmpFirst,
    tmpLast : TInteger;
begin
  result:=nil;

  if AIndex>=0 then
  begin
    if Index=nil then  // assert ???
    begin
      tmpFirst:=AIndex;
      tmpLast:=AMaster.Count-1;
    end
    else
    begin
      if Index.Count>AIndex then
      begin
        if AIndex=0 then
           tmpFirst:=0
        else
           tmpFirst:=Index[AIndex-1];

        tmpLast:=Index[AIndex]-1;
      end
      else
        Exit;
    end;

    // Fill result
    if tmpLast>=tmpFirst then
    begin
      result.Resize(tmpLast-tmpFirst+1);

      for t:=tmpFirst to tmpLast do
          result[t-tmpFirst]:=t;
    end
    else
      result:=nil;
  end;
end;

procedure TDataItem.TMasterDetail.Notify(const AEvent: TBIEvent);
begin
  if AEvent=TBIEvent.Changed then
     //TryUnloadData
  else
  if AEvent=TBIEvent.Destroyed then
     Data:=nil;
end;

procedure TDataItem.TMasterDetail.Removed;
begin
  {$IFNDEF FPC}
  if FData<>nil then
     FData.FConsumers.Remove(Notify);
  {$ENDIF}
end;

procedure TDataItem.TMasterDetail.SetData(const Value: TDataItem);
begin
  if FData<>Value then
  begin
    Removed;

    FData:=Value;

    {$IFNDEF FPC}
    if FData<>nil then
       FData.FConsumers.Add(Notify);
    {$ENDIF}
  end;
end;

procedure TDataItem.TMasterDetail.SetOrigin(const Value: String);
begin
  if FOrigin<>Value then
  begin
    FOrigin:=Value;
    IsSelfDetail:=Origin='..';
  end;
end;

{ TDataProvider }

Destructor TDataProvider.Destroy;
begin
  FConsumers.Broadcast(TBIEvent.Destroyed);
  FConsumers.Items:=nil;

  inherited;
end;

procedure TDataProvider.Assign(Source: TPersistent);
begin
  if Source is TDataProvider then
     FTitle:=TDataProvider(Source).FTitle
  else
     inherited;
end;

procedure TDataProvider.BeginUpdate;
begin
  Inc(FConsumers.Changing);
end;

procedure TDataProvider.EndUpdate;
begin
  Dec(FConsumers.Changing);

  if FConsumers.Changing=0 then
     // Important: Calling "Changed" just produces unexpected results at BIQuery
     // because it overrides "Changed", so we're calling broadcast directly
     FConsumers.Broadcast(TBIEvent.Changed);
end;

function TDataProvider.GetStream(const AData,ANext:TDataItem):TStream;
begin
  result:=nil; // Default
end;

function TDataProvider.GetStream(const AItems:TDataArray):TStream;
begin
  result:=nil; // Default
end;

function TDataProvider.NewData: TDataItem;
begin
  result:=TDataItem.Create(Self);
  result.KeepProvider:=True;
end;

procedure TDataProvider.Changed;
begin
  FConsumers.Broadcast(TBIEvent.Changed);
end;

function TDataProvider.Changing: Boolean;
begin
  result:=FConsumers.Changing>0;
end;

procedure TDataProvider.GetItems(const AData: TDataItem);
begin // Default: do nothing
end;

{ TBIConsumers }

procedure TBIConsumers.Add(const AConsumer: TBIConsumer);
var L : Integer;
begin
  if IndexOf(AConsumer)=-1  then
  begin
    L:=Length(Items);
    SetLength(Items,L+1);
    Items[L]:=AConsumer;
  end;
end;

procedure TBIConsumers.Broadcast(const AEvent: TBIEvent);
var t : Integer;
begin
  if Changing=0 then
  begin
    t:=0;

    while t<Length(Items) do
    begin
      Items[t](AEvent);
      Inc(t);
    end;
  end;
end;

function TBIConsumers.IndexOf(const AConsumer:TBIConsumer):Integer;
var t : Integer;
begin
  for t:=0 to High(Items) do
      {$IFDEF FPC}
      if (TMethod(Items[t]).Code=TMethod(AConsumer).Code) and
         (TMethod(Items[t]).Data=TMethod(AConsumer).Data) then
      {$ELSE}
      if TMethod(Items[t])=TMethod(AConsumer) then
      {$ENDIF}
         Exit(t);

  result:=-1;
end;

procedure TBIConsumers.Remove(const AConsumer: TBIConsumer);
var t,
    tmp : Integer;
begin
  tmp:=IndexOf(AConsumer);

  if tmp<>-1 then
  begin
    for t:=tmp to High(Items)-1 do
        Items[t]:=Items[t+1];

    SetLength(Items,High(Items));
  end;
end;

{ TMemory }

class function TMemory.Allocated: Int64;
{$IFDEF FPC}
var mem : TMemoryManager;
begin
  mem.NeedLock:=False; // skip hint
  GetMemoryManager(mem);
  result:=mem.GetHeapStatus.TotalAllocated;
end;
{$ELSE}

{$IFDEF MSWINDOWS}
var State: TMemoryManagerState;
    Small: TSmallBlockTypeState;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
  GetMemoryManagerState(State);
  {$WARN SYMBOL_PLATFORM ON}

  result:=State.TotalAllocatedMediumBlockSize + State.TotalAllocatedLargeBlockSize;

  for Small in State.SmallBlockTypeStates do
      Inc(result, Small.UseableBlockSize * Small.AllocatedBlockCount);

  {$ELSE}
  result:=0;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TMemory.Status:MemoryStatusEx;
begin
  FillChar(result,SizeOf(MemoryStatusEx),#0);
  result.dwLength:=SizeOf(MemoryStatusEx);
  GlobalMemoryStatusEx(result);
end;
{$ENDIF}
{$ENDIF}

class function TMemory.Available: UInt64;
{$IFDEF FPC}
var mem : TMemoryManager;
{$ENDIF}
begin
  {$IFDEF FPC}
  mem.NeedLock:=False; // skip hint
  GetMemoryManager(mem);
  result:=mem.GetHeapStatus.TotalFree;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result:=Status.ullAvailPhys;
  {$ELSE}
  result:=0;
  {$ENDIF}
  {$ENDIF}
end;

class procedure TMemory.Init;
begin
  TMemory.MinLimit:=TMemory.TotalPhysical div 5;
end;

class function TMemory.TotalPhysical: UInt64;
begin
  {$IFDEF FPC}
  result:=0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  result:=Status.ullTotalPhys;
  {$ELSE}
  result:=0;
  {$ENDIF}
  {$ENDIF}
end;

{ TDetailData }

Constructor TDetailData.Create(const ADetail: TDataItem; const AName: String);
begin
  inherited Create;
  FName:=AName;
  Detail:=ADetail;
end;

initialization
  TMemory.Init;
end.

