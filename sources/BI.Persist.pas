{*********************************************}
{  TeeBI Software Library                     }
{  Data Persistence classes                   }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Persist;

{
   This unit contains TDataDefinition and TStore classes, to make easier to
   load and save TDataItem data from "stores".

   An "store" is an abstract location.
   It can be a folder on disk, a web server url, etc.
}

{$IFNDEF FPC}
{$EXCESSPRECISION OFF}
{$ENDIF}

{$IF Declared(Extended)}
{$IF SizeOf(Extended) = 10}
  {$DEFINE EXTENDEDIS10BYTES}
{$ENDIF}
{$ENDIF}

interface

uses
  {System.}Classes, {System.}SysUtils,
  {$IFDEF FPC}
  BI.FPC,
  {$ENDIF}
  BI.DataItem, BI.Arrays, BI.Arrays.Strings, BI.Streams;

type
  TBIError=function(const Sender:TObject; const Error:String):Boolean of object;

  TBIErrorProc=
    {$IFDEF FPC}
    function(const Sender:TObject; const Error:String):Boolean of object;
    {$ELSE}
    reference to function(const Sender:TObject; const Error:String):Boolean;
    {$ENDIF}

  TBIProgress=procedure(const Sender:TObject; const Percent:Single; var Cancel:Boolean) of object;

  TRefreshUnit=(Seconds,Minutes,Hours,Days,Weeks,Months,Years);

  // Abstract provider class with its own Data property
  TBaseDataImporter=class(TDataProvider)
  private
    function GetData:TDataItem;
    procedure TryUnloadData;
  protected
    FData : TDataItem;

    IKeepData : Boolean;

    procedure Changed; override;
    procedure Notify(const AEvent:TBIEvent);
  public
    Destructor Destroy; override;

    property Data:TDataItem read GetData;
  end;

  TRefreshSettings=record
  private
    function Increment(const ADate:TDateTime):TDateTime;
  public
    Period : Integer;
    Units : TRefreshUnit;
    Enabled : Boolean;
  end;

  TDataDefinition=class;

  TDatabaseDefinition=class(TPersistent)
  private
    IDefinition : TDataDefinition;

    function GetSystem: Boolean;
    procedure SetSystem(const Value: Boolean);
    function GetViews: Boolean;
    procedure SetViews(const Value: Boolean);
  published
    property IncludeViews:Boolean read GetViews write SetViews default False;
    property IncludeSystem:Boolean read GetSystem write SetSystem default False;
  end;

  TDataRelation=class(TCollectionItem)
  private
    FMaster,
    FDetail : String;
  public
    procedure Assign(Source:TPersistent); override;
  published
    property Detail:String read FDetail write FDetail;
    property Master:String read FMaster write FMaster;
  end;

  TDataRelations=class(TOwnedCollection)
  private
    function Get(const Index: Integer): TDataRelation;
    procedure Put(const Index: Integer; const Value: TDataRelation);
  public
    function Add(const AMaster,ADetail:String):TDataRelation;
    property Item[const Index:Integer]:TDataRelation read Get write Put; default;
  end;

  TDataDefinitionKind=(Files,Database,Web,Manual,Unknown);

  // Abstract class with all settings necessary to import data from different
  // kinds of sources (files, web, database or manual)
  TDataDefinition=class(TBaseDataImporter)
  private
    FDatabase : TDatabaseDefinition;
    FFileName : String;
    FLinks    : TDataRelations;

    FOnError : TBIError;
    FOnImporting : TBIProgress;

    ILoading : Boolean;

    function GetCalcStats: Boolean;
    function GetKind: TDataDefinitionKind;
    procedure GetKindSettings(const Value: TDataDefinitionKind);
    function GetLinks: TDataRelations;
    function GetParallel: Boolean;
    procedure GetSettings;
    function GetStrings:TStrings;
    function GetValue(const Index: String): String;
    procedure NotifyManual(const AEvent:TBIEvent);
    procedure PrecalculateStats(const AData:TDataArray);
    procedure ReadData(Stream: TStream);
    procedure SetCalcStats(const AValue: Boolean);
    procedure SetFileName(const Value: String);
    procedure SetKind(const Value: TDataDefinitionKind);
    procedure SetLinks(const Value: TDataRelations);
    procedure SetParallel(const AValue: Boolean);
    procedure SetValue(const Index: String; const Value: String);
    procedure SetStrings(const Value: TStrings);
    procedure TryLoadDetailRelations(const AData:TDataArray; const AStore:String);
    procedure TryLoadFile;
    procedure WriteData(Stream: TStream);
  protected
    FStrings : TStrings;

    Refresh : TRefreshSettings;

    Volatile : Boolean; // When True, changes to Strings aren't saved

    // Embedd a TDataItem saved / loaded from a TStream in DFM/FMX
    procedure DefineProperties(Filer: TFiler); override;

    procedure GetItems(const AData:TDataItem); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Loaded; override;
    procedure Save;
    procedure TryLoadDetails(const AStore:String);
  public
    const
      Extension='.def';

    var
      Store : String;

    Constructor Create(AOwner:TComponent); override;

    Constructor FromFile(const AOwner:TComponent; const AFileName:String);
    Destructor Destroy; override;

    function AsBoolean(const Key:String; const ADefault:Boolean=True):Boolean;

    class procedure CreateFile(const FileName:String; const Kind:TDataDefinitionKind);
    function Description: String;

    class function KindToString(const AKind:TDataDefinitionKind):String;
    function Import(const AStore:String):TDataArray;

    function LastImport:String;

    class function LinksFrom(const AStrings:TStrings):TDataRelations; static;

    procedure LoadFromFile(const AFileName:String);
    procedure LoadFromText(const AText:String);

    class procedure Merge(const AData: TDataItem; const AItems:TDataArray); overload; static;
    class procedure Merge(const AData,ASource: TDataItem); overload; static;

    function MultiLineText(const ATag:String):String;
    function NextRefresh:TDateTime;
    procedure SaveLinks;

    class procedure SetMasters(const AData:TDataItem;
                               const AStore:String;
                               const Items:TDataRelations); overload; static;

    class procedure SetMasters(const AData:TDataItem;
                               const AStore:String;
                               const AStrings:TStrings); overload; static;

    property AsDatabase:TDatabaseDefinition read FDatabase;
    property CalcStats:Boolean read GetCalcStats write SetCalcStats default False;
    property Parallel:Boolean read GetParallel write SetParallel default False;
    property Value[const Index:String]:String read GetValue write SetValue; default;
  published
    property Kind:TDataDefinitionKind read GetKind write SetKind default TDataDefinitionKind.Unknown;
    property FileName:String read FFileName write SetFileName;
    property Links:TDataRelations read GetLinks write SetLinks;
    property Strings:TStrings read GetStrings write SetStrings;

    property OnError:TBIError read FOnError write FOnError;
    property OnImporting:TBIProgress read FOnImporting write FOnImporting;
  end;

  TDataImporter=class;
  TDataImporterClass=class of TDataImporter;

  // Abstract class to handle loading data from a TStore repository
  TDataImporter=class(TDataProvider)
  protected
    Definition : TDataDefinition;

    class var
      Importers:Array of TDataImporterClass;

    procedure DoProgress(const Percent:Single; var Cancel:Boolean);
    class function Find(const AClass:TDataImporterClass):Integer;
    //procedure Load(const AData:TDataItem; const Children:Boolean); overload; virtual; abstract;
  public
    Constructor CreateDefinition(const AStore:String; const ADefinition:TDataDefinition); virtual;

    function AllData:TStringArray; virtual; abstract;
    function GetDefinition(const AName:String):TDataDefinition; virtual; abstract;
    class function Guess(const Kind:TDataDefinitionKind;
                         const Store:String;
                         const ADefinition:TDataDefinition):TDataImporter;
    function Import:TDataArray; virtual; abstract;
    class function IsRemote:Boolean; virtual; abstract;
    function Load(const AName:String):TDataItem; virtual; abstract;

    class procedure RegisterClass(const AClass:TDataImporterClass);
    class function Supports(const Kind:TDataDefinitionKind; const ADefinition:TDataDefinition=nil):Boolean; virtual; abstract;
    class procedure UnRegisterClass(const AClass:TDataImporterClass);
  end;

  // Represents a disk folder or BIWeb server data repository
  TStore=class
  private
    class var
      IDefault : String;

    class function GetDefaultName: String; static;
    class function GuessImporter(const AStore:String=''):TDataImporter; static;
    class function NameOrDefault(const AName: String): String;
    class procedure SetDefaultName(const Value: String); static;
  public
    class function AllData(const AStore:String=''):TStringArray; overload; static;

    class function DefinitionOf(const Name:string):String; overload; static;
    class function DefinitionOf(const AStore,Name: string): String; overload; static;

    class function NameOf(const FileName:String):String; static;

    class function FullPath(const FileName:string):String; overload; static;
    class function FullPath(const AStore,FileName: string): String; overload; static;

    class function GetDefinition(const AStore,AName:String):TDataDefinition; static;
    class function IsRemote(const AStore:String):Boolean; static;

    class function Load(const AName:String):TDataItem; overload; static;
    class function Load(const AStore,AName:String; const OnError:TBIErrorProc=nil):TDataItem; overload; static;

    class function DataToStream(const AData:TDataItem; const Zip:Boolean=False):TStream; overload; static;
    class function DataToStream(const APath,AName:String; const Zip:Boolean=False):TStream; overload; static;
    class function DataToStream(const AName:String; const Zip:Boolean=False):TStream; overload; static;

    class function NotRegistered(const AName: String):EBIException;

    class function OriginOf(const AData:TDataItem;
                            const DefaultStore:String;
                            const RelativeTo:TDataItem=nil):String; static;

    class function OriginToData(const AData:TDataItem; const AStore,AOrigin:String;
          const Error:TBIErrorProc=nil):TDataItem; static;

    class function PathOf(const AName:String):String; static;

    class procedure RemoveData(const AStore,AName:String); static;
    class procedure Rename(const AStore,AOld,ANew:String); static;

    class procedure Save(const Data:TDataItem; const AFileName:String;
                         const AsFolder:Boolean=False;
                         const Progress:TBIProgress=nil); overload; static;

    class function StoreOf(const AData:TDataItem):TDataItem; static;

    class procedure UnLoad(const AStore,AName:String); static;
    class procedure UnLoadAll; static;

    class function ZipStream(const AStream: TStream): TStream; static;

    class property DefaultName:String read GetDefaultName write SetDefaultName;
  end;

  // Global list of registered Stores in this machine / device
  TStores=class
  private
    class procedure CheckEmptyName(const AName: String);
    class function ReadPart(const Index,Part:Integer):String; static;
  public
    class procedure Add(const AName,AOrigin:String); static;
    class procedure AllTo(const Items:TStrings); static;
    class function All(const APart:Integer=0):TStringArray; static;
    class procedure ChangeName(const AOld,ANew:String); static;
    class procedure ChangePath(const AName,AOrigin:String); static;
    class function Count:Integer; static;
    class function Exists(const AName:String):Boolean; static;
    class function GlobalCache:TDataItem; static; // cannot be inlined
    class function IndexOf(const AName:String):Integer; static;
    class function NewName:String; static;
    class procedure Remove(const AName:String); static;
    class procedure Save(const S:TStringArray); overload; static;
    class procedure Save(const Index:Integer; const AName,AOrigin:String); overload; static;
  end;

  // Loads and saves Items INFORMATION (structure) to/from streams or disk files
  TPersistence=class
  private
    class procedure Load(const Reader:TBIReader; const History:TImportHistory); overload; static;
    class procedure Save(const Writer:TBIWriter; const History:TImportHistory); overload; static;
  protected
    class function Load(const Reader:TBIReader):TDataItem; overload; static;

    class procedure ReadItem(const Reader: TBIReader; const AData: TDataItem); static;
    class procedure WriteItem(const Writer: TBIWriter; const AData: TDataItem); static;
  public
    const
      Version=8;
      SupportedFrom=7;
      DefaultBufferSize=32768;
      Extension='.bi';

    class var HideErrors : Boolean; // Do not raise exceptions when data cannot be loaded
    class var Raw : Boolean; // <-- when "True", data is always saved using TBIBinaryWriter

    class function CreateFile(const FileName:String):TFileStream; static;

    class function Load(const Stream:TStream):TDataItem; overload; static;
    class function Load(const FileName:String):TDataItem; overload; static;

    class function OpenFile(const FileName:String):TFileStream; static;

    class procedure Save(const Data:TDataItem; const Writer:TBIWriter); overload; static;
    class procedure Save(const Data:TDataItem; const Stream:TStream); overload; static;
    class procedure Save(const Data:TDataItem; const FileName:String); overload; static;
  end;

  // Loads and saves Items DATA to/from streams or disk files
  TDataPersistence=class
  private
    class function NumOfItems(const AData:TDataItem):Integer; static;
    class procedure Save(const Data:TDataItem; const Writer: TBIWriter; out APos:Int64); overload; static;
  protected
    class procedure LoadItem(const Reader:TBIReader; const AData:TDataItem); static;
    class procedure Load(const Stream: TStream; out Index: TInt32Array); overload; static;
    class procedure Load(const Stream: TStream; out Index: TInt64Array); overload; static;
  public
  const
    Version=5;
    SupportedFrom=5;
    Extension='.databi';

    class procedure DoLoad(const Reader:TBIReader; const Data:TDataItem;
                           const Children:Boolean; const ForceLoad:Boolean=False); static;

    class procedure Load(const Reader:TBIReader; const Data:TDataItem; const DelayLoad:Boolean=True); overload; static;
    class procedure Load(const Stream: TStream; const Items: TDataArray); overload; static;
    class procedure Load(const Stream:TStream; const Data:TDataItem; const DelayLoad:Boolean=True); overload; static;
    class procedure Load(const FileName:String; const Data:TDataItem; const DelayLoad:Boolean=True); overload; static;
    class procedure LoadFromFolder(const Folder:String; const Data:TDataItem; const DelayLoad:Boolean=True); overload; static;

    class procedure Save(const Items:TDataArray; const Stream:TStream); overload; static;
    class procedure Save(const Data:TDataItem; const Writer:TBIWriter; const Progress:TBIProgress=nil); overload; static;
    class procedure Save(const Data:TDataItem; const Stream:TStream; const Progress:TBIProgress=nil); overload; static;
    class procedure Save(const Data:TDataItem; const FileName:String; const Progress:TBIProgress=nil); overload; static;
    class procedure SaveToFolder(const Data:TDataItem; const Folder:String; const Progress:TBIProgress=nil); overload; static;
  end;

  // Both Data info (structure) and Data values, all together into a single stream
  TDataItemPersistence=class
  public
  const
    Extension='.bidata';

    class function Load(const Stream:TStream):TDataItem; overload; static;
    class function Load(const FileName:String):TDataItem; overload; static;

    class procedure Save(const Data:TDataItem; const Stream:TStream); overload; static;
    class procedure Save(const Data:TDataItem; const FileName:String); overload; static;
  end;

  // Cross-platform registry / inifile
  TBIRegistry=class
  private
    class function SteemaIni:String; static;
  public
    class var
      IniFile:String;
      UseRegistry:Boolean;

    class function Exists(const Key,Name:String):Boolean; static;

    class function ReadBoolean(const Key,Name:String; const Default:Boolean):Boolean; static;
    class function ReadInteger(const Key,Name:String; const Default:Integer):Integer; static;
    class function ReadString(const Key, Name:String; const Default: String=''): String; static;

    class procedure RemoveItem(const Key, Name:String); static;

    class procedure WriteBoolean(const Key,Name:String; const Value:Boolean); static;
    class procedure WriteInteger(const Key,Name:String; const Value:Integer); static;
    class procedure WriteString(const Key,Name,Value:String); static;
  end;

  EBILoadVersionException=class(Exception)
  public
    WrongVersion : Integer;

    Constructor CreateVersion(const AVersion:Integer);
  end;

  // Shared by all "DelayHandler" providers (Stream, Folder, Web)
  TDataDelayProvider=class(TDataProvider)
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  {WinApi.}Windows, Registry,
  {$ENDIF}

  {System.}IniFiles, {System.}DateUtils, {System.}SyncObjs,

  {$IFNDEF FPC}
  System.IOUtils,
  {$ENDIF}

  BI.Compression, BI.Expression, BI.Languages.English;

type
  // Provider class used to load data from a Stream, using the data "DelayPos"
  // as the offset position into the Stream where to start loading from.
  TDelayHandlerStream=class(TDataDelayProvider)
  private
    FLock : TCriticalSection;
    FStream : TStream;
  public
    Reader : TBIReader;

    Constructor Create(AOwner:TComponent); override;
    Constructor CreateStream(const AStream:TStream);
    Destructor Destroy; override;

    function GetStream(const AData,ANext:TDataItem):TStream; override;
    function GetStream(const AItems:TDataArray):TStream; override;
    procedure Init(const Tag:TObject); //override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Reset;

    property Stream:TStream read FStream;
  end;

{ TDelayHandlerStream }

Constructor TDelayHandlerStream.Create(AOwner:TComponent);
begin
  inherited;
  FLock:=TCriticalSection.Create;
end;

Constructor TDelayHandlerStream.CreateStream(const AStream:TStream);
begin
  Create(nil);
  FStream:=AStream;
end;

Destructor TDelayHandlerStream.Destroy;
begin
  Reader.Free;
  FStream.Free;
  FLock.Free;

  inherited;
end;

function TDelayHandlerStream.GetStream(const AItems: TDataArray): TStream;
begin
  result:=TMemoryStream.Create;

  // The trick of reading data directly from Stream is not possible with Items
  // because we don't know the size of the last item.
  TDataPersistence.Save(AItems,result);
  result.Position:=0;
end;

type
  TDataItemAccess=class(TDataItem);

// Returns a stream with the portion of bytes that belongs to AData.
// ANext is used to determine the end position of AData.
function TDelayHandlerStream.GetStream(const AData,ANext:TDataItem): TStream;

  function DelayPosOf(const AData:TDataItem):TInteger;
  begin
    result:=TDataItemAccess(AData).DelayPos;

    if result=0 then
       result:=TDataItemAccess(AData).SavedDelayPos;
  end;

var tmp : TInteger;
    tmpNext: TInteger;
    RawMode : Boolean;
begin
  tmp:=DelayPosOf(AData);
  Stream.Position:=tmp;

  if ANext=nil then
  begin
    if AData.AsTable or (AData.Items.Count=0) then
       tmpNext:=Stream.Size
    else
    begin
      tmpNext:=DelayPosOf(AData.Items[0]);

      if tmpNext=0 then
         tmpNext:=Stream.Size
    end;

  end
  else
     tmpNext:=DelayPosOf(ANext);

  result:=TMemoryStream.Create;

  // First byte (Raw mode)
  RawMode:=Reader.Raw;
  result.Write(RawMode,1);

  result.CopyFrom(Stream,tmpNext-tmp);
  result.Position:=0;
end;

procedure TDelayHandlerStream.Init(const Tag:TObject);
begin
  Reader:=Tag as TBIReader;
end;

procedure TDelayHandlerStream.Reset;
begin
  Reader.Free;
end;

procedure TDelayHandlerStream.Load(const AData:TDataItem; const Children:Boolean);
begin
  FLock.Enter;
  try
    Reader.Position:=TDataItemAccess(AData).DelayPos;
    TDataPersistence.DoLoad(Reader,AData,Children);
  finally
    FLock.Leave;
  end;
end;

{ TPersistence }

class function TPersistence.CreateFile(const FileName:String):TFileStream;
begin
  {$IFDEF FPC}
  result:=TFileStream.Create(FileName,fmCreate);
  {$ELSE}

  {$IF RTLVersion>=31.0}
  result:=TBufferedFileStream.Create(FileName,fmCreate);
  {$ELSE}
  result:=TFileStream.Create(FileName,fmCreate);
  {$ENDIF}

  {$ENDIF}
end;

class function TPersistence.OpenFile(const FileName:String):TFileStream;
begin
  {$IFDEF FPC}
  result:=TFileStream.Create(FileName,fmOpenRead+fmShareDenyWrite);
  {$ELSE}

  {$IF RTLVersion>=31.0}
  result:=TBufferedFileStream.Create(FileName,fmOpenRead+fmShareDenyWrite);
  {$ELSE}
  result:=TFileStream.Create(FileName,fmOpenRead+fmShareDenyWrite);
  {$ENDIF}

  {$ENDIF}
end;

class procedure TPersistence.Save(const Data: TDataItem; const FileName: String);
var f: TFileStream;
begin
  f:=CreateFile(FileName);
  try
    Save(Data,f);
  finally
    f.Free;
  end;
end;

class procedure TPersistence.Save(const Data: TDataItem; const Stream: TStream);
var w : TBIWriter;
begin
  w:=TBIWriter.Create(Stream,DefaultBufferSize,TPersistence.Raw);
  try
    w.WriteInteger(Version);
    Save(Data,w);
  finally
    w.Free;
  end;
end;

class procedure TPersistence.Save(const Writer: TBIWriter; const History: TImportHistory);

  procedure WriteTimes(const Times:TDataTimes);
  begin
    Writer.WriteInteger(Times.Opening);
    Writer.WriteInteger(Times.LoadingInfo);
    Writer.WriteInteger(Times.LoadingData);
    Writer.WriteInteger(Times.Total);
  end;

begin
  Writer.WriteDate(History.DateTime);
  Writer.WriteInteger(History.Memory);
  WriteTimes(History.Times);
end;

type
  TDataAccess=class(TDataItem);
  TDataMapAccess=class(TDataMap);

// Make sure DataMap is not nil
function CheckMap(const AData:TDataItem):TDataMap;
begin
  if AData.DataMap=nil then
     TDataAccess(AData).FDataMap:=TDataAccess(AData).CreateMap(AData.Kind);

  result:=AData.DataMap;
end;

class procedure TPersistence.ReadItem(const Reader: TBIReader; const AData: TDataItem);

  procedure ReadMap;
  var tmp : TDataMap;
  begin
    if Reader.ReadBoolean then
    begin
      tmp:=CheckMap(AData);

      TDataMapAccess(tmp).CachedCount:=Reader.ReadInt64;
      TDataMapAccess(tmp).FSorted:=TDataOrder(Reader.ReadInteger);
    end;
  end;

  procedure DoReadStats(const AStats:TInt32Stats); overload;
  begin
    AStats.Min:=Reader.ReadInteger;
    AStats.Max:=Reader.ReadInteger;
    AStats.Median:=Reader.ReadInteger;
    AStats.Mode:=Reader.ReadInteger;
  end;

  procedure DoReadStats(const AStats:TInt64Stats); overload;
  begin
    AStats.Min:=Reader.ReadInt64;
    AStats.Max:=Reader.ReadInt64;
    AStats.Median:=Reader.ReadInt64;
    AStats.Mode:=Reader.ReadInt64;
  end;

  procedure DoReadStats(const AStats:TSingleStats); overload;
  begin
    AStats.Min:=Reader.ReadSingle;
    AStats.Max:=Reader.ReadSingle;
    AStats.Median:=Reader.ReadSingle;
    AStats.Mode:=Reader.ReadSingle;
  end;

  procedure DoReadStats(const AStats:TDoubleStats); overload;
  begin
    AStats.Min:=Reader.ReadDouble;
    AStats.Max:=Reader.ReadDouble;
    AStats.Median:=Reader.ReadDouble;
    AStats.Mode:=Reader.ReadDouble;
  end;

  procedure DoReadExtendedStats(const AStats:TExtendedStats); overload;
  begin
    AStats.Min:=Reader.ReadFloat;
    AStats.Max:=Reader.ReadFloat;
    AStats.Median:=Reader.ReadFloat;
    AStats.Mode:=Reader.ReadFloat;
  end;

  procedure DoReadStats(const AStats:TDateTimeStats); overload;
  begin
    AStats.Min:=Reader.ReadDate;
    AStats.Max:=Reader.ReadDate;
    AStats.Median:=Reader.ReadDate;
    AStats.Mode:=Reader.ReadDate;
  end;

  procedure ReadStats(const AData:TDataItem);
  var Stats : TDataStats;
  begin
    if Reader.ReadBoolean then
    begin
      Stats:=TDataAccess(AData).CreateStats;

      Stats.Mean:=Reader.ReadFloat;
      Stats.Range:=Reader.ReadFloat;
      Stats.StdDeviation:=Reader.ReadFloat;
      Stats.Sum:=Reader.ReadFloat;
      Stats.Variance:=Reader.ReadFloat;
      Stats.Peakedness:=Reader.ReadFloat;
      Stats.Skewness:=Reader.ReadFloat;

      case AData.Kind of
          dkInt32: DoReadStats(TInt32Stats(Stats));
          dkInt64: DoReadStats(TInt64Stats(Stats));
         dkSingle: DoReadStats(TSingleStats(Stats));
         dkDouble: DoReadStats(TDoubleStats(Stats));
       dkExtended: DoReadExtendedStats(TExtendedStats(Stats));
           dkText: ;
       dkDateTime: DoReadStats(TDateTimeStats(Stats));
        dkBoolean: ;
        dkUnknown: ;
      end;
    end;
  end;

  procedure ReadMaster;
  begin
    if Reader.ReadBoolean then
       TDataAccess(AData).IMaster.Origin:=Reader.ReadString;
  end;

begin
  AData.Name:=Reader.ReadString;

  TDataAccess(AData).CheckEmptyName;

  TDataAccess(AData).FKind:=TDataKind(Reader.ReadInteger);

  AData.AsTable:=Reader.ReadBoolean;

  TDataAccess(AData).SetInternalDate(TDateTimePart(Reader.ReadInteger));

  TDataAccess(AData).FCount:=Reader.ReadInt64;
  AData.Missing.Count:=Reader.ReadInt64;

  TDataAccess(AData).FUnique:=Reader.ReadBoolean;
  AData.Primary:=Reader.ReadBoolean;

  if Reader.Version>=8 then
     AData.NumericValues:=TNumericData(Reader.ReadInteger);

  ReadMaster;

  ReadMap;
  ReadStats(AData);

  // Special case, Stats aren't valid because Missing items are not considered,
  // so, destroy Stats
  if Reader.Version<=7 then
     if AData.Missing.Count>0 then
     begin
       TDataAccess(AData).FStats.Free;
       TDataAccess(AData).FStats:=nil;
     end;
end;

class procedure TPersistence.WriteItem(const Writer: TBIWriter; const AData: TDataItem);

  procedure WriteMap(const Map:TDataMap);
  begin
    if Map=nil then
       Writer.WriteBoolean(False)
    else
    begin
      Writer.WriteBoolean(True);

      Writer.WriteInteger(TDataMapAccess(Map).CachedCount);
      Writer.WriteInteger(Ord(Map.Sorted));
    end;
  end;

  procedure DoWriteStats(const AStats:TInt32Stats); overload;
  begin
    Writer.WriteInteger(AStats.Min);
    Writer.WriteInteger(AStats.Max);
    Writer.WriteInteger(AStats.Median);
    Writer.WriteInteger(AStats.Mode);
  end;

  procedure DoWriteStats(const AStats:TInt64Stats); overload;
  begin
    Writer.WriteInteger(AStats.Min);
    Writer.WriteInteger(AStats.Max);
    Writer.WriteInteger(AStats.Median);
    Writer.WriteInteger(AStats.Mode);
  end;

  procedure DoWriteStats(const AStats:TSingleStats); overload;
  begin
    Writer.WriteSingle(AStats.Min);
    Writer.WriteSingle(AStats.Max);
    Writer.WriteSingle(AStats.Median);
    Writer.WriteSingle(AStats.Mode);
  end;

  procedure DoWriteStats(const AStats:TDoubleStats); overload;
  begin
    Writer.WriteDouble(AStats.Min);
    Writer.WriteDouble(AStats.Max);
    Writer.WriteDouble(AStats.Median);
    Writer.WriteDouble(AStats.Mode);
  end;

  procedure DoWriteExtendedStats(const AStats:TExtendedStats); overload;
  begin
    // We cannot save Stats as Extended in x64, because we're
    // changing Kind to dkDouble too.

    {$IFDEF EXTENDEDIS10BYTES}
    Writer.WriteFloat(AStats.Min);
    Writer.WriteFloat(AStats.Max);
    Writer.WriteFloat(AStats.Median);
    Writer.WriteFloat(AStats.Mode);
    {$ELSE}
    Writer.WriteDouble(AStats.Min);
    Writer.WriteDouble(AStats.Max);
    Writer.WriteDouble(AStats.Median);
    Writer.WriteDouble(AStats.Mode);
    {$ENDIF}
  end;

  procedure DoWriteStats(const AStats:TDateTimeStats); overload;
  begin
    Writer.WriteDate(AStats.Min);
    Writer.WriteDate(AStats.Max);
    Writer.WriteDate(AStats.Median);
    Writer.WriteDate(AStats.Mode);
  end;

  procedure WriteStats(const Kind:TDataKind; const Stats:TDataStats);
  begin
    if Stats=nil then
       Writer.WriteBoolean(False)
    else
    begin
      Writer.WriteBoolean(True);

      Writer.WriteFloat(Stats.Mean);
      Writer.WriteFloat(Stats.Range);
      Writer.WriteFloat(Stats.StdDeviation);
      Writer.WriteFloat(Stats.Sum);
      Writer.WriteFloat(Stats.Variance);
      Writer.WriteFloat(Stats.Peakedness);
      Writer.WriteFloat(Stats.Skewness);

      case Kind of
          dkInt32: DoWriteStats(TInt32Stats(Stats));
          dkInt64: DoWriteStats(TInt64Stats(Stats));
         dkSingle: DoWriteStats(TSingleStats(Stats));
         dkDouble: DoWriteStats(TDoubleStats(Stats));
       dkExtended: DoWriteExtendedStats(TExtendedStats(Stats));
           dkText: ;
       dkDateTime: DoWriteStats(TDateTimeStats(Stats));
        dkBoolean: ;
        dkUnknown: ;
      end;
    end;
  end;

  procedure WriteMaster(const AMasterOrigin:String);
  begin
    if AMasterOrigin='' then
       Writer.WriteBoolean(False)
    else
    begin
      Writer.WriteBoolean(True);
      Writer.WriteString(AMasterOrigin);
    end;
  end;

begin
  Writer.WriteString(AData.Name);

  {$IFNDEF EXTENDEDIS10BYTES}
  if AData.Kind=dkExtended then
     Writer.WriteInteger(Ord(dkDouble))
  else
  {$ENDIF}
     Writer.WriteInteger(Ord(AData.Kind));

  Writer.WriteBoolean(AData.AsTable);

  Writer.WriteInteger(Ord(TDataAccess(AData).IDate));

  Writer.WriteInt64(AData.Count);
  Writer.WriteInt64(AData.Missing.Count);
  Writer.WriteBoolean(AData.Unique);
  Writer.WriteBoolean(AData.Primary);
  Writer.WriteInteger(Ord(AData.NumericValues));

  WriteMaster(TDataAccess(AData).IMaster.Origin);

  // Removed. Do not force Stats (its caller responsability)
  //AData.Stats;

  // No map will be saved if DataMap=nil
  WriteMap(AData.DataMap);

  // No Stats will be saved if FStats=nil
  WriteStats(AData.Kind,TDataItemAccess(AData).FStats);
end;

class procedure TPersistence.Load(const Reader:TBIReader; const History:TImportHistory);

  procedure ReadTimes(var Times:TDataTimes);
  begin
    Times.Opening:=Reader.ReadInt64;
    Times.LoadingInfo:=Reader.ReadInt64;
    Times.LoadingData:=Reader.ReadInt64;
    Times.Total:=Reader.ReadInt64;
  end;

begin
  History.DateTime:=Reader.ReadDate;
  History.Memory:=Reader.ReadInt64;
  ReadTimes(History.Times);
end;

procedure VersionError(const Old:Integer);
begin
  raise EBILoadVersionException.CreateVersion(Old);
end;

class function TPersistence.Load(const Stream:TStream): TDataItem;
var r : TBIReader;
begin
  r:=TBIReader.Create(Stream,DefaultBufferSize);
  try
    // Check version
    r.Version:=r.ReadInteger;

    if (r.Version<SupportedFrom) or (r.Version>Version) then
    begin
      if not TPersistence.HideErrors then
         VersionError(r.Version);

      result:=nil;
    end
    else
      result:=Load(r);
  finally
    r.Free;
  end;
end;

type
  TDataItemsAccess=class(TDataItems);

class function TPersistence.Load(const Reader: TBIReader):TDataItem;
var t : Integer;
begin
  result:=TDataItem.Create;

  try
    ReadItem(Reader,result);
    Load(Reader,result.History);

    for t:=0 to Reader.ReadInteger-1 do
        TDataItemsAccess(result.Items).AddDirect(Load(Reader)); // <-- speed opt

  except
    on EReadError do
    begin
      // Avoid leak
      result.Free;

      raise;
    end;
  end;
end;

class procedure TPersistence.Save(const Data: TDataItem; const Writer: TBIWriter);
var tmp : TDataItem;
begin
  WriteItem(Writer,Data);
  Save(Writer,Data.History);

  if TDataAccess(Data).HasItems then
  begin
    Writer.WriteInteger(Data.Items.Count);

    for tmp in Data.Items.AsArray do
        Save(tmp,Writer);
  end
  else
    Writer.WriteInt64(0);
end;

class function TPersistence.Load(const FileName: String): TDataItem;
var f : TFileStream;
begin
  if TFile.Exists(FileName) then
  begin
    f:=OpenFile(FileName);
    try
      result:=Load(f);
    finally
      f.Free;
    end;
  end
  else
    result:=nil;
end;

{ TDataPersistence }

class procedure TDataPersistence.Save(const Data:TDataItem; const Writer: TBIWriter; out APos:Int64);

  procedure WriteItem(const AData:TDataItem);

    procedure WriteData(const Data:TInt32Array); overload;
    begin
      Writer.WriteInt64(Data.Count);

      if Data.Count>0 then
         Writer.Write(Data[0],Data.Count*SizeOf(Integer));
    end;

    procedure WriteData(const Data:TInt64Array); overload;
    begin
      Writer.WriteInt64(Data.Count);

      if Data.Count>0 then
         Writer.Write(Data[0],Data.Count*SizeOf(Int64));
    end;

    procedure WriteData(const Data:TSingleArray); overload;
    begin
      Writer.WriteInt64(Data.Count);

      if Data.Count>0 then
         Writer.Write(Data[0],Data.Count*SizeOf(Single));
    end;

    procedure WriteData(const Data:TDoubleArray); overload;
    begin
      Writer.WriteInt64(Data.Count);

      if Data.Count>0 then
         Writer.Write(Data[0],Data.Count*SizeOf(Double));
    end;

    {$IFDEF CPUX86}
    procedure WriteData(const Data:TExtendedArray); overload;
    begin
      Writer.WriteInt64(Data.Count);

      if Data.Count>0 then
         Writer.Write(Data[0],Data.Count*SizeOf(Extended));
    end;
    {$ENDIF}

    procedure WriteData(const Data:TTextArray); overload;
    var t : TLoopInteger;
    begin
      Writer.WriteInt64(Data.Count);

      for t:=0 to Data.Count-1 do
         Writer.WriteString(Data[t]);
    end;

    procedure WriteData(const Data:TDateTimeArray); overload;
    begin
      Writer.WriteInt64(Data.Count);

      if Data.Count>0 then
         Writer.Write(Data[0],Data.Count*SizeOf(TDateTime));
    end;

    procedure WriteData(const Data:TBooleanArray); overload;
    begin
      Writer.WriteInt64(Data.Count);

      if Data.Count>0 then
         Writer.Write(Data[0],Data.Count*SizeOf(Boolean));
    end;

    {$IFNDEF CPUX64}
    // For 32bit cpu, convert 32bit integer array to 64bit Int64 array.
    // This is a speed penalty (only for self-detail items) when saving data
    // but it allows internal use of 32bit integers for 32bit cpu
    procedure WriteData64(const Data:TNativeIntArray);
    var t : TLoopInteger;
        tmp : TInt64Array;
    begin
      {$IFDEF FPC}
      tmp:=nil;
      {$ENDIF}

      tmp.Resize(Data.Count);

      for t:=0 to High(Data) do
          tmp[t]:=Data[t];

      WriteData(tmp);
    end;
    {$ENDIF}

  var tmp : TDataMap;
      tmpSelf : Boolean;
  begin
    // Data

    {$IFNDEF EXTENDEDIS10BYTES}
    if AData.Kind=dkExtended then
       Writer.WriteInteger(Ord(dkDouble))
    else
    {$ENDIF}
       Writer.WriteInteger(Ord(AData.Kind));

    case AData.Kind of
        dkInt32: WriteData(AData.Int32Data);
        dkInt64: WriteData(AData.Int64Data);
       dkSingle: WriteData(AData.SingleData);
       dkDouble: WriteData(AData.DoubleData);
     dkExtended: WriteData(AData.ExtendedData);
         dkText: WriteData(AData.TextData);
     dkDateTime: WriteData(AData.DateTimeData);
      dkBoolean: WriteData(AData.BooleanData);
      dkUnknown: ;
    end;

    // Self-detail
    tmpSelf:=TDataAccess(AData).IMaster.IsSelfDetail;

    Writer.WriteBoolean(tmpSelf);

    if tmpSelf then
       {$IFDEF CPUX64}
       WriteData(TDataAccess(AData).IMaster.Index);
       {$ELSE}
       WriteData64(TDataAccess(AData).IMaster.Index);
       {$ENDIF}

    // Missing
    if AData.Missing.Items=nil then
       Writer.WriteInteger(Int64(0))
    else
       WriteData(AData.Missing.Items);

    // Map
    tmp:=AData.DataMap;

    if (tmp=nil) or (tmp.Count=0) then
       Writer.WriteBoolean(False)
    else
    begin
      Writer.WriteBoolean(True);
      WriteData(tmp.Map);

      case AData.Kind of
          dkInt32: WriteData(TInt32Map(tmp).Items);
          dkInt64: WriteData(TInt64Map(tmp).Items);
         dkSingle: WriteData(TSingleMap(tmp).Items);
         dkDouble: WriteData(TDoubleMap(tmp).Items);
       dkExtended: WriteData(TExtendedMap(tmp).Items);
           dkText: begin
                     //Writer.WriteBoolean(TTextMap(tmp).IgnoreCase);
                     WriteData(TTextMap(tmp).Items);
                   end;

       dkDateTime: WriteData(TDateTimeMap(tmp).Items);
        dkBoolean: begin
                     Writer.WriteInteger(TBooleanMap(tmp).Map[1]);
                     Writer.WriteInteger(TBooleanMap(tmp).Map[0]);
                   end;
      end;
    end;
  end;

begin
  Data.Load;

  APos:=Writer.Position;
  WriteItem(Data);
end;

{$IFNDEF CPUX86}

// Philipp S.  http://cc.embarcadero.com/Item/28488
type
  TExtended87=packed record
    Bytes : Array[0..9] of Byte;
    class operator Implicit(const E:TExtended87):Double; {$IFNDEF FPC}{$IF CompilerVersion>26}static;{$ENDIF}{$ENDIF}
  end;

class operator TExtended87.Implicit(const E:TExtended87):Double;
{$IFDEF LINUX}
begin
  result:=0; // NOT SUPPORTED in Lazarus + Linux 64bit
end;
{$ELSE}
{$IFDEF OSX}
begin
  result:=0; // NOT SUPPORTED in OSX64 ?? PENDING 
end;
{$ELSE}
{$IFDEF CPUX64}
{$IFDEF FPC}
begin
  System.Move(E,result,SizeOf(E));
end;
{$ELSE}
asm
  fld tbyte ptr E.Bytes; // [rcx]
  fstp qword ptr [rsp+08h]
  //fwait
  movsd xmm0, [rsp+08h];// Result:= xmm0
end;
{$ENDIF}
{$ELSE}
begin
  result:=Extended(E); // Pending
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$ENDIF}

class procedure TDataPersistence.LoadItem(const Reader:TBIReader; const AData: TDataItem);

  {  Possible?
  procedure ReadData<T>(var Data:TArray<T>);
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       Reader.Read(Data[0],Data.Count*SizeOf(T));
  end;
  }

  procedure ReadData(var Data:TInt32Array); overload;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       Reader.Read(Data[0],Data.Count*SizeOf(Integer));
  end;

  procedure ReadData(var Data:TInt64Array); overload;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       Reader.Read(Data[0],Data.Count*SizeOf(Int64));
  end;

  procedure ReadData(var Data:TSingleArray); overload;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       Reader.Read(Data[0],Data.Count*SizeOf(Single));
  end;

  procedure ReadData(var Data:TDoubleArray); overload;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       Reader.Read(Data[0],Data.Count*SizeOf(Double));
  end;

  {$IFDEF CPUX86}
  procedure ReadData(var Data:TExtendedArray); overload;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       Reader.Read(Data[0],Data.Count*SizeOf(Extended));
  end;
  {$ENDIF}

  {$IFNDEF CPUX86}
  procedure ConvertToDouble(var Data:TDoubleArray); overload;
  var Dummy : Array of TExtended87;
      t : TLoopInteger;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
    begin
      // "Extended" only for Win32
      SetLength(Dummy,Data.Count);
      Reader.Read(Dummy[0],Data.Count*SizeOf(TExtended87));

      // Pending: Conversion to double or single
      for t:=0 to Data.Count-1 do
          Data[t]:=Extended(Dummy[t]);
    end;
  end;
  {$ENDIF}

  procedure ReadData(var Data:TTextArray); overload;
  var t : TLoopInteger;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       for t:=0 to Data.Count-1 do
           Data[t]:=Reader.ReadString;
  end;

  procedure ReadData(var Data:TDateTimeArray); overload;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       Reader.Read(Data[0],Data.Count*SizeOf(TDateTime));
  end;

  procedure ReadData(var Data:TBooleanArray); overload;
  begin
    Data.Resize(Reader.ReadInt64);

    if Data.Count>0 then
       Reader.Read(Data[0],Data.Count*SizeOf(Boolean));
  end;

  {$IFNDEF CPUX64}
  // For 32bit cpu, convert 64bit Int64 array to 32bit Integer array.
  // This is a speed penalty (only for self-detail items) when loading data
  // but it allows internal use of 32bit integers for 32bit cpu
  procedure ReadData64(var Data:TNativeIntArray);
  var t : TLoopInteger;
      tmp : TInt64Array;
  begin
    {$IFDEF FPC}
    tmp:=nil;
    {$ENDIF}

    ReadData(tmp);

    Data.Resize(tmp.Count);

    for t:=0 to High(tmp) do
        Data[t]:=tmp[t];
  end;
  {$ENDIF}

  procedure ReadMap;
  var tmp : TDataMap;
  begin
    tmp:=CheckMap(AData);

    ReadData(tmp.Map);

    case AData.Kind of
        dkInt32: ReadData(TInt32Map(tmp).Items);
        dkInt64: ReadData(TInt64Map(tmp).Items);
       dkSingle: ReadData(TSingleMap(tmp).Items);
       dkDouble: ReadData(TDoubleMap(tmp).Items);
     dkExtended: ReadData(TExtendedMap(tmp).Items);
         dkText: begin
                   //TTextMap(tmp).IgnoreCase:=Reader.ReadBoolean;
                   ReadData(TTextMap(tmp).Items);
                 end;

     dkDateTime: ReadData(TDateTimeMap(tmp).Items);
      dkBoolean: begin
                   TBooleanMap(tmp).Map[1]:=Reader.ReadInt64;
                   TBooleanMap(tmp).Map[0]:=Reader.ReadInt64;
                 end;
    end;
  end;

var tmpKind : TDataKind;
begin
  // Data
  tmpKind:=TDataKind(Reader.ReadInteger);

  if AData.Kind<>tmpKind then
     raise EBIException.CreateFmt(BIMsg_Persist_CorruptLoad,[AData.Name,AData.Kind.ToString]);

  case AData.Kind of
      dkInt32: ReadData(AData.Int32Data); // --> ReadData<Integer>(AData.Int32Data);
      dkInt64: ReadData(AData.Int64Data);
     dkSingle: ReadData(AData.SingleData);
     dkDouble: ReadData(AData.DoubleData);
   dkExtended: {$IFDEF CPUX86}ReadData(AData.ExtendedData){$ELSE}ConvertToDouble(AData.ExtendedData){$ENDIF};
       dkText: ReadData(AData.TextData);
   dkDateTime: ReadData(AData.DateTimeData);
    dkBoolean: ReadData(AData.BooleanData);
    dkUnknown: ;
  end;

  // Self-detail
  if Reader.ReadBoolean then
     {$IFDEF CPUX64}
     ReadData(TDataAccess(AData).IMaster.Index);
     {$ELSE}
     ReadData64(TDataAccess(AData).IMaster.Index);
     {$ENDIF}

  // Missing
  ReadData(AData.Missing.Items);

  // Map
  if Reader.ReadBoolean then
     ReadMap;
end;

class procedure TDataPersistence.DoLoad(const Reader:TBIReader; const Data:TDataItem;
              const Children:Boolean; const ForceLoad:Boolean=False);
var Item : TDataItem;
    tmp : Int64;
begin
  tmp:=TDataAccess(Data).DelayPos;

  // Safety check:
  if ForceLoad or (tmp>0) then
  begin
    if tmp>0 then
       Reader.Position:=tmp;

    LoadItem(Reader,Data);

    if Children and (Data.AsTable or (Data.Kind=TDataKind.dkUnknown)) then
       for Item in Data.Items.AsArray do
           DoLoad(Reader,Item,Children,ForceLoad);

    TDataAccess(Data).ClearDelay;
  end;
end;

class procedure TDataPersistence.Load(const Reader:TBIReader; const Data: TDataItem; const DelayLoad:Boolean=True);

  procedure ReadIndex;
  var
    Offset : Integer;
    Index : TInt64Array;

    procedure SetDelayPos(const ACol:TDataItem);
    var tmp : TDataItem;
    begin
      TDataAccess(ACol).DelayPos:=Index[Offset];
      Inc(Offset);

      if TDataAccess(ACol).HasItems then
         for tmp in ACol.Items.AsArray do
             SetDelayPos(tmp);
    end;

  begin
    {$IFDEF FPC}
    Index:=nil;
    {$ENDIF}

    Index.Resize(Reader.ReadInt64);

    if Index.Count=NumOfItems(Data) then
    begin
      Reader.Read(Index[0],Index.Count*SizeOf(Int64));

      Offset:=0;

      if DelayLoad then
         SetDelayPos(Data);
    end
    else
       raise EBIException.CreateFmt(BIMsg_Persist_CorruptIndex,[Data.Name,Index.Count]);
  end;

begin
  ReadIndex;

  if DelayLoad and (Data.Provider<>nil) then
     TDelayHandlerStream(Data.Provider).Init(Reader)
  else
     DoLoad(Reader,Data,True,True);
end;

class procedure TDataPersistence.Load(const Stream: TStream; out Index: TInt32Array);
var r : TBIReader;
    n : TInteger;
begin
  r:=TBIReader.Create(Stream,TPersistence.DefaultBufferSize);
  try
    n:=r.ReadInt64;

    {$IFDEF FPC}
    Index:=nil;
    {$ENDIF}

    Index.Resize(n);
    r.Read(Index[0],n*SizeOf(Integer));
  finally
    r.Free;
  end;
end;

class procedure TDataPersistence.Load(const Stream: TStream; out Index: TInt64Array);
var r : TBIReader;
    n : TInteger;
begin
  r:=TBIReader.Create(Stream,TPersistence.DefaultBufferSize);
  try
    n:=r.ReadInt64;

    {$IFDEF FPC}
    Index:=nil;
    {$ENDIF}

    Index.Resize(n);
    r.Read(Index[0],n*SizeOf(Int64));
  finally
    r.Free;
  end;
end;

// Recursive count of Items, including AData
class function TDataPersistence.NumOfItems(const AData:TDataItem):Integer;
var tmp : TDataItem;
begin
  result:=1;

  if TDataAccess(AData).HasItems then
     for tmp in AData.Items.AsArray do
         Inc(result,NumOfItems(tmp));
end;

class procedure TDataPersistence.Load(const Stream: TStream; const Items: TDataArray);
var r : TBIReader;
    Item : TDataItem;
begin
  r:=TBIReader.Create(Stream,TPersistence.DefaultBufferSize);
  try
    r.Version:=r.ReadInteger;

    if (r.Version<SupportedFrom) or (r.Version>Version) then
       VersionError(r.Version)
    else
    for Item in Items do
        DoLoad(r,Item,True);
  finally
    r.Free;
  end;
end;

class procedure TDataPersistence.Load(const Stream: TStream; const Data: TDataItem; const DelayLoad:Boolean);
var r : TBIReader;
begin
  if DelayLoad then
     Data.Provider:=TDelayHandlerStream.CreateStream(Stream);

  r:=TBIReader.Create(Stream,TPersistence.DefaultBufferSize);
  try
    r.Version:=r.ReadInteger;

    if (r.Version<SupportedFrom) or (r.Version>Version) then
       VersionError(r.Version)
    else
       Load(r,Data,DelayLoad);
  finally
    if not DelayLoad then
       r.Free;
  end;
end;

class procedure TDataPersistence.Load(const FileName: String; const Data: TDataItem; const DelayLoad:Boolean);
var f : TFileStream;
begin
  f:=TPersistence.OpenFile(FileName);
  try
    Load(f,Data,DelayLoad);
  finally
    if not DelayLoad then
       f.Free;
  end;
end;

type
  // Provider class used to load data from files in a (sub)Folder.
  TDelayHandlerFolder=class(TDataDelayProvider)
  private
    IFolder : String; // Root folder
  protected
    function GetStream(const AData,ANext:TDataItem):TStream; override;
    function GetStream(const AData:TDataArray):TStream; override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    Constructor CreateFolder(const AFolder:String);
  end;

Constructor TDelayHandlerFolder.CreateFolder(const AFolder:String);
begin
  Create(nil);
  IFolder:=AFolder;
end;

class procedure TDataPersistence.LoadFromFolder(const Folder:String;
                  const Data:TDataItem; const DelayLoad:Boolean=True);
var f : TFileStream;
    r : TBIReader;
    tmp : TDataItem;
    Col : TDataItem;
begin
  if DelayLoad then
     Data.Provider:=TDelayHandlerFolder.CreateFolder(Folder)
  else
    for tmp in Data.Items.AsArray do
    begin
      f:=TPersistence.OpenFile(TPath.Combine(Folder,tmp.Name+Extension));
      try
        r:=TBIReader.Create(f,TPersistence.DefaultBufferSize);
        try
          Col:=tmp;
          LoadItem(r,Col);
        finally
          r.Free;
        end;
      finally
        f.Free;
      end;
    end;
end;

class procedure TDataPersistence.Save(const Data: TDataItem; const Stream:TStream; const Progress:TBIProgress=nil);
var w : TBIWriter;
begin
  w:=TBIWriter.Create(Stream,TPersistence.DefaultBufferSize,TPersistence.Raw);
  try
    w.WriteInteger(Version);
    Save(Data,w,Progress);
  finally
    w.Free;
  end;
end;

class procedure TDataPersistence.Save(const Data: TDataItem; const Writer:TBIWriter; const Progress:TBIProgress=nil);
var
  Index : TInt64Array;
  tmpInv : Single;
  Offset : Integer;

  procedure DoSave(const AData:TDataItem; var Cancel:Boolean);
  var tmp : TDataItem;
  begin
    Save(AData,Writer,Index[Offset]);
    Inc(Offset);

    if TDataAccess(AData).HasItems then
       for tmp in AData.Items.AsArray do
       begin
         DoSave(tmp,Cancel);

         if Cancel then
            break;
       end;

    if Assigned(Progress) then
       Progress(AData,Offset*tmpInv,Cancel);
  end;

  procedure WriteIndex;
  begin
    Writer.Write(Index[0],Index.Count*SizeOf(Int64));
  end;

var IndexPos : TInteger;
    tmpCancel : Boolean;
begin
  tmpCancel:=False;

  if Assigned(Progress) then
     Progress(Data,0,tmpCancel);

  if not tmpCancel then
  begin
    {$IFDEF FPC}
    Index:=nil;
    {$ENDIF}

    Index.Resize(NumOfItems(Data));

    Writer.WriteInt64(Index.Count);

    IndexPos:=Writer.Position;
    WriteIndex;

    if Index.Count>0 then
    begin
      tmpInv:=100/Index.Count;

      Offset:=0;
      DoSave(Data,tmpCancel);
    end;

    Writer.FlushBuffer;

    // Rewrite Index
    Writer.Position:=IndexPos;
    WriteIndex;

    if Assigned(Progress) then
       Progress(Data,100,tmpCancel);
  end;
end;

class procedure TDataPersistence.Save(const Data: TDataItem;
  const FileName: String; const Progress:TBIProgress=nil);
var f : TFileStream;
begin
  f:=TPersistence.CreateFile(FileName);
  try
    Save(Data,f,Progress);
  finally
    f.Free;
  end;
end;

class procedure TDataPersistence.Save(const Items: TDataArray; const Stream: TStream);
var
  w : TBIWriter;

  procedure DoSave(const AData:TDataItem);
  var Dummy : Int64;
      Item : TDataItem;
  begin
    Save(AData,w,Dummy);

    if TDataAccess(AData).HasItems then
       for Item in AData.Items.AsArray do
           DoSave(Item);
  end;

var Item : TDataItem;
begin
  w:=TBIWriter.Create(Stream,TPersistence.DefaultBufferSize,TPersistence.Raw);
  try
    for Item in Items do
        DoSave(Item);
  finally
    w.Free;
  end;
end;

class procedure TDataPersistence.SaveToFolder(const Data:TDataItem; const Folder:String; const Progress:TBIProgress=nil);

  function FileNameOf(const AData:TDataItem):String;
  begin
    result:=TPath.Combine(Folder,AData.Name+Extension);
  end;

var t : Integer;
    tmpInv : Single;
    tmpCancel : Boolean;
begin
  tmpCancel:=False;

  if Assigned(Progress) then
     Progress(Data,0,tmpCancel);

  if not tmpCancel then
  try
    ForceDirectories(Folder);

    if Data.AsTable then
    begin
      if Data.Items.Count>0 then
      begin
        tmpInv:=100/Data.Items.Count;

        for t:=0 to Data.Items.Count-1 do
        begin
          Save(Data.Items.AsArray[t],FileNameOf(Data.Items.AsArray[t]),Progress);

          if Assigned(Progress) then
          begin
            Progress(Data,t*tmpInv,tmpCancel);

            if tmpCancel then
               break;
          end;
        end;
      end;
    end
    else
      Save(Data,FileNameOf(Data),Progress);

  finally
    if Assigned(Progress) then
       Progress(Data,100,tmpCancel);
  end;
end;

{ TDataItemPersistence }

class function TDataItemPersistence.Load(const Stream: TStream): TDataItem;
begin
  result:=TPersistence.Load(Stream);
  TDataPersistence.Load(Stream,result,False);
end;

class function TDataItemPersistence.Load(const FileName: String): TDataItem;
var f : TFileStream;
begin
  f:=TPersistence.OpenFile(FileName);
  try
    result:=Load(f);
  finally
    f.Free;
  end;
end;

class procedure TDataItemPersistence.Save(const Data: TDataItem; const Stream: TStream);
begin
  // Call Data.Load, to make sure (in case of Data.Provider is a BIQuery)
  // the Query will be executed before returning the output.
  //
  // This is not always necessary, but in case of BIQuery is very important
  // because initially Data.AsTable=False, and after executing the BIQuery
  // it can usually be set to True
  Data.Load;

  TPersistence.Save(Data,Stream);
  TDataPersistence.Save(Data,Stream);
end;

class procedure TDataItemPersistence.Save(const Data: TDataItem; const FileName: String);
var f : TFileStream;
begin
  f:=TPersistence.CreateFile(FileName);
  try
    Save(Data,f);
  finally
    f.Free;
  end;
end;

{ TStore }

class function TStore.IsRemote(const AStore:String):Boolean;
var tmp : String;
begin
  if AStore='' then
     tmp:=PathOf(DefaultName)
  else
     tmp:=PathOf(AStore);

  result:=SameText(Copy(tmp,1,4),'WEB:');
end;

class function TStore.GuessImporter(const AStore:String):TDataImporter;
var tmp : String;
begin
  if IsRemote(AStore) then
     result:=TDataImporter.Guess(TDataDefinitionKind.Web,AStore,nil)
  else
  begin
    if AStore='' then
       tmp:=PathOf(DefaultName)
    else
       tmp:=PathOf(AStore);

    if DirectoryExists(tmp) then
       result:=TDataImporter.Guess(TDataDefinitionKind.Files,AStore,nil)
    else
       result:=nil;
  end;

  if result=nil then
     raise EBIException.CreateFmt(BIMsg_Store_MissingImporter,[AStore]);
end;

class function TStore.AllData(const AStore:String): TStringArray;
var D : TDataImporter;
begin
  D:=GuessImporter(AStore);

  if D=nil then
     result:=nil
  else
  try
    result:=D.AllData;
  finally
    D.Free;
  end;
end;

class function TStore.DataToStream(const AName:String; const Zip:Boolean):TStream;
begin
  result:=TStore.DataToStream(TStore.DefaultName,AName,Zip);
end;

class function TStore.ZipStream(const AStream: TStream): TStream;
var tmp : TStream;
begin
  tmp:=AStream;
  try
    tmp.Position:=0;
    result:=TCompression.Compress(tmp,'data');
  finally
    tmp.Free;
  end;
end;

class function TStore.DataToStream(const AData:TDataItem; const Zip:Boolean=False):TStream;
begin
  if AData=nil then
     result:=nil
  else
  begin
    result:=TMemoryStream.Create;
    TPersistence.Save(AData,result);

    if Zip then
       result:=ZipStream(result);
  end;
end;

class function TStore.DataToStream(const APath,AName:String; const Zip:Boolean=False):TStream;
begin
  result:=DataToStream(Load(APath,AName),Zip);
end;

type
  // Placeholder
  TIdentifiers=record
    // Geo World tree identifiers (Continents->Countries->Regions->Provinces->Cities, etc)
    // Colors ('Black'-> TAlphaColor.Black...)
  end;

  // Data cache management
  TEnvironment=class
  private
    class procedure Init; static;
    class procedure Finish; static;
  public
    class var Data : TDataItem;
    class var Identifiers : TIdentifiers;

    class procedure Add(const AStore:String; const AData:TDataItem); static;
    class function Find(const AStore,AName:String):TDataItem; static;
    class procedure TryKeepMemory; static;
  end;

{ TEnvironment }

class procedure TEnvironment.Init;
begin
  Data:=TDataItem.Create;
  Data.Name:='Global Cache';
end;

class procedure TEnvironment.Add(const AStore:String; const AData: TDataItem);
var tmp : TDataItem;
begin
  tmp:=Data.Items.Find(AStore);

  if tmp=nil then
  begin
    tmp:=TDataItem.Create;
    tmp.Name:=AStore;
    Data.Items.Add(tmp);
  end;

  tmp.Items.Add(AData);
end;

class function TEnvironment.Find(const AStore,AName: String): TDataItem;
begin
  result:=Data.Items.Find(AStore);

  if result<>nil then
     result:=result.Items.Find(AName);
end;

class procedure TEnvironment.Finish;
begin
  Data.Free;
end;

type
  TDataAccessTimes=class
  private
    Access : TDateTimeArray;
    Data : TDataArray;

    function Add(const AData:TDataItem):TInteger;
    procedure SwapItem(const A,B:TInteger);
  end;

function TDataAccessTimes.Add(const AData: TDataItem):TInteger;
begin
  result:=Data.Count;
  Data.Add(AData);

  Access.Resize(result+1);
  Access[result]:=AData.LastAccess;
end;

procedure TDataAccessTimes.SwapItem(const A,B:TInteger);
var tmp : TDataItem;
begin
  Access.Swap(A,B);

  tmp:=Data[A];
  Data[A]:=Data[B];
  Data[B]:=tmp;
end;

class procedure TEnvironment.TryKeepMemory;
var
  Inst : TDataAccessTimes;

  procedure AddData(const AData:TDataItem);
  var Item : TDataItem;
  begin
    if TDataAccess(AData).HasItems then
       for Item in AData.Items.AsArray do
       begin
         if TDataAccess(Item).DelayPos=0 then
            Inst.Add(Item);

         AddData(Item);
       end;
  end;

var tmpIndex,
    tmpCount : Integer;
begin
  if TMemory.Available<TMemory.MinLimit then
  begin
    Inst:=TDataAccessTimes.Create;
    try
      AddData(Data);

      Inst.Access.Sort(True,Inst.SwapItem);

      tmpCount:=Inst.Access.Count;
      tmpIndex:=0;

      while (tmpIndex<tmpCount) and (TMemory.Available<TMemory.MinLimit) do
      begin
        Inst.Data[tmpIndex].UnloadData;
        Inc(tmpIndex);
      end;

    finally
      Inst.Free;
    end;
  end;
end;

class function TStore.Load(const AStore,AName:String; const OnError:TBIErrorProc):TDataItem;

  procedure TryRaiseError;
  var tmp : String;
  begin
    tmp:=Format(BIMsg_Store_DataLoadError,[AName,AStore]);

    if (not Assigned(OnError)) or (not OnError(nil,tmp)) then
        raise EBIException.Create(tmp);
  end;

var D : TDataImporter;
begin
  result:=TEnvironment.Find(AStore,AName);

  if result=nil then
  begin
    D:=GuessImporter(AStore);
    try
      try
        result:=D.Load(AName);

        if result=nil then
           TryRaiseError
        else
           TEnvironment.Add(AStore,result);

      except
        on E:Exception do
           if (not Assigned(OnError)) or (not OnError(nil,E.Message)) then
              raise;
      end;

    finally
      D.Free;
    end;
  end;

  if result<>nil then
     result.LastAccess:=Now;
end;

class function TStore.Load(const AName:String):TDataItem;
begin
  result:=Load(DefaultName,AName);
end;

class procedure TStore.UnLoad(const AStore,AName:String);
var tmp : TDataItem;
begin
  tmp:=TEnvironment.Find(AStore,AName);

  if tmp<>nil then
     tmp.Free;
end;

class procedure TStore.UnLoadAll;
begin
  TEnvironment.Data.UnLoadData;
end;

class function TStore.NameOf(const FileName: String): String;
begin
  result:=TPath.GetFileNameWithoutExtension(FileName);
end;

// Origin string for AData, optionally relative to RelativeTo data, or abolute if RelativeTo=nil
class function TStore.OriginOf(const AData:TDataItem;
                               const DefaultStore:String;
                               const RelativeTo:TDataItem=nil):String;

  function CommonParent(out AParent:TDataItem):String;
  begin
    result:='';

    AParent:=AData.Parent;

    if AParent<>nil then
    begin
      if AParent=RelativeTo then
         Exit('.|')
      else
      repeat
        result:='..|'+result;

        if RelativeTo.IsChildOf(AParent) then
           Exit
        else
           AParent:=AParent.Parent;

      until AParent=nil;
    end;

    result:=''; // raise !!!
  end;

  function PathFrom(const AParent,ADest:TDataItem):String;
  var tmp : TDataItem;
  begin
    tmp:=ADest;
    result:=tmp.Name;

    repeat
      if tmp.Parent=AParent then
         Exit
      else
      begin
        result:=tmp.Parent.Name+'|'+result;
        tmp:=tmp.Parent;
      end;

    until tmp=nil;

    result:=''; // raise !!
  end;

var tmp : TDataItem;
begin
  if AData=nil then
     result:=''
  else
  begin
    if RelativeTo=nil then
    begin
      result:=AData.Name;

      if AData.Parent<>nil then
         if AData.Parent=TStores.GlobalCache then
            if SameText(AData.Name,DefaultStore) then
               result:=''
            else
               result:=result+':'
         else
            result:=OriginOf(AData.Parent,DefaultStore)+'|'+result;
    end
    else
    begin
      if AData=RelativeTo.Parent then
         result:='..'
      else
      begin
        result:=CommonParent(tmp);

        if tmp=nil then
           result:=OriginOf(AData,DefaultStore)
        else
           result:=result+PathFrom(tmp,AData);
      end;
    end;
  end;
end;

class function TStore.OriginToData(const AData:TDataItem; const AStore,AOrigin:String;
                                   const Error:TBIErrorProc=nil):TDataItem;
var
  tmpStore : String;
  tmpData : TDataItem;

  procedure DoError(const S:String);
  begin
    if (not Assigned(Error)) or (not Error(nil,S)) then
       raise EBIException.Create(S)
    else
       tmpData:=nil;
  end;

  procedure DoLoad(const AName:String);

     procedure FindData;
     var tmp : TDataItem;
     begin
       if tmpData=nil then
          tmpData:=AData;

       if AName<>'.' then
          if AName='..' then
          begin
            if tmpData.Parent=nil then
               DoError(Format(BIMsg_RelativeOriginNoParent,[tmpData.Name]))
            else
               tmpData:=tmpData.Parent;
          end
          else
          if Assigned(Error) then
          begin
            tmp:=tmpData.Items.Find(AName);

            if tmp=nil then
               DoError(Format(BIMsg_CannotAccessData,[AName,tmpData.Name]))
            else
               tmpData:=tmp;
          end
          else
            tmpData:=tmpData[AName];
     end;

   begin
     if tmpData=nil then
     begin
       if tmpStore='' then
          if AData<>nil then
          begin
            //if AData.Parent=nil then // ??
            begin
              if SameText(AData.Name,AName) then
                 tmpData:=AData
              else
                 FindData;
            end
            {
            else
              FindData};

            Exit;
          end
          else
             tmpStore:=AStore;

       if tmpStore='' then
          tmpData:=TStore.Load(TStore.DefaultName,AName,Error)
       else
          tmpData:=TStore.Load(tmpStore,AName,Error);
     end
     else
       FindData;
  end;

var tmp,
    tmpS : String;
    i : Integer;
begin
  tmp:=Trim(AOrigin);

  tmpData:=nil;

  if tmp<>'' then
  begin
    i:=Pos(':',tmp);

    if i>0 then
    begin
      tmpStore:=Copy(tmp,1,i-1);
      System.Delete(tmp,1,i);

      if tmpStore='' then
         tmpStore:=TStore.DefaultName;
    end
    else
      tmpStore:='';

    while tmp<>'' do
    begin
      i:=Pos('|',tmp);

      if i>0 then
      begin
        tmpS:=Copy(tmp,1,i-1);

        if tmpS<>'' then
           DoLoad(tmpS);

        System.Delete(tmp,1,i);
      end
      else
      begin
        DoLoad(tmp);
        tmp:='';
      end;
    end;
  end;

  result:=tmpData;
end;

class function TStore.NameOrDefault(const AName: String): String;
begin
  result:=Trim(AName);

  if result='' then
     result:=BIMsg_Default;
end;

class function TStore.NotRegistered(const AName: String):EBIException;
begin
  result:=EBIException.CreateFmt(BIMsg_Store_NotRegistered,[NameOrDefault(AName)]);
end;

class function TStore.PathOf(const AName: String): String;
var tmp : Integer;
begin
  tmp:=TStores.IndexOf(AName);

  if tmp=-1 then
     raise NotRegistered(AName)
  else
     result:=TStores.ReadPart(tmp,1);
end;

class procedure TStore.RemoveData(const AStore,AName: String);
var tmp : String;
begin
  tmp:=TStore.DefinitionOf(AStore,AName);

  if TFile.Exists(tmp) then
     TFile.Delete(tmp);

  tmp:=TStore.FullPath(AStore,AName+TPersistence.Extension);

  if TFile.Exists(tmp) then
     TFile.Delete(tmp);

  tmp:=TStore.FullPath(AStore,AName+TDataPersistence.Extension);

  if TFile.Exists(tmp) then
     TFile.Delete(tmp);

  // Pending: Remove AName sub-folder
end;

class procedure TStore.Rename(const AStore,AOld,ANew:String);

  procedure DoRenameFile(const AOld:String);
  var tmpNew : String;
  begin
    tmpNew:=TPath.Combine(TPath.GetDirectoryName(AOld),ANew);
    tmpNew:=TPath.ChangeExtension(tmpNew,TPath.GetExtension(AOld));
    TFile.Move(AOld,tmpNew);
  end;

  procedure TryRename(const AOld:String);
  begin
    if TFile.Exists(AOld) then
       DoRenameFile(AOld);
  end;

var tmpDir : String;
begin
  // Ensure data is not loaded before renaming its files and folder:
  TEnvironment.Find(AStore,AOld).{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};

  // No transaction support. Any error leaves data files partially renamed.

  TryRename(TStore.DefinitionOf(AStore,AOld));
  TryRename(TStore.FullPath(AStore,AOld+TPersistence.Extension));
  TryRename(TStore.FullPath(AStore,AOld+TDataPersistence.Extension));

  // Pending extensions: .detail, .queries

  // Rename sub-folder

  tmpDir:=TStore.FullPath(AStore,AOld);

  if TDirectory.Exists(tmpDir) then
     TDirectory.Move(tmpDir,TStore.FullPath(AStore,ANew));
end;

const
  Steema_Key='Software\Steema Software\TeeBI\';
  Steema_Ini='TeeBI.ini';
  Default_Key='Default';

class function TBIRegistry.SteemaIni:String;
begin
  if IniFile='' then
     result:=TPath.Combine(TPath.GetHomePath,Steema_Ini)
  else
     result:=IniFile;
end;

class function TBIRegistry.Exists(const Key,Name:String):Boolean;
begin
  {$IFDEF MSWINDOWS}
  if UseRegistry then
  with TRegistry.Create(KEY_READ) do
  try
    result:=OpenKeyReadOnly(Steema_Key+Key) and ValueExists(Name);
  finally
    Free;
  end
  else
  {$ENDIF}
  with TIniFile.Create(SteemaIni) do
  try
    result:=ValueExists(Key,Name);
  finally
    Free;
  end;
end;

class function TBIRegistry.ReadBoolean(const Key, Name: String; const Default:Boolean): Boolean;
begin
  {$IFDEF MSWINDOWS}
  result:=Default;

  if UseRegistry then
  with TRegistry.Create(KEY_READ) do
  try
    if OpenKeyReadOnly(Steema_Key+Key) then
       if ValueExists(Name) then
          result:=ReadBool(Name);
  finally
    Free;
  end
  else
  {$ENDIF}
  with TIniFile.Create(SteemaIni) do
  try
    result:=ReadBool(Key,Name,Default);
  finally
    Free;
  end;
end;

class function TBIRegistry.ReadInteger(const Key, Name: String;
  const Default: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  result:=Default;

  if UseRegistry then
  with TRegistry.Create(KEY_READ) do
  try
    if OpenKeyReadOnly(Steema_Key+Key) then
       if ValueExists(Name) then
          result:=ReadInteger(Name);
  finally
    Free;
  end
  else
  {$ENDIF}
  with TIniFile.Create(SteemaIni) do
  try
    result:=ReadInteger(Key,Name,Default);
  finally
    Free;
  end;
end;

class procedure TBIRegistry.WriteBoolean(const Key, Name: String;
  const Value: Boolean);
begin
  {$IFDEF MSWINDOWS}
  if UseRegistry then
  with TRegistry.Create do
  try
    if OpenKey(Steema_Key+Key,True) then
       WriteBool(Name,Value);
  finally
    Free;
  end
  else
  {$ENDIF}
  with TIniFile.Create(SteemaIni) do
  try
    WriteBool(Key,Name,Value);
  finally
    Free;
  end;
end;

class procedure TBIRegistry.WriteInteger(const Key, Name: String;
  const Value: Integer);
begin
  {$IFDEF MSWINDOWS}
  if UseRegistry then
  with TRegistry.Create do
  try
    if OpenKey(Steema_Key+Key,True) then
       WriteInteger(Name,Value);
  finally
    Free;
  end
  else
  {$ENDIF}
  with TIniFile.Create(SteemaIni) do
  try
    WriteInteger(Key,Name,Value);
  finally
    Free;
  end;
end;

class procedure TBIRegistry.WriteString(const Key,Name,Value:String);
begin
  {$IFDEF MSWINDOWS}
  if UseRegistry then
  with TRegistry.Create do
  try
    if OpenKey(Steema_Key+Key,True) then
       WriteString(Name,Value);
  finally
    Free;
  end
  else
  {$ENDIF}
  with TIniFile.Create(SteemaIni) do
  try
    WriteString(Key,Name,Value);
  finally
    Free;
  end;
end;

class function TBIRegistry.ReadString(const Key, Name, Default: String): String;
begin
  {$IFDEF MSWINDOWS}
  result:=Default;

  if UseRegistry then
  with TRegistry.Create(KEY_READ) do
  try
    if OpenKeyReadOnly(Steema_Key+Key) then
       if ValueExists(Name) then
          result:=ReadString(Name);
  finally
    Free;
  end
  else
  {$ENDIF}
  with TIniFile.Create(SteemaIni) do
  try
    result:=ReadString(Key,Name,Default);
  finally
    Free;
  end;
end;

class procedure TBIRegistry.RemoveItem(const Key, Name: String);
begin
  {$IFDEF MSWINDOWS}
  if UseRegistry then
  with TRegistry.Create do
  try
    if OpenKey(Steema_Key+Key,False) then
       if ValueExists(Name) then
          DeleteValue(Name);
  finally
    Free;
  end
  else
  {$ENDIF}
  with TIniFile.Create(SteemaIni) do
  try
    DeleteKey(Key,Name);
  finally
    Free;
  end;
end;

{ TStore }

class procedure TStore.Save(const Data: TDataItem; const AFileName: String;
                            const AsFolder:Boolean=False; const Progress:TBIProgress=nil);
begin
  TPersistence.Save(Data,AFileName+TPersistence.Extension);

  if AsFolder then
     TDataPersistence.SaveToFolder(Data,AFileName,Progress)
  else
     TDataPersistence.Save(Data,AFileName+TDataPersistence.Extension,Progress);
end;

class procedure TStore.SetDefaultName(const Value: String);
begin
  IDefault:=Value;
  TBIRegistry.WriteString('Store',Default_Key,Value);
end;

class function TStore.StoreOf(const AData: TDataItem): TDataItem;
var tmp : TDataItem;
begin
  if AData<>nil then
  begin
    tmp:=AData.Parent;

    while tmp<>nil do
    begin
      if tmp.Parent=TStores.GlobalCache then
         Exit(tmp)
      else
         tmp:=tmp.Parent;
    end;
  end;

  result:=nil;
end;

class function TStore.GetDefaultName: String;
begin
  result:=TBIRegistry.ReadString('Store',Default_Key);
end;

class function TStore.GetDefinition(const AStore, AName: String): TDataDefinition;
var D : TDataImporter;
begin
  D:=GuessImporter(AStore);
  try
    result:=D.GetDefinition(AName);
  finally
    D.Free;
  end;
end;

class function TStore.DefinitionOf(const AStore,Name: string): String;
begin
  result:=TPath.Combine(PathOf(AStore),Name+TDataDefinition.Extension);
end;

class function TStore.DefinitionOf(const Name: string): String;
begin
  result:=TStore.DefinitionOf(TStore.DefaultName,Name);
end;

class function TStore.FullPath(const AStore,FileName: string): String;
begin
  if AStore='' then
     result:=FileName
  else
     result:=TPath.Combine(PathOf(AStore),FileName);
end;

class function TStore.FullPath(const FileName: string): String;
begin
  result:=TStore.FullPath(TStore.DefaultName,FileName);
end;

{ TDataDefinition }

// Creates a new empty *.def data definition file
Constructor TDataDefinition.Create(AOwner: TComponent);
begin
  inherited;

  FDatabase:=TDatabaseDefinition.Create;
  FDatabase.IDefinition:=Self;
end;

// Creates a TDataDefinition instance from an existing *.def FileName
Constructor TDataDefinition.FromFile(const AOwner:TComponent; const AFileName: String);
begin
  Create(AOwner);
  FileName:=AFileName;
end;

Destructor TDataDefinition.Destroy;
begin
  FLinks.Free;
  FStrings.Free;
  FDatabase.Free;

  if FData<>nil then
     TDataItemAccess(FData).FConsumers.Remove(NotifyManual);

  inherited;
end;

function TDataDefinition.AsBoolean(const Key: String; const ADefault:Boolean): Boolean;
var tmp : String;
begin
  tmp:=Value[Key];

  if SameText(tmp,'TRUE') then
     result:=True
  else
  if SameText(tmp,'FALSE') then
     result:=False
  else
     result:=ADefault;
end;

class function TDataDefinition.KindToString(const AKind:TDataDefinitionKind):String;
begin
  case AKind of
       Files: result:='FILE';
    Database: result:='DB';
         Web: result:='WEB';
      Manual: result:='MANUAL';
  else
    result:='?';
  end;
end;

class procedure TDataDefinition.CreateFile(const FileName:String; const Kind: TDataDefinitionKind);
var S : TStrings;
begin
  S:=TStringList.Create;
  try
    S.Values['Kind']:=KindToString(Kind);
    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;

function TDataDefinition.GetCalcStats: Boolean;
begin
  result:=AsBoolean('CalcStats',False);
end;

procedure TDataDefinition.GetItems(const AData: TDataItem);
begin
  inherited;
  Load(AData,True);
end;

function TDataDefinition.GetKind: TDataDefinitionKind;
var tmp : String;
begin
  tmp:=GetValue('Kind');

  if SameText(tmp,KindToString(TDataDefinitionKind.Files)) then
     result:=TDataDefinitionKind.Files
  else
  if SameText(tmp,KindToString(TDataDefinitionKind.Database)) then
     result:=TDataDefinitionKind.Database
  else
  if SameText(tmp,KindToString(TDataDefinitionKind.Web)) then
     result:=TDataDefinitionKind.Web
  else
  if SameText(tmp,KindToString(TDataDefinitionKind.Manual)) then
     result:=TDataDefinitionKind.Manual
  else
     result:=TDataDefinitionKind.Unknown;
end;

function TDataDefinition.GetLinks: TDataRelations;
begin
  if FLinks=nil then
     FLinks:=TDataRelations.Create(Self,TDataRelation);

  result:=FLinks;
end;

function TDataDefinition.GetParallel: Boolean;
begin
  result:=AsBoolean('Parallel',False);
end;

function TDataDefinition.GetStrings:TStrings;
begin
  if FStrings=nil then
     FStrings:=TStringList.Create;

  result:=FStrings;
end;

function TDataDefinition.GetValue(const Index: String): String;
begin
  if FStrings=nil then
     result:=''
  else
     result:=FStrings.Values[Index];
end;

procedure TDataDefinition.SetCalcStats(const AValue: Boolean);
begin
  Value['CalcStats']:=BoolToStr(AValue,True);
end;

procedure TDataDefinition.TryLoadFile;
begin
  if (not (csLoading in ComponentState)) and (FileName<>'') then
     LoadFromFile(FileName);
end;

procedure TDataDefinition.SetFileName(const Value: String);
begin
  if FFileName<>Value then
  begin
    FFileName:=Value;
    TryLoadFile;
  end;
end;

procedure TDataDefinition.NotifyManual(const AEvent:TBIEvent);
var tmp : String;
begin
  case AEvent of
    TBIEvent.Changed,
    TBIEvent.ChangedValues:
      begin
        tmp:=TStore.FullPath(Store,Description)+TPersistence.Extension;
        TDataItemPersistence.Save(Data,tmp);
      end;
  end;
end;

procedure TDataDefinition.GetKindSettings(const Value: TDataDefinitionKind);
begin
  IKeepData:=Value=TDataDefinitionKind.Manual;
end;

procedure TDataDefinition.SetKind(const Value: TDataDefinitionKind);
begin
  if Kind<>Value then
  begin
    SetValue('Kind',KindToString(Value));
    GetKindSettings(Value);
  end;
end;

procedure TDataDefinition.SetLinks(const Value: TDataRelations);
begin
  Links.Assign(Value);
end;

class procedure TDataDefinition.SetMasters(const AData:TDataItem;
                                           const AStore:String;
                                           const Items:TDataRelations);
var Master,
    Detail : TDataItem;
    tmp: TDataRelation;
    t : Integer;
begin
  for t:=0 to Items.Count-1 do
  begin
    tmp:=Items[t];

    Master:=TStore.OriginToData(AData,AStore,tmp.Master);
    Detail:=TStore.OriginToData(AData,AStore,tmp.Detail);

    Detail.Master:=Master;
  end;
end;

class procedure TDataDefinition.SetMasters(const AData:TDataItem;
                               const AStore:String;
                               const AStrings:TStrings);
var tmp : TDataRelations;
begin
  tmp:=LinksFrom(AStrings);
  try
    SetMasters(AData,AStore,tmp);
  finally
    tmp.Free;
  end;
end;

procedure TDataDefinition.SetParallel(const AValue: Boolean);
begin
  Value['Parallel']:=BoolToStr(AValue,True);
end;

procedure TDataDefinition.SetStrings(const Value: TStrings);
begin
  if Value=nil then
     FreeAndNil(FStrings)
  else
     Strings.Assign(Value);
end;

class function TDataDefinition.LinksFrom(const AStrings:TStrings):TDataRelations;
var S : String;
    i : Integer;
begin
  result:=TDataRelations.Create(nil,TDataRelation);

  for S in AStrings do
  if Copy(Trim(S),1,2)<>'//' then
  begin
    i:=Pos('=',S);

    if i>0 then
       result.Add(Trim(Copy(S,1,i-1)),Trim(Copy(S,i+1,Length(S))));
  end;
end;

procedure TDataDefinition.TryLoadDetails(const AStore:String);
var tmp : TStrings;
    tmpFile : String;
    tmpLinks : TDataRelations;
begin
  Links.Clear;

  tmpFile:=TStore.FullPath(AStore,Description)+'.detail';

  if TFile.Exists(tmpFile) then
  begin
    tmp:=TStringList.Create;
    try
      tmp.LoadFromFile(tmpFile);

      tmpLinks:=LinksFrom(tmp);
      try
        Links.Assign(tmpLinks);
      finally
        tmpLinks.Free;
      end;
    finally
      tmp.Free;
    end;
  end;
end;

procedure TDataDefinition.TryLoadDetailRelations(const AData:TDataArray; const AStore:String);
var tmpData : TDataItem;
begin
  TryLoadDetails(AStore);

  if Links.Count>0 then
  begin
    tmpData:=TDataItem.Create(AData);
    try
      TDataDefinition.SetMasters(tmpData,AStore,Links);
    finally
      // Clear Parent and prevent destroying AData items
      tmpData.Items.Clear;
      tmpData.Free;
    end;
  end;
end;

procedure TDataDefinition.PrecalculateStats(const AData:TDataArray);
var tmp : TDataItem;
begin
  for tmp in AData do
      tmp.ReCalculate(Parallel);
end;

function TDataDefinition.Import(const AStore:String):TDataArray;
var Importer : TDataImporter;
    tmp : String;
begin
  Importer:=TDataImporter.Guess(Kind,AStore,Self);

  if Importer=nil then
  begin
    result:=nil;

    tmp:=Format(BIMsg_ImporterMissing,[AStore]);

    if (not Assigned(FOnError)) or (not FOnError(Self,tmp)) then
       raise EBIException.Create(tmp);
  end
  else
  try
    result:=Importer.Import;

    TryLoadDetailRelations(result,AStore);

    if CalcStats then
       PrecalculateStats(result);

  finally
    Importer.Free;
  end;
end;

procedure TDataDefinition.ReadData(Stream: TStream);
begin
  FData.Free;
  FData:=TDataItemPersistence.Load(Stream);

  if FData<>nil then
  begin
    // Set Provider = Self to enable linking
    FData.Provider:=Self;

    // Avoid destroying Self when Data is destroyed
    TDataItemAccess(FData).KeepProvider:=True;
  end;
end;

procedure TDataDefinition.WriteData(Stream: TStream);
begin
  TDataItemPersistence.Save(FData,Stream);
end;

procedure TDataDefinition.DefineProperties(Filer: TFiler);

  function HasManualData:Boolean;
  begin
    result:=(Kind=TDataDefinitionKind.Manual) and (FData<>nil) {and Ancestor.IData<>IData};
  end;

begin
  inherited;
  Filer.DefineBinaryProperty('Data',ReadData,WriteData,HasManualData);
end;

function TDataDefinition.Description: String;
begin
  if FFileName='' then
     result:=Name
  else
     result:=TPath.GetFileNameWithoutExtension(FFileName);
end;

function TDataDefinition.NextRefresh: TDateTime;
var tmp : String;
begin
  tmp:=LastImport;

  if (tmp<>'') and TFile.Exists(tmp) then
     result:=Refresh.Increment(TFile.GetLastWriteTime(tmp))
  else
     result:=Now;
end;

function TDataDefinition.LastImport: String;
begin
  result:=Name;

  if result<>'' then
     result:=TStore.FullPath(Store,result+TPersistence.Extension);
end;

procedure TDataDefinition.GetSettings;

  procedure GetRefreshSettings;
  var tmp : Integer;
  begin
    Refresh.Enabled:=AsBoolean('RefreshEnabled',False);
    Refresh.Period:=StrToIntDef(Value['RefreshPeriod'],1);

    tmp:=StrToIntDef(Value['RefreshUnit'],1);

    if tmp=-1 then
       Refresh.Units:=TRefreshUnit.Hours
    else
       Refresh.Units:=TRefreshUnit(tmp);
  end;

begin
  GetRefreshSettings;
  GetKindSettings(Kind);
end;

{.$DEFINE ALTER}

// Adds AItems to AData trying to avoid children level
class procedure TDataDefinition.Merge(const AData: TDataItem; const AItems:TDataArray);

  {$IFDEF ALTER}
  procedure AddTable(const ASource:TDataItem);
  var t : Integer;
      tmpItems : TDataArray;
  begin
    tmpItems:=ASource.Items.AsArray;

    ASource.Items.Clear;

    AData.AsTable:=True;

    for t:=0 to High(tmpItems) do
        AData.Items.Add(tmpItems[t]);

    TDataAccess(AData).FCount:=ASource.Count;
  end;

  {$ELSE}

  procedure AddItems(const ASource:TDataArray);
  var tmpCopy : TDataArray;
      t : Integer;
  begin
    // Clone a copy of ASource array, because when we do AData.Items.Add,
    // some elements in ASource array might "dissapear" due to parenting change.
    tmpCopy:=ASource.Copy;

    // Use the cloned copy instead of ASource
    for t:=0 to High(tmpCopy) do
        AData.Items.Add(tmpCopy[t]);
  end;
  {$ENDIF}

  procedure AddItem(const ASource:TDataItem);
  begin
    TDataAccess(AData).FKind:=ASource.Kind;

    // Should clone or simply assign data?
    // TDataAccess(AData).CloneData(ASource,0,ASource.Count);

    TDataAccess(AData).FCount:=ASource.Count;

    AData.Missing.Items:=ASource.Missing.Items;
    AData.Missing.Count:=ASource.Missing.Count;

    case ASource.Kind of
        dkInt32: AData.Int32Data:=ASource.Int32Data;
        dkInt64: AData.Int64Data:=ASource.Int64Data;
       dkSingle: AData.SingleData:=ASource.SingleData;
       dkDouble: AData.DoubleData:=ASource.DoubleData;
     dkExtended: AData.ExtendedData:=ASource.ExtendedData;
         dkText: AData.TextData:=ASource.TextData;
     dkDateTime: AData.DateTimeData:=ASource.DateTimeData;
      dkBoolean: AData.BooleanData:=ASource.BooleanData;
    end;
  end;

var tmp : TDataItem;
begin
  {$IFDEF ALTER}

  // Alternative way:
  for tmp in AItems do
      if tmp.AsTable then
      try
        AddTable(tmp);
      finally
        tmp.Free;
      end
      else
      if tmp.Kind=TDataKind.dkUnknown then
         AData.Items.Add(tmp)
      else
      try
        AddItem(tmp);
      finally
        tmp.Free;
      end;
  {$ELSE}

  if (not AData.AsTable) and (High(AItems)=0) then
  begin
    tmp:=AItems[0];

    if tmp.Kind<>TDataKind.dkUnknown then
    begin
      AData.Name:=tmp.Name;
      TDataAccess(AData).FKind:=tmp.Kind;

      AddItem(tmp);
    end
    else
    begin
      TDataAccess(AData).FKind:=TDataKind.dkUnknown;
      AData.AsTable:=tmp.AsTable;
      AData.Name:=tmp.Name;

      TDataAccess(AData).FCount:=tmp.Count;

      AddItems(tmp.Items.AsArray);

      // Direct set Parent=nil to avoid destroy
      while tmp.Items.Count>0 do
            tmp.Items.AsArray.Delete(0);
    end
  end
  else
    AddItems(AItems);
  {$ENDIF}
end;

procedure TDataDefinition.Load(const AData: TDataItem; const Children: Boolean);

  procedure TryLoadFromFile(const AName:String);
  var tmp : String;
      tmpData : TDataItem;
  begin
    // Future:
    // tmpData:=TStore.Load(Store,AName) // <--- using TBIFileImporter class

    tmp:=TStore.FullPath(Store,AName)+TPersistence.Extension;

    if TFile.Exists(tmp) then
    begin
      tmpData:=TDataItemPersistence.Load(tmp);
      try
        AData.Clear;
        TDataDefinition.Merge(AData,tmpData);
      finally
        // Prevent new data clear at tmp.Free
        TDataItemAccess(tmpData).FKind:=TDataKind.dkUnknown;
        tmpData.Free;
      end;
    end;
  end;

  procedure TryLoadManualFile;
  begin
    if FileName<>'' then
    begin
      AData.Name:=Description;
      TryLoadFromFile(AData.Name);

      TDataItemAccess(AData).FConsumers.Add(NotifyManual);
    end;
  end;

begin
  if (not ILoading) and
     (Kind<>TDataDefinitionKind.Unknown) then
  begin
    ILoading:=True;
    try
      if Kind=TDataDefinitionKind.Manual then
         TryLoadManualFile
      else
      begin
        AData.Clear;
        TDataDefinition.Merge(AData,Import(Store));
      end;

      TDataAccess(AData).ClearDelay;
    finally
      ILoading:=False;
    end;
  end;
end;

procedure TDataDefinition.Loaded;
begin
  inherited;
  TryLoadFile;
end;

procedure TDataDefinition.LoadFromFile(const AFileName: String);
begin
  Strings.LoadFromFile(AFileName);
  FFileName:=AFileName;

  GetSettings;
end;

procedure TDataDefinition.LoadFromText(const AText: String);
begin
  Strings.Text:=AText;
  FFileName:='';

  GetSettings;
end;

class procedure TDataDefinition.Merge(const AData, ASource: TDataItem);
begin
  AData.AsTable:=ASource.AsTable;
  TDataAccess(AData).FCount:=ASource.Count;

  Merge(AData,ASource.Items.AsArray);
end;

function TDataDefinition.MultiLineText(const ATag: String): String;
begin
  result:=Value[ATag];
  result:=StringReplace(result,'\CRLF',sLineBreak,[rfReplaceAll]);
end;

procedure TDataDefinition.Save;
begin
  if Strings<>nil then
  begin
    if (FFileName<>'') and (not (csLoading in ComponentState)) then
       Strings.SaveToFile(FFileName);
  end;
  // else raise ?
end;

procedure TDataDefinition.SaveLinks;
var tmpFile : String;
    tmp : TStrings;
    t : Integer;
begin
  tmp:=TStringList.Create;
  try
    for t:=0 to Links.Count-1 do
        tmp.Add(Links[t].Master+'='+Links[t].Detail);

    tmpFile:=TStore.FullPath(Store,Description)+'.detail';

    tmp.SaveToFile(tmpFile);
  finally
    tmp.Free;
  end;
end;

procedure TDataDefinition.SetValue(const Index: String; const Value: String);
begin
  if Strings.Values[Index]<>Value then
  begin
    Strings.Values[Index]:=Value;

    if not Volatile then
       Save;

    // BAD (too many refresh): Changed;
  end;
end;

{ TDataImporter }

Constructor TDataImporter.CreateDefinition(const AStore: String; const ADefinition:TDataDefinition);
begin
  Create(nil);
  Definition:=ADefinition;
end;

procedure TDataImporter.DoProgress(const Percent: Single; var Cancel:Boolean);
begin
  if Definition<>nil then
     if Assigned(Definition.OnImporting) then
        Definition.OnImporting(Self,Percent,Cancel);
end;

class function TDataImporter.Find(const AClass: TDataImporterClass): Integer;
var t : Integer;
begin
  for t:=0 to High(Importers) do
      if Importers[t]=AClass then
         Exit(t);

  result:=-1;
end;

class function TDataImporter.Guess(const Kind:TDataDefinitionKind;
                                   const Store: String;
                                   const ADefinition:TDataDefinition): TDataImporter;
var t : Integer;
begin
  for t:=0 to High(Importers) do
      if Importers[t].Supports(Kind,ADefinition) then
         Exit(Importers[t].CreateDefinition(Store,ADefinition));

  result:=nil;
end;

class procedure TDataImporter.RegisterClass(const AClass: TDataImporterClass);
var L : Integer;
begin
  if Find(AClass)=-1 then
  begin
    L:=Length(Importers);
    SetLength(Importers,L+1);
    Importers[L]:=AClass;
  end;
end;

class procedure TDataImporter.UnRegisterClass(const AClass: TDataImporterClass);
var t,L,
    tmp : Integer;
begin
  tmp:=Find(AClass);

  if tmp<>-1 then
  begin
    L:=Length(Importers);

    for t:=tmp to L-2 do
        Importers[t]:=Importers[t+1];

    SetLength(Importers,L-1);
  end;
end;

function TDelayHandlerFolder.GetStream(const AData: TDataArray): TStream;
begin
//  TDataPersistence.Load(TPath.Combine(IFolder,AData.Name+TDataPersistence.Extension),AData);
  result:=nil;
end;

function TDelayHandlerFolder.GetStream(const AData, ANext: TDataItem): TStream;
begin
  result:=TPersistence.OpenFile(TPath.Combine(IFolder,AData.Name+TDataPersistence.Extension));
end;

procedure TDelayHandlerFolder.Load(const AData: TDataItem; const Children:Boolean);
begin
  TDataPersistence.Load(GetStream(AData,nil),AData);

  // Children?
end;

{ TStores }

class procedure TStores.AllTo(const Items: TStrings);
var S : String;
    tmp : TTextArray;
begin
  // Sort all stores (this is done here due to VCL invisible TComboBox.Sorted)
  tmp:=nil;

  for s in TStores.All do
      if s<>'' then
         tmp.Append(s);

  tmp.Sort(True);

  // Add sorted array
  Items.BeginUpdate;
  try
    Items.Clear;

    for s in tmp do
        Items.Add(s);
  finally
    Items.EndUpdate;
  end;
end;

class procedure TStores.CheckEmptyName(const AName: String);
begin
  if Trim(AName)='' then
     raise EBIException.Create(BIMsg_Store_EmptyName);
end;

class function TStores.Count: Integer;
begin
  result:=TBIRegistry.ReadInteger('Store','Count',0);
end;

class function TStores.Exists(const AName: String): Boolean;
begin
  result:=IndexOf(AName)>-1;
end;

class procedure TStores.ChangeName(const AOld, ANew: String);
var tmp : Integer;
    tmpOrig : String;
begin
  if Pos(':',ANew)>0 then
     raise EBIException.CreateFmt(BIMsg_Store_WrongName,[ANew]);

  CheckEmptyName(ANew);

  tmp:=IndexOf(AOld);

  if tmp=-1 then
     raise EBIException.CreateFmt(BIMsg_Store_NotRegistered,[AOld])
  else
  begin
    tmpOrig:=TStore.PathOf(AOld);
    Save(tmp,ANew,tmpOrig);

    if SameText(TStore.DefaultName,AOld) then
       TStore.DefaultName:=ANew;
  end;
end;

class procedure TStores.ChangePath(const AName, AOrigin: String);
var tmp : Integer;
begin
  CheckEmptyName(AName);

  tmp:=IndexOf(AName);

  if tmp=-1 then
     raise EBIException.CreateFmt(BIMsg_Store_NotRegistered,[AName])
  else
     Save(tmp,AName,AOrigin);
end;

class function TStores.GlobalCache: TDataItem;
begin
  result:=TEnvironment.Data;
end;

class function TStores.IndexOf(const AName: String): Integer;
var t : Integer;
begin
  for t:=0 to Count-1 do
      if SameText(ReadPart(t,0),AName) then
         Exit(t);

  result:=-1;
end;

class function TStores.NewName: String;
var t : Integer;
begin
  t:=0;

  repeat
    Inc(t);
    result:='Store'+IntToStr(t);

  until IndexOf(result)=-1;
end;

class procedure TStores.Add(const AName,AOrigin: String);
var L : Integer;
begin
  if IndexOf(AName)<>-1 then
     raise EBIException.CreateFmt(BIMsg_Store_AlreadyExists,[AName])
  else
  begin
    L:=Count;

    TBIRegistry.WriteInteger('Store','Count',L+1);
    Save(L,AName,AOrigin);
  end;
end;

class function TStores.All(const APart:Integer=0):TStringArray;
var t : Integer;
begin
  t:=Count;

  if t>0 then
  begin
    SetLength(result,t);

    for t:=0 to High(result) do
        result[t]:=ReadPart(t,APart);
  end
  else
    result:=nil;
end;

class procedure TStores.Remove(const AName: String);
var S : TStringArray;
    tmp,
    t : Integer;
begin
  tmp:=TStores.IndexOf(AName);

  if tmp<>-1 then
  begin
    // Read all stores, including the Store path/origin:
    S:=All(-1);

    for t:=tmp to High(S)-1 do
        S[t]:=S[t+1];

    SetLength(S,High(S));
    Save(S);

    if S=nil then
       TStore.DefaultName:='';
  end;
end;

class procedure TStores.Save(const Index: Integer; const AName,
  AOrigin: String);
begin
  TBIRegistry.WriteString('Store','Item'+IntToStr(Index),AName+':'+Trim(AOrigin));
end;

class procedure TStores.Save(const S:TStringArray);
var t : Integer;
begin
  TBIRegistry.WriteInteger('Store','Count',Length(S));

  for t:=0 to High(S) do
      TBIRegistry.WriteString('Store','Item'+IntToStr(t),S[t]);

  // Remove potentially existing remaining/old:
  for t:=S.Count to 100 do
      TBIRegistry.RemoveItem('Store','Item'+IntToStr(t));
end;

class function TStores.ReadPart(const Index,Part:Integer):String;
var tmp : String;
    i : Integer;
begin
  tmp:=Trim(TBIRegistry.ReadString('Store','Item'+IntToStr(Index)));

  if Part=-1 then
     result:=tmp
  else
  begin
    i:=Pos(':',tmp);

    if i>0 then
       if Part=0 then
          result:=Copy(tmp,1,i-1)
       else
          result:=Copy(tmp,i+1,Length(tmp))
    else
    if Part=0 then
       result:=tmp
    else
       result:='';
  end;
end;

{ EBILoadVersionException }

Constructor EBILoadVersionException.CreateVersion(const AVersion: Integer);
begin
  inherited CreateFmt(BIMsg_Persist_IncompatibleVersion,[AVersion]);
  WrongVersion:=AVersion;
end;

{ TBaseDataImporter }

Destructor TBaseDataImporter.Destroy;
begin
  if FData<>nil then
  begin
    TDataAccess(FData).FConsumers.Remove(Notify);
    FData.Free;
    FData:=nil;
  end;

  inherited;
end;

procedure TBaseDataImporter.Changed;
begin
  TryUnloadData;
  inherited;
end;

function TBaseDataImporter.GetData: TDataItem;
begin
  if FData=nil then
  begin
    FData:=NewData;
    FData.Name:=Name;
    TDataAccess(FData).FConsumers.Add(Notify);
  end;

  result:=FData;
end;

procedure TBaseDataImporter.Notify(const AEvent: TBIEvent);
begin
  if AEvent=TBIEvent.Changed then
     TryUnloadData
  else
  if AEvent=TBIEvent.Destroyed then
     FData:=nil;
end;

procedure TBaseDataImporter.TryUnloadData;
begin
  if (not IKeepData) and (FData<>nil) then
     FData.UnLoadData(True);
end;

{ TRefreshSettings }

function TRefreshSettings.Increment(const ADate: TDateTime): TDateTime;
begin
  result:=ADate;

  case Units of
    Seconds: result:=IncSecond(result,Period);
    Minutes: result:=IncMinute(result,Period);
      Hours: result:=IncHour(result,Period);
       Days: result:=IncDay(result,Period);
      Weeks: result:=IncWeek(result,Period);
     Months: result:=IncMonth(result,Period);
      Years: result:=IncYear(result,Period);
  end;
end;

{ TDatabaseDefinition }

function TDatabaseDefinition.GetSystem: Boolean;
begin
  result:=IDefinition.AsBoolean('DBSystem',False);
end;

function TDatabaseDefinition.GetViews: Boolean;
begin
  result:=IDefinition.AsBoolean('DBViews',False);
end;

procedure TDatabaseDefinition.SetSystem(const Value: Boolean);
begin
  IDefinition['DBSystem']:=BoolToStr(Value,True);
end;

procedure TDatabaseDefinition.SetViews(const Value: Boolean);
begin
  IDefinition['DBViews']:=BoolToStr(Value,True);
end;

{ TDataRelations }

function TDataRelations.Add(const AMaster, ADetail: String): TDataRelation;
begin
  result:=TDataRelation(inherited Add);
  result.FMaster:=AMaster;
  result.FDetail:=ADetail;
end;

function TDataRelations.Get(const Index: Integer): TDataRelation;
begin
  result:=TDataRelation(inherited Items[Index]);
end;

procedure TDataRelations.Put(const Index: Integer; const Value: TDataRelation);
begin
  inherited Items[Index]:=Value;
end;

{ TDataRelation }

procedure TDataRelation.Assign(Source: TPersistent);
begin
  if Source is TDataRelation then
  begin
    FMaster:=TDataRelation(Source).FMaster;
    FDetail:=TDataRelation(Source).FDetail;
  end
  else
    inherited;
end;

initialization
  TBIRegistry.UseRegistry:=True;
  TBIRegistry.IniFile:='';

  TEnvironment.Init;
finalization
  TEnvironment.Finish;
end.
