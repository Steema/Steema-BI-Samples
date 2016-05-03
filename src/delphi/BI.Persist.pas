{*********************************************}
{  TeeBI Software Library                     }
{  Data Persistence classes                   }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Persist;

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
  System.Classes, System.SysUtils, System.Types, BI.Data, BI.Arrays, BI.Streams;

type
  TBIError=function(const Sender:TObject; const Error:String):Boolean of object;
  TBIErrorProc={$IFNDEF FPC}reference to{$ENDIF} function(const Sender:TObject; const Error:String):Boolean;

  TBIProgress=procedure(const Sender:TObject; const Percent:Single; var Cancel:Boolean) of object;

  TRefreshUnit=(Seconds,Minutes,Hours,Days,Weeks,Months,Years);

  // Abstract class with all settings necessary to import a given data
  TDataDefinitionKind=(Files,Database,Web,Unknown);

  TDataDefinition=class(TDataProvider)
  private
    FFileName : String;

    FOnError : TBIError;
    FOnImporting : TBIProgress;

    function GetKind: TDataDefinitionKind;
    procedure GetRefreshSettings;
    function GetValue(const Index: String): String;
    procedure SetValue(const Index: String; const Value: String);
    procedure TryLoadDetailRelations(const AData:TDataArray);
  protected
    Strings : TStrings;

    RefreshPeriod : Integer;
    RefreshUnit : TRefreshUnit;

    Volatile : Boolean; // When True, changes to Strings aren't saved

    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure Load; overload;
    procedure LoadFromString(const AText:String);
    procedure Save;
  public
    const
      Extension='.def';

    var
      Store : String;

    Constructor FromFile(const AFileName:String);

    Destructor Destroy; override;

    function AsBoolean(const Key:String):Boolean;

    class procedure CreateFile(const FileName:String; const Kind:TDataDefinitionKind);

    class function KindToString(const AKind:TDataDefinitionKind):String;
    function Import(const AStore:String):TDataArray;

    function LastImport:String;

    procedure LoadFromFile(const FileName:String);
    procedure LoadFromText(const AText:String);

    function MultiLineText(const ATag:String):String;

    function Name: String;

    function NextRefresh:TDateTime;
    class procedure SetMasters(const AData:TDataItem; const Items:TStrings); static;

    property Kind:TDataDefinitionKind read GetKind;
    property Value[const Index:String]:String read GetValue write SetValue; default;

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
  public
    Constructor CreateDefinition(const AStore:String; const ADefinition:TDataDefinition); virtual;

    function AllData:TStringDynArray; virtual; abstract;
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
    class function AllData(const AStore:String=''):TStringDynArray; overload; static;

    class function DefinitionOf(const Name:string):String; overload; static;
    class function DefinitionOf(const AStore,Name: string): String; overload; static;

    class function NameOf(const FileName:String):String; static;

    class function FullPath(const FileName:string):String; overload; static;
    class function FullPath(const AStore,FileName: string): String; overload; static;

    class function GetDefinition(const AStore,AName:String):TDataDefinition; static;
    class function IsRemote(const AStore:String):Boolean; static;

    class function Load(const AName:String):TDataItem; overload; static;
    class function Load(const AStore,AName:String; const OnError:TBIErrorProc=nil):TDataItem; overload; static;

    class function DataToStream(const APath,AName:String; const Zip:Boolean=False):TStream; overload; static;
    class function DataToStream(const AName:String):TStream; overload; static;

    class function NotRegistered(const AName: String):EBIException;

    class function OriginOf(const AData:TDataItem; const DefaultStore:String; const RelativeTo:TDataItem=nil):String; static;
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

    class function ZipStream(const AStream: TStream): TMemoryStream; static;

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
    class function All(const APart:Integer=0):TStringDynArray; static;
    class procedure ChangeName(const AOld,ANew:String); static;
    class procedure ChangePath(const AName,AOrigin:String); static;
    class function Exists(const AName:String):Boolean; static;
    class function GlobalCache:TDataItem; static;
    class function IndexOf(const AName:String):Integer; static;
    class function NewName:String; static;
    class procedure Remove(const AName:String); static;
    class procedure Save(const S:TStringDynArray); overload; static;
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
    class function Load(const Stream:TStream):TDataItem; overload; static;
    class function Load(const FileName:String):TDataItem; overload; static;

    class procedure Save(const Data:TDataItem; const Stream:TStream); overload; static;
    class procedure Save(const Data:TDataItem; const FileName:String); overload; static;
  end;

  // Cross-platform registry / inifile
  TBIRegistry=record
  private
    {$IFNDEF MSWINDOWS}
    class function SteemaIni:String; static;
    {$ENDIF}
  public
    class function ReadBoolean(const Key,Name:String; const Default:Boolean):Boolean; static;
    class function ReadInteger(const Key,Name:String; const Default:Integer):Integer; static;
    class function ReadString(const Key, Name, Default: String): String; static;

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
