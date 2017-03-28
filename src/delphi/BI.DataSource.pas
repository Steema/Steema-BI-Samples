{*********************************************}
{  TeeBI Software Library                     }
{  Abstract datasource and cursor classes     }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DataSource;

interface

uses
  {$IFDEF MSWINDOWS}
  {WinApi.}Windows,
  {$ENDIF}

  {System.}Classes, {System.}SysUtils, BI.Arrays, BI.Data, BI.Persist,
  BI.Compression, BI.Expression, {System.}Types, BI.Data.Expressions;

type
  // Base abstract class to define a "Source" of Data to import into a TDataItem
  TBISource=class
  private
    FIgnoreMissing : Boolean;

    FOnError : TBIError;
    FOnProgress : TBIProgress;
  protected
    IDefinition : TDataDefinition;

    function CallOnError(const Text:String):Boolean;
    procedure DoProgress(const Percent:Single; var Cancel:Boolean);

    function NameOfFile(const FileName:String):String;
  public
    Parallel : Boolean; // Use multiple-cpus to import data

    Constructor Create(const Definition:TDataDefinition=nil;
                       const MultiThread:Boolean=False); virtual;

    class function FromData(const AData:TDataArray):TDataItem; static;

    property IgnoreMissing:Boolean read FIgnoreMissing write FIgnoreMissing default False;

    property OnError:TBIError read FOnError write FOnError;
    property OnProgress:TBIProgress read FOnProgress write FOnProgress;
  end;

  // TDataCursor types

  TCursorLoop={$IFNDEF FPC}reference to{$ENDIF} procedure(const AIndex:TInteger);
  TCursorLoopObject=procedure(const AIndex:TInteger) of object;

  TCursorIndex=TNativeIntArray;

  TDataCursorItem=record // TDataCollectionItem ?
  public
    Active : Boolean;
    Data : TDataItem;
    Name : String;
    OwnsData : Boolean;
  end;

  TDataCursorItems=Array of TDataCursorItem;

  TDataCursorItemsHelper=record helper for TDataCursorItems
  public
    procedure Add(const AData:TDataItem);
    procedure Clear;
    function Count:Integer;
    procedure Delete(const AIndex:Integer);
    procedure Exchange(const A,B:Integer);
    function Find(const AData:TDataItem):Integer;
  end;

  TDataCursor=class(TDataProvider) // <-- pending: inherit from TBaseDataImporter
  private
    FCurrent : TInteger;
    FFilter : TExpression;
    FMax : TInteger;
    FStart : TInteger;
    FUseFilter : Boolean;

    function ApplyFilterTo(const AIndex:TCursorIndex):TCursorIndex;
    procedure CalcStartFinish(out AStart,AFinish:TInteger);
    function HopsFromFilter:TDataHops;
    procedure SetFilter(const Value:TExpression);
  protected
    Parent : TObject; // see TBIExport.Destroy

    ValidIndex : Boolean;

    procedure ApplyFilter;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
    procedure SetDirectFilter(const Value: TExpression);

    property Current:TInteger read FCurrent;
  public
    Data : TDataItem;
    Items : TDataCursorItems;
    Index : TCursorIndex;
    SortBy : TSortItems;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Add(const AData:TDataItem); overload;
    procedure Add(const AData:TDataItem; const AExpression:String); overload;
    procedure Add(const AData:TDataArray); overload;
    procedure Add(const AExpression:TExpression); overload;

    procedure Assign(Source:TPersistent); override;
    procedure Clear;
    function Count:TInteger;

    class function CreateFilter(const AMasters,ADetails:TDataArray;
                                const AIndex:TInteger;
                                out Expressions:TExpressions):TExpression; static;

    function DataItems:TDataArray;

    procedure Delete(const APosition:TInteger);

    function DetailIndex(const AMasters,ADetails:TDataArray; const AIndex:TInteger):TExpressions; overload;

    class function DetailIndex(const AData:TDataItem;
                               const AMasters,ADetails:TDataArray;
                               const AIndex:TInteger):TCursorIndex; overload; static;

    procedure GuessItems(const S:String);

    function IndexOf(const AData:TDataItem):Integer;

    procedure LoadData;

    {$IFNDEF FPC}
    procedure Loop(const AProc:TCursorLoop); overload;
    {$ENDIF}

    procedure Loop(const AProc:TCursorLoopObject); overload;

    class function MasterDetailIndex(const AData:TDataItem;
                                     const AMasters:TDataArray;
                                     const AIndex:TInteger):TCursorIndex;

    class function PackIndex(const AIndex:TCursorIndex):TCursorIndex; static;

    function Position(const APosition:TInteger):TInteger;
    procedure PrepareIndex(const AIndex:TCursorIndex=nil; const AllRows:Boolean=True);
    procedure SetItems(const AItems:TDataArray);

    class procedure SetMasterExpression(const Master:TDataItem;
                                        const MasterCol:TExpression;
                                        const AIndex:TInteger); static;

    function ToData:TDataItem;
  published
    property Filter:TExpression read FFilter write SetFilter;
    property Start:TInteger read FStart write FStart;
    property Max:TInteger read FMax write FMax;
    property UseFilter:Boolean read FUseFilter write FUseFilter;
  end;

  TFileFilter=record
  public
    Description,
    Extensions : String;

    function FirstExtension:String;
  end;

  TFileFilters=Array of TFileFilter;

  TFileFiltersHelper=record helper for TFileFilters
  public
    procedure Add(const ADescription,AExtensions:String);
    procedure ToDialogFilter(var AFilter:String; var All:String);
  end;

  // Export types

  TBIExport=class;

  TBIExportClass=class of TBIExport;

  TExportGetText=procedure(const Sender:TBIExport; const Data:TDataItem; const AIndex:TInteger; var Text:String) of object;

  TBIExporters=Array of TBIExportClass;

  TBIExportersHelper=record helper for TBIExporters
  protected
    class function Find(const AClass:TBIExportClass):Integer; static;
  public
    class var
      Items : TBIExporters;

    class function GuessExtension(const Extension:String):TBIExportClass; static;

    class procedure RegisterClass(const AClass:TBIExportClass); static;
    class procedure UnRegisterClass(const AClass:TBIExportClass); static;
  end;

  // Base abstract class to export a TDataItem
  TBIExport=class
  private
    FSchemaOnly : Boolean;

    function GetData: TDataItem;
    procedure SetData(const Value: TDataItem);
  protected
    FOnGetText : TExportGetText;

    Items : TDataArray;

    procedure DoEmit(const AItems: TStrings); virtual;
  public
    BinaryOnly : Boolean;
    Cursor : TDataCursor;
    Totals : TDataItem;

    Constructor Create; virtual;
    Destructor Destroy; override;

    procedure EmitTo(const AItems: TStrings);

    function AsString:String; overload;
    class function AsString(const AData:TDataItem):String; overload;

    class function FileFilter:TFileFilters; virtual;

    procedure SaveToFile(const AFileName:String); overload; virtual;

    class procedure SaveToFile(const AData:TDataItem; const AFileName:String); overload;

    procedure SaveToStream(const AStream:TStream); // virtual

    class function Supports(const Extension:String):Boolean; virtual; abstract;

    function ToStrings:TStrings; overload;
    class function ToStrings(const AData:TDataItem):TStrings; overload;

    property Data:TDataItem read GetData write SetData;
    property SchemaOnly:Boolean read FSchemaOnly write FSchemaOnly default False;

    property OnGetText:TExportGetText read FOnGetText write FOnGetText;
  end;

  TBITextExport=class(TBIExport)
  public
    FloatFormat : String;

    function DataToString(const AData:TDataItem; const AIndex:TInteger):String;
  end;

  TBIItemsSource=class(TBISource)
  private
    FExclude,
    FInclude : String;

    function GetInclude:String;
    function GetExclude:String;
    procedure SetExclude(const Value:String);
    procedure SetInclude(const Value:String);
  public
    property ExcludePattern:String read GetExclude write SetExclude;
    property IncludePattern:String read GetInclude write SetInclude;
  end;

  TBIFileSourceClass=class of TBIFileSource;

  // Base abstract class to import Data from Files
  TBIFileSource=class(TBIItemsSource)
  protected
    IFiles : TStringDynArray;
    IResult : Array of TDataArray;

    class procedure AppendData(var AResult:TDataArray; const AItems:TDataArray); static;
    class function DataArrayFrom(const AData:TDataItem):TDataArray; static;
    function DoImportFile(const FileName:String):TDataArray; virtual;
    function DoImportStream(const AExtension:String; const AStream:TStream):TDataArray; virtual;
    function FindImporterClass(const AFileName:String):TBIFileSourceClass;
    function ImportOneFile(const AFile:String):TDataArray;
    function ImportZip(const FileName:String):TDataArray;
    procedure TaskImport(Sender:TObject);
  public
    class function DataFromFiles(const AFiles:TStringDynArray; const Local:Boolean): TDataItem; static;

    class function FileFilter:TFileFilters; virtual;

    class function FromFile(const AFileName:String):TDataItem; overload;
    class function FromStrings(const AStrings:TStrings):TDataItem;
    class function FromText(const AText:String):TDataItem;

    class function GetFileSize(const FileName:String):Int64; static;
    class function GuessFromContent(const S:TStrings):TBIFileSourceClass; static;

    class function IncludedFiles(const AStore:String; const ADef:TDataDefinition):TStringDynArray; static;

    function Import(const Folder,IncludePattern,ExcludePattern:String; Recursive:Boolean):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; overload; virtual;

    function ImportFile(const FileName:String):TDataArray; overload; virtual;
    function ImportStream(const AFileName: String; const AStream:TStream): TDataArray;

    class function Supports(const Extension:String):Boolean; virtual;
  end;

  TFileShareMode=(Exclusive, DenyWrite, DenyRead, DenyNone);

  // Base abstract class to import Data from Text files
  TBITextSource=class(TBIFileSource)
  protected
    ISettings : TFormatSettings;

    FText : String;

    QuotedBooleans,
    DecimalDetected : Boolean; // <-- Used to detect "," or "." as decimal format

    // Internal use, to detect "Zero Based Strings"
    class var ZBS : Boolean; // Zero-based strings

    // 0 or 1, indicates the index of the first char of strings
    // Windows = 1,  Mobile = 0
    class var FirstStringChar : Integer;

    class procedure ChangeBoolToInt(const Col:TDataItem; const ARow:TInteger);
    class procedure ChangeToBoolean(const Col:TDataItem; const AValue:Boolean);
    class procedure ChangeToText(const Col:TDataItem; const Index,Total:TInteger); static;
    function DoImportFile(const FileName:String):TDataArray; override;
    function GuessKind(const Value:String):TDataKind;
    function InternalImportFile(const FileName:String):TDataArray; virtual;
    procedure SetColumn(const Col:TDataItem; const ARow:TInteger; const Value:String);
    function TryDecimal(const AText:String; out AValue:Single):Boolean;

  public
    MissingValue : String;      // <-- For example: "NA" in some CSV files (R Datasets)
    ZeroOneAsBoolean : Boolean; // <-- Experimental. Treat 0 and 1 as True/False

    Constructor Create(const Definition:TDataDefinition=nil;
                       const MultiThread:Boolean=False); override;

    class function LoadFromFile(const FileName:String):String; overload;
    class function LoadFromFile(const FileName:String; const ShareMode:TFileShareMode):String; overload;

    class function StringsFrom(const FileName:String):TStrings;

    property Text:String read FText;
  end;

  // Base abstract class to import Data from hierarchical-capable sources,
  // like for example JSON or XML.  (That provide tree-like data structures)
  TBIHierarchicalSource=class(TBITextSource)
  public
    Hierarchical : Boolean; // <-- Default is False (try to return a flat table)

    Constructor Create(const Definition:TDataDefinition=nil;
                       const MultiThread:Boolean=False); override;
  end;

  TBIFileImporters=Array of TBIFileSourceClass;

  TBIFileImportersHelper=record helper for TBIFileImporters
  protected
    class function Find(const AClass:TBIFileSourceClass):Integer; static;
  public
    class var
      Items : TBIFileImporters;

    class function GuessExtension(const Extension:String):TBIFileSourceClass; static;

    class procedure RegisterClass(const AClass:TBIFileSourceClass); static;
    class procedure UnRegisterClass(const AClass:TBIFileSourceClass); static;
  end;

  // Perform "select ...." queries
  TDataSelect=class(TDataCursor)
  private
    FDistinct : Boolean;

    procedure AddItem(const AResult,AItem:TDataItem; const AName:String);
    procedure AddItems(const AData: TDataItem);
    function FoundLast(const AResult:TDataItem; const ACount:TInteger):Boolean;
    procedure GuessMainData;
  protected
    procedure GetItems(const AData:TDataItem); override;
    procedure Load(const AData:TDataItem; const Children:Boolean); override;
  public
    procedure Assign(Source:TPersistent); override;
    function Calculate:TDataItem; overload;
    procedure Calculate(const AData:TDataItem); overload;
    function MainData:TDataItem;
    class function SetupHops(const Hops:TDataHops; const AItems:TDataArray):TInt32Array;
    function ToString:String; override;

    property Distinct:Boolean read FDistinct write FDistinct default False;
  end;

  // Simple method to Clone a TDataItem, using a temporary memory stream or
  // a loop on all ASource data.
  TDataClone=record // class(TBaseDataImporter) ?
  public
    class function Clone(const AData:TDataItem):TDataItem; overload; static;
    class function Clone(const AData:TDataItem; const AIndex:TNativeIntArray):TDataItem; overload; static;

    class procedure Clone(const ASource,ADest:TDataItem; const AItems:TDataArray=nil); overload; static;

    class procedure CloneStructure(const ADest,AData:TDataItem;
                                   const AItems:TDataArray=nil); overload; static;

    class function CloneStructure(const AData:TDataItem;
                                  const AItems:TDataArray=nil):TDataItem; overload; static;

    class procedure CopyData(const ASource,ADest:TDataItem;
                             const AIndex,ADestPos:TInteger;
                             const AItems:TDataArray=nil); overload; static;

    class procedure CopyData(const ASource,ADest:TDataArray;
                             const AIndex,ADestPos:TInteger); overload; static;
  end;

implementation
