{*********************************************}
{  TeeBI Software Library                     }
{  Abstract datasource and cursor classes     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DataSource;

interface

uses
  {$IFDEF MSWINDOWS}
  WinApi.Windows,
  {$ENDIF}

  System.Classes, System.SysUtils, BI.Arrays, BI.Data, BI.Persist,

  {$IFNDEF FPC}
  System.Zip,
  {$ENDIF}

  BI.Expression, System.Types, BI.Data.Expressions;

type
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
    Parallel : Boolean;

    Constructor Create(const Definition:TDataDefinition=nil;
                       const MultiThread:Boolean=False); virtual;

    class function FromData(const AData:TDataArray):TDataItem; static;

    property IgnoreMissing:Boolean read FIgnoreMissing write FIgnoreMissing default False;

    property OnError:TBIError read FOnError write FOnError;
    property OnProgress:TBIProgress read FOnProgress write FOnProgress;
  end;

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
    function CreateFilter(const AMaster,ADetail:TDataItem; out MasterCol:TExpression):TLogicalExpression;

    function DataItems:TDataArray;

    procedure Delete(const APosition:TInteger);

    class function DetailIndex(const Cols,AMaster,ADetail:TDataItem; const AIndex:TInteger):TCursorIndex; static;

    procedure GuessItems(const S:String);

    function IndexOf(const AData:TDataItem):Integer;

    procedure LoadData;

    {$IFNDEF FPC}
    procedure Loop(const AProc:TCursorLoop); overload;
    {$ENDIF}

    procedure Loop(const AProc:TCursorLoopObject); overload;

    class function MasterDetailIndex(const Cols:TDataItem; const AMaster:TDataItem; const AIndex:TInteger):TCursorIndex;

    class function PackIndex(const AIndex:TCursorIndex):TCursorIndex; static;

    function Position(const APosition:TInteger):TInteger;
    procedure PrepareIndex(const AIndex:TCursorIndex=nil; const AllRows:Boolean=True);
    procedure SetItems(const AItems:TDataArray);
    procedure SetMasterExpression(const Master:TDataItem; const MasterCol:TExpression; const AIndex:TInteger);

  published
    property Filter:TExpression read FFilter write SetFilter;
    property Start:TInteger read FStart write FStart;
    property Max:TInteger read FMax write FMax;
    property UseFilter:Boolean read FUseFilter write FUseFilter;
  end;

  TBIExport=class;

  TBIExportClass=class of TBIExport;

  TExportGetText=procedure(const Sender:TBIExport; const Data:TDataItem; const AIndex:TInteger; var Text:String) of object;

  TBIExport=class
  private
    FSchemaOnly : Boolean;

    function GetData: TDataItem;
    procedure SetData(const Value: TDataItem);
  protected
    FOnGetText : TExportGetText;

    Items : TDataArray;

    procedure DoEmit(const AItems: TStrings); virtual; abstract;
  public
    Cursor : TDataCursor;
    Totals : TDataItem;

    Constructor Create; virtual;
    Destructor Destroy; override;

    procedure EmitTo(const AItems: TStrings);

    function AsString:String; overload;
    class function AsString(const AData:TDataItem):String; overload;

    procedure SaveToFile(const AFileName:String); overload;

    class procedure SaveToFile(const AData:TDataItem; const AFileName:String); overload;

    procedure SaveToStream(const AStream:TStream);

    function ToStrings:TStrings; overload;
    class function ToStrings(const AData:TDataItem):TStrings; overload;

    property Data:TDataItem read GetData write SetData;
    property SchemaOnly:Boolean read FSchemaOnly write FSchemaOnly default False;

    property OnGetText:TExportGetText read FOnGetText write FOnGetText;
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

{$IFNDEF FPC}
{$IF CompilerVersion>=31}
{$DEFINE ENCRYPTEDZIP}
{$ENDIF}
{$ENDIF}

  TBIFileSource=class(TBIItemsSource)

  {$IFDEF ENCRYPTEDZIP}
  private
    function DecryptZipStream(const InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader; IsEncrypted: Boolean): TStream;
  {$ENDIF}

  protected
    IFiles : TStringDynArray;
    IResult : Array of TDataArray;

    class procedure AppendData(var AResult:TDataArray; const AItems:TDataArray); static;
    class function DataArrayFrom(const AData:TDataItem):TDataArray; static;
    function DoImportFile(const FileName:String):TDataArray; virtual;
    function DoImportStream(const AExtension:String; const AStream:TStream):TDataArray; virtual;
    function FindImporterClass(const AFileName:String):TBIFileSourceClass;
    function ImportOneFile(const AFile:String):TDataArray;
    function ImportURLFile(const FileName:String):TDataArray;
    function ImportZip(const FileName:String):TDataArray;
    procedure TaskImport(Sender:TObject);
  public
    class function DataFromFiles(const AFiles:TStringDynArray; const Local:Boolean): TDataItem; static;

    class function ExportFormat:TBIExport; virtual;

    class function FromFile(const AFileName:String):TDataItem; overload;
    class function FromStrings(const AStrings:TStrings):TDataItem;
    class function FromText(const AText:String):TDataItem;
    class function FromURL(const AURL:String):TDataItem;

    class function GetFileSize(const FileName:String):Int64; static;
    class function GuessFromContent(const S:TStrings):TBIFileSourceClass; static;

    class function IncludedFiles(const AStore:String; const ADef:TDataDefinition):TStringDynArray; static;

    class function IsURL(const FileName:String):Boolean; static;

    function Import(const Folder,IncludePattern,ExcludePattern:String; Recursive:Boolean):TDataArray; overload;
    function Import(const Strings:TStrings):TDataArray; overload; virtual; abstract;

    function ImportFile(const FileName:String):TDataArray; overload; virtual;
    function ImportStream(const AFileName: String; const AStream:TStream): TDataArray;
    function ImportURL(const URL:String):TDataArray;

    class function Supports(const Extension:String):Boolean; virtual;
  end;

  TFileShareMode=(Exclusive, DenyWrite, DenyRead, DenyNone);

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

  TBIFileImporters=class abstract
  protected
    class function Find(const AClass:TBIFileSourceClass):Integer;
  public
    class var
      Importers:Array of TBIFileSourceClass;

    class function GuessExtension(const Extension:String):TBIFileSourceClass;
    function Import:TDataItem; virtual; abstract;

    class procedure RegisterClass(const AClass:TBIFileSourceClass);
    class procedure UnRegisterClass(const AClass:TBIFileSourceClass);
  end;

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
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;
    function Calculate:TDataItem; overload;
    procedure Calculate(const AData:TDataItem); overload;
    function MainData:TDataItem;
    class function SetupHops(const Hops:TDataHops; const AItems:TDataArray):TInt32Array;
    function ToString:String; override;

    property Distinct:Boolean read FDistinct write FDistinct default False;
  end;

  // Simple method to Clone a TDataItem using a temporary memory stream
  TDataClone=record // class(TBaseDataImporter) ?
  public
    class function Clone(const AData:TDataItem):TDataItem; overload; static;
    class procedure Clone(const ASource,ADest:TDataItem); overload; static;
  end;

implementation
