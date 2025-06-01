{*********************************************}
{  TeeBI Software Library                     }
{  Abstract datasource and cursor classes     }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DataSource;

interface

uses
  {$IFDEF MSWINDOWS}
  {WinApi.}Windows,
  {$ENDIF}

  {System.}Classes, {System.}SysUtils, BI.Arrays, BI.DataItem, BI.Persist,
  BI.Compression, BI.Expression, {System.}Types, BI.Expressions;

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
    function DoImportStream(const AExtension:String; const AStream:TStream):TDataArray; override;
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
    // TopIsPercent: Boolean; // Made public as per refined plan for simplicity like Max, Start
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
  public // Added TopIsPercent here
    TopIsPercent: Boolean;
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

  // Helper methods to convert a string to a float.
  // This is an improved version of the RTL TryStrToFloat and StrToFloat functions.
  //
  // It adds support for "thousands" separator and/or currency symbols.
  //
  // Examples:
  //
  // if TStringToFloat.TryConvert('123,456.78') then ...
  // if TStringToFloat.TryConvert('€ 123.45') then ...
  // var tmp : Double ; tmp:=TStringToFloat.Convert('£99,123.56789');
  //
  // NOTE: This function might be SLOW, compared to plain numbers like: '123.45'
  //
  TStringToFloat=record
  public
    class function TryConvert(const Value:String; out AFloat:Single;
                                    const Settings:TFormatSettings):Boolean; overload; static;

    class function TryConvert(const Value:String; out AFloat:Double;
                                    const Settings:TFormatSettings):Boolean; overload; static;

    class function TryConvert(const Value:String; out AFloat:Extended;
                                    const Settings:TFormatSettings):Boolean; overload; static;

    class function TryConvert(const Value:String):Boolean; overload; static; inline;
  end;

implementation

{$IFNDEF FPC}
{$IF COMPILERVERSION>=29} // XE8
{$DEFINE HASTHREADING}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF HASTHREADING}
  System.Threading,
  {$ENDIF}

  {$IFDEF POSIX}
  // Skip inline hint
  Posix.Unistd,
  {$ENDIF}

  {System.}Masks,

  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.IOUtils,
  System.Diagnostics,
  {$ENDIF}

  BI.Arrays.Strings,
  BI.Languages.English, BI.UI, BI.SQL;

{ TBISource }

Constructor TBISource.Create(const Definition:TDataDefinition;
                             const MultiThread: Boolean);
begin
  inherited Create;

  IDefinition:=Definition;
  Parallel:=MultiThread;
end;

function TBISource.CallOnError(const Text:String):Boolean;
begin
  result:=Assigned(FOnError) and FOnError(Self,Text);
end;

function TBISource.NameOfFile(const FileName: String): String;
begin
  result:=TPath.GetFileNameWithoutExtension(FileName);
end;

procedure TBISource.DoProgress(const Percent: Single; var Cancel:Boolean);
begin
  if Assigned(FOnProgress) then
     FOnProgress(Self,Percent,Cancel);
end;

class function TBISource.FromData(const AData:TDataArray):TDataItem;
begin
  if AData=nil then
     result:=nil
  else
  if AData.Count=1 then
     result:=AData[0]
  else
     result:=TDataItem.Create(AData);
end;

function GetFilesFrom(const Folder,IncludePattern,ExcludePattern:String; Recursive:Boolean):TStringDynArray;
var
  tmpInclude,
  tmpExclude : TMask;

  // From IOUtils (private)
  procedure Find(var AResult:TStringDynArray; const Folder:String);
  const
    Current='.';
    Parent='..';

  var L : Integer;
      SearchRec : TSearchRec;
  begin
    if FindFirst(TPath.Combine(Folder, '*'), faAnyFile, SearchRec) = 0 then
    try
      repeat
         if SearchRec.Attr and {$IFNDEF FPC}System.{$ENDIF}SysUtils.faDirectory <> 0 then
         begin
           if Recursive and (SearchRec.Name <> Current) and (SearchRec.Name <> Parent) then
              Find(AResult,TPath.Combine(Folder, SearchRec.Name));
         end
         else
         if ((tmpInclude=nil) or tmpInclude.Matches(SearchRec.Name)) and
            ((tmpExclude=nil) or (not tmpExclude.Matches(SearchRec.Name))) then
         begin
           L:=Length(AResult);
           SetLength(AResult,L+1);
           AResult[L]:=TPath.Combine(Folder,SearchRec.Name);
         end;

      until FindNext(SearchRec) <> 0;
    finally
      {$IFNDEF FPC}System.{$ENDIF}SysUtils.FindClose(SearchRec);
    end;
  end;

begin
  if IncludePattern='' then
     tmpInclude:=nil
  else
     tmpInclude:=TMask.Create(IncludePattern);

  if ExcludePattern='' then
     tmpExclude:=nil
  else
     tmpExclude:=TMask.Create(ExcludePattern);

  result:=nil;
  try
    Find(result,Folder);
  finally
    tmpExclude.Free;
    tmpInclude.Free;
  end;
end;

{ TBIItemsSource }

function TBIItemsSource.GetExclude: String;
begin
  if IDefinition=nil then
     result:=FExclude
  else
     result:=Trim(IDefinition['ExcludeMask']);
end;

function TBIItemsSource.GetInclude: String;
begin
  if IDefinition=nil then
     result:=FInclude
  else
     result:=Trim(IDefinition['IncludeMask']);
end;

procedure TBIItemsSource.SetExclude(const Value: String);
begin
  if IDefinition=nil then
     FExclude:=Trim(Value)
  else
     IDefinition['ExcludeMask']:=Trim(Value);
end;

procedure TBIItemsSource.SetInclude(const Value: String);
begin
  if IDefinition=nil then
     FInclude:=Trim(Value)
  else
     IDefinition['IncludeMask']:=Trim(Value);
end;

{ TBIHierarchicalSource }

Constructor TBIHierarchicalSource.Create(const Definition: TDataDefinition; const MultiThread: Boolean);
begin
  inherited;

  if Definition<>nil then
     Hierarchical:=Definition.AsBoolean('HIERARCHICAL',False);
end;

{ TFileFiltersHelper }

procedure TFileFiltersHelper.Add(const ADescription,AExtensions: String);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);

  Self[L].Description:=ADescription;
  Self[L].Extensions:=AExtensions;
end;

procedure TFileFiltersHelper.ToDialogFilter(var AFilter, All: String);
var tmp : TFileFilter;
begin
  for tmp in Self do
  begin
    if AFilter<>'' then
       AFilter:=AFilter+'|';

    AFilter:=AFilter+tmp.Description+'|'+tmp.Extensions;

    if All<>'' then
       All:=All+';';

    All:=All+tmp.Extensions;
  end;
end;

{ TBIFileSource }

function TBIFileSource.ImportZip(const FileName:String):TDataArray;

  function Password:String;
  begin
    if IDefinition=nil then
       result:=''
    else
       result:=IDefinition['ZipPassword'];
  end;

var t : Integer;
    tmpFiles : TStringArray;
begin
  result:=nil;

  tmpFiles:=TCompression.DeCompress(FileName,Password,CallOnError);
  try
    for t:=Low(tmpFiles) to High(tmpFiles) do
        AppendData(result,ImportFile(tmpFiles[t]));
  finally
    for t:=Low(tmpFiles) to High(tmpFiles) do
        {$IFNDEF FPC}System.{$ENDIF}SysUtils.DeleteFile(tmpFiles[t]);
  end;
end;

class function TBIFileSource.DataArrayFrom(const AData:TDataItem):TDataArray;
begin
  if AData=nil then
     result:=nil
  else
  begin
    SetLength(result,1);
    result[0]:=AData;
  end;
end;

function TBIFileSource.DoImportFile(const FileName: String): TDataArray;
var tmp : String;
begin
  tmp:=TPath.GetExtension(FileName);

  if SameText(tmp,'.zip') then
     result:=ImportZip(FileName)
  else
  if SameText(tmp,TDataPersistence.Extension) then
     result:=DataArrayFrom(TDataItemPersistence.Load(FileName))
  else
     result:=nil;
end;

function TBIFileSource.DoImportStream(const AExtension:String; const AStream:TStream):TDataArray;
var tmp : String;
begin
  if SameText(AExtension,TDataPersistence.Extension) then
     result:=TBIFileSource.DataArrayFrom(TDataItemPersistence.Load(AStream))
  else
  begin
    tmp:='Cannot import stream';

    if not CallOnError(tmp) then
       raise EBIException.Create(tmp);

    result:=nil;
  end;
end;

class function TBIFileSource.FromStrings(const AStrings:TStrings):TDataItem;
var tmp : TBIFileSource;
begin
  tmp:=Self.Create;
  try
    result:=FromData(tmp.Import(AStrings));
  finally
    tmp.Free;
  end;
end;

class function TBIFileSource.FromText(const AText:String):TDataItem;
var tmp : TStrings;
begin
  tmp:=TStringList.Create;
  try
    tmp.Text:=AText;
    result:=FromStrings(tmp);
  finally
    tmp.Free;
  end;
end;

// Pending: Compare with alternative GetFileSize, TFileStream, etc
class function TBIFileSource.GetFileSize(const FileName:String):Int64;
var
  {$IFDEF MSWINDOWS}
  tmp : TWin32FileAttributeData;
  {$ELSE}
  f : TFileStream;
  {$ENDIF}
begin
  // Alternative way (Windows only):
  {$IFDEF MSWINDOWS}
  if GetFileAttributesEx(PChar(Pointer(FileName)), GetFileExInfoStandard, @tmp) then
     result:=tmp.nFileSizeLow or (Int64(tmp.nFileSizeHigh) shl 32)
  else
     result:=-1;

  {$ELSE}

  {$IFDEF FPC}
  result:=TFile.Size(FileName);
  {$ELSE}
  try
    f:=TFile.Open(FileName,TFileMode.fmOpen {$IFDEF MSWINDOWS},TFileAccess.faRead,TFileShare.fsRead{$ENDIF});
    try
      result:=f.Size;
    finally
      f.Free;
    end;
  except
    on EInOutError do
    begin
      result:=-1; // raise ?
    end;
  end;
  {$ENDIF}
  {$ENDIF}
end;

// Returns data item with all files from folder that pass the
// included/excluded filters:
class function TBIFileSource.DataFromFiles(const AFiles:TStringDynArray; const Local:Boolean): TDataItem;
var S : String;
    t : TLoopInteger;
begin
  result:=TDataItem.Create(True);

  result.Items.Add('Name',TDataKind.dkText);
  result.Items.Add('Extension',TDataKind.dkText);
  result.Items.Add('Path',TDataKind.dkText);
  result.Items.Add('Size',TDataKind.dkInt64);
  result.Items.Add('Modified',TDataKind.dkDateTime);

  result.Resize(Length(AFiles));

  for t:=0 to result.Count-1 do
  begin
    S:=AFiles[t];

    result.Items[0].TextData[t]:=TPath.GetFileName(S);
    result.Items[1].TextData[t]:=TPath.GetExtension(S);
    result.Items[2].TextData[t]:=TPath.GetDirectoryName(S);

    if Local then
    begin
      result.Items[3].Int64Data[t]:=GetFileSize(S);
      result.Items[4].DateTimeData[t]:=TFile.GetLastWriteTime(S);
    end
    else
    begin
      // Pending: FTP file size
      result.Items[3].Missing[t]:=True;
      result.Items[4].Missing[t]:=True;
    end;
  end;
end;

// Returns string array with all files from folder that pass the
// included/excluded filters:
class function TBIFileSource.IncludedFiles(const AStore:String; const ADef:TDataDefinition): TStringDynArray;
var tmpFolder : String;
begin
  tmpFolder:=ADef['Folder'];

  if TPath.IsRelativePath(tmpFolder) then
     tmpFolder:=TStore.FullPath(AStore,tmpFolder);

  result:=GetFilesFrom(tmpFolder,ADef['IncludeMask'],ADef['ExcludeMask'],ADef.AsBoolean('Recursive'));
end;

class function TBIFileSource.FromFile(const AFileName:String):TDataItem;
var tmp : TBIFileSource;
begin
  tmp:=Self.Create;
  try
    result:=FromData(tmp.ImportFile(AFileName));
  finally
    tmp.Free;
  end;
end;

procedure TBIFileSource.TaskImport(Sender:TObject);
var Idx : Integer;
begin
  Idx:=Integer(Sender);
  IResult[Idx]:=ImportFile(IFiles[Idx]);
end;

class procedure TBIFileSource.AppendData(var AResult:TDataArray; const AItems:TDataArray);
var L,
    t : Integer;
begin
  L:=Length(AResult);
  SetLength(AResult,L+AItems.Count);

  for t:=0 to High(AItems) do
      AResult[L+t]:=AItems[t];
end;

function TBIFileSource.ImportOneFile(const AFile:String):TDataArray;
begin
  try
    result:=ImportFile(AFile);
  except
    on E:Exception do
       if not CallOnError(E.Message+' File: '+AFile) then
          raise;
  end;
end;

function TBIFileSource.Import(const Folder,IncludePattern,ExcludePattern:String; Recursive:Boolean):TDataArray;

  {$IFDEF HASTHREADING}
  procedure DoInParallel(const N:Integer);
  var t : Integer;
      tmpTask : ITask;
  begin
    tmpTask:=TTask.Run(procedure
    begin
      t:=0;

      TParallel.&For(0,n-1,procedure(Idx:Integer)
          var Old : TBIProgress;
          begin
            Old:=FOnProgress;
            FOnProgress:=nil;
            try
              IResult[Idx]:=ImportOneFile(IFiles[Idx]);
            finally
              FOnProgress:=Old;
            end;

            AtomicIncrement(t);

            // Problem: queued calls are executed *after* tmpTask.Wait
            TThread.Queue(nil,procedure
            var tmpCancel : Boolean;
            begin
              // Safety check
              if tmpTask<>nil then
              begin
                tmpCancel:=False;
                DoProgress(t*100/n,tmpCancel);

                // if tmpCancel then <-- how to stop all TParallel tasks?
              end;
            end);
          end);
    end);

    tmpTask.Wait;
    tmpTask:=nil;
  end;
  {$ENDIF}

  procedure DestroyResults;
  var tmp : TDataArray;
  begin
    for tmp in IResult do
        tmp.FreeAll;

    IResult:=nil;
  end;

var t,
    n : Integer;
    tmpCancel : Boolean;
    Old : TBIProgress;
begin
  result:=nil;

  IFiles:=GetFilesFrom(Folder,IncludePattern,ExcludePattern,Recursive);

  tmpCancel:=False;

  DoProgress(0,tmpCancel);

  if not tmpCancel then
  begin
    n:=Length(IFiles);
    SetLength(IResult,n);

    try
      {$IFDEF HASTHREADING}
      if Parallel then
         DoInParallel(n)
      else
      {$ENDIF}
      for t:=0 to n-1 do
      begin
        Old:=FOnProgress;
        FOnProgress:=nil;
        try
          IResult[t]:=ImportOneFile(IFiles[t]);
        finally
          FOnProgress:=Old;
        end;

        if Assigned(OnProgress) then
           DoProgress(t*100/n,tmpCancel);

        if tmpCancel then
        begin
          DestroyResults;
          break;
        end;
      end;

    finally
      for t:=0 to High(IResult) do
          if IResult[t]<>nil then
             AppendData(result,IResult[t]);

      IFiles:=nil;
    end;
  end;

  DoProgress(100,tmpCancel);
end;

function TBIFileSource.Import(const Strings:TStrings):TDataArray;
var tmp : TBIFileSourceClass;
    tmpImport : TBIFileSource;
begin
  tmp:=GuessFromContent(Strings);

  if tmp=nil then
     raise EBIException.CreateFmt(BIMsg_ImporterMissing,['(text)'])
  else
  begin
    tmpImport:=tmp.Create(IDefinition,Parallel);
    try
      result:=tmpImport.Import(Strings);
    finally
      tmpImport.Free;
    end;
  end;
end;

function TBIFileSource.FindImporterClass(const AFileName:String):TBIFileSourceClass;
var tmpError : String;
begin
  result:=TBIFileImporters.GuessExtension(TPath.GetExtension(AFileName));

  if result=nil then
  begin
    tmpError:=Format(BIMsg_FileImporterMissing,[AFileName]);

    if not CallOnError(tmpError) then
       raise EBIException.Create(tmpError);
  end
end;

function TBIFileSource.ImportStream(const AFileName: String; const AStream:TStream): TDataArray;
var tmp : TBIFileSourceClass;
    tmpImport : TBIFileSource;
    tmpExtension : String;
begin
  tmpExtension:=TPath.GetExtension(AFileName);

  tmp:=FindImporterClass(tmpExtension);

  if tmp=nil then
     result:=nil
  else
  begin
    if Self.ClassType=tmp then
       result:=DoImportStream(tmpExtension,AStream)
    else
    begin
      tmpImport:=tmp.Create(IDefinition,Parallel);
      try
        tmpImport.OnProgress:=FOnProgress;
        result:=tmpImport.DoImportStream(tmpExtension,AStream);
      finally
        tmpImport.Free;
      end;
    end;
  end;
end;

function TBIFileSource.ImportFile(const FileName: String): TDataArray;
var tmp : TBIFileSourceClass;
    tmpImport : TBIFileSource;
begin
  tmp:=FindImporterClass(TPath.GetExtension(FileName));

  if tmp=nil then
     result:=nil
  else
  begin
    if Self.ClassType=tmp then
       result:=DoImportFile(FileName)
    else
    begin
      tmpImport:=tmp.Create(IDefinition,Parallel);
      try
        tmpImport.OnProgress:=FOnProgress;
        result:=tmpImport.DoImportFile(FileName);
      finally
        tmpImport.Free;
      end;
    end;
  end;
end;

class function TBIFileSource.GuessFromContent(const S:TStrings):TBIFileSourceClass;

  function SeemsTSV(const S:String):Boolean;
  begin
    result:=TStringArray.Split(S,#9).Count>1;
  end;

  function IsHtml(const S:String):Boolean;
  begin
    result:=(Pos('<!doctype html>',S)>0) or
            (Pos('<html>',S)>0) or
            (Pos('<table',S)>0);
  end;

var t,
    tmpMax : Integer;
    tmp : String;
begin
  result:=nil;

  tmpMax:=S.Count-1;

  if tmpMax>100 then
     tmpMax:=100;

  for t:=0 to tmpMax do
  begin
    tmp:=LowerCase(Trim(S[t]));

    if tmp<>'' then
    begin
      if Pos('<?xml',tmp)>0 then
         tmp:='.xml'
      else
      if (Copy(tmp,1,1)='{') or (Copy(tmp,1,1)='[') then
         tmp:='.json'
      else
      if SeemsTSV(tmp) then
         tmp:='.tsv'
      else
      if IsHtml(tmp) then
         tmp:='.htm'
      else
      //if SeemsCSV(tmp) then <-- always force delimited text
         tmp:='.csv';

      result:=TBIFileImporters.GuessExtension(tmp);
      break;
    end;
  end;
end;

class function TBIFileSource.FileFilter:TFileFilters;
begin
  result:=nil;
  result.Add('Zip files','*.zip');
end;

class function TBIFileSource.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.zip');
end;

{ TBITextSource }

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 20.0}
     {$DEFINE HAS_STRINGSTREAM}
  {$ENDIF}
{$ENDIF}

Constructor TBITextSource.Create(const Definition: TDataDefinition; const MultiThread: Boolean);
var tmp : String;
begin
  inherited;
  ISettings:=FormatSettings;

  if Definition<>nil then
  begin
    tmp:=Definition['MissingValues'];

    if tmp<>'' then
       MissingValue:=tmp;
  end;
end;

// Returns text content from FileName, with ShareMode access
class function TBITextSource.LoadFromFile(const FileName:String; const ShareMode:TFileShareMode):String;

  function CalcShareMode:Word;
  begin
    case ShareMode of
      Exclusive: result:=fmShareExclusive;
      DenyWrite: result:=fmShareDenyWrite;

      {$WARN SYMBOL_PLATFORM OFF}
       {$IFDEF MSWINDOWS}
       DenyRead: result:=fmShareDenyRead;
       {$ENDIF}
      {$WARN SYMBOL_PLATFORM ON}
    else
      result:=fmShareDenyNone;
    end;
  end;

var tmpFile : TStream;
    tmpSt: TStringStream;
begin
  tmpFile:=TFileStream.Create(FileName, fmOpenRead or CalcShareMode);
  try
    tmpSt:=TStringStream.Create('');
    try
      tmpSt.CopyFrom(tmpFile,tmpFile.Size);
      result:=tmpSt.DataString;
    finally
      tmpSt.Free;
    end;
  finally
    tmpFile.Free;
  end;
end;

// Returns text content from FileName
class function TBITextSource.LoadFromFile(const FileName:String):String;
var m : TStringStream;
    {$IFNDEF HAS_STRINGSTREAM}
    f : TFileStream;
    {$ENDIF}
begin
  // Several ways to read a text file:

  (*
  // Using a TFileStream:

  f:=OpenRead(Path);
  try
    SetLength(result, f.Size);
    f.ReadBuffer(result, Length(result));
  finally
    f.Free;
  end;
  *)

  // Using TFile:
  // TFile.ReadAllText()

  {$IFDEF HAS_STRINGSTREAM}

  // Using a TStringStream:
  m:=TStringStream.Create('');
  try
    m.LoadFromFile(FileName);
    result:=m.DataString;
  finally
    m.Free;
  end;
  {$ELSE}

  // Using a TStringStream with data from a TFileStream:
  f:=TPersistence.OpenFile(FileName); // Buffered ?
  try
    m:=TStringStream.Create('');
    try
      m.CopyFrom(f,f.Size);
      result:=m.DataString;
    finally
      m.Free;
    end;
  finally
    f.Free;
  end;
  {$ENDIF}
end;

class function TBITextSource.StringsFrom(const FileName:String):TStrings;
begin
  result:=TStringList.Create;
  result.LoadFromFile(FileName);
end;

function TBITextSource.TryDecimal(const AText:String; out AValue:Single):Boolean;

  procedure ReverseDecimal;
  begin
    if ISettings.DecimalSeparator='.' then
       ISettings.DecimalSeparator:=','
    else
       ISettings.DecimalSeparator:='.';
  end;

begin
  result:=TStringToFloat.TryConvert(AText,AValue,ISettings);

  if not result then
  begin
    ReverseDecimal;

    result:=TStringToFloat.TryConvert(AText,AValue,ISettings);

    DecimalDetected:=result;

    if not result then
       ReverseDecimal;
  end;
end;

type
  TDataAccess=class(TDataItem);

class procedure TBITextSource.ChangeToText(const Col:TDataItem; const Index,Total:TInteger);
var tmpOld : TDataKind;
    t : TLoopInteger;
begin
  tmpOld:=Col.Kind;

  TDataAccess(Col).FKind:=dkText;
  Col.Resize(Total);

  case tmpOld of
     dkInt32 : for t:=0 to Index-1 do
                   Col.TextData[t]:=IntToStr(Col.Int32Data[t]);

     dkInt64 : for t:=0 to Index-1 do
                   Col.TextData[t]:=IntToStr(Col.Int64Data[t]);

    dkSingle : for t:=0 to Index-1 do
                   Col.TextData[t]:=FloatToStr(Col.SingleData[t]);

    dkDouble : for t:=0 to Index-1 do
                   Col.TextData[t]:=FloatToStr(Col.DoubleData[t]);

  dkExtended : for t:=0 to Index-1 do
                   Col.TextData[t]:=FloatToStr(Col.ExtendedData[t]);

   dkBoolean : for t:=0 to Index-1 do
                   Col.TextData[t]:=BoolToStr(Col.BooleanData[t],True);
  end;

  Col.SingleData:=nil;
  Col.DoubleData:=nil;
  Col.ExtendedData:=nil;
  Col.BooleanData:=nil;
end;

class procedure TBITextSource.ChangeToBoolean(const Col:TDataItem; const AValue:Boolean);
var t : TLoopInteger;
begin
  if Col.Kind<>TDataKind.dkBoolean then
  begin
    Col.BooleanData.Resize(Col.Parent.Count);

    if Col.Kind=TDataKind.dkText then
       for t:=0 to Col.Count-1 do
           Col.BooleanData[t]:=StrToBool(Col.TextData[t]);

    TDataAccess(Col).FKind:=TDataKind.dkBoolean;

    Col.Int32Data:=nil;
    Col.Int64Data:=nil;
    Col.SingleData:=nil;
    Col.DoubleData:=nil;
    Col.ExtendedData:=nil;
    Col.DateTimeData:=nil;
    Col.TextData:=nil;
  end;
end;

class procedure TBITextSource.ChangeBoolToInt(const Col:TDataItem; const ARow:TInteger);
var t : TLoopInteger;
begin
  TDataAccess(Col).FKind:=TDataKind.dkInt32;
  Col.Resize(Col.Parent.Count);

  for t:=0 to ARow-1 do
      Col.Int32Data[t]:=Ord(Col.BooleanData[t]);

  Col.BooleanData:=nil;
end;

// Returns Value parameter, with all potential AChar characters removed
function RemoveChar(const Value:String; const AChar:Char):String;
var tmp : Char;
    t   : Integer;
begin
  result:='';

  for t:=1 to Length(Value) do
  begin
    tmp:=Value[t];

    if tmp<>AChar then
       result:=result+tmp;
  end;
end;

// http://www.xe.com/symbols.php
function IsCurrencySymbol(const Value:Char):Boolean; overload;
const
  Currencies : Array[0..4] of Char =('$','€','¢','£','¥');  //fix for versions <D21

var t : Integer;
begin
  for t:=Low(Currencies) to High(Currencies) do
      if Currencies[t]=Value then
         Exit(True);

  result:=False;
end;

function IsCurrencySymbol(const Value:String; const APos:Integer):Boolean; overload;
begin
  if Length(Value)>=APos then
     result:=IsCurrencySymbol(Value[APos])
  else
     result:=False;
end;

// Returns Value parameter, with potential currency symbol removed from start or end
function RemoveCurrency(const Value:String):String;
var tmp : Integer;
begin
  result:=Trim(Value);

  tmp:=Length(result);

  if IsCurrencySymbol(result,1) then // at start
  begin
    Dec(tmp);
    result:=Copy(result,2,tmp);
  end;

  if IsCurrencySymbol(result,tmp) then // at end
     result:=Copy(result,1,tmp-1);
end;

// Improved TryStrToFloat function.
// Adds support for "thousand" separators and/or currency symbols.
//
// Examples:
//
// TryStringToFloat('123,456.78');
// TryStringToFloat('€ 123.45');
//
// NOTE: This function might be SLOW, compared to plain numbers like: '123.45'

class function TStringToFloat.TryConvert(const Value:String; out AFloat:Single;
                                         const Settings:TFormatSettings):Boolean;
var tmp : String;
begin
  if Value='' then
     Exit(False)
  else
     result:=TryStrToFloat(Value,AFloat,Settings);

  if not result then
  begin
    // Try removing ThousandSeparator from string. (Locale-dependant)
    tmp:=RemoveChar(Value,Settings.ThousandSeparator);

    result:=TryStrToFloat(tmp,AFloat,Settings);

    if not result then
       // Try also removing possible currency symbols at start or end ($, €, etc)
       result:=TryStrToFloat(RemoveCurrency(tmp),AFloat,Settings);
  end;
end;

class function TStringToFloat.TryConvert(const Value:String; out AFloat:Double;
                                         const Settings:TFormatSettings):Boolean;
var tmp : String;
begin
  if Value='' then
     Exit(False)
  else
     result:=TryStrToFloat(Value,AFloat,Settings);

  if not result then
  begin
    // Try removing ThousandSeparator from string. (Locale-dependant)
    tmp:=RemoveChar(Value,Settings.ThousandSeparator);

    result:=TryStrToFloat(tmp,AFloat,Settings);

    if not result then
       // Try also removing possible currency symbols at start or end ($, €, etc)
       result:=TryStrToFloat(RemoveCurrency(tmp),AFloat,Settings);
  end;
end;

class function TStringToFloat.TryConvert(const Value:String; out AFloat:Extended;
                                         const Settings:TFormatSettings):Boolean;
var tmp : String;
begin
  if Value='' then
     Exit(False)
  else
     result:=TryStrToFloat(Value,AFloat,Settings);

  if not result then
  begin
    // Try removing ThousandSeparator from string. (Locale-dependant)
    tmp:=RemoveChar(Value,Settings.ThousandSeparator);

    result:=TryStrToFloat(tmp,AFloat,Settings);

    if not result then
       // Try also removing possible currency symbols at start or end ($, €, etc)
       result:=TryStrToFloat(RemoveCurrency(tmp),AFloat,Settings);
  end;
end;

class function TStringToFloat.TryConvert(const Value:String):Boolean;
var Dummy : Double;
begin
  result:=TryConvert(Value,Dummy,FormatSettings);
end;

procedure TBITextSource.SetColumn(const Col:TDataItem; const ARow:TInteger; const Value:String);

  procedure DoChangeToText;
  begin
    ChangeToText(Col,ARow,Col.Parent.Count);
    Col.TextData[ARow]:=(*{$IFNDEF BISPLIT}Dequoted{$ENDIF}*)(Value);
  end;

  procedure TryBoolean;
  function IsBoolean(const AText:String):Boolean;
  begin
    result:=SameText(Value,AText) or (QuotedBooleans and SameText(Value,'"'+AText+'"'));
  end;

  var tmp : Integer;
  begin
    if ZeroOneAsBoolean then
    begin
      if SameText(Value,'1') then
         Col.BooleanData[ARow]:=True
      else
      if SameText(Value,'0') then
         Col.BooleanData[ARow]:=False
      else
      begin
        if TryStrToInt(Value,tmp) then
        begin
          ChangeBoolToInt(Col,ARow);
          Col.Int32Data[ARow]:=tmp;
        end
        else
          DoChangeToText;
      end;
    end
    else
    if IsBoolean('TRUE') then
    begin
      ChangeToBoolean(Col,True);
      Col.BooleanData[ARow]:=True;
    end
    else
    if IsBoolean('FALSE') then
    begin
      ChangeToBoolean(Col,False);
      Col.BooleanData[ARow]:=False;
    end
    else
      DoChangeToText;
  end;

  procedure TryExtended;
  var tmp : Extended;
      OldKind : TDataKind;
      t : TLoopInteger;
  begin
    if TStringToFloat.TryConvert(Value,tmp,ISettings) then
    begin
      OldKind:=Col.Kind;

      if OldKind<>dkExtended then
      begin
        TDataAccess(Col).FKind:=dkExtended;
        Col.Resize(Col.Parent.Count);

        case OldKind of
          dkInt32: begin
                     for t:=0 to ARow-1 do
                         Col.ExtendedData[t]:=Col.Int32Data[t];

                     Col.Int32Data:=nil;
                   end;

          dkInt64: begin
                     for t:=0 to ARow-1 do
                         Col.ExtendedData[t]:=Col.Int64Data[t];

                     Col.Int64Data:=nil;
                   end;

         dkSingle: begin
                     for t:=0 to ARow-1 do
                         Col.ExtendedData[t]:=Col.SingleData[t];

                     Col.SingleData:=nil;
                   end;

         dkDouble: begin
                     for t:=0 to ARow-1 do
                         Col.ExtendedData[t]:=Col.DoubleData[t];

                     Col.DoubleData:=nil;
                   end;
        end;
      end;

      Col.ExtendedData[ARow]:=tmp;
    end
    else
      TryBoolean;
  end;

  procedure TryDouble;
  var tmp : Double;
      OldKind : TDataKind;
      t : TLoopInteger;
  begin
    if TStringToFloat.TryConvert(Value,tmp,ISettings) then
    begin
      OldKind:=Col.Kind;

      if OldKind<>dkDouble then
      begin
        TDataAccess(Col).FKind:=dkDouble;
        Col.Resize(Col.Parent.Count);

        case OldKind of
          dkInt32: begin
                     for t:=0 to ARow-1 do
                         Col.DoubleData[t]:=Col.Int32Data[t];

                     Col.Int32Data:=nil;
                   end;

          dkInt64: begin
                     for t:=0 to ARow-1 do
                         Col.DoubleData[t]:=Col.Int64Data[t];

                     Col.Int64Data:=nil;
                   end;

         dkSingle: begin
                     for t:=0 to ARow-1 do
                         Col.DoubleData[t]:=Col.SingleData[t];

                     Col.SingleData:=nil;
                   end;
        end;
      end;

      Col.DoubleData[ARow]:=tmp;
    end
    else
       TryExtended;
  end;

  procedure TrySingle;
  var tmp : Single;
      OldKind : TDataKind;
      t : TLoopInteger;
      tmpOk : Boolean;
  begin
    if DecimalDetected then
       tmpOk:=TStringToFloat.TryConvert(Value,tmp,ISettings)
    else
       tmpOk:=TryDecimal(Value,tmp);

    if tmpOk then
    begin
      OldKind:=Col.Kind;

      if OldKind<>dkSingle then
      begin
        TDataAccess(Col).FKind:=dkSingle;
        Col.Resize(Col.Parent.Count);

        if OldKind=dkInt32 then
        begin
          for t:=0 to ARow-1 do
              Col.SingleData[t]:=Col.Int32Data[t];

          Col.Int32Data:=nil;
        end
        else
        begin
          for t:=0 to ARow-1 do
              Col.SingleData[t]:=Col.Int64Data[t];

          Col.Int64Data:=nil;
        end;
      end;

      Col.SingleData[ARow]:=tmp;
    end
    else
       TryDouble;
  end;

  procedure TryInt64;
  var tmp : Int64;
      t : TLoopInteger;
      ValError : Integer;
  begin
    Val(Value,tmp,ValError);

    if ValError=0 then
    begin
      if Col.Kind=dkInt32 then
      begin
        TDataAccess(Col).FKind:=dkInt64;
        Col.Resize(Col.Parent.Count);

        for t:=0 to ARow-1 do
            Col.Int64Data[t]:=Col.Int32Data[t];

        Col.Int32Data:=nil;
      end;

      Col.Int64Data[ARow]:=tmp;
    end
    else
      TrySingle;
  end;

  procedure DoError;
  begin
    raise EBIException.Create('Error at SetColumn: Data: '+Col.Name+' Kind is Unknown');
  end;

var ValError,
    tmp : Integer;
begin
  if (not IgnoreMissing) and ((Value='') or (Value=MissingValue)) then
     Col.Missing[ARow]:=True
  else
  case Col.Kind of
     dkInt32 : begin
                 Val(Value,tmp,ValError);

                 if ValError=0 then
                    Col.Int32Data[ARow]:=tmp
                 else
                    TryInt64;
               end;

     dkInt64 : TryInt64;
    dkSingle : TrySingle;
    dkDouble : TryDouble;
  dkExtended : TryExtended;

      dkText : Col.TextData[ARow]:=Value;
  dkDateTime : Col.DateTimeData[ARow]:=StrToDateTime(Value);
   dkBoolean : TryBoolean;
  else
    DoError;
  end;
end;

// Try to determine the kind of data inside Value string
function TBITextSource.GuessKind(const Value:String): TDataKind;
var tmpInt32 : Integer;
    tmpInt64 : Int64;
    tmpSingle : Single;
    tmpDouble : Double;
    tmpExtended : Extended;
    tmpDate : TDateTime;

    tmpOk : Boolean;
begin
  if ZeroOneAsBoolean and (SameText(Value,'0') or SameText(Value,'1')) then
     result:=dkBoolean
  else

  if TryStrToInt(Value,tmpInt32) then
     result:=dkInt32
  else
  if TryStrToInt64(Value,tmpInt64) then
     result:=dkInt64
  else
  begin
    if DecimalDetected then
       tmpOk:=TStringToFloat.TryConvert(Value,tmpSingle,ISettings)
    else
       tmpOk:=TryDecimal(Value,tmpSingle);

    if tmpOk then
       result:=dkSingle
    else
    if TStringToFloat.TryConvert(Value,tmpDouble,ISettings) then
       result:=dkDouble
    else
    if TStringToFloat.TryConvert(Value,tmpExtended,ISettings) then
       result:=dkExtended
    else
    if SameText(Value,'TRUE') or SameText(Value,'FALSE') then
       result:=dkBoolean
    else
    if QuotedBooleans and (SameText(Value,'"TRUE"') or SameText(Value,'"FALSE"')) then
       result:=dkBoolean
    else
    if TryStrToDateTime(Value,tmpDate) then
       result:=dkDateTime
    else
       result:=dkText
  end;
end;

function TBITextSource.InternalImportFile(const FileName:String):TDataArray;
var S : TStrings;
begin
  S:=TBITextSource.StringsFrom(FileName);
  try
    result:=Import(S);
  finally
    S.Free;
  end;
end;

function TBITextSource.DoImportFile(const FileName:String):TDataArray;
begin
  result:=InternalImportFile(FileName);

  // Pending to clarify "purge" of nil imported items
  if Length(result)=1 then
     if result[0]=nil then
        result:=nil
     else
        result[0].Name:=NameOfFile(FileName);
end;

function TBITextSource.DoImportStream(const AExtension: String;
  const AStream: TStream): TDataArray;
var tmp : TStrings;
begin
  if Supports(AExtension) then
  begin
    tmp:=TStringList.Create;
    try
      tmp.LoadFromStream(AStream);
      result:=Import(tmp);
    finally
      tmp.Free;
    end;
  end
  else
     inherited;
end;

procedure CheckZBS;
begin
  {$IFDEF D18}
  TBITextSource.ZBS:=Low(String)=0;
  {$ELSE}
  TBITextSource.ZBS:=False;
  {$ENDIF}

  if TBITextSource.ZBS then
     TBITextSource.FirstStringChar:=0
  else
     TBITextSource.FirstStringChar:=1;
end;

type
  TBIFileImporter=class(TDataImporter)
  private
    FStore : String;
  public
    Constructor CreateDefinition(const AStore:String; const ADefinition:TDataDefinition); override;

    function AllData:TStringArray; override;
    function GetDefinition(const AName:String):TDataDefinition; override;
    function Import:TDataArray; override;
    class function IsRemote:Boolean; override;
    function Load(const AName:String):TDataItem; override;

    class function Supports(const Kind:TDataDefinitionKind;
                       const ADefinition:TDataDefinition=nil):Boolean; override;
  end;

{ TBIFileImporter }

Constructor TBIFileImporter.CreateDefinition(const AStore: String; const ADefinition:TDataDefinition);
begin
  inherited;
  FStore:=AStore;
end;

function TBIFileImporter.AllData:TStringArray;

  procedure TryAdd(const S:String);
  var t : Integer;
  begin
    for t:=Low(result) to High(result) do
        if SameText(result[t],S) then
           Exit;

    result.Add(S);
  end;

var tmp : TStringDynArray;
    L,t : Integer;
    tmpPath : String;
begin
  if FStore='' then
     tmpPath:=TStore.PathOf(TStore.DefaultName)
  else
     tmpPath:=TStore.PathOf(FStore);

  if DirectoryExists(tmpPath) then
  begin
    // Candidate to lock-protect:
    // Lock.Enter
    // try

    // Try append possible binary data files:
    tmp:=TDirectory.GetFiles(tmpPath,'*'+TPersistence.Extension);

    // finally
    //   Lock.Leave
    // end;

    L:=Length(tmp);
    SetLength(result,L);

    for t:=Low(tmp) to High(tmp) do
        result[t-Low(tmp)]:=TStore.NameOf(tmp[t]);

    // Try append possible data definitions:
    tmp:=TDirectory.GetFiles(tmpPath,'*'+TDataDefinition.Extension);

    for t:=Low(tmp) to High(tmp) do
        TryAdd(TStore.NameOf(tmp[t]));

    // Try append possible sub-folders:
    tmp:=TDirectory.GetDirectories(tmpPath);

    for t:=Low(tmp) to High(tmp) do
        TryAdd(TPath.GetFileName(tmp[t]));
  end
  else
     result:=nil;
end;

function TBIFileImporter.Load(const AName:String):TDataItem;

  procedure AppendSubItems(const AData:TDataItem; const AFolder:String);
  var tmpFiles : TStringDynArray;
      tmp : String;
      tmpFolder,
      tmpItem : TDataItem;
  begin
    tmpFiles:=TDirectory.GetFiles(AFolder,'*'+TPersistence.Extension);

    for tmp in tmpFiles do
    begin
      tmpItem:=TPersistence.Load(tmp);
      TDataPersistence.Load(TPath.ChangeExtension(tmp,TDataPersistence.Extension),tmpItem,True);

      AData.Items.Add(tmpItem);
    end;

    tmpFiles:=TDirectory.GetDirectories(AFolder);

    if Length(tmpFiles)>0 then
    begin
      for tmp in tmpFiles do
      begin
        tmpFolder:=AData.Items.Add(TPath.GetFileName(tmp),TDataKind.dkUnknown);

        AppendSubItems(tmpFolder,tmp);
      end;
    end;
  end;

var tmpName : String;
begin
  tmpName:=TStore.FullPath(FStore,AName);

  if TFile.Exists(tmpName+TDataPersistence.Extension) then
  begin
    // Two data files, *.bi and *.databi
    result:=TPersistence.Load(tmpName+TPersistence.Extension);

    if result<>nil then
    begin
      result.Name:=AName;
      TDataPersistence.Load(tmpName+TDataPersistence.Extension,result,True);
    end;
  end
  else
  if TFile.Exists(tmpName+TPersistence.Extension) then
     // Single file (only *.bi) containing both structure and data
     result:=TDataItemPersistence.Load(tmpName+TPersistence.Extension)
  else
     result:=nil;

  // Folder exists with same data name?
  if TDirectory.Exists(tmpName) then
  begin
    if result=nil then
    begin
      result:=TDataItem.Create;
      result.Name:=TPath.GetFileName(tmpName);
    end;

    AppendSubItems(result,tmpName);
  end;
end;

function TBIFileImporter.GetDefinition(const AName:String):TDataDefinition;
var tmp : String;
begin
  tmp:=TStore.DefinitionOf(FStore,AName);

  if TFile.Exists(tmp) then
     result:=TDataDefinition.FromFile(nil,tmp) // <-- important: always "nil" as owner !
  else
     result:=TDataDefinition.Create(nil); // <-- important: always "nil" as owner !

  // NO: result.Name:= (AName might contain invalid TComponentName)
end;

function TBIFileImporter.Import:TDataArray;
var tmp : TBIFileSource;
    tmpFolder,
    tmpFile : String;
    tmpCancel : Boolean;
begin
  result:=nil;

  tmpCancel:=False;
  DoProgress(0,tmpCancel);

  if not tmpCancel then
  begin
    tmp:=TBIFileSource.Create(Definition,Definition.Parallel);
    try
      tmp.OnProgress:=Definition.OnImporting;
      tmp.OnError:=Definition.OnError;

      tmpFile:=Definition['FileName'];

      if tmpFile<>'' then
      begin
        if (not TCommonUI.IsURL(tmpFile)) and TPath.IsRelativePath(tmpFile) then
           tmpFile:=TStore.FullPath(FStore,tmpFile);

        result:=tmp.ImportFile(tmpFile);
      end
      else
      begin
        tmpFolder:=Trim(Definition['Folder']);

        if tmpFolder<>'' then
        begin
          if TPath.IsRelativePath(tmpFolder) then
             tmpFolder:=TStore.FullPath(FStore,tmpFolder);

          if tmpFolder<>'' then
             result:=tmp.Import(tmpFolder,Definition['IncludeMask'],
                   Definition['ExcludeMask'],Definition.AsBoolean('Recursive'));
        end;
      end;
    finally
      tmp.Free;
    end;
  end;

  DoProgress(100,tmpCancel);
end;

class function TBIFileImporter.IsRemote: Boolean;
begin
  result:=False;
end;

class function TBIFileImporter.Supports(const Kind:TDataDefinitionKind;
                               const ADefinition:TDataDefinition=nil): Boolean;
begin
  result:=Kind=TDataDefinitionKind.Files;
end;

{ TBIFileImporters }

class function TBIFileImportersHelper.Find(const AClass: TBIFileSourceClass): Integer;
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t]=AClass then
         Exit(t);

  result:=-1;
end;

class function TBIFileImportersHelper.GuessExtension(const Extension:String): TBIFileSourceClass;
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t].Supports(Extension) then
         Exit(Items[t]);

  result:=nil;
end;

class procedure TBIFileImportersHelper.RegisterClass(const AClass: TBIFileSourceClass);
var L : Integer;
begin
  if Find(AClass)=-1 then
  begin
    L:=Length(Items);
    SetLength(Items,L+1);
    Items[L]:=AClass;
  end;
end;

class procedure TBIFileImportersHelper.UnRegisterClass(const AClass: TBIFileSourceClass);
var t,L,
    tmp : Integer;
begin
  tmp:=Find(AClass);

  if tmp<>-1 then
  begin
    L:=Length(Items);

    for t:=tmp to L-2 do
        Items[t]:=Items[t+1];

    SetLength(Items,L-1);
  end;
end;

{ TDataCursorItemsHelper }

procedure TDataCursorItemsHelper.Add(const AData: TDataItem);
var L : Integer;
begin
  L:=Count;
  SetLength(Self,L+1);

  Self[L].Active:=True;
  Self[L].Data:=AData;
end;

procedure TDataCursorItemsHelper.Clear;
var tmp : TDataCursorItem;
begin
  for tmp in Self do
      if tmp.OwnsData then
         {$IFDEF AUTOREFCOUNT}
         tmp.Data.DisposeOf;
         {$ELSE}
         tmp.Data.Free;
         {$ENDIF}

  Self:=nil;
end;

function TDataCursorItemsHelper.Count: Integer;
begin
  result:=Length(Self);
end;

procedure TDataCursorItemsHelper.Delete(const AIndex: Integer);
var t : Integer;
begin
  for t:=AIndex to High(Self)-1 do
      Self[t]:=Self[t+1];

  SetLength(Self,High(Self));
end;

procedure TDataCursorItemsHelper.Exchange(const A, B: Integer);
var tmp : TDataCursorItem;
begin
  tmp:=Self[A];
  Self[A]:=Self[B];
  Self[B]:=tmp;
end;

function TDataCursorItemsHelper.Find(const AData: TDataItem): Integer;
var t : Integer;
begin
  for t:=0 to High(Self) do
      if Self[t].Data=AData then
         Exit(t);

  result:=-1;
end;

{ TDataCursor }

Constructor TDataCursor.Create(AOwner:TComponent);
begin
  inherited;

  ValidIndex:=True;
  FUseFilter:=True;
end;

Destructor TDataCursor.Destroy;
begin
  Clear;
  inherited;
end;

procedure TDataCursor.Clear;
begin
  Data:=nil;
  ValidIndex:=True;

  Items.Clear;
  Index:=nil;

  SortBy.Clear;

  // Offset
  Start:=0;

  // Limit
  Max:=0;
  TopIsPercent:=False; // Initialize TopIsPercent

  {$IFNDEF AUTOREFCOUNT}
  FFilter.Free;
  {$ENDIF}

  FFilter:=nil;
end;

function TDataCursor.Count: TInteger;
begin
  if Index=nil then
     if ValidIndex and (Data<>nil) then
     begin
       Data.Load;

       if Data.AsTable or (Data.Kind<>TDataKind.dkUnknown) then
          result:=Data.Count
       else
          result:=Data.Items.Count
     end
     else
        result:=0
  else
     result:=Length(Index);
end;

procedure TDataCursor.SetDirectFilter(const Value: TExpression);
begin
  FFilter.Free;
  FFilter:=Value;
end;

procedure TDataCursor.SetFilter(const Value: TExpression);
begin
  if FFilter<>Value then
     SetDirectFilter(TExpression.Clone(Value));
end;

class procedure TDataCursor.SetMasterExpression(const Master:TDataItem; const MasterCol:TExpression; const AIndex:TInteger);
begin
  if MasterCol<>nil then
  case Master.Kind of
       dkInt32: TIntegerExpression(MasterCol).Number:=Master.Int32Data[AIndex];
       dkInt64: TIntegerExpression(MasterCol).Number:=Master.Int64Data[AIndex];
      dkSingle: TFloatExpression(MasterCol).Number:=Master.SingleData[AIndex];
      dkDouble: TFloatExpression(MasterCol).Number:=Master.DoubleData[AIndex];
    dkExtended: TFloatExpression(MasterCol).Number:=Master.ExtendedData[AIndex];
        dkText: TTextExpression(MasterCol).Text:=Master.TextData[AIndex];
    dkDateTime: TDateTimeExpression(MasterCol).DateTime:=Master.DateTimeData[AIndex];
     dkBoolean: TBooleanExpression(MasterCol).Logical:=Master.BooleanData[AIndex];
  end;
end;

// Return the sortered, filtered, extracted portion of Data.
// Note: If no sort, filter, etc is defined, this function just returns
// the Data property for speed reasons.
function TDataCursor.ToData: TDataItem;

  function AllData:Boolean;
  begin
    result:=((not UseFilter) or (FFilter=nil)) and
            (FMax=0) and
            (FStart=0) and
            (Index=nil) and
            (Items=nil) and
            (SortBy.ActiveCount=0);
  end;

var tmpStart,
    Finish : TInteger;
    t : TLoopInteger;
    tmp : TDataArray;
begin
  result:=Data;

  if result<>nil then
     if AllData then
        result.Load
     else
     begin
       PrepareIndex;

       if ValidIndex then
       begin
         CalcStartFinish(tmpStart,Finish);

         tmp:=DataItems;

         result:=TDataClone.CloneStructure(Data,tmp);
         result.Resize(Finish-tmpStart+1);

         for t:=tmpStart to Finish do
             TDataClone.CopyData(Data,result,Position(t),t-tmpStart,tmp);
       end;
     end;
end;

(*
// Experimental fast-compare expression, as an alternative to (slower)
   TLogicalExpression :  "data = value"

type
  TCompareExpression=class(TLogicalExpression)
  private
    FA,
    FB : TDataItem;
  public
    IndexA,
    IndexB : TInteger;

    Constructor Create(const A,B:TDataItem; const AIndex:TInteger);
    function Value:TData; override;
  end;

{ TCompareExpression }

Constructor TCompareExpression.Create(const A, B: TDataItem;
  const AIndex: TInteger);
begin
  inherited Create;
  FA:=A;
  FB:=B;
  IndexA:=AIndex;
end;

function TCompareExpression.Value: TData;
begin
  result:=TDataCompare.Compare(FA,IndexA,FB,IndexB);
end;
*)

// Return the logical expression (MasterCol) that will be used to decide which
// ADetail rows belong to the current AMaster row
class function TDataCursor.CreateFilter(const AMasters,ADetails:TDataArray;
                                  const AIndex:TInteger;
                                  out Expressions:TExpressions): TExpression;

  function ExpressionFrom(const AData:TDataItem):TDataItemExpression;
  begin
    result:=TDataItemExpression.Create(AData);
  end;

  function ExpressionOf(const AMaster,ADetail:TDataItem; out MasterCol:TExpression):TExpression;
  begin
    MasterCol:=nil;

    if AMaster.Kind=dkUnknown then
       result:=nil
    else
    begin
      if AMaster.Missing[AIndex] then
         result:=TIsNullData.Create(ExpressionFrom(ADetail))
      else
      {
      // Pending: Possible optimization when both Kind are identical
      if ADetail.Kind=AMaster.Kind then
         result:=TCompareExpression.Create(AMaster,ADetail,AIndex)
      else
      }
      begin
        case AMaster.Kind of
            dkInt32,
            dkInt64: MasterCol:=TIntegerExpression.Create(0);
           dkSingle,
           dkDouble,
         dkExtended: MasterCol:=TFloatExpression.Create(0);
             dkText: MasterCol:=TTextExpression.Create('');
         dkDateTime: MasterCol:=TDateTimeExpression.Create(0);
          dkBoolean: MasterCol:=TBooleanExpression.Create(False);
        end;

        result:=TLogicalExpression.Create(ExpressionFrom(ADetail),TLogicalOperand.Equal,MasterCol);
      end;
    end;
  end;

var t : Integer;
    tmp : TExpression;
    tmpMaster : TExpression;
begin
  Expressions:=nil;
  result:=nil;

  for t:=0 to High(AMasters) do
  begin
    tmp:=ExpressionOf(AMasters[t],ADetails[t],tmpMaster);
    Expressions.Add(tmpMaster);

    if result=nil then
       result:=tmp
    else
       result:=TLogicalExpression.Create(result,TLogicalOperand.&And,tmp);
  end;
end;

function TDataCursor.DetailIndex(const AMasters,ADetails:TDataArray;
                                 const AIndex:TInteger):TExpressions;
var t : Integer;
begin
  SetDirectFilter(CreateFilter(AMasters,ADetails,AIndex,result));

  for t:=0 to High(result) do
      SetMasterExpression(AMasters[t],result[t],AIndex);
end;

class function TDataCursor.DetailIndex(const AData:TDataItem;
                                       const AMasters,ADetails:TDataArray;
                                       const AIndex:TInteger):TCursorIndex;
var tmp : TDataCursor;
begin
  AData.Load;

  tmp:=TDataCursor.Create(nil);
  try
    tmp.Data:=AData;

    tmp.DetailIndex(AMasters,ADetails,AIndex);

    tmp.PrepareIndex;
    result:=tmp.Index;
  finally
    tmp.Free;
  end;
end;

class function TDataCursor.MasterDetailIndex(const AData:TDataItem;
                                             const AMasters:TDataArray;
                                             const AIndex:TInteger):TCursorIndex;

  function FindDetails(const AItems:TDataItems):TDataArray;
  var Item : TDataItem;
  begin
    result:=nil;

    for Item in AItems.AsArray do
        if TDataAccess(Item).HasMaster then
           if AMasters.Exists(Item.Master) then
              result.Add(Item);
  end;

var Details : TDataArray;
begin
  Details:=FindDetails(AData.Items);

  if Details=nil then
     result:=nil
  else
     result:=DetailIndex(AData,AMasters,Details,AIndex);
end;

function TDataCursor.DataItems: TDataArray;
var L : Integer;
    Item : TDataCursorItem;
begin
  result:=nil;
  L:=0;

  for Item in Items do
      if Item.Active then
      begin
        SetLength(result,L+1);

        Item.Data.Load;

        result[L]:=Item.Data;
        Inc(L);
      end;
end;

procedure TDataCursor.Delete(const APosition: TInteger);
begin
  if Data<>nil then
     if Index=nil then
        Data.Delete(APosition)
     else
        Data.Delete(Index[APosition]);
end;

procedure TDataCursor.GuessItems(const S: String);
var tmp : TStringArray;
    t, L : Integer;
    tmpS : String;
begin
  if S='' then
     Items:=nil
  else
  begin
    tmp:=TStringArray.Split(S,',');

    L:=tmp.Count;
    SetLength(Items,L);

    for t:=0 to L-1 do
    begin
      tmpS:=tmp[t];

      if Data.Items.Exists(tmpS) then
         Items[t].Data:=Data[tmpS]
      else
      begin
        Items[t].Data:=TExpressionColumn.From(Data,tmpS,True,tmpS);
        Data.Items.Add(Items[t].Data);
      end;

      Items[t].Active:=True;
    end;
  end;
end;

procedure TDataCursor.CalcStartFinish(out AStart,AFinish:TInteger);
var tmp : TInteger;
begin
  tmp:=Count-1;

  if Max<1 then
     AFinish:=tmp
  else
  begin
    AFinish:=Start+Max-1;

    if AFinish>tmp then
       AFinish:=tmp;
  end;

  AStart:=Start;

  if AStart<0 then
  begin
    AStart:=tmp+Start+1;

    if AStart<0 then
       AStart:=0;
  end;
end;

{$IFNDEF FPC}
procedure TDataCursor.Loop(const AProc: TCursorLoop);
var t,
    Finish : TInteger;
begin
  PrepareIndex;

  if ValidIndex then
  begin
    CalcStartFinish(t,Finish);

    while t<=Finish do
    begin
      AProc(Position(t));
      Inc(t);
    end;
  end;
end;
{$ENDIF}

function TDataCursor.Position(const APosition: TInteger): TInteger;
begin
  if Index=nil then
     result:=APosition
  else
     result:=Index[APosition];
end;

function TDataCursor.IndexOf(const AData: TDataItem): Integer;
var t : Integer;
begin
  for t:=Low(Items) to High(Items) do
      if Items[t].Data=AData then
         Exit(t);

  result:=-1;
end;

procedure TDataCursor.SetItems(const AItems:TDataArray);
var L, t,
    tmp : Integer;
begin
  L:=AItems.Count;
  SetLength(Items,L);

  for t:=Low(AItems) to High(AItems) do
  begin
    tmp:=t-Low(AItems);
    Items[tmp].Active:=True;
    Items[tmp].Data:=AItems[t];
  end;
end;

procedure TDataCursor.Load(const AData: TDataItem; const Children: Boolean);
begin
  // Dummy method. Unused here in TDataCursor.
  // An alternative would be making TDataProvider an interface (IDataProvider)
  // instead of deriving TDataSelect from it, make TDataSelect implement it.
end;

procedure TDataCursor.LoadData;
var Item : TDataCursorItem;
begin
  if (Items=nil) or ((Data<>nil) and (Data.Provider<>nil)) then
  begin
    if Data<>nil then
       Data.Load;
  end
  else
  for Item in Items do
      if Item.Active then
         Item.Data.Load;
end;

procedure TDataCursor.Loop(const AProc: TCursorLoopObject);
var t,
    Finish : TInteger;
begin
  PrepareIndex;

  if ValidIndex then
  begin
    CalcStartFinish(t,Finish);

    while t<=Finish do
    begin
      AProc(Position(t));
      Inc(t);
    end;
  end;
end;

function TDataCursor.HopsFromFilter:TDataHops;
begin
  if (not UseFilter) or (Filter=nil) then
     result:=nil
  else
  begin
    result:=TDataHops.Create;
    try
      result.Main:=Data;
      result.Add(Filter,False);

      // Pending to verify if the following Init is no longer needed
      //result.Init;
    except
      on Exception do
      begin
        // Avoid memory leak, destroy useless TDataHops
        result.Free;
        //result:=nil;

        raise;
      end;
    end;
  end;
end;

// Return a new TCursorIndex based from AIndex, after filtering it
function TDataCursor.ApplyFilterTo(const AIndex:TCursorIndex):TCursorIndex;
var FilterHops : TDataHops;
    t : TLoopInteger;
begin
  FilterHops:=HopsFromFilter;
  try
    if Index=nil then
    begin
      // Pending optimization:
      // Try to delimite index "tmp" size, if Filter is using a sorted data item,
      // searching for first..last.

      {$IFDEF FPC}
      result:=nil;
      {$ENDIF}

      // Current (not optimized) filter size is: 0..Data.Count
      result.Resize(Data.Count);

      for t:=0 to High(result) do
      begin
        FCurrent:=t;

        FilterHops.Invalidate(t);

        if Filter.Value then
           result[t]:=t
        else
           result[t]:=-1;
      end;
    end
    else
    begin
      result:=AIndex.Copy;

      for t:=0 to High(result) do
      begin
        FCurrent:=result[t];

        FilterHops.Invalidate(result[t]);

        if not Filter.Value then
           result[t]:=-1;
      end;
    end;
  finally
    FilterHops.Free;
  end;
end;

procedure TDataCursor.Add(const AData: TDataItem);
begin
  Items.Add(AData);
end;

procedure TDataCursor.Add(const AData: TDataArray);
var tmp : TDataItem;
begin
  for tmp in AData do
      Add(tmp);
end;

procedure TDataCursor.Add(const AExpression: TExpression);
var tmp : TDataItem;
begin
  tmp:=TExpressionColumn.Create(AExpression);
  tmp.Name:=AExpression.ToString;
  Add(tmp);

  Items[High(Items)].OwnsData:=True;
end;

procedure TDataCursor.Add(const AData:TDataItem; const AExpression: String);
var tmp : TExpression;
begin
  tmp:=TDataExpression.FromString(AData,AExpression);

  if tmp is TDataItemExpression then
  begin
    Add(TDataItemExpression(tmp).Data);
    tmp.Free;
  end
  else
    Add(tmp);
end;

procedure TDataCursor.Assign(Source:TPersistent);

  procedure AssignItems(const AItems:TDataCursorItems);
  var t, L : Integer;
      tmp : TExpression;
      tmpExp : TExpressionColumn;
  begin
    L:=Length(AItems);
    SetLength(Items,L);

    for t:=0 to L-1 do
    begin
      if AItems[t].Data is TExpressionColumn then
      begin
        tmpExp:=TExpressionColumn(AItems[t].Data);
        tmp:=TExpression.Clone(tmpExp.Expression);

        Items[t].Data:=TExpressionColumn.Create(tmp);
        Items[t].Data.Name:=tmpExp.Name;

        TExpressionColumn(Items[t].Data).Data:=tmpExp.Data;
      end
      else
        Items[t].Data:=AItems[t].Data;

      Items[t].Active:=AItems[t].Active;
      Items[t].OwnsData:=False;
    end;
  end;

var tmp : TDataCursor;
begin
  if Source is TDataCursor then
  begin
    tmp:=TDataCursor(Source);

    Max:=tmp.Max;
    Start:=tmp.Start;

    FFilter.Free;

    if tmp.Filter=nil then
       FFilter:=nil
    else
       FFilter:=TExpression.Clone(tmp.Filter);

    FUseFilter:=tmp.UseFilter;

    Data:=tmp.Data;
    SortBy:=tmp.SortBy;

    AssignItems(tmp.Items);

    Index:=tmp.Index;
  end;

  inherited;
end;

// Returns a new TCursorIndex with all AIndex elements that are <> -1
class function TDataCursor.PackIndex(const AIndex:TCursorIndex):TCursorIndex;
var t : TLoopInteger;
    tmp : TInteger;
begin
  {$IFDEF FPC}
  result:=nil;
  {$ENDIF}

  result.Resize(Length(AIndex));

  tmp:=0;

  for t:=0 to High(AIndex) do
      if AIndex[t]<>-1 then
      begin
        result[tmp]:=AIndex[t];
        Inc(tmp);
      end;

  if tmp>0 then
     result.Resize(tmp) // resize result to real size
  else
     result:=nil;
end;

// Calculate AIndex after applying sort order and filtering
procedure TDataCursor.PrepareIndex(const AIndex:TCursorIndex; const AllRows:Boolean);
begin
  LoadData;

  ValidIndex:=AllRows;

  // Pending, use AIndex parameter to apply Sort
  if SortBy.Items=nil then
     Index:=AIndex
  else
  if ValidIndex or (AIndex<>nil) then
     Index:=SortBy.Sort(Data,AIndex)
  else
     Index:=AIndex;

  ApplyFilter;
end;

procedure TDataCursor.ApplyFilter;
begin
  if UseFilter and (Filter<>nil) then
  begin
    Index:=PackIndex(ApplyFilterTo(Index));
    ValidIndex:=Index<>nil;
  end;
end;

{ TBIExport }

Constructor TBIExport.Create;
begin
end;

Destructor TBIExport.Destroy;
begin
  if Cursor<>nil then
     if Cursor.Parent=Self then
        Cursor.Free;

  inherited;
end;

procedure TBIExport.DoEmit(const AItems: TStrings);
begin
  if BinaryOnly then
     raise EBIException.Create('Error: ('+ClassName+') Exporting as text is not supported');
end;

class function TBIExport.AsString(const AData: TDataItem): String;
var tmp : TBIExport;
begin
  tmp:=Self.Create;
  try
    tmp.Data:=AData;
    result:=tmp.AsString;
  finally
    tmp.Free;
  end;
end;

procedure TBIExport.EmitTo(const AItems: TStrings);
begin
  if (Cursor=nil) or (Cursor.Data=nil) then
     raise EBIException.Create(BIMsg_Export_EmptyData);

  AItems.BeginUpdate;
  try
    Items:=Cursor.DataItems;

    if Items=nil then
       if Cursor.Data.AsTable then
          Items:=Cursor.Data.Items.AsArray
       else
          Items.Add(Cursor.Data);

    DoEmit(AItems);
  finally
    AItems.EndUpdate;
  end;
end;

function TBIExport.GetData: TDataItem;
begin
  if Cursor=nil then
     result:=nil
  else
     result:=Cursor.Data;
end;

class function TBIExport.FileFilter:TFileFilters;
begin
  result:=nil;
end;

procedure TBIExport.SaveToFile(const AFileName: String);
var tmp : TStrings;
begin
  tmp:=ToStrings;
  try
    tmp.SaveToFile(AFileName);
  finally
    tmp.Free;
  end;
end;

class procedure TBIExport.SaveToFile(const AData: TDataItem; const AFileName: String);
var tmp : TBIExport;
begin
  tmp:=Self.Create;
  try
    tmp.Data:=AData;
    tmp.SaveToFile(AFileName);
  finally
    tmp.Free;
  end;
end;

procedure TBIExport.SaveToStream(const AStream: TStream);
var tmp : TStrings;
begin
  tmp:=ToStrings;
  try
    tmp.SaveToStream(AStream);
  finally
    tmp.Free;
  end;
end;

procedure TBIExport.SetData(const Value: TDataItem);
begin
  if Value=nil then
  begin
    if Cursor<>nil then
       Cursor.Data:=nil;
  end
  else
  begin
    if Cursor=nil then
    begin
      Cursor:=TDataCursor.Create(nil);
      Cursor.Parent:=Self;
    end;

    Cursor.Data:=Value;
  end;
end;

class function TBIExport.ToStrings(const AData: TDataItem): TStrings;
var tmp : TBIExport;
begin
  tmp:=Self.Create;
  try
    tmp.Data:=AData;
    result:=tmp.ToStrings;
  finally
    tmp.Free;
  end;
end;

function TBIExport.ToStrings:TStrings;
begin
  result:=TStringList.Create;
  EmitTo(result);
end;

function TBIExport.AsString:String;
var tmp : TStrings;
begin
  tmp:=TStringList.Create;
  try
    EmitTo(tmp);
    result:=tmp.Text;
  finally
    tmp.Free;
  end;
end;

{ TBITextExport }

function TBITextExport.DataToString(const AData: TDataItem;
  const AIndex: TInteger): String;
begin
  if FloatFormat='' then
     result:=AData.DataToString(AIndex)
  else
  case AData.Kind of
    dkSingle   : result:=FormatFloat(FloatFormat,AData.SingleData[AIndex]);
    dkDouble   : result:=FormatFloat(FloatFormat,AData.DoubleData[AIndex]);
    dkExtended : result:=FormatFloat(FloatFormat,AData.ExtendedData[AIndex]);
  else
    result:=AData.DataToString(AIndex);
  end;
end;

{ TDataSelect }

function TDataSelect.MainData:TDataItem;
begin
  if Data=nil then
     GuessMainData;

  result:=Data;
end;

type
  TDataItemAccess=class(TDataItem);

// When Distinct=True, this function is used to search for any row that contains
// the same values as the last added row
function TDataSelect.FoundLast(const AResult:TDataItem; const ACount:TInteger):Boolean;
var t,tt : TLoopInteger;
    tmpEqual : Boolean;
    tmpLast : TInteger;
    tmpItems : TDataItems;
begin
  tmpLast:=ACount-1;

  if AResult.AsTable then
  begin
    tmpItems:=AResult.Items; // <-- cached

    if tmpItems.Count=1 then
       result:=TDataItemAccess(tmpItems[0]).ExistsBefore(tmpLast)
    else
    begin
      for t:=0 to tmpLast-1 do
      begin
        // Inner loop here, for speed
        tmpEqual:=True;

        for tt:=0 to tmpItems.Count-1 do
            if not tmpItems[tt].SameData(t,tmpLast) then
            begin
              tmpEqual:=False;
              break;
            end;

        if tmpEqual then
           Exit(True);
      end;

      result:=False;
    end;
  end
  else
    result:=TDataItemAccess(AResult).ExistsBefore(tmpLast);
end;

procedure TDataSelect.AddItems(const AData: TDataItem);
var tmp : TDataCursorItem;
    t : Integer;
    tmpSource : TDataItems;
begin
  // Pending: AsTable=False if AItems contains a single item?

  AData.AsTable:=True;

  for tmp in Items do
      if tmp.Active and (tmp.Data<>nil) then
      begin
        if tmp.Data.AsTable then
        begin
          // AddItem(AData,tmp) <--- add as "sub table"

          tmpSource:=tmp.Data.Items;

          for t:=0 to tmpSource.Count-1 do
              AddItem(AData,tmpSource[t],tmp.Name);
        end
        else
          AddItem(AData,tmp.Data,tmp.Name);
      end;
end;

procedure TDataSelect.GetItems(const AData: TDataItem);
begin
  AData.Load;
end;

procedure TDataSelect.Load(const AData: TDataItem; const Children: Boolean);
begin
  Calculate(AData);
  TDataAccess(AData).ClearDelay;
end;

class function TDataSelect.SetupHops(const Hops:TDataHops;
                                     const AItems:TDataArray):TInt32Array;

  procedure TrySetMainHops;

    function SetMainHops(const AData:TDataItem):Boolean;
    begin
      Hops.Main:=AData;
      Hops.Init;
      result:=Hops.Valid;
    end;

  var tmp : TDataItem;
      tmpData : TDataItem;
  begin
    for tmp in AItems do
    begin
      if tmp.AsTable then
         tmpData:=tmp
      else
      begin
        tmpData:=tmp.Parent;

        if tmpData=nil then
           if SetMainHops(tmp) then  // <-- enable case: select * from "item-without-parent-that-is-not-a-table"
              break;
      end;

      if tmpData<>nil then
         if SetMainHops(tmpData) then
            break;
    end;
  end;

var tmp : TDataItem;
    t : Integer;
begin
  SetLength(result,Length(AItems));

  for t:=0 to High(AItems) do
  begin
    tmp:=AItems[t];

    tmp.Load;

    if tmp is TExpressionColumn then
    begin
      (* Useless??????
         Main is set again in the next for loop

      if Hops.Main=nil then
      begin
        Hops.Main:=MainData;

        if Hops.Main=nil then
           Hops.Main:=tmp.Parent; // <-- last resort
      end;
      *)

      Hops.Add(TExpressionColumn(tmp).Expression,False);
    end
    else
    if tmp.AsTable or (tmp.Parent=nil) then
       Hops.Add(tmp)
    else
       Hops.Add(tmp.Parent);

    result[t]:=High(Hops.Hops);
  end;

  // try several Main until all hops Init are valid
  if not Hops.Valid then
     TrySetMainHops;

  if not Hops.Valid then
     raise EBIException.Create('Error: Cannot locate all data items from a common start data');
end;

function TDataSelect.ToString: String;
begin
  result:=TBISQL.From(Self);
end;

// Adds a new AItem to the AResult output
procedure TDataSelect.AddItem(const AResult,AItem:TDataItem; const AName:String);

  function CheckUniqueName(const AName:String):String;
  begin
    result:=AName;

    while AResult.Items.Exists(result) do
       if AItem.Parent=nil then
          result:='_'+result
       else
          result:=AItem.Parent.Name+'.'+result;
  end;

var tmp : TDataItem;
    tmpDest : TDataItem;
    tmpName : String;
begin
  if AName='' then
     tmpName:=CheckUniqueName(AItem.Name)
  else
     tmpName:=CheckUniqueName(AName);

  if AItem.AsTable then
  begin
    tmpDest:=TDataItem.Create(True);

    tmpDest.Name:=tmpName;
    AResult.Items.Add(tmpDest);

    for tmp in AItem.Items.AsArray do
        AddItem(tmpDest,tmp,'');
  end
  else
  begin
    tmp:=AResult.Items.Add(tmpName,AItem.Kind,AItem); // <-- Remember AItem as TagObject

    TDataAccess(tmp).SetInternalDate(TDataAccess(AItem).IDate);

    // "Remember" the original Data Item that corresponds to each Item in the query.
    // Note: Using the "Master" property is not a good solution (maybe better a new "Origin" property)
    tmp.Master:=AItem;
  end;
end;

procedure TDataSelect.Assign(Source:TPersistent);
begin
  if Source is TDataSelect then
  begin
    FDistinct:=TDataSelect(Source).Distinct;
    TopIsPercent:=TDataSelect(Source).TopIsPercent; // Assign TopIsPercent
  end;

  inherited;
end;

function TDataSelect.Calculate: TDataItem;
begin
  result:=TDataItem.Create;
  try
    Calculate(result);
  except
    on Exception do
    begin
      // Avoid memory leak
      result.Free;

      raise;
    end;
  end;
end;

procedure TDataSelect.GuessMainData;
var tmp : TDataArray;
    tmpHops : TDataHops;
begin
  tmp:=DataItems;

  if tmp.Count>0 then
  begin
    tmpHops:=TDataHops.Create;
    try
      SetupHops(tmpHops,tmp);
      Data:=tmpHops.Main;
    finally
      tmpHops.Free;
    end;
  end;
end;

type
  TExpressionColumnAccess=class(TExpressionColumn);

procedure TDataSelect.Calculate(const AData: TDataItem);
var
  HopsIndex : TInt32Array;
  Hops : TDataHops;

  procedure AddRow(const ASource,ADest:TDataArray; APos:TInteger);
  var t,
      tt : Integer;
      tmpSource : TDataItem;

      tmpPos,
      tmpIndex : TInteger;

      tmpSourceItems : TDataArray;
  begin
    tmpPos:=0;

    for t:=0 to ASource.Count-1 do
    begin
      tmpSource:=ASource[t];

      if tmpSource is TExpressionColumn then
         TExpressionColumnAccess(tmpSource).SetValue(ADest[tmpPos],APos)
      else
      begin
        tmpIndex:=Hops.Hops[HopsIndex[t]].Index;

        if tmpIndex<>-1 then
        begin
          // Special case, tmpSource can be "AsTable" for example "select *..."
          if tmpSource.AsTable then
          begin
            tmpSourceItems:=tmpSource.Items.AsArray;

            for tt:=0 to tmpSourceItems.Count-1 do
                TDataClone.CopyData(tmpSourceItems[tt],ADest[tmpPos+tt],tmpIndex,APos);

            Inc(tmpPos,tmpSourceItems.Count-1);
          end
          else
            TDataClone.CopyData(tmpSource,ADest[tmpPos],tmpIndex,APos);
        end;
      end;

      Inc(tmpPos);
    end;
  end;

const
  Capacity=32;

var
  FilterHops : TDataHops;
  HasSort : Boolean;
  tmpItems : TDataArray;
  tmpValid : Boolean;

  procedure LoopMain;
  var tmpMain : TLoopInteger;
      tmpSkip,
      tmpPos : TInteger;
      tmpDataItems : TDataArray;
  begin
    tmpPos:=0;

    if (Start>0) and (not HasSort) and (not Distinct) then
       tmpSkip:=Start
    else
       tmpSkip:=0;

    tmpDataItems:=AData.Items.AsArray;

    // This should be always a complete loop, from 0 to Hops.Main.Count-1
    for tmpMain:=0 to Hops.Main.Count-1 do
    begin
      // Apply filter ("where")
      if FilterHops<>nil then
      begin
        FilterHops.Invalidate(tmpMain);
        tmpValid:=Filter.Value;
      end;

      if tmpValid then
      begin
        if tmpSkip>0 then
           Dec(tmpSkip)
        else
        begin
          // Resize destination with extra capacity
          if tmpPos>AData.Count-1 then
             AData.Resize(tmpPos+Capacity);

          // Fill new result row
          Hops.Invalidate(tmpMain);
          AddRow(tmpItems,tmpDataItems,tmpPos);

          // Remove last row if its not distinct from other rows in result
          if Distinct and FoundLast(AData,tmpPos+1) then
             AData.Resize(tmpPos)
          else
             Inc(tmpPos); // Increment real counter

          // Top max (if not sorted)
          // Apply break only if Max is absolute and condition met
          if (not HasSort) and (not TopIsPercent) and (Max > 0) then
             if tmpPos >= Max then
                break;
          // If TopIsPercent is true, we collect all relevant rows first, then truncate after the loop.
        end;
      end;
    end;

    // Reset Count to exact real counter (tmpPos might be larger than final if TopIsPercent applies)
    // The final resizing considering TopIsPercent will happen *after* this LoopMain finishes
    // and after SortBy (if any)
    AData.Resize(tmpPos);
  end;

  function CanDirectCopy:Boolean;

    // Temporary. Remove when Expressions are supported in DirectCopy
    function HasExpressions:Boolean;
    var t : Integer;
    begin
      for t:=0 to High(tmpItems) do
          if tmpItems[t] is TExpressionColumn then
             Exit(True);

      result:=False;
    end;

    function AllMainChildren:Boolean;
    var t : Integer;
    begin
      for t:=0 to High(tmpItems) do
          if tmpItems[t].Parent<>Hops.Main then
             if tmpItems[t]<>Hops.Main then
                Exit(False);

      result:=True;
    end;

  {$DEFINE DIRECTCOPY}
  begin
    {$IFNDEF DIRECTCOPY}
    result:=False;
    {$ELSE}
    result:=(not Distinct) and (FilterHops=nil) and (not HasExpressions)
             and AllMainChildren;
    {$ENDIF}
  end;

  procedure DirectCopy;
  var tmpMax,
      tmpStart : TInteger;

    // Candidate to be moved to a new TDataClone.Clone overload
    procedure CloneItems(const ADest:TDataItem; const APos:Integer; const AItems:TDataItems);
    var tt : Integer;
    begin
      for tt:=0 to AItems.Count-1 do
          if AItems[tt].AsTable then
             CloneItems(ADest.Items[tt],APos,AItems[tt].Items)
          else
             TDataItemAccess(ADest.Items[APos+tt]).CloneData(AItems[tt],tmpStart,tmpMax);
    end;

  var L,
      t,
      tmpPos : Integer;
      tmpSource : TDataItems;
  begin
    if HasSort or (Max=0) then
       tmpMax:=Hops.Main.Count
    else
       tmpMax:=Max;

    if HasSort then
       tmpStart:=0
    else
       tmpStart:=Start;

    TDataAccess(AData).FCount:=tmpMax; // <-- redundant?

    L:=Length(tmpItems);

    tmpPos:=0;

    for t:=0 to L-1 do
    begin
      if tmpItems[t].AsTable then
      begin
        tmpSource:=tmpItems[t].Items;

        CloneItems(AData,tmpPos,tmpSource);

        Inc(tmpPos,tmpSource.Count);
      end
      else
      begin
        TDataItemAccess(AData.Items[tmpPos]).CloneData(tmpItems[t],tmpStart,tmpMax);
        Inc(tmpPos);
      end;
    end;
  end;

var L : Integer;
begin
  AData.Clear;

  tmpItems:=DataItems;

  L:=Length(tmpItems);

  if L>0 then
  begin
    AddItems(AData);

    //if AData.Items.Count=0 then
    //   GetItems(AData); // <-- probably it never gets here (AData.Items already forces GetItems)

    Hops:=TDataHops.Create;
    try
      HopsIndex:=SetupHops(Hops,tmpItems);

      Data:=Hops.Main;

      LoadData;

      tmpValid:=True;

      HasSort:=SortBy.ActiveCount>0;

      FilterHops:=HopsFromFilter;
      try
        if CanDirectCopy then
           DirectCopy
        else
           LoopMain;

        if HasSort then
        begin
          SortBy.SortData(AData);

          if Start>0 then // Apply Offset
          begin
            if AData.Count > Start then
              AData.Delete(0,Start)
            else
              AData.Resize(0); // Offset is beyond the number of rows
          end;

          // Apply TOP N or TOP N PERCENT after sorting and offset
          if Max > 0 then // Max holds N (either count or percentage)
          begin
            declare
              EffectiveMaxRows: Int64;
            begin
              if TopIsPercent then
              begin
                if AData.Count > 0 then
                  EffectiveMaxRows := (Int64(AData.Count) * Max) div 100 // Use Int64 for intermediate calc
                else
                  EffectiveMaxRows := 0;
              end
              else // Absolute Max
                EffectiveMaxRows := Max;

              if AData.Count > EffectiveMaxRows then
                 AData.Resize(EffectiveMaxRows);
            end;
          end;
        end
        else // Not HasSort
        begin
          // For non-sorted queries, truncation for TOP N PERCENT happens after LoopMain.
          // LoopMain has already applied absolute Max if not TopIsPercent.
          // AData.Count at this point is tmpPos from LoopMain.
          if TopIsPercent and (Max > 0) then
          begin
            declare
              EffectiveMaxRows: Int64;
            begin
              if AData.Count > 0 then // AData.Count is effectively tmpPos here
                EffectiveMaxRows := (Int64(AData.Count) * Max) div 100
              else
                EffectiveMaxRows := 0;

              if AData.Count > EffectiveMaxRows then
                AData.Resize(EffectiveMaxRows);
            end;
          end;
        end;

      finally
        FilterHops.Free;
      end;
    finally
      Hops.Free;
    end;
  end;

//  RestoreSortData; // <-- restore back all "real" data items

  TDataItemAccess(AData).ClearDelay;
end;

{ TDataClone }

// Returns a new TDataItem with a full copy of AData (structure and values)
class function TDataClone.Clone(const AData: TDataItem): TDataItem;
var tmp : TMemoryStream;
begin
  tmp:=TMemoryStream.Create;
  try
    TDataItemPersistence.Save(AData,tmp);

    tmp.Position:=0;

    result:=TDataItemPersistence.Load(tmp);
  finally
    tmp.Free;
  end;
end;

type
  TDataItemsAccess=class(TDataItems);

// Clone all ASource data to ADest without creating a new TDataItem
class procedure TDataClone.Clone(const ASource, ADest: TDataItem;
                                 const AItems:TDataArray);

  procedure AddItems(const ASource,ADest:TDataItem; const AItems:TDataArray);
  var tmp : TDataItem;
      tmpName : String;
  begin
    ASource.Load;

    ADest.AsTable:=ASource.AsTable;

    if not ADest.AsTable then
       TDataAccess(ADest).FKind:=ASource.Kind;

    if ASource.Count>0 then
       TDataAccess(ADest).CloneData(ASource,0,ASource.Count);

    for tmp in AItems do
    begin
      tmpName:=tmp.Name;

      if (tmpName='') or ADest.Items.Exists(tmpName) then
          tmpName:=TDataItemsAccess(ADest.Items).GetEmptyName;

      AddItems(tmp,ADest.Items.Add(tmpName,tmp.Kind),tmp.Items.AsArray);
    end;
  end;

begin
  // Alternative way:
  // BI.DataSource
  // ADest:=TDataClone.Clone(ASource)

  ADest.Name:=ASource.Name;

  ADest.Clear;

  if AItems=nil then
     AddItems(ASource,ADest,ASource.Items.AsArray)
  else
     AddItems(ASource,ADest,AItems);
end;

// Clones the structure of AData into existing ADest
class procedure TDataClone.CloneStructure(const ADest, AData: TDataItem;
                                          const AItems: TDataArray);

  procedure DoClone(const AItems:TDataArray);
  var t : Integer;
  begin
    for t:=0 to AItems.Count-1 do
        ADest.Items.Add(CloneStructure(AItems[t]));
  end;

begin
  AData.Load;

  ADest.Clear;

  ADest.AsTable:=AData.AsTable;
  TDataItemAccess(ADest).FKind:=AData.Kind;

  if AItems=nil then
     DoClone(AData.Items.AsArray)
  else
     DoClone(AItems);
end;

// Just return a clone of the AData structure (empty, with no array data).
// Clone also all items of AData, recursively
class function TDataClone.Clone(const AData: TDataItem;
  const AIndex: TNativeIntArray): TDataItem;

  procedure CopyData(const ASource,ADest:TDataItem);
  var t : TLoopInteger;
  begin
    for t:=0 to High(AIndex) do
        ADest.CopyFrom(t,ASource,AIndex[t]);
  end;

  procedure DoCopy(const ASource,ADest:TDataItem);
  var t : Integer;
      tmpSource,
      tmpDest : TDataArray;
  begin
    if ASource.AsTable then
    begin
      tmpSource:=ASource.Items.AsArray;
      tmpDest:=ADest.Items.AsArray;

      for t:=0 to tmpSource.Count-1 do
          DoCopy(tmpSource[t],tmpDest[t]);
    end
    else
      CopyData(ASource,ADest);
  end;

begin
  result:=TDataClone.CloneStructure(AData);
  result.Resize(AIndex.Count);

  DoCopy(AData,result);
end;

// Returns a new TDataItem with the same structure as ASource and a clone
// of all AItems structure, recursively
function CloneItem(const ASource:TDataItem; const AItems:TDataArray):TDataItem;
var tmp : TDataItem;
begin
  result:=TDataItem.Create(ASource.AsTable);
  result.Name:=ASource.Name;
  TDataAccess(result).FKind:=ASource.Kind;

  for tmp in AItems do
      result.Items.Add(CloneItem(tmp,tmp.Items.AsArray));
end;

// Returns a new TDataItem with the same structure as AData,
// optionally using only the AItems parameter (or all items if AItems is nil)

class function TDataClone.CloneStructure(const AData: TDataItem; const AItems:TDataArray): TDataItem;
begin
  AData.Load;

  if AItems=nil then
     result:=CloneItem(AData,AData.Items.AsArray)
  else
     result:=CloneItem(AData,AItems);
end;

class procedure TDataClone.CopyData(const ASource,ADest:TDataArray;
                                    const AIndex,ADestPos:TInteger);
var t : Integer;
begin
  for t:=0 to ASource.Count-1 do
      CopyData(ASource[t],ADest[t],AIndex,ADestPos);
end;

// Recursive copy items data.
// Note: ADest structure must be exactly the same as AItems (or ASource.Items)
class procedure TDataClone.CopyData(const ASource,ADest:TDataItem;
                                    const AIndex,ADestPos:TInteger;
                                    const AItems:TDataArray=nil);
var tmp: TDataArray;
begin
  if ASource.AsTable then
  begin
    if AItems=nil then
       tmp:=ASource.Items.AsArray
    else
       tmp:=AItems;

    CopyData(tmp,ADest.Items.AsArray,AIndex,ADestPos);
  end
  else
    ADest.CopyFrom(ADestPos,ASource,AIndex);
end;

type
  TBINativeImporter=class(TBIFileSource)
  public
    class function Supports(const Extension:String):Boolean; override;
  end;

{ TBINativeImporter }

class function TBINativeImporter.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,TDataItemPersistence.Extension);
end;

type
  TBINativeExport=class(TBIExport)
  public
    Constructor Create; override;

    class function FileFilter: TFileFilters; override;
    procedure SaveToFile(const AFileName:String); override;
    class function Supports(const Extension:String):Boolean; override;
  end;

Constructor TBINativeExport.Create;
begin
  inherited;
  BinaryOnly:=True;
end;

class function TBINativeExport.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add(BIMsg_NativeFiles,'*'+TDataPersistence.Extension);
end;

procedure TBINativeExport.SaveToFile(const AFileName:String);
begin
  TDataItemPersistence.Save(Data,AFileName);
end;

class function TBINativeExport.Supports(const Extension:String):Boolean;
begin
  result:=TBINativeImporter.Supports(Extension);
end;

{ TBIExportersHelper }

class function TBIExportersHelper.Find(const AClass: TBIExportClass): Integer;
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t]=AClass then
         Exit(t);

  result:=-1;
end;

class function TBIExportersHelper.GuessExtension(const Extension:String):TBIExportClass;
var t : Integer;
begin
  for t:=0 to High(Items) do
      if Items[t].Supports(Extension) then
         Exit(Items[t]);

  result:=nil;
end;

class procedure TBIExportersHelper.RegisterClass(const AClass:TBIExportClass);
var L : Integer;
begin
  if Find(AClass)=-1 then
  begin
    L:=Length(Items);
    SetLength(Items,L+1);
    Items[L]:=AClass;
  end;
end;

class procedure TBIExportersHelper.UnRegisterClass(const AClass:TBIExportClass);
var t,L,
    tmp : Integer;
begin
  tmp:=Find(AClass);

  if tmp<>-1 then
  begin
    L:=Length(Items);

    for t:=tmp to L-2 do
        Items[t]:=Items[t+1];

    SetLength(Items,L-1);
  end;
end;

{ TFileFilter }

// Return first extension found, without the "*."
function TFileFilter.FirstExtension: String;
var i : Integer;
begin
  result:=Extensions;

  i:=Pos(';',result);

  if i>0 then
     Delete(result,i,Length(result));

  if Copy(result,1,2)='*.' then
     Delete(result,1,2);
end;


initialization
  CheckZBS;

  TDataImporter.RegisterClass(TBIFileImporter);

  TBIFileImporters.RegisterClass(TBIFileSource);
  TBIFileImporters.RegisterClass(TBINativeImporter);

  TBIExporters.RegisterClass(TBINativeExport);

finalization
  TBIExporters.UnRegisterClass(TBINativeExport);

  TBIFileImporters.UnRegisterClass(TBINativeImporter);
  TBIFileImporters.UnRegisterClass(TBIFileSource);

  TDataImporter.UnRegisterClass(TBIFileImporter);
end.

