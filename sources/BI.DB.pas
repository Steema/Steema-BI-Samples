{*********************************************}
{  TeeBI Software Library                     }
{  Generic Database data import and export    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB;

interface

uses
  {System.}Classes, {System.}Types, {Data.}DB,
  BI.DB.Dataset, BI.DataItem, BI.Arrays, BI.DataSource, BI.Persist;

type
  TBIDB=class;

  TBIDBTester=class abstract
  public
    class procedure Test(const Driver,Database,Server,Port,User,Password:String;
                         const Prompt:Boolean); virtual; abstract;
  end;

  TBIDBTesterClass=class of TBIDBTester;

  TBIDBEngine=class abstract
  public
    class procedure AddFields(const Fields:TFieldDefs; const AItems:TDataArray);
    class function CloneConnection(const AConn:TCustomConnection): TCustomConnection; virtual; abstract;
    class function CreateConnection(const Definition:TDataDefinition):TCustomConnection; virtual; abstract;
    class function CreateDataSet(const AOwner:TComponent; const AData:TDataItem):TDataSet; virtual; abstract;
    class function CreateQuery(const AConnection:TCustomConnection; const SQL:String):TDataSet; virtual; abstract;
    class function DriverNames:TStringDynArray; virtual; abstract;
    class function DriverToName(const ADriver:String):String; virtual; abstract;
    class function FileFilter:TFileFilters; virtual; abstract;
    class function GetConnectionName(const AConnection:TCustomConnection):String; virtual; abstract;
    class function GetDriver(const AIndex:Integer):String; virtual; abstract;
    class function GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings; virtual; abstract;
    class function GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet; virtual; abstract;
    class function GetItemNames(const AConnection:TCustomConnection;
                                const IncludeSystem,IncludeViews:Boolean):TStrings; virtual; abstract;
    class procedure GuessForeignKeys(const AName:String; const Table:TDataSet;
                                     const AData:TDataItem; const Source:TBISource); virtual; abstract;
    class function ImportFile(const Source:TBIDB; const AFileName:String):TDataArray; virtual; abstract;
    class procedure StartParallel; virtual;
    class function Supports(const Extension:String):Boolean; virtual; abstract;
    class function SupportsParallel:Boolean; virtual;
    class function Tester:TBIDBTesterClass; virtual; abstract;
  end;

  TBIDB=class(TBIDatasetSource)
  private
    class var
      FEngine : TBIDBEngine;

    function Import(const Connection:TCustomConnection; const AName:String):TDataItem; overload;
    class function GetEngine: TBIDBEngine; static;
    function GetItems(const Connection:TCustomConnection):TStrings; overload;
    class function GetItems(const ADefinition:TDataDefinition):TStrings; overload;
    class procedure SetEngine(const Value: TBIDBEngine); static;
  protected
    function DoImportFile(const FileName:String):TDataArray; override;
  public
    Constructor CreateEngine(const AEngine:TBIDBEngine);

    class function FileFilter: TFileFilters; override;
    function Import(const Connection:TCustomConnection):TDataArray; overload;

    class function IncludedItems(const ADef:TDataDefinition): TDataItem;
    class function Import(const Connection:TCustomConnection; const MultiThread:Boolean):TDataArray; overload;
    class function Supports(const Extension:String):Boolean; override;

    class property Engine:TBIDBEngine read GetEngine write SetEngine;
  end;

  TBIDBExport=class
  public
    class procedure Add(const ADataSet:TDataSet; const AData:TDataItem); static;
    class function From(const AOwner:TComponent; const AData:TDataItem):TDataSet; static;
  end;

implementation

{$IFNDEF FPC}
{$IF CompilerVersion>27}
{$DEFINE THREADING}
{$ENDIF}
{$ENDIF}

uses
  {System.}SysUtils, {System.}Masks,

  {$IFDEF FPC}
  // MTProcs <-- DoParallel
  {$ELSE}
  System.IOUtils, System.Diagnostics,
  {$ENDIF}

  {$IFDEF THREADING}
  System.Threading,
  {$ENDIF}

  BI.Languages.English;

{ TBIDB }

Constructor TBIDB.CreateEngine(const AEngine: TBIDBEngine);
begin
  inherited Create;
  Engine:=AEngine;
end;

type
  TDataAccess=class(TDataItem);

function TBIDB.Import(const Connection:TCustomConnection; const AName:String):TDataItem;

  procedure GuessIndexed(const ATable:String);
  var tmp : TStrings;
      t : Integer;
      tmpCol : TDataItem;
  begin
    tmp:=Engine.GetKeyFieldNames(Connection,ATable);
    try
      for t:=0 to tmp.Count-1 do
      begin
        tmpCol:=result.Items.Find(tmp[t]);

        if tmpCol<>nil then
           tmpCol.Primary:=True;
      end;
    finally
      tmp.Free;
    end;
  end;

var tmpTable : TDataSet;
begin
  result:=nil;

  try
    tmpTable:=Engine.GetTable(Connection,AName);
    try
      result:=Import(tmpTable,AName);
      //result.Schema:=tmpTable.SchemaName;

      GuessIndexed(AName);

      Engine.GuessForeignKeys(AName,tmpTable,result,Self);
    finally
      tmpTable.Free;
    end;

  except
    on E:Exception do
    begin
      result.Free;
      result:=nil;

      if not CallOnError(E.Message) then
         raise;
    end;
  end;
end;

function TBIDB.DoImportFile(const FileName: String): TDataArray;
begin
  result:=Engine.ImportFile(Self,FileName);
end;

class function TBIDB.GetEngine: TBIDBEngine;
begin
  if FEngine=nil then
     raise EBIException.Create(BIMsg_DB_NoEngineConfigured);

  result:=FEngine;
end;

class function TBIDB.Import(const Connection:TCustomConnection; const MultiThread:Boolean):TDataArray;
begin
  with TBIDB.Create(nil,MultiThread) do
  try
    result:=Import(Connection);
  finally
    Free;
  end;
end;

class procedure TBIDB.SetEngine(const Value: TBIDBEngine);
begin
  if FEngine<>Value then
  begin
    FEngine.Free;
    FEngine:=Value;
  end;
end;

function TBIDB.GetItems(const Connection:TCustomConnection):TStrings;

  procedure Filter(const AStrings:TStrings; const AInclude,AExclude:TMask);
  var t : Integer;
  begin
    t:=0;

    while t<AStrings.Count do
         if ((AInclude=nil) or AInclude.Matches(AStrings[t])) and
            ((AExclude=nil) or (not AExclude.Matches(AStrings[t]))) then
             Inc(t)
          else
             AStrings.Delete(t);
  end;

  procedure GetMasks(out AInclude,AExclude:TMask);
  begin
    AInclude:=nil;
    AExclude:=nil;

    if (IDefinition=nil) or (not IDefinition.AsBoolean('DBALL')) then
    begin
      if IncludePattern<>'' then
         AInclude:=TMask.Create(IncludePattern);

      if ExcludePattern<>'' then
         AExclude:=TMask.Create(ExcludePattern);
    end;
  end;

  function GetItemNames:TStrings;
  var tmpViews,
      tmpSystem : Boolean;
  begin
    tmpSystem:=(IDefinition<>nil) and IDefinition.AsDatabase.IncludeSystem;
    tmpViews:=(IDefinition<>nil) and IDefinition.AsDatabase.IncludeViews;

    result:=Engine.GetItemNames(Connection,tmpSystem,tmpViews);
  end;

var tmpInclude,
    tmpExclude : TMask;
begin
  result:=GetItemNames;

  GetMasks(tmpInclude,tmpExclude);

  if (tmpInclude<>nil) or (tmpExclude<>nil) then
  try
    Filter(result,tmpInclude,tmpExclude);
  finally
    tmpExclude.Free;
    tmpInclude.Free;
  end;
end;

class function TBIDB.FileFilter: TFileFilters;
begin
  // Warning: Check "FEngine" private instead of "Engine" to avoid exception
  if FEngine=nil then
     result:=nil
  else
     result:=Engine.FileFilter;
end;

class function TBIDB.Supports(const Extension: String): Boolean;
begin
  // Warning: Check "FEngine" private instead of "Engine" to avoid exception
  result:=(FEngine<>nil) and Engine.Supports(Extension);
end;

// Removes the common left-most part of all Items Names (if they have a common prefix)
procedure CleanPrefix(var Items:TDataArray);

  function AllItemsSamePrefix(out PrefixLength:Integer):Boolean;

    function Prefix(const S:String):String;
    var i : Integer;
    begin
      i:=Pos('.',S); //S.IndexOf('.');

      if i>0 then
         result:=Copy(S,1,i) //S.Substring(0,i)
      else
         result:='';
    end;

  var tmp : String;
      t : Integer;
  begin
    result:=False;
    PrefixLength:=0;

    if Items.Count>0 then
    begin
      tmp:=Prefix(Items[0].Name);

      if tmp<>'' then
      begin
        for t:=1 to High(Items) do
            if Prefix(Items[t].Name)<>tmp then
               Exit;

        PrefixLength:=Length(tmp);
        result:=True;
      end;
    end;
  end;

var t : Integer;
    tmp : Integer;
begin
  if AllItemsSamePrefix(tmp) then
     for t:=0 to High(Items) do
         Items[t].Name:=Copy(Items[t].Name,tmp+1,Length(Items[t].Name)); //Items[t].Name.Substring(tmp+1);
end;

function TBIDB.Import(const Connection:TCustomConnection):TDataArray;
var tmp : TStrings;
    t,
    n   : Integer;

    tmpCancel : Boolean;

    {$IFDEF THREADING}
    tmpConn : Array of TCustomConnection;
    tmpResult : TDataArray;
    tmpTask : ITask;
    tmpCount : Integer;
    {$ENDIF}
begin
  result:=nil;

  tmp:=GetItems(Connection);
  try
    n:=tmp.Count;

    if n>0 then
    begin
      SetLength(result,n);

      tmpCancel:=False;
      DoProgress(0,tmpCancel);

      {$IFDEF THREADING}
      if Parallel then
      begin
        tmpResult:=result;

        SetLength(tmpConn,n);

        Engine.StartParallel;

        tmpCount:=0;

        tmpTask:=TTask.Run(procedure
        begin
          TParallel.&For(0,n-1,procedure(Idx:Integer)
            begin
              tmpConn[Idx]:=Engine.CloneConnection(Connection);
              try
                tmpResult[Idx]:=Import(tmpConn[Idx],tmp[Idx]);
              finally
                tmpConn[Idx].Free;
              end;

              AtomicIncrement(tmpCount);

              // Problem: queued calls are executed *after* tmpTask.Wait
              TThread.Queue(nil,procedure
              begin
                // Safety check
                if tmpTask<>nil then
                   DoProgress(tmpCount*100/n,tmpCancel);
              end);
            end);
        end);

        tmpTask.Wait;
        tmpTask:=nil;
      end
      else
      {$ENDIF}
      for t:=0 to n-1 do
      begin
        result[t]:=Import(Connection,tmp[t]);

        if Assigned(OnProgress) then
           DoProgress(t*100/n,tmpCancel);

        if tmpCancel then
           break;
      end;

      CleanPrefix(result);

      DoProgress(100,tmpCancel);
    end;
  finally
    tmp.Free;

    if Length(result)=1 then
       if result[0].Name='' then
          result[0].Name:=Engine.GetConnectionName(Connection);
  end;
end;

class function TBIDB.GetItems(const ADefinition:TDataDefinition): TStrings;
var C : TCustomConnection;
    D : TBIDB;
begin
  C:=TBIDB.Engine.CreateConnection(ADefinition);
  try
    C.Open;

    D:=TBIDB.Create(ADefinition);
    try
      result:=D.GetItems(C);
    finally
      D.Free;
    end;
  finally
    C.Free;
  end;
end;

// Return table with information about database items that pass the
// included/excluded filters:
class function TBIDB.IncludedItems(const ADef:TDataDefinition): TDataItem;
var tmp : TStrings;
    t : TLoopInteger;
begin
  tmp:=GetItems(ADef);

  result:=TDataItem.Create(True);
  result.Items.Add('Name',TDataKind.dkText);

  if tmp<>nil then
  try
    result.Resize(tmp.Count);

    for t:=0 to result.Count-1 do
        result.Items[0].TextData[t]:=tmp[t];
  finally
    tmp.Free;
  end;
end;

{ TBIDBImporter }

type
  TBIDBImporter=class(TDataImporter)
  private
    function ImportQuery(const AConnection:TCustomConnection; ASQL:String):TDataItem;
  public
    function Import:TDataArray; override;
    class function Supports(const Kind:TDataDefinitionKind;
                            const ADefinition:TDataDefinition=nil):Boolean; override;
  end;

function TBIDBImporter.ImportQuery(const AConnection:TCustomConnection; ASQL:String):TDataItem;
var Query : TDataSet;
    tmpData : TBIDataSetSource;
begin
  Query:=TBIDB.Engine.CreateQuery(AConnection,ASQL);
  try
    tmpData:=TBIDataSetSource.Create(Definition);
    try
      result:=tmpData.Import(Query);
    finally
      tmpData.Free;
    end;
  finally
    Query.Free;
  end;
end;

function TBIDBImporter.Import:TDataArray;

  function UseMultipleThreads:Boolean;
  begin
    result:=TBIDB.Engine.SupportsParallel and Definition.Parallel;
  end;

var C : TCustomConnection;
    D : TBIDB;
    SQL : String;
    tmp : TDataItem;
    tmpCancel : Boolean;
begin
  result:=nil;

  tmpCancel:=False;
  DoProgress(0,tmpCancel);

  if not tmpCancel then
  begin
    C:=TBIDB.Engine.CreateConnection(Definition);
    try
      C.Open;

      SQL:=Definition.MultiLineText('SQL');

      if SQL='' then
      begin
        D:=TBIDB.Create(Definition,UseMultipleThreads);
        try
          D.OnProgress:=Definition.OnImporting;
          D.OnError:=Definition.OnError;

          result:=D.Import(C);
        finally
          D.Free;
        end;
      end
      else
      begin
        tmp:=ImportQuery(C,SQL);

        if tmp<>nil then
        begin
          SetLength(result,1);
          result[0]:=tmp;
        end;
      end;
    finally
      C.Free;
    end;
  end;

  DoProgress(100,tmpCancel);
end;

class function TBIDBImporter.Supports(const Kind: TDataDefinitionKind;
                                      const ADefinition:TDataDefinition=nil): Boolean;
begin
  result:=Kind=TDataDefinitionKind.Database;
end;

{ TBIDBEngine }

class procedure TBIDBEngine.AddFields(const Fields:TFieldDefs; const AItems:TDataArray);
begin
  TBIDataSetSource.Add(Fields,AItems);
end;

class procedure TBIDBEngine.StartParallel;
begin
end;

class function TBIDBEngine.SupportsParallel: Boolean;
begin
  result:=True;
end;

{ TBIDBExport }

class procedure TBIDBExport.Add(const ADataSet:TDataSet; const AData: TDataItem);

  procedure AddData(const APos:TInteger; const AFields:TFields; const AItems:TDataArray);
  var t : Integer;
      Col : TDataItem;
      Field : TField;
  begin
    for t:=0 to AItems.Count-1 do
    begin
      Col:=AItems[t];
      Field:=AFields[t];

      {$IFNDEF FPC}
      if Col.AsTable then
         AddData(APos,TADTField(Field).Fields,Col.Items.AsArray)
      else
      {$ENDIF}
      if Col.Missing[APos] then
         Field.Clear
      else
      case Col.Kind of
        dkInt32: Field.AsInteger:=Col.Int32Data[APos];
        dkInt64: Field.AsLargeInt:=Col.Int64Data[APos];
       dkSingle: Field.{$IFDEF FPC}AsFloat{$ELSE}AsSingle{$ENDIF}:=Col.SingleData[APos];
       dkDouble: Field.AsFloat:=Col.DoubleData[APos];
     dkExtended: Field.{$IFDEF FPC}AsFloat{$ELSE}AsExtended{$ENDIF}:=Col.ExtendedData[APos];
         dkText: Field.AsString:=Col.TextData[APos];
     dkDateTime: Field.AsDateTime:=Col.DateTimeData[APos];
      dkBoolean: Field.AsBoolean:=Col.BooleanData[APos];
      end;
    end;
  end;

var t : TLoopInteger;
    tmp : TDataArray;
begin
  ADataSet.Open;

  ADataSet.DisableControls;
  try
    AData.Load;

    tmp:=AData.Items.AsArray;

    for t:=0 to AData.Count-1 do
    begin
      ADataSet.Append;
      AddData(t,ADataSet.Fields,tmp);
      ADataSet.Post;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

class function TBIDBExport.From(const AOwner:TComponent; const AData: TDataItem): TDataSet;
begin
  result:=TBIDB.Engine.CreateDataSet(AOwner,AData);

  if IsValidIdent(AData.Name) then
     if (AOwner=nil) or (AOwner.FindComponent(AData.Name)=nil) then
        result.Name:=AData.Name;

  Add(result,AData);
end;

initialization
  TDataImporter.RegisterClass(TBIDBImporter);
  TBIFileImporters.RegisterClass(TBIDB);
finalization
  TBIFileImporters.UnRegisterClass(TBIDB);
  TDataImporter.UnRegisterClass(TBIDBImporter);
  TBIDB.Engine:=nil;
end.

