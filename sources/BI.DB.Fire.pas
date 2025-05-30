{*********************************************}
{  TeeBI Software Library                     }
{  FireDAC Database data import and export    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.Fire;

interface

(*
  Just include this unit in the "uses" of any unit, to enable FireDAC as the
  "Engine" for all TeeBI database operations.

  Note: Better to not include BI.DB.ZeosLib or any other unit that
        also provides a different "Engine".

  When using multiple engines (optionally),
  the current one must be manually selected like:

  uses BI.DB;
    TBIDB.Engine:=TDBFireDACEngine.Create; <--- the desired engine class

*)

uses
  System.Classes, System.Types, Data.DB, BI.DataItem, BI.DB, BI.DataSource,
  BI.Persist, FireDAC.Comp.Client;

type
  TDBFireDACEngine=class(TBIDBEngine)
  private
    class procedure SetQueryParameters(const Query:TFDQuery); static;
  public
    class function CloneConnection(const AConn:TCustomConnection): TCustomConnection; override;
    class function CreateConnection(const Definition:TDataDefinition):TCustomConnection; override;
    class function CreateDataSet(const AOwner:TComponent; const AData:TDataItem):TDataSet; override;
    class function CreateQuery(const AConnection:TCustomConnection; const SQL:String):TDataSet; override;
    class function DriverNames:TStringDynArray; override;
    class function DriverToName(const ADriver:String):String; override;
    class function FileFilter: TFileFilters; override;
    class function GetConnectionName(const AConnection:TCustomConnection):String; override;
    class function GetDriver(const AIndex:Integer):String; override;
    class function GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings; override;
    class function GetSchemas(const AConnection:TCustomConnection):TStrings;
    class function GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet; override;
    class function GetItemNames(const AConnection:TCustomConnection;
                                const IncludeSystem,IncludeViews:Boolean):TStrings; override;
    class procedure GuessForeignKeys(const AName:String; const Table:TDataSet; const AData:TDataItem; const Source:TBISource); override;
    class function ImportFile(const Source:TBIDB; const AFileName:String):TDataArray; override;
    class procedure StartParallel; override;
    class function Supports(const Extension:String):Boolean; override;
    class function Tester:TBIDBTesterClass; override;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, BI.UI, BI.Arrays,
  FireDAC.Comp.UI, FireDAC.UI.Intf, FireDAC.Stan.Intf,
  FireDAC.Stan.Error, FireDAC.Phys.Intf, FireDAC.Stan.Option,
  BI.DB.Fire.AllDrivers,
  FireDAC.Comp.DataSet;

{$IF CompilerVersion>=30}
{$DEFINE HASFIREDAC_MONGODB}
{$ENDIF}

const
   MSacc='MSAcc';

   {$IF CompilerVersion>27}
   DBDriverNames:Array of String=['SQLite','MySQL','Oracle','Microsoft SQL Server',
       'ODBC','Microsoft Access','Embarcadero Interbase','MariaDB','dBase'
            {$IFDEF HASFIREDAC_MONGODB},'MongoDB'{$ENDIF},'PostGreSQL'];
   DBDrivers:Array of String=['SQLite','MySQL','Ora','MSSQL','ODBC',MSAcc,'IB','MySQL','ADS'
            {$IFDEF HASFIREDAC_MONGODB},'Mongo'{$ENDIF},'PG'];  // 'IBLite' ?
   {$ELSE}
   DBDriverNames:Array[0..7] of String=('SQLite','MySQL','Oracle','Microsoft SQL Server',
       'ODBC','Microsoft Access','Embarcadero Interbase','MariaDB');
   DBDrivers:Array[0..7] of String=('SQLite','MySQL','Ora','MSSQL','ODBC',MSAcc,'IB','MySQL');  // 'IBLite' ?
   {$ENDIF}

type
  TDataAccess=class(TDataItem);
  TBISourceAccess=class(TBISource);

class procedure TDBFireDACEngine.GuessForeignKeys(const AName:String; const Table:TDataSet; const AData:TDataItem; const Source:TBISource);

  procedure SetMaster(const ADetail,AMasterTable,AMasterField:String);
  var tmp : TDataItem;
  begin
    tmp:=AData.Items.Find(ADetail);

    if tmp=nil then
      // ??
    else
      // "..|.." means database parent of tmp
      TDataAccess(tmp).IMaster.Origin:='..|..|'+AMasterTable+'|'+AMasterField;
  end;

  procedure FindForeignOf(const AFields:TFDMetaInfoQuery; const AKey,ATable:String);
  var LocalColumn,
      ForeignColumn : TStringField;
  begin
    AFields.Close;

    AFields.BaseObjectName:=AName;
    AFields.ObjectName:=AKey;
    AFields.MetaInfoKind:=TFDPhysMetaInfoKind.mkForeignKeyFields;
    AFields.Open;

    LocalColumn:=AFields.Fields.FindField('COLUMN_NAME') as TStringField;
    ForeignColumn:=AFields.Fields.FindField('PKEY_COLUMN_NAME') as TStringField;

    AFields.First;

    while not AFields.Eof do
    begin
      SetMaster(LocalColumn.AsString,ATable,ForeignColumn.AsString);

      AFields.Next;
    end;
  end;

  // Special case for Microsoft Access database *.mdb or *.accdb
  // Foreign-keys metadata can be obtained by querying the special system
  // table "MSysRelationships", provided it has Read permission
  // (permission can be manually added using Access "Users and Security" dialog)
  procedure TryAccessSystem(const AConnection:TFDCustomConnection);
  const
    RelationShips='MSysRelationships';
    DetailTable='szObject';
    DetailField='szColumn';
    MasterTable='szReferencedObject';
    MasterField='szReferencedColumn';

  var tmp : TDataSet;
      tmpSQL,
      tmpName : String;

      tmpDetail,
      tmpMasterTable,
      tmpMasterField : TStringField;

  begin
    tmpName:=QuotedStr(AName);

    tmpSQL:='select '+DetailField+','+DetailTable+','+MasterTable+','+MasterField+
            ' from '+RelationShips+' where '+DetailTable+' = '+tmpName;

    tmp:=CreateQuery(AConnection,tmpSQL);
    try
      tmpDetail:=tmp.Fields.FindField(DetailField) as TStringField;
      tmpMasterTable:=tmp.Fields.FindField(MasterTable) as TStringField;
      tmpMasterField:=tmp.Fields.FindField(MasterField) as TStringField;

      tmp.First;

      while not tmp.Eof do
      begin
        SetMaster(tmpDetail.AsString,tmpMasterTable.AsString,tmpMasterField.AsString);
        tmp.Next;
      end;

    finally
      tmp.Free;
    end;
  end;

var Meta : TFDMetaInfoQuery;
    Fields : TFDMetaInfoQuery;

    ForeignSchema,
    ForeignKey,
    ForeignTable : TStringField;

    tmpSchema : String;
begin

  // Note: MsAccess is not capable of returning foreign-keys metadata.
  // Output is always empty.

  Meta:=TFDMetaInfoQuery.Create(nil);
  try
    Meta.MetaInfoKind:=TFDPhysMetaInfoKind.mkForeignKeys;
    Meta.ObjectName:=AName;
    Meta.Connection:=(Table as TFDRdbmsDataSet).Connection;

    try
      Meta.Open;

      ForeignSchema:=Meta.Fields.FindField('PKEY_SCHEMA_NAME') as TStringField;
      ForeignTable:=Meta.Fields.FindField('PKEY_TABLE_NAME') as TStringField;
      ForeignKey:=Meta.Fields.FindField('FKEY_NAME') as TStringField;

      Fields:=TFDMetaInfoQuery.Create(nil);
      try
        Fields.Connection:=Meta.Connection;

        Meta.First;

        while not Meta.Eof do
        begin
          tmpSchema:=ForeignSchema.AsString;

          if tmpSchema<>'' then
             tmpSchema:=tmpSchema+'.';

          FindForeignOf(Fields,ForeignKey.AsString,tmpSchema+ForeignTable.AsString);

          Meta.Next;
        end;

        Meta.Close;

        // Special case for Microsoft Access
        if SameText(Meta.Connection.DriverName,MSAcc) and (Meta.RecordCount=0) then
           TryAccessSystem(Meta.Connection);

      finally
        Fields.Free;
      end;
    except
      on E:Exception do
      begin
        if not TBISourceAccess(Source).CallOnError(E.Message) then
           raise;
      end;
    end;
  finally
    Meta.Free;
  end;
end;

class function TDBFireDACEngine.GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings;
begin
  result:=TStringList.Create;

  // Note: MsAccess ACE/JET cannot return key field names for "Views" or "Queries".
  // It only works with normal Tables.

  if AConnection is TFDCustomConnection then
     TFDCustomConnection(AConnection).GetKeyFieldNames('','',ATable,'',result);
end;

class function TDBFireDACEngine.CloneConnection(const AConn:TCustomConnection): TCustomConnection;
begin
  {$IF CompilerVersion<=28}
  result:=TFDCustomConnectionClass(AConn.ClassType).Create(nil);
  result.Assign(AConn);
  {$ELSE}
  result:=TFDCustomConnection(AConn).CloneConnection;
  {$ENDIF}
end;

function CanGetMetaData(const Connection:TCustomConnection):Boolean;
begin
  result:=Connection is TFDCustomConnection;

  if result then
     if SameText(TFDCustomConnection(Connection).DriverName,'Mongo') then
        result:=False;  // Capability not supported
end;

class function TDBFireDACEngine.GetSchemas(const AConnection: TCustomConnection): TStrings;
begin
  result:=TStringList.Create;

  if CanGetMetaData(AConnection) then
     TFDCustomConnection(AConnection).GetSchemaNames(TFDCustomConnection(AConnection).ConnectionName,'',result);
end;

class function TDBFireDACEngine.GetItemNames(const AConnection:TCustomConnection;
                                             const IncludeSystem,IncludeViews:Boolean):TStrings;

  function Scopes:TFDPhysObjectScopes;
  begin
    if IncludeSystem then
       result:=[osMy,osOther,osSystem]
    else
       result:=[osMy,osOther];
  end;

  function Kinds:TFDPhysTableKinds;
  begin
    // Synonyms, Local, Temp

    if IncludeViews then
       result:=[tkTable,tkView]
    else
       result:=[tkTable];
  end;

begin
  result:=TStringList.Create;

  try
    if CanGetMetaData(AConnection) then
       TFDCustomConnection(AConnection).GetTableNames('','','',result,Scopes,Kinds);
  except
    on Exception do
    begin
      result.Free;
      raise;
    end;
  end;
end;

class function TDBFireDACEngine.GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet;
begin
{ // Slow. Better use a Query instead of a Table:
  result:=TFDTable.Create(AConnection.Owner);
  TFDTable(result).Connection:=AConnection as TFDCustomConnection;
  TFDTable(result).TableName:=AName;
  result.Open;
}
  result:=CreateQuery(AConnection,'select * from '+AName);
end;

class procedure TDBFireDACEngine.SetQueryParameters(const Query:TFDQuery);
begin
  (* Tests:
  Query.IndexesActive:=False;
  *)
  //Query.ObjectView:=False;
  (*
  // Query.FetchOptions.Items:=[];
  Query.FetchOptions.CursorKind:=TFDCursorKind.ckAutomatic;
  Query.FetchOptions.Unidirectional:=True;
  Query.FetchOptions.Mode:=fmOnDemand;
  // Query.ResourceOptions.CmdExecMode:=amNonBlocking;
  *)

  Query.FetchOptions.CursorKind:=TFDCursorKind.ckForwardOnly;
  Query.FetchOptions.Mode:=TFDFetchMode.fmOnDemand; // fmAll

  // RX10: Stack Overflow in FetchBlobs:
  Query.FetchOptions.Unidirectional:=True;
//  Query.FetchOptions.Mode:=fmManual;
  Query.FetchOptions.Items:=Query.FetchOptions.Items-[fiBlobs,fiDetails,fiMeta];

  Query.UpdateOptions.ReadOnly:=True;
  Query.UpdateOptions.RequestLive:=False;
  // Query.FetchOptions.RowsetSize:=1024;

  // Prevent write to readonly query:
  // Query.ResourceOptions.CmdExecMode:=amAsync;
end;

class function TDBFireDACEngine.CreateQuery(const AConnection:TCustomConnection; const SQL:String):TDataSet;
var Query : TFDQuery;
begin
  Query:=TFDQuery.Create(AConnection.Owner);
  try
    Query.Connection:=AConnection as TFDCustomConnection;

    SetQueryParameters(Query);

    Query.Open(SQL);

    // Query.RefreshMetadata;

    result:=Query;
  except
    on Exception do
    begin
      Query.Free; // release memory (avoid leak)
      raise;
    end;
  end;
end;

class function TDBFireDACEngine.DriverNames: TStringDynArray;
var t : Integer;
begin
  SetLength(result,Length(DBDriverNames));

  for t:=0 to High(DBDriverNames) do
      result[t]:=DBDriverNames[t];
end;

class function TDBFireDACEngine.DriverToName(const ADriver:String):String;
var t : Integer;
begin
  for t:=0 to High(DBDrivers) do
      if DBDrivers[t]=ADriver then
         Exit(DBDriverNames[t]);

  result:='';
end;

class function TDBFireDACEngine.ImportFile(const Source:TBIDB; const AFileName:String):TDataArray;

  function ConnectionFromFile(const ADriver:String):TFDConnection;
  begin
    result:=TFDConnection.Create(nil);
    result.DriverName:=ADriver;
    result.Params.Add('Database='+AFileName);
    result.LoginPrompt:=False;
  end;

var tmp : String;
    C : TFDConnection;
begin
  tmp:=TPath.GetExtension(AFileName);

  if SameText(tmp,'.sdb') then
     C:=ConnectionFromFile('SQLite')
  else
  if SameText(tmp,'.dbf') then // dBase
     C:=ConnectionFromFile('ADS')
  else
  if SameText(tmp,'.mdb') or SameText(tmp,'.accdb') then
     C:=ConnectionFromFile(MSAcc)
  else
     raise EBIException.Create('Error Unknown FireDAC file cannot be imported: '+AFileName);

  try
    result:=Source.Import(C);
  finally
    C.Free;
  end;
end;

class function TDBFireDACEngine.GetConnectionName(const AConnection:TCustomConnection):String;
begin
  if AConnection is TFDCustomConnection then
     result:=TFDCustomConnection(AConnection).ConnectionName
  else
     result:=AConnection.Name;
end;

class function TDBFireDACEngine.GetDriver(const AIndex: Integer): String;
begin
  if (AIndex>=Low(DBDrivers)) and (AIndex<=High(DBDrivers)) then
     result:=DBDrivers[AIndex]
  else
     result:='?';
end;

class function TDBFireDACEngine.CreateConnection(const Definition:TDataDefinition):TCustomConnection;

  function IsFileBased(const ADriver:String):Boolean;
  begin
    result:=SameText(ADriver,'SQLite') or
            SameText(ADriver,'ODBC') or
            SameText(ADriver,MSAcc);
  end;

var C : TFDConnection;
    tmpServer,
    tmpPort,
    tmpData : String;
begin
  C:=TFDConnection.Create(nil);

  C.DriverName:=Definition['DBDriver'];

  tmpServer:=Trim(Definition['DBServer']);

  C.Params.Add('Server='+tmpServer);

  tmpPort:=Definition['DBPort'];

  if tmpPort<>'' then
     C.Params.Add('Port='+tmpPort);

  tmpData:=Definition['DBDatabase'];

  if tmpData<>'' then
  begin
    if IsFileBased(C.DriverName) then
       if (tmpServer='') and TPath.IsRelativePath(tmpData) then
           if Definition.Store='' then
           begin
             if Definition.FileName<>'' then
                tmpData:=TPath.Combine(TPath.GetDirectoryName(Definition.FileName),tmpData);
           end
           else
              tmpData:=TStore.FullPath(Definition.Store,tmpData);

    C.Params.Add('Database='+tmpData);
  end;

  C.Params.Add('User_Name='+Definition['DBUser']);
  C.Params.Add('Password='+TCrypto.Decrypt(Definition['DBPassword']));

  C.LoginPrompt:=Definition.AsBoolean('DBLogin');

  C.FetchOptions.Items:=[fiMeta];
  C.FetchOptions.RowsetSize:=1000;
  C.ResourceOptions.SilentMode:=True;

  result:=C;
end;

class function TDBFireDACEngine.CreateDataSet(const AOwner:TComponent; const AData:TDataItem):TDataSet;
begin
  result:=TFDMemTable.Create(AOwner);

  AddFields(result.FieldDefs,AData.Items.AsArray);

  TFDMemTable(result).CreateDataSet;
end;

class procedure TDBFireDACEngine.StartParallel;
begin
  inherited;
  FDManager.Active:=True;
end;

class function TDBFireDACEngine.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('Microsoft Access files','*.mdb;*.accdb');
  result.Add('dBase files','*.dbf');
  result.Add('Paradox files','*.db');
  result.Add('SQLite files','*.sdb');
  result.Add('Interbase files','*.gdb');
  result.Add('Firebird files','*.fdb');
end;

class function TDBFireDACEngine.Supports(const Extension:String):Boolean;
begin
  result:=SameText(Extension,'.mdb') or  // Access
          SameText(Extension,'.dbf') or  // DBase
          SameText(Extension,'.accdb') or  // Access
          SameText(Extension,'.db') or  // Paradox
          SameText(Extension,'.sdb') or  // SQLite
          SameText(Extension,'.gdb') or  // Interbase http://www.firebirdfaq.org/faq353/
          SameText(Extension,'.fdb');  // Firebird
end;

class function TDBFireDACEngine.Tester: TBIDBTesterClass;
begin
  result:=TBIDBFireDACTester;
end;

initialization
  TBIDB.Engine:=TDBFireDACEngine.Create;
finalization
  TBIDB.Engine:=nil;
end.
