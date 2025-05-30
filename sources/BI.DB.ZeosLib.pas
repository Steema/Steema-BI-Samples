{*********************************************}
{  TeeBI Software Library                     }
{  ZeosLib Database data import and export    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.ZeosLib;

(*
  ZeosLib, database drivers for Delphi

  https://sourceforge.net/projects/zeoslib
  http://zeoslib.sourceforge.net/index.php


*)

interface

uses
  System.Classes, System.Types, Data.DB, BI.DataItem, BI.DB, BI.DataSource,
  BI.Persist, ZClasses, ZDbcIntfs, ZCompatibility, ZDbcConnection,
  ZAbstractConnection;

type
  TDbcConnection=ZDbcConnection.TZAbstractConnection;
  TZAbstractConnectionClass=class of TDbcConnection;

  TBIZeosLib=class(TBIItemsSource)
  private
    class procedure Append(const AStrings:TStrings;
                           const AResultSet:IZResultSet;
                           const AName:String); static;

    class function ConnectionClass(const ADriver:String):TZAbstractConnectionClass; static;
    class function CreateConnection(const Definition:TDataDefinition):TDbcConnection; static;

    class function CreateQuery(const AConnection:TZAbstractConnection; const SQL:String):TDataSet; static;

    class function GetConnectionName(const AConnection:TDbcConnection):String; static;

    class function GetKeyFieldNames(const AConnection:TDbcConnection;
                                    const ATable:String):TStrings; static;

    class function GetSchemas(const AConnection: TDbcConnection): TStrings; static;

    class function GetItemNames(const AConnection:TDbcConnection;
                                const IncludeSystem,IncludeViews:Boolean):TStrings;

    class function GetTable(const AConnection:TZAbstractConnection; const AName:String):TDataSet; static;

    class function QueryTable(const AConnection:TDbcConnection; const ATable:String):IZResultSet; static;
  public
    class function DriverNames: TStringDynArray; static;
    class function FileFilter: TFileFilters; static;

    class function Import(const AConnection:TDbcConnection;
                          const IncludeViews:Boolean=False):TDataItem; overload; static;

    class function Import(const AFileName,AProtocol:String;
                          const IncludeViews:Boolean=False):TDataItem; overload; static;
    class function Import(const AResultSet:IZResultSet):TDataItem; overload; static;

    class function Supports(const Extension:String):Boolean; static;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, BI.UI, BI.Arrays,

  ZDataset, ZURL,

  ZDbcMySql, ZDbcInterbase6, ZDbcPostgreSql, ZDbcDBLib, ZDbcOracle, ZDbcSQLite,
  ZDbcASA, ZDbcAdo;

const
   DBDriverNames:Array of String=['MySQL','Interbase','Postgres','MSSQL','Sybase',
     'Oracle','SQLite','ASA','ADO'];

type
  TDataAccess=class(TDataItem);
  TBISourceAccess=class(TBISource);

(*
class procedure TBIZeosLib.GuessForeignKeys(const AName:String; const Table:TDataSet; const AData:TDataItem; const Source:TBISource);

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

  {
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
  }

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
*)

class procedure TBIZeosLib.Append(const AStrings:TStrings;
                                        const AResultSet:IZResultSet;
                                        const AName:String);
begin
  with AResultSet do
  begin
    while Next do
          AStrings.Add(GetStringByName(AName));

    Close;
  end;
end;

class function TBIZeosLib.GetKeyFieldNames(
      const AConnection:TDbcConnection;
      const ATable:String):TStrings;
begin
  result:=TStringList.Create;

  // Note: MsAccess ACE/JET cannot return key field names for "Views" or "Queries".
  // It only works with normal Tables.

  Append(result,AConnection.GetMetadata.GetPrimaryKeys('','',ATable),'COLUMN_NAME');
end;

class function TBIZeosLib.GetSchemas(const AConnection: TDbcConnection): TStrings;
begin
  result:=TStringList.Create;
  Append(result,AConnection.GetMetadata.GetSchemas,'SCHEMA_NAME');
end;

class function TBIZeosLib.GetItemNames(const AConnection:TDbcConnection;
                                       const IncludeSystem,IncludeViews:Boolean):TStrings;

  function Kinds:TStringDynArray;
  begin
    // Synonyms, Local, Temp
    if IncludeViews then
    begin
      SetLength(result,2);
      result[0]:='TABLE';
      result[1]:='VIEW';
    end
    else
    begin
      SetLength(result,1);
      result[0]:='TABLE';
    end;
  end;

begin
  result:=TStringList.Create;
  Append(result,AConnection.GetMetadata.GetTables(AConnection.GetCatalog,'','',Kinds),'TABLE_NAME');
end;

class function TBIZeosLib.GetTable(const AConnection:TZAbstractConnection; const AName:String):TDataSet;
begin
  result:=TZReadOnlyQuery.Create(nil);

//  TZReadOnlyQuery(result).Connection:=AConnection;
  TZReadOnlyQuery(result).SQL.Text:='select * from '+AName;
end;

class function TBIZeosLib.Import(
  const AConnection: TDbcConnection;
  const IncludeViews:Boolean): TDataItem;
var tmp : TStrings;
    t : Integer;
begin
  tmp:=TBIZeosLib.GetItemNames(AConnection,False,IncludeViews);
  try
    result:=TDataItem.Create;

    try
      for t:=0 to tmp.Count-1 do
          result.Items.Add(TBIZeosLib.Import(TBIZeosLib.QueryTable(AConnection,tmp[t])));
    except
      on Exception do
      begin
        result.Free;
        raise;
      end;
    end;
  finally
    tmp.Free;
  end;
end;

class function TBIZeosLib.Import(const AResultSet: IZResultSet): TDataItem;
begin
  result:=TDataItem.Create(True);
  result.Name:=AResultSet.GetCursorName;

  if AResultSet.First then
  begin
    while AResultSet.Next do
    begin

    end;
  end;
end;

class function TBIZeosLib.QueryTable(
  const AConnection: TDbcConnection;
  const ATable: String): IZResultSet;
var tmp : IZPreparedStatement;
begin
  tmp:=AConnection.PrepareStatement('select * from '+ATable);
  result:=tmp.ExecuteQueryPrepared;
end;

class function TBIZeosLib.CreateQuery(const AConnection:TZAbstractConnection; const SQL:String):TDataSet;
var Query : TZReadOnlyQuery;
begin
  Query:=TZReadOnlyQuery.Create(nil);
  try
    Query.Connection:=AConnection;
    Query.SQL.Text:=SQL;
    Query.Open;

    result:=Query;
  except
    on Exception do
    begin
      Query.Free; // release memory (avoid leak)
      raise;
    end;
  end;
end;

class function TBIZeosLib.DriverNames: TStringDynArray;
var t : Integer;
begin
  SetLength(result,Length(DBDriverNames));

  for t:=0 to High(DBDriverNames) do
      result[t]:=DBDriverNames[t];
end;

class function TBIZeosLib.Import(const AFileName,AProtocol:String;
                                 const IncludeViews:Boolean):TDataItem;

  function ConnectionFromFile(const ADriver:String):TDbcConnection;
  var tmpURL : TZURL;
  begin
    tmpURL:=TZURL.Create;
    try
      tmpURL.Protocol:=AProtocol;
      result:=ConnectionClass(ADriver).Create(tmpURL);
    finally
      tmpURL.Free;
    end;
  end;

var tmp : String;
    C : TDbcConnection;
begin
  tmp:=TPath.GetExtension(AFileName);

  if SameText(tmp,'.sdb') then
     C:=ConnectionFromFile('SQLite')
  else
  if SameText(tmp,'.fdb') then
     C:=ConnectionFromFile('Interbase')
  else
  if SameText(tmp,'.dbf') then // dBase
     C:=ConnectionFromFile('ADS')
  else
  if SameText(tmp,'.mdb') or SameText(tmp,'.accdb') then
     C:=ConnectionFromFile('MSAcc')
  else
     raise EBIException.Create('Error Unknown ZeosLib file cannot be imported: '+AFileName);

  try
    result:=TBIZeosLib.Import(C,IncludeViews);
  finally
    C.Free;
  end;
end;

class function TBIZeosLib.ConnectionClass(const ADriver:String):TZAbstractConnectionClass;
begin
  if SameText(ADriver,'MySQL') then
     result:=TZMySQLConnection
  else
  if SameText(ADriver,'Postgres') then
     result:=TZPostgreSQLConnection
  else
  if SameText(ADriver,'Interbase') then
     result:=TZInterbase6Connection
  else
  if SameText(ADriver,'Oracle') then
     result:=TZOracleConnection
  else
  if SameText(ADriver,'ASA') then
     result:=TZASAConnection
  else
  if SameText(ADriver,'ADO') then
     result:=TZAdoConnection
  else
  if SameText(ADriver,'MSSQL') or SameText(ADriver,'Sybase') then
     result:=TZDBLibConnection
  else
     raise EBIException.Create('Error: Unknown ZeosLib driver: '+ADriver);
end;

class function TBIZeosLib.GetConnectionName(const AConnection:TDbcConnection):String;
begin
  result:=AConnection.GetDescription;
end;

class function TBIZeosLib.CreateConnection(const Definition:TDataDefinition):TDbcConnection;

  function IsFileBased(const ADriver:String):Boolean;
  begin
    result:=SameText(ADriver,'SQLite') or
            SameText(ADriver,'MSAcc');
  end;

  function URLFrom(const ADefinition:TDataDefinition):TZURL;
  var tmpPort : String;
  begin
    result:=TZURL.Create;

    result.HostName:=Trim(Definition['DBServer']);
    tmpPort:=Definition['DBPort'];

    if tmpPort<>'' then
       result.Port:=StrToInt(tmpPort);

    result.UserName:=Definition['DBUser'];
    result.Password:=TCrypto.Decrypt(Definition['DBPassword']);

    result.Database:=Definition['DBDatabase'];

    if result.Database<>'' then
       if IsFileBased(Definition['DBDriver']) then
          if (result.HostName='') and TPath.IsRelativePath(result.Database) then
             if Definition.Store='' then
             begin
               if Definition.FileName<>'' then
                  result.Database:=TPath.Combine(TPath.GetDirectoryName(Definition.FileName),result.Database);
             end
             else
                result.Database:=TStore.FullPath(Definition.Store,result.Database);
  end;

var tmp : TZURL;
begin
  tmp:=URLFrom(Definition);
  try
    result:=ConnectionClass(Definition['DBDriver']).Create(tmp);
    result.SetReadOnly(True);
  finally
    tmp.Free;
  end;
end;

class function TBIZeosLib.FileFilter: TFileFilters;
begin
  result:=nil;

  result.Add('Microsoft Access files','*.mdb;*.accdb');
  result.Add('dBase files','*.dbf');
  result.Add('Paradox files','*.db');
  result.Add('SQLite files','*.sdb');
  result.Add('Interbase files','*.gdb');
  result.Add('Firebird files','*.fdb');
end;

class function TBIZeosLib.Supports(const Extension:String):Boolean;
begin
  result:=SameText(Extension,'.mdb') or  // Access
          SameText(Extension,'.dbf') or  // DBase
          SameText(Extension,'.accdb') or  // Access
          SameText(Extension,'.db') or  // Paradox
          SameText(Extension,'.sdb') or  // SQLite
          SameText(Extension,'.gdb') or  // Interbase http://www.firebirdfaq.org/faq353/
          SameText(Extension,'.fdb');  // Firebird
end;

end.
