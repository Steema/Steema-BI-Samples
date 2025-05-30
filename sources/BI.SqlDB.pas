{*********************************************}
{  TeeBI Software Library                     }
{  Lazarus Database Import                    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.SqlDB;

interface

uses
  Classes, Types, DB, BI.DataItem, BI.Persist, BI.DataSource, BI.DB,
  SqlDB;

type
  { TDBSqlDBEngine }

  TDBSqlDBEngine=class(TBIDBEngine)
  protected
    class function CreateConnection(const AOwner:TComponent; const ADriver:String):TSQLConnection; overload; static;
  public
    const
       DBDriverNames:Array[0..7] of String=('SQLite','MySQL','Oracle','Microsoft SQL Server',
           'ODBC','Embarcadero Interbase','Firebird','Postgres');

       DBDrivers:Array[0..7] of String=('SQLite','MySQL','Ora','MSSQL','ODBC','IB','FB','Postgres');

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
    class function Supports(const Extension:String):Boolean; override;
    class function Tester:TBIDBTesterClass; override;
  end;

implementation

{$DEFINE HASDB_IB} // <-- valid for Firebird too
{.$DEFINE HASDB_Oracle}
{$DEFINE HASDB_SQLite}
{$DEFINE HASDB_MSSQL}
{$DEFINE HASDB_MySQL}
{$DEFINE HASDB_Postgres}
{$DEFINE HASDB_ODBC}

{ // Pending:
SybaseASA
SybaseASE
Informix
Db2
}

uses
  SysUtils, BI.Arrays, BI.UI, BI.FPC,

  // Drivers:
  {$IFDEF HASDBEXPR_FB}
  Data.DBXFirebird,
  {$ENDIF}

  {$IFDEF HASDB_IB}
  ibconnection,
  {$ENDIF}

  {$IFDEF HASDB_Oracle}
  oracleconnection,
  {$ENDIF}

  {$IFDEF HASDB_Postgres}
  pqconnection,
  {$ENDIF}

  {$IFDEF HASDB_ODBC}
  odbcconn,
  {$ENDIF}

  {$IFDEF HASDB_SQLite}
  sqlite3conn,
  {$ENDIF}

  {$IFDEF HASDB_MSSQL}
  mssqlconn,
  {$ENDIF}

  {$IFDEF HASDB_MySQL}
  mysql57conn,
  {$ENDIF}

  BI.Languages.English;

{ TDBSqlDBEngine }

class function TDBSqlDBEngine.CreateConnection(const AOwner:TComponent; const ADriver: String): TSQLConnection;
begin
  {$IFDEF HASDB_SQLITE}
  if SameText(ADriver,'SQLite') then
     result:=TSQLite3Connection.Create(AOwner)
  else
  {$ENDIF}

  {$IFDEF HASDB_MYSQL}
  if SameText(ADriver,'MySQL') then
     result:=TMySQL57Connection.Create(AOwner)
  else
  {$ENDIF}

  {$IFDEF HASDB_ORACLE}
  if SameText(ADriver,'Ora') then
     result:=TOracleConnection.Create(AOwner)
  else
  {$ENDIF}

  {$IFDEF HASDB_MSSQL}
  if SameText(ADriver,'MSSQL') then
     result:=TMSSQLConnection.Create(AOwner)
  else
  {$ENDIF}

  {$IFDEF HASDB_ODBC}
  if SameText(ADriver,'ODBC') then
     result:=TODBCConnection.Create(AOwner)
  else
  {$ENDIF}

  {$IFDEF HASDB_IB}
  if SameText(ADriver,'IB') or SameText(ADriver,'FB') then
     result:=TIBConnection.Create(AOwner)
  else
  {$ENDIF}

  {$IFDEF HASDB_POSTGRES}
  if SameText(ADriver,'Postgres') then
     result:=TPQConnection.Create(AOwner)
  else
  {$ENDIF}

     raise EBIException.Create('Error: CreateConnection, driver: '+ADriver+' not supported');
end;

class function TDBSqlDBEngine.CloneConnection(const AConn: TCustomConnection): TCustomConnection;
begin
  result:=AConn;
end;

class function TDBSqlDBEngine.CreateConnection(const Definition: TDataDefinition): TCustomConnection;
var C : TSQLConnection;
begin
  C:=CreateConnection(nil,Definition['DBDriver']);

  C.HostName:=Definition['DBServer'];
  C.Params.Values['Port']:=Definition['DBPort'];
  C.DatabaseName:=Definition['DBDatabase'];
  C.UserName:=Definition['DBUser'];
  C.Password:=TCrypto.Decrypt(Definition['DBPassword']);

  C.LoginPrompt:=Definition.AsBoolean('DBLogin');

  result:=C;
end;

class function TDBSqlDBEngine.CreateDataSet(const AOwner: TComponent; const AData: TDataItem): TDataSet;
begin
  result:=TSQLQuery.Create(AOwner);

  AddFields(result.FieldDefs,AData.Items.AsArray);

  //  TSQLDataSet(result).CreateDataSet;
end;

class function TDBSqlDBEngine.CreateQuery(const AConnection: TCustomConnection; const SQL: String): TDataSet;
var Query : TSQLQuery;
begin
  Query:=TSQLQuery.Create(AConnection.Owner);
  Query.SQLConnection:=AConnection as TSQLConnection;

  Query.SQL.Text:=SQL;

  // Workaround for SqlDB bug, set to False to avoid SqlDB to obtain query indexes.
  // The bug happens when any SQL "from" table name contains spaces (ie: "Order Details")
  Query.UsePrimaryKeyAsKey:=False;

  Query.Open;

  result:=Query;
end;

class function TDBSqlDBEngine.DriverNames: TStringDynArray;
var t : Integer;
begin
  SetLength(result,Length(DBDriverNames));

  for t:=0 to High(DBDriverNames) do
      result[t]:=DBDriverNames[t];
end;

class function TDBSqlDBEngine.DriverToName(const ADriver: String): String;
var t : Integer;
begin
  for t:=0 to High(DBDrivers) do
      if DBDrivers[t]=ADriver then
         Exit(DBDriverNames[t]);

  result:='';
end;

class function TDBSqlDBEngine.GetConnectionName(const AConnection: TCustomConnection): String;
begin
  result:=AConnection.Name;
end;

class function TDBSqlDBEngine.GetDriver(const AIndex: Integer): String;
begin
  result:=DBDrivers[AIndex];
end;

class function TDBSqlDBEngine.GetKeyFieldNames(const AConnection: TCustomConnection; const ATable: String): TStrings;
begin
  result:=TStringList.Create;

  //  if AConnection is TSQLConnection then
  //     TSQLConnection(AConnection).GetKeyFieldNames('','',ATable,'',result);
end;

class function TDBSqlDBEngine.GetSchemas(const AConnection: TCustomConnection): TStrings;
begin
  result:=TStringList.Create;

  if AConnection is TSQLConnection then
     TSQLConnection(AConnection).GetSchemaNames(result);
end;

class function TDBSqlDBEngine.GetTable(const AConnection: TCustomConnection;
const AName: String): TDataSet;
begin
  if Pos(' ',AName)>0 then
     result:=CreateQuery(AConnection,'select * from "'+AName+'"')
  else
     result:=CreateQuery(AConnection,'select * from '+AName);
end;

class function TDBSqlDBEngine.GetItemNames(const AConnection: TCustomConnection;
                       const IncludeSystem,IncludeViews:Boolean): TStrings;

  function DoGetItems(const AConnection:TSQLConnection):TStrings;
  var
    tmpSchemas : TStrings;

    procedure AppendItems(const AResult:TStrings; const IsSystem:Boolean);

      procedure DoAppend(const AList:TStrings; const APrefix:String);
      var t : Integer;
      begin
        for t:=0 to AList.Count-1 do
            AResult.Add(APrefix+AList[t]);
      end;

    var tmpList : TStrings;
        t : Integer;

        {$IFDEF HASDB_SQLITE}
        tmp : Integer;
        {$ENDIF}
    begin
      if tmpSchemas.Count=0 then
      begin
        tmpList:=TStringList.Create;
        try
          {
          if IncludeViews then
             AConnection.TableScope:=[tsTable,tsView]
          else
             AConnection.TableScope:=[tsTable];
          }

          AConnection.GetTableNames(tmpList,IsSystem);

          {$IFDEF HASDB_SQLITE}
          // SQLite workaround, do not include "sqlite_sequence" system table that
          // is (wrongly?) returned by GetTableNames

          if (not IsSystem) and (AConnection is TSQLite3Connection) then
          begin
            tmp:=tmpList.IndexOf('sqlite_sequence');

            if tmp<>-1 then
               tmpList.Delete(tmp);
          end;
          {$ENDIF}

          DoAppend(tmpList,'');
        finally
          tmpList.Free;
        end;
      end
      else
      for t:=0 to tmpSchemas.Count-1 do
          if (tmpSchemas[t]<>'sys') then // !!??? dbExpress bug, cannot handle "sys" schema ?
          begin
            tmpList:=TStringList.Create;
            try
              AConnection.GetTableNames(tmpList,IsSystem);
              DoAppend(tmpList,tmpSchemas[t]+'.');
            finally
              tmpList.Free;
            end;
          end;
    end;

  begin
    result:=TStringList.Create;

    tmpSchemas:=TStringList.Create;
    try
      if AConnection.Transaction=nil then
      begin
        AConnection.Transaction:=TSQLTransaction.Create(nil);
        AConnection.Transaction.DataBase:=AConnection;
      end;

      AConnection.StartTransaction;
      try
        try
          AConnection.GetSchemaNames(tmpSchemas);

        except
          on EDatabaseError do ;
        end;

      finally
        AConnection.EndTransaction;
      end;

      if IncludeSystem then
         AppendItems(result,True);

      AppendItems(result,False);
    finally
      tmpSchemas.Free;
    end;
  end;

begin
  if AConnection is TSQLConnection then
     result:=DoGetItems(TSQLConnection(AConnection))
  else
     raise EBIException.Create(BIMsg_DB_SqlExpressWrongConnection);
end;

class procedure TDBSqlDBEngine.GuessForeignKeys(const AName: String;
const Table: TDataSet; const AData: TDataItem; const Source: TBISource);
begin
// ??
end;

class function TDBSqlDBEngine.ImportFile(const Source: TBIDB;
const AFileName: String): TDataArray;
{$IFDEF HASDB_SQLITE}
var tmp : String;
    C : TSQLite3Connection;
{$ENDIF}
begin
  {$IFDEF HASDB_SQLITE}
  tmp:=TPath.GetExtension(AFileName);

  if SameText(tmp,'.sdb') then
  begin
    C:=TSQLite3Connection.Create(nil);
    try
      C.DatabaseName:=AFileName;
      C.LoginPrompt:=False;

      result:=Source.Import(C);
    finally
      C.Free;
    end;
  end;
  {$ENDIF}
end;

class function TDBSqlDBEngine.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('Microsoft Access files','*.mdb;*.accdb');
  result.Add('dBase files','*.dbf');
  result.Add('Paradox files','*.db');
  result.Add('SQLite files','*.sdb');
  result.Add('Interbase files','*.gdb');
  result.Add('Firebird files','*.fdb');
end;

class function TDBSqlDBEngine.Supports(const Extension:String):Boolean;
begin
  result:=SameText(Extension,'.mdb') or  // Access
          SameText(Extension,'.dbf') or  // DBase
          SameText(Extension,'.accdb') or  // Access
          SameText(Extension,'.db') or  // Paradox
          SameText(Extension,'.sdb') or  // SQLite
          SameText(Extension,'.gdb') or  // Interbase http://www.firebirdfaq.org/faq353/
          SameText(Extension,'.fdb');  // Firebird
end;

class function TDBSqlDBEngine.Tester: TBIDBTesterClass;
begin
  result:=nil;
end;

initialization
  TBIDB.Engine:=TDBSqlDBEngine.Create;
finalization
  TBIDB.Engine:=nil;
end.

