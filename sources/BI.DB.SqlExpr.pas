{*********************************************}
{  TeeBI Software Library                     }
{  SqlExpress Database import and export      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.SqlExpr;

interface

uses
  System.Classes, System.Types, Data.DB, BI.DataItem, BI.Persist,
  BI.DataSource, BI.DB;

type
  TDBSqlExprEngine=class(TBIDBEngine)
  private
    class function DoCreateConnection(const ADriver,AServer,APort,
                                            ADatabase,AUser,APassword:String;
                                      const LoginPrompt:Boolean):TCustomConnection;
  public
    const
       DBDriverNames:Array[0..8] of String=('SQLite','MySQL','Oracle','Microsoft SQL Server',
           'ODBC','Microsoft Access','Embarcadero Interbase','Firebird','MariaDB');

       DBDrivers:Array[0..8] of String=('SQLite','MySQL','Ora','MSSQL','ODBC','ODBC','IB','FB','MySQL');  // 'IBLite' ?

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

{$DEFINE HASDBEXPR_FB}
{$DEFINE HASDBEXPR_IB}
{$DEFINE HASDBEXPR_Oracle}
{$DEFINE HASDBEXPR_SQLite}
{$DEFINE HASDBEXPR_MSSQL}
{$DEFINE HASDBEXPR_MySQL}
{$DEFINE HASDBEXPR_ODBC}

{ // Pending:
SybaseASA
SybaseASE
Informix
Db2
}

uses
  Data.SqlExpr, System.IOUtils, System.SysUtils, BI.Arrays, BI.UI,

  // Drivers:
  {$IFDEF HASDBEXPR_FB}
  Data.DBXFirebird,
  {$ENDIF}

  {$IFDEF HASDBEXPR_IB}
  Data.DBXInterbase,
  {$ENDIF}

  {$IFDEF HASDBEXPR_Oracle}
  Data.DBXOracle,
  {$ENDIF}

  {$IFDEF HASDBEXPR_SQLite}
  Data.DBXSQLite,
  {$ENDIF}

  {$IFDEF HASDBEXPR_MSSQL}
  Data.DBXMSSQL,
  {$ENDIF}

  {$IFDEF HASDBEXPR_MySQL}
  Data.DBXMySQL,
  {$ENDIF}

  {$IFDEF HASDBEXPR_ODBC}
  Data.DBXODBC,
  {$ENDIF}

  BI.Languages.English;

{ TDBSqlExprEngine }

class function TDBSqlExprEngine.CloneConnection(
  const AConn: TCustomConnection): TCustomConnection;
begin
  result:=AConn;
end;

class function TDBSqlExprEngine.DoCreateConnection(
               const ADriver,AServer,APort,ADatabase,AUser,APassword:String;
               const LoginPrompt:Boolean):TCustomConnection;
var C : TSQLConnection;
begin
  C:=TSQLConnection.Create(nil);

  C.DriverName:=ADriver;

  C.Params.Values['HostName']:=AServer;
  C.Params.Values['Port']:=APort;
  C.Params.Values['Database']:=ADatabase;
  C.Params.Values['User_Name']:=AUser;
  C.Params.Values['Password']:=APassword;

  C.LoginPrompt:=LoginPrompt;

  result:=C;
end;

class function TDBSqlExprEngine.CreateConnection(
  const Definition: TDataDefinition): TCustomConnection;
begin
  result:=DoCreateConnection(Definition['DBDriver'],
                             Definition['DBServer'],
                             Definition['DBPort'],
                             Definition['DBDatabase'],
                             Definition['DBUser'],
                             TCrypto.Decrypt(Definition['DBPassword']),
                             Definition.AsBoolean('DBLogin'));
end;

class function TDBSqlExprEngine.CreateDataSet(const AOwner: TComponent;
  const AData: TDataItem): TDataSet;
begin
  result:=TSQLDataSet.Create(AOwner);

  AddFields(result.FieldDefs,AData.Items.AsArray);

//  TSQLDataSet(result).CreateDataSet;
end;

class function TDBSqlExprEngine.CreateQuery(
  const AConnection: TCustomConnection; const SQL: String): TDataSet;
var Query : TSQLQuery;
begin
  Query:=TSQLQuery.Create(AConnection.Owner);
  Query.SQLConnection:=AConnection as TSQLConnection;

  Query.SQL.Text:=SQL;
  Query.Open;

  result:=Query;
end;

class function TDBSqlExprEngine.DriverNames: TStringDynArray;
var t : Integer;
begin
  SetLength(result,Length(DBDriverNames));

  for t:=0 to High(DBDriverNames) do
      result[t]:=DBDriverNames[t];
end;

class function TDBSqlExprEngine.DriverToName(const ADriver: String): String;
var t : Integer;
begin
  for t:=0 to High(DBDrivers) do
      if DBDrivers[t]=ADriver then
         Exit(DBDriverNames[t]);

  result:='';
end;

class function TDBSqlExprEngine.GetConnectionName(
  const AConnection: TCustomConnection): String;
begin
  if AConnection is TSQLConnection then
     result:=TSQLConnection(AConnection).ConnectionName
  else
     result:=AConnection.Name;
end;

class function TDBSqlExprEngine.GetDriver(const AIndex: Integer): String;
begin
  result:=DBDrivers[AIndex];
end;

class function TDBSqlExprEngine.GetKeyFieldNames(
  const AConnection: TCustomConnection; const ATable: String): TStrings;
begin
  result:=TStringList.Create;

//  if AConnection is TSQLConnection then
//     TSQLConnection(AConnection).GetKeyFieldNames('','',ATable,'',result);
end;

class function TDBSqlExprEngine.GetSchemas(
  const AConnection: TCustomConnection): TStrings;
begin
  result:=TStringList.Create;

  if AConnection is TSQLConnection then
     TSQLConnection(AConnection).GetSchemaNames(result);
end;

class function TDBSqlExprEngine.GetTable(const AConnection: TCustomConnection;
  const AName: String): TDataSet;
begin
  if Pos(' ',AName)>0 then
     result:=CreateQuery(AConnection,'select * from ['+AName+']')
  else
     result:=CreateQuery(AConnection,'select * from '+AName);
end;

class function TDBSqlExprEngine.GetItemNames(const AConnection: TCustomConnection;
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
    begin
      if tmpSchemas.Count=0 then
      begin
        tmpList:=TStringList.Create;
        try
          AConnection.GetTableNames(tmpList,IsSystem);
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
              AConnection.GetTableNames(tmpList,tmpSchemas[t],IsSystem);
              DoAppend(tmpList,tmpSchemas[t]+'.');
            finally
              tmpList.Free;
            end;
          end;
    end;

  begin
    tmpSchemas:=TStringList.Create;
    try
      if IncludeViews then
         AConnection.TableScope:=[tsTable,tsView]
      else
         AConnection.TableScope:=[tsTable];

      AConnection.GetSchemaNames(tmpSchemas);

      result:=TStringList.Create;
      try
        if IncludeSystem then
           AppendItems(result,True);

        AppendItems(result,False);
      except
        on Exception do
        begin
          result.Free; // avoid memory leak
          raise;
        end;
      end;

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

class procedure TDBSqlExprEngine.GuessForeignKeys(const AName: String;
  const Table: TDataSet; const AData: TDataItem; const Source: TBISource);
begin
  // ??
end;

class function TDBSqlExprEngine.ImportFile(const Source: TBIDB;
  const AFileName: String): TDataArray;

  function ImportFile(const ADriver:String):TDataArray;
  var C : TSQLConnection;
  begin
    C:=TSQLConnection.Create(nil);
    try
      C.DriverName:=ADriver;
      C.Params.Add('Database='+AFileName);

      // User_Name should not be empty (SQLExpress error)
      //C.Params.Add('User_Name=""');
      //C.Params.Add('Password=""');

      C.LoginPrompt:=False;

      result:=Source.Import(C);
    finally
      C.Free;
    end;
  end;

var tmp : String;
begin
  tmp:=TPath.GetExtension(AFileName);

  if SameText(tmp,'.sdb') then
     result:=ImportFile('SQlite')
  else
  if SameText(tmp,'.mdb') or SameText(tmp,'.accdb') then
     result:=ImportFile('Odbc')
  else
     result:=nil;
end;

class function TDBSqlExprEngine.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('Microsoft Access files','*.mdb;*.accdb');
  result.Add('dBase files','*.dbf');
  result.Add('Paradox files','*.db');
  result.Add('SQLite files','*.sdb');
  result.Add('Interbase files','*.gdb');
  result.Add('Firebird files','*.fdb');
end;

class function TDBSqlExprEngine.Supports(const Extension:String):Boolean;
begin
  result:=SameText(Extension,'.mdb') or  // Access
          SameText(Extension,'.dbf') or  // DBase
          SameText(Extension,'.accdb') or  // Access
          SameText(Extension,'.db') or  // Paradox
          SameText(Extension,'.sdb') or  // SQLite
          SameText(Extension,'.gdb') or  // Interbase http://www.firebirdfaq.org/faq353/
          SameText(Extension,'.fdb');  // Firebird
end;

type
  TDBSqlExprTester=class(TBIDBTester)
  public
    class procedure Test(const Driver,Database,Server,Port,User,Password:String;
                         const Prompt:Boolean); override;
  end;

class function TDBSqlExprEngine.Tester: TBIDBTesterClass;
begin
  result:=TDBSqlExprTester;
end;

{ TDBSqlExprTester }

class procedure TDBSqlExprTester.Test(const Driver, Database, Server, Port,
  User, Password: String; const Prompt: Boolean);
var tmp : TDBSqlExprEngine;
    tmpC : TCustomConnection;
begin
  tmp:=TDBSqlExprEngine.Create;
  try
    tmpC:=tmp.DoCreateConnection(Driver,Server,Port,Database,User,Password,Prompt);
    try
      tmpC.Open;
    finally
      tmpC.Free;
    end;
  finally
    tmp.Free;
  end;
end;

initialization
  TBIDB.Engine:=TDBSqlExprEngine.Create;
finalization
  TBIDB.Engine:=nil;
end.
