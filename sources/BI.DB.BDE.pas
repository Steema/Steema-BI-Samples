{*********************************************}
{  TeeBI Software Library                     }
{  Borland BDE data import                    }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.BDE;

interface

// "Engine" class to support importing BDE ("Borland Database Engine")
// databases, tables and queries (TDatabase, TTable and TQuery components)

(*
  Use this unit to enable BDE as the "Engine" for all TeeBI database operations.

  Usage:

    uses
      BI.DB, BI.DB.BDE;

    TBIDB.Engine:=TDBBDEEngine.Create;
*)

uses
  System.Classes, System.Types,
  Data.DB, BDE.DBTables, BI.DataItem, BI.DB, BI.Persist, BI.DataSource;

type
  TDBBDEEngine=class(TBIDBEngine)
  private
    class procedure TryOpenTable(const ATable:TTable); static;
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
    class function GetItemNames(const AConnection:TCustomConnection;
                                const IncludeSystem,IncludeViews:Boolean):TStrings; override;
    class function GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings; override;
    class function GetSchemas(const AConnection:TCustomConnection):TStrings;
    class function GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet; override;
    class procedure GuessForeignKeys(const AName:String; const Table:TDataSet; const AData:TDataItem; const Source:TBISource); override;
    class function ImportFile(const Source:TBIDB; const AFileName:String):TDataArray; override;
    class function Supports(const Extension:String):Boolean; override;
    class function SupportsParallel:Boolean; override;
    class function Tester:TBIDBTesterClass; override;
  end;

implementation

uses
  System.IOUtils, System.SysUtils;

{ TDBBDEEngine }

class function TDBBDEEngine.CloneConnection(
  const AConn: TCustomConnection): TCustomConnection;
begin
  result:=AConn;
end;

function CreateDatabase(const AName:String):TDatabase;
begin
  result:=TDatabase.Create(nil);
  result.DatabaseName:=AName;
  result.ReadOnly:=True;
end;

class function TDBBDEEngine.CreateConnection(
  const Definition: TDataDefinition): TCustomConnection;
var tmp : String;
    tmpSession : TSession;
begin
  result:=CreateDatabase(Definition['DBDatabase']);

  tmp:=Definition['DBPassword'];

  if tmp<>'' then
  begin
    tmpSession:=TSession.Create(result);

    tmpSession.SessionName:='BI';
    tmpSession.AddPassword(tmp);
    tmpSession.Active:=True;

    TDatabase(result).SessionName:=tmpSession.SessionName;
  end;
end;

class function TDBBDEEngine.CreateDataSet(const AOwner: TComponent;
  const AData: TDataItem): TDataSet;
begin
  result:=TBDEDataSet.Create(AOwner);
end;

class function TDBBDEEngine.CreateQuery(const AConnection: TCustomConnection;
  const SQL: String): TDataSet;
begin
  result:=TQuery.Create(AConnection.Owner);
  TQuery(result).DatabaseName:=(AConnection as TDatabase).DatabaseName;
  TQuery(result).SQL.Text:=SQL;
  result.Open;
end;

const
  BDE_Name='Borland Database Engine';

class function TDBBDEEngine.DriverNames: TStringDynArray;
begin
  SetLength(result,1);
  result[0]:=BDE_Name;
end;

class function TDBBDEEngine.DriverToName(const ADriver: String): String;
begin
  if ADriver='BDE' then
     result:=BDE_Name
  else
     result:='';
end;

class function TDBBDEEngine.GetConnectionName(
  const AConnection: TCustomConnection): String;
begin
  result:=(AConnection as TDatabase).DatabaseName;
end;

class function TDBBDEEngine.GetDriver(const AIndex: Integer): String;
begin
  result:='BDE';
end;

class function TDBBDEEngine.GetKeyFieldNames(
  const AConnection: TCustomConnection; const ATable: String): TStrings;
begin
  result:=TStringList.Create;
end;

class function TDBBDEEngine.GetSchemas(
  const AConnection: TCustomConnection): TStrings;
begin
  result:=TStringList.Create;
end;

type
  TBIBDETester=class(TBIDBTester)
  public
    class procedure Test(const Driver,Database,Server,Port,User,Password:String;
                         const Prompt:Boolean); override;
  end;

class function TDBBDEEngine.Tester: TBIDBTesterClass;
begin
  result:=TBIBDETester;
end;

class procedure TDBBDEEngine.TryOpenTable(const ATable:TTable);
var tmp : TTableType;
begin
  if ATable.TableType=TTableType.ttDefault then
  begin
    tmp:=TTableType.ttDefault;
    repeat
      try
        ATable.TableType:=tmp;
        ATable.Open;
        Exit;
      except
        case ATable.TableType of
          TTableType.ttDefault: tmp:=TTableType.ttParadox;
          TTableType.ttParadox: tmp:=TTableType.ttDBase;
          TTableType.ttDBase: tmp:=TTableType.ttFoxPro;
          TTableType.ttFoxPro: tmp:=TTableType.ttASCII;
        else
          Exit;
        end;
      end;
    until tmp=TTableType.ttASCII;
  end
  else
    ATable.Open;
end;

class function TDBBDEEngine.GetTable(const AConnection: TCustomConnection;
  const AName: String): TDataSet;
begin
  result:=TTable.Create(AConnection.Owner);
  TTable(result).DatabaseName:=(AConnection as TDatabase).DatabaseName;
  TTable(result).SessionName:=(AConnection as TDatabase).SessionName;
  TTable(result).TableName:=AName;
  TTable(result).ReadOnly:=True;

  TryOpenTable(TTable(result));
end;

class function TDBBDEEngine.GetItemNames(const AConnection:TCustomConnection;
                                         const IncludeSystem,IncludeViews:Boolean):TStrings;
begin
  result:=TStringList.Create;
  (AConnection as TDatabase).GetTableNames(result);
end;

class procedure TDBBDEEngine.GuessForeignKeys(const AName: String;
  const Table: TDataSet; const AData: TDataItem; const Source: TBISource);
begin
  inherited;

  // Pending
end;

class function TDBBDEEngine.ImportFile(const Source: TBIDB;
  const AFileName: String): TDataArray;
var tmp : TTable;
begin
  if Supports(TPath.GetExtension(AFileName)) then
  begin
    tmp:=TTable.Create(nil);
    try
      tmp.TableName:=AFileName;
      tmp.Open;

      SetLength(result,1);
      result[0]:=Source.Import(tmp,TPath.GetFileNameWithoutExtension(AFileName));
    finally
      tmp.Free;
    end;
  end
  else
     result:=nil;
end;

class function TDBBDEEngine.FileFilter: TFileFilters;
begin
  result:=nil;
  result.Add('dBase files','*.dbf');
  result.Add('Paradox files','*.db');
end;

class function TDBBDEEngine.Supports(const Extension: String): Boolean;
begin
  result:=SameText(Extension,'.dbf') or  // DBase
          SameText(Extension,'.db');  // Paradox
end;

class function TDBBDEEngine.SupportsParallel:Boolean;
begin
  result:=False; // BDE is not multi-thread
end;

{ TBIBDETester }

class procedure TBIBDETester.Test(const Driver, Database, Server, Port, User,
  Password: String; const Prompt: Boolean);
var tmp : TDatabase;
begin
  tmp:=CreateDatabase(Database);
  try
    try
      tmp.Open;
    except
      on Exception do ;
    end;
  finally
    tmp.Free;
  end;
end;

initialization
  TBIDB.Engine:=TDBBDEEngine.Create;
finalization
  TBIDB.Engine:=nil;
end.
