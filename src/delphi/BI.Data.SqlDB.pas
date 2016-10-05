{*********************************************}
{  TeeBI Software Library                     }
{  Lazarus Database Import                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.SqlDB;

interface

uses
  Classes, Types, DB, BI.Data, BI.Persist, BI.DataSource, BI.Data.DB,
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
