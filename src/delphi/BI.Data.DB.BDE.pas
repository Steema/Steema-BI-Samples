{*********************************************}
{  TeeBI Software Library                     }
{  Borland BDE data import                    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.DB.BDE;

interface

// "Engine" class to support importing BDE ("Borland Database Engine")
// databases, tables and queries (TDatabase, TTable and TQuery components)

uses
  System.Classes, System.Types,
  Data.DB, BDE.DBTables, BI.Data, BI.Data.DB, BI.Persist, BI.DataSource;

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
    class function GetConnectionName(const AConnection:TCustomConnection):String; override;
    class function GetDriver(const AIndex:Integer):String; override;
    class function GetItemNames(const AConnection:TCustomConnection; const IncludeSystem:Boolean):TStrings; override;
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
