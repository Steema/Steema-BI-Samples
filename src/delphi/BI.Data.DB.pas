{*********************************************}
{  TeeBI Software Library                     }
{  Generic Database data import and export    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.DB;

interface

uses
  System.Classes, System.Types, Data.DB,
  BI.Data.Dataset, BI.Data, BI.Arrays, BI.DataSource, BI.Persist;

type
  TBIDB=class;

  TBIDBEngine=class abstract
  public
    class procedure AddFields(const Fields:TFieldDefs; const Data:TDataItem);
    class function CloneConnection(const AConn:TCustomConnection): TCustomConnection; virtual; abstract;
    class function CreateConnection(const Definition:TDataDefinition):TCustomConnection; virtual; abstract;
    class function CreateDataSet(const AOwner:TComponent; const AData:TDataItem):TDataSet; virtual; abstract;
    class function CreateQuery(const AConnection:TCustomConnection; const SQL:String):TDataSet; virtual; abstract;
    class function DriverNames:TStringDynArray; virtual; abstract;
    class function DriverToName(const ADriver:String):String; virtual; abstract;
    class function GetConnectionName(const AConnection:TCustomConnection):String; virtual; abstract;
    class function GetDriver(const AIndex:Integer):String; virtual; abstract;
    class function GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings; virtual; abstract;
    class function GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet; virtual; abstract;
    class function GetItemNames(const AConnection:TCustomConnection; const IncludeSystem:Boolean):TStrings; virtual; abstract;
    class procedure GuessForeignKeys(const AName:String; const Table:TDataSet;
                                     const AData:TDataItem; const Source:TBISource); virtual; abstract;
    class function ImportFile(const Source:TBIDB; const AFileName:String):TDataArray; virtual; abstract;
    class procedure StartParallel; virtual;
    class function Supports(const Extension:String):Boolean; virtual; abstract;
  end;

  TBIDB=class(TBIDatasetSource)
  private
    class var
      FEngine : TBIDBEngine;

    function Import(const Connection:TCustomConnection; const AName:String):TDataItem; overload;
    class function GetEngine: TBIDBEngine; static;
    function GetItems(const Connection:TCustomConnection):TStrings;
    class procedure SetEngine(const Value: TBIDBEngine); static;
  protected
    function DoImportFile(const FileName:String):TDataArray; override;
  public
    Constructor CreateEngine(const AEngine:TBIDBEngine);

    function Import(const Connection:TCustomConnection):TDataArray; overload;

    class function IncludedItems(const ADef:TDataDefinition): TDataItem;
    class function Import(const Connection:TCustomConnection; const MultiThread:Boolean):TDataArray; overload;
    class function Supports(const Extension:String):Boolean; override;
    class property Engine:TBIDBEngine read GetEngine write SetEngine;
  end;

  TBIDBExport=class
  public
    class function ToMemTable(const AOwner:TComponent; const Data:TDataItem):TDataSet; static;
//    class function ToMemTable(const AOwner:TComponent; const Cursor:TDataCursor):TDataSet; overload; static;
  end;

implementation
