{*********************************************}
{  TeeBI Software Library                     }
{  UniDAC database support                    }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.DB.UniDAC;

interface

// Database Engine for DevArt UniDAC "Data Access Components"

{
  https://www.devart.com/unidac

  "MSAccess" and "MSClasses" units are required by this unit.

  Usage:

    uses
      BI.DB, BI.DB.UniDAC;

    TBIDB.Engine:=TDBUniDACEngine.Create;

}

uses
  System.Classes, System.Types, Data.DB, MSAccess, BI.DB, BI.DataItem,
  BI.Arrays, BI.Persist, BI.DataSource;

type
  TDBUniDACEngine=class(TBIDBEngine)
  public
    class function CloneConnection(const AConn:TCustomConnection): TCustomConnection; override;
    class function CreateConnection(const Definition:TDataDefinition):TCustomConnection; override;
    class function CreateDataSet(const AOwner:TComponent; const AData:TDataItem):TDataSet; override;
    class function CreateQuery(const AConnection:TCustomConnection; const SQL:String):TDataSet; override;
    class function DriverNames:TStringDynArray; override;
    class function DriverToName(const ADriver:String):String; override;
    class function GetConnectionName(const AConnection:TCustomConnection):String; override;
    class function GetDriver(const AIndex:Integer):String; override;
    class function GetKeyFieldNames(const AConnection:TCustomConnection; const ATable:String):TStrings; override;
    class function GetSchemas(const AConnection:TCustomConnection):TStrings;
    class function GetTable(const AConnection:TCustomConnection; const AName:String):TDataSet; override;
    class function GetTableNames(const AConnection:TCustomConnection):TStrings; override;
    class procedure GuessForeignKeys(const AName:String; const Table:TDataSet; const AData:TDataItem; const Source:TBISource); override;
    class function ImportFile(const Source:TBIDB; const AFileName:String):TDataArray; override;
    class function Tester:TBIDBTesterClass; override;
  end;

  {
  TBIUniDAC=class(TBIDB)
  private
    function Import(const Connection:TMSConnection; const AName:String):TDataItem; overload;
  public
    function Import(const Connection:TMSConnection):TDataItem; overload;
    class function Import(const Connection:TMSConnection; const MultiThread:Boolean):TDataItem; overload;
  end;
  }

implementation
